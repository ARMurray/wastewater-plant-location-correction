# Add at the very top of the script, before any library() calls
library(tidyverse)
library(sf)
library(DBI)
library(parallelly)
library(duckdb)
library(h3)
library(terra)
library(exactextractr)
library(furrr)
library(arrow)

# ── Logging helper ────────────────────────────────────────────────────────────
log_status <- function(...) {
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
  flush.console()
}

# ── Command line arguments ────────────────────────────────────────────────────
args       <- commandArgs(trailingOnly = TRUE)
STATE_CODE <- args[1]
K_RINGS    <- as.integer(args[2])

log_status("========================================")
log_status("Job started")
log_status("State:", STATE_CODE)
log_status("K rings:", K_RINGS)
log_status("Node:", Sys.getenv("SLURMD_NODENAME"))
log_status("========================================")

# ── Config ────────────────────────────────────────────────────────────────────
N_WORKERS    <- parallelly::availableCores()
PARQUET_GLOB <- "data/parcels/*/*.parquet"
PLANTS_PATH <- "data/Location_Repair/02_feature_engineering/CWNS_files/PHYSICAL_LOCATION.txt"
NLCD_PATH    <- "data/nlcd/Annual_NLCD_LndCov_2023_CU_C1V1.tif"
OUT_DIR <- "data/Location_Repair/02_feature_engineering/nlcd_outputs"

log_status("Workers available:", N_WORKERS)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ── Plants ────────────────────────────────────────────────────────────────────
log_status("Loading treatment plants...")
plants_df <- read_csv(PLANTS_PATH, show_col_types = FALSE)

plants_state <- plants_df %>%
  filter(STATE_CODE == !!STATE_CODE) %>%
  rowwise() %>%
  mutate(h3_res9 = h3::geo_to_h3(c(LATITUDE, LONGITUDE), res = 9)) %>%
  ungroup()

log_status("Plants in state:", nrow(plants_state))
stopifnot(nrow(plants_state) > 0)
stopifnot(n_distinct(plants_state$h3_res9) > 0)

# ── Expand H3 search window ───────────────────────────────────────────────────
log_status("Expanding H3 search window (k =", K_RINGS, ")...")
if (K_RINGS > 0) {
  plants_state <- plants_state %>%
    rowwise() %>%
    mutate(h3_search_cells = list(h3::k_ring(h3_res9, K_RINGS))) %>%
    ungroup()
} else {
  plants_state <- plants_state %>%
    mutate(h3_search_cells = map(h3_res9, ~ .x))
}

h3_all <- plants_state %>%
  pull(h3_search_cells) %>%
  unlist() %>%
  unique()

log_status("Unique H3 cells to query:", length(h3_all))

# ── DuckDB setup ──────────────────────────────────────────────────────────────
# Each job uses its own in-memory instance for setup checks
# Workers query parquet directly so no shared DB file is needed here
log_status("Setting up DuckDB connection...")
con <- dbConnect(duckdb::duckdb())  # in-memory, no shared file
dbExecute(con, "INSTALL spatial;")
dbExecute(con, "LOAD spatial;")
dbExecute(con, paste0("PRAGMA threads=", N_WORKERS))
dbExecute(con, "SET enable_geoparquet_conversion = false;")
dbDisconnect(con, shutdown = TRUE)
log_status("DuckDB setup complete, connection released")

# ── Parallel parcel fetch ─────────────────────────────────────────────────────
log_status("Starting parallel parcel fetch...")
plan(multisession, workers = N_WORKERS)

h3_chunks <- split(h3_all, ceiling(seq_along(h3_all) / 500))
log_status("H3 chunks to process:", length(h3_chunks))

fetch_hex_chunk <- function(h3_ids) {
  library(DBI)
  library(duckdb)
  
  con_w <- dbConnect(duckdb::duckdb())  # in-memory, no shared file
  dbExecute(con_w, "LOAD spatial;")
  dbExecute(con_w, "SET enable_geoparquet_conversion = false;")
  on.exit(dbDisconnect(con_w, shutdown = TRUE))
  
  h3_filter <- paste0("'", h3_ids, "'", collapse = ", ")
  state     <- STATE_CODE
  
  dbGetQuery(con_w, paste0("
    SELECT
      p.ll_uuid,
      p.h3_index_9,
      p.state,
      ST_AsWKB(ST_GeomFromWKB(p.wkb_geometry)) AS geom_wkb
    FROM read_parquet(
      'data/parcels/state=", state, "/*.parquet'
    ) p
    WHERE p.h3_index_9 IN (", h3_filter, ")
  "))
}

t1 <- system.time({
  parcel_list  <- future_map(
    h3_chunks,
    fetch_hex_chunk,
    .options = furrr_options(seed = TRUE)
  )
  parcel_geoms <- bind_rows(parcel_list) %>%
    distinct(ll_uuid, .keep_all = TRUE)
})

log_status("Parcel fetch complete")
log_status("  Time elapsed:     ", round(t1["elapsed"], 1), "s")
log_status("  Parcels retrieved:", nrow(parcel_geoms))
log_status("  Unique H3 cells:  ", n_distinct(parcel_geoms$h3_index_9))
stopifnot(nrow(parcel_geoms) > 0)

# ── Deserialize WKB to sf ─────────────────────────────────────────────────────
log_status("Deserializing geometries to sf...")
parcel_sf <- parcel_geoms %>%
  mutate(geometry = st_as_sfc(geom_wkb, crs = 4326)) %>%
  select(-geom_wkb) %>%
  st_as_sf()
log_status("sf object created:", nrow(parcel_sf), "features")

# ── NLCD extraction ───────────────────────────────────────────────────────────
log_status("Loading NLCD raster...")
nlcd        <- terra::rast(NLCD_PATH)
parcel_proj <- st_transform(parcel_sf, crs = st_crs(as.character(crs(nlcd))))
log_status("Parcels reprojected to NLCD CRS")

extract_water_metrics <- function(df) {
  vals <- df$value[df$coverage_fraction >= 0.5]
  vals <- vals[!is.na(vals)]
  
  total_pixels   <- length(vals)
  water_pixels   <- sum(vals == 11)
  dominant_class <- if (total_pixels > 0) as.integer(names(which.max(table(vals)))) else NA_integer_
  dominant_count <- if (total_pixels > 0) as.integer(max(table(vals)))              else NA_integer_
  
  data.frame(
    total_pixels   = total_pixels,
    water_pixels   = water_pixels,
    dominant_class = dominant_class,
    dominant_count = dominant_count
  )
}

log_status("Starting NLCD extraction for", nrow(parcel_proj), "parcels...")
t2 <- system.time({
  nlcd_results <- exact_extract(
    nlcd,
    parcel_proj,
    fun                 = extract_water_metrics,
    summarize_df        = TRUE,
    max_cells_in_memory = 3e7,
    progress            = TRUE
  )
})
log_status("NLCD extraction complete")
log_status("  Time elapsed:", round(t2["elapsed"], 1), "s")

# ── Assemble final table ──────────────────────────────────────────────────────
log_status("Assembling final results table...")
nlcd_results <- nlcd_results %>%
  mutate(
    ll_uuid    = parcel_proj$ll_uuid,
    h3_index_9 = parcel_proj$h3_index_9,
    state      = parcel_proj$state,
    has_water  = water_pixels > 0
  ) %>%
  select(ll_uuid, h3_index_9, state, total_pixels, water_pixels,
         has_water, dominant_class, dominant_count)

log_status("Results summary:")
log_status("  Parcels extracted:    ", nrow(nlcd_results))
log_status("  Parcels with water:   ", sum(nlcd_results$water_pixels > 0, na.rm = TRUE))
log_status("  Parcels with 0 pixels:", sum(nlcd_results$total_pixels == 0, na.rm = TRUE))
log_status("  Dominant classes seen:", n_distinct(nlcd_results$dominant_class))

# ── Write output ──────────────────────────────────────────────────────────────
out_path <- file.path(OUT_DIR, paste0("nlcd_", STATE_CODE, "_k", K_RINGS, ".parquet"))
log_status("Writing output to:", out_path)
arrow::write_parquet(nlcd_results, out_path)
log_status("Output written successfully")

# ── Done ──────────────────────────────────────────────────────────────────────
total_time <- round((t1["elapsed"] + t2["elapsed"]) / 60, 1)
log_status("========================================")
log_status("Job complete for state:", STATE_CODE)
log_status("Total pipeline time:", total_time, "minutes")
log_status("Output:", out_path)
log_status("========================================")