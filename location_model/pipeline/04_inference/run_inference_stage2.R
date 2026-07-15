
library(tidyverse)
library(tidymodels)
library(sf)
library(DBI)
library(duckdb)
library(h3)
library(stringi)
library(arrow)

args       <- commandArgs(trailingOnly = TRUE)
STATE_CODE <- args[1]

log_status <- function(...) {
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
  flush.console()
}

# ── Config ────────────────────────────────────────────────────────────────────
PARCEL_ATTRS <- paste0("data/Location_Repair/02_feature_engineering/outputs/parcel_attrs_by_state/parcel_attrs_", STATE_CODE, ".parquet")
MODEL_DIR    <- "data/Location_Repair/03_models"
OUT_DIR      <- "data/Location_Repair/04_inference"
STAGE2_DIR   <- file.path(OUT_DIR, "stage2_by_state")
PLANTS_DIR   <- file.path(OUT_DIR, "stage2_by_state")
PARQUET_DIR  <- "data/parcels"
K_RINGS       <- 18
NO_PARQUET_STATES <- c("AS")
STAGE2_THRESHOLD  <- readRDS(file.path(MODEL_DIR, "stage2_optimal_threshold.rds"))

log_status("========================================")
log_status("Stage 2 Inference —", STATE_CODE)
log_status("K rings:", K_RINGS)
log_status("Stage 2 threshold:", round(STAGE2_THRESHOLD, 3))
log_status("========================================")

# Skip if no parquet data for this state
if (STATE_CODE %in% NO_PARQUET_STATES) {
  log_status("No parquet data for", STATE_CODE, "— skipping")
  quit(save = "no")
}

# Skip if already complete
out_path <- file.path(STAGE2_DIR, paste0("stage2_results_", STATE_CODE, ".parquet"))
if (file.exists(out_path)) {
  log_status("Output already exists — skipping:", out_path)
  quit(save = "no")
}

# ── Load plant list for this state ───────────────────────────────────────────
plants_path <- file.path(PLANTS_DIR, paste0("stage2_plants_", STATE_CODE, ".csv"))
if (!file.exists(plants_path)) {
  log_status("No Stage 2 plants for", STATE_CODE, "— skipping")
  quit(save = "no")
}

stage2_plants <- read_csv(plants_path, show_col_types = FALSE)
log_status("   Plants for Stage 2:", nrow(stage2_plants))

# ── Load models ───────────────────────────────────────────────────────────────
stage2_model <- readRDS(file.path(MODEL_DIR, "stage2_rf_model.rds"))
log_status("   Stage 2 model loaded")

# ── Keywords and helpers ──────────────────────────────────────────────────────
# [copy add_name_matching and prep_features from run_inference.R]

# ── Pull candidate parcels ────────────────────────────────────────────────────
log_status("Pulling candidate parcels...")

h3_cells <- stage2_plants %>%
  pull(h3_res9) %>%
  map(~ h3::k_ring(.x, K_RINGS)) %>%
  unlist() %>%
  unique()

h3_chunks <- split(h3_cells, ceiling(seq_along(h3_cells) / 500))

stage2_candidates <- map_dfr(h3_chunks, function(chunk) {
  h3_filter <- paste0("'", chunk, "'", collapse = ", ")
  
  con_w <- dbConnect(duckdb::duckdb())
  dbExecute(con_w, "INSTALL spatial;")
  dbExecute(con_w, "LOAD spatial;")
  dbExecute(con_w, "SET enable_geoparquet_conversion = false;")
  
  result <- tryCatch(dbGetQuery(con_w, paste0("
    SELECT ll_uuid, h3_index_9, state
    FROM read_parquet('", PARQUET_DIR, "/state=", STATE_CODE, "/*.parquet')
    WHERE h3_index_9 IN (", h3_filter, ")
  ")), error = function(e) NULL)
  
  dbDisconnect(con_w, shutdown = TRUE)
  result
}) %>%
  distinct(ll_uuid, .keep_all = TRUE)

log_status("   Candidate parcels:", nrow(stage2_candidates))

# ── Load parcel features for this state ──────────────────────────────────────
log_status("Loading parcel features...")

parcel_attrs <- arrow::read_parquet(PARCEL_ATTRS)

stage2_parcel_features <- stage2_candidates %>%
  left_join(parcel_attrs %>% select(-any_of(c("state", "h3_index_9"))),
            by = "ll_uuid")

log_status("   Parcel features joined:", nrow(stage2_parcel_features))

# ── Score candidates per plant ────────────────────────────────────────────────
log_status("Scoring candidates...")

stage2_results <- map_dfr(stage2_plants$CWNS_ID, function(id) {
  plant <- stage2_plants %>% filter(CWNS_ID == id)
  if (nrow(plant) == 0) return(NULL)
  
  plant_h3_cells <- h3::k_ring(plant$h3_res9, K_RINGS)
  candidates     <- stage2_parcel_features %>%
    filter(h3_index_9 %in% plant_h3_cells)
  
  if (nrow(candidates) == 0) return(NULL)
  
  candidate_centers <- candidates %>%
    mutate(
      centroid_lat = h3::h3_to_geo(h3_index_9)[,1],
      centroid_lng = h3::h3_to_geo(h3_index_9)[,2]
    )
  
  reported_pt <- sf::st_sfc(
    sf::st_point(c(plant$LONGITUDE[1], plant$LATITUDE[1])),
    crs = 4269
  ) %>% sf::st_transform(5070)
  
  candidate_sf <- candidate_centers %>%
    sf::st_as_sf(coords = c("centroid_lng", "centroid_lat"), crs = 4326) %>%
    sf::st_transform(5070)
  
  distances <- sf::st_distance(candidate_sf, reported_pt) %>% as.numeric()
  
  candidates_with_dist <- candidates %>%
    mutate(
      CWNS_ID      = id,
      distance_m   = distances,
      log_distance = log1p(distances),
      within_1km   = as.integer(distances <= 1000),
      within_5km   = as.integer(distances <= 5000)
    )
  
  candidates_full <- candidates_with_dist %>%
    left_join(plant %>% select(-STATE_CODE), by = "CWNS_ID") %>%
    add_name_matching(owner_keywords, ww_keywords) %>%
    prep_features()
  
  preds <- predict(stage2_model, candidates_full, type = "prob") %>%
    bind_cols(candidates_with_dist %>%
                select(CWNS_ID, ll_uuid, h3_index_9, state,
                       distance_m, within_1km, within_5km))
  
  preds %>%
    filter(.pred_Correct >= STAGE2_THRESHOLD) %>%
    arrange(desc(.pred_Correct)) %>%
    rename(stage2_prob_correct   = .pred_Correct,
           stage2_prob_incorrect = .pred_Incorrect)
})

log_status("   Candidates above threshold:", nrow(stage2_results))
log_status("   Plants with candidates:", n_distinct(stage2_results$CWNS_ID))

arrow::write_parquet(stage2_results, out_path)
log_status("   Written:", out_path)

log_status("========================================")
log_status("Stage 2 complete —", STATE_CODE)
log_status("========================================")