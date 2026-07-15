
library(tidyverse)
library(sf)
library(DBI)
library(duckdb)
library(h3)
library(arrow)
library(stringi)
library(blob)

# ── Logging helper ─────────────────────────────────────────────────────────────
log_status <- function(...) {
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
  flush.console()
}

# ── Config ─────────────────────────────────────────────────────────────────────
BASE_DIR <- "data/Location_Repair/02_feature_engineering"
OUT_DIR     <- file.path(BASE_DIR, "outputs")
STATE_DIR   <- file.path(OUT_DIR, "parcel_attrs_by_state")
DB_PATH     <- file.path(OUT_DIR, "final_features.duckdb")
GPKG_PATH   <- file.path(BASE_DIR, "training_locations.gpkg")
K_RINGS     <- 18

log_status("========================================")
log_status("build_features_part3.R started")
log_status("K rings:", K_RINGS)
log_status("========================================")

# ── Keywords ───────────────────────────────────────────────────────────────────
owner_keywords <- c('area','auth','authority','bay','beach','board','borough',
                    'city','co','commissioners','council','county','dist','district',
                    'falls','fort','lake','metro','municipal','new','north','plant',
                    'public','regional','river','san','sanitary','sanitation',
                    'service','sewage','sewer','sewerage','state','town','township',
                    'treatment','utilities','utility','village','wastewater',
                    'water','works')

ww_keywords <- c('wwtp','stp','system','cs','collection','wwtf','sewer','sewers',
                 'wastewater','sd','wwt','stormwater','wpcp','sewerage','water',
                 'plant','sanitary','treatment','sewage','decentralized','potw',
                 'management','wrp','authority','sanitation')

# ── Name matching helper ───────────────────────────────────────────────────────
add_name_matching <- function(df, owner_keywords, ww_keywords) {
  df %>%
    mutate(
      owner_clean = str_squish(tolower(replace_na(owner, ""))),
      geo_terms = paste(
        ifelse(is.na(subdivision) | nchar(subdivision) == 0, "", subdivision),
        ifelse(is.na(place)       | nchar(place) == 0,       "", place),
        ifelse(is.na(county)      | nchar(county) == 0,      "", county),
        sep = "|"
      ),
      geo_terms = str_remove_all(geo_terms, "\\|{2,}"),
      geo_terms = str_remove_all(geo_terms, "^\\||\\|$"),
      owner_base = paste0("\\b(", paste(owner_keywords, collapse = "|"), ")\\b"),
      ww_base    = paste0("\\b(", paste(ww_keywords,    collapse = "|"), ")\\b"),
      owner_pattern = ifelse(
        nchar(geo_terms) > 0,
        paste0("\\b(", paste(owner_keywords, collapse = "|"), "|", geo_terms, ")\\b"),
        owner_base
      ),
      ww_pattern = ifelse(
        nchar(geo_terms) > 0,
        paste0("\\b(", paste(ww_keywords, collapse = "|"), "|", geo_terms, ")\\b"),
        ww_base
      ),
      is_municipal = stri_detect_regex(owner_clean, owner_pattern,
                                       case_insensitive = TRUE),
      is_municipal = replace_na(is_municipal, FALSE),
      owner_water  = stri_detect_regex(owner_clean, ww_pattern,
                                       case_insensitive = TRUE),
      owner_water  = replace_na(owner_water, FALSE),
      sd_match     = case_when(
        is.na(subdivision) | nchar(subdivision) == 0 ~ NA,
        TRUE ~ stri_detect_regex(owner_clean, subdivision,
                                 case_insensitive = TRUE)
      ),
      place_match  = case_when(
        is_rural ~ NA,
        TRUE     ~ stri_detect_regex(owner_clean, place,
                                     case_insensitive = TRUE)
      ),
      county_match = case_when(
        is.na(county) | nchar(county) == 0 ~ FALSE,
        TRUE ~ stri_detect_regex(owner_clean, county,
                                 case_insensitive = TRUE)
      ),
      county_match  = replace_na(county_match, FALSE),
      any_geo_match = county_match |
        (!is.na(sd_match) & sd_match) |
        (!is_rural & !is.na(place_match) & place_match)
    ) %>%
    select(-owner_clean, -geo_terms, -owner_base,
           -ww_base, -owner_pattern, -ww_pattern, -owner)
}

# ── Safe DuckDB connect ────────────────────────────────────────────────────────
safe_duckdb_connect <- function(db_path, read_only = FALSE) {
  wal_path <- paste0(db_path, ".wal")
  if (file.exists(wal_path)) {
    warning("Removing stale WAL file: ", wal_path)
    file.remove(wal_path)
  }
  dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
}

# ═══════════════════════════════════════════════════════════════════════════════
# STEP A: Load plant features
# ═══════════════════════════════════════════════════════════════════════════════
log_status("Loading plant features...")
plant_features <- arrow::read_parquet(file.path(OUT_DIR, "05_plant_features.parquet"))
log_status("   Plant features:", nrow(plant_features), "rows")

# ═══════════════════════════════════════════════════════════════════════════════
# STEP B: Consolidate state parcel parquets into parcel_features
# ═══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("STEP B: Consolidating state parcel parquets")
log_status("========================================")

state_files <- list.files(STATE_DIR, pattern = "^parcel_attrs_.*\\.parquet$",
                          full.names = TRUE)
log_status("   State parquet files found:", length(state_files))

# Check for missing states
expected_states <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA",
                     "GU","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD",
                     "ME","MI","MN","MO","MP","MS","MT","NC","ND","NE","NH",
                     "NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC",
                     "SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")
completed_states <- str_extract(basename(state_files), "(?<=parcel_attrs_)[A-Z]+(?=\\.parquet)")
missing_states   <- setdiff(expected_states, completed_states)

if (length(missing_states) > 0) {
  warning("Missing state parquets: ", paste(missing_states, collapse = ", "))
  log_status("   WARNING: Missing states:", paste(missing_states, collapse = ", "))
} else {
  log_status("   All 55 states present")
}

log_status("   Reading and combining state parquets...")
parcel_features <- map_dfr(state_files, arrow::read_parquet) %>%
  distinct(ll_uuid, .keep_all = TRUE)

log_status("   Parcel features combined:", nrow(parcel_features), "rows")
log_status("   Columns:", ncol(parcel_features))
log_status("   has_ww_keyword TRUE:", sum(parcel_features$has_ww_keyword, na.rm = TRUE))

arrow::write_parquet(parcel_features,
                     file.path(OUT_DIR, "10_parcel_features.parquet"))
log_status("   Written: 10_parcel_features.parquet")

gc()

# ═══════════════════════════════════════════════════════════════════════════════
# STEP C: Install spatial extension once
# ═══════════════════════════════════════════════════════════════════════════════
con_temp <- dbConnect(duckdb::duckdb())
dbExecute(con_temp, "INSTALL spatial;")
dbDisconnect(con_temp, shutdown = TRUE)
rm(con_temp)

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 11: Load training locations
# ═══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("STEP 11: Loading training locations")
log_status("========================================")

classes <- st_read(GPKG_PATH, layer = "classes", quiet = TRUE) %>%
  st_transform(4326) %>%
  mutate(h3_res9 = h3::geo_to_h3(
    cbind(st_coordinates(geom)[,2],
          st_coordinates(geom)[,1]), res = 9
  )) %>%
  left_join(plant_features %>% select(CWNS_ID, STATE_CODE), by = "CWNS_ID")

corrections <- st_read(GPKG_PATH, layer = "corrections", quiet = TRUE) %>%
  st_transform(4326)

log_status("   Classes:", nrow(classes))
log_status("   Corrections:", nrow(corrections))

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 12: Point-in-parcel lookup for reported locations
# ═══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("STEP 12: Point-in-parcel lookup — reported locations")
log_status("========================================")

classes_upload <- classes %>%
  mutate(geom_wkb = blob::as_blob(unclass(st_as_binary(geom)))) %>%
  st_drop_geometry() %>%
  select(CWNS_ID, class, h3_res9, STATE_CODE, geom_wkb)

reported_parcels <- map_dfr(unique(na.omit(classes$STATE_CODE)), function(s) {
  pts <- classes_upload %>% filter(STATE_CODE == s)
  if (nrow(pts) == 0) return(NULL)

  con_w <- dbConnect(duckdb::duckdb())
  dbExecute(con_w, "INSTALL spatial;")
  dbExecute(con_w, "LOAD spatial;")
  dbExecute(con_w, "SET enable_geoparquet_conversion = false;")
  on.exit(dbDisconnect(con_w, shutdown = TRUE))
  dbWriteTable(con_w, "pts", pts, overwrite = TRUE)

  tryCatch(dbGetQuery(con_w, paste0("
    SELECT pts.CWNS_ID, pts.class, p.ll_uuid
    FROM read_parquet(
      'data/parcels/state=", s, "/*.parquet'
    ) p
    INNER JOIN pts ON p.h3_index_9 = pts.h3_res9
    WHERE ST_Intersects(
      ST_GeomFromWKB(p.wkb_geometry),
      ST_GeomFromWKB(pts.geom_wkb)
    )
  ")), error = function(e) {
    cat("Reported parcel lookup failed for state", s, "\n")
    NULL
  })
}) %>%
  distinct(CWNS_ID, .keep_all = TRUE)

log_status("   Reported parcels found:", nrow(reported_parcels))
arrow::write_parquet(reported_parcels,
                     file.path(OUT_DIR, "12_reported_parcels.parquet"))
log_status("   Written: 12_reported_parcels.parquet")

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 13: Point-in-parcel lookup for corrected locations
# ═══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("STEP 13: Point-in-parcel lookup — corrected locations")
log_status("========================================")

corrected_sf <- corrections %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("Corrected_X", "Corrected_Y"), crs = 4269) %>%
  st_transform(4326) %>%
  mutate(h3_res9 = h3::geo_to_h3(
    cbind(st_coordinates(geometry)[,2],
          st_coordinates(geometry)[,1]), res = 9
  ))

corrected_upload <- corrected_sf %>%
  mutate(geom_wkb = blob::as_blob(unclass(st_as_binary(geometry)))) %>%
  st_drop_geometry() %>%
  select(CWNS_ID, h3_res9, geom_wkb)

corrected_parcels <- map_dfr(unique(plant_features$STATE_CODE), function(s) {
  cwns_in_state <- plant_features %>%
    filter(STATE_CODE == s) %>%
    pull(CWNS_ID)

  pts <- corrected_upload %>% filter(CWNS_ID %in% cwns_in_state)
  if (nrow(pts) == 0) return(NULL)

  con_w <- dbConnect(duckdb::duckdb())
  dbExecute(con_w, "INSTALL spatial;")
  dbExecute(con_w, "LOAD spatial;")
  dbExecute(con_w, "SET enable_geoparquet_conversion = false;")
  on.exit(dbDisconnect(con_w, shutdown = TRUE))
  dbWriteTable(con_w, "pts", pts, overwrite = TRUE)

  tryCatch(dbGetQuery(con_w, paste0("
    SELECT pts.CWNS_ID, p.ll_uuid AS corrected_ll_uuid
    FROM read_parquet(
      'data/parcels/state=", s, "/*.parquet'
    ) p
    INNER JOIN pts ON p.h3_index_9 = pts.h3_res9
    WHERE ST_Intersects(
      ST_GeomFromWKB(p.wkb_geometry),
      ST_GeomFromWKB(pts.geom_wkb)
    )
  ")), error = function(e) {
    cat("Corrected parcel lookup failed for state", s, "\n")
    NULL
  })
}) %>%
  distinct(CWNS_ID, .keep_all = TRUE)

log_status("   Corrected parcels found:", nrow(corrected_parcels))
arrow::write_parquet(corrected_parcels,
                     file.path(OUT_DIR, "13_corrected_parcels.parquet"))
log_status("   Written: 13_corrected_parcels.parquet")

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 14: Build Stage 1 training data
# ═══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("STEP 14: Building Stage 1 training data")
log_status("========================================")

stage1_training <- reported_parcels %>%
  left_join(plant_features %>% select(-STATE_CODE), by = "CWNS_ID") %>%
  left_join(parcel_features %>% select(-state),     by = "ll_uuid") %>%
  add_name_matching(owner_keywords, ww_keywords)

log_status("   Stage 1 training rows:", nrow(stage1_training))
log_status("   Correct:",   sum(stage1_training$class == "Correct"))
log_status("   Incorrect:", sum(stage1_training$class == "Incorrect"))

arrow::write_parquet(stage1_training,
                     file.path(OUT_DIR, "14_stage1_training.parquet"))
log_status("   Written: 14_stage1_training.parquet")

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 15: Build Stage 2 training data
# ═══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("STEP 15: Building Stage 2 training data")
log_status("========================================")

corrected_cwns <- corrected_parcels$CWNS_ID
stage2_plants  <- plant_features %>% filter(CWNS_ID %in% corrected_cwns)
log_status("   Plants for Stage 2:", nrow(stage2_plants))

# Pull candidate parcels within k=K_RINGS for corrected plants
stage2_candidates <- map_dfr(unique(stage2_plants$STATE_CODE), function(s) {
  state_plants <- stage2_plants %>% filter(STATE_CODE == s)
  h3_cells     <- state_plants %>%
    pull(h3_res9) %>%
    map(~ h3::k_ring(.x, K_RINGS)) %>%
    unlist() %>%
    unique()

  h3_filter <- paste0("'", h3_cells, "'", collapse = ", ")

  con_w <- dbConnect(duckdb::duckdb())
  dbExecute(con_w, "INSTALL spatial;")
  dbExecute(con_w, "LOAD spatial;")
  dbExecute(con_w, "SET enable_geoparquet_conversion = false;")
  on.exit(dbDisconnect(con_w, shutdown = TRUE))

  tryCatch(dbGetQuery(con_w, paste0("
    SELECT ll_uuid, h3_index_9, state
    FROM read_parquet(
      'data/parcels/state=", s, "/*.parquet'
    )
    WHERE h3_index_9 IN (", h3_filter, ")
  ")), error = function(e) {
    cat("Stage 2 candidate fetch failed for state", s, "\n")
    NULL
  })
}) %>%
  distinct(ll_uuid, .keep_all = TRUE)

log_status("   Stage 2 candidate parcels:", nrow(stage2_candidates))

# Join parcel features
stage2_parcel_features <- stage2_candidates %>%
  left_join(
    parcel_features %>% select(-state, -h3_index_9),
    by = "ll_uuid"
  )

stopifnot("h3_index_9" %in% names(stage2_parcel_features))
log_status("   h3_index_9 confirmed in stage2_parcel_features")

# Corrected plant coordinates for distance calculation
corrected_coords <- corrections %>%
  st_drop_geometry() %>%
  select(CWNS_ID, Original_X, Original_Y)

stage2_training <- map_dfr(corrected_cwns, function(id) {
  plant       <- stage2_plants     %>% filter(CWNS_ID == id)
  correct_id  <- corrected_parcels %>% filter(CWNS_ID == id) %>% pull(corrected_ll_uuid)
  reported_id <- reported_parcels  %>% filter(CWNS_ID == id) %>% pull(ll_uuid)
  coords      <- corrected_coords  %>% filter(CWNS_ID == id)

  if (nrow(plant) == 0 || length(correct_id) == 0) return(NULL)
  if (nrow(coords) == 0) return(NULL)

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
    sf::st_point(c(coords$Original_X[1], coords$Original_Y[1])),
    crs = 4269
  ) %>% sf::st_transform(5070)

  candidate_sf <- candidate_centers %>%
    sf::st_as_sf(coords = c("centroid_lng", "centroid_lat"), crs = 4326) %>%
    sf::st_transform(5070)

  distances <- sf::st_distance(candidate_sf, reported_pt) %>% as.numeric()

  candidates %>%
    mutate(
      CWNS_ID       = id,
      label         = as.integer(ll_uuid == correct_id),
      is_reported   = ll_uuid %in% reported_id,
      distance_m    = distances,
      log_distance  = log1p(distances),
      distance_ring = floor(distances / 330),
      within_1km    = distances <= 1000,
      within_5km    = distances <= 5000
    )
})

log_status("   Stage 2 raw candidates labeled:", nrow(stage2_training))

# Join plant features and name matching
stage2_training <- stage2_training %>%
  left_join(stage2_plants %>% select(-STATE_CODE), by = "CWNS_ID") %>%
  add_name_matching(owner_keywords, ww_keywords)

log_status("   Stage 2 training rows:", nrow(stage2_training))
log_status("   Positive labels:", sum(stage2_training$label == 1))
log_status("   Negative labels:", sum(stage2_training$label == 0))
log_status("   Reported parcels included:",
           sum(stage2_training$is_reported, na.rm = TRUE))
log_status("   has_ww_keyword positives:",
           sum(stage2_training$has_ww_keyword & stage2_training$label == 1, na.rm = TRUE))

arrow::write_parquet(stage2_training,
                     file.path(OUT_DIR, "15_stage2_training.parquet"))
log_status("   Written: 15_stage2_training.parquet")

rm(stage2_candidates, stage2_parcel_features, candidate_centers)
gc()

# ═══════════════════════════════════════════════════════════════════════════════
# STEP D: Write final DuckDB
# ═══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("STEP D: Writing final DuckDB")
log_status("========================================")

con <- safe_duckdb_connect(DB_PATH, read_only = FALSE)
dbExecute(con, "LOAD spatial;")

dbBegin(con)
tryCatch({
  # Plant features
  dbWriteTable(con, "plant_features",  plant_features,  overwrite = TRUE)
  dbExecute(con, "CREATE UNIQUE INDEX pf_cwns  ON plant_features (CWNS_ID);")
  dbExecute(con, "CREATE INDEX pf_state        ON plant_features (STATE_CODE);")
  dbExecute(con, "CREATE INDEX pf_h3           ON plant_features (h3_res9);")
  log_status("   plant_features written:", nrow(plant_features), "rows")

  # Parcel features
  dbWriteTable(con, "parcel_features", parcel_features, overwrite = TRUE)
  dbExecute(con, "CREATE UNIQUE INDEX pcf_uuid  ON parcel_features (ll_uuid);")
  dbExecute(con, "CREATE INDEX pcf_state        ON parcel_features (state);")
  dbExecute(con, "CREATE INDEX pcf_h3           ON parcel_features (h3_index_9);")
  dbExecute(con, "CREATE INDEX pcf_state_h3     ON parcel_features (state, h3_index_9);")
  log_status("   parcel_features written:", nrow(parcel_features), "rows")

  # Stage 1 training
  dbWriteTable(con, "stage1_training", stage1_training, overwrite = TRUE)
  dbExecute(con, "CREATE INDEX s1_cwns  ON stage1_training (CWNS_ID);")
  dbExecute(con, "CREATE INDEX s1_class ON stage1_training (class);")
  log_status("   stage1_training written:", nrow(stage1_training), "rows")

  # Stage 2 training
  dbWriteTable(con, "stage2_training", stage2_training, overwrite = TRUE)
  dbExecute(con, "CREATE INDEX s2_cwns  ON stage2_training (CWNS_ID);")
  dbExecute(con, "CREATE INDEX s2_label ON stage2_training (label);")
  log_status("   stage2_training written:", nrow(stage2_training), "rows")

  dbCommit(con)
  log_status("   DuckDB committed successfully")
}, error = function(e) {
  dbRollback(con)
  stop(e)
})

dbDisconnect(con, shutdown = TRUE)

log_status("========================================")
log_status("build_features_part3.R complete")
log_status("Outputs in:", OUT_DIR)
log_status("DuckDB:", DB_PATH)
log_status("========================================")
