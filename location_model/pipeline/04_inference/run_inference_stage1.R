
library(tidyverse)
library(tidymodels)
library(sf)
library(DBI)
library(duckdb)
library(h3)
library(stringi)
library(furrr)
library(parallelly)
library(arrow)
library(blob)

# ── Logging ───────────────────────────────────────────────────────────────────
log_status <- function(...) {
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
  flush.console()
}

# ── Config ────────────────────────────────────────────────────────────────────
N_WORKERS        <- parallelly::availableCores()
DB_PATH          <- "data/Location_Repair/02_feature_engineering/outputs/final_features.duckdb"
PARQUET_DIR      <- "data/parcels"
PLANTS_PATH      <- "data/Location_Repair/02_feature_engineering/CWNS_files/PHYSICAL_LOCATION.txt"
MODEL_DIR        <- "data/Location_Repair/03_models"
OUT_DIR          <- "data/Location_Repair/04_inference"
K_RINGS          <- 18
STAGE1_THRESHOLD <- readRDS(file.path(MODEL_DIR, "stage1_optimal_threshold.rds"))
NO_PARQUET_STATES <- c("AS")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

log_status("========================================")
log_status("Stage 1 Inference Pipeline")
log_status("Workers:", N_WORKERS)
log_status("Stage 1 threshold:", round(STAGE1_THRESHOLD, 3))
log_status("K rings:", K_RINGS)
log_status("========================================")

# ── Keywords ──────────────────────────────────────────────────────────────────
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
      owner_clean   = str_squish(tolower(replace_na(owner, ""))),
      geo_terms     = paste(
        ifelse(is.na(subdivision) | nchar(subdivision) == 0, "", subdivision),
        ifelse(is.na(place)       | nchar(place) == 0,       "", place),
        ifelse(is.na(county)      | nchar(county) == 0,      "", county),
        sep = "|"
      ),
      geo_terms     = str_remove_all(geo_terms, "\\|{2,}"),
      geo_terms     = str_remove_all(geo_terms, "^\\||\\|$"),
      owner_base    = paste0("\\b(", paste(owner_keywords, collapse = "|"), ")\\b"),
      ww_base       = paste0("\\b(", paste(ww_keywords,    collapse = "|"), ")\\b"),
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
      is_municipal  = stri_detect_regex(owner_clean, owner_pattern,
                                        case_insensitive = TRUE),
      is_municipal  = replace_na(is_municipal, FALSE),
      owner_water   = stri_detect_regex(owner_clean, ww_pattern,
                                        case_insensitive = TRUE),
      owner_water   = replace_na(owner_water, FALSE),
      sd_match      = case_when(
        is.na(subdivision) | nchar(subdivision) == 0 ~ NA,
        TRUE ~ stri_detect_regex(owner_clean, subdivision,
                                 case_insensitive = TRUE)
      ),
      place_match   = case_when(
        is_rural ~ NA,
        TRUE     ~ stri_detect_regex(owner_clean, place,
                                     case_insensitive = TRUE)
      ),
      place_match   = replace_na(place_match, 0L),
      county_match  = case_when(
        is.na(county) | nchar(county) == 0 ~ FALSE,
        TRUE ~ stri_detect_regex(owner_clean, county,
                                 case_insensitive = TRUE)
      ),
      county_match  = replace_na(county_match, FALSE),
      any_geo_match = county_match |
        (!is.na(sd_match) & sd_match) |
        (!is_rural & !is.na(place_match) & place_match)
    ) %>%
    select(-owner_clean, -geo_terms, -owner_base, -ww_base,
           -owner_pattern, -ww_pattern)
}

# ── Feature prep helper ────────────────────────────────────────────────────────
prep_features <- function(df) {
  df %>%
    mutate(
      place_match = replace_na(place_match, 0L),
      across(where(is.logical), as.integer)
    ) %>%
    select(-any_of(c("zoning_type", "h3_res9", "h3_index_9", "state",
                     "geoid", "county_geoid", "n_parcels",
                     "pct_lbcs_activity_known", "pct_owner_known",
                     "LATITUDE", "LONGITUDE", "owner",
                     "subdivision", "place", "county")))
}

# ── Load models and plant features ────────────────────────────────────────────
log_status("Loading models and data...")

stage1_model <- readRDS(file.path(MODEL_DIR, "stage1_rf_model.rds"))

con <- dbConnect(duckdb::duckdb(), dbdir = DB_PATH, read_only = TRUE)
plant_features <- dbGetQuery(con, "SELECT * FROM plant_features")
dbDisconnect(con, shutdown = TRUE)

log_status("   Plants loaded:", nrow(plant_features))
log_status("   Stage 1 model loaded")

# ── Install spatial once ───────────────────────────────────────────────────────
con_temp <- dbConnect(duckdb::duckdb())
dbExecute(con_temp, "INSTALL spatial;")
dbDisconnect(con_temp, shutdown = TRUE)
rm(con_temp)

# ═══════════════════════════════════════════════════════════════════════════════
# STAGE 1: Score reported parcel for every plant
# ═══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("STAGE 1: Scoring reported locations...")
log_status("========================================")

plants_sf <- plant_features %>%
  drop_na(LATITUDE, LONGITUDE) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269) %>%
  st_transform(4326) %>%
  mutate(h3_res9 = h3::geo_to_h3(
    cbind(st_coordinates(geometry)[,2],
          st_coordinates(geometry)[,1]), res = 9
  ))

plants_upload <- plants_sf %>%
  mutate(geom_wkb = blob::as_blob(unclass(st_as_binary(geometry)))) %>%
  st_drop_geometry() %>%
  select(CWNS_ID, STATE_CODE, h3_res9, geom_wkb)

# Expand to k=1 neighbors to handle H3 boundary cases
plants_h3_expanded <- plants_upload %>%
  select(CWNS_ID, STATE_CODE, h3_res9) %>%
  mutate(search_cells = map(h3_res9, ~ h3::k_ring(.x, 1))) %>%
  unnest(search_cells) %>%
  rename(h3_search = search_cells)

log_status("   Running point-in-parcel lookup for", nrow(plants_upload), "plants...")

reported_parcels <- map_dfr(
  unique(na.omit(plants_upload$STATE_CODE)),
  function(s, pu = plants_upload, ph = plants_h3_expanded) {
    
    if (s %in% NO_PARQUET_STATES) return(NULL)
    
    pts    <- pu %>% filter(STATE_CODE == s)
    pts_h3 <- ph %>% filter(STATE_CODE == s)
    
    if (nrow(pts) == 0) return(NULL)
    
    con_w <- dbConnect(duckdb::duckdb())
    dbExecute(con_w, "INSTALL spatial;")
    dbExecute(con_w, "LOAD spatial;")
    dbExecute(con_w, "SET enable_geoparquet_conversion = false;")
    
    result <- tryCatch({
      dbWriteTable(con_w, "pts",    pts,    overwrite = TRUE)
      dbWriteTable(con_w, "pts_h3", pts_h3, overwrite = TRUE)
      
      dbGetQuery(con_w, paste0("
        SELECT pts.CWNS_ID, p.ll_uuid
        FROM read_parquet('", PARQUET_DIR, "/state=", s, "/*.parquet') p
        INNER JOIN pts_h3 ON p.h3_index_9 = pts_h3.h3_search
        INNER JOIN pts    ON pts_h3.CWNS_ID = pts.CWNS_ID
        WHERE ST_Intersects(
          ST_GeomFromWKB(p.wkb_geometry),
          ST_GeomFromWKB(pts.geom_wkb)
        )
      "))
    }, error = function(e) {
      cat("Parcel lookup failed for state", s, ":", conditionMessage(e), "\n")
      NULL
    })
    
    dbDisconnect(con_w, shutdown = TRUE)
    result
  }
) %>%
  distinct(CWNS_ID, .keep_all = TRUE)

log_status("   Reported parcels found:", nrow(reported_parcels))
log_status("   Plants with no parcel:", nrow(plant_features) - nrow(reported_parcels))

# Pull parcel features for reported parcels
con <- dbConnect(duckdb::duckdb(), dbdir = DB_PATH, read_only = TRUE)
reported_parcel_features <- dbGetQuery(con, paste0("
  SELECT * FROM parcel_features
  WHERE ll_uuid IN (", paste0("'", reported_parcels$ll_uuid, "'", collapse = ", "), ")
"))
dbDisconnect(con, shutdown = TRUE)

# Build Stage 1 input
stage1_input <- reported_parcels %>%
  left_join(plant_features, by = "CWNS_ID") %>%
  left_join(reported_parcel_features %>% select(-state), by = "ll_uuid") %>%
  add_name_matching(owner_keywords, ww_keywords) %>%
  prep_features()

log_status("   Stage 1 input rows:", nrow(stage1_input))

# Score with Stage 1 model
stage1_preds <- predict(stage1_model, stage1_input, type = "prob") %>%
  bind_cols(stage1_input %>% select(CWNS_ID)) %>%
  rename(stage1_prob_correct   = .pred_Correct,
         stage1_prob_incorrect = .pred_Incorrect)

# Flag plants for Stage 2
plants_no_parcel <- plant_features %>%
  filter(!CWNS_ID %in% reported_parcels$CWNS_ID) %>%
  mutate(
    stage1_prob_correct   = NA_real_,
    stage1_prob_incorrect = NA_real_,
    no_parcel_found       = TRUE,
    trigger_reason        = "no_parcel"
  )

stage1_results <- stage1_preds %>%
  mutate(
    no_parcel_found = FALSE,
    trigger_reason  = case_when(
      stage1_prob_correct < STAGE1_THRESHOLD ~ "low_confidence",
      TRUE                                   ~ "none"
    )
  )

all_stage1 <- bind_rows(
  stage1_results,
  plants_no_parcel %>% select(CWNS_ID, stage1_prob_correct,
                              stage1_prob_incorrect, no_parcel_found,
                              trigger_reason)
)

stage2_cwns <- all_stage1 %>%
  filter(trigger_reason != "none") %>%
  pull(CWNS_ID)

log_status("   Plants scored by Stage 1:", nrow(stage1_results))
log_status("   Plants with no parcel:", nrow(plants_no_parcel))
log_status("   Plants flagged for Stage 2:", length(stage2_cwns))
log_status("   Trigger — low confidence:",
           sum(all_stage1$trigger_reason == "low_confidence", na.rm = TRUE))
log_status("   Trigger — no parcel:",
           sum(all_stage1$trigger_reason == "no_parcel", na.rm = TRUE))

# Write Stage 1 results
arrow::write_parquet(all_stage1,
                     file.path(OUT_DIR, "stage1_results.parquet"))
log_status("   Written: stage1_results.parquet")

# Write final results with reported parcel UUIDs
final_results <- all_stage1 %>%
  left_join(
    reported_parcels %>% rename(reported_ll_uuid = ll_uuid),
    by = "CWNS_ID"
  )

arrow::write_parquet(final_results,
                     file.path(OUT_DIR, "final_results.parquet"))
log_status("   Written: final_results.parquet")

# ═══════════════════════════════════════════════════════════════════════════════
# EXPORT PER-STATE STAGE 2 PLANT LISTS
# ═══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("Exporting Stage 2 plant lists by state...")
log_status("========================================")

stage2_state_dir <- file.path(OUT_DIR, "stage2_by_state")
dir.create(stage2_state_dir, recursive = TRUE, showWarnings = FALSE)

stage2_plants <- plant_features %>%
  filter(CWNS_ID %in% stage2_cwns)

walk(unique(stage2_plants$STATE_CODE), function(s) {
  state_cwns <- stage2_plants %>%
    filter(STATE_CODE == s) %>%
    select(CWNS_ID, STATE_CODE, h3_res9, LATITUDE, LONGITUDE)
  
  write_csv(state_cwns,
            file.path(stage2_state_dir, paste0("stage2_plants_", s, ".csv")))
})

log_status("   Stage 2 plant CSVs written by state:",
           length(unique(stage2_plants$STATE_CODE)), "states")

log_status("========================================")
log_status("Stage 1 inference complete")
log_status("Total plants processed:      ", nrow(plant_features))
log_status("Plants with reported parcel: ", nrow(reported_parcels))
log_status("Plants with no parcel:       ", nrow(plants_no_parcel))
log_status("Plants flagged for Stage 2:  ", length(stage2_cwns))
log_status("========================================")