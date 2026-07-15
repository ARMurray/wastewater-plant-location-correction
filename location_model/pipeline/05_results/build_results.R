
library(tidyverse)
library(sf)
library(arrow)
library(DBI)
library(duckdb)

# ── Logging ───────────────────────────────────────────────────────────────────
log_status <- function(...) {
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
  flush.console()
}

# ══════════════════════════════════════════════════════════════════════════════
# CONFIG — adjust these before running
# ══════════════════════════════════════════════════════════════════════════════
INFERENCE_DIR <- "data/Location_Repair/04_inference"
PLANTS_PATH   <- "data/Location_Repair/02_feature_engineering/CWNS_files/PHYSICAL_LOCATION.txt"
FEATURE_DIR   <- "data/Location_Repair/02_feature_engineering/outputs"
OUT_DIR       <- "data/Location_Repair/05_results/results_download"
PARQUET_DIR    <- "data/parcels"

# Geopackage parameters
MAX_PLANTS         <- 500    # max treatment plants to include in geopackage
MAX_CANDIDATES     <- 10     # max candidate parcels per plant
MIN_STAGE2_PROB    <- 0.5    # minimum Stage 2 probability to include a candidate

# Stratified sample sizes per tier
N_TIER1  <- 200   # high confidence, within 5km
N_TIER2  <- 200   # high confidence, beyond 5km or no_parcel trigger
N_TIER3  <- 100   # spot check — trigger_reason = "none"

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

log_status("========================================")
log_status("Building results download package")
log_status("MAX_PLANTS:", MAX_PLANTS)
log_status("MAX_CANDIDATES:", MAX_CANDIDATES)
log_status("MIN_STAGE2_PROB:", MIN_STAGE2_PROB)
log_status("========================================")

# ══════════════════════════════════════════════════════════════════════════════
# PART 1: Copy parquet results files
# ══════════════════════════════════════════════════════════════════════════════
log_status("Copying parquet results files...")

parquet_out <- file.path(OUT_DIR, "parquet")
dir.create(parquet_out, recursive = TRUE, showWarnings = FALSE)

file.copy(
  file.path(INFERENCE_DIR, "stage1_results.parquet"),
  file.path(parquet_out,   "stage1_results.parquet"),
  overwrite = TRUE
)
file.copy(
  file.path(INFERENCE_DIR, "stage2_results.parquet"),
  file.path(parquet_out,   "stage2_results.parquet"),
  overwrite = TRUE
)

log_status("   Parquet files copied")

# ══════════════════════════════════════════════════════════════════════════════
# PART 2: Load results
# ══════════════════════════════════════════════════════════════════════════════
log_status("Loading results...")

s1 <- read_parquet(file.path(INFERENCE_DIR, "stage1_results.parquet"))
s2 <- read_parquet(file.path(INFERENCE_DIR, "stage2_results.parquet"))
final <- read_parquet(file.path(INFERENCE_DIR, "final_results.parquet"))

# Load plant coordinates
plants_coords <- read_csv(PLANTS_PATH, show_col_types = FALSE) %>%
  select(CWNS_ID, FACILITY_NAME = contains("NAME"),
         LATITUDE, LONGITUDE, STATE_CODE) %>%
  drop_na(LATITUDE, LONGITUDE)

# Load plant features for context
plant_features <- read_parquet(file.path(FEATURE_DIR, "05_plant_features.parquet")) %>%
  select(CWNS_ID, pop_served, requires_npdes, surface_water_discharge,
         subdivision, place, county, is_rural)

log_status("   Stage 1 results:", nrow(s1), "plants")
log_status("   Stage 2 results:", nrow(s2), "candidates")

# ══════════════════════════════════════════════════════════════════════════════
# PLANNED ADDITION 1: Exclude reported parcels from Stage 2 candidates
# ══════════════════════════════════════════════════════════════════════════════
log_status("Excluding reported parcels from Stage 2 candidates...")

reported_uuids <- final %>%
  filter(!is.na(reported_ll_uuid)) %>%
  pull(reported_ll_uuid) %>%
  unique()

s2 <- s2 %>%
  filter(!ll_uuid %in% reported_uuids)

log_status("   Reported UUIDs excluded:", length(reported_uuids))
log_status("   Stage 2 candidates remaining:", nrow(s2))


# ══════════════════════════════════════════════════════════════════════════════
# PART 3: Assign tiers and build summary
# ══════════════════════════════════════════════════════════════════════════════
log_status("Assigning tiers...")

# Top Stage 2 candidate per plant
top_s2 <- s2 %>%
  filter(stage2_prob_correct >= MIN_STAGE2_PROB) %>%
  # Resolve competition: for each parcel, keep only the plant with highest score
  group_by(ll_uuid) %>%
  slice_max(stage2_prob_correct, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # Then take top candidate per plant
  group_by(CWNS_ID) %>%
  slice_max(stage2_prob_correct, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(CWNS_ID, top_stage2_prob = stage2_prob_correct,
         top_ll_uuid = ll_uuid, top_distance_m = distance_m,
         within_1km, within_5km)

plant_summary <- s1 %>%
  left_join(top_s2,         by = "CWNS_ID") %>%
  left_join(plants_coords,  by = "CWNS_ID") %>%
  left_join(plant_features, by = "CWNS_ID") %>%
  mutate(
    tier = case_when(
      trigger_reason == "none"                                          ~ "Tier3_likely_correct",
      trigger_reason == "no_parcel"  & top_stage2_prob >= MIN_STAGE2_PROB ~ "Tier2_no_parcel_corrected",
      trigger_reason == "no_parcel"  & (is.na(top_stage2_prob) | top_stage2_prob < MIN_STAGE2_PROB) ~ "Tier2_no_parcel_uncorrected",
      trigger_reason == "low_confidence" & within_5km == 1             ~ "Tier1_nearby_correction",
      trigger_reason == "low_confidence" & within_5km == 0             ~ "Tier1_distant_correction",
      trigger_reason == "low_confidence" & is.na(within_5km)           ~ "Tier1_no_candidate",
      TRUE                                                              ~ "Other"
    )
  )

log_status("   Tier distribution:")
print(count(plant_summary, tier, sort = TRUE))

# Save full summary
write_csv(plant_summary,
          file.path(OUT_DIR, "plant_summary.csv"))
log_status("   Written: plant_summary.csv")

# ══════════════════════════════════════════════════════════════════════════════
# PART 4: Stratified sample for geopackage
# ══════════════════════════════════════════════════════════════════════════════
log_status("Building stratified sample...")

set.seed(42)

tier1_sample <- plant_summary %>%
  filter(tier %in% c("Tier1_nearby_correction", "Tier1_distant_correction")) %>%
  drop_na(LATITUDE, LONGITUDE, top_stage2_prob) %>%
  slice_sample(n = min(N_TIER1, nrow(.)))

tier2_sample <- plant_summary %>%
  filter(tier %in% c("Tier2_no_parcel_corrected")) %>%
  drop_na(LATITUDE, LONGITUDE, top_stage2_prob) %>%
  slice_sample(n = min(N_TIER2, nrow(.)))

tier3_sample <- plant_summary %>%
  filter(tier == "Tier3_likely_correct") %>%
  drop_na(LATITUDE, LONGITUDE) %>%
  slice_sample(n = min(N_TIER3, nrow(.)))

gpkg_plants <- bind_rows(tier1_sample, tier2_sample, tier3_sample) %>%
  slice_sample(n = min(MAX_PLANTS, nrow(.)))

log_status("   Sample plants:", nrow(gpkg_plants))
log_status("   Tier 1:", sum(str_starts(gpkg_plants$tier, "Tier1")))
log_status("   Tier 2:", sum(str_starts(gpkg_plants$tier, "Tier2")))
log_status("   Tier 3:", sum(str_starts(gpkg_plants$tier, "Tier3")))

# ══════════════════════════════════════════════════════════════════════════════
# PART 5: Pull candidate parcel centroids from parquet
# ══════════════════════════════════════════════════════════════════════════════
log_status("Pulling candidate parcel centroids...")

# Get top N candidates per sampled plant
sample_cwns <- gpkg_plants$CWNS_ID

candidates_sample <- s2 %>%
  filter(
    CWNS_ID %in% sample_cwns,
    stage2_prob_correct >= MIN_STAGE2_PROB
  ) %>%
  group_by(CWNS_ID) %>%
  slice_max(stage2_prob_correct, n = MAX_CANDIDATES, with_ties = FALSE) %>%
  ungroup()

log_status("   Candidates to geocode:", nrow(candidates_sample))

# Pull centroids from parquet by state
target_states <- unique(candidates_sample$state)

parcel_centroids <- map_dfr(target_states, function(s) {
  uuids <- candidates_sample %>%
    filter(state == s) %>%
    pull(ll_uuid)
  
  uuid_filter <- paste0("'", uuids, "'", collapse = ", ")
  
  con_w <- dbConnect(duckdb::duckdb())
  dbExecute(con_w, "INSTALL spatial;")
  dbExecute(con_w, "LOAD spatial;")
  dbExecute(con_w, "SET enable_geoparquet_conversion = false;")
  
  result <- tryCatch(dbGetQuery(con_w, paste0("
    SELECT
      p.ll_uuid,
      ST_X(ST_Centroid(ST_GeomFromWKB(p.wkb_geometry))) AS centroid_lon,
      ST_Y(ST_Centroid(ST_GeomFromWKB(p.wkb_geometry))) AS centroid_lat
    FROM read_parquet('", PARQUET_DIR, "/state=", s, "/*.parquet') p
    WHERE p.ll_uuid IN (", uuid_filter, ")
  ")), error = function(e) {
    cat("Centroid pull failed for state", s, "\n")
    NULL
  })
  
  dbDisconnect(con_w, shutdown = TRUE)
  result
})

log_status("   Centroids retrieved:", nrow(parcel_centroids))

# Join centroids to candidates
candidates_geo <- candidates_sample %>%
  left_join(parcel_centroids, by = "ll_uuid") %>%
  drop_na(centroid_lon, centroid_lat)

# ══════════════════════════════════════════════════════════════════════════════
# PART 6: Build geopackage layers
# ══════════════════════════════════════════════════════════════════════════════
log_status("Building geopackage layers...")

GPKG_PATH <- file.path(OUT_DIR, "inference_results.gpkg")

# ── Layer 1: Reported plant locations ────────────────────────────────────────
reported_pts <- gpkg_plants %>%
  drop_na(LATITUDE, LONGITUDE) %>%
  select(CWNS_ID, tier,
         stage1_prob_correct, trigger_reason, no_parcel_found,
         top_stage2_prob, top_distance_m, within_1km, within_5km,
         pop_served, requires_npdes, surface_water_discharge,
         is_rural, subdivision, place, county,
         STATE_CODE, LATITUDE, LONGITUDE) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

st_write(reported_pts, GPKG_PATH,
         layer = "reported_locations", append = FALSE, quiet = TRUE)
log_status("   Layer 1 written: reported_locations (", nrow(reported_pts), "features)")

# ── Layer 2: Candidate parcel centroid points ─────────────────────────────────
candidate_pts <- candidates_geo %>%
  left_join(
    gpkg_plants %>% select(CWNS_ID, tier, stage1_prob_correct,
                           trigger_reason, pop_served, STATE_CODE),
    by = "CWNS_ID"
  ) %>%
  select(CWNS_ID, ll_uuid, tier, trigger_reason,
         stage1_prob_correct, stage2_prob_correct, stage2_prob_incorrect,
         distance_m, within_1km, within_5km,
         pop_served, STATE_CODE,
         centroid_lon, centroid_lat) %>%
  st_as_sf(coords = c("centroid_lon", "centroid_lat"), crs = 4326)

st_write(candidate_pts, GPKG_PATH,
         layer = "candidate_locations", append = FALSE, quiet = TRUE)
log_status("   Layer 2 written: candidate_locations (", nrow(candidate_pts), "features)")

# ── Layer 3: Correction lines ─────────────────────────────────────────────────
reported_lookup <- gpkg_plants %>%
  drop_na(LATITUDE, LONGITUDE) %>%
  select(CWNS_ID, LONGITUDE, LATITUDE)

correction_lines <- map_dfr(unique(candidates_geo$CWNS_ID), function(id) {
  plant_row  <- reported_lookup %>% filter(CWNS_ID == id)
  cands      <- candidates_geo  %>% filter(CWNS_ID == id)
  plant_meta <- gpkg_plants    %>% filter(CWNS_ID == id)
  
  if (nrow(plant_row) == 0 || nrow(cands) == 0 || nrow(plant_meta) == 0) return(NULL)
  
  map_dfr(1:nrow(cands), function(i) {
    line <- st_sfc(
      st_linestring(rbind(
        c(plant_row$LONGITUDE[1], plant_row$LATITUDE[1]),
        c(cands$centroid_lon[i],  cands$centroid_lat[i])
      )),
      crs = 4326
    )
    
    st_as_sf(data.frame(
      CWNS_ID             = id,
      ll_uuid             = cands$ll_uuid[i],
      tier                = plant_meta$tier[1],
      trigger_reason      = plant_meta$trigger_reason[1],
      stage1_prob_correct = plant_meta$stage1_prob_correct[1],
      stage2_prob_correct = cands$stage2_prob_correct[i],
      prob_delta          = cands$stage2_prob_correct[i] -
        replace_na(plant_meta$stage1_prob_correct[1], 0),
      distance_m          = cands$distance_m[i],
      within_1km          = cands$within_1km[i],
      within_5km          = cands$within_5km[i],
      pop_served          = plant_meta$pop_served[1],
      STATE_CODE          = plant_meta$STATE_CODE[1]
    ), geometry = line)
  })
})

st_write(correction_lines, GPKG_PATH,
         layer = "correction_lines", append = FALSE, quiet = TRUE)
log_status("   Layer 3 written: correction_lines (", nrow(correction_lines), "features)")

# ══════════════════════════════════════════════════════════════════════════════
# PART 7: Summary statistics CSV
# ══════════════════════════════════════════════════════════════════════════════
log_status("Writing summary statistics...")

summary_stats <- list(
  overall = tibble(
    total_plants              = nrow(s1),
    plants_with_parcel        = sum(!s1$no_parcel_found, na.rm = TRUE),
    plants_no_parcel          = sum(s1$no_parcel_found,  na.rm = TRUE),
    plants_low_confidence     = sum(s1$trigger_reason == "low_confidence", na.rm = TRUE),
    plants_likely_correct     = sum(s1$trigger_reason == "none",           na.rm = TRUE),
    plants_flagged_stage2     = sum(s1$trigger_reason != "none",           na.rm = TRUE),
    plants_with_s2_candidate  = n_distinct(s2$CWNS_ID),
    total_s2_candidates       = nrow(s2),
    s2_candidates_above_0.5   = sum(s2$stage2_prob_correct >= 0.5),
    s2_candidates_above_0.8   = sum(s2$stage2_prob_correct >= 0.8),
    s2_candidates_above_0.9   = sum(s2$stage2_prob_correct >= 0.9)
  ),
  stage1_score_distribution = s1 %>%
    filter(!is.na(stage1_prob_correct)) %>%
    summarise(
      min    = round(min(stage1_prob_correct),    3),
      q25    = round(quantile(stage1_prob_correct, 0.25), 3),
      median = round(median(stage1_prob_correct),  3),
      mean   = round(mean(stage1_prob_correct),    3),
      q75    = round(quantile(stage1_prob_correct, 0.75), 3),
      max    = round(max(stage1_prob_correct),    3)
    ),
  top_s2_score_distribution = top_s2 %>%
    summarise(
      min    = round(min(top_stage2_prob),    3),
      q25    = round(quantile(top_stage2_prob, 0.25), 3),
      median = round(median(top_stage2_prob),  3),
      mean   = round(mean(top_stage2_prob),    3),
      q75    = round(quantile(top_stage2_prob, 0.75), 3),
      max    = round(max(top_stage2_prob),    3)
    ),
  tier_summary = count(plant_summary, tier, sort = TRUE)
)

for (name in names(summary_stats)) {
  write_csv(summary_stats[[name]],
            file.path(OUT_DIR, paste0("summary_", name, ".csv")))
}

log_status("   Summary CSVs written")

# ══════════════════════════════════════════════════════════════════════════════
# PART 8: Final output manifest
# ══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("Results download package complete")
log_status("Output directory:", OUT_DIR)
log_status("")
log_status("Files created:")
log_status("  parquet/stage1_results.parquet  — Stage 1 scores for all plants")
log_status("  parquet/stage2_results.parquet  — Stage 2 candidates for flagged plants")
log_status("  plant_summary.csv               — Full plant summary with tiers")
log_status("  inference_results.gpkg          — Geopackage with 3 layers:")
log_status("    reported_locations            —", nrow(reported_pts), "plant points")
log_status("    candidate_locations           —", nrow(candidate_pts), "candidate points")
log_status("    correction_lines              —", nrow(correction_lines), "lines")
log_status("  summary_overall.csv             — Overall statistics")
log_status("  summary_stage1_score_distribution.csv")
log_status("  summary_top_s2_score_distribution.csv")
log_status("  summary_tier_summary.csv")
log_status("========================================")