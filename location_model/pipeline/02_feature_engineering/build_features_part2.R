
library(tidyverse)
library(DBI)
library(duckdb)
library(arrow)
library(stringi)
library(furrr)
library(parallelly)

# ── Logging helper ─────────────────────────────────────────────────────────────
log_status <- function(...) {
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
  flush.console()
}

# ── Command line arguments ─────────────────────────────────────────────────────
args       <- commandArgs(trailingOnly = TRUE)
STATE_CODE <- args[1]
K_RINGS    <- as.integer(args[2])

log_status("========================================")
log_status("build_features_part2_state.R started")
log_status("State:", STATE_CODE)
log_status("K rings:", K_RINGS)
log_status("Node:", Sys.getenv("SLURMD_NODENAME"))
log_status("========================================")

# ── Config ─────────────────────────────────────────────────────────────────────
N_WORKERS   <- parallelly::availableCores()
BASE_DIR <- "data/Location_Repair/02_feature_engineering"
NLCD_DIR <- "data/Location_Repair/02_feature_engineering/nlcd_outputs"
OUT_DIR     <- file.path(BASE_DIR, "outputs")
PARQUET_DIR <- "data/parcels"
OSM_PATH    <- "data/osm/Wastewater_Plants.gpkg"

STATE_OUT   <- file.path(OUT_DIR, "parcel_attrs_by_state")
dir.create(STATE_OUT, recursive = TRUE, showWarnings = FALSE)

OUT_PATH    <- file.path(STATE_OUT, paste0("parcel_attrs_", STATE_CODE, ".parquet"))

log_status("Workers:", N_WORKERS)
log_status("Output path:", OUT_PATH)

# ── Skip if already done ───────────────────────────────────────────────────────
if (file.exists(OUT_PATH)) {
  log_status("Output already exists — skipping:", OUT_PATH)
  quit(save = "no", status = 0)
}

# ── LBCS reclassification functions ───────────────────────────────────────────
reclass_activity <- function(x) {
  case_when(
    x %in% c('Activities associated with utilities (water, sewer, power, etc.)',
             'Health care, medical, or treatment',
             'Industrial, manufacturing, and waste-related',
             'Sewer-related control, monitor, or distribution',
             'Sewer treatment and processing',
             'Social, institutional, or infrastructure-related',
             'Solid waste management','Water-supply-related',
             'Water purification and filtration',
             'Water storing, pumping, or piping')                            ~ "Water Utility",
    x %in% c('Power generation, control, monitor, or distribution',
             'Power generation, storage, or processing',
             'Power transmission lines or control',
             'Storage of natural gas, fuels, etc.',
             'Telecommunications-related control, monitor, or distribution') ~ "Utility Other",
    x %in% c('Farming, tilling, plowing, harvesting, or related',
             'Livestock related','Logging',
             'Mining including surface and subsurface strip mining',
             'Pasturing, grazing, etc.')                                     ~ "Agriculture",
    x %in% c('Agricultural vacant land','Vacant parcel',
             'Vacant parcel, undevelopable','Commercial vacant land',
             'Residential vacant land','Industrial vacant land')             ~ "Vacant",
    x %in% c('Leisure','Boating, sailing, etc.','Camping','Golf',
             'Sailing, boating, and other port, marine and water-based',
             'Promenading and other activities in parks',
             'Passive leisure activity','Gatherings at fairs and exhibitions',
             'Hockey, ice skating, etc.','Skiing, snowboarding, etc.')       ~ "Leisure",
    x %in% c('Movies, concerts, or entertainment shows',
             'Goods-oriented shopping','Service-oriented shopping',
             'Shopping','Shopping, business, or trade',
             'Restaurant-type activity',
             'Restaurant-type activity with drive-through')                  ~ "Commercial",
    x %in% c('Household','Residential')                                      ~ "Household",
    x == 'Unknown'                                                           ~ "Unknown",
    is.na(x)                                                                 ~ "Unknown",
    TRUE                                                                     ~ "Other"
  )
}

reclass_function <- function(x) {
  case_when(
    x %in% c('Waste treatment and disposal',
             'Utilities and utility services',
             'Transportation, communication, information, and utilities',
             'Sewer, solid waste, and related services') ~ "Utility",
    is.na(x)                                             ~ "Unknown",
    TRUE                                                 ~ "Other"
  )
}

reclass_ownership <- function(x) {
  case_when(
    x %in% c('City, Village, Township, etc.','State government',
             'Federal government','County, Parish, Province, etc.') ~ "Government",
    x %in% c('Private persons and private joint ownership',
             'Private trusts')                                      ~ "Private",
    x %in% c('Businesses and commercial entities')                  ~ "Commercial",
    is.na(x)                                                        ~ "Other / Unknown",
    TRUE                                                            ~ "Other / Unknown"
  )
}

reclass_dominant_class <- function(x) {
  case_when(
    x == 11                ~ "Open Water",
    x %in% c(21,22,23,24) ~ "Developed",
    x %in% c(41,42,43)    ~ "Forest",
    x %in% c(81,82)       ~ "Agriculture",
    x %in% c(90,95)       ~ "Wetland",
    x %in% c(31,52,71)    ~ "Barren/Shrub/Grassland",
    is.na(x)              ~ "Unknown",
    TRUE                  ~ "Other"
  )
}

# ── Wastewater keyword pattern ─────────────────────────────────────────────────
ww_keyword_pattern <- paste0(
  "\\b(",
  paste(c("sewer", "sewage", "sewerage", "wastewater", "treatment",
          "disposal", "sanitary", "sanitation", "potw", "wwtp",
          "wwtf", "effluent", "lagoon", "digester", "biosolid", "wpcp"),
        collapse = "|"),
  ")\\b"
)

# ── 1. Load NLCD features for this state ──────────────────────────────────────
log_status("Loading NLCD features for state:", STATE_CODE)

nlcd_file <- file.path(NLCD_DIR, paste0("nlcd_", STATE_CODE, "_k", K_RINGS, ".parquet"))

if (!file.exists(nlcd_file)) {
  stop("NLCD parquet not found: ", nlcd_file)
}

nlcd_raw <- arrow::read_parquet(nlcd_file)

nlcd_features <- nlcd_raw %>%
  mutate(
    water_pixel_pct      = ifelse(total_pixels > 0, water_pixels / total_pixels, 0),
    dominant_class_group = reclass_dominant_class(dominant_class)
  ) %>%
  select(ll_uuid, h3_index_9, state,
         total_pixels, water_pixels, water_pixel_pct,
         dominant_class_group, dominant_count, has_water) %>%
  distinct(ll_uuid, .keep_all = TRUE)

log_status("   NLCD features loaded:", nrow(nlcd_features))

target_uuids <- nlcd_features$ll_uuid

# ── 2. Fetch parcel attributes in parallel chunks ──────────────────────────────
log_status("Fetching parcel attributes from raw parquet...")

uuid_chunks <- split(target_uuids, ceiling(seq_along(target_uuids) / 5000))
log_status("   UUID chunks:", length(uuid_chunks))

plan(multisession, workers = N_WORKERS)
options(future.globals.maxSize = 10 * 1024^3)

fetch_chunk <- function(uuids, state) {
  library(DBI)
  library(duckdb)

  uuid_filter <- paste0("'", uuids, "'", collapse = ", ")

  con_w <- dbConnect(duckdb::duckdb())
  dbExecute(con_w, "INSTALL spatial;")
  dbExecute(con_w, "LOAD spatial;")
  dbExecute(con_w, "SET enable_geoparquet_conversion = false;")
  on.exit(dbDisconnect(con_w, shutdown = TRUE))

  tryCatch(dbGetQuery(con_w, paste0("
    SELECT
      ll_uuid, owner, ll_gisacre, ll_bldg_count,
      lbcs_activity_desc, lbcs_function_desc,
      lbcs_structure_desc, lbcs_site_desc,
      lbcs_ownership_desc, zoning_type, zoning_subtype, geoid
    FROM read_parquet(
      'data/parcels/state=", state, "/*.parquet'
    )
    WHERE ll_uuid IN (", uuid_filter, ")
  ")), error = function(e) {
    cat("Chunk fetch failed:", conditionMessage(e), "\n")
    NULL
  })
}

t1 <- system.time({
  attr_list    <- future_map(
    uuid_chunks,
    ~ fetch_chunk(.x, STATE_CODE),
    .options = furrr_options(seed = TRUE)
  )
  parcel_attrs_raw <- bind_rows(attr_list)
})

log_status("   Raw attributes fetched:", nrow(parcel_attrs_raw),
           "in", round(t1["elapsed"] / 60, 1), "minutes")

rm(attr_list)
gc()

# ── 3. Feature engineering ─────────────────────────────────────────────────────
log_status("Engineering parcel features...")

parcel_attrs <- parcel_attrs_raw %>%
  mutate(
    # LBCS reclassification
    lbcs_activity  = reclass_activity(lbcs_activity_desc),
    lbcs_function  = reclass_function(lbcs_function_desc),
    lbcs_structure = replace_na(lbcs_structure_desc, "Unknown"),
    lbcs_site      = replace_na(lbcs_site_desc,      "Unknown"),
    lbcs_ownership = reclass_ownership(lbcs_ownership_desc),
    zoning_type    = replace_na(zoning_type,          "unknown"),
    # Size features
    log_gisacre    = log1p(replace_na(ll_gisacre, 0)),
    bldg_per_acre  = case_when(
      is.na(ll_gisacre) | ll_gisacre == 0 ~ NA_real_,
      TRUE ~ replace_na(ll_bldg_count, 0) / ll_gisacre
    ),
    # Cross-field wastewater keyword boolean
    has_ww_keyword = stri_detect_regex(
      paste(
        replace_na(zoning_type,         ""),
        replace_na(zoning_subtype,       ""),
        replace_na(owner,                ""),
        replace_na(lbcs_ownership_desc,  ""),
        replace_na(lbcs_activity_desc,   ""),
        replace_na(lbcs_function_desc,   ""),
        replace_na(lbcs_structure_desc,  ""),
        replace_na(lbcs_site_desc,       ""),
        sep = " "
      ),
      ww_keyword_pattern,
      case_insensitive = TRUE
    ),
    has_ww_keyword = replace_na(has_ww_keyword, FALSE),
    # Owner boolean features
    owner_clean        = str_squish(tolower(replace_na(owner, ""))),
    owner_is_person    = str_detect(owner_clean,
      "\\b(etux|sfr|borrower|surv)\\b") |
      str_detect(owner_clean, "^[a-z]+ [a-z]+$"),
    owner_is_govt      = str_detect(owner_clean,
      "\\b(city|county|municipality|municipal|district|authority|auth|
       township|borough|state|federal|dept|department|board|
       commission|muni|pub|wtr|commonw|commwlth)\\b"),
    owner_is_utility   = str_detect(owner_clean,
      "\\b(water|sewer|wastewater|utility|utilities|sanitary|
       sanitation|treatment|wwtp|wsd|msd|puc|pwd|mwrd|
       wpcp|potw|sewerage|wtf|wwtf)\\b"),
    owner_is_electric  = str_detect(owner_clean,
      "\\b(electric|elec|power|energy|entergy|grid|
       verizon|telecom|telephone|tel|tele|hydropower|
       centerpoint|oncor|niagara|nyseg|xcel)\\b"),
    owner_has_llc_corp = str_detect(owner_clean,
      "\\b(llc|inc|corp|ltd|lp|company|association|trust|
       partners|partnership|holdings|ventures|properties|
       realty|development|developer|builders|homes|
       lennar|pulte|horton|forestar)\\b"),
    across(c(owner_is_person, owner_is_govt, owner_is_utility,
             owner_is_electric, owner_has_llc_corp, has_ww_keyword),
           ~ replace_na(.x, FALSE))
  ) %>%
  select(ll_uuid, geoid, owner, owner_clean,
         ll_gisacre, log_gisacre, ll_bldg_count, bldg_per_acre,
         lbcs_activity, lbcs_function, lbcs_structure,
         lbcs_site, lbcs_ownership, zoning_type,
         owner_is_person, owner_is_govt, owner_is_utility,
         owner_is_electric, owner_has_llc_corp,
         has_ww_keyword)

log_status("   Parcel attrs engineered:", nrow(parcel_attrs))
log_status("   has_ww_keyword TRUE:", sum(parcel_attrs$has_ww_keyword))

rm(parcel_attrs_raw)
gc()

# ── 4. OSM features ───────────────────────────────────────────────────────────
log_status("Computing OSM wastewater features...")

osm_pts <- sf::st_read(OSM_PATH, layer = "Points", quiet = TRUE) %>%
  sf::st_transform(4326) %>%
  mutate(h3_index = h3::geo_to_h3(
    cbind(sf::st_coordinates(geom)[,2],
          sf::st_coordinates(geom)[,1]), res = 9
  ))

osm_upload <- osm_pts %>%
  mutate(geom_wkb = blob::as_blob(unclass(sf::st_as_binary(geom)))) %>%
  sf::st_drop_geometry() %>%
  select(h3_index, geom_wkb)

con_osm <- dbConnect(duckdb::duckdb())
dbExecute(con_osm, "LOAD spatial;")
dbExecute(con_osm, "SET enable_geoparquet_conversion = false;")
dbWriteTable(con_osm, "osm_points", osm_upload, overwrite = TRUE)

osm_parcel_join <- tryCatch(dbGetQuery(con_osm, paste0("
  SELECT DISTINCT p.ll_uuid
  FROM read_parquet(
    'data/parcels/state=", STATE_CODE, "/*.parquet'
  ) p
  INNER JOIN osm_points o ON p.h3_index_9 = o.h3_index
  WHERE ST_Intersects(
    ST_GeomFromWKB(p.wkb_geometry),
    ST_GeomFromWKB(o.geom_wkb)
  )
")), error = function(e) {
  cat("OSM join failed:", conditionMessage(e), "\n")
  data.frame(ll_uuid = character(0))
})

dbDisconnect(con_osm, shutdown = TRUE)

osm_features <- nlcd_features %>%
  select(ll_uuid) %>%
  mutate(osm_ww = ll_uuid %in% osm_parcel_join$ll_uuid)

log_status("   Parcels with OSM wastewater tag:", sum(osm_features$osm_ww))

rm(osm_pts, osm_upload, osm_parcel_join)
gc()

# ── 5. County data quality ────────────────────────────────────────────────────
log_status("Computing county data quality scores...")

county_data_quality <- parcel_attrs %>%
  group_by(geoid) %>%
  summarise(
    n_parcels               = n(),
    pct_lbcs_activity_known = mean(lbcs_activity  != "Unknown"),
    pct_lbcs_function_known = mean(lbcs_function  != "Unknown"),
    pct_lbcs_owner_known    = mean(lbcs_ownership != "Other / Unknown"),
    pct_owner_known         = mean(nchar(owner_clean) > 0),
    pct_zoning_known        = mean(zoning_type != "unknown"),
    data_quality_score      = (pct_lbcs_activity_known +
                                 pct_lbcs_owner_known +
                                 pct_owner_known +
                                 pct_zoning_known) / 4,
    .groups = "drop"
  )

log_status("   County data quality computed for", nrow(county_data_quality),
           "counties")

# ── 6. Combine and write ───────────────────────────────────────────────────────
log_status("Combining parcel features...")

parcel_features_state <- nlcd_features %>%
  left_join(parcel_attrs   %>% select(-owner_clean), by = "ll_uuid") %>%
  left_join(osm_features,                            by = "ll_uuid") %>%
  left_join(county_data_quality,                     by = "geoid") %>%
  select(
    # Keys
    ll_uuid, h3_index_9, state, geoid,
    # NLCD
    total_pixels, water_pixels, water_pixel_pct,
    dominant_class_group, dominant_count, has_water,
    # Parcel size
    ll_gisacre, log_gisacre, ll_bldg_count, bldg_per_acre,
    # LBCS
    lbcs_activity, lbcs_function, lbcs_structure,
    lbcs_site, lbcs_ownership, zoning_type,
    # Owner
    owner, owner_is_person, owner_is_govt,
    owner_is_utility, owner_is_electric, owner_has_llc_corp,
    # Wastewater keyword flag
    has_ww_keyword,
    # OSM
    osm_ww,
    # County data quality
    data_quality_score, n_parcels,
    pct_lbcs_activity_known, pct_owner_known
  )

log_status("   Combined rows:", nrow(parcel_features_state))
log_status("   Columns:", ncol(parcel_features_state))

arrow::write_parquet(parcel_features_state, OUT_PATH)
log_status("   Written:", OUT_PATH)

# ── Done ───────────────────────────────────────────────────────────────────────
log_status("========================================")
log_status("Job complete for state:", STATE_CODE)
log_status("========================================")
