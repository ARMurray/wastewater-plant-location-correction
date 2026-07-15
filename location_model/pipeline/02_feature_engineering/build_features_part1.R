
library(tidyverse)
library(sf)
library(DBI)
library(duckdb)
library(h3)
library(terra)
library(exactextractr)
library(furrr)
library(parallelly)
library(arrow)
library(vroom)
library(stringi)
library(blob)

SKIP_PART1 <- TRUE   # plant features already built
SKIP_PART2 <- FALSE   # parcel features already built

# ── Logging helper ────────────────────────────────────────────────────────────
log_status <- function(...) {
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
  flush.console()
}

# ── Config ────────────────────────────────────────────────────────────────────
N_WORKERS   <- parallelly::availableCores()
BASE_DIR <- "data/Location_Repair/02_feature_engineering"
CWNS_DIR    <- file.path(BASE_DIR, "CWNS_files")
OUT_DIR     <- file.path(BASE_DIR, "outputs")
PARQUET_DIR <- "data/parcels"
NLCD_PATH   <- "data/nlcd/Annual_NLCD_LndCov_2023_CU_C1V1.tif"
CENSUS_GDB  <- "data/Census/tlgdb_2022_a_us_substategeo.gdb"
OSM_PATH    <- "data/osm/Wastewater_Plants.gpkg"
GPKG_PATH   <- file.path(BASE_DIR, "training_locations.gpkg")
DB_PATH     <- file.path(OUT_DIR, "final_features.duckdb")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

log_status("========================================")
log_status("build_features.R started")
log_status("Workers:", N_WORKERS)
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

# ── LBCS reclassification functions ──────────────────────────────────────────
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
             'Water storing, pumping, or piping')                          ~ "Water Utility",
    x %in% c('Power generation, control, monitor, or distribution',
             'Power generation, storage, or processing',
             'Power transmission lines or control',
             'Storage of natural gas, fuels, etc.',
             'Telecommunications-related control, monitor, or distribution') ~ "Utility Other",
    x %in% c('Farming, tilling, plowing, harvesting, or related',
             'Livestock related','Logging',
             'Mining including surface and subsurface strip mining',
             'Pasturing, grazing, etc.')                                   ~ "Agriculture",
    x %in% c('Agricultural vacant land','Vacant parcel',
             'Vacant parcel, undevelopable','Commercial vacant land',
             'Residential vacant land','Industrial vacant land')           ~ "Vacant",
    x %in% c('Leisure','Boating, sailing, etc.','Camping','Golf',
             'Sailing, boating, and other port, marine and water-based',
             'Promenading and other activities in parks',
             'Passive leisure activity','Gatherings at fairs and exhibitions',
             'Hockey, ice skating, etc.','Skiing, snowboarding, etc.')     ~ "Leisure",
    x %in% c('Movies, concerts, or entertainment shows',
             'Goods-oriented shopping','Service-oriented shopping',
             'Shopping','Shopping, business, or trade',
             'Restaurant-type activity',
             'Restaurant-type activity with drive-through')                ~ "Commercial",
    x %in% c('Household','Residential')                                    ~ "Household",
    x == 'Unknown'                                                         ~ "Unknown",
    is.na(x)                                                               ~ "Unknown",
    TRUE                                                                   ~ "Other"
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

# ═══════════════════════════════════════════════════════════════════════════════
# PART 1: PLANT FEATURES
# ═══════════════════════════════════════════════════════════════════════════════

if (!SKIP_PART1) {
  log_status("========================================")
  log_status("PART 1: Building plant features")
  log_status("========================================")
  
  # ── 01: Plant base ────────────────────────────────────────────────────────────
  log_status("01: Loading plant base data...")
  
  # Filter to treatment plants only using FACILITY_TYPES.txt
  treatment_plant_ids <- vroom(
    file.path(CWNS_DIR, "FACILITY_TYPES.txt"),
    show_col_types = FALSE
  ) %>%
    filter(FACILITY_TYPE == "Treatment Plant") %>%
    distinct(CWNS_ID) %>%
    pull(CWNS_ID)
  
  log_status("   Treatment plant CWNS IDs:", length(treatment_plant_ids))
  
  plant_base <- vroom(
    file.path(CWNS_DIR, "PHYSICAL_LOCATION.txt"),
    show_col_types = FALSE
  ) %>%
    filter(
      CWNS_ID %in% treatment_plant_ids,
      !is.na(LATITUDE),
      !is.na(LONGITUDE)
    ) %>%
    select(CWNS_ID, STATE_CODE, LATITUDE, LONGITUDE) %>%
    distinct(CWNS_ID, .keep_all = TRUE) %>%
    rowwise() %>%
    mutate(h3_res9 = h3::geo_to_h3(c(LATITUDE, LONGITUDE), res = 9)) %>%
    ungroup()
  
  stopifnot(n_distinct(plant_base$h3_res9) > 1)
  log_status("   Plants loaded:", nrow(plant_base))
  
  arrow::write_parquet(plant_base, file.path(OUT_DIR, "01_plant_base.parquet"))
  log_status("   Written: 01_plant_base.parquet")
  
  # ── 02: Discharge features ────────────────────────────────────────────────────
  log_status("02: Building discharge features...")
  
  discharge_features <- vroom(
    file.path(CWNS_DIR, "DISCHARGES.csv"),
    col_names = c("CWNS_ID","FACILITY_ID","STATE_CODE","DISCHARGE_TYPE",
                  "PRESENT_DISCHARGE_PERCENTAGE","PROJECTED_DISCHARGE_PERCENTAGE",
                  "DISCHARGES_TO"),
    show_col_types = FALSE
  ) %>%
    filter(CWNS_ID %in% treatment_plant_ids) %>%
    select(CWNS_ID, DISCHARGE_TYPE, PRESENT_DISCHARGE_PERCENTAGE) %>%
    group_by(CWNS_ID) %>%
    summarise(
      surface_water_discharge = any(DISCHARGE_TYPE == "Outfall To Surface Waters"),
      surface_water_pct       = sum(PRESENT_DISCHARGE_PERCENTAGE[
        DISCHARGE_TYPE == "Outfall To Surface Waters"],
        na.rm = TRUE),
      ocean_discharge         = any(DISCHARGE_TYPE == "Ocean Discharge"),
      cso_discharge           = any(DISCHARGE_TYPE == "CSO Discharge"),
      requires_npdes          = any(DISCHARGE_TYPE %in% c(
        "Outfall To Surface Waters",
        "Ocean Discharge",
        "CSO Discharge",
        "Overland Flow With Discharge")),
      any_reuse               = any(str_starts(DISCHARGE_TYPE, "Reuse:")),
      n_discharge_types       = n_distinct(DISCHARGE_TYPE),
      .groups = "drop"
    )
  
  log_status("   Discharge features built:", nrow(discharge_features))
  arrow::write_parquet(discharge_features,
                       file.path(OUT_DIR, "02_discharge_features.parquet"))
  log_status("   Written: 02_discharge_features.parquet")
  
  # ── 03: Population features ───────────────────────────────────────────────────
  log_status("03: Building population features...")
  
  population_features <- vroom(
    file.path(CWNS_DIR, "POPULATION_WASTEWATER.txt"),
    show_col_types = FALSE
  ) %>%
    filter(CWNS_ID %in% treatment_plant_ids) %>%
    select(CWNS_ID, TOTAL_RES_POPULATION_2022) %>%
    rename(pop_served = TOTAL_RES_POPULATION_2022) %>%
    distinct(CWNS_ID, .keep_all = TRUE)
  
  log_status("   Population features built:", nrow(population_features))
  arrow::write_parquet(population_features,
                       file.path(OUT_DIR, "03_population_features.parquet"))
  log_status("   Written: 03_population_features.parquet")
  
  # ── 04: Census features ───────────────────────────────────────────────────────
  log_status("04: Building census features...")
  
  plants_sf <- plant_base %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269) %>%
    st_transform(5070)
  
  clean_geo <- function(x, remove_terms) {
    x %>%
      tolower() %>%
      str_remove_all(remove_terms) %>%
      str_squish()
  }
  
  sd_remove     <- "cdp|municipio|municipality|parish|city|st\\.|borough|district|census area|town|village|ccd|township|barrio|precinct|census subarea"
  place_remove  <- "cdp|municipio|municipality|parish|city|st\\.|borough|district|census area|town|village"
  county_remove <- "county|municipio|municipality|parish|city|st\\.|borough|district|census area"
  
  log_status("   Loading census geographies...")
  subdivisions <- st_read(CENSUS_GDB, layer = "County_Subdivision", quiet = TRUE) %>%
    select(NAMELSAD) %>%
    st_transform(5070) %>%
    st_make_valid()
  
  places_inc <- st_read(CENSUS_GDB, layer = "Incorporated_Place", quiet = TRUE) %>%
    select(NAMELSAD) %>%
    st_transform(5070)
  
  places_cdp <- st_read(CENSUS_GDB, layer = "Census_Designated_Place", quiet = TRUE) %>%
    select(NAMELSAD) %>%
    st_transform(5070)
  
  places <- rbind(places_inc, places_cdp) %>% st_make_valid()
  
  counties <- st_read(CENSUS_GDB, layer = "County", quiet = TRUE) %>%
    select(NAMELSAD, GEOID) %>%
    st_transform(5070) %>%
    st_make_valid()
  
  log_status("   Running census spatial joins...")
  
  sd_join <- st_intersection(plants_sf, subdivisions) %>%
    st_drop_geometry() %>%
    select(CWNS_ID, NAMELSAD) %>%
    mutate(subdivision = clean_geo(NAMELSAD, sd_remove)) %>%
    select(CWNS_ID, subdivision)
  
  place_join <- st_intersection(plants_sf, places) %>%
    st_drop_geometry() %>%
    select(CWNS_ID, NAMELSAD) %>%
    mutate(place = clean_geo(NAMELSAD, place_remove)) %>%
    select(CWNS_ID, place)
  
  county_join <- st_intersection(plants_sf, counties) %>%
    st_drop_geometry() %>%
    select(CWNS_ID, NAMELSAD, GEOID) %>%
    mutate(county = clean_geo(NAMELSAD, county_remove)) %>%
    select(CWNS_ID, county, county_geoid = GEOID)
  
  # Pull census populations for ratio features
  log_status("   Loading census population data...")
  # Note: requires ACS or decennial census population by county/place
  # Using county GEOID as join key for population data
  # If you have a census population file, join it here
  # For now we store the GEOID for future population join
  
  census_features <- plant_base %>%
    select(CWNS_ID) %>%
    left_join(sd_join,     by = "CWNS_ID") %>%
    left_join(place_join,  by = "CWNS_ID") %>%
    left_join(county_join, by = "CWNS_ID") %>%
    mutate(is_rural = is.na(place) | nchar(place) == 0)
  
  log_status("   Census features built:", nrow(census_features))
  log_status("   Rural plants:", sum(census_features$is_rural))
  arrow::write_parquet(census_features,
                       file.path(OUT_DIR, "04_census_features.parquet"))
  log_status("   Written: 04_census_features.parquet")
  
  # ── 05: Combine plant features ────────────────────────────────────────────────
  log_status("05: Combining plant features...")
  
  plant_features <- plant_base %>%
    left_join(discharge_features,  by = "CWNS_ID") %>%
    left_join(population_features, by = "CWNS_ID") %>%
    left_join(census_features %>%
                select(CWNS_ID, subdivision, place, county,
                       county_geoid, is_rural),
              by = "CWNS_ID")
  
  log_status("   Plant features combined:", nrow(plant_features))
  log_status("   Columns:", ncol(plant_features))
  arrow::write_parquet(plant_features,
                       file.path(OUT_DIR, "05_plant_features.parquet"))
  log_status("   Written: 05_plant_features.parquet")
  
  # Free memory before parcel processing
  rm(plants_sf, subdivisions, places, places_inc, places_cdp, counties,
     sd_join, place_join, county_join, discharge_features,
     population_features, census_features)
  gc()
  log_status("   Memory freed")
}




log_status("========================================")
log_status("build_features_part1.R complete")
log_status("Outputs in:", OUT_DIR)
log_status("========================================")