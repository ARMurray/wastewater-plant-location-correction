
library(tidyverse)
library(arrow)
library(DBI)
library(duckdb)

log_status <- function(...) {
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
  flush.console()
}

OUT_DIR    <- "data/Location_Repair/04_inference"
STAGE2_DIR <- file.path(OUT_DIR, "stage2_by_state")
DB_PATH    <- "data/Location_Repair/02_feature_engineering/outputs/final_features.duckdb"

log_status("Combining Stage 2 state results...")

state_files <- list.files(STAGE2_DIR, pattern = "stage2_results_.*\\.parquet",
                          full.names = TRUE)
log_status("   State files found:", length(state_files))

stage2_results <- map_dfr(state_files, arrow::read_parquet)
log_status("   Total candidates:", nrow(stage2_results))
log_status("   Plants with candidates:", n_distinct(stage2_results$CWNS_ID))

arrow::write_parquet(stage2_results, file.path(OUT_DIR, "stage2_results.parquet"))
log_status("   Written: stage2_results.parquet")

# Load Stage 1 for final results
stage1_results <- arrow::read_parquet(file.path(OUT_DIR, "stage1_results.parquet"))
final_results  <- arrow::read_parquet(file.path(OUT_DIR, "final_results.parquet"))

# Write to DuckDB
con_write <- dbConnect(duckdb::duckdb(), dbdir = DB_PATH)
dbExecute(con_write, "LOAD spatial;")
dbBegin(con_write)
tryCatch({
  dbWriteTable(con_write, "stage1_results",    stage1_results,  overwrite = TRUE)
  dbWriteTable(con_write, "stage2_results",    stage2_results,  overwrite = TRUE)
  dbWriteTable(con_write, "inference_results", final_results,   overwrite = TRUE)
  
  dbExecute(con_write, "DROP INDEX IF EXISTS ir_cwns;")
  dbExecute(con_write, "DROP INDEX IF EXISTS s2_cwns;")
  dbExecute(con_write, "DROP INDEX IF EXISTS s2_uuid;")
  
  dbExecute(con_write, "CREATE INDEX ir_cwns ON inference_results (CWNS_ID);")
  dbExecute(con_write, "CREATE INDEX s2_cwns ON stage2_results (CWNS_ID);")
  dbExecute(con_write, "CREATE INDEX s2_uuid ON stage2_results (ll_uuid);")
  
  dbCommit(con_write)
  log_status("   Results written to DuckDB")
}, error = function(e) {
  dbRollback(con_write)
  stop(e)
})
dbDisconnect(con_write, shutdown = TRUE)

log_status("========================================")
log_status("Combine complete")
log_status("   Total candidates:", nrow(stage2_results))
log_status("   Plants with candidates:", n_distinct(stage2_results$CWNS_ID))
log_status("========================================")