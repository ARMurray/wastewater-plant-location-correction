
library(tidyverse)
library(tidymodels)
library(ranger)
library(spatialsample)
library(sf)
library(arrow)
library(butcher)
library(vip)
library(parallelly)
library(doParallel)

# ── Logging ───────────────────────────────────────────────────────────────────
log_status <- function(...) {
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
  flush.console()
}

# ── Config ────────────────────────────────────────────────────────────────────
N_WORKERS   <- parallelly::availableCores()
OUT_DIR     <- "data/Location_Repair/02_feature_engineering/outputs"
MODEL_DIR   <- "data/Location_Repair/03_models"
PLANTS_PATH <- "data/Location_Repair/02_feature_engineering/CWNS_files/PHYSICAL_LOCATION.txt"

dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)

log_status("========================================")
log_status("Stage 1 Model Training — Random Forest")
log_status("Workers:", N_WORKERS)
log_status("========================================")

# ── Load training data ────────────────────────────────────────────────────────
log_status("Loading Stage 1 training data...")
s1 <- arrow::read_parquet(file.path(OUT_DIR, "14_stage1_training.parquet"))

log_status("   Rows:", nrow(s1))
log_status("   Correct:", sum(s1$class == "Correct"))
log_status("   Incorrect:", sum(s1$class == "Incorrect"))

# ── Feature selection ─────────────────────────────────────────────────────────
drop_cols <- c("ll_uuid", "h3_index_9", "h3_res9", "state", "geoid",
               "county_geoid", "LATITUDE", "LONGITUDE", "n_parcels",
               "pct_lbcs_activity_known", "pct_owner_known",
               "subdivision", "place", "county")

# Keep CWNS_ID temporarily for spatial join
s1_model_with_id <- s1 %>%
  select(-any_of(drop_cols)) %>%
  mutate(
    class = factor(class, levels = c("Correct", "Incorrect")),
    across(where(is.character), as.factor),
    across(where(is.logical),   as.integer)
  )

log_status("   Model columns (with ID):", ncol(s1_model_with_id))

# ── Spatial cross-validation setup ────────────────────────────────────────────
log_status("Setting up cross-validation folds...")

plant_coords <- read_csv(PLANTS_PATH, show_col_types = FALSE) %>%
  select(CWNS_ID, LATITUDE, LONGITUDE) %>%
  drop_na()

# Build sf object for spatial fold creation
s1_sf <- s1_model_with_id %>%
  left_join(plant_coords, by = "CWNS_ID") %>%
  drop_na(LATITUDE, LONGITUDE) %>%
  select(-CWNS_ID) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(5070)

# Final model dataframe — no CWNS_ID, no geometry
s1_model <- s1_model_with_id %>% select(-CWNS_ID)%>%
  mutate(place_match = replace_na(place_match, 0L)) %>%
  select(-zoning_type)

log_status("   s1_sf rows:", nrow(s1_sf))
log_status("   s1_model rows:", nrow(s1_model))
log_status("   Features:", ncol(s1_model) - 1)

set.seed(123)
spatial_folds <- spatial_clustering_cv(s1_sf, v = 5)

# Strip geometry from spatial fold splits
spatial_folds$splits <- lapply(spatial_folds$splits, function(split) {
  split$data <- sf::st_drop_geometry(split$data)
  split
})

standard_folds <- vfold_cv(s1_model, v = 5, strata = class)

log_status("   Spatial folds:", nrow(spatial_folds))
log_status("   Standard folds:", nrow(standard_folds))

# Check class distribution across spatial folds
for (i in seq_along(spatial_folds$splits)) {
  fold_train <- training(spatial_folds$splits[[i]])
  cat("   Fold", i, "train — Correct:",
      sum(fold_train$class == "Correct"),
      "Incorrect:", sum(fold_train$class == "Incorrect"), "\n")
}

# ── Recipe ────────────────────────────────────────────────────────────────────
log_status("Building recipe...")

s1_recipe <- recipe(class ~ ., data = s1_model) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_median(all_integer_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# ── Class weights ─────────────────────────────────────────────────────────────
n_correct   <- sum(s1$class == "Correct")
n_incorrect <- sum(s1$class == "Incorrect")
class_weights <- c(
  Correct   = n_incorrect / nrow(s1),
  Incorrect = n_correct   / nrow(s1)
)
log_status("   Class weights — Correct:", round(class_weights["Correct"], 3),
           "| Incorrect:", round(class_weights["Incorrect"], 3))

# ── Model spec ────────────────────────────────────────────────────────────────
rf_spec <- rand_forest(
  mtry  = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger",
             importance    = "permutation",
             class.weights = class_weights,
             num.threads   = N_WORKERS)

rf_workflow <- workflow() %>%
  add_recipe(s1_recipe) %>%
  add_model(rf_spec)

rf_grid <- grid_latin_hypercube(
  mtry(range = c(1, ncol(s1_model) - 1)),
  min_n(),
  size = 20
)

metrics <- metric_set(roc_auc, accuracy, sensitivity, specificity, brier_class)

# ── Register parallel backend ─────────────────────────────────────────────────
# Register parallel backend and export N_WORKERS to workers
cl <- makePSOCKcluster(N_WORKERS)
clusterExport(cl, ls())
registerDoParallel(cl)

# ═══════════════════════════════════════════════════════════════════════════════
# SPATIAL CV
# ═══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("Tuning with SPATIAL cross-validation...")
log_status("========================================")

t1 <- system.time({
  rf_spatial_results <- tune_grid(
    rf_workflow,
    resamples = spatial_folds,
    grid      = rf_grid,
    metrics   = metrics
  )
})
log_status("   Spatial CV complete in", round(t1["elapsed"] / 60, 1), "minutes")
log_status("   Best spatial ROC AUC:",
           round(show_best(rf_spatial_results, metric = "roc_auc")$mean[1], 4))

# ═══════════════════════════════════════════════════════════════════════════════
# STANDARD CV
# ═══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("Tuning with STANDARD cross-validation...")
log_status("========================================")

t2 <- system.time({
  rf_standard_results <- tune_grid(
    rf_workflow,
    resamples = standard_folds,
    grid      = rf_grid,
    metrics   = metrics
  )
})
log_status("   Standard CV complete in", round(t2["elapsed"] / 60, 1), "minutes")
log_status("   Best standard ROC AUC:",
           round(show_best(rf_standard_results, metric = "roc_auc")$mean[1], 4))

stopCluster(cl)

# ═══════════════════════════════════════════════════════════════════════════════
# COMPARE CV APPROACHES
# ═══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("CV Comparison:")
log_status("========================================")

compare_cv <- bind_rows(
  collect_metrics(rf_spatial_results)  %>% mutate(cv = "Spatial"),
  collect_metrics(rf_standard_results) %>% mutate(cv = "Standard")
) %>%
  filter(.metric == "roc_auc") %>%
  group_by(cv) %>%
  slice_max(mean, n = 1) %>%
  select(cv, roc_auc = mean, std_err = std_err) %>%
  arrange(desc(roc_auc))

print(compare_cv)
arrow::write_parquet(compare_cv,
                     file.path(MODEL_DIR, "stage1_cv_comparison.parquet"))

# ═══════════════════════════════════════════════════════════════════════════════
# FINAL FIT
# ═══════════════════════════════════════════════════════════════════════════════
log_status("========================================")
log_status("Fitting final model on full training data...")
log_status("========================================")

set.seed(456)
data_split <- initial_split(s1_model, prop = 0.75, strata = class)
train_data <- training(data_split)
test_data  <- testing(data_split)

best_params    <- select_best(rf_spatial_results, metric = "roc_auc")
final_workflow <- finalize_workflow(rf_workflow, best_params)
final_fit      <- last_fit(final_workflow, data_split)

log_status("Test set metrics:")
print(collect_metrics(final_fit))

log_status("Confusion matrix:")
collect_predictions(final_fit) %>%
  conf_mat(truth = class, estimate = .pred_class) %>%
  print()

# ── Feature importance ────────────────────────────────────────────────────────
log_status("Computing feature importance...")
rf_importance <- extract_workflow(final_fit) %>%
  extract_fit_parsnip() %>%
  vi() %>%
  arrange(desc(Importance))

log_status("Top 20 features:")
print(rf_importance, n = 20)

arrow::write_parquet(rf_importance,
                     file.path(MODEL_DIR, "stage1_rf_importance.parquet"))

# ── Threshold analysis ────────────────────────────────────────────────────────
log_status("Running threshold analysis...")
# Calculate optimal threshold manually using ROC curve
roc_data <- collect_predictions(final_fit) %>%
  roc_curve(truth = class, .pred_Correct, event_level = "first")

# Youden J = sensitivity + specificity - 1
# specificity = 1 - (1 - specificity) = 1 - FPR
threshold_df <- roc_data %>%
  mutate(
    j_index    = sensitivity + specificity - 1,
    .threshold = .threshold
  )

optimal_threshold <- threshold_df %>%
  slice_max(j_index, n = 1) %>%
  pull(.threshold)

log_status("   Optimal Stage 1 threshold (Youden J):", round(optimal_threshold, 3))

# Save threshold data
arrow::write_parquet(threshold_df,
                     file.path(MODEL_DIR, "stage1_threshold_analysis.parquet"))

# ── Save model ────────────────────────────────────────────────────────────────
log_status("Saving model...")

stage1_model <- extract_workflow(final_fit) %>% butcher()
saveRDS(stage1_model,
        file.path(MODEL_DIR, "stage1_rf_model.rds"))
saveRDS(optimal_threshold,
        file.path(MODEL_DIR, "stage1_optimal_threshold.rds"))

log_status("   stage1_rf_model.rds saved:",
           round(file.size(file.path(MODEL_DIR, "stage1_rf_model.rds")) / 1e6, 1), "MB")
log_status("   stage1_optimal_threshold.rds saved")

log_status("========================================")
log_status("Stage 1 training complete")
log_status("========================================")


# ── Final summary output ──────────────────────────────────────────────────────
log_status("========================================")
log_status("FINAL RESULTS SUMMARY")
log_status("========================================")

log_status("Test set metrics:")
print(collect_metrics(final_fit))

log_status("Confusion matrix:")
collect_predictions(final_fit) %>%
  conf_mat(truth = class, estimate = .pred_class) %>%
  print()

log_status("Optimal threshold:", round(optimal_threshold, 3))

log_status("Top 20 features:")
print(rf_importance, n = 20)

log_status("CV Comparison:")
print(compare_cv)