# Location Model — Tabular Candidate Scoring (R)

This is the **supporting tabular stage** of the pipeline. The primary
computer-vision component lives in [`../object_detection`](../object_detection);
this stage scores whether a facility's reported location is correct and, when it
isn't, ranks nearby parcels as candidate corrections.

> Reference / methodology code. It was developed as part of an EPA-org-hosted
> effort (led/architected by the author) and ran on an HPC cluster (SLURM, R
> 4.4). The scripts depend on licensed parcel data (Regrid) and internal source
> tables that are **not** distributed here, so they are not runnable as-is —
> they document the method. Data paths are shown as a relative `data/` root.

## Two-stage design

**Stage 1 — is the reported location correct?**
A gradient-boosted / random-forest classifier (tidymodels) scores each plant's
reported coordinate against features of the parcel it lands on. Plants below the
confidence threshold, or that don't land on any parcel, are flagged for Stage 2.

**Stage 2 — which nearby parcel is the real one?**
For each flagged plant, candidate parcels are gathered from **H3 hexagon rings
(k = 9, ≈5 km)** around the reported point, and a second model scores each
candidate. The top-scoring candidate becomes the proposed correction, with a
tiering scheme (nearby / distant / no-candidate) that routes low-confidence
cases to manual GIS review.

Reported performance (see [`report/`](report)): **Stage 1 ROC AUC ≈ 0.986**,
**Stage 2 ROC AUC ≈ 0.975**, evaluated across the national set of CWNS plants
(hundreds of thousands of candidate parcels).

## Pipeline layout

```
pipeline/
  01_data_prep/          extract parcels (DuckDB), attach NLCD land cover, H3 index
  02_feature_engineering/ build parcel/plant features (Census, OSM, footprints)
  03_models/             train_stage1.R, train_stage2.R (tidymodels)
  04_inference/          score all plants; gather + score H3 candidates
  05_results/            assemble corrections, tiers, and the results geopackage
report/
  cwns_location_correction_report.qmd   results-review report (metrics, tiers, maps)
```

## Stack

R · tidymodels (ranger / xgboost) · **sf** · **terra** · **arrow** (GeoParquet) ·
**DuckDB** · **H3** (`h3` R bindings) · spatialsample (spatial CV) · Quarto.

The object-detection stage supplies an independent imagery-based location signal;
combined, the two stages cross-check reported coordinates against both tabular
parcel evidence and what is actually visible in aerial imagery.
