# Object-Detection Module — Technical Overview

## Background

This module is the computer-vision component of a larger pipeline that corrects
incorrectly reported coordinates for wastewater treatment plants. A companion
tabular model (see [`../location_model`](../location_model)) uses text and
locational features to score and correct locations; roughly **40% of reported
plant coordinates are wrong**. This module adds an aerial-imagery signal:
detecting visible treatment-plant infrastructure directly in NAIP imagery so the
detection centroid can be converted into a corrected coordinate.

## Goal

Train an object detector that identifies specific wastewater-treatment
infrastructure in NAIP aerial imagery. At inference the model returns bounding
boxes; box centroids are converted to geographic coordinates (see
[`infer.py`](infer.py)) to produce a corrected location per facility.

## Data sources

- **Facility locations** — verified treatment-plant coordinates (correct-by-parcel
  and confirmed-original subsets), stratified by Census region × population bin.
- **Industrial hard negatives** — a sample of TRI (Toxics Release Inventory)
  facilities pulled from the public EPA TRI feature service, filtered to
  manufacturing sectors with no on-site water treatment. These supply visually
  similar structures (circular tanks, rectangular buildings) that are *not*
  treatment plants, reducing false positives.
- **Parcel boundaries** — commercial parcel data (Regrid), used only to frame the
  imagery footprint around a facility. *Parcel data is licensed and is not
  distributed here; this repo describes methodology only.*
- **Aerial imagery** — NAIP (USDA National Agriculture Imagery Program), 4-band
  (R, G, B, NIR) from the public USDA CONUS PRIME image service. Native
  resolution (30–60 cm) is standardized to 60 cm. Two products per tile: RGB
  (PNG, for annotation/training) and NDWI (TIF, reserved for a future NIR channel).

## Infrastructure classes (6)

| Class | ID | Description |
|---|---|---|
| `aeration_basin` | 0 | Rectangular mixing tanks; turbulent water, visible aerators |
| `chlorine_contact` | 1 | Elongated rectangular tanks near the plant outlet |
| `clarifier` | 2 | Circular settling tanks; reflective surface, rotating-arm shadow |
| `digester` | 3 | Covered circular tanks; darker, no visible water surface |
| `drying_bed` | 4 | Rectangular sludge-drying cells; sandy/brown surface |
| `oxidation_pond` | 5 | Large open lagoons; common at small rural facilities |

## Tile design

- **Tile size:** 200 × 200 m, **resolution:** 60 cm (333 × 333 px)
- **Overlap:** 33% (≈134 m stride) in both axes; parcels smaller than a tile get a
  single centered tile
- **TRI tiles:** one centered tile per facility point (no parcel needed)

Tile filename convention:

```
{CWNS_ID}_{parcel_uuid}_r{row:02d}_c{col:02d}_rgb.png   # treatment-plant tiles
TRI_{facility_id}_r01_c01_rgb.png                       # industrial hard-negative tiles
```

## Training

- **Base model:** YOLOv8s (COCO-pretrained), transfer-learned
- **Split:** by facility (no plant appears in both train and val) — see
  `prepare_dataset.py`
- **Augmentation:** rotation + horizontal/vertical flips (nadir imagery has no
  canonical orientation), mosaic/mixup, HSV and scale jitter — tuned for a small
  hand-annotated set
- **Round-1 result (`wwtp_v1`):** mAP@50 ≈ 0.77, mAP@50-95 ≈ 0.40 on a 124-image
  train / 58-image val split. This is an honest first-pass proof of concept;
  round 2 expands the sample and adds hard negatives (see below).
- **Annotation:** Label Studio; class list in `classes.txt`

## Known failure modes / iteration

| Issue | Status | Notes |
|---|---|---|
| Oxidation-pond false positives on natural water | Active | Primary v1 failure; addressed with TRI + empty-scene hard negatives |
| Duplicate boxes on dense facilities | Active | Confidence-threshold and geographic-NMS tuning at inference |
| Large plants underrepresented | Resolved | Oversampled in round 2 via population-bin weights |

## Downstream use

Predictions return class, confidence, and a pixel-space bounding box. Centroids
are converted to lon/lat via each tile's recorded geographic footprint
(`tile_metadata.csv`). Detections across overlapping tiles are deduplicated in
geographic space using non-maximum suppression with a distance threshold based on
minimum realistic infrastructure separation (~10–15 m). The resulting corrected
coordinates feed the tabular candidate-scoring stage in
[`../location_model`](../location_model).
