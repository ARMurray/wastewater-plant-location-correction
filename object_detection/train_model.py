"""
train_model.py
--------------
Trains a YOLOv8s object detection model on annotated NAIP tiles for
wastewater treatment plant infrastructure detection, then validates the
best checkpoint and prints per-class AP.

Requires: ultralytics, torch (CUDA-enabled recommended)

Outputs:
  runs/wwtp_v1/          - training run directory
    weights/best.pt      - best checkpoint (use this for inference)
    weights/last.pt      - final epoch checkpoint
    results.csv          - per-epoch metrics
    confusion_matrix.png - class confusion matrix
    val_batch*.jpg       - sample validation predictions
"""

import os
from pathlib import Path

import torch
from ultralytics import YOLO

# ---------------------------------------------------------------------------
# CONFIG — BASE_DIR defaults to this script's directory; override with WWTP_BASE.
# ---------------------------------------------------------------------------
BASE_DIR    = Path(os.environ.get("WWTP_BASE", Path(__file__).resolve().parent))
YAML_PATH   = BASE_DIR / "dataset.yaml"
RUN_NAME    = "wwtp_v1"
RUN_DIR     = BASE_DIR / "runs"

# Model size — yolov8s balances accuracy against an 8GB VRAM budget and a
# small training set. Options (increasing size): yolov8n, s, m, l, x.
MODEL       = "yolov8s.pt"   # downloads pretrained COCO weights automatically

# Training hyperparameters
EPOCHS      = 100            # early stopping will usually halt sooner
PATIENCE    = 20             # stop if val mAP doesn't improve for 20 epochs
IMG_SIZE    = 333            # matches the 200 m tile at 60 cm/px (333x333)
BATCH_SIZE  = 16             # fits 8GB VRAM with yolov8s
LR          = 0.001          # initial LR (Adam) — fine for transfer learning
WORKERS     = 4

# Augmentation — matters a lot with limited training data. Rotation and both
# flips are enabled because aerial infrastructure has no canonical orientation.
AUGMENT_CONFIG = dict(
    hsv_h      = 0.015,   # hue shift (subtle — aerial hue is meaningful)
    hsv_s      = 0.5,     # saturation jitter
    hsv_v      = 0.4,     # brightness jitter (clouds, shadows)
    degrees    = 90,      # random rotation
    fliplr     = 0.5,     # horizontal flip
    flipud     = 0.5,     # vertical flip (valid for nadir imagery)
    mosaic     = 0.5,     # mosaic augmentation — helps a small dataset
    mixup      = 0.1,     # light mixup
    scale      = 0.3,     # scale jitter
    translate  = 0.1,     # translation
    shear      = 0.0,     # no shear — boxes don't transform cleanly
    perspective= 0.0,     # no perspective warp — imagery is already orthographic
)
# ---------------------------------------------------------------------------


def main():
    print("=== train_model.py ===\n")

    device = "cuda" if torch.cuda.is_available() else "cpu"
    if device == "cuda":
        print(f"GPU: {torch.cuda.get_device_name(0)}")
        print(f"VRAM available: {torch.cuda.get_device_properties(0).total_memory / 1e9:.1f} GB")
    else:
        print("WARNING: No GPU found, training on CPU. This will be slow.")
    print()

    if not YAML_PATH.exists():
        print(f"ERROR: dataset.yaml not found at {YAML_PATH}")
        print("Run prepare_dataset.py first.")
        return

    print(f"Loading model: {MODEL}")
    model = YOLO(MODEL)
    print()

    print(f"Starting training — run name: {RUN_NAME}")
    print(f"Results will be saved to: {RUN_DIR / RUN_NAME}\n")

    model.train(
        data        = str(YAML_PATH),
        epochs      = EPOCHS,
        patience    = PATIENCE,
        imgsz       = IMG_SIZE,
        batch       = BATCH_SIZE,
        lr0         = LR,
        device      = device,
        workers     = WORKERS,
        project     = str(RUN_DIR),
        name        = RUN_NAME,
        exist_ok    = False,
        pretrained  = True,
        optimizer   = "Adam",
        verbose     = True,
        seed        = 42,
        plots       = True,
        **AUGMENT_CONFIG,
    )

    print("\n=== Training Complete ===")
    best_weights = RUN_DIR / RUN_NAME / "weights" / "best.pt"
    if best_weights.exists():
        print(f"Best weights saved to: {best_weights}")
    print()

    print("Running validation with best weights...")
    best_model = YOLO(str(best_weights))
    metrics = best_model.val(
        data     = str(YAML_PATH),
        imgsz    = IMG_SIZE,
        device   = device,
        plots    = True,
        project  = str(RUN_DIR),
        name     = RUN_NAME + "_val",
        exist_ok = True,
    )

    print("\n=== Validation Metrics ===")
    print(f"  mAP50    : {metrics.box.map50:.3f}")
    print(f"  mAP50-95 : {metrics.box.map:.3f}")
    print(f"  Precision: {metrics.box.mp:.3f}")
    print(f"  Recall   : {metrics.box.mr:.3f}")
    print()
    print("Per-class AP50:")
    class_names = metrics.names
    for i, ap in enumerate(metrics.box.ap50):
        print(f"  {class_names[i]:<20}: {ap:.3f}")

    print("\nDone. Next step: infer.py")


if __name__ == "__main__":
    main()
