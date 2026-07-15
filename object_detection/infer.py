"""
infer.py
--------
Runs the trained YOLOv8 model over a directory of NAIP RGB tiles and writes a
detection table. When a tile_metadata.csv is available (produced during tile
extraction), each detection's pixel-space bounding-box centroid is converted
to a geographic coordinate so detections can feed the coordinate-correction
step.

Usage:
  python infer.py --weights weights/best.pt --tiles path/to/rgb_tiles \
      --metadata tile_metadata.csv --out detections.csv --conf 0.25

The geographic conversion assumes each tile's bounding box (in WGS84) is
recorded in tile_metadata.csv as bbox_xmin/ymin/xmax/ymax with a fixed pixel
size (image_px). Detections across overlapping tiles should afterward be
deduplicated in geographic space (non-maximum suppression with a ~10-15 m
distance threshold — see README, "Downstream use").
"""

import argparse
import csv
from pathlib import Path

# NOTE: `ultralytics`/`torch` are imported inside main() so the pure helper
# functions below (load_tile_metadata, pixel_to_geo) can be imported and unit
# tested without the heavy DL dependencies installed.


def load_tile_metadata(metadata_path: Path) -> dict:
    """Return {tile_id: row_dict} keyed by the tile filename stem."""
    if not metadata_path or not Path(metadata_path).exists():
        return {}
    index = {}
    with open(metadata_path, newline="") as f:
        for row in csv.DictReader(f):
            tile_id = row.get("tile_id") or Path(row.get("rgb_path", "")).stem
            if tile_id:
                index[tile_id.replace("_rgb", "")] = row
    return index


def pixel_to_geo(cx_px, cy_px, meta, image_px):
    """
    Map a pixel-space centroid to a lon/lat using the tile's WGS84 bbox.
    Returns (lon, lat) or (None, None) if metadata is incomplete.
    Image origin is top-left; y increases downward, so latitude is flipped.
    """
    try:
        xmin = float(meta["bbox_xmin"])
        xmax = float(meta["bbox_xmax"])
        ymin = float(meta["bbox_ymin"])
        ymax = float(meta["bbox_ymax"])
    except (KeyError, TypeError, ValueError):
        return None, None
    lon = xmin + (cx_px / image_px) * (xmax - xmin)
    lat = ymax - (cy_px / image_px) * (ymax - ymin)  # image y is top-down; flip to lat
    return lon, lat


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--weights", default="weights/best.pt")
    ap.add_argument("--tiles", required=True, help="directory of RGB tiles")
    ap.add_argument("--metadata", default=None, help="tile_metadata.csv (optional)")
    ap.add_argument("--out", default="detections.csv")
    ap.add_argument("--conf", type=float, default=0.25)
    ap.add_argument("--imgsz", type=int, default=333)
    args = ap.parse_args()

    from ultralytics import YOLO  # imported here to keep helpers dependency-free

    model = YOLO(args.weights)
    names = model.names
    meta_index = load_tile_metadata(args.metadata)

    tile_paths = sorted(Path(args.tiles).glob("*.png"))
    print(f"Running inference on {len(tile_paths)} tiles (conf >= {args.conf})...")

    rows = []
    for tp in tile_paths:
        result = model.predict(source=str(tp), conf=args.conf, imgsz=args.imgsz, verbose=False)[0]
        tile_stem = tp.stem.replace("_rgb", "")
        meta = meta_index.get(tile_stem)
        for box in result.boxes:
            cls_id = int(box.cls[0])
            conf = float(box.conf[0])
            x1, y1, x2, y2 = (float(v) for v in box.xyxy[0])
            cx_px, cy_px = (x1 + x2) / 2.0, (y1 + y2) / 2.0
            lon, lat = pixel_to_geo(cx_px, cy_px, meta, args.imgsz) if meta else (None, None)
            rows.append({
                "tile": tp.name,
                "class": names[cls_id],
                "confidence": round(conf, 4),
                "cx_px": round(cx_px, 1),
                "cy_px": round(cy_px, 1),
                "lon": lon,
                "lat": lat,
            })

    with open(args.out, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["tile", "class", "confidence", "cx_px", "cy_px", "lon", "lat"])
        writer.writeheader()
        writer.writerows(rows)

    print(f"Wrote {len(rows)} detections to {args.out}")


if __name__ == "__main__":
    main()
