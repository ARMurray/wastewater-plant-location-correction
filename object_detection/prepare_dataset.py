"""
prepare_dataset.py
------------------
Reconstructs minimal metadata from tile filenames, matches Label Studio
YOLO exports to source images, performs a plant-level train/val split,
and populates the dataset/ directory structure for YOLOv8 training.

The train/val split is done *by facility* (CWNS_ID), so no facility ever
appears in both train and val — this avoids optimistic metrics from tiles
of the same plant leaking across the split.

Expected folder layout (all relative to BASE_DIR, which defaults to this
script's directory and can be overridden with the WWTP_BASE env var):
  tiles/rgb/png/      - RGB PNG tiles
  tiles/ndwi/         - NDWI TIF tiles (reserved for a future NIR/NDWI channel)
  ls_export/labels/   - YOLO .txt label files from a Label Studio export
  ls_export/classes.txt
  dataset/            - created by this script
"""

import os
import re
import shutil
import random
from pathlib import Path
from collections import defaultdict

# ---------------------------------------------------------------------------
# CONFIG — override BASE_DIR with the WWTP_BASE environment variable if your
# data lives elsewhere. Defaults to the directory this script sits in.
# ---------------------------------------------------------------------------
BASE_DIR        = Path(os.environ.get("WWTP_BASE", Path(__file__).resolve().parent))
RGB_DIR         = BASE_DIR / "tiles" / "rgb" / "png"
LS_LABELS_DIR   = BASE_DIR / "ls_export" / "labels"
CLASSES_FILE    = BASE_DIR / "classes.txt"
DATASET_DIR     = BASE_DIR / "dataset"
YAML_PATH       = BASE_DIR / "dataset.yaml"

VAL_FRACTION    = 0.2     # 20% of plants held out for validation
RANDOM_SEED     = 42
INCLUDE_NEGATIVES = True  # include tiles with no annotations (empty label files)
# ---------------------------------------------------------------------------


def parse_tile_stem(label_filename: str) -> "tuple[str, str] | None":
    """
    Extract (cwns_id, tile_stem) from a Label Studio label filename.

    Two observed export patterns:
      1. Path-encoded: {hash}__...%5C{tile_stem}_rgb.txt  (Windows path escaped)
      2. Clean:        {hash}-{tile_stem}_rgb.txt

    tile_stem looks like:
      17000281001_6fcc5c83-3169-4bd4-930e-143ad9895c1e_r01_c01

    Returns (cwns_id, tile_stem) or None if parsing fails.
    """
    name = Path(label_filename).stem  # drop .txt

    # Pattern 1: URL-encoded path — grab everything after the last %5C
    if "%5C" in name:
        name = name.split("%5C")[-1]
    else:
        # Pattern 2: strip leading 8-char hex hash and separator
        name = re.sub(r'^[0-9a-f]{8}[-_]', '', name)

    # name should now be "{tile_stem}_rgb" or just "{tile_stem}"
    name = re.sub(r'_rgb$', '', name)

    # tile_stem = {cwns_id}_{ll_uuid}_r{row}_c{col}
    match = re.match(
        r'^(\d+)_([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})_(r\d+_c\d+)$',
        name
    )
    if not match:
        return None

    cwns_id  = match.group(1)
    ll_uuid  = match.group(2)
    rowcol   = match.group(3)
    tile_stem = f"{cwns_id}_{ll_uuid}_{rowcol}"
    return cwns_id, tile_stem


def load_classes(classes_file: Path) -> "list[str]":
    with open(classes_file) as f:
        return [line.strip() for line in f if line.strip()]


def build_label_index(ls_labels_dir: Path) -> "dict[str, Path]":
    """
    Returns {tile_stem: label_path} for every parseable label file.
    Logs any files that couldn't be parsed.
    """
    index = {}
    unparsed = []
    for label_file in ls_labels_dir.glob("*.txt"):
        result = parse_tile_stem(label_file.name)
        if result is None:
            unparsed.append(label_file.name)
            continue
        cwns_id, tile_stem = result
        index[tile_stem] = label_file

    if unparsed:
        print(f"  WARNING: Could not parse {len(unparsed)} label filenames:")
        for f in unparsed[:10]:
            print(f"    {f}")
        if len(unparsed) > 10:
            print(f"    ... and {len(unparsed) - 10} more")

    return index


def build_rgb_index(rgb_dir: Path) -> "dict[str, Path]":
    """Returns {tile_stem: rgb_path} for every PNG in tiles/rgb/."""
    index = {}
    for png in rgb_dir.glob("*.png"):
        stem = re.sub(r'_rgb$', '', png.stem)
        index[stem] = png
    return index


def split_by_plant(
    matched: "list[tuple[str, Path, Path]]",
    val_fraction: float,
    seed: int
) -> "tuple[list, list]":
    """
    matched: list of (cwns_id, rgb_path, label_path)
    Splits by CWNS_ID so no plant appears in both train and val.
    Returns (train_list, val_list) of the same tuples.
    """
    plant_to_tiles = defaultdict(list)
    for i, (cwns_id, rgb_path, label_path) in enumerate(matched):
        plant_to_tiles[cwns_id].append(i)

    plants = list(plant_to_tiles.keys())
    random.seed(seed)
    random.shuffle(plants)

    n_val = max(1, round(len(plants) * val_fraction))
    val_plants   = set(plants[:n_val])
    train_plants = set(plants[n_val:])

    train = [matched[i] for p in train_plants for i in plant_to_tiles[p]]
    val   = [matched[i] for p in val_plants   for i in plant_to_tiles[p]]

    return train, val


def copy_split(split: list, split_name: str, dataset_dir: Path):
    img_dir = dataset_dir / "images" / split_name
    lbl_dir = dataset_dir / "labels" / split_name
    img_dir.mkdir(parents=True, exist_ok=True)
    lbl_dir.mkdir(parents=True, exist_ok=True)

    for cwns_id, rgb_path, label_path in split:
        shutil.copy2(rgb_path, img_dir / rgb_path.name)
        shutil.copy2(label_path, lbl_dir / label_path.name.replace(
            label_path.stem, rgb_path.stem  # make label filename match image filename
        ))


def write_yaml(yaml_path: Path, dataset_dir: Path, classes: "list[str]"):
    lines = [
        f"path: {dataset_dir.as_posix()}",
        "train: images/train",
        "val:   images/val",
        "",
        f"nc: {len(classes)}",
        f"names: {classes}",
        "",
    ]
    with open(yaml_path, "w") as f:
        f.write("\n".join(lines))


def is_negative(label_path: Path) -> bool:
    """True if the label file is empty (no annotations)."""
    return label_path.stat().st_size == 0


def main():
    print("=== prepare_dataset.py ===\n")

    classes = load_classes(CLASSES_FILE)
    print(f"Classes ({len(classes)}): {classes}\n")

    print("Indexing label files...")
    label_index = build_label_index(LS_LABELS_DIR)
    print(f"  Parsed {len(label_index)} label files\n")

    print("Indexing RGB tiles...")
    rgb_index = build_rgb_index(RGB_DIR)
    print(f"  Found {len(rgb_index)} RGB tiles\n")

    matched = []
    missing_rgb = []
    n_positive = 0
    n_negative = 0

    for tile_stem, label_path in label_index.items():
        if tile_stem not in rgb_index:
            missing_rgb.append(tile_stem)
            continue

        negative = is_negative(label_path)
        if negative and not INCLUDE_NEGATIVES:
            n_negative += 1
            continue

        result = parse_tile_stem(label_path.name)
        cwns_id = result[0] if result else "unknown"

        matched.append((cwns_id, rgb_index[tile_stem], label_path))
        if negative:
            n_negative += 1
        else:
            n_positive += 1

    print(f"Matched {len(matched)} tiles to labels")
    print(f"  Positive (annotated): {n_positive}")
    print(f"  Negative (empty):     {n_negative}")
    if missing_rgb:
        print(f"  WARNING: {len(missing_rgb)} label files had no matching RGB tile")
        for s in missing_rgb[:5]:
            print(f"    {s}")
    print()

    if not matched:
        print("ERROR: No matched tiles found. Check your paths and filename patterns.")
        return

    train, val = split_by_plant(matched, VAL_FRACTION, RANDOM_SEED)

    train_plants = len(set(c for c, _, _ in train))
    val_plants   = len(set(c for c, _, _ in val))
    print("Train/val split (by plant):")
    print(f"  Train: {len(train)} tiles across {train_plants} plants")
    print(f"  Val:   {len(val)} tiles across {val_plants} plants")
    print()

    print(f"Copying files to {DATASET_DIR} ...")
    if DATASET_DIR.exists():
        shutil.rmtree(DATASET_DIR)
    copy_split(train, "train", DATASET_DIR)
    copy_split(val,   "val",   DATASET_DIR)
    print("  Done.\n")

    write_yaml(YAML_PATH, DATASET_DIR, classes)
    print(f"Written: {YAML_PATH}\n")

    print("=== Summary ===")
    print(f"  dataset/images/train/ : {len(train)} images")
    print(f"  dataset/images/val/   : {len(val)} images")
    print(f"  dataset/labels/train/ : {len(train)} label files")
    print(f"  dataset/labels/val/   : {len(val)} label files")
    print(f"  dataset.yaml          : {YAML_PATH}")
    print("\nReady for train_model.py")


if __name__ == "__main__":
    main()
