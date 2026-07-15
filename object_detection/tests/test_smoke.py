"""
Lightweight smoke tests for the object-detection module.

These test pure logic only (filename parsing, pixel->geo conversion, dataset
config consistency) so they run in CI in seconds without a GPU, the training
data, or the heavy torch/ultralytics stack.
"""

import sys
from pathlib import Path

import yaml

# Make the module importable whether tests run from repo root or this dir.
MODULE_DIR = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(MODULE_DIR))

import prepare_dataset  # noqa: E402
import infer  # noqa: E402


def test_parse_tile_stem_clean_pattern():
    cwns_id, stem = prepare_dataset.parse_tile_stem(
        "1a2b3c4d-17000281001_6fcc5c83-3169-4bd4-930e-143ad9895c1e_r01_c01_rgb.txt"
    )
    assert cwns_id == "17000281001"
    assert stem == "17000281001_6fcc5c83-3169-4bd4-930e-143ad9895c1e_r01_c01"


def test_parse_tile_stem_path_encoded_pattern():
    cwns_id, stem = prepare_dataset.parse_tile_stem(
        "deadbeef__Users%5Cuser%5Cpng%5C08000160003_527c62ef-2e0e-4b48-98c9-25f2b49deb17_r02_c01_rgb.txt"
    )
    assert cwns_id == "08000160003"
    assert stem.endswith("_r02_c01")


def test_parse_tile_stem_rejects_garbage():
    assert prepare_dataset.parse_tile_stem("not_a_real_tile_name.txt") is None


def test_pixel_to_geo_center_and_corners():
    # A 100x100 px tile spanning lon [-80, -79.99], lat [35.0, 35.01]
    meta = {"bbox_xmin": "-80.0", "bbox_xmax": "-79.99",
            "bbox_ymin": "35.0", "bbox_ymax": "35.01"}
    # Center pixel -> center coordinate
    lon, lat = infer.pixel_to_geo(50, 50, meta, image_px=100)
    assert abs(lon - (-79.995)) < 1e-9
    assert abs(lat - 35.005) < 1e-9
    # Top-left pixel (0,0) -> (xmin, ymax) because image y is flipped
    lon0, lat0 = infer.pixel_to_geo(0, 0, meta, image_px=100)
    assert abs(lon0 - (-80.0)) < 1e-9
    assert abs(lat0 - 35.01) < 1e-9


def test_pixel_to_geo_handles_missing_metadata():
    assert infer.pixel_to_geo(10, 10, {}, image_px=100) == (None, None)


def test_dataset_yaml_matches_classes_file():
    cfg = yaml.safe_load((MODULE_DIR / "dataset.yaml").read_text())
    classes = [c for c in (MODULE_DIR / "classes.txt").read_text().splitlines() if c.strip()]
    assert cfg["nc"] == len(classes) == 6
    assert cfg["names"] == classes
