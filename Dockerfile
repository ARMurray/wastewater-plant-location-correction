# Reproducible CPU environment for the object-detection module.
# Runs dataset prep, inference, and the test suite. For GPU training, use an
# NVIDIA CUDA base image and install a matching torch build instead.
FROM python:3.12-slim

# System libs required by opencv / rasterio-style image IO
RUN apt-get update && apt-get install -y --no-install-recommends \
        libgl1 libglib2.0-0 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Install Python deps first for better layer caching
COPY object_detection/requirements.txt ./requirements.txt
RUN pip install --no-cache-dir -r requirements.txt pytest

# Copy the module
COPY object_detection/ ./object_detection/

WORKDIR /app/object_detection

# Default: run the smoke tests. Override CMD to train or run inference, e.g.
#   docker run --rm IMAGE python train_model.py
CMD ["python", "-m", "pytest", "tests/", "-q"]
