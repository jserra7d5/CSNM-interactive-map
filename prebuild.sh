#!/bin/bash
# Pre-build script to ensure correct files are deployed

echo "Pre-build cleanup starting..."

# Create empty placeholder to overwrite old cached file on Vercel
echo '{"type":"FeatureCollection","features":[]}' | gzip > data/CSNM_Polygons_with_Data.geojson.gz
echo "Created empty placeholder for CSNM_Polygons_with_Data.geojson.gz"

# List all polygon files
echo "All polygon files before cleanup:"
ls -la data/CSNM_Polygons* || echo "No polygon files found"

# Show file sizes
echo ""
echo "File sizes:"
du -h data/CSNM_Polygons* 2>/dev/null || echo "No files to measure"

echo "Pre-build cleanup complete"