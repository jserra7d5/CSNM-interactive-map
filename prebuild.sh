#!/bin/bash
# Pre-build script to ensure correct files are deployed

echo "Pre-build cleanup..."

# Remove any non-WGS84 polygon files
rm -f data/CSNM_Polygons_with_Data.geojson
rm -f data/CSNM_Polygons_with_Data.geojson.gz
rm -f data/test_polygons.geojson

# List remaining polygon files
echo "Remaining polygon files:"
ls -la data/CSNM_Polygons* || echo "No polygon files found"

echo "Pre-build cleanup complete"