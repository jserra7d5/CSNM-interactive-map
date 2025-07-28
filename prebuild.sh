#!/bin/bash
# Pre-build script to ensure correct files are deployed

echo "=== PRE-BUILD CLEANUP STARTING ==="
echo "Date: $(date)"

# Force remove any old polygon files
echo "Removing old polygon files..."
rm -f data/CSNM_Polygons_with_Data.geojson
rm -f data/CSNM_Polygons_with_Data.geojson.gz

# Create empty placeholders to overwrite cached files
echo "Creating empty placeholders..."
echo '{"type":"FeatureCollection","features":[]}' > data/CSNM_Polygons_with_Data.geojson
echo '{"type":"FeatureCollection","features":[]}' | gzip > data/CSNM_Polygons_with_Data.geojson.gz

# List all polygon files
echo ""
echo "All polygon files after cleanup:"
ls -la data/CSNM_Polygons* 2>/dev/null || echo "No polygon files found"

# Show file sizes
echo ""
echo "File sizes:"
du -h data/CSNM_Polygons* 2>/dev/null || echo "No files to measure"

# Verify correct files exist
echo ""
echo "Verifying WGS84 files exist:"
if [ -f "data/CSNM_Polygons_WGS84.geojson.gz" ]; then
    echo "✓ WGS84 polygon file exists ($(du -h data/CSNM_Polygons_WGS84.geojson.gz | cut -f1))"
else
    echo "✗ ERROR: WGS84 polygon file missing!"
    exit 1
fi

echo ""
echo "=== PRE-BUILD CLEANUP COMPLETE ==="