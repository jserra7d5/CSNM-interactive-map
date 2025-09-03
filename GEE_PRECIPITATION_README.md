# PRISM Precipitation Analysis for CSNM - Google Earth Engine Script

## Overview

This simplified Google Earth Engine script focuses exclusively on processing and displaying precipitation data from the PRISM Climate Normals (1991-2020) dataset for the Cascade-Siskiyou National Monument.

## Key Fixes Applied

### 1. **Data Processing Issues Fixed**
- **Original Problem**: Used `.mean()` on already-averaged normal data
- **Fix**: Use `.sum()` for annual totals and proper monthly filtering for seasonal data
- **Result**: Accurate precipitation values that represent actual climate normals

### 2. **Visualization Parameters Optimized**
- **Original Problem**: Range 0-2000mm was too broad and inappropriate
- **Fix**: Optimized ranges based on Pacific Northwest climate:
  - Annual: 200-1500mm (realistic for CSNM region)
  - Seasonal: 0-400mm (monthly averages)
- **Result**: Better color contrast and meaningful visual representation

### 3. **Layer Visibility Fixed**
- **Original Problem**: Precipitation layer was hidden by default (`false`)
- **Fix**: Annual precipitation layer is now visible by default (`true`)
- **Result**: Precipitation map displays immediately upon running

### 4. **Boundary Definition Improved**
- **Original Problem**: Asset path might not be accessible
- **Fix**: Added fallback boundary using coordinate polygon
- **Result**: Script works even if original asset is unavailable

## Script Features

### Data Products Generated
1. **Annual Precipitation**: Sum of all 12 monthly normals
2. **Seasonal Precipitation**: 
   - Winter (Dec-Jan-Feb average)
   - Spring (Mar-Apr-May average)
   - Summer (Jun-Jul-Aug average)
   - Fall (Sep-Oct-Nov average)

### Visualization Improvements
- **Color Palette**: Brown-to-blue gradient optimized for precipitation
- **Value Ranges**: Appropriate for CSNM's climate patterns
- **Layer Organization**: Annual visible by default, seasonal layers toggleable

### Debugging Tools
- Comprehensive statistics output for all precipitation layers
- Data inspection functions to understand value ranges
- Progress messages throughout script execution
- Error handling with fallback boundary definition

## How to Use

1. **Copy the script** from `gee_prism_precipitation_only.js`
2. **Paste into Google Earth Engine** Code Editor
3. **Run the script**
4. **Check the Console** for statistics and progress messages
5. **View the map** - precipitation should now display properly
6. **Toggle layers** to see seasonal variations
7. **Export data** using the Tasks tab if needed

## Expected Results

- **Map Display**: Brown-to-blue precipitation map showing annual totals
- **Console Output**: Statistics showing realistic precipitation values (typically 300-1200mm annually for CSNM)
- **Export Options**: Multiple GeoTIFF files available for download

## Troubleshooting

If the map still doesn't display:

1. **Check Console Messages**: Look for error messages or data statistics
2. **Verify Boundary**: Ensure the monument boundary loaded (red outline should appear)
3. **Check Layer Visibility**: Make sure "Annual Precipitation" layer is checked
4. **Adjust Visualization**: If values are outside expected range, modify the `min` and `max` in visualization parameters

## Technical Notes

- **Resolution**: 100m output (from ~4km native PRISM resolution)
- **Data Source**: PRISM Climate Normals 1991-2020
- **Coordinate System**: WGS84 (EPSG:4326)
- **Processing**: Client-side processing in Google Earth Engine
- **Export Format**: GeoTIFF suitable for use in GIS applications

This simplified version eliminates complexity while ensuring the precipitation data displays correctly and provides meaningful visualizations for the Cascade-Siskiyou National Monument region.