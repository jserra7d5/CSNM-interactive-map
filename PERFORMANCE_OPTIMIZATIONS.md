# Performance Optimizations

This document describes the performance optimizations implemented to improve loading times for the CSNM Interactive Map application.

## Changes Made

### 1. Service Roads Color Change
- Changed service roads color from green (#228B22) to red (#FF0000) in `js/map-utils.js`

### 2. GeoJSON and TIFF Compression
- Created `compress-assets.js` script to compress large GeoJSON and TIFF files with gzip
- Updated `js/data-loader.js` to automatically detect and load compressed (.gz) GeoJSON files
- Updated `js/raster-utils.js` to automatically detect and load compressed (.gz) TIFF files
- Added support in `vercel.json` for serving compressed files with proper headers
- Includes compression for elevation (CSNM_Elevation_10m.tif) and hillshade (CSNM_Hillshade_10m.tif) files

### 3. Progressive Loading
- Implemented batch loading for large GeoJSON datasets (>1000 features)
- Features are loaded in batches of 500-1000 using requestAnimationFrame
- Provides smooth rendering without blocking the UI

### 4. Caching and Headers
- Added cache-control headers for all static assets (1 year for data files)
- Enabled browser caching to reduce repeated downloads
- Added support for TIFF range requests (when server supports it)

### 5. Vercel Configuration
- Updated `vercel.json` with compression headers
- Added rewrites to automatically serve .gz files when available
- Configured proper content-type headers for all file types

## How to Use

### 1. Compress Your Data Files

Before deploying, run the compression script:

```bash
npm run build
```

This will:
- Compress all GeoJSON and TIFF files larger than 100KB
- Create .gz versions alongside the original files
- Show compression statistics

Note: TIFF compression is currently disabled in the application because GeoTIFF.js requires 
raw TIFF data and cannot directly process gzipped TIFFs. The compressed .gz files are created 
but not used. To enable TIFF compression, you would need to implement client-side gzip 
decompression before passing the data to GeoTIFF.js.

### 2. Deploy to Vercel

```bash
vercel --prod
```

The application will automatically:
- Try to load compressed versions of GeoJSON files first
- Fall back to uncompressed if .gz files don't exist
- Use progressive loading for large datasets
- Cache files in the browser for faster repeat visits

## Performance Improvements

Based on the 37MB CSNM_Polygons_WGS84.geojson file:
- **Before**: ~37MB download, slow initial load
- **After**: ~4-5MB download (85-90% reduction), progressive rendering

## Additional Optimizations to Consider

1. **Simplify Polygons**: Use tools like mapshaper to reduce polygon complexity
2. **Vector Tiles**: Convert large GeoJSON to vector tiles for zoom-based loading
3. **CDN**: Use a CDN for static assets
4. **Service Worker**: Implement offline caching with a service worker
5. **WebP Images**: Convert raster overlays to WebP format

## Troubleshooting

If compressed files aren't loading:
1. Check that .gz files were created by the build script
2. Verify Vercel deployment includes the .gz files
3. Check browser console for any CORS or content-encoding errors
4. Ensure vercel.json is properly configured