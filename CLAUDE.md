# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the **Cascade-Siskiyou Soil Explorer**, an interactive web application for exploring soil properties and environmental data in the Cascade-Siskiyou National Monument. The application is a JavaScript translation of an original R Shiny application, featuring real TIFF raster processing, multi-depth soil data visualization, and comprehensive GIS overlays.

## Development Commands

**Start Development Server:**
```bash
python3 serve.py
```
Access at http://localhost:8000

**Alternative Start Commands:**
```bash
npm run dev    # Alternative way to start server
npm start      # Another alias for server start
```

**Build for Production:**
```bash
npm run build
```
This runs the compression script that gzips large data files for optimal deployment.

**Asset Compression:**
```bash
npm run compress
```
Compresses GeoJSON and TIFF files >100KB using gzip with maximum compression.

**Deploy to Production:**
```bash
npm run build
vercel --prod
```

**Make prebuild script executable (if needed):**
```bash
chmod +x prebuild.sh
```

## Code Architecture

### Core Application Structure

The application follows a **modular class-based architecture** with clear separation of concerns:

1. **`js/app.js`** - Main application orchestrator (`SoilExplorerApp` class)
   - Initializes and coordinates all other modules
   - Handles application lifecycle and error recovery
   - Event-driven architecture using custom events

2. **`js/config.js`** - Central configuration hub
   - Map settings, depth levels, and color palettes
   - SSURGO depth band mappings
   - Soil order classifications and styling

3. **`js/map-utils.js`** - Leaflet map management (`MapManager` class)
   - Layer management (base layers, overlays, rasters)
   - Legend creation and styling
   - Interactive popup handling

4. **`js/raster-utils.js`** - Raster data processing (`RasterManager` class)
   - Client-side TIFF loading using GeoTIFF.js
   - Color mapping and hillshade blending algorithms
   - Multi-depth soil data handling

5. **`js/ui-controls.js`** - User interface management (`UIController` class)
   - Control panel state management
   - Event handling for layer switching and depth selection
   - Dropdown persistence and mobile responsiveness

6. **`js/data-loader.js`** - Data loading and caching (`DataLoader` singleton)
   - Asynchronous GeoJSON and TIFF loading
   - Smart caching with fallback mechanisms
   - Loading state management

7. **`js/prism-data-service.js`** - PRISM climate data management (`PRISMDataService` class)
   - Real-time PRISM AN81M monthly climate data integration
   - IndexedDB caching for offline access and performance
   - Climate variable processing (temperature, precipitation)
   - Monthly data time series support

8. **`js/story-map.js`** - Story map narrative functionality
   - Sequential storytelling interface
   - Progress tracking and section management
   - Integration with main map application

### Data Flow Pattern

The application uses an **event-driven data flow**:

1. **Initialization**: `app.js` coordinates module initialization
2. **Data Loading**: `data-loader.js` handles all async data fetching with caching
3. **Layer Creation**: `raster-utils.js` processes TIFF data, `map-utils.js` creates Leaflet layers
4. **UI Updates**: `ui-controls.js` manages control panel state and triggers layer changes
5. **Event Communication**: Custom events (`layerChanged`, `depthChanged`) coordinate between modules

### Key Technical Features

**Real TIFF Processing:**
- Client-side TIFF loading using GeoTIFF.js library
- Fallback mechanisms for TIFF loading failures
- Multi-band support for different soil depth levels

**Hillshade + Elevation Integration:**
- Dual-layer approach combining grayscale hillshade with colored elevation data
- Pixel-level blending for 3D terrain visualization
- Smart transparency handling for clean monument boundaries

**Multi-Depth Soil Data:**
- 6 SSURGO depth intervals (0-5cm to 100-200cm)
- Dynamic color scaling per depth level
- Band name mapping in `config.js` for data file organization

**PRISM Climate Integration:**
- Real-time monthly climate data (last 12 months)
- Temperature and precipitation variables from PRISM AN81M
- IndexedDB persistent storage (~50MB total)
- Automatic monthly data updates on the 15th
- Monument-clipped data processing

## Data Structure

**Soil Polygon Data:**
- Source: Real SSURGO data in `data/CSNM_Polygons_WGS84.geojson`
- Dominant component filtering applied
- Soil order classification extracted from `taxorder` field

**Raster Data Organization:**
```
data/rasters/
├── oc/          # Organic carbon by depth
├── ph/          # Soil pH by depth  
├── elevation/   # DEM and hillshade
├── land-cover/  # ESA WorldCover classification
└── prism/       # PRISM climate data (cached)
```

**Application Entry Points:**
- `index.html` - Main soil explorer application
- `story-map.html` - Narrative storytelling interface
- `verify-files.html` - Development utility for file verification

**Compression Strategy:**
- Files >100KB automatically compressed with gzip
- Vercel deployment uses `.gz` files with proper headers
- Compression ratios typically 70-90% for GeoJSON/TIFF data

## Deployment Architecture

**Vercel Configuration (`vercel.json`):**
- CORS headers for geospatial data access
- Specialized content-type headers for TIFF/GeoJSON files
- Aggressive caching for static assets (1 year for data, 1 day for code)
- Automatic gzip serving with content-encoding headers

**Pre-build Process (`prebuild.sh`):**
- Ensures old polygon files are removed
- Creates empty placeholders to prevent caching issues  
- Validates required WGS84 files exist before deployment
- Must be executable (`chmod +x prebuild.sh`) for Vercel deployment

**Asset Compression (`compress-assets.js`):**
- Node.js script that compresses files >100KB using gzip
- Applies maximum compression (level 9) for optimal file sizes
- Targets GeoJSON, TIFF, and other data files
- Creates `.gz` versions served by Vercel with proper headers

## Testing Notes

**Manual Testing Workflow:**
1. Test all 6 map types (Soil Orders, Organic Carbon, pH, Land Cover, Elevation, Satellite)
2. Verify depth selection works for OC and pH layers (6 depth levels each)  
3. Test all overlay toggles (boundaries, highways, service roads, info center)
4. Check hillshade + elevation integration for 3D terrain effect
5. Verify mobile responsiveness and sidebar collapse functionality
6. Test interactive popups and coordinate display
7. Test PRISM climate data loading and month slider functionality
8. Verify story map navigation and progress tracking
9. Check asset compression and gzip serving in production

**Browser Console Monitoring:**
- TIFF loading progress and data analysis logs
- Layer switching confirmations
- Performance timing information
- Raster value range debugging data
- PRISM data loading and IndexedDB caching status
- Story map section transitions and progress updates

## File Naming Conventions

**JavaScript Modules:** Use kebab-case filenames, PascalCase class names
**Data Files:** Follow SSURGO/USGS naming conventions with depth indicators
**Raster Files:** Use property_depth.tif format (e.g., `CSNM_OC_0_5cm.tif`)
**Compressed Files:** Always include `.gz` extension for gzipped files

## Dependencies and External Services

**Core JavaScript Libraries:**
- Leaflet 1.9.4 for interactive mapping
- GeoTIFF.js for client-side TIFF processing
- Font Awesome 6.0.0 for icons

**External Data Services:**
- PRISM Oregon State University (https://data.prism.oregonstate.edu) - Climate data
- OpenStreetMap, CartoDB, Esri - Base map tiles
- USGS - Elevation and satellite imagery

**Browser Requirements:**
- Modern browsers with ES6+ support
- IndexedDB support for PRISM data caching
- Canvas 2D context for raster processing
- Minimum Node.js 14.0.0 for development

**No Build Tools Required:**
- Pure ES6 modules with native browser support
- No webpack, rollup, or other bundlers needed
- Direct script loading with module imports