# Latest Updates - Codebase Familiarization

## Overview
This is a comprehensive web-based soil data explorer for the Cascade-Siskiyou National Monument (CSNM), built with vanilla JavaScript and Leaflet.js. The application has been recently refactored from R Shiny to JavaScript for better performance and accessibility.

## Architecture Summary

### Core Components

1. **`index.html`** - Main HTML structure with:
   - Leaflet CSS/JS and Plotly.js for mapping and charting
   - Responsive layout with collapsible sidebar
   - Modal dialogs for information display
   - Loading overlays and coordinate displays

2. **`js/config.js`** - Central configuration containing:
   - Map settings (center: [42.1, -122.466], zoom: 11)
   - Color palettes for soil orders, land cover, elevation
   - Data file paths and projection settings
   - UI constants and utility functions

3. **`js/app.js`** - Main application orchestrator:
   - SoilExplorerApp class manages initialization
   - Coordinates between MapManager, UIController, and DataLoader
   - Handles application lifecycle and error management

4. **`js/data-loader.js`** - Data management:
   - Caches GeoJSON and raster data
   - Handles projection validation
   - Manages loading states and error handling

5. **`js/map-utils.js`** - Map functionality:
   - MapManager class for Leaflet integration
   - Layer management (base, polygons, overlays, rasters)
   - Event handling for map interactions

6. **`js/raster-utils.js`** - Raster processing:
   - RasterManager class for TIFF file handling
   - Uses GeoTIFF.js for client-side raster processing
   - Color mapping and hillshade blending algorithms

7. **`js/ui-controls.js`** - User interface:
   - UIController class managing all UI elements
   - Event listeners for controls and modals
   - State management for view switching

### Data Sources

- **Soil Polygons**: SSURGO data in `CSNM_Polygons_WGS84.geojson`
- **Raster Data**: Multi-depth soil properties (OC, pH) and environmental layers
- **Overlays**: Highways, service roads, monument boundaries
- **Base Maps**: OpenStreetMap, satellite imagery, topographic maps

### Key Features

- **Multi-layer visualization**: Soil orders, organic carbon, pH, land cover, elevation
- **Depth-specific data**: 6 depth intervals from 0-5cm to 100-200cm
- **Interactive overlays**: Toggleable boundaries, roads, points of interest
- **Real-time raster processing**: Client-side TIFF handling with GeoTIFF.js
- **Responsive design**: Mobile-friendly with collapsible sidebar
- **Error handling**: Graceful fallbacks and user feedback

### Technical Implementation

- **Modular architecture**: Clean separation of concerns across 6 JS modules
- **Event-driven design**: Custom events for component communication
- **Caching system**: Efficient data loading and management
- **Performance optimization**: Lazy loading and efficient rendering
- **Browser compatibility**: Uses modern JavaScript with fallback support

## Current State

The application is fully functional with:
- ✅ Complete UI implementation
- ✅ Real TIFF raster support
- ✅ Multi-depth soil data visualization
- ✅ Interactive map overlays
- ✅ Responsive design
- ✅ Error handling and fallbacks

## Development Setup

1. Navigate to project directory
2. Run `python3 serve.py` to start local server
3. Open `http://localhost:8000` in browser
4. Check browser console for debugging information

The codebase is well-structured, documented, and ready for development or deployment.