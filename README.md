# Soils of the Siskiyous

A modern web-based application for exploring soil data in the Cascade-Siskiyou National Monument, translated from the original R Shiny application.

## Features Implemented

### ✅ Core Map Functionality
- **Interactive Leaflet map** with multiple base layers (OpenStreetMap, Satellite, Topographic)
- **Soil polygon display** with soil order classification and USDA-standard color coding
- **Real TIFF raster support** using GeoTIFF.js for client-side processing
- **Multi-depth soil data** with 6 depth levels (0-5cm to 100-200cm)
- **Responsive sidebar** with collapsible controls and mobile support
- **Mouse coordinate display** showing current lat/lng position

### ✅ Data Layers
- **Soil Orders**: Colored polygons with taxonomic classifications (Alfisols, Mollisols, etc.)
- **Organic Carbon**: Enhanced 3-tier color scheme for better contrast between low/high values
- **Soil pH**: Red-green-blue gradient with proper pH scaling
- **Land Cover**: ESA WorldCover 2021 classification with discrete color mapping
- **Elevation with Hillshade**: Terrain visualization combining elevation colors with hillshade relief

### ✅ Map Overlays
- **Map Unit Boundaries**: Toggleable yellow dashed boundary lines
- **Highways**: Deep blue thick lines for major highways
- **Service Roads**: Green thinner lines for service roads
- **Information Center**: Purple star marker at visitor center location
- **Monument Boundary**: Always-visible red dashed boundary
- **Color Previews**: Visual indicators next to overlay checkboxes when enabled

### ✅ Enhanced Visualization
- **Hillshade Integration**: Combines hillshade background with colored elevation data for 3D terrain effect
- **Transparency Handling**: Proper no-data value filtering for clean boundaries
- **Crisp Pixel Rendering**: Optimized for raster data display
- **Interactive Popups**: Click any layer for detailed information
- **Progressive Loading**: Loading screen with status updates

### ✅ UI Components
- **Layer selector** for 6 different map types (Soil Orders, Organic Carbon, pH, Land Cover, Elevation, Satellite)
- **Depth selector** for OC and pH layers with all 6 SSURGO depth intervals
- **Overlay toggles** for boundaries, roads, and points of interest
- **Info modal** with comprehensive dataset information
- **Keyboard shortcuts** (S for sidebar, I for info, Esc to close)
- **Mobile responsive** design with collapsible sidebar

### ✅ Technical Implementation
- **Real TIFF Loading**: Client-side TIFF processing with fallback support
- **Modular Architecture**: Separate utilities for raster, map, UI, and data management
- **Event-Driven Design**: Custom events for component communication
- **Error Handling**: Graceful fallbacks and user feedback
- **Performance Optimized**: Caching and efficient rendering

## How to Test the Application

1. **Start the development server**:
   ```bash
   cd "/mnt/c/Users/redst/Documents/CSNM Map Apps/CSNM-interactive-map"
   python3 serve.py
   ```

2. **Open your browser** to `http://localhost:8000`

3. **Test different layer types**:
   - **Soil Orders**: Default view showing colored soil classifications
   - **Organic Carbon**: Enhanced color scheme with 6 depth levels
   - **Soil pH**: pH gradient visualization with depth selection
   - **Land Cover**: ESA WorldCover 2021 with 11 land cover classes
   - **Elevation**: Terrain colors with hillshade relief for 3D effect
   - **Satellite**: Standard satellite imagery base layer

4. **Test overlay controls**:
   - **Map Unit Boundaries**: Toggle yellow dashed polygon boundaries
   - **Highways**: Show/hide deep blue highway lines
   - **Service Roads**: Toggle green service road lines
   - **Information Center**: Purple star marker at visitor center
   - **Color Previews**: Notice color indicators appear when overlays are checked

5. **Test enhanced elevation**:
   - Select "Elevation" to see combined hillshade + elevation colors
   - Notice the 3D terrain effect from hillshade blending
   - Transparent areas outside monument boundary
   - Click anywhere for elevation values in meters

## Data Sources and Processing

**Soil Polygon Data**: Real SSURGO data from CSNM in `CSNM_Polygons_WGS84.geojson`
- Dominant component filtering (majcompflag = "Yes" or highest comppct_r)
- Soil order extraction from taxorder field
- WGS84 reprojected for web mapping

**Raster Data**: Multi-format TIFF support
- **Organic Carbon**: Multi-depth files (0-5cm through 100-200cm)
- **Soil pH**: H2O pH values across all depth intervals  
- **Land Cover**: ESA WorldCover 2021 10m resolution
- **Elevation**: USGS 10m DEM with accompanying hillshade

**Vector Overlays**:
- Highway and service road networks
- Monument boundary polygon
- Points of interest (Information Center)

## Architecture

The application uses a clean modular architecture:

- **`js/config.js`** - Configuration, constants, and color schemes
- **`js/data-loader.js`** - Asynchronous data loading with caching
- **`js/raster-utils.js`** - TIFF processing, color mapping, and hillshade blending
- **`js/map-utils.js`** - Leaflet integration, layer management, and legends
- **`js/ui-controls.js`** - UI event handling and state management
- **`js/app.js`** - Main application orchestration and event coordination

## Technical Features

### Enhanced Organic Carbon Visualization
- **3-Tier Color System**: Light cream → orange/brown → dark brown
- **Better Contrast**: Improved differentiation between low and high values
- **Depth-Specific**: Separate color scaling for each depth interval

### Hillshade + Elevation Integration
- **Dual Layer Approach**: Grayscale hillshade background + colored elevation overlay
- **Pixel-Level Blending**: Custom algorithm combines elevation colors with hillshade intensity
- **3D Terrain Effect**: Creates realistic topographic relief visualization
- **Smart Transparency**: Proper no-data handling for clean monument boundaries

### Real TIFF Processing
- **Client-Side**: Uses GeoTIFF.js for browser-based TIFF reading
- **Multi-Band Support**: Handles complex multi-depth soil rasters
- **Fallback System**: Graceful degradation if TIFF loading fails
- **Optimized Rendering**: Crisp pixel display with proper color mapping

### Land Cover Classification
- **ESA WorldCover 2021**: Complete 11-class land cover system
- **Discrete Colors**: Scientifically accurate color scheme for each class
- **Interactive Labels**: Click for land cover class information

## Browser Console

Open Developer Tools (F12) and check the Console for:
- TIFF loading progress and data analysis
- Layer switching confirmations
- Hillshade blending status
- Performance timing information
- Debug data about raster value ranges

## Recent Updates

### ✨ NEW: PRISM Climate Data Integration (v0.2.0)
**Real-time Climate Monitoring**:
- **Monthly Climate Data**: Access last 12 months of PRISM AN81M data
- **Climate Variables**: Temperature and precipitation visualization
- **Time Series Animation**: Play through monthly data with smooth transitions
- **Automatic Updates**: Checks for new data on the 15th of each month
- **Monument-Clipped Data**: All data properly cropped to CSNM boundaries

**Climate Visualization Features**:
- Temperature maps with blue-white-red color scheme (-10°C to 40°C)
- Precipitation maps with brown-white-blue scheme (0-500mm)
- Interactive month slider with play/pause animation
- Click anywhere for detailed climate time series

**Technical Implementation**:
- Client-side GeoTIFF processing with boundary clipping
- IndexedDB caching for offline access and performance
- Progressive loading with ~50MB total storage
- Web Services API integration with PRISM Oregon State

**Enhanced Terrain Visualization**:
- Combined hillshade and elevation for realistic 3D effect
- Proper transparency handling outside monument boundaries
- Pixel-level color blending algorithms

**Improved Organic Carbon Display**:
- 3-tier color scheme for better extreme value differentiation
- Enhanced legend with color gradients
- Depth-specific color optimization

**Complete Overlay System**:
- Highway and service road networks
- Information Center purple star marker
- Color preview indicators in UI
- Comprehensive toggle controls

**Fixed Data Issues**:
- Eliminated gray background boxes around raster data
- Proper no-data value filtering for all raster types
- Transparent areas outside monument boundaries