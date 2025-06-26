# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a modular R Shiny application for exploring soil data in the Cascade-Siskiyou National Monument. The application displays interactive maps with soil organic carbon (OC) and pH data at multiple depth levels, along with soil polygon classifications and profile visualizations.

## Running the Application

```r
# Main entry point - run the Shiny app
shiny::runApp("app.R")

# Test individual components in R console
source("config.R")
source("data_module.R")
test_data <- load_and_prepare_data()
```

## Architecture

The application follows a modular architecture with clear separation of concerns:

- **app.R** (34 lines): Main entry point that loads libraries, sources modules, and launches the app
- **config.R** (44 lines): Centralized configuration including map projections, depth levels, color palettes, and file paths
- **data_module.R** (259 lines): Data loading and processing functions for rasters, polygons, and soil components
- **ui_module.R** (277 lines): User interface components and dashboard layout
- **server_module.R** (386 lines): Server logic, reactive values, and event handling
- **map_utils.R** (330 lines): Leaflet map creation, spatial operations, and layer management
- **plot_utils.R** (229 lines): Plotly visualization functions for soil profiles
- **soil_sketch_module.R** (445 lines): Additional soil sketch functionality
- **soil_order_export.R** (158 lines): Export utilities for soil order data

## Key Data Sources

The application expects these data files to be present:
- `CSNM_OC_AllDepths.tif`: Multi-band raster with organic carbon data at 6 depth levels
- `CSNM_pH_AllDepths.tif`: Multi-band raster with pH data at 6 depth levels  
- `CSNM_Polygons_with_Data.geojson`: Soil polygon boundaries with classification data
- `Mapunit_OR_table.csv`: Soil component mapping table

## Configuration Management

All settings are centralized in `config.R`:
- **PROJECTION_CRS**: Map projection (EPSG:3857)
- **MAP_CENTER**: Default map view coordinates
- **DEPTH_LEVELS**: Six depth intervals (0-5cm to 100-200cm) with color palettes
- **SOIL_ORDER_COLORS**: Color scheme for soil classification visualization
- **DATA_PATHS**: File path configuration for all data sources

## Core Functions

### Data Loading (`data_module.R`)
- `load_and_prepare_data()`: Main orchestrator loading all spatial data
- `load_raster_data()`: Processes multi-band OC and pH raster stacks
- `load_polygon_data()`: Handles soil polygons and component classifications

### Map Operations (`map_utils.R`) 
- `create_base_map()`: Initialize Leaflet map with base layers
- `extract_soil_profile()`: Point-based data extraction from rasters
- `add_polygon_layers()` / `add_raster_layers()`: Dynamic layer management

### Server Logic (`server_module.R`)
- `create_server()`: Main server function coordinating all reactive logic
- `handle_map_click()` / `handle_shape_click()`: User interaction handlers
- `setup_interaction_observers()`: Event observer configuration

## Development Workflow

1. **Configuration changes**: Edit `config.R` for colors, projections, or file paths
2. **UI modifications**: Update `ui_module.R` for layout and control changes
3. **Data processing**: Modify `data_module.R` for new data sources or processing logic
4. **Map functionality**: Edit `map_utils.R` for spatial operations and layer management
5. **Visualization updates**: Change `plot_utils.R` for chart modifications
6. **Server logic**: Update `server_module.R` for reactive behavior changes

## Testing Components

```r
# Test data loading in isolation
source("config.R")
source("data_module.R")
test_data <- load_and_prepare_data()

# Test UI components
source("config.R") 
source("ui_module.R")
test_ui <- create_dashboard_ui()

# Test spatial functions
source("config.R")
source("map_utils.R")
profile <- extract_soil_profile(42.1, -122.466, test_data$rasters, "oc")
```

## R Project Configuration

The project uses RStudio with:
- 2-space indentation
- UTF-8 encoding
- Code indexing enabled
- Standard workspace settings