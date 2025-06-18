# Cascade-Siskiyou Soil Explorer - Refactored Structure

This document explains the new modular structure of the Shiny application for better maintainability and organization.

## File Structure

```
├── app.R                 # Main application entry point
├── config.R              # Configuration and constants
├── data_module.R          # Data loading and processing functions
├── ui_module.R            # User interface components
├── map_utils.R            # Map and spatial utilities
├── plot_utils.R           # Plotting and visualization utilities
├── server_module.R        # Server logic functions
└── README.md             # This file
```

## Module Descriptions

### 📁 `app.R` - Main Application
- **Purpose**: Entry point that loads libraries and sources all modules
- **Size**: ~20 lines (down from 800+)
- **Contains**: Library imports, module sourcing, UI/server definitions, app launch

### 📁 `config.R` - Configuration & Constants
- **Purpose**: Centralized configuration management
- **Contains**:
  - Map projection settings and center coordinates
  - Depth level definitions and color palettes
  - Soil order color schemes
  - File paths for data sources
- **Benefits**: Easy to modify settings without hunting through code

### 📁 `data_module.R` - Data Processing
- **Purpose**: All data loading and preprocessing functions
- **Key Functions**:
  - `load_and_prepare_data()` - Main data loading orchestrator
  - `load_raster_data()` - Handle OC and pH raster processing
  - `load_polygon_data()` - Process soil polygons and components
  - `process_oc_layer()` / `process_ph_layer()` - Individual layer processing
- **Benefits**: Isolated data logic, easier testing and debugging

### 📁 `ui_module.R` - User Interface
- **Purpose**: All UI component creation functions
- **Key Functions**:
  - `create_dashboard_ui()` - Main UI orchestrator
  - `create_sidebar()` - Sidebar with controls
  - `create_layer_controls()` - Map layer selection inputs
  - `create_legend_html()` - Soil order legend
- **Benefits**: Clean separation of UI logic, reusable components

### 📁 `map_utils.R` - Spatial & Mapping Functions
- **Purpose**: Leaflet map creation and spatial operations
- **Key Functions**:
  - `create_base_map()` - Base leaflet map
  - `add_polygon_layers()` / `add_raster_layers()` - Layer management
  - `extract_soil_profile()` - Point-based data extraction
  - `handle_layer_switching()` - Dynamic layer visibility
- **Benefits**: Focused spatial functionality, easier map feature development

### 📁 `plot_utils.R` - Visualization Functions
- **Purpose**: Plotly chart creation and data formatting
- **Key Functions**:
  - `create_soil_profile_plot()` - Main plotting orchestrator
  - `create_oc_plot()` / `create_ph_plot()` - Individual property plots
  - `format_selection_info()` - Text formatting for selected points
- **Benefits**: Separated plotting logic, easier to modify visualizations

### 📁 `server_module.R` - Server Logic
- **Purpose**: Reactive logic and event handling
- **Key Functions**:
  - `create_server()` - Main server orchestrator
  - `setup_map_outputs()` - Map-related reactive outputs
  - `handle_map_click()` / `handle_shape_click()` - User interaction handlers
  - `setup_interaction_observers()` - Event observer setup
- **Benefits**: Organized reactive logic, easier debugging of interactions

## Key Improvements

### 🎯 **Maintainability**
- **Before**: 800+ line single file
- **After**: 7 focused modules (~100-150 lines each)
- **Benefit**: Much easier to find and modify specific functionality

### 🔧 **Debugging**
- **Before**: Complex interactions buried in large file
- **After**: Clear separation of concerns with focused functions
- **Benefit**: Easier to isolate and fix issues

### 🔄 **Reusability**
- **Before**: Monolithic functions doing multiple things
- **After**: Single-purpose functions that can be reused
- **Benefit**: Components can be easily reused or swapped out

### 📚 **Readability**
- **Before**: Long file requiring lots of scrolling
- **After**: Logical grouping of related functions
- **Benefit**: Faster understanding for new developers

### ⚡ **Performance**
- **Before**: All functions loaded regardless of use
- **After**: Modular loading (could add conditional loading later)
- **Benefit**: Potential for lazy loading and better memory management

## Usage Notes

### 🚀 **Running the Application**
```r
# Simply run the main file - it handles all sourcing
shiny::runApp("app.R")
```

### 🛠 **Making Changes**

1. **Configuration changes**: Edit `config.R`
2. **UI modifications**: Edit `ui_module.R`
3. **Map functionality**: Edit `map_utils.R`
4. **Data processing**: Edit `data_module.R`
5. **Plotting changes**: Edit `plot_utils.R`
6. **Server logic**: Edit `server_module.R`

### 🧪 **Testing Individual Components**
```r
# Test data loading
source("config.R")
source("data_module.R")
test_data <- load_and_prepare_data()

# Test UI components
source("config.R")
source("ui_module.R")
test_ui <- create_dashboard_ui()
```

## Future Enhancements

With this modular structure, future improvements become much easier:

- **Add new data sources**: Modify `data_module.R`
- **New visualization types**: Add to `plot_utils.R`
- **Additional map layers**: Extend `map_utils.R`
- **UI improvements**: Update `ui_module.R`
- **Performance optimizations**: Target specific modules
- **Unit testing**: Test individual modules in isolation

This refactored structure provides a solid foundation for ongoing development and maintenance of the soil mapping application.