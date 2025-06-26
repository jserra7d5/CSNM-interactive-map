# AQP Soil Profile Integration Guide

## Overview

This guide explains the integration of the AQP (Algorithms for Quantitative Pedology) package with the Cascade-Siskiyou Soil Explorer Shiny application. The integration adds NRCS soil profile visualization capabilities that appear below the existing soil depth profiles when users click on map units.

## Key Features Added

- **NRCS Soil Profile Visualization**: Uses the `aqp` package to create professional soil profile diagrams
- **Automatic Data Fetching**: Integrates with `soilDB` package to fetch Official Series Description (OSD) data
- **Interactive Map Integration**: Profiles appear when clicking on soil map units
- **Horizon Depth Visualization**: Shows horizon boundaries with proper depth labels
- **Soil Color Rendering**: Displays accurate Munsell soil colors
- **Profile Summary Tables**: Provides detailed information about each soil series

## Files Modified/Created

### New Files Created:
1. **`soil_aqp_module.R`** - Main AQP integration module
2. **`test_aqp_integration.R`** - Test script for integration
3. **`AQP_INTEGRATION_GUIDE.md`** - This documentation

### Modified Files:
1. **`app.R`** - Added soil_aqp_module.R to source list
2. **`ui_module.R`** - Added NRCS profile display UI components
3. **`server_module.R`** - Integrated AQP functionality with existing server logic

## Dependencies

The integration requires these additional R packages:
- `aqp` - Core soil profile functionality
- `soilDB` - NRCS data fetching
- `RColorBrewer` - Additional color palettes

These packages are automatically installed when the AQP module is initialized.

## How It Works

### 1. Map Unit Click Detection
When a user clicks on a soil map unit polygon:
- The existing `handle_shape_click()` function is enhanced to store component data
- Component information is stored in reactive values for AQP processing

### 2. Soil Series Extraction
The `extract_soil_series_from_components()` function:
- Extracts soil series names from the clicked map unit
- Prioritizes major components (majcompflag = "Yes")
- Limits to top 3-4 components to avoid overcrowding
- Cleans and standardizes series names for NRCS lookup

### 3. NRCS Data Fetching
The `fetch_nrcs_soil_profiles()` function:
- Uses `soilDB::fetchOSD()` to retrieve official soil series data
- Fetches both soil profile data and extended metadata
- Handles error cases gracefully (no internet, invalid series names)
- Prepares soil colors using Munsell color conversion

### 4. Profile Visualization
The `create_aqp_soil_profile_plot()` function:
- Creates publication-quality soil profile diagrams
- Optimizes layout for sidebar display (350px width)
- Shows horizon depths with proper labeling
- Displays accurate soil colors
- Adjusts sizing based on number of profiles

### 5. UI Integration
The enhanced UI includes:
- Conditional panels that show/hide based on data availability
- Loading indicators during NRCS data fetching
- Profile summary tables with component percentages
- Error handling for cases where no data is available

## Testing the Integration

### Prerequisites
Ensure you have all required packages installed:
```r
install.packages(c("aqp", "soilDB", "RColorBrewer"))
```

### Basic Testing
1. Run the test script:
```r
source("test_aqp_integration.R")
```

2. Start the application:
```r
shiny::runApp()
```

### Interactive Testing
1. Open the application in your browser
2. Click on any soil map unit polygon
3. Observe the NRCS soil profile section appears below the soil depth profile
4. Check that:
   - Loading indicators appear during data fetching
   - Soil profiles display with proper horizon colors and depths
   - Profile summary table shows component information
   - Error messages appear appropriately for areas without data

## Troubleshooting

### Common Issues and Solutions

#### 1. Package Installation Errors
**Problem**: Missing packages or installation failures
**Solution**: 
```r
install.packages(c("aqp", "soilDB", "RColorBrewer"), dependencies = TRUE)
```

#### 2. No Profiles Displayed
**Problem**: NRCS profiles don't appear after clicking map units
**Solutions**:
- Check internet connection (required for NRCS data)
- Verify soil series names are valid
- Check browser console for JavaScript errors
- Ensure component data exists for the clicked map unit

#### 3. Profile Rendering Issues
**Problem**: Profiles appear but look incorrect
**Solutions**:
- Check horizon depth data integrity
- Verify Munsell color conversion
- Adjust plot sizing parameters in `create_aqp_soil_profile_plot()`

#### 4. Performance Issues
**Problem**: Slow profile loading
**Solutions**:
- Reduce number of soil series fetched (currently limited to 4)
- Implement local caching for frequently accessed series
- Add timeout handling for NRCS requests

## Configuration Options

### Soil Series Limits
To change the number of soil series displayed:
```r
# In extract_soil_series_from_components()
top_components <- head(component_names, 4)  # Change 4 to desired number
```

### Plot Sizing
To adjust profile plot dimensions:
```r
# In create_aqp_soil_profile_plot()
profile_width <- case_when(
  n_profiles == 1 ~ 0.7,      # Adjust these values
  n_profiles == 2 ~ 0.45,
  n_profiles <= 4 ~ 0.3,
  TRUE ~ 0.2
)
```

### Color Preferences
To use dry vs. moist soil colors:
```r
# In fetch_nrcs_soil_profiles()
profile_data <- fetch_nrcs_soil_profiles(soil_series, color_state = "dry")  # Change to "dry"
```

## Data Sources

### NRCS Official Series Descriptions (OSD)
- Updated quarterly
- Accessed via SoilWeb API
- Includes horizon depths, designations, colors, pH, and texture
- Site-level attributes include taxonomic classification and acreage estimates

### Data Availability
Not all soil series have complete OSD data. The integration handles:
- Missing soil series gracefully
- Incomplete horizon data
- Network connectivity issues
- API rate limiting

## Performance Considerations

### Data Fetching
- NRCS data fetching is asynchronous with progress indicators
- Profiles are cached within session to avoid repeated requests
- Error handling prevents UI blocking

### Rendering
- Profile plots are optimized for sidebar display
- Dynamic height adjustment based on profile count
- Efficient color processing using vectorized operations

## Future Enhancements

### Potential Improvements
1. **Local Caching** - Store frequently accessed profiles locally
2. **Batch Processing** - Fetch multiple series more efficiently
3. **Enhanced Metadata** - Display additional soil properties
4. **Export Functionality** - Allow profile plot export
5. **Comparison Tools** - Side-by-side profile comparisons

### Integration Opportunities
1. **Soil Taxonomy Hierarchy** - Link to higher-level classifications
2. **Pedon Database** - Integrate with NASIS pedon data
3. **Laboratory Data** - Include measured soil properties
4. **Geomorphic Context** - Add landscape position information

## Support and Resources

### Documentation
- [AQP Package Documentation](https://ncss-tech.github.io/aqp/)
- [soilDB Package Documentation](https://ncss-tech.github.io/soilDB/)
- [NRCS Soil Survey](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/)

### Community Support
- [AQP GitHub Issues](https://github.com/ncss-tech/aqp/issues)
- [soilDB GitHub Issues](https://github.com/ncss-tech/soilDB/issues)
- [NCSS-Tech GitHub Organization](https://github.com/ncss-tech)

This integration provides a powerful enhancement to the soil exploration capabilities of the Cascade-Siskiyou application, bringing professional-grade soil profile visualization directly into the interactive mapping interface.