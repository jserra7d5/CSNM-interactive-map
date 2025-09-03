// =====================================================
// PRISM Climate Normals (1991-2020) Data Extraction
// Cascade-Siskiyou National Monument
// =====================================================

// Define the monument boundary
// Replace this with your actual monument boundary definition
var monument = ee.FeatureCollection("USFS/GTAC/02/ADMINISTRATIVE/BoundariesNationalMonuments")
  .filter(ee.Filter.eq('MONUMENTNAME', 'Cascade-Siskiyou'));

// Or if you have a custom boundary, use something like:
// var monument = ee.FeatureCollection('users/yourUsername/CSNM_boundary');

// Get the monument geometry for clipping
var monumentGeometry = monument.geometry();

// Center the map on the monument
Map.centerObject(monument, 11);
Map.addLayer(monument, {color: 'red'}, 'Monument Boundary', true, 0.5);

// =====================================================
// Load PRISM Normal 91m Dataset (30-year averages 1991-2020)
// =====================================================

var prism = ee.ImageCollection('OREGONSTATE/PRISM/Norm91m');

// Since this is a normal dataset, we need to get the monthly data
// and create annual or seasonal averages as needed

// =====================================================
// Extract Individual Climate Variables
// =====================================================

// 1. PRECIPITATION (mm)
var precipitation = prism.select('ppt')
  .mean()  // Average across all months
  .clip(monumentGeometry);

// 2. MEAN TEMPERATURE (°C)
var tmean = prism.select('tmean')
  .mean()
  .clip(monumentGeometry);

// 3. MINIMUM TEMPERATURE (°C)
var tmin = prism.select('tmin')
  .mean()
  .clip(monumentGeometry);

// 4. MAXIMUM TEMPERATURE (°C)
var tmax = prism.select('tmax')
  .mean()
  .clip(monumentGeometry);

// 5. MINIMUM VAPOR PRESSURE DEFICIT (hPa)
var vpdmin = prism.select('vpdmin')
  .mean()
  .clip(monumentGeometry);

// 6. MAXIMUM VAPOR PRESSURE DEFICIT (hPa)
var vpdmax = prism.select('vpdmax')
  .mean()
  .clip(monumentGeometry);

// 7. SOLAR RADIATION - HORIZONTAL (MJ/m²/day)
var soltotal = prism.select('soltotal')
  .mean()
  .clip(monumentGeometry);

// 8. SOLAR RADIATION - SLOPED (MJ/m²/day)
var solslope = prism.select('solslope')
  .mean()
  .clip(monumentGeometry);

// 9. SOLAR RADIATION - CLEAR SKY (MJ/m²/day)
var solclear = prism.select('solclear')
  .mean()
  .clip(monumentGeometry);

// =====================================================
// Create Monthly Composites for Seasonal Analysis
// =====================================================

// Function to get data for specific months
var getMonthlyData = function(variable, monthList) {
  return prism.filter(ee.Filter.inList('month', monthList))
    .select(variable)
    .mean()
    .clip(monumentGeometry);
};

// Seasonal precipitation
var winterPrecip = getMonthlyData('ppt', [12, 1, 2]);  // Dec, Jan, Feb
var springPrecip = getMonthlyData('ppt', [3, 4, 5]);   // Mar, Apr, May
var summerPrecip = getMonthlyData('ppt', [6, 7, 8]);   // Jun, Jul, Aug
var fallPrecip = getMonthlyData('ppt', [9, 10, 11]);   // Sep, Oct, Nov

// Seasonal temperatures
var winterTemp = getMonthlyData('tmean', [12, 1, 2]);
var springTemp = getMonthlyData('tmean', [3, 4, 5]);
var summerTemp = getMonthlyData('tmean', [6, 7, 8]);
var fallTemp = getMonthlyData('tmean', [9, 10, 11]);

// =====================================================
// Visualization Parameters
// =====================================================

var precipVis = {
  min: 0,
  max: 2000,
  palette: ['red', 'orange', 'yellow', 'green', 'cyan', 'blue', 'purple']
};

var tempVis = {
  min: -10,
  max: 30,
  palette: ['blue', 'cyan', 'green', 'yellow', 'orange', 'red']
};

var vpdVis = {
  min: 0,
  max: 50,
  palette: ['blue', 'green', 'yellow', 'orange', 'red']
};

var solarVis = {
  min: 0,
  max: 30,
  palette: ['purple', 'blue', 'green', 'yellow', 'orange', 'red']
};

// =====================================================
// Add Layers to Map
// =====================================================

Map.addLayer(precipitation, precipVis, 'Annual Precipitation (mm)', false);
Map.addLayer(tmean, tempVis, 'Mean Temperature (°C)', false);
Map.addLayer(tmin, tempVis, 'Min Temperature (°C)', false);
Map.addLayer(tmax, tempVis, 'Max Temperature (°C)', false);
Map.addLayer(vpdmin, vpdVis, 'Min VPD (hPa)', false);
Map.addLayer(vpdmax, vpdVis, 'Max VPD (hPa)', false);
Map.addLayer(soltotal, solarVis, 'Solar Radiation - Total', false);
Map.addLayer(solslope, solarVis, 'Solar Radiation - Sloped', false);
Map.addLayer(solclear, solarVis, 'Solar Radiation - Clear Sky', false);

// Add seasonal layers
Map.addLayer(winterPrecip, precipVis, 'Winter Precipitation', false);
Map.addLayer(summerTemp, tempVis, 'Summer Temperature', false);

// =====================================================
// Export Functions
// =====================================================

// Set export parameters
var exportScale = 100;  // meters - adjust as needed
var exportCRS = 'EPSG:4326';  // WGS84

// Function to export an image
var exportImage = function(image, description, fileNamePrefix) {
  Export.image.toDrive({
    image: image,
    description: description,
    fileNamePrefix: fileNamePrefix,
    region: monumentGeometry,
    scale: exportScale,
    crs: exportCRS,
    maxPixels: 1e13,
    fileFormat: 'GeoTIFF'
  });
};

// Export all climate variables
exportImage(precipitation, 'PRISM_Precipitation_Annual', 'CSNM_precipitation_annual');
exportImage(tmean, 'PRISM_Temperature_Mean', 'CSNM_temperature_mean');
exportImage(tmin, 'PRISM_Temperature_Min', 'CSNM_temperature_min');
exportImage(tmax, 'PRISM_Temperature_Max', 'CSNM_temperature_max');
exportImage(vpdmin, 'PRISM_VPD_Min', 'CSNM_vpd_min');
exportImage(vpdmax, 'PRISM_VPD_Max', 'CSNM_vpd_max');
exportImage(soltotal, 'PRISM_Solar_Total', 'CSNM_solar_total');
exportImage(solslope, 'PRISM_Solar_Sloped', 'CSNM_solar_sloped');
exportImage(solclear, 'PRISM_Solar_Clear', 'CSNM_solar_clear');

// Export seasonal data
exportImage(winterPrecip, 'PRISM_Precipitation_Winter', 'CSNM_precipitation_winter');
exportImage(springPrecip, 'PRISM_Precipitation_Spring', 'CSNM_precipitation_spring');
exportImage(summerPrecip, 'PRISM_Precipitation_Summer', 'CSNM_precipitation_summer');
exportImage(fallPrecip, 'PRISM_Precipitation_Fall', 'CSNM_precipitation_fall');

exportImage(winterTemp, 'PRISM_Temperature_Winter', 'CSNM_temperature_winter');
exportImage(springTemp, 'PRISM_Temperature_Spring', 'CSNM_temperature_spring');
exportImage(summerTemp, 'PRISM_Temperature_Summer', 'CSNM_temperature_summer');
exportImage(fallTemp, 'PRISM_Temperature_Fall', 'CSNM_temperature_fall');

// =====================================================
// Calculate Statistics
// =====================================================

// Function to calculate and print statistics
var calculateStats = function(image, name) {
  var stats = image.reduceRegion({
    reducer: ee.Reducer.mean()
      .combine(ee.Reducer.min(), '', true)
      .combine(ee.Reducer.max(), '', true)
      .combine(ee.Reducer.stdDev(), '', true),
    geometry: monumentGeometry,
    scale: exportScale,
    maxPixels: 1e13
  });
  
  print(name + ' Statistics:', stats);
};

// Calculate statistics for each variable
calculateStats(precipitation, 'Precipitation');
calculateStats(tmean, 'Mean Temperature');
calculateStats(tmin, 'Min Temperature');
calculateStats(tmax, 'Max Temperature');
calculateStats(vpdmin, 'Min VPD');
calculateStats(vpdmax, 'Max VPD');
calculateStats(soltotal, 'Solar Total');

// =====================================================
// Create Combined Multi-band Image for Export
// =====================================================

var climateStack = ee.Image.cat([
  precipitation.rename('precipitation_mm'),
  tmean.rename('temperature_mean_C'),
  tmin.rename('temperature_min_C'),
  tmax.rename('temperature_max_C'),
  vpdmin.rename('vpd_min_hPa'),
  vpdmax.rename('vpd_max_hPa'),
  soltotal.rename('solar_total_MJm2day'),
  solslope.rename('solar_sloped_MJm2day'),
  solclear.rename('solar_clear_MJm2day')
]);

// Export the stacked image
Export.image.toDrive({
  image: climateStack,
  description: 'PRISM_Climate_Stack_All_Variables',
  fileNamePrefix: 'CSNM_climate_normals_stack',
  region: monumentGeometry,
  scale: exportScale,
  crs: exportCRS,
  maxPixels: 1e13,
  fileFormat: 'GeoTIFF'
});

print('Script complete! Check the Tasks tab to run exports.');
print('Variables extracted (excluding mean dew point and cloud transmittance):');
print('1. Precipitation');
print('2. Mean Temperature');
print('3. Minimum Temperature');
print('4. Maximum Temperature');
print('5. Minimum Vapor Pressure Deficit');
print('6. Maximum Vapor Pressure Deficit');
print('7. Solar Radiation (horizontal/total)');
print('8. Solar Radiation (sloped)');
print('9. Solar Radiation (clear sky)');