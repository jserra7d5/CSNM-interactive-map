// =====================================================
// PRISM Climate Normals (1991-2020) - PRECIPITATION ONLY
// Cascade-Siskiyou National Monument
// Simplified version focusing only on precipitation display
// =====================================================

print('Starting PRISM Precipitation Analysis...');

// =====================================================
// Define Monument Boundary with Fallback Options
// =====================================================

// Try the original asset first
var monument;
try {
  monument = ee.FeatureCollection("projects/osu-cascade-sis-nat-mon/assets/CSNM_boundary");
  print('Using project asset for monument boundary');
} catch (e) {
  // Fallback: Create a simple rectangle around CSNM area
  // CSNM coordinates: approximately 42.0°N to 42.3°N, -122.8°W to -122.1°W
  var coords = [
    [-122.8, 42.0],
    [-122.1, 42.0], 
    [-122.1, 42.3],
    [-122.8, 42.3],
    [-122.8, 42.0]
  ];
  monument = ee.FeatureCollection([
    ee.Feature(ee.Geometry.Polygon([coords]), {name: 'CSNM_boundary'})
  ]);
  print('Using fallback boundary coordinates');
}

// Get the monument geometry for clipping
var monumentGeometry = monument.geometry();

// Center the map on the monument
Map.centerObject(monument, 11);
Map.addLayer(monument, {color: 'red'}, 'Monument Boundary', true, 0.5);

print('Monument boundary loaded successfully');

// =====================================================
// Load PRISM Normal 91m Dataset - PRECIPITATION ONLY
// =====================================================

print('Loading PRISM precipitation data...');

var prism = ee.ImageCollection('OREGONSTATE/PRISM/Norm91m');

// Check what's available in the dataset
var prismInfo = prism.first();
print('First PRISM image info:', prismInfo);
print('Available bands:', prismInfo.bandNames());

// =====================================================
// Process Precipitation Data
// =====================================================

// Method 1: Get annual precipitation (sum of all 12 months)
var precipAnnual = prism.select('ppt')
  .sum()  // Sum all monthly normals to get annual total
  .clip(monumentGeometry)
  .rename('annual_precipitation_mm');

// Method 2: Get monthly precipitation images for different seasons
var monthlyPrecip = ee.List.sequence(1, 12).map(function(month) {
  var monthNum = ee.Number(month);
  return prism.filter(ee.Filter.eq('month', monthNum))
    .select('ppt')
    .first()
    .clip(monumentGeometry)
    .set('month', monthNum);
});

// Convert to ImageCollection
var monthlyPrecipCollection = ee.ImageCollection(monthlyPrecip);

// Calculate seasonal averages
var winterPrecip = monthlyPrecipCollection
  .filter(ee.Filter.inList('month', [12, 1, 2]))
  .mean()
  .clip(monumentGeometry)
  .rename('winter_precipitation_mm');

var springPrecip = monthlyPrecipCollection
  .filter(ee.Filter.inList('month', [3, 4, 5]))
  .mean()
  .clip(monumentGeometry)
  .rename('spring_precipitation_mm');

var summerPrecip = monthlyPrecipCollection
  .filter(ee.Filter.inList('month', [6, 7, 8]))
  .mean()
  .clip(monumentGeometry)
  .rename('summer_precipitation_mm');

var fallPrecip = monthlyPrecipCollection
  .filter(ee.Filter.inList('month', [9, 10, 11]))
  .mean()
  .clip(monumentGeometry)
  .rename('fall_precipitation_mm');

print('Precipitation data processed successfully');

// =====================================================
// Data Inspection and Statistics
// =====================================================

// Function to calculate and print statistics
var calculateStats = function(image, name) {
  var stats = image.reduceRegion({
    reducer: ee.Reducer.mean()
      .combine(ee.Reducer.min(), '', true)
      .combine(ee.Reducer.max(), '', true)
      .combine(ee.Reducer.stdDev(), '', true),
    geometry: monumentGeometry,
    scale: 100,
    maxPixels: 1e9
  });
  
  print(name + ' Statistics:', stats);
  return stats;
};

// Calculate statistics to determine appropriate visualization ranges
print('=== PRECIPITATION STATISTICS ===');
calculateStats(precipAnnual, 'Annual Precipitation');
calculateStats(winterPrecip, 'Winter Precipitation');
calculateStats(springPrecip, 'Spring Precipitation');
calculateStats(summerPrecip, 'Summer Precipitation');
calculateStats(fallPrecip, 'Fall Precipitation');

// =====================================================
// Visualization Parameters - Optimized for CSNM
// =====================================================

// Annual precipitation visualization (adjusted for Pacific Northwest)
var precipAnnualVis = {
  min: 200,
  max: 1500,
  palette: [
    '#8B4513', // Saddle brown (very dry)
    '#CD853F', // Sandy brown
    '#F4A460', // Sandy brown
    '#FFE4B5', // Moccasin
    '#FFFFFF', // White (moderate)
    '#E0E0E0', // Light gray
    '#ADD8E6', // Light blue
    '#87CEEB', // Sky blue
    '#4169E1', // Royal blue
    '#0000FF', // Blue
    '#0000CD', // Medium blue
    '#000080'  // Navy (very wet)
  ]
};

// Seasonal precipitation visualization (smaller range)
var precipSeasonalVis = {
  min: 0,
  max: 400,
  palette: [
    '#8B4513', // Brown (dry)
    '#DEB887', // Burlywood
    '#F5DEB3', // Wheat
    '#FFFFFF', // White
    '#B0E0E6', // Powder blue
    '#87CEEB', // Sky blue
    '#4682B4', // Steel blue
    '#0000FF'  // Blue (wet)
  ]
};

// =====================================================
// Add Layers to Map - ALL VISIBLE BY DEFAULT
// =====================================================

print('Adding precipitation layers to map...');

// Add annual precipitation layer (main layer - visible)
Map.addLayer(precipAnnual, precipAnnualVis, 'Annual Precipitation (mm)', true);

// Add seasonal precipitation layers (initially hidden for clarity)
Map.addLayer(winterPrecip, precipSeasonalVis, 'Winter Precipitation (mm)', false);
Map.addLayer(springPrecip, precipSeasonalVis, 'Spring Precipitation (mm)', false);
Map.addLayer(summerPrecip, precipSeasonalVis, 'Summer Precipitation (mm)', false);
Map.addLayer(fallPrecip, precipSeasonalVis, 'Fall Precipitation (mm)', false);

print('Layers added to map successfully');

// =====================================================
// Export Functions - Precipitation Only
// =====================================================

var exportScale = 100;  // 100 meter resolution
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
    maxPixels: 1e10,
    fileFormat: 'GeoTIFF'
  });
  print('Export task created: ' + description);
};

// Export precipitation data
exportImage(precipAnnual, 'PRISM_Precipitation_Annual_CSNM', 'CSNM_precipitation_annual');
exportImage(winterPrecip, 'PRISM_Precipitation_Winter_CSNM', 'CSNM_precipitation_winter');
exportImage(springPrecip, 'PRISM_Precipitation_Spring_CSNM', 'CSNM_precipitation_spring');
exportImage(summerPrecip, 'PRISM_Precipitation_Summer_CSNM', 'CSNM_precipitation_summer');
exportImage(fallPrecip, 'PRISM_Precipitation_Fall_CSNM', 'CSNM_precipitation_fall');

// =====================================================
// Create Multi-band Precipitation Stack
// =====================================================

var precipitationStack = ee.Image.cat([
  precipAnnual,
  winterPrecip,
  springPrecip,
  summerPrecip,
  fallPrecip
]);

// Export the stacked precipitation image
Export.image.toDrive({
  image: precipitationStack,
  description: 'PRISM_Precipitation_Stack_CSNM',
  fileNamePrefix: 'CSNM_precipitation_all_seasons',
  region: monumentGeometry,
  scale: exportScale,
  crs: exportCRS,
  maxPixels: 1e10,
  fileFormat: 'GeoTIFF'
});

// =====================================================
// Final Status and Instructions
// =====================================================

print('====================================');
print('PRISM PRECIPITATION ANALYSIS COMPLETE');
print('====================================');
print('✓ Monument boundary loaded');
print('✓ PRISM precipitation data processed');
print('✓ Annual precipitation layer displayed');
print('✓ Seasonal precipitation layers available');
print('✓ Export tasks created');
print('');
print('NEXT STEPS:');
print('1. Check the map for the precipitation display');
print('2. Toggle seasonal layers on/off as needed');
print('3. Run export tasks from the Tasks tab');
print('4. Adjust visualization parameters if needed');
print('');
print('Data Range Info:');
print('- Annual: 200-1500mm range optimized for CSNM');
print('- Seasonal: 0-400mm range for monthly averages');
print('- Resolution: 100m (~4km native PRISM resolution)');