// Configuration and Constants
// Soils of the Siskiyous Configuration

// Application Version
const APP_VERSION = '0.2.0';  // Updated with PRISM climate data integration

// Map Settings
const CONFIG = {
    // Map center and zoom
    mapCenter: [42.1, -122.466],
    mapZoom: 11,
    
    // Tile loading bounds - expanded to allow full map viewing
    // Set to null to load tiles everywhere, or expand bounds as needed
    tileBounds: null,  // [[41.7, -123.0], [42.5, -122.0]] for wider area
    
    // Projection
    crs: L.CRS.EPSG3857,
    
    // Depth configuration
    depthLevels: {
        labels: ["0-5 cm", "5-15 cm", "15-30 cm", "30-60 cm", "60-100 cm", "100-200 cm"],
        values: ["0_5", "5_15", "15_30", "30_60", "60_100", "100_200"],
        bandNames: {
            oc: [
                "soc_0.5cm_mean", "soc_5.15cm_mean", "soc_15.30cm_mean",
                "soc_30.60cm_mean", "soc_60.100cm_mean", "soc_100.200cm_mean"
            ],
            ph: [
                "phh2o_0.5cm_mean", "phh2o_5.15cm_mean", "phh2o_15.30cm_mean",
                "phh2o_30.60cm_mean", "phh2o_60.100cm_mean", "phh2o_100.200cm_mean"
            ],
            meanTemp: [
                "temp_0.5cm_mean", "temp_5.15cm_mean", "temp_15.30cm_mean",
                "temp_30.60cm_mean", "temp_60.100cm_mean", "temp_100.200cm_mean"
            ]
        },
        depthsCm: [2.5, 10, 22.5, 45, 80, 150], // Mid-points for profile plotting
        
        // Color palettes for different depths and properties
        colorPalettes: {
            oc: {
                surface: ["#FFF8DC", "#DEB887", "#D2691E", "#8B4513", "#654321"],
                shallow: ["#F5F5DC", "#DDD7AA", "#C19A6B", "#8B7355", "#5D4E37"],
                deep: ["#F0F8FF", "#B0C4DE", "#4682B4", "#2F4F4F", "#1C1C1C"]
            },
            ph: ["#0000FF", "#4169E1", "#00BFFF", "#32CD32", "#FFFF00", "#FFA500", "#FF4500", "#FF0000"],
            meanTemp: ["#0000FF", "#4169E1", "#00BFFF", "#32CD32", "#FFFF00", "#FFA500", "#FF4500", "#FF0000"]
        }
    },
    
    // Soil order color palette (matching R configuration)
    soilOrderColors: {
        "Alfisols": "#B5D55D",
        "Andisols": "#EA028C", 
        "Aridisols": "#FDDCB9",
        "Entisols": "#75CDD6",
        "Gelisols": "#31A4BF",
        "Histosols": "#AE5044",
        "Inceptisols": "#CB7662",
        "Mollisols": "#00A551",
        "Oxisols": "#EC1F25",
        "Spodosols": "#D4BEC4",
        "Ultisols": "#FAAF19",
        "Vertisols": "#FFF100",
        // Non-soil areas
        "Rock outcrop": "#696969",      // Dark gray for exposed bedrock
        "Rubble land": "#A9A9A9",       // Light gray for rocky debris
        "Water": "#1E90FF",             // Blue for water bodies
        "Non-soil area": "#D3D3D3",     // Light gray for other non-soil areas
        "Unknown": "#808080"            // Keep as fallback
    },
    
    // Family Particle Class colors
    // Colors chosen to reflect texture: coarser (sandy) = warmer/lighter, finer (clayey) = cooler/darker
    particleSizeColors: {
        // Fine textures (high clay content)
        "fine": "#2C3E50",           // Dark blue-gray
        "very-fine": "#1A252F",      // Very dark blue-gray
        "fine-silty": "#34495E",     // Medium blue-gray
        "fine-loamy": "#5D6D7E",     // Light blue-gray
        
        // Medium textures
        "loamy": "#7B8D9F",          // Light gray-blue
        "coarse-loamy": "#95A5A6",   // Light gray
        
        // Coarse textures (sandy)
        "sandy": "#D4B896",          // Sandy brown
        "sandy-skeletal": "#E8D5B8", // Light sandy
        
        // Skeletal classes (rocky)
        "loamy-skeletal": "#8B7355", // Brown
        "clayey-skeletal": "#654321", // Dark brown
        "medial-skeletal": "#A0522D", // Sienna brown
        
        // Special classes
        "clayey": "#4A5568",         // Gray-blue
        "medial": "#CD853F",         // Peru (volcanic)
        "fine-loamy over clayey": "#6B7280", // Mixed gray
        "not used": "#CCCCCC",       // Light gray
        "Unknown": "#808080"         // Gray
    },

    // Parent Material Classification Colors (based on geomdesc field)
    parentMaterialColors: {
        // Alluvial materials (deposited by flowing water)
        "Alluvial": "#4A90E2",           // Blue for water-deposited
        "Fluvial": "#357ABD",            // Darker blue for river deposits
        
        // Colluvial materials (gravity-deposited on slopes)
        "Colluvial": "#8B7355",          // Brown for slope deposits
        
        // Residual materials (weathered in place)
        "Residual": "#CD853F",           // Peru/tan for weathered bedrock
        "Mountainous": "#654321",        // Dark brown for mountain materials
        
        // Lacustrine materials (lake deposits)
        "Lacustrine": "#6BB6FF",         // Light blue for lake deposits
        
        // Volcanic materials
        "Volcanic": "#D2691E",           // Orange for volcanic materials
        
        // Unknown/other
        "Unknown": "#808080"             // Gray
    },
    
    // NLCD Land Cover Classification Colors
    nlcdColors: {
        11: { color: "#466b9f", name: "Open Water" },
        12: { color: "#d1def8", name: "Perennial Ice/Snow" },
        21: { color: "#dec5c5", name: "Developed, Open Space" },
        22: { color: "#d99282", name: "Developed, Low Intensity" },
        23: { color: "#eb0000", name: "Developed, Medium Intensity" },
        24: { color: "#ab0000", name: "Developed, High Intensity" },
        31: { color: "#b3ac9f", name: "Barren Land" },
        41: { color: "#68ab5f", name: "Deciduous Forest" },
        42: { color: "#1c5f2c", name: "Evergreen Forest" },
        43: { color: "#b5c58f", name: "Mixed Forest" },
        52: { color: "#ccb879", name: "Shrub/Scrub" },
        71: { color: "#dfdfc2", name: "Grassland/Herbaceous" },
        81: { color: "#dcd939", name: "Pasture/Hay" },
        82: { color: "#ab6c28", name: "Cultivated Crops" },
        90: { color: "#b8d9eb", name: "Woody Wetlands" },
        95: { color: "#6c9fb8", name: "Emergent Herbaceous Wetlands" }
    },
    
    // Lithology colors (geological rock types)
    lithologyColors: {
        0: { color: "#356eff", name: "Water" },
        1: { color: "#acb6da", name: "Carbonate" },
        3: { color: "#d6b879", name: "Non-carbonate" },
        4: { color: "#313131", name: "Alkaline intrusive" },
        5: { color: "#eda800", name: "Silicic residual" },
        7: { color: "#616161", name: "Extrusive volcanic" },
        8: { color: "#d6d6d6", name: "Colluvial sediment" },
        9: { color: "#d0ddae", name: "Glacial till clay" },
        10: { color: "#b8d279", name: "Glacial till loam" },
        11: { color: "#d5d378", name: "Glacial till coarse" },
        13: { color: "#141414", name: "Glacial lake sediment fine" },
        14: { color: "#6db155", name: "Glacial outwash coarse" },
        15: { color: "#9b6d55", name: "Hydric" },
        16: { color: "#feeec9", name: "Eolian sediment coarse" },
        17: { color: "#d6b879", name: "Eolian sediment fine" },
        18: { color: "#00b7ec", name: "Saline lake sediment" },
        19: { color: "#ffda90", name: "Alluvium and coastal sediment fine" },
        20: { color: "#f8b28c", name: "Coastal sediment coarse" }
    },
    
    // Elevation color scheme (terrain colors)
    elevationColors: {
        // Colors will be interpolated based on elevation values
        lowColor: "#1e3a8a",     // Deep blue for low elevations
        midLowColor: "#10b981",  // Green for mid-low elevations
        midColor: "#fbbf24",     // Yellow for mid elevations
        midHighColor: "#f97316", // Orange for mid-high elevations
        highColor: "#dc2626",    // Red for high elevations
        peakColor: "#ffffff"     // White for peaks
    },
    
    // Climate variable color schemes - following scientific standards
    climateColors: {
        precipitation: {
            // Brown (dry) to white to blue (wet) - standard meteorological gradient
            // Following NOAA/NWS convention for precipitation maps
            colors: ["#8B4513", "#A0522D", "#CD853F", "#DEB887", "#F5DEB3", "#FFFAF0", "#E0FFFF", "#87CEEB", "#4682B4", "#1E90FF", "#0000CD"],
            min: 0,    // mm - adjusted to include full range
            max: 2000  // mm - adjusted for western US precipitation patterns
        },
        temperature: {
            // Blue (cold) to red (hot) - diverging color scheme matching MEANTEMP raster
            // RdBu diverging color palette from ColorBrewer
            colors: ["#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#fee090", "#fdae61", "#f46d43", "#d73027", "#b40426"],
            min: 0,   // °C
            max: 20   // °C
        },
        temperatureExtreme: {
            // For min/max temperatures with wider range - same diverging palette
            colors: ["#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#fee090", "#fdae61", "#f46d43", "#d73027", "#b40426"],
            min: -5,  // °C
            max: 30   // °C
        },
        vpd: {
            // Green (low deficit/moist) to yellow to red (high deficit/dry) for water stress
            // Following plant stress visualization standards
            colors: ["#006400", "#228B22", "#32CD32", "#7FFF00", "#FFFF00", "#FFD700", "#FFA500", "#FF6347", "#DC143C", "#8B0000"],
            min: 0,   // hPa
            max: 30   // hPa - for vpdMax
        },
        vpdMin: {
            // Specialized range for minimum VPD (nighttime/early morning values)
            // Blue-green (very low deficit) to orange (moderate deficit) for min VPD
            colors: ["#004d40", "#00695c", "#00796b", "#009688", "#26a69a", "#4db6ac", "#80cbc4", "#b2dfdb", "#66bb6a", "#9ccc65", "#d4e157", "#ffee58", "#ffca28", "#ffa726", "#ff7043"],
            min: 0.7, // hPa - actual minimum value in CSNM data
            max: 3.2  // hPa - actual maximum value in CSNM data
        },
        solar: {
            // Purple/blue (low) to yellow/orange (high) for solar radiation
            // Following energy visualization standards
            colors: ["#4B0082", "#6A0DAD", "#8B008B", "#9370DB", "#87CEEB", "#FFFFE0", "#FFD700", "#FFA500", "#FF8C00", "#FF6347"],
            min: 10,  // MJ/m²/day
            max: 25   // MJ/m²/day
        }
    },
    
    // Data source information
    dataSources: {
        ssurgo: {
            name: "SSURGO Data - SoilWeb View",
            description: "Soil Survey Geographic Database - Detailed soil survey data including map unit delineations, soil properties, and interpretations",
            agency: "USDA Natural Resources Conservation Service (NRCS)",
            url: "https://websoilsurvey.nrcs.usda.gov/",
            dataUrl: "https://sdmdataaccess.nrcs.usda.gov/",
            citation: "Soil Survey Staff, Natural Resources Conservation Service, United States Department of Agriculture. Web Soil Survey. Available online. Accessed [date].",
            resolution: "Variable (1:12,000 to 1:63,360 scale)",
            lastUpdate: "Continuously updated"
        },
        soil: {
            name: "Soil Orders",
            description: "USDA Soil Taxonomy classification system at the order level - highest hierarchical level of soil classification",
            agency: "USDA Natural Resources Conservation Service",
            url: "https://www.nrcs.usda.gov/wps/portal/nrcs/main/soils/survey/class/taxonomy/",
            citation: "Soil Survey Staff. 2014. Keys to Soil Taxonomy, 12th ed. USDA-Natural Resources Conservation Service, Washington, DC.",
            resolution: "Derived from SSURGO polygons",
            lastUpdate: "2014 (12th Edition)"
        },
        particleSize: {
            name: "Family Particle Class",
            description: "Soil texture classification at the family level based on particle size distribution - indicates relative proportions of sand, silt, and clay, including coarse fragments",
            agency: "USDA Natural Resources Conservation Service",
            url: "https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/class/taxonomy/",
            citation: "Soil Survey Staff. 2014. Keys to Soil Taxonomy, 12th ed. USDA-Natural Resources Conservation Service, Washington, DC.",
            resolution: "Derived from SSURGO component data",
            lastUpdate: "Continuously updated",
            classes: "Fine, coarse-loamy, loamy-skeletal, medial, and others based on texture and rock fragment content"
        },
        parentMaterial: {
            name: "Parent Material (SSURGO)",
            description: "Parent material classification based on geological setting and depositional environment - indicates the origin and nature of unconsolidated material from which soil forms",
            agency: "USDA Natural Resources Conservation Service",
            url: "https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/edu/",
            citation: "Soil Survey Staff. 2014. Keys to Soil Taxonomy, 12th ed. USDA-Natural Resources Conservation Service, Washington, DC.",
            resolution: "Derived from SSURGO component geomdesc field",
            lastUpdate: "Continuously updated",
            classes: "Alluvial, colluvial, residual, lacustrine, fluvial, volcanic, mountainous materials based on depositional environment"
        },
        oc: {
            name: "Soil Organic Carbon",
            description: "Soil organic carbon content predictions at six standard depth intervals from machine learning models",
            agency: "ISRIC - World Soil Information",
            url: "https://soilgrids.org/",
            dataUrl: "https://maps.isric.org/",
            citation: "Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B., Ribeiro, E., and Rossiter, D.: SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty, SOIL, 7, 217–240, 2021.",
            resolution: "250 meters",
            units: "g/kg",
            lastUpdate: "2020"
        },
        ph: {
            name: "Soil pH",
            description: "Soil pH (H2O) predictions at six standard depth intervals from machine learning models",
            agency: "ISRIC - World Soil Information",
            url: "https://soilgrids.org/",
            dataUrl: "https://maps.isric.org/",
            citation: "Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B., Ribeiro, E., and Rossiter, D.: SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty, SOIL, 7, 217–240, 2021.",
            resolution: "250 meters",
            units: "pH units (0-14 scale)",
            lastUpdate: "2020"
        },
        meanTemp: {
            name: "Mean Temperature",
            description: "PRISM 30-year normal mean temperature (1991-2020)",
            agency: "PRISM Climate Group, Oregon State University",
            url: "https://prism.oregonstate.edu",
            citation: "PRISM Climate Group, Oregon State University, https://prism.oregonstate.edu, data created 4 Feb 2014, accessed Dec 2024",
            resolution: "800 meters",
            units: "°C",
            lastUpdate: "2021",
            band: 2
        },
        nlcd: {
            name: "NLCD Land Cover",
            description: "National Land Cover Database 2024 - Multi-resolution land cover classification for the United States",
            agency: "U.S. Geological Survey (USGS)",
            url: "https://www.mrlc.gov/",
            dataUrl: "https://www.mrlc.gov/data",
            citation: "Dewitz, J., and U.S. Geological Survey, 2024, National Land Cover Database (NLCD) 2024 Products: U.S. Geological Survey data release.",
            resolution: "30 meters",
            lastUpdate: "2024",
            classes: "16 land cover classes including developed, forest, agriculture, and wetlands"
        },
        lithology: {
            name: "Parent Material",
            description: "Provides classes of the general types of parent material of soil on the surface - ecologically-relevant maps of landforms and physiographic diversity for climate adaptation planning",
            agency: "Conservation Science Partners (CSP)",
            url: "https://developers.google.com/earth-engine/datasets/catalog/CSP_ERGo_1_0_US_lithology",
            dataUrl: "https://www.csp-inc.org/",
            citation: "Theobald, D. M., et al. (2015). Ecologically-relevant maps of landforms and physiographic diversity for climate adaptation planning. PLoS ONE, 10(12): e0143619.",
            resolution: "100 meters",
            lastUpdate: "2011",
            dateRange: "2006-2011",
            license: "CC-BY-NC-SA-4.0",
            classes: "20 classes including water, carbonate/non-carbonate rocks, volcanic materials, glacial deposits, and various sediment types"
        },
        elevation: {
            name: "Digital Elevation Model",
            description: "USGS 3D Elevation Program (3DEP) - Seamless DEM providing elevation data",
            agency: "U.S. Geological Survey",
            citation: "U.S. Geological Survey, 2023, USGS 3D Elevation Program Digital Elevation Model: U.S. Geological Survey.",
            resolution: "30 meters (1 arc-second)",
            units: "meters above sea level",
            lastUpdate: "Continuously updated",
            verticalAccuracy: "±3.04 meters RMSE"
        },
        satellite: {
            name: "Satellite Imagery",
            description: "High-resolution satellite and aerial imagery from various sources",
            agency: "Esri, Maxar, Earthstar Geographics, and the GIS User Community",
            url: "https://www.arcgis.com/home/item.html?id=10df2279f9684e4a9f6a7f08febac2a9",
            citation: "Esri, Maxar, Earthstar Geographics, and the GIS User Community",
            resolution: "Variable (0.3m to 1m typical)",
            lastUpdate: "Continuously updated",
            coverage: "Global"
        },
        boundaries: {
            name: "Map Unit Boundaries",
            description: "SSURGO soil map unit polygon boundaries delineating areas of similar soils",
            agency: "USDA Natural Resources Conservation Service",
            url: "https://websoilsurvey.nrcs.usda.gov/",
            citation: "Soil Survey Staff, Natural Resources Conservation Service, United States Department of Agriculture. Soil Survey Geographic (SSURGO) Database. Available online. Accessed [date].",
            resolution: "1:12,000 to 1:63,360 scale",
            lastUpdate: "Continuously updated"
        },
        // PRISM Climate Normals (30-year averages 1991-2020)
        precipitation: {
            name: "Annual Precipitation",
            description: "PRISM 30-year normal annual precipitation (1991-2020)",
            agency: "PRISM Climate Group, Oregon State University",
            url: "https://prism.oregonstate.edu",
            dataUrl: "https://prism.oregonstate.edu/normals/",
            citation: "PRISM Climate Group, Oregon State University, https://prism.oregonstate.edu, data created 4 Feb 2014, accessed Dec 2024",
            resolution: "800 meters (30 arc-seconds)",
            units: "mm",
            lastUpdate: "2021",
            band: 1
        },
        temperatureMean: {
            name: "Mean Temperature",
            description: "PRISM 30-year normal mean temperature (1991-2020)",
            agency: "PRISM Climate Group, Oregon State University",
            url: "https://prism.oregonstate.edu",
            citation: "PRISM Climate Group, Oregon State University, https://prism.oregonstate.edu, data created 4 Feb 2014, accessed Dec 2024",
            resolution: "800 meters",
            units: "°C",
            lastUpdate: "2021",
            band: 2
        },
        temperatureMin: {
            name: "Minimum Temperature",
            description: "PRISM 30-year normal minimum temperature (1991-2020)",
            agency: "PRISM Climate Group, Oregon State University",
            url: "https://prism.oregonstate.edu",
            citation: "PRISM Climate Group, Oregon State University, https://prism.oregonstate.edu, data created 4 Feb 2014, accessed Dec 2024",
            resolution: "800 meters",
            units: "°C",
            lastUpdate: "2021",
            band: 3
        },
        temperatureMax: {
            name: "Maximum Temperature",
            description: "PRISM 30-year normal maximum temperature (1991-2020)",
            agency: "PRISM Climate Group, Oregon State University",
            url: "https://prism.oregonstate.edu",
            citation: "PRISM Climate Group, Oregon State University, https://prism.oregonstate.edu, data created 4 Feb 2014, accessed Dec 2024",
            resolution: "800 meters",
            units: "°C",
            lastUpdate: "2021",
            band: 4
        },
        vpdMin: {
            name: "Min Vapor Pressure Deficit",
            description: "PRISM 30-year normal minimum vapor pressure deficit (1991-2020)",
            agency: "PRISM Climate Group, Oregon State University",
            url: "https://prism.oregonstate.edu",
            citation: "PRISM Climate Group, Oregon State University, https://prism.oregonstate.edu, data created 4 Feb 2014, accessed Dec 2024",
            resolution: "800 meters",
            units: "hPa",
            lastUpdate: "2021",
            band: 5
        },
        vpdMax: {
            name: "Max Vapor Pressure Deficit",
            description: "PRISM 30-year normal maximum vapor pressure deficit (1991-2020)",
            agency: "PRISM Climate Group, Oregon State University",
            url: "https://prism.oregonstate.edu",
            citation: "PRISM Climate Group, Oregon State University, https://prism.oregonstate.edu, data created 4 Feb 2014, accessed Dec 2024",
            resolution: "800 meters",
            units: "hPa",
            lastUpdate: "2021",
            band: 6
        },
        solarTotal: {
            name: "Solar Radiation (Total)",
            description: "PRISM 30-year normal total solar radiation on horizontal surface (1991-2020)",
            agency: "PRISM Climate Group, Oregon State University",
            url: "https://prism.oregonstate.edu",
            citation: "PRISM Climate Group, Oregon State University, https://prism.oregonstate.edu, data created 4 Feb 2014, accessed Dec 2024",
            resolution: "800 meters",
            units: "MJ/m²/day",
            lastUpdate: "2021",
            band: 7
        },
        solarSloped: {
            name: "Solar Radiation (Sloped)",
            description: "PRISM 30-year normal solar radiation on sloped surfaces (1991-2020)",
            agency: "PRISM Climate Group, Oregon State University",
            url: "https://prism.oregonstate.edu",
            citation: "PRISM Climate Group, Oregon State University, https://prism.oregonstate.edu, data created 4 Feb 2014, accessed Dec 2024",
            resolution: "800 meters",
            units: "MJ/m²/day",
            lastUpdate: "2021",
            band: 8
        },
        solarClear: {
            name: "Solar Radiation (Clear Sky)",
            description: "PRISM 30-year normal clear sky solar radiation (1991-2020)",
            agency: "PRISM Climate Group, Oregon State University",
            url: "https://prism.oregonstate.edu",
            citation: "PRISM Climate Group, Oregon State University, https://prism.oregonstate.edu, data created 4 Feb 2014, accessed Dec 2024",
            resolution: "800 meters",
            units: "MJ/m²/day",
            lastUpdate: "2021",
            band: 9
        }
    },

    // Data file paths (for local development, copy files to data directory)
    dataPaths: {
        mapunitTable: "data/Mapunit_OR_table.csv",
        soilPolygons: "data/CSNM_Polygons_WGS84_MajorComponents.geojson", // Major components only - cleaner overlaps
        boundaryPolygon: "data/CSNM_boundary_WGS84.geojson",
        highways: "data/CSNM_Highways.geojson",
        serviceRoads: "data/CSNM_ServiceRoads.geojson",
        elevation: "data/rasters/CSNM_Elevation_30m.tif",
        hillshade: "data/rasters/CSNM_Hillshade_30m.tif",
        nlcd: "data/rasters/NLCD_2024_CSNM.tif",
        lithology: "data/rasters/Lithology_CSNM.tif",
        climateNormals: "data/rasters/CSNM_climate_normals_stack.tif",
        precipitationAnnual: "data/rasters/CSNM_precipitation_annual.tif"
    },
    
    // Points of Interest
    pointsOfInterest: {
        informationCenter: {
            coordinates: [42.12274998657863, -122.46443553077413],
            name: "Cascade-Siskiyou Information Center",
            type: "facility",
            description: "Main information and visitor center for the Cascade-Siskiyou National Monument"
        }
    },
    
    // Projection information
    projection: {
        sourceEPSG: 5070, // Albers Equal Area Conic (CONUS)
        targetEPSG: 4326, // WGS84 for web mapping
        // Note: The polygon data is in EPSG:5070 and needs reprojection for web display
        // For now, we'll assume the data has been pre-transformed or handle the error gracefully
    },
    
    // Base map layers
    baseLayers: {
        osm: {
            name: "OpenStreetMap",
            url: "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
            attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
        },
        terrain: {
            name: "Terrain",
            url: "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",
            attribution: 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ, TomTom, Intermap, iPC, USGS, FAO, NPS, NRCAN, GeoBase, Kadaster NL, Ordnance Survey, Esri Japan, METI, Esri China (Hong Kong), and the GIS User Community'
        },
        satellite: {
            name: "Satellite",
            url: "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
            attribution: 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community'
        },
        topo: {
            name: "Topographic",
            url: "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
            attribution: 'Map data: &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, <a href="http://viewfinderpanoramas.org">SRTM</a> | Map style: &copy; <a href="https://opentopomap.org">OpenTopoMap</a> (<a href="https://creativecommons.org/licenses/by-sa/3.0/">CC-BY-SA</a>)'
        }
    },
    
    // Map styling options
    polygonStyle: {
        default: {
            color: '#3388ff',
            weight: 2,
            opacity: 0.8,
            fillOpacity: 0.3
        },
        highlighted: {
            color: '#ff7800',
            weight: 3,
            opacity: 1.0,
            fillOpacity: 0.5
        },
        boundary: {
            color: '#333333',
            weight: 2,
            opacity: 0.8,
            fillOpacity: 0.1,
            dashArray: '5, 5'
        }
    },
    
    // UI settings
    ui: {
        sidebarWidth: 350,
        animationDuration: 300,
        tooltipDelay: 500
    },
    
    // PRISM Climate Data Configuration
    prism: {
        baseURL: 'https://data.prism.oregonstate.edu/',
        variables: {
            'ppt': { name: 'Precipitation', unit: 'mm' },
            'tmean': { name: 'Mean Temperature', unit: '°C' },
            'tmin': { name: 'Minimum Temperature', unit: '°C' },
            'tmax': { name: 'Maximum Temperature', unit: '°C' }
        },
        updateDay: 15, // Check for updates on 15th of each month
        cacheExpiry: 30 * 24 * 60 * 60 * 1000, // 30 days in milliseconds
        colorSchemes: {
            temperature: {
                colors: ['#0000FF', '#FFFFFF', '#FF0000'], // Blue-White-Red
                range: [-10, 40] // °C
            },
            precipitation: {
                colors: ['#8B4513', '#FFFFFF', '#0000FF'], // Brown-White-Blue
                range: [0, 500] // mm
            }
        },
        monthNames: [
            'January', 'February', 'March', 'April', 'May', 'June',
            'July', 'August', 'September', 'October', 'November', 'December'
        ]
    }
};

// Utility functions for configuration
const ConfigUtils = {
    // Get soil order color by name
    getSoilOrderColor: function(soilOrder) {
        return CONFIG.soilOrderColors[soilOrder] || CONFIG.soilOrderColors["Unknown"];
    },
    
    // Get particle size color by name
    getParticleSizeColor: function(particleSize) {
        return CONFIG.particleSizeColors[particleSize] || CONFIG.particleSizeColors["Unknown"];
    },

    // Get parent material color by name
    getParentMaterialColor: function(parentMaterial) {
        return CONFIG.parentMaterialColors[parentMaterial] || CONFIG.parentMaterialColors["Unknown"];
    },
    
    // Get NLCD color by value
    getNLCDColor: function(value) {
        const nlcd = CONFIG.nlcdColors[value];
        return nlcd ? nlcd.color : "#808080";
    },
    
    // Get NLCD name by value
    getNLCDName: function(value) {
        const nlcd = CONFIG.nlcdColors[value];
        return nlcd ? nlcd.name : "Unknown";
    },
    
    // Get lithology color by value
    getLithologyColor: function(value) {
        const lithology = CONFIG.lithologyColors[value];
        return lithology ? lithology.color : "#808080";
    },
    
    // Get lithology name by value
    getLithologyName: function(value) {
        const lithology = CONFIG.lithologyColors[value];
        return lithology ? lithology.name : "Unknown";
    },
    
    // Get elevation color by normalized value (0-1)
    getElevationColor: function(normalizedValue) {
        const colors = CONFIG.elevationColors;
        
        if (normalizedValue <= 0.2) {
            // Low elevations: blue to green
            return this.interpolateColor(colors.lowColor, colors.midLowColor, normalizedValue / 0.2);
        } else if (normalizedValue <= 0.4) {
            // Mid-low elevations: green to yellow
            return this.interpolateColor(colors.midLowColor, colors.midColor, (normalizedValue - 0.2) / 0.2);
        } else if (normalizedValue <= 0.6) {
            // Mid elevations: yellow to orange
            return this.interpolateColor(colors.midColor, colors.midHighColor, (normalizedValue - 0.4) / 0.2);
        } else if (normalizedValue <= 0.8) {
            // Mid-high elevations: orange to red
            return this.interpolateColor(colors.midHighColor, colors.highColor, (normalizedValue - 0.6) / 0.2);
        } else {
            // High elevations: red to white
            return this.interpolateColor(colors.highColor, colors.peakColor, (normalizedValue - 0.8) / 0.2);
        }
    },
    
    // Interpolate between two hex colors
    interpolateColor: function(color1, color2, factor) {
        const rgb1 = this.hexToRgb(color1);
        const rgb2 = this.hexToRgb(color2);
        
        const r = Math.round(rgb1.r + factor * (rgb2.r - rgb1.r));
        const g = Math.round(rgb1.g + factor * (rgb2.g - rgb1.g));
        const b = Math.round(rgb1.b + factor * (rgb2.b - rgb1.b));
        
        return `rgb(${r}, ${g}, ${b})`;
    },
    
    // Convert hex to RGB
    hexToRgb: function(hex) {
        const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
        return result ? {
            r: parseInt(result[1], 16),
            g: parseInt(result[2], 16),
            b: parseInt(result[3], 16)
        } : { r: 0, g: 0, b: 0 };
    },
    
    // Get depth label by index
    getDepthLabel: function(depthIndex) {
        return CONFIG.depthLevels.labels[depthIndex] || "Unknown depth";
    },
    
    // Get band name for property and depth
    getBandName: function(property, depthIndex) {
        const bandNames = CONFIG.depthLevels.bandNames[property];
        return bandNames ? bandNames[depthIndex] : null;
    },
    
    // Get color palette for property and depth category
    getColorPalette: function(property, category = 'surface') {
        if (property === 'oc') {
            return CONFIG.depthLevels.colorPalettes.oc[category] || CONFIG.depthLevels.colorPalettes.oc.surface;
        } else if (property === 'ph') {
            return CONFIG.depthLevels.colorPalettes.ph;
        }
        return [];
    },
    
    // Validate configuration
    validate: function() {
        const errors = [];
        
        // Check if essential properties exist
        if (!CONFIG.mapCenter || CONFIG.mapCenter.length !== 2) {
            errors.push("Invalid map center coordinates");
        }
        
        if (!CONFIG.soilOrderColors || Object.keys(CONFIG.soilOrderColors).length === 0) {
            errors.push("Soil order colors not defined");
        }
        
        if (!CONFIG.dataPaths || !CONFIG.dataPaths.soilPolygons) {
            errors.push("Data paths not properly configured");
        }
        
        if (errors.length > 0) {
            return false;
        }
        
        return true;
    }
};

// Export for module systems (if needed)
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { CONFIG, ConfigUtils };
}