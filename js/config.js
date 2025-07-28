// Configuration and Constants
// Cascade-Siskiyou National Monument Soil Explorer Configuration

// Map Settings
const CONFIG = {
    // Map center and zoom
    mapCenter: [42.1, -122.466],
    mapZoom: 11,
    
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
        "Unknown": "#808080"
    },
    
    // WorldCover 2021 Land Cover Classification Colors
    landCoverColors: {
        10: { color: "#006400", name: "Tree cover" },
        20: { color: "#ffbb22", name: "Shrubland" },
        30: { color: "#ffff4c", name: "Grassland" },
        40: { color: "#f096ff", name: "Cropland" },
        50: { color: "#fa0000", name: "Built-up" },
        60: { color: "#b4b4b4", name: "Bare / sparse vegetation" },
        70: { color: "#f0f0f0", name: "Snow and ice" },
        80: { color: "#0064c8", name: "Permanent water bodies" },
        90: { color: "#0096a0", name: "Herbaceous wetland" },
        95: { color: "#00cf75", name: "Mangroves" },
        100: { color: "#fae6a0", name: "Moss and lichen" }
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
    
    // Data file paths (for local development, copy files to data directory)
    dataPaths: {
        ocRaster: "CSNM_OC_AllDepths.tif",
        phRaster: "CSNM_pH_AllDepths.tif",
        meanTempRaster: "data/rasters/CSNM_MeanTemperature_PRISM.tif",
        mapunitTable: "data/Mapunit_OR_table.csv",
        soilPolygons: "data/CSNM_Polygons_WGS84.geojson", // WGS84 projected SSURGO data
        boundaryPolygon: "data/CSNM_boundary_WGS84.geojson",
        highways: "data/CSNM_Highways.geojson",
        serviceRoads: "data/CSNM_ServiceRoads.geojson",
        landCover: "data/CSNM_LandCover_WorldCover2021.tif",
        elevation: "data/rasters/CSNM_Elevation_10m.tif",
        hillshade: "data/rasters/CSNM_Hillshade_10m.tif"
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
    }
};

// Utility functions for configuration
const ConfigUtils = {
    // Get soil order color by name
    getSoilOrderColor: function(soilOrder) {
        return CONFIG.soilOrderColors[soilOrder] || CONFIG.soilOrderColors["Unknown"];
    },
    
    // Get land cover color by value
    getLandCoverColor: function(value) {
        const landCover = CONFIG.landCoverColors[value];
        return landCover ? landCover.color : "#808080";
    },
    
    // Get land cover name by value
    getLandCoverName: function(value) {
        const landCover = CONFIG.landCoverColors[value];
        return landCover ? landCover.name : "Unknown";
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
            console.error("Configuration validation errors:", errors);
            return false;
        }
        
        return true;
    }
};

// Export for module systems (if needed)
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { CONFIG, ConfigUtils };
}