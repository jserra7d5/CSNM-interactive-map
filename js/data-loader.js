// Data Loading and Management
// Handles loading and processing of geospatial data

class DataLoader {
    constructor() {
        this.cache = new Map();
        this.loadingStates = new Map();
    }
    
    // Load GeoJSON data with caching
    async loadGeoJSON(url, cacheKey = null) {
        const key = cacheKey || url;
        
        // Return cached data if available
        if (this.cache.has(key)) {
            return this.cache.get(key);
        }
        
        // Check if already loading
        if (this.loadingStates.has(key)) {
            return this.loadingStates.get(key);
        }
        
        // Start loading
        const loadPromise = this._fetchGeoJSON(url);
        this.loadingStates.set(key, loadPromise);
        
        try {
            const data = await loadPromise;
            this.cache.set(key, data);
            this.loadingStates.delete(key);
            return data;
        } catch (error) {
            this.loadingStates.delete(key);
            throw error;
        }
    }
    
    // Private method to fetch GeoJSON
    async _fetchGeoJSON(url) {
        try {
            // FORCE CORRECT FILE: Always use WGS84 polygons
            if (url.includes('CSNM_Polygons')) {
                if (!url.includes('WGS84')) {
                    url = url.replace(/CSNM_Polygons[^\/]*/, 'CSNM_Polygons_WGS84');
                }
                // Add cache buster to force fresh load
                if (!url.includes('?')) {
                    url += '?v=' + Date.now();
                }
            }
            
            // Add timestamp to help debug caching issues
            const timestamp = new Date().getTime();
            const urlWithTimestamp = `${url}?t=${timestamp}`;
            
            // Simply fetch the URL - let Vercel's rewrite rules handle serving compressed version
            const response = await fetch(urlWithTimestamp);
            
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            
            // Log response headers for debugging
            const contentEncoding = response.headers.get('content-encoding');
            const contentLength = response.headers.get('content-length');
            const contentType = response.headers.get('content-type');
            
            // Extract filename from URL for logging
            const requestedFile = url.split('/').pop().split('?')[0];
            const actualFile = response.url.split('/').pop().split('?')[0];
            
            // CRITICAL CHECK: Verify we're getting the right file
            if (requestedFile.includes('WGS84') && actualFile.includes('with_Data')) {
                throw new Error('Server returned wrong polygon file - deployment issue');
            }
            
            // Parse JSON - browser will automatically handle decompression if Content-Encoding is set
            const data = await response.json();
            
            
            // Check if coordinates look like they need reprojection
            this._checkProjection(data);
            
            return data;
        } catch (error) {
            throw new Error(`Failed to load data from ${url}: ${error.message}`);
        }
    }
    
    // Check if GeoJSON data appears to be in the wrong projection
    _checkProjection(geoJsonData) {
        if (geoJsonData.features && geoJsonData.features.length > 0) {
            const firstFeature = geoJsonData.features[0];
            if (firstFeature.geometry && firstFeature.geometry.coordinates) {
                const coords = firstFeature.geometry.coordinates[0][0];
                if (Array.isArray(coords) && coords.length >= 2) {
                    const [x, y] = coords;
                    // Check if coordinates are outside normal lat/lng bounds
                    if (Math.abs(x) > 180 || Math.abs(y) > 90) {
                    }
                }
            }
        }
    }
    
    // Load CSV data
    async loadCSV(url, cacheKey = null) {
        const key = cacheKey || url;
        
        // Return cached data if available
        if (this.cache.has(key)) {
            return this.cache.get(key);
        }
        
        try {
            const response = await fetch(url);
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            
            const text = await response.text();
            const data = this._parseCSV(text);
            this.cache.set(key, data);
            return data;
        } catch (error) {
            throw new Error(`Failed to load CSV from ${url}: ${error.message}`);
        }
    }
    
    // Simple CSV parser
    _parseCSV(text) {
        const lines = text.trim().split('\n');
        const headers = lines[0].split(',').map(h => h.trim().replace(/"/g, ''));
        const rows = [];
        
        for (let i = 1; i < lines.length; i++) {
            const values = lines[i].split(',').map(v => v.trim().replace(/"/g, ''));
            const row = {};
            headers.forEach((header, index) => {
                row[header] = values[index] || '';
            });
            rows.push(row);
        }
        
        return { headers, rows };
    }
    
    // Load all required data for the application
    async loadAllData() {
        const loadingTasks = [];
        const dataPromises = {};
        
        try {
            // Load soil polygons
            dataPromises.soilPolygons = this.loadGeoJSON(
                CONFIG.dataPaths.soilPolygons, 
                'soilPolygons'
            );
            
            // Load boundary polygon
            dataPromises.boundaryPolygon = this.loadGeoJSON(
                CONFIG.dataPaths.boundaryPolygon,
                'boundaryPolygon'
            );
            
            // Load mapunit table
            dataPromises.mapunitTable = this.loadCSV(
                CONFIG.dataPaths.mapunitTable,
                'mapunitTable'
            );
            
            // Load highways
            dataPromises.highways = this.loadGeoJSON(
                CONFIG.dataPaths.highways,
                'highways'
            );
            
            // Load service roads
            dataPromises.serviceRoads = this.loadGeoJSON(
                CONFIG.dataPaths.serviceRoads,
                'serviceRoads'
            );
            
            // Wait for all data to load
            const results = await Promise.allSettled([
                dataPromises.soilPolygons,
                dataPromises.boundaryPolygon,
                dataPromises.mapunitTable,
                dataPromises.highways,
                dataPromises.serviceRoads
            ]);
            
            // Check for failures
            const failures = results.filter(result => result.status === 'rejected');
            if (failures.length > 0) {
            }
            
            // Return successful results
            const data = {
                soilPolygons: results[0].status === 'fulfilled' ? results[0].value : null,
                boundaryPolygon: results[1].status === 'fulfilled' ? results[1].value : null,
                mapunitTable: results[2].status === 'fulfilled' ? results[2].value : null,
                highways: results[3].status === 'fulfilled' ? results[3].value : null,
                serviceRoads: results[4].status === 'fulfilled' ? results[4].value : null
            };
            
            // Process and enhance the data
            return this.processLoadedData(data);
            
        } catch (error) {
            throw error;
        }
    }
    
    // Process and enhance loaded data
    processLoadedData(data) {
        const processed = { ...data };
        
        // Process soil polygons if available
        if (data.soilPolygons) {
            // First filter to get only dominant components
            const dominantPolygons = this.filterDominantComponents(data.soilPolygons);
            processed.soilPolygons = this.enhanceSoilPolygons(dominantPolygons);
        }
        
        // Create lookup tables from mapunit data if available
        if (data.mapunitTable) {
            processed.mapunitLookup = this.createMapunitLookup(data.mapunitTable);
        }
        
        return processed;
    }
    
    // Filter to show only major components for soil order display
    filterDominantComponents(geoJsonData) {
        if (!geoJsonData.features) {
            return geoJsonData;
        }
        
        // For SSURGO view, we need ALL polygons, not just major components
        // So we'll keep all features but mark which ones are major
        geoJsonData.features.forEach(feature => {
            const isMajor = feature.properties.majcompflag && 
                           feature.properties.majcompflag.trim().toLowerCase() === 'yes';
            feature.properties._isMajorComponent = isMajor;
        });
        
        
        return geoJsonData;
    }
    
    // Enhance soil polygons with additional properties
    enhanceSoilPolygons(geoJsonData) {
        if (!geoJsonData.features) {
            return geoJsonData;
        }
        
        // Add enhanced properties to each feature
        geoJsonData.features.forEach(feature => {
            const props = feature.properties;
            
            // Extract soil order from taxorder field (this is the main field in the data)
            let soilOrder = props.taxorder || props.soilorder || 'Unknown';
            
            // Handle null values
            if (soilOrder === null || soilOrder === undefined || soilOrder === '') {
                soilOrder = 'Unknown';
            }
            
            // Ensure soil order is properly classified
            if (soilOrder && !CONFIG.soilOrderColors[soilOrder]) {
                soilOrder = 'Unknown';
            }
            
            // Add color property for styling
            props.color = ConfigUtils.getSoilOrderColor(soilOrder);
            props.soilOrder = soilOrder;
            
            // Extract and process particle size
            let particleSize = props.taxpartsize || 'Unknown';
            
            // Handle null/empty values
            if (particleSize === null || particleSize === undefined || particleSize === '') {
                particleSize = 'Unknown';
            }
            
            // Add particle size properties
            props.particleSize = particleSize;
            props.particleSizeColor = ConfigUtils.getParticleSizeColor(particleSize);
            
            // Add formatted display properties
            props.displayName = props.MUSYM || props.musym || 'Unknown Map Unit';
            props.soilOrderDisplay = soilOrder;
            props.particleSizeDisplay = particleSize;
        });
        
        return geoJsonData;
    }
    
    // Create lookup table from mapunit CSV data
    createMapunitLookup(csvData) {
        const lookup = new Map();
        
        if (csvData.rows) {
            csvData.rows.forEach(row => {
                const key = row.musym || row.mukey;
                if (key) {
                    lookup.set(key, row);
                }
            });
        }
        
        return lookup;
    }
    
    // Get detailed soil information for a map unit
    getSoilDetails(musym, mapunitLookup) {
        if (!mapunitLookup || !musym) {
            return null;
        }
        
        return mapunitLookup.get(musym) || null;
    }
    
    // Extract soil profile data from raster (placeholder - would need raster processing library)
    async extractSoilProfile(lat, lng, property) {
        // This is a placeholder implementation
        // In a real application, you would need to:
        // 1. Load raster data (TIFF files)
        // 2. Extract values at the given coordinates
        // 3. Return profile data for all depths
        
        
        // Return mock data for demonstration
        return this.generateMockProfileData(property);
    }
    
    // Generate mock soil profile data for demonstration
    generateMockProfileData(property) {
        const depths = CONFIG.depthLevels.depthsCm;
        const profile = [];
        
        depths.forEach((depth, index) => {
            let value;
            if (property === 'oc') {
                // Organic carbon typically decreases with depth
                value = Math.max(0.5, 15 * Math.exp(-depth / 30) + Math.random() * 2);
            } else if (property === 'ph') {
                // pH often increases slightly with depth
                value = 6.0 + (depth / 100) * 0.5 + (Math.random() - 0.5) * 0.8;
                value = Math.max(4.0, Math.min(8.5, value));
            } else if (property === 'meanTemp') {
                // Mean temperature typically increases with depth (geothermal gradient)
                value = 10.0 + (depth / 50) * 2.0 + (Math.random() - 0.5) * 1.5;
                value = Math.max(8.0, Math.min(18.0, value));
            } else {
                value = Math.random() * 10;
            }
            
            profile.push({
                depth: depth,
                depthRange: CONFIG.depthLevels.labels[index],
                value: Math.round(value * 100) / 100,
                property: property
            });
        });
        
        return profile;
    }
    
    // Clear cache
    clearCache() {
        this.cache.clear();
        this.loadingStates.clear();
    }
    
    // Get cache statistics
    getCacheStats() {
        return {
            cachedItems: this.cache.size,
            loadingItems: this.loadingStates.size,
            cacheKeys: Array.from(this.cache.keys())
        };
    }
}

// Singleton instance
const dataLoader = new DataLoader();

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { DataLoader, dataLoader };
}