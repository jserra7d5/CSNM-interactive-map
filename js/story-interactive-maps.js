// Interactive Maps for Story Map
// Lightweight Leaflet maps with pre-colored simplified data

class StoryInteractiveMaps {
    constructor() {
        this.maps = new Map();
        this.data = {
            soilOrders: null,
            particleSizes: null,
            parentMaterial: null,
            boundary: null
        };
        this.rasterManager = null;
        this.initialized = false;
        this.currentDepths = new Map(); // Track current depth for OC/pH maps
    }
    
    // Initialize and load simplified data
    async init() {
        if (this.initialized) return;
        
        try {
            console.log('Loading full resolution major components data for story maps...');
            
            // Load full resolution major components data
            const [majorComponents, boundary] = await Promise.all([
                this.loadGeoJSON('data/CSNM_Polygons_WGS84_MajorComponents.geojson'),
                this.loadGeoJSON('data/CSNM_boundary_WGS84.geojson')
            ]);
            
            // Process the major components data for different map types
            if (majorComponents) {
                // All map types use the same data, just styled differently
                this.data.soilOrders = majorComponents;
                this.data.particleSizes = majorComponents;
                this.data.parentMaterial = majorComponents;
            }
            this.data.boundary = boundary;
            
            // Use the global raster manager instance (same as main app)
            if (window.rasterManager) {
                this.rasterManager = window.rasterManager;
                console.log('Using global RasterManager for story maps');
            } else if (typeof RasterManager !== 'undefined') {
                // Fallback: create new instance if global doesn't exist
                this.rasterManager = new RasterManager();
                console.log('Created new RasterManager for story maps');
            }
            
            this.initialized = true;
            console.log('Story map data loaded successfully');
            
        } catch (error) {
            console.error('Failed to load story map data:', error);
            // Continue without data - maps will still be created but empty
            this.initialized = true;
        }
    }
    
    // Simple GeoJSON loader
    async loadGeoJSON(url) {
        try {
            const response = await fetch(url);
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
            return await response.json();
        } catch (error) {
            console.warn(`Failed to load ${url}:`, error);
            return null;
        }
    }
    
    // Create an interactive map in the specified container
    createMap(containerId, mapType = 'soilOrders', options = {}) {
        const container = document.getElementById(containerId);
        if (!container) {
            console.warn(`Container ${containerId} not found`);
            return null;
        }
        
        // Check if map already exists
        if (this.maps.has(containerId)) {
            return this.maps.get(containerId);
        }
        
        // Create the map with restricted controls for story context
        const map = L.map(containerId, {
            center: options.center || [42.1, -122.466],
            zoom: options.zoom || 10,
            zoomControl: true,
            scrollWheelZoom: false, // Disable scroll wheel to not interfere with page scroll
            dragging: true,
            touchZoom: true,
            doubleClickZoom: true,
            boxZoom: false,
            keyboard: false,
            attributionControl: false
        });
        
        // Add minimal attribution
        L.control.attribution({
            position: 'bottomright',
            prefix: false
        }).addTo(map);
        
        // Add base layer
        const baseLayer = L.tileLayer('https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png', {
            attribution: '© OpenStreetMap, © CartoDB',
            maxZoom: 18
        }).addTo(map);
        
        // Store map reference
        this.maps.set(containerId, {
            map: map,
            type: mapType,
            layers: {
                base: baseLayer
            }
        });
        
        // Add data layers based on type
        this.addDataLayer(containerId, mapType);
        
        // Add boundary if available
        if (this.data.boundary) {
            this.addBoundaryLayer(containerId);
        }
        
        return map;
    }
    
    // Extract soil order exactly like main app
    extractSoilOrder(properties) {
        // Check for non-soil areas first, before looking at taxorder
        if (properties.compkind === 'Miscellaneous area' && properties.compname) {
            // Return the specific non-soil area name
            return properties.compname;
        }
        
        // Check for water features
        if (properties.compname && properties.compname.toLowerCase().includes('water')) {
            return 'Water';
        }
        
        // Now check for actual soil orders
        let order = properties.soilOrder || properties.taxorder || properties.soilorder;
        
        // If we have a valid soil order, return it
        if (order && order !== null && order !== undefined && order !== '') {
            return order;
        }
        
        // Default fallback
        return 'Unknown';
    }
    
    // Add data layer based on type
    addDataLayer(containerId, layerType) {
        const mapObj = this.maps.get(containerId);
        if (!mapObj) return;
        
        let data, styleFunction, popupFunction;
        
        switch (layerType) {
            case 'soilOrders':
                data = this.data.soilOrders;
                styleFunction = (feature) => {
                    const order = this.extractSoilOrder(feature.properties);
                    const color = CONFIG.soilOrderColors[order] || CONFIG.soilOrderColors['Unknown'];
                    return {
                        fillColor: color,
                        weight: 0,  // Remove borders to eliminate white gaps
                        opacity: 1,
                        color: color,
                        fillOpacity: 0.9,  // Increase fill opacity for better coverage
                        smoothFactor: 0  // Reduce polygon simplification
                    };
                };
                popupFunction = (feature) => {
                    const props = feature.properties;
                    const order = this.extractSoilOrder(props);
                    
                    return `
                        <div style="font-size: 12px;">
                            <strong>${order}</strong><br>
                            ${props.compname || ''}<br>
                            <span style="color: #666;">Map Unit: ${props.MUSYM || props.musym || ''}</span>
                        </div>
                    `;
                };
                break;
                
            case 'particleSizes':
                data = this.data.particleSizes;
                styleFunction = (feature) => {
                    const size = feature.properties.taxpartsize || 'Unknown';
                    const color = CONFIG.particleSizeColors[size] || CONFIG.particleSizeColors['Unknown'] || '#808080';
                    return {
                        fillColor: color,
                        weight: 0,  // Remove borders to eliminate white gaps
                        opacity: 1,
                        color: color,
                        fillOpacity: 0.9,  // Increase fill opacity for better coverage
                        smoothFactor: 0  // Reduce polygon simplification
                    };
                };
                popupFunction = (feature) => {
                    const props = feature.properties;
                    const size = props.taxpartsize || 'Unknown';
                    return `
                        <div style="font-size: 12px;">
                            <strong>Particle Size</strong><br>
                            ${size}<br>
                            <span style="color: #666;">Map Unit: ${props.MUSYM || props.musym || ''}</span>
                        </div>
                    `;
                };
                break;
                
            case 'parentMaterial':
                data = this.data.parentMaterial;
                
                // Parent material color mapping - only 6 classes in the actual data
                const parentMaterialColors = {
                    "Alluvial": "#5b92e5",  // Blue
                    "Colluvial": "#8b7355",  // Brown
                    "Fluvial": "#6495ed",    // Steel blue
                    "Lacustrine": "#87ceeb", // Sky blue
                    "Mountainous": "#654321", // Dark brown
                    "Volcanic": "#cd853f"     // Peru/tan brown
                };
                
                styleFunction = (feature) => {
                    const props = feature.properties;
                    // Use pmorigin field directly if available
                    let material = props.pmorigin;
                    
                    // If pmorigin not available, derive from other fields
                    if (!material) {
                        const compname = (props.compname || '').toLowerCase();
                        const taxorder = props.taxorder || '';
                        const geomdesc = (props.geomdesc || '').toLowerCase();
                        
                        if (taxorder === 'Andisols' || compname.includes('ash') || geomdesc.includes('lava')) {
                            material = 'Volcanic';
                        } else if (geomdesc.includes('alluvial') || geomdesc.includes('flood')) {
                            material = 'Alluvial';
                        } else if (geomdesc.includes('stream') || geomdesc.includes('river')) {
                            material = 'Fluvial';
                        } else if (geomdesc.includes('lake') || geomdesc.includes('lacustrine')) {
                            material = 'Lacustrine';
                        } else if (geomdesc.includes('mountain') || geomdesc.includes('hill') || geomdesc.includes('slope')) {
                            material = 'Mountainous';
                        } else if (geomdesc.includes('colluvium') || geomdesc.includes('talus')) {
                            material = 'Colluvial';
                        } else {
                            // Default to Colluvial if can't determine
                            material = 'Colluvial';
                        }
                    }
                    
                    const color = parentMaterialColors[material] || parentMaterialColors['Colluvial'];
                    
                    return {
                        fillColor: color,
                        weight: 0,  // Remove borders to eliminate white gaps
                        opacity: 1,
                        color: color,
                        fillOpacity: 0.9,  // Increase fill opacity for better coverage
                        smoothFactor: 0  // Reduce polygon simplification
                    };
                };
                
                popupFunction = (feature) => {
                    const props = feature.properties;
                    let material = props.pmorigin;
                    
                    if (!material) {
                        const compname = (props.compname || '').toLowerCase();
                        const taxorder = props.taxorder || '';
                        const geomdesc = (props.geomdesc || '').toLowerCase();
                        
                        if (taxorder === 'Andisols' || compname.includes('ash') || geomdesc.includes('lava')) {
                            material = 'Volcanic';
                        } else if (geomdesc.includes('alluvial') || geomdesc.includes('flood')) {
                            material = 'Alluvial';
                        } else if (geomdesc.includes('stream') || geomdesc.includes('river')) {
                            material = 'Fluvial';
                        } else if (geomdesc.includes('lake') || geomdesc.includes('lacustrine')) {
                            material = 'Lacustrine';
                        } else if (geomdesc.includes('mountain') || geomdesc.includes('hill') || geomdesc.includes('slope')) {
                            material = 'Mountainous';
                        } else if (geomdesc.includes('colluvium') || geomdesc.includes('talus')) {
                            material = 'Colluvial';
                        } else {
                            material = 'Colluvial';
                        }
                    }
                    
                    return `
                        <div style="font-size: 12px;">
                            <strong>Parent Material</strong><br>
                            ${material}<br>
                            <span style="color: #666;">Landform: ${props.geomdesc || 'N/A'}</span><br>
                            <span style="color: #666;">Map Unit: ${props.MUSYM || props.musym || ''}</span>
                        </div>
                    `;
                };
                break;
                
            default:
                console.warn(`Unknown layer type: ${layerType}`);
                return;
        }
        
        if (!data) {
            console.warn(`No data available for ${layerType}`);
            return;
        }
        
        // Create and add the layer
        const dataLayer = L.geoJSON(data, {
            style: styleFunction,
            onEachFeature: (feature, layer) => {
                if (popupFunction) {
                    layer.bindPopup(popupFunction(feature), {
                        maxWidth: 200,
                        className: 'story-map-popup'
                    });
                }
                
                // Add hover effect with subtle highlight
                layer.on({
                    mouseover: (e) => {
                        const layer = e.target;
                        layer.setStyle({
                            fillOpacity: 1  // Just increase opacity, no border
                        });
                    },
                    mouseout: (e) => {
                        dataLayer.resetStyle(e.target);
                    }
                });
            }
        }).addTo(mapObj.map);
        
        mapObj.layers.data = dataLayer;
        
        // Fit to bounds if this is the first data layer
        if (data.features && data.features.length > 0) {
            try {
                const bounds = dataLayer.getBounds();
                mapObj.map.fitBounds(bounds, { padding: [20, 20] });
            } catch (e) {
                console.warn('Could not fit bounds:', e);
            }
        }
    }
    
    // Add boundary layer
    addBoundaryLayer(containerId) {
        const mapObj = this.maps.get(containerId);
        if (!mapObj || !this.data.boundary) return;
        
        const boundaryLayer = L.geoJSON(this.data.boundary, {
            style: {
                color: '#dc2626',
                weight: 2,
                opacity: 0.8,
                fillOpacity: 0,
                dashArray: '5, 5'
            }
        }).addTo(mapObj.map);
        
        mapObj.layers.boundary = boundaryLayer;
    }
    
    // Add a simple legend
    addLegend(containerId, items) {
        const mapObj = this.maps.get(containerId);
        if (!mapObj) return;
        
        // Remove existing legend if present
        if (mapObj.legend) {
            mapObj.map.removeControl(mapObj.legend);
        }
        
        const legend = L.control({ position: 'bottomleft' });
        
        legend.onAdd = function(map) {
            const div = L.DomUtil.create('div', 'story-map-legend');
            
            // Check if items is a string (HTML) or an array
            if (typeof items === 'string') {
                // Direct HTML legend (for gradients)
                div.innerHTML = items;
            } else if (Array.isArray(items)) {
                // Array of legend items
                div.innerHTML = '<div class="legend-title">Legend</div>';
                
                items.forEach(item => {
                    div.innerHTML += `
                        <div class="legend-item">
                            <span class="legend-color" style="background: ${item.color}"></span>
                            <span class="legend-label">${item.label}</span>
                        </div>
                    `;
                });
            }
            
            return div;
        };
        
        legend.addTo(mapObj.map);
        mapObj.legend = legend;
    }
    
    // Switch data layer on existing map
    switchLayer(containerId, newLayerType) {
        const mapObj = this.maps.get(containerId);
        if (!mapObj) return;
        
        // Remove existing data layer
        if (mapObj.layers.data) {
            mapObj.map.removeLayer(mapObj.layers.data);
        }
        
        // Add new layer
        mapObj.type = newLayerType;
        this.addDataLayer(containerId, newLayerType);
    }
    
    // Cleanup a map
    destroyMap(containerId) {
        const mapObj = this.maps.get(containerId);
        if (!mapObj) return;
        
        // Remove the map
        mapObj.map.remove();
        this.maps.delete(containerId);
    }
    
    // Create raster-based map (elevation, land cover, climate, OC, pH)
    async createRasterMap(containerId, mapType = 'elevation', options = {}) {
        const container = document.getElementById(containerId);
        if (!container) {
            console.warn(`Container ${containerId} not found`);
            return null;
        }
        
        // Check if map already exists
        if (this.maps.has(containerId)) {
            return this.maps.get(containerId);
        }
        
        // Create the map with explicit CRS
        const map = L.map(containerId, {
            center: options.center || [42.1, -122.466],
            zoom: options.zoom || 10,
            zoomControl: true,
            scrollWheelZoom: false,
            dragging: true,
            touchZoom: true,
            doubleClickZoom: true,
            boxZoom: false,
            keyboard: false,
            attributionControl: false,
            crs: L.CRS.EPSG3857  // Explicitly set Web Mercator
        });
        
        // Add minimal attribution
        L.control.attribution({
            position: 'bottomright',
            prefix: false
        }).addTo(map);
        
        // Add base layer
        const baseLayer = L.tileLayer('https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png', {
            attribution: '© OpenStreetMap, © CartoDB',
            maxZoom: 18
        }).addTo(map);
        
        // Store map reference
        this.maps.set(containerId, {
            map: map,
            type: mapType,
            layers: {
                base: baseLayer
            }
        });
        
        // Add raster layer based on type
        if (this.rasterManager) {
            await this.addRasterLayer(containerId, mapType, options);
        }
        
        // Add boundary
        if (this.data.boundary) {
            this.addBoundaryLayer(containerId);
        }
        
        return map;
    }
    
    // Add raster layer to map
    async addRasterLayer(containerId, layerType, options = {}) {
        const mapObj = this.maps.get(containerId);
        if (!mapObj || !this.rasterManager) return;

        try {
            let rasterResult;

            switch (layerType) {
                case 'elevation':
                    // Combine elevation with hillshade
                    rasterResult = await this.rasterManager.createTiffLayer('elevation', null, {
                        includeHillshade: true
                    });
                    break;

                case 'landcover':
                    rasterResult = await this.rasterManager.createTiffLayer('nlcd', null);
                    break;

                case 'precipitation':
                    rasterResult = await this.rasterManager.createTiffLayer('precipitationAnnual', null);
                    break;

                case 'temperature':
                    rasterResult = await this.rasterManager.createTiffLayer('meanTemp', null);
                    break;

                case 'oc':
                case 'ph':
                    // Use custom rendering for OC/pH to fix alignment issues
                    const depth = options.depth || 0;
                    rasterResult = await this.createCustomSoilRasterLayer(mapObj.map, layerType, depth);
                    this.currentDepths.set(containerId, depth);
                    break;

                default:
                    console.warn(`Unknown raster layer type: ${layerType}`);
                    return;
            }
            
            if (rasterResult && rasterResult.layer) {
                rasterResult.layer.addTo(mapObj.map);
                mapObj.layers.raster = rasterResult.layer;

                // Store data range for legend creation
                mapObj.dataRange = rasterResult.dataRange;

                // Force map to properly recalculate after adding raster
                setTimeout(() => {
                    mapObj.map.invalidateSize();
                    // Don't change the zoom/bounds - let the raster stretch to fit
                }, 100);
            }
            
        } catch (error) {
            console.error(`Failed to load raster layer ${layerType}:`, error);
        }
    }
    
    // Change depth for OC/pH maps
    async changeDepth(containerId, newDepth) {
        const mapObj = this.maps.get(containerId);
        if (!mapObj || !this.rasterManager) {
            console.warn(`Cannot change depth for ${containerId} - map or rasterManager not found`);
            return;
        }
        
        console.log(`Changing depth for ${containerId} to ${newDepth}`);
        
        // Remove existing raster layer
        if (mapObj.layers.raster) {
            mapObj.map.removeLayer(mapObj.layers.raster);
            delete mapObj.layers.raster;
        }
        
        // Store the new depth
        this.currentDepths.set(containerId, newDepth);
        
        // Add new layer with different depth
        const options = { depth: newDepth };
        await this.addRasterLayer(containerId, mapObj.type, options);
        
        // Force map to recalculate after layer change
        setTimeout(() => {
            mapObj.map.invalidateSize();
        }, 100);
    }
    
    // Alias for changeDepth for backwards compatibility
    async updateRasterDepth(containerId, newDepth) {
        return this.changeDepth(containerId, newDepth);
    }
    
    // Toggle between two map types (e.g., OC/pH, precip/temp)
    async toggleMapType(containerId, newType) {
        const mapObj = this.maps.get(containerId);
        if (!mapObj) return;
        
        // Remove existing raster layer
        if (mapObj.layers.raster) {
            mapObj.map.removeLayer(mapObj.layers.raster);
        }
        
        // Update type and add new layer
        mapObj.type = newType;
        const currentDepth = this.currentDepths.get(containerId);
        await this.addRasterLayer(containerId, newType, { depth: currentDepth });
    }
    
    // Create custom soil raster layer with proper alignment
    async createCustomSoilRasterLayer(map, property, depth) {
        console.log(`Creating custom raster layer for ${property} at depth ${depth}`);

        // Get depth name for legend
        const depthNames = ['0-5cm', '5-15cm', '15-30cm', '30-60cm', '60-100cm', '100-200cm'];
        const depthName = depthNames[depth] || '0-5cm';

        // Get the TIFF data directly
        const filename = this.rasterManager.getRasterFilename(property, depth);
        const response = await fetch(filename);
        const arrayBuffer = await response.arrayBuffer();

        // Load TIFF using GeoTIFF library
        const tiff = await GeoTIFF.fromArrayBuffer(arrayBuffer);
        const image = await tiff.getImage();
        const rasters = await image.readRasters();
        const data = rasters[0];

        const width = image.getWidth();
        const height = image.getHeight();
        const bbox = image.getBoundingBox(); // [west, south, east, north]

        console.log(`TIFF dimensions: ${width}x${height}, bbox: ${bbox}`);

        // Monument bounds from boundary file (verified correct)
        const monumentBounds = {
            west: -122.6740,
            south: 41.9459,
            east: -122.1502,
            north: 42.3171
        };

        // Calculate actual data range - matching main map's approach
        // Filter out no-data values (0, -9999, null, NaN)
        const validData = Array.from(data).filter(v => {
            return v !== null && !isNaN(v) && v !== -9999 && v !== 0 && v > 0;
        });

        // Calculate min/max/mean like the main map
        let dataMin, dataMax, mean;
        if (validData.length > 0) {
            dataMin = validData.reduce((acc, val) => Math.min(acc, val), validData[0]);
            dataMax = validData.reduce((acc, val) => Math.max(acc, val), validData[0]);
            mean = validData.reduce((a, b) => a + b, 0) / validData.length;
        } else {
            dataMin = dataMax = mean = 0;
        }

        console.log(`🌍 RASTER: ${property} data range: min=${dataMin.toFixed(2)}, max=${dataMax.toFixed(2)}, mean=${mean.toFixed(2)}, valid values: ${validData.length}`);

        // Store data and bounds for tile rendering
        const tiffData = data;
        const tiffWidth = width;
        const tiffHeight = height;
        const soilProperty = property;

        // Color functions need to be defined outside the GridLayer
        const getOCColor = function(normalized) {
            // Brown color scale for organic carbon
            if (normalized < 0.25) {
                return {r: 255, g: 248, b: 220}; // Light cream
            } else if (normalized < 0.5) {
                return {r: 222, g: 184, b: 135}; // Tan
            } else if (normalized < 0.75) {
                return {r: 210, g: 105, b: 30}; // Brown
            } else {
                return {r: 139, g: 69, b: 19}; // Dark brown
            }
        };

        const getPHColor = function(normalized) {
            // Rainbow scale for pH
            if (normalized < 0.25) {
                return {r: 255, g: 0, b: 0}; // Red (acidic)
            } else if (normalized < 0.5) {
                return {r: 255, g: 255, b: 0}; // Yellow
            } else if (normalized < 0.75) {
                return {r: 0, g: 255, b: 0}; // Green
            } else {
                return {r: 0, g: 0, b: 255}; // Blue (alkaline)
            }
        };

        // Create custom GridLayer for on-the-fly rendering
        const CustomRasterLayer = L.GridLayer.extend({
            createTile: function(coords) {
                const tile = document.createElement('canvas');
                const ctx = tile.getContext('2d');
                tile.width = tile.height = 256;

                // Get tile bounds in lat/lng
                const tileBounds = this._tileCoordsToBounds(coords);
                const west = tileBounds.getWest();
                const east = tileBounds.getEast();
                const south = tileBounds.getSouth();
                const north = tileBounds.getNorth();

                // Create ImageData for the tile
                const imageData = ctx.createImageData(256, 256);

                // Fill tile pixels by sampling from TIFF data
                for (let py = 0; py < 256; py++) {
                    for (let px = 0; px < 256; px++) {
                        // Calculate lat/lng for this pixel
                        const lng = west + (px / 256) * (east - west);
                        const lat = north - (py / 256) * (north - south);

                        // Map to TIFF pixel coordinates using monument bounds (with interpolation)
                        const tiffXFloat = (lng - monumentBounds.west) /
                                          (monumentBounds.east - monumentBounds.west) * (tiffWidth - 1);
                        const tiffYFloat = (monumentBounds.north - lat) /
                                          (monumentBounds.north - monumentBounds.south) * (tiffHeight - 1);

                        const tiffX = Math.floor(tiffXFloat);
                        const tiffY = Math.floor(tiffYFloat);

                        // Check if within TIFF bounds
                        if (tiffX >= 0 && tiffX < tiffWidth - 1 && tiffY >= 0 && tiffY < tiffHeight - 1) {
                            // Bilinear interpolation for smoother rendering
                            const fx = tiffXFloat - tiffX;
                            const fy = tiffYFloat - tiffY;

                            const v00 = tiffData[tiffY * tiffWidth + tiffX];
                            const v10 = tiffData[tiffY * tiffWidth + tiffX + 1];
                            const v01 = tiffData[(tiffY + 1) * tiffWidth + tiffX];
                            const v11 = tiffData[(tiffY + 1) * tiffWidth + tiffX + 1];

                            // Check if all values are valid for interpolation
                            if (v00 <= 0 || v10 <= 0 || v01 <= 0 || v11 <= 0 ||
                                isNaN(v00) || isNaN(v10) || isNaN(v01) || isNaN(v11)) {
                                continue; // Skip this pixel if any surrounding value is invalid
                            }

                            // Interpolate
                            const value = v00 * (1 - fx) * (1 - fy) +
                                         v10 * fx * (1 - fy) +
                                         v01 * (1 - fx) * fy +
                                         v11 * fx * fy;

                            // Skip if interpolated value is invalid
                            if (value <= 0 || isNaN(value)) {
                                continue;
                            }

                            // Get color for value
                            let color;
                            if (soilProperty === 'oc') {
                                // Organic carbon color scale using actual data range
                                const normalized = Math.min(Math.max((value - dataMin) / (dataMax - dataMin), 0), 1);
                                color = getOCColor(normalized);
                            } else {
                                // pH color scale - pH typically ranges 4.5-7.5
                                // But use actual data range if available
                                const phMin = dataMin / 10; // pH values might be scaled by 10
                                const phMax = dataMax / 10;
                                const phValue = value / 10;
                                const normalized = Math.min(Math.max((phValue - phMin) / (phMax - phMin), 0), 1);
                                color = getPHColor(normalized);
                            }

                            // Set pixel
                            const idx = (py * 256 + px) * 4;
                            imageData.data[idx] = color.r;
                            imageData.data[idx + 1] = color.g;
                            imageData.data[idx + 2] = color.b;
                            imageData.data[idx + 3] = value > 0 ? 230 : 0; // Transparency
                        }
                    }
                }

                ctx.putImageData(imageData, 0, 0);
                return tile;
            }
        });

        // Create and return the layer
        const propertyName = property === 'oc' ? 'Organic Carbon' : 'Soil pH';
        const layer = new CustomRasterLayer({
            opacity: 0.9,
            attribution: '',
            name: `${propertyName} (${depthName})`,
            title: `${propertyName} (${depthName})`
        });

        // Return layer and data range (mean was already calculated earlier)
        return {
            layer: layer,
            dataRange: { min: dataMin, max: dataMax, mean }
        };
    }

    // Cleanup all maps
    destroy() {
        for (const [containerId, mapObj] of this.maps) {
            mapObj.map.remove();
        }
        this.maps.clear();
        this.currentDepths.clear();
    }
}

// Export for use in story-map.js
if (typeof module !== 'undefined' && module.exports) {
    module.exports = StoryInteractiveMaps;
}