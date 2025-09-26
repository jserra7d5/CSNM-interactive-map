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
            
            // Initialize raster manager for handling TIFF files
            if (typeof RasterManager !== 'undefined') {
                this.rasterManager = new RasterManager();
                console.log('RasterManager initialized for story maps');
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
        
        // Create the map
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
                    const ocDepth = options.depth || 0; // Default to 0-5cm
                    rasterResult = await this.rasterManager.createTiffLayer('oc', ocDepth);
                    this.currentDepths.set(containerId, ocDepth);
                    break;
                    
                case 'ph':
                    const phDepth = options.depth || 0; // Default to 0-5cm
                    rasterResult = await this.rasterManager.createTiffLayer('ph', phDepth);
                    this.currentDepths.set(containerId, phDepth);
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