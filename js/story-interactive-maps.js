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
        this.initialized = false;
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
                        weight: 0.5,
                        opacity: 0.7,
                        color: '#666',
                        fillOpacity: 0.8
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
                        weight: 0.5,
                        opacity: 0.7,
                        color: '#666',
                        fillOpacity: 0.8
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
                
                // Parent material color mapping
                const parentMaterialColors = {
                    "Volcanic": "#D2691E",
                    "Serpentine": "#2E7D32",
                    "Alluvial": "#4682B4",
                    "Marine": "#5D6D7E",
                    "Basin deposits": "#8B7355",
                    "Clay-rich sediments": "#FFF100",
                    "Mixed colluvium": "#95A5A6",
                    "Plateau deposits": "#CD853F",
                    "Mixed/Undifferentiated": "#808080"
                };
                
                styleFunction = (feature) => {
                    // Determine parent material from properties
                    const props = feature.properties;
                    const compname = (props.compname || '').toLowerCase();
                    const taxorder = props.taxorder || '';
                    const geomdesc = (props.geomdesc || '').toLowerCase();
                    
                    let material;
                    if (taxorder === 'Andisols' || compname.includes('ash')) {
                        material = 'Volcanic';
                    } else if (compname.includes('serpent')) {
                        material = 'Serpentine';
                    } else if (geomdesc.includes('alluvial') || geomdesc.includes('flood') || geomdesc.includes('terrace')) {
                        material = 'Alluvial';
                    } else if (geomdesc.includes('lava')) {
                        material = 'Volcanic';
                    } else if (geomdesc.includes('mountain') || geomdesc.includes('hill')) {
                        if (taxorder === 'Vertisols') {
                            material = 'Clay-rich sediments';
                        } else {
                            material = 'Mixed colluvium';
                        }
                    } else if (geomdesc.includes('basin')) {
                        material = 'Basin deposits';
                    } else if (geomdesc.includes('plateau')) {
                        material = 'Plateau deposits';
                    } else {
                        material = 'Mixed/Undifferentiated';
                    }
                    
                    const color = parentMaterialColors[material] || parentMaterialColors['Mixed/Undifferentiated'];
                    
                    return {
                        fillColor: color,
                        weight: 0.5,
                        opacity: 0.7,
                        color: '#666',
                        fillOpacity: 0.8
                    };
                };
                
                popupFunction = (feature) => {
                    const props = feature.properties;
                    const compname = (props.compname || '').toLowerCase();
                    const taxorder = props.taxorder || '';
                    const geomdesc = (props.geomdesc || '').toLowerCase();
                    
                    // Determine material type (same logic as above)
                    let material;
                    if (taxorder === 'Andisols' || compname.includes('ash')) {
                        material = 'Volcanic';
                    } else if (compname.includes('serpent')) {
                        material = 'Serpentine';
                    } else if (geomdesc.includes('alluvial') || geomdesc.includes('flood') || geomdesc.includes('terrace')) {
                        material = 'Alluvial';
                    } else if (geomdesc.includes('lava')) {
                        material = 'Volcanic';
                    } else if (geomdesc.includes('mountain') || geomdesc.includes('hill')) {
                        if (taxorder === 'Vertisols') {
                            material = 'Clay-rich sediments';
                        } else {
                            material = 'Mixed colluvium';
                        }
                    } else if (geomdesc.includes('basin')) {
                        material = 'Basin deposits';
                    } else if (geomdesc.includes('plateau')) {
                        material = 'Plateau deposits';
                    } else {
                        material = 'Mixed/Undifferentiated';
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
                
                // Add hover effect
                layer.on({
                    mouseover: (e) => {
                        const layer = e.target;
                        layer.setStyle({
                            weight: 2,
                            opacity: 1
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
        
        const legend = L.control({ position: 'bottomleft' });
        
        legend.onAdd = function(map) {
            const div = L.DomUtil.create('div', 'story-map-legend');
            
            div.innerHTML = '<div class="legend-title">Legend</div>';
            
            items.forEach(item => {
                div.innerHTML += `
                    <div class="legend-item">
                        <span class="legend-color" style="background: ${item.color}"></span>
                        <span class="legend-label">${item.label}</span>
                    </div>
                `;
            });
            
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
    
    // Cleanup all maps
    destroy() {
        for (const [containerId, mapObj] of this.maps) {
            mapObj.map.remove();
        }
        this.maps.clear();
    }
}

// Export for use in story-map.js
if (typeof module !== 'undefined' && module.exports) {
    module.exports = StoryInteractiveMaps;
}