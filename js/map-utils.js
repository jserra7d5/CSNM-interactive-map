// Map Utilities and Leaflet Integration
// Handles map creation, layer management, and spatial operations

class MapManager {
    constructor(containerId) {
        this.containerId = containerId;
        this.map = null;
        this.layers = {
            base: new Map(),
            polygons: new Map(),
            overlays: new Map(),
            rasters: new Map()
        };
        this.currentBaseLayer = null;
        this.rasterUniqueValues = {}; // Store unique values for classification rasters
        this.currentPolygonLayer = null;
        this.currentRasterLayer = null;
        this.boundaryLayer = null;
        this.selectedFeature = null;
        this.data = null;
        this.informationCenterMarker = null;
        this.clickMarker = null;
    }
    
    // Initialize the map
    initializeMap() {
        // Create map instance with performance optimizations
        this.map = L.map(this.containerId, {
            center: CONFIG.mapCenter,
            zoom: CONFIG.mapZoom,
            zoomControl: false,
            preferCanvas: true,
            renderer: L.canvas(),
            wheelDebounceTime: 40,
            wheelPxPerZoomLevel: 120
        });
        
        // Add zoom control to top right
        L.control.zoom({
            position: 'topright'
        }).addTo(this.map);
        
        // Add scale control
        L.control.scale({
            position: 'bottomleft'
        }).addTo(this.map);
        
        // Add north arrow control
        this.addNorthArrow();
        
        // Create base layers
        this.createBaseLayers();
        
        // Set default base layer to terrain
        this.setBaseLayer('terrain');
        
        // Setup event listeners
        this.setupEventListeners();
        
        // Setup raster processing progress listener
        this.setupRasterProgressListener();
        
        return this.map;
    }
    
    // Add north arrow to the map
    addNorthArrow() {
        const NorthArrowControl = L.Control.extend({
            options: {
                position: 'topright'
            },
            
            onAdd: function(map) {
                const container = L.DomUtil.create('div', 'leaflet-north-arrow leaflet-bar leaflet-control');
                
                // Create the north arrow using SVG
                container.innerHTML = `
                    <svg width="40" height="40" viewBox="0 0 40 40" xmlns="http://www.w3.org/2000/svg">
                        <!-- Outer circle -->
                        <circle cx="20" cy="20" r="18" fill="white" stroke="#333" stroke-width="2"/>
                        <!-- Arrow pointing north -->
                        <path d="M 20 8 L 16 24 L 20 20 L 24 24 Z" fill="#333" stroke="none"/>
                        <!-- N letter -->
                        <text x="20" y="34" text-anchor="middle" font-family="Arial, sans-serif" 
                              font-size="10" font-weight="bold" fill="#333">N</text>
                    </svg>
                `;
                
                // Prevent map interactions when clicking on the control
                L.DomEvent.disableClickPropagation(container);
                
                return container;
            }
        });
        
        // Add the control to the map
        new NorthArrowControl().addTo(this.map);
    }
    
    // Create base map layers
    createBaseLayers() {
        Object.entries(CONFIG.baseLayers).forEach(([key, config]) => {
            const layer = L.tileLayer(config.url, {
                attribution: config.attribution,
                maxZoom: 18,
                maxNativeZoom: 17,
                keepBuffer: 4,  // Preload 4 tiles in each direction (default is 2)
                updateWhenIdle: false,  // Update tiles during panning, not just after
                updateInterval: 100,  // Limit tile updates to every 100ms
                tileSize: 256,
                zoomOffset: 0,
                bounds: CONFIG.tileBounds || null  // Restrict to monument area if defined
            });
            this.layers.base.set(key, layer);
        });
    }
    
    // Set base layer
    setBaseLayer(layerKey) {
        
        // Remove current base layer
        if (this.currentBaseLayer) {
            this.map.removeLayer(this.currentBaseLayer);
        }
        
        // Add new base layer
        const newLayer = this.layers.base.get(layerKey);
        if (newLayer) {
            newLayer.addTo(this.map);
            this.currentBaseLayer = newLayer;
        } else {
        }
    }
    
    // Setup map event listeners
    setupEventListeners() {
        // Map click event
        this.map.on('click', (e) => {
            this.handleMapClick(e);
        });
        
        // Mouse move event for coordinates
        this.map.on('mousemove', (e) => {
            this.updateMouseCoordinates(e.latlng);
        });
        
        // Map ready event
        this.map.whenReady(() => {
            this.onMapReady();
        });
    }
    
    // Setup raster processing progress listener
    setupRasterProgressListener() {
        document.addEventListener('rasterProcessingProgress', (e) => {
            const { property, progress, message } = e.detail;
            
            // Update loading screen with progress
            const loadingElement = document.getElementById('loading');
            const progressFill = document.querySelector('.loading-progress-fill');
            const progressText = document.querySelector('.loading-progress-text');
            const loadingMessage = document.querySelector('.loading-overlay span');
            
            if (loadingElement) {
                // Ensure loading screen is visible
                loadingElement.style.display = 'flex';
                loadingElement.style.visibility = 'visible';
                loadingElement.style.opacity = '1';
                loadingElement.style.zIndex = '2000';
            }
            
            if (progressFill) {
                progressFill.style.width = `${progress}%`;
            }
            
            if (progressText) {
                progressText.textContent = `${progress}%`;
            }
            
            if (loadingMessage) {
                loadingMessage.textContent = message || `Loading ${property} data...`;
            }
            
            // Hide loading screen when complete
            if (progress === 100) {
                setTimeout(() => {
                    this.hideLoadingScreen(300);
                }, 500); // Small delay to show 100% completion
            }
        });
    }
    
    // Handle map click events - new centralized click handler system
    handleMapClick(e) {
        const { lat, lng } = e.latlng;
        
        console.log('🎯 Central click handler:', this.currentMapType, 'at', [lat, lng]);
        
        // Check if we clicked on a feature layer
        const clickedLayer = this.findClickedFeatureLayer(e);
        
        if (clickedLayer) {
            console.log('🎯 Feature clicked:', clickedLayer);
            this.handleFeatureClick(clickedLayer, e);
        } else {
            console.log('🎯 Map background clicked');
            this.handleBackgroundClick(e);
        }
        
        // Always emit custom event for other components
        const event = new CustomEvent('mapClick', {
            detail: { lat, lng, originalEvent: e }
        });
        document.dispatchEvent(event);
    }
    
    // Find the feature layer that was clicked (if any) using leaflet-pip
    findClickedFeatureLayer(e) {
        // Check if leaflet-pip is available
        if (typeof leafletPip === 'undefined') {
            console.warn('⚠️ leaflet-pip library not loaded, falling back to ray-casting detection');
            return this.findClickedFeatureLayerFallback(e);
        }
        
        const clickedLayers = [];
        
        // Check soil polygons layer
        const soilLayer = this.layers.polygons.get('soil');
        
        if (soilLayer && this.map.hasLayer(soilLayer)) {
            // Use leaflet-pip for accurate point-in-polygon detection
            // Note: leaflet-pip expects [lng, lat] order
            const point = [e.latlng.lng, e.latlng.lat];
            
            // For each GeoJSON layer group within the soil layer
            soilLayer.eachLayer((geoJsonLayer) => {
                if (geoJsonLayer.toGeoJSON) {
                    try {
                        // Use leaflet-pip to find all polygons containing this point
                        const results = leafletPip.pointInLayer(point, geoJsonLayer, false);
                        clickedLayers.push(...results);
                    } catch (error) {
                        console.warn('⚠️ leaflet-pip error:', error);
                        // Fall back to manual checking for this layer
                        if (geoJsonLayer.eachLayer) {
                            geoJsonLayer.eachLayer((featureLayer) => {
                                if (featureLayer.feature && this.isLayerClickable(featureLayer, e)) {
                                    clickedLayers.push(featureLayer);
                                }
                            });
                        }
                    }
                }
            });
            
            // Debug logging
            if (clickedLayers.length > 0) {
                console.log(`📍 leaflet-pip found ${clickedLayers.length} polygon(s) at click location`);
            } else {
                console.log(`📍 No polygons found at click location`);
            }
        }
        
        // Return the most relevant layer (prefer smaller polygons = more specific)
        return this.selectMostRelevantLayer(clickedLayers, e.latlng);
    }
    
    // Fallback method using our ray-casting implementation if leaflet-pip is not available
    findClickedFeatureLayerFallback(e) {
        const clickedLayers = [];
        const soilLayer = this.layers.polygons.get('soil');
        
        if (soilLayer && this.map.hasLayer(soilLayer)) {
            let layersInBounds = 0;
            let layersInPolygon = 0;
            
            soilLayer.eachLayer((geoJsonLayer) => {
                if (geoJsonLayer.eachLayer) {
                    geoJsonLayer.eachLayer((featureLayer) => {
                        if (featureLayer.feature) {
                            // First check bounding box (fast)
                            if (featureLayer.getBounds && featureLayer.getBounds().contains(e.latlng)) {
                                layersInBounds++;
                                // Then check actual polygon using ray-casting
                                if (this.isLayerClickable(featureLayer, e)) {
                                    layersInPolygon++;
                                    clickedLayers.push(featureLayer);
                                }
                            }
                        }
                    });
                } else if (geoJsonLayer.feature && this.isLayerClickable(geoJsonLayer, e)) {
                    clickedLayers.push(geoJsonLayer);
                }
            });
            
            if (layersInBounds > 0) {
                console.log(`📍 Ray-casting detection: ${layersInBounds} in bounds, ${layersInPolygon} in actual polygon`);
            }
        }
        
        return this.selectMostRelevantLayer(clickedLayers, e.latlng);
    }
    
    // Check if a layer is clickable at the given event location
    isLayerClickable(layer, e) {
        // Use Leaflet's built-in bounds checking as a fast first-pass filter
        if (!layer.getBounds || !layer.getBounds().contains(e.latlng)) {
            return false;
        }
        
        // For GeoJSON layers, use actual point-in-polygon detection
        if (layer.feature && layer.feature.geometry) {
            return this.isPointInPolygon(e.latlng, layer.feature.geometry);
        }
        
        // Fallback to bounding box check for non-GeoJSON layers
        return true;
    }
    
    // Check if a point is inside a polygon using ray-casting algorithm
    isPointInPolygon(latlng, geometry) {
        const point = [latlng.lng, latlng.lat];
        
        if (geometry.type === 'Polygon') {
            return this.pointInPolygonRings(point, geometry.coordinates);
        } else if (geometry.type === 'MultiPolygon') {
            for (const polygon of geometry.coordinates) {
                if (this.pointInPolygonRings(point, polygon)) {
                    return true;
                }
            }
            return false;
        }
        
        // Not a polygon geometry
        return false;
    }
    
    // Check if point is in polygon rings (handles exterior and holes)
    pointInPolygonRings(point, rings) {
        if (rings.length === 0) return false;
        
        // Check if point is inside exterior ring
        if (!this.pointInRing(point, rings[0])) {
            return false;
        }
        
        // Check if point is not in any holes
        for (let i = 1; i < rings.length; i++) {
            if (this.pointInRing(point, rings[i])) {
                return false; // Point is in a hole
            }
        }
        
        return true;
    }
    
    // Ray-casting algorithm for point in ring test
    pointInRing(point, ring) {
        let inside = false;
        const x = point[0], y = point[1];
        
        for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) {
            const xi = ring[i][0], yi = ring[i][1];
            const xj = ring[j][0], yj = ring[j][1];
            
            const intersect = ((yi > y) !== (yj > y))
                && (x < (xj - xi) * (y - yi) / (yj - yi) + xi);
            
            if (intersect) inside = !inside;
        }
        
        return inside;
    }
    
    // Select the most relevant layer from multiple candidates
    selectMostRelevantLayer(layers, latlng) {
        if (layers.length === 0) return null;
        if (layers.length === 1) {
            console.log(`📍 Single polygon found at click location`);
            return layers[0];
        }
        
        console.log(`📍 Multiple polygons (${layers.length}) found at click location, selecting best match...`);
        
        // Calculate distance from click point to polygon centroid for each layer
        const layerScores = layers.map(layer => {
            const score = {
                layer: layer,
                boundsArea: this.getLayerBoundsArea(layer),
                distanceToCenter: Infinity,
                mukey: layer.feature?.properties?.MUKEY || layer.feature?.properties?.mukey || 'Unknown',
                musym: layer.feature?.properties?.MUSYM || layer.feature?.properties?.musym || 'Unknown',
                compname: layer.feature?.properties?.compname || 'Unknown'
            };
            
            // Calculate distance to polygon centroid
            if (layer.getBounds) {
                const bounds = layer.getBounds();
                const center = bounds.getCenter();
                score.distanceToCenter = latlng.distanceTo(center);
            }
            
            return score;
        });
        
        // Sort by distance to center (closest first), then by area (smallest first)
        layerScores.sort((a, b) => {
            // Prefer polygons where click is closer to center
            const distDiff = a.distanceToCenter - b.distanceToCenter;
            if (Math.abs(distDiff) > 0.0001) { // Significant distance difference
                return distDiff;
            }
            // If similar distance, prefer smaller polygons
            return a.boundsArea - b.boundsArea;
        });
        
        // Log the selection for debugging
        console.log(`📍 Selected polygon:`, {
            mukey: layerScores[0].mukey,
            musym: layerScores[0].musym,
            component: layerScores[0].compname,
            distanceToCenter: layerScores[0].distanceToCenter.toFixed(2) + 'm',
            otherCandidates: layerScores.slice(1).map(s => ({
                musym: s.musym,
                distance: s.distanceToCenter.toFixed(2) + 'm'
            }))
        });
        
        return layerScores[0].layer;
    }
    
    // Calculate approximate area of layer bounds
    getLayerBoundsArea(layer) {
        if (!layer.getBounds) return Infinity;
        const bounds = layer.getBounds();
        const sw = bounds.getSouthWest();
        const ne = bounds.getNorthEast();
        return Math.abs((ne.lat - sw.lat) * (ne.lng - sw.lng));
    }
    
    // Handle clicks on feature layers
    handleFeatureClick(layer, e) {
        console.log('🎯 handleFeatureClick called for map type:', this.currentMapType);
        
        // Debug the actual feature properties when clicked
        if (layer && layer.feature && layer.feature.properties) {
            const props = layer.feature.properties;
            const mapUnit = props.MUSYM || props.musym || 'Unknown';
            const mukey = props.MUKEY || props.mukey || 'Unknown';
            console.log(`🔍 Clicked feature - Map Unit: ${mapUnit}, MUKEY: ${mukey}`);
            
            if (props.MUSYM === '33A' || props.musym === '33A') {
                console.log(`🔍 Detailed 33A properties:`, {
                    MUKEY: mukey,
                    compname: props.compname,
                    cokey: props.cokey,
                    comppct_r: props.comppct_r,
                    majcompflag: props.majcompflag,
                    taxorder: props.taxorder,
                    soilOrder: props.soilOrder,
                    _isMajorComponent: props._isMajorComponent
                });
            }
        }
        
        // Route to appropriate handler based on current map type
        switch (this.currentMapType) {
            case 'ssurgo':
                this.handleSSURGOFeatureClick(layer, e);
                break;
                
            case 'soil':
            case 'particleSize': 
            case 'parentMaterial':
                this.handlePolygonFeatureClick(layer, e);
                break;
                
            default:
                console.log('🎯 No specific handler for map type:', this.currentMapType);
                break;
        }
    }
    
    // Handle SSURGO-specific feature clicks (red marker + sidebar)
    handleSSURGOFeatureClick(layer, e) {
        console.log('🔍 SSURGO feature click:', layer);
        
        // Use the existing selectFeature logic for SSURGO
        this.selectFeature({ target: layer, latlng: e.latlng });
    }
    
    // Handle polygon feature clicks (simple popups)
    handlePolygonFeatureClick(layer, e) {
        console.log('🎯 Polygon feature click for', this.currentMapType, ':', layer);
        
        // Use the feature data that's actually being styled (layer.feature) 
        // This ensures consistency between what's displayed and what's in the popup
        const properties = layer.feature ? layer.feature.properties : 
                         (layer._featureData ? layer._featureData.properties : null);
        
        if (properties) {
            // Debug log to verify data consistency
            if (layer.feature && layer._featureData) {
                const featureProps = layer.feature.properties;
                const storedProps = layer._featureData.properties;
                if (featureProps.cokey !== storedProps.cokey) {
                    console.warn('⚠️ Data mismatch detected:', {
                        feature_cokey: featureProps.cokey,
                        feature_compname: featureProps.compname,
                        stored_cokey: storedProps.cokey,
                        stored_compname: storedProps.compname
                    });
                }
            }
            
            const popupContent = this.createSimplePopupContent(properties);
            const popup = L.popup()
                .setLatLng(e.latlng)
                .setContent(popupContent)
                .openOn(this.map);
        }
    }
    
    // Handle clicks on map background (no features)
    handleBackgroundClick(e) {
        // Close any open popups
        this.map.closePopup();
        
        // Clear any selected features (for SSURGO)
        if (this.currentMapType === 'ssurgo') {
            this.clearSelectedFeature();
        }
    }
    
    // Clear selected feature (for SSURGO view)
    clearSelectedFeature() {
        // Remove red marker if it exists
        if (this.selectedMarker) {
            this.map.removeLayer(this.selectedMarker);
            this.selectedMarker = null;
        }
        
        // Hide component sidebar
        const sidebar = document.getElementById('component-sidebar');
        if (sidebar) {
            sidebar.style.display = 'none';
        }
        
        console.log('🔍 SSURGO: Cleared selected feature');
    }
    
    // Update mouse coordinates display
    updateMouseCoordinates(latlng) {
        const coordsElement = document.getElementById('coords-display');
        if (coordsElement) {
            const lat = latlng.lat.toFixed(6);
            const lng = latlng.lng.toFixed(6);
            coordsElement.textContent = `${lat}, ${lng}`;
        }
    }
    
    // Called when map is ready
    onMapReady() {
        
        // Emit map ready event
        const event = new CustomEvent('mapReady', {
            detail: { map: this.map }
        });
        document.dispatchEvent(event);
    }
    
    // Load and display soil polygon data
    async loadSoilPolygons(data) {
        this.data = data;
        
        if (!data.soilPolygons) {
            return;
        }
        
        // Filter to only include dominant components per unique geographic area (MUKEY + Shape_Area + Geometry)
        const allFeatures = data.soilPolygons.features;
        const dominantFeaturesByArea = new Map();
        
        // Group features by geographic area and select the dominant component for each
        allFeatures.forEach(feature => {
            const areaKey = this.getGeographicAreaKey(feature);
            if (!areaKey) return;
            
            const existing = dominantFeaturesByArea.get(areaKey);
            if (!existing || this.isDominantComponent(feature, existing)) {
                // Log dominant component selection
                if (feature.properties.MUSYM === '33A' || feature.properties.musym === '33A' || 
                    feature.properties.MUSYM === '28E' || feature.properties.musym === '28E') {
                    console.log(`🎯 Selecting dominant for ${feature.properties.MUSYM || feature.properties.musym}:`, {
                        areaKey: areaKey.substring(0, 50) + '...',
                        compname: feature.properties.compname,
                        comppct_r: feature.properties.comppct_r,
                        majcompflag: feature.properties.majcompflag,
                        cokey: feature.properties.cokey,
                        taxorder: feature.properties.taxorder,
                        replacing: existing ? {
                            compname: existing.properties.compname,
                            comppct_r: existing.properties.comppct_r,
                            cokey: existing.properties.cokey
                        } : 'none'
                    });
                }
                dominantFeaturesByArea.set(areaKey, feature);
            }
        });
        
        const features = Array.from(dominantFeaturesByArea.values());
        const totalFeatures = features.length;
        
        // Store the filtered dominant features for legend and popup consistency
        this.dominantFeatures = features;
        
        console.log(`📊 Dominant component filtering complete:`, {
            totalOriginalFeatures: allFeatures.length,
            uniqueGeographicAreas: dominantFeaturesByArea.size,
            dominantFeaturesKept: features.length
        });
        
        
        // Create empty layer groups for progressive loading
        const soilLayer = L.layerGroup();
        const permanentBoundaryLayer = L.layerGroup();
        const toggleableBoundaryLayer = L.layerGroup();
        const ssurgoBoundaryLayer = L.layerGroup();
        
        // Function to load features in batches
        const loadFeaturesProgressively = async () => {
            const batchSize = totalFeatures > 5000 ? 1000 : 500;
            let loaded = 0;
            
            const loadBatch = () => {
                const batch = features.slice(loaded, loaded + batchSize);
                const batchGeoJSON = {
                    type: 'FeatureCollection',
                    features: batch
                };
                
                // Create layers for this batch with canvas renderer
                const batchSoilLayer = L.geoJSON(batchGeoJSON, {
                    style: (feature) => this.getSoilFillStyle(feature),
                    onEachFeature: (feature, layer) => this.onEachPolygon(feature, layer),
                    renderer: L.canvas({ padding: 0.5 })
                });
                
                const batchPermanentBoundary = L.geoJSON(batchGeoJSON, {
                    style: (feature) => this.getPermanentBoundaryStyle(feature),
                    interactive: false,
                    renderer: L.canvas({ padding: 0.5 })
                });
                
                const batchToggleableBoundary = L.geoJSON(batchGeoJSON, {
                    style: (feature) => this.getToggleableBoundaryStyle(feature),
                    interactive: false,
                    renderer: L.canvas({ padding: 0.5 })
                });
                
                const batchSsurgoBoundary = L.geoJSON(batchGeoJSON, {
                    style: (feature) => this.getSsurgoBoundaryStyle(feature),
                    interactive: false,
                    renderer: L.canvas({ padding: 0.5 })
                });
                
                // Add batches to main layers
                soilLayer.addLayer(batchSoilLayer);
                permanentBoundaryLayer.addLayer(batchPermanentBoundary);
                toggleableBoundaryLayer.addLayer(batchToggleableBoundary);
                ssurgoBoundaryLayer.addLayer(batchSsurgoBoundary);
                
                loaded += batch.length;
                
                // Update progress
                const progress = Math.round((loaded / totalFeatures) * 100);
                if (loaded % 1000 === 0 || loaded === totalFeatures) {
                }
                
                // Continue loading if more features remain
                if (loaded < totalFeatures) {
                    requestAnimationFrame(loadBatch);
                } else {
                }
            };
            
            // Start loading
            loadBatch();
        };
        
        // Use progressive loading for large datasets
        if (totalFeatures > 1000) {
            loadFeaturesProgressively();
        } else {
            // Load all at once for smaller datasets with canvas renderer - USE FILTERED FEATURES!
            const filteredGeoJSON = {
                type: 'FeatureCollection',
                features: features  // Use filtered features, not original data.soilPolygons!
            };
            
            
            const allSoilLayer = L.geoJSON(filteredGeoJSON, {
                style: (feature) => this.getSoilFillStyle(feature),
                onEachFeature: (feature, layer) => this.onEachPolygon(feature, layer),
                renderer: L.canvas({ padding: 0.5 })
            });
            
            const allPermanentBoundary = L.geoJSON(filteredGeoJSON, {
                style: (feature) => this.getPermanentBoundaryStyle(feature),
                interactive: false,
                renderer: L.canvas({ padding: 0.5 })
            });
            
            const allToggleableBoundary = L.geoJSON(filteredGeoJSON, {
                style: (feature) => this.getToggleableBoundaryStyle(feature),
                interactive: false,
                renderer: L.canvas({ padding: 0.5 })
            });
            
            const allSsurgoBoundary = L.geoJSON(filteredGeoJSON, {
                style: (feature) => this.getSsurgoBoundaryStyle(feature),
                interactive: false,
                renderer: L.canvas({ padding: 0.5 })
            });
            
            soilLayer.addLayer(allSoilLayer);
            permanentBoundaryLayer.addLayer(allPermanentBoundary);
            toggleableBoundaryLayer.addLayer(allToggleableBoundary);
            ssurgoBoundaryLayer.addLayer(allSsurgoBoundary);
        }
        
        // Store layers
        this.layers.polygons.set('soil', soilLayer);
        this.layers.polygons.set('permanent-boundaries', permanentBoundaryLayer);
        this.layers.polygons.set('toggleable-boundaries', toggleableBoundaryLayer);
        this.layers.polygons.set('ssurgo-boundaries', ssurgoBoundaryLayer);
        
        // Debug log what we stored
        
        // Don't add layers to map here - let updateLayers handle it when user selects a map type
        this.currentPolygonLayer = soilLayer;
        
        // Fit map to polygon bounds (but don't display the layer)
        this.fitToBounds(soilLayer);
    }
    
    // Load and display boundary polygon
    async loadBoundaryPolygon(data) {
        if (!data.boundaryPolygon) {
            return;
        }
        
        // Create monument boundary layer (always visible in red)
        this.monumentBoundaryLayer = L.geoJSON(data.boundaryPolygon, {
            style: this.getMonumentBoundaryStyle(),
            interactive: false
        });
        
        // Store layer
        this.layers.overlays.set('monument-boundary', this.monumentBoundaryLayer);
        
        // Add to map immediately (always visible)
        this.monumentBoundaryLayer.addTo(this.map);
        
    }
    
    // Load and display highways
    async loadHighways(data) {
        if (!data.highways) {
            return;
        }
        
        // Create highway layer (deep blue, thicker lines)
        this.highwayLayer = L.geoJSON(data.highways, {
            style: this.getHighwayStyle(),
            onEachFeature: (feature, layer) => this.onEachRoadFeature(feature, layer, 'Highway')
        });
        
        // Store layer
        this.layers.overlays.set('highways', this.highwayLayer);
        
    }
    
    // Load and display service roads
    async loadServiceRoads(data) {
        if (!data.serviceRoads) {
            return;
        }
        
        // Create service roads layer (green, thinner lines)
        this.serviceRoadLayer = L.geoJSON(data.serviceRoads, {
            style: this.getServiceRoadStyle(),
            onEachFeature: (feature, layer) => this.onEachRoadFeature(feature, layer, 'Service Road')
        });
        
        // Store layer
        this.layers.overlays.set('service-roads', this.serviceRoadLayer);
        
    }
    
    // Get style for soil-filled polygons
    getSoilFillStyle(feature) {
        // For soil orders view, show ALL components with their soil order colors
        const soilOrder = this.extractSoilOrder(feature.properties);
        const color = ConfigUtils.getSoilOrderColor(soilOrder);
        
        // Debug logging for more detailed analysis
        if (!this._soilStyleLogCount) this._soilStyleLogCount = 0;
        if (this._soilStyleLogCount < 10) {
            const mapUnit = feature.properties.MUSYM || feature.properties.musym || 'Unknown';
            const mukey = feature.properties.MUKEY || feature.properties.mukey || 'Unknown';
            const compName = feature.properties.compname || 'Unknown';
            const cokey = feature.properties.cokey || 'Unknown';
            console.log(`🎨 Styling - Map Unit: ${mapUnit}, MUKEY: ${mukey}, Component: ${compName}, Soil Order: ${soilOrder}, Color: ${color}, Cokey: ${cokey}`);
            this._soilStyleLogCount++;
        }
        
        return {
            fillColor: color,
            weight: 0,  // Remove borders to eliminate white gaps
            color: color,
            opacity: 1,
            fillOpacity: 0.9,  // Increase fill opacity for better coverage
            fill: true,
            smoothFactor: 0  // Reduce polygon simplification
        };
    }
    
    // Get style for particle size-filled polygons
    getParticleSizeFillStyle(feature) {
        // For particle size view, show ALL components with their particle size colors
        const particleSize = this.extractParticleSize(feature.properties);
        const color = ConfigUtils.getParticleSizeColor(particleSize);
        
        // Debug logging for first few features
        if (!this._particleStyleLogCount) this._particleStyleLogCount = 0;
        if (this._particleStyleLogCount < 5) {
            console.log(`🎨 Styling - Particle Size: ${particleSize}, Color: ${color}, MUKEY: ${feature.properties.MUKEY || feature.properties.mukey}`);
            this._particleStyleLogCount++;
        }
        
        return {
            fillColor: color,
            weight: 0,  // Remove borders to eliminate white gaps
            color: color,
            opacity: 1,
            fillOpacity: 0.9,  // Increase fill opacity for better coverage
            fill: true,
            smoothFactor: 0  // Reduce polygon simplification
        };
    }

    // Get style for parent material-filled polygons
    getParentMaterialFillStyle(feature) {
        // For parent material view, show ALL components with their parent material colors
        const parentMaterial = this.extractParentMaterial(feature.properties);
        const color = ConfigUtils.getParentMaterialColor(parentMaterial);
        
        // Debug logging for first few features
        if (!this._materialStyleLogCount) this._materialStyleLogCount = 0;
        if (this._materialStyleLogCount < 5) {
            console.log(`🎨 Styling - Parent Material: ${parentMaterial}, Color: ${color}, MUKEY: ${feature.properties.MUKEY || feature.properties.mukey}`);
            this._materialStyleLogCount++;
        }
        
        return {
            fillColor: color,
            weight: 0,  // Remove borders to eliminate white gaps
            color: color,
            opacity: 1,
            fillOpacity: 0.9,  // Increase fill opacity for better coverage
            fill: true,
            smoothFactor: 0  // Reduce polygon simplification
        };
    }
    
    // Get style for permanent boundaries (always visible with soil orders)
    getPermanentBoundaryStyle(feature) {
        return {
            fillColor: 'transparent',
            fillOpacity: 0,
            color: '#333333',
            weight: 0.65,
            opacity: 0.4,
            dashArray: '1, 1'
        };
    }
    
    // Get style for SSURGO view boundaries (purple like toggleable boundaries)
    getSsurgoBoundaryStyle(feature) {
        return {
            fillColor: 'transparent',
            fillOpacity: 0,
            color: '#9932CC',  // Purple color matching toggleable boundaries
            weight: 1.5,       // Thinner for better performance
            opacity: 0.8,
            dashArray: null    // Solid line instead of dashed
        };
    }
    
    // Get style for toggleable boundaries (purple, controlled by checkbox)
    getToggleableBoundaryStyle(feature) {
        return {
            fillColor: 'transparent',
            fillOpacity: 0,
            color: '#9932CC',
            weight: 0.65,
            opacity: 0.8,
            dashArray: '3, 3'
        };
    }
    
    // Get style for monument boundary (always visible in red)
    getMonumentBoundaryStyle() {
        return {
            fillColor: 'transparent',
            fillOpacity: 0,
            color: '#FF0000',
            weight: 3,
            opacity: 0.9,
            dashArray: '5, 5'
        };
    }
    
    // Get style for highways (deep blue, thicker)
    getHighwayStyle() {
        return {
            color: '#0b4e99',        // Deep blue
            weight: 4,              // Thicker than service roads
            opacity: 0.8,
            lineCap: 'round',
            lineJoin: 'round'
        };
    }
    
    // Get style for service roads (red, thinner)
    getServiceRoadStyle() {
        return {
            color: '#FF0000',        // Red
            weight: 2,              // Thinner than highways
            opacity: 0.7,
            lineCap: 'round',
            lineJoin: 'round'
        };
    }
    
    // Get style for boundary-only display (legacy method for compatibility)
    getBoundaryStyle(feature) {
        return this.getToggleableBoundaryStyle(feature);
    }
    
    // Get dominant component for a map unit (MUKEY)
    getDominantComponent(mukey) {
        if (!this.data || !this.data.soilPolygons) return null;
        
        // Get all components for this MUKEY
        const components = this.data.soilPolygons.features.filter(feature => 
            feature.properties.MUKEY === mukey
        );
        
        if (components.length === 0) return null;
        if (components.length === 1) return components[0];
        
        // Find component with highest percentage
        let dominant = components[0];
        let maxPercentage = this.getComponentPercentage(dominant.properties);
        
        for (let i = 1; i < components.length; i++) {
            const component = components[i];
            const percentage = this.getComponentPercentage(component.properties);
            
            // Higher percentage wins
            if (percentage > maxPercentage) {
                dominant = component;
                maxPercentage = percentage;
            } 
            // If tied, prefer major component flag
            else if (percentage === maxPercentage && 
                     component.properties.majcompflag === 'Yes' && 
                     dominant.properties.majcompflag !== 'Yes') {
                dominant = component;
            }
            // If still tied, use COKEY as tie-breaker (consistent ordering)
            else if (percentage === maxPercentage && 
                     component.properties.majcompflag === dominant.properties.majcompflag &&
                     component.properties.cokey < dominant.properties.cokey) {
                dominant = component;
            }
        }
        
        return dominant;
    }
    
    // Get component percentage, handling various field names and missing values
    getComponentPercentage(properties) {
        const percentage = properties.comppct_r || properties.comppct_h || properties.comppct_l;
        return percentage ? parseFloat(percentage) : 0;
    }
    
    // Generate unique key for geographic area (MUKEY + Shape_Area + Geometry Hash)
    getGeographicAreaKey(feature) {
        if (!feature || !feature.properties) return null;
        
        const mukey = feature.properties.MUKEY;
        const shapeArea = feature.properties.Shape_Area;
        
        // Round Shape_Area to handle floating-point precision issues
        const roundedArea = Math.round(shapeArea * 1000000) / 1000000;
        
        // Add geometry-based uniqueness for cases where MUKEY + Shape_Area isn't unique enough
        let geometryHash = '';
        if (feature.geometry && feature.geometry.coordinates) {
            // Create a simple hash from first few coordinates to distinguish different polygons
            const coords = feature.geometry.coordinates;
            let coordString = '';
            
            if (coords[0] && coords[0][0]) {
                // Get first 3 coordinate pairs for hashing
                const firstCoords = coords[0][0].slice(0, 3);
                coordString = firstCoords.map(coord => 
                    `${Math.round(coord[0] * 100000)},${Math.round(coord[1] * 100000)}`
                ).join('_');
            }
            
            // Simple hash function for coordinates
            geometryHash = this.simpleHash(coordString);
        }
        
        return `${mukey}_${roundedArea}_${geometryHash}`;
    }
    
    // Simple hash function for creating geometry-based identifiers
    simpleHash(str) {
        if (!str) return '0';
        let hash = 0;
        for (let i = 0; i < str.length; i++) {
            const char = str.charCodeAt(i);
            hash = ((hash << 5) - hash) + char;
            hash = hash & hash; // Convert to 32bit integer
        }
        return Math.abs(hash).toString(16);
    }
    
    // Compare two components to determine which is dominant for the same geographic area
    isDominantComponent(newComponent, existingComponent) {
        if (!newComponent || !newComponent.properties) return false;
        if (!existingComponent || !existingComponent.properties) return true;
        
        const newProps = newComponent.properties;
        const existingProps = existingComponent.properties;
        
        const newPct = this.getComponentPercentage(newProps);
        const existingPct = this.getComponentPercentage(existingProps);
        
        // Higher percentage wins
        if (newPct > existingPct) {
            return true;
        }
        if (newPct < existingPct) {
            return false;
        }
        
        // If tied, prefer major component flag
        const newMajor = newProps.majcompflag && newProps.majcompflag.trim() === 'Yes';
        const existingMajor = existingProps.majcompflag && existingProps.majcompflag.trim() === 'Yes';
        
        if (newMajor && !existingMajor) {
            return true;
        }
        if (!newMajor && existingMajor) {
            return false;
        }
        
        // If still tied, use COKEY for consistent ordering (lower COKEY wins)
        const newCokey = parseInt(newProps.cokey) || 0;
        const existingCokey = parseInt(existingProps.cokey) || 0;
        const newWins = newCokey < existingCokey;
        
        return newWins;
    }
    
    // Check if a single feature is the dominant component in its geographic area
    isFeatureDominant(feature) {
        if (!feature || !this.data || !this.data.soilPolygons) return false;
        
        const areaKey = this.getGeographicAreaKey(feature);
        if (!areaKey) return false;
        
        // Find all features in the same geographic area
        const areaFeatures = this.data.soilPolygons.features.filter(f => 
            this.getGeographicAreaKey(f) === areaKey
        );
        
        if (areaFeatures.length <= 1) return true;
        
        // Find the dominant feature for this area
        let dominantFeature = areaFeatures[0];
        for (let i = 1; i < areaFeatures.length; i++) {
            if (this.isDominantComponent(areaFeatures[i], dominantFeature)) {
                dominantFeature = areaFeatures[i];
            }
        }
        
        // Check if this feature is the dominant one
        return dominantFeature.properties.cokey === feature.properties.cokey;
    }

    // Extract soil order from feature properties
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
        
        // Default fallback for areas with no taxonomic classification
        return 'Non-soil area';
    }
    
    // Extract particle size from feature properties
    extractParticleSize(properties) {
        // First check the particleSize property set by enhanceSoilPolygons
        let size = properties.particleSize || properties.taxpartsize || 'Unknown';
        
        // Handle null values
        if (size === null || size === undefined || size === '') {
            size = 'Unknown';
        }
        
        return size;
    }

    // Extract parent material from feature properties
    extractParentMaterial(properties) {
        // Get the geomdesc field which contains parent material information
        let geomdesc = properties.geomdesc || 'Unknown';
        
        // Handle null values
        if (geomdesc === null || geomdesc === undefined || geomdesc === '') {
            geomdesc = 'Unknown';
        }
        
        // Categorize based on geomdesc content
        const desc = geomdesc.toLowerCase();
        
        // Alluvial materials (water-deposited)
        if (desc.includes('alluvial') || desc.includes('flood plain') || desc.includes('alluvial plain')) {
            return 'Alluvial';
        }
        
        // Fluvial materials (river deposits)
        if (desc.includes('river') || desc.includes('stream terrace') || desc.includes('terrace')) {
            return 'Fluvial';
        }
        
        // Lacustrine materials (lake deposits)
        if (desc.includes('lake') || desc.includes('basin floor') || desc.includes('basin')) {
            return 'Lacustrine';
        }
        
        // Volcanic materials
        if (desc.includes('lava') || desc.includes('volcanic')) {
            return 'Volcanic';
        }
        
        // Colluvial materials (slope deposits)
        if (desc.includes('hillslope') || desc.includes('hill') || desc.includes('slope')) {
            return 'Colluvial';
        }
        
        // Mountainous/residual materials
        if (desc.includes('mountain') || desc.includes('plateau') || desc.includes('ridge') || desc.includes('knoll')) {
            return 'Mountainous';
        }
        
        // Default to Unknown
        return 'Unknown';
    }
    
    // Setup interactions for each polygon
    onEachPolygon(feature, layer) {
        // Add unique identifier to the layer for reference
        layer.polygonId = feature.id || feature.properties.OBJECTID;
        layer.componentKey = feature.properties.cokey;
        
        // Store popup content and feature data for later use
        layer._popupContent = this.createPopupContent(feature.properties);
        layer._featureData = {
            properties: feature.properties,
            geometry: feature.geometry,
            id: feature.id
        };
    }
    
    // Setup interactions for each road feature
    onEachRoadFeature(feature, layer, roadType) {
        // Create popup content for roads
        const props = feature.properties;
        const roadName = props.fullname || props.name || `Unnamed ${roadType}`;
        const roadId = props.linearid || props.id || 'Unknown ID';
        
        const popupContent = `
            <div class="road-popup">
                <h5>${roadType}</h5>
                <p><strong>Name:</strong> ${roadName}</p>
                <p><strong>ID:</strong> ${roadId}</p>
                <p><strong>Type:</strong> ${props.mtfcc || 'Unknown'}</p>
                ${props.rttyp ? `<p><strong>Route Type:</strong> ${props.rttyp}</p>` : ''}
            </div>
        `;
        
        layer.bindPopup(popupContent);
        
        // Add hover effects
        layer.on({
            mouseover: (e) => {
                e.target.setStyle({
                    weight: e.target.options.weight + 2,
                    opacity: 1.0
                });
            },
            mouseout: (e) => {
                const originalStyle = roadType === 'Highway' ? this.getHighwayStyle() : this.getServiceRoadStyle();
                e.target.setStyle(originalStyle);
            }
        });
    }
    
    // Create popup content for polygons
    createPopupContent(properties) {
        const soilOrder = this.extractSoilOrder(properties);
        const particleSize = this.extractParticleSize(properties);
        const parentMaterial = this.extractParentMaterial(properties);
        const mapUnit = properties.MUSYM || properties.musym || 'Unknown Map Unit';
        const compName = properties.compname || '';
        const compPct = properties.comppct_r;
        const objectId = properties.OBJECTID || properties.id || 'N/A';
        const cokey = properties.cokey || 'N/A';
        
        return `
            <div class="popup-content">
                <h4>Map Unit: ${mapUnit}</h4>
                ${compName ? `<p><strong>Major Component:</strong> ${compName} <span style="color: #4CAF50; font-size: 11px;">(Major)</span></p>` : ''}
                <p><strong>Soil Order:</strong> ${soilOrder}</p>
                <p><strong>Particle Size:</strong> ${particleSize}</p>
                <p><strong>Parent Material:</strong> ${parentMaterial}</p>
                ${compPct ? `<p><strong>Component %:</strong> ${compPct}%</p>` : ''}
                <hr style="margin: 8px 0; border: none; border-top: 1px solid #eee;">
                <p style="font-size: 12px; color: #666;">
                    <strong>Polygon ID:</strong> ${objectId}<br>
                    <strong>Component Key:</strong> ${cokey}
                </p>
                <p><em>Click to view soil profile</em></p>
            </div>
        `;
    }
    
    // Create simple popup content based on current map type
    createSimplePopupContent(properties) {
        const mapUnit = properties.MUSYM || properties.musym || 'Unknown';
        const mukey = properties.MUKEY || properties.mukey || 'Unknown';
        
        if (this.currentMapType === 'soil') {
            const soilOrder = this.extractSoilOrder(properties);
            const color = ConfigUtils.getSoilOrderColor(soilOrder);
            console.log(`🔍 Popup - Soil Order: ${soilOrder}, Color: ${color}, Map Unit: ${mapUnit}, MUKEY: ${mukey}`);
            return `
                <div class="simple-popup">
                    <strong>Map Unit:</strong> ${mapUnit}<br>
                    <strong>MUKEY:</strong> ${mukey}<br>
                    <strong>Soil Order:</strong> ${soilOrder}
                </div>
            `;
        } else if (this.currentMapType === 'particleSize') {
            const particleSize = this.extractParticleSize(properties);
            const color = ConfigUtils.getParticleSizeColor(particleSize);
            console.log(`🔍 Popup - Particle Size: ${particleSize}, Color: ${color}, Map Unit: ${mapUnit}, MUKEY: ${mukey}`);
            return `
                <div class="simple-popup">
                    <strong>Map Unit:</strong> ${mapUnit}<br>
                    <strong>MUKEY:</strong> ${mukey}<br>
                    <strong>Particle Size:</strong> ${particleSize}
                </div>
            `;
        } else if (this.currentMapType === 'parentMaterial') {
            const parentMaterial = this.extractParentMaterial(properties);
            const color = ConfigUtils.getParentMaterialColor(parentMaterial);
            console.log(`🔍 Popup - Parent Material: ${parentMaterial}, Color: ${color}, Map Unit: ${mapUnit}, MUKEY: ${mukey}`);
            return `
                <div class="simple-popup">
                    <strong>Map Unit:</strong> ${mapUnit}<br>
                    <strong>MUKEY:</strong> ${mukey}<br>
                    <strong>Parent Material:</strong> ${parentMaterial}
                </div>
            `;
        }
        
        return '';
    }
    
    // Highlight feature on hover
    highlightFeature(e) {
        const layer = e.target;
        
        layer.setStyle(CONFIG.polygonStyle.highlighted);
        layer.bringToFront();
        
        // Update cursor
        this.map.getContainer().style.cursor = 'pointer';
    }
    
    // Reset highlight on mouse out
    resetHighlight(e) {
        const layer = e.target;
        
        // Only reset if this isn't the currently selected polygon
        if (this.selectedFeature && layer.polygonId === this.selectedFeature.polygonId) {
            // Keep selected polygon highlighted
            return;
        }
        
        // Reset to original style
        if (this.currentPolygonLayer) {
            this.currentPolygonLayer.resetStyle(layer);
        }
        
        // Reset cursor
        this.map.getContainer().style.cursor = '';
    }
    
    // Select feature on click
    selectFeature(e) {
        console.log('🎯 selectFeature called!', 'currentMapType:', this.currentMapType, 'event:', e);
        
        const layer = e.target;
        const feature = layer.feature;
        
        console.log('🎯 Layer:', layer, 'Feature:', feature);
        
        // Validate feature exists
        if (!feature) {
            console.warn('No feature available for selection');
            return;
        }
        
        // Show simple popup for soil order, particle size, and parent material views
        if (this.currentMapType === 'soil' || this.currentMapType === 'particleSize' || this.currentMapType === 'parentMaterial') {
            // Create and show simple popup
            const popupContent = this.createSimplePopupContent(feature.properties);
            const popup = L.popup()
                .setLatLng(e.latlng)
                .setContent(popupContent)
                .openOn(this.map);
            
            return; // Don't continue with normal selection behavior
        }
        
        // Prevent popup from opening in SSURGO view
        if (this.currentMapType === 'ssurgo') {
            L.DomEvent.stopPropagation(e);
        }
        
        // Store selected feature with unique identifiers
        this.selectedFeature = {
            ...feature,
            polygonId: layer.polygonId,
            componentKey: layer.componentKey
        };
        
        // Log selection info for debugging
        
        // Emit selection event
        const event = new CustomEvent('featureSelected', {
            detail: { 
                feature: this.selectedFeature, 
                layer: layer,
                latlng: e.latlng,
                polygonId: layer.polygonId,
                componentKey: layer.componentKey
            }
        });
        document.dispatchEvent(event);
        
        // Open popup only if not in SSURGO view
        if (this.currentMapType !== 'ssurgo' && layer._popupContent) {
            layer.bindPopup(layer._popupContent);
            layer.openPopup();
        }
    }
    
    // Toggle map unit boundary visibility (yellow boundaries)
    toggleBoundaries(show) {
        const boundaryLayer = this.layers.polygons.get('toggleable-boundaries');
        if (!boundaryLayer) {
            return;
        }
        
        if (show) {
            boundaryLayer.addTo(this.map);
            // Ensure proper drawing order after adding overlay
            this.ensureOverlayDrawingOrder();
        } else {
            this.map.removeLayer(boundaryLayer);
        }
    }
    
    // Toggle highway visibility
    toggleHighways(show) {
        const highwayLayer = this.layers.overlays.get('highways');
        if (!highwayLayer) {
            return;
        }
        
        if (show) {
            highwayLayer.addTo(this.map);
            // Ensure proper drawing order after adding overlay
            this.ensureOverlayDrawingOrder();
        } else {
            this.map.removeLayer(highwayLayer);
        }
    }
    
    // Toggle service road visibility
    toggleServiceRoads(show) {
        const serviceRoadLayer = this.layers.overlays.get('service-roads');
        if (!serviceRoadLayer) {
            return;
        }
        
        if (show) {
            serviceRoadLayer.addTo(this.map);
            // Ensure proper drawing order after adding overlay
            this.ensureOverlayDrawingOrder();
        } else {
            this.map.removeLayer(serviceRoadLayer);
        }
    }
    
    // Toggle information center visibility
    toggleInformationCenter(show) {
        if (show) {
            if (!this.informationCenterMarker) {
                this.createInformationCenterMarker();
            }
            if (this.informationCenterMarker) {
                this.informationCenterMarker.addTo(this.map);
                // Ensure proper drawing order after adding overlay
                this.ensureOverlayDrawingOrder();
            }
        } else {
            if (this.informationCenterMarker && this.map.hasLayer(this.informationCenterMarker)) {
                this.map.removeLayer(this.informationCenterMarker);
            }
        }
    }
    
    // Ensure overlays are drawn in the correct order on top of soil polygons
    // Order: boundaries -> highways -> service roads -> information center (top)
    ensureOverlayDrawingOrder() {
        // Only apply overlay ordering for soil, particleSize, parentMaterial, and ssurgo map types
        if (this.currentMapType !== 'soil' && this.currentMapType !== 'particleSize' && this.currentMapType !== 'parentMaterial' && this.currentMapType !== 'ssurgo') {
            return;
        }
        
        // Get overlay layers
        const boundaryLayer = this.layers.polygons.get('toggleable-boundaries');
        const highwayLayer = this.layers.overlays.get('highways');
        const serviceRoadLayer = this.layers.overlays.get('service-roads');
        const informationCenterMarker = this.layers.overlays.get('information-center');
        
        // Bring overlays to front in the specified order
        // 1. Map unit boundaries (drawn first among overlays)
        if (boundaryLayer && this.map.hasLayer(boundaryLayer)) {
            if (boundaryLayer.bringToFront) {
                boundaryLayer.bringToFront();
            } else {
                // For layer groups, bring each sub-layer to front
                boundaryLayer.eachLayer((layer) => {
                    if (layer.bringToFront) {
                        layer.bringToFront();
                    }
                });
            }
        }
        
        // 2. Highways (drawn second)
        if (highwayLayer && this.map.hasLayer(highwayLayer)) {
            if (highwayLayer.bringToFront) {
                highwayLayer.bringToFront();
            } else {
                // For layer groups, bring each sub-layer to front
                highwayLayer.eachLayer((layer) => {
                    if (layer.bringToFront) {
                        layer.bringToFront();
                    }
                });
            }
        }
        
        // 3. Service roads (drawn third)
        if (serviceRoadLayer && this.map.hasLayer(serviceRoadLayer)) {
            if (serviceRoadLayer.bringToFront) {
                serviceRoadLayer.bringToFront();
            } else {
                // For layer groups, bring each sub-layer to front
                serviceRoadLayer.eachLayer((layer) => {
                    if (layer.bringToFront) {
                        layer.bringToFront();
                    }
                });
            }
        }
        
        // 4. Information center (drawn last/on top)
        if (informationCenterMarker && this.map.hasLayer(informationCenterMarker)) {
            if (informationCenterMarker.bringToFront) {
                informationCenterMarker.bringToFront();
            }
        }
    }
    
    // Create information center marker
    createInformationCenterMarker() {
        const poi = CONFIG.pointsOfInterest.informationCenter;
        const [lat, lng] = poi.coordinates;
        
        // Function to update icon size based on zoom
        const updateIconSize = () => {
            const zoom = this.map.getZoom();
            // Scale from 60px at zoom 10 to 120px at zoom 16
            const baseSize = 60;
            const scaleFactor = 1 + (zoom - 10) * 0.2;
            const size = Math.max(baseSize, Math.min(120, baseSize * scaleFactor));
            
            const starIcon = L.divIcon({
                className: 'information-center-marker',
                html: `<div class="star-icon" style="font-size: ${size}px; width: ${size}px; height: ${size}px;">★</div>`,
                iconSize: [size, size],
                iconAnchor: [size/2, size/2],
                popupAnchor: [0, -size/2]
            });
            
            if (this.informationCenterMarker) {
                this.informationCenterMarker.setIcon(starIcon);
            }
            
            return starIcon;
        };
        
        // Create initial icon
        const initialIcon = updateIconSize();
        
        // Create marker
        this.informationCenterMarker = L.marker([lat, lng], {
            icon: initialIcon
        });
        
        // Update icon size on zoom
        this.map.on('zoomend', updateIconSize);
        
        // Add popup
        const popupContent = `
            <div class="info-center-popup">
                <h5>${poi.name}</h5>
                <p><strong>Type:</strong> ${poi.type}</p>
                <p>${poi.description}</p>
                <p><strong>Coordinates:</strong><br>
                   ${lat.toFixed(6)}, ${lng.toFixed(6)}</p>
            </div>
        `;
        
        this.informationCenterMarker.bindPopup(popupContent);
        
        // Store in overlays
        this.layers.overlays.set('information-center', this.informationCenterMarker);
    }
    
    // Create hillshade background layer for enhanced terrain visualization
    async createHillshadeBackground() {
        // Remove existing hillshade if present
        if (this.hillshadeLayer && this.map.hasLayer(this.hillshadeLayer)) {
            this.map.removeLayer(this.hillshadeLayer);
        }
        
        try {
            
            // Check if rasterManager is available
            if (!window.rasterManager) {
                return;
            }
            
            // Load hillshade TIFF
            const hillshadeTiff = await window.rasterManager.loadTiff(CONFIG.dataPaths.hillshade);
            if (!hillshadeTiff) {
                return;
            }
            
            const hillshadeImage = await hillshadeTiff.getImage(0);
            const rasters = await hillshadeImage.readRasters();
            const data = rasters[0];
            const bbox = hillshadeImage.getBoundingBox();
            const [width, height] = [hillshadeImage.getWidth(), hillshadeImage.getHeight()];
            
            // Create grayscale hillshade canvas
            const canvas = document.createElement('canvas');
            canvas.width = width;
            canvas.height = height;
            canvas.style.imageRendering = 'pixelated';
            canvas.style.imageRendering = 'crisp-edges';
            
            const ctx = canvas.getContext('2d');
            ctx.imageSmoothingEnabled = false;
            
            const imageData = ctx.createImageData(width, height);
            
            // Create grayscale hillshade
            for (let i = 0; i < data.length; i++) {
                const value = data[i];
                
                // Check for no-data values in hillshade (match criteria from raster-utils.js)
                const isNoData = value === null || 
                               value === undefined || 
                               isNaN(value) || 
                               value === 0 ||          // Common NoData for hillshade
                               value === 255 ||        // Sometimes used as NoData
                               value === 256 ||        // 16-bit promoted NoData
                               value === -1 ||         // Negative NoData
                               value === -9999 ||      // Standard NoData
                               value === -3.4028235e+38 || // Float32 NoData
                               value < 0 ||            // Any negative value
                               value > 255;            // Any value above 8-bit range
                
                const pixelIndex = i * 4;
                if (isNoData) {
                    // Transparent for no-data areas
                    imageData.data[pixelIndex] = 0;     // Red
                    imageData.data[pixelIndex + 1] = 0; // Green
                    imageData.data[pixelIndex + 2] = 0; // Blue
                    imageData.data[pixelIndex + 3] = 0; // Alpha (fully transparent)
                } else {
                    const grayValue = Math.max(0, Math.min(255, value)); // Ensure 0-255 range
                    
                    imageData.data[pixelIndex] = grayValue;     // Red
                    imageData.data[pixelIndex + 1] = grayValue; // Green
                    imageData.data[pixelIndex + 2] = grayValue; // Blue
                    imageData.data[pixelIndex + 3] = 120;       // Semi-transparent for subtle effect
                }
            }
            
            ctx.putImageData(imageData, 0, 0);
            
            // Create Leaflet image overlay
            const bounds = [
                [bbox[1], bbox[0]], // SW corner
                [bbox[3], bbox[2]]  // NE corner
            ];
            
            this.hillshadeLayer = L.imageOverlay(canvas.toDataURL(), bounds, {
                opacity: 0.4,
                interactive: false,
                className: 'hillshade-background'
            });
            
            // Add to map below the elevation layer
            this.hillshadeLayer.addTo(this.map);
            this.layers.rasters.set('hillshade', this.hillshadeLayer);
            
            
        } catch (error) {
        }
    }
    
    // Fit map to layer bounds
    fitToBounds(layer) {
        if (layer && layer.getBounds) {
            const bounds = layer.getBounds();
            if (bounds.isValid()) {
                this.map.fitBounds(bounds, { padding: [20, 20] });
            }
        }
    }
    
    // Add marker at coordinates
    addMarker(lat, lng, options = {}) {
        const marker = L.marker([lat, lng], options);
        marker.addTo(this.map);
        return marker;
    }
    
    // Add click marker (red circle with white X)
    addClickMarker(latlng) {
        // Remove existing click marker if any
        if (this.clickMarker && this.map.hasLayer(this.clickMarker)) {
            this.map.removeLayer(this.clickMarker);
        }
        
        // Create a custom icon that combines the red circle and white X
        const combinedIcon = L.divIcon({
            className: 'combined-click-marker',
            html: `
                <svg width="24" height="24" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
                    <circle cx="12" cy="12" r="10" fill="#ff0000" fill-opacity="0.8" stroke="#cc0000" stroke-width="2"/>
                    <path d="M 7 7 L 17 17 M 17 7 L 7 17" stroke="white" stroke-width="2" stroke-linecap="round"/>
                </svg>
            `,
            iconSize: [24, 24],
            iconAnchor: [12, 12]
        });
        
        // Create a single marker with the combined icon
        this.clickMarker = L.marker(latlng, {
            icon: combinedIcon,
            interactive: false
        });
        
        this.clickMarker.addTo(this.map);
        
        return this.clickMarker;
    }
    
    // Remove click marker
    removeClickMarker() {
        if (this.clickMarker && this.map.hasLayer(this.clickMarker)) {
            this.map.removeLayer(this.clickMarker);
            this.clickMarker = null;
        } else if (this.clickMarker) {
            this.clickMarker = null;
        }
    }
    
    // Remove all markers
    clearMarkers() {
        this.map.eachLayer((layer) => {
            if (layer instanceof L.Marker) {
                this.map.removeLayer(layer);
            }
        });
    }
    
    // Update legend and layer visibility based on current layer type
    async updateLayers(layerType, depth = 0) {
        console.log(`🔄 updateLayers called: ${this.currentMapType} -> ${layerType}`);
        
        try {
            // Store previous map type for debugging
            this.previousMapType = this.currentMapType;
            
            // Store current map type
            this.currentMapType = layerType;
            
            // Reset debug log counts for fresh logging
            this._soilStyleLogCount = 0;
            this._particleStyleLogCount = 0;
            this._materialStyleLogCount = 0;
            
            // If no layer type selected, hide all layers except monument boundary
            if (!layerType) {
                this.hideAllLayers();
                return;
            }
            
            console.log(`🔄 After initial checks, proceeding with ${layerType}`);
        
        // Show loading screen for all map type changes
        const loadingElement = document.getElementById('loading');
        if (loadingElement) {
            const loadingText = loadingElement.querySelector('span');
            const progressContainer = loadingElement.querySelector('.loading-progress-container');
            
            if (loadingText) {
                let mapTypeName = '';
                switch(layerType) {
                    case 'ssurgo': mapTypeName = 'SoilWeb view'; break;
                    case 'soil': mapTypeName = 'soil orders'; break;
                    case 'oc': mapTypeName = 'organic carbon'; break;
                    case 'ph': mapTypeName = 'soil pH'; break;
                    case 'meanTemp': mapTypeName = 'mean temperature'; break;
                    case 'landcover': mapTypeName = 'land cover'; break;
                    case 'elevation': mapTypeName = 'elevation map'; break;
                    case 'satellite': mapTypeName = 'satellite imagery'; break;
                    // Climate normal variables
                    case 'precipitation': mapTypeName = 'annual precipitation'; break;
                    case 'temperatureMean': mapTypeName = 'mean temperature'; break;
                    case 'temperatureMin': mapTypeName = 'minimum temperature'; break;
                    case 'temperatureMax': mapTypeName = 'maximum temperature'; break;
                    case 'vpdMin': mapTypeName = 'minimum vapor pressure deficit'; break;
                    case 'vpdMax': mapTypeName = 'maximum vapor pressure deficit'; break;
                    case 'solarTotal': mapTypeName = 'total solar radiation'; break;
                    case 'solarSloped': mapTypeName = 'sloped solar radiation'; break;
                    case 'solarClear': mapTypeName = 'clear sky solar radiation'; break;
                    default: mapTypeName = 'map data';
                }
                loadingText.textContent = `Loading ${mapTypeName}...`;
            }
            
            // Show progress bar for raster layers
            if (progressContainer) {
                const rasterTypes = ['oc', 'ph', 'meanTemp', 'landcover', 'elevation', 'nlcd', 'lithology',
                                   'precipitation', 'temperatureMean', 'temperatureMin', 'temperatureMax',
                                   'vpdMin', 'vpdMax', 'solarTotal', 'solarSloped', 'solarClear'];
                if (rasterTypes.includes(layerType)) {
                    progressContainer.style.display = 'block';
                    const progressFill = loadingElement.querySelector('.loading-progress-fill');
                    const progressText = loadingElement.querySelector('.loading-progress-text');
                    if (progressFill) progressFill.style.width = '0%';
                    if (progressText) progressText.textContent = '0%';
                } else {
                    progressContainer.style.display = 'none';
                }
            }
            
            loadingElement.style.display = 'flex';
            loadingElement.style.visibility = 'visible';
            loadingElement.style.opacity = '1';
            loadingElement.style.zIndex = '2000';
        }
        
        const legendElement = document.getElementById('soil-legend');
        const soilLayer = this.layers.polygons.get('soil');
        const permanentBoundaryLayer = this.layers.polygons.get('permanent-boundaries');
        const monumentBoundaryLayer = this.layers.overlays.get('monument-boundary');
        
        
        // Remove current raster layer if it exists
        if (this.currentRasterLayer && this.map.hasLayer(this.currentRasterLayer)) {
            this.map.removeLayer(this.currentRasterLayer);
            this.currentRasterLayer = null;
        }
        
        // Remove hillshade background if switching away from elevation
        if (layerType !== 'elevation' && this.hillshadeLayer && this.map.hasLayer(this.hillshadeLayer)) {
            this.map.removeLayer(this.hillshadeLayer);
        }
        
        // Always ensure monument boundary is visible
        if (monumentBoundaryLayer && !this.map.hasLayer(monumentBoundaryLayer)) {
            monumentBoundaryLayer.addTo(this.map);
        }
        
        console.log(`🔄 Checking layer type: ${layerType}`);
        
        if (layerType === 'ssurgo') {
            console.log('🔍 SSURGO: Starting SSURGO view setup');
            // SSURGO view - show polygons with yellow boundaries for click detection
            if (soilLayer) {
                if (!this.map.hasLayer(soilLayer)) {
                    console.log('🔍 SSURGO: Adding soil layer to map');
                    soilLayer.addTo(this.map);
                }
                
                let totalLayers = 0;
                let featureLayers = 0;
                
                // Update all polygons to have transparent fill with orange boundaries
                // Need to iterate through nested layers properly
                soilLayer.eachLayer((geoJsonLayer) => {
                    totalLayers++;
                    console.log('🔍 SSURGO: Processing layer', totalLayers, 'type:', geoJsonLayer.constructor.name);
                    
                    if (geoJsonLayer.eachLayer) {
                        // This is a GeoJSON layer group, iterate through its features
                        geoJsonLayer.eachLayer((featureLayer) => {
                            featureLayers++;
                            console.log('🔍 SSURGO: Processing feature layer', featureLayers, 
                                       'has setStyle:', !!featureLayer.setStyle,
                                       'has feature:', !!featureLayer.feature);
                            
                            if (featureLayer.setStyle) {
                                featureLayer.setStyle({
                                    fillColor: 'transparent',
                                    fillOpacity: 0,
                                    color: '#ff6600',  // Orange boundaries
                                    weight: 0.65,      // Thin lines
                                    opacity: 0.8
                                });
                                
                                // No longer need individual click handlers - using centralized system
                            }
                        });
                    } else if (geoJsonLayer.setStyle) {
                        // Maybe it's a direct feature layer?
                        console.log('🔍 SSURGO: Direct feature layer found');
                        geoJsonLayer.setStyle({
                            fillColor: 'transparent',
                            fillOpacity: 0,
                            color: '#ff6600',
                            weight: 0.65,
                            opacity: 0.8
                        });
                        
                        // No longer need individual click handlers - using centralized system
                    }
                });
                
                console.log(`🔍 SSURGO: Setup complete - Total layers: ${totalLayers}, Feature layers: ${featureLayers}`);
            } else {
                console.log('🔍 SSURGO: No soil layer available!');
            }
            // Hide legend for SSURGO view
            if (legendElement) {
                legendElement.style.display = 'none';
            }
            
            // Ensure overlays are drawn on top in the correct order
            this.ensureOverlayDrawingOrder();
            
            // Hide loading screen for non-raster layers with a small delay
            this.hideLoadingScreen(300);
        } else if (layerType === 'soil') {
            console.log('🌈 SOIL: Switching to soil view from', this.previousMapType);
            
            // Show soil polygons, permanent boundaries, and legend
            if (soilLayer && !this.map.hasLayer(soilLayer)) {
                soilLayer.addTo(this.map);
            }
            
            // Restore original soil polygon colors
            if (soilLayer) {
                let colorCount = 0;
                let totalLayers = 0;
                
                // Need to iterate through nested layers
                soilLayer.eachLayer((geoJsonLayer) => {
                    if (geoJsonLayer.eachLayer) {
                        // This is a GeoJSON layer group, iterate through its features
                        geoJsonLayer.eachLayer((featureLayer) => {
                            totalLayers++;
                            if (featureLayer.setStyle && featureLayer.feature) {
                                const style = this.getSoilFillStyle(featureLayer.feature);
                                featureLayer.setStyle(style);
                                
                                // Update the stored feature data to match what we're styling
                                // This ensures popup consistency
                                if (featureLayer._featureData) {
                                    featureLayer._featureData.properties = featureLayer.feature.properties;
                                }
                                
                                // Log specific details for debugging map unit 33A
                                if ((featureLayer.feature.properties.MUSYM === '33A' || featureLayer.feature.properties.musym === '33A') && colorCount < 2) {
                                    console.log(`🔍 Re-styling Map Unit 33A:`, {
                                        compname: featureLayer.feature.properties.compname,
                                        cokey: featureLayer.feature.properties.cokey,
                                        taxorder: featureLayer.feature.properties.taxorder,
                                        soilOrder: featureLayer.feature.properties.soilOrder,
                                        color: style.fillColor
                                    });
                                    colorCount++;
                                }
                            }
                        });
                    } else if (geoJsonLayer.setStyle) {
                        // Handle direct GeoJSON layers (non-nested)
                        totalLayers++;
                        // No longer need to manually manage click handlers
                    }
                });
                console.log(`🌈 SOIL: Processed ${totalLayers} layers`);
            }
            
            // Bring soil layer to front to ensure it's visible
            if (soilLayer) {
                // LayerGroup doesn't have bringToFront, need to iterate through nested layers
                soilLayer.eachLayer((geoJsonLayer) => {
                    if (geoJsonLayer.eachLayer) {
                        geoJsonLayer.eachLayer((featureLayer) => {
                            if (featureLayer.bringToFront) {
                                featureLayer.bringToFront();
                            }
                        });
                    }
                });
                
                // Test visibility by checking first layer
                let testLayer = null;
                soilLayer.eachLayer((layer) => {
                    if (!testLayer && layer.feature) {
                        testLayer = layer;
                        const bounds = layer.getBounds();
                        return false; // Stop after first layer
                    }
                });
            }
            
            if (permanentBoundaryLayer && !this.map.hasLayer(permanentBoundaryLayer)) {
                permanentBoundaryLayer.addTo(this.map);
            }
            
            if (legendElement) {
                this.showSoilOrderLegend();
            }
            
            // Ensure overlays are drawn on top in the correct order
            this.ensureOverlayDrawingOrder();
            
            // Hide loading screen for non-raster layers with a small delay
            this.hideLoadingScreen(300);
        } else if (layerType === 'particleSize') {
            
            // Show soil polygons with particle size colors
            if (soilLayer && !this.map.hasLayer(soilLayer)) {
                soilLayer.addTo(this.map);
            }
            
            // Update polygons with particle size colors
            if (soilLayer) {
                let colorCount = 0;
                let totalLayers = 0;
                
                // Need to iterate through nested layers
                soilLayer.eachLayer((geoJsonLayer) => {
                    if (geoJsonLayer.eachLayer) {
                        // This is a GeoJSON layer group, iterate through its features
                        geoJsonLayer.eachLayer((featureLayer) => {
                            totalLayers++;
                            if (featureLayer.setStyle && featureLayer.feature) {
                                const style = this.getParticleSizeFillStyle(featureLayer.feature);
                                featureLayer.setStyle(style);
                                
                                // Update the stored feature data to match what we're styling
                                // This ensures popup consistency
                                if (featureLayer._featureData) {
                                    featureLayer._featureData.properties = featureLayer.feature.properties;
                                }
                                
                                if (colorCount < 5) { // Log first 5 for debugging
                                    colorCount++;
                                }
                            }
                        });
                    } else if (geoJsonLayer.setStyle) {
                        // Handle direct GeoJSON layers (non-nested)
                        totalLayers++;
                        // No longer need to manually manage click handlers
                    }
                });
                
                // Bring layers to front
                soilLayer.eachLayer((geoJsonLayer) => {
                    if (geoJsonLayer.eachLayer) {
                        geoJsonLayer.eachLayer((featureLayer) => {
                            if (featureLayer.bringToFront) {
                                featureLayer.bringToFront();
                            }
                        });
                    }
                });
            }
            
            // Show boundaries
            if (permanentBoundaryLayer && !this.map.hasLayer(permanentBoundaryLayer)) {
                permanentBoundaryLayer.addTo(this.map);
            }
            
            // Show particle size legend
            if (legendElement) {
                this.showParticleSizeLegend();
            }
            
            // Ensure overlays are drawn on top in the correct order
            this.ensureOverlayDrawingOrder();
            
            // Hide loading screen for non-raster layers with a small delay
            this.hideLoadingScreen(300);
        } else if (layerType === 'parentMaterial') {
            
            // Show soil polygons with parent material colors
            if (soilLayer && !this.map.hasLayer(soilLayer)) {
                soilLayer.addTo(this.map);
            }
            
            // Update polygons with parent material colors
            if (soilLayer) {
                let colorCount = 0;
                let totalLayers = 0;
                
                // Need to iterate through nested layers
                soilLayer.eachLayer((geoJsonLayer) => {
                    if (geoJsonLayer.eachLayer) {
                        // This is a GeoJSON layer group, iterate through its features
                        geoJsonLayer.eachLayer((featureLayer) => {
                            totalLayers++;
                            if (featureLayer.setStyle && featureLayer.feature) {
                                const style = this.getParentMaterialFillStyle(featureLayer.feature);
                                featureLayer.setStyle(style);
                                
                                // Update the stored feature data to match what we're styling
                                // This ensures popup consistency
                                if (featureLayer._featureData) {
                                    featureLayer._featureData.properties = featureLayer.feature.properties;
                                }
                                
                                if (colorCount < 5) { // Log first 5 for debugging
                                    colorCount++;
                                }
                            }
                        });
                    } else if (geoJsonLayer.setStyle) {
                        // Handle direct GeoJSON layers (non-nested)
                        totalLayers++;
                        // No longer need to manually manage click handlers
                    }
                });
                
                // Bring layers to front
                soilLayer.eachLayer((geoJsonLayer) => {
                    if (geoJsonLayer.eachLayer) {
                        geoJsonLayer.eachLayer((featureLayer) => {
                            if (featureLayer.bringToFront) {
                                featureLayer.bringToFront();
                            }
                        });
                    }
                });
            }
            
            // Show boundaries
            if (permanentBoundaryLayer && !this.map.hasLayer(permanentBoundaryLayer)) {
                permanentBoundaryLayer.addTo(this.map);
            }
            
            // Show parent material legend
            if (legendElement) {
                this.showParentMaterialLegend();
            }
            
            // Ensure overlays are drawn on top in the correct order
            this.ensureOverlayDrawingOrder();
            
            // Hide loading screen for non-raster layers with a small delay
            this.hideLoadingScreen(300);
        } else if (layerType === 'oc' || layerType === 'ph' || layerType === 'meanTemp' || layerType === 'elevation' || layerType === 'nlcd' || layerType === 'lithology' ||
                   layerType === 'precipitation' || layerType === 'temperatureMean' || layerType === 'temperatureMin' || layerType === 'temperatureMax' ||
                   layerType === 'vpdMin' || layerType === 'vpdMax' || layerType === 'solarTotal' || layerType === 'solarSloped' || layerType === 'solarClear') {
            console.log(`🔄 Entering raster layer branch for ${layerType}`);
            // Hide soil polygons and permanent boundaries for raster layers
            if (soilLayer && this.map.hasLayer(soilLayer)) {
                this.map.removeLayer(soilLayer);
            }
            if (permanentBoundaryLayer && this.map.hasLayer(permanentBoundaryLayer)) {
                this.map.removeLayer(permanentBoundaryLayer);
            }
            // Also remove SSURGO boundaries if they were shown
            const ssurgoBoundaryLayer = this.layers.polygons.get('ssurgo-boundaries');
            if (ssurgoBoundaryLayer && this.map.hasLayer(ssurgoBoundaryLayer)) {
                this.map.removeLayer(ssurgoBoundaryLayer);
            }
            
            // Load raster layer first, then show appropriate legend
            await this.loadRasterLayer(layerType, depth);
            // Loading screen will be hidden by raster loading completion
        } else {
            console.log(`🔄 Falling through to else branch for ${layerType}`);
            // Hide soil-related layers for satellite/other, but keep monument boundary
            if (soilLayer && this.map.hasLayer(soilLayer)) {
                this.map.removeLayer(soilLayer);
            }
            if (permanentBoundaryLayer && this.map.hasLayer(permanentBoundaryLayer)) {
                this.map.removeLayer(permanentBoundaryLayer);
            }
            // Also remove SSURGO boundaries if they were shown
            const ssurgoBoundaryLayer = this.layers.polygons.get('ssurgo-boundaries');
            if (ssurgoBoundaryLayer && this.map.hasLayer(ssurgoBoundaryLayer)) {
                this.map.removeLayer(ssurgoBoundaryLayer);
            }
            if (legendElement) {
                legendElement.style.display = 'none';
            }
            // Hide loading screen for non-raster layers with a small delay
            this.hideLoadingScreen(300);
        }
        
        } catch (error) {
            console.error('🔄 ERROR in updateLayers:', error);
            console.error('Stack trace:', error.stack);
            // Hide loading screen on error
            this.hideLoadingScreen(0);
        }
    }
    
    // Hide loading screen with optional delay
    hideLoadingScreen(delay = 0) {
        const loadingElement = document.getElementById('loading');
        if (loadingElement) {
            if (delay > 0) {
                setTimeout(() => {
                    loadingElement.style.display = 'none';
                }, delay);
            } else {
                loadingElement.style.display = 'none';
            }
        }
    }
    
    // Load raster layer
    async loadRasterLayer(property, depth) {
        console.log(`🗺️ MAP: loadRasterLayer called for ${property}, depth ${depth}`);
        
        // Ensure loading screen is visible for raster loading
        const loadingElement = document.getElementById('loading');
        if (loadingElement) {
            loadingElement.style.display = 'flex';
            loadingElement.style.visibility = 'visible';
            loadingElement.style.opacity = '1';
            loadingElement.style.zIndex = '2000';
        }
        
        // Check if this raster layer is already loaded and cached
        const cacheKey = `${property}_${depth}`;
        const cachedLayer = this.layers.rasters.get(cacheKey);
        
        if (cachedLayer) {
            if (!this.map.hasLayer(cachedLayer)) {
                // Layer cached but not on map, add it
                cachedLayer.addTo(this.map);
                this.currentRasterLayer = cachedLayer;
            }
            // Layer already loaded, hide loading screen with small delay
            this.hideLoadingScreen(300);
            // Show appropriate legend
            if (cachedLayer.dataRange) {
                this.showRasterLegend(property, depth, cachedLayer.dataRange);
            }
            return;
        }
        
        // Loading screen is now handled in updateLayers method
        
        // For elevation, create hillshade background first
        if (property === 'elevation') {
            // Only create hillshade if it doesn't exist
            if (!this.hillshadeLayer || !this.map.hasLayer(this.hillshadeLayer)) {
                await this.createHillshadeBackground();
            }
        }
        
        // Create either a real TIFF layer or fall back to mock data
        console.log(`🗺️ MAP: Calling createRasterLayer for ${property}`);
        const rasterInfo = await this.createRasterLayer(property, depth);
        console.log(`🗺️ MAP: createRasterLayer returned:`, rasterInfo);
        
        // Don't hide loading screen for elevation here - let the progress completion event handle it
        // This allows the processing percentage to be displayed
        
        // Show appropriate legend after raster is loaded
        if (rasterInfo && rasterInfo.dataRange) {
            this.showRasterLegend(property, depth, rasterInfo.dataRange);
        }
    }
    
    // Create a raster layer from TIFF file or fall back to mock
    async createRasterLayer(property, depth) {
        const depthLabel = CONFIG.depthLevels.labels[depth];
        
        console.log(`🗺️ MAP: createRasterLayer - rasterManager exists:`, !!window.rasterManager);
        
        try {
            // Try to create a real TIFF layer first
            let rasterResult = await window.rasterManager.createTiffLayer(property, depth);
            
            // Store unique values for classification rasters
            if (rasterResult && rasterResult.uniqueValues && (property === 'nlcd' || property === 'lithology')) {
                this.rasterUniqueValues[property] = rasterResult.uniqueValues;
            }
            
            if (!rasterResult || !rasterResult.layer) {
                
                // Fall back to mock raster overlay
                const bounds = [
                    [41.9, -122.7],  // Southwest corner
                    [42.3, -122.3]   // Northeast corner
                ];
                const rasterLayer = window.rasterManager.createMockRasterOverlay(property, depth, bounds);
                let dataRange;
                if (property === 'oc') {
                    dataRange = { min: 0, max: 20 };
                } else if (property === 'ph') {
                    dataRange = { min: 4.0, max: 8.5 };
                } else if (property === 'meanTemp') {
                    dataRange = { min: 8.0, max: 18.0 };
                } else {
                    dataRange = { min: 40, max: 80 };
                }
                rasterResult = {
                    layer: rasterLayer,
                    dataRange: dataRange
                };
            }
            
            // Add to map and store reference
            if (rasterResult && rasterResult.layer) {
                rasterResult.layer.addTo(this.map);
                this.currentRasterLayer = rasterResult.layer;
                // Store the layer with its dataRange for caching
                rasterResult.layer.dataRange = rasterResult.dataRange;
                this.layers.rasters.set(`${property}_${depth}`, rasterResult.layer);
                
                return rasterResult;
            } else {
                // Hide loading screen on failure
                this.hideLoadingScreen(300);
                return null;
            }
        } catch (error) {
            // Hide loading screen on error
            this.hideLoadingScreen(300);
            return null;
        }
    }
    
    // Show soil order legend
    showSoilOrderLegend() {
        const legendElement = document.getElementById('soil-legend');
        const legendItems = document.getElementById('legend-items');
        
        if (!legendElement || !legendItems) return;
        
        // Clear existing items
        legendItems.innerHTML = '';
        
        // Update legend title explicitly
        const legendTitle = legendElement.querySelector('h4');
        if (legendTitle) {
            legendTitle.textContent = 'Soil Orders';
        }
        
        // Get available soil orders from data
        const availableOrders = this.getAvailableSoilOrders();
        
        // Create legend items
        availableOrders.forEach(order => {
            const color = ConfigUtils.getSoilOrderColor(order);
            const item = document.createElement('div');
            item.className = 'legend-item';
            item.innerHTML = `
                <div class="legend-color" style="background-color: ${color};"></div>
                <span>${order}</span>
            `;
            legendItems.appendChild(item);
        });
        
        legendElement.style.display = 'block';
    }
    
    // Get available soil orders from loaded data (only from dominant components)
    getAvailableSoilOrders() {
        // Use the filtered dominant features if available
        if (this.dominantFeatures && this.dominantFeatures.length > 0) {
            const orders = new Set();
            
            // Process only the dominant features that are actually rendered
            this.dominantFeatures.forEach(feature => {
                const order = this.extractSoilOrder(feature.properties);
                orders.add(order);
            });
            
            // Sort orders alphabetically, putting Unknown at the end
            const sortedOrders = Array.from(orders).sort((a, b) => {
                if (a === 'Unknown') return 1;
                if (b === 'Unknown') return -1;
                return a.localeCompare(b);
            });
            
            return sortedOrders;
        }
        
        // Fallback if dominant features not yet loaded
        if (!this.data || !this.data.soilPolygons) {
            return Object.keys(CONFIG.soilOrderColors);
        }
        
        const orders = new Set();
        
        // Only process dominant components to match what's displayed on the map
        this.data.soilPolygons.features.forEach(feature => {
            if (this.isFeatureDominant(feature)) {
                const order = this.extractSoilOrder(feature.properties);
                orders.add(order);
            }
        });
        
        // Sort orders alphabetically, putting Unknown at the end
        const sortedOrders = Array.from(orders).sort((a, b) => {
            if (a === 'Unknown') return 1;
            if (b === 'Unknown') return -1;
            return a.localeCompare(b);
        });
        
        return sortedOrders;
    }
    
    // Show particle size legend
    showParticleSizeLegend() {
        const legendElement = document.getElementById('soil-legend');
        const legendItems = document.getElementById('legend-items');
        
        if (!legendElement || !legendItems) return;
        
        // Clear existing items
        legendItems.innerHTML = '';
        
        // Update legend title
        const legendTitle = legendElement.querySelector('h4');
        if (legendTitle) {
            legendTitle.textContent = 'Family Particle Classes';
        }
        
        // Get available particle sizes from data
        const availableSizes = this.getAvailableParticleSizes();
        
        // Create legend items
        availableSizes.forEach(size => {
            const color = ConfigUtils.getParticleSizeColor(size);
            const item = document.createElement('div');
            item.className = 'legend-item';
            item.innerHTML = `
                <div class="legend-color" style="background-color: ${color};"></div>
                <span>${size}</span>
            `;
            legendItems.appendChild(item);
        });
        
        legendElement.style.display = 'block';
    }
    
    // Get available particle sizes from loaded data (only from dominant components)
    getAvailableParticleSizes() {
        // Use the filtered dominant features if available
        if (this.dominantFeatures && this.dominantFeatures.length > 0) {
            const sizes = new Set();
            
            // Process only the dominant features that are actually rendered
            this.dominantFeatures.forEach(feature => {
                const size = this.extractParticleSize(feature.properties);
                // Only add sizes that have defined colors and are not Unknown or "not used"
                if (size && 
                    CONFIG.particleSizeColors[size] && 
                    size !== 'Unknown' && 
                    size !== 'not used') {
                    sizes.add(size);
                }
            });
            
            // Sort sizes by texture categories
            const sortedSizes = Array.from(sizes).sort((a, b) => {
                // Define sort order based on texture categories
                const order = {
                    'very-fine': 1,
                    'fine': 2,
                    'fine-loamy': 3,
                    'coarse-loamy': 4,
                    'fine-silty': 5,
                    'coarse-silty': 6,
                    'sandy': 7,
                    'sandy-skeletal': 8,
                    'loamy-skeletal': 9,
                    'clayey-skeletal': 10,
                    'fragmental': 11,
                    'cindery': 12,
                    'pumiceous': 13,
                    'medial': 14,
                    'medial-skeletal': 15,
                    'hydrous': 16,
                    'hydrous-skeletal': 17
                };
                
                return (order[a] || 99) - (order[b] || 99);
            });
            
            return sortedSizes;
        }
        
        // Fallback if dominant features not yet loaded
        if (!this.data || !this.data.soilPolygons) {
            return [];
        }
        
        const sizes = new Set();
        
        // Only process dominant components to match what's displayed on the map
        this.data.soilPolygons.features.forEach(feature => {
            if (this.isFeatureDominant(feature)) {
                const size = this.extractParticleSize(feature.properties);
                // Only add sizes that have defined colors and are not Unknown or "not used"
                if (size && 
                    CONFIG.particleSizeColors[size] && 
                    size !== 'Unknown' && 
                    size !== 'not used') {
                    sizes.add(size);
                }
            }
        });
        
        // Sort sizes by texture categories
        const sortedSizes = Array.from(sizes).sort((a, b) => {
            // Define sort order based on texture categories
            const order = {
                'very-fine': 1,
                'fine': 2,
                'fine-silty': 3,
                'fine-loamy': 4,
                'loamy': 5,
                'coarse-loamy': 6,
                'sandy': 7,
                'sandy-skeletal': 8,
                'loamy-skeletal': 9,
                'clayey-skeletal': 10,
                'medial-skeletal': 11,
                'clayey': 12,
                'medial': 13,
                'fine-loamy over clayey': 14
            };
            
            return (order[a] || 999) - (order[b] || 999);
        });
        
        return sortedSizes;
    }

    // Show parent material legend
    showParentMaterialLegend() {
        const legendElement = document.getElementById('soil-legend');
        const legendItems = document.getElementById('legend-items');
        
        if (!legendElement || !legendItems) return;
        
        // Clear existing items
        legendItems.innerHTML = '';
        
        // Update legend title
        const legendTitle = legendElement.querySelector('h4');
        if (legendTitle) {
            legendTitle.textContent = 'Parent Material Classes';
        }
        
        // Get available parent materials from data
        const availableMaterials = this.getAvailableParentMaterials();
        
        // Create legend items
        availableMaterials.forEach(material => {
            const color = ConfigUtils.getParentMaterialColor(material);
            const item = document.createElement('div');
            item.className = 'legend-item';
            item.innerHTML = `
                <div class="legend-color" style="background-color: ${color};"></div>
                <span>${material}</span>
            `;
            legendItems.appendChild(item);
        });
        
        legendElement.style.display = 'block';
    }
    
    // Get available parent materials from loaded data (only from dominant components)
    getAvailableParentMaterials() {
        // Use the filtered dominant features if available
        if (this.dominantFeatures && this.dominantFeatures.length > 0) {
            const materials = new Set();
            
            // Process only the dominant features that are actually rendered
            this.dominantFeatures.forEach(feature => {
                const material = this.extractParentMaterial(feature.properties);
                // Only add materials that have defined colors and are not Unknown
                if (material && 
                    CONFIG.parentMaterialColors[material] && 
                    material !== 'Unknown') {
                    materials.add(material);
                }
            });
            
            // Sort materials alphabetically, putting Unknown at the end
            const sortedMaterials = Array.from(materials).sort((a, b) => {
                if (a === 'Unknown') return 1;
                if (b === 'Unknown') return -1;
                return a.localeCompare(b);
            });
            
            return sortedMaterials;
        }
        
        // Fallback if dominant features not yet loaded
        if (!this.data || !this.data.soilPolygons) {
            return [];
        }
        
        const materials = new Set();
        
        // Only process dominant components to match what's displayed on the map
        this.data.soilPolygons.features.forEach(feature => {
            if (this.isFeatureDominant(feature)) {
                const material = this.extractParentMaterial(feature.properties);
                // Only add materials that have defined colors and are not Unknown
                if (material && 
                    CONFIG.parentMaterialColors[material] && 
                    material !== 'Unknown') {
                    materials.add(material);
                }
            }
        });
        
        // Sort materials alphabetically, putting Unknown at the end
        const sortedMaterials = Array.from(materials).sort((a, b) => {
            if (a === 'Unknown') return 1;
            if (b === 'Unknown') return -1;
            return a.localeCompare(b);
        });
        
        return sortedMaterials;
    }
    
    // Show raster legend for OC, pH, and Land Cover
    showRasterLegend(property, depth, dataRange) {
        const legendElement = document.getElementById('soil-legend');
        const legendItems = document.getElementById('legend-items');
        
        if (!legendElement || !legendItems) return;
        
        // Clear existing items
        legendItems.innerHTML = '';
        
        if (property === 'nlcd') {
            // Show NLCD classification legend
            this.showNLCDLegend(legendElement, legendItems);
        } else if (property === 'lithology') {
            // Show lithology classification legend
            this.showLithologyLegend(legendElement, legendItems);
        } else if (property === 'elevation') {
            // Show elevation gradient legend
            this.showElevationLegend(legendElement, legendItems, dataRange);
        } else {
            // Show continuous raster legend
            const depthLabel = CONFIG.depthLevels.labels[depth];
            let propertyName;
            let units;
            
            // Handle both soil properties and climate variables
            if (property === 'oc') {
                propertyName = 'Soil Organic Carbon';
                units = '[g/kg]';
            } else if (property === 'ph') {
                propertyName = 'Soil pH';
                units = '[pH]';
            } else if (property === 'meanTemp') {
                propertyName = 'Mean Temperature';
                units = '[°C]';
            } else if (property === 'precipitation') {
                propertyName = 'Annual Precipitation';
                units = '[mm]';
            } else if (property === 'temperatureMean') {
                propertyName = 'Mean Temperature';
                units = '[°C]';
            } else if (property === 'temperatureMin') {
                propertyName = 'Minimum Temperature';
                units = '[°C]';
            } else if (property === 'temperatureMax') {
                propertyName = 'Maximum Temperature';
                units = '[°C]';
            } else if (property === 'vpdMin') {
                propertyName = 'Min Vapor Pressure Deficit';
                units = '[hPa]';
            } else if (property === 'vpdMax') {
                propertyName = 'Max Vapor Pressure Deficit';
                units = '[hPa]';
            } else if (property === 'solarTotal') {
                propertyName = 'Total Solar Radiation';
                units = '[MJ/m²/day]';
            } else if (property === 'solarSloped') {
                propertyName = 'Sloped Solar Radiation';
                units = '[MJ/m²/day]';
            } else if (property === 'solarClear') {
                propertyName = 'Clear Sky Solar Radiation';
                units = '[MJ/m²/day]';
            }
            
            // Set legend title with units
            const legendTitle = legendElement.querySelector('h4');
            if (legendTitle) {
                // For climate variables, don't show depth label
                const climateVariables = ['precipitation', 'temperatureMean', 'temperatureMin', 'temperatureMax',
                                        'vpdMin', 'vpdMax', 'solarTotal', 'solarSloped', 'solarClear'];
                if (climateVariables.includes(property)) {
                    legendTitle.textContent = `${propertyName} ${units}`;
                } else {
                    legendTitle.textContent = `${propertyName} (${depthLabel}) ${units}`;
                }
            }
            
            // Create color scale legend
            this.createColorScaleLegend(property, dataRange, legendItems);
        }
        
        legendElement.style.display = 'block';
    }
    
    // Show elevation gradient legend
    showElevationLegend(legendElement, legendItems, dataRange) {
        // Set legend title
        const legendTitle = legendElement.querySelector('h4');
        if (legendTitle) {
            legendTitle.textContent = 'Elevation with Hillshade [meters]';
        }
        
        // Create elevation color scale legend
        this.createColorScaleLegend('elevation', dataRange, legendItems);
    }
    
    // Show NLCD classification legend
    showNLCDLegend(legendElement, legendItems) {
        // Set legend title
        const legendTitle = legendElement.querySelector('h4');
        if (legendTitle) {
            legendTitle.textContent = 'Land Cover Classification';
        }
        
        // Get unique values from the raster if available
        const uniqueValues = this.rasterUniqueValues?.nlcd || new Set();
        
        // Create legend items only for values that exist in the raster
        Object.entries(CONFIG.nlcdColors).forEach(([value, info]) => {
            // Only show legend item if this value exists in the raster data
            if (uniqueValues.size === 0 || uniqueValues.has(parseInt(value))) {
                const item = document.createElement('div');
                item.className = 'legend-item';
                item.innerHTML = `
                    <div class="legend-color" style="background-color: ${info.color};"></div>
                    <span>${info.name}</span>
                `;
                legendItems.appendChild(item);
            }
        });
    }
    
    // Show lithology classification legend
    showLithologyLegend(legendElement, legendItems) {
        // Set legend title
        const legendTitle = legendElement.querySelector('h4');
        if (legendTitle) {
            legendTitle.textContent = 'Parent Material Classification';
        }
        
        // Get unique values from the raster if available
        const uniqueValues = this.rasterUniqueValues?.lithology || new Set();
        
        // Create legend items only for values that exist in the raster data
        Object.entries(CONFIG.lithologyColors).forEach(([value, info]) => {
            // Only show legend item if this value exists in the raster data
            if (uniqueValues.size === 0 || uniqueValues.has(parseInt(value))) {
                const item = document.createElement('div');
                item.className = 'legend-item';
                item.innerHTML = `
                    <div class="legend-color" style="background-color: ${info.color};"></div>
                    <span>${info.name}</span>
                `;
                legendItems.appendChild(item);
            }
        });
    }
    
    // Create color scale legend with tick marks
    createColorScaleLegend(property, dataRange, container) {
        const { min, max } = dataRange;
        
        // Create legend container
        const scaleContainer = document.createElement('div');
        scaleContainer.className = 'raster-legend-scale';
        scaleContainer.style.cssText = `
            width: 200px;
            height: 60px;
            position: relative;
            margin: 10px 0;
        `;
        
        // Create color gradient bar
        const colorBar = document.createElement('div');
        colorBar.style.cssText = `
            width: 100%;
            height: 20px;
            background: linear-gradient(to right, ${this.getColorGradient(property, min, max)});
            border: 1px solid #333;
            margin-bottom: 5px;
        `;
        
        // Create tick marks and labels
        const ticksContainer = document.createElement('div');
        ticksContainer.style.cssText = `
            width: 100%;
            height: 35px;
            position: relative;
        `;
        
        // Add 5 tick marks across the scale
        const numTicks = 5;
        for (let i = 0; i < numTicks; i++) {
            const position = (i / (numTicks - 1)) * 100;
            const value = min + (max - min) * (i / (numTicks - 1));
            
            // Create tick mark
            const tick = document.createElement('div');
            tick.style.cssText = `
                position: absolute;
                left: ${position}%;
                transform: translateX(-50%);
                width: 1px;
                height: 8px;
                background: #333;
                top: 0;
            `;
            
            // Create label
            const label = document.createElement('div');
            label.style.cssText = `
                position: absolute;
                left: ${position}%;
                transform: translateX(-50%);
                top: 12px;
                font-size: 11px;
                color: #333;
                text-align: center;
                white-space: nowrap;
            `;
            
            // Format value based on property
            let displayValue;
            const climateVariables = ['precipitation', 'temperatureMean', 'temperatureMin', 'temperatureMax',
                                    'vpdMin', 'vpdMax', 'solarTotal', 'solarSloped', 'solarClear'];
            
            if (property === 'elevation') {
                // Elevation values in meters, no decimals needed
                displayValue = Math.round(value).toString();
            } else if (property === 'ph') {
                if (value > 10) {
                    // pH values are scaled by 10
                    displayValue = (value / 10).toFixed(1);
                } else {
                    displayValue = value.toFixed(1);
                }
            } else if (property === 'precipitation') {
                // Precipitation in mm, show as integer
                displayValue = Math.round(value).toString();
            } else if (property.startsWith('temperature')) {
                // Temperature in °C, show one decimal
                displayValue = value.toFixed(1);
            } else if (property.startsWith('vpd')) {
                // VPD in hPa, show one decimal
                displayValue = value.toFixed(1);
            } else if (property.startsWith('solar')) {
                // Solar radiation in MJ/m²/day, show one decimal
                displayValue = value.toFixed(1);
            } else {
                // OC and other values
                displayValue = value.toFixed(1);
            }
            
            label.textContent = displayValue;
            
            ticksContainer.appendChild(tick);
            ticksContainer.appendChild(label);
        }
        
        scaleContainer.appendChild(colorBar);
        scaleContainer.appendChild(ticksContainer);
        container.appendChild(scaleContainer);
    }
    
    // Get color gradient string for CSS
    getColorGradient(property, min, max) {
        const steps = 10;
        const colors = [];
        
        // Check if this is a climate variable
        const climateVariables = ['precipitation', 'temperatureMean', 'temperatureMin', 'temperatureMax',
                                'vpdMin', 'vpdMax', 'solarTotal', 'solarSloped', 'solarClear'];
        
        for (let i = 0; i <= steps; i++) {
            const value = min + (max - min) * (i / steps);
            let color;
            
            if (climateVariables.includes(property)) {
                // Use the same color logic as in raster-utils.js
                color = window.rasterManager.getColorForValue(property, value, min, max);
            } else if (property === 'elevation') {
                // Elevation uses terrain color gradient
                const normalized = i / steps;
                color = ConfigUtils.getElevationColor(normalized);
            } else if (property === 'oc') {
                // Enhanced OC gradient matching the raster colors
                const normalized = (value - min) / (max - min);
                
                if (normalized < 0.2) {
                    // Very low values: light cream/yellow
                    const intensity = normalized / 0.2;
                    const r = Math.floor(255 - (55 * intensity));  // 255 to 200
                    const g = Math.floor(255 - (55 * intensity));  // 255 to 200  
                    const b = Math.floor(220 - (120 * intensity)); // 220 to 100
                    color = `rgb(${r}, ${g}, ${b})`;
                } else if (normalized < 0.5) {
                    // Low-medium values: orange/light brown
                    const intensity = (normalized - 0.2) / 0.3;
                    const r = Math.floor(200 - (50 * intensity));  // 200 to 150
                    const g = Math.floor(200 - (100 * intensity)); // 200 to 100
                    const b = Math.floor(100 - (70 * intensity));  // 100 to 30
                    color = `rgb(${r}, ${g}, ${b})`;
                } else {
                    // High values: dark brown to very dark brown
                    const intensity = (normalized - 0.5) / 0.5;
                    const r = Math.floor(150 - (90 * intensity));  // 150 to 60
                    const g = Math.floor(100 - (70 * intensity));  // 100 to 30
                    const b = Math.floor(30 - (20 * intensity));   // 30 to 10
                    color = `rgb(${r}, ${g}, ${b})`;
                }
            } else {
                // pH red-green-blue gradient - more saturated for legend visibility
                const range = max - min;
                const normalized = (value - min) / range;
                
                if (normalized < 0.33) {
                    const intensity = normalized / 0.33;
                    color = `rgb(${Math.floor(220 + 35 * (1 - intensity))}, ${Math.floor(40 * intensity)}, 40)`;
                } else if (normalized > 0.67) {
                    const intensity = (normalized - 0.67) / 0.33;
                    color = `rgb(40, ${Math.floor(40 * (1 - intensity))}, ${Math.floor(120 + 135 * intensity)})`;
                } else {
                    const intensity = Math.abs(normalized - 0.5) / 0.17;
                    color = `rgb(40, ${Math.floor(180 + 75 * (1 - intensity))}, 40)`;
                }
            }
            
            colors.push(color);
        }
        
        return colors.join(', ');
    }
    
    // Utility function to darken a color
    darkenColor(color, factor) {
        // Simple color darkening - convert hex to RGB and darken
        const hex = color.replace('#', '');
        const r = parseInt(hex.substr(0, 2), 16);
        const g = parseInt(hex.substr(2, 2), 16);
        const b = parseInt(hex.substr(4, 2), 16);
        
        const darkenedR = Math.round(r * (1 - factor));
        const darkenedG = Math.round(g * (1 - factor));
        const darkenedB = Math.round(b * (1 - factor));
        
        return `rgb(${darkenedR}, ${darkenedG}, ${darkenedB})`;
    }
    
    // Get map instance
    getMap() {
        return this.map;
    }
    
    // Hide all layers except monument boundary
    hideAllLayers() {
        const monumentBoundaryLayer = this.layers.overlays.get('monument-boundary');
        const legendElement = document.getElementById('soil-legend');
        
        // Hide legend
        if (legendElement) {
            legendElement.style.display = 'none';
        }
        
        // Remove all polygon layers
        this.layers.polygons.forEach((layer, key) => {
            if (this.map.hasLayer(layer)) {
                this.map.removeLayer(layer);
            }
        });
        
        // Remove current raster layer
        if (this.currentRasterLayer && this.map.hasLayer(this.currentRasterLayer)) {
            this.map.removeLayer(this.currentRasterLayer);
            this.currentRasterLayer = null;
        }
        
        // Remove hillshade layer
        if (this.hillshadeLayer && this.map.hasLayer(this.hillshadeLayer)) {
            this.map.removeLayer(this.hillshadeLayer);
        }
        
        // Ensure monument boundary stays visible
        if (monumentBoundaryLayer && !this.map.hasLayer(monumentBoundaryLayer)) {
            monumentBoundaryLayer.addTo(this.map);
        }
    }
    
    // Cleanup
    destroy() {
        if (this.map) {
            this.map.remove();
            this.map = null;
        }
        this.layers.base.clear();
        this.layers.polygons.clear();
        this.layers.overlays.clear();
    }
}

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { MapManager };
}// Cache bust: Tue Sep  2 17:31:43 PDT 2025
