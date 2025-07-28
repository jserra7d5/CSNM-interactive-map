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
    
    // Create base map layers
    createBaseLayers() {
        console.log('Creating base layers...');
        Object.entries(CONFIG.baseLayers).forEach(([key, config]) => {
            console.log(`Creating layer: ${key} with URL: ${config.url}`);
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
        console.log('Base layers created:', Array.from(this.layers.base.keys()));
    }
    
    // Set base layer
    setBaseLayer(layerKey) {
        console.log(`Setting base layer to: ${layerKey}`);
        
        // Remove current base layer
        if (this.currentBaseLayer) {
            this.map.removeLayer(this.currentBaseLayer);
        }
        
        // Add new base layer
        const newLayer = this.layers.base.get(layerKey);
        if (newLayer) {
            newLayer.addTo(this.map);
            this.currentBaseLayer = newLayer;
            console.log(`Base layer ${layerKey} added successfully`);
        } else {
            console.error(`Base layer ${layerKey} not found!`);
            console.log('Available layers:', Array.from(this.layers.base.keys()));
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
            console.log(`Raster processing progress for ${property}: ${progress}% - ${message}`);
            
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
    
    // Handle map click events
    handleMapClick(e) {
        const { lat, lng } = e.latlng;
        
        // Emit custom event for other components to listen to
        const event = new CustomEvent('mapClick', {
            detail: { lat, lng, originalEvent: e }
        });
        document.dispatchEvent(event);
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
        console.log('Map initialized successfully');
        
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
            console.warn('No soil polygon data available');
            return;
        }
        
        const features = data.soilPolygons.features;
        const totalFeatures = features.length;
        console.log(`Loading ${totalFeatures} soil polygon features...`);
        
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
                    console.log(`Loaded ${loaded}/${totalFeatures} features (${progress}%)`);
                }
                
                // Continue loading if more features remain
                if (loaded < totalFeatures) {
                    requestAnimationFrame(loadBatch);
                } else {
                    console.log('All soil polygons loaded successfully');
                }
            };
            
            // Start loading
            loadBatch();
        };
        
        // Use progressive loading for large datasets
        if (totalFeatures > 1000) {
            loadFeaturesProgressively();
        } else {
            // Load all at once for smaller datasets with canvas renderer
            const allSoilLayer = L.geoJSON(data.soilPolygons, {
                style: (feature) => this.getSoilFillStyle(feature),
                onEachFeature: (feature, layer) => this.onEachPolygon(feature, layer),
                renderer: L.canvas({ padding: 0.5 })
            });
            
            const allPermanentBoundary = L.geoJSON(data.soilPolygons, {
                style: (feature) => this.getPermanentBoundaryStyle(feature),
                interactive: false,
                renderer: L.canvas({ padding: 0.5 })
            });
            
            const allToggleableBoundary = L.geoJSON(data.soilPolygons, {
                style: (feature) => this.getToggleableBoundaryStyle(feature),
                interactive: false,
                renderer: L.canvas({ padding: 0.5 })
            });
            
            const allSsurgoBoundary = L.geoJSON(data.soilPolygons, {
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
        console.log('=== SOIL POLYGON LAYERS STORED ===');
        console.log('Soil layer size:', soilLayer.getLayers().length);
        console.log('Permanent boundaries size:', permanentBoundaryLayer.getLayers().length);
        console.log('Toggleable boundaries size:', toggleableBoundaryLayer.getLayers().length);
        console.log('SSURGO boundaries size:', ssurgoBoundaryLayer.getLayers().length);
        
        // Don't add layers to map here - let updateLayers handle it when user selects a map type
        this.currentPolygonLayer = soilLayer;
        
        // Fit map to polygon bounds (but don't display the layer)
        this.fitToBounds(soilLayer);
    }
    
    // Load and display boundary polygon
    async loadBoundaryPolygon(data) {
        if (!data.boundaryPolygon) {
            console.warn('No boundary polygon data available');
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
        
        console.log('Monument boundary loaded and displayed');
    }
    
    // Load and display highways
    async loadHighways(data) {
        if (!data.highways) {
            console.warn('No highway data available');
            return;
        }
        
        // Create highway layer (deep blue, thicker lines)
        this.highwayLayer = L.geoJSON(data.highways, {
            style: this.getHighwayStyle(),
            onEachFeature: (feature, layer) => this.onEachRoadFeature(feature, layer, 'Highway')
        });
        
        // Store layer
        this.layers.overlays.set('highways', this.highwayLayer);
        
        console.log('Highways loaded and ready');
    }
    
    // Load and display service roads
    async loadServiceRoads(data) {
        if (!data.serviceRoads) {
            console.warn('No service road data available');
            return;
        }
        
        // Create service roads layer (green, thinner lines)
        this.serviceRoadLayer = L.geoJSON(data.serviceRoads, {
            style: this.getServiceRoadStyle(),
            onEachFeature: (feature, layer) => this.onEachRoadFeature(feature, layer, 'Service Road')
        });
        
        // Store layer
        this.layers.overlays.set('service-roads', this.serviceRoadLayer);
        
        console.log('Service roads loaded and ready');
    }
    
    // Get style for soil-filled polygons
    getSoilFillStyle(feature) {
        // For soil orders view, show ALL components with their soil order colors
        const soilOrder = this.extractSoilOrder(feature.properties);
        const color = ConfigUtils.getSoilOrderColor(soilOrder);
        
        return {
            fillColor: color,
            weight: 0.5,
            color: '#333',
            opacity: 0.8,
            fillOpacity: 0.7,
            fill: true
        };
    }
    
    // Get style for particle size-filled polygons
    getParticleSizeFillStyle(feature) {
        // For particle size view, show ALL components with their particle size colors
        const particleSize = this.extractParticleSize(feature.properties);
        const color = ConfigUtils.getParticleSizeColor(particleSize);
        
        return {
            fillColor: color,
            weight: 0.5,
            color: '#333',
            opacity: 0.8,
            fillOpacity: 0.7,
            fill: true
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
    
    // Get style for SSURGO view boundaries (yellow like toggleable boundaries)
    getSsurgoBoundaryStyle(feature) {
        return {
            fillColor: 'transparent',
            fillOpacity: 0,
            color: '#FFD700',  // Yellow color matching toggleable boundaries
            weight: 1.5,       // Thinner for better performance
            opacity: 0.8,
            dashArray: null    // Solid line instead of dashed
        };
    }
    
    // Get style for toggleable boundaries (yellow, controlled by checkbox)
    getToggleableBoundaryStyle(feature) {
        return {
            fillColor: 'transparent',
            fillOpacity: 0,
            color: '#FFD700',
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
    
    // Extract soil order from feature properties
    extractSoilOrder(properties) {
        // Try different possible fields for soil order
        // First check the soilOrder property set by enhanceSoilPolygons
        let order = properties.soilOrder || properties.taxorder || properties.soilorder || 'Unknown';
        return order;
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
    
    // Setup interactions for each polygon
    onEachPolygon(feature, layer) {
        // Add unique identifier to the layer for reference
        layer.polygonId = feature.id || feature.properties.OBJECTID;
        layer.componentKey = feature.properties.cokey;
        
        // Store popup content but don't bind it yet
        layer._popupContent = this.createPopupContent(feature.properties);
        
        // Only add click handler, no hover effects
        layer.on({
            click: (e) => {
                console.log('Polygon clicked!');
                // Stop the click from propagating to the map
                L.DomEvent.stopPropagation(e);
                this.selectFeature(e);
            }
        });
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
        
        if (this.currentMapType === 'soil') {
            const soilOrder = this.extractSoilOrder(properties);
            return `
                <div class="simple-popup">
                    <strong>Map Unit:</strong> ${mapUnit}<br>
                    <strong>Soil Order:</strong> ${soilOrder}
                </div>
            `;
        } else if (this.currentMapType === 'particleSize') {
            const particleSize = this.extractParticleSize(properties);
            return `
                <div class="simple-popup">
                    <strong>Map Unit:</strong> ${mapUnit}<br>
                    <strong>Particle Size:</strong> ${particleSize}
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
        const layer = e.target;
        const feature = layer.feature;
        
        // Show simple popup for soil order and particle size views
        console.log('selectFeature - currentMapType:', this.currentMapType);
        if (this.currentMapType === 'soil' || this.currentMapType === 'particleSize') {
            console.log('Creating simple popup for', this.currentMapType);
            // Create and show simple popup
            const popupContent = this.createSimplePopupContent(feature.properties);
            console.log('Popup content:', popupContent);
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
        console.log(`Selected polygon - ID: ${layer.polygonId}, Component: ${layer.componentKey}, Map Unit: ${feature.properties.MUSYM}, Component Name: ${feature.properties.compname}`);
        
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
            console.warn('Toggleable boundary layer not available');
            return;
        }
        
        if (show) {
            boundaryLayer.addTo(this.map);
            console.log('Yellow map unit boundaries added to map');
        } else {
            this.map.removeLayer(boundaryLayer);
            console.log('Yellow map unit boundaries removed from map');
        }
    }
    
    // Toggle highway visibility
    toggleHighways(show) {
        const highwayLayer = this.layers.overlays.get('highways');
        if (!highwayLayer) {
            console.warn('Highway layer not available');
            return;
        }
        
        if (show) {
            highwayLayer.addTo(this.map);
            console.log('Highways added to map');
        } else {
            this.map.removeLayer(highwayLayer);
            console.log('Highways removed from map');
        }
    }
    
    // Toggle service road visibility
    toggleServiceRoads(show) {
        const serviceRoadLayer = this.layers.overlays.get('service-roads');
        if (!serviceRoadLayer) {
            console.warn('Service road layer not available');
            return;
        }
        
        if (show) {
            serviceRoadLayer.addTo(this.map);
            console.log('Service roads added to map');
        } else {
            this.map.removeLayer(serviceRoadLayer);
            console.log('Service roads removed from map');
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
                console.log('Information center marker added to map');
            }
        } else {
            if (this.informationCenterMarker && this.map.hasLayer(this.informationCenterMarker)) {
                this.map.removeLayer(this.informationCenterMarker);
                console.log('Information center marker removed from map');
            }
        }
    }
    
    // Create information center marker
    createInformationCenterMarker() {
        const poi = CONFIG.pointsOfInterest.informationCenter;
        const [lat, lng] = poi.coordinates;
        
        // Create a custom star icon
        const starIcon = L.divIcon({
            className: 'information-center-marker',
            html: '<div class="star-icon">★</div>',
            iconSize: [20, 20],
            iconAnchor: [10, 10],
            popupAnchor: [0, -10]
        });
        
        // Create marker
        this.informationCenterMarker = L.marker([lat, lng], {
            icon: starIcon
        });
        
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
            console.log('Creating hillshade background layer...');
            console.log('window.rasterManager available?', window.rasterManager);
            
            // Check if rasterManager is available
            if (!window.rasterManager) {
                console.error('rasterManager not available. Check if raster-utils.js loaded correctly.');
                return;
            }
            
            // Load hillshade TIFF
            const hillshadeTiff = await window.rasterManager.loadTiff(CONFIG.dataPaths.hillshade);
            if (!hillshadeTiff) {
                console.warn('Could not load hillshade data for background');
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
            
            console.log('Hillshade background layer created successfully');
            
        } catch (error) {
            console.error('Error creating hillshade background:', error);
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
        // Store current map type
        this.currentMapType = layerType;
        console.log('updateLayers - setting currentMapType to:', layerType);
        
        // If no layer type selected, hide all layers except monument boundary
        if (!layerType) {
            this.hideAllLayers();
            return;
        }
        
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
                    default: mapTypeName = 'map data';
                }
                loadingText.textContent = `Loading ${mapTypeName}...`;
            }
            
            // Show progress bar for raster layers
            if (progressContainer) {
                if (['oc', 'ph', 'meanTemp', 'landcover', 'elevation'].includes(layerType)) {
                    progressContainer.style.display = 'block';
                    const progressFill = loadingElement.querySelector('.loading-progress-fill');
                    const progressText = loadingElement.querySelector('.loading-progress-text');
                    if (progressFill) progressFill.style.width = '0%';
                    if (progressText) progressText.textContent = '0%';
                } else {
                    progressContainer.style.display = 'none';
                }
            }
            
            console.log('Setting loading screen to visible in updateLayers');
            loadingElement.style.display = 'flex';
            loadingElement.style.visibility = 'visible';
            loadingElement.style.opacity = '1';
            loadingElement.style.zIndex = '2000';
        }
        
        const legendElement = document.getElementById('soil-legend');
        const soilLayer = this.layers.polygons.get('soil');
        const permanentBoundaryLayer = this.layers.polygons.get('permanent-boundaries');
        const monumentBoundaryLayer = this.layers.overlays.get('monument-boundary');
        
        console.log(`Updating layers for type: ${layerType}`);
        console.log('Available polygon layers:', Array.from(this.layers.polygons.keys()));
        
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
        
        if (layerType === 'ssurgo') {
            // SSURGO view - show polygons with yellow boundaries for click detection
            if (soilLayer) {
                if (!this.map.hasLayer(soilLayer)) {
                    soilLayer.addTo(this.map);
                }
                // Update all polygons to have transparent fill with orange boundaries
                soilLayer.eachLayer((layer) => {
                    if (layer.setStyle) {
                        layer.setStyle({
                            fillColor: 'transparent',
                            fillOpacity: 0,
                            color: '#ff6600',  // Orange boundaries
                            weight: 0.65,      // Thin lines
                            opacity: 0.8
                        });
                    }
                });
            }
            // Hide legend for SSURGO view
            if (legendElement) {
                legendElement.style.display = 'none';
            }
            // Hide loading screen for non-raster layers with a small delay
            this.hideLoadingScreen(300);
        } else if (layerType === 'soil') {
            console.log('=== ACTIVATING SOIL ORDER VIEW ===');
            console.log('Soil layer exists:', !!soilLayer);
            console.log('Soil layer already on map:', soilLayer ? this.map.hasLayer(soilLayer) : false);
            
            // Show soil polygons, permanent boundaries, and legend
            if (soilLayer && !this.map.hasLayer(soilLayer)) {
                console.log('Adding soil layer to map');
                soilLayer.addTo(this.map);
            }
            
            // Restore original soil polygon colors
            if (soilLayer) {
                console.log('Updating soil layer styles for soil order view');
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
                                if (colorCount < 5) { // Log first 5 for debugging
                                    console.log(`Applied soil order style to layer ${colorCount}:`, style);
                                    colorCount++;
                                }
                            }
                        });
                    }
                });
                console.log(`Total soil layers processed: ${totalLayers}`);
            }
            
            // Bring soil layer to front to ensure it's visible
            if (soilLayer) {
                console.log('Bringing soil layer to front');
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
                        console.log('Test layer bounds:', bounds);
                        console.log('Test layer visible in viewport:', this.map.getBounds().intersects(bounds));
                        console.log('Test layer options:', layer.options);
                        return false; // Stop after first layer
                    }
                });
            }
            
            if (permanentBoundaryLayer && !this.map.hasLayer(permanentBoundaryLayer)) {
                console.log('Adding permanent boundary layer');
                permanentBoundaryLayer.addTo(this.map);
            }
            
            if (legendElement) {
                console.log('Showing soil order legend');
                this.showSoilOrderLegend();
            }
            
            console.log('=== SOIL ORDER VIEW ACTIVATION COMPLETE ===');
            
            // Hide loading screen for non-raster layers with a small delay
            this.hideLoadingScreen(300);
        } else if (layerType === 'particleSize') {
            console.log('=== ACTIVATING PARTICLE SIZE VIEW ===');
            
            // Show soil polygons with particle size colors
            if (soilLayer && !this.map.hasLayer(soilLayer)) {
                soilLayer.addTo(this.map);
            }
            
            // Update polygons with particle size colors
            if (soilLayer) {
                console.log('Updating soil layer styles for particle size view');
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
                                if (colorCount < 5) { // Log first 5 for debugging
                                    console.log(`Applied particle size style to layer ${colorCount}:`, style);
                                    colorCount++;
                                }
                            }
                        });
                    }
                });
                console.log(`Total particle size layers processed: ${totalLayers}`);
                
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
            
            console.log('=== PARTICLE SIZE VIEW ACTIVATION COMPLETE ===');
            
            // Hide loading screen for non-raster layers with a small delay
            this.hideLoadingScreen(300);
        } else if (layerType === 'oc' || layerType === 'ph' || layerType === 'meanTemp' || layerType === 'elevation' || layerType === 'nlcd' || layerType === 'lithology') {
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
    }
    
    // Hide loading screen with optional delay
    hideLoadingScreen(delay = 0) {
        console.log(`hideLoadingScreen called with delay: ${delay}ms`);
        const loadingElement = document.getElementById('loading');
        if (loadingElement) {
            if (delay > 0) {
                console.log(`Hiding loading screen after ${delay}ms delay`);
                setTimeout(() => {
                    loadingElement.style.display = 'none';
                }, delay);
            } else {
                console.log('Hiding loading screen immediately');
                loadingElement.style.display = 'none';
            }
        }
    }
    
    // Load raster layer for OC or pH
    async loadRasterLayer(property, depth) {
        console.log(`Loading ${property} raster for depth level ${depth}`);
        
        // Ensure loading screen is visible for raster loading
        const loadingElement = document.getElementById('loading');
        if (loadingElement) {
            console.log('Setting loading screen to visible in updateLayers');
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
            console.log(`Using cached ${property} layer for depth ${depth}`);
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
        const rasterInfo = await this.createRasterLayer(property, depth);
        
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
        
        try {
            // Try to create a real TIFF layer first
            let rasterResult = await window.rasterManager.createTiffLayer(property, depth);
            
            // Store unique values for classification rasters
            if (rasterResult && rasterResult.uniqueValues && (property === 'nlcd' || property === 'lithology')) {
                this.rasterUniqueValues[property] = rasterResult.uniqueValues;
                console.log(`Unique values for ${property}:`, Array.from(rasterResult.uniqueValues).sort((a, b) => a - b));
            }
            
            if (!rasterResult || !rasterResult.layer) {
                console.log(`TIFF loading failed for ${property}, falling back to mock data`);
                
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
                
                console.log(`${property.toUpperCase()} raster layer added for depth: ${depthLabel}`);
                return rasterResult;
            } else {
                console.error(`Failed to create raster layer for ${property}`);
                // Hide loading screen on failure
                this.hideLoadingScreen(300);
                return null;
            }
        } catch (error) {
            console.error(`Error creating raster layer for ${property}:`, error);
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
    
    // Get available soil orders from loaded data
    getAvailableSoilOrders() {
        if (!this.data || !this.data.soilPolygons) {
            return Object.keys(CONFIG.soilOrderColors);
        }
        
        const orders = new Set();
        this.data.soilPolygons.features.forEach(feature => {
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
    
    // Get available particle sizes from loaded data
    getAvailableParticleSizes() {
        if (!this.data || !this.data.soilPolygons) {
            return [];
        }
        
        const sizes = new Set();
        this.data.soilPolygons.features.forEach(feature => {
            const size = this.extractParticleSize(feature.properties);
            // Only add sizes that have defined colors
            if (size && CONFIG.particleSizeColors[size]) {
                sizes.add(size);
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
                'fine-loamy over clayey': 14,
                'not used': 15,
                'Unknown': 16
            };
            
            return (order[a] || 999) - (order[b] || 999);
        });
        
        return sortedSizes;
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
            // Show continuous raster legend for OC, pH, and Mean Temperature
            const depthLabel = CONFIG.depthLevels.labels[depth];
            let propertyName;
            if (property === 'oc') {
                propertyName = 'Soil Organic Carbon';
            } else if (property === 'ph') {
                propertyName = 'Soil pH';
            } else if (property === 'meanTemp') {
                propertyName = 'Mean Temperature';
            }
            
            // Set legend title with units
            const legendTitle = legendElement.querySelector('h4');
            if (legendTitle) {
                let units;
                if (property === 'oc') {
                    units = '[g/kg]';
                } else if (property === 'ph') {
                    units = '[pH]';
                } else if (property === 'meanTemp') {
                    units = '[°C]';
                }
                legendTitle.textContent = `${propertyName} (${depthLabel}) ${units}`;
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
            legendTitle.textContent = 'Lithology Classification';
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
            } else {
                // OC values
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
        
        for (let i = 0; i <= steps; i++) {
            const value = min + (max - min) * (i / steps);
            let color;
            
            if (property === 'elevation') {
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
}