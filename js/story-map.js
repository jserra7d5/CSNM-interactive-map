// Story Map JavaScript
// Interactive narrative for CSNM Soil Explorer

class StoryMap {
    constructor() {
        this.currentSection = 0;
        this.sections = [];
        this.maps = {};
        this.dataLoader = null;
        this.appData = null;
        this.scrollTimeout = null;
        this.isScrolling = false;
        
        this.init();
    }
    
    async init() {
        try {
            // Setup sections first (most important)
            this.setupSections();
            
            // Setup interactive elements
            this.setupInteractivity();
            
            // Setup scroll listeners
            this.setupScrollListeners();
            
            // Start story immediately so content is visible
            this.startStory();
            
            // Initialize data loader (async)
            try {
                this.dataLoader = window.dataLoader || dataLoader;
            } catch (e) {
                console.warn('Data loader not available, continuing without maps');
            }
            
            // Load data and initialize maps (optional)
            try {
                await this.loadStoryData();
                this.initializeMaps();
            } catch (error) {
                console.warn('Maps not available, story will continue without interactive maps:', error);
            }
            
        } catch (error) {
            console.error('Story Map initialization error:', error);
            // Still try to show content
            this.setupSections();
            this.startStory();
        }
    }
    
    setupSections() {
        this.sections = Array.from(document.querySelectorAll('.story-section'));
        console.log(`Found ${this.sections.length} story sections`);
    }
    
    setupScrollListeners() {
        // Throttled scroll handler for performance
        const handleScroll = () => {
            if (!this.isScrolling) {
                this.isScrolling = true;
                requestAnimationFrame(() => {
                    this.updateProgress();
                    this.checkSectionVisibility();
                    this.isScrolling = false;
                });
            }
        };
        
        window.addEventListener('scroll', handleScroll, { passive: true });
        
        // Intersection Observer for better performance
        const observer = new IntersectionObserver(
            (entries) => {
                entries.forEach((entry) => {
                    if (entry.isIntersecting) {
                        entry.target.classList.add('visible');
                        this.onSectionVisible(entry.target);
                    }
                });
            },
            {
                threshold: 0.3,
                rootMargin: '-10% 0px -10% 0px'
            }
        );
        
        this.sections.forEach(section => {
            observer.observe(section);
        });
    }
    
    setupInteractivity() {
        // Factor cards interaction
        this.setupFactorCards();
        
        // Property selector interaction
        this.setupPropertySelector();
        
        // Depth selector interaction
        this.setupDepthSelector();
        
        // Climate toggle interaction
        this.setupClimateToggle();
        
        // Restart button
        const restartBtn = document.getElementById('restart-story');
        if (restartBtn) {
            restartBtn.addEventListener('click', () => {
                this.restartStory();
            });
        }
    }
    
    setupFactorCards() {
        const factorCards = document.querySelectorAll('.factor-card');
        const factorDetails = document.querySelectorAll('.factor-detail');
        
        // Initialize the first active factor (climate)
        const activeCard = document.querySelector('.factor-card.active');
        if (activeCard) {
            const initialFactor = activeCard.getAttribute('data-factor');
            this.updateFactorsMap(initialFactor);
        }
        
        factorCards.forEach(card => {
            card.addEventListener('click', () => {
                const factor = card.getAttribute('data-factor');
                
                // Update active states
                factorCards.forEach(c => c.classList.remove('active'));
                factorDetails.forEach(d => d.classList.remove('active'));
                
                card.classList.add('active');
                const detail = document.querySelector(`[data-detail="${factor}"]`);
                if (detail) {
                    detail.classList.add('active');
                }
                
                // Update map if available
                this.updateFactorsMap(factor);
            });
        });
    }
    
    updateFactorsMap(factor) {
        // Get all CLORPT map images
        const allMaps = document.querySelectorAll('#clorpt-map-container img');
        const targetMap = document.getElementById(`clorpt-${factor}-img`);
        
        // Remove active class from all maps
        allMaps.forEach(img => {
            img.classList.remove('active');
        });
        
        // Add active class to the selected map
        if (targetMap) {
            targetMap.classList.add('active');
            console.log(`Showing ${factor} map:`, targetMap.id);
        } else {
            console.warn(`Map not found for factor: ${factor}`);
        }
        
        // Debug: Log current state
        console.log('CLORPT Maps state:', {
            factor: factor,
            targetMapId: `clorpt-${factor}-img`,
            targetMapFound: !!targetMap,
            allMapsCount: allMaps.length
        });
    }
    
    setupPropertySelector() {
        const propertyBtns = document.querySelectorAll('.property-btn');
        const propertyInfos = document.querySelectorAll('.property-info');
        
        propertyBtns.forEach(btn => {
            btn.addEventListener('click', () => {
                const property = btn.getAttribute('data-property');
                
                // Update active states
                propertyBtns.forEach(b => b.classList.remove('active'));
                propertyInfos.forEach(i => i.classList.remove('active'));
                
                btn.classList.add('active');
                const info = document.querySelector(`[data-info="${property}"]`);
                if (info) {
                    info.classList.add('active');
                }
                
                // Update screenshot visibility
                this.updatePropertiesScreenshots(property);
            });
        });
    }
    
    updatePropertiesScreenshots(property) {
        const ocScreenshot = document.getElementById('oc-screenshot');
        const phScreenshot = document.getElementById('ph-screenshot');
        
        if (ocScreenshot && phScreenshot) {
            if (property === 'oc') {
                ocScreenshot.style.display = 'block';
                phScreenshot.style.display = 'none';
            } else if (property === 'ph') {
                ocScreenshot.style.display = 'none';
                phScreenshot.style.display = 'block';
            }
        }
    }
    
    setupDepthSelector() {
        const depthOptions = document.querySelectorAll('.depth-option');
        
        depthOptions.forEach(option => {
            option.addEventListener('click', () => {
                const depth = parseInt(option.getAttribute('data-depth'));
                
                // Update active states
                depthOptions.forEach(o => o.classList.remove('active'));
                option.classList.add('active');
                
                // Update properties map with new depth
                const activeProperty = document.querySelector('.property-btn.active');
                if (activeProperty) {
                    const property = activeProperty.getAttribute('data-property');
                    this.updatePropertiesMap(property, depth);
                }
            });
        });
    }
    
    setupClimateToggle() {
        const climateBtns = document.querySelectorAll('.climate-btn');
        const precipitationImg = document.getElementById('precipitation-screenshot');
        const temperatureImg = document.getElementById('temperature-screenshot');
        const climateCaption = document.getElementById('climate-caption');
        
        climateBtns.forEach(btn => {
            btn.addEventListener('click', () => {
                const climate = btn.getAttribute('data-climate');
                
                // Update active states
                climateBtns.forEach(b => b.classList.remove('active'));
                btn.classList.add('active');
                
                // Toggle images
                if (climate === 'precipitation') {
                    if (precipitationImg) precipitationImg.style.display = 'block';
                    if (temperatureImg) temperatureImg.style.display = 'none';
                    if (climateCaption) {
                        climateCaption.textContent = 'Annual precipitation varies from 20 inches in rain shadow valleys to over 60 inches on exposed ridges';
                    }
                } else if (climate === 'temperature') {
                    if (precipitationImg) precipitationImg.style.display = 'none';
                    if (temperatureImg) temperatureImg.style.display = 'block';
                    if (climateCaption) {
                        climateCaption.textContent = 'Mean annual temperature ranges from 35°F at high elevations to 55°F in low valleys';
                    }
                }
            });
        });
    }
    
    async loadStoryData() {
        try {
            // Load essential data for story maps
            this.appData = await this.dataLoader.loadAllData();
            console.log('Story data loaded successfully');
        } catch (error) {
            console.error('Failed to load story data:', error);
            // Create fallback data for story to continue
            this.appData = {
                soilPolygons: null,
                boundaryPolygon: null
            };
        }
    }
    
    initializeMaps() {
        // Initialize each story map
        const mapConfigs = {
            'intro-map': {
                center: CONFIG.mapCenter,
                zoom: 10,
                layers: ['satellite', 'boundary']
            },
            'factors-map': {
                center: CONFIG.mapCenter,
                zoom: 11,
                layers: ['terrain', 'boundary']
            },
            'serpentine-map': {
                center: CONFIG.mapCenter,
                zoom: 12,
                layers: ['soil', 'boundary']
            },
            'volcanic-map': {
                center: CONFIG.mapCenter,
                zoom: 11,
                layers: ['elevation', 'boundary']
            },
            'climate-map': {
                center: CONFIG.mapCenter,
                zoom: 10,
                layers: ['meanTemp', 'boundary']
            },
            'properties-map': {
                center: CONFIG.mapCenter,
                zoom: 11,
                layers: ['oc', 'boundary']
            },
            'conservation-map': {
                center: CONFIG.mapCenter,
                zoom: 10,
                layers: ['satellite', 'boundary']
            }
        };
        
        Object.keys(mapConfigs).forEach(mapId => {
            this.createStoryMap(mapId, mapConfigs[mapId]);
        });
    }
    
    createStoryMap(containerId, config) {
        const container = document.getElementById(containerId);
        if (!container) {
            console.warn(`Map container ${containerId} not found`);
            return;
        }
        
        // Create Leaflet map
        const map = L.map(containerId, {
            center: config.center,
            zoom: config.zoom,
            zoomControl: false,
            scrollWheelZoom: false,
            dragging: true,
            touchZoom: true,
            doubleClickZoom: false,
            boxZoom: false,
            keyboard: false,
            attributionControl: false
        });
        
        // Add attribution control in bottom right
        L.control.attribution({
            position: 'bottomright',
            prefix: false
        }).addTo(map);
        
        // Store map reference
        this.maps[containerId] = {
            map: map,
            config: config,
            layers: {}
        };
        
        // Add base layer
        this.addBaseLayer(containerId, config.layers[0] || 'satellite');
        
        // Add boundary if specified
        if (config.layers.includes('boundary') && this.appData.boundaryPolygon) {
            this.addBoundaryLayer(containerId);
        }
        
        // Add initial data layers
        this.updateMapLayers(containerId, config.layers);
    }
    
    addBaseLayer(containerId, baseType) {
        const mapObj = this.maps[containerId];
        if (!mapObj) return;
        
        const layerConfig = CONFIG.baseLayers[baseType] || CONFIG.baseLayers.satellite;
        
        if (mapObj.layers.base) {
            mapObj.map.removeLayer(mapObj.layers.base);
        }
        
        mapObj.layers.base = L.tileLayer(layerConfig.url, {
            attribution: layerConfig.attribution,
            maxZoom: 18
        }).addTo(mapObj.map);
    }
    
    addBoundaryLayer(containerId) {
        const mapObj = this.maps[containerId];
        if (!mapObj || !this.appData.boundaryPolygon) return;
        
        if (mapObj.layers.boundary) {
            mapObj.map.removeLayer(mapObj.layers.boundary);
        }
        
        mapObj.layers.boundary = L.geoJSON(this.appData.boundaryPolygon, {
            style: {
                color: '#dc2626',
                weight: 3,
                opacity: 0.8,
                fillOpacity: 0.05,
                dashArray: '8, 8'
            }
        }).addTo(mapObj.map);
    }
    
    updateMapLayers(containerId, layers) {
        const mapObj = this.maps[containerId];
        if (!mapObj) return;
        
        // Add soil polygons for soil-related maps
        if (layers.includes('soil') && this.appData.soilPolygons) {
            this.addSoilLayer(containerId);
        }
        
        // Add other layers as needed
        if (layers.includes('elevation')) {
            this.addElevationLayer(containerId);
        }
    }
    
    addSoilLayer(containerId) {
        const mapObj = this.maps[containerId];
        if (!mapObj || !this.appData.soilPolygons) return;
        
        if (mapObj.layers.soil) {
            mapObj.map.removeLayer(mapObj.layers.soil);
        }
        
        mapObj.layers.soil = L.geoJSON(this.appData.soilPolygons, {
            style: (feature) => {
                const soilOrder = feature.properties.taxorder || feature.properties.soil_order;
                const color = ConfigUtils.getSoilOrderColor(soilOrder);
                return {
                    fillColor: color,
                    weight: 1,
                    opacity: 0.7,
                    color: '#666',
                    fillOpacity: 0.8
                };
            },
            onEachFeature: (feature, layer) => {
                const soilOrder = feature.properties.taxorder || feature.properties.soil_order || 'Unknown';
                const musym = feature.properties.MUSYM || feature.properties.musym || 'N/A';
                
                layer.bindPopup(`
                    <div class="story-popup">
                        <h5>Soil Information</h5>
                        <p><strong>Soil Order:</strong> ${soilOrder}</p>
                        <p><strong>Map Unit:</strong> ${musym}</p>
                    </div>
                `);
            }
        }).addTo(mapObj.map);
    }
    
    addElevationLayer(containerId) {
        // Placeholder for elevation layer - would integrate with raster system
        console.log(`Adding elevation layer to ${containerId}`);
    }
    
    updateProgress() {
        const scrollTop = window.pageYOffset || document.documentElement.scrollTop;
        const documentHeight = document.documentElement.scrollHeight - window.innerHeight;
        const progress = Math.min(scrollTop / documentHeight, 1);
        
        const progressFill = document.querySelector('.story-progress-fill');
        if (progressFill) {
            progressFill.style.width = `${progress * 100}%`;
        }
    }
    
    checkSectionVisibility() {
        const viewportCenter = window.innerHeight / 2;
        
        this.sections.forEach((section, index) => {
            const rect = section.getBoundingClientRect();
            const sectionCenter = rect.top + rect.height / 2;
            
            // Check if section center is near viewport center
            if (Math.abs(sectionCenter - viewportCenter) < window.innerHeight * 0.3) {
                if (this.currentSection !== index) {
                    this.currentSection = index;
                    this.onSectionChange(index);
                }
            }
        });
    }
    
    onSectionVisible(section) {
        const sectionType = section.getAttribute('data-section');
        console.log(`Section visible: ${sectionType}`);
        
        // Trigger any section-specific animations or updates
        this.animateSection(section);
    }
    
    onSectionChange(index) {
        const section = this.sections[index];
        if (!section) return;
        
        const sectionType = section.getAttribute('data-section');
        console.log(`Section changed to: ${sectionType} (${index})`);
        
        // Update maps based on current section
        this.updateMapsForSection(sectionType);
    }
    
    animateSection(section) {
        // Add entrance animations
        const elements = section.querySelectorAll('.story-text > *, .story-visual > *');
        elements.forEach((element, index) => {
            setTimeout(() => {
                element.style.opacity = '1';
                element.style.transform = 'translateY(0)';
            }, index * 100);
        });
    }
    
    updateMapsForSection(sectionType) {
        switch (sectionType) {
            case 'factors':
                // Start with climate (first in CLORPT)
                this.updateFactorsMap('climate');
                break;
            case 'serpentine':
                this.focusOnSerpentineAreas();
                break;
            case 'volcanic':
                this.focusOnVolcanicAreas();
                break;
            case 'properties':
                this.updatePropertiesMap('oc');
                break;
        }
    }
    
    updateFactorsMap(factor) {
        const mapId = 'factors-map';
        const mapObj = this.maps[mapId];
        if (!mapObj) return;
        
        console.log(`Updating factors map for: ${factor}`);
        
        // Update map based on CLORPT factor
        switch (factor) {
            case 'climate':
                this.addBaseLayer(mapId, 'satellite');
                // Could add climate data layer here if available
                break;
            case 'organisms':
                this.addBaseLayer(mapId, 'satellite');
                // Shows vegetation patterns
                break;
            case 'relief':
                this.addBaseLayer(mapId, 'topo');
                // Shows topographic relief
                break;
            case 'parent':
                this.addBaseLayer(mapId, 'terrain');
                // Shows geological substrate
                break;
            case 'time':
                this.addBaseLayer(mapId, 'terrain');
                // Shows landscape age features
                break;
        }
    }
    
    updatePropertiesMap(property, depth = 0) {
        const mapId = 'properties-map';
        const mapObj = this.maps[mapId];
        if (!mapObj) return;
        
        console.log(`Updating properties map for: ${property} at depth ${depth}`);
        
        // This would integrate with the raster system for actual data display
        // For now, just update the base layer
        switch (property) {
            case 'oc':
                this.addBaseLayer(mapId, 'terrain');
                break;
            case 'ph':
                this.addBaseLayer(mapId, 'satellite');
                break;
        }
    }
    
    focusOnSerpentineAreas() {
        const mapId = 'serpentine-map';
        const mapObj = this.maps[mapId];
        if (!mapObj) return;
        
        // Focus map on areas with serpentine soils
        console.log('Focusing on serpentine areas');
    }
    
    focusOnVolcanicAreas() {
        const mapId = 'volcanic-map';
        const mapObj = this.maps[mapId];
        if (!mapObj) return;
        
        // Focus map on volcanic areas
        console.log('Focusing on volcanic areas');
    }
    
    startStory() {
        // Initial animation
        document.body.classList.add('story-loaded');
        
        // Make sure first section is visible
        const firstSection = this.sections[0];
        if (firstSection) {
            firstSection.classList.add('visible');
        }
        
        console.log('Story map started');
    }
    
    restartStory() {
        // Scroll to top
        window.scrollTo({ top: 0, behavior: 'smooth' });
        
        // Reset all interactive states
        this.resetInteractiveStates();
        
        // Reset progress
        const progressFill = document.querySelector('.story-progress-fill');
        if (progressFill) {
            progressFill.style.width = '0%';
        }
        
        console.log('Story restarted');
    }
    
    resetInteractiveStates() {
        // Reset factor cards
        document.querySelectorAll('.factor-card').forEach((card, index) => {
            card.classList.toggle('active', index === 0);
        });
        document.querySelectorAll('.factor-detail').forEach((detail, index) => {
            detail.classList.toggle('active', index === 0);
        });
        
        // Reset property selector
        document.querySelectorAll('.property-btn').forEach((btn, index) => {
            btn.classList.toggle('active', index === 0);
        });
        document.querySelectorAll('.property-info').forEach((info, index) => {
            info.classList.toggle('active', index === 0);
        });
        
        // Reset depth selector
        document.querySelectorAll('.depth-option').forEach((option, index) => {
            option.classList.toggle('active', index === 0);
        });
    }
    
    isElementInViewport(element) {
        const rect = element.getBoundingClientRect();
        return (
            rect.top >= 0 &&
            rect.left >= 0 &&
            rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
            rect.right <= (window.innerWidth || document.documentElement.clientWidth)
        );
    }
    
    // Cleanup method
    destroy() {
        // Remove all event listeners and maps
        Object.values(this.maps).forEach(mapObj => {
            if (mapObj.map) {
                mapObj.map.remove();
            }
        });
        
        console.log('Story map destroyed');
    }
}

// Initialize story map when DOM is loaded
document.addEventListener('DOMContentLoaded', async () => {
    console.log('DOM loaded, initializing Story Map');
    
    try {
        // Create global story map instance immediately
        window.storyMap = new StoryMap();
        console.log('Story Map initialized successfully');
        
    } catch (error) {
        console.error('Failed to initialize Story Map:', error);
        
        // Fallback: at least show the content
        try {
            const sections = document.querySelectorAll('.story-section');
            sections.forEach(section => {
                section.style.opacity = '1';
                section.style.transform = 'translateY(0)';
            });
            console.log('Fallback: Made story sections visible');
        } catch (fallbackError) {
            console.error('Even fallback failed:', fallbackError);
        }
    }
});

// Handle page visibility changes
document.addEventListener('visibilitychange', () => {
    if (document.hidden && window.storyMap) {
        // Pause any animations or updates when page is hidden
        console.log('Story map paused');
    } else if (window.storyMap) {
        // Resume when page becomes visible
        console.log('Story map resumed');
    }
});

// Export for module systems
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { StoryMap };
}