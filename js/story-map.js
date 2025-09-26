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
        this.interactiveMaps = null; // For the new interactive maps
        this.timelineItems = [];
        this.visitedSections = new Set();

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
            
            // Initialize interactive maps module
            try {
                this.interactiveMaps = new StoryInteractiveMaps();
                await this.interactiveMaps.init();
                // Small delay to ensure DOM is ready
                setTimeout(async () => {
                    await this.createInteractiveMaps();
                }, 100);
            } catch (e) {
                console.warn('Interactive maps not available:', e);
            }
            
            // Initialize data loader (async) - for old map system
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
        this.timelineItems = Array.from(document.querySelectorAll('.timeline-item'));
        console.log(`Found ${this.sections.length} story sections`);
        console.log(`Found ${this.timelineItems.length} timeline items`);

        // Setup timeline click handlers
        this.setupTimelineNavigation();
    }

    setupTimelineNavigation() {
        this.timelineItems.forEach((item, index) => {
            item.addEventListener('click', () => {
                const sectionId = item.getAttribute('data-section');
                const targetSection = document.querySelector(`.story-section[data-section="${sectionId}"]`);

                if (targetSection) {
                    // Smooth scroll to section
                    const yOffset = -80; // Account for fixed header
                    const y = targetSection.getBoundingClientRect().top + window.pageYOffset + yOffset;

                    window.scrollTo({
                        top: y,
                        behavior: 'smooth'
                    });
                }
            });
        });
    }
    
    // Create interactive maps in their containers
    async createInteractiveMaps() {
        if (!this.interactiveMaps) return;
        
        console.log('Creating interactive story maps...');
        
        // CLORPT intro section now has no map - just header text
        
        // Create main soil orders map
        const soilOrdersMap = document.getElementById('soil-orders-map');
        if (soilOrdersMap) {
            this.interactiveMaps.createMap('soil-orders-map', 'soilOrders', {
                zoom: 10.5,  // Zoomed in one more notch
                center: [42.13, -122.466]
            });
            
            // Add the same legend
            const soilOrderLegend = [
                { color: '#00A551', label: 'Mollisols (Prairie)' },
                { color: '#CB7662', label: 'Inceptisols (Developing)' },
                { color: '#EA028C', label: 'Andisols (Volcanic)' },
                { color: '#FFF100', label: 'Vertisols (Shrink-Swell)' },
                { color: '#B5D55D', label: 'Alfisols (Forest)' },
                { color: '#75CDD6', label: 'Entisols (Young)' }
            ];
            this.interactiveMaps.addLegend('soil-orders-map', soilOrderLegend);
        }
        
        // Create particle size map
        const particleMap = document.getElementById('particle-size-map');
        if (particleMap) {
            this.interactiveMaps.createMap('particle-size-map', 'particleSizes', {
                zoom: 9.5,
                center: [42.13, -122.466]
            });
            
            // Add legend for particle sizes
            const particleLegend = [
                { color: '#2C3E50', label: 'Fine' },
                { color: '#5D6D7E', label: 'Fine-loamy' },
                { color: '#7B8D9F', label: 'Loamy' },
                { color: '#95A5A6', label: 'Coarse-loamy' },
                { color: '#D4B896', label: 'Sandy' },
                { color: '#8B7355', label: 'Loamy-skeletal' },
                { color: '#CD853F', label: 'Medial (volcanic)' }
            ];
            this.interactiveMaps.addLegend('particle-size-map', particleLegend);
        }
        
        // Create parent material map for CLORPT section
        const clorptParentMap = document.getElementById('clorpt-parent-material-map');
        if (clorptParentMap) {
            console.log('Creating CLORPT parent material map...');
            this.interactiveMaps.createMap('clorpt-parent-material-map', 'parentMaterial', {
                zoom: 10.5,  // Zoomed in more to focus on parent materials
                center: [42.13, -122.466],
                zoomControl: true
            });
            
            // No legend for parent material map - descriptions in text
            console.log('CLORPT parent material map created successfully');
        }
        
        // Also create parent material map for Geological Foundation section (if it exists)
        const geoParentMap = document.getElementById('parent-material-map');
        if (geoParentMap) {
            console.log('Creating Geological Foundation parent material map...');
            // Add a small delay to ensure data is fully loaded
            setTimeout(() => {
                try {
                    this.interactiveMaps.createMap('parent-material-map', 'parentMaterial', {
                        zoom: 10.5,  // Zoomed in more to focus on parent materials
                        center: [42.13, -122.466]
                    });
                    // Use same legend as CLORPT - recreate it since it's out of scope
                    const geoParentLegend = [
                        { color: '#356eff', label: 'Water' },
                        { color: '#acb6da', label: 'Carbonate' },
                        { color: '#d6b879', label: 'Non-carbonate' },
                        { color: '#313131', label: 'Alkaline intrusive' },
                        { color: '#eda800', label: 'Silicic residual' },
                        { color: '#616161', label: 'Extrusive volcanic' },
                        { color: '#d6d6d6', label: 'Colluvial sediment' },
                        { color: '#d0ddae', label: 'Glacial till clay' },
                        { color: '#b8d279', label: 'Glacial till loam' },
                        { color: '#d5d378', label: 'Glacial till coarse' },
                        { color: '#70a663', label: 'Glacial lake sediment' },
                        { color: '#cc6a70', label: 'Glacial outwash fine' },
                        { color: '#8ab3d5', label: 'Glacial outwash sandy' },
                        { color: '#6db155', label: 'Glacial outwash coarse' },
                        { color: '#9b6d55', label: 'Hydric' },
                        { color: '#feeec9', label: 'Eolian sediment coarse' },
                        { color: '#d6b879', label: 'Eolian sediment fine' },
                        { color: '#00b7ec', label: 'Saline lake sediment' },
                        { color: '#ffda90', label: 'Alluvium fine' },
                        { color: '#f8b28c', label: 'Coastal sediment coarse' }
                    ];
                    // Legend is displayed externally, not inside the map
                    console.log('Geological Foundation parent material map created successfully');
                } catch (error) {
                    console.error('Error creating geological parent material map:', error);
                }
            }, 500); // Delay to ensure data is loaded
        }
        
        // Initialize Land Cover Map
        if (document.getElementById('organisms-landcover-map')) {
            try {
                await this.interactiveMaps.createRasterMap('organisms-landcover-map', 'landcover', {
                    title: 'Land Cover Classification',
                    zoom: 9.5,
                    center: [42.13, -122.466]
                });
                
                // No need to add legend - it's now outside the map as a vertical legend
                console.log('Land cover map created successfully');
            } catch (error) {
                console.error('Error creating land cover map:', error);
            }
        }
        
        // Initialize Elevation Map  
        if (document.getElementById('relief-elevation-map')) {
            try {
                await this.interactiveMaps.createRasterMap('relief-elevation-map', 'elevation', {
                    title: 'Elevation & Hillshade',
                    zoom: 9.5,
                    center: [42.13, -122.466]
                });
                
                // No need to add legend - it's now outside the map as a vertical legend
                console.log('Elevation map created successfully');
            } catch (error) {
                console.error('Error creating elevation map:', error);
            }
        }
        
        // Initialize Climate Maps (Precipitation and Temperature)
        if (document.getElementById('climate-precip-map')) {
            try {
                await this.interactiveMaps.createRasterMap('climate-precip-map', 'precipitation', {
                    title: '30-Year Normal Precipitation',
                    zoom: 9.5,  // Zoomed out to show regional context
                    center: [42.13, -122.466]  // Slightly shifted north to move monument down in view
                });
                
                // Legend is displayed externally, not inside the map
                console.log('Precipitation map created successfully');
            } catch (error) {
                console.error('Error creating precipitation map:', error);
            }
        }
        
        if (document.getElementById('climate-temp-map')) {
            try {
                await this.interactiveMaps.createRasterMap('climate-temp-map', 'temperature', {
                    title: '30-Year Normal Temperature',
                    zoom: 9.5,  // Zoomed out to show regional context
                    center: [42.13, -122.466]  // Slightly shifted north to move monument down in view
                });
                
                // Legend is displayed externally, not inside the map
                console.log('Temperature map created successfully');
            } catch (error) {
                console.error('Error creating temperature map:', error);
            }
        }
        
        // Initialize Soil Property Maps (Organic Carbon and pH)
        if (document.getElementById('properties-oc-map')) {
            try {
                const ocMap = await this.interactiveMaps.createRasterMap('properties-oc-map', 'oc', {
                    title: 'Organic Carbon',
                    depth: 0,
                    zoom: 9.5,
                    center: [42.13, -122.466]
                });
                // Force map to recalculate size to fix aspect ratio
                if (ocMap) {
                    setTimeout(() => {
                        ocMap.invalidateSize();
                        // Also trigger resize event to ensure proper rendering
                        window.dispatchEvent(new Event('resize'));
                    }, 200);
                }
                
                // Add organic carbon legend
                const ocLegend = `
                    <h5>Organic Carbon (g/kg) - 0-5cm depth</h5>
                    <div class="gradient-legend">
                        <div class="gradient-bar" style="background: linear-gradient(to right, #FFF8DC, #DEB887, #D2691E, #8B4513, #654321);"></div>
                        <div class="gradient-labels">
                            <span>0</span>
                            <span>20</span>
                            <span>40</span>
                            <span>60</span>
                            <span>80</span>
                        </div>
                    </div>
                `;
                this.interactiveMaps.addLegend('properties-oc-map', ocLegend);
                console.log('Organic carbon map created successfully');
            } catch (error) {
                console.error('Error creating organic carbon map:', error);
            }
        }
        
        if (document.getElementById('properties-ph-map')) {
            try {
                const phMap = await this.interactiveMaps.createRasterMap('properties-ph-map', 'ph', {
                    title: 'Soil pH',
                    depth: 0,
                    zoom: 9.5,
                    center: [42.13, -122.466]
                });
                // Force map to recalculate size to fix aspect ratio
                if (phMap) {
                    setTimeout(() => {
                        phMap.invalidateSize();
                        // Also trigger resize event to ensure proper rendering
                        window.dispatchEvent(new Event('resize'));
                    }, 200);
                }
                
                // Add pH legend
                const phLegend = `
                    <h5>Soil pH - 0-5cm depth</h5>
                    <div class="gradient-legend">
                        <div class="gradient-bar" style="background: linear-gradient(to right, #FF0000, #FF6600, #FFFF00, #00FF00, #0000FF);"></div>
                        <div class="gradient-labels">
                            <span>4.5</span>
                            <span>5.5</span>
                            <span>6.5</span>
                            <span>7.5</span>
                            <span>8.5</span>
                        </div>
                    </div>
                `;
                this.interactiveMaps.addLegend('properties-ph-map', phLegend);
                console.log('pH map created successfully');
            } catch (error) {
                console.error('Error creating pH map:', error);
            }
        }
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
                threshold: 0.2,
                rootMargin: '-25% 0px -50% 0px'  // Trigger when section is 25% from top
            }
        );
        
        this.sections.forEach(section => {
            observer.observe(section);
        });
    }
    
    setupInteractivity() {
        // Factor cards interaction - removed (no longer using preview cards)
        // this.setupFactorCards();
        
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
        const ocMap = document.getElementById('properties-oc-map');
        const phMap = document.getElementById('properties-ph-map');
        const ocLegend = document.getElementById('oc-legend');
        const phLegend = document.getElementById('ph-legend');
        
        if (ocMap && phMap) {
            if (property === 'oc') {
                ocMap.style.display = 'block';
                phMap.style.display = 'none';
                if (ocLegend) ocLegend.style.display = 'block';
                if (phLegend) phLegend.style.display = 'none';
                // Invalidate OC map size when shown
                const ocMapObj = this.interactiveMaps.maps.get('properties-oc-map');
                if (ocMapObj && ocMapObj.map) {
                    setTimeout(() => ocMapObj.map.invalidateSize(), 100);
                }
            } else if (property === 'ph') {
                ocMap.style.display = 'none';
                phMap.style.display = 'block';
                if (ocLegend) ocLegend.style.display = 'none';
                if (phLegend) phLegend.style.display = 'block';
                // Invalidate pH map size when shown
                const phMapObj = this.interactiveMaps.maps.get('properties-ph-map');
                if (phMapObj && phMapObj.map) {
                    setTimeout(() => phMapObj.map.invalidateSize(), 100);
                }
            }
        }
        
        // Update depth selector in case of depth-specific updates
        const depthSelector = document.getElementById('properties-depth');
        if (depthSelector && this.interactiveMaps) {
            const depth = parseInt(depthSelector.value) || 0;
            this.updatePropertiesMap(property, depth);
        }
    }
    
    setupDepthSelector() {
        // Handle both old depth options and new dropdown
        const depthOptions = document.querySelectorAll('.depth-option');
        const depthDropdown = document.getElementById('properties-depth');
        
        // Old style depth options (if they exist)
        depthOptions.forEach(option => {
            option.addEventListener('click', () => {
                const depth = parseInt(option.getAttribute('data-depth'));
                console.log(`Depth option clicked: ${depth}`);
                
                // Update active states
                depthOptions.forEach(o => o.classList.remove('active'));
                option.classList.add('active');
                
                // Update properties map with new depth
                const activeProperty = document.querySelector('.property-btn.active');
                if (activeProperty) {
                    const property = activeProperty.getAttribute('data-property');
                    console.log(`Updating ${property} map to depth ${depth}`);
                    this.updatePropertiesMap(property, depth);
                }
            });
        });
        
        // New dropdown depth selector
        if (depthDropdown) {
            console.log('Setting up depth dropdown listener');
            depthDropdown.addEventListener('change', (e) => {
                const depth = parseInt(e.target.value);
                console.log(`Depth dropdown changed to: ${depth}`);
                const activeProperty = document.querySelector('.property-btn.active');
                if (activeProperty) {
                    const property = activeProperty.getAttribute('data-property');
                    console.log(`Updating ${property} map to depth ${depth}`);
                    this.updatePropertiesMap(property, depth);
                } else {
                    console.warn('No active property button found');
                }
            });
        } else {
            console.warn('Depth dropdown not found');
        }
    }
    
    setupClimateToggle() {
        const climateBtns = document.querySelectorAll('.climate-btn');
        const precipMap = document.getElementById('climate-precip-map');
        const tempMap = document.getElementById('climate-temp-map');
        const precipLegend = document.getElementById('precip-legend');
        const tempLegend = document.getElementById('temp-legend');
        const climateCaption = document.getElementById('climate-caption');
        
        climateBtns.forEach(btn => {
            btn.addEventListener('click', () => {
                const climate = btn.getAttribute('data-climate');
                
                // Update active states
                climateBtns.forEach(b => b.classList.remove('active'));
                btn.classList.add('active');
                
                // Toggle maps and legends
                if (climate === 'precipitation') {
                    if (precipMap) precipMap.style.display = 'block';
                    if (tempMap) tempMap.style.display = 'none';
                    if (precipLegend) precipLegend.style.display = 'block';
                    if (tempLegend) tempLegend.style.display = 'none';
                    if (climateCaption) {
                        climateCaption.textContent = 'Annual precipitation varies from 20 inches in rain shadow valleys to over 60 inches on exposed ridges';
                    }
                } else if (climate === 'temperature') {
                    if (precipMap) precipMap.style.display = 'none';
                    if (tempMap) tempMap.style.display = 'block';
                    if (precipLegend) precipLegend.style.display = 'none';
                    if (tempLegend) tempLegend.style.display = 'block';
                    if (climateCaption) {
                        climateCaption.textContent = 'Mean annual temperature ranges from 35°F at high elevations to 55°F in low valleys';
                    }
                }
            });
        });
    }
    
    updatePropertiesMap(property, depth) {
        // Update the raster map with new depth
        if (!this.interactiveMaps) return;
        
        const mapId = property === 'oc' ? 'properties-oc-map' : 'properties-ph-map';
        const mapContainer = document.getElementById(mapId);
        
        if (mapContainer && mapContainer.style.display !== 'none') {
            // Update the raster layer with new depth
            console.log(`Updating ${property} map to depth index ${depth}`);
            this.interactiveMaps.changeDepth(mapId, depth).then(() => {
                // Force map to recalculate size after depth change
                const mapObj = this.interactiveMaps.maps.get(mapId);
                if (mapObj && mapObj.map) {
                    setTimeout(() => mapObj.map.invalidateSize(), 150);
                }
            });
            
            // Update legend with new depth
            const depthLabels = ['0-5cm', '5-15cm', '15-30cm', '30-60cm', '60-100cm', '100-200cm'];
            
            // Update the legend on the map
            let newLegend;
            if (property === 'oc') {
                newLegend = `
                    <h5>Organic Carbon (g/kg) - ${depthLabels[depth]} depth</h5>
                    <div class="gradient-legend">
                        <div class="gradient-bar" style="background: linear-gradient(to right, #FFF8DC, #DEB887, #D2691E, #8B4513, #654321);"></div>
                        <div class="gradient-labels">
                            <span>0</span>
                            <span>20</span>
                            <span>40</span>
                            <span>60</span>
                            <span>80</span>
                        </div>
                    </div>
                `;
            } else {
                newLegend = `
                    <h5>Soil pH - ${depthLabels[depth]} depth</h5>
                    <div class="gradient-legend">
                        <div class="gradient-bar" style="background: linear-gradient(to right, #FF0000, #FF6600, #FFFF00, #00FF00, #0000FF);"></div>
                        <div class="gradient-labels">
                            <span>4.5</span>
                            <span>5.5</span>
                            <span>6.5</span>
                            <span>7.5</span>
                            <span>8.5</span>
                        </div>
                    </div>
                `;
            }
            this.interactiveMaps.addLegend(mapId, newLegend);
            
            // Also update the HTML legend if it exists
            const legendId = property === 'oc' ? 'oc-legend' : 'ph-legend';
            const legend = document.getElementById(legendId);
            if (legend) {
                const title = legend.querySelector('h5');
                if (title) {
                    if (property === 'oc') {
                        title.textContent = `Organic Carbon (g/kg) - ${depthLabels[depth]} depth`;
                    } else {
                        title.textContent = `Soil pH - ${depthLabels[depth]} depth`;
                    }
                }
            }
        }
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
        const viewportTop = window.innerHeight * 0.3; // Check point is 30% from top of viewport

        let activeSection = -1;

        this.sections.forEach((section, index) => {
            const rect = section.getBoundingClientRect();

            // Section is considered active when its top crosses the check point
            // or when it's the last section that has started
            if (rect.top <= viewportTop && rect.bottom > viewportTop) {
                activeSection = index;
            }

            // Mark sections as visited if they've been scrolled past
            if (rect.top < viewportTop) {
                this.visitedSections.add(index);
            }
        });

        // If no section is exactly at the checkpoint, use the last visited section
        if (activeSection === -1 && this.visitedSections.size > 0) {
            activeSection = Math.max(...this.visitedSections);
        }

        // Update if the active section has changed
        if (activeSection !== -1 && this.currentSection !== activeSection) {
            this.currentSection = activeSection;
            this.onSectionChange(activeSection);
            this.updateTimelineIndicator(activeSection);
        }

        // Update visited state on timeline
        this.updateTimelineVisitedStates();
    }

    updateTimelineIndicator(activeIndex) {
        // Remove active class from all timeline items
        this.timelineItems.forEach(item => {
            item.classList.remove('active');
        });

        // Add active class to current timeline item
        if (this.timelineItems[activeIndex]) {
            this.timelineItems[activeIndex].classList.add('active');
        }
    }

    updateTimelineVisitedStates() {
        // No longer adding visited class since we don't want to highlight previously visited sections
        // Still tracking visited sections internally for other logic
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
            case 'clorpt-intro':
                // Introduction to CLORPT factors
                this.updateFactorsMap('climate');
                break;
            case 'clorpt-climate':
                // Climate factor section
                this.updateFactorsMap('climate');
                break;
            case 'clorpt-organisms':
                // Organisms factor section
                this.updateFactorsMap('organisms');
                break;
            case 'clorpt-relief':
                // Relief/topography factor section
                this.updateFactorsMap('relief');
                break;
            case 'clorpt-parent':
                // Parent material factor section
                this.updateFactorsMap('parent');
                break;
            case 'clorpt-time':
                // Time factor section
                this.updateFactorsMap('time');
                break;
            case 'soil-history':
                // Soil formation history section
                this.updateFactorsMap('time');
                break;
            case 'soil-orders':
                // Soil orders classification section
                this.addBaseLayer('conservation-map', 'terrain');
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

        // Initialize first timeline item as active
        if (this.timelineItems[0]) {
            this.timelineItems[0].classList.add('active');
            // Still track visited internally but don't add class
            this.visitedSections.add(0);
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

        // Reset timeline
        this.visitedSections.clear();
        this.visitedSections.add(0);
        this.currentSection = 0;
        this.timelineItems.forEach((item, index) => {
            item.classList.remove('active', 'visited');
            if (index === 0) {
                item.classList.add('active');
            }
        });

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