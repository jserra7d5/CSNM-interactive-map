// Main Application Entry Point
// Cascade-Siskiyou National Monument Soil Explorer

class SoilExplorerApp {
    constructor() {
        this.mapManager = null;
        this.uiController = null;
        this.dataLoader = dataLoader; // Use singleton instance
        this.appData = null;
        this.initialized = false;
    }
    
    // Initialize the application
    async init() {
        try {
            console.log('Initializing Cascade-Siskiyou Soil Explorer...');
            
            // Validate configuration
            if (!ConfigUtils.validate()) {
                throw new Error('Invalid application configuration');
            }
            
            // Initialize UI controller
            this.uiController = new UIController();
            
            // Initialize map manager
            this.mapManager = new MapManager('map');
            this.mapManager.initializeMap();
            
            // Setup application event listeners
            this.setupEventListeners();
            
            // Load application data
            await this.loadData();
            
            // Initialize map layers
            await this.initializeMapLayers();
            
            // Mark as initialized
            this.initialized = true;
            
            console.log('Application initialized successfully');
            
        } catch (error) {
            console.error('Failed to initialize application:', error);
            this.handleInitializationError(error);
        }
    }
    
    // Load all required data
    async loadData() {
        try {
            this.uiController.showLoading('Loading soil data...');
            
            console.log('Loading application data...');
            this.appData = await this.dataLoader.loadAllData();
            
            console.log('Data loaded successfully:', {
                soilPolygons: !!this.appData.soilPolygons,
                boundaryPolygon: !!this.appData.boundaryPolygon,
                mapunitTable: !!this.appData.mapunitTable
            });
            
            // Don't hide loading yet - keep it for polygon loading
            this.uiController.updateUIForDataState(true);
            
        } catch (error) {
            console.error('Failed to load data:', error);
            this.uiController.hideLoading();
            this.uiController.updateUIForDataState(false);
            throw error;
        }
    }
    
    // Initialize map layers with loaded data
    async initializeMapLayers() {
        if (!this.appData) {
            console.warn('No data available for map layers');
            this.uiController.hideLoading();
            return;
        }
        
        try {
            // Load soil polygons
            if (this.appData.soilPolygons) {
                this.uiController.updateLoadingMessage('Loading soil polygons...');
                await this.mapManager.loadSoilPolygons(this.appData);
                console.log('Soil polygons loaded to map');
            }
            
            // Load boundary polygon
            if (this.appData.boundaryPolygon) {
                this.uiController.updateLoadingMessage('Loading boundaries...');
                await this.mapManager.loadBoundaryPolygon(this.appData);
                console.log('Boundary polygon loaded');
            }
            
            // Load highways
            if (this.appData.highways) {
                this.uiController.updateLoadingMessage('Loading highways...');
                await this.mapManager.loadHighways(this.appData);
                console.log('Highways loaded');
            }
            
            // Load service roads
            if (this.appData.serviceRoads) {
                this.uiController.updateLoadingMessage('Loading service roads...');
                await this.mapManager.loadServiceRoads(this.appData);
                console.log('Service roads loaded');
            }
            
            // Update layers for initial map type (soil orders)
            this.uiController.updateLoadingMessage('Setting up soil orders map...');
            this.mapManager.updateLayers('soil');
            
            // Hide loading screen once soil orders map is ready
            this.uiController.hideLoading();
            
        } catch (error) {
            console.error('Failed to initialize map layers:', error);
            this.uiController.hideLoading();
        }
    }
    
    // Setup application-level event listeners
    setupEventListeners() {
        // Map ready event
        document.addEventListener('mapReady', () => {
            console.log('Map is ready');
        });
        
        // Sidebar toggle event
        document.addEventListener('sidebarToggled', (e) => {
            // Invalidate map size when sidebar is toggled
            setTimeout(() => {
                if (this.mapManager && this.mapManager.getMap()) {
                    this.mapManager.getMap().invalidateSize();
                }
            }, CONFIG.ui.animationDuration);
        });
        
        // Map type change event
        document.addEventListener('mapTypeChanged', (e) => {
            this.handleMapTypeChange(e.detail);
        });
        
        // Depth change event
        document.addEventListener('depthChanged', (e) => {
            this.handleDepthChange(e.detail);
        });
        
        // Boundaries toggle event
        document.addEventListener('boundariesToggled', (e) => {
            this.handleBoundariesToggle(e.detail);
        });
        
        // Highways toggle event
        document.addEventListener('highwaysToggled', (e) => {
            this.handleHighwaysToggle(e.detail);
        });
        
        // Service roads toggle event
        document.addEventListener('serviceRoadsToggled', (e) => {
            this.handleServiceRoadsToggle(e.detail);
        });
        
        // Information center toggle event
        document.addEventListener('informationCenterToggled', (e) => {
            this.handleInformationCenterToggle(e.detail);
        });
        
        // Map click event
        document.addEventListener('mapClick', (e) => {
            this.handleMapClick(e.detail);
        });
        
        // Feature selection event
        document.addEventListener('featureSelected', (e) => {
            this.handleFeatureSelection(e.detail);
        });
        
        // Window beforeunload
        window.addEventListener('beforeunload', () => {
            this.cleanup();
        });
    }
    
    // Handle map type changes
    async handleMapTypeChange(detail) {
        const { mapType, depth } = detail;
        
        console.log(`Map type changed to: ${mapType}`);
        
        // Update base layer based on map type
        if (mapType === 'satellite') {
            this.mapManager.setBaseLayer('satellite');
        } else if (mapType === 'elevation') {
            this.mapManager.setBaseLayer('topo');
        } else {
            this.mapManager.setBaseLayer('terrain');
        }
        
        // Update layers and legend visibility
        const currentDepth = this.uiController.getCurrentState().currentDepth;
        await this.mapManager.updateLayers(mapType, currentDepth);
        
        // Raster layers are now handled in updateLayers method
    }
    
    // Handle depth changes
    handleDepthChange(detail) {
        const { mapType, depth } = detail;
        
        console.log(`Depth changed to: ${depth} for ${mapType}`);
        
        // Update raster layer for depth changes
        this.mapManager.updateLayers(mapType, depth);
    }
    
    // Handle boundaries toggle
    handleBoundariesToggle(detail) {
        const { show } = detail;
        
        console.log(`Boundaries visibility: ${show}`);
        this.mapManager.toggleBoundaries(show);
    }
    
    // Handle highways toggle
    handleHighwaysToggle(detail) {
        const { show } = detail;
        
        console.log(`Highways visibility: ${show}`);
        this.mapManager.toggleHighways(show);
    }
    
    // Handle service roads toggle
    handleServiceRoadsToggle(detail) {
        const { show } = detail;
        
        console.log(`Service roads visibility: ${show}`);
        this.mapManager.toggleServiceRoads(show);
    }
    
    // Handle information center toggle
    handleInformationCenterToggle(detail) {
        const { show } = detail;
        
        console.log(`Information center visibility: ${show}`);
        this.mapManager.toggleInformationCenter(show);
    }
    
    // Handle map clicks
    handleMapClick(detail) {
        const { lat, lng } = detail;
        
        console.log(`Map clicked at: ${lat}, ${lng}`);
        
        // Extract soil profile data (placeholder)
        this.extractSoilProfile(lat, lng);
    }
    
    // Handle feature selection
    handleFeatureSelection(detail) {
        const { feature, latlng } = detail;
        
        console.log('Feature selected:', feature.properties);
        
        // Get detailed soil information
        const soilDetails = this.getSoilDetails(feature.properties);
        
        // Extract soil profile for the location
        this.extractSoilProfile(latlng.lat, latlng.lng, feature.properties);
    }
    
    // Extract soil profile data
    async extractSoilProfile(lat, lng, featureProps = null) {
        try {
            // For now, generate mock data since raster processing isn't implemented
            const currentMapType = this.uiController.getCurrentState().currentMapType;
            const property = (currentMapType === 'oc' || currentMapType === 'ph') ? currentMapType : 'oc';
            
            const profileData = await this.dataLoader.extractSoilProfile(lat, lng, property);
            
            console.log('Soil profile extracted:', profileData);
            
            // Update UI with profile data
            this.updateSoilProfileDisplay(profileData, featureProps);
            
        } catch (error) {
            console.error('Failed to extract soil profile:', error);
        }
    }
    
    // Get detailed soil information
    getSoilDetails(properties) {
        if (!this.appData || !this.appData.mapunitLookup) {
            return properties;
        }
        
        const musym = properties.musym;
        const details = this.dataLoader.getSoilDetails(musym, this.appData.mapunitLookup);
        
        return { ...properties, ...details };
    }
    
    // Update soil profile display
    updateSoilProfileDisplay(profileData, featureProps) {
        // This would be enhanced with actual chart rendering
        console.log('Updating soil profile display with:', profileData);
        
        // For now, just log the data
        // TODO: Implement Plotly chart rendering
    }
    
    // Handle initialization errors
    handleInitializationError(error) {
        const errorMessage = `
            <div style="padding: 20px; text-align: center; color: #d32f2f;">
                <h3>Application Error</h3>
                <p>Failed to initialize the Soil Explorer application.</p>
                <p><strong>Error:</strong> ${error.message}</p>
                <p>Please check the console for more details and try refreshing the page.</p>
            </div>
        `;
        
        document.getElementById('map').innerHTML = errorMessage;
    }
    
    // Get application state
    getState() {
        return {
            initialized: this.initialized,
            dataLoaded: !!this.appData,
            ui: this.uiController ? this.uiController.getCurrentState() : null,
            cache: this.dataLoader ? this.dataLoader.getCacheStats() : null
        };
    }
    
    // Cleanup resources
    cleanup() {
        console.log('Cleaning up application resources...');
        
        if (this.mapManager) {
            this.mapManager.destroy();
        }
        
        if (this.dataLoader) {
            this.dataLoader.clearCache();
        }
    }
}

// Application initialization
document.addEventListener('DOMContentLoaded', async () => {
    // Create global app instance
    window.soilExplorerApp = new SoilExplorerApp();
    
    // Initialize the application
    try {
        await window.soilExplorerApp.init();
    } catch (error) {
        console.error('Application failed to start:', error);
    }
});

// Export for module systems
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { SoilExplorerApp };
}