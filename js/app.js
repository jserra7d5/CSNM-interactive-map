// Main Application Entry Point
// Soils of the Siskiyous

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
            
            
        } catch (error) {
            this.handleInitializationError(error);
        }
    }
    
    // Load all required data
    async loadData() {
        try {
            this.uiController.showLoading('Loading soil data...');
            
            this.appData = await this.dataLoader.loadAllData();

            // Expose app data globally for UI components to access (e.g., soil suitability)
            window.appData = this.appData;

            // Don't hide loading yet - keep it for polygon loading
            this.uiController.updateUIForDataState(true);
            
        } catch (error) {
            this.uiController.hideLoading();
            this.uiController.updateUIForDataState(false);
            throw error;
        }
    }
    
    // Initialize map layers with loaded data
    async initializeMapLayers() {
        if (!this.appData) {
            this.uiController.hideLoading();
            return;
        }
        
        try {
            // Load soil polygons
            if (this.appData.soilPolygons) {
                this.uiController.updateLoadingMessage('Loading soil polygons...');
                await this.mapManager.loadSoilPolygons(this.appData);
            }
            
            // Load boundary polygon
            if (this.appData.boundaryPolygon) {
                this.uiController.updateLoadingMessage('Loading boundaries...');
                await this.mapManager.loadBoundaryPolygon(this.appData);
            }
            
            // Load highways
            if (this.appData.highways) {
                this.uiController.updateLoadingMessage('Loading highways...');
                await this.mapManager.loadHighways(this.appData);
            }
            
            // Load service roads
            if (this.appData.serviceRoads) {
                this.uiController.updateLoadingMessage('Loading service roads...');
                await this.mapManager.loadServiceRoads(this.appData);
            }
            
            // Initialize with satellite view as default
            this.uiController.updateLoadingMessage('Loading satellite view...');
            this.mapManager.setBaseLayer('satellite');
            await this.mapManager.updateLayers('satellite');
            
            // Hide loading screen
            this.uiController.hideLoading();
            
        } catch (error) {
            this.uiController.hideLoading();
        }
    }
    
    // Setup application-level event listeners
    setupEventListeners() {
        // Map ready event
        document.addEventListener('mapReady', () => {
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
        
        console.log('📍 APP: handleMapTypeChange called with:', mapType, 'depth:', depth);
        
        // If switching away from SoilWeb view, hide the click marker and detail panel
        if (mapType !== 'ssurgo') {
            
            // Remove click marker
            if (this.mapManager) {
                this.mapManager.removeClickMarker();
            }
            
            // Close SSURGO detail panel
            if (this.uiController) {
                this.uiController.closeSsurgoDetailPanel();
            }
        }
        
        // Update base layer based on map type
        if (mapType === 'satellite') {
            this.mapManager.setBaseLayer('satellite');
        } else if (mapType === 'elevation') {
            this.mapManager.setBaseLayer('topo');
        } else if (mapType.startsWith('temperature') || mapType.startsWith('vpd') || mapType.startsWith('solar') || mapType === 'precipitation') {
            // Climate variables look best on terrain base
            this.mapManager.setBaseLayer('terrain');
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
        
        
        // Update raster layer for depth changes
        this.mapManager.updateLayers(mapType, depth);
    }
    
    // Handle boundaries toggle
    handleBoundariesToggle(detail) {
        const { show } = detail;
        
        this.mapManager.toggleBoundaries(show);
    }
    
    // Handle highways toggle
    handleHighwaysToggle(detail) {
        const { show } = detail;
        
        this.mapManager.toggleHighways(show);
    }
    
    // Handle service roads toggle
    handleServiceRoadsToggle(detail) {
        const { show } = detail;
        
        this.mapManager.toggleServiceRoads(show);
    }
    
    // Handle information center toggle
    handleInformationCenterToggle(detail) {
        const { show } = detail;
        
        this.mapManager.toggleInformationCenter(show);
    }
    
    // Handle map clicks
    handleMapClick(detail) {
        const { lat, lng } = detail;
        
        
        // Extract soil profile data (placeholder)
        this.extractSoilProfile(lat, lng);
    }
    
    // Handle feature selection
    handleFeatureSelection(detail) {
        const { feature, latlng } = detail;
        
        // Validate detail has required data
        if (!feature || !latlng) {
            console.warn('Feature selection missing required data');
            return;
        }
        
        // Check current map type
        const currentMapType = this.uiController.getCurrentState().currentMapType;
        
        if (currentMapType === 'ssurgo') {
            // In SSURGO mode, add click marker and show the detailed SSURGO panel
            this.mapManager.addClickMarker(latlng);
            this.showSsurgoDetailPanel(feature, latlng);
        } else {
            // In other modes, show soil details and profile
            const soilDetails = this.getSoilDetails(feature.properties);
            
            // Extract soil profile for the location
            this.extractSoilProfile(latlng.lat, latlng.lng, feature.properties);
        }
    }
    
    // Show SSURGO info panel with map unit data
    showSsurgoInfo(feature) {
        // Get all components for this map unit
        const mukey = feature.properties.MUKEY || feature.properties.mukey;
        const components = this.getMapUnitComponents(mukey);
        
        // Prepare data for the panel
        const ssurgoData = {
            mapunit: {
                mukey: mukey,
                musym: feature.properties.MUSYM || feature.properties.musym,
                muname: feature.properties.muname || 'N/A',
                muacres: feature.properties.muacres || 0
            },
            components: components,
            areasymbol: feature.properties.AREASYMBOL || 'N/A',
            spatialver: feature.properties.SPATIALVER || 'N/A'
        };
        
        // Open the SSURGO panel
        this.uiController.openSsurgoPanel(ssurgoData);
    }
    
    // Show SSURGO detail panel (SoilWeb style)
    showSsurgoDetailPanel(feature, latlng) {
        // Validate feature exists and has properties
        if (!feature || !feature.properties) {
            console.warn('No feature or properties available for SSURGO detail panel');
            return;
        }
        
        // Get all components for this map unit
        const mukey = feature.properties.MUKEY || feature.properties.mukey;
        const components = this.getMapUnitComponents(mukey);
        
        // Prepare enhanced data with placeholder for missing fields
        const detailData = {
            mapUnitName: feature.properties.muname || `${feature.properties.MUSYM || feature.properties.musym || 'placeholder'} - ${feature.properties.compname || 'placeholder'}`,
            mapUnitSymbol: feature.properties.MUSYM || feature.properties.musym || 'placeholder',
            components: components.map(comp => {
                return {
                    ...comp,
                    geomorphicPosition: comp.properties?.geomdesc || comp.geomdesc || 'placeholder',
                    horizonData: comp.comppct_r > 0 ? 'Available' : 'placeholder'
                };
            }),
            mapunitData: {
                mukey: mukey || 'placeholder',
                musym: feature.properties.MUSYM || feature.properties.musym || 'placeholder',
                nationalSymbol: 'placeholder',
                orderOfMapping: 'placeholder',
                mapUnitType: 'placeholder',
                farmlandClass: 'placeholder',
                waterStorage: 'placeholder',
                floodFrequency: 'placeholder',
                floodFrequencyMax: 'placeholder',
                pondingFrequency: 'placeholder',
                drainageClass: this.getDominantDrainageClass(components) || 'placeholder',
                drainageClassWet: this.getWettestDrainageClass(components) || 'placeholder',
                hydricSoilsProportion: this.getHydricSoilsProportion(components) || 'placeholder',
                waterTableDepthAnnual: 'placeholder',
                waterTableDepthGrowing: 'placeholder',
                bedrockDepth: 'placeholder',
                hydgrp: this.getDominantHydgrp(components) || 'placeholder'
            },
            surveyMetadata: {
                areaSymbol: 'placeholder',
                scale: 'placeholder',
                published: 'placeholder',
                lastExport: 'placeholder'
            }
        };
        
        // Pass to UI controller to display, including click location
        this.uiController.openSsurgoDetailPanel(detailData, latlng);
    }
    
    
    // Get all components for a map unit
    getMapUnitComponents(mukey) {
        if (!this.appData || !this.appData.soilPolygons) return [];
        
        const components = [];
        const seen = new Set();
        
        // Search through all features to find components with this mukey
        this.appData.soilPolygons.features.forEach(feature => {
            const props = feature.properties;
            if (props.mukey === mukey || props.MUKEY === mukey) {
                const compKey = `${props.compname}_${props.comppct_r}`;
                if (!seen.has(compKey)) {
                    seen.add(compKey);
                    components.push({
                        compname: props.compname || 'placeholder',
                        comppct_r: props.comppct_r || 'placeholder',
                        compkind: props.compkind || 'placeholder',
                        majcompflag: props.majcompflag || 'placeholder',
                        // Add MUKEY for soil suitability lookup
                        MUKEY: props.MUKEY || props.mukey,
                        cokey: props.cokey,
                        // Add taxonomy fields
                        taxclname: props.taxclname,
                        taxorder: props.taxorder,
                        taxsuborder: props.taxsuborder,
                        taxgrtgroup: props.taxgrtgroup,
                        taxsubgrp: props.taxsubgrp,
                        taxpartsize: props.taxpartsize,
                        taxpartsizemod: props.taxpartsizemod,
                        taxceactcl: props.taxceactcl,
                        taxreaction: props.taxreaction,
                        taxtempcl: props.taxtempcl,
                        taxmoistscl: props.taxmoistscl,
                        taxtempregime: props.taxtempregime,
                        soiltaxedition: props.soiltaxedition,
                        // Add land classification fields
                        nirrcapcl: props.nirrcapcl,
                        nirrcapscl: props.nirrcapscl,
                        irrcapcl: props.irrcapcl,
                        irrcapscl: props.irrcapscl,
                        castorieindex: props.castorieindex,
                        foragesuitgrpid: props.foragesuitgrpid,
                        // Add map unit data fields
                        drainagecl: props.drainagecl,
                        hydricrating: props.hydricrating,
                        hydgrp: props.hydgrp,
                        // Add hydraulic and erosion fields
                        wei: props.wei,
                        weg: props.weg,
                        tfact: props.tfact,
                        runoff: props.runoff,
                        // Store all properties for complete access
                        properties: props
                    });
                }
            }
        });
        
        // Sort by percentage (highest first)
        components.sort((a, b) => (b.comppct_r || 0) - (a.comppct_r || 0));
        
        return components;
    }
    
    // Get dominant drainage class from components
    getDominantDrainageClass(components) {
        if (!components || components.length === 0) return null;
        
        // Find the component with highest percentage that has drainage class
        for (const comp of components) {
            if (comp.drainagecl) {
                return comp.drainagecl;
            }
        }
        return null;
    }
    
    // Get wettest drainage class from components
    getWettestDrainageClass(components) {
        if (!components || components.length === 0) return null;
        
        // Drainage classes ordered from wettest to driest
        const drainageOrder = [
            'Very poorly drained',
            'Poorly drained',
            'Somewhat poorly drained',
            'Moderately well drained',
            'Well drained',
            'Somewhat excessively drained',
            'Excessively drained'
        ];
        
        let wettestIndex = -1;
        let wettestClass = null;
        
        for (const comp of components) {
            if (comp.drainagecl) {
                const index = drainageOrder.indexOf(comp.drainagecl);
                if (index !== -1 && (wettestIndex === -1 || index < wettestIndex)) {
                    wettestIndex = index;
                    wettestClass = comp.drainagecl;
                }
            }
        }
        
        return wettestClass;
    }
    
    // Get proportion of hydric soils
    getHydricSoilsProportion(components) {
        if (!components || components.length === 0) return null;
        
        let hydricPercentage = 0;
        let totalPercentage = 0;
        
        for (const comp of components) {
            const pct = typeof comp.comppct_r === 'number' ? comp.comppct_r : 0;
            totalPercentage += pct;
            
            if (comp.hydricrating === 'Yes') {
                hydricPercentage += pct;
            }
        }
        
        if (totalPercentage === 0) return '0%';
        
        return Math.round((hydricPercentage / totalPercentage) * 100) + '%';
    }
    
    // Get dominant hydrologic group
    getDominantHydgrp(components) {
        if (!components || components.length === 0) return null;
        
        // Find the component with highest percentage that has hydgrp
        for (const comp of components) {
            if (comp.hydgrp) {
                return comp.hydgrp;
            }
        }
        return null;
    }
    
    // Extract soil profile data
    async extractSoilProfile(lat, lng, featureProps = null) {
        try {
            // For now, generate mock data since raster processing isn't implemented
            const currentMapType = this.uiController.getCurrentState().currentMapType;
            const property = (currentMapType === 'oc' || currentMapType === 'ph') ? currentMapType : 'oc';
            
            const profileData = await this.dataLoader.extractSoilProfile(lat, lng, property);
            
            
            // Update UI with profile data
            this.updateSoilProfileDisplay(profileData, featureProps);
            
        } catch (error) {
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
    // Set app version in UI
    const versionElement = document.getElementById('app-version');
    if (versionElement && typeof APP_VERSION !== 'undefined') {
        versionElement.textContent = APP_VERSION;
    }
    
    // Function to initialize app when rasterManager is ready
    async function initializeApp() {
        if (window.rasterManager) {
            // Create global app instance
            window.soilExplorerApp = new SoilExplorerApp();
            
            // Initialize the application
            try {
                await window.soilExplorerApp.init();
            } catch (error) {
            }
        } else {
            setTimeout(initializeApp, 100);
        }
    }
    
    // Start initialization
    initializeApp();
});

// Export for module systems
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { SoilExplorerApp };
}