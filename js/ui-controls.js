// UI Controls and Interactions
// Handles user interface elements and their interactions

class UIController {
    constructor() {
        this.sidebarCollapsed = false;
        this.currentView = 'main'; // 'main' or 'profile'
        this.currentMapType = 'soil';
        this.currentDepth = 0;
        this.showBoundaries = false;
        this.showHighways = false;
        this.showServiceRoads = false;
        this.showInformationCenter = false;
        
        this.elements = {};
        this.initializeElements();
        this.setupEventListeners();
    }
    
    // Initialize DOM element references
    initializeElements() {
        this.elements = {
            sidebar: document.getElementById('sidebar'),
            sidebarToggle: document.getElementById('sidebar-toggle'),
            mapTypeRadios: document.querySelectorAll('input[name="map_type"]'),
            depthControls: document.getElementById('depth-controls'),
            depthLabel: document.getElementById('depth-label'),
            depthSelector: document.getElementById('depth-selector'),
            boundariesCheckbox: document.getElementById('show-boundaries'),
            highwaysCheckbox: document.getElementById('show-highways'),
            serviceRoadsCheckbox: document.getElementById('show-service-roads'),
            informationCenterCheckbox: document.getElementById('show-information-center'),
            selectionInfo: document.getElementById('selection-info'),
            selectionContent: document.getElementById('selection-content'),
            profileView: document.getElementById('profile-view'),
            backToMainBtn: document.getElementById('back-to-main'),
            infoBtn: document.getElementById('info-btn'),
            infoModal: document.getElementById('info-modal'),
            closeModalBtn: document.getElementById('close-modal'),
            loading: document.getElementById('loading'),
            soilLegend: document.getElementById('soil-legend'),
            boundariesColorPreview: document.getElementById('boundaries-color'),
            highwaysColorPreview: document.getElementById('highways-color'),
            serviceRoadsColorPreview: document.getElementById('service-roads-color'),
            informationCenterColorPreview: document.getElementById('information-center-color')
        };
    }
    
    // Setup event listeners for UI controls
    setupEventListeners() {
        // Sidebar toggle
        if (this.elements.sidebarToggle) {
            this.elements.sidebarToggle.addEventListener('click', () => {
                this.toggleSidebar();
            });
        }
        
        // Map type radio buttons
        this.elements.mapTypeRadios.forEach(radio => {
            radio.addEventListener('change', (e) => {
                if (e.target.checked) {
                    this.handleMapTypeChange(e.target.value);
                }
            });
        });
        
        // Depth selector
        if (this.elements.depthSelector) {
            this.elements.depthSelector.addEventListener('change', (e) => {
                this.handleDepthChange(parseInt(e.target.value));
            });
        }
        
        // Boundaries checkbox
        if (this.elements.boundariesCheckbox) {
            this.elements.boundariesCheckbox.addEventListener('change', (e) => {
                this.handleBoundariesToggle(e.target.checked);
            });
        }
        
        // Highways checkbox
        if (this.elements.highwaysCheckbox) {
            this.elements.highwaysCheckbox.addEventListener('change', (e) => {
                this.handleHighwaysToggle(e.target.checked);
            });
        }
        
        // Service roads checkbox
        if (this.elements.serviceRoadsCheckbox) {
            this.elements.serviceRoadsCheckbox.addEventListener('change', (e) => {
                this.handleServiceRoadsToggle(e.target.checked);
            });
        }
        
        // Information center checkbox
        if (this.elements.informationCenterCheckbox) {
            this.elements.informationCenterCheckbox.addEventListener('change', (e) => {
                this.handleInformationCenterToggle(e.target.checked);
            });
        }
        
        // Back to main button
        if (this.elements.backToMainBtn) {
            this.elements.backToMainBtn.addEventListener('click', () => {
                this.showMainView();
            });
        }
        
        // Info modal controls
        if (this.elements.infoBtn) {
            this.elements.infoBtn.addEventListener('click', () => {
                this.showInfoModal();
            });
        }
        
        if (this.elements.closeModalBtn) {
            this.elements.closeModalBtn.addEventListener('click', () => {
                this.hideInfoModal();
            });
        }
        
        // Modal backdrop click to close
        if (this.elements.infoModal) {
            this.elements.infoModal.addEventListener('click', (e) => {
                if (e.target === this.elements.infoModal) {
                    this.hideInfoModal();
                }
            });
        }
        
        // Keyboard shortcuts
        document.addEventListener('keydown', (e) => {
            this.handleKeyboardShortcuts(e);
        });
        
        // Window resize
        window.addEventListener('resize', () => {
            this.handleWindowResize();
        });
        
        // Custom events
        document.addEventListener('mapClick', (e) => {
            this.handleMapClick(e.detail);
        });
        
        document.addEventListener('featureSelected', (e) => {
            this.handleFeatureSelected(e.detail);
        });
    }
    
    // Toggle sidebar visibility
    toggleSidebar() {
        this.sidebarCollapsed = !this.sidebarCollapsed;
        
        if (this.elements.sidebar) {
            if (this.sidebarCollapsed) {
                this.elements.sidebar.classList.add('collapsed');
            } else {
                this.elements.sidebar.classList.remove('collapsed');
            }
        }
        
        // Emit event for map resize
        setTimeout(() => {
            const event = new CustomEvent('sidebarToggled', {
                detail: { collapsed: this.sidebarCollapsed }
            });
            document.dispatchEvent(event);
        }, CONFIG.ui.animationDuration);
    }
    
    // Handle map type changes
    handleMapTypeChange(mapType) {
        this.currentMapType = mapType;
        
        // Show/hide depth controls based on map type
        const showDepthControls = mapType === 'oc' || mapType === 'ph';
        
        if (this.elements.depthControls) {
            this.elements.depthControls.style.display = showDepthControls ? 'block' : 'none';
        }
        
        // Update depth label
        if (this.elements.depthLabel && showDepthControls) {
            const label = mapType === 'oc' ? 'Organic Carbon Depth:' : 'pH Depth:';
            this.elements.depthLabel.textContent = label;
        }
        
        // Emit map type change event
        const event = new CustomEvent('mapTypeChanged', {
            detail: { 
                mapType: mapType,
                depth: this.currentDepth
            }
        });
        document.dispatchEvent(event);
    }
    
    // Handle depth selector changes
    handleDepthChange(depth) {
        this.currentDepth = depth;
        
        // Emit depth change event
        const event = new CustomEvent('depthChanged', {
            detail: { 
                mapType: this.currentMapType,
                depth: depth
            }
        });
        document.dispatchEvent(event);
    }
    
    // Handle boundaries toggle
    handleBoundariesToggle(show) {
        this.showBoundaries = show;
        
        console.log(`Map unit boundaries ${show ? 'enabled' : 'disabled'}`);
        
        // Show/hide color preview
        if (this.elements.boundariesColorPreview) {
            this.elements.boundariesColorPreview.style.display = show ? 'flex' : 'none';
        }
        
        // Emit boundaries toggle event
        const event = new CustomEvent('boundariesToggled', {
            detail: { show: show }
        });
        document.dispatchEvent(event);
    }
    
    // Handle highways toggle
    handleHighwaysToggle(show) {
        this.showHighways = show;
        
        console.log(`Highways ${show ? 'enabled' : 'disabled'}`);
        
        // Show/hide color preview
        if (this.elements.highwaysColorPreview) {
            this.elements.highwaysColorPreview.style.display = show ? 'flex' : 'none';
        }
        
        // Emit highways toggle event
        const event = new CustomEvent('highwaysToggled', {
            detail: { show: show }
        });
        document.dispatchEvent(event);
    }
    
    // Handle service roads toggle
    handleServiceRoadsToggle(show) {
        this.showServiceRoads = show;
        
        console.log(`Service roads ${show ? 'enabled' : 'disabled'}`);
        
        // Show/hide color preview
        if (this.elements.serviceRoadsColorPreview) {
            this.elements.serviceRoadsColorPreview.style.display = show ? 'flex' : 'none';
        }
        
        // Emit service roads toggle event
        const event = new CustomEvent('serviceRoadsToggled', {
            detail: { show: show }
        });
        document.dispatchEvent(event);
    }
    
    // Handle information center toggle
    handleInformationCenterToggle(show) {
        this.showInformationCenter = show;
        
        console.log(`Information center ${show ? 'enabled' : 'disabled'}`);
        
        // Show/hide color preview
        if (this.elements.informationCenterColorPreview) {
            this.elements.informationCenterColorPreview.style.display = show ? 'flex' : 'none';
        }
        
        // Emit information center toggle event
        const event = new CustomEvent('informationCenterToggled', {
            detail: { show: show }
        });
        document.dispatchEvent(event);
    }
    
    // Handle map clicks
    handleMapClick(detail) {
        const { lat, lng } = detail;
        
        // Show selection info immediately without loading screen
        this.showSelectionInfo(lat, lng);
    }
    
    // Handle feature selection
    handleFeatureSelected(detail) {
        const { feature, latlng } = detail;
        
        // Show profile view immediately without loading screen
        this.showProfileView(feature, latlng);
    }
    
    // Show selection information
    showSelectionInfo(lat, lng) {
        if (!this.elements.selectionInfo || !this.elements.selectionContent) {
            return;
        }
        
        // Create content for the selected point
        const content = `
            <div class="selection-details">
                <p><strong>Coordinates:</strong><br>
                   Lat: ${lat.toFixed(6)}<br>
                   Lng: ${lng.toFixed(6)}</p>
                <p><em>Click on a soil polygon for detailed information</em></p>
            </div>
        `;
        
        this.elements.selectionContent.innerHTML = content;
        this.elements.selectionInfo.style.display = 'block';
    }
    
    // Show profile view
    showProfileView(feature, latlng) {
        this.currentView = 'profile';
        
        // Hide main view elements
        if (this.elements.selectionInfo) {
            this.elements.selectionInfo.style.display = 'none';
        }
        
        // Show profile view
        if (this.elements.profileView) {
            this.elements.profileView.style.display = 'block';
        }
        
        // Generate mock soil profile chart
        this.generateSoilProfileChart(feature.properties);
    }
    
    // Show main view
    showMainView() {
        this.currentView = 'main';
        
        // Hide profile view
        if (this.elements.profileView) {
            this.elements.profileView.style.display = 'none';
        }
        
        // Show selection info if it was visible
        if (this.elements.selectionInfo) {
            this.elements.selectionInfo.style.display = 'block';
        }
    }
    
    // Generate soil profile chart (placeholder implementation)
    generateSoilProfileChart(properties) {
        const chartContainer = document.getElementById('soil-profile-chart');
        if (!chartContainer) return;
        
        // Create a simple chart representation
        const soilOrder = properties.soilOrderDisplay || 'Unknown';
        const mapUnit = properties.displayName || 'Unknown Map Unit';
        
        chartContainer.innerHTML = `
            <div style="padding: 20px; text-align: center;">
                <h5>${mapUnit}</h5>
                <p><strong>Soil Order:</strong> ${soilOrder}</p>
                <div style="margin: 20px 0; padding: 20px; background: #f8f9fa; border-radius: 6px;">
                    <p><em>Soil profile chart would be rendered here</em></p>
                    <p>This would show depth vs. properties (OC, pH, etc.)</p>
                </div>
                <p><small>Note: Raster data processing not yet implemented</small></p>
            </div>
        `;
    }
    
    // Show info modal
    showInfoModal() {
        if (this.elements.infoModal) {
            this.elements.infoModal.style.display = 'flex';
        }
    }
    
    // Hide info modal
    hideInfoModal() {
        if (this.elements.infoModal) {
            this.elements.infoModal.style.display = 'none';
        }
    }
    
    // Show loading overlay
    showLoading(message = 'Loading...') {
        if (this.elements.loading) {
            const loadingText = this.elements.loading.querySelector('span');
            if (loadingText) {
                loadingText.textContent = message;
            }
            this.elements.loading.style.display = 'flex';
        }
    }
    
    // Update loading message without changing visibility
    updateLoadingMessage(message) {
        if (this.elements.loading) {
            const loadingText = this.elements.loading.querySelector('span');
            if (loadingText) {
                loadingText.textContent = message;
            }
        }
    }
    
    // Hide loading overlay
    hideLoading() {
        if (this.elements.loading) {
            this.elements.loading.style.display = 'none';
        }
    }
    
    // Handle keyboard shortcuts
    handleKeyboardShortcuts(e) {
        // Escape key to close modals or return to main view
        if (e.key === 'Escape') {
            if (this.elements.infoModal.style.display === 'flex') {
                this.hideInfoModal();
            } else if (this.currentView === 'profile') {
                this.showMainView();
            }
        }
        
        // Toggle sidebar with 'S' key
        if (e.key === 's' || e.key === 'S') {
            if (!e.target.matches('input, textarea, select')) {
                this.toggleSidebar();
            }
        }
        
        // Show info with 'I' key
        if (e.key === 'i' || e.key === 'I') {
            if (!e.target.matches('input, textarea, select')) {
                this.showInfoModal();
            }
        }
    }
    
    // Handle window resize
    handleWindowResize() {
        // Adjust for mobile responsiveness
        const isMobile = window.innerWidth <= 768;
        
        if (isMobile && this.elements.sidebar) {
            // On mobile, always collapse sidebar initially
            if (!this.sidebarCollapsed) {
                this.elements.sidebar.classList.add('collapsed');
            }
        }
    }
    
    // Update UI state based on data loading
    updateUIForDataState(dataLoaded) {
        // Enable/disable controls based on data availability
        const controls = [
            ...this.elements.mapTypeRadios,
            this.elements.depthSelector,
            this.elements.boundariesCheckbox
        ];
        
        controls.forEach(control => {
            if (control) {
                control.disabled = !dataLoaded;
            }
        });
        
        if (!dataLoaded) {
            this.showLoading('Loading map data...');
        } else {
            this.hideLoading();
        }
    }
    
    // Get current UI state
    getCurrentState() {
        return {
            sidebarCollapsed: this.sidebarCollapsed,
            currentView: this.currentView,
            currentMapType: this.currentMapType,
            currentDepth: this.currentDepth,
            showBoundaries: this.showBoundaries
        };
    }
    
    // Set UI state
    setState(state) {
        if (state.mapType && state.mapType !== this.currentMapType) {
            const radio = document.querySelector(`input[name="map_type"][value="${state.mapType}"]`);
            if (radio) {
                radio.checked = true;
                this.handleMapTypeChange(state.mapType);
            }
        }
        
        if (state.depth !== undefined && state.depth !== this.currentDepth) {
            if (this.elements.depthSelector) {
                this.elements.depthSelector.value = state.depth;
                this.handleDepthChange(state.depth);
            }
        }
        
        if (state.showBoundaries !== undefined && state.showBoundaries !== this.showBoundaries) {
            if (this.elements.boundariesCheckbox) {
                this.elements.boundariesCheckbox.checked = state.showBoundaries;
                this.handleBoundariesToggle(state.showBoundaries);
            }
        }
    }
}

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { UIController };
}