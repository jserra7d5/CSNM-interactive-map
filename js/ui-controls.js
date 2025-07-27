// UI Controls and Interactions
// Handles user interface elements and their interactions

class UIController {
    constructor() {
        this.currentView = 'main'; // 'main' or 'profile'
        this.currentMapType = 'satellite'; // Default to satellite view
        this.currentDepth = 0;
        this.showBoundaries = false;
        this.showHighways = false;
        this.showServiceRoads = false;
        this.showInformationCenter = false;
        // Initialize dropdown states from localStorage or default to all closed
        this.dropdownStates = this.loadDropdownStates() || {
            'ssurgo': false,
            'soil-properties': false,
            'forming-factors': false,
            'overlays': false
        };
        
        this.elements = {};
        this.initializeElements();
        this.setupEventListeners();
        this.applyDropdownStates();
        
        // Ensure satellite radio is checked on init
        const satelliteRadio = document.querySelector('input[name="map_type"][value="satellite"]');
        if (satelliteRadio) {
            satelliteRadio.checked = true;
        }
    }
    
    // Initialize DOM element references
    initializeElements() {
        this.elements = {
            // Dropdown menus
            dropdownHeaders: document.querySelectorAll('.dropdown-header'),
            dropdownMenus: document.querySelectorAll('.dropdown-menu'),
            
            // Map type radios
            mapTypeRadios: document.querySelectorAll('input[name="map_type"]'),
            
            // Depth controls
            depthControls: document.getElementById('depth-controls'),
            depthSelector: document.getElementById('depth-selector'),
            
            // Overlay checkboxes
            boundariesCheckbox: document.getElementById('show-boundaries'),
            highwaysCheckbox: document.getElementById('show-highways'),
            serviceRoadsCheckbox: document.getElementById('show-service-roads'),
            informationCenterCheckbox: document.getElementById('show-information-center'),
            
            // Landing page
            landingOverlay: document.getElementById('landing-overlay'),
            getStartedBtn: document.getElementById('get-started-btn'),
            
            // SSURGO info panel
            ssurgoInfoPanel: document.getElementById('ssurgo-info-panel'),
            closeSsurgoPanel: document.getElementById('close-ssurgo-panel'),
            sectionHeaders: document.querySelectorAll('.section-header'),
            
            // Other UI elements
            infoBtn: document.getElementById('info-btn'),
            infoModal: document.getElementById('info-modal'),
            closeModalBtn: document.getElementById('close-modal'),
            loading: document.getElementById('loading'),
            soilLegend: document.getElementById('soil-legend')
        };
    }
    
    // Setup event listeners for UI controls
    setupEventListeners() {
        // Dropdown menu headers
        this.elements.dropdownHeaders.forEach(header => {
            header.addEventListener('click', (e) => {
                const menuType = header.dataset.menu;
                this.toggleDropdown(menuType);
            });
        });
        
        // Landing page Get Started button
        if (this.elements.getStartedBtn) {
            this.elements.getStartedBtn.addEventListener('click', () => {
                this.closeLandingPage();
            });
        }
        
        // SSURGO panel close button
        if (this.elements.closeSsurgoPanel) {
            this.elements.closeSsurgoPanel.addEventListener('click', () => {
                this.closeSsurgoPanel();
            });
        }
        
        // SSURGO panel section headers
        this.elements.sectionHeaders.forEach(header => {
            header.addEventListener('click', (e) => {
                const section = header.dataset.section;
                this.toggleSection(section);
            });
        });
        
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
        const showDepthControls = mapType === 'oc' || mapType === 'ph' || mapType === 'meanTemp';
        
        if (this.elements.depthControls) {
            this.elements.depthControls.style.display = showDepthControls ? 'block' : 'none';
        }
        
        // Update depth label
        if (this.elements.depthLabel && showDepthControls) {
            let label;
            if (mapType === 'oc') {
                label = 'Organic Carbon Depth:';
            } else if (mapType === 'ph') {
                label = 'pH Depth:';
            } else if (mapType === 'meanTemp') {
                label = 'Mean Temperature Depth:';
            }
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
    
    // New dropdown menu methods
    
    // Toggle dropdown menu visibility
    toggleDropdown(menuType) {
        const menu = document.getElementById(`menu-${menuType}`);
        if (!menu) return;
        
        const isCollapsed = menu.classList.contains('collapsed');
        
        // Toggle the menu
        if (isCollapsed) {
            menu.classList.remove('collapsed');
            this.dropdownStates[menuType] = true;
        } else {
            menu.classList.add('collapsed');
            this.dropdownStates[menuType] = false;
        }
        
        // Save the state to localStorage
        this.saveDropdownStates();
    }
    
    // Close landing page
    closeLandingPage() {
        if (this.elements.landingOverlay) {
            this.elements.landingOverlay.style.display = 'none';
        }
    }
    
    // Open SSURGO info panel
    openSsurgoPanel(mapUnitData) {
        if (this.elements.ssurgoInfoPanel) {
            this.elements.ssurgoInfoPanel.style.display = 'flex';
            this.populateSsurgoPanel(mapUnitData);
        }
    }
    
    // Close SSURGO info panel
    closeSsurgoPanel() {
        if (this.elements.ssurgoInfoPanel) {
            this.elements.ssurgoInfoPanel.style.display = 'none';
        }
    }
    
    // Toggle section in SSURGO panel
    toggleSection(sectionName) {
        const section = document.querySelector(`.info-section .section-header[data-section="${sectionName}"]`).parentElement;
        if (section) {
            section.classList.toggle('collapsed');
        }
    }
    
    // Populate SSURGO panel with data
    populateSsurgoPanel(data) {
        // Map Unit Composition
        const compositionContent = document.getElementById('composition-content');
        if (compositionContent && data.components) {
            let html = '<table class="info-table">';
            html += '<tr><th>Component</th><th>%</th><th>Kind</th></tr>';
            data.components.forEach(comp => {
                html += `<tr>
                    <td>${comp.compname || 'N/A'}</td>
                    <td>${comp.comppct_r || 'N/A'}</td>
                    <td>${comp.compkind || 'N/A'}</td>
                </tr>`;
            });
            html += '</table>';
            compositionContent.innerHTML = html;
        }
        
        // Map Unit Data
        const mapunitContent = document.getElementById('mapunit-content');
        if (mapunitContent) {
            const muData = data.mapunit || {};
            mapunitContent.innerHTML = `
                <div class="info-item"><strong>MUKEY:</strong> ${muData.mukey || 'N/A'}</div>
                <div class="info-item"><strong>MUSYM:</strong> ${muData.musym || 'N/A'}</div>
                <div class="info-item"><strong>Name:</strong> ${muData.muname || 'N/A'}</div>
                <div class="info-item"><strong>Acres:</strong> ${muData.muacres ? muData.muacres.toFixed(1) : 'N/A'}</div>
            `;
        }
        
        // Survey Metadata
        const metadataContent = document.getElementById('metadata-content');
        if (metadataContent) {
            metadataContent.innerHTML = `
                <div class="info-item"><strong>Survey Area:</strong> ${data.areasymbol || 'N/A'}</div>
                <div class="info-item"><strong>Spatial Version:</strong> ${data.spatialver || 'N/A'}</div>
            `;
        }
    }
    
    // Load dropdown states from localStorage
    loadDropdownStates() {
        try {
            const savedStates = localStorage.getItem('csnm-dropdown-states');
            return savedStates ? JSON.parse(savedStates) : null;
        } catch (error) {
            console.error('Error loading dropdown states:', error);
            return null;
        }
    }
    
    // Save dropdown states to localStorage
    saveDropdownStates() {
        try {
            localStorage.setItem('csnm-dropdown-states', JSON.stringify(this.dropdownStates));
        } catch (error) {
            console.error('Error saving dropdown states:', error);
        }
    }
    
    // Apply saved dropdown states on initialization
    applyDropdownStates() {
        Object.entries(this.dropdownStates).forEach(([menuType, isOpen]) => {
            const menu = document.getElementById(`menu-${menuType}`);
            if (menu) {
                if (isOpen) {
                    menu.classList.remove('collapsed');
                } else {
                    menu.classList.add('collapsed');
                }
            }
        });
    }
}

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { UIController };
}