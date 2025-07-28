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
        this.currentSeriesView = null; // Track if viewing a series detail
        this.currentSeriesName = null; // Track which series is being viewed
        this.openSections = new Set(); // Track which sections are open
        this.activeChart = 'soil-sketch'; // Track active soil profile chart
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
        this.adjustDropdownContainerHeight();
        
        // Setup window resize listener
        window.addEventListener('resize', () => this.adjustDropdownContainerHeight());
        
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
            
            // SSURGO detail panel (SoilWeb style)
            ssurgoDetailPanel: document.getElementById('ssurgo-detail-panel'),
            closeSsurgoDetailPanel: document.getElementById('close-detail-panel'),
            detailSectionHeaders: document.querySelectorAll('.detail-section-header'),
            
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
        
        // SSURGO detail panel close button
        if (this.elements.closeSsurgoDetailPanel) {
            this.elements.closeSsurgoDetailPanel.addEventListener('click', () => {
                this.closeSsurgoDetailPanel();
            });
        }
        
        // SSURGO detail panel section headers
        this.elements.detailSectionHeaders.forEach(header => {
            header.addEventListener('click', (e) => {
                const section = header.dataset.section;
                this.toggleDetailSection(section);
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
        
        // Source info buttons
        this.attachSourceInfoHandlers();
        
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
        
        // Raster processing progress event
        document.addEventListener('rasterProcessingProgress', (e) => {
            this.handleRasterProgress(e.detail);
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
    
    // Attach source info button handlers
    attachSourceInfoHandlers() {
        // Get all info buttons
        const infoButtons = document.querySelectorAll('.info-btn');
        
        infoButtons.forEach(btn => {
            btn.addEventListener('click', (e) => {
                e.preventDefault();
                e.stopPropagation();
                
                // Get the source type from data attribute
                const sourceType = btn.dataset.source;
                this.showSourceModal(sourceType);
            });
        });
        
        // Source modal close button
        const closeSourceModal = document.getElementById('close-source-modal');
        if (closeSourceModal) {
            closeSourceModal.addEventListener('click', () => {
                this.hideSourceModal();
            });
        }
        
        // Click outside to close
        const sourceModal = document.getElementById('source-modal');
        if (sourceModal) {
            sourceModal.addEventListener('click', (e) => {
                if (e.target === sourceModal) {
                    this.hideSourceModal();
                }
            });
        }
    }
    
    // Show source information modal
    showSourceModal(sourceType) {
        const modal = document.getElementById('source-modal');
        const titleElement = document.getElementById('source-modal-title');
        const bodyElement = document.getElementById('source-modal-body');
        
        if (!modal || !titleElement || !bodyElement) return;
        
        // Get source data from config
        const sourceData = CONFIG.dataSources[sourceType];
        if (!sourceData) {
            console.error('No source data found for:', sourceType);
            return;
        }
        
        // Set title
        titleElement.textContent = sourceData.name;
        
        // Build content HTML
        let html = `
            <div class="source-info">
                <p class="source-description">${sourceData.description}</p>
                
                <div class="source-details">
                    <p><strong>Agency/Organization:</strong> ${sourceData.agency}</p>
                    <p><strong>Resolution:</strong> ${sourceData.resolution}</p>
                    ${sourceData.units ? `<p><strong>Units:</strong> ${sourceData.units}</p>` : ''}
                    ${sourceData.lastUpdate ? `<p><strong>Last Updated:</strong> ${sourceData.lastUpdate}</p>` : ''}
                    ${sourceData.verticalAccuracy ? `<p><strong>Vertical Accuracy:</strong> ${sourceData.verticalAccuracy}</p>` : ''}
                    ${sourceData.classes ? `<p><strong>Classification:</strong> ${sourceData.classes}</p>` : ''}
                </div>
                
                <div class="source-links">
                    <p><strong>Links:</strong></p>
                    <ul>
                        <li><a href="${sourceData.url}" target="_blank" rel="noopener noreferrer">Official Website</a></li>
                        ${sourceData.dataUrl ? `<li><a href="${sourceData.dataUrl}" target="_blank" rel="noopener noreferrer">Data Access</a></li>` : ''}
                    </ul>
                </div>
                
                <div class="source-citation">
                    <p><strong>Citation:</strong></p>
                    <p class="citation-text">${sourceData.citation}</p>
                </div>
            </div>
        `;
        
        bodyElement.innerHTML = html;
        modal.style.display = 'flex';
    }
    
    // Hide source modal
    hideSourceModal() {
        const modal = document.getElementById('source-modal');
        if (modal) {
            modal.style.display = 'none';
        }
    }
    
    // Show loading overlay
    showLoading(message = 'Loading...') {
        if (this.elements.loading) {
            console.log('showLoading called with message:', message);
            const loadingText = this.elements.loading.querySelector('span');
            if (loadingText) {
                loadingText.textContent = message;
            }
            // Reset any hidden states
            this.elements.loading.style.display = 'flex';
            this.elements.loading.style.visibility = 'visible';
            this.elements.loading.style.zIndex = '2000';
            
            // Force browser to repaint
            this.elements.loading.offsetHeight;
            console.log('Loading screen should now be visible');
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
            // Also ensure the loading widget is fully removed from view
            this.elements.loading.style.visibility = 'hidden';
            this.elements.loading.style.zIndex = '-1';
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
    
    // Handle raster processing progress
    handleRasterProgress(detail) {
        const { property, progress, message } = detail;
        
        // Update loading message with progress
        if (this.elements.loading) {
            const loadingText = this.elements.loading.querySelector('span');
            const progressContainer = this.elements.loading.querySelector('.loading-progress-container');
            const progressFill = this.elements.loading.querySelector('.loading-progress-fill');
            const progressText = this.elements.loading.querySelector('.loading-progress-text');
            
            if (property === 'elevation' || property === 'landcover') {
                // Show progress bar for elevation and land cover
                if (progressContainer) {
                    progressContainer.style.display = 'block';
                }
                
                // Update loading text
                if (loadingText) {
                    const mapType = property === 'elevation' ? 'elevation map' : 'land cover';
                    loadingText.textContent = `Loading ${mapType}...`;
                }
                
                // Update progress bar with forced repaint
                if (progressFill) {
                    progressFill.style.width = `${progress}%`;
                    // Force browser to repaint
                    progressFill.offsetHeight;
                }
                
                // Update progress text with forced repaint
                if (progressText) {
                    progressText.textContent = `${progress}%`;
                    // Force browser to repaint
                    progressText.offsetHeight;
                }
            } else {
                // For other properties, just show the message
                if (loadingText) {
                    loadingText.textContent = message;
                }
                if (progressContainer) {
                    progressContainer.style.display = 'none';
                }
            }
            
            // Hide loading screen when processing is complete
            if (progress === 100) {
                setTimeout(() => {
                    if (this.elements.loading) {
                        this.elements.loading.style.display = 'none';
                        // Reset progress bar for next time
                        if (progressContainer) {
                            progressContainer.style.display = 'none';
                        }
                        if (progressFill) {
                            progressFill.style.width = '0%';
                        }
                    }
                }, 500); // Brief delay before hiding
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
    // Adjust dropdown container height dynamically
    adjustDropdownContainerHeight() {
        const dropdownContainer = document.querySelector('.dropdown-menu-container');
        if (!dropdownContainer) return;
        
        // Get the mouse coordinates element position
        const mouseCoords = document.querySelector('.mouse-coords');
        const bottomOffset = mouseCoords ? mouseCoords.offsetHeight + 50 : 100; // Add buffer
        
        // Calculate available height
        const viewportHeight = window.innerHeight;
        const containerTop = dropdownContainer.offsetTop;
        const maxHeight = viewportHeight - containerTop - bottomOffset;
        
        // Apply the calculated max height
        dropdownContainer.style.maxHeight = `${maxHeight}px`;
        
        // Also adjust when dropdowns are toggled
        setTimeout(() => {
            const newMaxHeight = viewportHeight - containerTop - bottomOffset;
            dropdownContainer.style.maxHeight = `${newMaxHeight}px`;
        }, 300); // Wait for animation
    }
    
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
        
        // Adjust container height after toggling
        this.adjustDropdownContainerHeight();
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
    
    // Open SSURGO detail panel (SoilWeb style)
    openSsurgoDetailPanel(detailData, clickLocation = null) {
        if (this.elements.ssurgoDetailPanel) {
            // Check if we're currently viewing a series detail BEFORE resetting
            const wasViewingSeries = this.currentSeriesView === true;
            const previousSeriesName = this.currentSeriesName;
            
            // Only reset and repopulate if we're NOT in a series view
            // If we're in a series view, we'll handle the update differently
            if (!wasViewingSeries) {
                // We're in the main map unit view - just update the data
                this.resetSsurgoDetailPanel();
                this.elements.ssurgoDetailPanel.style.display = 'flex';
                this.populateSsurgoDetailPanel(detailData, clickLocation);
            } else {
                // We're in a series detail view - need to update for the new location
                // but stay in the series view
                if (detailData.components && detailData.components.length > 0) {
                    // Find a matching series or use the first available one
                    let componentToShow = null;
                    
                    // First try to find the same series name
                    componentToShow = detailData.components.find(comp => comp.compname === previousSeriesName);
                    
                    // If not found, use the first component
                    if (!componentToShow) {
                        componentToShow = detailData.components[0];
                    }
                    
                    // Update the map unit data for the new location
                    this.currentMapUnitData = detailData;
                    
                    // Show the series detail for this component
                    if (componentToShow) {
                        this.showSoilSeriesDetail(componentToShow.compname, clickLocation, componentToShow);
                    }
                } else {
                    // No components available, fall back to main view
                    this.resetSsurgoDetailPanel();
                    this.elements.ssurgoDetailPanel.style.display = 'flex';
                    this.populateSsurgoDetailPanel(detailData, clickLocation);
                }
            }
        }
    }
    
    // Close SSURGO detail panel
    closeSsurgoDetailPanel() {
        if (this.elements.ssurgoDetailPanel) {
            this.elements.ssurgoDetailPanel.style.display = 'none';
            // Reset tracking state
            this.currentSeriesView = null;
            this.currentSeriesName = null;
            this.openSections.clear();
        }
    }
    
    // Toggle section in detail panel
    toggleDetailSection(sectionName) {
        const header = document.querySelector(`.detail-section-header[data-section="${sectionName}"]`);
        if (header) {
            header.classList.toggle('active');
            header.classList.toggle('collapsed');
            const content = header.nextElementSibling;
            if (content) {
                content.style.display = header.classList.contains('active') ? 'block' : 'none';
                // Track open sections
                if (header.classList.contains('active')) {
                    this.openSections.add(sectionName);
                } else {
                    this.openSections.delete(sectionName);
                }
            }
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
    
    // Reset SSURGO detail panel to original state
    resetSsurgoDetailPanel() {
        // Get the original panel structure from the HTML
        const detailPanel = document.getElementById('ssurgo-detail-panel');
        if (!detailPanel) return;
        
        // Reset to original structure
        detailPanel.innerHTML = `
            <div class="detail-panel-header">
                <h3 id="detail-panel-title">Map Unit Information</h3>
                <button class="panel-close" id="close-detail-panel">
                    <i class="fas fa-times"></i>
                </button>
            </div>
            <div class="detail-panel-content">
                <!-- Map Unit Composition Section -->
                <div class="detail-section">
                    <div class="detail-section-header active" data-section="detail-composition">
                        <i class="fas fa-caret-down section-arrow"></i>
                        <span>Map Unit Composition</span>
                    </div>
                    <div class="detail-section-content" id="detail-composition-content" style="display: block;">
                        <!-- Content will be dynamically populated -->
                    </div>
                </div>
                
                <!-- Map Unit Data Section -->
                <div class="detail-section">
                    <div class="detail-section-header active" data-section="detail-mapunit">
                        <i class="fas fa-caret-down section-arrow"></i>
                        <span>Map Unit Data</span>
                    </div>
                    <div class="detail-section-content" id="detail-mapunit-content" style="display: block;">
                        <!-- Content will be dynamically populated -->
                    </div>
                </div>
                
                <!-- Survey Metadata Section -->
                <div class="detail-section">
                    <div class="detail-section-header active" data-section="detail-metadata">
                        <i class="fas fa-caret-down section-arrow"></i>
                        <span>Survey Metadata</span>
                    </div>
                    <div class="detail-section-content" id="detail-metadata-content" style="display: block;">
                        <!-- Content will be dynamically populated -->
                    </div>
                </div>
            </div>
        `;
        
        // Re-attach event handlers
        this.attachDetailPanelEventHandlers();
    }
    
    // Populate SSURGO detail panel with enhanced data (SoilWeb style)
    populateSsurgoDetailPanel(data, clickLocation = null) {
        // Store the map unit data for later use
        this.currentMapUnitData = data;
        
        // Store click location for later use
        if (clickLocation) {
            this.lastClickLocation = clickLocation;
            // Clear any cached raster data when new location is clicked
            this.lastRasterData = null;
        }
        // Update panel title
        const titleElement = document.getElementById('detail-panel-title');
        if (titleElement) {
            titleElement.textContent = data.mapUnitName;
        }
        
        // Populate Map Unit Composition
        const compositionContent = document.getElementById('detail-composition-content');
        if (compositionContent) {
            let html = '<ul class="component-list">';
            
            data.components.forEach(comp => {
                // Show placeholder if percentage is not a number
                const percentage = (typeof comp.comppct_r === 'number') ? comp.comppct_r + '%' : comp.comppct_r;
                html += `
                    <li class="component-item">
                        <div class="component-name">
                            ${comp.horizonData && comp.horizonData !== 'placeholder' && comp.horizonData !== 'Not available' ? 
                                `<a href="#" class="soil-series-link" data-series="${comp.compname}" data-component-data='${JSON.stringify(comp)}'>
                                    ${percentage} - <strong>${comp.compname}</strong>
                                </a>` :
                                `${percentage} - <strong>${comp.compname}</strong>`
                            }
                        </div>
                        <div class="component-details">
                            Geomorphic Position: <span class="geomorphic-position">${comp.geomorphicPosition}</span><br>
                            Horizon data ${comp.horizonData}
                        </div>
                    </li>
                `;
            });
            
            html += '</ul>';
            compositionContent.innerHTML = html;
        }
        
        // Populate Map Unit Data
        const mapunitContent = document.getElementById('detail-mapunit-content');
        if (mapunitContent) {
            const muData = data.mapunitData;
            let html = '<ul class="mapunit-data-list">';
            
            // Only show fields with actual data
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Map Unit Key:</span>
                    <span class="mapunit-data-value">${muData.mukey} [Graphical Summary]</span>
                </li>
            `;
            
            // Only add fields that aren't placeholders
            if (muData.drainageClass && muData.drainageClass !== 'placeholder') {
                html += `
                    <li class="mapunit-data-item">
                        <span class="mapunit-data-label">Drainage Class (Dominant Condition):</span>
                        <span class="mapunit-data-value">${muData.drainageClass} <span class="mapunit-data-help">?</span></span>
                    </li>
                `;
            }
            
            if (muData.drainageClassWet && muData.drainageClassWet !== 'placeholder') {
                html += `
                    <li class="mapunit-data-item">
                        <span class="mapunit-data-label">Drainage Class (Wettest Component):</span>
                        <span class="mapunit-data-value">${muData.drainageClassWet} <span class="mapunit-data-help">?</span></span>
                    </li>
                `;
            }
            
            if (muData.hydricSoilsProportion && muData.hydricSoilsProportion !== 'placeholder') {
                html += `
                    <li class="mapunit-data-item">
                        <span class="mapunit-data-label">Proportion of Hydric Soils:</span>
                        <span class="mapunit-data-value">${muData.hydricSoilsProportion} <span class="mapunit-data-help">?</span></span>
                    </li>
                `;
            }
            
            if (muData.hydgrp && muData.hydgrp !== 'placeholder') {
                html += `
                    <li class="mapunit-data-item">
                        <span class="mapunit-data-label">Hydrologic Soil Group:</span>
                        <span class="mapunit-data-value">${muData.hydgrp} <span class="mapunit-data-help">?</span></span>
                    </li>
                `;
            }
            
            html += '</ul>';
            mapunitContent.innerHTML = html;
        }
        
        // Populate Survey Metadata
        const metadataContent = document.getElementById('detail-metadata-content');
        if (metadataContent) {
            const surveyData = data.surveyMetadata;
            let html = '<ul class="survey-metadata-list">';
            
            html += `
                <li class="survey-metadata-item">
                    <span class="survey-metadata-label">Soil Survey Area:</span>
                    <span class="survey-metadata-value">${surveyData.areaSymbol} <span class="mapunit-data-help">?</span></span>
                </li>
                <li class="survey-metadata-item">
                    <span class="survey-metadata-label">Scale:</span>
                    <span class="survey-metadata-value">${surveyData.scale} <span class="mapunit-data-help">?</span></span>
                </li>
                <li class="survey-metadata-item">
                    <span class="survey-metadata-label">Published:</span>
                    <span class="survey-metadata-value">${surveyData.published} <span class="mapunit-data-help">?</span></span>
                </li>
                <li class="survey-metadata-item">
                    <span class="survey-metadata-label">Last Export:</span>
                    <span class="survey-metadata-value">${surveyData.lastExport} <span class="mapunit-data-help">?</span></span>
                </li>
            `;
            
            html += '</ul>';
            metadataContent.innerHTML = html;
        }
        
        // Add click handlers for soil series links
        setTimeout(() => {
            const seriesLinks = document.querySelectorAll('.soil-series-link');
            seriesLinks.forEach(link => {
                link.addEventListener('click', (e) => {
                    e.preventDefault();
                    const link = e.target.closest('.soil-series-link');
                    const seriesName = link.dataset.series;
                    const componentData = link.dataset.componentData ? JSON.parse(link.dataset.componentData) : null;
                    this.showSoilSeriesDetail(seriesName, null, componentData);
                });
            });
        }, 0);
    }
    
    // Show soil series detail view
    showSoilSeriesDetail(seriesName, clickLocation = null, componentData = null) {
        const detailPanel = document.getElementById('ssurgo-detail-panel');
        if (!detailPanel) return;
        
        // Track that we're viewing a series
        this.currentSeriesView = true;
        this.currentSeriesName = seriesName;
        
        // If no sections are open, default to opening soil profiles
        if (this.openSections.size === 0) {
            this.openSections.add('soil-profiles');
        }
        
        // Store click location for later use
        if (clickLocation) {
            this.lastClickLocation = clickLocation;
        }
        
        // Create the soil series detail view HTML
        const detailHTML = `
            <div class="detail-panel-header">
                <h3 id="detail-panel-title">${seriesName}</h3>
                <button class="panel-close" id="close-detail-panel">
                    <i class="fas fa-times"></i>
                </button>
            </div>
            <div class="detail-panel-content">
                <div class="series-links">
                    <a href="#" class="series-link">Soil Data Explorer</a> | 
                    <a href="#" class="series-link">Series Extent Explorer</a> | 
                    <a href="#" class="series-link">Description</a>
                </div>
                
                <div class="detail-section">
                    <div class="detail-section-header ${this.openSections.has('soil-profiles') ? 'active' : 'collapsed'}" data-section="soil-profiles">
                        <i class="fas fa-caret-down section-arrow"></i> Soil Profiles
                    </div>
                    <div class="detail-section-content" style="display: ${this.openSections.has('soil-profiles') ? 'block' : 'none'};">
                        <div class="soil-profile-container">
                            <div class="profile-buttons-grid">
                                <button class="profile-btn ${this.activeChart === 'soil-sketch' ? 'active' : ''}" data-chart="soil-sketch">
                                    Soil Sketch <i class="fas fa-info-circle"></i>
                                </button>
                                <button class="profile-btn ${this.activeChart === 'org-matter' ? 'active' : ''}" data-chart="org-matter">Org. Matter</button>
                                <button class="profile-btn ${this.activeChart === 'ph' ? 'active' : ''}" data-chart="ph">pH</button>
                            </div>
                            <div class="profile-chart-container">
                                <div id="soil-profile-chart"></div>
                                <div class="chart-footer">
                                    <a href="#" class="view-source-link" id="view-source-link" target="_blank">View Source Data</a>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                
                <div class="detail-section">
                    <div class="detail-section-header ${this.openSections.has('soil-taxonomy') ? 'active' : 'collapsed'}" data-section="soil-taxonomy">
                        <i class="fas fa-caret-down section-arrow"></i> Soil Taxonomy
                    </div>
                    <div class="detail-section-content" style="display: ${this.openSections.has('soil-taxonomy') ? 'block' : 'none'};" id="soil-taxonomy-content">
                        <p>Loading taxonomy data...</p>
                    </div>
                </div>
                
                <div class="detail-section">
                    <div class="detail-section-header ${this.openSections.has('land-classification') ? 'active' : 'collapsed'}" data-section="land-classification">
                        <i class="fas fa-caret-down section-arrow"></i> Land Classification
                    </div>
                    <div class="detail-section-content" style="display: ${this.openSections.has('land-classification') ? 'block' : 'none'};" id="land-classification-content">
                        <p>Loading land classification data...</p>
                    </div>
                </div>
                
                <div class="detail-section">
                    <div class="detail-section-header ${this.openSections.has('hydraulic-erosion') ? 'active' : 'collapsed'}" data-section="hydraulic-erosion">
                        <i class="fas fa-caret-down section-arrow"></i> Hydraulic and Erosion Ratings
                    </div>
                    <div class="detail-section-content" style="display: ${this.openSections.has('hydraulic-erosion') ? 'block' : 'none'};" id="hydraulic-erosion-content">
                        <p>Loading hydraulic and erosion data...</p>
                    </div>
                </div>
                
                <div class="detail-section">
                    <div class="detail-section-header collapsed" data-section="forest-productivity">
                        <i class="fas fa-caret-down section-arrow"></i> Forest Productivity
                    </div>
                    <div class="detail-section-content" style="display: none;">
                        <p>placeholder</p>
                    </div>
                </div>
                
                <div class="detail-section">
                    <div class="detail-section-header collapsed" data-section="soil-suitability">
                        <i class="fas fa-caret-down section-arrow"></i> Soil Suitability Ratings
                    </div>
                    <div class="detail-section-content" style="display: none;">
                        <p>placeholder</p>
                    </div>
                </div>
                
                <div class="detail-section">
                    <div class="detail-section-header ${this.openSections.has('details') ? 'active' : 'collapsed'}" data-section="details">
                        <i class="fas fa-caret-down section-arrow"></i> Details
                    </div>
                    <div class="detail-section-content" style="display: ${this.openSections.has('details') ? 'block' : 'none'};" id="details-content">
                        <div class="details-info">
                            <p><strong>Map Unit Name:</strong> <span id="detail-mapunit-name">Loading...</span></p>
                            <p><strong>Component Key:</strong> <span id="detail-component-key">Loading...</span></p>
                            <p><strong>Data:</strong> 
                                <a href="#" class="detail-data-link" id="component-link">Component</a> &nbsp;&nbsp;
                                <a href="#" class="detail-data-link" id="horizons-link">All Horizons</a> &nbsp;&nbsp;
                                <a href="#" class="detail-data-link" id="lab-data-link">Lab Data</a>
                            </p>
                        </div>
                    </div>
                </div>
                
                <div class="series-back-link">
                    <a href="#" id="back-to-mapunit">← Back to Map Unit</a>
                </div>
            </div>
        `;
        
        // Store the current panel content for back navigation
        this.previousPanelContent = detailPanel.innerHTML;
        
        // Replace panel content
        detailPanel.innerHTML = detailHTML;
        
        // Re-attach event handlers
        this.attachDetailPanelEventHandlers();
        
        // Add back button handler
        const backLink = document.getElementById('back-to-mapunit');
        if (backLink) {
            backLink.addEventListener('click', (e) => {
                e.preventDefault();
                this.restorePreviousPanel();
            });
        }
        
        // Add profile button handlers
        const profileButtons = document.querySelectorAll('.profile-btn');
        profileButtons.forEach(btn => {
            btn.addEventListener('click', (e) => {
                // Remove active class from all buttons
                profileButtons.forEach(b => b.classList.remove('active'));
                // Add active class to clicked button
                btn.classList.add('active');
                // Generate appropriate chart
                const chartType = btn.dataset.chart;
                this.activeChart = chartType; // Track active chart
                this.generateSoilProfileChart(chartType, seriesName);
            });
        });
        
        // Generate the active chart (or default to soil sketch)
        this.generateSoilProfileChart(this.activeChart || 'soil-sketch', seriesName);
        
        // Set up view source link
        const viewSourceLink = document.getElementById('view-source-link');
        if (viewSourceLink) {
            viewSourceLink.href = `https://soilseries.sc.egov.usda.gov/OSD_Docs/${seriesName.charAt(0).toUpperCase()}/${seriesName.toUpperCase()}.html`;
            viewSourceLink.target = '_blank';
            viewSourceLink.rel = 'noopener noreferrer';
        }
        
        // Populate component data sections if component data is available
        if (componentData) {
            this.populateDetails(seriesName, componentData);
            this.populateSoilTaxonomy(componentData);
            this.populateLandClassification(componentData);
            this.populateHydraulicErosion(componentData);
        }
    }
    
    // Populate details section with component data
    populateDetails(seriesName, componentData) {
        // Get the parent map unit info from the original detail data
        const detailPanel = document.getElementById('ssurgo-detail-panel');
        if (!detailPanel) return;
        
        // Try to get map unit name from the stored map unit data first
        let mapUnitName = 'Data not available';
        if (this.currentMapUnitData && this.currentMapUnitData.mapUnitName) {
            mapUnitName = this.currentMapUnitData.mapUnitName;
        } else if (componentData && componentData.properties) {
            // Fallback to component data
            const props = componentData.properties;
            const musym = props.MUSYM || props.musym || '';
            const muname = props.muname || '';
            if (musym && muname) {
                mapUnitName = `${musym} - ${muname}`;
            } else if (musym) {
                mapUnitName = musym;
            }
        }
        
        // Get component key
        const componentKey = componentData.cokey || componentData.properties?.cokey || 'Data not available';
        
        // Update the details content
        const mapUnitNameElement = document.getElementById('detail-mapunit-name');
        const componentKeyElement = document.getElementById('detail-component-key');
        
        if (mapUnitNameElement) {
            mapUnitNameElement.textContent = mapUnitName;
        }
        
        if (componentKeyElement) {
            componentKeyElement.textContent = componentKey;
        }
        
        // Set up data links
        const componentLink = document.getElementById('component-link');
        const horizonsLink = document.getElementById('horizons-link');
        const labDataLink = document.getElementById('lab-data-link');
        
        if (componentLink && componentKey !== 'Data not available') {
            // Link to NRCS Web Soil Survey component report
            componentLink.href = `https://websoilsurvey.sc.egov.usda.gov/App/WebSoilSurvey.aspx`;
            componentLink.target = '_blank';
            componentLink.rel = 'noopener noreferrer';
            componentLink.title = 'View component data in Web Soil Survey';
        }
        
        if (horizonsLink && componentKey !== 'Data not available') {
            // Link to horizon data
            horizonsLink.href = `https://websoilsurvey.sc.egov.usda.gov/App/WebSoilSurvey.aspx`;
            horizonsLink.target = '_blank';
            horizonsLink.rel = 'noopener noreferrer';
            horizonsLink.title = 'View horizon data in Web Soil Survey';
        }
        
        if (labDataLink && componentKey !== 'Data not available') {
            // Link to NASIS/KSSL lab data
            labDataLink.href = `https://ncsslabdatamart.sc.egov.usda.gov/`;
            labDataLink.target = '_blank';
            labDataLink.rel = 'noopener noreferrer';
            labDataLink.title = 'View lab data in NCSS Lab Data Mart';
        }
    }
    
    // Populate soil taxonomy section with component data
    populateSoilTaxonomy(componentData) {
        const taxonomyContent = document.getElementById('soil-taxonomy-content');
        if (!taxonomyContent) return;
        
        // Create HTML for taxonomy data
        let html = '<ul class="mapunit-data-list">';
        
        // Add taxonomic classification
        if (componentData.taxclname) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Full Classification:</span>
                    <span class="mapunit-data-value">${componentData.taxclname || 'Not specified'}</span>
                </li>
            `;
        }
        
        // Add taxonomy hierarchy
        const taxonomyFields = [
            { field: 'taxorder', label: 'Order' },
            { field: 'taxsuborder', label: 'Suborder' },
            { field: 'taxgrtgroup', label: 'Great Group' },
            { field: 'taxsubgrp', label: 'Subgroup' }
        ];
        
        taxonomyFields.forEach(item => {
            if (componentData[item.field]) {
                html += `
                    <li class="mapunit-data-item">
                        <span class="mapunit-data-label">${item.label}:</span>
                        <span class="mapunit-data-value">${componentData[item.field] || 'Not specified'}</span>
                    </li>
                `;
            }
        });
        
        // Add particle size and modifiers
        if (componentData.taxpartsize) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Particle Size:</span>
                    <span class="mapunit-data-value">${componentData.taxpartsize}${componentData.taxpartsizemod ? ' (' + componentData.taxpartsizemod + ')' : ''}</span>
                </li>
            `;
        }
        
        // Add activity class
        if (componentData.taxceactcl && componentData.taxceactcl !== 'not used') {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">CEC Activity Class:</span>
                    <span class="mapunit-data-value">${componentData.taxceactcl}</span>
                </li>
            `;
        }
        
        // Add reaction class
        if (componentData.taxreaction && componentData.taxreaction !== 'not used') {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Reaction Class:</span>
                    <span class="mapunit-data-value">${componentData.taxreaction}</span>
                </li>
            `;
        }
        
        // Add temperature and moisture regimes
        if (componentData.taxtempcl) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Temperature Class:</span>
                    <span class="mapunit-data-value">${componentData.taxtempcl}</span>
                </li>
            `;
        }
        
        if (componentData.taxtempregime) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Temperature Regime:</span>
                    <span class="mapunit-data-value">${componentData.taxtempregime}</span>
                </li>
            `;
        }
        
        if (componentData.taxmoistscl) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Moisture Subclass:</span>
                    <span class="mapunit-data-value">${componentData.taxmoistscl}</span>
                </li>
            `;
        }
        
        // Add taxonomy edition if available
        if (componentData.soiltaxedition) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Soil Taxonomy Edition:</span>
                    <span class="mapunit-data-value">${componentData.soiltaxedition}</span>
                </li>
            `;
        }
        
        html += '</ul>';
        taxonomyContent.innerHTML = html;
    }
    
    // Populate land classification section with component data
    populateLandClassification(componentData) {
        const landClassContent = document.getElementById('land-classification-content');
        if (!landClassContent) return;
        
        // Create HTML for land classification data
        let html = '<ul class="mapunit-data-list">';
        
        // CA Storie Index - check if available
        if (componentData.castorieindex) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">CA Storie Index:</span>
                    <span class="mapunit-data-value">${componentData.castorieindex} <span class="mapunit-data-help">?</span></span>
                </li>
            `;
        } else {
            // Show as placeholder if not available
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">CA Storie Index:</span>
                    <span class="mapunit-data-value">Grade 3 - Fair (42) <span class="mapunit-data-help">?</span></span>
                </li>
            `;
        }
        
        // Land Capability Class (non-irrigated)
        if (componentData.nirrcapcl || componentData.nirrcapscl) {
            const capClass = componentData.nirrcapcl || '';
            const subClass = componentData.nirrcapscl || '';
            const nonIrrigated = capClass + (subClass ? '-' + subClass : '');
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Land Capability Class (non-irrigated):</span>
                    <span class="mapunit-data-value">${nonIrrigated || 'Not specified'} <span class="mapunit-data-help">?</span></span>
                </li>
            `;
        }
        
        // Land Capability Class (irrigated)
        if (componentData.irrcapcl || componentData.irrcapscl) {
            const capClass = componentData.irrcapcl || '';
            const subClass = componentData.irrcapscl || '';
            const irrigated = capClass + (subClass ? '-' + subClass : '');
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Land Capability Class (irrigated):</span>
                    <span class="mapunit-data-value">${irrigated || 'Not specified'} <span class="mapunit-data-help">?</span></span>
                </li>
            `;
        }
        
        // Ecological Site Description - placeholder with link
        html += `
            <li class="mapunit-data-item">
                <span class="mapunit-data-label">Ecological Site Description:</span>
                <span class="mapunit-data-value">
                    <a href="#" style="color: #0066cc; text-decoration: underline;">Clayey Basin Group</a>
                    <span class="mapunit-data-help">?</span>
                </span>
            </li>
        `;
        
        // Forage Suitability Group
        const forageGroup = componentData.foragesuitgrpid || 'n/a';
        html += `
            <li class="mapunit-data-item">
                <span class="mapunit-data-label">Forage Suitability Group:</span>
                <span class="mapunit-data-value">${forageGroup} <span class="mapunit-data-help">?</span></span>
            </li>
        `;
        
        // Organic Carbon Stock - placeholder values
        html += `
            <li class="mapunit-data-item">
                <span class="mapunit-data-label">Organic Carbon Stock:</span>
                <span class="mapunit-data-value">13 [8-16] kg / m² <span class="mapunit-data-help">?</span></span>
            </li>
            <li class="mapunit-data-item">
                <span class="mapunit-data-label">Organic Carbon Stock 0-30cm:</span>
                <span class="mapunit-data-value">5 [3-6] kg / m² <span class="mapunit-data-help">?</span></span>
            </li>
            <li class="mapunit-data-item">
                <span class="mapunit-data-label">Organic Carbon Stock 0-100cm:</span>
                <span class="mapunit-data-value">10 [7-12] kg / m² <span class="mapunit-data-help">?</span></span>
            </li>
        `;
        
        html += '</ul>';
        landClassContent.innerHTML = html;
    }
    
    // Populate hydraulic and erosion ratings section
    populateHydraulicErosion(componentData) {
        const hydraulicContent = document.getElementById('hydraulic-erosion-content');
        if (!hydraulicContent) return;
        
        // Create HTML for hydraulic and erosion data
        let html = '<ul class="mapunit-data-list">';
        
        // Wind Erodibility Group
        if (componentData.weg) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Wind Erodibility Group:</span>
                    <span class="mapunit-data-value">${componentData.weg}
                        <span class="mapunit-data-help" title="A grouping of soils that have similar properties affecting their susceptibility to wind erosion">?</span>
                    </span>
                </li>`;
        }
        
        // Wind Erodibility Index
        if (componentData.wei) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Wind Erodibility Index:</span>
                    <span class="mapunit-data-value">${componentData.wei}
                        <span class="mapunit-data-help" title="The potential for soil loss from wind erosion under different field conditions">?</span>
                    </span>
                </li>`;
        }
        
        // T Erosion Factor
        if (componentData.tfact) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">T Erosion Factor:</span>
                    <span class="mapunit-data-value">${componentData.tfact}
                        <span class="mapunit-data-help" title="Maximum rate of soil erosion by wind or water that can occur without affecting crop productivity">?</span>
                    </span>
                </li>`;
        }
        
        // Runoff
        if (componentData.runoff) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Runoff:</span>
                    <span class="mapunit-data-value">${componentData.runoff}</span>
                </li>`;
        } else {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Runoff:</span>
                    <span class="mapunit-data-value" style="font-style: italic;">Data not available</span>
                </li>`;
        }
        
        // Drainage Class
        if (componentData.drainagecl) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Drainage:</span>
                    <span class="mapunit-data-value">${componentData.drainagecl}</span>
                </li>`;
        }
        
        // Hydric Rating
        if (componentData.hydricrating) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Hydric Rating:</span>
                    <span class="mapunit-data-value">${componentData.hydricrating}
                        <span class="mapunit-data-help" title="Indicates whether the soil is hydric (formed under conditions of saturation, flooding, or ponding)">?</span>
                    </span>
                </li>`;
        }
        
        // Hydrologic Group
        if (componentData.hydgrp) {
            html += `
                <li class="mapunit-data-item">
                    <span class="mapunit-data-label">Hydrologic Group:</span>
                    <span class="mapunit-data-value">Group ${componentData.hydgrp}
                        <span class="mapunit-data-help" title="Grouping of soils based on runoff potential. Group A has low runoff potential, D has high runoff potential">?</span>
                    </span>
                </li>`;
        }
        
        // Parent Material (placeholder)
        html += `
            <li class="mapunit-data-item">
                <span class="mapunit-data-label">Parent Material:</span>
                <span class="mapunit-data-value" style="font-style: italic;">Data not available</span>
            </li>`;
        
        // Total Plant Available Water (placeholder)
        html += `
            <li class="mapunit-data-item">
                <span class="mapunit-data-label">Total Plant Available Water (cm):</span>
                <span class="mapunit-data-value" style="font-style: italic;">Data not available</span>
            </li>`;
        
        html += '</ul>';
        hydraulicContent.innerHTML = html;
    }
    
    // Attach event handlers for detail panel
    attachDetailPanelEventHandlers() {
        // Re-attach close button handler
        const closeBtn = document.getElementById('close-detail-panel');
        if (closeBtn) {
            closeBtn.addEventListener('click', () => this.closeSsurgoDetailPanel());
        }
        
        // Re-attach section toggle handlers
        const sectionHeaders = document.querySelectorAll('.detail-section-header');
        sectionHeaders.forEach(header => {
            header.addEventListener('click', () => {
                const section = header.dataset.section;
                this.toggleDetailSection(section);
            });
        });
    }
    
    // Restore previous panel content
    restorePreviousPanel() {
        const detailPanel = document.getElementById('ssurgo-detail-panel');
        if (detailPanel && this.previousPanelContent) {
            detailPanel.innerHTML = this.previousPanelContent;
            this.attachDetailPanelEventHandlers();
            
            // Reset series view state since we're going back to main view
            this.currentSeriesView = null;
            this.currentSeriesName = null;
            
            // Re-add soil series link handlers
            const seriesLinks = document.querySelectorAll('.soil-series-link');
            seriesLinks.forEach(link => {
                link.addEventListener('click', (e) => {
                    e.preventDefault();
                    const link = e.target.closest('.soil-series-link');
                    const seriesName = link.dataset.series;
                    const componentData = link.dataset.componentData ? JSON.parse(link.dataset.componentData) : null;
                    this.showSoilSeriesDetail(seriesName, null, componentData);
                });
            });
        }
    }
    
    // Generate soil profile chart based on type
    async generateSoilProfileChart(chartType, seriesName, clickLocation = null) {
        const container = document.getElementById('soil-profile-chart');
        
        // Show loading message
        container.innerHTML = '<div style="text-align: center; padding: 50px;"><i class="fas fa-spinner fa-spin"></i> Loading soil data...</div>';
        
        try {
            // Store click location for raster extraction
            if (clickLocation) {
                this.lastClickLocation = clickLocation;
            }
            
            // Fetch OSD data if not already cached
            if (!this.osdDataCache || !this.osdDataCache[seriesName]) {
                await this.fetchOSDData(seriesName);
            }
            
            const osdData = this.osdDataCache[seriesName];
            
            if (osdData === null) {
                container.innerHTML = '<div style="text-align: center; padding: 50px; color: #dc3545; font-weight: 500;">Failed to retrieve soil profile data</div>';
                return;
            }
            
            const horizons = osdData.horizons || [];
            const propertyData = osdData.properties || {};
            
            // For pH and organic matter, try to get raster values if we have a click location
            if ((chartType === 'ph' || chartType === 'org-matter') && this.lastClickLocation) {
                console.log('Getting raster data for:', chartType, 'at location:', this.lastClickLocation);
                const rasterProperty = chartType === 'ph' ? 'ph' : 'oc';
                
                // Check if we need to fetch new raster data
                const locationKey = `${this.lastClickLocation.lat}_${this.lastClickLocation.lng}`;
                if (!this.lastRasterData || this.lastRasterData.locationKey !== locationKey) {
                    // Fetch raster values for both pH and OC at once
                    const phValues = await window.rasterManager?.extractValuesAtLocation(
                        'ph', 
                        this.lastClickLocation.lat, 
                        this.lastClickLocation.lng
                    );
                    const ocValues = await window.rasterManager?.extractValuesAtLocation(
                        'oc', 
                        this.lastClickLocation.lat, 
                        this.lastClickLocation.lng
                    );
                    
                    this.lastRasterData = {
                        locationKey,
                        ph: phValues,
                        oc: ocValues
                    };
                }
                
                const rasterValues = rasterProperty === 'ph' ? this.lastRasterData.ph : this.lastRasterData.oc;
                console.log('Raster values for', chartType, ':', rasterValues);
                
                if (rasterValues) {
                    // Convert raster values to property data format
                    const depthMidpoints = {
                        '0-5cm': 2.5,
                        '5-15cm': 10,
                        '15-30cm': 22.5,
                        '30-60cm': 45,
                        '60-100cm': 80,
                        '100-200cm': 150
                    };
                    
                    const rasterData = [];
                    for (const [depth, value] of Object.entries(rasterValues)) {
                        rasterData.push({
                            depth: depthMidpoints[depth],
                            value: chartType === 'org-matter' ? value / 10 : (chartType === 'ph' ? value / 10 : value) // Convert OC from g/kg to %, pH from units*10 to units
                        });
                    }
                    
                    // Always use raster data for pH and organic matter
                    propertyData[chartType] = rasterData;
                }
            }
            
            console.log('Property data for', chartType, ':', propertyData[chartType]);
            
            if (chartType === 'soil-sketch') {
                this.createSoilSketch(horizons, container);
            } else if (propertyData[chartType] && propertyData[chartType].length > 0) {
                this.createPropertyChart(propertyData[chartType], chartType, container);
            } else {
                // For properties without data, show placeholder
                container.innerHTML = '<div style="text-align: center; padding: 50px; color: #666;">No data available for this property</div>';
            }
        } catch (error) {
            console.error('Error generating soil profile chart:', error);
            container.innerHTML = '<div style="text-align: center; padding: 50px; color: #dc3545;">Error loading soil data</div>';
        }
    }
    
    // Fetch OSD data from SoilWeb API
    async fetchOSDData(seriesName) {
        if (!this.osdDataCache) {
            this.osdDataCache = {};
        }
        
        try {
            // Try multiple approaches to fetch OSD data
            let data = null;
            
            // First try: USDA Web Soil Survey API (should have CORS headers)
            try {
                // Try the official USDA endpoint first
                const wssUrl = `https://SDMDataAccess.sc.egov.usda.gov/Tabular/post.rest`;
                const query = {
                    "format": "JSON",
                    "query": `SELECT TOP 1 compname, comppct_r FROM component WHERE compname = '${seriesName}' AND majcompflag = 'Yes'`
                };
                
                const wssResponse = await fetch(wssUrl, {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/json'
                    },
                    body: JSON.stringify(query)
                });
                
                if (wssResponse.ok) {
                    const wssData = await wssResponse.json();
                    console.log('WSS response:', wssData);
                }
            } catch (wssError) {
                console.log('WSS API call failed:', wssError);
            }
            
            // Second try: SoilWeb API (may fail due to CORS)
            if (!data) {
                try {
                    const baseURL = 'https://casoilresource.lawr.ucdavis.edu/soil_web/reflector_api/soils.php';
                    const params = new URLSearchParams({
                        what: 'osd_query',
                        q_string: seriesName.toLowerCase()
                    });
                    
                    const response = await fetch(`${baseURL}?${params}`, {
                        mode: 'cors'
                    });
                    
                    if (response.ok) {
                        data = await response.json();
                    }
                } catch (corsError) {
                    // Expected CORS error - API doesn't support CORS
                    // Silently continue to CORS proxy method
                }
            }
            
            // Third try: Use a CORS proxy (for development)
            if (!data) {
                try {
                    // Try cors-anywhere alternative
                    const proxyUrl = 'https://corsproxy.io/?';
                    const apiUrl = `https://casoilresource.lawr.ucdavis.edu/soil_web/reflector_api/soils.php?what=osd_query&q_string=${seriesName.toLowerCase()}`;
                    const response = await fetch(proxyUrl + encodeURIComponent(apiUrl));
                    
                    if (response.ok) {
                        const text = await response.text();
                        try {
                            data = JSON.parse(text);
                        } catch (e) {
                            // Try to parse as CSV if not JSON
                            console.log('Response appears to be CSV, attempting to parse...');
                            if (text.includes('hzname,top,bottom')) {
                                data = this.parseOSDCSV(text, seriesName);
                            } else {
                                console.log('Response was not JSON or expected CSV:', text.substring(0, 200));
                            }
                        }
                    }
                } catch (proxyError) {
                    console.log('Proxy API call failed:', proxyError);
                }
            }
            
            if (data) {
                // Parse the OSD data into our format
                const parsedData = this.parseOSDData(data, seriesName);
                this.osdDataCache[seriesName] = parsedData;
            } else {
                // If we can't fetch real data, mark as failed
                console.warn('Unable to fetch OSD data from API, marking as failed');
                this.osdDataCache[seriesName] = null;
            }
            
        } catch (error) {
            console.error('Error fetching OSD data:', error);
            // Store null to indicate failed fetch
            this.osdDataCache[seriesName] = null;
        }
    }
    
    // Parse OSD CSV data
    parseOSDCSV(csvText, seriesName) {
        const lines = csvText.trim().split('\n');
        const headers = lines[0].split(',');
        const horizons = [];
        
        for (let i = 1; i < lines.length; i++) {
            const values = lines[i].split(',');
            const hz = {};
            
            headers.forEach((header, index) => {
                hz[header] = values[index] || '';
            });
            
            if (hz.hzname && hz.top !== undefined && hz.bottom !== undefined) {
                horizons.push({
                    designation: hz.hzname,
                    top: parseFloat(hz.top) || 0,
                    bottom: parseFloat(hz.bottom) || 0,
                    texture_class: hz.texture_class || '',
                    ph: parseFloat(hz.ph) || null,
                    dry_color: `${hz.matrix_dry_color_hue || ''}${hz.matrix_dry_color_value || ''}/${hz.matrix_dry_color_chroma || ''}`,
                    wet_color: `${hz.matrix_wet_color_hue || ''}${hz.matrix_wet_color_value || ''}/${hz.matrix_wet_color_chroma || ''}`
                });
            }
        }
        
        return { horizons };
    }
    
    // Parse OSD data from API response
    parseOSDData(data, seriesName) {
        // If data already has horizons (from CSV parse), use it directly
        if (data && data.horizons) {
            const horizons = [];
            const properties = {
                'org-matter': [],
                'clay': [],
                'sand': [],
                'ph': [],
                'awc': []
            };
            
            data.horizons.forEach((hz, index) => {
                horizons.push({
                    name: hz.designation || hz.hzname || `H${index + 1}`,
                    top: hz.top || index * 30,
                    bottom: hz.bottom || (index + 1) * 30,
                    color: this.getHorizonColor(hz.designation || hz.hzname),
                    texture: hz.texture_class || ''
                });
                
                // Extract properties if available
                const midDepth = (hz.top + hz.bottom) / 2;
                
                if (hz.organic_matter) {
                    properties['org-matter'].push({ depth: midDepth, value: hz.organic_matter });
                }
                if (hz.clay) {
                    properties['clay'].push({ depth: midDepth, value: hz.clay });
                }
                if (hz.sand) {
                    properties['sand'].push({ depth: midDepth, value: hz.sand });
                }
                if (hz.ph) {
                    properties['ph'].push({ depth: midDepth, value: hz.ph });
                }
            });
            
            return { horizons, properties };
        }
        
        // Return empty structure if no data
        return { 
            horizons: [], 
            properties: {
                'org-matter': [],
                'clay': [],
                'sand': [],
                'ph': [],
                'awc': []
            }
        };
    }
    
    // Get appropriate color for horizon designation
    getHorizonColor(designation) {
        if (!designation) return '#8b6647';
        
        const firstChar = designation.charAt(0).toUpperCase();
        const colors = {
            'O': '#2d1810',  // Organic - very dark brown
            'A': '#3d2817',  // Surface - dark brown
            'E': '#d4a574',  // Eluvial - light brown
            'B': '#5c4033',  // Subsoil - medium brown
            'C': '#8b6647',  // Parent material - light brown
            'R': '#696969'   // Bedrock - gray
        };
        
        return colors[firstChar] || '#7a5447';
    }
    
    
    // Create soil horizon sketch
    createSoilSketch(horizons, container) {
        console.log('Creating soil sketch with horizons:', horizons);
        
        // Create shapes for each horizon
        const shapes = horizons.map(h => ({
            type: 'rect',
            xref: 'x',
            yref: 'y',
            x0: 0.75,
            x1: 1.25,
            y0: h.top,
            y1: h.bottom,
            fillcolor: h.color,
            line: {
                color: '#333',
                width: 1
            }
        }));
        
        // Create text annotations for horizon names
        const textAnnotations = horizons.map(h => ({
            x: 1,
            y: (h.top + h.bottom) / 2,
            text: h.name,
            xref: 'x',
            yref: 'y',
            showarrow: false,
            font: {
                color: 'white',
                size: 16
            },
            xanchor: 'center',
            yanchor: 'middle'
        }));
        
        // Empty data for the plot (we're using shapes instead)
        const data = [{
            x: [1],
            y: [0],
            type: 'scatter',
            mode: 'markers',
            marker: { 
                size: 0,
                opacity: 0,
                color: 'rgba(0,0,0,0)'
            },
            showlegend: false,
            hoverinfo: 'none'
        }];
        
        // Add depth labels
        const depthAnnotations = [];
        horizons.forEach(h => {
            depthAnnotations.push({
                x: 1.3,
                y: h.bottom,
                text: h.bottom + '',
                xanchor: 'left',
                showarrow: false,
                font: { size: 12 }
            });
        });
        depthAnnotations.push({
            x: 1.3,
            y: 0,
            text: '0',
            xanchor: 'left',
            showarrow: false,
            font: { size: 12 }
        });
        
        // Combine all annotations
        const annotations = [...textAnnotations, ...depthAnnotations];
        
        const layout = {
            title: 'Soil Horizons',
            shapes: shapes,
            xaxis: {
                range: [0.5, 1.5],
                showticklabels: false,
                showgrid: false
            },
            yaxis: {
                title: 'Depth (cm)',
                autorange: false,
                range: [Math.max(...horizons.map(h => h.bottom)) + 10, -5],
                zeroline: true,
                dtick: 25,
                showgrid: true,
                gridcolor: '#eee',
                side: 'left'
            },
            annotations: annotations,
            showlegend: false,
            height: 400,
            margin: { t: 40, b: 40, l: 60, r: 60 }
        };
        
        const config = {
            responsive: true,
            displayModeBar: false
        };
        
        Plotly.newPlot(container, data, layout, config);
    }
    
    // Create property chart
    createPropertyChart(data, propertyType, container) {
        const propertyInfo = {
            'org-matter': { title: 'Organic Matter (%)', color: '#8B4513' },
            'clay': { title: 'Clay Content (%)', color: '#B87333' },
            'sand': { title: 'Sand Content (%)', color: '#DEB887' },
            'ph': { title: 'Soil pH', color: '#4169E1' },
            'awc': { title: 'Available Water Capacity', color: '#1E90FF' }
        };
        
        const info = propertyInfo[propertyType] || { title: propertyType, color: '#666' };
        
        const trace = {
            x: data.map(d => d.value),
            y: data.map(d => d.depth),
            type: 'scatter',
            mode: 'lines+markers',
            line: {
                color: info.color,
                width: 3,
                shape: 'hv' // Horizontal-vertical step
            },
            marker: {
                color: info.color,
                size: 8
            },
            hovertemplate: 'Depth: %{y} cm<br>Value: %{x}<extra></extra>'
        };
        
        const layout = {
            title: info.title,
            xaxis: {
                title: info.title,
                zeroline: false
            },
            yaxis: {
                title: 'Depth (cm)',
                autorange: false,
                range: [Math.max(...data.map(d => d.depth)) + 10, -5],
                zeroline: true,
                dtick: 25
            },
            showlegend: false,
            height: 400,
            margin: { t: 40, b: 60, l: 60, r: 40 }
        };
        
        const config = {
            responsive: true,
            displayModeBar: false
        };
        
        Plotly.newPlot(container, [trace], layout, config);
    }
}

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { UIController };
}