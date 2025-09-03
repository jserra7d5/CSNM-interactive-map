// Raster Utilities for Soil Data Visualization
// Handles loading and processing of raster data (TIFF files)

class RasterManager {
    constructor() {
        this.rasterCache = new Map();
        this.loadedTiffs = new Map();
        
        // Check for GeoTIFF availability more thoroughly
        this.isGeoTiffAvailable = this.checkGeoTiffAvailability();
    }
    
    checkGeoTiffAvailability() {
        return (typeof GeoTIFF !== 'undefined') || (typeof window.GeoTIFF !== 'undefined');
    }
    
    // Create a real raster layer from TIFF file
    async createTiffLayer(property, depth, options = {}) {
        console.log(`🌍 RASTER: Creating TIFF layer for ${property}, depth ${depth}`);
        
        if (!this.isGeoTiffAvailable) {
            console.warn('GeoTIFF library not available');
            return null;
        }
        
        // Check cache for this specific property-depth combination
        const cacheKey = `${property}_depth_${depth}`;
        if (this.rasterCache.has(cacheKey)) {
            console.log(`🌍 RASTER: Using cached layer for ${cacheKey}`);
            const cached = this.rasterCache.get(cacheKey);
            // Return a copy of the cached layer since Leaflet layers can only be added to one map at a time
            return {
                layer: cached.layer,
                dataRange: cached.dataRange
            };
        }
        
        try {
            const filename = this.getRasterFilename(property, depth);
            console.log(`🌍 RASTER: Loading file: ${filename}`);
            const fallbackFilename = this.getFallbackFilename(property, depth);
            const tiff = await this.loadTiff(filename, fallbackFilename);
            
            if (!tiff) {
                return null;
            }
            
            // Check how many images/bands are available
            const imageCount = await tiff.getImageCount();
            console.log(`🌍 RASTER: TIFF has ${imageCount} images (IFDs)`);
            
            // Get the first image to check for multi-band structure
            const firstImage = await tiff.getImage(0);
            const samplesPerPixel = firstImage.getSamplesPerPixel();
            console.log(`🌍 RASTER: First image has ${samplesPerPixel} samples/bands per pixel`);
            
            // For climate variables, determine which band/sample to use
            let imageIndex = 0;
            let bandIndex = 0; // Band within the image
            const climateVariables = ['meanTemp', 'temperatureMean', 'temperatureMin', 'temperatureMax',
                                    'vpdMin', 'vpdMax', 'solarTotal', 'solarSloped', 'solarClear'];
            
            if (climateVariables.includes(property)) {
                // Get band index from config
                const dataSource = CONFIG.dataSources[property];
                const desiredBand = (dataSource && dataSource.band) ? dataSource.band - 1 : 0; // Convert 1-based to 0-based
                
                // Check if this is a multi-band single image or multiple single-band images
                if (samplesPerPixel > 1) {
                    // Multi-band single image - use band within the image
                    bandIndex = desiredBand;
                    imageIndex = 0;
                    console.log(`🌍 RASTER: Multi-band image detected. Using band ${bandIndex} from image 0`);
                } else if (imageCount > 1) {
                    // Multiple single-band images - use different images
                    imageIndex = Math.min(desiredBand, imageCount - 1);
                    bandIndex = 0;
                    console.log(`🌍 RASTER: Multiple images detected. Using image ${imageIndex}`);
                } else {
                    // Single band, single image
                    console.warn(`🌍 RASTER: Only 1 band available in 1 image. Climate data may not be properly separated.`);
                    imageIndex = 0;
                    bandIndex = 0;
                }
            } else {
                // For depth-specific files, always use the first band of first image
                imageIndex = 0;
                bandIndex = 0;
            }
            
            const image = await tiff.getImage(imageIndex);
            console.log(`🌍 RASTER: Got image ${imageIndex}, will use band ${bandIndex}`);
            
            if (!image) {
                return null;
            }
            
            // Create canvas overlay
            const canvasResult = await this.createCanvasOverlay(image, property, depth, bandIndex);
            
            // Cache the result for this property-depth combination
            if (canvasResult && canvasResult.layer) {
                this.rasterCache.set(cacheKey, canvasResult);
            }
            
            return canvasResult;
            
        } catch (error) {
            console.error(`🌍 RASTER ERROR: Failed to create TIFF layer for ${property}:`, error);
            return null;
        }
    }
    
    // Load TIFF file using GeoTIFF.js with fallback support
    async loadTiff(filename, fallbackFilename = null) {
        if (this.loadedTiffs.has(filename)) {
            return this.loadedTiffs.get(filename);
        }
        
        try {
            // Test if the file is accessible
            const testResponse = await fetch(filename, { method: 'HEAD' });
            
            if (!testResponse.ok) {
                if (fallbackFilename) {
                    return this.loadTiff(fallbackFilename);
                }
                throw new Error(`File not accessible: ${testResponse.status} ${testResponse.statusText} for ${filename}`);
            }
            
            // Get GeoTIFF from global scope
            const GeoTIFFLib = typeof GeoTIFF !== 'undefined' ? GeoTIFF : window.GeoTIFF;
            
            if (!GeoTIFFLib) {
                throw new Error('GeoTIFF library not found');
            }
            
            // Use the reliable method: fetch then fromArrayBuffer
            let tiff = null;
            
            try {
                // Add cache control and range support for better performance
                const response = await fetch(filename, {
                    headers: {
                        'Cache-Control': 'max-age=31536000'
                    }
                });
                
                if (!response.ok) {
                    throw new Error(`Fetch failed: ${response.status}`);
                }
                
                // Check if server supports range requests for future optimization
                const acceptRanges = response.headers.get('Accept-Ranges');
                if (acceptRanges === 'bytes') {
                }
                
                const arrayBuffer = await response.arrayBuffer();
                const sizeInMB = (arrayBuffer.byteLength / 1024 / 1024).toFixed(2);
                
                tiff = await GeoTIFFLib.fromArrayBuffer(arrayBuffer);
            } catch (error) {
                throw error;
            }
            
            if (tiff) {
                this.loadedTiffs.set(filename, tiff);
                
                // Log some TIFF info for debugging
                const imageCount = await tiff.getImageCount();
                
                return tiff;
            } else {
                throw new Error('Failed to load TIFF with any method');
            }
        } catch (error) {
            return null;
        }
    }
    
    // Create canvas overlay from GeoTIFF image
    async createCanvasOverlay(image, property, depth, bandIndex = 0) {
        try {
            const rasters = await image.readRasters();
            
            // Check if rasters is multi-band
            console.log(`🌍 RASTER: Rasters array has ${rasters.length} bands`);
            
            // Select the appropriate band
            const data = rasters[bandIndex] || rasters[0];
            console.log(`🌍 RASTER: Using band index ${bandIndex}, data points: ${data ? data.length : 0}`);
            const bbox = image.getBoundingBox();
            const [width, height] = [image.getWidth(), image.getHeight()];
            // Raster data loaded successfully
        
        // For elevation, also load hillshade data if available
        let hillshadeData = null;
        if (property === 'elevation') {
            try {
                const hillshadeTiff = await this.loadTiff(CONFIG.dataPaths.hillshade);
                if (hillshadeTiff) {
                    const hillshadeImage = await hillshadeTiff.getImage(0);
                    const hillshadeRasters = await hillshadeImage.readRasters();
                    hillshadeData = hillshadeRasters[0];
                    
                    // Debug: Check hillshade value range (simplified to avoid performance issues)
                    if (hillshadeData.length < 100000) {  // Only debug small rasters
                        try {
                            const sampleSize = Math.min(1000, hillshadeData.length);
                            const sampleValues = [];
                            for (let j = 0; j < sampleSize; j += Math.floor(hillshadeData.length / sampleSize)) {
                                if (hillshadeData[j] !== null && !isNaN(hillshadeData[j])) {
                                    sampleValues.push(hillshadeData[j]);
                                }
                            }
                        } catch (debugError) {
                        }
                    }
                }
            } catch (error) {
            }
        }
        
        // Analyze the data to understand value ranges (use sampling for large datasets)
        let validValues;
        const sampleRate = data.length > 1000000 ? Math.floor(data.length / 100000) : 1; // Sample ~100k points for large rasters
        const sampledData = sampleRate > 1 ? data.filter((_, i) => i % sampleRate === 0) : data;
        
        // Climate variables where 0 can be a valid value
        const climateVariables = ['precipitation', 'meanTemp', 'temperatureMean', 'temperatureMin', 'temperatureMax',
                                'vpdMin', 'vpdMax', 'solarTotal', 'solarSloped', 'solarClear'];
        
        if (property === 'nlcd' || property === 'lithology') {
            // For classification rasters, filter out common no-data values including 0
            validValues = sampledData.filter(val => val !== null && !isNaN(val) && val !== -9999 && val !== 255 && val !== 0);
        } else if (property === 'elevation') {
            // For elevation, filter out no-data values (typically very low negative values or specific no-data codes)
            validValues = sampledData.filter(val => val !== null && !isNaN(val) && val !== -9999 && val !== -3.4028235e+38 && val > -1000);
        } else if (climateVariables.includes(property)) {
            // For climate variables, 0 can be valid (e.g., 0°C, 0mm precipitation)
            // Only filter out null, NaN, and standard no-data values
            validValues = sampledData.filter(val => val !== null && !isNaN(val) && val !== -9999 && val !== -3.4028235e+38);
        } else {
            // For other rasters (OC, pH), 0 is typically no-data
            validValues = sampledData.filter(val => val !== null && !isNaN(val) && val !== -9999 && val !== 0);
        }
        // Calculate min/max safely for large arrays
        let min, max, mean;
        if (validValues.length > 0) {
            min = validValues.reduce((acc, val) => Math.min(acc, val), validValues[0]);
            max = validValues.reduce((acc, val) => Math.max(acc, val), validValues[0]);
            mean = validValues.reduce((a, b) => a + b, 0) / validValues.length;
        } else {
            min = max = mean = 0;
        }
        
        // Debug data analysis
        console.log(`🌍 RASTER: ${property} data range: min=${min.toFixed(2)}, max=${max.toFixed(2)}, mean=${mean.toFixed(2)}, valid values: ${validValues.length}`);
        
        // Create canvas with crisp pixel rendering
        const canvas = document.createElement('canvas');
        canvas.width = width;
        canvas.height = height;
        canvas.style.imageRendering = 'pixelated';
        canvas.style.imageRendering = 'crisp-edges';
        canvas.style.imageRendering = '-moz-crisp-edges';
        canvas.style.imageRendering = '-webkit-crisp-edges';
        
        const ctx = canvas.getContext('2d');
        ctx.imageSmoothingEnabled = false;
        
        // Create image data
        const imageData = ctx.createImageData(width, height);
        
        // Color the pixels based on values
        let hillshadeDebugCount = 0;
        let hillshadeStats = { processed: 0, noData: 0, clamped: 0, validRange: 0 };
        const totalPixels = data.length;
        let lastProgressReport = 0;
        
        // Track unique values for classification rasters
        const uniqueValues = new Set();
        
        
        // Emit start event
        const startEvent = new CustomEvent('rasterProcessingProgress', {
            detail: {
                property: property,
                progress: 0,
                message: `Starting to process ${property}...`
            }
        });
        document.dispatchEvent(startEvent);
        
        // Process in chunks to allow UI updates
        const chunkSize = Math.floor(totalPixels / 20); // 5% chunks
        
        for (let chunk = 0; chunk < 20; chunk++) {
            const startIdx = chunk * chunkSize;
            const endIdx = Math.min((chunk + 1) * chunkSize, data.length);
            
            // Process this chunk
            for (let i = startIdx; i < endIdx; i++) {
                const value = data[i];
                
                // Check for no-data values
                let isNoData;
                if (property === 'nlcd' || property === 'lithology') {
                    // For classification rasters, common no-data values are 0, 255, and -9999
                    isNoData = value === null || isNaN(value) || value === -9999 || value === 255 || value === 0;
                } else if (property === 'elevation') {
                    // For elevation, check for various no-data representations
                    isNoData = value === null || isNaN(value) || value === -9999 || value === -3.4028235e+38 || value < -1000;
                } else if (climateVariables.includes(property)) {
                    // For climate variables, 0 can be valid
                    isNoData = value === null || isNaN(value) || value === -9999 || value === -3.4028235e+38;
                } else {
                    // For other rasters (OC, pH), 0 is typically no-data
                    isNoData = value === null || isNaN(value) || value === -9999 || value === 0;
                }
                
                if (isNoData) {
                    // Transparent for no-data
                    const pixelIndex = i * 4;
                    imageData.data[pixelIndex] = 0;     // Red
                    imageData.data[pixelIndex + 1] = 0; // Green
                    imageData.data[pixelIndex + 2] = 0; // Blue
                    imageData.data[pixelIndex + 3] = 0; // Alpha (fully transparent)
                } else {
                    // Track unique values for classification rasters
                    if (property === 'nlcd' || property === 'lithology') {
                        uniqueValues.add(value);
                    }
                    
                    const color = this.getColorForValue(property, value, min, max);
                    const rgb = this.hexToRgb(color);
                    
                    // For elevation with hillshade, blend the colors
                    if (property === 'elevation' && hillshadeData && hillshadeData[i] !== undefined) {
                        const hillshadeValue = hillshadeData[i];
                        
                        // More comprehensive no-data detection for hillshade based on research
                        // Hillshade should be 0-255 grayscale, where 0=darkest and 255=brightest
                        // Only truly invalid values should be considered no-data
                        const hillshadeIsNoData = hillshadeValue === null || 
                                                hillshadeValue === undefined || 
                                                isNaN(hillshadeValue) || 
                                                hillshadeValue === 256 ||        // 16-bit promoted NoData
                                                hillshadeValue === -1 ||         // Negative NoData
                                                hillshadeValue === -9999 ||      // Standard NoData
                                                hillshadeValue === -3.4028235e+38 || // Float32 NoData
                                                hillshadeValue < 0 ||            // Any negative value
                                                hillshadeValue > 255;            // Any value above 8-bit range
                        
                        // Clamp valid hillshade values to 0-255 range to prevent display artifacts
                        let clampedHillshade = hillshadeValue;
                        if (!hillshadeIsNoData) {
                            clampedHillshade = Math.max(0, Math.min(255, hillshadeValue));
                        }
                        
                        // Track hillshade statistics
                        hillshadeStats.processed++;
                        if (hillshadeIsNoData) {
                            hillshadeStats.noData++;
                        } else if (clampedHillshade !== hillshadeValue) {
                            hillshadeStats.clamped++;
                        } else {
                            hillshadeStats.validRange++;
                        }
                        
                        // Debug: log only a few sample pixels to avoid performance issues
                        if (hillshadeDebugCount < 5 && i % 1000000 === 0) {
                            hillshadeDebugCount++;
                        }
                        
                        if (!hillshadeIsNoData) {
                            const blendedRgb = this.blendWithHillshade(rgb, clampedHillshade);
                            
                            const pixelIndex = i * 4;
                            imageData.data[pixelIndex] = blendedRgb.r;     // Red
                            imageData.data[pixelIndex + 1] = blendedRgb.g; // Green
                            imageData.data[pixelIndex + 2] = blendedRgb.b; // Blue
                            imageData.data[pixelIndex + 3] = 230;          // Higher opacity (0.9 * 255 = 230)
                        } else {
                            // If hillshade is no-data, make this pixel transparent even if elevation is valid
                            const pixelIndex = i * 4;
                            imageData.data[pixelIndex] = 0;     // Red
                            imageData.data[pixelIndex + 1] = 0; // Green
                            imageData.data[pixelIndex + 2] = 0; // Blue
                            imageData.data[pixelIndex + 3] = 0; // Alpha (fully transparent)
                        }
                    } else {
                        // Not elevation, or no hillshade data - use regular coloring
                        
                        const pixelIndex = i * 4;
                        imageData.data[pixelIndex] = rgb.r;     // Red
                        imageData.data[pixelIndex + 1] = rgb.g; // Green
                        imageData.data[pixelIndex + 2] = rgb.b; // Blue
                        imageData.data[pixelIndex + 3] = 220;   // Alpha (higher opacity)
                    }
                }
            }
            
            // After each chunk, update progress and yield to browser
            const progress = Math.min(95, (chunk + 1) * 5); // Cap at 95%
            
            // Emit progress event
            const event = new CustomEvent('rasterProcessingProgress', {
                detail: {
                    property: property,
                    progress: progress,
                    message: `Processing ${property}: ${progress}% complete...`
                }
            });
            document.dispatchEvent(event);
            
            // Update progress bar directly
            const progressFill = document.querySelector('.loading-progress-fill');
            const progressText = document.querySelector('.loading-progress-text');
            if (progressFill) {
                progressFill.style.width = `${progress}%`;
            }
            if (progressText) {
                progressText.textContent = `${progress}%`;
            }
            
            // Yield control to browser
            await new Promise(resolve => setTimeout(resolve, 50));
        }
        
        // Put image data on canvas
        ctx.putImageData(imageData, 0, 0);
        
        // Log hillshade statistics if elevation was processed
        if (property === 'elevation' && hillshadeStats.processed > 0) {
        }
        
        // Emit completion event with a small delay to ensure the 100% is visible
        setTimeout(() => {
            const completeEvent = new CustomEvent('rasterProcessingProgress', {
                detail: {
                    property: property,
                    progress: 100,
                    message: `${property} processing complete!`
                }
            });
            document.dispatchEvent(completeEvent);
            
            // Force final UI update
            const progressFill = document.querySelector('.loading-progress-fill');
            const progressText = document.querySelector('.loading-progress-text');
            if (progressFill) progressFill.style.width = '100%';
            if (progressText) progressText.textContent = '100%';
        }, 100); // Small delay to ensure 100% is visible before hiding
        
        // Create Leaflet canvas overlay
        const bounds = [
            [bbox[1], bbox[0]], // SW corner
            [bbox[3], bbox[2]]  // NE corner
        ];
        
        const overlay = L.imageOverlay(canvas.toDataURL(), bounds, {
            opacity: 0.9,
            interactive: true,
            className: 'crisp-raster'
        });
        
        // Add click handler for raster values
        overlay.on('click', (e) => {
            this.handleRasterClick(e, image, property, depth);
        });
        
        // Return both the layer and data range information
        return {
            layer: overlay,
            dataRange: { min, max, mean },
            uniqueValues: uniqueValues // Include unique values for classification rasters
        };
        } catch (error) {
            return null;
        }
    }
    
    // Handle click on raster to show value
    async handleRasterClick(e, image, property, depth) {
        try {
            const { lat, lng } = e.latlng;
            const pixel = image.getPixelForLocation(lng, lat);
            
            if (pixel && pixel.length >= 2) {
                const rasters = await image.readRasters({
                    window: [pixel[0], pixel[1], pixel[0] + 1, pixel[1] + 1]
                });
                
                const value = rasters[0][0];
                const depthLabel = CONFIG.depthLevels.labels[depth];
                
                // Format the value appropriately
                let displayValue = 'No Data';
                if (value !== null && !isNaN(value)) {
                    if (property === 'nlcd') {
                        const className = ConfigUtils.getNLCDName(value);
                        displayValue = `${className} (${value})`;
                    } else if (property === 'lithology') {
                        const className = ConfigUtils.getLithologyName(value);
                        displayValue = `${className} (${value})`;
                    } else if (property === 'elevation') {
                        displayValue = `${Math.round(value)} m`;
                    } else if (property === 'ph' && value > 10) {
                        // pH values appear to be scaled by 10
                        displayValue = `${(value / 10).toFixed(1)} (raw: ${value})`;
                    } else {
                        displayValue = value.toFixed(2);
                    }
                }
                
                const popupTitle = property === 'nlcd' ? 'Land Cover' :
                                 property === 'lithology' ? 'Parent Material' :
                                 property === 'elevation' ? 'Elevation' : 
                                 `${property.toUpperCase()} Value`;
                const depthInfo = (property === 'nlcd' || property === 'lithology' || property === 'elevation') ? '' : `<p><strong>Depth:</strong> ${depthLabel}</p>`;
                
                L.popup()
                    .setLatLng([lat, lng])
                    .setContent(`
                        <div class="raster-cell-popup">
                            <h5>${popupTitle}</h5>
                            ${depthInfo}
                            <p><strong>Value:</strong> ${displayValue} ${this.getUnits(property)}</p>
                            <p><strong>Location:</strong> ${lat.toFixed(4)}, ${lng.toFixed(4)}</p>
                        </div>
                    `)
                    .openOn(e.target._map);
            }
        } catch (error) {
        }
    }
    
    // Convert hex color to RGB
    hexToRgb(hex) {
        if (hex.startsWith('rgb')) {
            const matches = hex.match(/rgb\((\d+),\s*(\d+),\s*(\d+)\)/);
            return matches ? {
                r: parseInt(matches[1]),
                g: parseInt(matches[2]),
                b: parseInt(matches[3])
            } : { r: 128, g: 128, b: 128 };
        }
        
        const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
        return result ? {
            r: parseInt(result[1], 16),
            g: parseInt(result[2], 16),
            b: parseInt(result[3], 16)
        } : { r: 128, g: 128, b: 128 };
    }
    
    // Blend elevation color with hillshade for enhanced 3D visualization
    blendWithHillshade(elevationRgb, hillshadeValue) {
        // Normalize hillshade value (typically 0-255) to 0-1
        const hillshadeNormalized = Math.max(0, Math.min(1, hillshadeValue / 255));
        
        // Create a shading factor: 0.5 = neutral, <0.5 = darker (shadows), >0.5 = lighter (highlights)
        const shadingFactor = 0.3 + (hillshadeNormalized * 0.4); // Range: 0.3 to 0.7
        
        // Apply shading to the elevation color
        const blendedR = Math.round(Math.max(0, Math.min(255, elevationRgb.r * (0.7 + shadingFactor * 0.6))));
        const blendedG = Math.round(Math.max(0, Math.min(255, elevationRgb.g * (0.7 + shadingFactor * 0.6))));
        const blendedB = Math.round(Math.max(0, Math.min(255, elevationRgb.b * (0.7 + shadingFactor * 0.6))));
        
        return {
            r: blendedR,
            g: blendedG,
            b: blendedB
        };
    }
    
    // Get raster filename for a property and depth
    getRasterFilename(property, depth = 0) {
        // Classification rasters and elevation don't have depth levels
        if (property === 'nlcd') {
            return CONFIG.dataPaths.nlcd;
        }
        if (property === 'lithology') {
            return CONFIG.dataPaths.lithology;
        }
        if (property === 'elevation') {
            return CONFIG.dataPaths.elevation;
        }
        
        // Handle precipitation separately from other climate variables
        if (property === 'precipitation') {
            return CONFIG.dataPaths.precipitationAnnual;
        }
        
        // Other climate normal variables - all in one multi-band file
        const climateVariables = ['meanTemp', 'temperatureMean', 'temperatureMin', 'temperatureMax',
                                'vpdMin', 'vpdMax', 'solarTotal', 'solarSloped', 'solarClear'];
        if (climateVariables.includes(property)) {
            return CONFIG.dataPaths.climateNormals;
        }
        
        // Map depth indices to filename patterns
        const depthMappings = {
            0: '0_5cm',     // 0-5 cm
            1: '5_15cm',    // 5-15 cm  
            2: '15_30cm',   // 15-30 cm
            3: '30_60cm',   // 30-60 cm
            4: '60_100cm',  // 60-100 cm
            5: '100_200cm'  // 100-200 cm
        };
        
        const depthSuffix = depthMappings[depth] || '0_5cm';
        
        if (property === 'oc') {
            return `data/rasters/oc/CSNM_OC_${depthSuffix}.tif`;
        } else if (property === 'ph') {
            return `data/rasters/ph/CSNM_pH_${depthSuffix}.tif`;
        } else if (property === 'meanTemp') {
            // Mean temperature now uses the same climate normals file as temperatureMean
            return CONFIG.dataPaths.climateNormals;
        }
        
        // No valid fallback - properties should be handled above
        return null;
    }
    
    // Get fallback filename for pH (handles dash vs underscore naming)
    getFallbackFilename(property, depth = 0) {
        if (property === 'ph' && depth === 0) {
            // For pH 0-5cm, try the dash version as fallback
            return `data/rasters/ph/CSNM_pH_0-5cm.tif`;
        }
        return null;
    }
    
    // Extract raster values at specific coordinates for all depths
    async extractValuesAtLocation(property, lat, lng) {
        if (!this.isGeoTiffAvailable) {
            return null;
        }
        
        const values = {};
        const depths = [0, 1, 2, 3, 4, 5]; // All depth indices
        
        for (const depth of depths) {
            try {
                const filename = this.getRasterFilename(property, depth);
                const fallbackFilename = this.getFallbackFilename(property, depth);
                
                // Load TIFF if not already loaded
                let tiff = this.loadedTiffs.get(filename);
                if (!tiff) {
                    tiff = await this.loadTiff(filename, fallbackFilename);
                    if (!tiff) continue;
                }
                
                // Get the first image/band
                const image = await tiff.getImage(0);
                const bbox = image.getBoundingBox();
                const [width, height] = [image.getWidth(), image.getHeight()];
                
                // Convert lat/lng to pixel coordinates
                const x = Math.floor((lng - bbox[0]) / (bbox[2] - bbox[0]) * width);
                const y = Math.floor((bbox[3] - lat) / (bbox[3] - bbox[1]) * height);
                
                // Check if coordinates are within bounds
                if (x >= 0 && x < width && y >= 0 && y < height) {
                    // Read a single pixel value
                    const window = [x, y, x + 1, y + 1];
                    const rasters = await image.readRasters({ window });
                    const value = rasters[0][0]; // First band, first pixel
                    
                    if (value !== null && !isNaN(value) && value !== -9999) {
                        // Map depth index to depth range string
                        const depthLabels = ['0-5cm', '5-15cm', '15-30cm', '30-60cm', '60-100cm', '100-200cm'];
                        values[depthLabels[depth]] = value;
                    }
                }
            } catch (error) {
            }
        }
        
        return Object.keys(values).length > 0 ? values : null;
    }
    
    // Create a tile layer from a raster file (fallback method for tile servers)
    createTileLayer(property, depth, options = {}) {
        // This is kept for compatibility but not used when TIFF loading works
        return null;
    }
    
    // Create a mock raster overlay for demonstration
    createMockRasterOverlay(property, depth, bounds) {
        const depthLabel = CONFIG.depthLevels.labels[depth];
        
        // Generate mock data points within bounds
        const mockData = this.generateMockRasterData(property, depth, bounds);
        
        // Create a layer group with multiple overlays to simulate raster data
        const rasterGroup = L.layerGroup();
        
        // Create a grid of colored rectangles to simulate raster pixels
        const gridSize = 0.02; // degrees
        const [sw, ne] = bounds;
        
        for (let lat = sw[0]; lat < ne[0]; lat += gridSize) {
            for (let lng = sw[1]; lng < ne[1]; lng += gridSize) {
                const cellBounds = [
                    [lat, lng],
                    [lat + gridSize, lng + gridSize]
                ];
                
                const value = this.getMockRasterValue(property, depth, lat, lng);
                const color = this.getColorForValue(property, value);
                
                const cell = L.rectangle(cellBounds, {
                    color: color,
                    fillColor: color,
                    fillOpacity: 0.6,
                    weight: 0,
                    opacity: 0
                });
                
                // Add value popup
                cell.bindPopup(`
                    <div class="raster-cell-popup">
                        <h5>${property.toUpperCase()} Value</h5>
                        <p><strong>Depth:</strong> ${depthLabel}</p>
                        <p><strong>Value:</strong> ${value.toFixed(2)} ${this.getUnits(property)}</p>
                        <p><strong>Location:</strong> ${lat.toFixed(4)}, ${lng.toFixed(4)}</p>
                    </div>
                `);
                
                rasterGroup.addLayer(cell);
            }
        }
        
        return rasterGroup;
    }
    
    // Generate mock raster value for a location
    getMockRasterValue(property, depth, lat, lng) {
        // Create some variation based on location and depth
        const baseLat = 42.1;
        const baseLng = -122.466;
        
        const latOffset = (lat - baseLat) * 100;
        const lngOffset = (lng - baseLng) * 100;
        const depthFactor = depth * 0.1;
        
        if (property === 'oc') {
            // Organic carbon: higher values at surface, varies by location
            const baseValue = 15 - (depth * 2);
            const variation = Math.sin(latOffset) * Math.cos(lngOffset) * 5;
            return Math.max(0.5, baseValue + variation);
        } else if (property === 'ph') {
            // pH: slightly increases with depth, varies by location
            const baseValue = 6.2 + (depth * 0.1);
            const variation = Math.sin(latOffset * 0.5) * Math.cos(lngOffset * 0.5) * 0.8;
            return Math.max(4.0, Math.min(8.5, baseValue + variation));
        }
        
        return 0;
    }
    
    // Get color for a raster value using actual data ranges
    getColorForValue(property, value, min, max) {
        if (property === 'nlcd') {
            // NLCD uses discrete classification values
            return ConfigUtils.getNLCDColor(value);
        } else if (property === 'lithology') {
            // Lithology uses discrete classification values
            return ConfigUtils.getLithologyColor(value);
        } else if (property === 'elevation') {
            // Elevation uses continuous color scale
            const normalized = Math.min(Math.max((value - min) / (max - min), 0), 1);
            return ConfigUtils.getElevationColor(normalized);
        } else if (property === 'oc') {
            // Enhanced organic carbon color scale with better contrast
            // Normalize value between min and max
            const normalized = Math.min(Math.max((value - min) / (max - min), 0), 1);
            
            // Create enhanced color scale: light yellow/cream to dark brown with better contrast
            if (normalized < 0.2) {
                // Very low values: light cream/yellow
                const intensity = normalized / 0.2;
                const r = Math.floor(255 - (55 * intensity));  // 255 to 200
                const g = Math.floor(255 - (55 * intensity));  // 255 to 200  
                const b = Math.floor(220 - (120 * intensity)); // 220 to 100
                return `rgb(${r}, ${g}, ${b})`;
            } else if (normalized < 0.5) {
                // Low-medium values: orange/light brown
                const intensity = (normalized - 0.2) / 0.3;
                const r = Math.floor(200 - (50 * intensity));  // 200 to 150
                const g = Math.floor(200 - (100 * intensity)); // 200 to 100
                const b = Math.floor(100 - (70 * intensity));  // 100 to 30
                return `rgb(${r}, ${g}, ${b})`;
            } else {
                // High values: dark brown to very dark brown
                const intensity = (normalized - 0.5) / 0.5;
                const r = Math.floor(150 - (90 * intensity));  // 150 to 60
                const g = Math.floor(100 - (70 * intensity));  // 100 to 30
                const b = Math.floor(30 - (20 * intensity));   // 30 to 10
                return `rgb(${r}, ${g}, ${b})`;
            }
        } else if (property === 'ph') {
            // pH color scale: Use the actual data range instead of assuming 4-8.5
            const range = max - min;
            const normalized = (value - min) / range; // 0 to 1
            
            if (normalized < 0.33) {
                // Lower third: red (acidic)
                const intensity = normalized / 0.33;
                return `rgb(${Math.floor(200 + 55 * (1 - intensity))}, ${Math.floor(50 * intensity)}, 50)`;
            } else if (normalized > 0.67) {
                // Upper third: blue (basic)
                const intensity = (normalized - 0.67) / 0.33;
                return `rgb(50, ${Math.floor(50 * (1 - intensity))}, ${Math.floor(100 + 155 * intensity)})`;
            } else {
                // Middle third: green (neutral)
                const intensity = Math.abs(normalized - 0.5) / 0.17; // Distance from center
                return `rgb(50, ${Math.floor(150 + 105 * (1 - intensity))}, 50)`;
            }
        } else if (property === 'meanTemp' || property === 'temperatureMean' || property === 'temperatureMin' || property === 'temperatureMax') {
            // Temperature uses blue (cold) to red (hot) color scheme
            const climateColors = CONFIG.climateColors;
            const tempConfig = (property === 'temperatureMin' || property === 'temperatureMax') ? 
                              climateColors.temperatureExtreme : climateColors.temperature;
            
            // Use configured min/max or fall back to actual data range
            const configMin = tempConfig.min;
            const configMax = tempConfig.max;
            const normalizedValue = (value - configMin) / (configMax - configMin);
            const clampedValue = Math.max(0, Math.min(1, normalizedValue));
            
            // Interpolate through color array
            const colors = tempConfig.colors;
            const colorIndex = clampedValue * (colors.length - 1);
            const lowerIndex = Math.floor(colorIndex);
            const upperIndex = Math.ceil(colorIndex);
            const fraction = colorIndex - lowerIndex;
            
            if (lowerIndex === upperIndex) {
                return colors[lowerIndex];
            }
            
            // Interpolate between two colors
            const lowerColor = this.hexToRgb(colors[lowerIndex]);
            const upperColor = this.hexToRgb(colors[upperIndex]);
            
            const r = Math.round(lowerColor.r + (upperColor.r - lowerColor.r) * fraction);
            const g = Math.round(lowerColor.g + (upperColor.g - lowerColor.g) * fraction);
            const b = Math.round(lowerColor.b + (upperColor.b - lowerColor.b) * fraction);
            
            return `rgb(${r}, ${g}, ${b})`;
        } else if (property === 'precipitation') {
            // Precipitation uses brown (dry) to blue (wet) color scheme
            const precipConfig = CONFIG.climateColors.precipitation;
            const configMin = precipConfig.min;
            const configMax = precipConfig.max;
            const normalizedValue = (value - configMin) / (configMax - configMin);
            const clampedValue = Math.max(0, Math.min(1, normalizedValue));
            
            const colors = precipConfig.colors;
            const colorIndex = clampedValue * (colors.length - 1);
            const lowerIndex = Math.floor(colorIndex);
            const upperIndex = Math.ceil(colorIndex);
            const fraction = colorIndex - lowerIndex;
            
            if (lowerIndex === upperIndex) {
                return colors[lowerIndex];
            }
            
            const lowerColor = this.hexToRgb(colors[lowerIndex]);
            const upperColor = this.hexToRgb(colors[upperIndex]);
            
            const r = Math.round(lowerColor.r + (upperColor.r - lowerColor.r) * fraction);
            const g = Math.round(lowerColor.g + (upperColor.g - lowerColor.g) * fraction);
            const b = Math.round(lowerColor.b + (upperColor.b - lowerColor.b) * fraction);
            
            return `rgb(${r}, ${g}, ${b})`;
        } else if (property === 'vpdMin') {
            // Min VPD uses specialized low-range color scheme (0.7-3.2 hPa)
            const vpdConfig = CONFIG.climateColors.vpdMin;
            const configMin = vpdConfig.min;
            const configMax = vpdConfig.max;
            const normalizedValue = (value - configMin) / (configMax - configMin);
            const clampedValue = Math.max(0, Math.min(1, normalizedValue));
            
            const colors = vpdConfig.colors;
            const colorIndex = clampedValue * (colors.length - 1);
            const lowerIndex = Math.floor(colorIndex);
            const upperIndex = Math.ceil(colorIndex);
            const fraction = colorIndex - lowerIndex;
            
            if (lowerIndex === upperIndex) {
                return colors[lowerIndex];
            }
            
            const lowerColor = this.hexToRgb(colors[lowerIndex]);
            const upperColor = this.hexToRgb(colors[upperIndex]);
            
            const r = Math.round(lowerColor.r + (upperColor.r - lowerColor.r) * fraction);
            const g = Math.round(lowerColor.g + (upperColor.g - lowerColor.g) * fraction);
            const b = Math.round(lowerColor.b + (upperColor.b - lowerColor.b) * fraction);
            
            return `rgb(${r}, ${g}, ${b})`;
        } else if (property === 'vpdMax') {
            // Max VPD uses generic VPD color scheme (0-30 hPa)
            const vpdConfig = CONFIG.climateColors.vpd;
            const configMin = vpdConfig.min;
            const configMax = vpdConfig.max;
            const normalizedValue = (value - configMin) / (configMax - configMin);
            const clampedValue = Math.max(0, Math.min(1, normalizedValue));
            
            const colors = vpdConfig.colors;
            const colorIndex = clampedValue * (colors.length - 1);
            const lowerIndex = Math.floor(colorIndex);
            const upperIndex = Math.ceil(colorIndex);
            const fraction = colorIndex - lowerIndex;
            
            if (lowerIndex === upperIndex) {
                return colors[lowerIndex];
            }
            
            const lowerColor = this.hexToRgb(colors[lowerIndex]);
            const upperColor = this.hexToRgb(colors[upperIndex]);
            
            const r = Math.round(lowerColor.r + (upperColor.r - lowerColor.r) * fraction);
            const g = Math.round(lowerColor.g + (upperColor.g - lowerColor.g) * fraction);
            const b = Math.round(lowerColor.b + (upperColor.b - lowerColor.b) * fraction);
            
            return `rgb(${r}, ${g}, ${b})`;
        } else if (property === 'solarTotal' || property === 'solarSloped' || property === 'solarClear') {
            // Solar uses purple (low) to yellow/red (high) color scheme
            const solarConfig = CONFIG.climateColors.solar;
            const configMin = solarConfig.min;
            const configMax = solarConfig.max;
            const normalizedValue = (value - configMin) / (configMax - configMin);
            const clampedValue = Math.max(0, Math.min(1, normalizedValue));
            
            const colors = solarConfig.colors;
            const colorIndex = clampedValue * (colors.length - 1);
            const lowerIndex = Math.floor(colorIndex);
            const upperIndex = Math.ceil(colorIndex);
            const fraction = colorIndex - lowerIndex;
            
            if (lowerIndex === upperIndex) {
                return colors[lowerIndex];
            }
            
            const lowerColor = this.hexToRgb(colors[lowerIndex]);
            const upperColor = this.hexToRgb(colors[upperIndex]);
            
            const r = Math.round(lowerColor.r + (upperColor.r - lowerColor.r) * fraction);
            const g = Math.round(lowerColor.g + (upperColor.g - lowerColor.g) * fraction);
            const b = Math.round(lowerColor.b + (upperColor.b - lowerColor.b) * fraction);
            
            return `rgb(${r}, ${g}, ${b})`;
        }
        
        return '#888888';
    }
    
    // Get units for a property
    getUnits(property) {
        const units = {
            'oc': 'g/kg',
            'ph': 'pH units',
            'meanTemp': '°C',
            'landcover': 'class',
            'elevation': 'meters',
            'prism-temp': '°C',
            'prism-precip': 'mm',
            'precipitation': 'mm',
            'temperatureMean': '°C',
            'temperatureMin': '°C',
            'temperatureMax': '°C',
            'vpdMin': 'hPa',
            'vpdMax': 'hPa',
            'solarTotal': 'MJ/m²/day',
            'solarSloped': 'MJ/m²/day',
            'solarClear': 'MJ/m²/day'
        };
        return units[property] || '';
    }
    
    // Process PRISM raster data
    async processPRISMRaster(data, variable, monthIndex) {
        if (!data || !data.data) {
            console.error('Invalid PRISM data provided');
            return null;
        }
        
        // Determine color scheme based on variable type
        const colorScheme = this.getPRISMColorScheme(variable);
        
        // Calculate statistics
        const stats = this.calculateRasterStats(data.data);
        
        // Create canvas for rendering
        const canvas = document.createElement('canvas');
        canvas.width = data.width;
        canvas.height = data.height;
        const ctx = canvas.getContext('2d');
        const imageData = ctx.createImageData(canvas.width, canvas.height);
        
        // Apply color mapping
        for (let i = 0; i < data.data.length; i++) {
            const value = data.data[i];
            const pixelIndex = i * 4;
            
            if (isNaN(value)) {
                // Transparent for no-data values
                imageData.data[pixelIndex] = 0;
                imageData.data[pixelIndex + 1] = 0;
                imageData.data[pixelIndex + 2] = 0;
                imageData.data[pixelIndex + 3] = 0;
            } else {
                const color = this.getPRISMColor(value, variable, stats.min, stats.max);
                imageData.data[pixelIndex] = color.r;
                imageData.data[pixelIndex + 1] = color.g;
                imageData.data[pixelIndex + 2] = color.b;
                imageData.data[pixelIndex + 3] = 255;
            }
        }
        
        ctx.putImageData(imageData, 0, 0);
        
        return {
            canvas,
            stats,
            variable,
            monthIndex,
            bounds: data.bbox
        };
    }
    
    // Get PRISM color scheme
    getPRISMColorScheme(variable) {
        const schemes = {
            'tmean': 'temperature',
            'tmin': 'temperature',
            'tmax': 'temperature',
            'ppt': 'precipitation'
        };
        
        return schemes[variable] || 'temperature';
    }
    
    // Get color for PRISM value
    getPRISMColor(value, variable, min, max) {
        const scheme = this.getPRISMColorScheme(variable);
        
        if (scheme === 'temperature') {
            // Blue to white to red (-10°C to 40°C)
            const tempMin = Math.max(min, -10);
            const tempMax = Math.min(max, 40);
            const normalized = (value - tempMin) / (tempMax - tempMin);
            
            if (normalized < 0.5) {
                // Blue to white
                const intensity = normalized * 2;
                return {
                    r: Math.floor(0 + 255 * intensity),
                    g: Math.floor(0 + 255 * intensity),
                    b: 255
                };
            } else {
                // White to red
                const intensity = (normalized - 0.5) * 2;
                return {
                    r: 255,
                    g: Math.floor(255 - 255 * intensity),
                    b: Math.floor(255 - 255 * intensity)
                };
            }
        } else if (scheme === 'precipitation') {
            // Brown to white to blue (0 to max mm)
            const precMin = 0;
            const precMax = Math.min(max, 500);
            const normalized = (value - precMin) / (precMax - precMin);
            
            if (normalized < 0.5) {
                // Brown to white
                const intensity = normalized * 2;
                return {
                    r: Math.floor(139 + 116 * intensity),
                    g: Math.floor(69 + 186 * intensity),
                    b: Math.floor(19 + 236 * intensity)
                };
            } else {
                // White to blue
                const intensity = (normalized - 0.5) * 2;
                return {
                    r: Math.floor(255 - 255 * intensity),
                    g: Math.floor(255 - 255 * intensity),
                    b: 255
                };
            }
        }
        
        // Default gray
        return { r: 128, g: 128, b: 128 };
    }
    
    // Calculate raster statistics
    calculateRasterStats(data) {
        let min = Infinity;
        let max = -Infinity;
        let sum = 0;
        let count = 0;
        
        for (let i = 0; i < data.length; i++) {
            const value = data[i];
            if (!isNaN(value)) {
                min = Math.min(min, value);
                max = Math.max(max, value);
                sum += value;
                count++;
            }
        }
        
        const mean = count > 0 ? sum / count : 0;
        
        // Calculate standard deviation
        let sumSquaredDiff = 0;
        for (let i = 0; i < data.length; i++) {
            const value = data[i];
            if (!isNaN(value)) {
                sumSquaredDiff += Math.pow(value - mean, 2);
            }
        }
        
        const stdDev = count > 0 ? Math.sqrt(sumSquaredDiff / count) : 0;
        
        return { min, max, mean, stdDev, count };
    }
    
    // Create PRISM layer for Leaflet
    async createPRISMLayer(data, variable, monthIndex) {
        const processed = await this.processPRISMRaster(data, variable, monthIndex);
        if (!processed) {
            return null;
        }
        
        // Convert canvas to data URL
        const dataUrl = processed.canvas.toDataURL();
        
        // Create Leaflet image overlay
        const bounds = [
            [processed.bounds[1], processed.bounds[0]], // SW corner
            [processed.bounds[3], processed.bounds[2]]  // NE corner
        ];
        
        const layer = L.imageOverlay(dataUrl, bounds, {
            opacity: 0.8,
            interactive: true
        });
        
        // Add metadata to layer
        layer.prismData = {
            variable,
            monthIndex,
            stats: processed.stats
        };
        
        return layer;
    }
    
    // Generate time series animation frames
    async generateTimeSeriesFrames(monthlyData, variable) {
        const frames = [];
        
        for (let i = 0; i < monthlyData.length; i++) {
            const frame = await this.createPRISMLayer(
                monthlyData[i].data,
                variable,
                i
            );
            
            if (frame) {
                frames.push({
                    layer: frame,
                    date: monthlyData[i].date,
                    year: monthlyData[i].year,
                    month: monthlyData[i].month
                });
            }
        }
        
        return frames;
    }
    
    // Generate mock raster data for demonstration
    generateMockRasterData(property, depth, bounds) {
        // This would load actual raster data in a real implementation
        return {
            property: property,
            depth: depth,
            bounds: bounds,
            generated: new Date().toISOString()
        };
    }
    
    // Check if raster data is available for a property
    isRasterAvailable(property) {
        const availableRasters = ['oc', 'ph', 'meanTemp', 'landcover', 'elevation', 'nlcd', 'lithology'];
        return availableRasters.includes(property);
    }
    
    // Get raster metadata
    getRasterMetadata(property) {
        const metadata = {
            'oc': {
                name: 'Organic Carbon',
                description: 'Soil organic carbon content',
                units: 'g/kg',
                source: 'SoilGrids 250m',
                depths: CONFIG.depthLevels.labels
            },
            'ph': {
                name: 'Soil pH',
                description: 'Soil pH (H2O)',
                units: 'pH units',
                source: 'SoilGrids 250m',
                depths: CONFIG.depthLevels.labels
            },
            'meanTemp': {
                name: 'Mean Temperature',
                description: 'Mean annual soil temperature',
                units: '°C',
                source: 'SoilGrids 250m',
                depths: CONFIG.depthLevels.labels
            },
            'landcover': {
                name: 'Land Cover',
                description: 'ESA WorldCover 2021 Land Cover Classification',
                units: 'class',
                source: 'ESA WorldCover 2021',
                depths: null
            },
            'elevation': {
                name: 'Elevation',
                description: 'Digital Elevation Model with Hillshade',
                units: 'meters',
                source: 'USGS 10m DEM',
                depths: null
            },
            'nlcd': {
                name: 'NLCD Land Cover',
                description: 'National Land Cover Database 2024',
                units: 'class',
                source: 'USGS NLCD 2024',
                depths: null
            },
            'lithology': {
                name: 'Parent Material',
                description: 'Geological parent material classification',
                units: 'class',
                source: 'USGS State Geologic Map',
                depths: null
            }
        };
        
        return metadata[property];
    }
}

// Initialize RasterManager when the window loads to ensure GeoTIFF is available
if (typeof window !== 'undefined') {
    // Check if GeoTIFF is already loaded
    if (typeof GeoTIFF !== 'undefined' || typeof window.GeoTIFF !== 'undefined') {
        window.rasterManager = new RasterManager();
        console.log('✅ RasterManager initialized immediately');
    } else {
        // Wait for window load to ensure all scripts are loaded
        window.addEventListener('load', function() {
            window.rasterManager = new RasterManager();
            console.log('✅ RasterManager initialized on window load');
        });
    }
}

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { RasterManager, rasterManager };
}