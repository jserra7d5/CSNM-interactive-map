// PRISM Data Service Module
// Handles fetching, caching, and processing of PRISM AN81M monthly climate data

class PRISMDataService {
    constructor() {
        this.baseURL = 'https://data.prism.oregonstate.edu/monthly/';
        this.variables = ['ppt', 'tmean', 'tmin', 'tmax'];
        this.cache = new Map();
        this.boundaryData = null;
        this.dbName = 'PRISMDataCache';
        this.dbVersion = 1;
        this.db = null;
        
        // Initialize IndexedDB
        this.initDB();
    }
    
    // Initialize IndexedDB for persistent storage
    async initDB() {
        return new Promise((resolve, reject) => {
            const request = indexedDB.open(this.dbName, this.dbVersion);
            
            request.onerror = () => {
                console.error('Failed to open IndexedDB:', request.error);
                reject(request.error);
            };
            
            request.onsuccess = () => {
                this.db = request.result;
                console.log('PRISM IndexedDB initialized');
                resolve();
            };
            
            request.onupgradeneeded = (event) => {
                const db = event.target.result;
                
                // Create object store for PRISM data
                if (!db.objectStoreNames.contains('prismData')) {
                    const store = db.createObjectStore('prismData', { keyPath: 'id' });
                    store.createIndex('timestamp', 'timestamp', { unique: false });
                    store.createIndex('variable', 'variable', { unique: false });
                }
                
                // Create object store for metadata
                if (!db.objectStoreNames.contains('metadata')) {
                    db.createObjectStore('metadata', { keyPath: 'key' });
                }
            };
        });
    }
    
    // Load monument boundary for clipping
    async loadBoundary() {
        if (this.boundaryData) {
            return this.boundaryData;
        }
        
        try {
            const response = await fetch(CONFIG.dataPaths.boundaryPolygon);
            this.boundaryData = await response.json();
            return this.boundaryData;
        } catch (error) {
            console.error('Failed to load monument boundary:', error);
            throw error;
        }
    }
    
    // Generate PRISM data URL
    getPRISMUrl(variable, year, month) {
        // PRISM uses format: PRISM_ppt_stable_4kmM3_YYYYMM_bil.zip
        const yearMonth = `${year}${String(month).padStart(2, '0')}`;
        const stability = this.getDataStability(year, month);
        return `${this.baseURL}${variable}/${yearMonth}/PRISM_${variable}_${stability}_4kmM3_${yearMonth}_bil.zip`;
    }
    
    // Determine data stability level based on recency
    getDataStability(year, month) {
        const now = new Date();
        const dataDate = new Date(year, month - 1);
        const monthsDiff = (now.getFullYear() - dataDate.getFullYear()) * 12 + 
                          (now.getMonth() - dataDate.getMonth());
        
        if (monthsDiff < 1) return 'early';
        if (monthsDiff < 6) return 'provisional';
        return 'stable';
    }
    
    // Fetch monthly data from PRISM
    async fetchMonthlyData(year, month, variable) {
        const cacheKey = `${variable}_${year}_${month}`;
        
        // Check memory cache first
        if (this.cache.has(cacheKey)) {
            return this.cache.get(cacheKey);
        }
        
        // Check IndexedDB cache
        const cachedData = await this.getCachedData(cacheKey);
        if (cachedData && this.isCacheValid(cachedData)) {
            this.cache.set(cacheKey, cachedData.data);
            return cachedData.data;
        }
        
        // Fetch from PRISM server
        try {
            const url = this.getPRISMUrl(variable, year, month);
            console.log(`Fetching PRISM data from: ${url}`);
            
            // Note: PRISM provides .bil format in zip files
            // For browser implementation, we'd need a proxy server to:
            // 1. Download and unzip the file
            // 2. Convert .bil to GeoTIFF
            // 3. Serve the GeoTIFF to the browser
            
            // For now, we'll use a placeholder that assumes a proxy service
            const proxyUrl = `/api/prism-proxy?url=${encodeURIComponent(url)}`;
            const response = await fetch(proxyUrl);
            
            if (!response.ok) {
                throw new Error(`Failed to fetch PRISM data: ${response.statusText}`);
            }
            
            const arrayBuffer = await response.arrayBuffer();
            const tiffData = await this.processPRISMData(arrayBuffer, variable);
            
            // Cache the processed data
            await this.cacheData(cacheKey, tiffData);
            this.cache.set(cacheKey, tiffData);
            
            return tiffData;
        } catch (error) {
            console.error(`Failed to fetch PRISM data for ${variable} ${year}-${month}:`, error);
            throw error;
        }
    }
    
    // Process raw PRISM data
    async processPRISMData(arrayBuffer, variable) {
        // Parse GeoTIFF using the GeoTIFF library
        const tiff = await GeoTIFF.fromArrayBuffer(arrayBuffer);
        const image = await tiff.getImage();
        const rasters = await image.readRasters();
        
        // Get georeferencing information
        const bbox = image.getBoundingBox();
        const width = image.getWidth();
        const height = image.getHeight();
        
        // Clip to monument boundary
        const clippedData = await this.clipToMonumentBoundary({
            data: rasters[0],
            width,
            height,
            bbox,
            variable
        });
        
        return clippedData;
    }
    
    // Clip raster data to monument boundary
    async clipToMonumentBoundary(rasterData) {
        const boundary = await this.loadBoundary();
        
        // Create canvas for masking
        const canvas = document.createElement('canvas');
        canvas.width = rasterData.width;
        canvas.height = rasterData.height;
        const ctx = canvas.getContext('2d');
        
        // Convert boundary GeoJSON to pixel coordinates
        const pixelBoundary = this.geoJSONToPixels(
            boundary,
            rasterData.bbox,
            rasterData.width,
            rasterData.height
        );
        
        // Create mask from boundary
        ctx.fillStyle = 'black';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.fillStyle = 'white';
        ctx.beginPath();
        
        // Draw boundary polygon
        pixelBoundary.forEach((ring, ringIndex) => {
            ring.forEach((coord, index) => {
                if (index === 0) {
                    ctx.moveTo(coord[0], coord[1]);
                } else {
                    ctx.lineTo(coord[0], coord[1]);
                }
            });
            if (ringIndex === 0) {
                ctx.closePath();
            }
        });
        ctx.fill();
        
        // Get mask data
        const maskData = ctx.getImageData(0, 0, canvas.width, canvas.height);
        
        // Apply mask to raster data
        const clippedData = new Float32Array(rasterData.data.length);
        for (let i = 0; i < rasterData.data.length; i++) {
            const maskValue = maskData.data[i * 4] / 255; // Use red channel as mask
            clippedData[i] = maskValue > 0.5 ? rasterData.data[i] : NaN;
        }
        
        return {
            ...rasterData,
            data: clippedData,
            clipped: true
        };
    }
    
    // Convert GeoJSON coordinates to pixel coordinates
    geoJSONToPixels(geoJSON, bbox, width, height) {
        const [minX, minY, maxX, maxY] = bbox;
        const xScale = width / (maxX - minX);
        const yScale = height / (maxY - minY);
        
        const features = geoJSON.features || [geoJSON];
        const pixelCoords = [];
        
        features.forEach(feature => {
            const geometry = feature.geometry;
            if (geometry.type === 'Polygon') {
                geometry.coordinates.forEach(ring => {
                    const pixelRing = ring.map(coord => {
                        const x = (coord[0] - minX) * xScale;
                        const y = height - (coord[1] - minY) * yScale; // Flip Y axis
                        return [x, y];
                    });
                    pixelCoords.push(pixelRing);
                });
            } else if (geometry.type === 'MultiPolygon') {
                geometry.coordinates.forEach(polygon => {
                    polygon.forEach(ring => {
                        const pixelRing = ring.map(coord => {
                            const x = (coord[0] - minX) * xScale;
                            const y = height - (coord[1] - minY) * yScale;
                            return [x, y];
                        });
                        pixelCoords.push(pixelRing);
                    });
                });
            }
        });
        
        return pixelCoords;
    }
    
    // Get last 12 months of data
    async getLastTwelveMonths(variable = 'tmean') {
        const now = new Date();
        const monthsData = [];
        
        for (let i = 0; i < 12; i++) {
            const date = new Date(now.getFullYear(), now.getMonth() - i, 1);
            const year = date.getFullYear();
            const month = date.getMonth() + 1; // JavaScript months are 0-indexed
            
            try {
                const data = await this.fetchMonthlyData(year, month, variable);
                monthsData.unshift({
                    year,
                    month,
                    variable,
                    data,
                    date: date.toISOString()
                });
            } catch (error) {
                console.warn(`Failed to fetch data for ${year}-${month}:`, error);
            }
        }
        
        return monthsData;
    }
    
    // Cache data to IndexedDB
    async cacheData(key, data) {
        if (!this.db) {
            await this.initDB();
        }
        
        return new Promise((resolve, reject) => {
            const transaction = this.db.transaction(['prismData'], 'readwrite');
            const store = transaction.objectStore('prismData');
            
            const record = {
                id: key,
                data: data,
                timestamp: Date.now(),
                variable: key.split('_')[0]
            };
            
            const request = store.put(record);
            
            request.onsuccess = () => resolve();
            request.onerror = () => reject(request.error);
        });
    }
    
    // Get cached data from IndexedDB
    async getCachedData(key) {
        if (!this.db) {
            await this.initDB();
        }
        
        return new Promise((resolve, reject) => {
            const transaction = this.db.transaction(['prismData'], 'readonly');
            const store = transaction.objectStore('prismData');
            const request = store.get(key);
            
            request.onsuccess = () => resolve(request.result);
            request.onerror = () => reject(request.error);
        });
    }
    
    // Check if cache is still valid
    isCacheValid(cachedData) {
        if (!cachedData || !cachedData.timestamp) {
            return false;
        }
        
        const cacheAge = Date.now() - cachedData.timestamp;
        const maxAge = 30 * 24 * 60 * 60 * 1000; // 30 days
        
        return cacheAge < maxAge;
    }
    
    // Check for data updates
    async checkForUpdates() {
        const now = new Date();
        const dayOfMonth = now.getDate();
        
        // Check on the 15th of each month
        if (dayOfMonth !== 15) {
            return false;
        }
        
        // Get last check timestamp from metadata
        const lastCheck = await this.getMetadata('lastUpdateCheck');
        if (lastCheck) {
            const lastCheckDate = new Date(lastCheck);
            if (lastCheckDate.toDateString() === now.toDateString()) {
                return false; // Already checked today
            }
        }
        
        // Perform update check
        console.log('Checking for PRISM data updates...');
        
        // Update last check timestamp
        await this.setMetadata('lastUpdateCheck', now.toISOString());
        
        // Check if new month's data is available
        const previousMonth = new Date(now.getFullYear(), now.getMonth() - 1, 1);
        const year = previousMonth.getFullYear();
        const month = previousMonth.getMonth() + 1;
        
        try {
            // Try to fetch the most recent month's data
            for (const variable of this.variables) {
                const cacheKey = `${variable}_${year}_${month}`;
                const cachedData = await this.getCachedData(cacheKey);
                
                if (!cachedData) {
                    console.log(`New data available for ${variable} ${year}-${month}`);
                    await this.fetchMonthlyData(year, month, variable);
                }
            }
            
            return true;
        } catch (error) {
            console.error('Error checking for updates:', error);
            return false;
        }
    }
    
    // Get metadata
    async getMetadata(key) {
        if (!this.db) {
            await this.initDB();
        }
        
        return new Promise((resolve, reject) => {
            const transaction = this.db.transaction(['metadata'], 'readonly');
            const store = transaction.objectStore('metadata');
            const request = store.get(key);
            
            request.onsuccess = () => {
                const result = request.result;
                resolve(result ? result.value : null);
            };
            request.onerror = () => reject(request.error);
        });
    }
    
    // Set metadata
    async setMetadata(key, value) {
        if (!this.db) {
            await this.initDB();
        }
        
        return new Promise((resolve, reject) => {
            const transaction = this.db.transaction(['metadata'], 'readwrite');
            const store = transaction.objectStore('metadata');
            const request = store.put({ key, value });
            
            request.onsuccess = () => resolve();
            request.onerror = () => reject(request.error);
        });
    }
    
    // Clear all cached data
    async clearCache() {
        this.cache.clear();
        
        if (!this.db) {
            await this.initDB();
        }
        
        return new Promise((resolve, reject) => {
            const transaction = this.db.transaction(['prismData'], 'readwrite');
            const store = transaction.objectStore('prismData');
            const request = store.clear();
            
            request.onsuccess = () => {
                console.log('PRISM cache cleared');
                resolve();
            };
            request.onerror = () => reject(request.error);
        });
    }
}

// Export singleton instance
const prismDataService = new PRISMDataService();

// Make available globally
window.PRISMDataService = PRISMDataService;
window.prismDataService = prismDataService;