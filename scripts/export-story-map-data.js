#!/usr/bin/env node

/**
 * Export Script for Story Map Data
 * Generates lightweight, pre-colored GeoJSON files from the full SSURGO dataset
 * Optimized for interactive story map previews
 */

const fs = require('fs');
const path = require('path');

// Color configurations matching CONFIG from the main app
const SOIL_ORDER_COLORS = {
    "Alfisols": "#B5D55D",
    "Andisols": "#EA028C",
    "Aridisols": "#FDDCB9",
    "Entisols": "#75CDD6",
    "Gelisols": "#31A4BF",
    "Histosols": "#AE5044",
    "Inceptisols": "#CB7662",
    "Mollisols": "#00A551",
    "Oxisols": "#EC1F25",
    "Spodosols": "#D4BEC4",
    "Ultisols": "#FAAF19",
    "Vertisols": "#FFF100",
    "Rock outcrop": "#696969",
    "Rubble land": "#A9A9A9",
    "Water": "#1E90FF",
    "Non-soil area": "#D3D3D3",
    "Unknown": "#808080"
};

const PARTICLE_SIZE_COLORS = {
    "fine": "#2C3E50",
    "very-fine": "#1A252F",
    "fine-silty": "#34495E",
    "fine-loamy": "#5D6D7E",
    "loamy": "#7B8D9F",
    "coarse-loamy": "#95A5A6",
    "sandy": "#D4B896",
    "sandy-skeletal": "#E8D5B8",
    "loamy-skeletal": "#8B7355",
    "clayey-skeletal": "#654321",
    "medial-skeletal": "#A0522D",
    "clayey": "#4A5568",
    "medial": "#CD853F",
    "fine-loamy over clayey": "#6B7280",
    "not used": "#CCCCCC",
    "Unknown": "#808080"
};

// Soil order descriptions for labels
const SOIL_ORDER_LABELS = {
    "Alfisols": "Forest Soils",
    "Andisols": "Volcanic Soils",
    "Aridisols": "Desert Soils",
    "Entisols": "Young Soils",
    "Gelisols": "Permafrost Soils",
    "Histosols": "Organic Soils",
    "Inceptisols": "Developing Soils",
    "Mollisols": "Prairie Soils",
    "Oxisols": "Tropical Soils",
    "Spodosols": "Acidic Forest Soils",
    "Ultisols": "Weathered Soils",
    "Vertisols": "Shrinking-Swelling Soils"
};

/**
 * Simplify coordinates by reducing precision and removing redundant points
 * @param {Array} coords - Coordinate array
 * @param {number} precision - Decimal places to keep (default 5)
 * @param {number} tolerance - Distance tolerance for point reduction
 */
function simplifyCoordinates(coords, precision = 5, tolerance = 0.0001) {
    if (Array.isArray(coords[0])) {
        return coords.map(c => simplifyCoordinates(c, precision, tolerance));
    }
    
    // Round coordinates
    const rounded = coords.map(c => Math.round(c * Math.pow(10, precision)) / Math.pow(10, precision));
    
    // Simple point reduction for polygons (not a full Douglas-Peucker, but good enough)
    if (rounded.length > 3 && Array.isArray(rounded[0])) {
        const simplified = [rounded[0]]; // Keep first point
        
        for (let i = 1; i < rounded.length - 1; i++) {
            const prev = simplified[simplified.length - 1];
            const curr = rounded[i];
            const next = rounded[i + 1];
            
            // Calculate if current point is significant
            const dist1 = Math.sqrt(Math.pow(curr[0] - prev[0], 2) + Math.pow(curr[1] - prev[1], 2));
            const dist2 = Math.sqrt(Math.pow(next[0] - curr[0], 2) + Math.pow(next[1] - curr[1], 2));
            
            // Keep point if it's far enough from previous or creates significant angle
            if (dist1 > tolerance || dist2 > tolerance) {
                simplified.push(curr);
            }
        }
        
        simplified.push(rounded[rounded.length - 1]); // Keep last point
        return simplified;
    }
    
    return rounded;
}

/**
 * Simplify geometry by reducing coordinate precision
 */
function simplifyGeometry(geometry, precision = 5, tolerance = 0.0001) {
    return {
        type: geometry.type,
        coordinates: simplifyCoordinates(geometry.coordinates, precision, tolerance)
    };
}

/**
 * Export soil orders with colors - ONLY MAJOR COMPONENTS
 */
function exportSoilOrders(features, outputPath) {
    console.log('Exporting soil orders (major components only)...');
    
    const simplifiedFeatures = features
        .filter(f => f.properties && f.properties.taxorder && f.properties.majcompflag && f.properties.majcompflag.trim() === 'Yes')
        .map(feature => {
            const order = feature.properties.taxorder || 'Unknown';
            return {
                type: 'Feature',
                properties: {
                    order: order,
                    color: SOIL_ORDER_COLORS[order] || SOIL_ORDER_COLORS['Unknown'],
                    label: SOIL_ORDER_LABELS[order] || order,
                    musym: feature.properties.MUSYM || '',
                    mukey: feature.properties.MUKEY || ''
                },
                geometry: simplifyGeometry(feature.geometry, 3, 0.0005) // More aggressive simplification
            };
        });
    
    const geojson = {
        type: 'FeatureCollection',
        features: simplifiedFeatures
    };
    
    fs.writeFileSync(outputPath, JSON.stringify(geojson));
    const size = fs.statSync(outputPath).size / (1024 * 1024);
    console.log(`  Created: ${outputPath} (${size.toFixed(2)} MB)`);
    console.log(`  Features: ${simplifiedFeatures.length}`);
    
    // Count by order
    const orderCounts = {};
    simplifiedFeatures.forEach(f => {
        const order = f.properties.order;
        orderCounts[order] = (orderCounts[order] || 0) + 1;
    });
    console.log('  Soil orders:', Object.keys(orderCounts).sort().join(', '));
}

/**
 * Export particle sizes with colors - ONLY MAJOR COMPONENTS
 */
function exportParticleSizes(features, outputPath) {
    console.log('Exporting particle sizes (major components only)...');
    
    const simplifiedFeatures = features
        .filter(f => f.properties && f.properties.taxpartsize && f.properties.majcompflag && f.properties.majcompflag.trim() === 'Yes')
        .map(feature => {
            const size = feature.properties.taxpartsize || 'Unknown';
            return {
                type: 'Feature',
                properties: {
                    particleSize: size,
                    color: PARTICLE_SIZE_COLORS[size] || PARTICLE_SIZE_COLORS['Unknown'],
                    musym: feature.properties.MUSYM || '',
                    mukey: feature.properties.MUKEY || ''
                },
                geometry: simplifyGeometry(feature.geometry, 3, 0.0005) // More aggressive simplification
            };
        });
    
    const geojson = {
        type: 'FeatureCollection',
        features: simplifiedFeatures
    };
    
    fs.writeFileSync(outputPath, JSON.stringify(geojson));
    const size = fs.statSync(outputPath).size / (1024 * 1024);
    console.log(`  Created: ${outputPath} (${size.toFixed(2)} MB)`);
    console.log(`  Features: ${simplifiedFeatures.length}`);
}

/**
 * Export combined minimal dataset with both soil order and particle size - ONLY MAJOR COMPONENTS
 */
function exportCombinedMinimal(features, outputPath) {
    console.log('Exporting combined minimal dataset (major components only)...');
    
    const simplifiedFeatures = features
        .filter(f => f.properties && f.properties.majcompflag && f.properties.majcompflag.trim() === 'Yes')
        .map(feature => {
        const order = feature.properties.taxorder || 'Unknown';
        const size = feature.properties.taxpartsize || 'Unknown';
        
        return {
            type: 'Feature',
            properties: {
                o: order, // Short property names to save space
                oc: SOIL_ORDER_COLORS[order] || SOIL_ORDER_COLORS['Unknown'],
                p: size,
                pc: PARTICLE_SIZE_COLORS[size] || PARTICLE_SIZE_COLORS['Unknown'],
                m: feature.properties.MUSYM || ''
            },
            geometry: simplifyGeometry(feature.geometry, 3, 0.001) // Even more aggressive for combined
        };
    });
    
    const geojson = {
        type: 'FeatureCollection',
        features: simplifiedFeatures
    };
    
    fs.writeFileSync(outputPath, JSON.stringify(geojson));
    const size = fs.statSync(outputPath).size / (1024 * 1024);
    console.log(`  Created: ${outputPath} (${size.toFixed(2)} MB)`);
    console.log(`  Features: ${simplifiedFeatures.length}`);
}

/**
 * Export parent material types with colors
 */
function exportParentMaterial(features, outputPath) {
    console.log('Exporting parent material types (major components only)...');
    
    // Parent material colors
    const PARENT_MATERIAL_COLORS = {
        "Volcanic": "#D2691E",
        "Serpentine": "#2E7D32",
        "Alluvial": "#4682B4",
        "Marine": "#5D6D7E",
        "Basin deposits": "#8B7355",
        "Clay-rich sediments": "#FFF100",
        "Mixed colluvium": "#95A5A6",
        "Plateau deposits": "#CD853F",
        "Mixed/Undifferentiated": "#808080"
    };
    
    const simplifiedFeatures = features
        .filter(f => f.properties && f.properties.majcompflag && f.properties.majcompflag.trim() === 'Yes')
        .map(feature => {
            // Determine parent material based on available data
            const compname = (feature.properties.compname || '').toLowerCase();
            const taxorder = feature.properties.taxorder || '';
            const geomdesc = (feature.properties.geomdesc || '').toLowerCase();
            
            let material;
            if (taxorder === 'Andisols' || compname.includes('ash')) {
                material = 'Volcanic';
            } else if (compname.includes('serpent')) {
                material = 'Serpentine';
            } else if (geomdesc.includes('alluvial') || geomdesc.includes('flood') || geomdesc.includes('terrace')) {
                material = 'Alluvial';
            } else if (geomdesc.includes('lava')) {
                material = 'Volcanic';
            } else if (geomdesc.includes('mountain') || geomdesc.includes('hill')) {
                if (taxorder === 'Vertisols') {
                    material = 'Clay-rich sediments';
                } else {
                    material = 'Mixed colluvium';
                }
            } else if (geomdesc.includes('basin')) {
                material = 'Basin deposits';
            } else if (geomdesc.includes('plateau')) {
                material = 'Plateau deposits';
            } else {
                material = 'Mixed/Undifferentiated';
            }
            
            return {
                type: 'Feature',
                properties: {
                    parentMaterial: material,
                    color: PARENT_MATERIAL_COLORS[material] || PARENT_MATERIAL_COLORS['Mixed/Undifferentiated'],
                    geomdesc: feature.properties.geomdesc || '',
                    musym: feature.properties.MUSYM || '',
                    mukey: feature.properties.MUKEY || ''
                },
                geometry: simplifyGeometry(feature.geometry, 3, 0.0005) // More aggressive simplification
            };
        });
    
    const geojson = {
        type: 'FeatureCollection',
        features: simplifiedFeatures
    };
    
    fs.writeFileSync(outputPath, JSON.stringify(geojson));
    const size = fs.statSync(outputPath).size / (1024 * 1024);
    console.log(`  Created: ${outputPath} (${size.toFixed(2)} MB)`);
    console.log(`  Features: ${simplifiedFeatures.length}`);
    
    // Count by parent material
    const materialCounts = {};
    simplifiedFeatures.forEach(f => {
        const material = f.properties.parentMaterial;
        materialCounts[material] = (materialCounts[material] || 0) + 1;
    });
    console.log('  Parent materials:', Object.keys(materialCounts).sort().join(', '));
}

/**
 * Main export function
 */
function main() {
    console.log('Starting Story Map Data Export...\n');
    
    // Input and output paths
    const inputPath = path.join(__dirname, '..', 'data', 'CSNM_Polygons_WGS84.geojson');
    const outputDir = path.join(__dirname, '..', 'data', 'story-maps');
    
    // Ensure output directory exists
    if (!fs.existsSync(outputDir)) {
        fs.mkdirSync(outputDir, { recursive: true });
    }
    
    // Load the full dataset
    console.log(`Loading source data from: ${inputPath}`);
    const rawData = fs.readFileSync(inputPath, 'utf8');
    const data = JSON.parse(rawData);
    const originalSize = fs.statSync(inputPath).size / (1024 * 1024);
    console.log(`  Original size: ${originalSize.toFixed(2)} MB`);
    console.log(`  Total features: ${data.features.length}\n`);
    
    // Export different versions
    exportSoilOrders(
        data.features,
        path.join(outputDir, 'soil-orders-simplified.geojson')
    );
    
    console.log('');
    
    exportParticleSizes(
        data.features,
        path.join(outputDir, 'particle-sizes-simplified.geojson')
    );
    
    console.log('');
    
    exportCombinedMinimal(
        data.features,
        path.join(outputDir, 'combined-minimal.geojson')
    );
    
    console.log('');
    
    exportParentMaterial(
        data.features,
        path.join(outputDir, 'parent-material-simplified.geojson')
    );
    
    // Also copy and simplify the boundary for reference
    const boundaryPath = path.join(__dirname, '..', 'data', 'CSNM_boundary_WGS84.geojson');
    if (fs.existsSync(boundaryPath)) {
        console.log('\nCopying boundary file...');
        const boundaryData = JSON.parse(fs.readFileSync(boundaryPath, 'utf8'));
        
        // Simplify boundary coordinates
        const simplifiedBoundary = {
            type: boundaryData.type,
            features: boundaryData.features.map(f => ({
                type: f.type,
                properties: { name: 'CSNM Boundary' },
                geometry: simplifyGeometry(f.geometry, 3, 0.001)
            }))
        };
        
        const boundaryOutput = path.join(outputDir, 'boundary-simplified.geojson');
        fs.writeFileSync(boundaryOutput, JSON.stringify(simplifiedBoundary));
        const boundarySize = fs.statSync(boundaryOutput).size / 1024;
        console.log(`  Created: ${boundaryOutput} (${boundarySize.toFixed(1)} KB)`);
    }
    
    console.log('\n✅ Export complete! Files saved to:', outputDir);
}

// Run the export
if (require.main === module) {
    main();
}

module.exports = { exportSoilOrders, exportParticleSizes, exportCombinedMinimal };