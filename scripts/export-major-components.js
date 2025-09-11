#!/usr/bin/env node

/**
 * Export Script for Major Components Only
 * Creates a full-resolution GeoJSON file with only major soil components
 * This resolves overlaps by keeping only the dominant soil type in each area
 */

const fs = require('fs');
const path = require('path');

/**
 * Main export function
 */
function exportMajorComponents() {
    console.log('Starting Major Components Export...\n');
    
    // Input and output paths
    const inputPath = path.join(__dirname, '..', 'data', 'CSNM_Polygons_WGS84.geojson');
    const outputPath = path.join(__dirname, '..', 'data', 'CSNM_Polygons_WGS84_MajorComponents.geojson');
    
    // Load the full dataset
    console.log(`Loading source data from: ${inputPath}`);
    const rawData = fs.readFileSync(inputPath, 'utf8');
    const data = JSON.parse(rawData);
    const originalSize = fs.statSync(inputPath).size / (1024 * 1024);
    console.log(`  Original size: ${originalSize.toFixed(2)} MB`);
    console.log(`  Total features: ${data.features.length}`);
    
    // Group features by MUKEY and select the dominant component
    console.log('\nSelecting dominant components per map unit...');
    const featuresByMukey = {};
    
    // Group all features by MUKEY
    data.features.forEach(feature => {
        const mukey = feature.properties.MUKEY || feature.properties.mukey;
        if (!mukey) return;
        
        if (!featuresByMukey[mukey]) {
            featuresByMukey[mukey] = [];
        }
        featuresByMukey[mukey].push(feature);
    });
    
    // For each MUKEY, select the component with highest percentage
    const majorComponents = [];
    Object.keys(featuresByMukey).forEach(mukey => {
        const features = featuresByMukey[mukey];
        
        // Find the feature with the highest comppct_r
        let dominantFeature = features[0];
        let maxPercent = 0;
        
        features.forEach(feature => {
            const percent = feature.properties.comppct_r || 0;
            // Only consider features marked as major components
            const majcompflag = feature.properties.majcompflag;
            const isMajor = majcompflag && majcompflag.trim() === 'Yes';
            
            if (isMajor && percent > maxPercent) {
                maxPercent = percent;
                dominantFeature = feature;
            }
        });
        
        // If no major component found, take the one with highest percentage
        if (maxPercent === 0) {
            features.forEach(feature => {
                const percent = feature.properties.comppct_r || 0;
                if (percent > maxPercent) {
                    maxPercent = percent;
                    dominantFeature = feature;
                }
            });
        }
        
        majorComponents.push(dominantFeature);
    });
    
    console.log(`  Map units (MUKEYs): ${Object.keys(featuresByMukey).length}`);
    console.log(`  Dominant components selected: ${majorComponents.length} features`);
    console.log(`  Removed: ${data.features.length - majorComponents.length} non-dominant components`);
    
    // Count by soil order
    const soilOrders = {};
    const particleSizes = {};
    
    majorComponents.forEach(feature => {
        const order = feature.properties.taxorder || 'Unknown';
        soilOrders[order] = (soilOrders[order] || 0) + 1;
        
        const size = feature.properties.taxpartsize || 'Unknown';
        particleSizes[size] = (particleSizes[size] || 0) + 1;
    });
    
    console.log('\nSoil Orders in major components:');
    Object.keys(soilOrders).sort().forEach(order => {
        console.log(`  ${order}: ${soilOrders[order]} features`);
    });
    
    console.log('\nParticle Size Classes in major components:');
    const topSizes = Object.entries(particleSizes)
        .sort((a, b) => b[1] - a[1])
        .slice(0, 8);
    topSizes.forEach(([size, count]) => {
        console.log(`  ${size}: ${count} features`);
    });
    
    // Create the output GeoJSON with major components only
    const outputData = {
        type: 'FeatureCollection',
        crs: data.crs, // Preserve CRS if present
        features: majorComponents
    };
    
    // Write the output file
    console.log(`\nWriting output to: ${outputPath}`);
    fs.writeFileSync(outputPath, JSON.stringify(outputData));
    
    // Report final statistics
    const outputSize = fs.statSync(outputPath).size / (1024 * 1024);
    const reduction = ((originalSize - outputSize) / originalSize * 100).toFixed(1);
    
    console.log(`  Output size: ${outputSize.toFixed(2)} MB`);
    console.log(`  Size reduction: ${reduction}% smaller`);
    console.log(`  Feature reduction: ${((data.features.length - majorComponents.length) / data.features.length * 100).toFixed(1)}% fewer polygons`);
    
    console.log('\n✅ Export complete!');
    console.log('\nTo use in the main application, update js/config.js:');
    console.log('  soilPolygons: "data/CSNM_Polygons_WGS84_MajorComponents.geojson"');
}

// Run the export
if (require.main === module) {
    try {
        exportMajorComponents();
    } catch (error) {
        console.error('❌ Export failed:', error.message);
        process.exit(1);
    }
}

module.exports = { exportMajorComponents };