#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const zlib = require('zlib');
const { promisify } = require('util');

const readdir = promisify(fs.readdir);
const stat = promisify(fs.stat);
const readFile = promisify(fs.readFile);
const writeFile = promisify(fs.writeFile);
const gzip = promisify(zlib.gzip);

const COMPRESSION_THRESHOLD = 1024 * 100; // 100KB
const DATA_DIR = path.join(__dirname, 'data');

async function compressFile(filePath) {
    try {
        const stats = await stat(filePath);
        const fileSize = stats.size;
        const fileName = path.basename(filePath);
        
        // Skip if already compressed or too small
        if (fileName.endsWith('.gz') || fileSize < COMPRESSION_THRESHOLD) {
            return null;
        }
        
        console.log(`Compressing ${fileName} (${(fileSize / 1024 / 1024).toFixed(2)} MB)...`);
        
        const content = await readFile(filePath);
        const compressed = await gzip(content, {
            level: 9 // Maximum compression
        });
        
        const compressedPath = `${filePath}.gz`;
        await writeFile(compressedPath, compressed);
        
        const compressedStats = await stat(compressedPath);
        const compressionRatio = ((1 - compressedStats.size / fileSize) * 100).toFixed(1);
        
        console.log(`  ✓ Compressed to ${(compressedStats.size / 1024 / 1024).toFixed(2)} MB (${compressionRatio}% reduction)`);
        
        return {
            original: filePath,
            compressed: compressedPath,
            originalSize: fileSize,
            compressedSize: compressedStats.size,
            ratio: compressionRatio
        };
    } catch (error) {
        console.error(`  ✗ Error compressing ${filePath}:`, error.message);
        return null;
    }
}

async function processDirectory(dir) {
    const files = await readdir(dir);
    const results = [];
    
    for (const file of files) {
        const filePath = path.join(dir, file);
        const stats = await stat(filePath);
        
        if (stats.isDirectory()) {
            // Recursively process subdirectories
            const subResults = await processDirectory(filePath);
            results.push(...subResults);
        } else if (file.endsWith('.geojson') || file.endsWith('.json') || 
                   file.endsWith('.tif') || file.endsWith('.tiff')) {
            // Compress GeoJSON, JSON, and TIFF files
            const result = await compressFile(filePath);
            if (result) {
                results.push(result);
            }
        }
    }
    
    return results;
}

async function main() {
    console.log('🔧 Starting asset compression...\n');
    
    try {
        // Check if data directory exists
        await stat(DATA_DIR);
        
        // Process all files in the data directory
        const results = await processDirectory(DATA_DIR);
        
        // Print summary
        console.log('\n📊 Compression Summary:');
        console.log('─'.repeat(50));
        
        if (results.length === 0) {
            console.log('No files were compressed.');
        } else {
            let totalOriginal = 0;
            let totalCompressed = 0;
            
            results.forEach(result => {
                totalOriginal += result.originalSize;
                totalCompressed += result.compressedSize;
            });
            
            console.log(`Files compressed: ${results.length}`);
            console.log(`Total original size: ${(totalOriginal / 1024 / 1024).toFixed(2)} MB`);
            console.log(`Total compressed size: ${(totalCompressed / 1024 / 1024).toFixed(2)} MB`);
            console.log(`Overall compression ratio: ${((1 - totalCompressed / totalOriginal) * 100).toFixed(1)}%`);
            
            // List large files that benefit most from compression
            const significantFiles = results
                .filter(r => r.originalSize > 1024 * 1024) // > 1MB
                .sort((a, b) => b.originalSize - a.originalSize);
            
            if (significantFiles.length > 0) {
                console.log('\n🎯 Largest compressed files:');
                significantFiles.slice(0, 5).forEach(result => {
                    const name = path.basename(result.original);
                    console.log(`  • ${name}: ${(result.originalSize / 1024 / 1024).toFixed(1)} MB → ${(result.compressedSize / 1024 / 1024).toFixed(1)} MB`);
                });
            }
        }
        
        console.log('\n✅ Compression complete!');
        console.log('\nNote: Update your application to load .gz files where available.');
        
    } catch (error) {
        if (error.code === 'ENOENT') {
            console.error('❌ Error: Data directory not found at', DATA_DIR);
        } else {
            console.error('❌ Error:', error.message);
        }
        process.exit(1);
    }
}

// Run the script
main().catch(console.error);