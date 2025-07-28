# Vercel Deployment Fix

## Issue
The Vercel deployment is still serving the old `CSNM_Polygons_with_Data.geojson.gz` file even though it has been deleted locally.

## Solutions

### Option 1: Force Redeploy
1. Make a small change to trigger a new deployment
2. Commit and push to trigger Vercel rebuild
3. Or manually redeploy from Vercel dashboard

### Option 2: Clear Vercel Cache
1. Go to your Vercel dashboard
2. Navigate to your project settings
3. Go to "Functions" tab
4. Click "Purge Cache"

### Option 3: Add Cache-Busting Version
1. Update the config to include a version parameter
2. This forces Vercel to serve fresh files

### Files Changed
- Deleted: `data/CSNM_Polygons_with_Data.geojson`
- Deleted: `data/CSNM_Polygons_with_Data.geojson.gz`
- Deleted: `data/test_polygons.geojson`
- Moved: `CSNM_Polygons_WGS84.geojson.gz` → `data/CSNM_Polygons_WGS84.geojson.gz`
- Moved: `CSNM_boundary_WGS84.geojson` → `data/CSNM_boundary_WGS84.geojson`

### Verification
After redeployment, the console should show:
- Loading: `data/CSNM_Polygons_WGS84.geojson.gz`
- No projection warnings (coordinates should be in WGS84)

### Additional Fix Applied
- Fixed info button CSS to prevent display issues with the "i" character