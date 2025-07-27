# Deployment Instructions for Vercel

## Prerequisites
- Git repository initialized and pushed to GitHub
- Vercel account (free tier works)
- GitHub account connected to Vercel

## Quick Deploy

1. **Push to GitHub**
   ```bash
   git add .
   git commit -m "Prepare for Vercel deployment"
   git push origin main
   ```

2. **Deploy to Vercel**
   - Go to [vercel.com](https://vercel.com)
   - Click "Add New Project"
   - Import your GitHub repository
   - Vercel will auto-detect the settings from vercel.json
   - Click "Deploy"

## Files Created for Deployment

- **vercel.json** - Configures headers, CORS, and caching
- **.vercelignore** - Excludes unnecessary files from deployment
- **package.json** - Project metadata (optional but recommended)

## Important Notes

- All data files (TIFF, GeoJSON) are included in deployment
- Total size is ~115MB which is within Vercel's limits
- CORS headers are configured for cross-origin access
- Static files are cached for performance

## Post-Deployment

1. Test all map layers load correctly
2. Verify TIFF files are served with correct headers
3. Check browser console for any CORS issues
4. Monitor performance in Vercel dashboard

## Environment Variables

None required - this is a fully static application.

## Custom Domain (Optional)

In Vercel project settings:
1. Go to "Domains"
2. Add your custom domain
3. Follow DNS configuration instructions