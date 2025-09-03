#!/usr/bin/env python3
"""
PRISM Data Proxy Server for Development
Handles fetching PRISM data and converting to GeoTIFF format
"""

import os
import json
import tempfile
import zipfile
import requests
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs
from datetime import datetime, timedelta

# Cache directory for PRISM data
CACHE_DIR = os.path.join(tempfile.gettempdir(), 'prism_cache')
os.makedirs(CACHE_DIR, exist_ok=True)

class PRISMProxyHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        """Handle GET requests for PRISM data"""
        parsed = urlparse(self.path)
        
        if parsed.path == '/api/prism-proxy':
            self.handle_prism_request(parsed)
        else:
            self.send_error(404, "Not Found")
    
    def handle_prism_request(self, parsed):
        """Handle PRISM data proxy requests"""
        params = parse_qs(parsed.query)
        
        # Get the PRISM URL from query params
        prism_url = params.get('url', [''])[0]
        
        if not prism_url:
            self.send_error(400, "Missing PRISM URL parameter")
            return
        
        # For development, return mock data
        # In production, you would:
        # 1. Download the .bil.zip file from PRISM
        # 2. Unzip and convert .bil to GeoTIFF
        # 3. Return the GeoTIFF data
        
        self.send_mock_prism_data()
    
    def send_mock_prism_data(self):
        """Send mock PRISM data for development"""
        # Create mock GeoTIFF-like response
        mock_data = {
            'status': 'success',
            'message': 'Mock PRISM data for development',
            'note': 'Replace with actual PRISM data fetching in production',
            'data': {
                'width': 100,
                'height': 100,
                'bbox': [-122.7, 41.9, -122.2, 42.3],  # CSNM approximate bounds
                'values': [20.5 + (i % 10) * 0.5 for i in range(10000)]  # Mock temperature values
            }
        }
        
        # Send response
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()
        self.wfile.write(json.dumps(mock_data).encode())
    
    def log_message(self, format, *args):
        """Override to reduce logging verbosity"""
        if '/api/prism-proxy' in args[0]:
            print(f"PRISM proxy request: {args[0]}")

def run_server(port=8001):
    """Run the PRISM proxy server"""
    server_address = ('', port)
    httpd = HTTPServer(server_address, PRISMProxyHandler)
    print(f"PRISM proxy server running on http://localhost:{port}")
    print(f"Cache directory: {CACHE_DIR}")
    print("Note: This is a development server providing mock data")
    print("For production, implement actual PRISM data fetching and conversion")
    
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down PRISM proxy server...")
        httpd.shutdown()

if __name__ == '__main__':
    run_server()