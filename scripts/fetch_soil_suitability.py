#!/usr/bin/env python3
"""
Fetch Soil Suitability Ratings from USDA SDA (Soil Data Access)
This script queries the cointerp table to get soil interpretations for all map units
in the CSNM GeoJSON file and saves them as a local JSON file.
"""

import json
import requests
import time
from collections import defaultdict
from pathlib import Path

SDA_URL = "https://SDMDataAccess.sc.egov.usda.gov/Tabular/post.rest"

# Categories to fetch (matching UC Davis SoilWeb organization)
CATEGORIES = {
    'engineering': 'ENG - %',
    'agriculture': 'AGR - %',
    'waste_management': 'WMS - %',
    'urban_recreation': 'URB/REC - %',
    'forestry': 'FOR - %',
    'wildlife': 'WLF - %',
    'rangeland': 'GRL - %',
    'building_site': 'DHS - %',
    'irrigation': 'AWM - %',
}

def query_sda(sql):
    """Execute a SQL query against SDA and return results."""
    try:
        response = requests.post(
            SDA_URL,
            data={'QUERY': sql, 'FORMAT': 'JSON'},
            headers={'Content-Type': 'application/x-www-form-urlencoded'},
            timeout=120
        )
        response.raise_for_status()
        data = response.json()
        return data.get('Table', [])
    except Exception as e:
        print(f"  Error: {e}")
        return []

def get_mukeys_from_geojson(filepath):
    """Extract unique MUKEYs from the GeoJSON file."""
    with open(filepath, 'r') as f:
        data = json.load(f)

    mukeys = set()
    for feature in data['features']:
        mukey = feature['properties'].get('MUKEY')
        if mukey:
            mukeys.add(str(mukey))

    return sorted(mukeys)

def fetch_interpretations_for_mukeys(mukeys, category_pattern, category_name):
    """Fetch interpretations for a list of MUKEYs."""
    print(f"  Fetching {category_name}...")

    # Build IN clause with all mukeys
    mukey_list = ", ".join([f"'{m}'" for m in mukeys])

    sql = f"""
    SELECT
        c.mukey,
        c.compname,
        c.comppct_r,
        ci.mrulename,
        ci.interphrc
    FROM component c
    INNER JOIN cointerp ci ON c.cokey = ci.cokey
    WHERE c.mukey IN ({mukey_list})
    AND c.majcompflag = 'Yes'
    AND ci.mrulename LIKE '{category_pattern}'
    AND ci.ruledepth = 0
    ORDER BY c.mukey, c.compname, ci.mrulename
    """

    results = query_sda(sql)
    print(f"    Got {len(results)} records")
    return results

def organize_by_mukey(results, category_name):
    """Organize results by MUKEY -> component -> interpretations."""
    organized = defaultdict(lambda: defaultdict(dict))

    for row in results:
        mukey, compname, comppct, rulename, rating = row

        # Clean up rule name (remove category prefix)
        clean_name = rulename
        for prefix in ['ENG - ', 'AGR - ', 'WMS - ', 'URB/REC - ', 'FOR - ', 'WLF - ', 'GRL - ', 'DHS - ', 'AWM - ']:
            if clean_name.startswith(prefix):
                clean_name = clean_name[len(prefix):]
                break

        if mukey not in organized:
            organized[mukey] = {}

        if compname not in organized[mukey]:
            organized[mukey][compname] = {
                'comppct': comppct,
                'interpretations': {}
            }

        if category_name not in organized[mukey][compname]['interpretations']:
            organized[mukey][compname]['interpretations'][category_name] = {}

        organized[mukey][compname]['interpretations'][category_name][clean_name] = rating

    return organized

def merge_organized_data(all_data, new_data):
    """Merge new category data into the main data structure."""
    for mukey, components in new_data.items():
        if mukey not in all_data:
            all_data[mukey] = {}

        for compname, comp_data in components.items():
            if compname not in all_data[mukey]:
                all_data[mukey][compname] = {
                    'comppct': comp_data['comppct'],
                    'interpretations': {}
                }

            for cat_name, interps in comp_data['interpretations'].items():
                if cat_name not in all_data[mukey][compname]['interpretations']:
                    all_data[mukey][compname]['interpretations'][cat_name] = {}
                all_data[mukey][compname]['interpretations'][cat_name].update(interps)

def main():
    script_dir = Path(__file__).parent
    project_dir = script_dir.parent
    geojson_path = project_dir / 'data' / 'CSNM_Polygons_WGS84_MajorComponents.geojson'
    output_path = project_dir / 'data' / 'soil_suitability_ratings.json'

    print("=" * 60)
    print("Soil Suitability Ratings Fetcher")
    print("=" * 60)

    # Get MUKEYs from GeoJSON
    print(f"\nReading MUKEYs from {geojson_path}...")
    mukeys = get_mukeys_from_geojson(geojson_path)
    print(f"Found {len(mukeys)} unique MUKEYs")

    # Fetch interpretations for each category
    all_data = {}

    print("\nFetching interpretations from SDA...")
    for category_name, pattern in CATEGORIES.items():
        results = fetch_interpretations_for_mukeys(mukeys, pattern, category_name)
        if results:
            organized = organize_by_mukey(results, category_name)
            merge_organized_data(all_data, organized)
        time.sleep(0.5)  # Be nice to the server

    # Add metadata
    output = {
        'metadata': {
            'source': 'USDA NRCS Soil Data Access (SDA)',
            'table': 'cointerp',
            'generated': time.strftime('%Y-%m-%d %H:%M:%S'),
            'mukey_count': len(mukeys),
            'categories': list(CATEGORIES.keys())
        },
        'data': all_data
    }

    # Save to JSON
    print(f"\nSaving to {output_path}...")
    with open(output_path, 'w') as f:
        json.dump(output, f, indent=2)

    # Summary
    total_interps = sum(
        len(comp['interpretations'].get(cat, {}))
        for mukey_data in all_data.values()
        for comp in mukey_data.values()
        for cat in CATEGORIES.keys()
    )

    print(f"\nDone!")
    print(f"  MUKEYs with data: {len(all_data)}")
    print(f"  Total interpretations: {total_interps}")
    print(f"  Output file: {output_path}")

if __name__ == '__main__':
    main()
