#!/usr/bin/env python3
"""
Generate map unit statistics from the full SSURGO GeoJSON file.

This script processes all components for each map unit and calculates
aggregate statistics like hydric soil proportion, drainage classes, etc.
The output is a lightweight JSON file that can be used for the SoilWeb
detail panel without loading the massive full GeoJSON file.
"""

import json
from pathlib import Path
from collections import defaultdict

def calculate_mapunit_stats(geojson_path):
    """
    Process full GeoJSON and calculate statistics for each map unit.
    """
    print(f"Loading {geojson_path}...")
    with open(geojson_path, 'r') as f:
        data = json.load(f)

    # Group components by MUKEY
    mapunits = defaultdict(lambda: {
        'components': [],
        'musym': None,
        'muname': None,
        'areasymbol': None,
        'spatialver': None
    })

    for feature in data['features']:
        props = feature['properties']
        mukey = props.get('MUKEY') or props.get('mukey')

        if not mukey:
            continue

        # Store map unit symbol and name
        if not mapunits[mukey]['musym']:
            mapunits[mukey]['musym'] = props.get('MUSYM') or props.get('musym')
        if not mapunits[mukey]['muname']:
            mapunits[mukey]['muname'] = props.get('muname')
        if not mapunits[mukey]['areasymbol']:
            mapunits[mukey]['areasymbol'] = props.get('AREASYMBOL') or props.get('areasymbol')
        if not mapunits[mukey]['spatialver']:
            mapunits[mukey]['spatialver'] = props.get('SPATIALVER') or props.get('spatialver')

        # Create component key to avoid duplicates
        comp_key = f"{props.get('compname')}_{props.get('comppct_r')}"

        # Check if we've already added this component
        existing_keys = [f"{c.get('compname')}_{c.get('comppct_r')}"
                        for c in mapunits[mukey]['components']]

        if comp_key not in existing_keys:
            mapunits[mukey]['components'].append({
                'compname': props.get('compname'),
                'comppct_r': props.get('comppct_r'),
                'hydricrating': props.get('hydricrating'),
                'drainagecl': props.get('drainagecl'),
                'hydgrp': props.get('hydgrp'),
            })

    print(f"Found {len(mapunits)} unique map units")

    # Calculate statistics for each map unit
    stats = {}

    for mukey, mu_data in mapunits.items():
        components = mu_data['components']

        if not components:
            continue

        # Calculate hydric soil proportion
        hydric_pct = 0
        total_pct = 0

        # Count drainage classes
        drainage_counts = defaultdict(int)

        # Count hydrologic groups
        hydgrp_counts = defaultdict(int)

        for comp in components:
            pct = comp['comppct_r'] if isinstance(comp['comppct_r'], (int, float)) else 0
            total_pct += pct

            # Hydric soils
            if comp['hydricrating'] == 'Yes':
                hydric_pct += pct

            # Drainage classes
            if comp['drainagecl']:
                drainage_counts[comp['drainagecl']] += pct

            # Hydrologic groups
            if comp['hydgrp']:
                hydgrp_counts[comp['hydgrp']] += pct

        # Calculate hydric percentage
        if total_pct > 0:
            hydric_proportion = round((hydric_pct / total_pct) * 100)
        else:
            hydric_proportion = 0

        # Get dominant drainage class
        dominant_drainage = max(drainage_counts.items(), key=lambda x: x[1])[0] if drainage_counts else None

        # Get wettest drainage class (rank from wettest to driest)
        drainage_order = [
            'Very poorly drained',
            'Poorly drained',
            'Somewhat poorly drained',
            'Moderately well drained',
            'Well drained',
            'Somewhat excessively drained',
            'Excessively drained'
        ]

        wettest_drainage = None
        for drainage in drainage_order:
            if drainage in drainage_counts and drainage_counts[drainage] > 0:
                wettest_drainage = drainage
                break

        # Get dominant hydrologic group
        dominant_hydgrp = max(hydgrp_counts.items(), key=lambda x: x[1])[0] if hydgrp_counts else None

        # Store statistics
        stats[mukey] = {
            'musym': mu_data['musym'],
            'muname': mu_data['muname'],
            'hydricSoilsProportion': f"{hydric_proportion}%",
            'dominantDrainage': dominant_drainage,
            'wettestDrainage': wettest_drainage,
            'dominantHydgrp': dominant_hydgrp,
            'componentCount': len(components),
            'areaSymbol': mu_data['areasymbol'],
            'spatialVersion': mu_data['spatialver']
        }

    return stats

def main():
    # Set up paths
    project_dir = Path(__file__).parent.parent
    input_file = project_dir / 'data' / 'CSNM_Polygons_WGS84.geojson'
    output_file = project_dir / 'data' / 'mapunit-statistics.json'

    if not input_file.exists():
        print(f"Error: {input_file} not found!")
        return

    # Calculate statistics
    stats = calculate_mapunit_stats(input_file)

    # Write output
    print(f"\nWriting statistics to {output_file}...")
    with open(output_file, 'w') as f:
        json.dump(stats, f, indent=2)

    # Print summary
    print(f"\n✅ Generated statistics for {len(stats)} map units")

    # Show some examples
    print("\nSample map unit statistics:")
    for i, (mukey, data) in enumerate(list(stats.items())[:3]):
        print(f"\n  MUKEY {mukey}:")
        print(f"    Symbol: {data['musym']}")
        print(f"    Hydric soils: {data['hydricSoilsProportion']}")
        print(f"    Dominant drainage: {data['dominantDrainage']}")
        print(f"    Wettest drainage: {data['wettestDrainage']}")
        print(f"    Hydrologic group: {data['dominantHydgrp']}")
        print(f"    Components: {data['componentCount']}")

    print(f"\nOutput file size: {output_file.stat().st_size / 1024:.1f} KB")

if __name__ == '__main__':
    main()
