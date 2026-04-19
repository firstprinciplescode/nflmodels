"""
PFF Receiving Depth Weekly Scraper (with targets)
Scrapes weekly receiving depth data from PFF API.
Uses explicit week replacement to prevent data corruption.

Events:
  Full season: {"season": 2025}
  Single week: {"season": 2025, "week": 12}
  Week range:  {"season": 2025, "weeks": [10, 11, 12]}
"""

import json
import os
import boto3
import pandas as pd
from io import BytesIO
from datetime import datetime

# AWS clients
s3_client = boto3.client('s3')
secrets_client = boto3.client('secretsmanager')

# Configuration
BUCKET_NAME = "nfl-pff-data-lucas"
S3_PREFIX = "data/receiving_depth_weekly_targets"
SECRET_NAME = 'pff-api-cookies'

# PFF API endpoint
API_URL = "https://premium.pff.com/api/v1/facet/receiving/depth"

# All weeks including playoffs
ALL_WEEKS = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,28,29,30,32]


def get_cookies():
    """Get PFF cookies from Secrets Manager"""
    response = secrets_client.get_secret_value(SecretId=SECRET_NAME)
    return json.loads(response['SecretString'])


def fetch_receiving_depth(season, week, cookies):
    """Fetch receiving depth data from PFF API for a specific season and week."""
    import requests
    
    url = f"{API_URL}?league=nfl&season={season}&week={week}"
    
    try:
        response = requests.get(url, cookies=cookies, timeout=30)
        
        if response.status_code == 401:
            print(f"ERROR: 401 Unauthorized - cookies expired!")
            return None
        
        if response.status_code != 200:
            print(f"ERROR: Status {response.status_code} for season {season} week {week}")
            return None
        
        data = response.json()
        
        # Check for restricted response (expired cookie) - FAIL HARD
        if 'restricted' in data and data['restricted']:
            raise ValueError(f"COOKIE EXPIRED! API returned {len(data['restricted'])} restricted fields. Re-authenticate at premium.pff.com")
        
        # Extract receiving depth data
        players = data.get('receiving_depth', [])
        
        if not players:
            print(f"No data found for season {season} week {week}")
            return None
        
        # Validate data quality - check for key fields
        sample = players[0]
        required_fields = ['player_id', 'player', 'team', 'position']
        missing = [f for f in required_fields if f not in sample]
        if missing:
            print(f"WARNING: Missing required fields: {missing}")
            return None
        
        print(f"Fetched {len(players)} players for season {season} week {week}")
        return players
        
    except Exception as e:
        print(f"Error fetching data for season {season} week {week}: {e}")
        return None


def save_to_s3_by_season_week(data, season, week):
    """
    Save data to S3, partitioned by season with explicit week replacement.
    ALL NUMERICS AS FLOAT64.
    """
    if not data:
        print(f"No data to save for season {season}, week {week}")
        return False
    
    # Convert to DataFrame
    new_df = pd.DataFrame(data)
    
    # Add week column to new data
    new_df['week'] = week
    new_df['season'] = season
    
    print(f"New data: {len(new_df)} rows for week {week}")
    
    # Define the S3 path for this season
    s3_key = f"{S3_PREFIX}/season={season}/data.parquet"
    
    # Try to load existing data for this season
    existing_df = None
    try:
        response = s3_client.get_object(Bucket=BUCKET_NAME, Key=s3_key)
        existing_df = pd.read_parquet(BytesIO(response['Body'].read()))
        print(f"Existing data: {len(existing_df)} rows")
        
        # Explicitly remove this week's data before concat
        rows_before = len(existing_df)
        existing_df = existing_df[existing_df['week'] != week]
        rows_removed = rows_before - len(existing_df)
        print(f"Removed {rows_removed} existing rows for week {week}")
        
    except s3_client.exceptions.NoSuchKey:
        print(f"No existing file found, creating new")
        existing_df = None
    except Exception as e:
        print(f"Error reading existing file: {e}")
        existing_df = None
    
    # Combine data
    if existing_df is not None and len(existing_df) > 0:
        combined_df = pd.concat([existing_df, new_df], ignore_index=True)
    else:
        combined_df = new_df
    
    print(f"Combined data: {len(combined_df)} rows")
    
    # TYPE MAPPING - ALL NUMERICS AS FLOAT64
    type_mapping = {
        'player_id': 'float64',
        'season': 'float64',
        'week': 'float64',
        'targets': 'float64',
        'base_targets': 'float64',
        'wide_snaps': 'float64',
        'slot_snaps': 'float64',
        'inline_snaps': 'float64',
        'player_game_count': 'float64',
        'franchise_id': 'float64',
        'draft_season': 'float64',
        'eligible_season': 'float64',
        'penalties': 'float64',
        'declined_penalties': 'float64',
        # Routes
        'left_deep_routes': 'float64',
        'left_medium_routes': 'float64',
        'left_short_routes': 'float64',
        'left_behind_los_routes': 'float64',
        'center_deep_routes': 'float64',
        'center_medium_routes': 'float64',
        'center_short_routes': 'float64',
        'center_behind_los_routes': 'float64',
        'right_deep_routes': 'float64',
        'right_medium_routes': 'float64',
        'right_short_routes': 'float64',
        'right_behind_los_routes': 'float64',
        'behind_los_routes': 'float64',
        'short_routes': 'float64',
        'medium_routes': 'float64',
        'deep_routes': 'float64',
        # Targets
        'left_deep_targets': 'float64',
        'left_medium_targets': 'float64',
        'left_short_targets': 'float64',
        'left_behind_los_targets': 'float64',
        'center_deep_targets': 'float64',
        'center_medium_targets': 'float64',
        'center_short_targets': 'float64',
        'center_behind_los_targets': 'float64',
        'right_deep_targets': 'float64',
        'right_medium_targets': 'float64',
        'right_short_targets': 'float64',
        'right_behind_los_targets': 'float64',
        'behind_los_targets': 'float64',
        'short_targets': 'float64',
        'medium_targets': 'float64',
        'deep_targets': 'float64',
        # Receptions
        'left_deep_receptions': 'float64',
        'left_medium_receptions': 'float64',
        'left_short_receptions': 'float64',
        'left_behind_los_receptions': 'float64',
        'center_deep_receptions': 'float64',
        'center_medium_receptions': 'float64',
        'center_short_receptions': 'float64',
        'center_behind_los_receptions': 'float64',
        'right_deep_receptions': 'float64',
        'right_medium_receptions': 'float64',
        'right_short_receptions': 'float64',
        'right_behind_los_receptions': 'float64',
        'behind_los_receptions': 'float64',
        'short_receptions': 'float64',
        'medium_receptions': 'float64',
        'deep_receptions': 'float64',
        # Yards
        'left_deep_yards': 'float64',
        'left_medium_yards': 'float64',
        'left_short_yards': 'float64',
        'left_behind_los_yards': 'float64',
        'center_deep_yards': 'float64',
        'center_medium_yards': 'float64',
        'center_short_yards': 'float64',
        'center_behind_los_yards': 'float64',
        'right_deep_yards': 'float64',
        'right_medium_yards': 'float64',
        'right_short_yards': 'float64',
        'right_behind_los_yards': 'float64',
        'behind_los_yards': 'float64',
        'short_yards': 'float64',
        'medium_yards': 'float64',
        'deep_yards': 'float64',
        # YAC
        'left_deep_yards_after_catch': 'float64',
        'left_medium_yards_after_catch': 'float64',
        'left_short_yards_after_catch': 'float64',
        'left_behind_los_yards_after_catch': 'float64',
        'center_deep_yards_after_catch': 'float64',
        'center_medium_yards_after_catch': 'float64',
        'center_short_yards_after_catch': 'float64',
        'center_behind_los_yards_after_catch': 'float64',
        'right_deep_yards_after_catch': 'float64',
        'right_medium_yards_after_catch': 'float64',
        'right_short_yards_after_catch': 'float64',
        'right_behind_los_yards_after_catch': 'float64',
        'behind_los_yards_after_catch': 'float64',
        'short_yards_after_catch': 'float64',
        'medium_yards_after_catch': 'float64',
        'deep_yards_after_catch': 'float64',
        # Touchdowns
        'left_deep_touchdowns': 'float64',
        'left_medium_touchdowns': 'float64',
        'left_short_touchdowns': 'float64',
        'left_behind_los_touchdowns': 'float64',
        'center_deep_touchdowns': 'float64',
        'center_medium_touchdowns': 'float64',
        'center_short_touchdowns': 'float64',
        'center_behind_los_touchdowns': 'float64',
        'right_deep_touchdowns': 'float64',
        'right_medium_touchdowns': 'float64',
        'right_short_touchdowns': 'float64',
        'right_behind_los_touchdowns': 'float64',
        'behind_los_touchdowns': 'float64',
        'short_touchdowns': 'float64',
        'medium_touchdowns': 'float64',
        'deep_touchdowns': 'float64',
        # First downs
        'left_deep_first_downs': 'float64',
        'left_medium_first_downs': 'float64',
        'left_short_first_downs': 'float64',
        'left_behind_los_first_downs': 'float64',
        'center_deep_first_downs': 'float64',
        'center_medium_first_downs': 'float64',
        'center_short_first_downs': 'float64',
        'center_behind_los_first_downs': 'float64',
        'right_deep_first_downs': 'float64',
        'right_medium_first_downs': 'float64',
        'right_short_first_downs': 'float64',
        'right_behind_los_first_downs': 'float64',
        'behind_los_first_downs': 'float64',
        'short_first_downs': 'float64',
        'medium_first_downs': 'float64',
        'deep_first_downs': 'float64',
        # Drops
        'left_deep_drops': 'float64',
        'left_medium_drops': 'float64',
        'left_short_drops': 'float64',
        'left_behind_los_drops': 'float64',
        'center_deep_drops': 'float64',
        'center_medium_drops': 'float64',
        'center_short_drops': 'float64',
        'center_behind_los_drops': 'float64',
        'right_deep_drops': 'float64',
        'right_medium_drops': 'float64',
        'right_short_drops': 'float64',
        'right_behind_los_drops': 'float64',
        'behind_los_drops': 'float64',
        'short_drops': 'float64',
        'medium_drops': 'float64',
        'deep_drops': 'float64',
        # Interceptions
        'left_deep_interceptions': 'float64',
        'left_medium_interceptions': 'float64',
        'left_short_interceptions': 'float64',
        'left_behind_los_interceptions': 'float64',
        'center_deep_interceptions': 'float64',
        'center_medium_interceptions': 'float64',
        'center_short_interceptions': 'float64',
        'center_behind_los_interceptions': 'float64',
        'right_deep_interceptions': 'float64',
        'right_medium_interceptions': 'float64',
        'right_short_interceptions': 'float64',
        'right_behind_los_interceptions': 'float64',
        'behind_los_interceptions': 'float64',
        'short_interceptions': 'float64',
        'medium_interceptions': 'float64',
        'deep_interceptions': 'float64',
        # Fumbles
        'left_deep_fumbles': 'float64',
        'left_medium_fumbles': 'float64',
        'left_short_fumbles': 'float64',
        'left_behind_los_fumbles': 'float64',
        'center_deep_fumbles': 'float64',
        'center_medium_fumbles': 'float64',
        'center_short_fumbles': 'float64',
        'center_behind_los_fumbles': 'float64',
        'right_deep_fumbles': 'float64',
        'right_medium_fumbles': 'float64',
        'right_short_fumbles': 'float64',
        'right_behind_los_fumbles': 'float64',
        'behind_los_fumbles': 'float64',
        'short_fumbles': 'float64',
        'medium_fumbles': 'float64',
        'deep_fumbles': 'float64',
        # Contested targets
        'left_deep_contested_targets': 'float64',
        'left_medium_contested_targets': 'float64',
        'left_short_contested_targets': 'float64',
        'left_behind_los_contested_targets': 'float64',
        'center_deep_contested_targets': 'float64',
        'center_medium_contested_targets': 'float64',
        'center_short_contested_targets': 'float64',
        'center_behind_los_contested_targets': 'float64',
        'right_deep_contested_targets': 'float64',
        'right_medium_contested_targets': 'float64',
        'right_short_contested_targets': 'float64',
        'right_behind_los_contested_targets': 'float64',
        'behind_los_contested_targets': 'float64',
        'short_contested_targets': 'float64',
        'medium_contested_targets': 'float64',
        'deep_contested_targets': 'float64',
        # Contested receptions
        'left_deep_contested_receptions': 'float64',
        'left_medium_contested_receptions': 'float64',
        'left_short_contested_receptions': 'float64',
        'left_behind_los_contested_receptions': 'float64',
        'center_deep_contested_receptions': 'float64',
        'center_medium_contested_receptions': 'float64',
        'center_short_contested_receptions': 'float64',
        'center_behind_los_contested_receptions': 'float64',
        'right_deep_contested_receptions': 'float64',
        'right_medium_contested_receptions': 'float64',
        'right_short_contested_receptions': 'float64',
        'right_behind_los_contested_receptions': 'float64',
        'behind_los_contested_receptions': 'float64',
        'short_contested_receptions': 'float64',
        'medium_contested_receptions': 'float64',
        'deep_contested_receptions': 'float64',
        # Avoided tackles
        'left_deep_avoided_tackles': 'float64',
        'left_medium_avoided_tackles': 'float64',
        'left_short_avoided_tackles': 'float64',
        'left_behind_los_avoided_tackles': 'float64',
        'center_deep_avoided_tackles': 'float64',
        'center_medium_avoided_tackles': 'float64',
        'center_short_avoided_tackles': 'float64',
        'center_behind_los_avoided_tackles': 'float64',
        'right_deep_avoided_tackles': 'float64',
        'right_medium_avoided_tackles': 'float64',
        'right_short_avoided_tackles': 'float64',
        'right_behind_los_avoided_tackles': 'float64',
        'behind_los_avoided_tackles': 'float64',
        'short_avoided_tackles': 'float64',
        'medium_avoided_tackles': 'float64',
        'deep_avoided_tackles': 'float64',
        # Longest
        'left_deep_longest': 'float64',
        'left_medium_longest': 'float64',
        'left_short_longest': 'float64',
        'left_behind_los_longest': 'float64',
        'center_deep_longest': 'float64',
        'center_medium_longest': 'float64',
        'center_short_longest': 'float64',
        'center_behind_los_longest': 'float64',
        'right_deep_longest': 'float64',
        'right_medium_longest': 'float64',
        'right_short_longest': 'float64',
        'right_behind_los_longest': 'float64',
        'behind_los_longest': 'float64',
        'short_longest': 'float64',
        'medium_longest': 'float64',
        'deep_longest': 'float64',
        # Pass blocks
        'left_deep_pass_blocks': 'float64',
        'left_medium_pass_blocks': 'float64',
        'left_short_pass_blocks': 'float64',
        'left_behind_los_pass_blocks': 'float64',
        'center_deep_pass_blocks': 'float64',
        'center_medium_pass_blocks': 'float64',
        'center_short_pass_blocks': 'float64',
        'center_behind_los_pass_blocks': 'float64',
        'right_deep_pass_blocks': 'float64',
        'right_medium_pass_blocks': 'float64',
        'right_short_pass_blocks': 'float64',
        'right_behind_los_pass_blocks': 'float64',
        'behind_los_pass_blocks': 'float64',
        'short_pass_blocks': 'float64',
        'medium_pass_blocks': 'float64',
        'deep_pass_blocks': 'float64',
        # Pass plays
        'left_deep_pass_plays': 'float64',
        'left_medium_pass_plays': 'float64',
        'left_short_pass_plays': 'float64',
        'left_behind_los_pass_plays': 'float64',
        'center_deep_pass_plays': 'float64',
        'center_medium_pass_plays': 'float64',
        'center_short_pass_plays': 'float64',
        'center_behind_los_pass_plays': 'float64',
        'right_deep_pass_plays': 'float64',
        'right_medium_pass_plays': 'float64',
        'right_short_pass_plays': 'float64',
        'right_behind_los_pass_plays': 'float64',
        'behind_los_pass_plays': 'float64',
        'short_pass_plays': 'float64',
        'medium_pass_plays': 'float64',
        'deep_pass_plays': 'float64',
        # Rate fields (already float)
        'left_deep_yards_per_reception': 'float64',
        'left_medium_yards_per_reception': 'float64',
        'left_short_yards_per_reception': 'float64',
        'left_behind_los_yards_per_reception': 'float64',
        'center_deep_yards_per_reception': 'float64',
        'center_medium_yards_per_reception': 'float64',
        'center_short_yards_per_reception': 'float64',
        'center_behind_los_yards_per_reception': 'float64',
        'right_deep_yards_per_reception': 'float64',
        'right_medium_yards_per_reception': 'float64',
        'right_short_yards_per_reception': 'float64',
        'right_behind_los_yards_per_reception': 'float64',
        'behind_los_yards_per_reception': 'float64',
        'short_yards_per_reception': 'float64',
        'medium_yards_per_reception': 'float64',
        'deep_yards_per_reception': 'float64',
        # YAC per reception
        'left_deep_yards_after_catch_per_reception': 'float64',
        'left_medium_yards_after_catch_per_reception': 'float64',
        'left_short_yards_after_catch_per_reception': 'float64',
        'left_behind_los_yards_after_catch_per_reception': 'float64',
        'center_deep_yards_after_catch_per_reception': 'float64',
        'center_medium_yards_after_catch_per_reception': 'float64',
        'center_short_yards_after_catch_per_reception': 'float64',
        'center_behind_los_yards_after_catch_per_reception': 'float64',
        'right_deep_yards_after_catch_per_reception': 'float64',
        'right_medium_yards_after_catch_per_reception': 'float64',
        'right_short_yards_after_catch_per_reception': 'float64',
        'right_behind_los_yards_after_catch_per_reception': 'float64',
        'behind_los_yards_after_catch_per_reception': 'float64',
        'short_yards_after_catch_per_reception': 'float64',
        'medium_yards_after_catch_per_reception': 'float64',
        'deep_yards_after_catch_per_reception': 'float64',
        # YPRR
        'left_deep_yprr': 'float64',
        'left_medium_yprr': 'float64',
        'left_short_yprr': 'float64',
        'left_behind_los_yprr': 'float64',
        'center_deep_yprr': 'float64',
        'center_medium_yprr': 'float64',
        'center_short_yprr': 'float64',
        'center_behind_los_yprr': 'float64',
        'right_deep_yprr': 'float64',
        'right_medium_yprr': 'float64',
        'right_short_yprr': 'float64',
        'right_behind_los_yprr': 'float64',
        'behind_los_yprr': 'float64',
        'short_yprr': 'float64',
        'medium_yprr': 'float64',
        'deep_yprr': 'float64',
        # aDOT
        'left_deep_avg_depth_of_target': 'float64',
        'left_medium_avg_depth_of_target': 'float64',
        'left_short_avg_depth_of_target': 'float64',
        'left_behind_los_avg_depth_of_target': 'float64',
        'center_deep_avg_depth_of_target': 'float64',
        'center_medium_avg_depth_of_target': 'float64',
        'center_short_avg_depth_of_target': 'float64',
        'center_behind_los_avg_depth_of_target': 'float64',
        'right_deep_avg_depth_of_target': 'float64',
        'right_medium_avg_depth_of_target': 'float64',
        'right_short_avg_depth_of_target': 'float64',
        'right_behind_los_avg_depth_of_target': 'float64',
        'behind_los_avg_depth_of_target': 'float64',
        'short_avg_depth_of_target': 'float64',
        'medium_avg_depth_of_target': 'float64',
        'deep_avg_depth_of_target': 'float64',
        # Targeted QB rating
        'left_deep_targeted_qb_rating': 'float64',
        'left_medium_targeted_qb_rating': 'float64',
        'left_short_targeted_qb_rating': 'float64',
        'left_behind_los_targeted_qb_rating': 'float64',
        'center_deep_targeted_qb_rating': 'float64',
        'center_medium_targeted_qb_rating': 'float64',
        'center_short_targeted_qb_rating': 'float64',
        'center_behind_los_targeted_qb_rating': 'float64',
        'right_deep_targeted_qb_rating': 'float64',
        'right_medium_targeted_qb_rating': 'float64',
        'right_short_targeted_qb_rating': 'float64',
        'right_behind_los_targeted_qb_rating': 'float64',
        'behind_los_targeted_qb_rating': 'float64',
        'short_targeted_qb_rating': 'float64',
        'medium_targeted_qb_rating': 'float64',
        'deep_targeted_qb_rating': 'float64',
        # Catch percent
        'left_deep_caught_percent': 'float64',
        'left_medium_caught_percent': 'float64',
        'left_short_caught_percent': 'float64',
        'left_behind_los_caught_percent': 'float64',
        'center_deep_caught_percent': 'float64',
        'center_medium_caught_percent': 'float64',
        'center_short_caught_percent': 'float64',
        'center_behind_los_caught_percent': 'float64',
        'right_deep_caught_percent': 'float64',
        'right_medium_caught_percent': 'float64',
        'right_short_caught_percent': 'float64',
        'right_behind_los_caught_percent': 'float64',
        'behind_los_caught_percent': 'float64',
        'short_caught_percent': 'float64',
        'medium_caught_percent': 'float64',
        'deep_caught_percent': 'float64',
        # Drop rate
        'left_deep_drop_rate': 'float64',
        'left_medium_drop_rate': 'float64',
        'left_short_drop_rate': 'float64',
        'left_behind_los_drop_rate': 'float64',
        'center_deep_drop_rate': 'float64',
        'center_medium_drop_rate': 'float64',
        'center_short_drop_rate': 'float64',
        'center_behind_los_drop_rate': 'float64',
        'right_deep_drop_rate': 'float64',
        'right_medium_drop_rate': 'float64',
        'right_short_drop_rate': 'float64',
        'right_behind_los_drop_rate': 'float64',
        'behind_los_drop_rate': 'float64',
        'short_drop_rate': 'float64',
        'medium_drop_rate': 'float64',
        'deep_drop_rate': 'float64',
        # Contested catch rate
        'left_deep_contested_catch_rate': 'float64',
        'left_medium_contested_catch_rate': 'float64',
        'left_short_contested_catch_rate': 'float64',
        'left_behind_los_contested_catch_rate': 'float64',
        'center_deep_contested_catch_rate': 'float64',
        'center_medium_contested_catch_rate': 'float64',
        'center_short_contested_catch_rate': 'float64',
        'center_behind_los_contested_catch_rate': 'float64',
        'right_deep_contested_catch_rate': 'float64',
        'right_medium_contested_catch_rate': 'float64',
        'right_short_contested_catch_rate': 'float64',
        'right_behind_los_contested_catch_rate': 'float64',
        'behind_los_contested_catch_rate': 'float64',
        'short_contested_catch_rate': 'float64',
        'medium_contested_catch_rate': 'float64',
        'deep_contested_catch_rate': 'float64',
        # Route rate
        'left_deep_route_rate': 'float64',
        'left_medium_route_rate': 'float64',
        'left_short_route_rate': 'float64',
        'left_behind_los_route_rate': 'float64',
        'center_deep_route_rate': 'float64',
        'center_medium_route_rate': 'float64',
        'center_short_route_rate': 'float64',
        'center_behind_los_route_rate': 'float64',
        'right_deep_route_rate': 'float64',
        'right_medium_route_rate': 'float64',
        'right_short_route_rate': 'float64',
        'right_behind_los_route_rate': 'float64',
        'behind_los_route_rate': 'float64',
        'short_route_rate': 'float64',
        'medium_route_rate': 'float64',
        'deep_route_rate': 'float64',
        # Pass block rate
        'left_deep_pass_block_rate': 'float64',
        'left_medium_pass_block_rate': 'float64',
        'left_short_pass_block_rate': 'float64',
        'left_behind_los_pass_block_rate': 'float64',
        'center_deep_pass_block_rate': 'float64',
        'center_medium_pass_block_rate': 'float64',
        'center_short_pass_block_rate': 'float64',
        'center_behind_los_pass_block_rate': 'float64',
        'right_deep_pass_block_rate': 'float64',
        'right_medium_pass_block_rate': 'float64',
        'right_short_pass_block_rate': 'float64',
        'right_behind_los_pass_block_rate': 'float64',
        'behind_los_pass_block_rate': 'float64',
        'short_pass_block_rate': 'float64',
        'medium_pass_block_rate': 'float64',
        'deep_pass_block_rate': 'float64',
        # Target percent
        'left_deep_targets_percent': 'float64',
        'left_medium_targets_percent': 'float64',
        'left_short_targets_percent': 'float64',
        'left_behind_los_targets_percent': 'float64',
        'center_deep_targets_percent': 'float64',
        'center_medium_targets_percent': 'float64',
        'center_short_targets_percent': 'float64',
        'center_behind_los_targets_percent': 'float64',
        'right_deep_targets_percent': 'float64',
        'right_medium_targets_percent': 'float64',
        'right_short_targets_percent': 'float64',
        'right_behind_los_targets_percent': 'float64',
        'behind_los_targets_percent': 'float64',
        'short_targets_percent': 'float64',
        'medium_targets_percent': 'float64',
        'deep_targets_percent': 'float64',
        # Grades pass route
        'left_deep_grades_pass_route': 'float64',
        'left_medium_grades_pass_route': 'float64',
        'left_short_grades_pass_route': 'float64',
        'left_behind_los_grades_pass_route': 'float64',
        'center_deep_grades_pass_route': 'float64',
        'center_medium_grades_pass_route': 'float64',
        'center_short_grades_pass_route': 'float64',
        'center_behind_los_grades_pass_route': 'float64',
        'right_deep_grades_pass_route': 'float64',
        'right_medium_grades_pass_route': 'float64',
        'right_short_grades_pass_route': 'float64',
        'right_behind_los_grades_pass_route': 'float64',
        'behind_los_grades_pass_route': 'float64',
        'short_grades_pass_route': 'float64',
        'medium_grades_pass_route': 'float64',
        'deep_grades_pass_route': 'float64',
        # Grades hands drop
        'left_deep_grades_hands_drop': 'float64',
        'left_medium_grades_hands_drop': 'float64',
        'left_short_grades_hands_drop': 'float64',
        'left_behind_los_grades_hands_drop': 'float64',
        'center_deep_grades_hands_drop': 'float64',
        'center_medium_grades_hands_drop': 'float64',
        'center_short_grades_hands_drop': 'float64',
        'center_behind_los_grades_hands_drop': 'float64',
        'right_deep_grades_hands_drop': 'float64',
        'right_medium_grades_hands_drop': 'float64',
        'right_short_grades_hands_drop': 'float64',
        'right_behind_los_grades_hands_drop': 'float64',
        'behind_los_grades_hands_drop': 'float64',
        'short_grades_hands_drop': 'float64',
        'medium_grades_hands_drop': 'float64',
        'deep_grades_hands_drop': 'float64',
        # String fields
        'team_name': 'str',
        'team': 'str',
        'player': 'str',
        'position': 'str',
        'jersey_number': 'str',
    }
    
    # Apply types - only for columns that exist
    for col, dtype in type_mapping.items():
        if col in combined_df.columns:
            try:
                combined_df[col] = combined_df[col].astype(dtype)
            except Exception as e:
                print(f"Warning: Could not convert {col} to {dtype}: {e}")
    
    # Sort by week, team, player
    sort_cols = [c for c in ['week', 'team', 'player'] if c in combined_df.columns]
    if sort_cols:
        combined_df = combined_df.sort_values(sort_cols).reset_index(drop=True)
    
    # Save to S3
    try:
        parquet_buffer = BytesIO()
        combined_df.to_parquet(
            parquet_buffer,
            index=False,
            engine='pyarrow',
            compression='snappy'
        )
        parquet_buffer.seek(0)
        
        s3_client.put_object(
            Bucket=BUCKET_NAME,
            Key=s3_key,
            Body=parquet_buffer.getvalue(),
            ContentType='application/octet-stream'
        )
        
        print(f"Successfully saved {len(combined_df)} rows to s3://{BUCKET_NAME}/{s3_key}")
        
        # Print week breakdown
        if 'week' in combined_df.columns:
            week_counts = combined_df['week'].value_counts().sort_index()
            print(f"Rows by week: {week_counts.to_dict()}")
        
        return True
    except Exception as e:
        print(f"Error saving to S3: {e}")
        return False


def lambda_handler(event, context):
    """AWS Lambda entry point.
    
    Events:
      Full season: {"season": 2025}
      Single week: {"season": 2025, "week": 12}
      Week range:  {"season": 2025, "weeks": [10, 11, 12]}
    """
    # Get parameters from event
    season = event.get('season', datetime.now().year)
    season = int(season)
    
    # Determine weeks to process
    if 'week' in event:
        # Single week
        weeks = [int(event['week'])]
    elif 'weeks' in event:
        # List of weeks
        weeks = [int(w) for w in event['weeks']]
    else:
        # Full season - all weeks including playoffs
        weeks = ALL_WEEKS
    
    print(f"Processing season {season}, weeks {weeks}")
    
    # Get cookies
    try:
        cookies = get_cookies()
    except Exception as e:
        return {
            'statusCode': 500,
            'body': json.dumps({'error': str(e)})
        }
    
    # Process each week
    results = []
    for week in weeks:
        print(f"\n{'='*50}")
        print(f"Processing week {week}")
        print(f"{'='*50}")
        
        # Fetch data
        data = fetch_receiving_depth(season, week, cookies)
        
        if data is None:
            results.append({'week': week, 'status': 'no_data', 'rows': 0})
            continue
        
        # Save to S3
        success = save_to_s3_by_season_week(data, season, week)
        
        if success:
            results.append({'week': week, 'status': 'success', 'rows': len(data)})
        else:
            results.append({'week': week, 'status': 'failed', 'rows': 0})
    
    # Summary
    total_rows = sum(r['rows'] for r in results)
    successful_weeks = sum(1 for r in results if r['status'] == 'success')
    
    return {
        'statusCode': 200,
        'body': json.dumps({
            'message': f'Processed {successful_weeks}/{len(weeks)} weeks for season {season}',
            'season': season,
            'total_rows': total_rows,
            'results': results
        })
    }