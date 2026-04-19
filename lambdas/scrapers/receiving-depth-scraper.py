import json
import boto3
import requests
import pandas as pd
from io import BytesIO
from datetime import datetime
import time

# AWS clients
secrets_client = boto3.client('secretsmanager')
s3_client = boto3.client('s3')
athena_client = boto3.client('athena')

# Configuration
BUCKET_NAME = 'nfl-pff-data-lucas'
SECRET_NAME = 'pff-api-cookies'
ATHENA_OUTPUT = 's3://nfl-pff-data-lucas/athena-results/'

def get_cookies():
    """Get PFF cookies from Secrets Manager"""
    response = secrets_client.get_secret_value(SecretId=SECRET_NAME)
    return json.loads(response['SecretString'])

def check_restricted_access(cookies):
    """
    Check if cookies have access to restricted endpoints by testing a known player
    Returns: True if restricted access, False otherwise
    Raises: Exception if cookies are invalid/expired (401)
    """
    test_player_id = 48294  # Justin Jefferson
    test_season = 2024
    test_week = 1
    
    restricted_url = f'https://premium.pff.com/api/v1/player/receiving/depth?league=nfl&season={test_season}&week={test_week}&player_id={test_player_id}'
    
    try:
        response = requests.get(restricted_url, cookies=cookies, timeout=10)
        
        if response.status_code == 401:
            raise Exception("401 Unauthorized - cookies expired!")
        
        if response.status_code == 200:
            data = response.json()
            # Check if response has restricted fields
            if data.get('receiving_depth') and len(data['receiving_depth']) > 0:
                first_record = data['receiving_depth'][0]
                # Restricted endpoint includes player metadata
                has_restricted = 'player' in first_record and 'position' in first_record
                print(f"✓ Access level: {'RESTRICTED (with metadata)' if has_restricted else 'STANDARD'}")
                return has_restricted
        
        print("✓ Access level: STANDARD")
        return False
        
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e):
            raise  # Re-raise auth errors immediately
        print(f"Warning: Could not determine access level, defaulting to STANDARD: {e}")
        return False

def query_athena_for_players(season, min_player_id=None, max_player_id=None):
    """
    Query Athena view to get all player_ids for a given season
    
    Args:
        season: NFL season year
        min_player_id: Minimum player_id to include (inclusive). If None, no lower bound.
        max_player_id: Maximum player_id to include (inclusive). If None, no upper bound.
    
    Returns: list of player_id integers
    """
    
    # Base query
    query = f"""
        SELECT DISTINCT player_id
        FROM nfl_data.vw_players_with_routes
        WHERE season = {season}
    """
    
    # Add player_id filtering if specified
    if min_player_id is not None:
        query += f" AND player_id >= {min_player_id}"
    if max_player_id is not None:
        query += f" AND player_id <= {max_player_id}"
    
    if min_player_id is not None or max_player_id is not None:
        print(f"Querying Athena for season {season} players (player_id: {min_player_id or 'all'} to {max_player_id or 'all'})...")
    else:
        print(f"Querying Athena for season {season} players (ALL)...")
    
    # Start query
    response = athena_client.start_query_execution(
        QueryString=query,
        QueryExecutionContext={'Database': 'nfl_data'},
        ResultConfiguration={'OutputLocation': ATHENA_OUTPUT}
    )
    
    query_execution_id = response['QueryExecutionId']
    
    # Wait for completion
    while True:
        result = athena_client.get_query_execution(QueryExecutionId=query_execution_id)
        status = result['QueryExecution']['Status']['State']
        
        if status in ['SUCCEEDED', 'FAILED', 'CANCELLED']:
            break
        time.sleep(1)
    
    if status != 'SUCCEEDED':
        error_info = result['QueryExecution']['Status'].get('StateChangeReason', 'Unknown error')
        raise Exception(f"Athena query failed: {status} - {error_info}")
    
    # Get results
    results = athena_client.get_query_results(QueryExecutionId=query_execution_id)
    
    player_ids = []
    for row in results['ResultSet']['Rows'][1:]:  # Skip header
        player_id = row['Data'][0].get('VarCharValue')
        if player_id:
            player_ids.append(int(player_id))
    
    print(f"✓ Found {len(player_ids)} players for season {season}")
    return player_ids

def scrape_receiving_depth_for_player(player_id, season, cookies, use_restricted=False):
    """
    Scrape receiving depth data for one player/season (ALL WEEKS)
    Returns: DataFrame with depth breakdown data
    """
    # All weeks in one call: regular season (1-18) + playoffs (28,29,30,32)
    url = f'https://premium.pff.com/api/v1/player/receiving/depth?league=nfl&season={season}&week=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,28,29,30,32&player_id={player_id}'
    
    try:
        response = requests.get(url, cookies=cookies, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            
            # Extract the "receiving_depth" array
            depth_data = data.get('receiving_depth', [])
            
            if not depth_data:
                # Player had routes but no depth data
                return pd.DataFrame()
            
            # Add metadata to each record
            for record in depth_data:
                # Only add these if not already present (restricted endpoint includes them)
                if 'player_id' not in record:
                    record['player_id'] = player_id
                if 'season' not in record:
                    record['season'] = season
                record['scraped_at'] = datetime.utcnow().isoformat()
            
            return pd.DataFrame(depth_data)
        
        elif response.status_code == 401:
            raise Exception("401 Unauthorized - cookies expired!")
        else:
            # Don't log every single failure, just return empty
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e):
            raise  # Re-raise auth errors to stop execution
        # Silently handle other errors
        return pd.DataFrame()

def scrape_all_receiving_depth(cookies, player_ids, season, use_restricted=False):
    """Scrape receiving depth data for all players in a season (FULL SEASON)"""
    all_data = []
    total = len(player_ids)
    
    print(f"\nScraping {total} players for season {season} (all weeks)...")
    
    for idx, player_id in enumerate(player_ids, 1):
        if idx % 100 == 0:
            print(f"Progress: {idx}/{total} players...")
        
        df = scrape_receiving_depth_for_player(player_id, season, cookies, use_restricted)
        
        if not df.empty:
            all_data.append(df)
        
        # Rate limiting - don't hammer the API
        time.sleep(0.1)
    
    if all_data:
        combined = pd.concat(all_data, ignore_index=True)
        print(f"✓ Scraped {len(combined)} receiving depth records")
        return combined
    else:
        return pd.DataFrame()

def save_to_s3_by_season(df, season, min_player_id=None, max_player_id=None):
    """
    Save receiving depth data to S3, partitioned by season
    If player_id filtering is used, includes range in path to avoid overwrites
    
    Structure: 
    - Full season: data/receiving_depth/season=2025/data.parquet
    - Batched: data/receiving_depth/season=2025/batch_1_50000/data.parquet
    """
    
    # ========================================================================
    # FORCE CONSISTENT SCHEMA - Critical for Athena
    # ========================================================================
    # Define type mapping for all possible columns
    type_mapping = {
        'player_id': 'Int64',
        'season': 'Int64',
        'week': 'Int64',
        'targets': 'Int64',
        'base_targets': 'Int64',
        'wide_snaps': 'Int64',
        'slot_snaps': 'Int64',
        'inline_snaps': 'Int64',
        'player_game_count': 'Int64',
        'franchise_id': 'Int64',
        'draft_season': 'Int64',
        'eligible_season': 'Int64',
        # All the left/center/right + deep/medium/short/behind_los combinations
        # Integer fields
        'left_deep_routes': 'Int64',
        'left_medium_routes': 'Int64',
        'left_short_routes': 'Int64',
        'left_behind_los_routes': 'Int64',
        'center_deep_routes': 'Int64',
        'center_medium_routes': 'Int64',
        'center_short_routes': 'Int64',
        'center_behind_los_routes': 'Int64',
        'right_deep_routes': 'Int64',
        'right_medium_routes': 'Int64',
        'right_short_routes': 'Int64',
        'right_behind_los_routes': 'Int64',
        'behind_los_routes': 'Int64',
        'short_routes': 'Int64',
        'medium_routes': 'Int64',
        'deep_routes': 'Int64',
        'left_deep_targets': 'Int64',
        'left_medium_targets': 'Int64',
        'left_short_targets': 'Int64',
        'left_behind_los_targets': 'Int64',
        'center_deep_targets': 'Int64',
        'center_medium_targets': 'Int64',
        'center_short_targets': 'Int64',
        'center_behind_los_targets': 'Int64',
        'right_deep_targets': 'Int64',
        'right_medium_targets': 'Int64',
        'right_short_targets': 'Int64',
        'right_behind_los_targets': 'Int64',
        'short_targets': 'Int64',
        'medium_targets': 'Int64',
        'deep_targets': 'Int64',
        'left_deep_receptions': 'Int64',
        'left_medium_receptions': 'Int64',
        'left_short_receptions': 'Int64',
        'left_behind_los_receptions': 'Int64',
        'center_deep_receptions': 'Int64',
        'center_medium_receptions': 'Int64',
        'center_short_receptions': 'Int64',
        'center_behind_los_receptions': 'Int64',
        'right_deep_receptions': 'Int64',
        'right_medium_receptions': 'Int64',
        'right_short_receptions': 'Int64',
        'right_behind_los_receptions': 'Int64',
        'short_receptions': 'Int64',
        'medium_receptions': 'Int64',
        'deep_receptions': 'Int64',
        'left_deep_yards': 'Int64',
        'left_medium_yards': 'Int64',
        'left_short_yards': 'Int64',
        'left_behind_los_yards': 'Int64',
        'center_deep_yards': 'Int64',
        'center_medium_yards': 'Int64',
        'center_short_yards': 'Int64',
        'center_behind_los_yards': 'Int64',
        'right_deep_yards': 'Int64',
        'right_medium_yards': 'Int64',
        'right_short_yards': 'Int64',
        'right_behind_los_yards': 'Int64',
        'short_yards': 'Int64',
        'medium_yards': 'Int64',
        'deep_yards': 'Int64',
        'left_deep_touchdowns': 'Int64',
        'left_medium_touchdowns': 'Int64',
        'left_short_touchdowns': 'Int64',
        'left_behind_los_touchdowns': 'Int64',
        'center_deep_touchdowns': 'Int64',
        'center_medium_touchdowns': 'Int64',
        'center_short_touchdowns': 'Int64',
        'center_behind_los_touchdowns': 'Int64',
        'right_deep_touchdowns': 'Int64',
        'right_medium_touchdowns': 'Int64',
        'right_short_touchdowns': 'Int64',
        'right_behind_los_touchdowns': 'Int64',
        'short_touchdowns': 'Int64',
        'medium_touchdowns': 'Int64',
        'deep_touchdowns': 'Int64',
        'left_deep_first_downs': 'Int64',
        'left_medium_first_downs': 'Int64',
        'left_short_first_downs': 'Int64',
        'left_behind_los_first_downs': 'Int64',
        'center_deep_first_downs': 'Int64',
        'center_medium_first_downs': 'Int64',
        'center_short_first_downs': 'Int64',
        'center_behind_los_first_downs': 'Int64',
        'right_deep_first_downs': 'Int64',
        'right_medium_first_downs': 'Int64',
        'right_short_first_downs': 'Int64',
        'right_behind_los_first_downs': 'Int64',
        'short_first_downs': 'Int64',
        'medium_first_downs': 'Int64',
        'deep_first_downs': 'Int64',
        'behind_los_first_downs': 'Int64',
        'left_deep_drops': 'Int64',
        'left_medium_drops': 'Int64',
        'left_short_drops': 'Int64',
        'left_behind_los_drops': 'Int64',
        'center_deep_drops': 'Int64',
        'center_medium_drops': 'Int64',
        'center_short_drops': 'Int64',
        'center_behind_los_drops': 'Int64',
        'right_deep_drops': 'Int64',
        'right_medium_drops': 'Int64',
        'right_short_drops': 'Int64',
        'right_behind_los_drops': 'Int64',
        'short_drops': 'Int64',
        'medium_drops': 'Int64',
        'deep_drops': 'Int64',
        'left_deep_fumbles': 'Int64',
        'left_medium_fumbles': 'Int64',
        'left_short_fumbles': 'Int64',
        'left_behind_los_fumbles': 'Int64',
        'center_deep_fumbles': 'Int64',
        'center_medium_fumbles': 'Int64',
        'center_short_fumbles': 'Int64',
        'center_behind_los_fumbles': 'Int64',
        'right_deep_fumbles': 'Int64',
        'right_medium_fumbles': 'Int64',
        'right_short_fumbles': 'Int64',
        'right_behind_los_fumbles': 'Int64',
        'short_fumbles': 'Int64',
        'medium_fumbles': 'Int64',
        'deep_fumbles': 'Int64',
        'left_deep_interceptions': 'Int64',
        'left_medium_interceptions': 'Int64',
        'left_short_interceptions': 'Int64',
        'left_behind_los_interceptions': 'Int64',
        'center_deep_interceptions': 'Int64',
        'center_medium_interceptions': 'Int64',
        'center_short_interceptions': 'Int64',
        'center_behind_los_interceptions': 'Int64',
        'right_deep_interceptions': 'Int64',
        'right_medium_interceptions': 'Int64',
        'right_short_interceptions': 'Int64',
        'right_behind_los_interceptions': 'Int64',
        'short_interceptions': 'Int64',
        'medium_interceptions': 'Int64',
        'deep_interceptions': 'Int64',
        'left_deep_contested_targets': 'Int64',
        'left_medium_contested_targets': 'Int64',
        'left_short_contested_targets': 'Int64',
        'left_behind_los_contested_targets': 'Int64',
        'center_deep_contested_targets': 'Int64',
        'center_medium_contested_targets': 'Int64',
        'center_short_contested_targets': 'Int64',
        'center_behind_los_contested_targets': 'Int64',
        'right_deep_contested_targets': 'Int64',
        'right_medium_contested_targets': 'Int64',
        'right_short_contested_targets': 'Int64',
        'right_behind_los_contested_targets': 'Int64',
        'short_contested_targets': 'Int64',
        'medium_contested_targets': 'Int64',
        'deep_contested_targets': 'Int64',
        'left_deep_contested_receptions': 'Int64',
        'left_medium_contested_receptions': 'Int64',
        'left_short_contested_receptions': 'Int64',
        'left_behind_los_contested_receptions': 'Int64',
        'center_deep_contested_receptions': 'Int64',
        'center_medium_contested_receptions': 'Int64',
        'center_short_contested_receptions': 'Int64',
        'center_behind_los_contested_receptions': 'Int64',
        'right_deep_contested_receptions': 'Int64',
        'right_medium_contested_receptions': 'Int64',
        'right_short_contested_receptions': 'Int64',
        'right_behind_los_contested_receptions': 'Int64',
        'short_contested_receptions': 'Int64',
        'medium_contested_receptions': 'Int64',
        'deep_contested_receptions': 'Int64',
        'left_deep_yards_after_catch': 'Int64',
        'left_medium_yards_after_catch': 'Int64',
        'left_short_yards_after_catch': 'Int64',
        'left_behind_los_yards_after_catch': 'Int64',
        'center_deep_yards_after_catch': 'Int64',
        'center_medium_yards_after_catch': 'Int64',
        'center_short_yards_after_catch': 'Int64',
        'center_behind_los_yards_after_catch': 'Int64',
        'right_deep_yards_after_catch': 'Int64',
        'right_medium_yards_after_catch': 'Int64',
        'right_short_yards_after_catch': 'Int64',
        'right_behind_los_yards_after_catch': 'Int64',
        'short_yards_after_catch': 'Int64',
        'medium_yards_after_catch': 'Int64',
        'deep_yards_after_catch': 'Int64',
        'left_deep_avoided_tackles': 'Int64',
        'left_medium_avoided_tackles': 'Int64',
        'left_short_avoided_tackles': 'Int64',
        'left_behind_los_avoided_tackles': 'Int64',
        'center_deep_avoided_tackles': 'Int64',
        'center_medium_avoided_tackles': 'Int64',
        'center_short_avoided_tackles': 'Int64',
        'center_behind_los_avoided_tackles': 'Int64',
        'right_deep_avoided_tackles': 'Int64',
        'right_medium_avoided_tackles': 'Int64',
        'right_short_avoided_tackles': 'Int64',
        'right_behind_los_avoided_tackles': 'Int64',
        'short_avoided_tackles': 'Int64',
        'medium_avoided_tackles': 'Int64',
        'deep_avoided_tackles': 'Int64',
        'left_deep_longest': 'Int64',
        'left_medium_longest': 'Int64',
        'left_short_longest': 'Int64',
        'left_behind_los_longest': 'Int64',
        'center_deep_longest': 'Int64',
        'center_medium_longest': 'Int64',
        'center_short_longest': 'Int64',
        'center_behind_los_longest': 'Int64',
        'right_deep_longest': 'Int64',
        'right_medium_longest': 'Int64',
        'right_short_longest': 'Int64',
        'right_behind_los_longest': 'Int64',
        'short_longest': 'Int64',
        'medium_longest': 'Int64',
        'deep_longest': 'Int64',
        'left_deep_pass_plays': 'Int64',
        'left_medium_pass_plays': 'Int64',
        'left_short_pass_plays': 'Int64',
        'left_behind_los_pass_plays': 'Int64',
        'center_deep_pass_plays': 'Int64',
        'center_medium_pass_plays': 'Int64',
        'center_short_pass_plays': 'Int64',
        'center_behind_los_pass_plays': 'Int64',
        'right_deep_pass_plays': 'Int64',
        'right_medium_pass_plays': 'Int64',
        'right_short_pass_plays': 'Int64',
        'right_behind_los_pass_plays': 'Int64',
        'short_pass_plays': 'Int64',
        'medium_pass_plays': 'Int64',
        'deep_pass_plays': 'Int64',
        'left_deep_pass_blocks': 'Int64',
        'left_medium_pass_blocks': 'Int64',
        'left_short_pass_blocks': 'Int64',
        'left_behind_los_pass_blocks': 'Int64',
        'center_deep_pass_blocks': 'Int64',
        'center_medium_pass_blocks': 'Int64',
        'center_short_pass_blocks': 'Int64',
        'center_behind_los_pass_blocks': 'Int64',
        'right_deep_pass_blocks': 'Int64',
        'right_medium_pass_blocks': 'Int64',
        'right_short_pass_blocks': 'Int64',
        'right_behind_los_pass_blocks': 'Int64',
        'short_pass_blocks': 'Int64',
        'medium_pass_blocks': 'Int64',
        'deep_pass_blocks': 'Int64',
        # Float fields
        'grades_offense': 'float64',
        'grades_pass_route': 'float64',
        'grades_pass_block': 'float64',
        'yprr': 'float64',
        'yards_after_catch_per_reception': 'float64',
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
        'short_route_rate': 'float64',
        'medium_route_rate': 'float64',
        'deep_route_rate': 'float64',
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
        'short_yprr': 'float64',
        'medium_yprr': 'float64',
        'deep_yprr': 'float64',
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
        'short_yards_per_reception': 'float64',
        'medium_yards_per_reception': 'float64',
        'deep_yards_per_reception': 'float64',
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
        'short_yards_after_catch_per_reception': 'float64',
        'medium_yards_after_catch_per_reception': 'float64',
        'deep_yards_after_catch_per_reception': 'float64',
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
        'short_avg_depth_of_target': 'float64',
        'medium_avg_depth_of_target': 'float64',
        'deep_avg_depth_of_target': 'float64',
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
        'short_targeted_qb_rating': 'float64',
        'medium_targeted_qb_rating': 'float64',
        'deep_targeted_qb_rating': 'float64',
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
        'short_caught_percent': 'float64',
        'medium_caught_percent': 'float64',
        'deep_caught_percent': 'float64',
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
        'short_drop_rate': 'float64',
        'medium_drop_rate': 'float64',
        'deep_drop_rate': 'float64',
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
        'short_contested_catch_rate': 'float64',
        'medium_contested_catch_rate': 'float64',
        'deep_contested_catch_rate': 'float64',
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
        'short_targets_percent': 'float64',
        'medium_targets_percent': 'float64',
        'deep_targets_percent': 'float64',
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
        'short_pass_block_rate': 'float64',
        'medium_pass_block_rate': 'float64',
        'deep_pass_block_rate': 'float64',
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
        'short_grades_pass_route': 'float64',
        'medium_grades_pass_route': 'float64',
        'deep_grades_pass_route': 'float64',
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
        'short_grades_hands_drop': 'float64',
        'medium_grades_hands_drop': 'float64',
        'deep_grades_hands_drop': 'float64',
        # String fields
        'team_name': 'str',
        'team': 'str',
        'player': 'str',
        'position': 'str',
        'jersey_number': 'str',
        'scraped_at': 'str'
    }
    
    # Only cast columns that exist
    existing_columns = {k: v for k, v in type_mapping.items() if k in df.columns}
    df = df.astype(existing_columns)
    
    # S3 path - include batch identifier if player_id filtering is used
    if min_player_id is not None or max_player_id is not None:
        min_str = str(min_player_id) if min_player_id is not None else 'start'
        max_str = str(max_player_id) if max_player_id is not None else 'end'
        s3_key = f"data/receiving_depth/season={season}/batch_{min_str}_{max_str}/data.parquet"
    else:
        s3_key = f"data/receiving_depth/season={season}/data.parquet"
    
    # Convert to Parquet
    parquet_buffer = BytesIO()
    df.to_parquet(
        parquet_buffer,
        index=False,
        engine='pyarrow',
        compression='snappy'
    )
    parquet_buffer.seek(0)
    
    # Upload
    s3_client.put_object(
        Bucket=BUCKET_NAME,
        Key=s3_key,
        Body=parquet_buffer.getvalue(),
        ContentType='application/octet-stream'
    )
    
    print(f"✓ Saved to s3://{BUCKET_NAME}/{s3_key}")
    return s3_key

def lambda_handler(event, context):
    """
    Lambda handler for receiving depth data scraper (FULL SEASON at once)
    
    Event:
    {
        "season": 2025,              // Required - which season to scrape
        "player_id_range": [1, 83666] // Optional - [min_player_id, max_player_id] (inclusive)
    }
    
    Usage:
    - Run all players: {"season": 2025}
    - Run specific player range: {"season": 2025, "player_id_range": [1, 83666]}
    """
    
    try:
        # Get parameters from event
        season = event.get('season')
        player_id_range = event.get('player_id_range')
        
        min_player_id = None
        max_player_id = None
        
        if player_id_range:
            min_player_id = player_id_range[0]
            max_player_id = player_id_range[1]
        
        if not season:
            raise ValueError("Must provide 'season' in event (e.g., {'season': 2025})")
        
        print("=" * 60)
        print(f"RECEIVING DEPTH SCRAPER - SEASON {season} (FULL SEASON)")
        
        if min_player_id is not None or max_player_id is not None:
            print(f"PLAYER_ID RANGE: [{min_player_id}, {max_player_id}]")
        print("=" * 60)
        
        # Get cookies and check access level
        cookies = get_cookies()
        use_restricted = check_restricted_access(cookies)
        
        # Query Athena for player IDs (with optional filtering)
        player_ids = query_athena_for_players(season, min_player_id, max_player_id)
        
        if not player_ids:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': f'No players found for season {season} with given filters'})
            }
        
        # Scrape full season for all players
        df = scrape_all_receiving_depth(cookies, player_ids, season, use_restricted)
        
        if df.empty:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': f'No receiving depth data found for season {season}'})
            }
        
        # Save to S3 (pass player_id range so batches don't overwrite each other)
        s3_key = save_to_s3_by_season(df, season, min_player_id, max_player_id)
        
        response_body = {
            'message': 'Success',
            'season': season,
            'players_scraped': len(player_ids),
            'total_records': len(df),
            'access_level': 'restricted' if use_restricted else 'standard',
            's3_key': s3_key
        }
        
        if min_player_id is not None or max_player_id is not None:
            response_body['player_id_range'] = [min_player_id, max_player_id]
        
        return {
            'statusCode': 200,
            'body': json.dumps(response_body)
        }
        
    except Exception as e:
        print(f"ERROR: {e}")
        
        return {
            'statusCode': 500,
            'body': json.dumps({
                'error': str(e)
            })
        }