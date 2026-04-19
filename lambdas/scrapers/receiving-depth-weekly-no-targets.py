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
    """Check if cookies have access to restricted endpoints"""
    test_url = 'https://premium.pff.com/api/v1/player/receiving/depth?league=nfl&season=2024&week=1&player_id=78092'
    
    try:
        response = requests.get(test_url, cookies=cookies, timeout=10)
        
        if response.status_code == 401:
            raise Exception("401 Unauthorized - cookies expired!")
        
        if response.status_code == 200:
            data = response.json()
            if data.get('receiving_depth') and len(data['receiving_depth']) > 0:
                first_record = data['receiving_depth'][0]
                has_restricted = 'player' in first_record and 'position' in first_record
                print(f"✓ Access level: {'RESTRICTED (with metadata)' if has_restricted else 'STANDARD'}")
                return has_restricted
        
        print("✓ Access level: STANDARD")
        return False
        
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e):
            raise
        print(f"Warning: Could not determine access level: {e}")
        return False

def query_athena_for_zero_target_players(season=None, week=None, min_player_id=None, max_player_id=None):
    """Query Athena view to get player_ids with routes but no targets"""
    
    query = """
        SELECT DISTINCT 
            player_id,
            week,
            season
        FROM nfl_data.vw_players_with_routes_no_targets
        WHERE 1=1
    """
    
    if season is not None:
        query += f" AND season = {season}"
    if week is not None:
        query += f" AND week = {week}"
    if min_player_id is not None:
        query += f" AND player_id >= {min_player_id}"
    if max_player_id is not None:
        query += f" AND player_id <= {max_player_id}"
    
    filter_desc = []
    if season: filter_desc.append(f"season={season}")
    if week: filter_desc.append(f"week={week}")
    if min_player_id or max_player_id: 
        filter_desc.append(f"player_id={min_player_id or 'min'}-{max_player_id or 'max'}")
    
    print(f"Querying Athena for zero-target players ({', '.join(filter_desc) if filter_desc else 'ALL'})...")
    
    response = athena_client.start_query_execution(
        QueryString=query,
        QueryExecutionContext={'Database': 'nfl_data'},
        ResultConfiguration={'OutputLocation': ATHENA_OUTPUT}
    )
    
    query_execution_id = response['QueryExecutionId']
    
    while True:
        result = athena_client.get_query_execution(QueryExecutionId=query_execution_id)
        status = result['QueryExecution']['Status']['State']
        
        if status in ['SUCCEEDED', 'FAILED', 'CANCELLED']:
            break
        time.sleep(1)
    
    if status != 'SUCCEEDED':
        error_info = result['QueryExecution']['Status'].get('StateChangeReason', 'Unknown error')
        raise Exception(f"Athena query failed: {status} - {error_info}")
    
    players = []
    next_token = None
    
    while True:
        if next_token:
            results = athena_client.get_query_results(
                QueryExecutionId=query_execution_id,
                NextToken=next_token
            )
        else:
            results = athena_client.get_query_results(
                QueryExecutionId=query_execution_id
            )
        
        start_idx = 1 if next_token is None else 0
        
        for row in results['ResultSet']['Rows'][start_idx:]:
            try:
                player_id = int(row['Data'][0].get('VarCharValue'))
                week = int(row['Data'][1].get('VarCharValue'))
                season = int(row['Data'][2].get('VarCharValue'))
                players.append({'player_id': player_id, 'week': week, 'season': season})
            except:
                continue
        
        next_token = results.get('NextToken')
        if not next_token:
            break
    
    print(f"✓ Found {len(players)} player-week combinations with zero targets")
    return players

def scrape_player_week(player_id, week, season, cookies):
    """Scrape receiving depth data for ONE player-week combination"""
    url = f'https://premium.pff.com/api/v1/player/receiving/depth?league=nfl&season={season}&week={week}&player_id={player_id}'
    
    try:
        response = requests.get(url, cookies=cookies, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            depth_data = data.get('receiving_depth', [])
            
            if not depth_data:
                return pd.DataFrame()
            
            for record in depth_data:
                if 'player_id' not in record:
                    record['player_id'] = player_id
                if 'season' not in record:
                    record['season'] = season
                if 'week' not in record:
                    record['week'] = week
                record['scraped_at'] = datetime.utcnow().isoformat()
            
            return pd.DataFrame(depth_data)
        
        elif response.status_code == 401:
            raise Exception("401 Unauthorized - cookies expired!")
        else:
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e):
            raise
        return pd.DataFrame()

def scrape_all_zero_target_players(cookies, players):
    """Scrape receiving depth data for all zero-target player-weeks"""
    all_data = []
    total = len(players)
    
    print(f"\nScraping {total} player-week combinations...")
    
    for idx, player_info in enumerate(players, 1):
        if idx % 100 == 0:
            print(f"Progress: {idx}/{total} ({idx/total*100:.1f}%)...")
        
        df = scrape_player_week(
            player_info['player_id'], 
            player_info['week'], 
            player_info['season'], 
            cookies
        )
        
        if not df.empty:
            all_data.append(df)
        
        time.sleep(0.1)
    
    if all_data:
        combined = pd.concat(all_data, ignore_index=True)
        print(f"✓ Scraped {len(combined)} records from {len(all_data)} successful player-weeks")
        return combined
    else:
        return pd.DataFrame()

def save_to_s3_zero_targets(df, season=None, week=None, min_player_id=None, max_player_id=None):
    """
    Save zero-target receiving depth data to S3
    APPENDS to existing file and deduplicates by player_id/week/season
    """
    
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
        'penalties': 'Int64',
        'declined_penalties': 'Int64',
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
        'behind_los_route_rate': 'float64',
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
        'behind_los_yprr': 'float64',
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
        'behind_los_yards_per_reception': 'float64',
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
        'behind_los_yards_after_catch_per_reception': 'float64',
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
        'behind_los_avg_depth_of_target': 'float64',
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
        'behind_los_targeted_qb_rating': 'float64',
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
        'behind_los_caught_percent': 'float64',
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
        'behind_los_drop_rate': 'float64',
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
        'behind_los_contested_catch_rate': 'float64',
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
        'behind_los_targets_percent': 'float64',
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
        'behind_los_pass_block_rate': 'float64',
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
        'behind_los_grades_pass_route': 'float64',
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
        'behind_los_grades_hands_drop': 'float64',
        'team_name': 'str',
        'team': 'str',
        'player': 'str',
        'position': 'str',
        'jersey_number': 'str',
        'scraped_at': 'str'
    }
    
    existing_columns = {k: v for k, v in type_mapping.items() if k in df.columns}
    df = df.astype(existing_columns)
    
    if season is not None:
        base_path = f"data/receiving_depth_weekly_no_targets/season={season}/"
    else:
        base_path = "data/receiving_depth_weekly_no_targets/all_seasons/"
    
    if min_player_id is not None or max_player_id is not None:
        min_str = str(min_player_id) if min_player_id is not None else 'start'
        max_str = str(max_player_id) if max_player_id is not None else 'end'
        s3_key = f"{base_path}batch_{min_str}_{max_str}/data.parquet"
    else:
        s3_key = f"{base_path}data.parquet"
    
    # READ EXISTING FILE IF IT EXISTS AND APPEND
# READ EXISTING FILE IF IT EXISTS AND APPEND
    try:
        print(f"Checking for existing data at {s3_key}...")
        existing_df = pd.read_parquet(BytesIO(s3_client.get_object(Bucket=BUCKET_NAME, Key=s3_key)['Body'].read()))
        
        print(f"  Found {len(existing_df)} existing records")
        
        # Remove the week we're adding BEFORE concat (like the working scraper)
        if week is not None:
            existing_df = existing_df[existing_df['week'] != week]
            print(f"  Kept {len(existing_df)} rows after removing week {week}")
        elif season is not None:
            existing_df = existing_df[existing_df['season'] != season]
            print(f"  Kept {len(existing_df)} rows after removing season {season}")
        
        df = pd.concat([existing_df, df], ignore_index=True)
        print(f"  After merge: {len(df)} total records")
    
    except s3_client.exceptions.NoSuchKey:
        print(f"  No existing file found, creating new")
    
    parquet_buffer = BytesIO()
    df.to_parquet(
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
    
    print(f"✓ Saved to s3://{BUCKET_NAME}/{s3_key}")
    return s3_key

def lambda_handler(event, context):
    """Lambda handler for zero-target receiving depth scraper"""
    
    try:
        season = event.get('season')
        week = event.get('week')
        player_id_range = event.get('player_id_range')
        
        min_player_id = None
        max_player_id = None
        
        if player_id_range:
            min_player_id = player_id_range[0]
            max_player_id = player_id_range[1]
        
        print("=" * 60)
        print("RECEIVING DEPTH SCRAPER - ZERO TARGETS")
        if season: print(f"Season: {season}")
        if week: print(f"Week: {week}")
        if min_player_id or max_player_id: 
            print(f"Player ID Range: [{min_player_id}, {max_player_id}]")
        print("=" * 60)
        
        cookies = get_cookies()
        use_restricted = check_restricted_access(cookies)
        
        players = query_athena_for_zero_target_players(
            season=season,
            week=week,
            min_player_id=min_player_id,
            max_player_id=max_player_id
        )
        
        if not players:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': 'No zero-target players found with given filters'})
            }
        
        df = scrape_all_zero_target_players(cookies, players)
        
        if df.empty:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': 'No data returned for zero-target players'})
            }
        
        s3_key = save_to_s3_zero_targets(
            df, 
            season=season, 
            week=week,
            min_player_id=min_player_id,
            max_player_id=max_player_id
        )
        
        response_body = {
            'message': 'Success',
            'player_weeks_scraped': len(players),
            'records_saved': len(df),
            'unique_players': df['player_id'].nunique() if 'player_id' in df.columns else 0,
            'access_level': 'restricted' if use_restricted else 'standard',
            's3_key': s3_key
        }
        
        if season: response_body['season'] = season
        if week: response_body['week'] = week
        if min_player_id or max_player_id: 
            response_body['player_id_range'] = [min_player_id, max_player_id]
        
        return {
            'statusCode': 200,
            'body': json.dumps(response_body)
        }
        
    except Exception as e:
        print(f"ERROR: {e}")
        return {
            'statusCode': 500,
            'body': json.dumps({'error': str(e)})
        }