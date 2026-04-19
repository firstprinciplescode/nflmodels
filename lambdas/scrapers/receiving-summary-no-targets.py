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
ATHENA_OUTPUT = f's3://{BUCKET_NAME}/athena-results/'

def get_cookies():
    """Get PFF cookies from Secrets Manager"""
    response = secrets_client.get_secret_value(SecretId=SECRET_NAME)
    return json.loads(response['SecretString'])

def query_athena_for_zero_target_players(season=None, weeks=None, min_player_id=None, max_player_id=None):
    """
    Query Athena view to get player_ids with routes but no targets
    
    Args:
        season: Filter by season (optional)
        weeks: Filter by week(s) - can be single int or list (optional)
        min_player_id: Minimum player_id (optional)
        max_player_id: Maximum player_id (optional)
    
    Returns: list of dicts with player_id, week, season
    """
    
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
    if weeks is not None:
        if isinstance(weeks, list):
            weeks_str = ','.join(map(str, weeks))
            query += f" AND week IN ({weeks_str})"
    if min_player_id is not None:
        query += f" AND player_id >= {min_player_id}"
    if max_player_id is not None:
        query += f" AND player_id <= {max_player_id}"
    
    filter_desc = []
    if season: filter_desc.append(f"season={season}")
    if weeks: filter_desc.append(f"weeks={weeks}")
    if min_player_id or max_player_id: 
        filter_desc.append(f"player_id={min_player_id or 'min'}-{max_player_id or 'max'}")
    
    print(f"Querying Athena for zero-target players ({', '.join(filter_desc) if filter_desc else 'ALL'})...")
    
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
    
    # Get results WITH PAGINATION
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
        
        # Process this page (skip header on first page only)
        start_idx = 1 if next_token is None else 0
        
        for row in results['ResultSet']['Rows'][start_idx:]:
            try:
                player_id = int(row['Data'][0].get('VarCharValue'))
                week = int(row['Data'][1].get('VarCharValue'))
                season = int(row['Data'][2].get('VarCharValue'))
                players.append({'player_id': player_id, 'week': week, 'season': season})
            except:
                continue
        
        # Check for next page
        next_token = results.get('NextToken')
        if not next_token:
            break
    
    print(f"✓ Found {len(players)} player-week combinations with zero targets")
    
    # Print unique player count
    unique_players = len(set([p['player_id'] for p in players]))
    print(f"✓ Unique players: {unique_players}")
    
    return players

def scrape_player_week(player_id, week, season, cookies):
    """
    Scrape receiving summary data for ONE player-week combination
    Returns: DataFrame with data (or empty if no data)
    """
    url = f'https://premium.pff.com/api/v1/player/receiving/summary?league=nfl&season={season}&week={week}&player_id={player_id}'
    
    try:
        response = requests.get(url, cookies=cookies, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            
            # Check for restricted access
            if 'restricted' in data and data['restricted']:
                raise Exception(f"RESTRICTED ACCESS DETECTED - cookies only provide limited fields. Missing fields: {', '.join(data['restricted'][:5])}... Update cookies for full access!")
            
            # Navigate the nested structure
            receiving_summary = data.get('receiving_summary', {})
            weeks_data = receiving_summary.get('weeks', [])
            
            if not weeks_data:
                return pd.DataFrame()
            
            # Extract week records and flatten everything
            flattened_records = []
            for week_record in weeks_data:
                # Start with a fresh dict for this record
                flat_record = {}
                
                # First, add all non-nested fields from week_record
                for key, value in week_record.items():
                    if key != 'game' and not isinstance(value, dict):
                        flat_record[key] = value
                
                # Now flatten the nested 'game' object if it exists
                if 'game' in week_record:
                    game = week_record['game']
                    for key, value in game.items():
                        # Use game_ prefix for fields that already exist in week_record
                        # to avoid duplicates, otherwise use the field name as-is
                        if key in week_record and key != 'game':
                            flat_record[f'game_{key}'] = value
                        elif key not in flat_record:
                            flat_record[key] = value
                
                # Add metadata fields - ensure they're always present
                flat_record['player_id'] = player_id
                flat_record['week'] = week
                flat_record['season'] = season
                flat_record['scraped_at'] = datetime.utcnow().isoformat()
                
                flattened_records.append(flat_record)
            
            # Create DataFrame
            df = pd.DataFrame(flattened_records)
            
            # Deduplicate columns - if there are duplicate column names, keep first
            if df.shape[0] > 0:
                df = df.loc[:, ~df.columns.duplicated(keep='first')]
            
            return df
        
        elif response.status_code == 401:
            raise Exception("401 Unauthorized - cookies expired!")
        else:
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e) or "RESTRICTED" in str(e):
            raise
        return pd.DataFrame()

def scrape_all_zero_target_players(cookies, players):
    """
    Scrape receiving summary data for all zero-target player-weeks
    
    Args:
        cookies: Auth cookies
        players: List of dicts with player_id, week, season
    
    Returns: DataFrame with all scraped data
    """
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
        # Combine all dataframes
        combined = pd.concat(all_data, ignore_index=True)
        
        # Remove duplicates based on player_id, week, season
        before_dedup = len(combined)
        combined = combined.drop_duplicates(subset=['player_id', 'week', 'season'], keep='first')
        after_dedup = len(combined)
        
        if before_dedup > after_dedup:
            print(f"✓ Removed {before_dedup - after_dedup} duplicate records")
        
        print(f"✓ Scraped {len(combined)} unique records from {len(all_data)} successful player-weeks")
        return combined
    else:
        return pd.DataFrame()

def save_to_s3_by_season(df, season, weeks=None, min_player_id=None, max_player_id=None):
    """Save receiving data to S3 partitioned by season with consistent types"""
    
    # Type mapping - SAME as with_targets scraper
    type_mapping = {
        'player_id': 'Int64',
        'season': 'Int64',
        'week': 'Int64',
        'player_game_count': 'Int64',
        'franchise_id': 'Int64',
        'draft_season': 'Int64',
        'eligible_season': 'Int64',
        'targets': 'Int64',
        'receptions': 'Int64',
        'yards': 'Int64',
        'touchdowns': 'Int64',
        'first_downs': 'Int64',
        'fumbles': 'Int64',
        'interceptions': 'Int64',
        'penalties': 'Int64',
        'declined_penalties': 'Int64',
        'contested_targets': 'Int64',
        'contested_receptions': 'Int64',
        'yards_after_catch': 'Int64',
        'avoided_tackles': 'Int64',
        'longest': 'Int64',
        'pass_plays': 'Int64',
        'pass_blocks': 'Int64',
        'routes': 'Int64',
        'drops': 'Int64',
        'wide_snaps': 'Int64',
        'slot_snaps': 'Int64',
        'inline_snaps': 'Int64',
        'game_id': 'Int64',
        'home_franchise_id': 'Int64',
        'away_franchise_id': 'Int64',
        'player_franchise_id': 'Int64',
        'grades_offense': 'float64',
        'grades_pass_route': 'float64',
        'grades_hands_drop': 'float64',
        'grades_hands_fumble': 'float64',
        'grades_pass_block': 'float64',
        'yprr': 'float64',
        'yards_per_reception': 'float64',
        'yards_after_catch_per_reception': 'float64',
        'avg_depth_of_target': 'float64',
        'targeted_qb_rating': 'float64',
        'caught_percent': 'float64',
        'drop_rate': 'float64',
        'contested_catch_rate': 'float64',
        'route_rate': 'float64',
        'pass_block_rate': 'float64',
        'wide_rate': 'float64',
        'slot_rate': 'float64',
        'inline_rate': 'float64',
        'targets_percent': 'float64',
        'team_name': 'str',
        'team': 'str',
        'player': 'str',
        'position': 'str',
        'jersey_number': 'str',
        'status': 'str',
        'scraped_at': 'str',
        'home_team_name': 'str',
        'away_team_name': 'str'
    }
    
    # S3 path
    if min_player_id is not None or max_player_id is not None:
        min_str = str(min_player_id) if min_player_id is not None else '1'
        max_str = str(max_player_id) if max_player_id is not None else 'end'
        s3_key = f"data/receiving_summary_no_targets/season={season}/batch_{min_str}_{max_str}/data.parquet"
    else:
        s3_key = f"data/receiving_summary_no_targets/season={season}/data.parquet"
    
    # Read existing and merge
    try:
        existing_obj = s3_client.get_object(Bucket=BUCKET_NAME, Key=s3_key)
        existing_df = pd.read_parquet(BytesIO(existing_obj['Body'].read()))
        print(f"✓ Read existing file with {len(existing_df)} rows")
        
        if weeks is not None:
            # Drop only the specific weeks we're adding
            weeks_list = weeks if isinstance(weeks, list) else [weeks]
            existing_df = existing_df[~existing_df['week'].isin(weeks_list)]
            print(f"✓ Kept {len(existing_df)} rows after removing weeks {weeks_list}")
        else:
            # Drop the entire season we're adding
            existing_df = existing_df[existing_df['season'] != season]
            print(f"✓ Kept {len(existing_df)} rows after removing season {season}")
        
        # Concat new data onto existing
        df = pd.concat([existing_df, df], ignore_index=True)
        print(f"✓ Combined total: {len(df)} rows")
        
    except s3_client.exceptions.NoSuchKey:
        print("✓ No existing file - creating new")
    except Exception as e:
        print(f"⚠ Could not read existing file: {e} - creating new")
    
    # Dedupe on player_id, week, season - keep last (most recent scrape)
    before_dedupe = len(df)
    df = df.drop_duplicates(subset=['player_id', 'week', 'season'], keep='last')
    after_dedupe = len(df)
    if before_dedupe > after_dedupe:
        print(f"✓ Removed {before_dedupe - after_dedupe} duplicates")
    
    # Only cast columns that exist
    existing_columns = {k: v for k, v in type_mapping.items() if k in df.columns}
    df = df.astype(existing_columns)
    
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
    
    print(f"✓ Saved {len(df)} rows to s3://{BUCKET_NAME}/{s3_key}")
    return s3_key

def lambda_handler(event, context):
    """
    Lambda handler for receiving summary scraper - zero targets
    Scrapes players with routes but zero targets
    
    Event:
    {
        "season": 2024,                    // Optional - filter by season
        "weeks": [9],                      // Optional - filter by week(s)
        "player_id_range": [8800, 11600]   // Optional - [min, max] for batching
    }
    
    Examples:
    - All zero-target players: {}
    - Single season: {"season": 2024}
    - Single week: {"season": 2024, "weeks": [9]}
    - Multiple weeks: {"season": 2024, "weeks": [9, 10, 11]}
    - Batched by player_id: {"season": 2024, "player_id_range": [1, 50000]}
    """
    
    try:
        season = event.get('season')
        weeks = event.get('weeks')
        player_id_range = event.get('player_id_range')
        
        min_player_id = None
        max_player_id = None
        
        if player_id_range:
            min_player_id = player_id_range[0]
            max_player_id = player_id_range[1]
        
        print("=" * 60)
        print("RECEIVING SUMMARY SCRAPER - ZERO TARGETS")
        if season: print(f"Season: {season}")
        if weeks: print(f"Weeks: {weeks}")
        if min_player_id or max_player_id: 
            print(f"Player ID Range: [{min_player_id}, {max_player_id}]")
        print("=" * 60)
        
        # Get cookies
        cookies = get_cookies()
        
        # Query Athena for zero-target players
        players = query_athena_for_zero_target_players(
            season=season,
            weeks=weeks,
            min_player_id=min_player_id,
            max_player_id=max_player_id
        )
        
        if not players:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': 'No zero-target players found with given filters'})
            }
        
        # Scrape all player-weeks
        df = scrape_all_zero_target_players(cookies, players)
        
        if df.empty:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': 'No data returned for zero-target players'})
            }
        
        # Save to S3
        s3_key = save_to_s3_by_season(
            df, 
            season,
            weeks=weeks,
            min_player_id=min_player_id,
            max_player_id=max_player_id
        )
                
        response_body = {
            'message': 'Success',
            'player_weeks_scraped': len(players),
            'records_saved': len(df),
            'unique_players': df['player_id'].nunique() if 'player_id' in df.columns else 0,
            's3_key': s3_key
        }
        
        if season: response_body['season'] = season
        if weeks: response_body['weeks'] = weeks
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