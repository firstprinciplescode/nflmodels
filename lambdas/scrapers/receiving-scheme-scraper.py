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

# Configuration
BUCKET_NAME = 'nfl-pff-data-lucas'
SECRET_NAME = 'pff-api-cookies'

def get_cookies():
    """Get PFF cookies from Secrets Manager"""
    response = secrets_client.get_secret_value(SecretId=SECRET_NAME)
    return json.loads(response['SecretString'])

def check_restricted_access(cookies):
    """
    Check if cookies have access to FULL endpoints (not restricted)
    Returns: True if FULL access
    Raises: Exception if cookies give restricted access or are invalid (401)
    """
    test_season = 2024
    test_week = 1
    
    url = f'https://premium.pff.com/api/v1/facet/receiving/scheme?league=nfl&season={test_season}&week={test_week}'
    
    try:
        response = requests.get(url, cookies=cookies, timeout=10)
        
        if response.status_code == 401:
            raise Exception("401 Unauthorized - cookies expired!")
        
        if response.status_code == 200:
            data = response.json()
            if data.get('receiving_scheme') and len(data['receiving_scheme']) > 0:
                first_record = data['receiving_scheme'][0]
                has_full = 'man_routes' in first_record and 'man_yards' in first_record
                
                if not has_full:
                    raise Exception("RESTRICTED ACCESS DETECTED - cookies only provide limited fields. Update cookies for full access!")
                
                print(f"✓ Access level: FULL (with detailed stats)")
                return True
        
        raise Exception("Unable to verify access level - unexpected response format")
        
    except Exception as e:
        raise

def scrape_receiver_scheme_for_week(season, week, cookies):
    """
    Scrape receiver scheme data for one season/week (gets ALL players in one call)
    Returns: DataFrame with man vs zone coverage data
    """
    url = f'https://premium.pff.com/api/v1/facet/receiving/scheme?league=nfl&season={season}&week={week}'
    
    try:
        response = requests.get(url, cookies=cookies, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            
            scheme_data = data.get('receiving_scheme', [])
            
            if not scheme_data:
                return pd.DataFrame()
            
            for record in scheme_data:
                record['season'] = season
                record['week'] = week
                record['scraped_at'] = datetime.utcnow().isoformat()
            
            return pd.DataFrame(scheme_data)
        
        elif response.status_code == 401:
            raise Exception("401 Unauthorized - cookies expired!")
        else:
            print(f"Failed week {week}: Status {response.status_code}")
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e):
            raise
        print(f"Error scraping week {week}: {e}")
        return pd.DataFrame()

def save_to_s3_by_week(df, season, week):
    """
    Save receiver scheme data to S3, partitioned by season and week
    APPENDS to existing file and deduplicates by player_id/week/season
    """
    
    type_mapping = {
        'player_id': 'Int64',
        'season': 'Int64',
        'week': 'Int64',
        'player_game_count': 'Int64',
        'franchise_id': 'Int64',
        'draft_season': 'Int64',
        'eligible_season': 'Int64',
        'base_targets': 'Int64',
        'penalties': 'Int64',
        'declined_penalties': 'Int64',
        'man_routes': 'Int64',
        'man_targets': 'Int64',
        'man_receptions': 'Int64',
        'man_yards': 'Int64',
        'man_touchdowns': 'Int64',
        'man_first_downs': 'Int64',
        'man_drops': 'Int64',
        'man_fumbles': 'Int64',
        'man_interceptions': 'Int64',
        'man_contested_targets': 'Int64',
        'man_contested_receptions': 'Int64',
        'man_yards_after_catch': 'Int64',
        'man_avoided_tackles': 'Int64',
        'man_longest': 'Int64',
        'man_pass_plays': 'Int64',
        'man_pass_blocks': 'Int64',
        'zone_routes': 'Int64',
        'zone_targets': 'Int64',
        'zone_receptions': 'Int64',
        'zone_yards': 'Int64',
        'zone_touchdowns': 'Int64',
        'zone_first_downs': 'Int64',
        'zone_drops': 'Int64',
        'zone_fumbles': 'Int64',
        'zone_interceptions': 'Int64',
        'zone_contested_targets': 'Int64',
        'zone_contested_receptions': 'Int64',
        'zone_yards_after_catch': 'Int64',
        'zone_avoided_tackles': 'Int64',
        'zone_longest': 'Int64',
        'zone_pass_plays': 'Int64',
        'zone_pass_blocks': 'Int64',
        'man_route_rate': 'float64',
        'man_yprr': 'float64',
        'man_yards_per_reception': 'float64',
        'man_yards_after_catch_per_reception': 'float64',
        'man_avg_depth_of_target': 'float64',
        'man_targeted_qb_rating': 'float64',
        'man_caught_percent': 'float64',
        'man_drop_rate': 'float64',
        'man_contested_catch_rate': 'float64',
        'man_targets_percent': 'float64',
        'man_pass_block_rate': 'float64',
        'man_grades_pass_route': 'float64',
        'man_grades_hands_drop': 'float64',
        'zone_route_rate': 'float64',
        'zone_yprr': 'float64',
        'zone_yards_per_reception': 'float64',
        'zone_yards_after_catch_per_reception': 'float64',
        'zone_avg_depth_of_target': 'float64',
        'zone_targeted_qb_rating': 'float64',
        'zone_caught_percent': 'float64',
        'zone_drop_rate': 'float64',
        'zone_contested_catch_rate': 'float64',
        'zone_targets_percent': 'float64',
        'zone_pass_block_rate': 'float64',
        'zone_grades_pass_route': 'float64',
        'zone_grades_hands_drop': 'float64',
        'team_name': 'str',
        'team': 'str',
        'player': 'str',
        'position': 'str',
        'jersey_number': 'str',
        'scraped_at': 'str'
    }
    
    existing_columns = {k: v for k, v in type_mapping.items() if k in df.columns}
    df = df.astype(existing_columns)
    
    s3_key = f"data/receiver_scheme/season={season}/week={week}/data.parquet"
    
    # READ EXISTING FILE IF IT EXISTS AND APPEND
    try:
        print(f"Checking for existing data at {s3_key}...")
        existing = s3_client.get_object(Bucket=BUCKET_NAME, Key=s3_key)
        existing_df = pd.read_parquet(BytesIO(existing['Body'].read()))
        
        print(f"  Found {len(existing_df)} existing records")
        
        combined = pd.concat([existing_df, df], ignore_index=True)
        combined = combined.drop_duplicates(subset=['player_id', 'week', 'season'], keep='last')
        
        print(f"  After merge: {len(combined)} total records")
        df = combined
        
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
    """
    Lambda handler for receiver scheme data scraper (WEEK BY WEEK)
    
    Event:
    {
        "season": 2025,
        "weeks": [9, 10]
    }
    """
    
    try:
        season = event.get('season')
        weeks = event.get('weeks')
        
        if not season:
            raise ValueError("Must provide 'season' in event (e.g., {'season': 2025})")
        
        if not weeks:
            weeks_to_scrape = list(range(1, 19)) + [28, 29, 30, 32]
        else:
            weeks_to_scrape = weeks
        
        print("=" * 60)
        print(f"RECEIVER SCHEME SCRAPER - SEASON {season}")
        print(f"Weeks to scrape: {weeks_to_scrape}")
        print("=" * 60)
        
        cookies = get_cookies()
        check_restricted_access(cookies)
        
        results = []
        total_players = 0
        
        for week in weeks_to_scrape:
            print(f"\n{'='*60}")
            print(f"Processing Week {week}")
            print('='*60)
            
            df = scrape_receiver_scheme_for_week(season, week, cookies)
            
            if df.empty:
                print(f"⚠ No data found for week {week}")
                results.append({
                    'week': week,
                    'status': 'no_data',
                    'players': 0
                })
                continue
            
            s3_key = save_to_s3_by_week(df, season, week)
            
            results.append({
                'week': week,
                'status': 'success',
                'players': len(df),
                's3_key': s3_key
            })
            total_players += len(df)
            
            time.sleep(0.5)
        
        response_body = {
            'message': 'Success',
            'season': season,
            'weeks_processed': len(weeks_to_scrape),
            'total_players': total_players,
            'access_level': 'full',
            'results': results
        }
        
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