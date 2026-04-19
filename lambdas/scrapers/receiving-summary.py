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

def scrape_receiving_week(season, week, cookies):
    """
    Scrape receiving data for ONE week - gets ALL players automatically
    Returns: DataFrame with all players for that week
    """
    url = f'https://premium.pff.com/api/v1/facet/receiving/summary?league=nfl&season={season}&week={week}'
    
    try:
        response = requests.get(url, cookies=cookies, timeout=30)
        
        if response.status_code == 200:
            data = response.json()
            
            if 'restricted' in data and data['restricted']:
                raise Exception(f"RESTRICTED ACCESS DETECTED - cookies only provide limited fields. Missing fields: {', '.join(data['restricted'][:5])}... Update cookies for full access!")
            
            receiving_data = data.get('receiving_summary', [])
            
            if not receiving_data:
                return pd.DataFrame()
            
            for record in receiving_data:
                if 'season' not in record:
                    record['season'] = season
                if 'week' not in record:
                    record['week'] = week
                record['scraped_at'] = datetime.utcnow().isoformat()
            
            return pd.DataFrame(receiving_data)
        
        elif response.status_code == 401:
            raise Exception("401 Unauthorized - cookies expired!")
        else:
            print(f"  ⚠ Week {week}: HTTP {response.status_code}")
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e) or "RESTRICTED" in str(e):
            raise
        print(f"  ⚠ Week {week}: {str(e)}")
        return pd.DataFrame()

def scrape_all_weeks(season, weeks, cookies):
    """Scrape receiving data for multiple weeks"""
    all_data = []
    
    print(f"\nScraping {len(weeks)} weeks for season {season}...")
    
    for week in weeks:
        print(f"  Scraping week {week}...", end='')
        
        df = scrape_receiving_week(season, week, cookies)
        
        if not df.empty:
            all_data.append(df)
            print(f" ✓ {len(df)} players")
        else:
            print(f" (no data)")
        
        time.sleep(0.5)
    
    if all_data:
        combined = pd.concat(all_data, ignore_index=True)
        print(f"\n✓ Total scraped: {len(combined)} player-week records")
        return combined
    else:
        return pd.DataFrame()

def save_to_s3_by_season(df, season):
    """
    Save receiving data to S3 partitioned by season with consistent types
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
        'grades_offense': 'float64',
        'grades_pass_route': 'float64',
        'grades_hands_drop': 'float64',
        'grades_hands_fumble': 'float64',
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
        'team_name': 'str',
        'team': 'str',
        'player': 'str',
        'position': 'str',
        'jersey_number': 'str',
        'status': 'str',
        'scraped_at': 'str'
    }
    
    existing_columns = {k: v for k, v in type_mapping.items() if k in df.columns}
    df = df.astype(existing_columns)
    
    s3_key = f"data/receiving_summary_with_targets/season={season}/data.parquet"
    
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
    Lambda handler for receiving summary scraper
    Uses facet endpoint - gets all players in one call per week
    
    Event:
    {
        "season": 2024,
        "weeks": [1, 2, 3]
    }
    """
    
    try:
        season = event.get('season')
        weeks = event.get('weeks')
        
        if weeks is None:
            weeks = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,28,29,30,32]
        
        if not season:
            raise ValueError("Must provide 'season' in event")
        
        print("=" * 60)
        print(f"RECEIVING SUMMARY SCRAPER - SEASON {season}")
        print(f"Weeks: {weeks}")
        print("=" * 60)
        
        cookies = get_cookies()
        
        df = scrape_all_weeks(season, weeks, cookies)
        
        if df.empty:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': f'No data found for season {season}'})
            }
        
        s3_key = save_to_s3_by_season(df, season)
        
        return {
            'statusCode': 200,
            'body': json.dumps({
                'message': 'Success',
                'season': season,
                'weeks_scraped': len(weeks),
                'total_records': len(df),
                'unique_players': df['player_id'].nunique() if 'player_id' in df.columns else 0,
                's3_key': s3_key
            })
        }
        
    except Exception as e:
        print(f"ERROR: {e}")
        return {
            'statusCode': 500,
            'body': json.dumps({'error': str(e)})
        }