import json
import boto3
import requests
import pandas as pd
from io import BytesIO
from datetime import datetime
import time

s3_client = boto3.client('s3')
secrets_client = boto3.client('secretsmanager')

BUCKET_NAME = 'nfl-pff-data-lucas'
SECRET_NAME = 'pff-api-cookies'

def get_cookies():
    response = secrets_client.get_secret_value(SecretId=SECRET_NAME)
    return json.loads(response['SecretString'])

def scrape_coverage_summary(season, week, cookies):
    url = f'https://premium.pff.com/api/v1/facet/defense/coverage?league=nfl&season={season}&week={week}'
    
    try:
        response = requests.get(url, cookies=cookies, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            
            if 'restricted' in data and data['restricted']:
                raise Exception(f"RESTRICTED ACCESS - cookies expired. Missing fields: {', '.join(data['restricted'][:5])}...")
            
            coverage_data = data.get('coverage_summary', [])
            
            if not coverage_data:
                return pd.DataFrame()
            
            df = pd.DataFrame(coverage_data)
            df['week'] = week
            df['season'] = season
            df['scraped_at'] = datetime.utcnow().isoformat()
            
            return df
        
        elif response.status_code == 401:
            raise Exception("401 Unauthorized - cookies expired!")
        else:
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e) or "RESTRICTED" in str(e):
            raise
        return pd.DataFrame()

def save_to_s3(df, season):
    type_mapping = {
        'player_id': 'float64',
        'season': 'float64',
        'week': 'float64',
        'player_game_count': 'float64',
        'franchise_id': 'float64',
        'draft_season': 'float64',
        'eligible_season': 'float64',
        'targets': 'float64',
        'receptions': 'float64',
        'yards': 'float64',
        'touchdowns': 'float64',
        'interceptions': 'float64',
        'dropped_ints': 'float64',
        'forced_incompletes': 'float64',
        'pass_break_ups': 'float64',
        'tackles': 'float64',
        'assists': 'float64',
        'missed_tackles': 'float64',
        'stops': 'float64',
        'penalties': 'float64',
        'declined_penalties': 'float64',
        'snap_counts_coverage': 'float64',
        'snap_counts_pass_play': 'float64',
        'longest': 'float64',
        'yards_after_catch': 'float64',
        'yards_per_coverage_snap': 'float64',
        'catch_rate': 'float64',
        'coverage_percent': 'float64',
        'yards_per_reception': 'float64',
        'avg_depth_of_target': 'float64',
        'qb_rating_against': 'float64',
        'coverage_snaps_per_target': 'float64',
        'coverage_snaps_per_reception': 'float64',
        'forced_incompletion_rate': 'float64',
        'missed_tackle_rate': 'float64',
        'grades_defense': 'float64',
        'grades_coverage_defense': 'float64',
        'grades_run_defense': 'float64',
        'grades_pass_rush_defense': 'float64',
        'grades_tackle': 'float64',
        'grades_defense_penalty': 'float64',
        'player': 'str',
        'position': 'str',
        'team': 'str',
        'team_name': 'str',
        'jersey_number': 'str',
        'scraped_at': 'str'
    }
    
    existing_columns = {k: v for k, v in type_mapping.items() if k in df.columns}
    df = df.astype(existing_columns)
    
    s3_key = f'data/coverage_summary/season={season}/data.parquet'
    
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
    try:
        season = event.get('season')
        weeks = event.get('weeks')
        
        if not season:
            raise Exception("season parameter required")
        
        if not weeks:
            weeks = list(range(1, 19)) + [28, 29, 30, 31, 32]
        
        print("=" * 60)
        print(f"COVERAGE SUMMARY SCRAPER - Season {season}")
        print(f"Weeks: {weeks}")
        print("=" * 60)
        
        cookies = get_cookies()
        
        all_data = []
        total = len(weeks)
        
        for idx, week in enumerate(weeks, 1):
            print(f"Scraping week {week} ({idx}/{total})...")
            
            df = scrape_coverage_summary(season, week, cookies)
            
            if not df.empty:
                all_data.append(df)
            
            time.sleep(0.1)
        
        if not all_data:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': 'No data found'})
            }
        
        combined = pd.concat(all_data, ignore_index=True)
        
        s3_key = save_to_s3(combined, season)
        
        return {
            'statusCode': 200,
            'body': json.dumps({
                'message': 'Success',
                'season': season,
                'weeks': weeks,
                'records': len(combined),
                'unique_players': combined['player_id'].nunique(),
                's3_key': s3_key
            })
        }
        
    except Exception as e:
        print(f"ERROR: {e}")
        return {
            'statusCode': 500,
            'body': json.dumps({'error': str(e)})
        }