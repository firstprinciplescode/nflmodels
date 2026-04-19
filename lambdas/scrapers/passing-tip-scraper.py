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

def scrape_time_in_pocket_for_week(season, week, cookies):
    """
    Scrape time in pocket data for one season/week combination
    Returns: DataFrame with time in pocket data
    """
    url = f'https://premium.pff.com/api/v1/facet/signature/passing/time_in_pocket?league=nfl&season={season}&week={week}'
    
    try:
        response = requests.get(url, cookies=cookies, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            
            if 'restricted' in data:
                restricted_fields = data.get('restricted', [])
                raise Exception(f"BAD COOKIES - Restricted fields present: {restricted_fields[:10]}")
            
            tip_data = data.get('time_in_pockets', [])
            
            if not tip_data:
                print(f"  No data for {season} Week {week}")
                return pd.DataFrame()
            
            if tip_data:
                first_record = tip_data[0]
                critical_fields = ['more_grades_pass', 'less_grades_pass', 'avg_time_to_throw']
                missing_fields = [f for f in critical_fields if f not in first_record]
                
                if missing_fields:
                    raise Exception(f"BAD COOKIES - Missing critical fields: {missing_fields}")
            
            for record in tip_data:
                record['season'] = season
                record['week'] = week
                record['scraped_at'] = datetime.utcnow().isoformat()
            
            print(f"  ✓ {season} Week {week}: {len(tip_data)} records")
            return pd.DataFrame(tip_data)
        
        elif response.status_code == 401:
            raise Exception("401 Unauthorized - cookies expired!")
        else:
            print(f"  Failed {season} Week {week}: Status {response.status_code}")
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e) or "BAD COOKIES" in str(e):
            raise
        print(f"  Error scraping {season} Week {week}: {e}")
        return pd.DataFrame()

def scrape_all_time_in_pocket(cookies, season, weeks):
    """Scrape time in pocket data for all weeks in a season"""
    all_data = []
    
    print(f"\nScraping {season} - {len(weeks)} weeks...")
    
    for week in weeks:
        df = scrape_time_in_pocket_for_week(season, week, cookies)
        
        if not df.empty:
            all_data.append(df)
        
        time.sleep(0.1)
    
    if all_data:
        combined = pd.concat(all_data, ignore_index=True)
        print(f"✓ Total: {len(combined)} time in pocket records")
        return combined
    else:
        return pd.DataFrame()

def save_to_s3_by_season(df, season):
    """
    Save time in pocket data to S3, partitioned by season
    APPENDS to existing file and deduplicates by player_id/week/season
    """
    
    type_mapping = {
        'player_id': 'Int64',
        'franchise_id': 'Int64',
        'season': 'Int64',
        'week': 'Int64',
        'player_game_count': 'Int64',
        'draft_season': 'Int64',
        'eligible_season': 'Int64',
        'dropbacks': 'Int64',
        'more_passing_snaps': 'Int64',
        'more_dropbacks': 'Int64',
        'more_aimed_passes': 'Int64',
        'more_attempts': 'Int64',
        'more_completions': 'Int64',
        'more_yards': 'Int64',
        'more_touchdowns': 'Int64',
        'more_interceptions': 'Int64',
        'more_sacks': 'Int64',
        'more_scrambles': 'Int64',
        'more_spikes': 'Int64',
        'more_thrown_aways': 'Int64',
        'more_drops': 'Int64',
        'more_bats': 'Int64',
        'more_big_time_throws': 'Int64',
        'more_turnover_worthy_plays': 'Int64',
        'more_first_downs': 'Int64',
        'more_def_gen_pressures': 'Int64',
        'more_hit_as_threw': 'Int64',
        'less_passing_snaps': 'Int64',
        'less_dropbacks': 'Int64',
        'less_aimed_passes': 'Int64',
        'less_attempts': 'Int64',
        'less_completions': 'Int64',
        'less_yards': 'Int64',
        'less_touchdowns': 'Int64',
        'less_interceptions': 'Int64',
        'less_sacks': 'Int64',
        'less_scrambles': 'Int64',
        'less_spikes': 'Int64',
        'less_thrown_aways': 'Int64',
        'less_drops': 'Int64',
        'less_bats': 'Int64',
        'less_big_time_throws': 'Int64',
        'less_turnover_worthy_plays': 'Int64',
        'less_first_downs': 'Int64',
        'less_def_gen_pressures': 'Int64',
        'less_hit_as_threw': 'Int64',
        'more_grades_offense': 'float64',
        'more_grades_pass': 'float64',
        'more_grades_run': 'float64',
        'more_grades_hands_fumble': 'float64',
        'more_grades_offense_penalty': 'float64',
        'more_grades_run_block': 'float64',
        'more_completion_percent': 'float64',
        'more_ypa': 'float64',
        'more_qb_rating': 'float64',
        'more_avg_depth_of_target': 'float64',
        'more_avg_time_to_throw': 'float64',
        'more_accuracy_percent': 'float64',
        'more_drop_rate': 'float64',
        'more_btt_rate': 'float64',
        'more_twp_rate': 'float64',
        'more_sack_percent': 'float64',
        'more_dropbacks_percent': 'float64',
        'more_pressure_to_sack_rate': 'float64',
        'less_grades_offense': 'float64',
        'less_grades_pass': 'float64',
        'less_grades_run': 'float64',
        'less_grades_hands_fumble': 'float64',
        'less_grades_offense_penalty': 'float64',
        'less_grades_run_block': 'float64',
        'less_completion_percent': 'float64',
        'less_ypa': 'float64',
        'less_qb_rating': 'float64',
        'less_avg_depth_of_target': 'float64',
        'less_avg_time_to_throw': 'float64',
        'less_accuracy_percent': 'float64',
        'less_drop_rate': 'float64',
        'less_btt_rate': 'float64',
        'less_twp_rate': 'float64',
        'less_sack_percent': 'float64',
        'less_dropbacks_percent': 'float64',
        'less_pressure_to_sack_rate': 'float64',
        'avg_time_to_throw': 'float64',
        'avg_ttt_attempts': 'float64',
        'avg_ttt_sacks': 'float64',
        'avg_ttt_scrambles': 'float64',
        'player': 'str',
        'position': 'str',
        'team': 'str',
        'team_name': 'str',
        'jersey_number': 'str',
        'scraped_at': 'str'
    }
    
    existing_columns = {k: v for k, v in type_mapping.items() if k in df.columns}
    df = df.astype(existing_columns)
    
    s3_key = f"data/time_in_pocket/season={season}/data.parquet"
    
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
    Lambda handler for time in pocket data scraper
    
    Event:
    {
        "season": 2025,
        "weeks": [1, 2, 3, 4, 5, 6, 7, 8]
    }
    """
    
    try:
        season = event.get('season')
        
        if not season:
            raise ValueError("Must provide 'season' in event (e.g., {'season': 2025})")
        
        weeks = event.get('weeks', [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 28, 29, 30, 32])
        
        print("=" * 60)
        print(f"TIME IN POCKET SCRAPER - SEASON {season}")
        print(f"Weeks: {weeks}")
        print("=" * 60)
        
        cookies = get_cookies()
        
        df = scrape_all_time_in_pocket(cookies, season, weeks)
        
        if df.empty:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': f'No time in pocket data found for season {season}'})
            }
        
        s3_key = save_to_s3_by_season(df, season)
        
        return {
            'statusCode': 200,
            'body': json.dumps({
                'message': 'Success',
                'season': season,
                'weeks_scraped': len(weeks),
                'time_in_pocket_records': len(df),
                's3_key': s3_key
            })
        }
        
    except Exception as e:
        print(f"ERROR: {e}")
        
        return {
            'statusCode': 500,
            'body': json.dumps({
                'error': str(e)
            })
        }