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

def scrape_passing_pressure_for_week(season, week, cookies):
    """
    Scrape passing pressure data for one season/week combination
    Returns: DataFrame with passing pressure data
    """
    url = f'https://premium.pff.com/api/v1/facet/passing/pressure?league=nfl&season={season}&week={week}'
    
    try:
        response = requests.get(url, cookies=cookies, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            
            if 'restricted' in data:
                restricted_fields = data.get('restricted', [])
                raise Exception(f"BAD COOKIES - Restricted fields present: {restricted_fields[:10]}")
            
            pressure_data = data.get('passing_pressure', [])
            
            if not pressure_data:
                print(f"  No data for {season} Week {week}")
                return pd.DataFrame()
            
            if pressure_data:
                first_record = pressure_data[0]
                critical_fields = ['grades_pass', 'pressure_completion_percent', 'pressure_ypa']
                missing_fields = [f for f in critical_fields if f not in first_record]
                
                if missing_fields:
                    raise Exception(f"BAD COOKIES - Missing critical fields: {missing_fields}")
            
            for record in pressure_data:
                record['season'] = season
                record['week'] = week
                record['scraped_at'] = datetime.utcnow().isoformat()
            
            print(f"  ✓ {season} Week {week}: {len(pressure_data)} records")
            return pd.DataFrame(pressure_data)
        
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

def scrape_all_passing_pressure(cookies, season, weeks):
    """Scrape passing pressure data for all weeks in a season"""
    all_data = []
    
    print(f"\nScraping {season} - {len(weeks)} weeks...")
    
    for week in weeks:
        df = scrape_passing_pressure_for_week(season, week, cookies)
        
        if not df.empty:
            all_data.append(df)
        
        time.sleep(0.1)
    
    if all_data:
        combined = pd.concat(all_data, ignore_index=True)
        print(f"✓ Total: {len(combined)} passing pressure records")
        return combined
    else:
        return pd.DataFrame()

def save_to_s3_by_season(df, season):
    """
    Save passing pressure data to S3, partitioned by season
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
        'penalties': 'Int64',
        'declined_penalties': 'Int64',
        'pressure_passing_snaps': 'Int64',
        'pressure_dropbacks': 'Int64',
        'pressure_aimed_passes': 'Int64',
        'pressure_attempts': 'Int64',
        'pressure_completions': 'Int64',
        'pressure_yards': 'Int64',
        'pressure_touchdowns': 'Int64',
        'pressure_interceptions': 'Int64',
        'pressure_sacks': 'Int64',
        'pressure_scrambles': 'Int64',
        'pressure_spikes': 'Int64',
        'pressure_thrown_aways': 'Int64',
        'pressure_drops': 'Int64',
        'pressure_bats': 'Int64',
        'pressure_big_time_throws': 'Int64',
        'pressure_turnover_worthy_plays': 'Int64',
        'pressure_first_downs': 'Int64',
        'pressure_def_gen_pressures': 'Int64',
        'pressure_hit_as_threw': 'Int64',
        'no_pressure_passing_snaps': 'Int64',
        'no_pressure_dropbacks': 'Int64',
        'no_pressure_aimed_passes': 'Int64',
        'no_pressure_attempts': 'Int64',
        'no_pressure_completions': 'Int64',
        'no_pressure_yards': 'Int64',
        'no_pressure_touchdowns': 'Int64',
        'no_pressure_interceptions': 'Int64',
        'no_pressure_sacks': 'Int64',
        'no_pressure_scrambles': 'Int64',
        'no_pressure_spikes': 'Int64',
        'no_pressure_thrown_aways': 'Int64',
        'no_pressure_drops': 'Int64',
        'no_pressure_bats': 'Int64',
        'no_pressure_big_time_throws': 'Int64',
        'no_pressure_turnover_worthy_plays': 'Int64',
        'no_pressure_first_downs': 'Int64',
        'no_pressure_def_gen_pressures': 'Int64',
        'no_pressure_hit_as_threw': 'Int64',
        'blitz_passing_snaps': 'Int64',
        'blitz_dropbacks': 'Int64',
        'blitz_aimed_passes': 'Int64',
        'blitz_attempts': 'Int64',
        'blitz_completions': 'Int64',
        'blitz_yards': 'Int64',
        'blitz_touchdowns': 'Int64',
        'blitz_interceptions': 'Int64',
        'blitz_sacks': 'Int64',
        'blitz_scrambles': 'Int64',
        'blitz_spikes': 'Int64',
        'blitz_thrown_aways': 'Int64',
        'blitz_drops': 'Int64',
        'blitz_bats': 'Int64',
        'blitz_big_time_throws': 'Int64',
        'blitz_turnover_worthy_plays': 'Int64',
        'blitz_first_downs': 'Int64',
        'blitz_def_gen_pressures': 'Int64',
        'blitz_hit_as_threw': 'Int64',
        'no_blitz_passing_snaps': 'Int64',
        'no_blitz_dropbacks': 'Int64',
        'no_blitz_aimed_passes': 'Int64',
        'no_blitz_attempts': 'Int64',
        'no_blitz_completions': 'Int64',
        'no_blitz_yards': 'Int64',
        'no_blitz_touchdowns': 'Int64',
        'no_blitz_interceptions': 'Int64',
        'no_blitz_sacks': 'Int64',
        'no_blitz_scrambles': 'Int64',
        'no_blitz_spikes': 'Int64',
        'no_blitz_thrown_aways': 'Int64',
        'no_blitz_drops': 'Int64',
        'no_blitz_bats': 'Int64',
        'no_blitz_big_time_throws': 'Int64',
        'no_blitz_turnover_worthy_plays': 'Int64',
        'no_blitz_first_downs': 'Int64',
        'no_blitz_def_gen_pressures': 'Int64',
        'no_blitz_hit_as_threw': 'Int64',
        'base_dropbacks': 'Int64',
        'grades_offense': 'float64',
        'grades_pass': 'float64',
        'grades_run': 'float64',
        'grades_hands_fumble': 'float64',
        'pressure_grades_offense': 'float64',
        'pressure_grades_pass': 'float64',
        'pressure_grades_run': 'float64',
        'pressure_grades_hands_fumble': 'float64',
        'pressure_grades_offense_penalty': 'float64',
        'pressure_completion_percent': 'float64',
        'pressure_ypa': 'float64',
        'pressure_qb_rating': 'float64',
        'pressure_avg_depth_of_target': 'float64',
        'pressure_avg_time_to_throw': 'float64',
        'pressure_accuracy_percent': 'float64',
        'pressure_drop_rate': 'float64',
        'pressure_btt_rate': 'float64',
        'pressure_twp_rate': 'float64',
        'pressure_sack_percent': 'float64',
        'pressure_dropbacks_percent': 'float64',
        'pressure_pressure_to_sack_rate': 'float64',
        'no_pressure_grades_offense': 'float64',
        'no_pressure_grades_pass': 'float64',
        'no_pressure_grades_run': 'float64',
        'no_pressure_grades_hands_fumble': 'float64',
        'no_pressure_grades_offense_penalty': 'float64',
        'no_pressure_completion_percent': 'float64',
        'no_pressure_ypa': 'float64',
        'no_pressure_qb_rating': 'float64',
        'no_pressure_avg_depth_of_target': 'float64',
        'no_pressure_avg_time_to_throw': 'float64',
        'no_pressure_accuracy_percent': 'float64',
        'no_pressure_drop_rate': 'float64',
        'no_pressure_btt_rate': 'float64',
        'no_pressure_twp_rate': 'float64',
        'no_pressure_sack_percent': 'float64',
        'no_pressure_dropbacks_percent': 'float64',
        'no_pressure_pressure_to_sack_rate': 'float64',
        'blitz_grades_offense': 'float64',
        'blitz_grades_pass': 'float64',
        'blitz_grades_run': 'float64',
        'blitz_grades_hands_fumble': 'float64',
        'blitz_grades_offense_penalty': 'float64',
        'blitz_completion_percent': 'float64',
        'blitz_ypa': 'float64',
        'blitz_qb_rating': 'float64',
        'blitz_avg_depth_of_target': 'float64',
        'blitz_avg_time_to_throw': 'float64',
        'blitz_accuracy_percent': 'float64',
        'blitz_drop_rate': 'float64',
        'blitz_btt_rate': 'float64',
        'blitz_twp_rate': 'float64',
        'blitz_sack_percent': 'float64',
        'blitz_dropbacks_percent': 'float64',
        'blitz_pressure_to_sack_rate': 'float64',
        'no_blitz_grades_offense': 'float64',
        'no_blitz_grades_pass': 'float64',
        'no_blitz_grades_run': 'float64',
        'no_blitz_grades_hands_fumble': 'float64',
        'no_blitz_grades_offense_penalty': 'float64',
        'no_blitz_completion_percent': 'float64',
        'no_blitz_ypa': 'float64',
        'no_blitz_qb_rating': 'float64',
        'no_blitz_avg_depth_of_target': 'float64',
        'no_blitz_avg_time_to_throw': 'float64',
        'no_blitz_accuracy_percent': 'float64',
        'no_blitz_drop_rate': 'float64',
        'no_blitz_btt_rate': 'float64',
        'no_blitz_twp_rate': 'float64',
        'no_blitz_sack_percent': 'float64',
        'no_blitz_dropbacks_percent': 'float64',
        'no_blitz_pressure_to_sack_rate': 'float64',
        'player': 'str',
        'position': 'str',
        'team': 'str',
        'team_name': 'str',
        'jersey_number': 'str',
        'scraped_at': 'str'
    }
    
    existing_columns = {k: v for k, v in type_mapping.items() if k in df.columns}
    df = df.astype(existing_columns)
    
    s3_key = f"data/passing_pressure/season={season}/data.parquet"
    
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
    Lambda handler for passing pressure data scraper
    
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
        print(f"PASSING PRESSURE SCRAPER - SEASON {season}")
        print(f"Weeks: {weeks}")
        print("=" * 60)
        
        cookies = get_cookies()
        
        df = scrape_all_passing_pressure(cookies, season, weeks)
        
        if df.empty:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': f'No passing pressure data found for season {season}'})
            }
        
        s3_key = save_to_s3_by_season(df, season)
        
        return {
            'statusCode': 200,
            'body': json.dumps({
                'message': 'Success',
                'season': season,
                'weeks_scraped': len(weeks),
                'passing_pressure_records': len(df),
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