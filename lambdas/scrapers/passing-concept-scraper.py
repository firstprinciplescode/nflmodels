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

def scrape_passing_concept_for_week(season, week, cookies):
    """
    Scrape passing concept data for one season/week combination
    Returns: DataFrame with passing concept data
    """
    url = f'https://premium.pff.com/api/v1/facet/passing/concept?league=nfl&season={season}&week={week}'
    
    try:
        response = requests.get(url, cookies=cookies, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            
            if 'restricted' in data:
                restricted_fields = data.get('restricted', [])
                raise Exception(f"BAD COOKIES - Restricted fields present: {restricted_fields[:10]}")
            
            concept_data = data.get('passing_concept', [])
            
            if not concept_data:
                print(f"  No data for {season} Week {week}")
                return pd.DataFrame()
            
            if concept_data:
                first_record = concept_data[0]
                critical_fields = ['pa_grades_pass', 'npa_grades_pass', 'screen_grades_pass']
                missing_fields = [f for f in critical_fields if f not in first_record]
                
                if missing_fields:
                    raise Exception(f"BAD COOKIES - Missing critical fields: {missing_fields}")
            
            for record in concept_data:
                record['season'] = season
                record['week'] = week
                record['scraped_at'] = datetime.utcnow().isoformat()
            
            print(f"  ✓ {season} Week {week}: {len(concept_data)} records")
            return pd.DataFrame(concept_data)
        
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

def scrape_all_passing_concept(cookies, season, weeks):
    """Scrape passing concept data for all weeks in a season"""
    all_data = []
    
    print(f"\nScraping {season} - {len(weeks)} weeks...")
    
    for week in weeks:
        df = scrape_passing_concept_for_week(season, week, cookies)
        
        if not df.empty:
            all_data.append(df)
        
        time.sleep(0.1)
    
    if all_data:
        combined = pd.concat(all_data, ignore_index=True)
        print(f"✓ Total: {len(combined)} passing concept records")
        return combined
    else:
        return pd.DataFrame()

def save_to_s3_by_season(df, season):
    """
    Save passing concept data to S3, partitioned by season
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
        'dropbacks': 'Int64',
        'pa_passing_snaps': 'Int64',
        'pa_dropbacks': 'Int64',
        'pa_aimed_passes': 'Int64',
        'pa_attempts': 'Int64',
        'pa_completions': 'Int64',
        'pa_yards': 'Int64',
        'pa_touchdowns': 'Int64',
        'pa_interceptions': 'Int64',
        'pa_sacks': 'Int64',
        'pa_scrambles': 'Int64',
        'pa_spikes': 'Int64',
        'pa_thrown_aways': 'Int64',
        'pa_drops': 'Int64',
        'pa_bats': 'Int64',
        'pa_big_time_throws': 'Int64',
        'pa_turnover_worthy_plays': 'Int64',
        'pa_first_downs': 'Int64',
        'pa_def_gen_pressures': 'Int64',
        'pa_hit_as_threw': 'Int64',
        'npa_passing_snaps': 'Int64',
        'npa_dropbacks': 'Int64',
        'npa_aimed_passes': 'Int64',
        'npa_attempts': 'Int64',
        'npa_completions': 'Int64',
        'npa_yards': 'Int64',
        'npa_touchdowns': 'Int64',
        'npa_interceptions': 'Int64',
        'npa_sacks': 'Int64',
        'npa_scrambles': 'Int64',
        'npa_spikes': 'Int64',
        'npa_thrown_aways': 'Int64',
        'npa_drops': 'Int64',
        'npa_bats': 'Int64',
        'npa_big_time_throws': 'Int64',
        'npa_turnover_worthy_plays': 'Int64',
        'npa_first_downs': 'Int64',
        'npa_def_gen_pressures': 'Int64',
        'npa_hit_as_threw': 'Int64',
        'screen_passing_snaps': 'Int64',
        'screen_dropbacks': 'Int64',
        'screen_aimed_passes': 'Int64',
        'screen_attempts': 'Int64',
        'screen_completions': 'Int64',
        'screen_yards': 'Int64',
        'screen_touchdowns': 'Int64',
        'screen_interceptions': 'Int64',
        'screen_sacks': 'Int64',
        'screen_scrambles': 'Int64',
        'screen_spikes': 'Int64',
        'screen_thrown_aways': 'Int64',
        'screen_drops': 'Int64',
        'screen_bats': 'Int64',
        'screen_big_time_throws': 'Int64',
        'screen_turnover_worthy_plays': 'Int64',
        'screen_first_downs': 'Int64',
        'screen_def_gen_pressures': 'Int64',
        'screen_hit_as_threw': 'Int64',
        'no_screen_passing_snaps': 'Int64',
        'no_screen_dropbacks': 'Int64',
        'no_screen_aimed_passes': 'Int64',
        'no_screen_attempts': 'Int64',
        'no_screen_completions': 'Int64',
        'no_screen_yards': 'Int64',
        'no_screen_touchdowns': 'Int64',
        'no_screen_interceptions': 'Int64',
        'no_screen_sacks': 'Int64',
        'no_screen_scrambles': 'Int64',
        'no_screen_spikes': 'Int64',
        'no_screen_thrown_aways': 'Int64',
        'no_screen_drops': 'Int64',
        'no_screen_bats': 'Int64',
        'no_screen_big_time_throws': 'Int64',
        'no_screen_turnover_worthy_plays': 'Int64',
        'no_screen_first_downs': 'Int64',
        'no_screen_def_gen_pressures': 'Int64',
        'no_screen_hit_as_threw': 'Int64',
        'pa_grades_offense': 'float64',
        'pa_grades_pass': 'float64',
        'pa_grades_run': 'float64',
        'pa_grades_hands_fumble': 'float64',
        'pa_grades_offense_penalty': 'float64',
        'pa_grades_run_block': 'float64',
        'pa_completion_percent': 'float64',
        'pa_ypa': 'float64',
        'pa_qb_rating': 'float64',
        'pa_avg_depth_of_target': 'float64',
        'pa_avg_time_to_throw': 'float64',
        'pa_accuracy_percent': 'float64',
        'pa_drop_rate': 'float64',
        'pa_btt_rate': 'float64',
        'pa_twp_rate': 'float64',
        'pa_sack_percent': 'float64',
        'pa_dropbacks_percent': 'float64',
        'pa_pressure_to_sack_rate': 'float64',
        'npa_grades_offense': 'float64',
        'npa_grades_pass': 'float64',
        'npa_grades_run': 'float64',
        'npa_grades_hands_fumble': 'float64',
        'npa_grades_offense_penalty': 'float64',
        'npa_grades_run_block': 'float64',
        'npa_completion_percent': 'float64',
        'npa_ypa': 'float64',
        'npa_qb_rating': 'float64',
        'npa_avg_depth_of_target': 'float64',
        'npa_avg_time_to_throw': 'float64',
        'npa_accuracy_percent': 'float64',
        'npa_drop_rate': 'float64',
        'npa_btt_rate': 'float64',
        'npa_twp_rate': 'float64',
        'npa_sack_percent': 'float64',
        'npa_dropbacks_percent': 'float64',
        'npa_pressure_to_sack_rate': 'float64',
        'screen_grades_offense': 'float64',
        'screen_grades_pass': 'float64',
        'screen_grades_run': 'float64',
        'screen_grades_hands_fumble': 'float64',
        'screen_grades_offense_penalty': 'float64',
        'screen_grades_run_block': 'float64',
        'screen_completion_percent': 'float64',
        'screen_ypa': 'float64',
        'screen_qb_rating': 'float64',
        'screen_avg_depth_of_target': 'float64',
        'screen_avg_time_to_throw': 'float64',
        'screen_accuracy_percent': 'float64',
        'screen_drop_rate': 'float64',
        'screen_btt_rate': 'float64',
        'screen_twp_rate': 'float64',
        'screen_sack_percent': 'float64',
        'screen_dropbacks_percent': 'float64',
        'screen_pressure_to_sack_rate': 'float64',
        'no_screen_grades_offense': 'float64',
        'no_screen_grades_pass': 'float64',
        'no_screen_grades_run': 'float64',
        'no_screen_grades_hands_fumble': 'float64',
        'no_screen_grades_offense_penalty': 'float64',
        'no_screen_grades_run_block': 'float64',
        'no_screen_completion_percent': 'float64',
        'no_screen_ypa': 'float64',
        'no_screen_qb_rating': 'float64',
        'no_screen_avg_depth_of_target': 'float64',
        'no_screen_avg_time_to_throw': 'float64',
        'no_screen_accuracy_percent': 'float64',
        'no_screen_drop_rate': 'float64',
        'no_screen_btt_rate': 'float64',
        'no_screen_twp_rate': 'float64',
        'no_screen_sack_percent': 'float64',
        'no_screen_dropbacks_percent': 'float64',
        'no_screen_pressure_to_sack_rate': 'float64',
        'comp_pct_diff': 'float64',
        'ypa_diff': 'float64',
        'player': 'str',
        'position': 'str',
        'team': 'str',
        'team_name': 'str',
        'jersey_number': 'str',
        'scraped_at': 'str'
    }
    
    existing_columns = {k: v for k, v in type_mapping.items() if k in df.columns}
    df = df.astype(existing_columns)
    
    s3_key = f"data/passing_concept/season={season}/data.parquet"
    
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
    Lambda handler for passing concept data scraper
    
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
        print(f"PASSING CONCEPT SCRAPER - SEASON {season}")
        print(f"Weeks: {weeks}")
        print("=" * 60)
        
        cookies = get_cookies()
        
        df = scrape_all_passing_concept(cookies, season, weeks)
        
        if df.empty:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': f'No passing concept data found for season {season}'})
            }
        
        s3_key = save_to_s3_by_season(df, season)
        
        return {
            'statusCode': 200,
            'body': json.dumps({
                'message': 'Success',
                'season': season,
                'weeks_scraped': len(weeks),
                'passing_concept_records': len(df),
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