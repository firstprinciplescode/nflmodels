import json
import boto3
import requests
import pandas as pd
from io import BytesIO
from datetime import datetime

# AWS clients
secrets_client = boto3.client('secretsmanager')
s3_client = boto3.client('s3')

# Configuration
BUCKET_NAME = 'nfl-pff-data-lucas'
SECRET_NAME = 'pff-api-cookies'

# All possible years and weeks
ALL_YEARS = [2025, 2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016]
ALL_WEEKS = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,28,29,30,32]

def get_cookies():
    """Get PFF cookies from Secrets Manager"""
    response = secrets_client.get_secret_value(SecretId=SECRET_NAME)
    return json.loads(response['SecretString'])

def get_existing_files_from_s3():
    """
    Check what data already exists in S3
    Returns: set of (year, week) tuples that already exist
    """
    try:
        prefix = 'data/games/'
        
        response = s3_client.list_objects_v2(
            Bucket=BUCKET_NAME,
            Prefix=prefix
        )
        
        if 'Contents' not in response:
            print("No existing files found - will do full load")
            return set()
        
        existing = set()
        
        for obj in response['Contents']:
            key = obj['Key']
            if 'season=' in key and 'week=' in key:
                try:
                    parts = key.split('/')
                    season = int([p for p in parts if 'season=' in p][0].split('=')[1])
                    week = int([p for p in parts if 'week=' in p][0].split('=')[1])
                    existing.add((season, week))
                except:
                    continue
        
        print(f"Found {len(existing)} existing year/week combinations in S3")
        return existing
        
    except Exception as e:
        print(f"Error checking existing data: {e}")
        return set()

def scrape_games(cookies, years, weeks):
    """
    Scrape game data from PFF API
    Returns: DataFrame with game data
    """
    flattened_data = []
    
    for year in years:
        for week in weeks:
            url = f'https://premium.pff.com/api/v1/games?league=nfl&season={year}&week={week}'
            print(f"Fetching: {year} Week {week}")
            
            try:
                response = requests.get(url, cookies=cookies, timeout=10)
                
                if response.status_code == 200:
                    games = response.json().get('games', [])
                    
                    for game in games:
                        flattened_game = {
                            'id': game.get('id', None),
                            'season': game.get('season', None),
                            'week': game.get('week', None),
                            'away_abbreviation': game['away_team'].get('abbreviation', None),
                            'away_franchise_id': game.get('away_franchise_id', None),
                            'home_abbreviation': game['home_team'].get('abbreviation', None),
                            'home_franchise_id': game.get('home_franchise_id', None),
                            'away_score': game.get('score', {}).get('away_team', None),
                            'home_score': game.get('score', {}).get('home_team', None),
                            'scraped_at': datetime.utcnow().isoformat()
                        }
                        flattened_data.append(flattened_game)
                
                elif response.status_code == 401:
                    raise Exception("401 Unauthorized - cookies expired!")
                else:
                    print(f"Failed {year} Week {week}: Status {response.status_code}")
                    
            except Exception as e:
                print(f"Error fetching {year} Week {week}: {e}")
                continue
    
    return pd.DataFrame(flattened_data)

def validate_data(df, expected_columns=9):
    """Validate scraped data quality"""
    if df.empty:
        raise ValueError("No data scraped!")
    
    required_cols = ['id', 'season', 'week', 'away_abbreviation', 'home_abbreviation']
    
    for col in required_cols:
        if col not in df.columns:
            raise ValueError(f"Missing required column: {col}")
        if df[col].isna().all():
            raise ValueError(f"Column {col} is all null - possible cookie issue")
    
    if len(df.columns) < expected_columns:
        raise ValueError(f"Only {len(df.columns)} columns (expected {expected_columns}+)")
    
    print(f"✓ Validation passed: {len(df)} records, {len(df.columns)} columns")
    return True

def save_to_s3_partitioned(df):
    """
    Save data to S3 partitioned by season and week (Athena-friendly)
    APPENDS to existing files and deduplicates by game id
    """
    
    df = df.astype({
        'id': 'Int64',
        'season': 'Int64',
        'week': 'Int64',
        'away_franchise_id': 'Int64',
        'home_franchise_id': 'Int64',
        'away_score': 'Int64',
        'home_score': 'Int64',
        'away_abbreviation': 'str',
        'home_abbreviation': 'str',
        'scraped_at': 'str'
    })
    
    saved_partitions = []
    
    for (season, week), group_df in df.groupby(['season', 'week']):
        
        s3_key = f"data/games/season={season}/week={week}/data.parquet"
        
        # READ EXISTING FILE IF IT EXISTS AND APPEND
        try:
            print(f"Checking for existing data at {s3_key}...")
            existing = s3_client.get_object(Bucket=BUCKET_NAME, Key=s3_key)
            existing_df = pd.read_parquet(BytesIO(existing['Body'].read()))
            
            print(f"  Found {len(existing_df)} existing records")
            
            combined = pd.concat([existing_df, group_df], ignore_index=True)
            combined = combined.drop_duplicates(subset=['id'], keep='last')
            
            print(f"  After merge: {len(combined)} total records")
            group_df = combined
            
        except s3_client.exceptions.NoSuchKey:
            print(f"  No existing file found, creating new")
        
        parquet_buffer = BytesIO()
        group_df.to_parquet(
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
        
        print(f"✓ Saved {len(group_df)} records to s3://{BUCKET_NAME}/{s3_key}")
        saved_partitions.append(s3_key)
    
    return saved_partitions

def lambda_handler(event, context):
    """
    Lambda handler for game scraper (S3-only version)
    
    Event options:
    - mode: 'full' (scrape all years/weeks) or 'incremental' (only new data)
    - years: list of specific years (optional)
    - weeks: list of specific weeks (optional)
    """
    
    try:
        mode = event.get('mode', 'incremental')
        
        print("=" * 60)
        print(f"MODE: {mode.upper()}")
        print("=" * 60)
        
        cookies = get_cookies()
        
        if mode == 'full':
            years = event.get('years', ALL_YEARS)
            weeks = event.get('weeks', ALL_WEEKS)
            print(f"Full load: {len(years)} years, {len(weeks)} weeks")
            
        elif mode == 'incremental':
            existing = get_existing_files_from_s3()
            
            current_year = datetime.now().year
            current_week = event.get('current_week', 7)
            
            years_weeks_to_scrape = [
                (current_year, w) 
                for w in range(1, current_week + 1)
                if (current_year, w) not in existing
            ]
            
            if not years_weeks_to_scrape:
                print("No new data to scrape!")
                return {
                    'statusCode': 200,
                    'body': json.dumps({'message': 'No new data needed'})
                }
            
            years = list(set(y for y, w in years_weeks_to_scrape))
            weeks = list(set(w for y, w in years_weeks_to_scrape))
            
            print(f"Incremental load: {len(years_weeks_to_scrape)} new year/week combinations")
        
        else:
            raise ValueError(f"Invalid mode: {mode}")
        
        print("\n" + "=" * 60)
        print("SCRAPING...")
        print("=" * 60)
        df = scrape_games(cookies, years, weeks)
        
        validate_data(df)
        
        print("\n" + "=" * 60)
        print("SAVING TO S3...")
        print("=" * 60)
        partitions = save_to_s3_partitioned(df)
        
        return {
            'statusCode': 200,
            'body': json.dumps({
                'message': 'Success',
                'mode': mode,
                'records_scraped': len(df),
                'partitions_saved': len(partitions),
                's3_bucket': BUCKET_NAME
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