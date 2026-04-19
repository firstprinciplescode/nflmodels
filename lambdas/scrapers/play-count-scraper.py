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

def get_cookies():
    """Get PFF cookies from Secrets Manager"""
    response = secrets_client.get_secret_value(SecretId=SECRET_NAME)
    return json.loads(response['SecretString'])

def get_game_ids_from_s3(years, weeks):
    """
    Read game IDs from the games data already in S3
    Returns: list of game_ids
    """
    print("Reading game IDs from S3...")
    
    all_game_ids = []
    
    for year in years:
        for week in weeks:
            s3_key = f"data/games/season={year}/week={week}/data.parquet"
            
            try:
                response = s3_client.get_object(Bucket=BUCKET_NAME, Key=s3_key)
                parquet_data = response['Body'].read()
                
                df = pd.read_parquet(BytesIO(parquet_data))
                
                game_ids = df['id'].tolist()
                all_game_ids.extend(game_ids)
                
                print(f"✓ Found {len(game_ids)} games for {year} Week {week}")
                
            except s3_client.exceptions.NoSuchKey:
                print(f"⚠ No games data for {year} Week {week}")
                continue
            except Exception as e:
                print(f"Error reading {year} Week {week}: {e}")
                continue
    
    print(f"\n✓ Total game IDs to scrape: {len(all_game_ids)}")
    return all_game_ids

def validate_response_structure(data, game_id):
    """
    Validate that response has snap count data (not restricted)
    Returns: (is_valid, error_message)
    """
    if not data:
        return False, "Empty response"
    
    if 'restricted' in data:
        restricted_fields = data.get('restricted', [])
        return False, f"BAD COOKIES - Restricted fields present: {restricted_fields[:5]}"
    
    offense_summary = data.get('offense_summary', [])
    
    if not offense_summary:
        return True, None
    
    first_player = offense_summary[0]
    
    required_snap_fields = ['snap_counts_run', 'snap_counts_pass', 'snap_counts_total']
    
    missing_fields = [field for field in required_snap_fields if field not in first_player]
    
    if missing_fields:
        return False, f"BAD COOKIES - Missing critical fields: {missing_fields}. Only have: {list(first_player.keys())}"
    
    return True, None

def scrape_plays_for_game(game_id, cookies):
    """Scrape offense summary (plays/snaps) for a single game"""
    url = f'https://premium.pff.com/api/v1/facet/offense/summary?game_id={game_id}'
    
    try:
        response = requests.get(url, cookies=cookies, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            
            is_valid, error_msg = validate_response_structure(data, game_id)
            
            if not is_valid:
                raise Exception(f"Cookie validation failed on game {game_id}: {error_msg}")
            
            offense_summary = data.get('offense_summary', [])
            
            if offense_summary:
                df = pd.DataFrame(offense_summary)
                df['game_id'] = game_id
                df['scraped_at'] = datetime.utcnow().isoformat()
                return df
            else:
                return pd.DataFrame()
        
        elif response.status_code == 401:
            raise Exception("401 Unauthorized - cookies expired!")
        else:
            print(f"Failed game {game_id}: Status {response.status_code}")
            return pd.DataFrame()
            
    except Exception as e:
        print(f"Error scraping game {game_id}: {e}")
        if "Cookie validation failed" in str(e) or "BAD COOKIES" in str(e) or "401" in str(e):
            raise
        return pd.DataFrame()

def scrape_all_plays(cookies, game_ids):
    """Scrape plays for all game IDs"""
    all_plays = []
    total_games = len(game_ids)
    
    print(f"\nScraping plays for {total_games} games...")
    
    for idx, game_id in enumerate(game_ids, 1):
        if idx % 50 == 0:
            print(f"Progress: {idx}/{total_games} games...")
        
        df = scrape_plays_for_game(game_id, cookies)
        
        if not df.empty:
            all_plays.append(df)
    
    if all_plays:
        return pd.concat(all_plays, ignore_index=True)
    else:
        return pd.DataFrame()

def validate_plays_data(df):
    """Validate scraped plays data"""
    if df.empty:
        raise ValueError("No plays data scraped!")
    
    required_cols = ['game_id', 'player_id']
    
    for col in required_cols:
        if col not in df.columns:
            raise ValueError(f"Missing required column: {col}")
    
    print(f"✓ Validation passed: {len(df)} play records")
    return True

def save_to_s3_by_game(df):
    """
    Save plays data to S3, partitioned by game_id
    APPENDS to existing files and deduplicates by player_id
    """
    saved_files = []
    
    for game_id, group_df in df.groupby('game_id'):
        
        s3_key = f"data/play_counts/game_id={game_id}/data.parquet"
        
        # READ EXISTING FILE IF IT EXISTS AND APPEND
        try:
            print(f"Checking for existing data at {s3_key}...")
            existing = s3_client.get_object(Bucket=BUCKET_NAME, Key=s3_key)
            existing_df = pd.read_parquet(BytesIO(existing['Body'].read()))
            
            print(f"  Found {len(existing_df)} existing records")
            
            combined = pd.concat([existing_df, group_df], ignore_index=True)
            combined = combined.drop_duplicates(subset=['player_id', 'game_id'], keep='last')
            
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
            Body=parquet_buffer.getvalue()
        )
        
        saved_files.append(s3_key)
    
    print(f"✓ Saved {len(saved_files)} game files to S3")
    return saved_files

def lambda_handler(event, context):
    """
    Lambda handler for plays scraper
    
    Event options:
    - years: list of years to scrape plays for
    - weeks: list of weeks to scrape plays for
    """
    
    try:
        years = event.get('years', [2024])
        weeks = event.get('weeks', [1])
        
        print("=" * 60)
        print(f"PLAYS SCRAPER")
        print(f"Years: {years}")
        print(f"Weeks: {weeks}")
        print("=" * 60)
        
        cookies = get_cookies()
        
        game_ids = get_game_ids_from_s3(years, weeks)
        
        if not game_ids:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': 'No games found to scrape plays for'})
            }
        
        df = scrape_all_plays(cookies, game_ids)
        
        validate_plays_data(df)
        
        files_saved = save_to_s3_by_game(df)
        
        return {
            'statusCode': 200,
            'body': json.dumps({
                'message': 'Success',
                'games_processed': len(game_ids),
                'play_records': len(df),
                'files_saved': len(files_saved)
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