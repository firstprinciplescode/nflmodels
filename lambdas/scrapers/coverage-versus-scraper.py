import json
import boto3
import requests
import pandas as pd
from io import BytesIO
from datetime import datetime
import time

s3_client = boto3.client('s3')
secrets_client = boto3.client('secretsmanager')
athena_client = boto3.client('athena')

BUCKET_NAME = 'nfl-pff-data-lucas'
SECRET_NAME = 'pff-api-cookies'
ATHENA_OUTPUT = f's3://{BUCKET_NAME}/athena-results/'

def get_cookies():
    response = secrets_client.get_secret_value(SecretId=SECRET_NAME)
    return json.loads(response['SecretString'])

def query_game_ids(season, weeks=None):
    """Query Athena for game_ids by season/week"""
    
    if isinstance(season, list):
        seasons_str = ','.join(map(str, season))
        season_clause = f"season IN ({seasons_str})"
    else:
        season_clause = f"season = {season}"
    
    query = f"""
        SELECT DISTINCT id as game_id, season, week
        FROM nfl_data.games
        WHERE {season_clause}
    """
    
    if weeks:
        weeks_str = ','.join(map(str, weeks))
        query += f" AND week IN ({weeks_str})"
    
    query += " ORDER BY week, game_id"
    
    print(f"Querying game_ids for season {season}, weeks {weeks or 'ALL'}...")
    
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
    
    games = []
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
                game_id = int(row['Data'][0].get('VarCharValue'))
                season_val = int(row['Data'][1].get('VarCharValue'))
                week = int(row['Data'][2].get('VarCharValue'))
                games.append({'game_id': game_id, 'season': season_val, 'week': week})
            except:
                continue
        
        next_token = results.get('NextToken')
        if not next_token:
            break
    
    print(f"Found {len(games)} games")
    return games

def scrape_receiving_coverage_versus(game_id, season, week, cookies):
    """Scrape receiving coverage versus (matchup-level) data for ONE game"""
    url = f'https://premium.pff.com/api/v1/facet/receiving/coverage?game_id={game_id}'
    
    try:
        response = requests.get(url, cookies=cookies, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            
            # Hard stop on restricted access
            if 'restricted' in data and data['restricted']:
                raise Exception(f"RESTRICTED ACCESS - cookies expired. Missing fields: {', '.join(str(f) for f in data['restricted'][:5])}...")
            
            # Check nested structure
            recv_cov = data.get('receiving_coverage_stats', {})
            
            # Also check restricted at the nested level
            if 'restricted' in recv_cov and recv_cov['restricted']:
                raise Exception(f"RESTRICTED ACCESS - cookies expired. Missing fields: {', '.join(str(f) for f in recv_cov['restricted'][:5])}...")
            
            versus_data = recv_cov.get('versus', [])
            
            if not versus_data:
                return pd.DataFrame()
            
            df = pd.DataFrame(versus_data)
            df['game_id'] = game_id
            df['week'] = week
            df['season'] = season
            df['scraped_at'] = datetime.utcnow().isoformat()
            
            return df
        
        elif response.status_code == 401:
            raise Exception("401 Unauthorized - cookies expired!")
        else:
            print(f"  Game {game_id}: HTTP {response.status_code}")
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e) or "RESTRICTED" in str(e):
            raise
        print(f"  Game {game_id}: Error - {e}")
        return pd.DataFrame()

def save_to_s3(df, season, weeks=None):
    """Save to S3 with append logic"""
    
    # Convert all numeric columns to float64
    for col in df.columns:
        if df[col].dtype in ['int64', 'Int64', 'int32', 'Int32', 'int16', 'Int16']:
            df[col] = df[col].astype('float64')
    
    s3_key = f'data/receiving_coverage_versus/season={season}/data.parquet'
    
    # Read existing and append
    try:
        print(f"Checking for existing data at {s3_key}...")
        existing = s3_client.get_object(Bucket=BUCKET_NAME, Key=s3_key)
        existing_df = pd.read_parquet(BytesIO(existing['Body'].read()))
        
        print(f"  Found {len(existing_df)} existing records")
        
        if weeks:
            # Remove only weeks we're re-scraping
            existing_df = existing_df[~existing_df['week'].isin(weeks)]
            print(f"  Kept {len(existing_df)} rows after removing weeks {weeks}")
        
        combined = pd.concat([existing_df, df], ignore_index=True)
        combined = combined.drop_duplicates(
            subset=['player_id', 'coverage_player_id', 'game_id'],
            keep='last'
        )
        
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
    
    print(f"Saved to s3://{BUCKET_NAME}/{s3_key}")
    return s3_key

def lambda_handler(event, context):
    """
    Lambda handler for receiving coverage versus scraper
    
    Event examples:
    {"mode": "full", "season": 2025}
    {"mode": "full", "season": [2019, 2020]}
    {"mode": "full", "season": 2025, "weeks": [16]}
    {"mode": "full", "season": 2025, "weeks": [14, 15, 16]}
    """
    
    try:
        mode = event.get('mode', 'full')
        season = event.get('season')
        weeks = event.get('weeks')
        
        if not season:
            raise Exception("season parameter required")
        
        # Normalize to list
        seasons = season if isinstance(season, list) else [season]
        
        print("=" * 60)
        print(f"RECEIVING COVERAGE VERSUS SCRAPER - Season(s) {seasons}")
        if weeks:
            print(f"Weeks: {weeks}")
        print("=" * 60)
        
        cookies = get_cookies()
        
        # Get game_ids from Athena
        games = query_game_ids(season, weeks)
        
        if not games:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': 'No games found'})
            }
        
        # Scrape each game
        all_data = []
        total = len(games)
        
        for idx, game_info in enumerate(games, 1):
            if idx % 10 == 0:
                print(f"Progress: {idx}/{total} ({idx/total*100:.1f}%)...")
            
            df = scrape_receiving_coverage_versus(
                game_info['game_id'],
                game_info['season'],
                game_info['week'],
                cookies
            )
            
            if not df.empty:
                all_data.append(df)
            
            time.sleep(0.1)
        
        if not all_data:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': 'No versus data found'})
            }
        
        combined = pd.concat(all_data, ignore_index=True)
        
        # Save per-season
        s3_keys = []
        for s in seasons:
            season_df = combined[combined['season'] == s]
            if not season_df.empty:
                key = save_to_s3(season_df, s, weeks)
                s3_keys.append(key)
        
        return {
            'statusCode': 200,
            'body': json.dumps({
                'message': 'Success',
                'seasons': seasons,
                'weeks': weeks,
                'games_scraped': len(games),
                'records': len(combined),
                'unique_receivers': combined['player_id'].nunique(),
                'unique_defenders': combined['coverage_player_id'].nunique(),
                's3_keys': s3_keys
            })
        }
        
    except Exception as e:
        print(f"ERROR: {e}")
        return {
            'statusCode': 500,
            'body': json.dumps({'error': str(e)})
        }