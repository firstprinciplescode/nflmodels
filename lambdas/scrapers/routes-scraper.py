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
ATHENA_OUTPUT = f's3://nfl-pff-data-lucas/athena-results/'

CLERK = 'https://clerk.pff.com/v1'
BROWSER_HDR = {'User-Agent': 'Mozilla/5.0', 'Accept': 'application/json',
               'Origin': 'https://premium.pff.com', 'Referer': 'https://premium.pff.com/'}


def get_auth():
    """PFF premium auth since Sept 2026 (Clerk). The secret holds ONE value, the long-lived
    __client cookie of a normal premium.pff.com login (DevTools > Application > Cookies >
    premium.pff.com > __client). The 60-second __session cookie the API actually reads is
    minted from it on demand by fresh_session(), exactly as the browser does. The login
    rolls 30 days from last activity; when Clerk says it is gone, re-paste __client."""
    secret = json.loads(secrets_client.get_secret_value(SecretId=SECRET_NAME)['SecretString'])
    client = secret.get('__client')
    if not client:
        raise Exception(f"secret {SECRET_NAME} has no __client -- paste the __client cookie from premium.pff.com")
    r = requests.get(f"{CLERK}/client?_clerk_js_version=5", cookies={'__client': client},
                     headers=BROWSER_HDR, timeout=15)
    if r.status_code != 200:
        raise Exception(f"Clerk /client HTTP {r.status_code} -- __client in secret {SECRET_NAME} is dead, re-paste it: {r.text[:200]}")
    body = r.json().get('response') or r.json()
    sessions = body.get('sessions') or []
    sid = body.get('last_active_session_id') or (sessions[0].get('id') if sessions else None)
    if not sid:
        raise Exception("no active PFF session behind this __client -- log in at premium.pff.com and re-paste __client")
    for s in sessions:
        if s.get('id') == sid and s.get('expire_at'):
            print(f"PFF login {sid[:9]}... expires in {(s['expire_at'] / 1000 - time.time()) / 86400:.1f} days (rolls on activity)")
    return {'client': client, 'sid': sid, 'session': None, 'minted': 0.0}


def fresh_session(auth):
    """A __session JWT that is at most 45 s old (they die at 60 s)."""
    if auth['session'] and time.time() - auth['minted'] < 45:
        return auth['session']
    r = requests.post(f"{CLERK}/client/sessions/{auth['sid']}/tokens?_clerk_js_version=5",
                      cookies={'__client': auth['client']}, headers=BROWSER_HDR, timeout=15)
    if r.status_code != 200:
        raise Exception(f"Clerk token mint HTTP {r.status_code} -- re-paste __client into secret {SECRET_NAME}: {r.text[:200]}")
    auth['session'] = r.json()['jwt']
    auth['minted'] = time.time()
    return auth['session']


def pff_get(url, auth, timeout=10, tries=5):
    """GET premium.pff.com/api/v1 with a fresh __session. 429/502/503/504 wait Retry-After and
    retry; a 401 re-mints once; a 200 whose body lists 'restricted' fields raises (that is
    the stripped free-tier payload, never silently 'no data'); any other non-200 is printed
    so a missing week shows up in CloudWatch instead of vanishing."""
    response = None
    for attempt in range(tries):
        response = requests.get(url, cookies={'__session': fresh_session(auth)}, headers=BROWSER_HDR, timeout=timeout)
        if response.status_code in (429, 502, 503, 504) and attempt < tries - 1:
            wait = int(float(response.headers.get('Retry-After', 2 ** attempt)))
            print(f"HTTP {response.status_code} from PFF, waiting {wait}s (attempt {attempt + 1}/{tries}): {url}")
            time.sleep(wait)
            continue
        if response.status_code == 401 and attempt < tries - 1:
            auth['session'] = None
            continue
        break
    if response.status_code in (401, 403):
        raise Exception(f"{response.status_code} from PFF with a freshly minted session -- login revoked? re-paste __client: {response.text[:200]}")
    if response.status_code != 200:
        print(f"HTTP {response.status_code} for {url}: {response.text[:200]}")
        return response
    try:
        restricted = response.json().get('restricted')
    except Exception:
        restricted = None
    if restricted:
        raise Exception(f"RESTRICTED payload ({len(restricted)} fields stripped) -- the session is not premium; re-paste __client into secret {SECRET_NAME}")
    return response

def query_athena_for_players(season):
    """
    Query Athena view to get all player_ids for a given season
    Returns: list of player_id integers
    """
    query = f"""
        SELECT DISTINCT player_id
        FROM nfl_data.vw_players_with_routes
        WHERE season = {season}
    """
    
    print(f"Querying Athena for season {season} players...")
    
    # Start query
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
    
    # Get results
    results = athena_client.get_query_results(QueryExecutionId=query_execution_id)
    
    player_ids = []
    for row in results['ResultSet']['Rows'][1:]:  # Skip header
        player_id = row['Data'][0].get('VarCharValue')
        if player_id:
            player_ids.append(int(player_id))
    
    print(f"✓ Found {len(player_ids)} players for season {season}")
    return player_ids

def scrape_receiving_for_player(player_id, season, auth):
    """
    Scrape receiving summary for one player/season
    Returns: DataFrame with week-by-week data
    """
    url = f'https://premium.pff.com/api/v1/player/receiving/summary?league=nfl&season={season}&week=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,28,29,30,32&player_id={player_id}'
    
    try:
        response = pff_get(url, auth, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            
            # Extract the "weeks" array (game-by-game data)
            weeks_data = data.get('receiving_summary', {}).get('weeks', [])
            
            if not weeks_data:
                # Player had routes but no receiving stats (0 targets all season)
                return pd.DataFrame()
            
            # Flatten the data
            flattened = []
            for week in weeks_data:
                # Merge game metadata with stats
                game_info = week.pop('game', {})
                record = {**week, **game_info}
                record['scraped_at'] = datetime.utcnow().isoformat()
                flattened.append(record)
            
            return pd.DataFrame(flattened)
        
        elif response.status_code == 401:
            raise Exception("401 Unauthorized - auth expired!")
        else:
            print(f"Failed player {player_id}: Status {response.status_code}")
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e):
            raise  # Re-raise auth errors to stop execution
        print(f"Error scraping player {player_id}: {e}")
        return pd.DataFrame()

def scrape_all_receiving(auth, player_ids, season):
    """Scrape receiving data for all players in a season"""
    all_data = []
    total = len(player_ids)
    
    print(f"\nScraping {total} players for season {season}...")
    
    for idx, player_id in enumerate(player_ids, 1):
        if idx % 100 == 0:
            print(f"Progress: {idx}/{total} players...")
        
        df = scrape_receiving_for_player(player_id, season, auth)
        
        if not df.empty:
            all_data.append(df)
        
        # Rate limiting - don't hammer the API
        time.sleep(0.1)
    
    if all_data:
        combined = pd.concat(all_data, ignore_index=True)
        print(f"✓ Scraped {len(combined)} receiving records")
        return combined
    else:
        return pd.DataFrame()

def save_to_s3_by_season(df, season):
    """
    Save receiving data to S3, partitioned by season
    Structure: data/receiving/season=2025/data.parquet
    """
    
    # ========================================================================
    # FORCE CONSISTENT SCHEMA - Critical for Athena
    # ========================================================================
    # Only cast columns that exist in the dataframe
    type_mapping = {
        'player_id': 'Int64',
        'game_id': 'Int64',
        'week': 'Int64',
        'targets': 'Int64',
        'receptions': 'Int64',
        'yards': 'Int64',
        'touchdowns': 'Int64',
        'first_downs': 'Int64',
        'fumbles': 'Int64',
        'interceptions': 'Int64',
        'drops': 'Int64',
        'contested_targets': 'Int64',
        'contested_receptions': 'Int64',
        'routes': 'Int64',
        'pass_plays': 'Int64',
        'pass_blocks': 'Int64',
        'penalties': 'Int64',
        'declined_penalties': 'Int64',
        'avoided_tackles': 'Int64',
        'longest': 'Int64',
        'yards_after_catch': 'Int64',
        'wide_snaps': 'Int64',
        'slot_snaps': 'Int64',
        'inline_snaps': 'Int64',
        'away_franchise_id': 'Int64',
        'home_franchise_id': 'Int64',
        'player_franchise_id': 'Int64',
        'grades_offense': 'float64',
        'grades_pass_route': 'float64',
        'grades_hands_drop': 'float64',
        'grades_hands_fumble': 'float64',
        'yards_after_catch_per_reception': 'float64',
        'yprr': 'float64',
        'targeted_qb_rating': 'float64',
        'caught_percent': 'float64',
        'drop_rate': 'float64',
        'contested_catch_rate': 'float64',
        'yards_per_reception': 'float64',
        'avg_depth_of_target': 'float64',
        'targets_percent': 'float64',
        'slot_rate': 'float64',
        'inline_rate': 'float64',
        'wide_rate': 'float64',
        'pass_block_rate': 'float64',
        'route_rate': 'float64',
        'status': 'str',
        'position': 'str',
        'jersey_number': 'str',
        'away_team_name': 'str',
        'home_team_name': 'str',
        'team_name': 'str',
        'scraped_at': 'str'
    }
    
    # Only cast columns that exist
    existing_columns = {k: v for k, v in type_mapping.items() if k in df.columns}
    df = df.astype(existing_columns)
    
    # S3 path
    s3_key = f"data/receiving/season={season}/data.parquet"
    
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
    
    print(f"✓ Saved to s3://{BUCKET_NAME}/{s3_key}")
    return s3_key

def lambda_handler(event, context):
    """
    Lambda handler for receiving data scraper
    
    Event:
    {
        "season": 2025  // Required - which season to scrape
    }
    
    Usage:
    - Run once per season: {"season": 2025}
    - Run historical: {"season": 2024}, then {"season": 2023}, etc.
    """
    
    try:
        # Get season from event
        season = event.get('season')
        
        if not season:
            raise ValueError("Must provide 'season' in event (e.g., {'season': 2025})")
        
        print("=" * 60)
        print(f"RECEIVING DATA SCRAPER - SEASON {season}")
        print("=" * 60)
        
        # Get auth
        auth = get_auth()
        
        # Query Athena for player IDs
        player_ids = query_athena_for_players(season)
        
        if not player_ids:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': f'No players found for season {season}'})
            }
        
        # Scrape receiving data
        df = scrape_all_receiving(auth, player_ids, season)
        
        if df.empty:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': f'No receiving data found for season {season}'})
            }
        
        # Save to S3
        s3_key = save_to_s3_by_season(df, season)
        
        return {
            'statusCode': 200,
            'body': json.dumps({
                'message': 'Success',
                'season': season,
                'players_scraped': len(player_ids),
                'receiving_records': len(df),
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