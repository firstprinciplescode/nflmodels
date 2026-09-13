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

def check_restricted_access(auth):
    """
    Check if auth have access to FULL endpoints (not restricted)
    Returns: True if FULL access
    Raises: Exception if auth give restricted access or are invalid (401)
    """
    test_season = 2024
    test_week = 1
    
    url = f'https://premium.pff.com/api/v1/facet/receiving/scheme?league=nfl&season={test_season}&week={test_week}'
    
    try:
        response = pff_get(url, auth, timeout=10)
        
        if response.status_code == 401:
            raise Exception("401 Unauthorized - auth expired!")
        
        if response.status_code == 200:
            data = response.json()
            if data.get('receiving_scheme') and len(data['receiving_scheme']) > 0:
                first_record = data['receiving_scheme'][0]
                has_full = 'man_routes' in first_record and 'man_yards' in first_record
                
                if not has_full:
                    raise Exception("RESTRICTED ACCESS DETECTED - auth only provide limited fields. Update auth for full access!")
                
                print(f"✓ Access level: FULL (with detailed stats)")
                return True
        
        raise Exception("Unable to verify access level - unexpected response format")
        
    except Exception as e:
        raise

def scrape_receiver_scheme_for_week(season, week, auth):
    """
    Scrape receiver scheme data for one season/week (gets ALL players in one call)
    Returns: DataFrame with man vs zone coverage data
    """
    url = f'https://premium.pff.com/api/v1/facet/receiving/scheme?league=nfl&season={season}&week={week}'
    
    try:
        response = pff_get(url, auth, timeout=10)
        
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
            raise Exception("401 Unauthorized - auth expired!")
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
        
        auth = get_auth()
        check_restricted_access(auth)
        
        results = []
        total_players = 0
        
        for week in weeks_to_scrape:
            print(f"\n{'='*60}")
            print(f"Processing Week {week}")
            print('='*60)
            
            df = scrape_receiver_scheme_for_week(season, week, auth)
            
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