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

def scrape_receiving_week(season, week, auth):
    """
    Scrape receiving data for ONE week - gets ALL players automatically
    Returns: DataFrame with all players for that week
    """
    url = f'https://premium.pff.com/api/v1/facet/receiving/summary?league=nfl&season={season}&week={week}'
    
    try:
        response = pff_get(url, auth, timeout=30)
        
        if response.status_code == 200:
            data = response.json()
            
            if 'restricted' in data and data['restricted']:
                raise Exception(f"RESTRICTED ACCESS DETECTED - auth only provide limited fields. Missing fields: {', '.join(data['restricted'][:5])}... Update auth for full access!")
            
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
            raise Exception("401 Unauthorized - auth expired!")
        else:
            print(f"  ⚠ Week {week}: HTTP {response.status_code}")
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e) or "RESTRICTED" in str(e):
            raise
        print(f"  ⚠ Week {week}: {str(e)}")
        return pd.DataFrame()

def scrape_all_weeks(season, weeks, auth):
    """Scrape receiving data for multiple weeks"""
    all_data = []
    
    print(f"\nScraping {len(weeks)} weeks for season {season}...")
    
    for week in weeks:
        print(f"  Scraping week {week}...", end='')
        
        df = scrape_receiving_week(season, week, auth)
        
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
        
        auth = get_auth()
        
        df = scrape_all_weeks(season, weeks, auth)
        
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