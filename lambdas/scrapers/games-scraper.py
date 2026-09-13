import json
import boto3
import requests
import time
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

def scrape_games(auth, years, weeks):
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
                response = pff_get(url, auth, timeout=10)
                
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
                    raise Exception("401 Unauthorized - auth expired!")
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
        
        auth = get_auth()
        
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
        df = scrape_games(auth, years, weeks)
        
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