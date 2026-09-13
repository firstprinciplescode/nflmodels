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

def scrape_time_in_pocket_for_week(season, week, auth):
    """
    Scrape time in pocket data for one season/week combination
    Returns: DataFrame with time in pocket data
    """
    url = f'https://premium.pff.com/api/v1/facet/signature/passing/time_in_pocket?league=nfl&season={season}&week={week}'
    
    try:
        response = pff_get(url, auth, timeout=10)
        
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
            raise Exception("401 Unauthorized - auth expired!")
        else:
            print(f"  Failed {season} Week {week}: Status {response.status_code}")
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e) or "BAD COOKIES" in str(e):
            raise
        print(f"  Error scraping {season} Week {week}: {e}")
        return pd.DataFrame()

def scrape_all_time_in_pocket(auth, season, weeks):
    """Scrape time in pocket data for all weeks in a season"""
    all_data = []
    
    print(f"\nScraping {season} - {len(weeks)} weeks...")
    
    for week in weeks:
        df = scrape_time_in_pocket_for_week(season, week, auth)
        
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
        
        auth = get_auth()
        
        df = scrape_all_time_in_pocket(auth, season, weeks)
        
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