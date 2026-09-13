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

def scrape_passing_pressure_for_week(season, week, auth):
    """
    Scrape passing pressure data for one season/week combination
    Returns: DataFrame with passing pressure data
    """
    url = f'https://premium.pff.com/api/v1/facet/passing/pressure?league=nfl&season={season}&week={week}'
    
    try:
        response = pff_get(url, auth, timeout=10)
        
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
            raise Exception("401 Unauthorized - auth expired!")
        else:
            print(f"  Failed {season} Week {week}: Status {response.status_code}")
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e) or "BAD COOKIES" in str(e):
            raise
        print(f"  Error scraping {season} Week {week}: {e}")
        return pd.DataFrame()

def scrape_all_passing_pressure(auth, season, weeks):
    """Scrape passing pressure data for all weeks in a season"""
    all_data = []
    
    print(f"\nScraping {season} - {len(weeks)} weeks...")
    
    for week in weeks:
        df = scrape_passing_pressure_for_week(season, week, auth)
        
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
        
        auth = get_auth()
        
        df = scrape_all_passing_pressure(auth, season, weeks)
        
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