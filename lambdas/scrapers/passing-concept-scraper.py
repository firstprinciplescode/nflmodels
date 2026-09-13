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

def scrape_passing_concept_for_week(season, week, auth):
    """
    Scrape passing concept data for one season/week combination
    Returns: DataFrame with passing concept data
    """
    url = f'https://premium.pff.com/api/v1/facet/passing/concept?league=nfl&season={season}&week={week}'
    
    try:
        response = pff_get(url, auth, timeout=10)
        
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
            raise Exception("401 Unauthorized - auth expired!")
        else:
            print(f"  Failed {season} Week {week}: Status {response.status_code}")
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e) or "BAD COOKIES" in str(e):
            raise
        print(f"  Error scraping {season} Week {week}: {e}")
        return pd.DataFrame()

def scrape_all_passing_concept(auth, season, weeks):
    """Scrape passing concept data for all weeks in a season"""
    all_data = []
    
    print(f"\nScraping {season} - {len(weeks)} weeks...")
    
    for week in weeks:
        df = scrape_passing_concept_for_week(season, week, auth)
        
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
        
        auth = get_auth()
        
        df = scrape_all_passing_concept(auth, season, weeks)
        
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