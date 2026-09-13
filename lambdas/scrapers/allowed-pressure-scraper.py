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

def get_auth():
    """PFF Developer API credential: the ak_live_ key stored in Secrets Manager under PFF_API_KEY.
    Replaced the browser-cookie jar on 2026-09-13: PFF moved premium auth to Clerk (60-second
    session tokens) and opened https://developer.pff.com -- same /v1 paths and parameters as
    premium.pff.com/api/v1, host api.pff.com, Authorization: Bearer <key>."""
    secret = json.loads(secrets_client.get_secret_value(SecretId=SECRET_NAME)['SecretString'])
    key = secret.get('PFF_API_KEY')
    if not key:
        raise Exception(f"secret {SECRET_NAME} has no PFF_API_KEY -- create one at https://www.pff.com/account/api-keys")
    return {'Authorization': f'Bearer {key}', 'Accept': 'application/json'}


def pff_get(url, auth, timeout=10, tries=5):
    """GET against api.pff.com honouring its contract: 429/502/503/504 wait Retry-After and
    retry; 401/403 raise with the API's own reason (never a silent 'no data'); any other
    non-200 is printed so a missing week shows up in CloudWatch instead of vanishing."""
    response = None
    for attempt in range(tries):
        response = requests.get(url, headers=auth, timeout=timeout)
        if response.status_code in (429, 502, 503, 504) and attempt < tries - 1:
            wait = int(float(response.headers.get('Retry-After', 2 ** attempt)))
            print(f"HTTP {response.status_code} from PFF, waiting {wait}s (attempt {attempt + 1}/{tries}): {url}")
            time.sleep(wait)
            continue
        break
    if response.status_code in (401, 403):
        try:
            err = response.json().get('error', {})
            reason = f"{err.get('code')} / {(err.get('details') or {}).get('reason')} request_id={err.get('request_id')}"
        except Exception:
            reason = response.text[:200]
        raise Exception(f"{response.status_code} Unauthorized - PFF API key rejected: {reason}")
    if response.status_code != 200:
        print(f"HTTP {response.status_code} for {url}: {response.text[:200]}")
    return response

def scrape_allowed_pressure_for_week(season, week, auth):
    """
    Scrape allowed pressure data for one season/week combination
    Returns: DataFrame with allowed pressure data
    """
    url = f'https://api.pff.com/v1/facet/passing/allowed_pressure?league=nfl&season={season}&week={week}'
    
    try:
        response = pff_get(url, auth, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            
            if 'restricted' in data:
                restricted_fields = data.get('restricted', [])
                raise Exception(f"BAD COOKIES - Restricted fields present: {restricted_fields[:10]}")
            
            pressure_data = data.get('passing_allowed_pressure', [])
            
            if not pressure_data:
                print(f"  No data for {season} Week {week}")
                return pd.DataFrame()
            
            if pressure_data:
                first_record = pressure_data[0]
                critical_fields = ['pressures_allowed', 'sacks_allowed', 'pressures_off']
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

def scrape_all_allowed_pressure(auth, season, weeks):
    """Scrape allowed pressure data for all weeks in a season"""
    all_data = []
    
    print(f"\nScraping {season} - {len(weeks)} weeks...")
    
    for week in weeks:
        df = scrape_allowed_pressure_for_week(season, week, auth)
        
        if not df.empty:
            all_data.append(df)
        
        time.sleep(0.1)
    
    if all_data:
        combined = pd.concat(all_data, ignore_index=True)
        print(f"✓ Total: {len(combined)} allowed pressure records")
        return combined
    else:
        return pd.DataFrame()

def save_to_s3_by_season(df, season):
    """
    Save allowed pressure data to S3, partitioned by season
    APPENDS to existing file and deduplicates by player_id/week/season
    """
    
    # ALL NUMERIC AS FLOAT64
    type_mapping = {
        'player_id': 'float64',
        'franchise_id': 'float64',
        'season': 'float64',
        'week': 'float64',
        'player_game_count': 'float64',
        'draft_season': 'float64',
        'eligible_season': 'float64',
        'penalties': 'float64',
        'declined_penalties': 'float64',
        'allowed_pressure_dropbacks': 'float64',
        'pressures_allowed': 'float64',
        'sacks_allowed': 'float64',
        'hits_allowed': 'float64',
        'hurries_allowed': 'float64',
        'pressures_off': 'float64',
        'pressures_ol_te': 'float64',
        'pressures_self': 'float64',
        'pressures_other': 'float64',
        'pressures_lt': 'float64',
        'pressures_lg': 'float64',
        'pressures_ce': 'float64',
        'pressures_rg': 'float64',
        'pressures_rt': 'float64',
        'pressures_te': 'float64',
        'lt_percent': 'float64',
        'lg_percent': 'float64',
        'ce_percent': 'float64',
        'rg_percent': 'float64',
        'rt_percent': 'float64',
        'te_percent': 'float64',
        'ol_te_percent': 'float64',
        'self_percent': 'float64',
        'other_percent': 'float64',
        'player': 'str',
        'position': 'str',
        'team': 'str',
        'team_name': 'str',
        'jersey_number': 'str',
        'scraped_at': 'str'
    }
    
    existing_columns = {k: v for k, v in type_mapping.items() if k in df.columns}
    df = df.astype(existing_columns)
    
    s3_key = f"data/allowed_pressure/season={season}/data.parquet"
    
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
    Lambda handler for allowed pressure data scraper
    
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
        print(f"ALLOWED PRESSURE SCRAPER - SEASON {season}")
        print(f"Weeks: {weeks}")
        print("=" * 60)
        
        auth = get_auth()
        
        df = scrape_all_allowed_pressure(auth, season, weeks)
        
        if df.empty:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': f'No allowed pressure data found for season {season}'})
            }
        
        s3_key = save_to_s3_by_season(df, season)
        
        return {
            'statusCode': 200,
            'body': json.dumps({
                'message': 'Success',
                'season': season,
                'weeks_scraped': len(weeks),
                'allowed_pressure_records': len(df),
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