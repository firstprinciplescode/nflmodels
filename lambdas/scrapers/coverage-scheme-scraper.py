import json
import boto3
import requests
import pandas as pd
from io import BytesIO
from datetime import datetime
import time

s3_client = boto3.client('s3')
secrets_client = boto3.client('secretsmanager')

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

def scrape_coverage_scheme(season, week, auth):
    url = f'https://api.pff.com/v1/facet/defense/coverage_scheme?league=nfl&season={season}&week={week}'
    
    try:
        response = pff_get(url, auth, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            
            if 'restricted' in data and data['restricted']:
                raise Exception(f"RESTRICTED ACCESS - auth expired. Missing fields: {', '.join(data['restricted'][:5])}...")
            
            coverage_data = data.get('coverage_scheme', [])
            
            if not coverage_data:
                return pd.DataFrame()
            
            df = pd.DataFrame(coverage_data)
            df['week'] = week
            df['season'] = season
            df['scraped_at'] = datetime.utcnow().isoformat()
            
            return df
        
        elif response.status_code == 401:
            raise Exception("401 Unauthorized - auth expired!")
        else:
            return pd.DataFrame()
    
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e) or "RESTRICTED" in str(e):
            raise
        return pd.DataFrame()

def save_to_s3(df, season):
    # Convert ALL numeric columns to float64
    for col in df.columns:
        if df[col].dtype in ['int64', 'Int64', 'int32', 'Int32', 'int16', 'Int16']:
            df[col] = df[col].astype('float64')
    
    s3_key = f'data/coverage_scheme/season={season}/data.parquet'
    
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
    try:
        season = event.get('season')
        weeks = event.get('weeks')
        
        if not season:
            raise Exception("season parameter required")
        
        if not weeks:
            weeks = list(range(1, 19)) + [28, 29, 30, 31, 32]
        
        print("=" * 60)
        print(f"COVERAGE SCHEME SCRAPER - Season {season}")
        print(f"Weeks: {weeks}")
        print("=" * 60)
        
        auth = get_auth()
        
        all_data = []
        total = len(weeks)
        
        for idx, week in enumerate(weeks, 1):
            print(f"Scraping week {week} ({idx}/{total})...")
            
            df = scrape_coverage_scheme(season, week, auth)
            
            if not df.empty:
                all_data.append(df)
            
            time.sleep(0.1)
        
        if not all_data:
            return {
                'statusCode': 200,
                'body': json.dumps({'message': 'No data found'})
            }
        
        combined = pd.concat(all_data, ignore_index=True)
        
        s3_key = save_to_s3(combined, season)
        
        return {
            'statusCode': 200,
            'body': json.dumps({
                'message': 'Success',
                'season': season,
                'weeks': weeks,
                'records': len(combined),
                'unique_players': combined['player_id'].nunique(),
                's3_key': s3_key
            })
        }
        
    except Exception as e:
        print(f"ERROR: {e}")
        return {
            'statusCode': 500,
            'body': json.dumps({'error': str(e)})
        }