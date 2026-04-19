"""
PFF Receiving Depth - Zero Targets - v2 FINAL (with restriction tracking)

DEPLOY TO: nfl-receiving-depth-weekly-no-targets-v2

WHAT THIS CATCHES that the old version missed:
- PFF returns HTTP 200 with a restricted response when cookies are downgraded
- Old Lambda treated that as success and wrote junk parquet files
- This version counts fields in the `restricted` array per response
- Tags every row with restricted_field_count and access_level
- Writes sidecar JSON to partial_log/ so you know WHICH player-weeks were partial
- Returns stats in the response body so orchestrator logs them too
"""

import json
import boto3
import requests
import pandas as pd
from io import BytesIO
from datetime import datetime
import time

secrets_client = boto3.client('secretsmanager')
s3_client = boto3.client('s3')
athena_client = boto3.client('athena')

BUCKET_NAME = 'nfl-pff-data-lucas'
SECRET_NAME = 'pff-api-cookies'
ATHENA_OUTPUT = 's3://nfl-pff-data-lucas/athena-results/'

STRING_COLUMNS = {
    'team_name', 'team', 'player', 'position', 'jersey_number',
    'scraped_at', 'access_level'
}


def get_cookies():
    response = secrets_client.get_secret_value(SecretId=SECRET_NAME)
    return json.loads(response['SecretString'])


def query_athena_for_zero_target_players(season=None, week=None, min_player_id=None, max_player_id=None):
    query = """
        SELECT DISTINCT player_id, week, season
        FROM nfl_data.vw_players_with_routes_no_targets
        WHERE 1=1
    """
    if season is not None:
        query += f" AND season = {season}"
    if week is not None:
        query += f" AND week = {week}"
    if min_player_id is not None:
        query += f" AND player_id >= {min_player_id}"
    if max_player_id is not None:
        query += f" AND player_id <= {max_player_id}"

    print(f"Querying Athena: season={season}, week={week}, range=[{min_player_id},{max_player_id}]")

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

    players = []
    next_token = None
    while True:
        if next_token:
            results = athena_client.get_query_results(QueryExecutionId=query_execution_id, NextToken=next_token)
        else:
            results = athena_client.get_query_results(QueryExecutionId=query_execution_id)

        start_idx = 1 if next_token is None else 0
        for row in results['ResultSet']['Rows'][start_idx:]:
            try:
                player_id = int(row['Data'][0].get('VarCharValue'))
                week = int(row['Data'][1].get('VarCharValue'))
                season = int(row['Data'][2].get('VarCharValue'))
                players.append({'player_id': player_id, 'week': week, 'season': season})
            except:
                continue

        next_token = results.get('NextToken')
        if not next_token:
            break

    print(f"Found {len(players)} player-week combinations")
    return players


def scrape_player_week(player_id, week, season, cookies):
    """
    Returns (DataFrame, restricted_count).
    restricted_count = number of fields PFF withheld. 0 = full access.
    -1 = HTTP error (non-200, non-401).
    """
    url = f'https://premium.pff.com/api/v1/player/receiving/depth?league=nfl&season={season}&week={week}&player_id={player_id}'
    try:
        response = requests.get(url, cookies=cookies, timeout=10)
        if response.status_code == 200:
            data = response.json()
            depth_data = data.get('receiving_depth', [])
            restricted_list = data.get('restricted', []) or []
            restricted_count = len(restricted_list)

            if not depth_data:
                return pd.DataFrame(), restricted_count

            access_level = 'full' if restricted_count == 0 else 'partial'

            for record in depth_data:
                if 'player_id' not in record:
                    record['player_id'] = player_id
                if 'season' not in record:
                    record['season'] = season
                if 'week' not in record:
                    record['week'] = week
                record['scraped_at'] = datetime.utcnow().isoformat()
                record['restricted_field_count'] = restricted_count
                record['access_level'] = access_level

            return pd.DataFrame(depth_data), restricted_count

        elif response.status_code == 401:
            raise Exception("401 Unauthorized - cookies expired!")
        else:
            return pd.DataFrame(), -1
    except Exception as e:
        if "401" in str(e) or "Unauthorized" in str(e):
            raise
        return pd.DataFrame(), -1


def scrape_all_zero_target_players(cookies, players, abort_after_partial=5):
    """
    abort_after_partial: if this many consecutive partial responses come back,
    raise an exception to kill the invocation. Better to fail loud than write
    silent garbage like the old Lambda did.
    """
    all_data = []
    total = len(players)

    full_access_count = 0
    partial_access_count = 0
    no_data_count = 0
    failed_count = 0
    max_restricted_seen = 0
    partial_player_weeks = []
    consecutive_partial = 0

    print(f"\nScraping {total} player-week combinations...")

    for idx, player_info in enumerate(players, 1):
        if idx % 100 == 0:
            print(f"Progress: {idx}/{total} ({idx/total*100:.1f}%)...")

        df, restricted_count = scrape_player_week(
            player_info['player_id'],
            player_info['week'],
            player_info['season'],
            cookies
        )

        if restricted_count == -1:
            failed_count += 1
            consecutive_partial = 0
        elif df.empty:
            no_data_count += 1
            consecutive_partial = 0
        elif restricted_count == 0:
            full_access_count += 1
            consecutive_partial = 0
            all_data.append(df)
        else:
            partial_access_count += 1
            consecutive_partial += 1
            max_restricted_seen = max(max_restricted_seen, restricted_count)
            partial_player_weeks.append({
                'player_id': player_info['player_id'],
                'week': player_info['week'],
                'season': player_info['season'],
                'restricted_count': restricted_count,
            })
            all_data.append(df)

            # Kill switch: if cookies got downgraded, stop wasting API calls
            if consecutive_partial >= abort_after_partial:
                print(f"!!! {abort_after_partial} consecutive partial responses - ABORTING")
                raise Exception(
                    f"Cookies appear downgraded - {consecutive_partial} consecutive partial responses "
                    f"(restricted_count={restricted_count}). Refresh cookies and rerun."
                )

        time.sleep(0.1)

    print(f"\n--- Access level summary ---")
    print(f"  Full access:    {full_access_count}")
    print(f"  Partial access: {partial_access_count}")
    print(f"  No data:        {no_data_count}")
    print(f"  Failed:         {failed_count}")
    if partial_access_count > 0:
        print(f"  Max restricted fields seen: {max_restricted_seen}")

    stats = {
        'full_access_count': full_access_count,
        'partial_access_count': partial_access_count,
        'no_data_count': no_data_count,
        'failed_count': failed_count,
        'max_restricted_seen': max_restricted_seen,
        'partial_player_weeks': partial_player_weeks,
    }

    if all_data:
        combined = pd.concat(all_data, ignore_index=True)
        return combined, stats
    else:
        return pd.DataFrame(), stats


def force_schema(df):
    for col in df.columns:
        if col in STRING_COLUMNS:
            df[col] = df[col].astype('str')
        else:
            df[col] = pd.to_numeric(df[col], errors='coerce').astype('float64')
    return df


def save_to_s3_v2(df, season, week, min_player_id=None, max_player_id=None):
    if season is None or week is None:
        raise ValueError("season and week are both required in v2")

    df = force_schema(df)

    run_ts = datetime.utcnow().strftime('%Y%m%dT%H%M%SZ')
    min_str = str(min_player_id) if min_player_id is not None else 'min'
    max_str = str(max_player_id) if max_player_id is not None else 'max'

    s3_key = (
        f"data/receiving_depth_weekly_no_targets_v2_raw/"
        f"season={season}/week={week}/"
        f"run_{run_ts}_batch_{min_str}_{max_str}.parquet"
    )

    parquet_buffer = BytesIO()
    df.to_parquet(parquet_buffer, index=False, engine='pyarrow', compression='snappy')
    parquet_buffer.seek(0)

    s3_client.put_object(
        Bucket=BUCKET_NAME,
        Key=s3_key,
        Body=parquet_buffer.getvalue(),
        ContentType='application/octet-stream'
    )
    print(f"Saved {len(df)} rows to s3://{BUCKET_NAME}/{s3_key}")
    return s3_key


def save_partial_log_to_s3(partial_player_weeks, season, week, min_player_id, max_player_id):
    if not partial_player_weeks:
        return None

    run_ts = datetime.utcnow().strftime('%Y%m%dT%H%M%SZ')
    min_str = str(min_player_id) if min_player_id is not None else 'min'
    max_str = str(max_player_id) if max_player_id is not None else 'max'

    s3_key = (
        f"data/receiving_depth_weekly_no_targets_v2_partial_log/"
        f"season={season}/week={week}/"
        f"partial_{run_ts}_batch_{min_str}_{max_str}.json"
    )

    body = json.dumps({
        'season': season,
        'week': week,
        'min_player_id': min_player_id,
        'max_player_id': max_player_id,
        'run_ts': run_ts,
        'partial_player_weeks': partial_player_weeks,
    }, indent=2)

    s3_client.put_object(
        Bucket=BUCKET_NAME,
        Key=s3_key,
        Body=body.encode('utf-8'),
        ContentType='application/json'
    )
    print(f"Logged {len(partial_player_weeks)} partial player-weeks to s3://{BUCKET_NAME}/{s3_key}")
    return s3_key


def lambda_handler(event, context):
    try:
        season = event.get('season')
        week = event.get('week')
        player_id_range = event.get('player_id_range')

        min_player_id = None
        max_player_id = None
        if player_id_range:
            min_player_id = player_id_range[0]
            max_player_id = player_id_range[1]

        print("=" * 60)
        print(f"RECEIVING DEPTH v2 FINAL - season={season} week={week} range=[{min_player_id},{max_player_id}]")
        print("=" * 60)

        if season is None or week is None:
            return {
                'statusCode': 400,
                'body': json.dumps({'error': 'season and week are both required'})
            }

        cookies = get_cookies()

        players = query_athena_for_zero_target_players(
            season=season, week=week,
            min_player_id=min_player_id, max_player_id=max_player_id
        )

        if not players:
            return {
                'statusCode': 200,
                'body': json.dumps({
                    'message': 'No zero-target players found',
                    'records_saved': 0,
                    'full_access_count': 0,
                    'partial_access_count': 0,
                    'no_data_count': 0,
                    'failed_count': 0,
                    'max_restricted_seen': 0,
                })
            }

        df, stats = scrape_all_zero_target_players(cookies, players)

        if df.empty:
            return {
                'statusCode': 200,
                'body': json.dumps({
                    'message': 'No data returned',
                    'records_saved': 0,
                    'players_attempted': len(players),
                    'full_access_count': stats['full_access_count'],
                    'partial_access_count': stats['partial_access_count'],
                    'no_data_count': stats['no_data_count'],
                    'failed_count': stats['failed_count'],
                    'max_restricted_seen': stats['max_restricted_seen'],
                })
            }

        s3_key = save_to_s3_v2(
            df, season=season, week=week,
            min_player_id=min_player_id, max_player_id=max_player_id
        )

        partial_log_key = save_partial_log_to_s3(
            stats['partial_player_weeks'], season, week, min_player_id, max_player_id
        )

        return {
            'statusCode': 200,
            'body': json.dumps({
                'message': 'Success',
                'season': season,
                'week': week,
                'player_id_range': [min_player_id, max_player_id] if player_id_range else None,
                'players_attempted': len(players),
                'records_saved': len(df),
                'unique_players': int(df['player_id'].nunique()) if 'player_id' in df.columns else 0,
                'full_access_count': stats['full_access_count'],
                'partial_access_count': stats['partial_access_count'],
                'no_data_count': stats['no_data_count'],
                'failed_count': stats['failed_count'],
                'max_restricted_seen': stats['max_restricted_seen'],
                's3_key': s3_key,
                'partial_log_key': partial_log_key,
            })
        }

    except Exception as e:
        print(f"ERROR: {e}")
        return {
            'statusCode': 500,
            'body': json.dumps({'error': str(e)})
        }