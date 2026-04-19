import boto3
import pandas as pd
from io import BytesIO

s3_client = boto3.client('s3')
BUCKET_NAME = 'nfl-pff-data-lucas'

def lambda_handler(event, context):
    seasons = event.get('seasons', [2025])
    
    if isinstance(seasons, str) and seasons.lower() == 'all':
        seasons = list(range(2014, 2030))
    
    results = []
    
    for season in seasons:
        s3_key = f'data/allowed_pressure/season={season}/data.parquet'
        
        try:
            print(f"Converting {s3_key}...")
            
            response = s3_client.get_object(Bucket=BUCKET_NAME, Key=s3_key)
            df = pd.read_parquet(BytesIO(response['Body'].read()))
            
            for col in df.columns:
                if df[col].dtype in ['int64', 'Int64', 'int32', 'Int32']:
                    df[col] = df[col].astype('float64')
            
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
            
            print(f"✓ Season {season}: {len(df)} records converted")
            results.append({'season': season, 'records': len(df), 'status': 'success'})
            
        except s3_client.exceptions.NoSuchKey:
            print(f"✗ Season {season}: No data found")
            results.append({'season': season, 'status': 'no_data'})
        except Exception as e:
            print(f"✗ Season {season}: {e}")
            results.append({'season': season, 'status': 'error', 'error': str(e)})
    
    return {
        'statusCode': 200,
        'body': results
    }