import boto3
import pandas as pd
from io import BytesIO

s3_client = boto3.client('s3')
BUCKET_NAME = 'nfl-pff-data-lucas'

def lambda_handler(event, context):
    seasons = event.get('seasons', [2025])
    
    if isinstance(seasons, str) and seasons.lower() == 'all':
        seasons = list(range(2015, 2027))
    
    results = []
    
    for season in seasons:
        # List all files under season partition (including batch subdirectories)
        prefix = f'data/receiving_depth_weekly_no_targets/season={season}/'
        
        try:
            paginator = s3_client.get_paginator('list_objects_v2')
            pages = paginator.paginate(Bucket=BUCKET_NAME, Prefix=prefix)
            
            parquet_files = []
            for page in pages:
                if 'Contents' in page:
                    for obj in page['Contents']:
                        if obj['Key'].endswith('.parquet'):
                            parquet_files.append(obj['Key'])
            
            print(f"Season {season}: Found {len(parquet_files)} parquet files")
            
            for s3_key in parquet_files:
                print(f"  Converting {s3_key}...")
                
                response = s3_client.get_object(Bucket=BUCKET_NAME, Key=s3_key)
                df = pd.read_parquet(BytesIO(response['Body'].read()))
                
                # Convert ALL numeric columns to float64
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
                
                print(f"    ✓ {len(df)} records converted")
            
            results.append({'season': season, 'files_converted': len(parquet_files), 'status': 'success'})
            
        except Exception as e:
            print(f"✗ Season {season}: {e}")
            results.append({'season': season, 'status': 'error', 'error': str(e)})
    
    return {
        'statusCode': 200,
        'body': results
    }