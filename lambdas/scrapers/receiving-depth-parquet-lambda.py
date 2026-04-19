import boto3
import pandas as pd
from io import BytesIO
import json

s3 = boto3.client('s3')
BUCKET = 'nfl-pff-data-lucas'

def list_all_parquet_files(prefix):
    """List all parquet files under a prefix"""
    files = []
    paginator = s3.get_paginator('list_objects_v2')
    
    for page in paginator.paginate(Bucket=BUCKET, Prefix=prefix):
        if 'Contents' in page:
            for obj in page['Contents']:
                if obj['Key'].endswith('.parquet'):
                    files.append(obj['Key'])
    
    return files

def convert_parquet_types(s3_key):
    """Download parquet, convert all int columns to float, re-upload"""
    print(f"Processing: {s3_key}")
    
    try:
        # Download
        obj = s3.get_object(Bucket=BUCKET, Key=s3_key)
        parquet_data = obj['Body'].read()
        
        # Read as pandas
        df = pd.read_parquet(BytesIO(parquet_data))
        
        # Convert all integer columns to float64
        for col in df.columns:
            if df[col].dtype in ['int64', 'Int64', 'int32', 'Int32']:
                df[col] = df[col].astype('float64')
        
        # Write back to parquet
        buffer = BytesIO()
        df.to_parquet(buffer, engine='pyarrow', compression='snappy', index=False)
        buffer.seek(0)
        
        # Upload back to same location
        s3.put_object(
            Bucket=BUCKET,
            Key=s3_key,
            Body=buffer.getvalue(),
            ContentType='application/octet-stream'
        )
        
        print(f"  ✓ Converted and uploaded")
        return True
        
    except Exception as e:
        print(f"  ✗ Error: {e}")
        return False

def lambda_handler(event, context):
    """
    Lambda handler to convert parquet INT64 columns to float64
    
    Event: {} - no params needed, processes both folders
    """
    
    folders = [
        'data/receiving_depth_weekly_no_targets/',
        'data/receiving_depth_weekly_targets/'
    ]
    
    results = {}
    
    for folder in folders:
        print(f"\n{'='*60}")
        print(f"Processing folder: {folder}")
        print(f"{'='*60}")
        
        files = list_all_parquet_files(folder)
        print(f"Found {len(files)} parquet files")
        
        success = 0
        failed = 0
        failed_files = []
        
        for f in files:
            if convert_parquet_types(f):
                success += 1
            else:
                failed += 1
                failed_files.append(f)
        
        results[folder] = {
            'total': len(files),
            'success': success,
            'failed': failed,
            'failed_files': failed_files
        }
        
        print(f"\n✓ Folder complete: {success} success, {failed} failed")
    
    return {
        'statusCode': 200,
        'body': json.dumps({
            'message': 'Conversion complete',
            'results': results
        })
    }