# Audit 2023 - find suspicious files that might be partial scrapes
# Compares each parquet's row count to what Athena says SHOULD be in that week/half

$ErrorActionPreference = 'Stop'

Write-Host "Fetching all 2023 parquet files from S3..." -ForegroundColor Cyan
$files = aws s3 ls s3://nfl-pff-data-lucas/data/receiving_depth_weekly_no_targets_v2_raw/season=2025/ --recursive | ForEach-Object {
    if ($_ -match '\s+(\d+)\s+(data/.*week=(\d+)/.*batch_(\d+)_(\d+)\.parquet)$') {
        [PSCustomObject]@{
            Size     = [int]$matches[1]
            Key      = $matches[2]
            Week     = [int]$matches[3]
            MinId    = $matches[4]
            MaxId    = $matches[5]
            Filename = Split-Path $matches[2] -Leaf
        }
    }
}

Write-Host "Found $($files.Count) parquet files for 2023" -ForegroundColor Cyan
Write-Host ""

# Suspicious = very small file size relative to neighbors in same week
# Real complete batches are ~200KB+. Partial batches tend to be much smaller.
Write-Host "=== File sizes grouped by week ===" -ForegroundColor Yellow
$files | Group-Object Week | Sort-Object @{Expression={[int]$_.Name}} | ForEach-Object {
    $weekNum = $_.Name
    $group = $_.Group
    $sizes = $group | Select-Object -ExpandProperty Size
    $min = ($sizes | Measure-Object -Minimum).Minimum
    $max = ($sizes | Measure-Object -Maximum).Maximum
    $avg = [int](($sizes | Measure-Object -Average).Average)
    $count = $group.Count

    # Flag weeks where smallest file is <50% of largest - possible partial scrape
    if ($max -gt 0 -and $min -lt ($max * 0.5) -and $count -gt 1) {
        Write-Host "  Week $weekNum : $count files, size min=$min max=$max avg=$avg  <-- SUSPICIOUS (size variance)" -ForegroundColor Red
    } else {
        Write-Host "  Week $weekNum : $count files, size min=$min max=$max avg=$avg"
    }
}

Write-Host ""
Write-Host "=== Individual small files (<50KB - possibly partial) ===" -ForegroundColor Yellow
$smallFiles = $files | Where-Object { $_.Size -lt 50000 }
if ($smallFiles.Count -eq 0) {
    Write-Host "  None found" -ForegroundColor Green
} else {
    $smallFiles | Format-Table Week, MinId, MaxId, Size, Filename -AutoSize
}

Write-Host ""
Write-Host "If any weeks are flagged or any small files show up, we can pull one down and inspect." -ForegroundColor Cyan