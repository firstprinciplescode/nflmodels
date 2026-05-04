param([string]$ScriptPath)
if (-not (Test-Path $ScriptPath)) { Write-Host "File not found: $ScriptPath" -ForegroundColor Red; exit }
$lines = Get-Content $ScriptPath -Raw
Write-Host "`n=== JOINS ===" -ForegroundColor Cyan
($lines -split "`n" | Select-String -Pattern "(_join|left_join|right_join|inner_join|full_join)\s*\(" | Select-Object -First 5 | ForEach-Object { $_.Line.Trim() }) -join "`n  "
Write-Host "`n=== TRANSFORMS ===" -ForegroundColor Cyan
($lines -split "`n" | Select-String -Pattern "(mutate|transmute|case_when|ifelse|recode|fifelse)\s*\(" | Select-Object -First 8 | ForEach-Object { $_.Line.Trim() }) -join "`n  "
Write-Host "`n=== DATA LOADS ===" -ForegroundColor Cyan
($lines -split "`n" | Select-String -Pattern "(read_|load_|aws\.|athena|s3://|parquet|csv|dbGetQuery)\s*\(" | Select-Object -First 5 | ForEach-Object { $_.Line.Trim() }) -join "`n  "
