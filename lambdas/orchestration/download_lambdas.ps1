# ============================================================
# Download all 36 Lambda functions from AWS into D:\nflmodels_UPDATE\lambdas\
# ============================================================

$REPO_ROOT = "D:\nflmodels_UPDATE"
$LAMBDAS_DIR = Join-Path $REPO_ROOT "lambdas"

if (-not (Test-Path $LAMBDAS_DIR)) {
    Write-Host "ERROR: $LAMBDAS_DIR does not exist" -ForegroundColor Red
    exit 1
}

$SCRAPERS_DIR      = Join-Path $LAMBDAS_DIR "scrapers"
$CLEANERS_DIR      = Join-Path $LAMBDAS_DIR "cleaners"
$ORCHESTRATION_DIR = Join-Path $LAMBDAS_DIR "orchestration"
$LOGS_DIR          = Join-Path $LAMBDAS_DIR "logs"

New-Item -ItemType Directory -Force -Path $SCRAPERS_DIR,$CLEANERS_DIR,$ORCHESTRATION_DIR,$LOGS_DIR | Out-Null

# Organize existing files in lambdas/ root
Write-Host "=== Organizing existing files in lambdas/ ===" -ForegroundColor Cyan

Get-ChildItem -Path $LAMBDAS_DIR -Filter "*.ps1" -File | ForEach-Object {
    Move-Item -Path $_.FullName -Destination $ORCHESTRATION_DIR -Force
    Write-Host "  Moved $($_.Name) -> orchestration/"
}

Get-ChildItem -Path $LAMBDAS_DIR -Filter "*.csv" -File | ForEach-Object {
    Move-Item -Path $_.FullName -Destination $LOGS_DIR -Force
    Write-Host "  Moved $($_.Name) -> logs/"
}

Get-ChildItem -Path $LAMBDAS_DIR -Filter "*.parquet" -File | ForEach-Object {
    Remove-Item -Path $_.FullName -Force
    Write-Host "  Deleted $($_.Name) (test artifact)"
}

Get-ChildItem -Path $LAMBDAS_DIR -Filter "*.py" -File | ForEach-Object {
    Move-Item -Path $_.FullName -Destination $SCRAPERS_DIR -Force
    Write-Host "  Moved $($_.Name) -> scrapers/"
}

# Download all Lambdas from AWS
$TMP_DIR = Join-Path $env:TEMP "lambda_downloads"
New-Item -ItemType Directory -Force -Path $TMP_DIR | Out-Null

$CLEANERS = @(
    'allowed-pressure-cleaner',
    'coverage-summary-cleaner',
    'nfl-receiving-depth-weekly-no-targets-cleaner',
    'nfl-receiving-depth-weekly-targets-cleaner',
    'nfl-receiving-summary-no-targets-cleaner',
    'nfl-receiving-with-targets-cleaner',
    'pass-rush-summary-cleaner',
    'run-defense-summary-cleaner',
    'rushing-summary-cleaner'
)

$SCRAPERS = @(
    'allowed-pressure-scraper',
    'nfl-coverage-game-id-scraper',
    'nfl-coverage-scheme-scraper',
    'nfl-coverage-summary-scraper',
    'nfl-coverage-versus-scraper',
    'nfl-games-scraper',
    'nfl-pass-block-summary',
    'nfl-pass-rush-kpis-scraper',
    'nfl-pass-rush-summary-scraper',
    'nfl-play-count-scraper',
    'nfl-receiving-depth-scraper',
    'nfl-receiving-depth-weekly-no-targets',
    'nfl-receiving-depth-weekly-no-targets-v2',
    'nfl-receiving-depth-weekly-targets-scraper',
    'nfl-receiving-scheme-scraper',
    'nfl-receiving-summary',
    'nfl-receiving-summary-no-targets',
    'nfl-routes-scraper',
    'nfl-run-blocking-scraper',
    'nfl-run-defense-scraper',
    'nfl-rushing-summary',
    'nfl-slot-coverage-scraper',
    'passing-concept-scraper',
    'passing-depth-scraper',
    'passing-pressure-scraper',
    'passing-tip-scraper',
    'receiving-depth-parquet-lambda'
)

function Get-LambdaCode {
    param([string]$FunctionName, [string]$DestFolder)

    Write-Host "  $FunctionName..." -NoNewline

    try {
        $codeUrl = aws lambda get-function --function-name $FunctionName --query 'Code.Location' --output text 2>&1
        if ($LASTEXITCODE -ne 0) {
            Write-Host " FAIL (get-function)" -ForegroundColor Red
            return
        }

        $zipPath = Join-Path $TMP_DIR "$FunctionName.zip"
        Invoke-WebRequest -Uri $codeUrl -OutFile $zipPath -UseBasicParsing 2>$null

        $extractDir = Join-Path $TMP_DIR $FunctionName
        if (Test-Path $extractDir) { Remove-Item -Recurse -Force $extractDir }
        Expand-Archive -Path $zipPath -DestinationPath $extractDir -Force

        $sourceFile = Join-Path $extractDir "lambda_function.py"
        if (-not (Test-Path $sourceFile)) {
            $candidates = Get-ChildItem -Path $extractDir -Filter "*.py" -File | Where-Object { $_.Name -notmatch '^test_' }
            if ($candidates.Count -eq 0) {
                Write-Host " NO .py" -ForegroundColor Yellow
                return
            }
            $sourceFile = $candidates[0].FullName
        }

        $destName = $FunctionName -replace '^nfl-', ''
        $destPath = Join-Path $DestFolder "$destName.py"
        Copy-Item -Path $sourceFile -Destination $destPath -Force

        Remove-Item -Path $zipPath -Force
        Remove-Item -Path $extractDir -Recurse -Force

        Write-Host " OK" -ForegroundColor Green
    } catch {
        Write-Host " EXCEPTION: $_" -ForegroundColor Red
    }
}

Write-Host ""
Write-Host "=== Downloading SCRAPERS (27) ===" -ForegroundColor Cyan
foreach ($fn in $SCRAPERS) {
    Get-LambdaCode -FunctionName $fn -DestFolder $SCRAPERS_DIR
}

Write-Host ""
Write-Host "=== Downloading CLEANERS (9) ===" -ForegroundColor Cyan
foreach ($fn in $CLEANERS) {
    Get-LambdaCode -FunctionName $fn -DestFolder $CLEANERS_DIR
}

$REQ_PATH = Join-Path $LAMBDAS_DIR "requirements.txt"
@"
boto3==1.34.0
pandas==2.0.3
pyarrow==14.0.1
requests==2.31.0
"@ | Set-Content -Path $REQ_PATH -Encoding UTF8

Write-Host ""
Write-Host "=== Summary ===" -ForegroundColor Cyan
Write-Host "Scrapers:      $((Get-ChildItem $SCRAPERS_DIR -Filter *.py).Count) files"
Write-Host "Cleaners:      $((Get-ChildItem $CLEANERS_DIR -Filter *.py).Count) files"
Write-Host "Orchestration: $((Get-ChildItem $ORCHESTRATION_DIR).Count) files"
Write-Host "Logs:          $((Get-ChildItem $LOGS_DIR).Count) files"
Write-Host ""
Write-Host "Next: security scan for hardcoded credentials." -ForegroundColor Yellow