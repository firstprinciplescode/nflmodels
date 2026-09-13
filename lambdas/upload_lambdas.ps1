# ============================================================
# Upload scraper Lambdas from lambdas/scrapers/ to AWS.
# Mirror of download_lambdas.ps1. Each function is a single-file zip on top
# of the AWSSDKPandas layer; the file inside the zip MUST be named after the
# function's handler module (rushing-summary.lambda_handler -> rushing-summary.py,
# lambda_function.lambda_handler -> lambda_function.py), which this reads live.
#
#   .\lambdas\upload_lambdas.ps1                 # DRY RUN: shows what would change, uploads nothing
#   .\lambdas\upload_lambdas.ps1 -Deploy         # uploads every scraper in the list
#   .\lambdas\upload_lambdas.ps1 -Deploy -Only nfl-rushing-summary
# ============================================================
param([switch]$Deploy, [string]$Only)

$REPO_ROOT    = Split-Path -Parent $PSScriptRoot
$SCRAPERS_DIR = Join-Path $PSScriptRoot "scrapers"
$TMP_DIR      = Join-Path $env:TEMP "lambda_uploads"
New-Item -ItemType Directory -Force -Path $TMP_DIR | Out-Null

$SCRAPERS = @(
    'allowed-pressure-scraper', 'nfl-coverage-game-id-scraper', 'nfl-coverage-scheme-scraper',
    'nfl-coverage-summary-scraper', 'nfl-coverage-versus-scraper', 'nfl-games-scraper',
    'nfl-pass-block-summary', 'nfl-pass-rush-kpis-scraper', 'nfl-pass-rush-summary-scraper',
    'nfl-play-count-scraper', 'nfl-receiving-depth-scraper', 'nfl-receiving-depth-weekly-no-targets',
    'nfl-receiving-depth-weekly-no-targets-v2', 'nfl-receiving-depth-weekly-targets-scraper',
    'nfl-receiving-scheme-scraper', 'nfl-receiving-summary', 'nfl-receiving-summary-no-targets',
    'nfl-routes-scraper', 'nfl-run-blocking-scraper', 'nfl-run-defense-scraper', 'nfl-rushing-summary',
    'nfl-slot-coverage-scraper', 'passing-concept-scraper', 'passing-depth-scraper',
    'passing-pressure-scraper', 'passing-tip-scraper'
)
if ($Only) { $SCRAPERS = $SCRAPERS | Where-Object { $_ -eq $Only }; if (-not $SCRAPERS) { Write-Host "no such scraper: $Only" -ForegroundColor Red; exit 1 } }

if (-not $Deploy) { Write-Host "DRY RUN -- add -Deploy to upload" -ForegroundColor Yellow }
$ok = 0; $bad = 0
foreach ($fn in $SCRAPERS) {
    $local = Join-Path $SCRAPERS_DIR (($fn -replace '^nfl-', '') + ".py")
    if (-not (Test-Path $local)) { Write-Host "  $fn : local file missing ($local)" -ForegroundColor Red; $bad++; continue }
    $cfg = aws lambda get-function-configuration --function-name $fn --query "[Handler,CodeSize]" --output text 2>&1
    if ($LASTEXITCODE -ne 0) { Write-Host "  $fn : get-function-configuration failed: $cfg" -ForegroundColor Red; $bad++; continue }
    $handler, $sizeBefore = $cfg -split "\s+"
    $module = ($handler -split '\.')[0]
    $stage = Join-Path $TMP_DIR $fn
    if (Test-Path $stage) { Remove-Item -Recurse -Force $stage }
    New-Item -ItemType Directory -Force -Path $stage | Out-Null
    Copy-Item $local (Join-Path $stage "$module.py")
    $zip = Join-Path $TMP_DIR "$fn.zip"
    if (Test-Path $zip) { Remove-Item -Force $zip }
    Compress-Archive -Path (Join-Path $stage "$module.py") -DestinationPath $zip
    $localKB = [math]::Round((Get-Item $local).Length / 1KB, 1)
    if (-not $Deploy) {
        Write-Host ("  {0,-46} handler {1,-38} zip entry {2,-28} local {3} KB (deployed {4} B)" -f $fn, $handler, "$module.py", $localKB, $sizeBefore)
        continue
    }
    $r = aws lambda update-function-code --function-name $fn --zip-file "fileb://$zip" --query "[CodeSize,LastModified]" --output text 2>&1
    if ($LASTEXITCODE -ne 0) { Write-Host "  $fn : UPLOAD FAILED: $r" -ForegroundColor Red; $bad++; continue }
    aws lambda wait function-updated --function-name $fn 2>&1 | Out-Null
    Write-Host ("  {0,-46} uploaded as {1,-22} CodeSize {2} -> {3}" -f $fn, "$module.py", $sizeBefore, ($r -split "\s+")[0]) -ForegroundColor Green
    $ok++
}
Write-Host ""
if ($Deploy) { Write-Host "uploaded: $ok   failed: $bad" -ForegroundColor Cyan } else { Write-Host "dry run over $($SCRAPERS.Count) functions; failures listed above: $bad" -ForegroundColor Cyan }
