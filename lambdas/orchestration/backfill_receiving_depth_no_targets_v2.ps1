$LambdaName = "nfl-receiving-depth-weekly-no-targets-v2"
$LogFile    = "backfill_log.csv"

$SeasonMedians = @{
    2016 =  8907
    2017 =  9588
    2018 = 10421
    2019 = 11894
    2020 = 27949
    2021 = 31670
    2022 = 44807
    2023 = 47020
    2024 = 61105
    2025 = 83355
}

$Weeks = @(1..22 + 28, 29, 30, 32) | Sort-Object -Unique
$Seasons = @(2023, 2024, 2025)

$Completed = @{}
if (Test-Path $LogFile) {
    Import-Csv $LogFile | Where-Object { $_.status -eq 'Success' } | ForEach-Object {
        $key = "$($_.season)_$($_.week)_$($_.half)"
        $Completed[$key] = $true
    }
    Write-Host "Resume mode: $($Completed.Count) invocations already completed." -ForegroundColor Cyan
} else {
    "timestamp,season,week,half,min_id,max_id,status,records_saved,full_access,partial_access,no_data,failed,max_restricted_seen,error" |
        Out-File $LogFile -Encoding UTF8
}

$totalJobs = $Seasons.Count * $Weeks.Count * 2
$currentJob = 0

foreach ($season in $Seasons) {
    $median = $SeasonMedians[$season]
    if (-not $median) { continue }

    Write-Host "`n==================== SEASON $season ====================" -ForegroundColor Green

    foreach ($week in $Weeks) {
        $halves = @(
            @{ name = "lower"; min = 1;           max = $median  },
            @{ name = "upper"; min = $median + 1; max = 200000   }
        )

        foreach ($half in $halves) {
            $currentJob++
            $key = "$($season)_$($week)_$($half.name)"

            if ($Completed.ContainsKey($key)) {
                Write-Host "[$currentJob/$totalJobs] SKIP $season wk$week $($half.name)" -ForegroundColor DarkGray
                continue
            }

            $payloadObj = @{
                season          = $season
                week            = $week
                player_id_range = @($half.min, $half.max)
            }
            $payload = $payloadObj | ConvertTo-Json -Compress

            $payloadFile  = "payload_$PID.json"
            $responseFile = "lambda_response_$PID.json"

            [System.IO.File]::WriteAllText(
                (Join-Path (Get-Location) $payloadFile),
                $payload,
                [System.Text.Encoding]::ASCII
            )

            Write-Host "[$currentJob/$totalJobs] $season wk$week $($half.name) [$($half.min)-$($half.max)]..." -NoNewline

            $timestamp = (Get-Date).ToString("yyyy-MM-ddTHH:mm:ss")

            try {
                aws lambda invoke `
                    --function-name $LambdaName `
                    --payload "file://$payloadFile" `
                    --cli-binary-format raw-in-base64-out `
                    $responseFile `
                    --no-cli-pager | Out-Null

                if (Test-Path $responseFile) {
                    $response = Get-Content $responseFile -Raw | ConvertFrom-Json
                    $body     = $response.body | ConvertFrom-Json

                    if ($response.statusCode -eq 200) {
                        $records    = if ($null -ne $body.records_saved)        { $body.records_saved }        else { 0 }
                        $fullAcc    = if ($null -ne $body.full_access_count)    { $body.full_access_count }    else { 0 }
                        $partialAcc = if ($null -ne $body.partial_access_count) { $body.partial_access_count } else { 0 }
                        $noData     = if ($null -ne $body.no_data_count)        { $body.no_data_count }        else { 0 }
                        $failed     = if ($null -ne $body.failed_count)         { $body.failed_count }         else { 0 }
                        $maxRestr   = if ($null -ne $body.max_restricted_seen)  { $body.max_restricted_seen }  else { 0 }

                        if ($partialAcc -gt 0) {
                            Write-Host " OK ($records rows, $partialAcc PARTIAL/$fullAcc full, max_restr=$maxRestr)" -ForegroundColor Yellow
                        } elseif ($records -eq 0) {
                            Write-Host " OK (0 rows)" -ForegroundColor DarkGreen
                        } else {
                            Write-Host " OK ($records rows)" -ForegroundColor Green
                        }

                        "$timestamp,$season,$week,$($half.name),$($half.min),$($half.max),Success,$records,$fullAcc,$partialAcc,$noData,$failed,$maxRestr," |
                            Out-File $LogFile -Append -Encoding UTF8
                    } else {
                        $err = "$($body.error)" -replace ',',';'
                        Write-Host " FAIL: $err" -ForegroundColor Red
                        "$timestamp,$season,$week,$($half.name),$($half.min),$($half.max),Failed,0,0,0,0,0,0,$err" |
                            Out-File $LogFile -Append -Encoding UTF8
                    }
                } else {
                    Write-Host " FAIL: no response file" -ForegroundColor Red
                    "$timestamp,$season,$week,$($half.name),$($half.min),$($half.max),Failed,0,0,0,0,0,0,no_response" |
                        Out-File $LogFile -Append -Encoding UTF8
                }
            } catch {
                $err = $_.Exception.Message -replace ',',';'
                Write-Host " EXCEPTION: $err" -ForegroundColor Red
                "$timestamp,$season,$week,$($half.name),$($half.min),$($half.max),Failed,0,0,0,0,0,0,$err" |
                    Out-File $LogFile -Append -Encoding UTF8
            } finally {
                if (Test-Path $responseFile) { Remove-Item $responseFile -Force }
                if (Test-Path $payloadFile)  { Remove-Item $payloadFile  -Force }
            }
        }
    }
}

Write-Host "`n==================== DONE ====================" -ForegroundColor Green