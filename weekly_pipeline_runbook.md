# NFL Pipeline — Weekly Runbook

*Run after each week's data has landed in S3 (Lambda scrapers done).*

## 0. One-time per PowerShell window
New windows forget the Python 3.12 path. If `dbt` isn't recognized:
```powershell
$env:Path = "C:\Users\vflre\AppData\Local\Programs\Python\Python312\Scripts;" + $env:Path
```

## 1. The command
```powershell
cd C:\Users\vflre\Documents\nflmodels\nfl_dbt
dbt build
```
That one command: syncs new S3 partitions (16 MSCK hooks) → dedups staging →
rebuilds all 36 models in dependency order → runs all 45+ tests → reports.
Takes ~5–8 min single-threaded (or ~3 with `threads: 4` in the profile).

## 2. Reading the tail
Look at the final line: `Done. PASS=… WARN=… ERROR=… SKIP=…`

| Result | Meaning | Action |
|---|---|---|
| ERROR=0, WARNs near baseline | Data is good | Go run the R/XGBoost layer |
| WARN spike (see baselines) | A feed drifted or a scrape doubled | Investigate before betting |
| ERROR > 0 | A hard invariant broke; downstream models were SKIPPED (protected, not deleted) | Read the [ERROR] block; fix or ask Claude |

**Warn baselines (as of 2026-07-25):**
- `assert_pass_rush_summary_vs_kpis_drift` ≈ 6,000 (PFF re-grading noise — normal)
- `assert_calcs_passing_tip` ≈ 3,500 (denominator definition gap — normal)
- `assert_rushing_summary_vs_vw_receiving_enriched` ≈ 6 (tiny cross-check gap)
- Grain-uniqueness warn tests: normally 0. **Any nonzero = duplicate scrape rows
  in that feed** → add dedup to that feed's staging model (copy the pattern in
  `stg_pff__receiving_depth_weekly_no_targets`).

## 3. Only refreshed some feeds? Targeted build
```powershell
dbt build --select stg_pff__receiving_with_targets+ stg_pff__games+
```
`model+` = that model and everything downstream. Hooks still run (they always do).

## 4. If code changed elsewhere (another machine, a merged PR)
```powershell
cd C:\Users\vflre\Documents\nflmodels
git checkout main
git pull
```
Do this BEFORE the weekly build if you merged anything since last week.

## 5. Making code changes (the forever ritual)
branch → edit → `dbt build` locally → commit → push → PR → robots go green → merge.
Never commit straight to main; the PR checks (pytest, sqlfluff, dbt-build) are the
safety net you built.

## Troubleshooting quick hits
- `dbt` not recognized → step 0.
- Credentials error → AWS CLI creds expired/changed on this machine (`aws s3 ls` to test).
- A model suddenly empty → check the raw feed's partition landed in S3
  (`aws s3 ls s3://nfl-pff-data-lucas/data/<feed>/season=2026/ --recursive | findstr week`).
- Everything on fire → nothing is lost: raw S3 is untouched by dbt; any model
  rebuilds from source; `git revert` undoes any code change.
