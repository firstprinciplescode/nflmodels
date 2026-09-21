# Session log, 2026-09-20 to 09-21 (JAX @ DEN week)

Written by Claude at the end of the session. Everything below was run or checked, not assumed. "Separate process" means an
`Rscript` run outside RStudio; the live R session was never touched.

## 1. Why old frames kept coming back (root cause, now switched off)

- RStudio restored `C:/Users/vflre/OneDrive/Documents/.RData` at **every** start. That file was saved **2026-01-27** and held the
  January copies: `rush_stats_final` and `receiving_func_base` without weeks 30 / 32, `qb_stats_df_final` without week 32 and 18 columns
  short, no `receiver_registry`, and the **January comparison lookups** (see section 4).
- `save.image()` then froze those January frames into `nfl_the_everything_workspace.RData`, so `load()` brought them back too.
- No file in the repo drops a week. Every build step is a left join; a 9-agent read of both build chains found no filter on week.

Done:
- The January `.RData` was renamed, then sent to the **Recycle Bin** (not hard-deleted). `OneDrive/Documents` has no `.RData` now.
- RStudio preferences (`%APPDATA%/RStudio/rstudio-prefs.json`, backup `rstudio-prefs.json.bak_20260920` beside it):
  `"load_workspace": false`, `"save_workspace": "never"`. RStudio was closed when this was written. **Check once:** Tools > Global
  Options > General should show "Restore .RData" unticked and "Save workspace on exit: Never".
- Still true: `nfl_the_everything_workspace.RData` holds the old frames. Do not `load()` it. Use `util/start_session.R`.

## 2. New files

| File | What it is |
|---|---|
| `util/start_session.R` | **Run this first in every session.** Packages, working directory, helpers, the good frames from `cache/`, the check table. Tested four ways (below). |
| `util/data_check.R` | Read-only check of every cached model frame; writes `cache/data_check_<date>.txt`. Run from the Terminal. |
| `util/frames_guard.R` | (other chat) `frames_check()`, `frames_restore()`, `safe_load()`, `safe_save_image()`, `registry_keep()`, `registry_back()`. |
| `pff_stats/rushing/rebuild_rush_stats_final_headless.R` | Rebuilds `rush_stats_final` + 20 sibling objects from the six rushing build files in ~3 min, from the Terminal. Refuses to write unless every season has weeks 30 / 32 AND no player-season changed cluster number. |
| `model_funcs/fg_func.R`, `model_funcs/dtd_func.R`, `data_build/dtd_games_build.R` | FG attempts and D/ST touchdown tools (earlier sessions). |

`start_session.R` tests (separate process, real cache files): fresh session = 101 objects in 9 seconds, none of "his" variables
touched; sourced again with weather joined and a marked frame = nothing replaced; January-style session (key frame cut at week 29,
a 6-group `c_dashboard`, a short `situation_cluster_df`, one edited lookup) = key frame restored, the two older-build objects swapped,
old copies kept as `<name>_STALE_<time>`, edited lookup left alone and counted; `SS_LOOKUPS <- "force"` = lookups swapped back.

## 3. Changed files

- `pff_stats/rushing/rush_stats_df_build.R`: removed the `ifelse(season == 2025, NA_real_, ...)` blanking of `part_xypc` and
  `part_xtd`. Ran it on the cached `rush_stats_final`; `rush_stats_high` (5,263 rows), `rush_stats_low` (4,917) and
  `rush_stats_rec` (2,600) are cached with 0 NA part columns in 2025 and weeks 28 / 29 / 30 / 32.
- `pff_stats/rushing/rush_thresholds_engine.R`: Trevor Lawrence added to `rush_template_jax` from TEMPLATE row 15
  (QB, C, RUN SIT `1 3 99`, GAP `4 99`, ZONE `(-99, 99) NA`, XTD `60 TO 100`). The QB row sits above the header row and had been missed.
- `pff_stats/rushing/rushing_stats_comparison.R` and `model_funcs/rush_func_AWS.R`: GAP cluster names redone for the build in
  `cache/` (12 of 16 had still named the June-workspace numbers). Situation names were redone by the other chat and check out.
- `model_funcs/rush_func_AWS.R` line 303: one extra `)` removed from a scratch query. The file did not parse; it does now.
  That is the only change to that query.
- `util/pipeline_status.R`: a `model_frames` unit so the STALE line covers `rush_stats_final` and `rush_stats_high / low / rec`.
- `.gitignore`: `cache/` added (it was only ignored by file extension, so a `.txt` log in it would have been committed).

## 4. Comparison lookups: NOT uploaded to S3, and why

Checked object by object (separate process): the ten comparison functions, ten scaled frames and ten importance matrices in the
live session are **identical to the 2026-01-27 `.RData`**. They are the January build. S3 (`qbgrp_def_functions/`, `def_functions/`)
holds the **2026-04-01 rebuild** from `df_builds/` (9 of 10 lenses differ; `less_def` is the same file, dated 01-27).

- Every workbook pushed so far, every saved tolerance, and this week's bets used the **January** set.
- The **April** set on S3 is newer, but its `comparison_pressure_def_func` and `comparison_pa_def_func` return a column named
  `Defense` where every engine reads `$QB`, so those two defense pools collapse to the one defense.
- Every `*_func_AWS.R` file downloads the April set when sourced; `stats_comparison_engine.R` only does when the functions are missing.
  So a session could hold either set depending on what was sourced last.
- Uploading the January set over S3 would destroy the April rebuild (the bucket has no versioning). Which set is "right" is Andy's
  ruling. Nothing was written to S3. The January set is in `cache/comparison_lookups_from_workspace_2026-09-19.rds` (40 objects,
  5 MB) and the April set was downloaded for comparison only.
- `start_session.R` loads the January set by default **only where a lookup is missing** (`SS_LOOKUPS <- "s3"` to skip).

## 5. Data check, 2026-09-21 (`cache/data_check_2026-09-21.txt`)

All five cached model frames + the three rushing team-game frames + `combined_pbp`: 50 checks, **2 flags**.

PASS: every season 2016-2025 has every regular-season week and playoff weeks 28 / 29 / 30 / 32 with 8 or 12 / 8 / 4 / 2 team-games;
32 defenses with 16 (17 from 2021) regular-season games; no duplicate team-games; NA share of every key column steady across seasons
(2025 part_ columns: 0 NA in `qb_stats_df_final`, `xtd_proportion`, `rush_stats_final`, `rush_stats_high / low / rec`); season means
within 15% of the ten-season mean; pbp and part xTD totals agree; shares add to 1 per team-game; every `qb_stats_df_final` team-game
has rushing and receiving rows; `combined_pbp` is the good build.

Sense checks: WR1 23.6% of targets / 37.6 routes, WR2 17.9%, WR3 11.8%, WR4 4.6%, **WR5 2.3% / 4.8 routes**, WR6 1.5%; the same
order inside the 2025 playoff weeks. Depth of target WR 10.9 > TE 6.7 > BACK 0.6. Lead backs 16.4 carries / 69% share, committee
7.7 / 31%, spot 1.9 / 8%. Cluster groups 4 / 5 / 4 situation and 8 / 4 / 4 gap.

FLAG 1: `qb_stats_df_final$less_rate_rank_def` runs to 1.067 in 326 rows (every other of the 144 rank columns sits in 0..1).
Small, one column, in the build file; not changed.
FLAG 2: `receiving_func_base` has 7 duplicated player-games where one PFF player matched two play-by-play names (Thielen PIT wk 14
2025 also matched "A.Rodgers", Ridley ATL wk 7 2020 also matched "H.Hurst", D.Thomas, Di.Johnson, M.Jones Jr., Mi.Wilson, N.Williams).
7 rows of 58,379; not changed.

## 6. Workbook and outputs

- `outcomes/JAX v DEN.xlsx`: 4-slot rank block in TEMPLATE (BS:DH); `Rush - T.Lawrence` added as the last sheet (J4 1.03, K4 1.28,
  L4 0.86; no tolerance widening needed; games In 23 / 27 / 29 / 56 / 30). Parity check first: Tuten B reproduced the pushed
  workbook exactly (In 11 / 11 / 7 / 20 / 15). Nothing was pushed to S3 for Lawrence; his file is in
  `outcomes/JAXLawrence-2025 vs DEN2025/`.
- Pinned docs: Receiver cap, Rusher cap (what +/-.125 percentile equals).

## 7. Not committed on purpose

- `AWS_MAP.md`: the GitHub repo is **public**, and that file states the AWS account number and that this machine logs in as the
  account root user. Left untracked. Everything else was scanned (three read-only reviewers): no keys, tokens, cookies or passwords.

## 8. Open, Andy's call

1. Which comparison lookups are the standard: January (what everything so far used) or April (S3, with the `Defense` column bug).
2. `less_rate_rank_def` above 1, and the 7 duplicate receiver rows.
3. Back up `cache/` (the only good copies of `combined_pbp` and `rush_stats_final`); not in git, not in OneDrive.
4. `tests/test_session_safety.py` fails locally because of `pff_stats/evaluation/FAKE_DATA_selftest_do_not_run.R` line 31
   (`rm(list = ...)` of its own fake objects). CI only runs on pull requests to main.
5. `model_funcs/rush_func_AWS.R` still runs a live `rush_func("TENWard-2025", "JAX2025", ...)` when sourced, which writes
   `outputs/Rush - TEN REC.xlsx` to S3; `fg_func.R` line 96 runs the JAX example on source.
6. `model_funcs/rec_func_AWS.R` line 695 is a bare `::summarise(...)` in the scratch area at the bottom. It has been in git since
   09-12, and it stops `source()` of the whole file (running the file piece by piece works). 43 of the 44 R files in this commit parse;
   that is the one that does not. Not changed.
