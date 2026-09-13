# REBUILD RUNBOOK — from an empty R session to `team_yoy("NE")`

*Written 2026-09-12 from the code as it is, not from memory. Every claim
below was checked against a file; where something is NOT verified it says so.*

**The one law:** never trust an object that is sitting in a session. If a file
didn't make it in this session, it is a ghost (the Mattison/Murphy id lived in a
session `combined_ids` for six weeks after the file was fixed). Everything comes
from a file, in this order. No file in the repo can wipe the session any more —
every `rm(list = setdiff(ls(), keep_objects))` is commented out (2026-09-12).

Working directory for everything below: the repo root (`nflmodels_UPDATE/`).

---

## Stage 0 — packages + credentials (once per session)

```r
source("util/R_packages.R")               # every library the chains use, incl. conflicted, gt, nflreadr
source("util/amazon_web_credentials.R")   # gitignored; AWS CLI must also work: aws s3 ls
```

## Stage 1 — base tables (`data_build/`)

Two ways. **A** is minutes, **B** is an afternoon.

**A. Load the last saved workspace** (Sept 9, 2026; 582 objects incl. `combined_pbp`
343,546 × 515, `combined_grade_epa_summary`, `cluster_join`, all the legacy
`rush_stats_*`, `full_pass_rush_qbgrp`, `run_defense_summary_qbgrp`, coverage frames):

```r
load("nfl_the_everything_workspace.RData")
```

It carries a **stale `combined_ids`** — Stage 2 overwrites it. Don't use its
`combined_ids` for anything.

**It also carries stale unit frames.** Its pass-rush, pass-block, run-block and
coverage frames (and `play_counts` / `games`) were pulled **Jan 13, 2026** and
stop at 2025 week 28 — the divisional round, conference championships and
Super Bowl (weeks 29, 30, 32) are missing. NE's faced diet comes up 18 weeks
instead of 21; the pass-rush and secondary league files wall on that, pass
block and run block **don't** and would price a wrong slate silently. Verified
2026-09-12. So: the workspace is for `combined_pbp` and
`combined_grade_epa_summary` (current through week 32); **every unit's step-0
file is re-run, always.** `pipeline_status()` shows a `wk25` column — 32 is
complete, 28 is a stale copy.

**B. Rebuild** (per `Lineage One.md`; order not re-verified today):
`data_build/pbp_nfl_base.R` → `data_build/part_nfl_base.R` →
`data_build/pbp_part_combined_join_AWS.R` → `combined_pbp`.

## Stage 2 — player ids (`data_build/`, run in this order, ~10 min of Athena)

```r
source("data_build/pff_ids_build_AWS.R")          # combined_ids          (offense)
source("data_build/pff_ids_build_defense_AWS.R")  # combined_ids_defense
source("data_build/pff_ids_validate_cross.R")     # id_xwalk, team_map, pff_team_lookup — STOPS on any cross-table id collision
```

`pff_ids_build_AWS.R` used to wipe the session on line 18. It no longer does.

## Stage 3 — shared constants

```r
source("pff_stats/shared_ne_2026_constants.R")    # opp_2026_teams, sched_2026, in_season, blend2, ol_pos_levels
```

Verbatim copy of `new_england_opp_ol_schedule.R` lines 39–45 / 121–126, which
every other unit needs but which the OL file only defines after a wall on
pass-block frames. If the slate changes in the OL file, change it here too.

It also defines **`percent_rank_avg`** — the canon `(rank−1)/(n−1)` from
`pff_pass_rush_AWS.R:187`. The schedule files carry a *different* fallback
(`rank/n`) that fires if nothing defined the function first. That is a
session-order landmine: on 2026-09-12 the fallback fired and the run-defense
and rushing chains rode the wrong scale until re-run. Sourcing this file first
ends that.

## Stage 4 — the seven units

Every unit is the same five-file family, run top to bottom:

| step | file pattern | makes |
|---|---|---|
| step-0 | `pff_*_AWS.R` / `*_step0_AWS.R` | the game-level frame + `qbgrp_ssn`/`def_ssn` |
| schedule | `new_england_opp_<unit>_schedule.R` | membership, 2025 faced, 2026 slate |
| c3 | `league_<unit>_evaluating_currency_three.R` | `<unit>_c3_pctl` |
| league opp | `league_opp_<unit>_schedule.R` | league-wide faced slates, `_lg` frames |
| availability | `league_<unit>_availability.R` | injury-priced values, `members_*` |

### Order between units — this matters

**Run defense must run before rushing.** Both rushing schedule files have a
second section (`new_england_opp_rushing_schedule.R:588`, `league_opp_rushing_schedule.R:~610`)
that walls on `run_defense_qbgrp`, `rundef_season_pctl_sos`, `team_band_2026`.
`source()` stops there, and the league-wide `_lg` section after it (line 734+)
never runs, so the rushing availability file walls too.

The other five units are independent of each other (OL/pass-block/run-block
share one schedule file).

### 4.1 Run defense

```r
source("pff_stats/run_defense/pff_run_defense_qbgrp_step0_AWS.R")      # NEW 2026-09-12 — needs Andy's stamp; expect 0 unmatched, ~102k raw rows
source("pff_stats/run_defense/new_england_opp_run_defense_schedule.R")
source("pff_stats/run_defense/league_run_defense_evaluating_currency_three.R")
source("pff_stats/run_defense/league_opp_run_defense_schedule.R")
source("pff_stats/run_defense/league_run_defense_availability.R")
```

Why the step-0 is new: the header of the schedule file says
"step 0 v3 in session: run_defense_qbgrp (raw + qbgrp/def ids)". That code
was never saved (it predates the oldest console history, Sept 7). The file
follows `pff_run_defense_AWS.R:50-92` minus the `>= 14` snap filter, which the
header explicitly says v3 removed. `pff_run_defense_AWS.R` itself makes the
*filtered* `run_defense_summary_qbgrp` — a different frame; it is the plotting
/ exploration file, not the chain's step-0.

### 4.2 Rushing (after 4.1)

```r
source("pff_stats/rushing/pff_rushing_qbgrp_step0_AWS.R")               # NEW 2026-09-12 — ran clean: 12,975 HB games, 0 unmatched
source("pff_stats/rushing/new_england_opp_rushing_schedule.R")
source("pff_stats/rushing/league_rushing_evaluating_currency_three.R")
source("pff_stats/rushing/league_opp_rushing_schedule.R")
source("pff_stats/rushing/league_rushing_availability.R")
```

### 4.3 Pass rush

```r
source("pff_stats/pass_rush/pff_pass_rush_AWS.R")                        # canon step-0: full_pass_rush_qbgrp, percent_rank_avg
source("pff_stats/pass_rush/new_england_opp_pass_rush_schedule.R")      # needs combined_ids_defense (Stage 2)
source("pff_stats/pass_rush/league_pass_rush_evaluating_currency_three.R")
source("pff_stats/pass_rush/league_opp_pass_rush_schedule.R")
source("pff_stats/pass_rush/league_pass_rush_final_evaluation.R")
source("pff_stats/pass_rush/league_pass_rush_availability.R")
```

`pff_pass_rush_AWS.R` starts with `conflicts_prefer(...)` — it needs
`library(conflicted)` loaded (Stage 0 does that). It contains `View()` calls;
fine in RStudio.

### 4.4 OL — pass block + run block (one schedule, two currencies)

```r
source("pff_stats/pass_block/pff_pass_block_AWS.R")                      # all_pass_block_summary + player-season summaries
source("pff_stats/run_block/pff_run_block_AWS.R")                        # run_block_summary_qbgrp, gap/zone_player_season_summary
source("pff_stats/pass_block/new_england_opp_ol_schedule.R")            # needs id_xwalk + pff_team_lookup (Stage 2)
source("pff_stats/pass_block/league_pass_block_evaluating_currency_three.R")
source("pff_stats/run_block/league_run_block_evaluating_currency_three.R")
source("pff_stats/pass_block/league_opp_pass_blocking_schedule.R")
source("pff_stats/run_block/league_opp_run_blocking_schedule.R")
source("pff_stats/pass_block/league_pass_block_final_evaluation.R")
source("pff_stats/pass_block/league_pass_block_availability.R")
source("pff_stats/run_block/league_run_block_availability.R")
```

`pff_run_block_AWS.R` needs `ggpath`, `ggimage`, `ggtext`; it builds its frames
by line 242 and then errors at line 254 on session-only exploration objects
(`run_defense_opp_position_percentile`, `rush_stats_high`). That error is
expected — everything the OL schedule needs already exists by then.

### 4.5 Receiving

```r
source("pff_stats/receiving/receiving_stats_build_AWS.R")                # receiving_func_base — needs combined_pbp, combined_ids, cluster_join (Stage 1A)
source("pff_stats/receiving/pff_receiving_man_zone_exploration_AWS.R")  # receiver_scheme_final
source("pff_stats/receiving/new_england_opp_receiving_schedule.R")
source("pff_stats/receiving/league_receiving_evaluating_currency_three.R")
source("pff_stats/receiving/league_opp_receiving_schedule.R")
source("pff_stats/receiving/league_receiving_availability.R")
```

### 4.6 Secondary

```r
source("pff_stats/secondary/pff_secondary_cache_step0_AWS.R")            # NEW 2026-09-12 — writes coverage_raw_build_cache_cov.rds; needs Andy's stamp
source("pff_stats/secondary/new_england_opp_secondary_schedule.R")      # needs combined_grade_epa_summary + combined_ids_defense; feed 2 = coverage_scheme (in session, else cached, else pulled)
source("pff_stats/secondary/league_secondary_evaluating_currency_three.R")
source("pff_stats/secondary/league_opp_secondary_schedule.R")
source("pff_stats/secondary/league_secondary_availability.R")
```

Why the step-0 is new: both secondary schedule files stop with "cache absent
-- run phase6_step0_secondary_gate_ritual.txt first". That ritual was console
code in a `.txt`, never committed; the `.rds` it wrote is on no machine. The
file rebuilds what the consumers demand (read off their code, not memory):
raw `coverage_summary`, **un-gated** (the modal-band law needs the un-gated
frame), `final_position` with the SCB class derived from
`coverage_summary_by_game` exactly as the tower does at
`pff_pass_coverage_AWS.R:128-139`, team-fixed. Receipts to match from the
ritual header: 69,250 rows; band4 vocab LB 24,279 / S 16,948 / CB 14,503 /
SCB 5,686.

## Any team, and comparing two

The league-wide `_lg` frames cover all 32 teams, so once a unit is in session
the viewer works for any code — nothing is re-run per team:

```r
team_yoy("DEN");  team_yoy("NE")
team_report("DEN")          # all five views: slate, sched, lastyear, yoy, own
```

There is no side-by-side function yet; run the two calls and read across.

## Seeing what is loaded

```r
source("util/pipeline_status.R")
pipeline_status()           # every unit x stage: LOADED (rows x cols) / MISSING, and the file that makes it
```

## Stage 5 — the viewer

```r
source("pff_stats/evaluation/any_team_evaluation.R")
team_yoy("NE")        # also: team_own, team_slate, team_sched, team_lastyear, team_report
```

It prints whatever units are in session and says `skipped -- missing frame: X`
for the rest. It only hard-stops if *no* unit frames exist.

---

## Save points (do this; it is what would have saved today)

`util/cache_frames.R` writes dated `.rds` receipts to `cache/` (gitignored) and
prints the date + row count of anything it loads back, so a stale frame is
visible the moment it enters the session.

```r
source("util/cache_frames.R")
cache_frames("combined_ids", "combined_ids_defense", "pff_team_lookup")   # after Stage 2
cache_frames("rushing_qbgrp", "run_defense_qbgrp")                        # after each step-0
cache_ls()                                                                # what is on disk, how old
```

Next session's fast path: `uncache_frames("rushing_qbgrp")` instead of the
Athena pull. The canon path is always re-sourcing the step-0 file; the cache is
the shortcut, and it says its own age.

## What is enforced (CI fails the PR)

`tests/test_session_safety.py`, run by `python -m pytest` on every PR:

1. **No file may wipe the session.** Any active `rm(list = ls())` /
   `rm(list = setdiff(ls(), ...))` in a `.R` file fails the build.
2. **Every walled-on object has a producer.** Every name in a
   `needed_* <- c(...)` wall under `pff_stats/` must be assigned somewhere in
   the repo. A step-0 that only ever lived in a session (today's
   `rushing_qbgrp`, `run_defense_qbgrp`, the OL constants) fails the build.
   `KNOWN_HOLES` in the test is the only allowlist, and each entry needs a
   reason.

## Habits the code cannot enforce

- **One working copy.** `Documents\nflmodels` (flat, Aug 13) and
  `Downloads\nflmodels_UPDATE` (the git repo) have drifted; the weekly runbook
  still points at `Documents`. Pick the git repo; retire the other.
- **Nothing runs in the console that is not in a file.** If it is worth
  running, it is worth `source()`-ing. Console-only code is how both step-0s
  were lost.
- **Commit at the end of every session** to a `wip-<date>` branch:
  `git add -A; git commit -m "wip <date>"; git push -u origin <branch>`.
  Ten days of engines sat uncommitted before tonight.
- **"Frozen" means check, not wait.** Task Manager → `rsession-utf8.exe`.
  CPU near 100% or an `aws` child = working. 0% and no child = the IDE lost
  the session; the queued commands never ran. Restart R.

## Known holes (2026-09-12)

- `load_all.R` (Downloads) does not work as written: non-recursive file search,
  no `data_build` stage, and it predates the `rm()` fix. Superseded by this file.
- Secondary step-0 (4.6) — lost, needs rebuilding.
- Run-defense step-0 (4.1) — rebuilt today from the canon join; needs a stamp
  against the 2026-08-14 receipts printed in the schedule header.
- Stage 1B order not re-verified today.
