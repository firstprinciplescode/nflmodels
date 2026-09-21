# Read this first (Kimi, Claude, any coding agent)

Owner: Andy. Talk in plain English, no jargon. When he has to run something, give the
exact copy-paste command and say where it runs (RStudio console, PowerShell, AWS console).
Find files and lines yourself; never ask him where something is.

## Before you touch anything
- `HOW_NFLMODELS_WORKS.md`: plain guide to every unit and the team viewer.
- `AWS_MAP.md`: how data gets from PFF into R, and what you may and may not run in AWS.
- `REBUILD_RUNBOOK.md`: build order. `weekly_pipeline_runbook.md`: the weekly dbt build.
- `git status` plus file modified-times: another agent may be mid-edit. Don't edit a file someone else changed in the last hour without saying so.

## Hard rules
- **Andy's live R session is off limits.** No `rm(list = ls())`. Don't load `cache/*.rds` or `.RData` into it. Never assign test data under the name of a real frame.
- **Only the change asked for.** Never commit, revert, or "fix" uncommitted edits you didn't make.
- **Use his percentiles, not raw production.** Check direction before flipping: OL pressure and hurries percentiles are ranked on the negated rate, so higher already means fewer allowed.
- **Rooms use his seat counts,** never "top 2". Coverage `N_SEC_LG` is CB 3, SCB 1, S 3, LB 2, split by man and zone. Pass rush is ED 4 and DI 4. Run defense `N_RD_LG` is DI 5, ED 5, LB 3, S 4. OL is one per slot. Receiving `N_CORPS` is 8. Rushing `N_RB` is 2.
- **Position comes from his `final_position`** (it has SCB). The raw PFF position column does not.
- **No "WR1".** Receivers are compared within role and target cluster.
- **Team comes from the game rows.** A traded player counts for each team he played for, with his snaps there, not only his main team.
- **Roster-impact work uses no `combined_pbp` and no on/off.** Team outcomes come from PFF totals (`nfl_data.passing_pressure`, `nfl_data.rushing_summary`) joined through `combined_grade_epa_summary`. Include playoffs.
- **2026 data is quarantined.** Don't pull it or bring it up while he's working on 2025.
- **AWS is read-only for agents.** See `AWS_MAP.md`. Never read secret values or `pff_cookies.json`, and never deploy, invoke scrapers, write S3, or change IAM.
