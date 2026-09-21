# How nflmodels works

Line citations are `file:line`, and they match the working tree as of 2026-09-14, Andy's uncommitted edits included. Player ids, team codes and seasons that appear in example calls at the bottom of files are examples Andy ran, not settings. A real setting is a threshold stored in a variable that the pipeline itself uses.

## How a team report gets built

**1. Data build.** Lambda scrapers under `lambdas/scrapers/` pull PFF premium tables into S3 as season-partitioned parquet files, and Athena exposes those as tables. `nfl_dbt` turns the raw tables into views and team-week marts. The key mart is `vw_team_passing_summary`, which names each team-week's starting QB. `data_build/pbp_nfl_base.R` loads nflfastR play-by-play and builds `combined_grade_epa_summary`. This is where the two keys every unit uses are created: `qbgrp_ssn` (team + starting QB last name + season) and `def_ssn` (opponent + season) (`pbp_nfl_base.R:315-317`). `pbp_combined_AWS.R`, `part_nfl_base.R` and `participation_combined_AWS.R` score the model predictions, and `pbp_part_combined_join_AWS.R:88-94` stamps the two keys onto every play of `combined_pbp`. Three files build the player id crosswalks: `pff_ids_build_AWS.R`, `pff_ids_build_defense_AWS.R` and `pff_ids_validate_cross.R`. They produce `combined_ids`, `combined_ids_defense`, `id_xwalk` and `pff_team_lookup`.

**2. Shared constants.** `pff_stats/shared_ne_2026_constants.R` is sourced right after data build. It puts `ol_pos_levels`, NE's 14 opponents (`opp_2026_teams`), NE's 17-game list (`sched_2026`), `in_season`, `blend2` and the canon `percent_rank_avg` into the session.

**3. Step-0 files (one per unit).** Each builds the unit's player-game frame, tagged with the offense faced and the defense:
- `run_defense/pff_run_defense_qbgrp_step0_AWS.R` → `run_defense_qbgrp`
- `rushing/pff_rushing_qbgrp_step0_AWS.R` → `rushing_qbgrp`
- `pass_rush/pff_pass_rush_qbgrp_step0_AWS.R` → `full_pass_rush_qbgrp`
- `pass_block/pff_pass_block_AWS.R` and `run_block/pff_run_block_AWS.R` → the OL game frames and season percentiles
- `secondary/pff_secondary_cache_step0_AWS.R` → the coverage cache
- receiving: `pff_receiving_man_zone_exploration_AWS.R`, then `receiving_stats_xpass.R`, then `receiving_stats_xtd.R`, then `receiving_stats_build_AWS.R` → `receiving_func_base`

`util/cache_frames.R` can save these slow frames to disk and load them back.

**4. New England opponent schedule files.** For NE's 14 opponents, each `new_england_opp_*_schedule.R` file:
- builds the unit's raw season currency (c1);
- prints a ledger of each 2026 roster, applies Andy's hand deltas and fills empty seats with phantoms, giving the 2026 rotation;
- compares the 2026 slate with what NE actually faced in 2025.

`pass_block/new_england_opp_ol_schedule.R` is the OL canon file, holding Andy's hand starter and backup tables. Run defense must come before rushing, because the rushing composite section needs `team_band_2026` (`util/pipeline_status.R:42`).

**5. Currency-three files.** Each `league_*_evaluating_currency_three.R` builds c3, the opponent-adjusted percentile: `rd_c3_pctl`, `ru_c3_pctl`, `prush_c3_pctl`, `pblk_c3_pctl`, `rblk_c3_pctl`, `rec_c3_pctl` and `cov_c3_pctl`.

**6. League files.** The `league_opp_*_schedule.R` files repeat the NE build for all 32 teams (for OL: `league_opp_pass_blocking_schedule.R` and `league_opp_run_blocking_schedule.R`). Walls check that NE's numbers come out identical. Outputs include `cur_lg`, `cur_rd_lg`, `cov_pctl_sec_lg`, `rot_2026_rd_lg`, `rot_2026_rb_lg`, `slots_full`, `league_prush`, `league_rundef`, `league_rush`, `league_cov` and `league_sys_both`. `league_pass_block_final_evaluation.R` adds `rot26_full_ol` and `slate_view_pb`.

**7. Availability files.** Each `league_*_availability.R` prices every 2026 rotation member by expected missed games:
- priced value = avail × healthy value + (1 − avail) × replacement level
- avail comes from the player's own job history, else the rookie line, else the backup line

They produce three kinds of frame:
- **Sweep frames** (`sweep_ra`, `sweep_pa`, `sweep_ru`, `sweep_unit_cv`, `sweep_rc`, `sweep_avail_pb`, `sweep_avail_rb`): each team's faced-2025, healthy-2026 and priced-2026 slate.
- **Member frames** (`members_ra`, `members_pa`, `memb_lg_ru`, `members_cv`, `members_rc`, `slot_value_26_pb_build`, `slot_value_26_rb_build`): each team's 2026 rotation, before and after pricing.
- **2025 job frames** (`stint25_ra`, `stint25_pa`, `starters_all_ru`, `starters_all_av`, `starters_all_rb`, `corps25_rc`).

`util/pipeline_status.R` reports which of these are loaded and names the next file to source.

**8. Team viewer.** `pff_stats/evaluation/any_team_evaluation.R` only reads. `team_report(code)` (`:2227`) runs these five blocks in order:
1. `team_slate`: opponents' units, faced 2025 vs priced 2026.
2. `team_sched`: the opponent set.
3. `team_lastyear`: 2025 job holders, where they are now, and retention.
4. `team_yoy`: own roster, 2025 real vs 2026 projected.
5. `team_own`: own players before and after pricing, then the unit aggregate.

A missing frame skips one unit, never the whole report. `ne_players_evaluation.R` is the older NE-only version of `team_own`.

| Unit | Schedule / league | c3 | Availability outputs team_report reads |
|---|---|---|---|
| Run defense | `new_england_opp_run_defense_schedule.R`, `league_opp_run_defense_schedule.R` | `rd_c3_pctl` | `sweep_ra`, `members_ra`, `stint25_ra` (+ `cur_rd_lg`, `slate_rd_lg`) |
| Rushing | `new_england_opp_rushing_schedule.R`, `league_opp_rushing_schedule.R` | `ru_c3_pctl` | `sweep_ru`, `memb_lg_ru`, `starters_all_ru` (+ `rush_season_pctl_sos`) |
| Pass rush | `new_england_opp_pass_rush_schedule.R`, `league_opp_pass_rush_schedule.R` | `prush_c3_pctl` | `sweep_pa`, `members_pa`, `stint25_pa` (+ `cur_lg`) |
| Pass block | `new_england_opp_ol_schedule.R`, `league_opp_pass_blocking_schedule.R` | `pblk_c3_pctl` | `sweep_avail_pb`, `slot_value_26_pb_build`, `starters_all_av` (+ `ol_season_pctl`) |
| Run block | same OL canon, `league_opp_run_blocking_schedule.R` | `rblk_c3_pctl` | `sweep_avail_rb`, `slot_value_26_rb_build`, `starters_all_rb` |
| Receiving | `new_england_opp_receiving_schedule.R`, `league_opp_receiving_schedule.R` | `rec_c3_pctl` | `sweep_rc`, `members_rc`, `corps25_rc` (+ `rec_season_pctl_sos`) |
| Secondary | `league_opp_secondary_schedule.R` | `cov_c3_pctl` | `sweep_unit_cv`, `members_cv` (+ `cov_pctl_sec_lg`, `sec_qbgrp_lg`) |

## Terms

- **'--'**: how a missing value prints in the viewer tables. It means one of three things: the unit has no such currency (a canon boundary), a pass-block box-score hole, or an optional frame is not in session.
- **_f / _p / _av**: `_f` is a player's healthy (before) value; `_p` and `_av` are the injury-priced (after) value, which is the same formula spelled differently by different units.
- **adv_position / det_position**: PFF's detailed per-game position, e.g. LCB, SCB or WLB for defenders and LT/LG/C/RG/RT for linemen.
- **align / rte / tgt cluster**: receiver season labels for alignment (WWR, WSWR, SWR, ITE, STE, RB), route depth (DT, MT, ST, BT, SMT, RB, LR) and target depth (DT, MT, ST, BT, ML, SMT, RB, G, LT).
- **all-snaps view vs TPS**: metrics computed over every pass-rush or pass-block snap, versus only true-pass-set snaps.
- **arm (rookie / vet)**: in the secondary priors, "rookie" when a player's entry year equals the season, otherwise "vet".
- **Athena / S3**: AWS's SQL query service and file store, where the raw PFF tables and the model files live.
- **avail**: the expected share (0-1) of a 17-game regular season a 2026 rotation member plays.
- **avail_job / avail_all / avail_s**: `avail_s` is one season's games played ÷ 17; `avail_job` averages it over seasons he held a job; `avail_all` averages it over every season he played.
- **avail_src**: which rule set a member's avail: `starter_history`, `rookie_line` or `backup_line`.
- **backup gms**: the sum of (1 − avail) over a slate's seats and games, i.e. the expected seat-games that backups fill.
- **backup line / rookie line**: the observed first-season games-played rate of first-time job holders who were already in the league (backup) or in their entry year (rookie).
- **band**: the role group a player is ranked within. Receiving: WWR/WSWR/SWR/ITE/STE/RB. Secondary: CB/SCB/S/LB. Pass rush: ED/DI. Run defense: DI/ED/LB/S. OL c3 files: the modal slot.
- **band weights / unit composite**: each secondary band's share of league split snaps, and the band-weighted team average that uses those weights.
- **blend2 / w25**: `blend2(x25, x24, w)` gives w × 2025 + (1 − w) × 2024, falling back to whichever year exists; `w25` is usually min(2025 qualifying games ÷ 10, 1) (receiving usage divides by 18).
- **boards**: printed player lists in the availability files, such as fragile starters, career backups, rookies and team exposure.
- **c1 (raw)**: a unit's plain season percentile within band (or slot) and season, not adjusted for opponents.
- **c3 (adjusted, same-slate)**: each game is ranked against same-band players who faced the same opponent that season; the season median of those ranks is then ranked within band and season.
- **canon**: Andy's stamped files and frames, treated as the source of truth; other files are checked against them.
- **canon hole**: a non-rookie OL starter with no 2025 or 2024 box-score percentile, left NA.
- **cluster_join**: the slim receiver label table the build joins onto every game row.
- **code bridge**: the map between PFF team spellings (ARZ, BLT, CLV, HST) and nflverse roster spellings (ARI/AZ, BAL, CLE, HOU), built from the data.
- **cohort / comps**: the players or player-seasons that pass a comparison's filters (receiving, rushing), or the most similar defender-seasons (coverage).
- **combined_grade_epa_summary**: one row per team-week with PFF grade, EPA, the starting QB, `qbgrp_ssn` and `def_ssn`.
- **combined_pbp**: the one-row-per-play table of nflfastR play-by-play plus participation data, both model families' predictions and the two keys.
- **committee / corps**: a team's group of players valued together: two backs in rushing, 8 pass catchers in receiving (`N_CORPS`), 9 seats per split in the secondary.
- **common-opponent percentile**: a focal game ranked only against comparison games played against the same defense (receiving) or the same offense (coverage).
- **comparison lookups / lens / tolerance**: the five QB-and-defense similarity views (blitz, depth, less, pa, pressure). Each returns teams within a weighted distance `tolerance` of the one you pass.
- **conf / battle**: a tag on OL hand-table starters: "high", or "battle" for an open camp competition.
- **cosine similarity**: a 0-1 score of how alike two weighted profiles are in proportions, ignoring volume.
- **currency**: a fixed, gated way of turning a player's season into a 0-1 percentile; currencies are never mixed in one average.
- **d / delta**: a difference column: 2026 minus 2025 on slate and year tables, after minus before on own-side tables.
- **def_ssn**: defense team code + season, no dash (e.g. SEA2025).
- **deltas (hand table)**: Andy's typed add, drop or band-override rows that change an automatic rotation. They are empty in every unit today, which means full sign-off.
- **diet / deployment**: in coverage, the share of a defender's targets that went to each kind of receiver, and the share of his snaps in man, zone and slot.
- **division x2**: division opponents appear twice in `sched_2026`, so they count twice in NE slate means.
- **entry-year prior**: the median percentile of players in their first NFL season, by band or slot. It values rookies, phantoms and anyone without a percentile.
- **exp miss**: expected missed games, 17 × (1 − avail).
- **facet / facet law**: one coverage stat, and the secondary table saying whether grade, supp and pbu are seated, noise, context or dead per split and band.
- **faced 2025**: the weighted quality of the players a team actually played against in 2025, playoffs included.
- **fill law / fill_share**: on the faced side, a player with no season percentile is priced at the band prior instead of dropped; `fill_share` is the share priced that way.
- **final_position**: the per-game coverage class (MLB, LB, S, SCB, CB, DL) derived from `adv_position`.
- **final_position_group**: the receiving build's position label: BACK, TE, WR or OTHER.
- **firewall**: the rule that c3 never feeds a c1 slate artifact; adjusted results are labeled as their own family.
- **flex_fill / modal rung / phantom rung**: the three OL ladder steps that fill all 160 league team-slots.
- **focal**: the team or player a row is about: in league files the team whose slate is measured, in comparison tools the player being evaluated.
- **G / X / gate**: X is the per-game floor for a game to qualify; G is the qualifying games a season needs (6 in every unit).
- **gap / zone**: run-scheme splits for run blocking.
- **gap cluster / situation cluster / gap_z / rank_grp**: rushing-tower labels. `rank_grp` is a per-game role letter A/B/C; the clusters are k-means groups of run location or game situation within a letter; `gap_z` is a gap-vs-zone z-score.
- **gt**: the R table package used for the printed colored tables.
- **gsis_id**: nflverse's player id; the id builds attach one to every PFF `player_id`.
- **healthy vs priced**: the 2026 value with everyone playing 17 games, versus after availability pricing.
- **healthy jump / real jump / swing**: healthy minus faced, priced minus faced, and their difference.
- **in_season(w)**: TRUE for weeks 18 and earlier and PFF playoff weeks 28 and later (28 wild card, 32 Super Bowl).
- **In / Out**: in the matchup engines, games against a comparable defense versus all other games of the comparable offenses.
- **interp_share / prior_share**: the share of a 2026 value resting on prior-valued members.
- **job / job season**: holding a rotation spot that season; in pass rush and run defense, a gated (6+ game) season.
- **ledger**: the printed list of every 2026 opponent roster player with band, status and whether he is proposed.
- **luck card**: the secondary table comparing chance wobble in PBU and completion rates with real between-player spread.
- **man_zone_grp_cluster**: the hand-coded receiver class, e.g. WR_SHORT, WR_DEEP, TE_LT, HB_DEEP or OTH.
- **members / member frame**: an availability layer's list of every team's 2026 rotation members with healthy, availability and priced values.
- **modal band / modal starter**: the band where a player logged the most snaps that season; the player with the most snaps at a team's OL slot that season.
- **percent_rank_avg**: canon percentile (rank − 1)/(n − 1) with averaged ties, 0.5 when only one value exists. Schedule files carry a rank/n fallback that fires only if canon is absent.
- **percentile (pctl)**: a 0-1 rank within a group; higher is a better player; tables show it × 100.
- **phantom**: a placeholder seat with no real player, valued at the band prior and priced at the backup line.
- **pbp_ / part_ / fastr_**: prediction families from play-by-play-only models, participation-data models, and nflfastR itself.
- **pool / singleton / vsopp_rank / med_vsopp**: a c3 pool is all same-band games against one opponent; a lone game scores 0.5; `vsopp_rank` is one game's rank in its pool; `med_vsopp` is its season median.
- **pos_rank / team_rank**: a player's usage rank in that game within his position group, or within his team.
- **prior law / promotion class / rung**: the secondary's prior prices, the past first-time committee members whose results set them, and the fallback steps used when a sample is thin.
- **prior_used**: TRUE when a member's value came from a prior.
- **proposed**: TRUE when a ledger player is automatically in his team's top N.
- **PRP / win rate**: PFF pass-rush productivity, and the share of rush snaps where the rusher beat his blocker.
- **qbgrp_ssn**: offense team + starting QB last name + "-" + season (e.g. NEMaye-2025).
- **qual_g / qualifying game**: a game that clears the unit's X floor, and the count of them in a season.
- **qs**: snaps summed over a stint: TPS rush snaps in pass rush, run snaps in run defense.
- **receipt / wall**: a receipt is a printed check; a wall stops the run when a rebuild differs from canon by more than 1e-8 or an object or column is missing.
- **replacement level (repl)**: the snap-, route- or attempt-weighted 2025 value of backups who played when a regular sat, per band or slot and per currency.
- **roster route**: team membership and names taken from nflreadr rosters rather than PFF frames.
- **rotation / rot_2026**: the final projected 2026 members per team and band, exactly N deep.
- **slate**: the set of opponents a team plays, and the average strength of their units.
- **slot**: an OL position, LT/LG/C/RG/RT (`ol_pos_levels`).
- **slot usage / slot curve**: 2025 snaps at each rotation rank, which members and phantoms without their own usage borrow as weight.
- **slot_pool_mismatch (*)**: an OL player whose grades were earned at a different slot from the one he is listed at.
- **split / lens**: the scheme view a value belongs to (man or zone, gap or zone, or overall).
- **status**: a ledger tag such as rookie, no_pff_id, has_2025_pctl, data_2024_only, usage_no_pctl or no-history.
- **stint**: a player's season with one team (pass block: one team and QB group).
- **supp**: the secondary suppression percentile, from negative completion rate allowed.
- **sweep**: the slate calculation run for all 32 teams; in the matchup engines, stepping tolerances until profiles clear the floor. In coverage code, `sweep()` is the base-R per-column multiplier.
- **tgt_share / onfield_perc / tgt_per_route**: a receiver's share of team targets, routes ÷ team pass snaps, and targets ÷ routes.
- **TPS**: true pass set, PFF's subset of clear pass-set snaps.
- **tribble**: a small table typed row by row in the code.
- **unscored_share**: the older faced-side share of snaps or routes by players with no percentile, left out of the mean.
- **usage_ord / usage_w / uw**: the larger of 2025 and 2024 usage, used to rank members, and the member's weight in team averages.
- **V / V_source**: an OL league slot value, and a label saying which fallback step produced it.
- **wk25**: in `pipeline_status()`, the last 2025 week a frame carries (32 = complete).
- **xpass_percentile / xtd_percentile / z_score_percentile**: receiver 0-100 ranks of how pass-expected his targets were, how TD-likely they were within his TD group, and how man-favoring he was within his man/zone group.

---

## The team viewer and shared pieces

### pff_stats/shared_ne_2026_constants.R
A 55-line verbatim copy of `new_england_opp_ol_schedule.R` lines 39-45 and 121-126. The OL file remains the source of truth (`:8-18`). The copy exists because the OL file stops on missing OL frames before reaching those lines. Source it after data_build and before any step-0 (`:20-21`).
- `ol_pos_levels` (`:27`): the five OL slots in display order.
- `opp_2026_teams` (`:28-29`): NE's 14 distinct 2026 opponents.
- `sched_2026` (`:30-31`): NE's 17 games. BUF, NYJ and MIA appear twice.
- `in_season(w)` (`:33`): regular season plus playoffs.
- `blend2(x25, x24, w)` (`:36-41`): NA if both inputs are missing, otherwise whichever exists, otherwise w × 2025 + (1 − w) × 2024. The member builds pass w = min(qualifying 2025 games/10, 1) (e.g. `league_pass_rush_availability.R:429`, `league_receiving_availability.R:729`, `league_secondary_availability.R:552`). Receiving's usage blend divides by 18 (`league_receiving_availability.R:730, 761`). An NA result means "no evidence either year"; callers fill it with the band prior and set a flag, and those flags feed the interp % columns.
- `percent_rank_avg(x)` (`:52-56`): the canon percentile. Defining it first means the rank/n fallback in the schedule files (`:43-51`) never fires.

### util/cache_frames.R
Dated snapshots of slow frames. Re-running step-0 is still the canon path (`:10-13`).
- `CACHE_DIR` (`:18`): `cache/` under the working directory.
- `cache_frames(...)` (`:20-35`): saves each named object to `cache/<name>.rds`, overwriting any earlier file.
- `uncache_frames(...)` (`:37-52`): loads each file back into the global session and prints its age. It overwrites any object of the same name.
- `cache_ls()` (`:54-65`): lists the cached files.

### util/pipeline_status.R
- `PIPELINE_MAP` (`:16-112`): a tribble with columns unit, stage (base/ids/constants, step0, schedule, c3, league, availability, functions) and object, plus the file that creates each object. Notes in the file: rushing needs the run-defense schedule first; run block shares the OL schedule file; the secondary step-0 writes a cache.
- `.shape_of(nm)` (`:114-120`): the object's shape.
- `.wk25_of(nm)` (`:126-133`): the last 2025 week in the frame. 32 = complete; 28 = a pull that stopped at wild-card weekend (`:122-125`).
- `pipeline_status(units = NULL)` (`:135-175`) prints four things:
  1. LOADED/MISSING, shape, wk25 and file for each object;
  2. a STALE line for loaded frames that stop before week 32;
  3. a per-unit verdict, either COMPLETE or "stops at <stage> -> source(<file>)";
  4. missing shared objects, pointing to REBUILD_RUNBOOK.md.
  
  LOADED only means an object with that name exists. wk25 is the only freshness check.

### pff_stats/evaluation/ne_players_evaluation.R
The original NE-only own-units table. It is a top-level script with no functions and does no math beyond after − before and 17 × (1 − avail) (`:27-30`). Colors are own-side: red means NE loses quality (`:20-25`).
- **Gates stop the script, not skip a unit** (`:68-116`):
  - all seven member frames plus `ol_pos_levels` and `REC_BANDS` must exist (`:68-77`);
  - required columns must be present (`:79-106`);
  - unit-identity checks on bands, splits and slots (`:110-116`).
  
  A placeholder gt table prints first (`:120-126`).
- **Churn board** (`:137-174`): 2025 vs 2026 nflreadr NE rosters matched on exact names, grouped by `grp_neo()` (`:140-147`). `new26_keys` (`:173`) drives the "*" star.
- **Extraction** (`:185-307`): filters each member frame to NE (`:185-191`) and stops on zero rows (`:210-212`). Rows are standardized to `unit, player, lens, role, avail, bef, aft, abef, aaft, uw` (the mapping matches team_own). Run block's adjusted values are always the blended `V_c3/V_c3_av` (`:257-268`). `ne_own_long` (`:291-305`) adds `emiss`, `d`, `ad` and the star.
- **Receipts** (`:312-330`): unmatched names, and NA and avail-bounds stops. Pass-block raw NAs are allowed.
- `sum_neo` (`:339-371`): console lines. `gt_neo` (`:378-419`): the player table. There is no aggregate table.

### pff_stats/evaluation/any_team_evaluation.R

#### What the file is (`:1-96`)
A view layer for any team code. It filters, joins and prints, and never changes an upstream frame. Every helper ends in `_tr`. Source it after all seven availability layers (`:87-89`).

There are two directions (`:53-60`):
- **Opponent side** (`team_slate`, `team_sched`): higher = harder for your team, and red = harder.
- **Own side** (`team_own`, `team_yoy`): higher = better for you, and red = you lose quality.

#### Shared vocabulary for every table
- **Values** are 0-1 percentiles, shown × 100. The OL and rushing priced values arrive rounded to 4 decimals; the pass-block sweep arrives rounded to 3.
- **Raw** is c1: receiving man/zone grade, rushing `grun`, pass block TPS grade, run block gap/zone, pass rush TPS grade, secondary coverage grade, run defense grade.
- **Adjusted** is c3 (`league_pass_rush_evaluating_currency_three.R:18-22`).
- **after** = avail × before + (1 − avail) × repl. The same formula appears in every unit: `league_run_defense_availability.R:483-485`, `league_receiving_availability.R:778-785`, `league_secondary_availability.R:573-576`, `league_pass_block_availability.R:356-359`, `league_run_block_availability.R:569-575`, `league_rushing_availability.R:411-416`.
- **avail** comes from a ladder recorded as `avail_src` (e.g. `league_pass_rush_availability.R:479-489`).
- **"*"** = new to the team in 2026. **"--"** = NA.

#### Frames it reads

| Frame | One row per | Used by |
|---|---|---|
| sweep_ra / sweep_pa / sweep_ru | focal | team_slate |
| sweep_unit_cv / sweep_rc | focal × split | team_slate |
| sweep_avail_pb / sweep_avail_rb | focal × slot | team_slate |
| slate_rd_lg | focal | team_slate (run-D interp %) |
| the seven member frames | 2026 member | team_own, team_yoy 2026 |
| corps25_rc, starters_all_ru/av/rb, stint25_pa/ra | 2025 job holder | team_yoy 2025, team_lastyear |
| cur_lg, cur_rd_lg, cov_pctl_sec_lg, sec_qbgrp_lg, rush_season_pctl_sos, rec_season_pctl_sos, rec_band_season, ol_season_pctl | player-season | team_yoy, team_lastyear |
| the seven c3 frames | player-season (× split) | team_yoy, team_lastyear |
| xw_rd_lg, xw_lg, rec_id_bridge, combined_ids | player | names in team_lastyear |

#### Skip messages
- `gate_tr` prints "missing frame: X" or "X is missing columns"; that unit is skipped.
- `have_tr` prints "session is missing" for non-frame objects.
- "[Unit] no CODE rows" means the frame has no rows for that team spelling.
- `frame_pick_tr` prints "riding X" or "none of ... cell stays '--'".

#### Team codes
- `norm_code_tr(code)` (`:198-237`):
  1. upper-cases the input;
  2. translates it to canon with `pff_code_tr`;
  3. checks it against the codes in the canon frames present.
  
  It stops if there are no frames or the code is unknown, and prints a one-time note (`note_done_tr`).
- `check_pipeline_tr()` (`:240-246`): stops if none of `sweep_ra`, `members_ra` or `members_pa` exists. It is called only in the tail.
- `code_bridge_tr()` (`:256-291`):
  1. joins 2026 nflreadr roster names to canon member names;
  2. counts (canon, roster) code pairs;
  3. keeps the most common pair per canon code.
  
  The result is cached for the session. It is empty if nflreadr fails.
- `ros_code_tr` / `pff_code_tr` (`:294-304`): translate one way or the other.
- `code_in_tr(vals, cd)` (`:306-323`): returns the spelling a given frame uses. The Arizona family tries ARZ, ARI and AZ. It is used on 2026 member frames; the 2025 side compares canon codes directly.
- `where_now_tr(player_names, home_code)` (`:328-346`, with the function at `:338` and `status26_tr` just above it): returns "no 2026 roster", "still here" or "-> TEAM" (roster spelling). Matching is on cleaned name only.

#### Small helpers (`:104-192`)
- `cl_tr`: name key (upper-case, letters only).
- `pick1_tr`, `col_or_na_tr`, `m_tr`, `wm_tr` (weighted mean over positive weights), `wfill_tr`.
- `frame_pick_tr` (`:142-153`): picks the first candidate frame that exists.
- `vpick_tr`: vector picker.
- `wpick_tr` (`:158-163`): the weight column, taken as the first found in this order: tps, run_snaps, pb_snaps, atts, q_snaps, snaps, qs, sn, q_atts, g_snaps, routes, targets, usage_w. If none is found it returns NULL, which means a plain mean.
- `have_tr`, `gate_tr`.

#### team_slate(code) (`:360-656`)
How hard this team's 2026 schedule is, unit by unit, compared with 2025. It prints a console board, a detail gt, a simple gt and a one-line summary, and returns `list(board, gt, gt_simple)` invisibly.

The sweeps are already unit-level. Upstream, each is a plain mean over the focal's 2026 schedule of each opponent's usage-weighted value; the faced side is the 2025 games the focal actually played (e.g. `league_run_defense_availability.R:714-728`, `league_pass_rush_availability.R:723-735`).

| Row (side) | Frame | faced25 raw | healthy26 | priced26 | faced25 adj | priced26 adj | fill % | interp % |
|---|---|---|---|---|---|---|---|---|
| Run defense (O) | sweep_ra | grade_25 | grade_26 | grade_26p | c3_25f | c3_26p | fill_share_25 | slate_rd_lg interp_share |
| Pass rush (O) | sweep_pa | g25 | v26 | v26p | a25 | v26ap | fill_share | prior_share |
| Secondary man/zone (O) | sweep_unit_cv (`:400-411`) | unit_25 | healthy_u | priced_u | NA | priced_a_u | fill_share_25 | interp_raw |
| Receiving man/zone (D) | sweep_rc | faced25 | v26 | v26p | a25 | v26ap | unscored_share | prior_share |
| Rushing (D) | sweep_ru (`:430-442`) | faced_grun | healthy_grun | priced_grun | adj_25_m | priced_adj | bkup_share / fill_share / bkup25 | NA |
| Pass block (D) | sweep_avail_pb | mean faced_r | mean paper_r | mean av_r | mean faced_a | mean av_a | NA | NA |
| Run block gap/zone (D) | sweep_avail_rb | mean faced_gap/zone | mean healthy_* | mean priced_* | mean faced_adj | mean priced_adj | NA | NA |

- **How it works:** each row passes through `gate_tr`. Single-row units need exactly one focal row. OL slots are averaged flat. `d_priced` and `d_adj` are computed at `:492-494`.
- **Output layout:** the console board is at `:496-509`. The detail gt (`:517-583`) groups rows as "your OFFENSE / your DEFENSE on the field", with faced '25 raw/adj, projected '26 (priced) raw/adj, Δ, and honesty fill %/interp %. The simple gt (`:593-648`) shows raw only, with an "ALL UNITS (avg of rows)" line; the summary line is at `:649-653`.
- **Trips people up:**
  - A unit without exactly one focal row is dropped silently.
  - The secondary's adjusted faced value is hard-coded NA.
  - `sweep_ru` carries `backup_gms`, none of the three names the rushing fill cell looks for, so that cell prints "--".
  - Two-lens units count twice in ALL UNITS.
  - Run block's adjusted value repeats on both of its rows.

#### team_sched(code) (`:696-800`), with `to_canon_sched_tr` (`:677-682`) and `load_sched_tr` (`:685-694`)
The opponent set in each year. It uses the 2025 schedule and, for 2026, `sch26_rd` or a fresh load. A schedule is accepted only if it has more than 200 rows and the needed columns. Weeks are capped at 18. Codes go through `to_pff_rd` (`league_opp_run_defense_schedule.R:1032-1036`) or a four-code fallback. Output: back / new / gone lists, and a gt showing "x2", "x1" or "--" per year. Returns `list(sets, gt)`.

#### team_own(code) (`:810-1252`)
Own 2026 players before and after pricing, then one aggregate row per unit. Returns `list(data, gt, gt_agg)` invisibly.
1. **Churn** (`:820-843`): roster-route IN and OUT lists, with "from" and "now"; the arrival star is at `:846-847`.
2. **Extraction**, one gate per unit, with rows filtered through `code_in_tr`:

| Unit | Frame | Rows | before / after | adj before / after | weight |
|---|---|---|---|---|---|
| Receiving (`:855-881`) | members_rc | man, zone | mg_f/zg_f → mg_p/zg_p | c3mg_f/c3zg_f → c3mg_p/c3zg_p | usage_w |
| Secondary (`:882-903`) | members_cv | per split seat | grade_f → grade_p | c3_f → adj_p | usage_w |
| Pass block (`:904-920`) | slot_value_26_pb_build | per slot | gf_raw → gf_raw_av | gf_adj → gf_adj_av | flat |
| Run block (`:921-956`) | slot_value_26_rb_build | gap, zone per slot | V_gap/V_zone → _av | V_c3_gap/zone → _av | flat |
| Rushing (`:961-978`) | memb_lg_ru | 1 | gf → gf_av | V_c3 → V_c3_av | uw |
| Pass rush (`:979-996`) | members_pa | 1 | gf → gf_p | c3_f → c3_p | uw |
| Run defense (`:997-1014`) | members_ra | 1 | grade_f → grade_p | c3_f → c3_p | usage_w |

   If the run block scheme c3 columns are missing, both rows fall back to the blended values and a note prints (`:934-942`, `:957-960`).
3. **Player gt** (`own_tr` `:1023-1037`, `gt_tr` `:1086-1124`): availability, Raw and Adjusted before/after/Δ on a fixed ±30-point red-to-blue scale. A warning prints for any avail outside 0-1 (`:1043-1050`).
4. **Console lines** (`sum_tr`, `:1053-1084`).
5. **Aggregate** (`agg_row_tr`, `:1132-1143`): a weighted mean per unit and lens (plain mean when the unit has no weight). `emiss` is the sum of 17 × (1 − avail). Rows are at `:1144-1178`, and ALL UNITS, the simple mean of the rows, at `:1182-1190`. The headline prints at `:1205-1209`, the gt at `:1213-1250`. Receiving and run block players count in two rows of the `emiss` sum.

#### team_yoy(code) (`:1271-1830`)
Own roster, 2025 real vs 2026 projected, raw and adjusted. Each unit fills r25, a25, r26 and a26. Returns `list(data, gt)`.

| Row | 2025 raw | 2025 adj | 2026 raw | 2026 adj |
|---|---|---|---|---|
| Receiving (`:1286-1349`) | rec_season_pctl_sos split pctl for corps25_rc, wpick_tr weight | rec_c3_pctl, plain mean | mg_p/zg_p | c3mg_p/c3zg_p |
| Rushing (`:1351-1390`) | starters_all_ru 2025 × grun_pctl, weighted by q_atts | ru_c3_pctl | gf_av | V_c3_av |
| Pass block (`:1392-1435`) | starters_all_av modal starters × ol_season_pctl tps_grade, flat | pblk_c3_pctl | gf_raw_av | gf_adj_av |
| Run block (`:1437-1562`) | first found of rblk_season_pctl_sos / rb_season_pctl_sos / ol_season_pctl | rblk_c3_pctl gap/zone | V_gap_av/V_zone_av | V_c3_gap_av/V_c3_zone_av |
| Pass rush (`:1564-1604`) | stint25_pa × cur_lg g_pctl, weighted by qs | prush_c3_pctl | gf_p | c3_p |
| Secondary (`:1606-1669`) | cov_pctl_sec_lg per split for sec_qbgrp_lg team players, weighted by q_snaps | cov_c3_pctl | grade_p | adj_p |
| Run defense (`:1671-1712`) | stint25_ra × cur_rd_lg, weighted by q_snaps | rd_c3_pctl | grade_p | c3_p |

- **Rushing 2025:** every back on `starters_all_ru` for that team-season (every back with a played week, ranked by attempts). The rows are not filtered to `has_job`.
- **Receiving 2025 raw:** `rec_season_pctl_sos` carries `man_routes`/`zone_routes`, which are not on the `wpick_tr` list, so the value is weighted only if `routes` or `targets` is also present.
- **Run block 2025 raw:** the two `*_season_pctl_sos` names are built nowhere, and `ol_season_pctl` has no season column (`new_england_opp_ol_schedule.R:68-101`). So this cell prints the one expected "--" (`:1488-1490`, `:1809-1811`). Adjusted 2025 drops starters without a scheme value (`:1471-1480`); the subtitle note comes from `rb_yoy_note_tr` (`:1554-1562`).
- **Output:** the console table is at `:1734-1744`, one-number lines at `:1754-1770`, and the gt at `:1775-1828`.
- **Trips people up:** the printed note at `:1749-1752` says adjusted 2025 exists only for receiving and run block, but the code fills it for every unit whose c3 frame is present. The 2026 side equals team_own's after aggregate.

#### team_lastyear(code) (`:1901-2220`), with name helpers (`:1841-1899`)
Last year's job holders per unit, their 2025 values, where each is now, and retention. Names come through `ros25_nm_tr`, `nm_pool_tr` (the id bridges present) and `nm_via_bridge_tr`. Returns `list(data, gt)`.

| Unit | Who is listed | value | 2nd value |
|---|---|---|---|
| Run defense (`:1911-1933`) | stint25_ra × cur_rd_lg | grade_pctl | stop_pctl |
| Pass rush (`:1934-1955`) | stint25_pa × cur_lg | g_pctl | -- |
| Secondary (`:1956-1982`) | cov_pctl_sec_lg per split | grade_pctl | supp_pctl |
| Receiving (`:1983-2024`) | corps25_rc, man and zone | adjusted grade pctl | adjusted YPRR pctl |
| Rushing (`:2025-2051`) | starters_all_ru 2025, by rb_rank | grun_pctl | mtf_pctl |
| Pass block (`:2052-2077`) | starters_all_av | tps_grade | -- |
| Run block (`:2078-2134`) | starters_all_rb | gap_c3_pctl | zone_c3_pctl |

A traded player appears under each team (`:2137-2138`). Retention per unit is at `:2143-2168`, and the gt board at `:2173-2218`.

#### team_report(code) (`:2227-2234`)
Resolves the code once, then runs slate, sched, lastyear, yoy and own, and returns team_own's list. To keep a table, assign the result (e.g. `x <- team_own(code)`; `x$gt_agg`).

#### File tail (`:2242-2249`)
Sourcing the file runs `check_pipeline_tr()` and then example calls on one team, ending with `team_yoy`. Several blocks print twice. The team code is an example.

---

## Coverage chain (pff_stats/secondary)

Five files build the opposing-secondary schedule, first for NE and then for all 32 teams. Everything is band by band (CB, SCB, S, LB) and man and zone separately; the two are never merged. Higher = better defender = harder for the offense.

Run order: shared constants → `pff_secondary_cache_step0_AWS.R` → `league_opp_secondary_schedule.R` (whose first ~1,611 lines are the NE build, nearly identical to `new_england_opp_secondary_schedule.R`) → `league_secondary_evaluating_currency_three.R` → `league_secondary_availability.R`.

team_report reads four things (`util/pipeline_status.R:97-108` lists their producers):
- `sweep_unit_cv` (`any_team_evaluation.R:400-411`)
- `members_cv` (`:882-901`, `:1648-1662`)
- `cov_pctl_sec_lg` + `sec_qbgrp_lg` (`:1611-1629`)
- `cov_c3_pctl` (`:1630-1646`)

**Shared inputs:**
- From the shared constants (`:28-56`): `opp_2026_teams`, `sched_2026`, `in_season`, `blend2`, `percent_rank_avg`.
- From data_build: `pff_team_lookup`, `combined_ids_defense`, `combined_grade_epa_summary`.

The schedule files carry a rank/n fallback (NE `:108-112`, league `:119-123`) that never fires once canon is defined. The c3 file's fallback (`:58-64`) is already canon.

### pff_stats/secondary/pff_secondary_cache_step0_AWS.R
This file is the old console "ritual" rebuilt as a file. The schedule files stop without its cache (NE `:171-173`, league `:182-184`).
- `run_athena_query(sql, max_wait = 120)` (`:43-82`): defined only if absent.
- **Pulls** (`:90-121`):
  - `cov_raw` (target 69,250 rows)
  - `by_game_raw` (for detailed position)
  - `coverage_scheme` (the man/zone split stats), saved as `coverage_scheme_cache_mz.rds`
  
  The schedule files prefer a session copy of `coverage_scheme`. A stale one, like the week-28 copy noted at `:105-110`, would win silently. The 2025 playoff weeks print; you want 28, 29, 30, 32.
- **Column walls** (`:126-159`): 10 required columns; the position column is picked by name; stops on duplicate keys.
- **Build** (`:166-202`): position join with no row change. `final_position` (`:175-183`) is decided in this order: MLB → MLB; ends in LB → LB; FS/SS → S; SCB → SCB; contains CB → CB; DL codes → DL; else NA. SCB is tested before "contains CB", so slot corners stay SCB.
- `canon_fix(v, season)` (`:186-195`): ARI→ARZ, BAL→BLT, CLE→CLV, HOU→HST, LAC 2016→SD, LV ≤2019→OAK.
- No snap gate is applied: the band map must be built un-gated.
- **Receipts** (`:207-224`):
  - band counts (LB 24,279 / S 16,948 / CB 14,503 / SCB 5,686)
  - per-season coverage
  - 2025 playoff weeks
- **Write** (`:229-235`): `coverage_raw_build_cache_cov.rds` in `getwd()`. Run everything from the same working directory.

### pff_stats/secondary/new_england_opp_secondary_schedule.R
NE's 2026 opposing-secondary strength in two currencies, man and zone. Its `_mz` objects carry a `split` column.

**Terms used here:**
- **split**: man or zone. About 11% of coverage snaps are neither and are unused. One player can hold a man seat and a zone seat independently.
- **qbgrp_ssn / def_ssn**: the passing game faced, and the defense's team-season.
- **ONE SESSION LAW**: re-source the whole file after any edit.

**Setup** (`:98-165`):
- `needed_mz` (`:100-106`): required objects.
- Helpers: `resolve_col(prefs, pool, label, required)` (`:149-163`) takes the first exact column name from a preference list and otherwise prints fuzzy candidates. `safe_sd` returns NA when there are fewer than 2 values.

**Block 0 and 0a** (`:171-189`, `:198-223`):
- Reads the cache and left-joins `combined_grade_epa_summary` with the defender's `team_name` = `opp`, plus week and season. This attaches the passing game each defender faced.
- Prints unmatched rows (you want about 0) and stops on old team codes.
- Scheme table, in preference order: session object, then cache, then a fresh pull. Stops on duplicate keys.

**Block 0b, the split card** (`:233-310`):
- A **facet** is one coverage stat: grade, snaps, tgt, rec, pbu, yards, yac, int, qbr, adot, mt.
- `split_prefs_mz` (`:239-251`) lists candidate names with an "S" placeholder. `resolve_split_mz(facet, split)` (`:253-265`) swaps in "man" or "zone" and looks in the joined frame, then the scheme table.
- A coverage matrix prints. The run stops if a core facet (grade, snaps, tgt, rec, pbu) is missing.
- `CM` and `CZ` (`:305-306`) map each facet to its real column. Later code reads names only through them.

**Block 1, bands** (`:321-361`):
- **band4** per game row: SCB stays SCB; MLB/LB → LB; otherwise CB or S. Rows need a non-missing combined grade and more than 0 snaps. The result is `secondary_qbgrp_mz`, one row per player-week, walled unique.
- Receipt: NE offense weeks vs NE defense weeks (21 each).
- **modal band** (`modal_band_mz`): the band4 with the most combined snaps that season, built un-gated ("identity vs currency"). A tripwire counts multi-band player-seasons (header receipt 979).

**Block 1b** (`:369-390`): `split_setup_mz(C_flat, sp, data)` fills a missing optional facet with a dummy zero column, flags it FALSE, and turns it back to NA later.

**Block 2, season builder** `season_currency_mz(x_qual, g_min, split, data)` (`:398-434`):
- **In:** the game floor, the season floor, and the split.
- **Out:** one row per player-season with q_games, q_snaps, q_tgts, q_rec, the snap-weighted grade, the rates `pbu_rt`, `cmp_rt`, `ypt_alw`, `yac_alw`, `ypcs_alw`, `int_rt`, `sn_per_tgt`, `sn_per_rec`, target-weighted `qbr_alw`/`adot` and snap-weighted `mt_rt`.
- **How:**
  1. Keep games at the floor.
  2. Attach the modal band.
  3. Sum per player-season.
  4. Keep seasons with g_min or more games.
  5. Set dummy-backed values to NA.
- Rates divide by `pmax(denominator, 1)`.

**Block 3, constants and cards** (`:443-541`):
- `X_MAN = 6`, `X_ZONE = 13` (the game floors). `G_MAN = G_ZONE = 6` (the season floor). `N_BAND_MZ` = CB 3, SCB 1, S 3, LB 2 (`:443-445`), which is 9 seats per team per split.
- `yardstick_stamp_mz` (`:447-465`): pools, target p10 and spreads.
- `base_rt_mz` (`:467-484`): league rates; the man reference floor is 8 here. Receipts: man .606/.0977, zone .723/.0658.
- **Luck card** `luck_stamp_mz` (`:486-497`): the binomial standard error at `tgts_p10` compared with the between-player sd, labeled "FLOOR MET" or "NOISE". The LUXURY test comes after FLOOR MET, so it is never reached.
- `grade_deciles_mz` (`:509-524`).
- **Facet law** `facet_law_mz` (`:530-539`):
  - grade is seated for every band and split;
  - man supp is noise;
  - zone supp is seated for CB and S, context for LB, dead for SCB;
  - pbu is context.
  
  **supp** is the percentile of negative completion rate. The facet law is a label layer: every percentile is still computed, and the unit composite, figures and gt tables use grade only (see the ANSWER print, `:1299-1304`). Andy vetoes a seat by editing this table.

**Block 4, c1** `cov_season_pctl_mz` (`:551-585`): percentiles within split × band × season of grade, supp, pbu, int, snt (snaps per target), and flipped ypt/yac/ypcs/qbr/mt. Prints pools and NE's own room as calibration.

**Block 4b** (`:593-627`): correlation matrices (a metric at .85 or more with grade stays table-only), plus top-10 eyeball lists.

**Block 5, prior audit** (`:650-795`):
1. `def_xwalk` (`:650-654`) is walled one-to-one. `entry_years_mz` (`:656-661`) holds entry years.
2. `committee_hist_one_mz(sp)` (`:663-679`) rebuilds past committees: the top N by split snaps, no G filter.
3. `promo_mz` (`:685-701`) is the **promotion class**: committee members from 2018 on with a gated season and none in the previous two years, tagged with an **arm**.
4. `promo_med_mz` and `promo_pool_mz` give the medians; the "phantom center" prints.
5. `V2_LAW_MZ` (`:726-730`) holds the old prices.
6. The loop (`:734-778`) runs CB, SCB, S, LB in that order:
   - `rookie_grade` / `vet_grade`: the arm median if there are at least 10 cases (`N_ARM_MIN`), else the pooled median if at least 10, else rung 3;
   - `phantom_grade`: the pooled median, else rung 3;
   - rung 3 is the v2 price, and SCB borrows the same-split CB price;
   - `pr_supp` / `pr_pbu`: the pooled medians, else 0.5.
   
   This produces `cov_prior_law_mz`. The PRIOR LAW v3 table prints, along with v3 vs v2.

**Block 6, ledger base** (`:814-920`):
- `def_2026_cov_mz` (`:817-836`): 2026 rosters (CB/S/DB/LB variants) for the 14 opponents, with PFF ids, walled unique.
- `cov_usage_one_mz(sp)` (`:838-849`): snaps and games for 2024/25.
- `band_map_mz` (`:851-857`): the 2025 modal band, else 2024. Roster strings never assign a band.
- `ledger_base_one_mz(sp)` (`:859-883`): status, first match of rookie → no_pff_id → has_2025_pctl → data_2024_only → usage_no_pctl → no_split_history. Zero-split-usage vets are excluded from that split, so a phantom fills the seat.
- `ledger_mz` (`:892-901`): ranked by **usage_ord** (the larger of 2025 and 2024 split snaps); `proposed` = rank N or better.

**Block 7** (`:931-952`): THE RULING LEDGER per split, plus the calls needed per team.

**Block 8, deltas** (`:971-984`): `opp_cov_2026_deltas_mz`, columns team_name, roster_name, band, action (add/drop), note. It is empty. The gate (`:976-981`) accepts only the 14 opponents' exact roster names. A drop removes the player from both splits; an add enters both, and the N cap by usage still applies.

**Block 9, faced 2025** (`:997-1086`):
- `faced_games_one_mz(sp)` (`:997-1010`): NE-offense 2025 in-season games at the floor, carrying g25/s25/p25.
- Fill (`:1013-1024`, the Echols rule): unscored games are priced at `vet_grade`/`pr_supp`/`pr_pbu`, with a `price_basis` label.
- `faced_band_mz` (`:1029-1040`): per band, defender_games, tot_snaps, `unscored_share` and snap-weighted means.
- `faced_team_band_mz` (`:1042-1054`): per team × band.
- `scheme_share_mz` (`:1057-1065`): man share, informational only.

**Block 10, 2026 projection** (`:1098-1339`):
- Slot tables `slot_usage_one_mz(sp)` (`:1098-1116`), `slot_usage_med_mz`, `fallback_sn_mz`.
- `proj_one_mz(sp)` (`:1128-1212`):
  1. adds joined to the ledger;
  2. membership = proposed − drops + adds, with walls;
  3. values: blend2 at w25; `prior_used` when the blend is NA; `grade_f` falls back to the rookie or vet prior;
  4. cap at the top N by usage (zero usage becomes NA), with slot-weight inheritance: team slot, then league median, then fallback;
  5. phantoms `PHANTOM_<band><slot>` at `phantom_grade`.
- `rot_2026_mz` (`:1214-1219`): walled to 14 × 9 × 2 rows.
- `team_band_2026_mz` (`:1227-1238`): per team × band n_members, `interp_share` and usage-weighted values.
- `band_wt_mz` (`:1242-1253`), `team_unit_2026_mz` (`:1255-1260`, grade only).
- `slate_band_2026_mz` / `slate_unit_2026_mz` (`:1263-1276`): means over the 17 schedule entries.
- `cmp_slate_mz` (`:1278-1284`) and `cmp_unit_mz` (`:1286-1295`): the answers, with d = 2026 − 2025.
- THE ANSWER prints at `:1297-1308`; `answer_mz` (THE TWO SLATES) at `:1312-1339`.

**Blocks 11-12:** `fig_one_mz(sp)` (`:1355-1400`) is the dot plot per split, with grey = faced and navy = 2026. `cov_gt_one_mz(sp)` (`:1412-1488`) is the per-opponent gt.

**Audit tool** `cell_drill_mz(tm, b, sp)` (`:1528-1590`): pass an opponent, a band and a split. It prints the faced games, the 2026 room and the committee arithmetic. Calls at `:1594-1599` are examples. In this file the final "D =" line uses earned-only g25, while the "faced '25" line uses filled g25_f.

### pff_stats/secondary/league_opp_secondary_schedule.R
**Part 1 (`:1-1611`):** the NE build, shifted about 11 lines, with the **FACED FILL LAW** (header `:37-47`): a faced defender without a gated season gets the arm prior for his split and band.
- `faced_games_one_mz` (`:1009-1034`) adds `arm` and `fill`.
- `faced_band_mz` (`:1042`) and `faced_team_band_mz` (`:1055`) report `fill_share`.
- `cmp_slate_mz` (`:1289`) carries `fill_share_25`, which the availability layer requires.
- `cell_drill_mz` is at `:1539`. The deltas tribble is at `:982` and its gate at `:987`.

**Part 2 (`:1613-2510`): the league engine** for all 32 teams (`_sec_lg`), a built-in test.
- **Setup** (`:1623-1669`):
  - an empty deltas table is created if missing (`:1633`);
  - column checks (`:1648-1655`);
  - `X_MAN_SEC = 6`, `G_MAN_SEC = 6`, `X_ZONE_SEC = 13`, `G_ZONE_SEC = 6`, `N_SEC_LG` = 3/1/3/2 (`:1657-1661`);
  - `to_pff_sec(x)` (`:1669`): lookup first, then the patch (ARI/AZ→ARZ, BAL→BLT, CLE→CLV, HOU→HST, LAR→LA, JAC→JAX, WSH→WAS).
- `sec_qbgrp_lg` and `modal_band_sec_lg` (`:1676-1710`).
- `cur_build_sec(split)` (`:1716-1735`) and `cov_pctl_sec_lg` (`:1737`): lean c1 with grade, supp and pbu. team_report reads this.
- Usage, band, ids and rosters (`:1755-1841`): `ros_sec_lg` (`:1792`), `pff32_sec` (`:1831`, the 32 codes), and a code wall (`:1835`).
- Prior audit (`:1850-1961`). Ledgers and committees (`:1967-2133`):
  - `proj_one_sec(split)` (`:2006`);
  - `rot_2026_sec_lg` (`:2093`, 576 rows);
  - `team_band_sec_lg` (`:2104`), `band_wt_sec_lg` (`:2115`), `team_unit_sec_lg` (`:2127`).
  
  Deltas stay limited to NE's 14 opponents by the gate at `:987`.
- **Faced side, all focals** (`:2149-2323`):
  - `faced_games_one_sec(split)` (`:2149`) applies the fill law to grade and supp.
  - The focal is extracted from the `qbgrp_ssn` prefix, longest code first so LAC wins over LA (`:2186-2203`), with a census print.
  - Walls at `:2207` and `:2222-2255` (NE's faced weeks match Part 1).
  - `faced_band_sec_lg` (`:2258`), `faced_unit_sec_lg` (`:2266`), `wk_diet_sec` (`:2274`).
  - NE WALL A (`:2289-2305`), then a regular-season-only receipt (`:2308-2323`).
- **2026 slate** (`:2333-2440`):
  - `load_sched_sec(season)` (`:2333-2361`) tries nflreadr, then games.rds, then games.csv;
  - regular season only; `opp26_sec` (`:2376-2381`, 544 rows);
  - `slate_band_sec_lg` (`:2384`, NE WALL B), `slate_unit_sec_lg` (`:2411`, NE WALL C).
- `league_cov` (`:2448-2456`): per focal × split, unit_25, fill_share_25, unit_26, interp_share, d_grade and ranks. Prints the ladder, summaries, NE, the man-vs-zone correlation, the AFC East and a league-mean check.

### pff_stats/secondary/league_secondary_evaluating_currency_three.R
c3 per split. Each qualifying game's split grade (6+ man or 13+ zone snaps) is ranked against same-band defenders who faced the same `qbgrp_ssn` that season. The season median of those ranks is then ranked within band and season. Inputs are required at `:51-56`; the column wall is at `:77-90`.
- `c3_one_split(sp)` (`:98-207`) returns `game`, `season` and `pctl` frames:
  1. keep games at the floor (`:104-112`; the `unname()` note is at `:106-109`);
  2. `vsopp_rank` within qbgrp_ssn × band, 0.5 for a singleton (`:113-119`);
  3. name from the top-snap game (`:166-168`);
  4. `qual_g`, `sp_snaps`, `med_vsopp`, gated at G (`:169-181`);
  5. `c3_pctl` (`:185-188`).
  
  Prints: honesty by season (`:130-150`), pool sizes (`:154-161`), and top/bottom 10.
- `cov_c3_game`, `cov_c3_season` and `cov_c3_pctl` (`:209-219`, pctl at `:214`), walled unique.
- Soft context (`:225-288`): c1 vs c3, where d = c3 − c1 and positive means the raw number was held down by a hard slate. Also NE's room and `league_cov` context. Legend at `:294-329`.
- Pools are per QB group and there is no week filter. The header (`:35-39`) marks pooling by qbgrp_ssn × modal band as "UNSIGNED". The firewall (`:17`, `:324-326`) says c3 never feeds a slate; the later availability layer labels its adjusted slate as its own family (availability header `:29-34`).

### pff_stats/secondary/league_secondary_availability.R
Prices expected absences into the 2026 side, per split, for NE and for all 32 teams. Source order: step 0 → league schedule → c3 → this file. "Played" here means meeting the split floor in a regular-season week.
- **Setup walls** (`:87-176`): 32 objects, columns, bands, splits, 576 rows, tibble class.
- **Knobs** (`:182-192`): `AVAIL_SEASONS_CV = 2023:2025`, `AVAIL_DENOM_CV = 17`, `SPLIT_FLOOR_CV` = the canon floors.
- `c3_entry_prior_cv` (`:203-236`): `pr_c3`, the entry-season c3 median per split × band, with a pooled fallback.
- `one_split_hist_cv(sp)` (`:251-284`): played weeks, committees (top N), `avail_s`, `yrs`, `job_yrs`, `avail_all`, `avail_job`. Avail is per split: a week with 5 man snaps is "not played" for man.
- `bench_cv` (`:312-346`): reference only.
- First-year lines (`:359-422`): `first_year_cv`, `first_year_lines_cv`, `lines_wide_cv` (`line_backup`, `line_rookie`). 2023 first-years are excluded.
- `one_split_repl_cv(sp)` → `repl_level_cv` (`:438-509`): 2025 non-committee games, valued earned else prior, giving snap-weighted `repl_g`, `repl_s`, `repl_p`, `repl_a`.
- **`members_cv`** (`:524-614`):
  1. start from `rot_2026_sec_lg`;
  2. recover ids through `ros_sec_lg`;
  3. `c3_f` = blend2 of a25/a24 at `w25a`, else `pr_c3`;
  4. `avail_src`: starter_history, else backup_line for phantoms, else rookie_line for 2026 entry or rookie status, else backup_line;
  5. priced `grade_p`, `supp_p`, `pbu_p`, `adj_p` (`:573-576`);
  6. provenance wall against the rotation at 1e-8.
- **NE slate** (`:632-880`):
  - `ne_frame_cv`, `tb_ne_cv` (healthy/priced g/s/p/a, `interp_raw`, `interp_adj`), `slate_band_ne_cv`, `backup_gms_ne_cv`;
  - walls against `cmp_slate_mz`; `faced_c3_one_cv(sp)` with a membership wall; `ne_slate_avail_cv`;
  - four printed lenses per split.
- **League sweep** (`:893-1081`):
  - `tb_lg_cv`; `sweep_band_cv` (walled to `slate_band_sec_lg`, 256 cells); `team_unit_cv`;
  - **`sweep_unit_cv`**: healthy_u, priced_u, healthy_a_u, priced_a_u, interp_raw, unit_25, fill_share_25, d_healthy, d_priced and ranks, walled to `league_cov`;
  - the injury-asymmetry print; ladders with swing and rank change.
- **Boards** (`:1089-1167`): fragile committee defenders, backups plus phantoms, rookies, team exposure.
- **Per-team visual** (`:1185-1347`): `team26_split_cv`, `faced_team_cv`, `faced_team_c3_cv`, `cmp_team_cv`, `ord_one_cv`, `gt_one_cv(sp, ord, tag)`. It uses healthy 2026 values, not priced.
- **Readers:** any_team_evaluation `:400-411`, `:882-901`, `:1648-1662`; ne_players_evaluation `:68-112`, `:186`.

---

## Coverage toolbox: pff_stats/secondary/pff_pass_coverage_AWS.R

Andy's coverage workbench ("the tower"), listed as the secondary step0 (LINEAGE.md:503). It reads five Athena tables. Its objects do not feed team_report: the league chain starts from the separate cache, which copies this file's `final_position` rules (`:128-139`) but skips its snap gate (`pff_secondary_cache_step0_AWS.R:17-22`).

It needs these in session: `combined_grade_epa_summary`, `play_counts`, `receiving_func_base` (`receiving_stats_build_AWS.R:304`), `RECV_CO_ORDER` and `%||%` (`receiving_stats_comparison.R:859`, `:279`).

**Setup and pulls:** `%ni%` (`:5`), `run_athena_query` (`:7-48`), then the five pulls (`:51-74`).

**`final_coverage_df`** (`:77-175`):
1. Full joins of summary + scheme (`:77-80`) and slot (`:88-99`). Renames are done by column number (`:82-85`, `:99`, `:102`).
2. `adv_position` join (`:102-105`).
3. Decile exploration (`:108-122`).
4. **Snap gate** (`:124-125`): keep a game if any of these holds: total ≥ 23, man ≥ 8, zone ≥ 13, slot ≥ 8.
5. `final_position` (`:128-139`), same rules as the cache.
6. Derived rates, with zero-target guards (`:141-159`).
7. Team codes (`:162-174`).

**`final_coverage_df_qbgrp`** (`:177-195`): the join on `team_name = opp` gives `qbgrp_ssn` and `def_ssn`; the `play_counts` join adds `game_id`; PBU rates are recomputed (`:190-193`).

**Gates actually applied** (notes at `:241-245`):

| Family | Game snaps | Team-position group | Season games |
|---|---|---|---|
| Combined | ≥23 (`:252`, `:323`) | ≥6 (`:287`) | ≥6 (`:340`) |
| Man | ≥7 (`:361`, `:427`) | ≥6 (`:392`) | ≥5 (`:444`) |
| Zone | ≥13 (`:465`, `:531`) | ≥6 (`:496`) | ≥5 (`:548`) |
| Slot | ≥8 (`:569`, `:616`) | ≥5 (`:588`) | ≥5 (`:627`) |

`valid_qbgrp_*` keeps offenses with 8 or more games.

**The four families** (combined `:251-355`, man `:360-459`, zone `:464-563`, slot `:568-636`). Lower-is-better metrics (catch rate, yards, YAC, QBR, missed tackles, aDOT) are negated, so a percentile of 1 is always good for the defender. A high aDOT percentile therefore means shallower targets. Slot has five metrics. Each family has three levels:
- **Level 1 `coverage_<fam>_player_agg`:** per-game percentiles vs the player's own games against that offense, and `*_def_ssn_perc` vs his defense-season at his position. `n_<fam>` counts the group's player-games, not the player's own games.
- **Level 2 `coverage_<fam>_opp_percentile`:** per offense, the mean own-team percentile by position, wide. High means that offense was easier on that position.
- **Level 3 `coverage_<fam>_player_season_summary`:** season means ranked within position × season. Rows are split by position and `def_ssn`.

**Matchup table `receiving_coverage_versus`** (`:639-677`):
- Defender helper (`:639-643`) and receiver helper (`:645-647`). The receiver labels come from the receiving pipeline (`receiving_stats_build_AWS.R:174-180`, `:268`, `:274`, `:229-236`).
- The join (`:651-656`) starts fresh from the raw table and stops on `.x`/`.y` collisions (`:657-659`). Sub-gate defender-games carry NA position.
- `add_pctl_buckets(df, cols)` (`:661-677`): Q1-Q4 quartile buckets.

**Other pieces:**
- `put_object` (`:694-698`): uploads a workspace to S3 whenever it runs.
- `passer_rating(att, comp, yds, td, int)` (`:700-707`).
- `coverage_archetype_profile(defender_id, dim, df, def_ssn_filter, position_group_filter)` (`:742-809`): the defender vs his teammates against one receiver category, with `*_oe` = his value minus theirs (negative = he allowed less). The dimension menu is at `:709-740`.
- **vs Exp** (`:817-864`):
  - receiver norms need ≥15 targets (`:818-824`);
  - `receiving_coverage_defender_base` (`:828-853`) drops sub-gate games (`:830-833`), uses the modal position, takes the target-weighted expected value, keeps `n_tgt` ≥ 10, and gives `cr_pctl`/`ypt_pctl`;
  - tripwire (`:856-861`); `receiving_coverage_defender_final` (`:863-864`).
- Self-check (`:866-871`): each search string also appears on the check's own lines, so the flags read TRUE while the block is present.

**Plots and comp tools:**
- `plot_coverage_card_player` (`:875-1032`): one family's season percentiles per position panel; DL has no panel.
- `plot_coverage_archetype` (`:1039-1135`).
- `build_defender_diet_matrix(cov_versus, cov_df, dims, min_total_targets = 20, min_games = 6)` (`:1155-1252`): deployment shares plus diet shares with prefixes pgrp_, posR_, rte_, tgt_, algn_, tdg_, mz_, zsc_, xpa_, xtd_. Labels are cut at the first space (`:1196`). The real build is `defender_diet_matrix` at `:1345`.
- `find_similar_defenders(defender_id, target_def_ssn, matrix_df, same_position, n_top, min_similarity, weights)` (`:1255-1343`):
  - weighted cosine similarity with default weights man/zone 1.75, slot 1.5, pgrp/posR 3, tgt/algn/zsc/xpa/xtd 1, rte/tdg/mz 0.75;
  - pool selection rules at `:1277-1290` (default top 15);
  - only the focal row is removed, so his other seasons can appear as comps.
- `inspect_defender_comps` (`:1350-1382`).
- `compare_comps_performance` (`:1387-1421`): league percentiles, not re-ranked within the cohort.
- `plot_comps_dots` (`:1446-1538`).
- `DEF_FAMILY_SPEC` (`:1555-1580`).
- `defender_comp_card` (`:1582-1634`): league_pctl vs cohort_pctl, plus two vs-exp rows that are all-coverage.
- `plot_defender_comp_card` (`:1636-1671`).
- `COV_METRIC_DICT` / `COV_GROUP_ORDER` (`:1687-1704`): the groups Grade, Avoid, Suppress, Disrupt, Damage, Context.
- `plot_coverage_season_heatmap` (`:1706-1755`).
- `coverage_common_opp_pctl(player_id_in, season_in, family, dict, df, min_comp = 3)` (`:1763-1801`): each game ranked against same-position defenders who faced the same offense group.
- `plot_co_pctl_bars` (`:1810-1853`): the same name is defined at `receiving_stats_comparison.R:865`, and the last file sourced owns it.
- `plot_vs_exp_strip` (`:1862-1912`).

**Examples, not settings:** `:680-691`, `:812-815`, `:1034-1036`, `:1138-1152`, `:1347-1348`, `:1384`, `:1424-1443`, `:1540-1545`, `:1673-1679`, `:1757-1758`, `:1803-1807`, `:1857-1858`, `:1914`.

---

## Pass rush (pff_stats/pass_rush)

Bands are ED and DI, from PFF's `position`. A player's band is his modal band by TPS snaps (NE file `:56-62`). His 2026 band is the 2025 band, else the 2024 band (`band_map`, `:134-140`); rookies get one only through deltas. c1 is the snap-weighted season TPS grade percentile; c3 is the same-opponent median. The whole slate chain is TPS-only.

### pff_pass_rush_AWS.R
The canon file. Lines 50-191 are step 0, `:194-339` build the card tables, and the rest is exploration that uses other units' objects, so sourcing the full file errors.
- `run_athena_query` (`:6-47`): returns a data.frame.
- Pulls and join (`:50-75`).
- Frame gate (`:93-94`): 11+ rush snaps or 7+ TPS snaps.
- Codes (`:96-108`).
- `full_pass_rush_qbgrp` (`:111-123`): the defense-side join, with renames by index at `:122-123`.
- Rates (`:174-184`). `percent_rank_avg` (`:187-191`, repeated at `:342-346`).
- `pass_rush_all_player_agg` (`:194-215`; 7+ snaps, groups of 8+), `valid_qbgrp_all` (`:217-221`), `pass_rush_all_opp_percentile` (`:223-238`; high = easier protection).
- `pass_rush_all_player_season_summary` (`:240-263`): 11+ snaps, 8+ games.
- TPS mirror (`:270-314`); `pass_rush_tps_player_season_summary` (`:316-339`): 11+ TPS snaps, 7+ games. These card gates differ from the slate gates (NE file `:52`).
- Pass-block re-ranks (`:349-357`). Exploration (`:361-472`).
- Plots:
  - `plot_pblk_games(df, focal, pos, metric, sort_by)` (`:474-518`)
  - `plot_prush_card(team_season, view, df_all, df_tps)` (`:549-621`): ED/DL/LB panels, league percentiles
  - `plot_prush_players` (`:624-697`)
  - `plot_prush_card_player` (`:710-804`)
  - `plot_prush_players_player` (`:807-891`)
- Example calls (`:894-907`): line 896 is written `c(52000, )`.
- The OL dumbbell (`:910-968`) reads `plot_det_opp_tps_*` names, while the frame built above it is `plot_ne_opp_tps_long`.

### pff_pass_rush_qbgrp_step0_AWS.R
A standalone copy of `:50-191` with receipts:
- `run_athena_query` (`:38-77`)
- gate (`:108-111`)
- `canon_fix` (`:116-130`)
- qbgrp join (`:135-148`)
- name-based renames (`:153-168`)
- contract wall and receipts: NE 21 weeks, the DI/ED census (`:198-220`)

### new_england_opp_pass_rush_schedule.R
Three blocks: the build (`:1-565`), a byte-identical second copy (`:568-1132`), and the per-team add-on (`:1135-1544`). Its faced side is observed-only.
- Gate (`:17-23`). `X_QUAL = 3`, `G_MIN = 6` (`:29-35`).
- c1 (`:52-89`): `prush_qual_games`, `prush_modal_band`, `prush_tps_season_pctl_sos` with grade, win, PRP, pressure and hurry percentiles.
- Crosswalk and rosters (`:96-118`). Ledger (`:127-166`), with status order rookie → no_pff_id → has_2025_pctl → data_2024_only → usage_no_pctl → no_prush_history.
- Proposal (`:174-210`): `N_BAND = 4` and `usage_ord`. Rookies are never auto-proposed.
- Ledger prints (`:216-244`). Deltas `opp_prush_2026_deltas` (`:261-274`): empty, name-walled.
- Faced (`:282-309`): `unscored_share`, NA left out.
- Projection (`:319-483`):
  - `prush_rookie_prior` (`:326-333`) and slot curve (`:340-358`)
  - `rot_real` (`:378-404`), cap (`:412-424`), `auto_phantoms` (`:426-440`)
  - `rot_2026` (`:442-452`, 112 rows), `team_band_2026` (`:454-465`), `slate_band_2026` (`:468-473`)
  - `cmp_prush_slate` (`:475-483`)
- `plot_prush_slate` (`:490-519`), `prush_slate_gt` (`:528-555`).
- Add-on:
  - regime snapshot (`:1192-1210`): copies the run-defense or pass-rush `team_band_2026` to a suffixed name, detected by its columns
  - `_prush` rebuild (`:1217-1402`), equality receipt (`:1409-1426`)
  - `faced_prush_team_2025` (`:1433-1453`), `prush_team_cmp` (`:1455-1469`), `prush_team_gt` (`:1478-1502`), `plot_prush_team_hm` (`:1504-1535`)

### league_pass_rush_evaluating_currency_three.R
- Gates (`:32-53`).
- `prush_game_c3` (`:60-72`): `vsopp_rank` within qbgrp_ssn × position. A team with two QBs forms two pools.
- Honesty prints (`:74-91`).
- `modal_band_c3`, `prush_name_c3`, `prush_c3_season` (`:97-124`): one row per player-season, traded players included once.
- `prush_c3_pctl` (`:130-149`).
- Optional context (`:155-207`): c1 vs c3 and the NE2025 block. The header is at `:4-16`.

### league_opp_pass_rush_schedule.R
A newer edition with the fill law. It runs after the NE file, so its generic names win.
- Fill law (header `:16-24`; `:287-350`): priors moved up (`:297-314`); `faced_games_2025` (`:316-334`); `faced_band_2025` (`:339-350`); `cmp_prush_slate` with `fill_share_25` (`:500-505`); per-team versions (`:893-931`).
- **League test** (`:1009-1368`):
  - `X_LG = 3`, `G_LG = 6`, `NB_LG = 4`; `cur_lg` (`:1019-1055`), which team_report reads
  - `xw_lg`, `ros_lg`, `use_lg`, `band_lg`, `prior_lg`, slot curve (`:1058-1120`)
  - `rot_lg` (machine-only), `ph_lg`, `team26_lg` (bands pooled) (`:1123-1158`)
  - `to_pff_lg`; `faced_games_lg` with regex focal extraction (`:1186-1228`); walls (`:1231-1260`); `faced_band_po`, `faced_lg`
  - NE wall (`:1267-1288`); `load_sched_lg` (`:1295-1323`)
  - `league_prush` (`:1325-1368`): faced includes playoffs, 2026 is regular season only

### league_pass_rush_final_evaluation.R
The adjusted edition (inputs at `:26-32`):
- `c3_rookie_prior` (`:39-47`)
- console lookups `player_c3(who)` (`:53-93`), `team_c3(team, ssn)` (`:95-157`), `team_c3_26(tm)` (`:159-230`)
- `faced_c3_2025` / `faced_band_c3` (`:237-254`)
- `rot_c3`, `slate_band_c3` (`:261-287`): `adj_26` is weighted over member rows, while the raw side is a plain mean of team values
- `cmp_adj`, `prush_adj_gt`, `plot_prush_slate_adj` (`:295-351`)
- `rot26_full` / `slate_view` (`:374-465`): these need `opp25_lg`, which nothing builds (REBUILD_RUNBOOK.md:362-366), so they print "[slate_view disabled]"

### league_pass_rush_availability.R
- Gates (`:101-216`): the identity stop requires `rot_2026` to be ED/DI only (`:163-174`); `wall_pa` helper.
- `c3_entry_prior_pa` (`:223-233`).
- History `games_pa`, `hist_pa`, `avail_pa` (`:246-282`): job seasons are `cur_lg` rows.
- `bench_pa` (`:288-296`). Lines `lines_pa` (`:306-343`).
- `stint25_pa` (6+ distinct weeks; team_report reads it), `back25_pa` (rank > 4), `repl_pa` (`:352-402`).
- `members_pa` (`:411-534`): rebuilt to `rot_lg` with a wall (`:442-460`); `w25` at `:429`; ladder at `:479-489`; `gf_p`, `c3_p`.
- League wall (`:542-548`).
- NE slate (`:558-702`): provenance wall (`:624-646`), NE wall (`:674-680`).
- Sweep `sweep_pa` (`:713-812`): v26, v26p, a25, v26ap, fill_share, prior_share.
- Board `b1_pa` (`:819-829`).

### the_hutch_study.R
Splits one defense-season into weeks with and without one player (`:2-14`). It re-runs the TPS card pipeline with a 3-game gate (`:20-98`) and draws the two cards (`:102-103`).

---

## Offensive line (pff_stats/pass_block, pff_stats/run_block)

Five slots, taken from PFF's per-game `det_position`. Pass blocking and run blocking are always kept separate.

How the slot is chosen:
- **one game:** that game's `det_position`;
- **2025/2024 primary slot:** the slot with the most pass-block snaps in 25+-snap games (`new_england_opp_ol_schedule.R:57-62`, `:104-115`);
- **NE opponents 2026:** Andy's hand table;
- **league 2026:** the ladder;
- **c3 band:** the modal slot by qualifying snaps;
- **availability job:** the season-modal starter;
- **faced cell:** the top pass-block-snap blocker per game, team and slot.

How the currencies differ:
- **c1 pass block:** within-defense game ranks, mean per stint (4+ games), ranked within slot × season.
- **c1 run block:** gap 7+ / zone 9+ snaps, `dplyr::percent_rank`, mean per season (6+ games).
- **c3:** a median of within-defense ranks, filed under the modal band.

### run_block/pff_run_block_AWS.R
- `run_athena_query` (`:1-59`). Loads (`:62-80`); keeps only T/G/C (`:82`); codes (`:95-107`).
- `run_block_summary_qbgrp` (`:110-124`): renames by column number, no snap filter.
- Gap/zone game frames (`:126-178`).
- Opponent tables (`:181-191`).
- **c1:** `gap_player_season_summary` / `zone_player_season_summary` (`:218-242`).
- `pff_to_nflverse` (`:271-274`).
- Plots: `plot_ol_run_block` (`:277-370`), `plot_team_rush_off` (`:397-439`), `plot_team_rush` (`:447-525`), `plot_ybc_yac_quadrants` (`:571-595`), `plot_def_pos_heatmap` (`:609-662`).

### pass_block/pff_pass_block_AWS.R
- `pass_block_summary` (`:49-55`), codes (`:71-83`).
- `pass_block_summary_qbgrp` (`:86-101`, renames by number).
- `all_pass_block_summary` (`:104-109`, 25+ snaps), `tps_pass_block_summary` (`:112-117`, 16+ TPS).
- `percent_rank_avg` (`:120-124`). Game ranks (`:127-157`). Opponent tables (`:160-176`).
- **c1:** season summaries (`:202-232`), one row per stint.
- `minmax01` scaling (`:235-265`).
- `plot_ol_pass_block` (`:277-371`). Pass-rush heatmaps (`:389-476`).
- QB-protection exploration (`:478-696`): `plot_det` (`:541-559`) and `plot_league` (`:601-622`). The team prefix is an example.

### pass_block/new_england_opp_ol_schedule.R (the canon file)
- Needs checked at `:32-37`; constants `:39-45`.
- Snap profiles (`:51-62`).
- `pctl_year(df, cols, yr)` and `build_pctl(yr)` (`:68-115`) → `ol_season_pctl` (`:101`) and `_24` (`:102`), plus `ol_2024_profile`.
- `blend2` (`:121-126`). `ol_2026` (`:130-155`, one hand-excluded row at `:142`; walls at `:128`, `:157`).
- `opp_ol_2026_starters` (`:163-235`): 70 rows, `conf` high/battle. `opp_ol_2026` (`:237-244`).
- `known_rookies_2026` (`:246-251`). Checks (`:253-273`), including `slot_pool_mismatch`.
- `opp_ol_2026_backups` (`:279-297`): ten live rows; three were commented out 2026-09-12. `backup_rows(teams_vec)` (`:302-320`).
- `entry_years`, `first_season_prior(df, col, out)`, `rookie_prior` (`:334-358`).
- `opp_ol_2026_final` (`:363-375`): rookies use the blend else the prior; others use the blend else raw. A leftover NA is a canon hole.
- `team_ol_2025_starters`, `ol_slot_delta` (`:381-405`).
- `ne_2025_opp_ol_games` (`:411-420`, earned only), `slate_profile` (`:434-442`), `cmp_ol_slate` (`:444-453`), `cmp_ol_slate_sens` (`:455-473`, battle sensitivity).
- Figure 1 `plot_ol_slate_delta` (`:479-509`); `audit_panel` (`:525-539`).
- Panel setup (`:549-580`). History gt `ol_hist_gt` with `year_cols` (`:590-660`).
- `ol_slate_cmp_gt` (`:671-724`).
- Figure 2 v2 (`:752-998`): `lvl_order_v2`, `v2_incoming`, `v2_faced`, `v2_swing`, `v2_rows`, `build_fig2v2` → `plot_v2_div/rep/new`.

### Currency-three files
- **Pass block** (`league_pass_block_evaluating_currency_three.R`):
  - `X_QUAL_PBLK = 16` (marked PROPOSED), `G_MIN_PBLK = 6` (`:95-97`)
  - `pblk_game_c3` (`:103-128`), season level (`:152-175`), `pblk_c3_pctl` (`:181-184`)
  - `c1_obj_pblk = NULL` (`:207-218`); legend (`:252-299`)
- **Run block** (`league_run_block_evaluating_currency_three.R`):
  - `X_QUAL_RBLK = 8`, `G_MIN_RBLK = 6`, `X_QUAL_GAP = 7`, `X_QUAL_ZONE = 9` (`:105-115`)
  - blended c3 (`:121-218`)
  - `scheme_c3_rblk(grade_col, snap_col, x_qual, tag)` (`:249-307`) → gap/zone runs (`:309-314`), with a check at `:320-325`
  - final `rblk_c3_pctl` with `gap_*`/`zone_*` columns (`:327-343`)
  - legend (`:354-448`): NA is an honest hole

### League ladder files
**`league_opp_pass_blocking_schedule.R`:**
- Needs at `:77-84`.
- `load_sched_ol` (`:97-125`); `to_pff_ol` (`:130-134`); `pff32_ol`; `opp_map_ol` (`:155-160`, 544 rows).
- `ol_2026_lg` (`:166-195`).
- **Ladder** (`:211-335`):
  - `cand_ol` (`:211-215`): slot = 2025 primary, else 2024; `usage_ord`
  - rung 1 `modal_pick` (`:217-223`)
  - rung 2 flex fill (`:233-252`): the team's best unused lineman, no reuse
  - rung 3 `slots_full` (`:265-269`)
  - prints: phantom ladder, THE 160-SLOT LEAGUE LADDER, `big25_unpicked` (`:305-335`)
- `league_one_lens(lens)` (`:344-464`): `V` from phantom / rookie / non-rookie fallback steps with `V_source`; `faced_cells` priced under the fill law; output v26, faced25, d, prior_share, fill_share.
- `league_sys_pb` / `league_wide_pb` (`:466-495`); receipts R1-R4 (`:501-576`).
- Legend (`:582-634`): d is positive for most teams by construction, so rank is the signal.

**`league_opp_run_blocking_schedule.R`:** the same code with gap and zone lenses. `league_sys_rb` (`:466-467`), `league_wide_rb` (`:473-489`), summaries (`:491-509`), receipts (`:515-603`). Selection still uses pass-block snaps (header `:17-20`).

### league_pass_block_final_evaluation.R
- `c3_rookie_prior_ol` (`:130-145`).
- Lookups: `get_tps_raw_hist_pb` (`:168-185`), `player_c3_pb(who)` (`:187-221`), `team_c3_pb(team, ssn)` (`:223-294`), `team_c3_26_pb(tm)` (`:296-365`).
- Faced adjusted `faced_c3_pb` (`:376-456`), with `fill_why` = earned / no_c3_2025 / off_modal_slot.
- `proj_c3_pb` (`:466-501`).
- `cmp_adj_pb`, `pblk_adj_gt`, `plot_pblk_slate_adj` (`:511-576`).
- `rot26_full_ol` and `slate_view_pb(focal)` (`:604-692`): the faced raw side fills at priors (header `:64-67`).

### Availability files
**`league_pass_block_availability.R`:**
- Knobs (`:92-98`): 2023-25, 8 snaps, 17.
- `starters_all_av`, `avail_pb` (`:111-143`): the feed holds only 25+-snap games (`pff_pass_block_AWS.R:105`).
- `slot_starter_avail_pb` (`:152-175`); `first_year_lines_pb` (`:188-257`); `repl_level_pb` (`:265-312`).
- **`slot_value_26_pb_build`** (`:322-377`): avail_src ladder; `gf_raw_av`, `gf_adj_av` (`:356-359`); walled against `rot26_full_ol` (`:361-375`).
- NE slate with canon holes (`:385-482`). Sweep `sweep_avail_pb` (`:490-615`). Boards (`:623-683`). Team split gt (`:697-799`).
- team_report reads it at any_team `:904-920` and `:1421-1433`; ne_players at `:187`.

**`league_run_block_availability.R`:**
- Knobs (`:150-157`). `starters_all_rb` (run-block snaps), `avail_rb` (`:174-240`).
- c3 priors `c3_rookie_prior_rb` and `c3_scheme_prior_rb` (`:249-286`).
- Lines (`:299-363`); `repl_level_rb` (`:373-461`).
- `faced_cells_rb` and **`slot_value_26_rb_build`** (`:483-648`): its avail_src differs, since any non-rookie non-history player gets the backup line; pricing at `:569-575`; league wall (`:595-646`).
- NE slate `proj_c3_rb`, whose scheme values carry no prior fill (`:662-917`).
- Sweep `sweep_avail_rb` (`:928-1037`). Boards (`:1045-1116`). `gt_gap_team_rb` / `gt_zone_team_rb` (`:1137-1295`).
- team_report reads it at any_team `:921-950` and `:1523-1547`; ne_players at `:188`.

---

## Run defense (pff_stats/run_defense)

Bands DI/ED/LB/S; `N_BAND` = DI 5, ED 5, LB 3, S 4. Safeties enter through "DB" roster rows; corners drop out as `no_rundef_history` (`new_england_opp_run_defense_schedule.R:21-28`, `:165-167`). Only run grade and stop rate travel past the eyeball prints.

### pff_run_defense_AWS.R (exploration, outside the chain)
- `run_athena_query` (`:6-47`).
- Pull (`:50-53`); **14+ run snaps** kept (`:65-66`); codes (`:69-81`); FB/WR dropped (`:84-85`).
- Joins (`:87-96`); renames by number (`:98-99`).
- Team-share columns (`:120-127`, left grouped).
- `run_defense_summary_player_agg` (`:141-165`, dplyr `percent_rank`, `n >= 6`); `valid_qbgrp_run` (`:167-171`); `run_defense_opp_position_percentile` (`:173-191`).
- `run_defense_player_season_summary` (`:195-219`): MT rate and ADoT are not negated here, so "dark" on the card means more misses or deeper tackles.
- `plot_ybc_yac_quadrants_def` (`:225-249`; `rush_stats_high` from `rush_stats_df_build.R:66`).
- Gap/zone opponent tables are viewed at `:254-255` and built in `pff_run_block_AWS.R:181-191`.
- `plot_run_def_card` (`:262-342`), `plot_run_def_card_player` (`:347-436`), helpers (`:461-467`).
- `plot_on_off_dist(gsis_id, outcome, bucket, player_name, def_ssn, df, breaks, clip, wp_range)` (`:469-606`): rush plays with the defender on the field (his gsis_id is in `defense_players`, `:518`) vs off. Without `def_ssn`, OFF includes every other play.
- `plot_def_ssn_dist` (`:652-761`).

### pff_run_defense_qbgrp_step0_AWS.R
`run_defense_qbgrp`: every raw row, no snap filter (header `:1-39`).
- helper (`:46-85`), pull (`:90-98`), `canon_fix` (`:103-117`)
- join with walls (`:124-170`); stops only if every row is unmatched
- contract wall (`:176-187`), coverage (`:192-199`), cleanup (`:200-202`)

### new_england_opp_run_defense_schedule.R
Faced side is observed-only (`unscored_share`).
- Gate and fallback (`:42-58`). Constants (`:64-67`).
- **c1** (`:77-117`): only games at the modal band count; snap-weighted grade; stop rate = stops / run_stop_opp; MT and depth negated.
- Prints (`:125-148`). Rosters (`:158-182`). Ledger (`:190-229`). Proposal (`:236-244`). Ledger prints (`:250-286`).
- `opp_rundef_2026_deltas` (`:301-314`): empty.
- Faced (`:322-346`).
- Projection (`:355-509`):
  - priors (`:355-371`), slots (`:375-393`), `rot_real` (`:396-417`), values (`:422-435`), cap (`:438-451`)
  - phantoms (`:453-468`), `rot_2026` (`:470-475`), `team_band_2026` (`:483-489`)
  - `slate_band_2026` (`:496-500`), `cmp_rundef_slate` (`:502-509`)
- Plot and gt (`:517-581`).
- Add-on (`:594-963`): `_rundef` rebuild, `status_rundef` (`:681-691`), equality receipt (`:830-847`), per-team frames, gt and heatmap (`:854-953`).

### league_run_defense_evaluating_currency_three.R
- `X_QUAL_RD = 3`, `G_MIN_RD = 6` (`:74-76`).
- `rd_game_c3` (`:82-100`): pools by qbgrp_ssn × game-row position.
- Season (`:124-147`): the median runs over all qualifying games, unlike c1.
- `rd_c3_pctl` (`:153-156`). Context (`:178-228`). Legend (`:234-277`).

### league_opp_run_defense_schedule.R
- **Part 1 (`:1-991`):** the NE build with the fill law (`:30-39`, `:340-384`, `:526-530`, `:880-899`).
- **Part 2 (`:993-1578`):** the league engine.
  - constants (`:1024-1026`); `to_pff_rd` (`:1032-1036`); `cur_rd_lg` (`:1039-1070`)
  - rosters etc. (`:1073-1162`); rotations `rot_2026_rd_lg`, `team_band_rd_lg`, `team26_rd_lg` (`:1168-1297`)
  - faced with focal extraction and walls (`:1315-1398`); NE wall #1 (`:1401-1417`)
  - `load_sched_rd` (`:1439-1467`); `opp26_rd` (`:1482-1487`); slate wall #2 (`:1490-1514`); `slate_rd_lg` (`:1517-1522`)
  - `league_rundef` (`:1530-1539`)

### league_run_defense_availability.R
- Gates and `wall_ra` (`:78-174`). `c3_entry_prior_ra` (`:181-191`).
- History (`:203-241`). Bench (`:248-256`). Lines (`:266-303`).
- `stint25_ra`, `back25_ra`, `repl_ra` (`:311-368`).
- **`members_ra`** (`:379-495`): id recovery with a rescue (`:405-425`); provenance walls (`:437-455`); ladder phantom → rookie → history → backup; pricing at `:483-485`.
- League walls (`:504-548`). NE slate (`:558-671`).
- **`sweep_ra`** (`:681-781`): healthy and priced grade/stop, priced c3, faced grade/stop, `c3_25f`, and deltas.
- Boards B1, B2, B4 (`:787-816`). Visual `gt_ra` (`:825-976`).
- Readers: team_slate (`:372-386`), team_own (`:997-1011`), team_yoy (`:1671-1709`), team_lastyear (`:1911-1933`); the rushing files gate on run-defense objects (`new_england_opp_rushing_schedule.R:611-621`, `league_opp_rushing_schedule.R:629-639`).

---

## Rushing (pff_stats/rushing)

There are two chains that share only PFF's rushing table.
- **Chain 1** feeds team_report. Order: step0 → NE schedule → c3 → league schedule → availability (`util/pipeline_status.R:42-52`).
- **Chain 2** is the play-level tower behind the matchup engines.

### Chain 1
- **`pff_rushing_qbgrp_step0_AWS.R`:** `run_athena_query` (`:42-81`); HB-only pull (`:86-108`); `canon_fix` (`:113-127`); id join (`:134-180`); wall (`:185-193`); receipt (`:198-205`). It overwrites the session's `rushing_summary` (`:90`, `:105`, `:125`). `rushing_qbgrp` is created here (`pipeline_status.R:43`).
- **`new_england_opp_rushing_schedule.R`:**
  - fallback (`:67-71`); `X_QUAL = 4`, `G_MIN = 6`, `N_RB = 2` (`:77-79`)
  - `rush_season_pctl_sos` (`:89-113`): percentiles of grun, ypa, yco, mtf_rt, brk_rt; only `grun_pctl` and `mtf_pctl` travel on
  - crosswalk and rosters (`:158-181`); ledger (`:187-217`); proposal (`:224-231`); prints (`:237-268`)
  - deltas (`:282-295`); faced observed-only (`:303-335`)
  - projection (`:344-452`): `rush_rookie_prior`, slots, `rot_real`, cap, `auto_phantoms`, `rot_2026_rb`
  - `team_rb_2026`, `slate_rb_2026`, `cmp_rush_slate` (`:460-485`); plots (`:497-573`)
  - composite pack (`:588-696`): needs the run-defense objects; `comp = (grun + mtf)/2`
- **`league_rushing_evaluating_currency_three.R`:** `X_QUAL_RU = 4`, `G_MIN_RU = 6`, `BAND_RU = "HB"` (`:73-75`); `ru_game_c3` pooled by `def_ssn` (`:81-99`); season (`:121-146`); context (`:148-204`); legend (`:210-254`).
- **`league_opp_rushing_schedule.R`:**
  - **Part 1 (`:1-723`):** the NE build under the fill law (`:37-46`, `:319-365`, `:496-500`)
  - **Part 2 (`:724-1104`):** the league test
    - `X_RB = 4; G_RB = 6; N_RB = 2L` (`:746`); `to_pff_rb` (`:752-756`); `cur_rb_lg` (`:759-773`)
    - `ros_rb_lg` (`:787-794`); priors and slots (`:809-837`); code wall (`:839-849`)
    - `rot_rb_lg` (machine-only, `:852-872`); `ph_rb_lg`; `rot_2026_rb_lg`; `team26_rb_lg` (`:874-900`)
    - `faced_games_rb_lg`, `faced_rb_lg` (`:915-949`); NE walls (`:952-958`, `:1045-1051`)
    - `load_sched_rb` (`:983-1011`); `opp26_rb`; `slate_rb_lg`; `league_rush` (`:1059-1068`)
- **`league_rushing_availability.R`:**
  - laws (`:1-102`), including the CROSSED-STREAMS LAW (`:57-86`): read only rushing-unique names
  - walls (`:108-172`); knobs (`:177-179`)
  - history (`:194-234`): **`starters_all_ru`** (`:200-208`) holds every back with a played week per season and team, ranked by attempts (`rb_rank`), with `has_job` = the top 2; `job_flag_ru` collapses traded backs
  - bench (`:241-249`); c3 prior (`:257-267`); lines (`:279-322`); `repl_level_ru` (`:333-367`)
  - **`memb_lg_ru`** (`:380-423`): pricing at `:411-416`
  - walls (`:433-500`); NE slate (`:516-729`)
  - **`sweep_ru`** (`:739-858`, `backup_gms` at `:756`); boards (`:865-937`); team split gts (`:955-1081`)

### Chain 2 (the play-level tower)
- **`pff_rushing_stats_build_one_AWS.R`:** `run_athena_query` (`:13-62`); `position_group` (`:76-82`); scrambles from `combined_pbp` (`:84-107`); `rank_grp` A/B/C (`:110-145`); `pbp_rush` (`:148-153`); down-distance exploration (`:157-446`); `ydstogo_group` (`:453-468`); `rusher_xpass_diff_df` (`:472-491`).
- **`..._two_situation_AWS.R`:** k-means per letter, A k=4 (`:106-108`), B k=5 (`:225-227`), C k=4 (`:344-346`); `validate_clusters` (`:476-601`); `cluster_dashboard` (`:638-739`); labels (`:748-775`); `situation_cluster_df` (`:783-784`).
- **`..._three_gap_AWS.R`:** `base_run_gap_cluster` (`:9-20`); A k=8 (`:103-105`), B k=4 (`:188-190`), C k=4 (`:266-268`); `gap_cluster_df` (`:293-294`).
- **`..._four_type_AWS.R`:** `player_zone_gap` (`:45-62`); `gap_z` (`:97-112`).
- **`..._five_xtd_AWS.R`:** `rusher_xtd_diff_df` (`:27-42`); `xtd_percentile` (`:126-142`).
- **`..._six_final.R`:** `rush_stats_final` (`:9-90`). `rush_share` is a carry share; `rush_proportion` (a run-snap share) sets the letter.
- **`rush_stats_df_build.R`:** `create_rush_stats(data, suffix)` (`:2-63`); subsets (`:66-78`).
- **`rush_thresholds_engine.R`:** `rush_floor` (`:30-34`); templates (`:40-59`; hand-typed matchup inputs); `rush_specs_from_df` (`:62-88`); `rush_hits` (`:111-122`); `rush_sweep` (`:136-199`).
- **`rush_comparison_engine.R`:** `rush_tune` (`:54-71`); `rush_func` (`:94-166`, writes xlsx to S3); `rush_run` (`:171-176`); `rush_push_all` (`:178-190`).
- **`rushing_stats_comparison.R`:** cluster helpers (`:2-63`; its label maps at `:38-47` differ from the build comments); identity lookups (`:66-160`); `compare_rusher_cohort` (`:175-271`); plots and scorecard (`:330-566`).

---

## Receiving (pff_stats/receiving)

**Run order for the base:** exploration → xpass → xtd → build → `receiving_func_base`, one row per player per team-game.

**Role and cluster labels.** The clusters are fit in `sagemaker/` and named by rank-and-remove rules in `sagemaker/cluster_naming.py`:
- alignment (`:18-88`): ITE, SWR, RB, WWR, WSWR, STE;
- routes (`:91-162`): DT, MT, ST, BT, SMT, RB;
- targets (`:165-251`): adds ML and G.

LR and LT mark too little volume (`pff_receiving_man_zone_exploration_AWS.R:133-135`). Label walls are at `:153-163` and `:973-978`.

### pff_receiving_man_zone_exploration_AWS.R
- `run_athena_query` (`:11-52`); intake (`:56-111`); `receiver_scheme_final` (`:115-165`).
- Study blocks (`:168-916`).
- **`man_zone_grp_cluster`**: the last `case_when` (`:853-897`) is the rule in force: WR_LT/SHORT/DEEP, TE_LT/SHORT/DEEP, HB_LT/DEEP/SHORT, and NA → OTH (`:965`).
- `z_score_percentile` and `z_source` (`:919-963`): composite z_tgt + 0.85 z_ypa + 0.65 z_cp + 0.45 z_adot + 0.85 z_qbr within group, with forced 100/0/50 values.
- `cluster_join` (`:984-1007`): can hold more than one row per player-team-season.

### receiving_stats_xpass.R and receiving_stats_xtd.R
- **xpass:** `pbp_pass` (`:4-63`), `receiver_xpass_diff_df` (`:66-82`), `xpass_percentile` pooled league-wide (`:201-238`).
- **xtd:** `receiver_xtd_diff_df` (`:9-25`), `td_grp_cluster` (`:141-166`), `xtd_percentile` within TD group (`:179-196`), and the join frame (`:202-203`).

### receiving_stats_build_AWS.R
- `pbp_receiver_stats_one` (`:58-74`). Id fixes (`:77-110`): the id string on `:94` and `:99` spans a line break. Pulls (`:122-131`).
- `final_position_group` (`:172-179`): BACK / TE / WR / OTHER. Shares (`:182-189`).
- Route-runners are added (`:194-276`); `team_rank`/`pos_rank` by routes with targets as tiebreak (`:264-276`).
- `onfield_perc` (`:279-290`). `receiving_func_base` (`:304`).
- The weather join at `:319-321` only prints; the assigning copy is `model_funcs/rec_func_AWS.R:96`.

### Tools
- **`receiving_stats_comparison.R`:**
  - `show_player_onfield` (`:2-17`); `get_player_cluster_neighbors` (`:65-82`)
  - `get_offense_receiver_identity`, defined twice (`:85-139`, `:378-415`); the second wins when sourced
  - `compare_receiver_cohort` (`:141-272`; OE metrics at `:189-198`, `:223-230`)
  - logging (`:279-349`); `get_player_identity_history` (`:356-376`)
  - `plot_receiver_pctl_heatmap` (`:672-736`); `common_opp_pctl` (`:751-784`); `threshold_sweep` (`:796-812`); co plots (`:830-899`)
- **`receiving_direct_comparison.R`:** merged-view pools (`:13-145`).
- **Contract files:** `plot_pay_vs_prod` (`:6-28`); `pay_ladder` (`doubs_contract_comparison.R:9-22`).
- **`rec_thresholds_engine.R`:** `rec_floor` (`:23-28`), `rec_specs_from_df` (`:90-117`), `rec_sweep` (`:173-291`). A sweep runs when sourced (`:370`).
- **`rec_comparison_engine.R`:** `rec_game_level` (`:129-206`); `rec_worker` (`:291-333`). The file calls `rec_tune` and `rec_push_all` when sourced (`:420-425`), which writes to S3.
- **`rec_fill_workbook.R`:** `rec_fill` (`:106-157`).

### 2026 chain
- **`new_england_opp_receiving_schedule.R`:**
  - constants: `X_QUAL = 8`, `G_MIN = 6`, `POOL_W = 3`, `N_CORPS = 8`, `SPLIT_RTE_MIN = 50`, `REC_BANDS` (`:88-93`)
  - c1 `rec_season_pctl_sos` (`:115-213`); priors (`:241-281`); ledger (`:287-379`); deltas (`:392-405`)
  - faced observed-only (`:413-443`); `rot_rec_2026` (`:452-527`); `cmp_rec_slate` (`:530-551`); plots (`:557-637`)
- **`league_opp_receiving_schedule.R`:** `opp_map` (`:44-49`); `league_one_split(sp)` (`:53-115`); `league_sys_both` (`:117-175`).
- **`league_receiving_evaluating_currency_three.R`:** local-only feed (`:135-278`); game frame (`:291-408`); `X_SPLIT_RTE = 5`; `c3_one_split_rec(sp)` (`:433-554`) → `rec_c3_pctl`; c1 vs c3 (`:574-640`).
- **`league_receiving_availability.R`:**
  - gates (`:108-214`); knobs (`:282-301`); c3 prior (`:318-358`); history (`:367-438`); lines (`:474-520`); repl (`:532-638`)
  - **`members_rc`** (`:701-891`): `w25`/`w25u` at `:729-730`, usage at `:761-765`, pricing at `:778-785`
  - NE slate (`:905-1245`); **`sweep_rc`** (`:1257-1370`); boards (`:1379-1447`); visuals (`:1458-1642`)
  - team_report reads `members_rc` (`:855-861`, `:1329-1340`) and the 2025 frames (`:1286-1327`)

---

## Foundation: data build, QB stats, model_funcs, util, scrapers, dbt

- **The keys** (`data_build/pbp_nfl_base.R:305-319`):
  - `qbgrp_ssn = posteam + last_name + "-" + season`, where the last name is the middle word of a three-word name, else the last word (`:308-313`);
  - `def_ssn = opp + season`.
  
  They are per team-week, so backup snaps inherit the week's starter's group. Postseason weeks are 28/29/30/32 (`:20-27`). The frame is uploaded at `:364-370` and joined onto plays at `pbp_part_combined_join_AWS.R:88-94`.
- **pbp_nfl_base.R:** features and filters (`:15-18`, `:130-170`); weather fill (`:174-196`); `run_athena_query` (`:43-84`); a `defteam` quirk at `:115`.
- **pbp_combined_AWS.R:** twelve xgboost models (`:52-494`); drive xTD (`:501-515`); `pbp_xtd` (`:517-531`).
- **part_nfl_base.R:** personnel parsing (`:78-455`); box-count imputation (`:459-515`).
- **part_weather_backfill.R:** `backfill_weather` and helpers (`:90-217`).
- **participation_combined_AWS.R:** artifact-driven features (`:14-31`); `part_xtd` (`:526-556`).
- **pbp_part_combined_join_AWS.R:** prefixes (`:4-20`); join (`:27-30`); `mixed_xpass` (`:100-116`).
- **Id builds:** `SEASONS <- 2016:2025` (`pff_ids_build_AWS.R:20`); cascade (`:226-264`); checks (`:324-334`). Defense cascade (`pff_ids_build_defense_AWS.R:142-153`). Cross-check `id_xwalk` (`pff_ids_validate_cross.R:60-71`) and `pff_team_lookup` (`:75-87`).
- **`pff_stats/qb_stats_df_build_AWS.R`:**
  - line 2 is a bare `combined_pbp`;
  - `qb_stats_df_base` (`:59-95`) has one row per qbgrp_ssn × def_ssn × team-game;
  - PFF split joins (`:180-262`); `_rank` within qbgrp_ssn and `_rank_def` within def_ssn (`:288-460`), with pressure/sack/twp/int flipped;
  - the weather join onto `receiving_func_base` (`:491-493`).
- **model_funcs:**
  - comparison lookups built in `df_builds/` (`df_blitz_build_AWS_NEW.R:402-435`); the per-family files; `rec_func` (`rec_func_AWS.R:168`, closest helpers `:590-630`); `rush_func` (`rush_func_AWS.R:163`)
  - tuner `comparison_engine_thresholds.R`: knobs (`:64-75`), `target_size` (`:95`), optimizer (`:167-280`)
  - `stats_comparison_engine.R`: categories (`:89-150`), families (`:165`), `stats_run` (`:399`). The bottom of the file makes live calls that write workbooks.
  - `pct_adjuster_engine.R` (`:17-43`); `entity_stats.R` (`:45`)
- **util:** `R_packages.R`; `amazon_web_credentials.R` (the Athena `con`); `cache_frames.R`; `pipeline_status.R` (`PIPELINE_MAP` `:16-112`); xpass/xtd normalization fits; `normal_script.R` and `nbinom_script.R` (odds examples); `rdata_push_AWS.R`; `xgboost_converter.R` (a basketball model, not in the NFL chain).
- **Scrapers:** `get_auth()` reads the `__client` Clerk cookie from the secret `pff-api-cookies`, then mints 60-second `__session` tokens and calls `pff_get` with retries (e.g. `lambdas/scrapers/passing-tip-scraper.py:22-90`). Each scraper writes a season (and sometimes week) parquet under `s3://nfl-pff-data-lucas/data/<table>/`. The cleaners cast int columns to float. `upload_lambdas.ps1` is dry-run by default.
- **nfl_dbt:** staging pass-throughs; the key marts are `vw_team_passing_summary` (`starting_qb` = most dropbacks), `vw_opponents`, the `vw_combined_*` splits and `vw_total_snaps`; 21 singular tests; `exposures.yml` is generated.
- **scripts / tests:** `pff_auth_probe.py`, `pff_lambda_local_test.py`, `build_lineage.py` (writes LINEAGE.md); tests for the cleaners, lineage freshness and session safety.
