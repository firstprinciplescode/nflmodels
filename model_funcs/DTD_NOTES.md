# D/ST touchdowns — what they go with, what predicts them, and how `dtd_func.R` works

Written 2026-09-20. "D/ST touchdown" here means any touchdown that is not scored by an offense:
interception returns, fumble returns, punt returns, kick returns, blocked punt / field-goal returns,
and special-teams fumble recoveries.

Three new files. Nothing that already existed was changed.

| file | what it is |
|---|---|
| `data_build/dtd_games_build.R` | builds the side frame (one function, `dtd_games_build()`; about 2 minutes) |
| `cache/dtd_games.rds`, `cache/dtd_plays.rds` | the frame (5,522 offense-games, 2016-2025) and the 807 touchdown plays |
| `model_funcs/dtd_func.R` | `dtd_rates`, `dtd_history`, `dtd_weight`, `dtd_project`, `dtd_game`, `kneel_project` |

```r
dtd_games <- readRDS("cache/dtd_games.rds")     # once per session; the file never loads it for you
source("model_funcs/dtd_func.R")                # nothing runs on source
dtd_game(home = list(off = "DENNix-2025", def = "DEN2025", ints = 0.42, sacks = 1.73),
         away = list(off = "JAXLawrence-2025", def = "JAX2025", ints = 0.70, sacks = 2.55), home_fav = 3.5)
```

## Why a new frame and not combined_pbp

`combined_pbp` holds pass and run plays only. It has the interception and fumble returns (my counts
match it exactly in all ten seasons: 56, 79, 66, 67, 52, 57, 63, 65, 48, 49) but no punts, kickoffs,
field goals, kneels or spikes — a quarter of these touchdowns are invisible to it. Taking kneels out of
the training data was the right call; the fix is not a second play-by-play, it is ONE game-level side
frame with the same keys as `qb_stats_df_final` (season, week, posteam, qbgrp_ssn, def_ssn), built from
the same nflverse source `pbp_nfl_base.R` starts from. All 5,522 of your rows found their game.
Two things I had to handle: your playoff weeks are 28 / 29 / 30 / 32, and `qb_stats_df_final` writes
`posteam` in your codes (BLT) but `defteam` in nflverse codes (BAL) — so the defense is read off `def_ssn`.

A row says: "in this game this offense's team GAVE UP these D/ST touchdowns to the other team", plus the
exposure (interceptions, sacks, fumbles, punts, kickoffs ...) and this team's own kneels and spikes. To
see what a team SCORED, read the rows where it is the defense.

## The anatomy (2016-2025, 807 touchdowns)

- **0.146 per team-game over ten years, 0.132 over the last three.** One game in four has at least one.
  Worth about 1.9 points per game. The level STEPPED DOWN after 2019 (defensive part 0.126 -> 0.099 per
  team-game) and has been flat since (2020-25 slope -0.7% a year, p = .81). Two thirds of the step is fewer
  fumble-return TDs (strip-sack TDs per sack 1.28% -> 0.73%), one third fewer interceptions (0.81 -> 0.73 per
  team-game). Use the last three seasons for the level, not ten and not one.
- **Interceptions are the biggest source, not fumbles.** Interception returns 47%, fumble returns 27%,
  special teams 25% (punt returns 9.5%, kick returns 8.3%, blocked punts 4.5%, blocked FGs 1.5%, recoveries 1.6%).
- **56% of fumble-return TDs are strip-sacks.** Ball-carrier fumbles are only 4-7% of the whole target. So a
  rusher's fumble grade speaks to about 5% of it; sacks speak to 15%; interceptions to 47%.
- **90% of defensive touchdowns start on a dropback.** A dropback is about six times as dangerous as a run.
- **The conversion rates are league numbers, not team skills.** 8.9% of interceptions are returned for a TD (no
  difference by season, p = .73). Fumble returns are the part that changed: per sack 1.28% in 2016-19 and 0.73%
  since 2020, other fumbles lost 5.2% since 2020 — so the function measures those two on the last six seasons.
  **One interception is worth about 12 sacks.** No defense keeps a better return rate than another (p = .86).
- **It is Poisson.** Variance / mean = 1.03. The two teams in a game are independent of each other
  (r = 0.00), and a team's defensive and special-teams touchdowns are independent too. So the chance of at
  least one is `1 - exp(-expected)`, and the game is the sum of the two sides.

## What it goes with in the same game (NOT usable before the game)

Correlation with the DEFENSIVE part (interception + fumble returns): turnovers 0.34, interceptions 0.27, plays
run while behind by 8+ 0.22, fumbles lost 0.21, strip-sack fumbles 0.20, dropbacks 0.15, sacks taken 0.11
(against all D/ST touchdowns the same list reads 0.31 / 0.26 / - / 0.18 / 0.17 / 0.16 / 0.11). It is a
turnover count times a constant: about 0.08-0.11
defensive TDs per turnover, in a straight line. Pick-sixes lean to trailing offenses (32% come when behind
by 8+) — but only because trailing teams throw more and throw more picks. The return rate itself is the
same whatever the score (8.7% behind by 8+ vs 9.0% otherwise).

## What predicts it BEFORE the game (forward test: predict 2020-2025 using only earlier seasons)

Honest headline: like field-goal attempts, this is mostly unpredictable. The best honest models explain
under 1% of the game-to-game variation. What is real is small, and it is all on the defensive part.

| input | verdict | size |
|---|---|---|
| **Your projected interceptions and sacks** | the best input by construction (the chain) | 0.0888 per interception + 0.0073 per sack + 0.015 flat |
| **Spread** | real: right sign in 10 of 10 seasons, survives the multiple-test correction | about 3% per point on the defensive part; a 7-point underdog's offense x1.22, a 7-point favourite's x0.78 (x1.15 / x0.85 when you feed it your own interceptions and sacks). Zero on special teams |
| **QB-team status** | real but small | returning pair (10+ games last year) x0.86, new pair x1.08, mid-season takeover x1.32 |
| **Defense's pressure** (prior-season pressure / QB-hit rate) | the one defense-side candidate (passes the correction inside its own family of 15 tests only) | top-8 pressure defense scores x1.35 the next year; carried as `pressure =` at a slope of 0.75 |
| An entity's own return-TD history | barely | weight 0.10 at most on either side (measured 0.095, range includes 0) |
| A defense's prior sack rank, interception rank, turnover-worthy rank, takeaway rate | nothing | p from .10 to .91 |
| **Special teams: team history, returner history, coverage unit, the 2025 punt-return spike (15), the new kickoff rules** | nothing | returned share went 36% -> 74% in 2025 and kick-return TDs per game did not move. Flat 0.037 |
| Total line | not proven | the COUNT does not reliably rise when the total is low (fails the correction). The SHARE does: 8% of touchdowns under a 40 total vs 4% at 48+ |
| Weather, dome, wind, rain, cold, home / away, week, playoffs | nothing | |
| Reading the stat off comparable-games pools | **hurts** | one pool in four has under 100 games, where 0.07 and 0.22 are the same number |

**The Browns hunch** ("bad QB play gives them up") is right in direction and much smaller than it feels:
a bottom-quartile interception offense gives up about 1.05-1.15x the next year (not distinguishable from 1);
a mid-season takeover QB about 1.3x. That is roughly 2.1 defensive TDs given up over 17 games instead of 1.9 — not 4 or 5.
What DOES persist is upstream: a QB-team's interception rate carries over at 0.25-0.41 and its sack rate
at 0.42-0.65 (the return-TD count itself only 0.1). That is why feeding your own interception and sack
projections into the chain beats any history of the touchdowns themselves.

## How `dtd_project()` builds the number

Two modes, chosen by what you give it.

1. **Chain** (you pass `ints =` and / or `sacks =`, your projections for the offense that could give it up):
   `interceptions x 0.0888 + sacks x 0.0073 + 0.015`, times a reduced spread factor (two thirds of the slope,
   because your projections already carry team quality; what is left is game script). History and status
   adjustments are switched off — both sides' tendencies are already inside your numbers.
2. **Level** (no projections): last-three-seasons league level 0.0947 x offense adjustment (QB-team status,
   plus own history at weight <= 0.10, held inside 0.75-1.35) x defense adjustment (own history at weight
   <= 0.10, optional pressure tilt, held inside 0.8-1.3) x the full spread factor. A history with fewer than
   10 games a season is not used at all (1 TD in 1 game reads 11x league and must not move the number).
   At league-average inputs the two modes agree to within 4% (chain 0.098, level 0.095).

Then add the flat special-teams 0.0371. Every constant is measured from the frame when the file first
runs (`dtd_rates()` prints them); nothing is typed in by hand except the 0.10 cap, the 0.75 pressure slope
and the two-thirds / 0.65 "do not double count the spread" reductions, which come from the re-check.

`dtd_game()` runs both directions and adds them (the sides are independent).

## Kneel-downs

Not in `combined_pbp`, rightly. They follow the result: 0.19 a game for the loser, 1.36 for the winner,
-1.09 yards each; 2.9% of official rush attempts. `kneel_project(fav = 3.5)` turns a spread (or
`p_win =`) into expected kneels and yards. Add them to the QB's OFFICIAL rushing line only.

## How it was tested

Six separate analyses on the same frame (anatomy, offense side, defense side, special teams, pre-game
context, comparable-games format), each then recomputed from scratch by an independent reviewer who also
hunted for leaks (same-game columns used as predictors, test-season data in the history) — none found, all
six reproduced to the digit. About 750 tests were run in total; after the multiple-test correction only the
spread stands on its own; the pressure tilt is the one defense-side candidate. The finished files then went
through a separate numbers check, which found two real problems that are fixed: short histories (under 10
games) were moving the number, and the chain ran 11% hot because it pooled the 2016-19 fumble-return rates. Scripts and outputs: scratchpad `dtd/<angle>/` and
`dtd/<angle>/verify/`.

## Known limits

- The frame ends with 2025. Rebuild with `dtd_games_build()` after `qb_stats_df_final` gains a season.
- Sanity range for one team: 0.08-0.22. If a projection lands outside it, the inputs are off.
- The spread coefficient's forward skill is +0.7% with a range that touches zero: a small real tilt, not an edge.
- Side flag found on the way (not touched): `stats_comp_pool()` reads `$QB`, but the pressure-lens
  defense function returns its first column as `Defense`, so that one pool can collapse to the defense
  itself on a fresh load.
