# wf_ cheat sheet — the waterfall bridge in one page

Written 2026-09-18. The long version is [EVALUATION_TOOLS.md](EVALUATION_TOOLS.md); this is the
"what do I type" version.

```r
source("pff_stats/evaluation/waterfall_bridge.R")   # needs your unit chains in session
```

## What it is

One regression underneath everything. For every team-season 2016-2025 it takes each
**position room's percentile** (your percentiles, weighted by your seat counts) and fits it
against that team's PFF outcomes. That turns "this room moved 10 percentile points" into:

- **passing:** YPA, completion %, passer rating, sack %, pressure %
- **rushing:** YPC, rush TD per game

Every number prints in two currencies:

| | meaning |
|---|---|
| **c1** | your season percentiles |
| **c3** | your opponent-adjusted percentiles |

## c1 vs c3 — what the two currencies actually are

Two ways of grading the same players. The regression is run once with each, so every table
shows both.

**c1 — currency one: the season percentile.** The player's season grade, percentile-ranked
within his position band that season (pass rush: true-pass-set grade among all ED, from
`cur_lg` / `prush_tps_season_pctl_sos`; the other units have their own `*_season_pctl_sos`).
It says how good his production was. It does not know who he played.

**c3 — currency three: opponent-adjusted, by common opponent.** Defined in
`pff_stats/pass_rush/league_pass_rush_evaluating_currency_three.R` (each unit has its own copy).

1. `vsopp_rank` — this game's grade, ranked among **every same-position player who faced that
   same offense** (`qbgrp_ssn`) that season. "Your day, vs everyone who faced the same OL."
2. Season value = the **median** of those game ranks (`med_vsopp`).
3. `c3_pctl` = that median, percentile-ranked within band x season.

Rulings baked in: **rank space, no expectation model, no OE columns**; blowouts compress on
purpose (a 5-sack demolition and a barely-won day both count as "a good day"); canon gates
(X = 3 TPS snaps, G = 6 games); and the firewall — **c3 never feeds any SoS slate**, because the
SoS measures schedules with unadjusted quality.

How to read them together:

| pattern | meaning |
|---|---|
| c1 well above c3 | his numbers were schedule-helped (feasted on bad opponents) |
| c3 well above c1 | he did it against tough opponents |
| c1 and c3 agree | the read is robust — trust it |
| they diverge sharply in ONE room | usually a thin seat (SCB, LB man); trust the total, not that bar |

Worked example (JAX `pass_def`, 2026-09-18): ED room c1 57.2 -> 74.2 (+17.0), c3 54.4 -> 72.4
(+18.0) — both currencies agree, worth **+1.46 pressure pts / +0.37 sack pts**. Same table, DI
room c1 42.4 -> 32.7 costs **-0.69 pressure pts**, so the pass rush nets about +0.8, not +1.5.
Always net the rooms, then read the summary block for the whole unit.

Scaling a `wf_pvp` result by hand: the effect is proportional to **(B - A gap) x (share of the
room)**. A run at the wrong share or against the wrong A can be rescaled: e.g. VET BACKUP ->
Gardeck was 29.6 pts x 0.08 share = 2.4 room-points -> +0.205 pressure pts; Smoot (11.3) ->
Gardeck (65.5) at Smoot's real 0.254 share = 13.8 room-points -> about +1.2. Note the outcome
columns are **percentage points** (+0.205 = two tenths of a point, not 20.5%). You can also type
a percentile directly: `wf_pvp("pass_rush", a = 0.113, b = "Dennis Gardeck")`.

## The functions

| function | the question it answers | example |
|---|---|---|
| `waterfall(team, side)` | How does the whole unit change, 2025 as played -> 2026 from the member frames? | `waterfall("CIN", "pass_def")` |
| `wf_pvp(units, a, b)` | Player B steps into player A's exact job. What changes? | `wf_pvp("secondary", a = "Trent McDuffie", b = "L'Jarius Sneed")` |
| `wf_without(team, unit, player)` | What is this player worth to his team as it lines up? (injuries) | `wf_without("KC", "pass_rush", "Chris Jones")` |
| `wf_swap(team, unit, out, inn)` | One player for another, in that team's exact 2026 seat | `wf_swap("KC", "secondary", "Jaylen Watson", "L'Jarius Sneed")` |
| `wf_rooms(team, season, side)` | Who sat in each room that season: snaps, grade, c3 | `wf_rooms("JAX", 2025, "pass_def")` |
| `wf_rooms26(team, side)` | Who sits in each room now (your 2026 member frames) | `wf_rooms26("CIN", "pass_def")` |
| `wf_priors(unit)` | The ROOKIE and VET BACKUP reference percentiles per position | `wf_priors("pass_rush")` |
| `wf_fit(side)` | The regression itself: effect per +10 percentile points per room, range, VIF | `wf_fit("pass_def")` |

Useful options:

- `waterfall(..., show_seats = TRUE)` prints who sits in each 2026 position under the bars.
- `waterfall(..., ol_avail = "player")` prices each OL starter at his own availability; `"full"` = full health.
- In `wf_pvp`, `b` (or `a`) can be a player name, `"ROOKIE"`, `"VET BACKUP"`, or your own number 0-1.

## Two vocabularies — don't mix them

| | values | used by |
|---|---|---|
| **sides** | `pass_def`, `rush_def`, `pass_off`, `rush_off` | `waterfall`, `wf_fit`, `wf_rooms`, `wf_rooms26` |
| **units** | `secondary`, `pass_rush`, `run_defense`, `pass_block`, `run_block`, `receiving`, `rushing` | `wf_pvp`, `wf_without`, `wf_swap`, `wf_priors` |

A player who touches more than one unit gets all of them:

```r
wf_pvp(c("secondary", "run_defense"), a = "Chamarri Conner", b = "Jaden Hicks")     # safeties
wf_pvp(c("pass_block", "run_block"),  a = "Josh Simmons",    b = "Jaylon Moore")    # linemen
```

## Reading the output

- **Defense:** a number above 0 = B allows **more** than A (B is worse).
- **Offense:** a number above 0 = B gains more.
- **Trust the total and its 95% range.** Single position rows are indicative only — rooms are
  correlated (man and zone coverage share players).
- `league avg` in `waterfall` is the same change averaged over all 32 teams — a sanity check.
- Rooms that read identical 2025 -> 2026 are the same players carrying their 2025 percentile
  forward. That is by construction, not a finding.

## It stops on purpose

It prints what is wrong instead of giving a wrong number:

- a frame missing from your session ("missing in session: ...") — source the unit file that builds it
- a name that matches two players
- a player who is **not in the 2026 frame**. `wf_pvp` reads A's role (position, playing time,
  share of the room) from the 2026 member frames, which are built from `load_rosters(2026)`.
  A free agent has no role to read, so the pair is skipped. Example (2026-09-18):
  `(pass_rush: Dawuane Smoot not in members_pa -- pair 1 skipped here)` — Smoot is unsigned.

  Work around it by making the rostered player A:

  ```r
  wf_pvp("pass_rush", a = "Dennis Gardeck", b = "VET BACKUP")
  wf_without("JAX", "pass_rush", "Dennis Gardeck")
  wf_rooms("JAX", 2025, "pass_def")          # what the departed player's seat was worth last year
  ```

## Player cards — look before you price

`impact_bridge.R` holds the cards: one player's percentiles season by season.

```r
source("pff_stats/evaluation/impact_bridge.R")
cov_card("L'Jarius Sneed")     # coverage: man / zone (/ slot)
ol_card("Josh Simmons")        # pass blocking (true pass sets) + run blocking (gap, zone)
pr_card("George Karlaftis")    # pass rush
rd_card("Bryan Cook")          # run defense
rb_card("Isiah Pacheco")       # rushing
wr_card("Jaylen Waddle")       # receiving
```

**Card numbers and `wf_pvp` numbers are different scales** (different pools and gates). Cards are
for looking; `wf_pvp` and `waterfall` are for the outcome change. Don't compare one to the other.

## Things learned the hard way

- **The OL availability bug (fixed 2026-09-18).** `waterfall()` prices each 2026 OL slot as
  `X x starter + (1 - X) x backup level`. X is the league-average starter availability, on TRUE
  PASS SETS (your default): a starter's weeks in `tps_pass_block_summary` (your >= 16 TPS
  guideline). Until 2026-09-18 that count was divided by **17** -- but the gate drops ~6 of 17
  weeks for the WHOLE TEAM (league mean 10.7 qualifying weeks), so a man who played every game
  read ~0.70 (DEN 2025: Bolles 12 rows, 17 games) and X came out **0.506**: the same five players
  "got worse" every year. The denominator is now **his team's qualifying weeks**, so the gate sits
  on both sides of the rate: Bolles 12/12 = 1.00, X = **0.811** (C .85, LG .81, LT .77, RG .82,
  RT .80). Same table, same guideline, no other knob. A wall stops it if X ever reads under 0.65.
  `wf_pvp` on `pass_block` / `run_block` used the same X for the slot share, so old OL pvp numbers
  were ~38% too small. **Rule: if a frame is gated, the gate goes on both sides of any rate.**
- **The receiving share bug in `wf_pvp` (fixed 2026-09-19).** The fit prices the receiving corps as
  ONE room per team (a usage-weighted mean over all eight seats). `wf_pvp` measured the player's
  share inside his BAND instead, so an RB read as 75% of "the room" when he is ~10% of the corps:
  every receiving `wf_pvp` was roughly 7x too large (ROOKIE for Harvey showed -0.6 to -1.1 YPA;
  the right answer is -0.08 to -0.16). The tell was that it disagreed with `wf_without` by an
  order of magnitude. Coverage, pass rush, run defense and rushing were never affected -- their
  fit inputs really are per band (or the whole backs room). Any receiving pvp run before this
  date should be re-run.
- Even with the right X, a line that was fully healthy last year projects DOWN under the default
  lens: 2025 is "as played" (Bolles 17 of 17), 2026 assumes league-average health (81%). That is
  the design, not a bug. If you believe the five are healthy, use `ol_avail = "full"`; for each
  man's own history use `ol_avail = "player"`.
- Availability everywhere else is "weeks over the unit's gate / 17". Starter medians: pass block
  .84, run block .88, run defense .84, receiving .82, rushing .82, pass rush .77 (top seat .95).
  **Coverage is the exception: man .53, zone .69**, because "played" there is the split floor
  (>= 6 man / >= 13 zone snaps, stamped 2026-08-21) -- a healthy corner on a zone-heavy team fails
  the man floor in weeks his team barely called man. It does not touch the waterfall (non-OL rooms
  are priced at full health) but it does sit inside `grade_p` / `supp_p` / `adj_p`.

- `waterfall()` only bridges **2025 -> 2026**. For any other pair of seasons, pull the room
  values yourself: `f <- .wf_fit_one("pass_def", "c3"); f$data` has every team-season's room
  percentiles, `.wf_inputs_2026("pass_def", "c3", "a")` has 2026, and `.wf_eff(f, v_new - v_old)`
  prices the gap.
- One-seat rooms (SCB especially) sit at the snap floor — a single player on ~50 snaps can
  swing c3 by 90 points while c1 moves the other way. Read those rows as low-information.
- The `[passing] PFF team-weeks with no match ... 2026` warning is just 2026 Week 1 not yet in
  `combined_grade_epa_summary`. Harmless.
- Running it outside RStudio: the Sep 15 `nfl_the_everything_workspace.RData` holds every frame
  it needs; it only lacks `run_athena_query`, which has to be defined before sourcing.
