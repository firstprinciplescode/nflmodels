# Evaluation tools: what to use, and how

Three files live here. Load them in the RStudio console **after your unit chains are in
session**. Nothing in them changes your session or writes to disk.

```r
source("pff_stats/evaluation/waterfall_bridge.R")   # team and player-vs-player impact
source("pff_stats/evaluation/impact_bridge.R")      # player cards (look at one player)
source("pff_stats/evaluation/roster_swap_lab.R")    # optional: swap inside your 2026 frames
```

| I want to... | Use |
|---|---|
| See a whole team's 2025 to 2026 change at a glance | `waterfall()` |
| Compare player A with player B (injuries, one-offs) | `wf_pvp()` |
| See what one player adds to his team (injury, Waddle-type add) | `wf_without()` |
| See the ROOKIE and VET BACKUP numbers for a position | `wf_priors()` |
| Look at one player's percentiles season by season | the cards in `impact_bridge.R` |

---

## waterfall_bridge.R — the impact engine

Every impact number comes from one **fit**: for every team-season 2016-2025, each
position room's value (your percentiles, your seat counts) against that team's PFF
totals. It turns "a room moved 10 percentile points" into YPA, completion %, passer
rating, sack %, pressure % (passing) or YPC, rush TD per game (rushing).

**Two currencies are always shown side by side:**
- **c1** = your season percentiles
- **c3** = your opponent-adjusted percentiles

**Sides** (used by `waterfall()` and `wf_fit()`):

| Side | What it predicts | Rooms that feed it |
|---|---|---|
| `pass_def` | passing ALLOWED | corners, slot, safeties, linebackers in coverage (man and zone), pass rush |
| `rush_def` | rushing ALLOWED | run defense DI, ED, LB, S |
| `pass_off` | the team's own passing | OL pass blocking (true pass sets, all 5 slots), receiving corps |
| `rush_off` | the team's own rushing | OL run blocking (gap, zone), the backs |

**Units** (used by `wf_pvp()`, `wf_without()`, `wf_priors()`, `wf_swap()`):
`secondary`, `pass_rush`, `run_defense`, `pass_block`, `run_block`, `receiving`, `rushing`.

### waterfall(team, side)
The whole team, 2025 as played versus 2026 from your frames.

```r
waterfall("KC", "pass_def")
waterfall("DEN", "pass_off")
```

Prints, in order: the **2025 to 2026 summary** (2025 actual, change with 95% range,
2026 projected, both currencies), then each position's 2025 and 2026 value, then what
each position's change does to the outcomes, then **the summary again**.

- OL: 2025 = the starters by games started; 2026 = X x starter + (1 - X) x your backup
  level, X = average OL starter availability from your data, on true pass sets: a starter's weeks in `tps_pass_block_summary` (your >= 16 TPS guideline) / his TEAM'S weeks in it (about 0.81). Fixed 2026-09-18 -- it used to divide by 17, but the gate drops ~6 of 17 weeks for the whole team, so a starter who played every game read ~0.70 and X read 0.506.
- Every other room: full health; your seat depth is the rotation.
- `show_seats = TRUE` adds who sits in each 2026 position. `ol_avail = "player"` prices
  each OL starter at his own availability; `"full"` sets X = 1.
- Trust the total and its range. Single position rows are indicative.

### wf_pvp(units, a, b)
Player vs player. B steps into A's role (A's position and A's playing time). No team,
no seats. Pairs go in order: a[1] vs b[1], a[2] vs b[2].

```r
wf_pvp("secondary", a = c("Jaylen Watson", "Trent McDuffie"), b = c("ROOKIE", "L'Jarius Sneed"))
wf_pvp(c("secondary", "run_defense"), a = c("Chamarri Conner", "Bryan Cook"), b = c("Jaden Hicks", "Alohi Gilman"))
wf_pvp(c("pass_block", "run_block"), a = "Josh Simmons", b = "Jaylon Moore")       # any OL slot
wf_pvp("secondary", a = "Trent McDuffie", b = "VET BACKUP")
```

- For b (or a) use a player's name, `ROOKIE`, `VET BACKUP`, or your own number (0-1).
- First table: each player's 2026 full-health percentile in A's role, the role's share of
  its room, and the ROOKIE and VET BACKUP numbers for that role.
- Second table: the change in outcomes, B minus A, with a 95% range, c1 and c3.
- Defense: a number above 0 means B allows MORE. Offense: above 0 means B gains more.
- Put every unit a player touches: safeties in `c("secondary", "run_defense")`,
  linemen in `c("pass_block", "run_block")`.

### wf_without(team, unit, player)
What one player adds to his team as it lines up: the team with him minus without him.
Without him, his playing time is spread over the rest of the room (so everyone behind
him moves up), or on the OL his slot goes to your backup level.

```r
wf_without("DEN", "receiving", "Jaylen Waddle")
wf_without("KC", "pass_rush", "Chris Jones")      # an injury
wf_without("KC", "pass_block", "Josh Simmons")
```

### wf_priors(unit)
The two reference players for every position of a unit, in percentile points.

```r
wf_priors("secondary")
wf_priors("pass_block")
```

- **ROOKIE** = your entry-year prior.
- **VET BACKUP** = a veteran with little playing time (your secondary `vet_grade`; the
  same rule measured on your seats for the other units).

### Also in the file
| Function | What it does |
|---|---|
| `wf_fit(side)` | The fit numbers: change per +10 percentile points at each room, with range and VIF |
| `wf_rooms(team, season, side)` | Who sat in each history room that season |
| `wf_rooms26(team, side)` | Who sits in each 2026 room, straight from your member frames |
| `wf_swap(team, unit, out, inn)` | One player for another in one team's exact 2026 seat |

**Stops on purpose** (it prints what is wrong instead of giving a wrong number): a frame
missing from your session, a team-season whose rooms and PFF outcomes do not line up, a
name that matches more than one player.

---

## impact_bridge.R — player cards

Use it to **look at one player** before a swap. Each card reads your season summaries
(the heatmap tables): percentiles within position and season, one card per position,
the last two seasons by default, and below-gate seasons shown instead of hidden.

```r
cov_card("L'Jarius Sneed")      # coverage: man / zone (/ slot)
ol_card("Josh Simmons")         # any lineman: pass blocking (true pass sets) + run blocking (gap, zone)
pr_card("George Karlaftis")     # pass rush
rd_card("Bryan Cook")           # run defense (MT% and tackle depth flipped so higher = better)
rb_card("Isiah Pacheco")        # rushing
wr_card("Jaylen Waddle")        # receiving: your machine percentiles + role-aware over-expected
```

- `card_seasons = 2023:2025` picks the card window.
- `ot_card()` still works; it is the same as `ol_card()`.
- Card percentiles come from your season summaries; `wf_pvp()` and `waterfall()` use your
  league machine's 2026 numbers. Same player, different pools and gates, so a card number
  and a `wf_pvp()` number are not the same scale. Cards are for looking; `wf_pvp()` is
  for the outcome change.
- `player_swap()` here is a shortcut to `wf_swap()`.
- `rookie_anchor(unit)` prints your entry-year priors (same idea as `wf_priors()`).

**Retired (they stop and tell you what to use):** `bridge_fit`, `bridge_delta`,
`bridge_fit_stats`, `bridge_stats_delta`, `bridge_wr1_add`. They used top-2 rooms, one
unit at a time, no safeties in run defense, and `combined_pbp`.

---

## roster_swap_lab.R — optional

Shows and swaps players inside your 2026 member frames, in percentile points only (no
outcome translation; use `wf_pvp()` for that).

```r
lab_team("KC")                                                        # who the machine prices, every unit
lab_swap("secondary", "KC", out = "Trent McDuffie", in_name = "L'Jarius Sneed")
lab_comp("L'Jarius Sneed", "secondary")                               # his record + nearest comps
```

`lab_wr1_swap()` is retired (it read `combined_pbp`).
