####
#### waterfall_bridge.R -- v2 2026-09-15 (Claude, built to Andy's spec; v1 2026-09-14).
#### Standalone. Writes nothing to disk. Never loads cache files. Never reads
#### combined_pbp. On first use it pulls two PFF team-total tables from Athena
#### through YOUR run_athena_query and keeps them in a private memo (.wf_memo).
####
#### WHAT IT DOES
####  1. ROOMS. For every team-season (playoffs included) it fills YOUR seats:
####       coverage   YOUR historical committee, committee_hist_sec_lg: CB 3 / SCB 1 /
####                  S 3 / LB 2 per split, ranked by that team's split snaps in floor
####                  weeks. Seats first, then the percentiles are joined on; a seat
####                  with no percentile is priced by your faced fill law (rookie / vet prior)
####       pass rush  ED 4 / DI 4 by true-pass-set snaps (qual_lg), gated rushers only,
####                  the way your rotation seats them
####       run def    DI 5 / ED 5 / LB 3 / S 4 by run snaps at band positions (>= 3 a game),
####                  gated players only, the way your rotation seats them
####       OL         LT LG C RG RT: each game's starter at the slot on true pass sets, by games started
####       receiving  corps of 8 by routes. Seats first; a receiver with no percentile
####                  that season stays in his seat unscored (your observed-only law)
####       rushing    2 backs by attempts in 4+ attempt games. Seats first; a back with
####                  no percentile is priced at your entry-year prior (your fill law)
####     Team = the team on the game rows, so a traded player sits for EACH team he
####     played for, with his snaps there.
####     Room value = the seats' percentiles weighted by that volume. Both currencies:
####     season percentiles (c1) and opponent-adjusted c3.
####  2. FIT. One regression per outcome with every room that touches it:
####       pass_def  coverage (man + zone grade per band; zone completion suppression
####                 for CB and S per facet_law_mz) + pass rush (ED, DI)
####                 -> PFF passing ALLOWED: YPA, comp %, passer rating, sack rate, pressure rate
####       rush_def  run defense DI / ED / LB / S (grade, stop %) -> PFF rushing ALLOWED: YPC, rush TD/g
####       pass_off  OL (true-pass-set grade per slot) + receiving corps (man / zone grade
####                 and YPRR) -> own passing. Seasons 2018+ (receiving percentiles start 2018).
####                 OL pressure is left out, per your 2026-08-08 ruling.
####       rush_off  OL (gap, zone) + backs (run grade, missed tackles forced) -> own rushing
####     Outcomes = PFF team totals (nfl_data.passing_pressure / rushing_summary), opponent
####     from YOUR combined_grade_epa_summary. No QB term (per Andy). No xTD (PFF has none).
####  3. WATERFALL. waterfall("KC", "pass_def"): start at the team's 2025 PFF line;
####     one bar per room input; end at the projection, with a range.
####     THE POSITION, NOT THE PLAYER.
####     OL (Andy 2026-09-15):
####       Everything on TRUE PASS SETS (your default): a game counts when it is a row of
####       tps_pass_block_summary (>= 16 true-pass-set snaps, pff_pass_block_AWS.R).
####       2025 = REAL GAMES: each game's starter at the slot (that game's true-pass-set
####              snap leader there), weighted by games started. KC LT 2025 = 8/17 Simmons +
####              9/17 the backup who started. A starter with no percentile at the slot is
####              priced by your faced fill law: the slot's entry-year prior (rookie_prior),
####              c3 at your slot c3 prior.
####       2026 = X x starter + (1 - X) x backup. X = league-average OL starter
####              availability FROM DATA, TRUE PASS SETS (your default, your guideline only):
####              job holder = season true-pass-set snap leader per team x season x slot; his
####              REG weeks in tps_pass_block_summary / HIS TEAM'S REG weeks in that frame
####              (the >= 16 TPS gate applies to BOTH sides of the rate);
####              averaged over 2023-25. One X for the pass and run lens. Backup = your
####              LEAGUE 2025 backup-start level at the slot (the same for every team), read
####              back from your pricing law (priced = avail x full + (1 - avail) x repl).
####       So a starter who missed half of 2025 and returns RAISES the slot.
####       ol_avail = "player": each starter at his own availability; "full": X = 1.
####     EVERY OTHER UNIT: no starter/backup split -- your seat depth (CB 3, DI 5, corps 8,
####     ...) already is how deep the squad rolls. 2025 = the seats as played (that team's
####     snaps; a seat with no percentile at your prior); 2026 = your seat-holders at full
####     health, weighted by your member usage (best of 2025/2024).
####     'league avg change' prints the same change averaged over all 32 teams as a check.
####     The 2026 seats are printed under the bars, so you see who actually sits.
####
#### RUN (RStudio console, after the unit chains are in session):
####   source("pff_stats/evaluation/waterfall_bridge.R")
####   wf_fit("pass_def")                  # fit numbers per 10 percentile points, both currencies
####   waterfall("KC", "pass_def")         # KC 2025 -> 2026 passing allowed
####   waterfall("KC", "rush_def")
####   waterfall("KC", "pass_off")         # KC OL + corps -> own passing
####   waterfall("DEN", "pass_off")        # Waddle lands here
####   wf_rooms("KC", 2025, "pass_def")    # who sat in each 2025 room
####   wf_rooms26("KC", "pass_def")        # who sits in each 2026 room
####   PLAYER VS PLAYER (injuries, one-offs; no team, no seats):
####   wf_pvp("secondary", a = "Trent McDuffie", b = "L'Jarius Sneed")
####   wf_pvp(c("pass_block", "run_block"), a = "Creed Humphrey", b = "BACKUP")   # any OL slot
####   wf_priors("secondary")                # the ROOKIE and VET BACKUP reference players
####   wf_without("DEN", "receiving", "Jaylen Waddle")   # what one player adds to his team
####
#### NEEDS IN SESSION (each function stops with the missing names):
####   run_athena_query, combined_grade_epa_summary
####   coverage:  committee_hist_sec_lg, cov_pctl_sec_lg, cov_c3_pctl, members_cv,
####              prior_law_sec_lg, c3_entry_prior_cv (ent_sec_lg for the rookie arm)
####   pass rush: qual_lg (else full_pass_rush_qbgrp), cur_lg (else prush_tps_season_pctl_sos),
####              prush_c3_pctl, members_pa
####   run def:   run_defense_qbgrp, rundef_season_pctl_sos, rd_c3_pctl, members_ra
####   OL:        tps_pass_block_summary, tps_pass_block_player_season_summary,
####              gap_player_season_summary, zone_player_season_summary,
####              pblk_c3_pctl, rblk_c3_pctl, slot_value_26_pb_build, slot_value_26_rb_build,
####              rookie_prior, c3_rookie_prior_ol, c3_scheme_prior_rb
####   receiving: receiving_func_base, rec_season_pctl_sos, rec_c3_pctl, members_rc
####   rushing:   rushing_qbgrp, rush_season_pctl_sos, ru_c3_pctl, memb_lg_ru
####              (rush_rookie_prior, c3_rookie_prior_ru used for the fill when present)
####
#### DIRECTION: every input percentile reads higher = better player.
#### HONESTY: cross-sectional team-seasons. The fit says what a room move has been
#### worth on average, not what it will be worth for one team. Rooms are correlated
#### (man and zone share players), so read the TOTAL and its range; single bars are
#### indicative (wf_fit prints each input's VIF).
####

library(dplyr)

.wf_memo <- new.env()

# ---------------------------------------------------------------- helpers ----

.wf_need <- function(frames) {
  miss <- frames[!vapply(frames, exists, logical(1), envir = .GlobalEnv)]
  if (length(miss))
    stop("missing in session: ", paste(miss, collapse = ", "),
         " -- source the unit files that build them first.")
  invisible(NULL)
}
.wf_has <- function(f) exists(f, envir = .GlobalEnv)
.wf_get <- function(f) {
  d <- tibble::as_tibble(get(f, envir = .GlobalEnv)) %>% ungroup()
  if ("season" %in% names(d)) d$season <- as.integer(d$season)
  d
}

# stop loudly if a table is not one row per key (PFF name drift splits a season)
.wf_unique <- function(d, keys, what) {
  dup <- d %>% count(across(all_of(keys))) %>% filter(n > 1)
  if (nrow(dup)) {
    print(as.data.frame(utils::head(dup, 20)), row.names = FALSE)
    stop(what, " is not one row per ", paste(keys, collapse = " x "),
         " (", nrow(dup), " duplicated keys, shown above).")
  }
  invisible(d)
}

# weighted mean that skips NA values and zero / NA weights
.wf_wm <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) NA_real_ else sum(x[ok] * w[ok]) / sum(w[ok])
}

# everything to PFF team codes (member frames come from nflreadr rosters)
.wf_code <- function(x) {
  x <- toupper(as.character(x))
  x[x %in% c("ARI", "AZ")] <- "ARZ"
  x[x == "BAL"] <- "BLT"
  x[x == "CLE"] <- "CLV"
  x[x == "HOU"] <- "HST"
  x[x == "WSH"] <- "WAS"
  x[x == "JAC"] <- "JAX"
  x[x == "LAR"] <- "LA"
  x
}

# season-aware PFF code for every HISTORICAL team column: your house recode (every
# PFF loader does it) -- Chargers SD in 2016, Raiders OAK through 2019 -- on top of
# .wf_code. Checked against Athena 2026-09-15: passing_pressure, rushing_summary and
# combined_grade_epa_summary all spell 2016 SD and 2016-19 OAK.
.wf_canon <- function(x, season) {
  x <- .wf_code(x)
  s <- as.integer(season)
  x[x %in% c("LAC", "SD") & s == 2016L] <- "SD"
  x[x %in% c("LAC", "SD") & s >= 2017L] <- "LAC"
  x[x %in% c("LV", "OAK") & s <= 2019L] <- "OAK"
  x[x %in% c("LV", "OAK") & s >= 2020L] <- "LV"
  x
}

# fill seats: top N by volume within each group, N looked up by band
.wf_seats <- function(d, n_map, group_cols, band_col = "band") {
  d %>%
    group_by(across(all_of(c(group_cols, band_col)))) %>%
    arrange(desc(vol), .by_group = TRUE) %>%
    mutate(seat = dplyr::row_number()) %>%
    filter(seat <= unname(n_map[.data[[band_col]]])) %>%
    ungroup()
}

.wf_n <- function(name, default) {
  if (.wf_has(name)) get(name, envir = .GlobalEnv) else default
}

# one display name per player_id (the name on his biggest row)
.wf_names <- function(d, vol_col) {
  d %>% filter(!is.na(player_id), !is.na(player)) %>%
    group_by(player_id) %>%
    slice_max(.data[[vol_col]], n = 1, with_ties = FALSE) %>%
    ungroup() %>% select(player_id, player)
}

# ------------------------------------------------------ PFF outcomes ----------

.wf_ids <- function() {
  .wf_need("combined_grade_epa_summary")
  .wf_get("combined_grade_epa_summary") %>%
    distinct(posteam, opp, week, season) %>%
    mutate(week = as.integer(week), posteam = .wf_canon(posteam, season), opp = .wf_canon(opp, season))
}

# join PFF team-weeks to the id table; say out loud what did not match
.wf_join_ids <- function(wk, what) {
  wk <- tibble::as_tibble(wk) %>%
    mutate(week = as.integer(week), season = as.integer(season), team_name = .wf_canon(team_name, season))
  ids <- .wf_ids()
  lost <- anti_join(wk, ids, by = c("team_name" = "posteam", "week", "season"))
  if (nrow(lost)) {
    cat("[", what, "] PFF team-weeks with no match in combined_grade_epa_summary: ", nrow(lost),
        " (by team-season below, first 15)\n", sep = "")
    print(as.data.frame(utils::head(lost %>% count(team_name, season), 15)), row.names = FALSE)
  }
  inner_join(wk, ids, by = c("team_name" = "posteam", "week", "season"))
}

.wf_pass_wk <- function() {
  if (!is.null(.wf_memo$pass_wk)) return(.wf_memo$pass_wk)
  .wf_need("run_athena_query")
  wk <- get("run_athena_query", envir = .GlobalEnv)("
    SELECT  team_name, CAST(week AS INTEGER) AS week, CAST(season AS INTEGER) AS season,
            SUM(COALESCE(pressure_dropbacks, 0) + COALESCE(no_pressure_dropbacks, 0))       AS dropbacks,
            SUM(COALESCE(pressure_dropbacks, 0))                                            AS pressured,
            SUM(COALESCE(pressure_attempts, 0) + COALESCE(no_pressure_attempts, 0))         AS att,
            SUM(COALESCE(pressure_completions, 0) + COALESCE(no_pressure_completions, 0))   AS comp_n,
            SUM(COALESCE(pressure_yards, 0) + COALESCE(no_pressure_yards, 0))               AS yds,
            SUM(COALESCE(pressure_touchdowns, 0) + COALESCE(no_pressure_touchdowns, 0))     AS td_n,
            SUM(COALESCE(pressure_interceptions, 0) + COALESCE(no_pressure_interceptions, 0)) AS int_n,
            SUM(COALESCE(pressure_sacks, 0) + COALESCE(no_pressure_sacks, 0))               AS sacks
    FROM    nfl_data.passing_pressure
    GROUP BY 1, 2, 3
  ")
  .wf_memo$pass_wk <- .wf_join_ids(wk, "passing")
  .wf_memo$pass_wk
}

.wf_rush_wk <- function() {
  if (!is.null(.wf_memo$rush_wk)) return(.wf_memo$rush_wk)
  .wf_need("run_athena_query")
  wk <- get("run_athena_query", envir = .GlobalEnv)("
    SELECT  team_name, CAST(week AS INTEGER) AS week, CAST(season AS INTEGER) AS season,
            SUM(COALESCE(attempts, 0)) AS rushes, SUM(COALESCE(yards, 0)) AS ryds,
            SUM(COALESCE(touchdowns, 0)) AS rtd
    FROM    nfl_data.rushing_summary
    GROUP BY 1, 2, 3
  ")
  .wf_memo$rush_wk <- .wf_join_ids(wk, "rushing")
  .wf_memo$rush_wk
}

# passing outcomes per team-season. side "defense" = passing ALLOWED.
.wf_pass_outcomes <- function(side = c("offense", "defense")) {
  side <- match.arg(side)
  g <- if (side == "offense") "team_name" else "opp"
  .wf_pass_wk() %>%
    group_by(team = .data[[g]], season) %>%
    summarise(games = dplyr::n_distinct(week),
              across(c(dropbacks, pressured, att, comp_n, yds, td_n, int_n, sacks),
                     ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    filter(att > 0) %>%
    mutate(ypa  = yds / att,
           comp = comp_n / att,
           a_ = pmin(pmax((comp - 0.3) * 5, 0), 2.375),
           b_ = pmin(pmax((ypa - 3) * 0.25, 0), 2.375),
           c_ = pmin(pmax(td_n / att * 20, 0), 2.375),
           d_ = pmin(pmax(2.375 - int_n / att * 25, 0), 2.375),
           rating        = (a_ + b_ + c_ + d_) / 6 * 100,
           sack_rate     = sacks / dropbacks,
           pressure_rate = pressured / dropbacks) %>%
    select(-a_, -b_, -c_, -d_)
}

.wf_rush_outcomes <- function(side = c("offense", "defense")) {
  side <- match.arg(side)
  g <- if (side == "offense") "team_name" else "opp"
  .wf_rush_wk() %>%
    group_by(team = .data[[g]], season) %>%
    summarise(games = dplyr::n_distinct(week), rushes = sum(rushes, na.rm = TRUE),
              ryds = sum(ryds, na.rm = TRUE), rtd = sum(rtd, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(ypc = ryds / rushes, rush_td_pg = rtd / games)
}

# ------------------------------------------------ historical rooms -------------

## COVERAGE: YOUR committee (committee_hist_sec_lg: team x season x split x band,
## top N_SEC_LG by that team's split snaps in floor weeks). Seats first; then c1
## (grade_pctl, supp_pctl) and c3 (c3_pctl) are joined on. A seat with no percentile
## that season is priced by your faced fill law (rookie / vet prior; flagged unscored).
.wf_cov_rooms <- function() {
  if (!is.null(.wf_memo$cov)) return(.wf_memo$cov)
  .wf_need(c("committee_hist_sec_lg", "cov_pctl_sec_lg", "cov_c3_pctl", "prior_law_sec_lg", "c3_entry_prior_cv"))
  if (!.wf_has("ent_sec_lg"))
    cat("[waterfall] ent_sec_lg not in session: every unrated coverage seat is priced at the vet prior.\n")
  c1 <- .wf_get("cov_pctl_sec_lg") %>%
    select(player_id, player, season, split, grade = grade_pctl, supp = supp_pctl)
  .wf_unique(c1, c("player_id", "season", "split"), "cov_pctl_sec_lg")
  c3 <- .wf_get("cov_c3_pctl") %>% select(player_id, season, split, c3 = c3_pctl)
  .wf_unique(c3, c("player_id", "season", "split"), "cov_c3_pctl")
  seats <- .wf_get("committee_hist_sec_lg") %>%
    transmute(player_id, team = .wf_canon(team, season), season, split, band, vol = sn, seat = band_rank) %>%
    left_join(c1, by = c("player_id", "season", "split")) %>%
    left_join(c3, by = c("player_id", "season", "split")) %>%
    mutate(unscored = is.na(grade))
  # a seated player with no percentile that season is priced by YOUR faced fill law
  # (league_opp_secondary_schedule.R faced_games_one_sec): rookie_grade in his entry
  # year, else vet_grade; supp at pr_supp; c3 at your c3 entry prior (c3_entry_prior_cv)
  if (.wf_has("prior_law_sec_lg")) {
    law <- .wf_get("prior_law_sec_lg") %>%
      select(split, band, any_of(c("rookie_grade", "vet_grade", "pr_supp")))
    seats <- seats %>% left_join(law, by = c("split", "band"))
    if (.wf_has("ent_sec_lg")) {
      seats <- seats %>% left_join(.wf_get("ent_sec_lg") %>% distinct(player_id, .keep_all = TRUE) %>%
                                     select(player_id, entry_year), by = "player_id")
    } else seats$entry_year <- NA_integer_
    if (all(c("rookie_grade", "vet_grade") %in% names(seats)))
      seats <- seats %>% mutate(grade = dplyr::coalesce(
        grade, ifelse(!is.na(entry_year) & entry_year == season, rookie_grade, vet_grade)))
    if ("pr_supp" %in% names(seats)) seats <- seats %>% mutate(supp = dplyr::coalesce(supp, pr_supp))
    seats <- seats %>% select(-any_of(c("rookie_grade", "vet_grade", "pr_supp", "entry_year")))
  }
  if (.wf_has("c3_entry_prior_cv"))
    seats <- seats %>%
      left_join(.wf_get("c3_entry_prior_cv") %>% select(split, band, pr_c3), by = c("split", "band")) %>%
      mutate(c3 = dplyr::coalesce(c3, pr_c3)) %>% select(-pr_c3)
  room <- seats %>%
    group_by(team, season, split, band) %>%
    summarise(grade = .wf_wm(grade, vol), supp = .wf_wm(supp, vol), c3 = .wf_wm(c3, vol),
              filled = dplyr::n(),
              unscored_share = sum(vol[unscored]) / sum(vol), .groups = "drop")
  .wf_memo$cov <- list(seats = seats, room = room)
  .wf_memo$cov
}

## PASS RUSH: ED 4 / DI 4 by true-pass-set snaps for that team (qual_lg: ED/DI rows,
## >= 3 TPS snaps). Gated rushers only, as your rotation seats them. c1 = cur_lg g_pctl
## (the same series your 2026 gf is built from; one row per player_id).
.wf_pr_rooms <- function() {
  if (!is.null(.wf_memo$pr)) return(.wf_memo$pr)
  .wf_need("prush_c3_pctl")
  N <- c(ED = 4L, DI = 4L)   # league_opp_pass_rush_schedule.R NB_LG <- 4 (N_BAND is also a run-defense name)
  q <- if (.wf_has("qual_lg")) .wf_get("qual_lg") else {
    .wf_need("full_pass_rush_qbgrp")
    .wf_get("full_pass_rush_qbgrp") %>%
      filter(position %in% names(N), true_pass_set_snap_counts_pass_rush >= .wf_n("X_LG", 3))
  }
  st <- q %>%
    group_by(player_id, team = .wf_canon(team_name, season), season) %>%
    summarise(vol = sum(true_pass_set_snap_counts_pass_rush, na.rm = TRUE), .groups = "drop") %>%
    filter(vol > 0)
  if (.wf_has("cur_lg")) {
    c1 <- .wf_get("cur_lg") %>% select(player_id, season, band, grade = g_pctl)
  } else {
    .wf_need("prush_tps_season_pctl_sos")
    c1 <- .wf_get("prush_tps_season_pctl_sos") %>% select(player_id, season, band, grade = tps_grade_sos_pctl)
  }
  .wf_unique(c1, c("player_id", "season"), "pass-rush season percentiles")
  nm <- if ("player" %in% names(q)) .wf_names(q, "true_pass_set_snap_counts_pass_rush") else
    tibble(player_id = numeric(), player = character())
  c3 <- .wf_get("prush_c3_pctl") %>% select(player_id, season, c3 = c3_pctl)
  .wf_unique(c3, c("player_id", "season"), "prush_c3_pctl")
  seats <- st %>%
    inner_join(c1, by = c("player_id", "season")) %>%
    filter(band %in% names(N)) %>%
    left_join(c3, by = c("player_id", "season")) %>%
    left_join(nm, by = "player_id") %>%
    .wf_seats(N, c("team", "season"))
  room <- seats %>% group_by(team, season, band) %>%
    summarise(grade = .wf_wm(grade, vol), c3 = .wf_wm(c3, vol),
              filled = dplyr::n(), .groups = "drop")
  .wf_memo$pr <- list(seats = seats, room = room)
  .wf_memo$pr
}

## RUN DEFENSE: DI 5 / ED 5 / LB 3 / S 4 by run snaps for that team, counting only
## band-position games with >= 3 run snaps (qual_rd_lg law). Gated players only.
.wf_rd_rooms <- function() {
  if (!is.null(.wf_memo$rd)) return(.wf_memo$rd)
  .wf_need(c("run_defense_qbgrp", "rundef_season_pctl_sos", "rd_c3_pctl"))
  N <- .wf_n("N_RD_LG", c(DI = 5L, ED = 5L, LB = 3L, S = 4L))
  rq <- .wf_get("run_defense_qbgrp") %>%
    filter(position %in% .wf_n("BANDS_RD_LG", c("DI", "ED", "LB", "S")),
           snap_counts_run >= .wf_n("X_RD_LG", 3))
  st <- rq %>%
    group_by(player_id, team = .wf_canon(team_name, season), season) %>%
    summarise(vol = sum(snap_counts_run, na.rm = TRUE), .groups = "drop") %>%
    filter(vol > 0)
  c1 <- .wf_get("rundef_season_pctl_sos") %>%
    select(player_id, player, season, band, grade = grade_pctl, stop = stop_pctl)
  .wf_unique(c1, c("player_id", "season"), "rundef_season_pctl_sos")
  c3 <- .wf_get("rd_c3_pctl") %>% select(player_id, season, c3 = c3_pctl)
  .wf_unique(c3, c("player_id", "season"), "rd_c3_pctl")
  seats <- st %>%
    inner_join(c1, by = c("player_id", "season")) %>%
    filter(band %in% names(N)) %>%
    left_join(c3, by = c("player_id", "season")) %>%
    .wf_seats(N, c("team", "season"))
  room <- seats %>% group_by(team, season, band) %>%
    summarise(grade = .wf_wm(grade, vol), stop = .wf_wm(stop, vol), c3 = .wf_wm(c3, vol),
              filled = dplyr::n(), .groups = "drop")
  .wf_memo$rd <- list(seats = seats, room = room)
  .wf_memo$rd
}

## OL: each game's starter at the slot (that game's true-pass-set snap leader there),
## weighted by games started, playoffs in; true-pass-set grade, gap and zone
## percentiles (within slot x season), pass-block c3 and run-block gap / zone c3.
## A starter with no percentile at the slot is priced by your faced fill law.
.wf_ol_rooms <- function() {
  if (!is.null(.wf_memo$ol)) return(.wf_memo$ol)
  .wf_need(c("tps_pass_block_summary", "tps_pass_block_player_season_summary",
             "gap_player_season_summary", "zone_player_season_summary",
             "pblk_c3_pctl", "rblk_c3_pctl", "rookie_prior", "c3_rookie_prior_ol", "c3_scheme_prior_rb"))
  SLOTS <- c("LT", "LG", "C", "RG", "RT")
  collapse <- function(f, col, out) {
    d <- .wf_get(f)
    w <- if ("n" %in% names(d)) d$n else rep(1, nrow(d))
    d %>% mutate(.w = w) %>%
      group_by(player_id, det_position, season) %>%
      summarise(!!out := .wf_wm(.data[[col]], .w), .groups = "drop")
  }
  vals <- collapse("tps_pass_block_player_season_summary", "grade_season_pctl", "tps") %>%
    full_join(collapse("gap_player_season_summary",  "gap_season_pctl",  "gap"),
              by = c("player_id", "det_position", "season")) %>%
    full_join(collapse("zone_player_season_summary", "zone_season_pctl", "zone"),
              by = c("player_id", "det_position", "season"))
  # c3 joins at the slot, as your faced law does (player_id + season + band = slot);
  # a starter off his c3 band gets the slot's c3 prior below
  pb3 <- .wf_get("pblk_c3_pctl") %>% select(player_id, season, any_of("band"), pb_c3 = c3_pctl)
  .wf_unique(pb3, c("player_id", "season"), "pblk_c3_pctl")
  rb3 <- .wf_get("rblk_c3_pctl") %>% select(player_id, season, any_of("band"),
                                            gap_c3 = gap_c3_pctl, zone_c3 = zone_c3_pctl)
  .wf_unique(rb3, c("player_id", "season"), "rblk_c3_pctl")
  pb_by <- if ("band" %in% names(pb3)) c("player_id", "season", "det_position" = "band") else c("player_id", "season")
  rb_by <- if ("band" %in% names(rb3)) c("player_id", "season", "det_position" = "band") else c("player_id", "season")
  # TRUE PASS SETS (your default). A game counts when it is a row of
  # tps_pass_block_summary -- your >= 16 true-pass-set snaps guideline
  # (pff_pass_block_AWS.R). That game's starter at the slot = the lineman with the most
  # true-pass-set snaps there; vol = games he started there that season.
  tpb <- .wf_get("tps_pass_block_summary") %>% filter(det_position %in% SLOTS) %>%
    distinct(player_id, season, week, det_position, .keep_all = TRUE)
  seats <- tpb %>%
    mutate(team = .wf_canon(team_name, season)) %>%
    group_by(team, season, week, det_position, player_id) %>%
    summarise(sn = sum(true_pass_set_snap_counts_pass_block, na.rm = TRUE), .groups = "drop") %>%
    filter(sn > 0) %>%
    group_by(team, season, week, det_position) %>%
    slice_max(sn, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    count(player_id, team, season, det_position, name = "vol") %>%
    left_join(.wf_names(tpb, "true_pass_set_snap_counts_pass_block"), by = "player_id") %>%
    left_join(vals, by = c("player_id", "det_position", "season")) %>%
    left_join(pb3, by = pb_by) %>%
    left_join(rb3, by = rb_by) %>%
    rename(band = det_position) %>%
    mutate(unscored = is.na(tps))
  # a starter with no percentile at the slot is priced by YOUR faced fill law
  # (league_pass_block_availability.R faced_league_pb, league_run_block_availability.R
  # faced_cells_rb): the slot's entry-year prior (rookie_prior), else the unit mean;
  # c3 at your slot c3 prior (c3_rookie_prior_ol; gap / zone c3 at c3_scheme_prior_rb)
  if (.wf_has("rookie_prior")) {
    rp <- .wf_get("rookie_prior") %>% select(det_position, any_of(c("pr_tps", "pr_gap", "pr_zone")))
    seats <- seats %>% left_join(rp, by = c("band" = "det_position"))
    for (v in c("tps", "gap", "zone")) {
      pc <- paste0("pr_", v)
      if (pc %in% names(seats)) seats[[v]] <- dplyr::coalesce(seats[[v]], seats[[pc]], mean(rp[[pc]], na.rm = TRUE))
    }
    seats <- seats %>% select(-any_of(c("pr_tps", "pr_gap", "pr_zone")))
  } else cat("[waterfall] rookie_prior not in session: OL starters with no percentile are left out of their slot.\n")
  if (.wf_has("c3_rookie_prior_ol"))
    seats <- seats %>% left_join(.wf_get("c3_rookie_prior_ol") %>% select(band, pr_c3), by = "band") %>%
      mutate(pb_c3 = dplyr::coalesce(pb_c3, pr_c3)) %>% select(-pr_c3)
  if (.wf_has("c3_scheme_prior_rb")) {
    sp <- .wf_get("c3_scheme_prior_rb") %>% select(band, any_of(c("pr_c3_gap", "pr_c3_zone")))
    seats <- seats %>% left_join(sp, by = "band")
    if ("pr_c3_gap"  %in% names(seats)) seats$gap_c3  <- dplyr::coalesce(seats$gap_c3,  seats$pr_c3_gap)
    if ("pr_c3_zone" %in% names(seats)) seats$zone_c3 <- dplyr::coalesce(seats$zone_c3, seats$pr_c3_zone)
    seats <- seats %>% select(-any_of(c("pr_c3_gap", "pr_c3_zone")))
  }
  room <- seats %>%
    group_by(team, season, band) %>%
    summarise(leader = player[which.max(vol)], games = sum(vol),
              unscored_share = sum(vol[unscored]) / sum(vol),
              tps = .wf_wm(tps, vol), pb_c3 = .wf_wm(pb_c3, vol),
              gap = .wf_wm(gap, vol), zone = .wf_wm(zone, vol),
              gap_c3 = .wf_wm(gap_c3, vol), zone_c3 = .wf_wm(zone_c3, vol), .groups = "drop")
  .wf_memo$ol <- list(seats = seats, room = room)
  .wf_memo$ol
}

## RECEIVING: corps of 8 by routes for that team (across bands). Seats first; values
## are within-band percentiles, so the corps average respects role. A receiver with no
## percentile that season keeps his seat, unscored (your observed-only law).
.wf_rc_rooms <- function() {
  if (!is.null(.wf_memo$rc)) return(.wf_memo$rc)
  .wf_need(c("receiving_func_base", "rec_season_pctl_sos", "rec_c3_pctl"))
  N_C <- .wf_n("N_CORPS", 8L)
  rfb <- .wf_get("receiving_func_base")
  vcol <- intersect(c("routes", "snap_counts_pass_route"), names(rfb))[1]
  if (is.na(vcol)) stop("receiving_func_base has neither routes nor snap_counts_pass_route")
  st <- rfb %>%
    filter(!is.na(player_id)) %>%
    distinct(player_id, season, week, abbreviation, .keep_all = TRUE) %>%   # a two-team week keeps both games
    group_by(player_id, team = .wf_canon(abbreviation, season), season) %>%
    summarise(vol = sum(.data[[vcol]], na.rm = TRUE), .groups = "drop") %>%
    filter(vol > 0)
  c1 <- .wf_get("rec_season_pctl_sos") %>%
    select(player_id, player, season, band,
           mg = man_grade_pctl, my = man_yprr_pctl, zg = zone_grade_pctl, zy = zone_yprr_pctl)
  .wf_unique(c1, c("player_id", "season"), "rec_season_pctl_sos")
  c3 <- .wf_get("rec_c3_pctl") %>%
    select(player_id, season, split, g3 = c3_grade_pctl, y3 = c3_yprr_pctl)
  .wf_unique(c3, c("player_id", "season", "split"), "rec_c3_pctl")
  c3 <- c3 %>% tidyr::pivot_wider(names_from = split, values_from = c(g3, y3))
  seats <- st %>%
    group_by(team, season) %>%
    arrange(desc(vol), .by_group = TRUE) %>%
    mutate(seat = dplyr::row_number()) %>%
    filter(seat <= N_C) %>%
    ungroup() %>%
    left_join(c1, by = c("player_id", "season")) %>%
    left_join(c3, by = c("player_id", "season"))
  .wf_memo$rc <- list(seats = seats, room = seats)
  .wf_memo$rc
}

## RUSHING: 2 backs by attempts in games with 4+ attempts, for that team. Seats first;
## a back with no percentile that season is priced at your entry-year prior
## (rush_rookie_prior / c3_rookie_prior_ru), as your faced fill law does.
.wf_ru_rooms <- function() {
  if (!is.null(.wf_memo$ru)) return(.wf_memo$ru)
  .wf_need(c("rushing_qbgrp", "rush_season_pctl_sos", "ru_c3_pctl"))
  N_R <- .wf_n("N_RB", 2L)
  rq <- .wf_get("rushing_qbgrp")
  tcol <- intersect(c("team", "team_name", "posteam"), names(rq))[1]
  rq <- rq %>% mutate(team = .wf_canon(.data[[tcol]], season))
  if ("week" %in% names(rq)) {
    rq <- rq %>% group_by(player_id, team, season, week) %>%
      summarise(attempts = sum(attempts, na.rm = TRUE), .groups = "drop") %>%
      filter(attempts >= 4)
  }
  st <- rq %>%
    group_by(player_id, team, season) %>%
    summarise(vol = sum(attempts, na.rm = TRUE), .groups = "drop") %>%
    filter(vol > 0)
  c1 <- .wf_get("rush_season_pctl_sos") %>%
    select(player_id, player, season, grun = grun_pctl, mtf = mtf_pctl)
  .wf_unique(c1, c("player_id", "season"), "rush_season_pctl_sos")
  c3 <- .wf_get("ru_c3_pctl") %>% select(player_id, season, c3 = c3_pctl)
  .wf_unique(c3, c("player_id", "season"), "ru_c3_pctl")
  pr  <- if (.wf_has("rush_rookie_prior")) .wf_get("rush_rookie_prior") else NULL
  pr3 <- if (.wf_has("c3_rookie_prior_ru")) .wf_get("c3_rookie_prior_ru") else NULL
  seats <- st %>%
    group_by(team, season) %>%
    arrange(desc(vol), .by_group = TRUE) %>%
    mutate(seat = dplyr::row_number()) %>%
    filter(seat <= N_R) %>%
    ungroup() %>%
    left_join(c1, by = c("player_id", "season")) %>%
    left_join(c3, by = c("player_id", "season")) %>%
    mutate(prior_used = is.na(grun))
  if (!is.null(pr))  seats <- seats %>% mutate(grun = dplyr::coalesce(grun, pr$pr_grun[1]),
                                               mtf  = dplyr::coalesce(mtf,  pr$pr_mtf[1]))
  if (!is.null(pr3)) seats <- seats %>% mutate(c3 = dplyr::coalesce(c3, pr3$pr_c3[1]))
  .wf_memo$ru <- list(seats = seats, room = seats)
  .wf_memo$ru
}

# ------------------------------------------- inputs: history and 2026 --------

.WF_SIDES <- c("pass_def", "rush_def", "pass_off", "rush_off")

.wf_labels <- function(x) {
  x <- sub("^cov_(man|zone)_(\\w+)_grade$", "\\2 \\1 coverage grade", x)
  x <- sub("^cov_zone_(\\w+)_supp$",       "\\1 zone completion suppression", x)
  x <- sub("^cov_(man|zone)_(\\w+)_c3$",   "\\2 \\1 coverage c3", x)
  x <- sub("^pr_(\\w+)_grade$",            "\\1 pass-rush grade (TPS)", x)
  x <- sub("^pr_(\\w+)_c3$",               "\\1 pass-rush c3", x)
  x <- sub("^rd_(\\w+)_grade$",            "\\1 run-defense grade", x)
  x <- sub("^rd_(\\w+)_stop$",             "\\1 run-defense stop %", x)
  x <- sub("^rd_(\\w+)_c3$",               "\\1 run-defense c3", x)
  x <- sub("^ol_(LT|LG|C|RG|RT)_tps$",     "\\1 pass-block grade (TPS)", x)
  x <- sub("^ol_(LT|LG|C|RG|RT)_c3$",      "\\1 pass-block c3", x)
  x <- sub("^ol_gap$",  "OL gap run-block", x); x <- sub("^ol_zone$", "OL zone run-block", x)
  x <- sub("^ol_gap_c3$", "OL gap c3", x);      x <- sub("^ol_zone_c3$", "OL zone c3", x)
  x <- sub("^rc_(man|zone)_(grade|yprr)$",    "corps \\1 \\2", x)
  x <- sub("^rc_(man|zone)_(grade|yprr)_c3$", "corps \\1 \\2 c3", x)
  x <- sub("^rb_grade$", "backs run grade", x); x <- sub("^rb_mtf$", "backs missed tackles forced", x)
  x <- sub("^rb_c3$", "backs c3", x)
  x
}

## ---- availability helpers (your own pricing law, read back from your frames) ----
.wf_is_phantom <- function(d) {
  ph <- rep(FALSE, nrow(d))
  nm <- intersect(c("roster_name", "full_name", "player"), names(d))[1]
  if (!is.na(nm)) ph <- ph | grepl("PHANTOM", d[[nm]], ignore.case = TRUE)
  for (pc in intersect(c("phantom", "is_phantom"), names(d)))
    ph <- ph | dplyr::coalesce(as.logical(d[[pc]]), FALSE)
  ph
}
# attach per-group summary columns (grp empty = one league-wide value)
.wf_attach <- function(d, s, grp) {
  if (is.null(s)) return(d)
  if (length(grp)) return(left_join(d, s, by = grp))
  for (nm in names(s)) d[[nm]] <- s[[nm]][1]
  d
}
# your 2025 replacement level per group, recovered from your pricing law
# (priced = avail x full + (1 - avail) x repl): median over rows with avail < 1
.wf_repl_d <- function(d, pairs, grp = character(0)) {
  if (!"avail" %in% names(d)) return(NULL)
  grp <- intersect(grp, names(d))
  out <- if (length(grp)) d %>% distinct(across(all_of(grp))) else tibble(.one = 1)
  for (nm in names(pairs)) {
    f <- pairs[[nm]][1]; p <- pairs[[nm]][2]
    if (!all(c(f, p) %in% names(d))) next
    r <- d %>%
      mutate(.rp = ifelse(!is.na(avail) & avail < 0.999,
                          (.data[[p]] - avail * .data[[f]]) / (1 - avail), NA_real_)) %>%
      group_by(across(all_of(grp))) %>%
      summarise(!!nm := suppressWarnings(stats::median(.rp, na.rm = TRUE)), .groups = "drop")
    out <- if (length(grp)) left_join(out, r, by = grp) else bind_cols(out, r)
  }
  out <- out %>% select(-any_of(".one"))
  if (ncol(out) == length(grp)) NULL else out
}
.wf_repl <- function(fr, pairs, grp = character(0)) {
  if (!.wf_has(fr)) return(NULL)
  .wf_repl_d(.wf_get(fr), pairs, grp)
}
# league-average availability lens: seat value = abar x full + (1 - abar) x repl, where
# abar = usage-weighted mean avail of that group's seat-holders across all 32 teams
# (phantom seats left out). New columns <full col>__a.
.wf_avg_lens <- function(d, pairs, grp = character(0), wcol = NULL, abar = NULL) {
  grp <- intersect(grp, names(d))
  fcols <- vapply(pairs, function(x) x[1], character(1))
  if (!"avail" %in% names(d)) {
    for (f in intersect(fcols, names(d))) d[[paste0(f, "__a")]] <- d[[f]]
    return(d)
  }
  d$.w <- if (!is.null(wcol) && wcol %in% names(d)) d[[wcol]] else 1
  rp <- .wf_repl_d(d, setNames(pairs, paste0(".r_", fcols)), grp)
  if (!is.null(abar)) {
    d$.abar <- abar                                   # a fixed X (OL: from data)
  } else {
    ab <- d[!.wf_is_phantom(d), ] %>% group_by(across(all_of(grp))) %>%
      summarise(.abar = .wf_wm(avail, .w), .groups = "drop")
    d <- .wf_attach(d, ab, grp)
  }
  d <- .wf_attach(d, rp, grp)
  for (f in intersect(fcols, names(d))) {
    r <- paste0(".r_", f)
    d[[paste0(f, "__a")]] <- if (r %in% names(d) && ".abar" %in% names(d)) {
      ifelse(is.na(d[[r]]) | is.na(d$.abar), d[[f]], d$.abar * d[[f]] + (1 - d$.abar) * d[[r]])
    } else d[[f]]
  }
  d %>% select(-any_of(c(".w", ".abar")), -starts_with(".r_"))
}

## historical inputs, one row per team-season, for one side and currency.
## rooms = optional list of room tables (cov / pr / rd / ol / rc / ru) to use instead
## of the historical ones -- the waterfall passes 2025 rooms re-valued at 2026 numbers.
.wf_inputs_hist <- function(side, currency, rooms = NULL) {
  rm_ <- function(u, builder) if (!is.null(rooms[[u]])) rooms[[u]] else builder()$room
  if (side == "pass_def") {
    cv <- rm_("cov", .wf_cov_rooms)
    pr <- rm_("pr", .wf_pr_rooms)
    if (currency == "c1") {
      a <- cv %>% transmute(team, season, key = paste0("cov_", split, "_", band, "_grade"), val = grade)
      b <- cv %>% filter(split == "zone", band %in% c("CB", "S")) %>%        # facet_law_mz: zone supp seated for CB, S
        transmute(team, season, key = paste0("cov_zone_", band, "_supp"), val = supp)
      p <- pr %>% transmute(team, season, key = paste0("pr_", band, "_grade"), val = grade)
    } else {
      a <- cv %>% transmute(team, season, key = paste0("cov_", split, "_", band, "_c3"), val = c3)
      b <- NULL
      p <- pr %>% transmute(team, season, key = paste0("pr_", band, "_c3"), val = c3)
    }
    long <- bind_rows(a, b, p)
  } else if (side == "rush_def") {
    rd <- rm_("rd", .wf_rd_rooms)
    long <- if (currency == "c1") {
      bind_rows(rd %>% transmute(team, season, key = paste0("rd_", band, "_grade"), val = grade),
                rd %>% transmute(team, season, key = paste0("rd_", band, "_stop"),  val = stop))
    } else rd %>% transmute(team, season, key = paste0("rd_", band, "_c3"), val = c3)
  } else if (side == "pass_off") {
    ol <- rm_("ol", .wf_ol_rooms)
    rc <- rm_("rc", .wf_rc_rooms)
    if (currency == "c1") {
      o1 <- ol %>% transmute(team, season, key = paste0("ol_", band, "_tps"), val = tps)
      r  <- rc %>% group_by(team, season) %>%
        summarise(rc_man_grade  = .wf_wm(mg, vol), rc_man_yprr  = .wf_wm(my, vol),
                  rc_zone_grade = .wf_wm(zg, vol), rc_zone_yprr = .wf_wm(zy, vol), .groups = "drop") %>%
        tidyr::pivot_longer(-c(team, season), names_to = "key", values_to = "val")
    } else {
      o1 <- ol %>% transmute(team, season, key = paste0("ol_", band, "_c3"), val = pb_c3)
      r  <- rc %>% group_by(team, season) %>%
        summarise(rc_man_grade_c3  = .wf_wm(g3_man,  vol), rc_man_yprr_c3  = .wf_wm(y3_man,  vol),
                  rc_zone_grade_c3 = .wf_wm(g3_zone, vol), rc_zone_yprr_c3 = .wf_wm(y3_zone, vol),
                  .groups = "drop") %>%
        tidyr::pivot_longer(-c(team, season), names_to = "key", values_to = "val")
    }
    long <- bind_rows(o1, r)
  } else if (side == "rush_off") {
    ol <- rm_("ol", .wf_ol_rooms)
    ru <- rm_("ru", .wf_ru_rooms)
    o <- ol %>% group_by(team, season) %>%
      summarise(ol_gap = mean(gap, na.rm = TRUE), ol_zone = mean(zone, na.rm = TRUE),
                ol_gap_c3 = mean(gap_c3, na.rm = TRUE), ol_zone_c3 = mean(zone_c3, na.rm = TRUE),
                .groups = "drop")
    b <- ru %>% group_by(team, season) %>%
      summarise(rb_grade = .wf_wm(grun, vol), rb_mtf = .wf_wm(mtf, vol), rb_c3 = .wf_wm(c3, vol),
                .groups = "drop")
    keep <- if (currency == "c1") c("ol_gap", "ol_zone", "rb_grade", "rb_mtf") else c("ol_gap_c3", "ol_zone_c3", "rb_c3")
    long <- full_join(o, b, by = c("team", "season")) %>%
      tidyr::pivot_longer(-c(team, season), names_to = "key", values_to = "val") %>%
      filter(key %in% keep)
  } else stop("side must be one of: ", paste(.WF_SIDES, collapse = ", "))
  long %>%
    mutate(val = ifelse(is.finite(val), val, NA_real_)) %>%
    tidyr::pivot_wider(names_from = key, values_from = val)
}

## 2026 inputs from your member frames (the numbers team_yoy compares).
## value = "p": availability-priced (what team_yoy shows); "f": full health.
.wf_inputs_2026 <- function(side, currency, ol = c("a", "p", "f"), frames = list()) {
  ol <- match.arg(ol)
  # frames: optional stand-in member frames by name (wf_swap passes a swapped copy);
  # nothing in your session is changed
  g <- function(fr) {
    if (is.null(frames[[fr]])) return(.wf_get(fr))
    d <- tibble::as_tibble(frames[[fr]]) %>% ungroup()
    if ("season" %in% names(d)) d$season <- as.integer(d$season)
    d
  }
  pick    <- function(f_col, p_col) f_col           # every non-OL room: full health (seat depth = rotation)
  lens    <- function(d, pairs, grp, w) d
  pick_ol <- function(f_col, p_col) switch(ol, p = p_col, f = f_col, a = paste0(f_col, "__a"))
  lens_ol <- function(d, pairs, u = "pb") if (ol == "a") .wf_avg_lens(d, pairs, "slot", NULL, abar = .wf_ol_x(u)$x) else d
  if (side == "pass_def") {
    .wf_need(c("members_cv", "members_pa"))
    cv <- g("members_cv") %>% mutate(team = .wf_code(team)) %>%
      lens(list(c("grade_f", "grade_p"), c("supp_f", "supp_p"), c("c3_f", "adj_p")), c("split", "band"), "usage_w")
    pa <- g("members_pa") %>% mutate(team = .wf_code(team)) %>%
      lens(list(c("gf", "gf_p"), c("c3_f", "c3_p")), "band", "uw")
    if (currency == "c1") {
      a <- cv %>% group_by(team, split, band) %>%
        summarise(val = .wf_wm(.data[[pick("grade_f", "grade_p")]], usage_w), .groups = "drop") %>%
        transmute(team, key = paste0("cov_", split, "_", band, "_grade"), val)
      b <- cv %>% filter(split == "zone", band %in% c("CB", "S")) %>% group_by(team, band) %>%
        summarise(val = .wf_wm(.data[[pick("supp_f", "supp_p")]], usage_w), .groups = "drop") %>%
        transmute(team, key = paste0("cov_zone_", band, "_supp"), val)
      p <- pa %>% group_by(team, band) %>%
        summarise(val = .wf_wm(.data[[pick("gf", "gf_p")]], uw), .groups = "drop") %>%
        transmute(team, key = paste0("pr_", band, "_grade"), val)
    } else {
      a <- cv %>% group_by(team, split, band) %>%
        summarise(val = .wf_wm(.data[[pick("c3_f", "adj_p")]], usage_w), .groups = "drop") %>%
        transmute(team, key = paste0("cov_", split, "_", band, "_c3"), val)
      b <- NULL
      p <- pa %>% group_by(team, band) %>%
        summarise(val = .wf_wm(.data[[pick("c3_f", "c3_p")]], uw), .groups = "drop") %>%
        transmute(team, key = paste0("pr_", band, "_c3"), val)
    }
    long <- bind_rows(a, b, p)
  } else if (side == "rush_def") {
    .wf_need("members_ra")
    ra <- g("members_ra") %>% mutate(team = .wf_code(team)) %>%
      lens(list(c("grade_f", "grade_p"), c("stop_f", "stop_p"), c("c3_f", "c3_p")), "band", "usage_w")
    long <- if (currency == "c1") {
      bind_rows(
        ra %>% group_by(team, band) %>% summarise(val = .wf_wm(.data[[pick("grade_f", "grade_p")]], usage_w), .groups = "drop") %>%
          transmute(team, key = paste0("rd_", band, "_grade"), val),
        ra %>% group_by(team, band) %>% summarise(val = .wf_wm(.data[[pick("stop_f", "stop_p")]], usage_w), .groups = "drop") %>%
          transmute(team, key = paste0("rd_", band, "_stop"), val))
    } else {
      ra %>% group_by(team, band) %>% summarise(val = .wf_wm(.data[[pick("c3_f", "c3_p")]], usage_w), .groups = "drop") %>%
        transmute(team, key = paste0("rd_", band, "_c3"), val)
    }
  } else if (side == "pass_off") {
    .wf_need(c("slot_value_26_pb_build", "members_rc"))
    pb <- g("slot_value_26_pb_build") %>% mutate(team = .wf_code(team_name)) %>%
      lens_ol(list(c("gf_raw", "gf_raw_av"), c("gf_adj", "gf_adj_av")))
    rc <- g("members_rc") %>% mutate(team = .wf_code(team_name)) %>%
      lens(list(c("mg_f", "mg_p"), c("my_f", "my_p"), c("zg_f", "zg_p"), c("zy_f", "zy_p"),
                c("c3mg_f", "c3mg_p"), c("c3my_f", "c3my_p"), c("c3zg_f", "c3zg_p"), c("c3zy_f", "c3zy_p")),
           character(0), "usage_w")
    if (currency == "c1") {
      o1 <- pb %>% transmute(team, key = paste0("ol_", slot, "_tps"), val = .data[[pick_ol("gf_raw", "gf_raw_av")]])
      r <- rc %>% group_by(team) %>%
        summarise(rc_man_grade  = .wf_wm(.data[[pick("mg_f", "mg_p")]], usage_w),
                  rc_man_yprr   = .wf_wm(.data[[pick("my_f", "my_p")]], usage_w),
                  rc_zone_grade = .wf_wm(.data[[pick("zg_f", "zg_p")]], usage_w),
                  rc_zone_yprr  = .wf_wm(.data[[pick("zy_f", "zy_p")]], usage_w), .groups = "drop") %>%
        tidyr::pivot_longer(-team, names_to = "key", values_to = "val")
    } else {
      o1 <- pb %>% transmute(team, key = paste0("ol_", slot, "_c3"), val = .data[[pick_ol("gf_adj", "gf_adj_av")]])
      r <- rc %>% group_by(team) %>%
        summarise(rc_man_grade_c3  = .wf_wm(.data[[pick("c3mg_f", "c3mg_p")]], usage_w),
                  rc_man_yprr_c3   = .wf_wm(.data[[pick("c3my_f", "c3my_p")]], usage_w),
                  rc_zone_grade_c3 = .wf_wm(.data[[pick("c3zg_f", "c3zg_p")]], usage_w),
                  rc_zone_yprr_c3  = .wf_wm(.data[[pick("c3zy_f", "c3zy_p")]], usage_w), .groups = "drop") %>%
        tidyr::pivot_longer(-team, names_to = "key", values_to = "val")
    }
    long <- bind_rows(o1, r)
  } else if (side == "rush_off") {
    .wf_need(c("slot_value_26_rb_build", "memb_lg_ru"))
    rb <- g("slot_value_26_rb_build") %>% mutate(team = .wf_code(team_name)) %>%
      lens_ol(list(c("V_gap", "V_gap_av"), c("V_zone", "V_zone_av"),
                   c("V_c3_gap", "V_c3_gap_av"), c("V_c3_zone", "V_c3_zone_av")), "rb")
    ru <- g("memb_lg_ru") %>% mutate(team = .wf_code(team)) %>%
      lens(list(c("gf", "gf_av"), c("mf", "mf_av"), c("V_c3", "V_c3_av")), character(0), "uw")
    if (currency == "c1") {
      o <- rb %>% group_by(team) %>%
        summarise(ol_gap  = mean(.data[[pick_ol("V_gap",  "V_gap_av")]],  na.rm = TRUE),
                  ol_zone = mean(.data[[pick_ol("V_zone", "V_zone_av")]], na.rm = TRUE), .groups = "drop")
      b <- ru %>% group_by(team) %>%
        summarise(rb_grade = .wf_wm(.data[[pick("gf", "gf_av")]], uw),
                  rb_mtf   = .wf_wm(.data[[pick("mf", "mf_av")]], uw), .groups = "drop")
    } else {
      need <- c("V_c3_gap", "V_c3_gap_av", "V_c3_zone", "V_c3_zone_av")
      if (!all(need %in% names(rb)))
        stop("slot_value_26_rb_build has no gap/zone c3 split -- re-source league_run_block_availability.R")
      o <- rb %>% group_by(team) %>%
        summarise(ol_gap_c3  = mean(.data[[pick_ol("V_c3_gap",  "V_c3_gap_av")]],  na.rm = TRUE),
                  ol_zone_c3 = mean(.data[[pick_ol("V_c3_zone", "V_c3_zone_av")]], na.rm = TRUE), .groups = "drop")
      b <- ru %>% group_by(team) %>%
        summarise(rb_c3 = .wf_wm(.data[[pick("V_c3", "V_c3_av")]], uw), .groups = "drop")
    }
    long <- full_join(o, b, by = "team") %>%
      tidyr::pivot_longer(-team, names_to = "key", values_to = "val")
  } else stop("side must be one of: ", paste(.WF_SIDES, collapse = ", "))
  long %>%
    mutate(val = ifelse(is.finite(val), val, NA_real_)) %>%
    tidyr::pivot_wider(names_from = key, values_from = val)
}

.wf_outcomes_for <- function(side) {
  switch(side,
         pass_def = .wf_pass_outcomes("defense"),
         pass_off = .wf_pass_outcomes("offense"),
         rush_def = .wf_rush_outcomes("defense"),
         rush_off = .wf_rush_outcomes("offense"))
}
.wf_outcome_cols <- function(side) {
  if (side %in% c("pass_def", "pass_off")) c("ypa", "comp", "rating", "sack_rate", "pressure_rate")
  else c("ypc", "rush_td_pg")
}
# display units: rates shown in percentage points
.wf_scale <- function(o) if (o %in% c("comp", "sack_rate", "pressure_rate")) 100 else 1

# ------------------------------------------------------------------ fit ------

.wf_fit_one <- function(side, currency) {
  key <- paste(side, currency)
  if (!is.null(.wf_memo$fits[[key]])) return(.wf_memo$fits[[key]])
  X <- .wf_inputs_hist(side, currency)
  inputs <- setdiff(names(X), c("team", "season"))
  Y <- .wf_outcomes_for(side)
  # TEAM-CODE WALL: every room team-season must have a PFF outcome and every outcome
  # team-season must have rooms (in the seasons both cover). Any miss stops, listed.
  sx <- intersect(unique(X$season), unique(Y$season))
  lostX <- anti_join(X %>% filter(season %in% sx), Y, by = c("team", "season")) %>% distinct(team, season)
  lostY <- anti_join(Y %>% filter(season %in% sx), X, by = c("team", "season")) %>% distinct(team, season)
  if (nrow(lostX) || nrow(lostY)) {
    cat("rooms with no PFF outcome:\n");  print(as.data.frame(lostX), row.names = FALSE)
    cat("PFF outcomes with no rooms:\n"); print(as.data.frame(lostY), row.names = FALSE)
    stop(side, " ", currency, ": team codes do not line up between the rooms and the PFF outcomes (listed above).")
  }
  d <- inner_join(X, Y, by = c("team", "season"))
  # a season where a whole input is missing (receiving before 2018) is left out, not imputed
  empty <- d %>% group_by(season) %>%
    summarise(across(all_of(inputs), ~ all(is.na(.x))), .groups = "drop")
  dropped <- empty$season[apply(as.matrix(empty[inputs]), 1, any)]
  d <- d %>% filter(!season %in% dropped)
  if (!nrow(d)) stop(side, " ", currency, ": no season has every input")
  # a single room with no qualified seat: that season's median room (counted, printed)
  imp_mask <- d[c("team", "season", inputs)]
  imp_mask[inputs] <- lapply(imp_mask[inputs], is.na)
  n_imp <- vapply(inputs, function(v) sum(is.na(d[[v]])), integer(1))
  d <- d %>% group_by(season) %>%
    mutate(across(all_of(inputs), ~ ifelse(is.na(.x), stats::median(.x, na.rm = TRUE), .x))) %>%
    ungroup()
  outs <- .wf_outcome_cols(side)
  models <- lapply(outs, function(o) stats::lm(stats::reformulate(inputs, o), data = d))
  names(models) <- outs
  vif <- vapply(inputs, function(v) {
    others <- setdiff(inputs, v)
    if (!length(others)) return(1)
    r2 <- summary(stats::lm(stats::reformulate(others, v), data = d))$r.squared
    if (r2 >= 1) Inf else 1 / (1 - r2)
  }, numeric(1))
  res <- list(side = side, currency = currency, inputs = inputs, models = models,
              n = nrow(d), seasons = range(d$season), dropped = dropped,
              n_imputed = n_imp, imputed = imp_mask, vif = vif, data = d)
  if (is.null(.wf_memo$fits)) .wf_memo$fits <- list()
  .wf_memo$fits[[key]] <- res
  res
}

wf_fit <- function(side = "pass_def", currency = c("c1", "c3")) {
  side <- match.arg(side, .WF_SIDES)
  for (cur in currency) {
    f <- .wf_fit_one(side, cur)
    cat("\n== ", side, " (", cur, ") -- ", f$n, " team-seasons, ", f$seasons[1], "-", f$seasons[2],
        " ==\nnumber per +10 percentile points at that room, everything else held fixed",
        " [range = 95%]\n", sep = "")
    if (length(f$dropped))
      cat("seasons left out (an input has no data at all that season): ",
          paste(sort(f$dropped), collapse = ", "), "\n", sep = "")
    tab <- tibble(input = .wf_labels(f$inputs))
    for (o in names(f$models)) {
      m   <- f$models[[o]]
      k   <- .wf_scale(o) * 0.1
      est <- stats::coef(m)[f$inputs] * k
      se  <- sqrt(pmax(diag(stats::vcov(m)), 0))[f$inputs] * k
      tab[[o]] <- ifelse(is.na(est), "aliased",
                         sprintf("%+.3f [%+.3f, %+.3f]", est, est - 1.96 * se, est + 1.96 * se))
    }
    tab$VIF <- round(f$vif, 1)
    print(as.data.frame(tab), row.names = FALSE)
    cat("R-squared: ", paste(sprintf("%s %.2f", names(f$models),
                                     vapply(f$models, function(m) summary(m)$r.squared, numeric(1))),
                             collapse = " | "), "\n", sep = "")
    if (any(f$n_imputed > 0))
      cat("rooms with no qualified seat (filled with that season's median): ",
          paste(sprintf("%s %d", f$inputs[f$n_imputed > 0], f$n_imputed[f$n_imputed > 0]), collapse = ", "),
          "\n", sep = "")
    cat("Units: comp %, sack rate and pressure rate in percentage points.",
        " VIF above ~5 = that room moves with others; trust the total more than its bar.\n", sep = "")
  }
  invisible(NULL)
}

# ------------------------------------------------------------ waterfall -----

## X = league-average OL starter availability, FROM DATA: for every team-season-slot in
## 2023-25, the primary starter (most starts there) and his REG games played (>= 8
## pass-block snaps in a week, any slot) / 17 -- your OL availability layer's
## games-played definition. X = the mean over all of them; by-slot values printed too.
## X on TRUE PASS SETS (your default), your guidelines only: a game counts when it is a
## row of tps_pass_block_summary (>= 16 true-pass-set snaps, pff_pass_block_AWS.R).
## Job holder = the season true-pass-set snap leader per team x season x slot, REG weeks,
## one row per player-season (trades: one row either way).
## Rate = his qualifying weeks / HIS TEAM'S qualifying weeks that season. X = the mean over all
## job-holder seasons, 2023-25. One X for the OL, pass and run lens alike.
## FIXED 2026-09-18: v2 divided by 17. The >= 16 TPS gate drops ~6 of 17 weeks for the whole
## team (league mean 10.7 qualifying weeks), so a starter who played every game read ~0.70
## (DEN 2025: Bolles 12 rows, 17 games) and X read 0.506 -- every 2026 OL slot was priced as
## half backup and the SAME five players "got worse". The gate now sits on both sides of the
## rate: weeks that do not count for the numerator do not count for the denominator either.
## Same table, same guideline, no other knob. The wall below stops a gated rate coming back.
.wf_ol_x <- function(unit = NULL) {
  if (!is.null(.wf_memo$ol_x)) return(.wf_memo$ol_x)
  .wf_need("tps_pass_block_summary")
  SLOTS <- c("LT", "LG", "C", "RG", "RT")
  d <- .wf_get("tps_pass_block_summary") %>%
    filter(season %in% 2023:2025, week <= 18) %>%
    distinct(player_id, season, week, det_position, .keep_all = TRUE) %>%
    transmute(player_id, team = .wf_canon(team_name, season), season, week, det_position,
              sn = true_pass_set_snap_counts_pass_block)
  team_g <- d %>% filter(det_position %in% SLOTS) %>% distinct(season, team, week) %>%
    count(season, team, name = "team_g")                       # the weeks that COUNT under the guideline
  holders <- d %>% filter(det_position %in% SLOTS) %>%
    group_by(season, team, det_position, player_id) %>%
    summarise(sn = sum(sn, na.rm = TRUE), .groups = "drop") %>%
    group_by(season, team, det_position) %>%
    slice_max(sn, n = 1, with_ties = FALSE) %>% ungroup()
  played <- d %>% distinct(player_id, season, team, week) %>% count(player_id, season, team, name = "gp")
  per <- holders %>% distinct(season, player_id, .keep_all = TRUE) %>%
    left_join(played, by = c("player_id", "season", "team")) %>%
    left_join(team_g, by = c("season", "team")) %>%
    mutate(rate = pmin(dplyr::coalesce(gp, 0L) / team_g, 1))
  x <- mean(per$rate, na.rm = TRUE)
  # WALL: a league-average starter plays ~80-90% of his team's games. Under 0.65 means the rate
  # is gated on one side only (the 2026-09-18 bug), not that linemen miss half the season.
  if (!is.finite(x) || x < 0.65 || x > 1)
    stop(sprintf("OL X = %.3f is not a believable starter availability -- the games-played rate is gated on one side. Use ol_avail = 'player' or 'full' until it is fixed.", x))
  .wf_memo$ol_x <- list(x = x, n = nrow(per),
                        what = "true pass sets: his weeks in tps_pass_block_summary (>= 16 TPS snaps) / his team's weeks in it",
                        by_slot = per %>% group_by(slot = det_position) %>%
                          summarise(x = round(mean(rate, na.rm = TRUE), 3), n = dplyr::n(), .groups = "drop"))
  .wf_memo$ol_x
}

.wf_out_label <- function(o) c(ypa = "YPA", comp = "comp %", rating = "passer rating", sack_rate = "sack %",
                               pressure_rate = "pressure %", ypc = "YPC", rush_td_pg = "rush TD / game")[[o]]
.wf_side_what <- function(side) switch(side, pass_def = "passing ALLOWED", rush_def = "rushing ALLOWED",
                                       pass_off = "own passing", rush_off = "own rushing")

# outcome change (with 95% range) for a vector of input changes dv, named by the fit's inputs
.wf_eff <- function(f, dv) {
  z <- setNames(rep(0, length(f$inputs)), f$inputs)
  h <- intersect(names(dv), f$inputs); z[h] <- dv[h]; z[is.na(z)] <- 0
  bind_rows(lapply(names(f$models), function(o) {
    m <- f$models[[o]]; k <- .wf_scale(o)
    b <- stats::coef(m)[f$inputs]; b[is.na(b)] <- 0
    V <- stats::vcov(m)[f$inputs, f$inputs]; V[is.na(V)] <- 0
    tt <- sum(b * z * k); se <- sqrt(max(as.numeric(t(z * k) %*% V %*% (z * k)), 0))
    tibble(outcome = o, change = tt, lo = tt - 1.96 * se, hi = tt + 1.96 * se)
  }))
}
# c1 and c3 side by side, one row per outcome
.wf_eff_table <- function(effs) {
  out <- NULL
  for (cur in names(effs)) {
    e <- effs[[cur]]
    t <- tibble(outcome = vapply(e$outcome, .wf_out_label, ""))
    t[[paste0(cur, " change")]]    <- sprintf("%+.3f", e$change)
    t[[paste0(cur, " 95% range")]] <- sprintf("[%+.3f, %+.3f]", e$lo, e$hi)
    out <- if (is.null(out)) t else left_join(out, t, by = "outcome")
  }
  out
}

waterfall <- function(team, side = "pass_def", currency = c("c1", "c3"),
                      ol_avail = c("league", "player", "full"), base_season = 2025, show_seats = FALSE) {
  side <- match.arg(side, .WF_SIDES)
  ol_avail <- match.arg(ol_avail)
  if (base_season != 2025) stop("the 2026 member frames only pair with base_season = 2025")
  lens <- c(league = "a", player = "p", full = "f")[[ol_avail]]
  tm   <- .wf_code(team)
  .wf_need(switch(side, pass_def = c("members_cv", "members_pa"), rush_def = "members_ra",
                  pass_off = c("slot_value_26_pb_build", "members_rc"),
                  rush_off = c("slot_value_26_rb_build", "memb_lg_ru")))
  base <- .wf_outcomes_for(side) %>% filter(team == tm, season == base_season)
  if (nrow(base) == 0) stop("no ", base_season, " PFF outcomes for ", tm)
  outs <- .wf_outcome_cols(side)
  what <- .wf_side_what(side)
  has_ol <- side %in% c("pass_off", "rush_off")
  ox <- if (has_ol && ol_avail == "league") .wf_ol_x() else NULL
  per <- list(); notes <- character(0)

  for (cur in currency) {
    f   <- .wf_fit_one(side, cur)
    h25 <- f$data %>% filter(season == base_season) %>% select(team, all_of(f$inputs))
    if (!tm %in% h25$team) stop("no ", base_season, " rooms for ", tm, " (", cur, ")")
    im <- f$imputed %>% filter(team == tm, season == base_season)
    if (nrow(im)) {
      was <- f$inputs[unlist(im[1, f$inputs])]
      if (length(was)) notes <- c(notes, paste0("[", cur, "] ", tm, " ", base_season,
        " positions with no qualified player, set to that season's league median: ", paste(.wf_labels(was), collapse = ", ")))
    }
    h26 <- .wf_inputs_2026(side, cur, lens)
    for (v in setdiff(f$inputs, names(h26))) h26[[v]] <- NA_real_
    all <- h25 %>% tidyr::pivot_longer(-team, names_to = "input", values_to = "v25") %>%
      left_join(h26 %>% select(team, all_of(f$inputs)) %>%
                  tidyr::pivot_longer(-team, names_to = "input", values_to = "v26"),
                by = c("team", "input")) %>%
      mutate(no26 = is.na(v26), v26 = ifelse(no26, v25, v26), dlt = v26 - v25)
    lg <- all %>% group_by(input) %>%
      summarise(lg = if (any(!no26)) mean(dlt[!no26]) else NA_real_, .groups = "drop")
    me <- all %>% filter(team == tm) %>% left_join(lg, by = "input")
    me <- me[match(f$inputs, me$input), ]
    dlt <- setNames(me$dlt, me$input)
    pos <- tibble(position = .wf_labels(f$inputs),
                  `2025` = round(100 * me$v25, 1), `2026` = round(100 * me$v26, 1),
                  change = round(100 * me$dlt, 1), `league avg` = round(100 * me$lg, 1))
    bars <- tibble(position = .wf_labels(f$inputs))
    for (o in outs) {
      b <- stats::coef(f$models[[o]])[f$inputs]; b[is.na(b)] <- 0
      bars[[.wf_out_label(o)]] <- round(b * dlt * .wf_scale(o), 3)
    }
    if (any(me$no26)) notes <- c(notes, paste0("[", cur, "] no 2026 value (no bar): ", paste(.wf_labels(f$inputs[me$no26]), collapse = ", ")))
    per[[cur]] <- list(pos = pos, bars = bars, eff = .wf_eff(f, dlt))
  }

  sm <- tibble(outcome = vapply(outs, .wf_out_label, ""),
               `2025` = round(vapply(outs, function(o) base[[o]] * .wf_scale(o), 0), 2))
  for (cur in names(per)) {
    e <- per[[cur]]$eff
    sm[[paste0(cur, " change")]]   <- sprintf("%+.2f", e$change)
    sm[[paste0(cur, " range")]]    <- sprintf("[%+.2f, %+.2f]", e$lo, e$hi)
    sm[[paste0("2026 ", cur)]]     <- round(sm$`2025` + e$change, 2)
  }
  show_sm <- function(title) {
    cat("\n==================== ", tm, " ", what, ": 2025 -> 2026", title, " ====================\n", sep = "")
    print(as.data.frame(sm), row.names = FALSE)
    cat(if (side %in% c("pass_def", "pass_off")) "comp %, sack %, pressure % are in percentage points. " else "",
        "c1 = season percentiles, c3 = opponent-adjusted.\n", sep = "")
  }
  show_sm("")
  if (!is.null(ox))
    cat("OL X (league-average OL starter availability, 2023-25; ", ox$what, "): ", round(ox$x, 3), "
", sep = "")
  for (cur in names(per)) {
    cat("\n-- ", cur, ": each position, 2025 as played -> 2026 (percentile points) --\n", sep = "")
    print(as.data.frame(per[[cur]]$pos), row.names = FALSE)
    cat("\n-- ", cur, ": what each position's change does to ", what, " --\n", sep = "")
    print(as.data.frame(per[[cur]]$bars), row.names = FALSE)
  }
  if (length(notes)) cat("\n", paste(notes, collapse = "\n"), "\n", sep = "")
  show_sm(" (summary again)")
  cat("2025 = what ", tm, " fielded. 2026 = your 2026 frames",
      if (has_ol) switch(ol_avail, league = " (OL: X x starter + (1 - X) x backup level)",
                         player = " (OL: each starter at his own availability)", full = " (OL: full health)") else "",
      ". 'league avg' = the same change averaged over all 32 teams.\n",
      "Trust the total and its range; single position rows are indicative.\n", sep = "")
  if (show_seats) wf_rooms26(tm, side, ol = lens)
  else cat("Who sits in each 2026 position: wf_rooms26(\"", tm, "\", \"", side, "\")\n", sep = "")
  invisible(per)
}

# ------------------------------------------------ one-off: player vs player -----
## wf_swap("KC", "secondary",  out = "Trent McDuffie", inn = "L'Jarius Sneed")
## wf_swap("KC", "pass_block", out = "Jawaan Taylor",  inn = "Jaylon Moore")
## wf_swap("KC", "secondary",  out = "Jaylen Watson",  value = 0.45)   # rookie: YOUR number
## The incoming player takes the outgoing player's exact 2026 seat and playing time
## (split, band / slot, usage weight). The team's rooms are rebuilt the same way the
## waterfall builds 2026, and the change runs through the waterfall fit numbers.
## Nothing in your session changes: the swap is done on a copy of the member frame.
.WF_UNITS <- list(
  secondary   = list(frame = "members_cv", side = "pass_def", team = "team", split = "split", role = "band",
                     cols = c("grade_f", "grade_p", "supp_f", "supp_p", "c3_f", "adj_p", "avail"),
                     c3 = c("c3_f", "adj_p")),
  pass_rush   = list(frame = "members_pa", side = "pass_def", team = "team", role = "band",
                     cols = c("gf", "gf_p", "c3_f", "c3_p", "avail"), c3 = c("c3_f", "c3_p")),
  run_defense = list(frame = "members_ra", side = "rush_def", team = "team", role = "band",
                     cols = c("grade_f", "grade_p", "stop_f", "stop_p", "c3_f", "c3_p", "avail"),
                     c3 = c("c3_f", "c3_p")),
  pass_block  = list(frame = "slot_value_26_pb_build", side = "pass_off", team = "team_name", role = "slot",
                     cols = c("gf_raw", "gf_raw_av", "gf_adj", "gf_adj_av", "avail"),
                     c3 = c("gf_adj", "gf_adj_av")),
  run_block   = list(frame = "slot_value_26_rb_build", side = "rush_off", team = "team_name", role = "slot",
                     cols = c("V_gap", "V_gap_av", "V_zone", "V_zone_av", "V_c3_gap", "V_c3_gap_av",
                              "V_c3_zone", "V_c3_zone_av", "avail"),
                     c3 = c("V_c3_gap", "V_c3_gap_av", "V_c3_zone", "V_c3_zone_av")),
  receiving   = list(frame = "members_rc", side = "pass_off", team = "team_name", role = "band",
                     cols = c("mg_f", "mg_p", "my_f", "my_p", "zg_f", "zg_p", "zy_f", "zy_p",
                              "c3mg_f", "c3mg_p", "c3my_f", "c3my_p", "c3zg_f", "c3zg_p", "c3zy_f", "c3zy_p", "avail"),
                     c3 = c("c3mg_f", "c3mg_p", "c3my_f", "c3my_p", "c3zg_f", "c3zg_p", "c3zy_f", "c3zy_p")),
  rushing     = list(frame = "memb_lg_ru", side = "rush_off", team = "team", role = NULL,
                     cols = c("gf", "gf_av", "mf", "mf_av", "V_c3", "V_c3_av", "avail"), c3 = c("V_c3", "V_c3_av")))

# rows for one player by name: exact (case-insensitive) first, unique substring as fallback
.wf_find <- function(d, name, what) {
  nc <- intersect(c("roster_name", "full_name", "player"), names(d))[1]
  if (is.na(nc)) stop("the frame has no name column (roster_name / full_name / player)")
  nn <- tolower(d[[nc]])
  i <- which(nn == tolower(name))
  if (!length(i)) i <- grep(tolower(name), nn, fixed = TRUE)
  if (!length(i)) {
    s <- unique(d[[nc]][agrep(tolower(name), nn, max.distance = 3, ignore.case = TRUE)])
    stop("no ", what, " matching '", name, "'.",
         if (length(s)) paste0(" Did you mean: ", paste(utils::head(s, 8), collapse = ", "), "?") else "")
  }
  who <- if ("player_id" %in% names(d)) unique(stats::na.omit(d$player_id[i])) else unique(d[[nc]][i])
  if (length(who) > 1)
    stop("'", name, "' matches ", length(who), " players: ", paste(unique(d[[nc]][i]), collapse = " / "),
         " -- use the exact full name.")
  i
}

wf_swap <- function(team, unit, out, inn = NULL, value = NULL, value_c3 = value,
                    currency = c("c1", "c3"), ol_avail = c("league", "player", "full")) {
  unit <- match.arg(unit, names(.WF_UNITS))
  ol_avail <- match.arg(ol_avail)
  u <- .WF_UNITS[[unit]]
  if (is.null(inn) == is.null(value))
    stop("give inn = a player in ", u$frame, ", or value = your percentile (0-1), not both.")
  .wf_need(u$frame)
  lens <- c(league = "a", player = "p", full = "f")[[ol_avail]]
  tm <- .wf_code(team)
  d0 <- .wf_get(u$frame)
  nc <- intersect(c("roster_name", "full_name", "player"), names(d0))[1]
  on_team <- which(.wf_code(d0[[u$team]]) == tm)
  if (!length(on_team)) stop("no ", u$frame, " rows for ", tm)
  io <- on_team[.wf_find(d0[on_team, ], out, paste0("player on ", tm))]
  cols <- intersect(u$cols, names(d0))
  d1 <- d0
  if (!is.null(inn)) {
    ii <- .wf_find(d0, inn, "incoming player")
    for (k in io) {
      src <- ii
      if (!is.null(u$split)) src <- ii[d0[[u$split]][ii] == d0[[u$split]][k]]
      if (!length(src))
        stop(d0[[nc]][ii[1]], " has no ", d0[[u$split]][k], " row in ", u$frame, " -- that split cannot be filled from him.")
      for (cc in cols) d1[[cc]][k] <- d0[[cc]][src[1]]
    }
    in_label <- paste0(d0[[nc]][ii[1]], " (", d0[[u$team]][ii[1]], ")")
    if (!is.null(u$role) && u$role %in% names(d0)) {
      r_out <- unique(d0[[u$role]][io]); r_in <- unique(d0[[u$role]][ii])
      if (!all(r_in %in% r_out))
        cat("NOTE: ", d0[[nc]][ii[1]], " is priced as ", paste(r_in, collapse = "/"), "; the seat is ",
            paste(r_out, collapse = "/"), ". His percentile comes from his own ", u$role, " pool.\n", sep = "")
    }
  } else {
    for (k in io) {
      for (cc in setdiff(cols, c(u$c3, "avail"))) d1[[cc]][k] <- value
      for (cc in intersect(u$c3, cols)) d1[[cc]][k] <- value_c3
    }
    in_label <- paste0("your number ", value, if (!identical(value_c3, value)) paste0(" (c3 ", value_c3, ")") else "")
  }

  cat("\n== ", tm, " ", unit, ": OUT ", d0[[nc]][io[1]], "  ->  IN ", in_label,
      ", in his seat and playing time ==\n", sep = "")
  pay <- u$side %in% c("pass_def", "rush_def")
  res <- list()
  for (cur in currency) {
    f  <- .wf_fit_one(u$side, cur)
    vv <- function(d) {
      x <- .wf_inputs_2026(u$side, cur, lens, frames = setNames(list(d), u$frame)) %>% filter(team == tm)
      z <- setNames(rep(NA_real_, length(f$inputs)), f$inputs)
      if (nrow(x)) { h <- intersect(f$inputs, names(x)); z[h] <- unlist(x[1, h]) }
      z
    }
    b0 <- vv(d0); b1 <- vv(d1)
    dl <- b1 - b0; dl[is.na(dl)] <- 0
    mv <- which(abs(dl) > 1e-9)
    rooms <- tibble(input = .wf_labels(f$inputs[mv]), before = round(100 * b0[mv], 1),
                    after = round(100 * b1[mv], 1), change = round(100 * dl[mv], 1))
    eff <- tibble(outcome = .wf_outcome_cols(u$side), change = NA_real_, range = NA_character_)
    for (i in seq_along(eff$outcome)) {
      o <- eff$outcome[i]; m <- f$models[[o]]; k <- .wf_scale(o)
      bb <- stats::coef(m)[f$inputs]; bb[is.na(bb)] <- 0
      V  <- stats::vcov(m)[f$inputs, f$inputs]; V[is.na(V)] <- 0
      tt <- sum(bb * dl * k)
      se <- sqrt(max(as.numeric(t(dl * k) %*% V %*% (dl * k)), 0))
      eff$change[i] <- round(tt, 3)
      eff$range[i]  <- sprintf("[%+.3f, %+.3f]", tt - 1.96 * se, tt + 1.96 * se)
    }
    cat("\n-- ", cur, ": rooms that move (percentile points) --\n", sep = "")
    if (nrow(rooms)) print(as.data.frame(rooms), row.names = FALSE) else cat("(no room moves: same values)\n")
    cat("change in ", if (pay) "what the defense ALLOWS" else "the offense's own line",
        " (comp %, sack %, pressure % in points):\n", sep = "")
    print(as.data.frame(eff), row.names = FALSE)
    res[[cur]] <- list(rooms = rooms, effect = eff)
  }
  cat("\nRead: IN takes OUT's exact seat and playing time. ",
      if (unit %in% c("pass_block", "run_block"))
        paste0("OL 2026 = ", switch(ol_avail, league = "X x starter + (1 - X) x backup level (X from data)",
                                    player = "each starter at his own availability", full = "full health"), ". ")
      else "Full-health values (seat depth is the rotation). ",
      "Range = 95% from the fit.\n", sep = "")
  invisible(res)
}

# ------------------------------------------- player vs player: no seats, no roster -----
## wf_pvp("secondary", a = c("Jaylen Watson", "Trent McDuffie"), b = c("ROOKIE", "L'Jarius Sneed"))
## wf_pvp(c("secondary", "run_defense"), a = c("Chamarri Conner", "Bryan Cook"),
##        b = c("Jaden Hicks", "Alohi Gilman"))
## wf_pvp(c("pass_block", "run_block"), a = c("Josh Simmons", "Jawaan Taylor"), b = c("Jaylon Moore", "ROOKIE"))
## B steps into A's ROLE: A's position (band / split / slot) and A's playing time.
## Values are your 2026 full-health numbers from the member frames, wherever each player
## is. B minus A at each stat, times the role's share of its position room (A's usage /
## the league's median room usage there; OL: one slot, times X), is the change in that
## position's value. Those changes run through the waterfall fit -> outcome change.
## OL only: a lineman who holds no 2026 slot (off every roster, or a backup) is still
## valued -- the same 2025 + 2024 blend your slot frames give everyone else -- and a NOTE
## line says he is off the rosters. wf_pvp only; the waterfall never includes him:
##   wf_pvp(c("pass_block", "run_block"), a = "Taylor Decker", b = "VET BACKUP")
## Reference players (printed on every row, so you never guess):
##   ROOKIE     = your entry-year prior for that position
##   VET BACKUP = a veteran backup: priced that season, not his entry year, no qualifying
##                season in the two before (your secondary law's vet_grade; the same rule
##                measured on your seats for every other unit)
##   a number (0-1) = your percentile
## wf_priors(unit) prints every reference value; wf_without(team, unit, player) = what one
## player adds to his team as it lines up (injuries, and Waddle's domino effect).
.WF_PAIRS <- list(
  secondary   = list(c("grade_f", "grade_p"), c("supp_f", "supp_p"), c("c3_f", "adj_p")),
  pass_rush   = list(c("gf", "gf_p"), c("c3_f", "c3_p")),
  run_defense = list(c("grade_f", "grade_p"), c("stop_f", "stop_p"), c("c3_f", "c3_p")),
  pass_block  = list(c("gf_raw", "gf_raw_av"), c("gf_adj", "gf_adj_av")),
  run_block   = list(c("V_gap", "V_gap_av"), c("V_zone", "V_zone_av"), c("V_c3_gap", "V_c3_gap_av"), c("V_c3_zone", "V_c3_zone_av")),
  receiving   = list(c("mg_f", "mg_p"), c("my_f", "my_p"), c("zg_f", "zg_p"), c("zy_f", "zy_p"),
                     c("c3mg_f", "c3mg_p"), c("c3my_f", "c3my_p"), c("c3zg_f", "c3zg_p"), c("c3zy_f", "c3zy_p")),
  rushing     = list(c("gf", "gf_av"), c("mf", "mf_av"), c("V_c3", "V_c3_av")))
.WF_GRP <- list(secondary = c("split", "band"), pass_rush = "band", run_defense = "band",
                pass_block = "slot", run_block = "slot", receiving = "band", rushing = character(0))

.wf_col_label <- function(unit, col) {
  lab <- switch(unit,
    secondary   = c(grade_f = "coverage grade", supp_f = "zone completion suppression", c3_f = "coverage c3"),
    pass_rush   = c(gf = "pass-rush grade (TPS)", c3_f = "pass-rush c3"),
    run_defense = c(grade_f = "run-defense grade", stop_f = "stop %", c3_f = "run-defense c3"),
    pass_block  = c(gf_raw = "pass-block grade (TPS)", gf_adj = "pass-block c3"),
    run_block   = c(V_gap = "gap run-block", V_zone = "zone run-block", V_c3_gap = "gap c3", V_c3_zone = "zone c3"),
    receiving   = c(mg_f = "man grade", my_f = "man YPRR", zg_f = "zone grade", zy_f = "zone YPRR",
                    c3mg_f = "man grade c3", c3my_f = "man YPRR c3", c3zg_f = "zone grade c3", c3zy_f = "zone YPRR c3"),
    rushing     = c(gf = "run grade", mf = "missed tackles forced", V_c3 = "backs c3"))
  if (col %in% names(lab)) lab[[col]] else col
}

# waterfall input key -> member-frame column, for one role row
.wf_pvp_keys <- function(unit, row) {
  s <- if ("split" %in% names(row)) row$split else NA; bd <- if ("band" %in% names(row)) row$band else NA
  sl <- if ("slot" %in% names(row)) row$slot else NA
  switch(unit,
    secondary   = c(setNames("grade_f", paste0("cov_", s, "_", bd, "_grade")),
                    setNames("c3_f", paste0("cov_", s, "_", bd, "_c3")),
                    if (identical(s, "zone") && bd %in% c("CB", "S")) setNames("supp_f", paste0("cov_zone_", bd, "_supp"))),
    pass_rush   = c(setNames("gf", paste0("pr_", bd, "_grade")), setNames("c3_f", paste0("pr_", bd, "_c3"))),
    run_defense = c(setNames("grade_f", paste0("rd_", bd, "_grade")), setNames("stop_f", paste0("rd_", bd, "_stop")),
                    setNames("c3_f", paste0("rd_", bd, "_c3"))),
    pass_block  = c(setNames("gf_raw", paste0("ol_", sl, "_tps")), setNames("gf_adj", paste0("ol_", sl, "_c3"))),
    run_block   = c(ol_gap = "V_gap", ol_zone = "V_zone", ol_gap_c3 = "V_c3_gap", ol_zone_c3 = "V_c3_zone"),
    receiving   = c(rc_man_grade = "mg_f", rc_man_yprr = "my_f", rc_zone_grade = "zg_f", rc_zone_yprr = "zy_f",
                    rc_man_grade_c3 = "c3mg_f", rc_man_yprr_c3 = "c3my_f", rc_zone_grade_c3 = "c3zg_f", rc_zone_yprr_c3 = "c3zy_f"),
    rushing     = c(rb_grade = "gf", rb_mtf = "mf", rb_c3 = "V_c3"))
}

.wf_pick1 <- function(d, key, val, col) {
  if (is.null(d) || !col %in% names(d) || !key %in% names(d)) return(NA_real_)
  v <- d[[col]][d[[key]] == val]; if (length(v)) v[1] else NA_real_
}
.wf_g1 <- function(f) if (.wf_has(f)) .wf_get(f) else NULL

# your entry-year prior for one role row (NA where your chain has none)
.wf_rookie_vals <- function(unit, row) {
  pk <- .wf_pick1; g1 <- .wf_g1
  if (unit == "secondary") {
    pl <- g1("prior_law_sec_lg"); c3 <- g1("c3_entry_prior_cv")
    pl <- if (!is.null(pl)) pl[pl$split == row$split, ] else NULL
    c3 <- if (!is.null(c3)) c3[c3$split == row$split, ] else NULL
    return(c(grade_f = pk(pl, "band", row$band, "rookie_grade"), supp_f = pk(pl, "band", row$band, "pr_supp"),
             c3_f = pk(c3, "band", row$band, "pr_c3")))
  }
  if (unit == "pass_rush")
    return(c(gf = pk(g1("prush_rookie_prior"), "band", row$band, "pr_grade"), c3_f = NA_real_))
  if (unit == "run_defense") {
    rp <- g1("rundef_rookie_prior")
    return(c(grade_f = pk(rp, "band", row$band, "pr_grade"), stop_f = pk(rp, "band", row$band, "pr_stop"), c3_f = NA_real_))
  }
  if (unit == "pass_block")
    return(c(gf_raw = pk(g1("rookie_prior"), "det_position", row$slot, "pr_tps"),
             gf_adj = pk(g1("c3_rookie_prior_ol"), "band", row$slot, "pr_c3")))
  if (unit == "run_block") {
    rp <- g1("rookie_prior"); sp <- g1("c3_scheme_prior_rb")
    return(c(V_gap = pk(rp, "det_position", row$slot, "pr_gap"), V_zone = pk(rp, "det_position", row$slot, "pr_zone"),
             V_c3_gap = pk(sp, "band", row$slot, "pr_c3_gap"), V_c3_zone = pk(sp, "band", row$slot, "pr_c3_zone")))
  }
  if (unit == "receiving") {
    rr <- g1("rec_rookie_prior")
    mg <- pk(rr, "band", row$band, "pr_man_grade");  my <- pk(rr, "band", row$band, "pr_man_yprr")
    zg <- pk(rr, "band", row$band, "pr_zone_grade"); zy <- pk(rr, "band", row$band, "pr_zone_yprr")
    return(c(mg_f = mg, my_f = my, zg_f = zg, zy_f = zy, c3mg_f = mg, c3my_f = my, c3zg_f = zg, c3zy_f = zy))
  }
  if (unit == "rushing") {
    rp <- g1("rush_rookie_prior"); c3 <- g1("c3_rookie_prior_ru")
    return(c(gf = if (!is.null(rp)) rp$pr_grun[1] else NA_real_, mf = if (!is.null(rp)) rp$pr_mtf[1] else NA_real_,
             V_c3 = if (!is.null(c3)) c3$pr_c3[1] else NA_real_))
  }
  NULL
}
# VET BACKUP at a position: players priced that season (a qualifying percentile), not in
# their entry year (seen in your frames before), with no qualifying season in the two
# seasons before -- your secondary law's promotion-class vet (vet_grade); the same rule
# measured on your seats for every other unit. Median by band / slot, seasons 2018+.
.wf_vetbk <- function(unit) {
  key <- paste0("vetbk_", unit)
  if (!is.null(.wf_memo[[key]])) return(.wf_memo[[key]])
  sp <- switch(unit,
    secondary   = list(seats = .wf_cov_rooms, map = c(grade = "grade_f", supp = "supp_f", c3 = "c3_f"),
                       grp = c("split", "band"), seen = c("committee_hist_sec_lg", "cov_pctl_sec_lg"),
                       gated = "cov_pctl_sec_lg", gkey = "split", flag = "unscored"),
    pass_rush   = list(seats = .wf_pr_rooms, map = c(grade = "gf", c3 = "c3_f"), grp = "band",
                       seen = c("qual_lg", "full_pass_rush_qbgrp"), gated = c("cur_lg", "prush_tps_season_pctl_sos")),
    run_defense = list(seats = .wf_rd_rooms, map = c(grade = "grade_f", stop = "stop_f", c3 = "c3_f"), grp = "band",
                       seen = "run_defense_qbgrp", gated = "rundef_season_pctl_sos"),
    pass_block  = list(seats = .wf_ol_rooms, map = c(tps = "gf_raw", pb_c3 = "gf_adj"), grp = "band",
                       seen = "tps_pass_block_summary", gated = "tps_pass_block_player_season_summary", flag = "unscored"),
    run_block   = list(seats = .wf_ol_rooms, map = c(gap = "V_gap", zone = "V_zone", gap_c3 = "V_c3_gap", zone_c3 = "V_c3_zone"),
                       grp = "band", seen = "tps_pass_block_summary", gated = "tps_pass_block_player_season_summary",
                       flag = "unscored"),
    receiving   = list(seats = .wf_rc_rooms, map = c(mg = "mg_f", my = "my_f", zg = "zg_f", zy = "zy_f",
                       g3_man = "c3mg_f", y3_man = "c3my_f", g3_zone = "c3zg_f", y3_zone = "c3zy_f"), grp = "band",
                       seen = "receiving_func_base", gated = "rec_season_pctl_sos"),
    rushing     = list(seats = .wf_ru_rooms, map = c(grun = "gf", mtf = "mf", c3 = "V_c3"), grp = character(0),
                       seen = "rushing_qbgrp", gated = "rush_season_pctl_sos", flag = "prior_used"))
  s <- sp$seats()$seats %>% filter(season >= 2018)
  seen_f <- sp$seen[vapply(sp$seen, .wf_has, logical(1))]
  first <- bind_rows(lapply(seen_f, function(f) .wf_get(f) %>% distinct(player_id, season))) %>%
    group_by(player_id) %>% summarise(first = min(season), .groups = "drop")
  gated_f <- sp$gated[vapply(sp$gated, .wf_has, logical(1))][1]
  gk <- c("player_id", sp$gkey)
  gt <- .wf_get(gated_f) %>% distinct(across(all_of(c(gk, "season"))))
  prev <- bind_rows(gt %>% mutate(season = season + 1L), gt %>% mutate(season = season + 2L)) %>% distinct()
  s <- s %>% left_join(first, by = "player_id") %>% filter(!is.na(first), first < season) %>%
    anti_join(prev, by = c(gk, "season"))
  if (!is.null(sp$flag) && sp$flag %in% names(s)) s <- s[!s[[sp$flag]] %in% TRUE, ]
  vc <- intersect(names(sp$map), names(s))
  out <- s %>% group_by(across(all_of(sp$grp))) %>%
    summarise(across(all_of(vc), ~ suppressWarnings(stats::median(.x, na.rm = TRUE))),
              n_vet_backup = dplyr::n(), .groups = "drop")
  names(out)[match(vc, names(out))] <- unname(sp$map[vc])
  if (unit %in% c("pass_block", "run_block")) out <- out %>% rename(slot = band)
  if (unit == "secondary" && .wf_has("prior_law_sec_lg")) {      # your law's own vet price wins
    law <- .wf_get("prior_law_sec_lg") %>% select(split, band, any_of(c("vet_grade", "pr_supp")))
    out <- out %>% full_join(law, by = c("split", "band"))
    if (!"grade_f" %in% names(out)) out$grade_f <- NA_real_
    if (!"supp_f"  %in% names(out)) out$supp_f  <- NA_real_
    if ("vet_grade" %in% names(out)) out$grade_f <- dplyr::coalesce(out$vet_grade, out$grade_f)
    if ("pr_supp"   %in% names(out)) out$supp_f  <- dplyr::coalesce(out$pr_supp, out$supp_f)
    out <- out %>% select(-any_of(c("vet_grade", "pr_supp")))
  }
  .wf_memo[[key]] <- out
  out
}
.wf_vetbk_vals <- function(unit, row) {
  tb <- .wf_vetbk(unit)
  fcols <- vapply(.WF_PAIRS[[unit]], function(x) x[1], character(1))
  grp <- setdiff(names(tb), c(fcols, "n_vet_backup"))
  r <- tb
  for (g in grp) if (g %in% names(row)) r <- r[r[[g]] %in% row[[g]], ]
  setNames(vapply(fcols, function(cc) if (nrow(r) && cc %in% names(r)) r[[cc]][1] else NA_real_, 0), fcols)
}

# a lineman who holds no 2026 slot (off every roster, or a backup), valued EXACTLY the way
# your 2026 slot frames value a returning player (league_opp_pass_blocking_schedule.R +
# league_pass_block_availability.R / league_run_block_availability.R): your 2025 + 2024
# blend (blend2, weight = 2025 games / 10), c3 blended the same way, then your slot prior.
# Slot = his 2025 main slot, else his 2024 one. NULL when he has no 2024-25 OL games.
# wf_pvp only: the waterfall / wf_swap / wf_without never see him.
.wf_ol_25 <- function(unit, name) {
  .wf_need(c("all_pass_block_summary", "ol_2025_snaps", "ol_season_pctl", "ol_2024_profile", "blend2", "rookie_prior"))
  SLOTS <- c("LT", "LG", "C", "RG", "RT")
  nm <- .wf_get("all_pass_block_summary") %>%
    filter(season %in% 2024:2025, det_position %in% SLOTS, !is.na(player), !is.na(player_id))
  i <- tryCatch(.wf_find(nm, name, "2024-25 lineman"), error = function(e) {
    if (grepl("^no 2024-25 lineman", conditionMessage(e))) integer(0) else stop(e) })
  if (!length(i)) return(NULL)
  last <- nm[i, ] %>% arrange(desc(season), desc(week))
  pid  <- last$player_id[1]
  b2   <- get("blend2", envir = .GlobalEnv)
  one  <- function(d, col) if (nrow(d) && col %in% names(d)) d[[col]][1] else NA_real_
  co   <- function(...) { v <- c(...); v <- v[!is.na(v)]; if (length(v)) v[1] else NA_real_ }
  at   <- function(f, key, col) {                       # a prior table's value at his slot
    if (!.wf_has(f)) return(NA_real_)
    d <- .wf_get(f); if (!all(c(key, col) %in% names(d))) return(NA_real_)
    d[[col]][match(slot, d[[key]])]
  }
  s25  <- .wf_get("ol_2025_snaps")   %>% filter(player_id == pid)
  p24  <- .wf_get("ol_2024_profile") %>% filter(player_id == pid)
  slot <- co(as.character(one(s25, "det_position")), as.character(one(p24, "det_position_24")))
  if (is.na(slot)) return(NULL)
  p25  <- .wf_get("ol_season_pctl") %>% filter(player_id == pid, det_position == as.character(one(s25, "det_position")))
  g25  <- one(s25, "g_2025_total")
  w25  <- min(co(g25, 0) / 10, 1)
  rp   <- .wf_get("rookie_prior")
  unit_pr <- function(col) if (col %in% names(rp)) mean(rp[[col]], na.rm = TRUE) else NA_real_
  out <- list(player = last$player[1], team = last$team_name[1], slot = slot, g25 = co(g25, 0),
              roster = if (.wf_has("ol_2026_lg")) {
                r <- .wf_get("ol_2026_lg") %>% filter(player_id == pid)
                if (nrow(r)) as.character(r$team_name[1]) else "none" } else NA_character_)
  if (unit == "pass_block") {
    .wf_need(c("pblk_c3_pctl", "c3_rookie_prior_ol"))
    pc <- .wf_get("pblk_c3_pctl") %>% filter(player_id == pid)
    c25 <- pc[pc$season == 2025, ]; c24 <- pc[pc$season == 2024, ]
    out$gf_raw <- co(b2(one(p25, "tps_grade"), one(p24, "tps_grade_24"), w25),
                     at("rookie_prior", "det_position", "pr_tps"), unit_pr("pr_tps"))
    out$gf_adj <- co(b2(one(c25, "c3_pctl"), one(c24, "c3_pctl"), min(co(one(c25, "qual_g"), 0) / 10, 1)),
                     at("c3_rookie_prior_ol", "band", "pr_c3"))
  } else {
    .wf_need(c("rblk_c3_pctl", "c3_scheme_prior_rb"))
    rc <- .wf_get("rblk_c3_pctl") %>% filter(player_id == pid)
    c25 <- rc[rc$season == 2025, ]; c24 <- rc[rc$season == 2024, ]
    pr3 <- co(at("c3_rookie_prior_rb", "band", "pr_c3"),
              if (.wf_has("c3_rookie_prior_rb")) mean(.wf_get("c3_rookie_prior_rb")$pr_c3, na.rm = TRUE) else NA_real_)
    out$V_gap  <- co(b2(one(p25, "gap"),  one(p24, "gap_24"),  w25), at("rookie_prior", "det_position", "pr_gap"),  unit_pr("pr_gap"))
    out$V_zone <- co(b2(one(p25, "zone"), one(p24, "zone_24"), w25), at("rookie_prior", "det_position", "pr_zone"), unit_pr("pr_zone"))
    out$V_c3_gap  <- co(b2(one(c25, "gap_c3_pctl"),  one(c24, "gap_c3_pctl"),  min(co(one(c25, "gap_qual_g"),  0) / 10, 1)),
                        at("c3_scheme_prior_rb", "band", "pr_c3_gap"),  pr3)
    out$V_c3_zone <- co(b2(one(c25, "zone_c3_pctl"), one(c24, "zone_c3_pctl"), min(co(one(c25, "zone_qual_g"), 0) / 10, 1)),
                        at("c3_scheme_prior_rb", "band", "pr_c3_zone"), pr3)
  }
  out
}

wf_pvp <- function(units, a, b, currency = c("c1", "c3")) {
  units <- match.arg(units, names(.WF_UNITS), several.ok = TRUE)
  a <- as.character(a); b <- as.character(b)
  if (length(a) != length(b)) stop("a and b need the same number of players: pairs go in order (a[1] vs b[1], ...).")
  tok  <- function(x) toupper(trimws(x))
  num  <- function(x) suppressWarnings(as.numeric(x))
  is_t <- function(x) tok(x) %in% c("ROOKIE", "VET", "VET BACKUP", "VET_BACKUP", "VETBACKUP") || !is.na(num(x))
  dl <- list(); prof <- list()
  for (unit in units) {
    u <- .WF_UNITS[[unit]]
    if (!.wf_has(u$frame)) { cat("(", unit, ": ", u$frame, " not in session -- skipped)\n", sep = ""); next }
    d  <- .wf_get(u$frame)
    wcol <- intersect(c("usage_w", "uw"), names(d))[1]
    grp <- intersect(.WF_GRP[[unit]], names(d))
    # sgrp = the ROOM the fit prices. Receiving is ONE room per team (the whole corps, .wf_wm over all
    # seats), so a player's share is his usage / the median CORPS usage -- not his share of his band.
    # FIXED 2026-09-19: it used grp (band) and read an RB as 75% of "the room"; he is ~10% of the corps,
    # so every receiving wf_pvp was ~7x too large. Band still picks the ROOKIE / VET BACKUP values.
    sgrp <- if (unit == "receiving") character(0) else grp
    fcols <- vapply(.WF_PAIRS[[unit]], function(x) x[1], character(1))
    repl <- .wf_repl_d(d, setNames(.WF_PAIRS[[unit]], fcols), grp)
    is_ol <- unit %in% c("pass_block", "run_block")
    X <- if (is_ol) .wf_ol_x()$x else 1
    # OL: a lineman who holds no 2026 slot (off every roster, or a backup) is NOTED and valued
    # the same way as everyone else -- your 2025 + 2024 blend (.wf_ol_25). He joins a COPY of
    # the frame inside this call only: your session, the waterfall and the swaps never see him.
    if (is_ol) for (nm in unique(c(a, b))) {
      if (is_t(nm)) next
      hit <- tryCatch(length(.wf_find(d, nm, "player")) > 0,
                      error = function(e) !grepl("^no player", conditionMessage(e)))
      if (hit) next
      h <- .wf_ol_25(unit, nm)
      if (is.null(h)) next
      nr <- as.data.frame(d[1, ]); nr[1, ] <- NA
      nr[[intersect(c("roster_name", "full_name", "player"), names(d))[1]]] <- h$player
      nr$slot <- h$slot; nr[[u$team]] <- h$team
      for (cc in intersect(fcols, names(h))) nr[[cc]] <- h[[cc]]
      d <- bind_rows(d, tibble::as_tibble(nr))
      cat("(", unit, ": NOTE -- ", h$player,
          if (is.na(h$roster)) " holds no 2026 starting slot in your frame"
          else if (h$roster == "none") " is on NO 2026 roster"
          else paste0(" is on ", h$roster, "'s 2026 roster but holds no starting slot in your frame"),
          ". Valued like everyone else: your 2025 + 2024 blend at ", h$slot, " (last team ", h$team, ", ",
          h$g25, " games in 2025). wf_pvp only -- the waterfall does not include him.)\n", sep = "")
    }
    if (!is_ol) {
      tot <- d %>% mutate(.team = .wf_code(.data[[u$team]]), .w = .data[[wcol]]) %>%
        group_by(.team, across(all_of(sgrp))) %>% summarise(tot = sum(.w, na.rm = TRUE), .groups = "drop") %>%
        group_by(across(all_of(sgrp))) %>% summarise(med_tot = stats::median(tot), .groups = "drop")
    }
    find <- function(nm) {
      if (is_t(nm)) return(integer(0))
      tryCatch(.wf_find(d, nm, "player"), error = function(e) {
        if (grepl("^no player", conditionMessage(e))) integer(0) else stop(e) })
    }
    vals <- function(who, rows, row) {
      t <- tok(who)
      if (t == "ROOKIE") { v <- .wf_rookie_vals(unit, row); return(setNames(v[fcols], fcols)) }
      if (t %in% c("VET", "VET BACKUP", "VET_BACKUP", "VETBACKUP")) return(.wf_vetbk_vals(unit, row)[fcols])
      if (!is.na(num(who))) return(setNames(rep(num(who), length(fcols)), fcols))
      src <- rows
      if (!is.null(u$split)) { m <- rows[d[[u$split]][rows] == row[[u$split]]]; if (length(m)) src <- m }
      setNames(vapply(fcols, function(cc) if (cc %in% names(d)) d[[cc]][src[1]] else NA_real_, 0), fcols)
    }
    for (i in seq_along(a)) {
      ra <- find(a[i]); rb <- find(b[i])
      miss <- c(if (!is_t(a[i]) && !length(ra)) a[i], if (!is_t(b[i]) && !length(rb)) b[i])
      if (length(miss)) { cat("(", unit, ": ", paste(miss, collapse = " / "), " not in ", u$frame,
                              if (is_ol) " and has no 2024-25 OL games" else "", " -- pair ", i, " skipped here)\n", sep = ""); next }
      role_rows <- if (length(ra)) ra else rb
      if (!length(role_rows)) { cat("(pair ", i, ": both sides are tokens -- skipped)\n", sep = ""); next }
      for (k in role_rows) {
        row <- d[k, ]
        share <- if (unit == "pass_block") 1 else if (unit == "run_block") 1 / 5 else {
          mt <- if (length(sgrp)) dplyr::semi_join(tot, row[sgrp], by = sgrp)$med_tot[1] else tot$med_tot[1]
          min(row[[wcol]] / mt, 1)
        }
        va <- vals(a[i], ra, row); vb <- vals(b[i], rb, row)
        rk <- .wf_rookie_vals(unit, row); vb2 <- .wf_vetbk_vals(unit, row)
        keys <- .wf_pvp_keys(unit, row)
        for (kk in names(keys)) {
          cc <- keys[[kk]]
          dd <- share * X * (vb[[cc]] - va[[cc]])
          if (!is.na(dd)) dl[[u$side]][kk] <- (if (is.null(dl[[u$side]]) || is.na(dl[[u$side]][kk])) 0 else dl[[u$side]][kk]) + dd
          p1 <- function(x) if (is.null(x) || !cc %in% names(x) || is.na(x[[cc]])) NA_real_ else round(100 * x[[cc]], 1)
          prof[[length(prof) + 1]] <- tibble(
            pair = paste0(a[i], " -> ", b[i]),
            role = paste(unlist(row[intersect(grp, names(row))]), collapse = " "),
            stat = .wf_labels(kk), A = p1(va), B = p1(vb), `B - A` = if (is.na(dd)) NA_real_ else round(100 * (vb[[cc]] - va[[cc]]), 1),
            share = round(share * X, 2), ROOKIE = p1(rk), `VET BACKUP` = p1(vb2))
        }
      }
      if (length(rb) && !is.null(u$role) && length(ra) && !is_t(b[i]) && u$role %in% names(d)) {
        r_a <- unique(d[[u$role]][ra]); r_b <- unique(d[[u$role]][rb])
        if (!all(r_b %in% r_a))
          cat("NOTE (", unit, "): ", b[i], " is priced as ", paste(r_b, collapse = "/"), "; ", a[i], "'s role is ",
              paste(r_a, collapse = "/"), ". His percentile comes from his own pool.\n", sep = "")
      }
    }
  }
  cat("\n==================== PLAYER VS PLAYER ====================\n")
  cat(paste0("  ", a, "  ->  ", b, collapse = "\n"), "\n", sep = "")
  cat("\n-- the players in A's role (percentile points). share = A's share of that position room\n",
      "   (OL: one slot x X). ROOKIE / VET BACKUP = YOUR reference players for that role --\n", sep = "")
  if (length(prof)) print(as.data.frame(bind_rows(prof)), row.names = FALSE)
  res <- list()
  for (side in names(dl)) {
    effs <- list()
    for (cur in currency) effs[[cur]] <- .wf_eff(.wf_fit_one(side, cur), dl[[side]])
    cat("\n-- if B plays A's role: change in ", .wf_side_what(side), " (B minus A",
        if (side %in% c("pass_def", "pass_off")) "; comp %, sack %, pressure % in points" else "", ") --\n", sep = "")
    print(as.data.frame(.wf_eff_table(effs)), row.names = FALSE)
    res[[side]] <- effs
  }
  cat("\nRead: a defense number above 0 = B allows MORE than A; an offense number above 0 = B gains more.\n",
      "The range is 95% from the fit. A blank B - A = no value for that stat (e.g. ROOKIE has no c3 there).\n", sep = "")
  invisible(list(profile = bind_rows(prof), effect = res))
}

## the reference players for a unit: ROOKIE (your entry-year prior) and VET BACKUP
## (a veteran backup) -- per band / split / slot, percentile points
wf_priors <- function(unit) {
  unit <- match.arg(unit, names(.WF_UNITS))
  u <- .WF_UNITS[[unit]]
  .wf_need(u$frame)
  d <- .wf_get(u$frame)
  grp <- intersect(.WF_GRP[[unit]], names(d))
  fcols <- vapply(.WF_PAIRS[[unit]], function(x) x[1], character(1))
  repl <- .wf_repl_d(d, setNames(.WF_PAIRS[[unit]], fcols), grp)
  roles <- if (length(grp)) d %>% distinct(across(all_of(grp))) %>% arrange(across(all_of(grp))) else tibble(.x = 1)
  out <- list()
  for (i in seq_len(nrow(roles))) {
    row <- roles[i, ]
    rk <- .wf_rookie_vals(unit, row); vb2 <- .wf_vetbk_vals(unit, row)
    for (cc in fcols) {
      p1 <- function(x) if (is.null(x) || !cc %in% names(x) || is.na(x[[cc]])) NA_real_ else round(100 * x[[cc]], 1)
      out[[length(out) + 1]] <- tibble(role = if (length(grp)) paste(unlist(row[grp]), collapse = " ") else "all",
                                       stat = .wf_col_label(unit, cc), ROOKIE = p1(rk), `VET BACKUP` = p1(vb2))
    }
  }
  cat("-- ", unit, ": YOUR reference players, percentile points (use as a or b in wf_pvp()) --\n",
      "   ROOKIE = your entry-year prior; VET BACKUP = a veteran backup: priced that season, not his\n",
      "   entry year, no qualifying season in the two before --\n", sep = "")
  print(as.data.frame(bind_rows(out)), row.names = FALSE)
  invisible(bind_rows(out))
}

## what one player adds to his team as it lines up in your 2026 frame: the team WITH him
## minus WITHOUT him. Weighted rooms: his playing time is spread over the rest of the room
## (so Waddle pushes Sutton, Franklin, ... down, the way the corps actually re-weights).
## OL: his slot goes to your 2025 backup-start level. For injuries: wf_without("KC", "pass_rush", "Chris Jones")
wf_without <- function(team, unit, player, currency = c("c1", "c3"), ol_avail = c("league", "player", "full")) {
  unit <- match.arg(unit, names(.WF_UNITS))
  ol_avail <- match.arg(ol_avail)
  u <- .WF_UNITS[[unit]]
  .wf_need(u$frame)
  lens <- c(league = "a", player = "p", full = "f")[[ol_avail]]
  tm <- .wf_code(team)
  d0 <- .wf_get(u$frame)
  on <- which(.wf_code(d0[[u$team]]) == tm)
  if (!length(on)) stop("no ", u$frame, " rows for ", tm)
  r <- on[.wf_find(d0[on, ], player, paste0("player on ", tm))]
  if (unit %in% c("pass_block", "run_block")) {
    fcols <- vapply(.WF_PAIRS[[unit]], function(x) x[1], character(1))
    repl <- .wf_repl_d(d0, setNames(.WF_PAIRS[[unit]], fcols), "slot")
    d1 <- d0
    for (k in r) for (pr in .WF_PAIRS[[unit]]) {
      rv <- if (!is.null(repl) && pr[1] %in% names(repl)) repl[[pr[1]]][repl$slot == d0$slot[k]] else numeric(0)
      if (length(rv) && !is.na(rv[1])) for (cc in intersect(pr, names(d1))) d1[[cc]][k] <- rv[1]
    }
    how <- "his slot at your 2025 backup-start level"
  } else {
    d1 <- d0[-r, ]
    how <- "his playing time spread over the rest of the room"
  }
  effs <- list(); rooms <- list()
  for (cur in currency) {
    f <- .wf_fit_one(u$side, cur)
    v <- function(dd) {
      x <- .wf_inputs_2026(u$side, cur, lens, frames = setNames(list(dd), u$frame)) %>% filter(team == tm)
      z <- setNames(rep(NA_real_, length(f$inputs)), f$inputs)
      if (nrow(x)) { h <- intersect(f$inputs, names(x)); z[h] <- unlist(x[1, h]) }
      z
    }
    w <- v(d0); wo <- v(d1); dl <- w - wo; dl[is.na(dl)] <- 0
    mv <- which(abs(dl) > 1e-9)
    rooms[[cur]] <- tibble(position = .wf_labels(f$inputs[mv]), `with him` = round(100 * w[mv], 1),
                           `without him` = round(100 * wo[mv], 1), `he adds` = round(100 * dl[mv], 1))
    effs[[cur]] <- .wf_eff(f, dl)
  }
  cat("\n==================== ", player, " on ", tm, ": WITH him minus WITHOUT him ====================\n", sep = "")
  cat("(without him = ", how, ")\n", sep = "")
  for (cur in names(rooms)) {
    cat("\n-- ", cur, ": positions that move (percentile points) --\n", sep = "")
    if (nrow(rooms[[cur]])) print(as.data.frame(rooms[[cur]]), row.names = FALSE) else cat("(none)\n")
  }
  cat("\n-- what he adds to ", tm, "'s ", .wf_side_what(u$side),
      if (u$side %in% c("pass_def", "pass_off")) " (comp %, sack %, pressure % in points)" else "", " --\n", sep = "")
  print(as.data.frame(.wf_eff_table(effs)), row.names = FALSE)
  if (u$side %in% c("pass_def", "rush_def"))
    cat("A negative number = the defense allows LESS with him.\n")
  invisible(list(rooms = rooms, effect = effs))
}

## who sat in each room for a team-season (history)
wf_rooms <- function(team, season, side = "pass_def") {
  side <- match.arg(side, .WF_SIDES)
  tm <- .wf_code(team); ss <- season
  show <- function(x, cols, title) {
    cat("\n-- ", title, " --\n", sep = "")
    d <- x %>% filter(team == tm, season == ss)
    if (!nrow(d)) { cat("(no seats)\n"); return(invisible(NULL)) }
    print(as.data.frame(d %>% select(any_of(cols)) %>%
                          mutate(across(where(is.double) & !any_of("vol"), ~ round(.x * 100, 1)))),
          row.names = FALSE)
  }
  if (side == "pass_def") {
    s <- .wf_cov_rooms()$seats %>% arrange(split, band, seat)
    show(s, c("split", "band", "seat", "player", "player_id", "vol", "grade", "supp", "c3", "unscored"),
         "coverage seats (vol = split snaps; unscored = no percentile that season, priced at your rookie / vet prior)")
    show(.wf_pr_rooms()$seats %>% arrange(band, seat), c("band", "seat", "player", "vol", "grade", "c3"),
         "pass-rush seats (vol = true-pass-set snaps)")
  } else if (side == "rush_def") {
    show(.wf_rd_rooms()$seats %>% arrange(band, seat), c("band", "seat", "player", "vol", "grade", "stop", "c3"),
         "run-defense seats (vol = run snaps)")
  } else if (side == "pass_off") {
    show(.wf_ol_rooms()$seats %>% arrange(factor(band, c("LT", "LG", "C", "RG", "RT")), desc(vol)),
         c("band", "player", "vol", "tps", "pb_c3", "unscored"),
         "OL starters by slot (vol = games he was that game's true-pass-set snap leader there; unscored = priced at your slot prior)")
    show(.wf_rc_rooms()$seats %>% arrange(seat), c("seat", "band", "player", "player_id", "vol", "mg", "my", "zg", "zy"),
         "receiving corps (vol = routes; blank = seated, no percentile)")
  } else {
    show(.wf_ol_rooms()$seats %>% arrange(factor(band, c("LT", "LG", "C", "RG", "RT")), desc(vol)),
         c("band", "player", "vol", "gap", "zone", "gap_c3", "zone_c3", "unscored"),
         "OL starters by slot (vol = games he was that game's true-pass-set snap leader there; unscored = priced at your slot prior)")
    show(.wf_ru_rooms()$seats %>% arrange(seat), c("seat", "player", "player_id", "vol", "grun", "mtf", "c3", "prior_used"),
         "backs (vol = attempts in 4+ attempt games; prior_used = priced at entry-year prior)")
  }
  invisible(NULL)
}

## who sits in each 2026 room (straight from your member frames)
wf_rooms26 <- function(team, side = "pass_def", ol = c("a", "p", "f")) {
  side <- match.arg(side, .WF_SIDES)
  ol <- match.arg(ol)
  tm <- .wf_code(team)
  ids  <- c("split", "band", "slot", "roster_name", "full_name", "player", "status",
            "prior_used", "is_phantom", "phantom", "avail_src", "avail", "usage_w", "uw")
  show <- function(f, vals, title, pairs = NULL, u = "pb") {
    cat("\n-- 2026 ", title, " (", f, ") --\n", sep = "")
    if (!.wf_has(f)) { cat("(not in session)\n"); return(invisible(NULL)) }
    d <- .wf_get(f)
    if (!is.null(pairs) && ol == "a") {            # OL: the value the waterfall uses (league-wide recovery first)
      d <- .wf_avg_lens(d, pairs, "slot", NULL, abar = .wf_ol_x(u)$x)
      vals <- c(paste0(vapply(pairs, function(x) x[1], character(1)), "__a"), vals)
      cat("(<col>__a = the value used: X x player + (1 - X) x league backup-start level at the slot)\n")
    }
    tc <- intersect(c("team", "team_name"), names(d))[1]
    d <- d %>% filter(.wf_code(.data[[tc]]) == tm)
    if (!nrow(d)) { cat("(no rows for ", tm, ")\n", sep = ""); return(invisible(NULL)) }
    keep <- intersect(c(ids, vals), names(d))
    ord  <- intersect(c("split", "band", "slot"), names(d))
    if (length(ord)) d <- d %>% arrange(across(all_of(ord)))
    print(as.data.frame(d %>% select(all_of(keep)) %>%
                          mutate(across(where(is.double) & !any_of(c("usage_w", "uw")), ~ round(.x * 100, 1)),
                                 across(any_of(c("usage_w", "uw")), ~ round(.x)))),
          row.names = FALSE)
  }
  if (side == "pass_def") {
    show("members_cv", c("grade_f", "grade_p", "supp_f", "supp_p", "c3_f", "adj_p"), "coverage seats")
    show("members_pa", c("gf", "gf_p", "c3_f", "c3_p"), "pass-rush seats")
  } else if (side == "rush_def") {
    show("members_ra", c("grade_f", "grade_p", "stop_f", "stop_p", "c3_f", "c3_p"), "run-defense seats")
  } else if (side == "pass_off") {
    show("slot_value_26_pb_build", c("gf_raw", "gf_raw_av", "gf_adj", "gf_adj_av"), "OL slots",
         pairs = list(c("gf_raw", "gf_raw_av"), c("gf_adj", "gf_adj_av")), u = "pb")
    show("members_rc", c("mg_f", "mg_p", "my_f", "my_p", "zg_f", "zg_p", "zy_f", "zy_p"), "receiving corps")
  } else {
    show("slot_value_26_rb_build", c("V_gap", "V_gap_av", "V_zone", "V_zone_av"), "OL slots",
         pairs = list(c("V_gap", "V_gap_av"), c("V_zone", "V_zone_av")), u = "rb")
    show("memb_lg_ru", c("gf", "gf_av", "mf", "mf_av", "V_c3", "V_c3_av"), "backs")
  }
  invisible(NULL)
}
