####
#### impact_bridge.R — v5 2026-09-14. Standalone. Read-only.
#### Changes nothing, saves nothing, deletes nothing, sources nothing.
#### V5 NOTE: the bottom section adds pr_card / rd_card / rb_card, upgrades
#### wr_card with the machine percentiles (rec_season_pctl_sos), adds bridge
#### units "prush" and "rundef" (rushing-allowed outcomes), and upgrades
#### (.br_need no longer loads cache files -- removed in V6.)
#### Where a v5 function redefines an earlier one, THE LATER DEFINITION WINS.
#### V8 (bottom, 2026-09-15, Claude on Andy's authority): the impact bridges are
#### RETIRED (they stop and name the replacement in waterfall_bridge.R);
#### player_swap() added; wr_card pool fixed; anchors print YOUR priors.
#### V9 (bottom): ol_card() = pass AND run blocking for any lineman; ot_card = ol_card.
####
#### THE BRIDGE: your percentiles -> ypa / comp% / passer rating / xtds.
####
#### v4 (depth pass, on top of v3's verified fixes):
####   - cov_card now reads YOUR coverage season summaries
####     (coverage_man/zone/slot_player_season_summary from pff_pass_coverage_AWS.R):
####     6 core metrics x man/zone(/slot), every percentile within
####     position x season — CB vs CB, S vs S, LB vs LB, exactly as the
####     diet-matrix / heatmap layer computes them. Grade, Snap/Tgt, Catch%,
####     PBU%, Y/CovSnp, QBR-against. Per season, plus weighted + unweighted cards.
####   - bridge_fit_stats("cb", family = "man"/"zone"/"combined"): per-stat
####     bridge built from those same percentiles. Room membership by season
####     coverage snaps from final_coverage_df_qbgrp. "lb" unit added.
####   - coverage raw frames no longer assumed to carry final_position —
####     derived from position/adv_position with the same mapping as
####     pff_pass_coverage_AWS.R:130-136.
####   - ot_card is now all-offensive-line: tackles, guards, centers. The OT
####     room bridge stays tackles-only; the card covers everyone.
####
#### v3 fixes (retained): slope per 10 points = 0.1*cov/var; receiver joins on
#### receiver_id; tps QB-stint rows collapsed; raw fallback for players missing
#### from the season summary; cards print weighted AND unweighted.
####
#### Known caveat: WR production percentiles are RAW league-wide production
#### (yds/tgt, catch%, EPA/tgt) with team context inside. They are starting
#### points for YOUR from/to reads, not verdicts.
####
#### Functions:
####   bridge_fit("cb"/"s"/"ot"/"wr1")   — buckets + slope per 10 points + cor
####   bridge_delta(unit, from, to)      — historical worth of a room move
####   cov_card("L'Jarius Sneed")        — man/zone(/slot) per-stat pctls/season
####   ot_card("Jawaan Taylor")          — any OL: TPS pctls, raw fallback
####   wr_card("Jaylen Waddle")          — production pctls by season (id-joined)
####   rookie_anchor("cb"/"s"/"ot") / backup_anchor(...)  — empty-year defaults
####   bridge_fit_stats("cb", family)    — per-stat room bridge (man/zone/comb)
####   bridge_stats_delta("cb", moves)   — apply per-stat moves
####   bridge_wr1_add()                  — teams that ADDED a proven #1 WR
####
#### Defense units (cb, cbx, s, lb) map onto PASSING ALLOWED: negative delta =
#### defense improves. Offense units (ot, wr1) map onto the team's own passing.
####
#### Needs in session, per unit (gates tell you what to source):
####   coverage_summary, coverage_man_player_season_summary,
####   coverage_zone_player_season_summary, (slot optional),
####   final_coverage_df_qbgrp                 <- pff_stats/secondary/pff_pass_coverage_AWS.R
####   all_pass_block_summary,
####   tps_pass_block_player_season_summary    <- pff_stats/pass_block/pff_pass_block_AWS.R
####   receiving_func_base                     <- pff_stats/receiving/receiving_stats_build_AWS.R
####   combined_grade_epa_summary, run_athena_query  <- bridge outcomes (PFF totals)
####   combined_pbp                            <- ONLY the wr1 pieces (do not use)
####
#### HONESTY PRINTED ON EVERY TABLE: cross-sectional team-seasons, not causal.
#### A good QB raises his WR1's percentile. Scheme confounds everything. One
#### game's noise swamps any season-rate delta.
####

#### ---------------- internals ----------------

.br_need <- function(frames) {
  missing <- frames[!vapply(frames, exists, logical(1), envir = .GlobalEnv)]
  if (length(missing))
    stop("missing frame(s): ", paste(missing, collapse = ", "),
         " — source the file named in this file's header for that unit first.")
  invisible(NULL)
}

.br_find <- function(df, name_col, name) {
  nn <- tolower(df[[name_col]])
  idx <- which(nn == tolower(name))
  if (length(idx) == 0) idx <- grep(tolower(name), nn, fixed = TRUE)
  if (length(idx) == 0) {
    sugg <- unique(df[[name_col]][agrep(tolower(name), nn, max.distance = 3, ignore.case = TRUE)])
    stop("no player matching '", name, "'.",
         if (length(sugg)) paste0(" Did you mean: ", paste(utils::head(sugg, 8), collapse = ", "), "?") else "")
  }
  idx
}

# raw coverage frames don't always carry final_position — derive it with the
# same mapping as pff_pass_coverage_AWS.R:130-136
.br_ensure_final_position <- function(cov) {
  if ("final_position" %in% names(cov)) return(cov)
  pos_col <- if ("adv_position" %in% names(cov)) "adv_position" else "position"
  if (!pos_col %in% names(cov))
    stop("coverage frame has no position column — source pff_pass_coverage_AWS.R fully.")
  p <- cov[[pos_col]]
  cov$final_position <- dplyr::case_when(
    p == "MLB"         ~ "MLB",
    grepl("LB$", p)    ~ "LB",
    p %in% c("FS","SS") ~ "S",
    p == "SCB"         ~ "SCB",
    grepl("CB", p)     ~ "CB",
    p %in% c("DE","ED","DI","DT","NT","DLE","DRE","DLT","DRT") ~ "DL",
    TRUE ~ "OTHER")
  cov
}

# ---- OUTCOMES FROM PFF, NOT PLAY-BY-PLAY (Claude, 2026-09-14, per Andy) ----
# Team-week passing lines = every QB on that team that week, summed from PFF
# nfl_data.passing_pressure (pressure + no-pressure splits cover every dropback).
# The opponent comes from YOUR id table combined_grade_epa_summary (posteam -> opp),
# so side = "defense" means passing ALLOWED. Pulled once per session and held in a
# private environment (.br_memo): nothing new lands in your global session.
.br_memo <- new.env()

.br_pff_ids <- function() {
  .br_need("combined_grade_epa_summary")
  get("combined_grade_epa_summary", envir = .GlobalEnv) %>% ungroup() %>%
    distinct(posteam, opp, week, season) %>%
    mutate(week = as.integer(week), season = as.integer(season))
}

.br_pff_passing_wk <- function() {
  if (!is.null(.br_memo$pass_wk)) return(.br_memo$pass_wk)
  .br_need("run_athena_query")
  wk <- get("run_athena_query", envir = .GlobalEnv)("
    SELECT  team_name, CAST(week AS INTEGER) AS week, CAST(season AS INTEGER) AS season,
            SUM(pressure_dropbacks + no_pressure_dropbacks)         AS dropbacks,
            SUM(pressure_dropbacks)                                 AS pressured,
            SUM(pressure_attempts + no_pressure_attempts)           AS att,
            SUM(pressure_completions + no_pressure_completions)     AS comp_n,
            SUM(pressure_yards + no_pressure_yards)                 AS yds,
            SUM(pressure_touchdowns + no_pressure_touchdowns)       AS td_n,
            SUM(pressure_interceptions + no_pressure_interceptions) AS int_n,
            SUM(pressure_sacks + no_pressure_sacks)                 AS sacks
    FROM    nfl_data.passing_pressure
    GROUP BY 1, 2, 3
  ")
  .br_memo$pass_wk <- wk %>%
    mutate(week = as.integer(week), season = as.integer(season)) %>%
    inner_join(.br_pff_ids(), by = c("team_name" = "posteam", "week", "season"))
  .br_memo$pass_wk
}

# pooled passing outcomes per team-season: offense = its own passing,
# defense = passing allowed. No xTD here (PFF has none).
.br_outcomes <- function(side = c("offense", "defense"), seasons = 2016:2025) {
  side <- match.arg(side)
  gcol <- if (side == "offense") "team_name" else "opp"
  .br_pff_passing_wk() %>%
    filter(season %in% seasons) %>%
    group_by(team = .data[[gcol]], season) %>%
    summarise(games = n_distinct(week),
              across(c(dropbacks, pressured, att, comp_n, yds, td_n, int_n, sacks),
                     ~ sum(.x, na.rm = TRUE)),
              .groups = "drop") %>%
    filter(att > 0) %>%
    mutate(
      ypa  = yds / att,
      comp = comp_n / att,
      a_ = pmin(pmax((comp - 0.3) * 5,         0), 2.375),
      b_ = pmin(pmax((yds / att - 3) * 0.25,   0), 2.375),
      c_ = pmin(pmax(td_n / att * 20,          0), 2.375),
      d_ = pmin(pmax(2.375 - int_n / att * 25, 0), 2.375),
      rating        = (a_ + b_ + c_ + d_) / 6 * 100,
      sack_rate     = sacks / dropbacks,
      pressure_rate = pressured / dropbacks
    ) %>% select(-a_, -b_, -c_, -d_)
}

#### ---------------- unit panels: room strength percentile per team-season ----------------

# coverage room: player-season grade percentiles within season x position pool
.br_cov_player_seasons <- function(positions, min_snaps = 200, seasons = 2016:2025) {
  # YOUR final_position (CB / SCB / S / LB / MLB) from final_coverage_df_qbgrp.
  # The raw PFF position column has no SCB, so slot and outside would be lumped.
  .br_need("final_coverage_df_qbgrp")
  cov <- get("final_coverage_df_qbgrp", envir = .GlobalEnv)
  cov %>% ungroup() %>%
    filter(season %in% seasons, final_position %in% positions) %>%
    group_by(player, player_id, final_position, team, season) %>%
    summarise(snaps = sum(snap_counts_coverage, na.rm = TRUE),
              grade = weighted.mean(grades_coverage_defense, snap_counts_coverage, na.rm = TRUE),
              games = n_distinct(week),
              .groups = "drop") %>%
    filter(snaps >= min_snaps, !is.na(grade)) %>%
    group_by(final_position, season) %>%            # CB vs CB, SCB vs SCB, S vs S
    mutate(pctl = percent_rank(grade)) %>%
    ungroup()
}

.br_cov_panel <- function(positions, room_n = 2, min_snaps = 200, seasons = 2016:2025) {
  ps <- .br_cov_player_seasons(positions, min_snaps, seasons)
  room <- ps %>%
    group_by(team, season) %>%
    slice_max(snaps, n = room_n, with_ties = FALSE) %>%
    summarise(room_strength = mean(pctl),
              room_players  = paste(player, collapse = " / "),
              .groups = "drop")
  room %>% inner_join(.br_outcomes("defense", seasons), by = c("team", "season"))
}

# OT room: top tackles by pass-block snaps, TPS grade percentile as strength.
# tps season summary is one row per QB-STINT: collapse to player_id x season
# first (games-weighted), or a tackle with two stints fills both room slots.
# positions = NULL means every OL position in the frame (cards use this).
.br_ot_player_seasons <- function(seasons = 2016:2025, positions = NULL) {
  .br_need(c("all_pass_block_summary", "tps_pass_block_player_season_summary"))
  pb  <- get("all_pass_block_summary", envir = .GlobalEnv)
  tps <- get("tps_pass_block_player_season_summary", envir = .GlobalEnv)
  pos_levels <- unique(pb$det_position)
  if (is.null(positions)) positions <- pos_levels
  t_pos <- intersect(positions, pos_levels)
  if (length(t_pos) == 0)
    stop("no rows at those positions. det_position levels are: ",
         paste(pos_levels, collapse = ", "), " — widen the filter yourself.")
  tps_c <- tps %>% ungroup() %>%
    mutate(.w = if ("n" %in% names(tps)) n else 1) %>%
    group_by(player_id, season) %>%
    summarise(grade_season_pctl    = weighted.mean(grade_season_pctl,    .w, na.rm = TRUE),
              pressure_season_pctl = weighted.mean(pressure_season_pctl, .w, na.rm = TRUE),
              hurries_season_pctl  = weighted.mean(hurries_season_pctl,  .w, na.rm = TRUE),
              .groups = "drop")
  pb %>% ungroup() %>%
    filter(season %in% seasons, det_position %in% t_pos) %>%
    group_by(player, player_id, team_name, season) %>%
    summarise(pb_snaps = sum(snap_counts_pass_block, na.rm = TRUE),
              games = n_distinct(week), .groups = "drop") %>%
    inner_join(tps_c, by = c("player_id", "season")) %>%
    filter(!is.na(grade_season_pctl))
}

.br_ot_panel <- function(room_n = 2, seasons = 2016:2025) {
  ps <- .br_ot_player_seasons(seasons, positions = c("LT", "RT", "T"))
  room <- ps %>%
    group_by(team_name, season) %>%
    slice_max(pb_snaps, n = room_n, with_ties = FALSE) %>%
    summarise(room_strength = mean(grade_season_pctl),
              room_pressure = mean(pressure_season_pctl, na.rm = TRUE),
              room_players  = paste(player, collapse = " / "),
              .groups = "drop")
  room %>% inner_join(.br_outcomes("offense", seasons),
                      by = c("team_name" = "team", "season"))
}

# receiver production percentiles from pbp, keyed by receiver_id.
# (pbp names are "J.Waddle"; receiving_func_base names are full. NEVER join
# those on name — receiver_id is the only safe bridge.)
.br_wr_production <- function(min_tgt = 50, seasons = 2016:2025) {
  .br_need("combined_pbp")
  get("combined_pbp", envir = .GlobalEnv) %>% ungroup() %>%
    filter(pass_attempt == 1, !is.na(receiver_id), season %in% seasons) %>%
    group_by(receiver_id, receiver_player_name, posteam, season) %>%
    summarise(tgts = n(),
              ypt   = sum(yards_gained, na.rm = TRUE) / tgts,
              catch = mean(complete_pass, na.rm = TRUE),
              epa_t = mean(epa, na.rm = TRUE),
              .groups = "drop") %>%
    filter(tgts >= min_tgt) %>%
    group_by(season) %>%
    mutate(ypt_pctl  = percent_rank(ypt),
           catch_pctl= percent_rank(catch),
           epa_pctl  = percent_rank(epa_t),
           prod_pctl = (ypt_pctl + catch_pctl + epa_pctl) / 3) %>%
    ungroup()
}

# resolve a full name ("Jaylen Waddle") to receiver_id via receiving_func_base
.br_wr_id <- function(name, seasons = 2016:2025) {
  .br_need("receiving_func_base")
  rfb <- get("receiving_func_base", envir = .GlobalEnv)
  ids <- rfb %>% ungroup() %>% filter(season %in% seasons) %>%
    distinct(receiver_id, player) %>%
    mutate(.i = tolower(player))
  hit <- ids %>% filter(.i == tolower(name))
  if (nrow(hit) == 0) hit <- ids[grepl(tolower(name), ids$.i, fixed = TRUE), ]
  if (nrow(hit) == 0)
    stop("no receiver matching '", name, "' in receiving_func_base.")
  if (length(unique(hit$receiver_id)) > 1)
    stop("'", name, "' matches several receiver_ids: ",
         paste(unique(hit$player), collapse = " / "), " — be more specific.")
  hit$receiver_id[1]
}

.br_wr1_panel <- function(min_games = 6, min_tgt = 50, seasons = 2016:2025) {
  .br_need("receiving_func_base")
  rfb <- get("receiving_func_base", envir = .GlobalEnv)
  wr1 <- rfb %>% ungroup() %>%
    filter(final_position_group == "WR", season %in% seasons) %>%
    group_by(abbreviation, season, receiver_id, player) %>%
    summarise(wr1_games = n(), tgt_share_avg = mean(tgt_share, na.rm = TRUE),
              .groups = "drop") %>%
    filter(wr1_games >= min_games) %>%
    group_by(abbreviation, season) %>%
    slice_max(tgt_share_avg, n = 1, with_ties = FALSE) %>%
    ungroup()
  prod <- .br_wr_production(min_tgt, seasons)
  wr1 %>%
    inner_join(prod, by = c("receiver_id" = "receiver_id",
                            "abbreviation" = "posteam", "season" = "season")) %>%
    inner_join(.br_outcomes("offense", seasons),
               by = c("abbreviation" = "team", "season" = "season"))
}

#### ---------------- the bridge: fit and apply ----------------

.br_panel_for <- function(unit, seasons, ...) {
  switch(unit,
    cb  = .br_cov_panel(positions = c("CB", "SCB"), seasons = seasons, ...),
    cbx = .br_cov_panel(positions = "CB",           seasons = seasons, ...),  # outside only
    s   = .br_cov_panel(positions = "S",            seasons = seasons, ...),
    lb  = .br_cov_panel(positions = c("LB", "MLB"), seasons = seasons, ...),
    ot  = .br_ot_panel(seasons = seasons, ...),
    wr1 = .br_wr1_panel(seasons = seasons, ...),
    stop("unknown unit '", unit, "'. One of: cb, cbx, s, lb, ot, wr1"))
}

## Show the historical map. Buckets are the ground truth; the slope is the bridge.
## x is a 0-1 percentile, so slope PER 10 POINTS = 0.1 * cov/var.
bridge_fit <- function(unit, seasons = 2016:2025, ...) {
  panel <- .br_panel_for(unit, seasons, ...)
  x <- if (unit == "wr1") panel$prod_pctl else panel$room_strength
  outs <- c("ypa", "comp", "rating", "xtds_pg", "sack_rate")

  cat("== ", toupper(unit), " bridge — ", nrow(panel), " team-seasons, ",
      min(panel$season), "-", max(panel$season), " ==\n", sep = "")
  cat("strength percentile (0-1) vs pooled passing",
      if (unit %in% c("cb", "cbx", "s", "lb")) " ALLOWED" else "", ":\n", sep = "")

  bk <- panel %>% mutate(strength = x) %>%
    mutate(bucket = cut(strength, breaks = seq(0, 1, .2), include.lowest = TRUE)) %>%
    group_by(bucket) %>%
    summarise(n = n(), strength_mid = round(mean(strength), 2),
              across(all_of(outs), ~ round(mean(.x, na.rm = TRUE), 3)),
              .groups = "drop")
  print(as.data.frame(bk), row.names = FALSE)

  slopes <- tibble(outcome = outs) %>%
    rowwise() %>%
    mutate(per_10pctl = round(0.1 * cov(x, panel[[outcome]], use = "complete.obs") /
                                var(x, na.rm = TRUE), 4),
           cor = round(cor(x, panel[[outcome]], use = "complete.obs"), 2)) %>%
    ungroup()
  cat("\nslope per +10 percentile points of room strength (and correlation):\n")
  print(as.data.frame(slopes), row.names = FALSE)
  cat("\nREAD IT YOURSELF: cross-sectional team-seasons, not causal. Scheme, QB and\n",
      "the pass rush / the rest of the corps all live inside these slopes.\n", sep = "")
  invisible(list(panel = panel, buckets = bk, slopes = slopes))
}

## Apply the bridge: from/to are percentiles (0-100 or 0-1, auto-detected).
## Defense units: negative delta = defense improves.
bridge_delta <- function(unit, from_pctl, to_pctl, seasons = 2016:2025, ...) {
  fit <- bridge_fit(unit, seasons, ...)
  if (from_pctl > 1 | to_pctl > 1) { from_pctl <- from_pctl / 100; to_pctl <- to_pctl / 100 }
  d <- to_pctl - from_pctl
  ans <- fit$slopes %>% mutate(delta = round(per_10pctl * (d * 10), 4))
  cat("\n== ", toupper(unit), ": room moves ", round(from_pctl * 100), " -> ",
      round(to_pctl * 100), " percentile (delta ", round(d * 100, 1), " points) ==\n",
      "historical worth of that move:\n", sep = "")
  print(as.data.frame(ans %>% select(outcome, delta)), row.names = FALSE)
  invisible(ans)
}

#### ---------------- player lookups ----------------

lab_cov_player <- function(name, seasons = 2016:2025) {
  ps <- .br_cov_player_seasons(positions = c("CB", "SCB", "S", "LB", "MLB"), min_snaps = 0, seasons = seasons)
  i <- .br_find(ps, "player", name)
  out <- ps[i, ] %>% arrange(season) %>%
    select(player, final_position, team, season, games, snaps, grade, pctl)
  cat("-- ", unique(out$player), " — season by season, percentile within season x position pool --\n", sep = "")
  print(as.data.frame(out %>% mutate(pctl = round(pctl * 100, 1), grade = round(grade, 1))),
        row.names = FALSE)
  invisible(out)
}

## v1 name-lookups routed through the v4 cards
lab_ot_player <- function(name, seasons = 2016:2025) ot_card(name, seasons = seasons)
lab_wr_player <- function(name, seasons = 2016:2025) wr_card(name, seasons = seasons)


#### ============================================================================
#### cards, anchors, per-stat bridge, WR1-add study. Rough by design.
#### ============================================================================

# pick the first column that exists; stop listing all columns if none
.br_pick <- function(df, candidates) {
  hit <- intersect(candidates, names(df))[1]
  if (is.na(hit))
    stop("none of [", paste(candidates, collapse = ", "), "] found. Columns are:\n",
         paste(names(df), collapse = ", "))
  hit
}

# per-stat coverage player-seasons + percentiles (pool: 150+ snaps, 15+ targets)
.br_cov_stat_seasons <- function(positions = c("CB", "SCB", "S"), seasons = 2016:2025,
                                 pool_snaps = 150, pool_tgts = 15) {
  .br_need("final_coverage_df_qbgrp")
  cov <- get("final_coverage_df_qbgrp", envir = .GlobalEnv)   # your final_position
  if (!"draft_season" %in% names(cov)) cov$draft_season <- NA_integer_
  tgt_col <- .br_pick(cov, c("targets", "tgts"))
  rec_col <- .br_pick(cov, c("receptions", "catches_allowed", "receptions_allowed"))
  yds_col <- .br_pick(cov, c("yards", "yards_allowed"))
  rtg_col <- .br_pick(cov, c("qb_rating_against", "passer_rating_against",
                             "targeted_qb_rating", "passer_rating"))
  ps <- cov %>% ungroup() %>%
    filter(season %in% seasons, final_position %in% positions) %>%
    group_by(player, player_id, final_position, team, season, draft_season) %>%
    summarise(snaps = sum(snap_counts_coverage, na.rm = TRUE),
              games = n_distinct(week),
              tgts  = sum(.data[[tgt_col]], na.rm = TRUE),
              comp_allowed  = sum(.data[[rec_col]], na.rm = TRUE) / pmax(sum(.data[[tgt_col]], na.rm = TRUE), 1),
              yds_per_snap  = sum(.data[[yds_col]], na.rm = TRUE) / pmax(snaps, 1),
              rtg_against   = weighted.mean(.data[[rtg_col]], pmax(.data[[tgt_col]], 1), na.rm = TRUE),
              grade = weighted.mean(grades_coverage_defense, snap_counts_coverage, na.rm = TRUE),
              .groups = "drop")

  # percentile scales from the qualifying pool: one empirical CDF per season x
  # position. "Allowed" stats are flipped so every p_* reads higher = better.
  pool <- ps %>% filter(snaps >= pool_snaps, tgts >= pool_tgts) %>%
    group_by(season, final_position) %>%
    summarise(grade_v = list(sort(grade[!is.na(grade)])),
              comp_v  = list(sort(comp_allowed[!is.na(comp_allowed)])),
              yps_v   = list(sort(yds_per_snap[!is.na(yds_per_snap)])),
              rtg_v   = list(sort(rtg_against[!is.na(rtg_against)])),
              .groups = "drop")

  ps <- ps %>% left_join(pool, by = c("season", "final_position"))

  ecdf_above <- function(x, v) if (is.na(x) || is.null(v) || length(v) == 0) NA_real_
                               else mean(x >= v)
  ecdf_below <- function(x, v) if (is.na(x) || is.null(v) || length(v) == 0) NA_real_
                               else mean(x <= v)

  ps %>%
    mutate(
      p_grade = mapply(ecdf_above, grade,        grade_v),
      p_comp  = mapply(ecdf_below, comp_allowed, comp_v),
      p_yps   = mapply(ecdf_below, yds_per_snap, yps_v),
      p_rtg   = mapply(ecdf_below, rtg_against,  rtg_v)
    ) %>%
    select(-grade_v, -comp_v, -yps_v, -rtg_v)
}

.br_card <- function(rows, stat_cols, wcol = "n", card_seasons = NULL) {
  if (!is.null(card_seasons)) rows <- rows %>% filter(season %in% card_seasons)
  w <- rows[[wcol]]
  weighted <- sapply(stat_cols, function(s) {
    v <- rows[[s]]; ok <- !is.na(v) & !is.na(w)
    if (!any(ok)) return(NA_real_)
    weighted.mean(v[ok], w[ok])
  })
  unweighted <- sapply(stat_cols, function(s) {
    v <- rows[[s]]; ok <- !is.na(v)
    if (!any(ok)) return(NA_real_)
    mean(v[ok])
  })
  list(weighted = round(weighted * 100, 1), unweighted = round(unweighted * 100, 1))
}

.br_card_print <- function(card, card_seasons, wlabel) {
  cat("\nCARD", if (!is.null(card_seasons)) paste0(" (", paste(range(card_seasons), collapse = "-"), ")") else " (all seasons shown)",
      " — ", wlabel, ":\n", sep = "")
  print(card$weighted)
  cat("unweighted per-season mean (a big healthy year does NOT bury hurt ones):\n")
  print(card$unweighted)
}

#### ---------------- THE COVERAGE CARD — built on your own summaries ----------------
#### Reads coverage_man/zone/slot_player_season_summary: the same percentiles
#### your diet-matrix / heatmap layer uses, within position x season.
#### CB is compared to CBs, S to S, LB to LB — the summaries already do that.

.br_cov_metric_labels <- c("Grade", "Snp/Tgt", "Catch%", "PBU%", "Y/CovSnp", "QBR")
.br_cov_metrics       <- c("grade_cov", "cov_snaps_per_target", "catch_rate",
                           "pass_break_up_rate", "yards_per_cov_snap", "qb_rating_against")

cov_card <- function(name, seasons = 2016:2025, card_seasons = NULL) {
  .br_need(c("coverage_man_player_season_summary", "coverage_zone_player_season_summary"))
  fams <- list(man  = get("coverage_man_player_season_summary",  envir = .GlobalEnv),
               zone = get("coverage_zone_player_season_summary", envir = .GlobalEnv))
  if (exists("coverage_slot_player_season_summary", envir = .GlobalEnv))
    fams$slot <- get("coverage_slot_player_season_summary", envir = .GlobalEnv)

  found <- FALSE
  cards <- list()
  for (fam in names(fams)) {
    df <- fams[[fam]] %>% ungroup() %>% filter(season %in% seasons)
    i <- tryCatch(.br_find(df, "player", name), error = function(e) integer(0))
    if (length(i) == 0) next
    found <- TRUE
    pcols <- paste0(fam, "_", .br_cov_metrics, "_season_pctl")
    have  <- pcols %in% names(df)
    labs  <- .br_cov_metric_labels[have]
    out <- df[i, ] %>% arrange(season) %>%
      select(player, final_position, def_ssn, season, n, all_of(pcols[have]))
    names(out)[match(pcols[have], names(out))] <- labs
    cat("\n-- ", unique(out$player), " — ", toupper(fam),
        " coverage percentiles (within position x season, higher = better) --\n", sep = "")
    print(as.data.frame(out %>% mutate(across(all_of(labs), ~ round(.x * 100, 1)))),
          row.names = FALSE)
    card <- .br_card(out, labs, "n", card_seasons)
    .br_card_print(card, card_seasons, paste0(fam, ": games-weighted avg percentile"))
    cards[[fam]] <- list(seasons = out, card = card)
  }
  if (!found)
    stop("no coverage rows for '", name,
         "' in the man/zone/slot season summaries. Check the name, or source ",
         "pff_pass_coverage_AWS.R first.")
  invisible(cards)
}

#### ---------------- THE OL CARD — tackles, guards, centers ----------------

ot_card <- function(name, card_seasons = NULL, seasons = 2016:2025) {
  ps <- .br_ot_player_seasons(seasons)     # positions = NULL: the whole line
  i <- tryCatch(.br_find(ps, "player", name), error = function(e) integer(0))

  if (length(i) == 0) {
    # season summary dropped him (e.g. split across positions). RAW fallback
    # from the game-level frame — percentiles impossible, raw rates only.
    pb <- get("all_pass_block_summary", envir = .GlobalEnv)
    j <- .br_find(pb, "player", name)   # let this one stop with suggestions
    out <- pb[j, ] %>% ungroup() %>%
      filter(season %in% seasons) %>%
      group_by(player, det_position, team_name, season) %>%
      summarise(games = n_distinct(week),
                pb_snaps = sum(snap_counts_pass_block, na.rm = TRUE),
                raw_grade = weighted.mean(grades_pass_block, snap_counts_pass_block, na.rm = TRUE),
                raw_pressure_rate = sum(pressures_allowed, na.rm = TRUE) / pmax(pb_snaps, 1),
                raw_hurry_rate = sum(hurries_allowed, na.rm = TRUE) / pmax(pb_snaps, 1),
                .groups = "drop") %>%
      arrange(season)
    cat("-- ", unique(out$player), " — NOT in the season summary; RAW rates only,\n",
        "   no percentiles (compare raw_pressure_rate to the room, roughly 0.05-0.10) --\n", sep = "")
    print(as.data.frame(out %>%
          mutate(raw_grade = round(raw_grade, 1),
                 raw_pressure_rate = round(raw_pressure_rate, 4),
                 raw_hurry_rate = round(raw_hurry_rate, 4))), row.names = FALSE)
    return(invisible(list(seasons = out, card = NULL)))
  }

  out <- ps[i, ] %>% arrange(season) %>%
    select(player, team_name, season, games, pb_snaps,
           grade_season_pctl, pressure_season_pctl, hurries_season_pctl)
  cat("-- ", unique(out$player), " — TPS percentiles (within position x season).\n",
      "   ALL THREE higher = better: pressure/hurries are ranked on the NEGATED\n",
      "   rate upstream (pff_pass_block_AWS.R:131-155), so high = FEWER allowed --\n", sep = "")
  print(as.data.frame(out %>% mutate(across(ends_with("pctl"), ~ round(.x * 100, 1)))),
        row.names = FALSE)
  # card: all three already direction-correct upstream — never flip them here
  inv <- out %>% mutate(p_grade = grade_season_pctl,
                        p_press = pressure_season_pctl,
                        p_hurr  = hurries_season_pctl)
  card <- .br_card(inv, c("p_grade", "p_press", "p_hurr"), "pb_snaps", card_seasons)
  .br_card_print(card, card_seasons, "pb-snaps-weighted; all three higher = better protector")
  invisible(list(seasons = out, card = card))
}

#### ---------------- THE WR CARD ----------------

wr_card <- function(name, card_seasons = NULL, seasons = 2016:2025, min_tgt_pool = 50) {
  rid <- .br_wr_id(name, seasons)                     # full name -> receiver_id
  pool <- .br_wr_production(min_tgt = min_tgt_pool, seasons = seasons)
  out <- pool %>% filter(receiver_id == rid) %>% arrange(season) %>%
    select(receiver_player_name, posteam, season, tgts, ypt, catch, epa_t,
           ypt_pctl, catch_pctl, epa_pctl, prod_pctl)
  if (nrow(out) == 0)
    stop("no ", min_tgt_pool, "+-target seasons for '", name,
         "' in this window — lower min_tgt_pool.")
  cat("-- ", name, " (pbp name ", unique(out$receiver_player_name),
      ") — RAW production percentiles vs ", min_tgt_pool, "+-target receivers.\n",
      "   Team context is INSIDE these numbers; pick the season(s) you believe --\n", sep = "")
  print(as.data.frame(out %>%
        mutate(ypt = round(ypt, 2), catch = round(catch, 3), epa_t = round(epa_t, 3),
               across(ends_with("pctl"), ~ round(.x * 100, 1)))), row.names = FALSE)
  card <- .br_card(out, c("ypt_pctl", "catch_pctl", "epa_pctl", "prod_pctl"), "tgts", card_seasons)
  .br_card_print(card, card_seasons, "targets-weighted avg percentile")
  invisible(list(seasons = out, card = card))
}

#### ---------------- empty-year anchors, from your data ----------------

rookie_anchor <- function(unit = "cb", seasons = 2016:2025) {
  if (unit %in% c("cb", "cbx", "s", "lb")) {
    pos <- switch(unit, cb = c("CB", "SCB"), cbx = "CB", s = "S", lb = c("LB", "MLB"))
    ps <- .br_cov_stat_seasons(positions = pos, seasons = seasons, pool_snaps = 1, pool_tgts = 1)
    rk <- ps %>% filter(!is.na(draft_season), season == draft_season, snaps >= 150)
    cat("-- first-year ", paste(pos, collapse = "/"), " seasons (", nrow(rk),
        " players, 150+ snaps) — average percentile --\n", sep = "")
    print(round(colMeans(rk[, c("p_grade", "p_comp", "p_yps", "p_rtg")], na.rm = TRUE) * 100, 1))
    invisible(rk)
  } else if (unit == "ot") {
    ps <- .br_ot_player_seasons(seasons)
    rk <- ps %>% group_by(player_id) %>% mutate(first_season = min(season)) %>% ungroup() %>%
      filter(season == first_season)
    cat("-- first-year OL (", nrow(rk), " player-seasons) — average percentile --\n", sep = "")
    print(round(c(grade = mean(rk$grade_season_pctl, na.rm = TRUE),
                  pressure = mean(rk$pressure_season_pctl, na.rm = TRUE)) * 100, 1))
    invisible(rk)
  }
}

backup_anchor <- function(unit = "cb", seasons = 2016:2025, snap_band = c(100, 350)) {
  if (unit %in% c("cb", "cbx", "s", "lb")) {
    pos <- switch(unit, cb = c("CB", "SCB"), cbx = "CB", s = "S", lb = c("LB", "MLB"))
    ps <- .br_cov_stat_seasons(positions = pos, seasons = seasons, pool_snaps = 1, pool_tgts = 1)
    bk <- ps %>% filter(snaps >= snap_band[1], snaps <= snap_band[2],
                        is.na(draft_season) | season != draft_season)
    cat("-- part-time ", paste(pos, collapse = "/"), " (", nrow(bk), " seasons, ",
        snap_band[1], "-", snap_band[2], " snaps, not rookies) — average percentile --\n", sep = "")
    print(round(colMeans(bk[, c("p_grade", "p_comp", "p_yps", "p_rtg")], na.rm = TRUE) * 100, 1))
    invisible(bk)
  } else if (unit == "ot") {
    ps <- .br_ot_player_seasons(seasons)
    bk <- ps %>% group_by(player_id) %>% mutate(first_season = min(season)) %>% ungroup() %>%
      filter(season != first_season, pb_snaps >= 100, pb_snaps <= 350)
    cat("-- part-time OL (", nrow(bk), " seasons, 100-350 pb snaps, not rookies) --\n", sep = "")
    print(round(c(grade = mean(bk$grade_season_pctl, na.rm = TRUE),
                  pressure = mean(bk$pressure_season_pctl, na.rm = TRUE)) * 100, 1))
    invisible(bk)
  }
}

#### ---------------- per-stat coverage bridge (man / zone / combined) ----------------
#### Player percentiles come straight from your coverage season summaries —
#### catch% pctl -> team comp% allowed, Y/CovSnp pctl -> ypa allowed,
#### QBR-against pctl -> rating allowed. Room membership by season coverage
#### snaps from final_coverage_df_qbgrp. x is 0-1: slope PER 10 PTS = 0.1*cov/var.

.br_cov_fam_stats <- function(family, pos, seasons) {
  stat_map <- c(comp = "catch_rate", ypa = "yards_per_cov_snap", rating = "qb_rating_against")
  get_fam <- function(df, fam) {
    df %>% ungroup() %>%
      filter(season %in% seasons, final_position %in% pos) %>%
      select(player_id, player, final_position, def_ssn, season, n,
             comp   = !!sym(paste0(fam, "_", stat_map["comp"],   "_season_pctl")),
             ypa    = !!sym(paste0(fam, "_", stat_map["ypa"],    "_season_pctl")),
             rating = !!sym(paste0(fam, "_", stat_map["rating"], "_season_pctl")))
  }
  m <- get_fam(get("coverage_man_player_season_summary",  envir = .GlobalEnv), "man")
  z <- get_fam(get("coverage_zone_player_season_summary", envir = .GlobalEnv), "zone")
  if (family == "man")  return(m)
  if (family == "zone") return(z)
  full_join(m, z, by = c("player_id", "player", "final_position", "def_ssn", "season"),
            suffix = c("_man", "_zone")) %>%
    mutate(comp   = rowMeans(cbind(comp_man,   comp_zone),   na.rm = TRUE),
           ypa    = rowMeans(cbind(ypa_man,    ypa_zone),    na.rm = TRUE),
           rating = rowMeans(cbind(rating_man, rating_zone), na.rm = TRUE),
           n      = pmax(n_man, n_zone, na.rm = TRUE)) %>%
    mutate(across(c(comp, ypa, rating), ~ ifelse(is.finite(.x), .x, NA_real_)))
}

bridge_fit_stats <- function(unit = "cb", family = c("combined", "man", "zone"),
                             room_n = 2, seasons = 2016:2025) {
  family <- match.arg(family)
  pos <- switch(unit, cb = c("CB", "SCB"), cbx = "CB", s = "S", lb = c("LB", "MLB"),
                stop("bridge_fit_stats covers cb / cbx / s / lb."))
  .br_need(c("coverage_man_player_season_summary", "coverage_zone_player_season_summary",
             "final_coverage_df_qbgrp"))

  ps <- .br_cov_fam_stats(family, pos, seasons)

  fcd <- get("final_coverage_df_qbgrp", envir = .GlobalEnv)
  team_col <- .br_pick(fcd, c("team", "team_name"))
  snaps <- fcd %>% ungroup() %>% filter(season %in% seasons) %>%
    group_by(player_id, season) %>%
    summarise(cov_snaps = sum(snap_counts_coverage, na.rm = TRUE),
              team = dplyr::last(.data[[team_col]]), .groups = "drop")

  room <- ps %>%
    inner_join(snaps, by = c("player_id", "season")) %>%
    mutate(team_code = sub("[0-9]{4}$", "", def_ssn)) %>%
    filter(!is.na(comp) | !is.na(ypa) | !is.na(rating)) %>%
    group_by(team_code, season) %>%
    slice_max(cov_snaps, n = room_n, with_ties = FALSE) %>%
    summarise(s_comp = mean(comp,   na.rm = TRUE),
              s_ypa  = mean(ypa,    na.rm = TRUE),
              s_rtg  = mean(rating, na.rm = TRUE),
              room_players = paste(player, collapse = " / "), .groups = "drop") %>%
    inner_join(.br_outcomes("defense", seasons), by = c("team_code" = "team", "season"))

  pairs <- list(comp = c("s_comp", "comp"), ypa = c("s_ypa", "ypa"), rating = c("s_rtg", "rating"))
  res <- lapply(names(pairs), function(nm) {
    x <- room[[pairs[[nm]][1]]]; y <- room[[pairs[[nm]][2]]]
    slope10 <- 0.1 * cov(x, y, use = "complete.obs") / var(x, na.rm = TRUE)
    tibble(stat = nm, n = sum(complete.cases(x, y)),
           per_10pctl = round(slope10, 4),
           cor = round(cor(x, y, use = "complete.obs"), 2))
  }) %>% bind_rows()
  cat("== per-stat ", toupper(unit), " bridge (", family, ") — ", nrow(room),
      " team-seasons ==\n",
      "room percentile (top-", room_n, " by coverage snaps) vs team passing ALLOWED:\n", sep = "")
  print(as.data.frame(res), row.names = FALSE)
  cat("(negative slope = better room, less production allowed. READ the correlations.)\n")
  invisible(list(room = room, slopes = res))
}

## moves: named list of from->to percentile pairs per stat (0-100 or 0-1);
## stat names: comp, ypa, rating
bridge_stats_delta <- function(unit = "cb", moves = list(), room_n = 2,
                               family = "combined", seasons = 2016:2025) {
  fit <- bridge_fit_stats(unit, family, room_n, seasons)
  out <- lapply(names(moves), function(nm) {
    s <- fit$slopes %>% filter(stat == nm)
    if (nrow(s) == 0) stop("no stat '", nm, "' in bridge. Have: ",
                           paste(fit$slopes$stat, collapse = ", "))
    mv <- moves[[nm]]
    if (any(mv > 1)) mv <- mv / 100
    d <- mv[2] - mv[1]
    tibble(stat = nm, from = mv[1] * 100, to = mv[2] * 100,
           delta = round(s$per_10pctl * (d * 10), 4))
  }) %>% bind_rows()
  cat("\n== ", toupper(unit), " per-stat deltas (", family, ") ==\n", sep = "")
  print(as.data.frame(out), row.names = FALSE)
  invisible(out)
}

#### ---------------- the Waddle question: adding a proven #1 ----------------

## Team-seasons where the WR1 changed AND the new WR1's PRIOR-season production
## percentile (any team) was >= min_prior_pctl. What did the team's pooled
## passing do? Cases printed by name; averages at the bottom.
## All receiver joins on receiver_id — pbp names are abbreviated.
bridge_wr1_add <- function(seasons = 2016:2025, min_games = 6, min_tgt = 50,
                           min_prior_pctl = 0.60) {
  for (f in c("receiving_func_base", "combined_pbp"))
    if (!exists(f, envir = .GlobalEnv))
      stop("missing frame: ", f, " — source the receiving build / load the workspace first.")
  rfb <- get("receiving_func_base", envir = .GlobalEnv)

  wr1 <- rfb %>% ungroup() %>%
    filter(final_position_group == "WR", season %in% seasons) %>%
    group_by(abbreviation, season, receiver_id, player) %>%
    summarise(wr1_games = n(), tgt_share_avg = mean(tgt_share, na.rm = TRUE), .groups = "drop") %>%
    filter(wr1_games >= min_games) %>%
    group_by(abbreviation, season) %>%
    slice_max(tgt_share_avg, n = 1, with_ties = FALSE) %>% ungroup()

  prod <- .br_wr_production(min_tgt, seasons) %>%
    select(receiver_id, posteam, season, prod_pctl, ypt, epa_t, tgts)

  outc <- .br_outcomes("offense", seasons)

  cases <- wr1 %>%
    inner_join(prod, by = c("receiver_id", "abbreviation" = "posteam", "season")) %>%
    inner_join(outc, by = c("abbreviation" = "team", "season" = "season")) %>%
    mutate(td_pg = td_n / games) %>%          # PFF outcomes carry real TDs, not xTD
    group_by(abbreviation) %>% arrange(season, .by_group = TRUE) %>%
    mutate(prev_season = lag(season), prev_wr1 = lag(player),
           prev_ypa = lag(ypa), prev_comp = lag(comp),
           prev_rating = lag(rating), prev_td_pg = lag(td_pg)) %>%
    ungroup() %>%
    filter(season == prev_season + 1, player != prev_wr1) %>%
    # prior-season proof comes AFTER the YoY lags, or the lag base loses rows
    inner_join(prod %>% transmute(receiver_id, next_season = season + 1,
                                  prior_pctl = prod_pctl, prior_tgts = tgts),
               by = c("receiver_id", "season" = "next_season")) %>%
    filter(!is.na(prior_pctl), prior_pctl >= min_prior_pctl) %>%
    mutate(d_ypa = ypa - prev_ypa, d_comp = comp - prev_comp,
           d_rating = rating - prev_rating, d_td_pg = td_pg - prev_td_pg) %>%
    select(abbreviation, season, prev_wr1, new_wr1 = player, prior_pctl,
           tgt_share_avg, ypa, prev_ypa, d_ypa, d_comp, d_rating, d_td_pg) %>%
    arrange(desc(prior_pctl))

  cat("-- ", nrow(cases), " cases: new WR1 whose PRIOR season was >= ",
      round(min_prior_pctl * 100), "th production percentile --\n", sep = "")
  if (nrow(cases) == 0) {
    cat("none in this window — widen seasons or lower min_prior_pctl.\n")
    return(invisible(cases))
  }
  print(as.data.frame(cases %>%
        mutate(prior_pctl = round(prior_pctl * 100, 1), tgt_share_avg = round(tgt_share_avg, 3),
               ypa = round(ypa, 2), prev_ypa = round(prev_ypa, 2),
               across(starts_with("d_"), ~ round(.x, 3)))), row.names = FALSE)
  cat("\naverages across cases (",
      "d_ypa / d_comp / d_rating / d_td_pg — TDs per game are REAL, not xTD):\n", sep = "")
  print(round(c(mean(cases$d_ypa, na.rm = TRUE), mean(cases$d_comp, na.rm = TRUE),
                mean(cases$d_rating, na.rm = TRUE), mean(cases$d_td_pg, na.rm = TRUE)), 3))
  cat("\nREAD IT YOURSELF: these teams also changed QBs, schemes, and luck.\n",
      "Cases with the same QB both years are the ones to lean on.\n", sep = "")
  invisible(cases)
}


#### ============================================================================
#### V5 — every unit, your frames. Later definitions supersede earlier ones.
#### ============================================================================

## .br_need — DELETED the cache-receipt loader (direct instruction 2026-09-14):
## frames come only from what is already in the session. Missing -> stop
## with the frame name. Never load cache files.
.br_need <- function(frames) {
  missing <- frames[!vapply(frames, exists, logical(1), envir = .GlobalEnv)]
  if (length(missing))
    stop("missing frame(s): ", paste(missing, collapse = ", "),
         " — source the file named in this file's header for that unit first.")
  invisible(NULL)
}

#### ---------------- PASS RUSH card ----------------
#### pass_rush_all/tps_player_season_summary: 6 percentiles within
#### position x season (pff_pass_rush_AWS.R:240-339). All rates higher = better.

pr_card <- function(name, view = c("tps", "all"), seasons = 2016:2025, card_seasons = NULL) {
  view <- match.arg(view)
  fn <- if (view == "tps") "pass_rush_tps_player_season_summary" else "pass_rush_all_player_season_summary"
  .br_need(fn)
  df0 <- get(fn, envir = .GlobalEnv) %>% ungroup() %>% filter(season %in% seasons)
  metrics <- c("grade_pass_rush", "prp", "pass_rush_win_rate",
               "pressure_rate", "hit_rate", "hurry_rate")
  labels  <- c("Grade", "PRP", "Win%", "Press%", "Hit%", "Hurry%")
  pref <- if (view == "tps") "tps_" else ""
  pcols <- paste0(pref, metrics, "_season_pctl")
  have  <- pcols %in% names(df0)
  if (!any(have)) stop("no percentile columns found in ", fn, ". Have: ",
                       paste(names(df0), collapse = ", "))
  labs <- labels[have]
  i <- .br_find(df0, "player", name)
  out <- df0[i, ] %>% arrange(season) %>%
    select(player, position, def_ssn, season, n, all_of(pcols[have]))
  names(out)[match(pcols[have], names(out))] <- labs
  cat("-- ", unique(out$player), " — PASS RUSH (", view,
      ") percentiles, within position x season, higher = better --\n", sep = "")
  print(as.data.frame(out %>% mutate(across(all_of(labs), ~ round(.x * 100, 1)))),
        row.names = FALSE)
  card <- .br_card(out, labs, "n", card_seasons)
  .br_card_print(card, card_seasons, paste0(view, ": games-weighted avg percentile"))
  invisible(list(seasons = out, card = card))
}

#### ---------------- RUN DEFENSE card ----------------
#### run_defense_player_season_summary (pff_run_defense_AWS.R:195-219).
#### DIRECTION FIX: the source frame does NOT negate missed_tackle_rate or
#### avg_depth_of_tackle (its :216-217). This card flips both so every
#### printed percentile reads higher = better, and labels them.

rd_card <- function(name, seasons = 2016:2025, card_seasons = NULL) {
  .br_need("run_defense_player_season_summary")
  df0 <- get("run_defense_player_season_summary", envir = .GlobalEnv) %>%
    ungroup() %>% filter(season %in% seasons)
  metrics <- c("grade_run_def", "grade_tackle", "stop_pct", "tackle_pct",
               "assists_pct", "missed_tackle_rate", "avg_depth_of_tackle")
  labels  <- c("RunDef", "TklGrade", "Stop%", "Tkl%", "Ast%", "MT%", "TklDep")
  flip    <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)   # source NOT negated
  pcols <- paste0(metrics, "_season_pctl")
  have  <- pcols %in% names(df0)
  if (!any(have)) stop("no percentile columns found. Have: ",
                       paste(names(df0), collapse = ", "))
  labs <- labels[have]; fl <- flip[have]
  i <- .br_find(df0, "player", name)
  out <- df0[i, ] %>% arrange(season) %>%
    select(player, position, def_ssn, season, n, all_of(pcols[have]))
  for (k in seq_along(pcols[have]))
    if (fl[k]) out[[pcols[have][k]]] <- 1 - out[[pcols[have][k]]]
  names(out)[match(pcols[have], names(out))] <- labs
  cat("-- ", unique(out$player), " — RUN DEFENSE percentiles, within position x season --\n",
      "   MT% and TklDep FLIPPED from source (source leaves them un-negated):\n",
      "   everywhere here, higher = better.\n", sep = "")
  print(as.data.frame(out %>% mutate(across(all_of(labs), ~ round(.x * 100, 1)))),
        row.names = FALSE)
  card <- .br_card(out, labs, "n", card_seasons)
  .br_card_print(card, card_seasons, "games-weighted avg percentile (after direction fix)")
  invisible(list(seasons = out, card = card))
}

#### ---------------- RUSHING card ----------------
#### rush_season_pctl_sos (league_opp_rushing_schedule.R:102-123): the machine's
#### HB currency — grade / yards-per-attempt / yards-after-contact / MTF / breakaway
#### percentiles within season, with qualifying gates already applied.

rb_card <- function(name, seasons = 2016:2025, card_seasons = NULL) {
  .br_need("rush_season_pctl_sos")
  df0 <- get("rush_season_pctl_sos", envir = .GlobalEnv) %>%
    ungroup() %>% filter(season %in% seasons)
  pcols <- c("grun_pctl", "ypa_pctl", "yco_pctl", "mtf_pctl", "brk_pctl")
  labels <- c("RunGrade", "Y/A", "YCO", "MTF", "Brk%")
  have  <- pcols %in% names(df0)
  if (!any(have)) stop("no percentile columns found. Have: ",
                       paste(names(df0), collapse = ", "))
  labs <- labels[have]
  i <- .br_find(df0, "player", name)
  keep_raw <- intersect(c("grun", "ypa", "yco", "mtf_rt", "brk_rt", "q_games", "q_atts"), names(df0))
  out <- df0[i, ] %>% arrange(season) %>%
    select(player, season, all_of(keep_raw), all_of(pcols[have]))
  names(out)[match(pcols[have], names(out))] <- labs
  cat("-- ", unique(out$player), " — RUSHING percentiles (machine currency, within season) --\n", sep = "")
  print(as.data.frame(out %>% mutate(across(all_of(labs), ~ round(.x * 100, 1)))),
        row.names = FALSE)
  wcol <- if ("q_atts" %in% names(out)) "q_atts" else "q_games"
  card <- .br_card(out, labs, wcol, card_seasons)
  .br_card_print(card, card_seasons, paste0(wcol, "-weighted avg percentile"))
  invisible(list(seasons = out, card = card))
}

#### ---------------- WR card upgrade: machine percentiles first ----------------
#### If rec_season_pctl_sos is in session (new_england_opp_receiving_schedule.R:
#### 191-208), print the machine block: grade + man/zone grade / yprr / tgt
#### percentiles (band x rolling-window currency). Raw-pbp block follows, labeled.

wr_card <- function(name, card_seasons = NULL, seasons = 2016:2025, min_tgt_pool = 50) {
  if (exists("rec_season_pctl_sos", envir = .GlobalEnv)) {
    ms <- get("rec_season_pctl_sos", envir = .GlobalEnv) %>%
      ungroup() %>% filter(season %in% seasons)
    i <- tryCatch(.br_find(ms, "player", name), error = function(e) integer(0))
    if (length(i) > 0) {
      pcols <- c("grade_pctl", "man_grade_pctl", "man_yprr_pctl", "man_tgt_pctl",
                 "zone_grade_pctl", "zone_yprr_pctl", "zone_tgt_pctl")
      labels <- c("Grade", "ManGrade", "ManYPRR", "ManTgt", "ZoneGrade", "ZoneYPRR", "ZoneTgt")
      have <- pcols %in% names(ms)
      labs <- labels[have]
      keep_raw <- intersect(c("band", "qual_g", "routes", "targets"), names(ms))
      out <- ms[i, ] %>% arrange(season) %>%
        select(player, season, all_of(keep_raw), all_of(pcols[have]))
      names(out)[match(pcols[have], names(out))] <- labs
      cat("-- ", unique(out$player), " — MACHINE receiving percentiles",
          " (band x rolling-window currency) --\n", sep = "")
      print(as.data.frame(out %>% mutate(across(all_of(labs), ~ round(.x * 100, 1)))),
            row.names = FALSE)
      wcol <- if ("routes" %in% names(out)) "routes" else "qual_g"
      card <- .br_card(out, labs, wcol, card_seasons)
      .br_card_print(card, card_seasons, paste0(wcol, "-weighted avg percentile"))
    } else {
      cat("-- ", name, " not in rec_season_pctl_sos (gates or window) — raw block only --\n", sep = "")
    }
  } else {
    cat("(rec_season_pctl_sos not in session — machine block skipped.",
        " source pff_stats/receiving/new_england_opp_receiving_schedule.R for it)\n")
  }

  # raw production block (unchanged mechanics)
  rid <- .br_wr_id(name, seasons)
  pool <- .br_wr_production(min_tgt = min_tgt_pool, seasons = seasons)
  out <- pool %>% filter(receiver_id == rid) %>% arrange(season) %>%
    select(receiver_player_name, posteam, season, tgts, ypt, catch, epa_t,
           ypt_pctl, catch_pctl, epa_pctl, prod_pctl)
  if (nrow(out) == 0)
    stop("no ", min_tgt_pool, "+-target seasons for '", name, "' in this window — lower min_tgt_pool.")
  cat("\n-- RAW production percentiles (team context INSIDE these; read with care) --\n", sep = "")
  print(as.data.frame(out %>%
        mutate(ypt = round(ypt, 2), catch = round(catch, 3), epa_t = round(epa_t, 3),
               across(ends_with("pctl"), ~ round(.x * 100, 1)))), row.names = FALSE)
  card2 <- .br_card(out, c("ypt_pctl", "catch_pctl", "epa_pctl", "prod_pctl"), "tgts", card_seasons)
  .br_card_print(card2, card_seasons, "targets-weighted avg percentile")
  invisible(list(raw = list(seasons = out, card = card2)))
}

#### ---------------- new bridge units: prush, rundef ----------------

# rushing-allowed outcomes per defense team-season
.br_outcomes_rush <- function(seasons = 2016:2025) {
  # rushing ALLOWED per defense team-season from PFF nfl_data.rushing_summary
  # (every ball carrier, scrambles included), opponent from your id table.
  if (is.null(.br_memo$rush_wk)) {
    .br_need("run_athena_query")
    wk <- get("run_athena_query", envir = .GlobalEnv)("
      SELECT  team_name, CAST(week AS INTEGER) AS week, CAST(season AS INTEGER) AS season,
              SUM(attempts) AS rushes, SUM(yards) AS ryds, SUM(touchdowns) AS rtd
      FROM    nfl_data.rushing_summary
      GROUP BY 1, 2, 3
    ")
    .br_memo$rush_wk <- wk %>%
      mutate(week = as.integer(week), season = as.integer(season)) %>%
      inner_join(.br_pff_ids(), by = c("team_name" = "posteam", "week", "season"))
  }
  .br_memo$rush_wk %>%
    filter(season %in% seasons) %>%
    group_by(team = opp, season) %>%
    summarise(games = n_distinct(week), rushes = sum(rushes, na.rm = TRUE),
              ryds = sum(ryds, na.rm = TRUE), rtd = sum(rtd, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(ypc = ryds / rushes, rush_td_pg = rtd / games)
}

.br_prush_panel <- function(view = "tps", room_n = 2, positions = c("ED", "DI"),
                            seasons = 2016:2025) {
  fn <- if (view == "tps") "pass_rush_tps_player_season_summary" else "pass_rush_all_player_season_summary"
  .br_need(fn)
  smry <- get(fn, envir = .GlobalEnv) %>% ungroup() %>%
    filter(season %in% seasons, position %in% positions)
  pcol <- paste0(if (view == "tps") "tps_" else "", "grade_pass_rush_season_pctl")
  if (!pcol %in% names(smry)) stop("no ", pcol, " in ", fn)
  room <- smry %>%
    group_by(team_code = sub("[0-9]{4}$", "", def_ssn), season) %>%
    slice_max(n, n = room_n, with_ties = FALSE) %>%
    summarise(room_strength = mean(.data[[pcol]], na.rm = TRUE),
              room_players = paste(player, collapse = " / "), .groups = "drop")
  room %>% inner_join(.br_outcomes("defense", seasons), by = c("team_code" = "team", "season"))
}

.br_rundef_panel <- function(room_n = 2, positions = c("DI", "LB", "ED"),
                             seasons = 2016:2025) {
  .br_need("run_defense_player_season_summary")
  smry <- get("run_defense_player_season_summary", envir = .GlobalEnv) %>% ungroup() %>%
    filter(season %in% seasons, position %in% positions)
  pcol <- "grade_run_def_season_pctl"
  if (!pcol %in% names(smry)) stop("no ", pcol, " in run_defense_player_season_summary")
  room <- smry %>%
    group_by(team_code = sub("[0-9]{4}$", "", def_ssn), season) %>%
    slice_max(n, n = room_n, with_ties = FALSE) %>%
    summarise(room_strength = mean(.data[[pcol]], na.rm = TRUE),
              room_players = paste(player, collapse = " / "), .groups = "drop")
  room %>% inner_join(.br_outcomes_rush(seasons), by = c("team_code" = "team", "season"))
}

## bridge_fit, extended: prush -> passing/sack outcomes allowed;
## rundef -> rushing outcomes allowed. Same 0.1*cov/var law.
bridge_fit <- function(unit, seasons = 2016:2025, ...) {
  if (unit == "prush") {
    panel <- .br_prush_panel(seasons = seasons, ...)
    outs <- c("ypa", "comp", "rating", "sack_rate", "pressure_rate")
  } else if (unit == "rundef") {
    panel <- .br_rundef_panel(seasons = seasons, ...)
    outs <- c("ypc", "rush_td_pg")
  } else {
    panel <- .br_panel_for(unit, seasons, ...)
    outs <- c("ypa", "comp", "rating", "sack_rate", "pressure_rate")
  }
  x <- if (unit == "wr1") panel$prod_pctl else panel$room_strength

  cat("== ", toupper(unit), " bridge — ", nrow(panel), " team-seasons, ",
      min(panel$season), "-", max(panel$season), " ==\n", sep = "")
  cat("strength percentile (0-1) vs pooled",
      if (unit == "rundef") " RUSHING ALLOWED" else if (unit %in% c("cb", "cbx", "s", "lb", "prush")) " passing ALLOWED" else "", ":\n", sep = "")

  bk <- panel %>% mutate(strength = x) %>%
    mutate(bucket = cut(strength, breaks = seq(0, 1, .2), include.lowest = TRUE)) %>%
    group_by(bucket) %>%
    summarise(n = n(), strength_mid = round(mean(strength), 2),
              across(all_of(outs), ~ round(mean(.x, na.rm = TRUE), 3)),
              .groups = "drop")
  print(as.data.frame(bk), row.names = FALSE)

  slopes <- tibble(outcome = outs) %>%
    rowwise() %>%
    mutate(per_10pctl = round(0.1 * cov(x, panel[[outcome]], use = "complete.obs") /
                                var(x, na.rm = TRUE), 4),
           cor = round(cor(x, panel[[outcome]], use = "complete.obs"), 2)) %>%
    ungroup()
  cat("\nslope per +10 percentile points of room strength (and correlation):\n")
  print(as.data.frame(slopes), row.names = FALSE)
  cat("\nREAD IT YOURSELF: cross-sectional team-seasons, not causal. Scheme, QB and\n",
      "the rest of the unit all live inside these slopes.\n", sep = "")
  invisible(list(panel = panel, buckets = bk, slopes = slopes))
}


#### ============================================================================
#### V6 — 2026-09-14. Later definitions supersede earlier ones.
####   - .br_need: cache loader DELETED above (direct instruction). Session
####     frames only; missing -> stop with the name.
####   - coverage position mapping fixed: raw coverage_summary labels safeties
####     "S" (only FS/SS were mapped before, so safeties became "OTHER" and
####     bridge_fit("s") found nobody). SCB now maps correctly too.
####   - (Claude, later on 2026-09-14) bridge_fit UN-PARKED: outcomes now come
####     from PFF team totals (passing_pressure / rushing_summary) + your id table.
####     Coverage rooms read YOUR final_position (final_coverage_df_qbgrp), ranked
####     within position x season. "wr1" pieces still read combined_pbp: don't use.
####   - cov_card / ot_card: card defaults to the player's TWO MOST RECENT
####     seasons (the machine's own blend window). Seasons below the gate are
####     shown as visible rows from the raw frame (games, snaps, raw rates),
####     never silently dropped. Cards never blend positions: a player with
####     rows at two positions gets a separate card per position.
####   - ot_card: positions no longer merged — the QB-stint collapse now keeps
####     det_position, and the season rows show the slot.
####   - wr_card: the raw block is replaced by ROLE-AWARE over-expected metrics
####     (cp_oe / ypa_oe / yac_oe / tgt_per_route / tgt_share), percentiled
####     within season x tgt_cluster — receivers compared within role and
####     target cluster, the way compare_receiver_cohort does it. Modal
####     clusters printed per season. Miami's raw totals no longer in the card.
#### ============================================================================

## position mapping fix: raw coverage frames label safeties "S"
.br_ensure_final_position <- function(cov) {
  if ("final_position" %in% names(cov)) return(cov)
  pos_col <- if ("adv_position" %in% names(cov)) "adv_position" else "position"
  if (!pos_col %in% names(cov))
    stop("coverage frame has no position column — source pff_pass_coverage_AWS.R fully.")
  p <- cov[[pos_col]]
  cov$final_position <- dplyr::case_when(
    p == "MLB"               ~ "MLB",
    grepl("LB$", p)          ~ "LB",
    p %in% c("S", "FS", "SS") ~ "S",
    p == "SCB"               ~ "SCB",
    grepl("CB", p)           ~ "CB",
    p %in% c("DE","ED","DI","DT","NT","DLE","DRE","DLT","DRT") ~ "DL",
    TRUE ~ "OTHER")
  cov
}

## bridge_fit UN-PARKED (Claude, 2026-09-14): its outcomes now come from PFF
## team totals + your id table (see .br_outcomes / .br_outcomes_rush), not
## combined_pbp. Exception: unit "wr1" still reads combined_pbp for receiver
## production -- do not use bridge_fit("wr1") or bridge_wr1_add().

## two most recent seasons present in a player's rows (the machine's window)
.br_card_window <- function(rows, card_seasons) {
  if (!is.null(card_seasons)) return(card_seasons)
  sort(utils::tail(unique(rows$season), 2))
}

#### ---------------- COVERAGE card v6 ----------------
#### Same man/zone/slot season summaries as before, plus:
####  - card window = player's two most recent seasons (machine's blend window)
####  - below-gate seasons appended from raw coverage_summary, marked, with
####    games / snaps / raw rates — nothing silently missing
####  - never blends positions: one card per final_position
cov_card <- function(name, seasons = 2016:2025, card_seasons = NULL) {
  .br_need(c("coverage_man_player_season_summary", "coverage_zone_player_season_summary"))
  fams <- list(man  = get("coverage_man_player_season_summary",  envir = .GlobalEnv),
               zone = get("coverage_zone_player_season_summary", envir = .GlobalEnv))
  if (exists("coverage_slot_player_season_summary", envir = .GlobalEnv))
    fams$slot <- get("coverage_slot_player_season_summary", envir = .GlobalEnv)

  found <- FALSE
  cards <- list()
  for (fam in names(fams)) {
    df <- fams[[fam]] %>% ungroup() %>% filter(season %in% seasons)
    i <- tryCatch(.br_find(df, "player", name), error = function(e) integer(0))
    if (length(i) == 0) next
    found <- TRUE
    pcols <- paste0(fam, "_", .br_cov_metrics, "_season_pctl")
    have  <- pcols %in% names(df)
    labs  <- .br_cov_metric_labels[have]
    out <- df[i, ] %>% arrange(season) %>%
      select(player, final_position, def_ssn, season, n, all_of(pcols[have]))
    names(out)[match(pcols[have], names(out))] <- labs

    for (pos in unique(out$final_position)) {          # never blend positions
      op <- out %>% filter(final_position == pos)
      win <- .br_card_window(op, card_seasons)
      cat("\n-- ", unique(op$player), " [", pos, "] — ", toupper(fam),
          " coverage percentiles (within position x season, higher = better) --\n", sep = "")
      print(as.data.frame(op %>% mutate(across(all_of(labs), ~ round(.x * 100, 1)))),
            row.names = FALSE)
      card <- .br_card(op, labs, "n", win)
      .br_card_print(card, win, paste0(fam, "/", pos, ": games-weighted, seasons ",
                                       paste(range(win), collapse = "-")))
      cards[[paste(fam, pos)]] <- list(seasons = op, card = card)
    }
  }

  ## below-gate seasons: visible rows from the raw frame, never dropped silently
  if (found && exists("coverage_summary", envir = .GlobalEnv)) {
    raw <- .br_ensure_final_position(get("coverage_summary", envir = .GlobalEnv))
    j <- tryCatch(.br_find(raw, "player", name), error = function(e) integer(0))
    if (length(j) > 0) {
      shown_seasons <- unique(unlist(lapply(cards, function(x) x$seasons$season)))
      bg <- raw[j, ] %>% ungroup() %>%
        filter(season %in% seasons, !(season %in% shown_seasons)) %>%
        group_by(player, final_position, team, season) %>%
        summarise(games = n_distinct(week),
                  snaps = sum(snap_counts_coverage, na.rm = TRUE),
                  raw_grade = weighted.mean(grades_coverage_defense, snap_counts_coverage, na.rm = TRUE),
                  .groups = "drop") %>% arrange(season)
      if (nrow(bg) > 0) {
        cat("\n-- BELOW THE GATE (not in any season summary — raw rows, no percentiles) --\n")
        print(as.data.frame(bg %>% mutate(raw_grade = round(raw_grade, 1))), row.names = FALSE)
      }
    }
  }
  if (!found)
    stop("no coverage rows for '", name,
         "' in the man/zone/slot season summaries. Check the name, or source ",
         "pff_pass_coverage_AWS.R first.")
  invisible(cards)
}

#### ---------------- OL card v6 ----------------
#### Whole line (T/G/C), position shown per row, stints collapsed WITHIN
#### position only — a player who swung LG/C keeps two separate rows/cards.

.br_ot_player_seasons <- function(seasons = 2016:2025, positions = NULL) {
  .br_need(c("all_pass_block_summary", "tps_pass_block_player_season_summary"))
  pb  <- get("all_pass_block_summary", envir = .GlobalEnv)
  tps <- get("tps_pass_block_player_season_summary", envir = .GlobalEnv)
  pos_levels <- unique(pb$det_position)
  if (is.null(positions)) positions <- pos_levels
  t_pos <- intersect(positions, pos_levels)
  if (length(t_pos) == 0)
    stop("no rows at those positions. det_position levels are: ",
         paste(pos_levels, collapse = ", "), " — widen the filter yourself.")
  # collapse QB-stints WITHIN player x position x season — never across positions
  join_keys <- intersect(c("player_id", "det_position", "season"), names(tps))
  tps_c <- tps %>% ungroup() %>%
    mutate(.w = if ("n" %in% names(tps)) n else 1) %>%
    group_by(across(all_of(join_keys))) %>%
    summarise(grade_season_pctl    = weighted.mean(grade_season_pctl,    .w, na.rm = TRUE),
              pressure_season_pctl = weighted.mean(pressure_season_pctl, .w, na.rm = TRUE),
              hurries_season_pctl  = weighted.mean(hurries_season_pctl,  .w, na.rm = TRUE),
              .groups = "drop")
  pb %>% ungroup() %>%
    filter(season %in% seasons, det_position %in% t_pos) %>%
    group_by(player, player_id, det_position, team_name, season) %>%
    summarise(pb_snaps = sum(snap_counts_pass_block, na.rm = TRUE),
              games = n_distinct(week), .groups = "drop") %>%
    inner_join(tps_c, by = join_keys) %>%
    filter(!is.na(grade_season_pctl))
}

ot_card <- function(name, card_seasons = NULL, seasons = 2016:2025) {
  ps <- .br_ot_player_seasons(seasons)     # the whole line
  i <- tryCatch(.br_find(ps, "player", name), error = function(e) integer(0))

  if (length(i) == 0) {
    pb <- get("all_pass_block_summary", envir = .GlobalEnv)
    j <- .br_find(pb, "player", name)
    out <- pb[j, ] %>% ungroup() %>%
      filter(season %in% seasons) %>%
      group_by(player, det_position, team_name, season) %>%
      summarise(games = n_distinct(week),
                pb_snaps = sum(snap_counts_pass_block, na.rm = TRUE),
                raw_grade = weighted.mean(grades_pass_block, snap_counts_pass_block, na.rm = TRUE),
                raw_pressure_rate = sum(pressures_allowed, na.rm = TRUE) / pmax(pb_snaps, 1),
                raw_hurry_rate = sum(hurries_allowed, na.rm = TRUE) / pmax(pb_snaps, 1),
                .groups = "drop") %>%
      arrange(season)
    cat("-- ", unique(out$player), " — NOT in the season summary; RAW rates only,\n",
        "   no percentiles (compare raw_pressure_rate to the room, roughly 0.05-0.10) --\n", sep = "")
    print(as.data.frame(out %>%
          mutate(raw_grade = round(raw_grade, 1),
                 raw_pressure_rate = round(raw_pressure_rate, 4),
                 raw_hurry_rate = round(raw_hurry_rate, 4))), row.names = FALSE)
    return(invisible(list(seasons = out, card = NULL)))
  }

  out <- ps[i, ] %>% arrange(season) %>%
    select(player, det_position, team_name, season, games, pb_snaps,
           grade_season_pctl, pressure_season_pctl, hurries_season_pctl)
  cards <- list()
  for (pos in unique(out$det_position)) {              # never blend positions
    op <- out %>% filter(det_position == pos)
    win <- .br_card_window(op, card_seasons)
    cat("-- ", unique(op$player), " [", pos, "] — TPS percentiles (within position x season).\n",
        "   ALL THREE higher = better: pressure/hurries are ranked on the NEGATED\n",
        "   rate upstream (pff_pass_block_AWS.R:131-155), so high = FEWER allowed --\n", sep = "")
    print(as.data.frame(op %>% mutate(across(ends_with("pctl"), ~ round(.x * 100, 1)))),
          row.names = FALSE)
    inv <- op %>% mutate(p_grade = grade_season_pctl,
                         p_press = pressure_season_pctl,
                         p_hurr  = hurries_season_pctl)
    card <- .br_card(inv, c("p_grade", "p_press", "p_hurr"), "pb_snaps", win)
    .br_card_print(card, win, paste0(pos, ": pb-snaps-weighted, seasons ",
                                     paste(range(win), collapse = "-"),
                                     "; all three higher = better protector"))
    cards[[pos]] <- list(seasons = op, card = card)
  }
  invisible(cards)
}

#### ---------------- WR card v6 ----------------
#### Machine block (rec_season_pctl_sos) first, then the ROLE-AWARE block:
#### over-expected metrics percentiled within season x tgt_cluster — receivers
#### compared within role and target cluster, the way the cohort functions do.

.br_wr_oe <- function(seasons = 2016:2025, min_tgt = 50) {
  .br_need("receiving_func_base")
  get_mode <- function(x) {
    tab <- table(x, useNA = "no")
    if (length(tab) == 0) NA_character_ else names(sort(tab, decreasing = TRUE))[1]
  }
  d <- get("receiving_func_base", envir = .GlobalEnv) %>% ungroup() %>%
    filter(season %in% seasons) %>%
    group_by(player_id, player, receiver_id, posteam, season) %>%
    summarise(
      tgts   = sum(targets, na.rm = TRUE),
      routes = sum(snap_counts_pass_route, na.rm = TRUE),
      tgt_per_route = tgts / pmax(routes, 1),
      tgt_share_avg = weighted.mean(tgt_share, snap_counts_pass_route, na.rm = TRUE),
      cp_oe  = weighted.mean(acc_rate, targets, na.rm = TRUE) - weighted.mean(pbp_cp,    targets, na.rm = TRUE),
      ypa_oe = weighted.mean(ypa,      targets, na.rm = TRUE) - weighted.mean(pbp_xypa,  targets, na.rm = TRUE),
      yac_oe = weighted.mean(yac,      targets, na.rm = TRUE) - weighted.mean(pbp_yac,   targets, na.rm = TRUE),
      adot   = weighted.mean(adot,     targets, na.rm = TRUE),
      tgt_cluster   = get_mode(tgt_cluster_name),
      align_cluster = get_mode(align_cluster_name),
      .groups = "drop") %>%
    filter(tgts >= min_tgt)

  # percentile within season x target-cluster role; pools under 5 are too thin
  d %>% group_by(season, tgt_cluster) %>%
    mutate(pool_n = n(),
           cp_oe_pctl      = if (pool_n[1] >= 5) percent_rank(cp_oe)        else NA_real_,
           ypa_oe_pctl     = if (pool_n[1] >= 5) percent_rank(ypa_oe)       else NA_real_,
           yac_oe_pctl     = if (pool_n[1] >= 5) percent_rank(yac_oe)       else NA_real_,
           tpr_pctl        = if (pool_n[1] >= 5) percent_rank(tgt_per_route) else NA_real_,
           tgtshare_pctl   = if (pool_n[1] >= 5) percent_rank(tgt_share_avg) else NA_real_) %>%
    ungroup()
}

wr_card <- function(name, card_seasons = NULL, seasons = 2016:2025, min_tgt_pool = 50) {
  ## machine block (unchanged)
  if (exists("rec_season_pctl_sos", envir = .GlobalEnv)) {
    ms <- get("rec_season_pctl_sos", envir = .GlobalEnv) %>%
      ungroup() %>% filter(season %in% seasons)
    i <- tryCatch(.br_find(ms, "player", name), error = function(e) integer(0))
    if (length(i) > 0) {
      pcols <- c("grade_pctl", "man_grade_pctl", "man_yprr_pctl", "man_tgt_pctl",
                 "zone_grade_pctl", "zone_yprr_pctl", "zone_tgt_pctl")
      labels <- c("Grade", "ManGrade", "ManYPRR", "ManTgt", "ZoneGrade", "ZoneYPRR", "ZoneTgt")
      have <- pcols %in% names(ms)
      labs <- labels[have]
      keep_raw <- intersect(c("band", "qual_g", "routes", "targets"), names(ms))
      out <- ms[i, ] %>% arrange(season) %>%
        select(player, season, all_of(keep_raw), all_of(pcols[have]))
      names(out)[match(pcols[have], names(out))] <- labs
      cat("-- ", unique(out$player), " — MACHINE receiving percentiles",
          " (band x rolling-window currency) --\n", sep = "")
      print(as.data.frame(out %>% mutate(across(all_of(labs), ~ round(.x * 100, 1)))),
            row.names = FALSE)
      wcol <- if ("routes" %in% names(out)) "routes" else "qual_g"
      card <- .br_card(out, labs, wcol, .br_card_window(out, card_seasons))
      .br_card_print(card, .br_card_window(out, card_seasons),
                     paste0(wcol, "-weighted avg percentile"))
    } else {
      cat("-- ", name, " not in rec_season_pctl_sos (gates or window) — machine block empty --\n", sep = "")
    }
  } else {
    cat("(rec_season_pctl_sos not in session — machine block skipped.",
        " source pff_stats/receiving/new_england_opp_receiving_schedule.R for it)\n")
  }

  ## role-aware over-expected block
  oe <- .br_wr_oe(seasons, min_tgt = 0)
  rid <- .br_wr_id(name, seasons)
  out <- oe %>% filter(receiver_id == rid) %>% arrange(season) %>%
    select(player, posteam, season, tgts, routes, tgt_cluster, align_cluster,
           adot, cp_oe, ypa_oe, yac_oe, tgt_per_route, tgt_share_avg,
           cp_oe_pctl, ypa_oe_pctl, yac_oe_pctl, tpr_pctl, tgtshare_pctl, pool_n)
  if (nrow(out) == 0)
    stop("no seasons for '", name, "' in receiving_func_base in this window.")
  cat("\n-- ROLE-AWARE over-expected percentiles — within season x tgt_cluster",
      "(his role, his cluster; NA = pool under 5) --\n", sep = "")
  print(as.data.frame(out %>%
        mutate(across(c(cp_oe, ypa_oe, yac_oe, tgt_per_route), ~ round(.x, 3)),
               tgt_share_avg = round(tgt_share_avg, 3), adot = round(adot, 1),
               across(ends_with("_pctl"), ~ round(.x * 100, 1)))), row.names = FALSE)
  card2 <- .br_card(out, c("cp_oe_pctl", "ypa_oe_pctl", "yac_oe_pctl", "tpr_pctl", "tgtshare_pctl"),
                    "tgts", .br_card_window(out, card_seasons))
  .br_card_print(card2, .br_card_window(out, card_seasons),
                 "targets-weighted avg percentile (within-role OE)")
  invisible(list(role_aware = list(seasons = out, card = card2)))
}


#### ============================================================================
#### V8 -- 2026-09-15 (Claude, on Andy's authority). Later definitions win.
####  - RETIRED: bridge_fit, bridge_delta, bridge_fit_stats, bridge_stats_delta,
####    bridge_wr1_add. Their rooms were top-2, fitted one unit at a time, the
####    run-defense room had no safeties, the percentiles were re-ranked here
####    instead of yours, and the wr1 pieces read combined_pbp. Each now stops and
####    names the replacement (waterfall_bridge.R):
####      a whole roster  -> waterfall("KC", "pass_def")
####      one player      -> player_swap("KC", "secondary", out = "...", inn = "...")
####      fit numbers     -> wf_fit("pass_def")
####  - player_swap(): player vs player at one team's 2026 seat, turned into
####    YPA / comp % / rating / sack % / pressure % (or YPC / rush TD per game)
####    through the waterfall fit. Needs waterfall_bridge.R sourced.
####  - wr_card: the role-aware pool is receivers with >= min_tgt_pool targets
####    (default 50) in that season x target cluster. It was 0, so 2-target
####    seasons set the scale. The player's own thinner seasons are still shown,
####    ranked against that pool, and marked below_pool.
####  - rookie_anchor / backup_anchor: print YOUR entry-year priors and YOUR 2025
####    replacement levels (read back from the availability frames), not numbers
####    re-derived here.
####  - Cards (cov / ot / pr / rd / rb / wr) unchanged: they show your season
####    summaries' percentiles (the heatmap tables). The waterfall and
####    player_swap use the league machine's tables. Same player, different pools
####    and gates, so a card number and a waterfall number are not the same scale.
#### ============================================================================

.br_retired <- function(fn, why, instead) {
  stop(fn, "() is retired (V8, 2026-09-15): ", why, "\n  Use instead: ", instead, call. = FALSE)
}
bridge_fit <- function(...) .br_retired("bridge_fit",
  "top-2 rooms fitted one unit at a time; the run-defense room had no safeties.",
  "wf_fit(\"pass_def\") / wf_fit(\"rush_def\") / wf_fit(\"pass_off\") / wf_fit(\"rush_off\") in waterfall_bridge.R")
bridge_delta <- function(...) .br_retired("bridge_delta", "it was built on bridge_fit.",
  "player_swap(team, unit, out, inn) for one player; waterfall(team, side) for a roster")
bridge_fit_stats <- function(...) .br_retired("bridge_fit_stats", "top-2 coverage rooms, one stat at a time.",
  "wf_fit(\"pass_def\")")
bridge_stats_delta <- function(...) .br_retired("bridge_stats_delta", "it was built on bridge_fit_stats.",
  "player_swap(team, \"secondary\", out, inn)")
bridge_wr1_add <- function(...) .br_retired("bridge_wr1_add", "it read combined_pbp and priced one 'WR1'.",
  "player_swap(team, \"receiving\", out, inn): corps of 8, within band")

## player vs player at one team's 2026 seat -> outcome change (waterfall_bridge.R wf_swap)
player_swap <- function(team, unit, out, inn = NULL, value = NULL, ...) {
  if (!exists("wf_swap", mode = "function"))
    stop("player_swap needs waterfall_bridge.R: source(\"pff_stats/evaluation/waterfall_bridge.R\") first.")
  wf_swap(team, unit, out, inn = inn, value = value, ...)
}

## ---- anchors: YOUR priors and YOUR replacement levels ----
.br_unit8 <- function(unit) {
  alias <- c(cb = "secondary", cbx = "secondary", s = "secondary", lb = "secondary",
             ot = "pass_block", ol = "pass_block")
  u <- if (unit %in% names(alias)) alias[[unit]] else unit
  ok <- c("secondary", "pass_rush", "run_defense", "pass_block", "run_block", "receiving", "rushing")
  if (!u %in% ok) stop("unit must be one of: ", paste(ok, collapse = ", "))
  u
}

rookie_anchor <- function(unit = "secondary", ...) {
  u <- .br_unit8(unit)
  fr <- switch(u,
    secondary   = c("prior_law_sec_lg", "c3_entry_prior_cv"),
    pass_rush   = "prush_rookie_prior",
    run_defense = "rundef_rookie_prior",
    pass_block  = c("rookie_prior", "c3_rookie_prior_ol"),
    run_block   = c("rookie_prior", "c3_rookie_prior_rb", "c3_scheme_prior_rb"),
    receiving   = "rec_rookie_prior",
    rushing     = c("rush_rookie_prior", "c3_rookie_prior_ru"))
  cat("-- YOUR entry-year priors for ", u,
      " (what the machine prices a player with no NFL history at) --\n", sep = "")
  for (f in fr) {
    cat("\n", f, ":\n", sep = "")
    if (!exists(f, envir = .GlobalEnv)) { cat("  (not in session -- source that unit's chain)\n"); next }
    x <- as.data.frame(get(f, envir = .GlobalEnv))
    num <- vapply(x, is.numeric, logical(1))
    x[num] <- lapply(x[num], function(v) round(v, 3))
    print(x, row.names = FALSE)
  }
  invisible(NULL)
}

backup_anchor <- function(unit = "secondary", ...) {
  u <- .br_unit8(unit)
  sp <- switch(u,
    secondary   = list(fr = "members_cv", grp = c("split", "band"),
                       pairs = list(grade = c("grade_f", "grade_p"), supp = c("supp_f", "supp_p"), c3 = c("c3_f", "adj_p"))),
    pass_rush   = list(fr = "members_pa", grp = "band", pairs = list(grade = c("gf", "gf_p"), c3 = c("c3_f", "c3_p"))),
    run_defense = list(fr = "members_ra", grp = "band",
                       pairs = list(grade = c("grade_f", "grade_p"), stop = c("stop_f", "stop_p"), c3 = c("c3_f", "c3_p"))),
    pass_block  = list(fr = "slot_value_26_pb_build", grp = "slot",
                       pairs = list(tps = c("gf_raw", "gf_raw_av"), c3 = c("gf_adj", "gf_adj_av"))),
    run_block   = list(fr = "slot_value_26_rb_build", grp = "slot",
                       pairs = list(gap = c("V_gap", "V_gap_av"), zone = c("V_zone", "V_zone_av"))),
    receiving   = list(fr = "members_rc", grp = character(0),
                       pairs = list(man_grade = c("mg_f", "mg_p"), zone_grade = c("zg_f", "zg_p"))),
    rushing     = list(fr = "memb_lg_ru", grp = character(0),
                       pairs = list(grade = c("gf", "gf_av"), mtf = c("mf", "mf_av"))))
  if (!exists(sp$fr, envir = .GlobalEnv)) stop(sp$fr, " not in session -- source the ", u, " availability layer.")
  d <- tibble::as_tibble(get(sp$fr, envir = .GlobalEnv)) %>% ungroup()
  if (!"avail" %in% names(d)) stop(sp$fr, " has no avail column.")
  grp <- intersect(sp$grp, names(d))
  out <- NULL
  for (nm in names(sp$pairs)) {
    fc <- sp$pairs[[nm]][1]; pc <- sp$pairs[[nm]][2]
    if (!all(c(fc, pc) %in% names(d))) next
    r <- d %>%
      mutate(.rp = ifelse(!is.na(avail) & avail < 0.999, (.data[[pc]] - avail * .data[[fc]]) / (1 - avail), NA_real_)) %>%
      group_by(across(all_of(grp))) %>%
      summarise(!!nm := round(100 * suppressWarnings(stats::median(.rp, na.rm = TRUE)), 1), .groups = "drop")
    out <- if (is.null(out)) r else if (length(grp)) left_join(out, r, by = grp) else bind_cols(out, r)
  }
  cat("-- YOUR 2025 replacement (backup) level for ", u, ", percentile points, read back from ", sp$fr,
      " (priced = avail x full + (1 - avail) x replacement) --\n", sep = "")
  print(as.data.frame(out), row.names = FALSE)
  invisible(out)
}

## ---- wr_card: role-aware pool = receivers with >= min_tgt_pool targets ----
.br_wr_oe8 <- function(seasons = 2016:2025, min_tgt = 50) {
  d <- .br_wr_oe(seasons, min_tgt = 0) %>% select(-ends_with("_pctl"), -any_of("pool_n"))
  mets <- c(cp_oe = "cp_oe_pctl", ypa_oe = "ypa_oe_pctl", yac_oe = "yac_oe_pctl",
            tgt_per_route = "tpr_pctl", tgt_share_avg = "tgtshare_pctl")
  pool <- d %>% filter(tgts >= min_tgt)
  pn <- pool %>% count(season, tgt_cluster, name = "pool_n")
  d <- d %>% left_join(pn, by = c("season", "tgt_cluster")) %>%
    mutate(pool_n = dplyr::coalesce(pool_n, 0L), below_pool = tgts < min_tgt)
  key <- paste(d$season, d$tgt_cluster)
  for (m in names(mets)) {
    pv <- split(pool[[m]], paste(pool$season, pool$tgt_cluster))
    d[[mets[[m]]]] <- mapply(function(x, k, n) {
      v <- pv[[k]]
      if (is.na(x) || is.null(v) || n < 5) NA_real_ else mean(v[!is.na(v)] <= x)
    }, d[[m]], key, d$pool_n)
  }
  d
}

wr_card <- function(name, card_seasons = NULL, seasons = 2016:2025, min_tgt_pool = 50) {
  ## machine block (as in v6)
  if (exists("rec_season_pctl_sos", envir = .GlobalEnv)) {
    ms <- get("rec_season_pctl_sos", envir = .GlobalEnv) %>%
      ungroup() %>% filter(season %in% seasons)
    i <- tryCatch(.br_find(ms, "player", name), error = function(e) integer(0))
    if (length(i) > 0) {
      pcols <- c("grade_pctl", "man_grade_pctl", "man_yprr_pctl", "man_tgt_pctl",
                 "zone_grade_pctl", "zone_yprr_pctl", "zone_tgt_pctl")
      labels <- c("Grade", "ManGrade", "ManYPRR", "ManTgt", "ZoneGrade", "ZoneYPRR", "ZoneTgt")
      have <- pcols %in% names(ms)
      labs <- labels[have]
      keep_raw <- intersect(c("band", "qual_g", "routes", "targets"), names(ms))
      out <- ms[i, ] %>% arrange(season) %>%
        select(player, season, all_of(keep_raw), all_of(pcols[have]))
      names(out)[match(pcols[have], names(out))] <- labs
      cat("-- ", unique(out$player), " -- MACHINE receiving percentiles",
          " (band x rolling-window currency) --\n", sep = "")
      print(as.data.frame(out %>% mutate(across(all_of(labs), ~ round(.x * 100, 1)))),
            row.names = FALSE)
      wcol <- if ("routes" %in% names(out)) "routes" else "qual_g"
      card <- .br_card(out, labs, wcol, .br_card_window(out, card_seasons))
      .br_card_print(card, .br_card_window(out, card_seasons),
                     paste0(wcol, "-weighted avg percentile"))
    } else {
      cat("-- ", name, " not in rec_season_pctl_sos (gates or window) -- machine block empty --\n", sep = "")
    }
  } else {
    cat("(rec_season_pctl_sos not in session -- machine block skipped.",
        " source pff_stats/receiving/new_england_opp_receiving_schedule.R for it)\n")
  }

  ## role-aware over-expected block, pool = >= min_tgt_pool targets in season x cluster
  oe <- .br_wr_oe8(seasons, min_tgt = min_tgt_pool)
  rid <- .br_wr_id(name, seasons)
  out <- oe %>% filter(receiver_id == rid) %>% arrange(season) %>%
    select(player, posteam, season, tgts, routes, tgt_cluster, align_cluster,
           adot, cp_oe, ypa_oe, yac_oe, tgt_per_route, tgt_share_avg,
           cp_oe_pctl, ypa_oe_pctl, yac_oe_pctl, tpr_pctl, tgtshare_pctl, pool_n, below_pool)
  if (nrow(out) == 0)
    stop("no seasons for '", name, "' in receiving_func_base in this window.")
  cat("\n-- ROLE-AWARE over-expected percentiles, within season x tgt_cluster. Pool = receivers\n",
      "   with >= ", min_tgt_pool, " targets there (pool_n); his thinner seasons are ranked against\n",
      "   that pool and marked below_pool. NA = pool under 5 --\n", sep = "")
  print(as.data.frame(out %>%
        mutate(across(c(cp_oe, ypa_oe, yac_oe, tgt_per_route), ~ round(.x, 3)),
               tgt_share_avg = round(tgt_share_avg, 3), adot = round(adot, 1),
               across(ends_with("_pctl"), ~ round(.x * 100, 1)))), row.names = FALSE)
  card2 <- .br_card(out, c("cp_oe_pctl", "ypa_oe_pctl", "yac_oe_pctl", "tpr_pctl", "tgtshare_pctl"),
                    "tgts", .br_card_window(out, card_seasons))
  .br_card_print(card2, .br_card_window(out, card_seasons),
                 "targets-weighted avg percentile (within-role OE)")
  invisible(list(role_aware = list(seasons = out, card = card2)))
}



#### ============================================================================
#### V9 -- 2026-09-15 (Claude, on Andy's authority): ol_card = the whole line,
#### PASS blocking (true pass sets, as before) AND RUN blocking (your gap / zone
#### season percentiles, plus run-block c3 when in session). ot_card() is now just
#### another name for ol_card().
#### ============================================================================
.br_ol_pass_card <- ot_card      # the v6 pass-block card (true pass sets)

ol_card <- function(name, card_seasons = NULL, seasons = 2016:2025) {
  cat("==================== PASS BLOCKING (true pass sets) ====================\n")
  res <- .br_ol_pass_card(name, card_seasons = card_seasons, seasons = seasons)
  cat("\n==================== RUN BLOCKING (gap / zone) ====================\n")
  pid <- integer(0)
  if (exists("all_pass_block_summary", envir = .GlobalEnv)) {
    pb <- get("all_pass_block_summary", envir = .GlobalEnv) %>% ungroup()
    j <- tryCatch(.br_find(pb, "player", name), error = function(e) integer(0))
    pid <- unique(pb$player_id[j])
  }
  have <- c(gap = exists("gap_player_season_summary", envir = .GlobalEnv),
            zone = exists("zone_player_season_summary", envir = .GlobalEnv))
  if (!length(pid) || !any(have)) {
    cat("(no run-block season rows: ", if (!length(pid)) "player not found" else
          "gap/zone_player_season_summary not in session -- source pff_stats/run_block/pff_run_block_AWS.R", ")\n", sep = "")
    return(invisible(res))
  }
  pick <- function(f, col, lab) {
    if (!exists(f, envir = .GlobalEnv)) return(NULL)
    get(f, envir = .GlobalEnv) %>% ungroup() %>%
      filter(player_id %in% pid, season %in% seasons) %>%
      transmute(det_position, season, !!paste0(lab, "_games") := n, !!lab := .data[[col]])
  }
  g <- pick("gap_player_season_summary", "gap_season_pctl", "gap")
  z <- pick("zone_player_season_summary", "zone_season_pctl", "zone")
  rb <- if (is.null(g)) z else if (is.null(z)) g else full_join(g, z, by = c("det_position", "season"))
  if (exists("rblk_c3_pctl", envir = .GlobalEnv)) {
    c3 <- get("rblk_c3_pctl", envir = .GlobalEnv) %>% ungroup() %>% filter(player_id %in% pid) %>%
      select(season, any_of(c("gap_c3_pctl", "zone_c3_pctl"))) %>%
      rename(any_of(c(gap_c3 = "gap_c3_pctl", zone_c3 = "zone_c3_pctl")))
    rb <- rb %>% left_join(c3, by = "season")
  }
  if (is.null(rb) || !nrow(rb)) { cat("(no qualifying run-block seasons -- your gate is 6+ games at a slot)\n"); return(invisible(res)) }
  rb <- rb %>% arrange(det_position, season)
  pc <- intersect(c("gap", "zone", "gap_c3", "zone_c3"), names(rb))
  cat("-- ", name, " -- run-block percentiles within slot x season (higher = better; c3 = opponent-adjusted) --\n", sep = "")
  print(as.data.frame(rb %>% mutate(across(all_of(pc), ~ round(.x * 100, 1)))), row.names = FALSE)
  for (pos in unique(rb$det_position)) {
    op <- rb %>% filter(det_position == pos)
    win <- .br_card_window(op, card_seasons)
    w <- op %>% filter(season %in% win)
    wm <- function(v, n) { ok <- !is.na(v) & !is.na(n); if (any(ok)) round(100 * sum(v[ok] * n[ok]) / sum(n[ok]), 1) else NA }
    cat("\nRUN CARD (", paste(range(win), collapse = "-"), ") -- ", pos, ": games-weighted\n", sep = "")
    print(c(gap  = if ("gap"  %in% names(w)) wm(w$gap,  w$gap_games)  else NA,
            zone = if ("zone" %in% names(w)) wm(w$zone, w$zone_games) else NA))
  }
  attr(res, "run_block") <- rb
  invisible(res)
}
ot_card <- ol_card
