####
#### roster_swap_lab.R — v2 2026-09-14. Standalone. Read-only lab.
#### Changes nothing, saves nothing, deletes nothing, sources nothing.
#### NEVER source stats_comparison_engine.R / rec_comparison_engine.R —
#### their last lines push workbooks to S3 just by sourcing them.
####
#### v2 fixes (verified against the repo after independent review):
####   - swaps match by EXACT name -> player_id, and move ALL of a player's rows
####     (secondary carries one row per man/zone split; v1 moved only one).
####     Substring matching is fallback only. ("McDuffie" also hits Isaiah.)
####   - hand in_value goes into the RAW (bef) and priced (aft) columns only;
####     the slate-adjusted c3 columns are set NA — a hand number is not in
####     opponent-adjusted currency.
####   - headline delta defaults to currency = "raw" (the *_f full-health
####     columns). The priced *_p columns already contain the injury discount —
####     for one game with a known-active lineup, raw is the honest lens and
####     hand-discounting Sneed on top of *_p would count his injuries twice.
####   - lab_wr1_swap rebuilt: WR1 quality is now mean(xpass_percentile,
####     xtd_percentile). z_score_percentile is NOT quality — it is a
####     man-minus-zone TENDENCY (pff_receiving_man_zone_exploration_AWS.R:919-953).
####     It is still printed, labeled mz_lean, as color only.
####   - team-season passing is POOLED COUNTS by posteam x season straight from
####     combined_pbp (never means of game ratios, never qbgrp_ssn — the qbgrp
####     split would break a team-year across two QBs). Passer rating is the
####     standard NFL formula from pooled comp/att/yds/td/int (0-158.3).
####   - optional same-starting-QB filter (dominant qbgrp base both seasons).
####
#### Functions:
####   lab_team(team)   — who the machine is pricing, per unit, all values exposed.
####   lab_swap(...)    — swap one player for another BY NAME, or a HAND percentile.
####   lab_comp(name)   — season-by-season record (injury years visible) + comps.
####   lab_wr1_swap()   — history of WR1 changes vs pooled ypa/comp/rating/xtds.
####
#### Needs in session (whatever units you use; each function gates on its own):
####   members_cv, members_pa, members_ra, members_rc, memb_lg_ru,
####   slot_value_26_pb_build, slot_value_26_rb_build     (availability layers)
####   receiving_func_base + combined_pbp                 (lab_wr1_swap only)
####
#### TONIGHT — Chiefs at Broncos (pass coverage + pass protection only):
####   lab_team("KC")
####   lab_comp("L'Jarius Sneed", "secondary")
####   lab_swap("secondary", "KC", out = "Trent McDuffie", in_name = "L'Jarius Sneed")
####   lab_swap("secondary", "KC", out = "Jaylen Watson",  in_value = 0.45)   # rookie CB: YOUR number
####   lab_swap("secondary", "KC", out = "Bryan Cook",     in_name = "Alohi Gilman")
####   lab_swap("secondary", "KC", out = "Chamarri Conner",in_name = "Jaden Hicks")
####   lab_swap("pass_block", "KC", out = "Jawaan Taylor", in_name = "Jaylon Moore")
####   lab_swap("pass_block", "KC", out = "Josh Simmons",  in_value = 0.35)   # Benson (rookie): YOUR number
####   lab_team("DEN")
####   lab_comp("Jaylen Waddle", "receiving")
####   lab_wr1_swap()
####
#### NOTE: if the 2026 roster frames already show the post-trade lineup, the
#### out= name may not be on the team — the error tells you; run lab_team first
#### and swap whoever the file actually lists.
####

#### ---------------- internals ----------------

.lab_frames <- list(
  pass_rush  = "members_pa",
  run_defense= "members_ra",
  rushing    = "memb_lg_ru",
  pass_block = "slot_value_26_pb_build",
  run_block  = "slot_value_26_rb_build",
  receiving  = "members_rc",
  secondary  = "members_cv"
)

# per-unit lens spec: raw bef/aft, adjusted abef/aaft, weight (NULL = flat OL mean)
.lab_spec <- function(unit) {
  specs <- list(
    pass_rush = list(team_col = "team", w = "uw", lenses = list(
      main = c(bef = "gf", aft = "gf_p", abef = "c3_f", aaft = "c3_p"))),
    run_defense = list(team_col = "team", w = "usage_w", lenses = list(
      main = c(bef = "grade_f", aft = "grade_p", abef = "c3_f", aaft = "c3_p"))),
    rushing = list(team_col = "team", w = "uw", lenses = list(
      main = c(bef = "gf", aft = "gf_av", abef = "V_c3", aaft = "V_c3_av"))),
    pass_block = list(team_col = "team_name", w = NULL, lenses = list(
      main = c(bef = "gf_raw", aft = "gf_raw_av", abef = "gf_adj", aaft = "gf_adj_av"))),
    run_block = list(team_col = "team_name", w = NULL, lenses = list(
      gap  = c(bef = "V_gap",  aft = "V_gap_av",  abef = "V_c3_gap",  aaft = "V_c3_gap_av"),
      zone = c(bef = "V_zone", aft = "V_zone_av", abef = "V_c3_zone", aaft = "V_c3_zone_av"))),
    receiving = list(team_col = "team_name", w = "usage_w", lenses = list(
      man  = c(bef = "mg_f", aft = "mg_p", abef = "c3mg_f", aaft = "c3mg_p"),
      zone = c(bef = "zg_f", aft = "zg_p", abef = "c3zg_f", aaft = "c3zg_p"))),
    secondary = list(team_col = "team", w = "usage_w", split_col = "split", lenses = list(
      main = c(bef = "grade_f", aft = "grade_p", abef = "c3_f", aaft = "adj_p"))))
  if (!unit %in% names(specs))
    stop("unknown unit '", unit, "'. One of: ", paste(names(specs), collapse = ", "))
  specs[[unit]]
}

.lab_get_frame <- function(unit) {
  fn <- .lab_frames[[unit]]
  if (!exists(fn, envir = .GlobalEnv))
    stop("missing frame: ", fn, " — source the ", unit,
         " availability layer first (see REBUILD_RUNBOOK.md Stage 4).")
  d <- get(fn, envir = .GlobalEnv)
  # v3: some frames name players full_name / player, not roster_name
  if (!"roster_name" %in% names(d)) {
    nc <- intersect(c("full_name", "player"), names(d))[1]
    if (!is.na(nc)) d$roster_name <- d[[nc]]
  }
  d
}

# v3: every team-code spelling (nflreadr and PFF) resolves to one team
.lab_canon <- function(x) {
  x <- toupper(as.character(x))
  x[x %in% c("ARI", "AZ")] <- "ARZ"; x[x == "BAL"] <- "BLT"; x[x == "CLE"] <- "CLV"
  x[x == "HOU"] <- "HST"; x[x == "WSH"] <- "WAS"; x[x == "JAC"] <- "JAX"; x[x == "LAR"] <- "LA"
  x
}
.lab_team_filter <- function(df, team_col, team) {
  df %>% filter(.lab_canon(.data[[team_col]]) == .lab_canon(team)[1])
}

# EXACT case-insensitive name match first; unique substring as fallback.
# Returns ALL row indices for the player (secondary = 2 split rows).
# Stops if the name spans more than one player_id.
.lab_find_rows <- function(df, name, label = "player") {
  rn <- tolower(df$roster_name)
  idx <- which(rn == tolower(name))
  if (length(idx) == 0) idx <- grep(tolower(name), rn, fixed = TRUE)
  if (length(idx) == 0) {
    sugg <- unique(df$roster_name[agrep(tolower(name), rn, max.distance = 3, ignore.case = TRUE)])
    stop("no ", label, " matching '", name, "'.",
         if (length(sugg)) paste0(" Did you mean: ", paste(utils::head(sugg, 8), collapse = ", "), "?")
         else "")
  }
  ids <- unique(df$player_id[idx])
  ids <- ids[!is.na(ids)]
  if (length(ids) > 1)
    stop("'", name, "' matches ", length(ids), " different player_ids: ",
         paste(unique(df$roster_name[idx]), collapse = " / "),
         " — use the exact full name.")
  idx
}

.lab_agg <- function(rows, spec, label) {
  out <- lapply(names(spec$lenses), function(lens) {
    c4 <- spec$lenses[[lens]]
    r <- rows
    if (!is.null(spec$split_col)) {
      return(bind_rows(lapply(split(r, r[[spec$split_col]]), function(rr) {
        .lab_agg_one(rr, c4, spec$w) %>% mutate(lens = rr[[spec$split_col]][1], .before = 1)
      })))
    }
    .lab_agg_one(r, c4, spec$w) %>% mutate(lens = lens, .before = 1)
  }) %>% bind_rows()
  out %>% mutate(unit = label, .before = 1)
}

.lab_agg_one <- function(r, c4, w) {
  agg <- function(col) {
    v <- r[[col]]
    keep <- !is.na(v)
    if (!any(keep)) return(NA_real_)
    if (is.null(w)) return(mean(v[keep]))
    ww <- r[[w]][keep]
    if (all(is.na(ww)) || sum(ww) <= 0) return(mean(v[keep]))
    weighted.mean(v[keep], ww)
  }
  tibble(n_priced = sum(!is.na(r[[c4["aft"]]])),
         bef  = agg(c4["bef"]),  aft  = agg(c4["aft"]),
         abef = agg(c4["abef"]), aaft = agg(c4["aaft"]),
         d    = aft - bef,       ad   = aaft - abef)
}

.lab_print_rows <- function(rows, spec, mark = character()) {
  show_cols <- unique(c("roster_name", "player_id",
                        intersect(c("band", "slot", "split", "status"), names(rows)),
                        spec$w, "avail"))
  show_cols <- show_cols[show_cols %in% names(rows)]
  lens1 <- spec$lenses[[1]]
  val_cols <- intersect(c(lens1["bef"], lens1["aft"], lens1["abef"], lens1["aaft"]), names(rows))
  out <- rows %>% select(all_of(c(show_cols, val_cols)))
  out$mark <- ifelse(out$roster_name %in% mark, "  <--", "")
  print(as.data.frame(out), row.names = FALSE)
  invisible(out)
}

#### ---------------- 1. who is the machine pricing ----------------

lab_team <- function(team, units = names(.lab_frames)) {
  for (u in units) {
    fn <- .lab_frames[[u]]
    if (!exists(fn, envir = .GlobalEnv)) {
      cat("\n== ", u, " ==  skipped -- missing frame: ", fn, "\n", sep = "")
      next
    }
    spec <- .lab_spec(u)
    rows <- .lab_team_filter(get(fn, envir = .GlobalEnv), spec$team_col, team)
    rows <- .lab_team_filter(.lab_get_frame(u), spec$team_col, team)
    cat("\n== ", u, " (", fn, ") — ", nrow(rows), " rows ==\n", sep = "")
    if (nrow(rows) == 0) { cat("no rows for ", team, "\n"); next }
    .lab_print_rows(rows, spec)
    print(as.data.frame(.lab_agg(rows, spec, u)), row.names = FALSE)
  }
  invisible(NULL)
}

#### ---------------- 2. swap a player, see the unit move ----------------

## out       : exact roster_name (or unique substring) of the player coming OUT.
##             ALL his rows move (secondary = both man and zone split rows).
## in_name   : exact name of the replacement anywhere in the same frame; his
##             values come over, matched on split where the frame has splits.
##             The OUT player's role/weight stays (CB1 slot, LT slot, etc.).
## in_value  : OR your hand percentile [0,1] — rookies, your own discount.
##             Sets RAW (bef) and priced (aft) = in_value, avail = in_avail;
##             adjusted c3 columns are NA (a hand number is not slate-adjusted).
## currency  : which lens the headline DELTA uses: "raw" (default — full-health
##             values, right for one game with a known lineup), "priced"
##             (injury-discounted), or "adjusted" (opponent-adjusted c3).
lab_swap <- function(unit, team, out, in_name = NULL, in_value = NULL,
                     in_avail = 1, currency = c("raw", "priced", "adjusted")) {
  currency <- match.arg(currency)
  if (is.null(in_name) && is.null(in_value))
    stop("give in_name (a player in the frame) or in_value (a hand percentile).")
  spec <- .lab_spec(unit)
  df   <- .lab_get_frame(unit)
  rows <- .lab_team_filter(df, spec$team_col, team)
  if (nrow(rows) == 0) stop("no ", unit, " rows for ", team,
                            " — if the 2026 roster already shows the post-trade lineup, run lab_team('",
                            toupper(team), "') and swap who is actually listed.")

  i_out  <- .lab_find_rows(rows, out, "outgoing player")
  out_nm <- rows$roster_name[i_out[1]]

  val_cols <- unique(unlist(lapply(spec$lenses, function(c4) c4[c("bef","aft","abef","aaft")])))

  new_rows <- rows[i_out, ]
  if (!is.null(in_name)) {
    i_in <- .lab_find_rows(df, in_name, "incoming player")
    for (k in seq_along(i_out)) {
      donor <- i_in[1]
      if (!is.null(spec$split_col)) {
        m <- which(df$split[i_in] == rows$split[i_out[k]])
        if (length(m) == 0) stop("incoming player has no ", rows$split[i_out[k]],
                                 " split row — cannot match ", out_nm, "'s split.")
        donor <- i_in[m[1]]
      }
      for (vc in val_cols) new_rows[k, vc] <- df[donor, vc]
      if ("avail" %in% names(df)) new_rows$avail[k] <- df$avail[donor]
    }
    role_col <- intersect(c("band", "slot"), names(df))[1]
    if (!is.na(role_col)) {
      r_out <- unique(rows[[role_col]][i_out]); r_in <- unique(df[[role_col]][i_in])
      if (!all(r_in %in% r_out))
        cat("NOTE: ", df$roster_name[i_in[1]], " is priced as ", paste(r_in, collapse = "/"),
            "; the seat is ", paste(r_out, collapse = "/"), ". His percentile comes from his own ",
            role_col, " pool.\n", sep = "")
    }
    new_rows$roster_name <- paste0(df$roster_name[i_in[1]], " [IN]")
    cat("IN : ", df$roster_name[i_in[1]], " (", df[[spec$team_col]][i_in[1]],
        ") — machine-priced values carried over, matched on split\n", sep = "")
  } else {
    for (k in seq_along(i_out)) {
      for (c4 in spec$lenses) {
        new_rows[k, c4["bef"]]  <- in_value     # raw: your price
        new_rows[k, c4["aft"]]  <- in_value     # priced == raw at avail = 1
        new_rows[k, c4["abef"]] <- NA_real_     # c3 columns: NOT hand-settable
        new_rows[k, c4["aaft"]] <- NA_real_
      }
      if ("avail" %in% names(new_rows)) new_rows$avail[k] <- in_avail
    }
    new_rows$roster_name <- paste0(out_nm, " -> HAND(", in_value, ")")
    cat("IN : hand-entered percentile ", in_value, " (0.50 = league average);",
        " avail = ", in_avail, ". c3 adjusted columns set NA — a hand number",
        " is not opponent-adjusted currency.\n", sep = "")
  }

  before <- .lab_agg(rows, spec, unit)
  swapped <- bind_rows(rows[-i_out, ], new_rows)
  after  <- .lab_agg(swapped, spec, unit)

  cat("\nOUT: ", out_nm, " (", length(i_out), " row(s) moved)\n", sep = "")
  cat("\n-- member rows AFTER swap --\n")
  .lab_print_rows(swapped, spec, mark = c(out_nm, new_rows$roster_name[1]))
  cat("\n-- unit aggregates, viewer law (weighted where the unit carries weights, flat for OL) --\n")
  report <- bind_rows(before %>% mutate(state = "before"),
                      after  %>% mutate(state = "after")) %>%
    select(unit, lens, state, everything()) %>%
    arrange(lens, state)
  print(as.data.frame(report), row.names = FALSE)

  cur_cols <- switch(currency, raw = c("bef"), priced = c("aft"), adjusted = c("aaft"))
  cur_col <- cur_cols[1]
  delta <- after %>% select(lens) %>%
    mutate(currency = currency,
           before   = before[[cur_col]][match(lens, before$lens)],
           after    = after[[cur_col]][match(lens, after$lens)],
           delta    = after - before)
  cat("\nDELTA in ", currency, " currency (full-season unit percentile points):\n", sep = "")
  print(as.data.frame(delta), row.names = FALSE)
  if (currency == "raw")
    cat("(raw = full-health values. For one game with a known lineup this is the\n",
        " honest lens; *_p already contains the injury discount, so do not\n",
        " hand-discount on top of it.)\n", sep = "")
  invisible(list(before = before, after = after, delta = delta))
}

#### ---------------- 3. the record behind a name + nearest comps ----------------

lab_comp <- function(name, unit, n = 10) {
  spec <- .lab_spec(unit)
  df   <- .lab_get_frame(unit)

  cat("-- what the machine currently prices --\n")
  i <- tryCatch(.lab_find_rows(df, name), error = function(e) integer(0))
  if (length(i) == 0) {
    cat("not in ", .lab_frames[[unit]], " at all — the machine has NO price for him.\n", sep = "")
  } else {
    .lab_print_rows(df[i, ], spec)
  }

  hist_frame <- list(pass_rush = "prush_c3_pctl", run_defense = "rd_c3_pctl",
                     rushing = "ru_c3_pctl", receiving = "rec_c3_pctl",
                     secondary = "cov_c3_pctl",
                     pass_block = "tps_pass_block_player_season_summary",
                     run_block = NULL)[[unit]]
  if (!is.null(hist_frame) && exists(hist_frame, envir = .GlobalEnv)) {
    h <- get(hist_frame, envir = .GlobalEnv)
    name_col <- intersect(c("player", "roster_name", "player_name"), names(h))[1]
    hn <- tolower(h[[name_col]])
    j <- which(hn == tolower(name))
    if (length(j) == 0) j <- grep(tolower(name), hn, fixed = TRUE)
    if (length(j) > 0) {
      keep <- intersect(c(name_col, "season", "split", "band", "team_name", "team",
                          "c3_pctl", "grade_season_pctl", "pressure_season_pctl",
                          "hurries_season_pctl", "sp_snaps", "n", "qual_g"), names(h))
      cat("\n-- season-by-season (", hist_frame, ") — every year shown, blend nothing --\n", sep = "")
      print(as.data.frame(h[j, ] %>% arrange(season) %>% select(all_of(keep))), row.names = FALSE)
    } else {
      cat("\nno rows in ", hist_frame, " either — true zero-history player.\n", sep = "")
    }
  } else if (!is.null(hist_frame)) {
    cat("\n(", hist_frame, " not in session — source the ", unit, " currency file for season history.)\n", sep = "")
  }

  if (length(i) > 0) {
    c4  <- spec$lenses[[1]]
    val <- df[[c4["bef"]]]
    pool <- df
    if ("band" %in% names(df) && !is.na(df$band[i[1]])) pool <- df %>% filter(band == df$band[i[1]])
    comps <- pool %>%
      filter(!is.na(.data[[c4["bef"]]]), .data$player_id != df$player_id[i[1]]) %>%
      mutate(dist = abs(.data[[c4["bef"]]] - val[i[1]])) %>%
      arrange(dist) %>% utils::head(n)
    cat("\n-- nearest ", nrow(comps), " comps",
        if ("band" %in% names(df) && !is.na(df$band[i[1]])) paste0(" in band ", df$band[i[1]]) else "",
        " by raw (full-health) value --\n", sep = "")
    .lab_print_rows(comps, spec)
  }
  invisible(NULL)
}

#### ---------------- 4. what a WR1 change does to the passing game ----------------

## WR1 = max season tgt_share among WRs with >= min_games games (receiving_func_base).
## WR1 QUALITY = mean(xpass_percentile, xtd_percentile) — model-based production
## percentiles, both season-grain. z_score_percentile is printed as mz_lean only:
## it is a man-minus-zone tendency, NOT quality.
## Team passing = POOLED COUNTS by posteam x season from combined_pbp:
##   att    = sum(pass_attempt) - sum(sack)        (house convention, sacks out)
##   ypa    = sum(passing_yards) / att
##   comp   = sum(complete_pass) / att
##   rating = standard NFL passer rating from pooled comp/yds/td/int (0-158.3)
##   xtds   = pooled pbp xTD model sums
## same_qb_only keeps only team-pairs where the dominant QB group is the same
## both seasons (the Nix-assumption, applied to history).
lab_wr1_swap <- function(seasons = 2016:2025, min_games = 6, same_qb_only = TRUE) {
  for (f in c("receiving_func_base", "combined_pbp"))
    if (!exists(f, envir = .GlobalEnv))
      stop("missing frame: ", f, " — ", if (f == "receiving_func_base")
        "source pff_stats/receiving/receiving_stats_build_AWS.R" else
        "load the workspace or run the data_build chain", " first.")
  rfb <- get("receiving_func_base", envir = .GlobalEnv)
  pbp <- get("combined_pbp",        envir = .GlobalEnv)

  wr1 <- rfb %>% ungroup() %>%
    filter(final_position_group == "WR", season %in% seasons) %>%
    group_by(abbreviation, season, player_id, player) %>%
    summarise(wr1_games = n(), tgt_share_avg = mean(tgt_share, na.rm = TRUE),
              wr1_quality = mean(c(xpass_percentile, xtd_percentile), na.rm = TRUE),
              mz_lean     = mean(z_score_percentile, na.rm = TRUE),
              .groups = "drop") %>%
    filter(wr1_games >= min_games) %>%
    group_by(abbreviation, season) %>%
    slice_max(tgt_share_avg, n = 1, with_ties = FALSE) %>%
    ungroup()

  pass_plays <- pbp %>% filter(pass_attempt == 1 | sack == 1)

  team_qb <- pass_plays %>% ungroup() %>%
    filter(season %in% seasons) %>%
    group_by(posteam, season) %>%
    summarise(
      att    = sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE),
      comp_n = sum(complete_pass, na.rm = TRUE),
      yds    = sum(passing_yards, na.rm = TRUE),
      td_n   = sum(pass_touchdown, na.rm = TRUE),
      int_n  = sum(interception, na.rm = TRUE),
      xtds   = sum(dplyr::coalesce(pbp_predicted_after_run_xtd,
                                   pbp_predicted_after_pass_xtd,
                                   pbp_predicted_after_scramble_xtd), na.rm = TRUE),
      .groups = "drop") %>%
    filter(att > 0) %>%
    mutate(
      ypa  = yds / att,
      comp = comp_n / att,
      # standard NFL passer rating, clamped components
      a_ = pmin(pmax((comp - 0.3) * 5,       0), 2.375),
      b_ = pmin(pmax((yds / att - 3) * 0.25, 0), 2.375),
      c_ = pmin(pmax(td_n / att * 20,        0), 2.375),
      d_ = pmin(pmax(2.375 - int_n / att * 25, 0), 2.375),
      rating = (a_ + b_ + c_ + d_) / 6 * 100
    ) %>% select(-a_, -b_, -c_, -d_)

  qb_base <- pass_plays %>% ungroup() %>%
    filter(season %in% seasons, !is.na(qbgrp_ssn)) %>%
    mutate(qb_base = sub("-[0-9]{4}$", "", qbgrp_ssn)) %>%
    count(posteam, season, qb_base) %>%
    group_by(posteam, season) %>% slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>% select(posteam, season, qb_base)

  swaps <- wr1 %>%
    rename(team = abbreviation, wr1_name = player) %>%
    inner_join(team_qb, by = c("team" = "posteam", "season" = "season")) %>%
    inner_join(qb_base, by = c("team" = "posteam", "season" = "season")) %>%
    group_by(team) %>% arrange(season, .by_group = TRUE) %>%
    mutate(prev_season  = lag(season),  prev_wr1  = lag(wr1_name),
           prev_quality = lag(wr1_quality), prev_qb = lag(qb_base),
           prev_ypa = lag(ypa), prev_comp = lag(comp),
           prev_rating = lag(rating), prev_xtds = lag(xtds)) %>%
    ungroup() %>%
    filter(season == prev_season + 1, wr1_name != prev_wr1)

  n_dropped_qb <- 0
  if (same_qb_only) {
    n_dropped_qb <- sum(swaps$qb_base != swaps$prev_qb)
    swaps <- swaps %>% filter(qb_base == prev_qb)
  }

  swaps <- swaps %>%
    mutate(d_quality = wr1_quality - prev_quality,
           d_ypa = ypa - prev_ypa, d_comp = comp - prev_comp,
           d_rating = rating - prev_rating, d_xtds = xtds - prev_xtds) %>%
    select(team, season, qb_base, prev_wr1, wr1_name, prev_quality, wr1_quality,
           mz_lean, d_quality, d_ypa, d_comp, d_rating, d_xtds, att) %>%
    arrange(desc(d_quality))

  cat("-- ", nrow(swaps), " WR1 changes, ", min(seasons), "-", max(seasons),
      " (WR1 = max season tgt_share among WRs with >= ", min_games, " games",
      if (same_qb_only) paste0("; ", n_dropped_qb, " dropped: starting QB changed") else "",
      ") --\n", sep = "")
  print(as.data.frame(swaps), row.names = FALSE)

  buckets <- swaps %>%
    mutate(d_q_bucket = cut(d_quality, breaks = c(-Inf, -10, 10, Inf),
                            labels = c("WR1 clearly worse", "similar", "WR1 clearly better"))) %>%
    group_by(d_q_bucket) %>%
    summarise(n = n(),
              avg_d_ypa    = mean(d_ypa,    na.rm = TRUE),
              avg_d_comp   = mean(d_comp,   na.rm = TRUE),
              avg_d_rating = mean(d_rating, na.rm = TRUE),
              avg_d_xtds   = mean(d_xtds,   na.rm = TRUE), .groups = "drop")
  cat("\n-- average pooled passing shift by WR1-quality delta --\n")
  print(as.data.frame(buckets), row.names = FALSE)
  cat("\nREAD IT YOURSELF: raw YoY shifts even with the same QB — scheme, the rest\n",
      "of the corps, and luck all moved too. The bucket row is a starting prior,\n",
      "not a price. mz_lean is man-vs-zone tendency, not quality.\n", sep = "")
  invisible(list(detail = swaps, buckets = buckets))
}

#### ---------------- v3 (2026-09-15, Claude on Andy's authority) ----------------
#### - lab_wr1_swap RETIRED: it read combined_pbp and priced one "WR1".
####   Use wf_swap(team, "receiving", out, inn) in waterfall_bridge.R instead:
####   corps of 8, within band, turned into YPA / comp % / rating.
#### - every team-code spelling now works in lab_team / lab_swap (BAL, BLT, CLE, ...).
#### - lab_swap prints a NOTE when the incoming player is priced at a different
####   band or slot than the seat he takes.
#### - frames that name players full_name / player (not roster_name) now work.
#### - Outcome change for a swap: wf_swap(...) in waterfall_bridge.R.
lab_wr1_swap <- function(...) {
  stop("lab_wr1_swap() is retired (v3, 2026-09-15): it read combined_pbp and priced one 'WR1'.\n",
       "  Use instead: wf_swap(team, \"receiving\", out, inn) in waterfall_bridge.R.", call. = FALSE)
}
