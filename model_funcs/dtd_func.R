# dtd_func.R
# PROPOSED -- Claude, UNSIGNED.  NEW file (2026-09-20). Changes nothing that exists.
#
# NON-OFFENSIVE TOUCHDOWNS per team-game ("D/ST touchdowns"): interception returns, fumble returns,
# punt returns, kick returns, blocked punt / FG returns, special-teams fumble recoveries.
# Same idea as fg_func.R: a league level, nudged by what is MEASURED to matter, and nothing else.
# Built from the 2026-09-20 bake-off (six angles, each independently re-computed; forward-chaining
# test 2020-2025). Full reasoning: model_funcs/DTD_NOTES.md. What survived:
#   * It is two independent Poisson counts per team: a DEFENSIVE part (int + fumble returns, ~0.095 per
#     team-game over the last three seasons) and a SPECIAL-TEAMS part (~0.037). The two teams in a game are
#     independent of each other too. P(at least one) = 1 - exp(-expected).
#   * Interceptions are the biggest source (47%), not fumbles (27%); special teams is 25%. 56% of
#     fumble-return TDs are strip-sacks. 90% of defensive TDs start on a dropback.
#   * The conversion rates are league numbers, not team skills: 8.9% of interceptions (flat across all ten
#     seasons); since 2020 about 0.7% of sacks (strip-sack path) and 5% of other fumbles lost (both were higher
#     in 2016-19). No defense keeps a better return rate (p = .86). So the best input is YOUR OWN projected
#     interceptions and sacks: one interception is worth about 12 sacks.
#   * Spread: the underdog's offense gives up more (about 3% per point, defensive part only, right sign
#     in 10 of 10 seasons). It is ALL interception volume -- the return rate does not change with the score.
#   * Offense: a returning QB-team pair gives up ~0.86x, a QB group that took over mid-season ~1.3x.
#     An entity's own return-TD history is worth a weight of 0.10 AT MOST (the range includes 0).
#   * Special teams is a FLAT league number. Team / returner history, the 2025 punt-return spike, the new
#     kickoff rules, totals, weather, dome, home / away, week, playoffs: all measured, all nothing.
#   * Reading this stat off a comparable-games sheet HURTS (small pools; 0.07 and 0.22 are the same
#     number at 100 games). Entities enter only through the small shrink weight, like fg_project().
#
# Session needs: dtd_games (the new side frame) and dplyr installed. Load the frame ONCE per session:
#   dtd_games <- readRDS("cache/dtd_games.rds")      # built by data_build/dtd_games_build.R
# This file never loads it for you and nothing runs when it is sourced.

.dtd_env <- new.env()

.dtd_frame <- function() {
  if (!exists("dtd_games")) stop('dtd_games is not in the session. Run this first:\n   dtd_games <- readRDS("cache/dtd_games.rds")', call. = FALSE)
  g0 <- get("dtd_games"); if (!is.null(.dtd_env$src) && identical(.dtd_env$src, g0)) return(.dtd_env$frame)   # any change to dtd_games, even one cell, rebuilds every constant
  g <- as.data.frame(g0)
  g$off_stem <- sub("-[0-9]{4}$", "", g$qbgrp_ssn); g$def_stem <- sub("[0-9]{4}$", "", g$def_ssn)
  g$lg_def <- stats::ave(g$dtd_def, g$season); g$lg_st <- stats::ave(g$dtd_st, g$season)           # that season's league level
  g$other_fl <- g$fumbles_lost - g$sack_fumbles_lost; g$other_fum_ret <- g$fum_ret - g$fum_ret_sack
  # QB-team status at the start of that season: returning (the same QB-team stem played 10+ games the season
  # before), new (no such season, but the group opened the year), takeover (first appeared in week 3 or later)
  tb <- table(paste(g$off_stem, g$season)); n_prev <- stats::setNames(as.integer(tb), names(tb))        # plain named vectors: a missing name reads NA, not an error
  fw <- tapply(g$week, g$qbgrp_ssn, min); first_wk <- stats::setNames(as.integer(fw), names(fw))
  prev_g <- unname(n_prev[paste(g$off_stem, g$season - 1)]); prev_g[is.na(prev_g)] <- 0L
  g$qb_status <- ifelse(prev_g >= 10, "returning", ifelse(unname(first_wk[g$qbgrp_ssn]) <= 2, "new", "takeover"))
  .dtd_env$src <- g0; .dtd_env$frame <- g; .dtd_env$const <- NULL
  for (k in ls(.dtd_env)) if (startsWith(k, "w ")) rm(list = k, envir = .dtd_env)
  g
}

# every constant the projection uses, measured on the frame (nothing typed in by hand)
.dtd_const <- function() {
  g <- .dtd_frame(); if (!is.null(.dtd_env$const)) return(.dtd_env$const)
  full <- as.integer(names(which(table(g$season) >= 500))); if (!length(full)) full <- sort(unique(g$season))        # full seasons only: a few weeks of a new season are not a season
  rec <- g$season %in% utils::tail(full, 3)                                   # the last three seasons: the level stepped down after 2019 and has been flat since
  era <- g$season %in% utils::tail(full, 6)                                   # fumble-return conversions stepped down after 2019 (strip-sack TDs per sack 1.28% -> 0.73%, p = .002): use the last six seasons
  first <- g$season > min(g$season)                                      # the first season has no "season before" to read a status from
  sp <- stats::glm(dtd_def ~ fav_pts, family = stats::poisson(), data = g[!is.na(g$fav_pts), ])
  b <- unname(stats::coef(sp)[2])
  ten <- tapply(g$dtd_def[first], g$qb_status[first], sum) / tapply(g$lg_def[first], g$qb_status[first], sum)
  won <- g$pts > g$opp_pts; wp <- stats::glm(won ~ fav_pts, family = stats::binomial(), data = data.frame(won = won, fav_pts = g$fav_pts))
  c0 <- list(seasons = range(g$season), recent = range(g$season[rec]),
             L_def = mean(g$dtd_def[rec]), L_st = mean(g$dtd_st), L_def_all = mean(g$dtd_def),
             p_int = sum(g$int_ret) / sum(g$ints),                                          # P(TD | interception) does not move by season (p = .73): all seasons
             p_sack = sum(g$fum_ret_sack[era]) / sum(g$sacks_taken[era]), p_other = sum(g$other_fum_ret[era]) / sum(g$other_fl[era]),
             other_fl = mean(g$other_fl[rec]), era = range(g$season[era]),
             lg_ints = mean(g$ints[rec]), lg_sacks = mean(g$sacks_taken[rec]),
             b = b, b_se = unname(sqrt(diag(stats::vcov(sp)))[2]), tenure = ten,
             kneel_lose = mean(g$kneels[!won]), kneel_win = mean(g$kneels[won]), kneel_yds = sum(g$kneel_yards) / sum(g$kneels), wp = stats::coef(wp))
  c0$other_td <- c0$p_other * c0$other_fl                                # other-fumble return TDs per team-game
  fv <- g$fav_pts[!is.na(g$fav_pts)]; c0$norm <- mean(exp(b * fv)); c0$norm23 <- mean(exp(b * 2 / 3 * fv))      # keeps the league average the league average
  .dtd_env$const <- c0; c0
}

# the numbers the projection is made of
dtd_rates <- function(digits = 4) {
  k <- .dtd_const()
  cat("\n== D/ST touchdowns: what the projection is made of (frame ", k$seasons[1], "-", k$seasons[2], ") ==\n", sep = "")
  cat(sprintf("league level per team-game:  defensive %.4f (%d-%d; the ten-season %.4f runs hot)   special teams %.4f (all seasons, flat)   total %.4f\n",
              k$L_def, k$recent[1], k$recent[2], k$L_def_all, k$L_st, k$L_def + k$L_st))
  cat(sprintf("chain:  %.4f per interception (all seasons)  +  %.5f per sack (strip-sack path, %d-%d)  +  %.4f flat for other fumbles (%.3f lost per game x %.4f returned, %d-%d)\n",
              k$p_int, k$p_sack, k$era[1], k$era[2], k$other_td, k$other_fl, k$p_other, k$era[1], k$era[2]))
  cat(sprintf("        one interception = %.1f sacks.   At league inputs (%d-%d: %.3f interceptions, %.2f sacks per team-game) the chain gives %.4f vs the level %.4f\n",
              k$p_int / k$p_sack, k$recent[1], k$recent[2], k$lg_ints, k$lg_sacks, k$p_int * k$lg_ints + k$p_sack * k$lg_sacks + k$other_td, k$L_def))
  cat(sprintf("spread (defensive part only): exp(%.4f x points the OFFENSE is favoured by) / %.4f, slope SE %.4f  ->  7-point underdog x%.2f, 7-point favourite x%.2f\n",
              k$b, k$norm, k$b_se, exp(-7 * k$b) / k$norm, exp(7 * k$b) / k$norm))
  cat(sprintf("        with your interceptions / sacks given: two thirds of that slope (they already carry team quality)  ->  x%.2f / x%.2f\n",
              exp(-7 * k$b * 2 / 3) / k$norm23, exp(7 * k$b * 2 / 3) / k$norm23))
  cat("QB-team status (defensive part only):  ", paste(sprintf("%s x%.2f", names(k$tenure), k$tenure), collapse = "   "), "\n", sep = "")
  cat(sprintf("own return-TD history: weight min(measured, 0.10).  measured K=1: offense %.3f, defense %.3f\n", dtd_weight(1, "off", cap = FALSE), dtd_weight(1, "def", cap = FALSE)))
  cat(sprintf("kneel-downs: %.2f if the team loses, %.2f if it wins, %.2f yards each\n", k$kneel_lose, k$kneel_win, k$kneel_yds))
  invisible(k)
}

# per-season history for a stem: dtd_history("CLVWatson") = what that OFFENSE gave up; dtd_history("DEN", side = "def") = what that TEAM scored
dtd_history <- function(stem, side = c("off", "def"), digits = 3) {
  side <- match.arg(side); g <- .dtd_frame()
  x <- if (side == "off") g[g$off_stem == stem, ] else g[g$def_stem == stem, ]
  if (!nrow(x)) stop("no games for '", stem, "' on side '", side, "'", if (side == "off") " -- offense stems look like DENNix, defense stems like DEN")
  out <- do.call(rbind, lapply(split(x, x$season), function(d) data.frame(
    season = d$season[1], games = nrow(d), dtd = sum(d$dtd), int_ret = sum(d$int_ret), fum_ret = sum(d$fum_ret), strip_sack = sum(d$fum_ret_sack), st = sum(d$dtd_st),
    def_per_g = mean(d$dtd_def), league = d$lg_def[1], ratio = sum(d$dtd_def) / sum(d$lg_def), ints_g = mean(d$ints), sacks_g = mean(d$sacks_taken), fum_lost_g = mean(d$fumbles_lost))))
  cat("\n== D/ST touchdowns ", if (side == "off") "GIVEN UP BY the offense / team of " else "SCORED BY the defense + special teams of ", stem,
      " ==  ratio = defensive part (int + fumble returns) / that season's league level; ints, sacks, fumbles are the OFFENSE's in those games\n", sep = "")
  print(out, digits = digits, row.names = FALSE); invisible(out)
}

# the weight a pooled K-season return-TD history deserves, measured on every entity (same method as fg_weight).
# The measured slope is ~0.1 with a range that includes 0, so dtd_project() never uses more than 0.10.
dtd_weight <- function(K, side = c("off", "def"), min_games = 10, cap = TRUE) {
  side <- match.arg(side); g <- .dtd_frame(); key <- paste("w", side, K, min_games)
  if (is.null(.dtd_env[[key]])) {
    d0 <- data.frame(stem = if (side == "off") g$off_stem else g$def_stem, season = g$season, n = 1, y = g$dtd_def, l = g$lg_def)
    gm <- stats::aggregate(cbind(n, y, l) ~ stem + season, data = d0, FUN = sum)
    o <- do.call(rbind, lapply(seq_len(nrow(gm)), function(i) {
      x <- gm[i, ]; if (x$n < min_games) return(NULL)
      pr <- gm[gm$stem == x$stem & gm$season < x$season & gm$season >= x$season - K, ]
      if (nrow(pr) < K || sum(pr$n) < min_games * K) return(NULL)
      data.frame(y = x$y / x$l, p = sum(pr$y) / sum(pr$l))
    }))
    w <- if (is.null(o) || nrow(o) < 30) NA_real_ else unname(stats::coef(stats::lm(y ~ p, o))[2])
    w <- min(max(w, 0), 1); attr(w, "n") <- if (is.null(o)) 0L else nrow(o); .dtd_env[[key]] <- w
  }
  w <- as.numeric(.dtd_env[[key]]); if (cap) min(if (is.na(w)) 0 else w, 0.10) else w              # too few entities to measure it (a short frame): the history gets no weight
}

# TDs the DEFENSE + SPECIAL TEAMS of `def` are expected to score against `off`.
#   off  the offense that could GIVE IT UP (qbgrp_ssn key or keys, e.g. "JAXLawrence-2025")
#   def  the team that would SCORE it (def_ssn key or keys, e.g. "DEN2025")
#   ints / sacks   YOUR projected interceptions thrown and sacks taken by `off` in this game. Give them and the
#                  defensive part is built from them (the chain) -- both sides' tendencies are already inside your
#                  numbers, so the history and status adjustments are switched off.
#   fav  points `off`'s team is favoured by (underdog = negative). Leave NULL if you do not want the spread in.
#   qb_status  "auto" (read from the frame: returning if that QB-team stem played 10+ games in its last season),
#              or say it: "returning", "new", "takeover" (a QB who took the job over mid-season)
#   pressure   optional, level mode only: the defense's projected pressure rate / league pressure rate (1.10 = 10% more)
#     dtd_project("JAXLawrence-2025", "DEN2025", ints = 0.70, sacks = 2.55, fav = -3.5)
#     dtd_project("CLVSanders-2025", "PIT2025", fav = -6, qb_status = "takeover")
dtd_project <- function(off, def, ints = NULL, sacks = NULL, fav = NULL, qb_status = c("auto", "returning", "new", "takeover"),
                        pressure = NULL, digits = 3, quiet = FALSE) {
  qb_status <- match.arg(qb_status); g <- .dtd_frame(); k <- .dtd_const()
  if (!length(off) || !length(def)) stop("give at least one off key (like JAXLawrence-2025) and one def key (like DEN2025)")
  if (!is.null(fav) && (length(fav) != 1 || is.na(fav))) stop("fav must be one number (points the offense is favoured by), or leave it out")
  for (v in list(ints, sacks, pressure)) if (!is.null(v) && (length(v) != 1 || is.na(v) || v < 0)) stop("ints, sacks and pressure must each be one number that is not negative")
  miss <- c(setdiff(off, g$qbgrp_ssn), setdiff(def, g$def_ssn)); if (length(miss)) stop("not in dtd_games: ", paste(miss, collapse = ", "))
  chain <- !is.null(ints) || !is.null(sacks)
  so <- g[g$qbgrp_ssn %in% off, ]; sd <- g[g$def_ssn %in% def, ]
  hist <- data.frame(side = c("off (gave up)", "def (scored)"), entity = c(paste(off, collapse = " + "), paste(def, collapse = " + ")),
                     games = c(nrow(so), nrow(sd)), def_tds = c(sum(so$dtd_def), sum(sd$dtd_def)), st_tds = c(sum(so$dtd_st), sum(sd$dtd_st)),
                     ratio = c(sum(so$dtd_def) / sum(so$lg_def), sum(sd$dtd_def) / sum(sd$lg_def)),
                     ints_g = c(mean(so$ints), mean(sd$ints)), sacks_g = c(mean(so$sacks_taken), mean(sd$sacks_taken)))
  if (chain) {
    note <- character(0)
    if (is.null(ints))  { ints  <- k$lg_ints;  note <- c(note, sprintf("no interceptions given -> league %.3f", ints)) }
    if (is.null(sacks)) { sacks <- k$lg_sacks; note <- c(note, sprintf("no sacks given -> league %.2f", sacks)) }
    parts <- c(pick_six = ints * k$p_int, strip_sack = sacks * k$p_sack, other_fumble = k$other_td)
    b_use <- k$b * 2 / 3                        # your interception / sack numbers already carry team quality; what is left of the spread is game script
    base <- sum(parts); adj <- c(off = 1, def = 1); note <- c(note, "spread at two thirds of the slope")
    if (!is.null(pressure)) note <- c(note, "pressure is ignored here: your sack number already carries it")
    if (qb_status != "auto") note <- c(note, "qb_status is ignored here: your interception number already carries it")
  } else {
    stems <- unique(so$off_stem); last <- so[so$season == max(so$season), ]
    st <- if (qb_status != "auto") qb_status else if (length(stems) == 1 && (nrow(last) >= 10 || all(last$qb_status == "returning"))) "returning" else "new"
    ten <- unname(k$tenure[st]); if (is.na(ten)) ten <- 1; if (!is.null(fav)) ten <- 1 + (ten - 1) * 0.65                      # the spread already carries ~35% of the status step
    Ko <- if (length(stems) == 1) min(length(unique(so$season)), 3) else 1
    Kd <- if (length(unique(sd$def_stem)) == 1) min(length(unique(sd$season)), 3) else 1
    w_o <- if (nrow(so) >= 10 * Ko) dtd_weight(Ko, "off") else 0                                  # the weight was measured on 10+-game seasons only: under that, a history is worth nothing
    w_d <- if (nrow(sd) >= 10 * Kd) dtd_weight(Kd, "def") else 0                                  # (1 TD in 1 game reads 11x league; it must not move the number)
    a_off <- min(max(ten + w_o * (hist$ratio[1] - ten), 0.75), 1.35)                              # centred on the status level, not on 1
    a_def <- 1 + w_d * (hist$ratio[2] - 1)
    if (!is.null(pressure)) a_def <- a_def * (1 + 0.75 * (pressure - 1))                           # measured slope 1.0 [0.4, 1.6]; carried at 0.75
    a_def <- min(max(a_def, 0.8), 1.3)
    adj <- c(off = a_off, def = a_def); base <- k$L_def; b_use <- k$b; parts <- NULL
    note <- c(sprintf("QB-team status: %s", st), if (w_o == 0) "offense history under 10 games a season: not used", if (w_d == 0) "defense history under 10 games a season: not used")
  }
  g_fav <- g$fav_pts[!is.na(g$fav_pts)]
  sprd <- if (is.null(fav)) 1 else exp(b_use * fav) / mean(exp(b_use * g_fav))                     # normalised so the league average stays the league average
  mu_def <- base * prod(adj) * sprd; mu_st <- k$L_st; mu <- mu_def + mu_st
  out <- list(expected = mu, defensive = mu_def, special_teams = mu_st, p_any = 1 - exp(-mu), points = mu * 6.95, mode = if (chain) "chain" else "level",
              parts = parts, adj = adj, spread = sprd, history = hist)
  if (!quiet) {
    cat("\n== D/ST touchdowns scored by ", paste(def, collapse = " + "), "  against  ", paste(off, collapse = " + "), " ==\n", sep = "")
    print(hist, digits = digits, row.names = FALSE)
    if (chain) cat(sprintf("\nCHAIN on your numbers: %.3f interceptions x %.4f = %.4f   +   %.2f sacks x %.5f = %.4f   +   other fumbles %.4f   =  %.4f\n",
                           ints, k$p_int, parts[1], sacks, k$p_sack, parts[2], parts[3], base))
    else cat(sprintf("\nLEVEL: league defensive level %.4f  x offense %.3f  x defense %.3f   (history above is shown for reference; it moves the number by its small weight only)\n", base, adj[1], adj[2]))
    if (length(note)) cat("  note: ", paste(note, collapse = "; "), "\n", sep = "")
    cat(sprintf("x spread %.3f%s   ->  defensive part %.4f   +  special teams %.4f (flat)   =  EXPECTED D/ST TDs %.3f\n", sprd,
                if (is.null(fav)) " (no spread given)" else if (fav < 0) sprintf(" (the offense is a %.1f-point underdog)", -fav) else sprintf(" (the offense is favoured by %.1f)", fav), mu_def, mu_st, mu))
    cat(sprintf("chance of at least one: %.1f%%     worth %.2f points\n", 100 * out$p_any, out$points))
    cat("sanity: league is about 0.13 per team-game (13% of team-games have one). Anything outside 0.08-0.22 needs a very good reason.\n")
  }
  invisible(out)
}

# both directions of one game. Each side is a list: off, def, and optionally ints, sacks, qb_status, pressure.
#   dtd_game(home = list(off = "DENNix-2025", def = "DEN2025", ints = 0.42, sacks = 1.73),
#            away = list(off = "JAXLawrence-2025", def = "JAX2025", ints = 0.70, sacks = 2.55), home_fav = 3.5)
dtd_game <- function(home, away, home_fav = NULL, digits = 3) {
  for (s in list(home, away)) {
    bad <- setdiff(names(s), c("off", "def", "ints", "sacks", "qb_status", "pressure"))
    if (!is.list(s) || length(bad) || is.null(s$off) || is.null(s$def))
      stop("each side is list(off = , def = ) plus optionally ints, sacks, qb_status, pressure", if (length(bad)) paste0(" -- not understood: ", paste(bad, collapse = ", ")))
  }
  one <- function(scorer, victim, fav) do.call(dtd_project, c(list(off = victim$off, def = scorer$def, ints = victim$ints, sacks = victim$sacks, fav = fav, quiet = TRUE),
                                                              if (!is.null(victim$qb_status)) list(qb_status = victim$qb_status), if (!is.null(scorer$pressure)) list(pressure = scorer$pressure)))
  h <- one(home, away, if (is.null(home_fav)) NULL else -home_fav); a <- one(away, home, home_fav)
  tab <- data.frame(scored_by = c(paste(home$def, collapse = " + "), paste(away$def, collapse = " + ")), against = c(paste(away$off, collapse = " + "), paste(home$off, collapse = " + ")),
                    mode = c(h$mode, a$mode), defensive = c(h$defensive, a$defensive), special_teams = c(h$special_teams, a$special_teams),
                    expected = c(h$expected, a$expected), p_any = c(h$p_any, a$p_any), points = c(h$points, a$points))
  tot <- h$expected + a$expected
  cat("\n== D/ST touchdowns, both sides ==\n"); print(tab, digits = digits, row.names = FALSE)
  cat(sprintf("\nGAME: %.3f expected   chance of at least one %.1f%%   worth %.2f points   (the two sides are independent: measured r = 0.00)\n", tot, 100 * (1 - exp(-tot)), tot * 6.95))
  invisible(list(home = h, away = a, table = tab, game = tot, p_any = 1 - exp(-tot)))
}

# kneel-downs are not in combined_pbp (rightly: they teach a model nothing) but they ARE official rush attempts.
# They follow the result, so they are projected from the chance of winning:  kneel_project(p_win = 0.65)  or  kneel_project(fav = 3.5)
kneel_project <- function(p_win = NULL, fav = NULL) {
  k <- .dtd_const(); if (is.null(p_win) && is.null(fav)) stop("give p_win (chance this team wins) or fav (points it is favoured by)")
  if (is.null(p_win)) { if (length(fav) != 1 || is.na(fav)) stop("fav must be one number"); p_win <- unname(stats::plogis(k$wp[1] + k$wp[2] * fav)) }
  if (length(p_win) != 1 || is.na(p_win) || p_win < 0 || p_win > 1) stop("p_win is a chance between 0 and 1 (0.65, not 65)")
  n <- k$kneel_lose + (k$kneel_win - k$kneel_lose) * p_win
  cat(sprintf("win chance %.0f%%  ->  %.2f kneel-downs, %.1f yards. Add them to the QB's OFFICIAL rushing line (attempts and yards); never to anything trained on combined_pbp.\n", 100 * p_win, n, n * k$kneel_yds))
  invisible(c(kneels = n, yards = n * k$kneel_yds, p_win = p_win))
}

# usage (nothing runs on source):
#   dtd_games <- readRDS("cache/dtd_games.rds")
#   dtd_rates()
#   dtd_history("JAXLawrence"); dtd_history("DEN", side = "def")
#   dtd_project("JAXLawrence-2025", "DEN2025", ints = 0.70, sacks = 2.55, fav = -3.5)     # your sheet's numbers -> the chain
#   dtd_project("JAXLawrence-2025", "DEN2025", fav = -3.5)                                # no projections -> league level x small adjustments
#   dtd_game(home = list(off = "DENNix-2025", def = "DEN2025", ints = 0.42, sacks = 1.73),
#            away = list(off = "JAXLawrence-2025", def = "JAX2025", ints = 0.70, sacks = 2.55), home_fav = 3.5)
#   kneel_project(fav = 3.5)
