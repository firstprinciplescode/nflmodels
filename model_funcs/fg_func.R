# fg_func.R
# PROPOSED -- Claude, UNSIGNED.
#
# FG ATTEMPTS per team-game (fgs = sum(field_goal_attempt); attempts, not makes).
# Built from the 2026-09-18 bake-off (six methods, leak-proof harness, 2023-25 test,
# 2020-22 replication). What survived:
#   * FG attempts are ~99% unpredictable pre-game (best honest R2_oos 0.003-0.009).
#   * The number is: last season's LEAGUE level, nudged by a heavily shrunk entity
#     tendency on each side, optionally by weather. That is fg_project().
#   * xTD only matters as a LOW-END HINGE: flat ~2.0 for xTD >= 1.5, falling below.
#     lm(fgs ~ xTD) is worth nothing, even with the perfect same-game xTD.
#   * Filtering to one qbgrp_ssn / def_ssn and refitting is harmful (R2_oos -0.05
#     to -0.30). Entities enter ONLY through the shrink weight below.
#
# The shrink weight is not a guess: fg_weight() regresses an entity's FG ratio in
# season t on its pooled ratio over the previous K seasons, across every entity in
# qb_stats_df_final, and uses that slope. 2016-2025: offense (same QB-team)
# K=1 .11, K=2 .13, K=3 .17; defense (franchise) K=1 .05, K=3 .35.
#
# Session needs: qb_stats_df_final, dplyr.

.fg_env <- new.env()

.fg_base <- function() {
  q <- dplyr::ungroup(qb_stats_df_final)
  lg <- q %>% dplyr::group_by(season) %>% dplyr::summarise(lg = mean(fgs, na.rm = TRUE), .groups = "drop")
  q %>% dplyr::left_join(lg, by = "season") %>%
    dplyr::mutate(off_stem = sub("-[0-9]{4}$", "", qbgrp_ssn), def_stem = sub("[0-9]{4}$", "", def_ssn))
}

# per-season FG history for a stem: fg_history("BUFAllen") or fg_history("DET", side = "def")
fg_history <- function(stem, side = c("off", "def"), digits = 3) {
  side <- match.arg(side); q <- .fg_base()
  g <- if (side == "off") q[q$off_stem == stem, ] else q[q$def_stem == stem, ]
  if (!nrow(g)) stop("no games for '", stem, "' on side '", side, "'")
  out <- g %>% dplyr::group_by(season) %>%
    dplyr::summarise(games = dplyr::n(), fga = mean(fgs), league = dplyr::first(lg), ratio = mean(fgs) / dplyr::first(lg),
                     tds = mean(tds), xtds = mean(pbp_xtds), plays = mean(plays), .groups = "drop")
  cat("\n== FG attempts ", if (side == "off") "BY " else "ALLOWED BY ", stem, " ==  ratio = fga / that season's league fga\n", sep = "")
  print(as.data.frame(out), digits = digits, row.names = FALSE)
  invisible(out)
}

# the weight a pooled K-season history deserves, measured on every entity in the frame
fg_weight <- function(K, side = c("off", "def"), min_games = 10) {
  side <- match.arg(side); key <- paste(side, K, min_games)
  if (!is.null(.fg_env[[key]])) return(.fg_env[[key]])
  q <- .fg_base(); q$stem <- if (side == "off") q$off_stem else q$def_stem
  gm <- q %>% dplyr::group_by(stem, season) %>% dplyr::summarise(g = dplyr::n(), f = sum(fgs), l = sum(lg), .groups = "drop")
  o <- dplyr::bind_rows(lapply(seq_len(nrow(gm)), function(i) {
    x <- gm[i, ]; if (x$g < min_games) return(NULL)
    pr <- gm[gm$stem == x$stem & gm$season < x$season & gm$season >= x$season - K, ]
    if (nrow(pr) < K || sum(pr$g) < min_games * K) return(NULL)
    data.frame(y = x$f / x$l, p = sum(pr$f) / sum(pr$l))
  }))
  w <- if (nrow(o) < 30) NA_real_ else unname(stats::coef(stats::lm(y ~ p, o))[2])
  w <- min(max(w, 0), 1); attr(w, "n") <- nrow(o); .fg_env[[key]] <- w; w
}

# off / def: one or more entity keys. Several seasons of the same stem are pooled.
#   fg_project(paste0("BUFAllen-", 2022:2025), "DET2025")
#   fg_project("DETGoff-2025", c("BUF2025", "LAC2021"), xtd = 2.6, temp = 38, wind = 14)
#   fg_project(paste0("BUFAllen-", 2022:2025), "DET2025", dome = TRUE)          # indoors = GOOD weather, not "no weather"
# weather: leave temp/wind/dome unset ONLY if you do not know it (neutral = 62F, 5 mph). Dome x1.032; 38F + 14 mph x0.928.
fg_project <- function(off, def, xtd = NULL, temp = NULL, wind = NULL, dome = FALSE, lg = NULL, digits = 3) {
  q <- .fg_base()
  if (dome) { temp <- 70; wind <- 0 }                                            # how dome games are coded in qb_stats_df_final: 70F, 0 mph -> x1.032
  if (is.null(lg)) lg <- mean(q$fgs[q$season == max(q$season)], na.rm = TRUE)     # last completed season's league level
  side <- function(keys, col, sd) {
    g <- q[q[[col]] %in% keys, ]
    miss <- setdiff(keys, unique(q[[col]])); if (length(miss)) stop("not in qb_stats_df_final: ", paste(miss, collapse = ", "))
    K <- length(unique(g$season)); ratio <- sum(g$fgs) / sum(g$lg)              # season-normalised, so era drift does not leak in
    stems <- unique(if (sd == "off") g$off_stem else g$def_stem)
    Kw <- if (length(stems) == 1) min(K, 3) else 1                              # multi-season weight is only earned by the SAME entity; mixed comps get the 1-season weight
    w <- fg_weight(Kw, sd)
    data.frame(side = sd, entity = paste(keys, collapse = " + "), seasons = K, games = nrow(g), fga = mean(g$fgs),
               ratio = ratio, weight = as.numeric(w), adj = 1 + as.numeric(w) * (ratio - 1))
  }
  s <- rbind(side(off, "qbgrp_ssn", "off"), side(def, "def_ssn", "def"))
  wx <- if (is.null(temp) && is.null(wind)) 1 else exp(0.00194 * ((if (is.null(temp)) 62 else temp) - 62) - 0.00314 * ((if (is.null(wind)) 5 else wind) - 5))
  fg <- lg * prod(s$adj) * wx
  hinge <- if (is.null(xtd)) NA_real_ else 1.145 + 0.580 * min(xtd, 1.5)
  cat("\n== FG attempts: ", paste(off, collapse = " + "), "  vs  ", paste(def, collapse = " + "), " ==\n", sep = "")
  print(s, digits = digits, row.names = FALSE)
  cat(sprintf("\nleague level %.3f  x off %.3f  x def %.3f  x weather %.3f   =  FG ATTEMPTS %.2f\n", lg, s$adj[1], s$adj[2], wx, fg))
  if (!is.null(xtd)) cat(sprintf("xTD hinge cross-check (xTD %.2f): %.2f%s\n", xtd, hinge,
      if (xtd >= 1.5) "   -- xTD >= 1.5: flat zone, ignore xTD and keep the number above"
      else "   -- xTD < 1.5: broken-offense zone; sit BETWEEN the two (hinge was fit on realised xTD and over-reacts to a projection)"))
  cat("sanity: 10 seasons support 1.6-2.4; test-season quantiles 1.76 / 1.94 / 2.28. These are ATTEMPTS (league make rate ~0.87).\n")
  invisible(list(fg = fg, league = lg, sides = s, weather = wx, hinge = hinge))
}

# usage:
#   fg_history("BUFAllen"); fg_history("DET", side = "def")
#   fg_project(paste0("BUFAllen-", 2022:2025), "DET2025")
fg_project("JAXLawrence-2025", c("DEN2025"), xtd = 1.625)
#   fg_weight(3, "off"); fg_weight(3, "def")      # the shrink weights, measured
