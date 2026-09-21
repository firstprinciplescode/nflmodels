# ============================================================================
# cmp26_peek -- look things up in cache/cmp26.rds without typing dplyr chains
# ============================================================================
# source("pff_stats/evaluation/cmp26_peek.R")
#
#   cmp_team("LA")                     offense focus metrics, all 2025 baseline
#   cmp_team("NYJ", baseline = "each")  one block per 2025 QB group
#   cmp_team("KC", side = "defense")
#   cmp_team("LA", focus_only = FALSE) every metric
#   cmp_metric("scr_rate")             all 32 teams on one metric
#   cmp_metric("blitz_rate", side = "defense")
#   cmp_opp()                          rows with a 2025 game vs this week's opponent
#   cmp_place("above 2025 max")        everything that broke its own range
#
# Columns: v26 = 2026 wk1. med25/min25/max25/q25_25/q75_25 = that team's 2025
# shape. delta = v26 - med25. delta_sd = delta in that baseline's own SDs.
# pctl25 = share of its 2025 games at or below v26. place = where it landed.
# n_more_extreme = how many of its own 2025 games were further from the median.
# ============================================================================

suppressMessages(library(dplyr))

if (!exists("cmp26")) {
  if (file.exists("cache/cmp26.rds")) {
    cmp26 <- readRDS("cache/cmp26.rds")
    cat("loaded cmp26:", nrow(cmp26), "rows\n")
  } else {
    stop("cache/cmp26.rds not found -- run pff_stats/evaluation/compare_2026_vs_2025.R first")
  }
}

.cmp_cols <- c("metric", "v26", "med25", "delta", "delta_sd", "pctl25", "place",
               "min25", "max25", "n_more_extreme", "n25")

cmp_team <- function(team_code, side = "offense", focus_only = TRUE,
                     baseline = "ALL", digits = 3) {
  d <- cmp26 %>% filter(team == team_code, side == !!side)
  if (focus_only) d <- d %>% filter(focus)
  if (identical(baseline, "each")) {
    for (b in unique(d$baseline)) {
      blk <- d %>% filter(baseline == b)
      cat("\n--", team_code, side, "vs", b,
          "(", blk$baseline_qb[1], ",", blk$n25[1], "games )\n")
      print(as.data.frame(blk[, .cmp_cols]), row.names = FALSE, digits = digits)
    }
    return(invisible(d))
  }
  d <- d %>% filter(baseline == !!baseline)
  cat("\n--", team_code, side, "vs", baseline, " opponent this week:", d$opp26[1], "\n")
  print(as.data.frame(d[, .cmp_cols]), row.names = FALSE, digits = digits)
  invisible(d)
}

cmp_metric <- function(metric_name, side = "offense", baseline = "ALL",
                       sort_by = "delta_sd", digits = 3) {
  d <- cmp26 %>%
    filter(metric == metric_name, side == !!side, baseline == !!baseline) %>%
    arrange(desc(abs(.data[[sort_by]])))
  print(as.data.frame(d[, c("team", "opp26", "v26", "med25", "delta", "delta_sd",
                            "pctl25", "place", "min25", "max25", "n25")]),
        row.names = FALSE, digits = digits)
  invisible(d)
}

cmp_opp <- function(focus_only = TRUE, side = NULL, digits = 3) {
  d <- cmp26 %>% filter(baseline == "ALL", n_opp25 > 0)
  if (focus_only) d <- d %>% filter(focus)
  if (!is.null(side)) d <- d %>% filter(side == !!side)
  d <- d %>% arrange(desc(abs(delta_vs_opp)))
  print(as.data.frame(d[, c("team", "side", "metric", "opp26", "v26", "v25_vs_opp",
                            "delta_vs_opp", "n_opp25", "med25", "delta")]),
        row.names = FALSE, digits = digits)
  invisible(d)
}

cmp_place <- function(which_place = "above 2025 max", focus_only = TRUE, digits = 3) {
  d <- cmp26 %>% filter(baseline == "ALL", place == which_place)
  if (focus_only) d <- d %>% filter(focus)
  d <- d %>% arrange(desc(abs(delta_sd)))
  print(as.data.frame(d[, c("team", "side", "metric", "v26", "med25", "min25",
                            "max25", "delta", "delta_sd")]),
        row.names = FALSE, digits = digits)
  invisible(d)
}

cat("cmp_team() cmp_metric() cmp_opp() cmp_place() ready\n")
