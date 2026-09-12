# pct_adjuster_engine.R
# PROPOSED -- Claude, UNSIGNED.
#
# Per stat family, per metric: median of the RIGHT rank column per entity
# (offense uses <metric>_rank_def, defense uses <metric>_rank), shift by
# that metric's own modifier, then carry each side's adjusted percentile
# into the OTHER side's game set and read the raw metric at that
# percentile (quantile type 4: position n*p, linear between the
# bracketing sorted values). Ends with mean / median / middle-4 across
# the twelve carried values of each metric.
#
# Session needs: qb_stats_df_final, STATS_QBS, STATS_DEFS, dplyr.

# metric column -> modifier, in percentile points (0.058 = 5.8 points).
# NA = not set yet; pct_carry refuses to run that family until it is.
carry_spec <- list(
  xtds  = c(pbp_xtds = 0, part_xtds = 0),
  plays = c(plays = .095),
  xpass = c(pass_rate = .046, fastr_xpass_rate = .023,
            pbp_xpass_rate = .034, part_xpass_rate = .034),
  scr_rate = c(scr_rate = -.055),
  scr_ypc  = c(scr_ypc = -.053, pbp_scr_xypc = -.002, part_scr_xypc = -.002),
  cp    = c(acc_rate = -.044, fastr_cp = -.002, pbp_cp = -.013, part_cp = -.034),
  ypa   = c(ypa = -.013, pbp_xypa = -.028, part_xypa = -.034),
  sack  = c(sack_rate = -.028, pbp_sack_rate = -.039, part_sack_rate = .006),
  twp   = c(twp_rate = .005, int_rate = .005),
  xtd_prop = c(pbp_pass_prop = -.017, part_pass_prop = -.017,
               pbp_qb_scramble_prop = .009, part_qb_scramble_prop = .009,
               pbp_run_prop = .008, part_run_prop = .008)
)

# family -> data frame it reads. Anything not listed reads qb_stats_df_final.
carry_data <- list(xtd_prop = "xtd_proportion")

# metric -> rank column stem, where it isn't just the metric name.
# the run leg ranks on run_xtd, not run_prop.
carry_rank <- list(pbp_run_prop = "pbp_run_xtd", part_run_prop = "part_run_xtd")


pct_q <- function(x, p)   # a modifier can push a high median past 1: clamp to the edge
  unname(stats::quantile(x, probs = pmin(pmax(p, 0), 1), type = 4, na.rm = TRUE))

pct_carry <- function(stat, qbs = STATS_QBS, defs = STATS_DEFS, digits = 3) {
  adj <- carry_spec[[stat]]
  if (is.null(adj)) stop("no carry spec for '", stat, "' -- add it to carry_spec")
  if (anyNA(adj))   stop("modifiers for '", stat, "' not set: ",
                         paste(names(adj)[is.na(adj)], collapse = ", "))
  mets <- names(adj)
  df <- if (!is.null(carry_data[[stat]])) get(carry_data[[stat]]) else qb_stats_df_final
  
  rank_stem <- function(m) if (!is.null(carry_rank[[m]])) carry_rank[[m]] else m
  rank_col  <- function(m, side) paste0(rank_stem(m), if (side == "QB") "_rank_def" else "_rank")
  
  miss <- setdiff(c(mets, vapply(mets, rank_col, "", side = "QB"),
                    vapply(mets, rank_col, "", side = "DEF")), names(df))
  if (length(miss)) stop("data frame is missing: ", paste(miss, collapse = ", "))
  
  games <- function(e, side) {
    if (side == "QB") df[df$qbgrp_ssn == e, ] else df[df$def_ssn == e, ]
  }
  
  # 1. medians of the right rank column, each shifted by its own modifier
  med <- dplyr::bind_rows(lapply(c(qbs, defs), function(e) {
    side <- if (e %in% qbs) "QB" else "DEF"
    g <- games(e, side)
    row <- data.frame(entity = e, side = side, games = nrow(g))
    for (m in mets) {
      row[[paste0("med_", m)]]         <- stats::median(g[[rank_col(m, side)]], na.rm = TRUE)
      row[[paste0("med_", m, "_adj")]] <- row[[paste0("med_", m)]] + adj[[m]]
    }
    row
  }))
  pct <- function(e, m) med[med$entity == e, paste0("med_", m, "_adj")]
  
  # 2. carry: from-entity's percentile read inside of-entity's games
  carry <- function(from, of, of_side) {
    g <- games(of, of_side)
    row <- data.frame(pct_from = from, games_of = of, n = nrow(g))
    for (m in mets) {
      row[[paste0("pct_", m)]] <- pct(from, m)
      row[[m]] <- round(pct_q(g[[m]], pct(from, m)), digits)
    }
    row
  }
  off_on_def <- dplyr::bind_rows(lapply(qbs,  function(q) lapply(defs, function(d) carry(q, d, "DEF"))))
  def_on_off <- dplyr::bind_rows(lapply(defs, function(d) lapply(qbs,  function(q) carry(d, q, "QB"))))
  
  # 3. the twelve carried values per metric, both directions together
  summ <- dplyr::bind_rows(lapply(mets, function(m) {
    v <- c(off_on_def[[m]], def_on_off[[m]]); s <- sort(v); lo <- length(s) / 2 - 1
    data.frame(metric = m, n = length(v), mean = mean(v), median = stats::median(v),
               mid4_mean = mean(s[lo:(lo + 3)]))
  }))
  
  cat("\n== ", stat, "  modifiers: ",
      paste(sprintf("%s %+.3f", mets, adj), collapse = "  "), " ==\n", sep = "")
  print(med, digits = digits, row.names = FALSE)
  cat("\n-- offense percentile -> defense's games --\n"); print(off_on_def, digits = digits + 2, row.names = FALSE)
  cat("\n-- defense percentile -> QB's games --\n");      print(def_on_off, digits = digits + 2, row.names = FALSE)
  cat("\n-- twelve carried values, both directions --\n"); print(summ, digits = digits + 2, row.names = FALSE)
  invisible(list(medians = med, off_on_def = off_on_def, def_on_off = def_on_off, summary = summ))
}

# usage:
#   pct_carry("xpass")
#   r <- pct_carry("plays"); r$summary
# set a family's modifiers in carry_spec, then pct_carry("<family>")

pct_carry("xtd_prop")
