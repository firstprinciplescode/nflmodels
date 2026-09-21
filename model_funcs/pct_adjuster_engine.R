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

# metric -> modifier(s), in percentile points (0.058 = 5.8 points). NA = not set yet;
# pct_carry refuses to run a family until every slot in it is filled. Three ways to write one:
#   one number        -> shifts every matchup the same
#   ONE PER MATCHUP   -> length(STATS_QBS) x length(STATS_DEFS) numbers, QB first, then defense.
#                        With STATS_QBS = c("DENNix-2025", "MIATagovailoa-2023") and
#                        STATS_DEFS = c("JAX2025", "BUF2025") the four slots are, in order:
#                          1 DENNix-2025 vs JAX2025          2 DENNix-2025 vs BUF2025
#                          3 MIATagovailoa-2023 vs JAX2025   4 MIATagovailoa-2023 vs BUF2025
#                        (or name them, any order: c("DENNix-2025 vs JAX2025" = .03, ...))
#   one per defense   -> length(STATS_DEFS) numbers in STATS_DEFS order, or named by defense
# A matchup's modifier is used in both carry directions -- the QB's percentile read inside that
# defense's games, and that defense's percentile read inside the QB's games.
# Vectors need list(), not c(): c() flattens them into pbp_xtds1, pbp_xtds2 (pct_carry folds that back).
carry_spec <- list(
  xtds  = list(pbp_xtds = c(-.045, -.04, -.005, .015), part_xtds = c(-.065, -.05, -.005, -.02)),
  plays = list(plays = c(.025, .005, .04, .075)),
  xpass = list(pass_rate = c(.07, .045, .085, .08), fastr_xpass_rate = c(.04, .035, .06, .055),
               pbp_xpass_rate = c(.04, .04, .06, .065), part_xpass_rate = c(.05, .05, .07, .065)),
  scr_rate = list(scr_rate = c(.005, 0, -.045, -.03)),
  scr_ypc  = list(scr_ypc = c(-.045, -.015, -.015, -.015), pbp_scr_xypc = c(-.03, -.02, -.025, -.025), part_scr_xypc = c(-.045, -.01, -.035, -.035)),
  cp    = list(acc_rate = c(-.06, -.02, -.04, -.075), fastr_cp = c(-.01, -.01, -.055, -.045),
               pbp_cp = c(-.005, -.015, -.015, -.055), part_cp = c(-.025, -.045, -.03, -.08)),
  ypa   = list(ypa = c(-.025, -.025, -.025, -.02), pbp_xypa = c(.015, -.01, .025, .015), part_xypa = c(.005, .025, .015, -.04)),
  sack  = list(sack_rate = c(-.025, -.01, -.025, 0), pbp_sack_rate = c(-.035, -.035, -.055, -.045), part_sack_rate = c(-.035, -.025, -.06, -.05)),
  twp   = list(twp_rate = c(-.035, .02, -.05, -.015), int_rate = c(-.035, .015, -.025, -.025)),
  xtd_prop = list(pbp_pass_prop = c(.065, .065, .05, .11), part_pass_prop = c(.07, .05, .07, .11),
                  pbp_qb_scramble_prop = c(.015, .015, 0, -.02), part_qb_scramble_prop = c(.02, .03, -.01, -.02),
                  pbp_run_prop = c(-.08, -.08, -.05, -.09), part_run_prop = c(-.09, -.08, -.06, -.09))
)

# KEPT, not lost -- the JAXLawrence-2025 vs DEN2025 modifiers that sat here before 2026-09-19
# (one QB, one defense, so one number each). To use them again, paste this over carry_spec above.
# carry_spec <- list(
#   xtds  = list(pbp_xtds = -.075, part_xtds = -.075),
#   plays = c(plays = .1),
#   xpass = list(pass_rate = .045, fastr_xpass_rate = .03,
#             pbp_xpass_rate = .03, part_xpass_rate = .055),
#   scr_rate = c(scr_rate = .03),
#   scr_ypc  = c(scr_ypc = -.02, pbp_scr_xypc = -.015, part_scr_xypc = .005),
#   cp    = c(acc_rate = -.035, fastr_cp = -.03, pbp_cp = -.005, part_cp = -.035),
#   ypa   = c(ypa = -.05, pbp_xypa = .005, part_xypa = -.02),
#   sack  = c(sack_rate = -.055, pbp_sack_rate = -.035, part_sack_rate = -.035),
#   twp   = c(twp_rate = -.045, int_rate = -.07),
#   xtd_prop = c(pbp_pass_prop = -.005, part_pass_prop = .025,
#                pbp_qb_scramble_prop = .02, part_qb_scramble_prop = .054,
#                pbp_run_prop = -.015, part_run_prop = -.07)
# )

carry_data <- list(xtd_prop = "xtd_proportion")   # same frame stats_comparison_engine reads for XTD_Prop
carry_rank <- list(pbp_run_prop = "pbp_run_xtd", part_run_prop = "part_run_xtd")

pct_q <- function(x, p)
  unname(stats::quantile(x, probs = pmin(pmax(p, 0), 1), type = 4, na.rm = TRUE))

pct_carry <- function(stat, qbs = STATS_QBS, defs = STATS_DEFS, digits = 3) {
  stat <- tolower(trimws(stat))   # capitals and stray spaces do not matter: "xtdS", "XTDS", " cp " all work
  adj <- carry_spec[[stat]]
  if (is.null(adj)) stop("no family '", stat, "' -- the families are: ", paste(names(carry_spec), collapse = ", "))
  if (is.atomic(adj)) {   # written with c(): c(plays = c(a, b)) flattened to plays1, plays2 -- fold it back
    stem <- sub("[0-9]+$", "", names(adj))
    adj  <- lapply(split(unname(adj), factor(stem, levels = unique(stem))), unname)
  }
  if (anyNA(unlist(adj))) stop("modifiers for '", stat, "' not set: ",
                               paste(names(adj)[vapply(adj, anyNA, TRUE)], collapse = ", "))
  mets <- names(adj)
  df <- if (!is.null(carry_data[[stat]])) dplyr::ungroup(get(carry_data[[stat]])) else qb_stats_df_final   # xtd_proportion arrives grouped
  
  # the modifier for metric m in the matchup QB q vs defense d
  mod_for <- function(m, q, d) {
    a <- adj[[m]]
    if (length(a) == 1) return(unname(a))
    key <- paste(q, "vs", d)
    if (!is.null(names(a)) && key %in% names(a)) return(a[[key]])
    if (!is.null(names(a)) && d %in% names(a))   return(a[[d]])
    if (length(a) == length(qbs) * length(defs))                   # one per matchup: QB first, then defense
      return(a[[(match(q, qbs) - 1) * length(defs) + match(d, defs)]])
    if (length(a) == length(defs)) return(a[[match(d, defs)]])     # one per defense
    stop("modifier '", m, "' has ", length(a), " values: give 1, or one per matchup (",
         length(qbs) * length(defs), ", in this order: ",
         paste(vapply(qbs, function(x) paste(paste(x, "vs", defs), collapse = ", "), ""), collapse = ", "),
         "), or one per defense (", length(defs), "), or name them")
  }
  
  rank_stem <- function(m) if (!is.null(carry_rank[[m]])) carry_rank[[m]] else m
  rank_col  <- function(m, side) paste0(rank_stem(m), if (side == "QB") "_rank_def" else "_rank")
  
  miss <- setdiff(c(mets, vapply(mets, rank_col, "", side = "QB"),
                    vapply(mets, rank_col, "", side = "DEF")), names(df))
  if (length(miss)) stop("data frame is missing: ", paste(miss, collapse = ", "))
  
  games <- function(e, side) {
    if (side == "QB") df[df$qbgrp_ssn == e, ] else df[df$def_ssn == e, ]
  }
  
  # 1. medians of the right rank column -- unshifted here; the shift belongs to the matchup (step 2)
  med <- dplyr::bind_rows(lapply(c(qbs, defs), function(e) {
    side <- if (e %in% qbs) "QB" else "DEF"
    g <- games(e, side)
    row <- data.frame(entity = e, side = side, games = nrow(g))
    for (m in mets) row[[paste0("med_", m)]] <- stats::median(g[[rank_col(m, side)]], na.rm = TRUE)
    row
  }))
  pct <- function(e, m) med[med$entity == e, paste0("med_", m)]
  
  # 2. carry: from-entity's percentile + this matchup's modifier, read inside of-entity's games
  carry <- function(from, of, of_side) {
    d <- if (of_side == "DEF") of else from       # the defense in this matchup
    q <- if (of_side == "DEF") from else of       # the QB group in this matchup
    g <- games(of, of_side)
    row <- data.frame(pct_from = from, games_of = of, n = nrow(g))
    for (m in mets) {
      p <- pct(from, m) + mod_for(m, q, d)
      row[[paste0("mod_", m)]] <- mod_for(m, q, d)
      row[[paste0("pct_", m)]] <- p
      row[[m]] <- round(pct_q(g[[m]], p), digits)
    }
    row
  }
  off_on_def <- dplyr::bind_rows(lapply(qbs,  function(q) lapply(defs, function(d) carry(q, d, "DEF"))))
  def_on_off <- dplyr::bind_rows(lapply(defs, function(d) lapply(qbs,  function(q) carry(d, q, "QB"))))
  
  # 3. the carried values per metric, both directions together
  summ <- dplyr::bind_rows(lapply(mets, function(m) {
    v <- c(off_on_def[[m]], def_on_off[[m]]); s <- sort(v); k <- length(s)
    mid2 <- if (k %% 2 == 0) s[c(k / 2, k / 2 + 1)] else s[(k + 1) / 2]   # the middle two (or the middle one)
    data.frame(metric = m, n = length(v), mean = mean(v), median = stats::median(v),
               mid2_mean = mean(mid2))
  }))
  
  cat("\n== ", stat, "  modifiers by matchup:  ",
      paste(vapply(mets, function(m) paste0(m, " [",
        paste(unlist(lapply(qbs, function(q) sprintf("%s vs %s %+.4f", q, defs, vapply(defs, function(d) mod_for(m, q, d), 0)))), collapse = ", "), "]"), ""),
        collapse = "   "), " ==\n", sep = "")
  print(med, digits = digits, row.names = FALSE)
  cat("\n-- offense percentile (+ matchup mod) -> defense's games --\n"); print(off_on_def, digits = digits + 2, row.names = FALSE)
  cat("\n-- defense percentile (+ matchup mod) -> QB's games --\n");      print(def_on_off, digits = digits + 2, row.names = FALSE)
  cat("\n-- carried values, both directions --\n"); print(summ, digits = digits + 2, row.names = FALSE)
  invisible(list(medians = med, off_on_def = off_on_def, def_on_off = def_on_off, summary = summ))
}

# usage:
#   pct_carry("xpass")
#   r <- pct_carry("plays"); r$summary
# set a family's modifiers in carry_spec, then pct_carry("<family>")
# Nothing runs when this file is sourced: fill a family's four slots, source, then call it, e.g.
pct_carry("twp")

cat("pct_adjuster_engine loaded. Families still holding NA slots: ",
    paste(names(carry_spec)[vapply(carry_spec, function(a) anyNA(unlist(a)), TRUE)], collapse = ", "), "\n", sep = "")