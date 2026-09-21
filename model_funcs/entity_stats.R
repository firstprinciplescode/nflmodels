# entity_stats.R
# PROPOSED -- Claude, UNSIGNED.
#
# Replaces the five View(qb_stats_df_final %>% filter(...) %>% summarise(...))
# calls at the bottom of the build. One call per stat family:
#
#   entity_stats("sack")
#
# gives one row per entity (3 QBs + 2 DEFs), the per-game mean of the actual,
# every expected version of it, and the over-expected differentials.
# OE is the mean of the per-game difference (raw differential), not a
# difference of means -- same thing when nothing is NA, honest when it isn't.
#
# Session needs: qb_stats_df_final, STATS_QBS, STATS_DEFS, dplyr.

entity_family_cols <- list(
  xtds  = list(actual = "tds",       expected = c("pbp_xtds", "part_xtds"),
               extra = "fgs"),
  plays = list(actual = "plays",     expected = character(0),
               extra = "no_huddle"),
  xpass = list(actual = "pass_rate", expected = c("fastr_xpass_rate", "pbp_xpass_rate", "part_xpass_rate"),
               extra = character(0)),
  cp    = list(actual = "acc_rate",  expected = c("fastr_cp", "pbp_cp", "part_cp"),
               extra = character(0)),
  sack  = list(actual = "sack_rate", expected = c("pbp_sack_rate", "part_sack_rate"),
               extra = c("pressure_rate", "pbp_pressure", "part_pressure_before", "part_pressure_after"),
               oe = "ratio"),   # sack only: real DIVIDED BY expected (mean actual / mean expected); 1 = as expected
  ypa   = list(actual = "ypa",       expected = c("pbp_xypa", "part_xypa"),
               extra = "adot"),
  ypc   = list(actual = "ypc",       expected = c("pbp_xypc", "part_xypc"),
               extra = character(0)),
  yac   = list(actual = "yac",       expected = c("pbp_yac", "part_yac"),
               extra = character(0)),
  scr   = list(actual = "scr_ypc",   expected = c("pbp_scr_xypc", "part_scr_xypc"),
               extra = "scr_rate"),
  twp   = list(actual = "twp_rate",  expected = character(0),
               extra = "int_rate",  ratio = c(int_rate = "twp_rate"))
)

# NOTE on rank direction: twp_rate, int_rate, sack_rate and the pbp/part sack
# versions are inverted in the build -- (n() - rank(x)) / (n() - 1) instead of
# (rank(x) - 1) / (n() - 1). So for those, a HIGH rank means a LOW rate, which
# is good for the offense. Every rank in qb_stats_df_final therefore reads the
# same direction: high = good for the offense.

entity_stats <- function(stat, qbs = STATS_QBS, defs = STATS_DEFS,
                         ranks = FALSE, digits = 3) {
  sp <- entity_family_cols[[stat]]
  if (is.null(sp)) stop("no family '", stat, "' -- have: ",
                        paste(names(entity_family_cols), collapse = ", "))
  cols <- c(sp$actual, sp$expected, sp$extra)
  miss <- setdiff(cols, names(qb_stats_df_final))
  if (length(miss)) stop("qb_stats_df_final is missing: ", paste(miss, collapse = ", "))
  
  out <- dplyr::bind_rows(lapply(c(qbs, defs), function(e) {
    side <- if (e %in% qbs) "QB" else "DEF"
    g <- if (side == "QB") qb_stats_df_final[qb_stats_df_final$qbgrp_ssn == e, ]
    else               qb_stats_df_final[qb_stats_df_final$def_ssn   == e, ]
    row <- data.frame(entity = e, side = side, games = nrow(g))
    for (cl in cols) row[[cl]] <- mean(g[[cl]], na.rm = TRUE)
    for (cl in sp$expected)   # oe_<source>: keep the pbp/part/fastr tag, they are different models
      row[[paste0("oe_", cl)]] <- if (identical(sp$oe, "ratio")) {
        ok <- !is.na(g[[sp$actual]]) & !is.na(g[[cl]])          # same games on both sides of the division
        mean(g[[sp$actual]][ok]) / mean(g[[cl]][ok])
      } else mean(g[[sp$actual]] - g[[cl]], na.rm = TRUE)
    for (nm in names(sp$ratio)) { # rate-of-rate, e.g. int_rate / twp_rate: same rule as the sack ratio
      ok <- !is.na(g[[nm]]) & !is.na(g[[sp$ratio[[nm]]]])      # same games on both sides of the division
      row[[paste0(nm, "_per_", sp$ratio[[nm]])]] <- mean(g[[nm]][ok]) / mean(g[[sp$ratio[[nm]]]][ok])
    }
    if (ranks) for (cl in cols) {
      rk <- paste0(cl, if (side == "QB") "_rank_def" else "_rank")   # off reads _rank_def, def reads _rank
      if (rk %in% names(g)) row[[paste0("rk_", cl)]] <- stats::median(g[[rk]], na.rm = TRUE)
    }
    row
  }))
  cat("\n== ", stat, " ==  actual: ", sp$actual,
      if (length(sp$ratio)) paste0("  |  ", names(sp$ratio), "_per_", unlist(sp$ratio),
                                   " = mean(", names(sp$ratio), ") / mean(", unlist(sp$ratio), "), same games") else "",
      if (length(sp$expected)) paste0("  |  oe_ = ", sp$actual, if (identical(sp$oe, "ratio")) " / each expected (ratio, 1 = as expected)" else " - each expected") else "", "\n", sep = "")
  print(as.data.frame(out), digits = digits + 2, row.names = FALSE)
  invisible(out)
}

# usage:
#   entity_stats("sack")
#   entity_stats("xtds", ranks = TRUE)          # adds median of the right rank column per side
#   e <- entity_stats("cp"); e$oe_cp
#   entity_stats("ypa", qbs = "NEMaye-2025", defs = "SEA2025")
# new family: one entry in entity_family_cols -- actual, expected, extra


entity_stats("xtds", ranks = TRUE)


# every family in one call: means, actual - expected (oe_), and MEAN rank on the right side (QB -> _rank_def, DEF -> _rank)
entity_stats_all <- function(qbs = STATS_QBS, defs = STATS_DEFS, families = names(entity_family_cols), digits = 3) {
  out <- list()
  for (f in families) {
    e  <- suppressMessages(entity_stats(f, qbs = qbs, defs = defs, ranks = FALSE, digits = digits)) 
    sp <- entity_family_cols[[f]]
    for (cl in c(sp$actual, sp$expected)) e[[paste0("mnrk_", cl)]] <- vapply(seq_len(nrow(e)), function(i) {
      rk <- paste0(cl, if (e$side[i] == "QB") "_rank_def" else "_rank")
      if (!rk %in% names(qb_stats_df_final)) return(NA_real_)
      g <- if (e$side[i] == "QB") qb_stats_df_final[qb_stats_df_final$qbgrp_ssn == e$entity[i], ] else qb_stats_df_final[qb_stats_df_final$def_ssn == e$entity[i], ]
      mean(g[[rk]], na.rm = TRUE) }, 0)
    out[[f]] <- e
  }
  cat("\n\n################ ALL FAMILIES, side by side: oe_ = actual - expected; mnrk_ = mean rank ################\n")
  for (f in names(out)) { cat("\n--", f, "--\n"); print(as.data.frame(out[[f]]), digits = digits + 1, row.names = FALSE) }
  invisible(out)
}

# usage:
#   r <- entity_stats_all()                 # every family for STATS_QBS / STATS_DEFS
#   r$cp; r$sack
#   entity_stats_all(qbs = "CINBurrow-2025", defs = c("CIN2025", "JAX2022"), families = c("xtds", "cp"))
r <- entity_stats_all()          # every family, for STATS_QBS / STATS_DEFS
r$scr_ypc; 
r$twp                     # or grab one
entity_stats_all(qbs = "JAXLawrence-2025", defs = c("DEN2025"), families = c("xtds", "cp", "sack"))