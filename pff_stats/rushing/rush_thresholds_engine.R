# rush_thresholds_engine.R
# PROPOSED -- Claude, UNSIGNED. Andy stamps or corrects.
#
# The rushing twin of rec_thresholds_engine.R. Same machine: template
# block -> specs (profile only) -> rush_hits once per rusher -> rush_sweep
# over specs x matchups x lenses -> the smallest symmetric widening of
# the tuned QB/DEF tolerances that clears each rusher's floor.
#
# THE CHAIN is rush_func_AWS.R lines 189-201, verbatim: rank_grp %in%
# rank_grp_input, position_group %in% position_group_input,
# situation_cluster %in% situation_input, gap_cluster %in% gap_input,
# gap_z in gap_z_range (or NA when gap_z_NA), xtd_percentile in
# xtd_vec_input (or NA when td_na). Spec fields carry rush_func's own
# argument names so a spec IS the call.
# PROPOSED: THE FLOOR (rush_floor): rank A -> 10, B -> 5, C -> 3; QB -> 10.
# LENSES: rush_func's five are stats_categories' five.
#
# TEMPLATE FORMAT (the rushing block): RUSH GRP is a letter set ("A",
# "A B"); RUN SIT and GAP are space-separated cluster ids with a literal
# NA token allowed ("1 3 99 NA" -> c(1, 3, 99, NA), the canon call's
# c(3, NA) form); ZONE (RSH) is "(a, b)" or "A TO B" with an optional
# trailing NA, or a bare "NA" meaning only NA gap_z matches; XTD PERC
# is "A TO B [NA]".
#
# Session needs: rush_stats_final (build six, with temp/wind/rain_ind/
# snow_ind joined for the weather modes), stats_categories, a tolerance
# table covering every entity in the matchup frame, dplyr, tibble.

# ---- 1. the floor ---------------------------------------------------------
rush_floor <- function(position_group, rank_grp) {
  if (position_group == "QB") return(10)
  g <- rank_grp[1]
  if (g == "A") 10 else if (g == "B") 5 else 3
}

# ---- 2. the template ------------------------------------------------------
# the rushing block of the TEMPLATE sheet, Andy's paste 2026-09-08, verbatim.
# Only rows that carry a RUSH GRP; position_group is HB for backs, QB for
# the quarterback, REC for receivers.
rush_template_ne <- tibble::tribble(
  ~rusher              , ~position_group, ~rush_grp, ~run_sit   , ~gap     , ~zone           , ~xtd,
  "Drake Maye"         , "QB"           , "C"      , "1"        , "4"      , "(-99, -.01) NA", "0 TO 100 NA",
  "Rhamondre Stevenson", "HB"           , "A"      , "1"        , "1 3 5 8", "(.25, 99)"     , "50 TO 100",
  "Corey Kiner"        , "HB"           , "C"      , "1 3 99 NA", "1 3 NA" , "(-.5, 3) NA"   , "0 TO 60 NA",
  "Mack Hollins"       , "REC"          , "C"      , "1 99 NA"  , "6 99 NA", "NA"            , "0 TO 60 NA",
  "Demario Douglas"    , "REC"          , "C"      , "1 99 NA"  , "6 99 NA", "NA"            , "0 TO 60 NA",
  "Kyle Williams"      , "REC"          , "C"      , "1 99 NA"  , "6 99 NA", "NA"            , "0 TO 60 NA"
)

# the SEA rushing block, Andy's paste 2026-09-08, verbatim. Same rules.
rush_template_sea <- tibble::tribble(
  ~rusher             , ~position_group, ~rush_grp, ~run_sit  , ~gap    , ~zone         , ~xtd,
  "Jadarian Price"    , "HB"           , "A"      , "3 4"     , "3 6 7" , "(-99, -.01)" , "0 TO 70",
  "George Holani"     , "HB"           , "C"      , "1 2 4 6" , "1 3 4" , "(-3, 3) NA"  , "0 TO 50",
  "AJ Barner"         , "REC"          , "C"      , "3 4 6 NA", "1 4 NA", "(-99, 99) NA", "0 TO 60 NA",
  "Jaxon Smith-Njigba", "REC"          , "C"      , "3 4 6 NA", "1 4 NA", "(-99, 99) NA", "0 TO 60 NA",
  "Rashid Shaheed"    , "REC"          , "C"      , "3 4 6 NA", "1 4 NA", "(-99, 99) NA", "0 TO 60 NA",
  "Sam Darnold"       , "QB"           , "C"      , "4 NA"    , "3 NA"  , "(-99, 99) NA", "0 TO 50 NA"
)

# the JAX rushing block, read off the TEMPLATE sheet of "JAX v DEN.xlsx" as saved 2026-09-20 12:59 (columns RUSH GRP / RUN SIT / GAP /
# ZONE (RSH) / XTD PERC), verbatim. NEW FORMAT in this block: a back who plays in two rank groups carries BOTH in one row --
# RUSH GRP "A B", and RUN SIT / GAP written "A part | B part". rush_template_split() below turns that into one row PER GROUP
# ("Bhayshul Tuten A", "Bhayshul Tuten B"), so each group gets its own spec, its own floor and its own workbook.
rush_template_jax <- tibble::tribble(
  ~rusher              , ~position_group, ~rush_grp, ~run_sit     , ~gap           , ~zone          , ~xtd,
  "Trevor Lawrence"    , "QB"           , "C"      , "1 3 99"     , "4 99"         , "(-99, 99) NA" , "60 TO 100",      # TEMPLATE row 15 (the QB row sits ABOVE the header row 16 -- it was missed in the first read; added 2026-09-20)
  "Bhayshul Tuten"     , "HB"           , "A B"    , "1 2 | 2 5"  , "1 6 | 2"      , "(-99, 0)"     , "0 TO 60",
  "Chris Rodriguez Jr.", "HB"           , "B C"    , "3 4 | 1 2 3", "1 2 4 | 1 2 3", "(-99, 99) NA" , "0 TO 100",
  "Ameer Abdullah"     , "HB"           , "C"      , "2 99 NA"    , "1 2 3 NA"     , "(-99, 99) NA" , "50 TO 100 NA",
  "Jakobi Meyers"      , "REC"          , "C"      , "1 NA"       , "2 NA"         , "(-99, 99) NA" , "30 TO 90",
  "Parker Washington"  , "REC"          , "C"      , "1 NA"       , "2 NA"         , "(-99, 99) NA" , "30 TO 90",
  "Brian Thomas Jr."   , "REC"          , "C"      , "1 NA"       , "2 NA"         , "(-99, 99) NA" , "30 TO 90",
  "Travis Hunter"      , "REC"          , "C"      , "1 NA"       , "2 NA"         , "(-99, 99) NA" , "30 TO 90"
)

# one row per rank group; all the receivers folded into ONE row (one spec, one workbook) named wr_name.
#   "A B" + "1 | 2 5"  ->  "<rusher> A" with "1"   and   "<rusher> B" with "2 5". A field with no "|" is used for every group.
#   Receivers are folded only when their rows are identical; if they differ it stops and says which.
rush_template_split <- function(df, wr_name = NULL) {
  out <- list()
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]; g <- trimws(unlist(strsplit(r$rush_grp, "[[:space:]]+"))); g <- g[g != ""]
    if (length(g) == 1) { out[[length(out) + 1]] <- r; next }
    for (k in seq_along(g)) { rk <- r; rk$rusher <- paste(r$rusher, g[k]); rk$rush_grp <- g[k]
      for (f in c("run_sit", "gap", "zone", "xtd")) { parts <- trimws(unlist(strsplit(r[[f]], "|", fixed = TRUE)))
        if (length(parts) == 1) next
        if (length(parts) != length(g)) stop(r$rusher, ": ", f, " has ", length(parts), " parts for ", length(g), " groups: '", r[[f]], "'")
        rk[[f]] <- parts[k] }
      out[[length(out) + 1]] <- rk }
  }
  out <- dplyr::bind_rows(out)
  if (!is.null(wr_name) && any(out$position_group == "REC")) {
    w <- out[out$position_group == "REC", ]; key <- unique(w[, c("rush_grp", "run_sit", "gap", "zone", "xtd")])
    if (nrow(key) > 1) stop("the receivers do not all carry the same rushing profile, so they cannot share one row: ",
                            paste(w$rusher, w$rush_grp, w$run_sit, w$gap, w$zone, w$xtd, sep = " / ", collapse = "  ||  "))
    cat(wr_name, "= one spec for:", paste(w$rusher, collapse = ", "), "\n")
    out <- dplyr::bind_rows(out[out$position_group != "REC", ], dplyr::mutate(w[1, ], rusher = wr_name))
  }
  out
}

# ---- 3. specs (profile only; matchups ride beside) ---------------------------
rush_specs_from_df <- function(df) {
  # "A TO B [NA]"  or  "(a, b) [NA]"  or a bare "NA" (= only NA rows match: range NA/NA, flag on)
  band   <- function(s) { s0 <- s; s <- gsub("\\s*NA\\s*$", "", s)
  if (!nzchar(s)) return(c(NA_real_, NA_real_))
  s <- gsub("[()]", "", s); v <- suppressWarnings(as.numeric(trimws(strsplit(s, ",| TO ")[[1]])))
  if (length(v) != 2 || any(is.na(v))) stop("band not 'A TO B [NA]' / '(a, b) [NA]' / 'NA': '", s0, "'"); v }
  has_na <- function(s) grepl("NA\\s*$", s)
  toks   <- function(s) { t <- trimws(unlist(strsplit(s, "\\s+"))); t <- t[t != ""]
  v <- suppressWarnings(as.integer(ifelse(t == "NA", NA, t)))
  if (any(is.na(v) & t != "NA")) stop("cluster list not integers / NA: '", s, "'")
  if (any(v %in% 99) && !anyNA(v)) v <- c(v, NA)          # Andy 2026-09-26: "99 is like the NA thing" -- no cluster; rush_stats_final codes those rows NA
  v }
  lets   <- function(s) { t <- trimws(unlist(strsplit(s, "\\s+"))); t[t != ""] }
  specs <- list()
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    specs[[r$rusher]] <- list(
      position_group       = r$position_group,
      position_group_input = r$position_group,
      rank_grp_input       = lets(r$rush_grp),
      situation_input      = toks(r$run_sit),
      gap_input            = toks(r$gap),
      gap_z_range          = band(r$zone),
      gap_z_NA             = has_na(r$zone),
      xtd_vec_input        = band(r$xtd),
      td_na                = has_na(r$xtd))
  }
  specs
}

rush_matchups_ne <- expand.grid(qbgrp = c("NEMaye-2025", "TENTannehill-2019", "DALPrescott-2025"),
                                defgrp = c("SEA2025", "SEA2024"), stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
rush_matchups_sea <- data.frame(qbgrp = "SEADarnold-2025", defgrp = "NE2025", stringsAsFactors = FALSE)
rush_matchups_jax <- data.frame(qbgrp = "JAXLawrence-2025", defgrp = "DEN2025", stringsAsFactors = FALSE)

# the SEA matchup's TUNED BASE, banked here under entity keys and merged
# into stats_tol_qb / stats_tol_def (entity-level add; NE entities untouched).
# From the 2026-09-07 tuner run (qb_base / def_base in the SEA receiving
# sweep). Both offenses now read stats_tol_qb / stats_tol_def; the tuner
# tail (xtds_tol_*) is not passed to any engine.
if (exists("stats_tol_qb") && exists("stats_tol_def")) {
  stats_tol_qb[["SEADarnold-2025"]] <- c(blitz = 0.960, depth = 0.920, less = 0.955, pa = 1.065, pressure = 0.950)
  stats_tol_def[["NE2025"]]         <- c(blitz = 0.960, depth = 1.015, less = 1.010, pa = 0.970, pressure = 0.965)
  # JAX @ DEN: the tuner result pasted at the bottom of comparison_engine_thresholds.R. Added ONLY if the session has no row yet (a live tune wins).
  if (is.null(stats_tol_qb[["JAXLawrence-2025"]])) stats_tol_qb[["JAXLawrence-2025"]] <- c(blitz = 0.910, depth = 0.980, less = 0.930, pa = 0.950, pressure = 0.905)
  if (is.null(stats_tol_def[["DEN2025"]]))         stats_tol_def[["DEN2025"]]         <- c(blitz = 1.220, depth = 1.105, less = 1.165, pa = 1.050, pressure = 1.090)
  cat("stats_tol_qb covers:", paste(names(stats_tol_qb), collapse = ", "), "| stats_tol_def covers:", paste(names(stats_tol_def), collapse = ", "), "\n")
} else stop("source stats_comparison_engine.R before this file")

# ---- LAC @ BUF (2026-09-26) ------------------------------------------------
# LAC rushing block copied verbatim from LAC v BUF_3.xlsx, TEMPLATE cols BC:BG (Andy's save 2026-09-26 18:00):
# row 15 (QB row, above the header) and rows 17-28 that carry a RUSH GRP. POS GRP (col BH) was blank, so position_group
# is set the way the other blocks spell it: QB / HB / REC. Harris, Johnson and Davis carry the same profile, so
# rush_template_split() folds them into ONE spec, "LAC WR" (as "JAX WR"). Names as typed on the sheet.
rush_template_lac <- tibble::tribble(
  ~rusher           , ~position_group, ~rush_grp, ~run_sit, ~gap    , ~zone          , ~xtd,
  "Justin Herbert"  , "QB"           , "C"      , "1 3 99 NA", "4 99 NA"  , "(0, 99) NA"   , "0 TO 100 NA",
  "Omarion Hampton" , "HB"           , "A"      , "4"        , "1 8"      , "(.25, 99)"    , "30 TO 80",
  "Keaton Mitchell" , "HB"           , "B"      , "4 5"      , "1 3"      , "(0, 99)"      , "25 TO 75",
  "Kimani Vidal"    , "HB"           , "C"      , "1 99 NA"  , "1 3 99 NA", "(-.25, 99) NA", "0 TO 100 NA",
  "Tre Harris"      , "REC"          , "C"      , "99 NA"    , "99 NA"    , "(-99, 99) NA" , "0 TO 100 NA",
  "Quentin Johnson" , "REC"          , "C"      , "99 NA"    , "99 NA"    , "(-99, 99) NA" , "0 TO 100 NA",
  "Derius Davis"    , "REC"          , "C"      , "99 NA"    , "99 NA"    , "(-99, 99) NA" , "0 TO 100 NA"
)
# Andy 2026-09-26: every 99 in RUN SIT / GAP also carries an explicit NA ("just in case"); the sheet had the 99 alone.

# BUF rushing block, copied from LAC v BUF_3.xlsx TEMPLATE cols BC:BG (Andy's save 2026-09-26 19:40): rows 30-35 that carry
# a RUSH GRP plus row 42 (the QB row). POS GRP blank -> QB / HB / REC. Andy: "given low usage, Frank Gore / Ray Davis are
# basically the same" -- their sheet rows differ only in RUN SIT (1 2 99 vs 1 3 99), kept as typed. NA added beside each 99.
rush_template_buf <- tibble::tribble(
  ~rusher      , ~position_group, ~rush_grp, ~run_sit   , ~gap          , ~zone          , ~xtd,
  "Josh Allen" , "QB"           , "B"      , "2 4 5"    , "2"           , "(.5, 99)"     , "55 TO 100",
  "James Cook" , "HB"           , "A"      , "2 4"      , "5 6"         , "(-1.5, 1.5)"  , "0 TO 40",
  "Ray Davis"  , "HB"           , "C"      , "1 2 99 NA", "1 99 NA"     , "(-.25, 99) NA", "0 TO 50 NA",
  "Frank Gore" , "HB"           , "C"      , "1 3 99 NA", "1 99 NA"     , "(-.25, 99) NA", "0 TO 50 NA",
  "DJ Moore"   , "REC"          , "C"      , "1 2 99 NA", "1 2 3 4 99 NA", "(0, 99) NA"  , "20 TO 80 NA"
)
# BUF offense: Allen 2025 against the 2025 Chargers defense (one matchup)
rush_matchups_buf <- data.frame(qbgrp = "BUFAllen-2025", defgrp = "LAC2025", stringsAsFactors = FALSE)
# the tuned base Andy asked for: the ORIGINAL passing-side tuner result for BUFAllen-2025 vs LAC2025 (tuner tail as pasted
# 2026-09-25 evening; the file's tail has since been overwritten by the LAC side, so the numbers are banked here)
rush_base_tol_qb_buf <- list(
  "BUFAllen-2025" = c(blitz = 0.955, depth = 0.995, less = 0.975, pa = 0.940, pressure = 0.945)
)
rush_base_tol_def_buf <- list(
  "LAC2025" = c(blitz = 1.000, depth = 1.030, less = 1.045, pa = 1.005, pressure = 1.090)
)
# LAC offense: Herbert 2025 against BUF2025 and LAC2021 (Andy's stand-in for the Bills D)
rush_matchups_lac <- data.frame(qbgrp = c("LACHerbert-2025", "LACHerbert-2025"), defgrp = c("BUF2025", "LAC2021"), stringsAsFactors = FALSE)
# the tuned base for this game, verbatim from the tuner tail of comparison_engine_thresholds.R (2026-09-26 00:03)
rush_base_tol_qb_lac <- list(
  "LACHerbert-2025" = c(blitz = 0.970, depth = 0.990, less = 0.960, pa = 0.970, pressure = 0.930)
)
rush_base_tol_def_lac <- list(
  "BUF2025" = c(blitz = 0.925, depth = 1.120, less = 0.955, pa = 1.095, pressure = 1.015),
  "LAC2021" = c(blitz = 0.985, depth = 0.945, less = 0.920, pa = 0.925, pressure = 0.935)
)

# ---- NE @ JAX (2026-09-27) -----------------------------------------------------
# Both rushing blocks copied verbatim from NE v JAX.xlsx TEMPLATE cols BC:BG (Andy's save 2026-09-27 11:10). NE = row 15 (QB)
# + rows 17-27 with a RUSH GRP; JAX = rows 31-43. POS GRP -> QB / HB / REC. Two-group backs use the "A part | B part" split.
# Lawrence's row 43 has no ZONE / XTD PERC typed -> last week's "(-99, 99) NA" / "0 TO 100 NA" used; flagged to Andy.
# NA added beside each 99 (Andy's rule). The four JAX WRs share one profile -> folded into "JAX WR".
rush_template_nej <- tibble::tribble(
  ~rusher              , ~position_group, ~rush_grp, ~run_sit         , ~gap                  , ~zone            , ~xtd,
  "Drake Maye"         , "QB"           , "C"      , "1 3"            , "4"                   , "(-99, -.01) NA" , "0 TO 100 NA",
  "Rhamondre Stevenson", "HB"           , "A B"    , "2 3 | 4"        , "1 2 3 5 | 2 3 4"     , "(0, 99)"        , "30 TO 100",
  "TreVeyon Henderson" , "HB"           , "A B"    , "2 3 4 | 4 5"    , "1 2 3 5 8 | 2 3 4"   , "(.5, 99)"       , "0 TO 100",
  "Corey Kiner"        , "HB"           , "C"      , "1 3 NA"         , "1 3 4 NA"            , "(-.5, 3) NA"    , "0 TO 100 NA",
  "Mack Hollins"       , "REC"          , "C"      , "3 99 NA"        , "1 99 NA"             , "NA"             , "0 TO 60 NA",
  "Demario Douglas"    , "REC"          , "C"      , "3 99 NA"        , "1 99 NA"             , "NA"             , "0 TO 60 NA",
  "Kyle Williams"      , "REC"          , "C"      , "3 99 NA"        , "1 99 NA"             , "NA"             , "0 TO 60 NA"
)
rush_template_jaxn <- tibble::tribble(
  ~rusher              , ~position_group, ~rush_grp, ~run_sit         , ~gap                  , ~zone            , ~xtd,
  "Trevor Lawrence"    , "QB"           , "C"      , "1"              , "2 4"                 , "(-99, 99) NA"   , "0 TO 100 NA",
  "Bhayshul Tuten"     , "HB"           , "A B"    , "1 2 4 | 2 4 5"  , "1 3 7 8 | 1 2 3"     , "(-99, .25)"     , "0 TO 60",
  "Chris Rodriguez Jr.", "HB"           , "B C"    , "3 4 5 | 1 2"    , "2 3 4 | 1 3"         , "(.25, 99)"      , "0 TO 80",
  "Ameer Abdullah"     , "HB"           , "C"      , "1 3 99 NA"      , "1 2 4 99 NA"         , "(-99, 0) NA"    , "50 TO 100 NA",
  "Jakobi Meyers"      , "REC"          , "C"      , "1 NA"           , "2 NA"                , "(-99, 99) NA"   , "35 TO 100",
  "Parker Washington"  , "REC"          , "C"      , "1 NA"           , "2 NA"                , "(-99, 99) NA"   , "35 TO 100",
  "Brian Thomas Jr."   , "REC"          , "C"      , "1 NA"           , "2 NA"                , "(-99, 99) NA"   , "35 TO 100",
  "Travis Hunter"      , "REC"          , "C"      , "1 NA"           , "2 NA"                , "(-99, 99) NA"   , "35 TO 100"
)
rush_matchups_nej  <- data.frame(qbgrp = c("NEMaye-2025", "NEMaye-2025"), defgrp = c("BUF2025", "JAX2025"), stringsAsFactors = FALSE)
rush_matchups_jaxn <- data.frame(qbgrp = "JAXLawrence-2025", defgrp = "NE2025", stringsAsFactors = FALSE)
# starting tolerances = the passing tuner rows for this game (both tails as pasted / printed 2026-09-26; BUF/JAX = engine bank)
rush_base_tol_qb_nej  <- list("NEMaye-2025" = c(blitz = 0.985, depth = 0.950, less = 0.985, pa = 1.015, pressure = 1.025))
rush_base_tol_def_nej <- list("BUF2025" = c(blitz = 0.925, depth = 1.120, less = 0.955, pa = 1.095, pressure = 1.015),
                              "JAX2025" = c(blitz = 0.970, depth = 1.035, less = 0.940, pa = 1.225, pressure = 0.950))
rush_base_tol_qb_jaxn  <- list("JAXLawrence-2025" = c(blitz = 0.910, depth = 0.980, less = 0.930, pa = 0.950, pressure = 0.905))
rush_base_tol_def_jaxn <- list("NE2025" = c(blitz = 0.960, depth = 1.015, less = 1.010, pa = 0.970, pressure = 0.965))

rush_specs_ne  <- rush_specs_from_df(rush_template_ne)
rush_specs_sea <- rush_specs_from_df(rush_template_sea)
rush_specs_jax <- rush_specs_from_df(rush_template_split(rush_template_jax, wr_name = "JAX WR"))
cat("rush_specs_ne:", length(rush_specs_ne), "rushers --", paste(names(rush_specs_ne), collapse = ", "), "\n")
cat("rush_specs_sea:", length(rush_specs_sea), "rushers --", paste(names(rush_specs_sea), collapse = ", "), "\n")
cat("rush_specs_jax:", length(rush_specs_jax), "specs --", paste(names(rush_specs_jax), collapse = ", "), "\n")
rush_specs_lac <- rush_specs_from_df(rush_template_split(rush_template_lac, wr_name = "LAC WR"))
cat("rush_specs_lac:", length(rush_specs_lac), "specs --", paste(names(rush_specs_lac), collapse = ", "), "\n")
rush_specs_buf <- rush_specs_from_df(rush_template_buf)
cat("rush_specs_buf:", length(rush_specs_buf), "specs --", paste(names(rush_specs_buf), collapse = ", "), "\n")
rush_specs_nej  <- rush_specs_from_df(rush_template_split(rush_template_nej,  wr_name = "NE WR"))
rush_specs_jaxn <- rush_specs_from_df(rush_template_split(rush_template_jaxn, wr_name = "JAX WR"))
cat("rush_specs_nej:",  length(rush_specs_nej),  "specs --", paste(names(rush_specs_nej),  collapse = ", "), "\n")
cat("rush_specs_jaxn:", length(rush_specs_jaxn), "specs --", paste(names(rush_specs_jaxn), collapse = ", "), "\n")

# ---- LA @ DEN (2026-09-27, Sunday night) ------------------------------------------
# DEN rushing block copied verbatim from LA v DEN.xlsx TEMPLATE cols BC:BG, rows 31-42 + row 46 (Andy's save 2026-09-27 18:43).
# POS GRP (col BH) blank -> QB / HB / REC (Adkins is a TE, so REC). Nix (row 46) is a two-group QB, "B C", typed the
# "B part | C part" way -> rush_template_split() makes "Bo Nix B" and "Bo Nix C" (Andy: "MAKE SURE BO NIX HAS TWO LINES").
# Adkins, Franklin, Bryant and Mims carry one identical REC profile -> folded into "DEN WR" (Andy: "nate adkins is part of the
# WR group" -- in rushing, WR means every REC). NA is written where the sheet has it.
rush_template_den <- tibble::tribble(
  ~rusher         , ~position_group, ~rush_grp, ~run_sit       , ~gap      , ~zone           , ~xtd,
  "Bo Nix"        , "QB"           , "B C"    , "1 2 4 5 | 2 4", "1 2 | 2 4", "(-99, 99)"    , "0 TO 100",
  "JK Dobbins"    , "HB"           , "A"      , "1 2"          , "3 7"     , "(.25, 99)"     , "50 TO 100",
  "RJ Harvey"     , "HB"           , "B"      , "2 4 5"        , "1 2 3 4" , "(-99, .25)"    , "0 TO 100",
  "Tyler Badie"   , "HB"           , "C"      , "1 2 4"        , "1 3"     , "(.25, 99)"     , "0 TO 40",
  "Adam Prentice" , "HB"           , "C"      , "3 99 NA"      , "3 4"     , "(-99, .25) NA" , "40 TO 100 NA",
  "Nate Adkins"   , "REC"          , "C"      , "1 2 4 99 NA"  , "2 99 NA" , "(.25, 99) NA"  , "10 TO 60 NA",
  "Troy Franklin" , "REC"          , "C"      , "1 2 4 99 NA"  , "2 99 NA" , "(.25, 99) NA"  , "10 TO 60 NA",
  "Pat Bryant"    , "REC"          , "C"      , "1 2 4 99 NA"  , "2 99 NA" , "(.25, 99) NA"  , "10 TO 60 NA",
  "Marvin Mims"   , "REC"          , "C"      , "1 2 4 99 NA"  , "2 99 NA" , "(.25, 99) NA"  , "10 TO 60 NA"
)
# DEN offense: Nix 2025 against LA2025 (the real one) and PHI2025 (the family stand-in), in STATS_DEFS order
rush_matchups_den <- data.frame(qbgrp = c("DENNix-2025", "DENNix-2025"), defgrp = c("LA2025", "PHI2025"), stringsAsFactors = FALSE)
# starting tolerances = the passing tuner rows for this game, verbatim from the tail of comparison_engine_thresholds.R
# (saved 2026-09-27 16:55, the rows the 16:56 DENNix pushes were built on). Andy: "STARTS AS THE BASELINE. AND THEN WE GO FROM THERE."
rush_base_tol_qb_den  <- list("DENNix-2025" = c(blitz = 0.880, depth = 0.995, less = 0.920, pa = 0.985, pressure = 0.960))
rush_base_tol_def_den <- list("LA2025"  = c(blitz = 0.950, depth = 0.970, less = 0.945, pa = 0.945, pressure = 0.965),
                              "PHI2025" = c(blitz = 0.970, depth = 1.020, less = 1.025, pa = 0.995, pressure = 0.950))
rush_specs_den <- rush_specs_from_df(rush_template_split(rush_template_den, wr_name = "DEN WR"))
cat("rush_specs_den:", length(rush_specs_den), "specs --", paste(names(rush_specs_den), collapse = ", "), "\n")

# ---- PHI @ CHI (2026-09-28, Monday night) ------------------------------------------
# PHI rushing block copied verbatim from PHI v CHI.xlsx TEMPLATE cols BC:BG: row 15 (Hurts) + rows 17-19, 23, 25, 26 (Andy's save 2026-09-28 16:46).
# POS GRP (col BH) blank -> QB / HB / REC. Hurts is a two-group QB, "B C", typed the "B part | C part" way -> "Jalen Hurts B" / "Jalen Hurts C".
# Smith, Lemon and Cooper carry one identical REC profile -> folded into "PHI WR" (same rule as DEN WR). Ertz / Mundt / Jenkins / Wicks have no rushing row.
rush_template_phi <- tibble::tribble(
  ~rusher          , ~position_group, ~rush_grp, ~run_sit        , ~gap         , ~zone           , ~xtd,
  "Jalen Hurts"    , "QB"           , "B C"    , "2 3 4 | 1 3"   , "1 2 | 1 2 4", "(-.25, 2)"     , "50 TO 100",
  "Saquon Barkley" , "HB"           , "A"      , "2 4"           , "3"          , "(-99, 0)"      , "0 TO 100",
  "Tank Bigsby"    , "HB"           , "C"      , "1 99 NA"       , "1 3 99 NA"  , "(-.25, 99) NA" , "0 TO 50 NA",
  "Will Shipley"   , "HB"           , "C"      , "1 2 3 4 99 NA" , "1 3 99 NA"  , "(-99, -.25) NA", "60 TO 100 NA",
  "DeVonta Smith"  , "REC"          , "C"      , "1 2 3 4 99 NA" , "2 99 NA"    , "(-99, 99) NA"  , "0 TO 100 NA",
  "Makai Lemon"    , "REC"          , "C"      , "1 2 3 4 99 NA" , "2 99 NA"    , "(-99, 99) NA"  , "0 TO 100 NA",
  "Darius Cooper"  , "REC"          , "C"      , "1 2 3 4 99 NA" , "2 99 NA"    , "(-99, 99) NA"  , "0 TO 100 NA"
)
rush_matchups_phi <- data.frame(qbgrp = "PHIHurts-2025", defgrp = "CHI2025", stringsAsFactors = FALSE)
# starting tolerances = Andy's pasted xtds rows for this game (2026-09-28): "THIS AS THE BASELINE"
rush_base_tol_qb_phi  <- list("PHIHurts-2025" = c(blitz = 0.960, depth = 1.020, less = 0.925, pa = 0.935, pressure = 0.955))
rush_base_tol_def_phi <- list("CHI2025"       = c(blitz = 0.955, depth = 1.000, less = 0.990, pa = 1.110, pressure = 0.990))
rush_specs_phi <- rush_specs_from_df(rush_template_split(rush_template_phi, wr_name = "PHI WR"))
cat("rush_specs_phi:", length(rush_specs_phi), "specs --", paste(names(rush_specs_phi), collapse = ", "), "\n")

# ---- DAL @ HST (2026-10-03) ----------------------------------------------------------
# DAL rushing block as Andy pasted it 2026-10-03 (DAL v HST.xlsx TEMPLATE, cols RUSH GRP / RUN SIT / GAP / ZONE (RSH) / XTD PERC).
# POS GRP -> QB / HB / REC. Goodson is a two-group back, "B C", typed the "B part | C part" way -> "Tyler Goodson B" / "Tyler Goodson C".
# His GAP was pasted "1 2 3'| 1 3": the stray apostrophe is dropped -> "1 2 3 | 1 3" (flagged to Andy). Hunter Luepke (FB -> HB, as Prentice) added from Andy's second paste.
# Lamb and Turpin carry one identical REC profile -> folded into "DAL WR" (same rule as PHI WR / DEN WR).
rush_template_dal <- tibble::tribble(
  ~rusher           , ~position_group, ~rush_grp, ~run_sit        , ~gap           , ~zone          , ~xtd,
  "Dak Prescott"    , "QB"           , "C"      , "3 99 NA"       , "4 99 NA"      , "(-1, 99) NA"  , "0 TO 100",
  "Javonte Williams", "HB"           , "A"      , "1 2 4"         , "2 5"          , "(-3, 1)"      , "0 TO 50",
  "Tyler Goodson"   , "HB"           , "B C"    , "1 | 2 4"       , "1 2 3 | 1 3"  , "(-3, 1)"      , "0 TO 40",
  "Hunter Luepke"   , "HB"           , "C"      , "2 4 99 NA"     , "3 4 99 NA"    , "(-99, 0) NA"  , "0 TO 80",
  "CeeDee Lamb"   , "REC"          , "C"      , "1 2 3 99 NA"   , "2 99 NA"      , "(-.5, 99) NA" , "0 TO 100 NA",
  "KaVontae Turpin" , "REC"          , "C"      , "1 2 3 99 NA"   , "2 99 NA"      , "(-.5, 99) NA" , "0 TO 100 NA"
)
rush_matchups_dal <- data.frame(qbgrp = "DALPrescott-2025", defgrp = "HST2025", stringsAsFactors = FALSE)
# starting tolerances = Andy's pasted xtds rows for this game (2026-10-03): "WITH THIS AS THE STARTING POINT"
rush_base_tol_qb_dal  <- list("DALPrescott-2025" = c(blitz = 0.955, depth = 0.950, less = 0.935, pa = 0.925, pressure = 0.930))
rush_base_tol_def_dal <- list("HST2025"          = c(blitz = 1.095, depth = 1.125, less = 1.135, pa = 0.980, pressure = 1.005))
rush_specs_dal <- rush_specs_from_df(rush_template_split(rush_template_dal, wr_name = "DAL WR"))
cat("rush_specs_dal:", length(rush_specs_dal), "specs --", paste(names(rush_specs_dal), collapse = ", "), "\n")

# HST rushing block, read off DAL v HST.xlsx TEMPLATE cols BC:BG (Andy's save 2026-10-03 19:40): rows 31-32, 37-40, 43.
# Montgomery is a two-group back, "A B" -> "David Montgomery A" / "David Montgomery B". Collins, Hutchinson and Noel carry one
# identical REC profile -> folded into "HST WR"; Boutte has no RUSH GRP -> no row. Stroud (row 43) from Andy's paste 2026-10-03 ~20:00.
rush_template_hst <- tibble::tribble(
  ~rusher             , ~position_group, ~rush_grp, ~run_sit       , ~gap           , ~zone          , ~xtd,
  "C.J. Stroud"       , "QB"           , "C"      , "3 99 NA"      , "1 3 4 99 NA"  , "(-99, 99) NA" , "0 TO 100",
  "David Montgomery"  , "HB"           , "A B"    , "2 4 | 2 3 4"  , "2 4 | 2 3 4"  , "(.5, 99)"     , "0 TO 100",
  "Woody Marks"       , "HB"           , "B"      , "1 2 4 5"      , "3 4"          , "(.5, 99)"     , "0 TO 40",
  "Nico Collins"      , "REC"          , "C"      , "1 3 99 NA"    , "2 99 NA"      , "(-99, 99) NA" , "0 TO 60",
  "Xavier Hutchinson" , "REC"          , "C"      , "1 3 99 NA"    , "2 99 NA"      , "(-99, 99) NA" , "0 TO 60",
  "Jaylin Noel"       , "REC"          , "C"      , "1 3 99 NA"    , "2 99 NA"      , "(-99, 99) NA" , "0 TO 60"
)
# HST offense: Stroud 2025 and Goff 2020 (the McVay-tree comp) against DAL2025 and IND2025 -- STATS_QBS x STATS_DEFS order of the book tabs
rush_matchups_hst <- data.frame(qbgrp = c("HSTStroud-2025", "HSTStroud-2025", "LAGoff-2020", "LAGoff-2020"),
                                defgrp = c("DAL2025", "IND2025", "DAL2025", "IND2025"), stringsAsFactors = FALSE)
# starting tolerances = Andy's pasted xtds rows for this game (2026-10-03), = comparison_engine_thresholds.R lines 330-336
rush_base_tol_qb_hst  <- list("HSTStroud-2025" = c(blitz = 0.940, depth = 0.965, less = 0.995, pa = 0.975, pressure = 1.000),
                              "LAGoff-2020"    = c(blitz = 0.935, depth = 1.025, less = 0.950, pa = 0.965, pressure = 1.005))
rush_base_tol_def_hst <- list("DAL2025" = c(blitz = 1.035, depth = 1.020, less = 1.015, pa = 1.050, pressure = 1.070),
                              "IND2025" = c(blitz = 0.955, depth = 1.010, less = 0.970, pa = 0.940, pressure = 0.970))
rush_specs_hst <- rush_specs_from_df(rush_template_split(rush_template_hst, wr_name = "HST WR"))
cat("rush_specs_hst:", length(rush_specs_hst), "specs --", paste(names(rush_specs_hst), collapse = ", "), "\n")

# ---- NE @ BUF (2026-10-03) ------------------------------------------------------------
# NE rushing block as Andy pasted it 2026-10-03 (NE v BUF.xlsx TEMPLATE). POS GRP -> QB / HB / REC. Henderson and Stevenson are
# two-group backs, "A B" -> "<name> A" / "<name> B". Doubs, Douglas, Hollins and Kyle Williams carry one identical REC profile ->
# folded into "NE WR". Their "1 99" / "2 99" get the NA added by toks() (Andy's 99 rule).
rush_template_neb <- tibble::tribble(
  ~rusher              , ~position_group, ~rush_grp, ~run_sit       , ~gap              , ~zone          , ~xtd,
  "Drake Maye"         , "QB"           , "C"      , "1 3"          , "4"               , "(-99, 0)"     , "60 TO 100",
  "TreVeyon Henderson" , "HB"           , "A B"    , "2 3 4 | 4 5"  , "1 5 8 | 2 3 4"   , "(.5, 99)"     , "0 TO 100",
  "Rhamondre Stevenson", "HB"           , "A B"    , "1 2 3 | 4"    , "1 2 5 | 2 3 4"   , "(.35, 99)"    , "35 TO 100",
  "Corey Kiner"        , "HB"           , "C"      , "1 3 99 NA"    , "1 3 4 99 NA"     , "(0, 99)"      , "60 TO 100",
  "Romeo Doubs"        , "REC"          , "C"      , "1 99"         , "2 99"            , "(-99, 99) NA" , "0 TO 45",
  "DeMario Douglas"    , "REC"          , "C"      , "1 99"         , "2 99"            , "(-99, 99) NA" , "0 TO 45",
  "Mack Hollins"       , "REC"          , "C"      , "1 99"         , "2 99"            , "(-99, 99) NA" , "0 TO 45",
  "Kyle Williams"      , "REC"          , "C"      , "1 99"         , "2 99"            , "(-99, 99) NA" , "0 TO 45"
)
# NE offense: Maye 2025 and Watson 2019 (the Patriots-tree comp) against BUF2025 and LAC2021 -- book tab order (NE v BUF25, NE v LAC21, HST v BUF25, HST v LAC21)
rush_matchups_neb <- data.frame(qbgrp = c("NEMaye-2025", "NEMaye-2025", "HSTWatson-2019", "HSTWatson-2019"),
                                defgrp = c("BUF2025", "LAC2021", "BUF2025", "LAC2021"), stringsAsFactors = FALSE)
# starting tolerances = Andy's pasted xtds rows for this game (2026-10-03), = comparison_engine_thresholds.R lines 351-357
rush_base_tol_qb_neb  <- list("NEMaye-2025"    = c(blitz = 1.000, depth = 0.950, less = 0.970, pa = 1.015, pressure = 1.070),
                              "HSTWatson-2019" = c(blitz = 0.920, depth = 0.975, less = 0.970, pa = 0.980, pressure = 0.970))
rush_base_tol_def_neb <- list("BUF2025" = c(blitz = 0.920, depth = 1.135, less = 0.955, pa = 1.090, pressure = 1.030),
                              "LAC2021" = c(blitz = 0.975, depth = 0.945, less = 0.920, pa = 0.925, pressure = 0.935))
rush_specs_neb <- rush_specs_from_df(rush_template_split(rush_template_neb, wr_name = "NE WR"))
cat("rush_specs_neb:", length(rush_specs_neb), "specs --", paste(names(rush_specs_neb), collapse = ", "), "\n")

# BUF rushing block, read off NE v BUF.xlsx TEMPLATE cols BC:BG (Andy's save 2026-10-03 20:20): row 43 (Allen, the QB row) + rows 31, 34,
# 37, 38. POS GRP -> QB / HB / REC. Knox (TE), Shakir and Moore carry one identical REC profile -> folded into "BUF WR" (REC = WR + TE in
# rush_stats_final). Kincaid (row 33) has no RUSH GRP -> no row.
rush_template_bufn <- tibble::tribble(
  ~rusher         , ~position_group, ~rush_grp, ~run_sit        , ~gap            , ~zone          , ~xtd,
  "Josh Allen"    , "QB"           , "B"      , "2 4 5"         , "2"             , "(.5, 99)"     , "55 TO 100",   # Andy 20:30: "JUST LAST WEEK'S ROW" (the sheet's 2 3 4 / 2 4 / 60-100 matched 7 games, floor 10)
  "James Cook"    , "HB"           , "A"      , "2"             , "5 6"           , "(0, 99)"      , "0 TO 35",
  "Ty Johnson"    , "HB"           , "C"      , "1 2 3 99 NA"   , "1 3 99 NA"     , "(-99, 0) NA"  , "15 TO 65",     # added from Andy's paste 2026-10-04 ("just in case")
  "Ray Davis"     , "HB"           , "C"      , "1 2 99 NA"     , "1 3 99 NA"     , "(.5, 99) NA"  , "0 TO 50",      # added from Andy's paste 2026-10-04
  "Dawson Knox"   , "REC"          , "C"      , "1 2 3 4 99 NA" , "1 2 3 4 99 NA" , "(-99, 99) NA" , "0 TO 45",
  "Khalil Shakir" , "REC"          , "C"      , "1 2 3 4 99 NA" , "1 2 3 4 99 NA" , "(-99, 99) NA" , "0 TO 45",
  "DJ Moore"      , "REC"          , "C"      , "1 2 3 4 99 NA" , "1 2 3 4 99 NA" , "(-99, 99) NA" , "0 TO 45"
)
# BUF offense: Allen 2025 and Watson 2018 against NE2025 -- book tab order (BUF v NE25, HST v NE25)
rush_matchups_bufn <- data.frame(qbgrp = c("BUFAllen-2025", "HSTWatson-2018"), defgrp = c("NE2025", "NE2025"), stringsAsFactors = FALSE)
# starting tolerances = Andy's pasted xtds rows for this game (2026-10-03), = comparison_engine_thresholds.R lines 362-367
rush_base_tol_qb_bufn  <- list("BUFAllen-2025"  = c(blitz = 0.955, depth = 1.005, less = 0.985, pa = 0.930, pressure = 0.975),
                               "HSTWatson-2018" = c(blitz = 0.980, depth = 0.995, less = 1.000, pa = 1.010, pressure = 1.005))
rush_base_tol_def_bufn <- list("NE2025" = c(blitz = 0.950, depth = 0.980, less = 1.010, pa = 0.985, pressure = 0.950))
rush_specs_bufn <- rush_specs_from_df(rush_template_split(rush_template_bufn, wr_name = "BUF WR"))
cat("rush_specs_bufn:", length(rush_specs_bufn), "specs --", paste(names(rush_specs_bufn), collapse = ", "), "\n")

# ---- JAX @ CIN (2026-10-03), JAX offense only ----------------------------------------------
# JAX rushing block as Andy pasted it 2026-10-03 ~20:30 (JAX v CIN.xlsx TEMPLATE). POS GRP -> QB / HB / REC. Tuten "A B" and Rodriguez "B C"
# are two-group backs -> one row per group. Abdullah's XTD was typed "0 T0 100 NA" (zero for O) -> read as "0 TO 100 NA" (flagged).
# Washington, Meyers and Thomas Jr. carry one identical REC profile -> folded into "JAX WR".
rush_template_jaxc <- tibble::tribble(
  ~rusher             , ~position_group, ~rush_grp, ~run_sit        , ~gap             , ~zone          , ~xtd,
  "Trevor Lawrence"   , "QB"           , "C"      , "1 2 99 NA"     , "2 4 99 NA"      , "(-99, 1)"     , "30 TO 100",
  "Bhayshul Tuten"    , "HB"           , "A B"    , "2 4 | 4"       , "1 7 8 | 1 2 3"  , "(-.25, 2)"    , "15 TO 55",
  "Chris Rodriguez"   , "HB"           , "B C"    , "4 5 | 1 2"     , "2 4 | 1 3"      , "(-99, 99)"    , "0 TO 50",
  "Ameer Abdullah"    , "HB"           , "C"      , "1 3 99 NA"     , "1 3 99 NA"      , "(-99, 0) NA"  , "0 TO 100 NA",
  "Parker Washington" , "REC"          , "C"      , "1 2 3 4 99 NA" , "2 99 NA"        , "(-99, 99) NA" , "65 TO 100 NA",
  "Jakobi Meyers"     , "REC"          , "C"      , "1 2 3 4 99 NA" , "2 99 NA"        , "(-99, 99) NA" , "65 TO 100 NA",
  "Brian Thomas Jr."  , "REC"          , "C"      , "1 2 3 4 99 NA" , "2 99 NA"        , "(-99, 99) NA" , "65 TO 100 NA"
)
# JAX offense: Lawrence 2025 against CIN2025 and PHI2016 (the Schwartz-family stand-in) -- book tab order (JAX v CIN25, JAX v PHI16)
rush_matchups_jaxc <- data.frame(qbgrp = c("JAXLawrence-2025", "JAXLawrence-2025"), defgrp = c("CIN2025", "PHI2016"), stringsAsFactors = FALSE)
# starting tolerances = Andy's pasted xtds rows for this game (2026-10-03), = comparison_engine_thresholds.R lines 341-346
rush_base_tol_qb_jaxc  <- list("JAXLawrence-2025" = c(blitz = 0.915, depth = 0.975, less = 0.945, pa = 0.940, pressure = 0.945))
rush_base_tol_def_jaxc <- list("CIN2025" = c(blitz = 1.025, depth = 1.105, less = 1.085, pa = 1.005, pressure = 0.995),
                               "PHI2016" = c(blitz = 1.095, depth = 1.045, less = 1.060, pa = 0.990, pressure = 0.955))
rush_specs_jaxc <- rush_specs_from_df(rush_template_split(rush_template_jaxc, wr_name = "JAX WR"))
cat("rush_specs_jaxc:", length(rush_specs_jaxc), "specs --", paste(names(rush_specs_jaxc), collapse = ", "), "\n")

# ---- 4. the profile match, once per rusher (rush_func lines 189-201) ---------
rush_hits <- function(sp) {
  rush_stats_final %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      rank_grp %in% sp$rank_grp_input &
        position_group %in% sp$position_group_input &
        situation_cluster %in% sp$situation_input &
        gap_cluster %in% sp$gap_input &
        ((gap_z >= sp$gap_z_range[1] & gap_z <= sp$gap_z_range[2]) | (sp$gap_z_NA & is.na(gap_z))) &
        ((xtd_percentile >= sp$xtd_vec_input[1] & xtd_percentile <= sp$xtd_vec_input[2]) | (sp$td_na & is.na(xtd_percentile)))) %>%
    dplyr::distinct(qbgrp_ssn, def_ssn, week, season)
}
rush_in_count <- function(h, qb_teams, def_teams) sum(h$qbgrp_ssn %in% qb_teams & h$def_ssn %in% def_teams)

# prints the chain's conditions for one spec, so it can be checked against rush_func in one look
rush_chain_receipt <- function(sp) {
  cat("rank_grp %in% ", paste(sp$rank_grp_input, collapse = "/"),
      " | position_group %in% ", paste(sp$position_group_input, collapse = "/"),
      " | situation_cluster %in% ", paste(ifelse(is.na(sp$situation_input), "NA", sp$situation_input), collapse = "/"),
      " | gap_cluster %in% ", paste(ifelse(is.na(sp$gap_input), "NA", sp$gap_input), collapse = "/"),
      " | gap_z in [", paste(sp$gap_z_range, collapse = ", "), "]", if (sp$gap_z_NA) " or NA" else "",
      " | xtd_percentile in [", paste(sp$xtd_vec_input, collapse = ", "), "]", if (sp$td_na) " or NA" else "", "\n", sep = "")
}

# ---- 5. the sweep (rec_sweep, re-keyed to the rushing chain) ----------------
rush_sweep <- function(specs, matchups, tol_qb, tol_def,
                       deltas = seq(0, 0.35, by = 0.005), lenses = names(stats_categories),
                       floor_fn = rush_floor, side = c("both", "qb", "def")) {
  side <- match.arg(side); dq <- if (side %in% c("both", "qb")) 1 else 0; dd <- if (side %in% c("both", "def")) 1 else 0
  if (!length(specs)) stop("specs is empty")
  if (!exists("rush_stats_final")) stop("rush_stats_final is not in session")
  need <- c("qbgrp_ssn", "def_ssn", "week", "season", "position_group", "rank_grp", "situation_cluster", "gap_cluster", "gap_z", "xtd_percentile")
  miss <- setdiff(need, names(rush_stats_final)); if (length(miss)) stop("rush_stats_final is missing: ", paste(miss, collapse = ", "))
  for (w in list(list(tab = tol_qb, keys = unique(matchups$qbgrp), lab = "QB", arg = "tol_qb"),
                 list(tab = tol_def, keys = unique(matchups$defgrp), lab = "DEF", arg = "tol_def"))) for (k in w$keys) {
                   v <- w$tab[[k]]
                   if (is.null(v)) stop("no tolerances for ", w$lab, " '", k, "' in ", w$arg, " -- it has: ", paste(names(w$tab), collapse = ", "))
                   ml <- setdiff(lenses, names(v)); if (length(ml)) stop(w$lab, " '", k, "' has no tolerance for lens: ", paste(ml, collapse = ", "))
                 }
  hits <- lapply(specs, rush_hits)
  cat("== profile-matched team-games per rusher (the ceiling no tolerance can beat) ==\n")
  for (nm in names(specs)) {
    fl <- floor_fn(specs[[nm]]$position_group, specs[[nm]]$rank_grp_input)
    cat(sprintf("   %-22s %5d  floor %2d%s\n", nm, nrow(hits[[nm]]), fl,
                if (nrow(hits[[nm]]) < fl) "  <- profile matches fewer games than the floor, unreachable at ANY tolerance" else ""))
  }
  pools <- new.env()
  pool_at <- function(qb, def, L, dl) {
    key <- paste(qb, def, L, dl, sep = "|")
    if (is.null(pools[[key]])) pools[[key]] <- list(
      qb  = c(stats_categories[[L]]$qb_func(qb,   tol_qb[[qb]][[L]]   + dq * dl)$QB, qb),
      def = c(stats_categories[[L]]$def_func(def, tol_def[[def]][[L]] + dd * dl)$QB, def))
    pools[[key]]
  }
  rows <- list()
  for (nm in names(specs)) {
    sp <- specs[[nm]]; fl <- floor_fn(sp$position_group, sp$rank_grp_input); h <- hits[[nm]]
    for (m in seq_len(nrow(matchups))) { qb <- matchups$qbgrp[m]; def <- matchups$defgrp[m]
    for (L in lenses) {
      base_in <- NA; chosen <- NA; in_at <- NA
      for (dl in deltas) { p <- pool_at(qb, def, L, dl); n <- rush_in_count(h, p$qb, p$def)
      if (dl == deltas[1]) base_in <- n
      in_at <- n; if (n >= fl) { chosen <- dl; break } }
      used <- if (is.na(chosen)) max(deltas) else chosen
      rows[[length(rows) + 1]] <- data.frame(rec = nm, qbgrp = qb, defgrp = def, lens = L, floor = fl, n_profile = nrow(h),
                                             in_at_base = base_in, delta = used, in_final = in_at, cleared = !is.na(chosen),
                                             qb_base = tol_qb[[qb]][[L]], qb_tol = tol_qb[[qb]][[L]] + dq * used,
                                             def_base = tol_def[[def]][[L]], def_tol = tol_def[[def]][[L]] + dd * used)
    } }
  }
  out <- dplyr::bind_rows(rows)
  cat("\n== rushers x matchups x lenses: smallest widening from the tuned base that clears the floor ==\n\n")
  print(as.data.frame(out %>% dplyr::select(rec, qbgrp, defgrp, lens, floor, n_profile, in_at_base, delta, in_final, cleared, qb_tol, def_tol)),
        row.names = FALSE, digits = 3)
  unreach <- unique(out$rec[out$n_profile < out$floor])
  need_tab <- out %>% dplyr::filter(!rec %in% unreach) %>% dplyr::group_by(qbgrp, defgrp, lens) %>%
    dplyr::summarise(qb_base = qb_base[1], def_base = def_base[1], max_delta = max(delta), binding = rec[which.max(delta)],
                     qb_needed = qb_base[1] + dq * max(delta), def_needed = def_base[1] + dd * max(delta),
                     n_widened = sum(delta > 0), n_uncleared = sum(!cleared), .groups = "drop")
  cat("\n== per matchup x lens: what the rush_func tolerance block has to be for EVERY rusher to clear (side = ", side, ") ==\n", sep = "")
  if (length(unreach)) cat("   EXCLUDED, profile matches nothing at any tolerance: ", paste(unreach, collapse = ", "), "\n")
  cat("\n"); print(as.data.frame(need_tab), row.names = FALSE, digits = 3)
  cat("\n# ---- paste block: same shape as stats_tol_qb / stats_tol_def ----\n")
  for (q in unique(need_tab$qbgrp)) { t <- need_tab[need_tab$qbgrp == q, ] %>% dplyr::group_by(lens) %>% dplyr::summarise(v = max(qb_needed), .groups = "drop")
  t <- t[match(lenses, t$lens), ]; cat(sprintf('"%s" = c(%s)\n', q, paste(sprintf("%s = %.3f", t$lens, t$v), collapse = ", "))) }
  for (d in unique(need_tab$defgrp)) { t <- need_tab[need_tab$defgrp == d, ] %>% dplyr::group_by(lens) %>% dplyr::summarise(v = max(def_needed), .groups = "drop")
  t <- t[match(lenses, t$lens), ]; cat(sprintf('"%s" = c(%s)\n', d, paste(sprintf("%s = %.3f", t$lens, t$v), collapse = ", "))) }
  invisible(out)
}

# usage (nothing above fires except building rush_specs_ne):
#   rush_specs_ne[["Rhamondre Stevenson"]]                                          # one parsed profile
#   rush_chain_receipt(rush_specs_ne[["Mack Hollins"]])                             # the spec in rush_func's argument names
#   sw <- rush_sweep(rush_specs_ne,  rush_matchups_ne,  stats_tol_qb, stats_tol_def)   # NE offense: engine's banked 3 x 2
#   sw <- rush_sweep(rush_specs_sea, rush_matchups_sea, stats_tol_qb, stats_tol_def)   # SEA offense: same tables, SEA entities banked above