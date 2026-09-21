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
  if (any(is.na(v) & t != "NA")) stop("cluster list not integers / NA: '", s, "'"); v }
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

rush_specs_ne  <- rush_specs_from_df(rush_template_ne)
rush_specs_sea <- rush_specs_from_df(rush_template_sea)
rush_specs_jax <- rush_specs_from_df(rush_template_split(rush_template_jax, wr_name = "JAX WR"))
cat("rush_specs_ne:", length(rush_specs_ne), "rushers --", paste(names(rush_specs_ne), collapse = ", "), "\n")
cat("rush_specs_sea:", length(rush_specs_sea), "rushers --", paste(names(rush_specs_sea), collapse = ", "), "\n")
cat("rush_specs_jax:", length(rush_specs_jax), "specs --", paste(names(rush_specs_jax), collapse = ", "), "\n")

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