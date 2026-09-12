# rec_thresholds_engine.R
# PROPOSED -- Claude, UNSIGNED. Andy stamps or corrects.
#
# The receiving analog of comparison_engine_thresholds.R. Per receiver and
# lens, finds the SMALLEST symmetric widening (delta) of the tuned QB/DEF
# tolerances at which that receiver's "In" count -- team-games in the
# QB-comp x DEF-comp both-bucket that contain a profile-matched receiver,
# exactly as rec_func counts `players` -- clears the receiver's floor.
#
# Base tolerances are the tuned ones (stats_tol_qb / stats_tol_def), NOT
# the hardcoded block inside rec_func (lines 172-176), which is a different
# set from a different matchup (pa def 1.32 there vs 0.97 tuned).
#
# The floor is by ROLE, which is the stated rule (10 for a star, 3 for a
# fifth receiver). The workbook data says the binding cells are NOT the
# stars -- see rec_floor_note below.
#
# Session needs: receiving_func_base, stats_categories (the engine), a
# tolerance table that carries every entity in the matchup frame, dplyr,
# tibble.

# ---- 1. the floor, by role. Andy's numbers, formalized. -------------------
rec_floor <- function(position_group, pos_rank) {
  if (position_group %in% c("WR", "TE", "BACK") && pos_rank == 1) return(10)
  if (position_group == "WR" && pos_rank %in% c(2, 3)) return(5)
  if (position_group == "TE" && pos_rank == 2) return(5)
  3
}


xtds_tol_qb <- list(
  "NEMaye-2025" = c(blitz = 0.995, depth = 0.960, less = 0.990, pa = 1.020, pressure = 1.035),
  "TENTannehill-2019" = c(blitz = 1.090, depth = 1.100, less = 0.995, pa = 0.980, pressure = 1.115),
  "DALPrescott-2025" = c(blitz = 0.970, depth = 0.965, less = 0.940, pa = 0.940, pressure = 0.940)
)

xtds_tol_def <- list(
  "SEA2025" = c(blitz = 0.995, depth = 1.110, less = 1.020, pa = 1.035, pressure = 1.005),
  "SEA2024" = c(blitz = 0.965, depth = 1.085, less = 0.955, pa = 1.005, pressure = 1.010)
)


# ---- 2. receiver specs. One template per side, cols 61-71 verbatim. A
#      spec is a PROFILE only -- it does not know its matchup. Matchups are
#      a separate (qbgrp, defgrp) frame so one profile set runs against
#      six pairs without six copies. Nothing read from disk.
rec_template_sea <- tibble::tribble(
  ~rec,                  ~position_group, ~pos_rank, ~team_rank, ~tgt_cluster,                   ~rte_cluster,               ~align_cluster,     ~man_zone_grp,                     ~man_z,         ~xpass,        ~xtd_grp,                     ~xtd,
  "Jadarian Price",      "BACK",          "2 TO 99", "6 TO 99",  "BT / RB / LT",                 "BT / RB / LR",             "RB",               "HB_SHORT / HB_DEEP / HB_LT / NA", "0 TO 60",      "0 TO 50 NA",  "TD_LOW / HB_LT / NA",        "0 TO 100",
  "George Holani",       "BACK",          "1 TO 2",  "4 TO 7",   "RB / G / SMT",                 "RB / SMT",                 "RB",               "HB_DEEP",                         "0 TO 40",      "50 TO 100",   "TD_LOW",                     "0 TO 70",
  "AJ Barner",           "TE",            "1 TO 1",  "1 TO 6",   "ST / SMT",                     "RB / ST / SMT",            "ITE / STE",        "TE_SHORT",                        "0 TO 100",     "0 TO 40",     "TD_HIGH",                    "0 TO 100",
  "Elijah Arroyo",       "TE",            "2 TO 2",  "4 TO 8",   "G / SMT / ML / DT / ST / LT",  "RB / ST / SMT / DT / LR",  "ITE / STE",        "TE_DEEP / TE_LT",                 "40 TO 100",    "0 TO 45 NA",  "TD_LOW / REC_LT",            "0 TO 100 NA",
  "Eric Saubert",        "TE",            "3 TO 99", "7 TO 99",  "ST / SMT / G / RB",            "RB / DT / BT / ST / LR",   "ITE",              "TE_SHORT / TE_LT",                "0 TO 40",      "0 TO 45 NA",  "TD_HIGH / REC_LT",           "40 TO 100 NA",
  "Jaxon Smith-Njigba",  "WR",            "1 TO 2",  "1 TO 3",   "G / ML / MT / DT",             "SMT / RB / DT",            "WWR / WSWR",       "WR_SHORT / WR_DEEP",              "55 TO 100",    "40 TO 90",    "TD_HIGH",                    "0 TO 100",
  "Cooper Kupp",         "WR",            "1 TO 2",  "1 TO 3",   "SMT (ML)",                     "SMT / RB",                 "WWR / WSWR / SWR", "WR_DEEP",                         "0 TO 65",      "40 TO 100",   "TD_LOW",                     "5 TO 45",
  "Rashid Shaheed",      "WR",            "1 TO 3",  "1 TO 5",   "DT / MT / ML / G",             "SMT / RB / DT",            "WWR / WSWR",       "WR_DEEP",                         "0 TO 80",      "35 TO 100",   "TD_HIGH",                    "40 TO 100",
  "Tory Horton",         "WR",            "3 TO 99", "6 TO 99",  "DT / MT / ML / LT",            "SMT / MT / DT / RB / LR",  "WWR / WSWR",       "WR_DEEP / WR_LT",                 "60 TO 100 NA", "0 TO 100 NA", "TD_HIGH / REC_LT",           "0 TO 100 NA",
  "Montorie Foster Jr.", "WR",            "4 TO 99", "8 TO 99",  "DT / MT / ML / LT",            "SMT / MT / DT / RB / LR",  "WWR / WSWR",       "WR_DEEP / WR_LT",                 "60 TO 100 NA", "0 TO 100 NA", "TD_HIGH / TD_LOW / REC_LT",  "0 TO 100 NA"
)

# NE rows: Andy's paste, 2026-09-07 (Kiner in the Henderson slot with his
# own profile; Gilliam's man-zone carries HB_LT / TE_LT / OTH_LT / NA).
rec_template_ne <- tibble::tribble(
  ~rec                 , ~position_group, ~pos_rank, ~team_rank, ~tgt_cluster        , ~rte_cluster        , ~align_cluster  , ~man_zone_grp                    , ~man_z        , ~xpass        , ~xtd_grp                      , ~xtd,
  "Rhamondre Stevenson", "BACK"         , "1 TO 1" , "1 TO 6"  , "BT / RB"           , "SMT / DT / RB"     , "RB"            , "HB_SHORT / HB_DEEP"             , "40 TO 100"   , "25 TO 85"    , "TD_LOW"                      , "0 TO 100",
  "Corey Kiner"        , "BACK"         , "2 TO 99", "6 TO 99" , "RB / ST / LT"      , "RB / ST / LR"      , "RB"            , "HB_SHORT / HB_DEEP / HB_LT / NA", "25 TO 100 NA", "0 TO 80 NA"  , "TD_LOW / HB_LT / NA"         , "0 TO 100 NA",
  "Reggie Gilliam"     , "BACK"         , "2 TO 99", "7 TO 99" , "DT / SMT / ST / LT", "DT / MT / RB / LR" , "RB / STE / SWR", "HB_LT / TE_LT / OTH_LT / NA"    , "0 TO 100 NA" , "0 TO 100 NA" , "HB_LT / OTH_LT / REC_LT / NA", "0 TO 100 NA",
  "Hunter Henry"       , "TE"           , "1 TO 1" , "1 TO 5"  , "SMT / ML"          , "SMT / MT / DT / RB", "STE"           , "TE_SHORT / TE_DEEP"                       , "0 TO 60"     , "20 TO 70"    , "TD_LOW / TD_HIGH"            , "50 TO 100",
  "Eli Raridon"        , "TE"           , "2 TO 2" , "5 TO 8"  , "ST / SMT"          , "ST / SMT"          , "ITE"           , "TE_DEEP / TE_SHORT"             , "0 TO 100"    , "0 TO 40"     , "TD_HIGH"                     , "0 TO 100",
  "AJ Brown"           , "WR"           , "1 TO 3" , "1 TO 5"  , "ML"                , "RB / ST / SMT / DT", "WWR"           , "WR_DEEP"                        , "45 TO 100"   , "15 TO 85"    , "TD_HIGH"                     , "30 TO 100",
  "Romeo Doubs"        , "WR"           , "1 TO 3" , "1 TO 5"  , "ML"                , "BT / DT / RB / SMT", "WWR"           , "WR_DEEP"                        , "0 TO 70"     , "30 TO 80"    , "TD_LOW / TD_HIGH"            , "20 TO 85",
  "Mack Hollins"       , "WR"           , "2 TO 4" , "3 TO 7"  , "MT / ML / DT"      , "SMT / MT / DT"     , "WSWR / WWR"    , "WR_DEEP / WR_LT"                , "0 TO 45"     , "0 TO 60"     , "TD_HIGH / REC_LT"            , "0 TO 65",
  "Demario Douglas"    , "WR"           , "2 TO 5" , "5 TO 10" , "G / ML / LT"       , "RB / DT"           , "WSWR / SWR"    , "WR_SHORT / WR_DEEP / WR_LT"     , "25 TO 100"   , "15 TO 100"   , "TD_LOW / REC_LT"             , "25 TO 100",
  "Kyle Williams"      , "WR"           , "4 TO 99", "7 TO 99" , "DT / MT / ML / LT" , "SMT / MT / DT / LR", "WWR"           , "WR_DEEP / WR_LT"                , "0 TO 100 NA" , "60 TO 100 NA", "TD_HIGH / REC_LT / NA"       , "0 TO 100 NA"
)

# the matchup frames. SEA offense sees one pair; NE offense sees the
# 3 x 2 grid the stats engine banks.
rec_matchups_sea <- data.frame(qbgrp = "SEADarnold-2025", defgrp = "NE2025", stringsAsFactors = FALSE)
rec_matchups_ne  <- expand.grid(qbgrp  = c("NEMaye-2025", "TENTannehill-2019", "DALPrescott-2025"),
                                defgrp = c("SEA2025", "SEA2024"),
                                stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)

# parser: "A TO B" -> c(A, B); trailing NA on a band sets the *_na flag;
# "X / Y / Z" -> c("X","Y","Z") with a literal NA token kept as NA_character_
# (rec_func's %in% matches NA rows that way); "SMT (ML)" -> c("SMT","ML").
# position_group passes through verbatim -- the base spells backs BACK, not
# RB/HB/FB (rec_trace, 2026-09-07). pos_rank for the floor is the band's
# lower bound. No qbgrp / defgrp in a spec: matchups are passed separately.
rec_specs_from_df <- function(df) {
  band   <- function(s) { s <- gsub("\\s*NA\\s*$", "", s); as.numeric(strsplit(s, " TO ")[[1]]) }
  has_na <- function(s) grepl("NA\\s*$", s)
  lst    <- function(s) { t <- trimws(unlist(strsplit(gsub("[()]", " / ", s), "/"))); t <- t[t != ""]
  ifelse(t == "NA", NA_character_, t) }
  specs <- list()
  for (i in seq_len(nrow(df))) {
    r <- df[i, ]
    specs[[r$rec]] <- list(
      position_group       = r$position_group,
      pos_rank             = band(r$pos_rank)[1],
      tgt_cluster_input    = lst(r$tgt_cluster),
      rte_cluster_input    = lst(r$rte_cluster),
      align_cluster_input  = lst(r$align_cluster),
      position_group_input = r$position_group,
      pos_rank_vec         = band(r$pos_rank),
      team_rank_vec        = band(r$team_rank),
      man_zone_grp_input   = lst(r$man_zone_grp),
      man_z_na             = has_na(r$man_z),
      man_z_vec_input      = band(r$man_z),
      xpass_na             = has_na(r$xpass),
      xpass_vec_input      = band(r$xpass),
      xtd_grp_input        = lst(r$xtd_grp),
      xtd_grp_na           = has_na(r$xtd),
      xtd_vec_input        = band(r$xtd))
  }
  specs
}

rec_specs_sea <- rec_specs_from_df(rec_template_sea)
rec_specs_ne  <- rec_specs_from_df(rec_template_ne)
cat("rec_specs_sea:", length(rec_specs_sea), "receivers x", nrow(rec_matchups_sea), "matchup  |  rec_specs_ne:",
    length(rec_specs_ne), "receivers x", nrow(rec_matchups_ne), "matchups\n")

# ---- 3. the profile match, computed ONCE per receiver ---------------------
# rec_ind (rec_func lines 182-213) depends only on the receiver's profile,
# never on a tolerance. So each receiver reduces to the set of team-games
# (qbgrp_ssn, def_ssn, week, season) containing a profile-matched receiver,
# and the In count at ANY tolerance is a membership test on that set.
# dplyr::filter drops NA rows the same way rec_func's ifelse -> NA -> not
# counted does.
# PROPOSED: the z_score_percentile line carries a man_z_na branch the way
# xpass and xtd do, because the template puts NA on Horton's and Foster's
# man_z band. rec_func's chain has no such branch; without it those NA rows
# fall out of the count. Andy rules whether rec_func gets the branch.
rec_hits <- function(sp) {
  receiving_func_base %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      tgt_cluster_name %in% sp$tgt_cluster_input &
        rte_cluster_name %in% sp$rte_cluster_input &
        align_cluster_name %in% sp$align_cluster_input &
        final_position_group %in% sp$position_group_input &
        pos_rank >= sp$pos_rank_vec[1] & pos_rank <= sp$pos_rank_vec[2] &
        team_rank >= sp$team_rank_vec[1] & team_rank <= sp$team_rank_vec[2] &
        man_zone_grp_cluster %in% sp$man_zone_grp_input &
        ((z_score_percentile >= sp$man_z_vec_input[1] & z_score_percentile <= sp$man_z_vec_input[2]) |
           (sp$man_z_na & is.na(z_score_percentile))) &
        ((xpass_percentile >= sp$xpass_vec_input[1] & xpass_percentile <= sp$xpass_vec_input[2]) |
           (sp$xpass_na & is.na(xpass_percentile))) &
        td_grp_cluster %in% sp$xtd_grp_input &
        ((xtd_percentile >= sp$xtd_vec_input[1] & xtd_percentile <= sp$xtd_vec_input[2]) |
           (sp$xtd_grp_na & is.na(xtd_percentile)))) %>%
    dplyr::distinct(qbgrp_ssn, def_ssn, week, season)
}

rec_in_count <- function(h, qb_teams, def_teams)
  sum(h$qbgrp_ssn %in% qb_teams & h$def_ssn %in% def_teams)

# ---- 4. the sweep --------------------------------------------------------
# Per receiver x matchup x lens: the smallest symmetric widening of the
# tuned QB/DEF tolerances at which the In count clears the role floor.
# deltas run to +0.35 so an insane answer prints as the number (1.32),
# not as "cap".
#
# tol_qb / tol_def must carry every entity in `matchups`. For the SEA side
# that is the tuner tail's xtds_tol_qb / xtds_tol_def; for the NE side it
# is the engine's banked stats_tol_qb / stats_tol_def (NEMaye / Tannehill /
# Prescott x SEA2025 / SEA2024). The wall names whatever is missing.
#
# floor_fn: the role floor, default rec_floor. side: PROPOSED. "both"
# widens QB and DEF by the same delta; "qb" / "def" widen one side and
# hold the other at its tuned base.
rec_sweep <- function(specs, matchups, tol_qb, tol_def,
                      deltas = seq(0, 0.35, by = 0.005), lenses = names(stats_categories),
                      floor_fn = rec_floor, side = c("both", "qb", "def")) {
  side <- match.arg(side)
  dq <- if (side %in% c("both", "qb"))  1 else 0
  dd <- if (side %in% c("both", "def")) 1 else 0
  if (!length(specs)) stop("specs is empty")
  if (!all(c("qbgrp", "defgrp") %in% names(matchups)) || !nrow(matchups))
    stop("matchups needs columns qbgrp, defgrp and at least one row")
  if (!exists("receiving_func_base")) stop("receiving_func_base is not in session")
  need <- c("qbgrp_ssn", "def_ssn", "week", "season", "tgt_cluster_name", "rte_cluster_name",
            "align_cluster_name", "final_position_group", "pos_rank", "team_rank",
            "man_zone_grp_cluster", "z_score_percentile", "xpass_percentile",
            "td_grp_cluster", "xtd_percentile")
  miss <- setdiff(need, names(receiving_func_base))
  if (length(miss)) stop("receiving_func_base is missing: ", paste(miss, collapse = ", "))
  
  # wall: every entity in the matchup frame must have a tolerance vector
  # covering every lens, or this stops HERE with the name
  for (w in list(list(tab = tol_qb, keys = unique(matchups$qbgrp),  lab = "QB",  arg = "tol_qb"),
                 list(tab = tol_def, keys = unique(matchups$defgrp), lab = "DEF", arg = "tol_def"))) {
    for (k in w$keys) {
      v <- w$tab[[k]]
      if (is.null(v))
        stop("no tolerances for ", w$lab, " '", k, "' in ", w$arg, " -- it has: ",
             paste(names(w$tab), collapse = ", "),
             ". SEA side: xtds_tol_qb / xtds_tol_def (tuner tail). NE side: stats_tol_qb / stats_tol_def (engine bank).")
      ml <- setdiff(lenses, names(v))
      if (length(ml)) stop(w$lab, " '", k, "' has no tolerance for lens: ", paste(ml, collapse = ", "))
    }
  }
  
  # one profile match per receiver; the count is a membership test from here
  hits <- lapply(specs, rec_hits)
  cat("== profile-matched team-games per receiver (the ceiling no tolerance can beat) ==\n")
  for (nm in names(specs)) {
    fl <- floor_fn(specs[[nm]]$position_group, specs[[nm]]$pos_rank)
    cat(sprintf("   %-22s %5d  floor %2d%s\n", nm, nrow(hits[[nm]]), fl,
                if (nrow(hits[[nm]]) < fl) "  <- profile matches fewer games than the floor, unreachable at ANY tolerance" else ""))
  }
  
  # comparison lookups depend only on (entity, tol): cache once per
  # matchup x lens x delta, filled lazily as the sweep climbs
  pools <- new.env()
  pool_at <- function(qb, def, L, dl) {
    key <- paste(qb, def, L, dl, sep = "|")
    if (is.null(pools[[key]]))
      pools[[key]] <- list(
        qb  = c(stats_categories[[L]]$qb_func(qb,   tol_qb[[qb]][[L]]   + dq * dl)$QB, qb),
        def = c(stats_categories[[L]]$def_func(def, tol_def[[def]][[L]] + dd * dl)$QB, def))
    pools[[key]]
  }
  
  rows <- list()
  for (nm in names(specs)) {
    sp <- specs[[nm]]; fl <- floor_fn(sp$position_group, sp$pos_rank); h <- hits[[nm]]
    for (m in seq_len(nrow(matchups))) {
      qb <- matchups$qbgrp[m]; def <- matchups$defgrp[m]
      for (L in lenses) {
        base_in <- NA; chosen <- NA; in_at <- NA
        for (dl in deltas) {
          p <- pool_at(qb, def, L, dl)
          n <- rec_in_count(h, p$qb, p$def)
          if (dl == deltas[1]) base_in <- n
          in_at <- n
          if (n >= fl) { chosen <- dl; break }
        }
        used <- if (is.na(chosen)) max(deltas) else chosen
        rows[[length(rows) + 1]] <- data.frame(
          rec = nm, qbgrp = qb, defgrp = def, lens = L, floor = fl,
          n_profile = nrow(h), in_at_base = base_in, delta = used, in_final = in_at,
          cleared = !is.na(chosen),
          qb_base = tol_qb[[qb]][[L]], qb_tol = tol_qb[[qb]][[L]] + dq * used,
          def_base = tol_def[[def]][[L]], def_tol = tol_def[[def]][[L]] + dd * used)
      }
    }
  }
  out <- dplyr::bind_rows(rows)
  
  cat("\n== receivers x matchups x lenses: smallest widening from the tuned base that clears the floor ==\n")
  cat("   (delta 0 = tuned base already clears; cleared = FALSE = not even at +", max(deltas), ")\n\n", sep = "")
  print(as.data.frame(out %>% dplyr::select(rec, qbgrp, defgrp, lens, floor, n_profile, in_at_base, delta,
                                            in_final, cleared, qb_tol, def_tol)),
        row.names = FALSE, digits = 3)
  
  # a profile that matches fewer team-games than its floor is a profile
  # problem, not a tolerance problem -- it stays OUT of the per-lens max
  unreach <- unique(out$rec[out$n_profile < out$floor])
  need_tab <- out %>%
    dplyr::filter(!rec %in% unreach) %>%
    dplyr::group_by(qbgrp, defgrp, lens) %>%
    dplyr::summarise(qb_base = qb_base[1], def_base = def_base[1],
                     max_delta = max(delta), binding = rec[which.max(delta)],
                     qb_needed = qb_base[1] + dq * max(delta), def_needed = def_base[1] + dd * max(delta),
                     n_widened = sum(delta > 0), n_uncleared = sum(!cleared), .groups = "drop")
  
  cat("\n== per matchup x lens: what the rec_func tolerance block has to be for EVERY receiver to clear ==\n")
  cat("   (tuned base + the largest delta any receiver needed; binding = who needed it; side = ", side, ")\n", sep = "")
  if (length(unreach))
    cat("   EXCLUDED, profile matches nothing at any tolerance -- run rec_trace() on: ",
        paste(unreach, collapse = ", "), "\n")
  cat("\n")
  print(as.data.frame(need_tab), row.names = FALSE, digits = 3)
  
  # paste block: per entity, max across the matchups it appears in (a QB
  # that faces two defenses takes the wider of the two -- surplus is free)
  cat("\n# ---- paste block: same shape as stats_tol_qb / stats_tol_def ----\n")
  for (q in unique(need_tab$qbgrp)) {
    t <- need_tab[need_tab$qbgrp == q, ] %>% dplyr::group_by(lens) %>% dplyr::summarise(v = max(qb_needed), .groups = "drop")
    t <- t[match(lenses, t$lens), ]
    cat(sprintf('"%s" = c(%s)\n', q, paste(sprintf("%s = %.3f", t$lens, t$v), collapse = ", ")))
  }
  for (d in unique(need_tab$defgrp)) {
    t <- need_tab[need_tab$defgrp == d, ] %>% dplyr::group_by(lens) %>% dplyr::summarise(v = max(def_needed), .groups = "drop")
    t <- t[match(lenses, t$lens), ]
    cat(sprintf('"%s" = c(%s)\n', d, paste(sprintf("%s = %.3f", t$lens, t$v), collapse = ", ")))
  }
  invisible(out)
}

# ---- 5. why does a profile match nothing -----------------------------------
# rec_trace: each condition of the profile, applied ALONE to the whole base
# (how many rows it keeps by itself) and CUMULATIVELY in rec_func order (how
# many survive so far). The first cumulative zero is the killer; an alone
# zero is a value the base does not contain at all. which() drops NA the
# way dplyr::filter does.
rec_trace <- function(nm, specs = rec_specs_sea) {
  sp <- specs[[nm]]
  if (is.null(sp)) stop("no spec named '", nm, "' -- names are: ", paste(names(specs), collapse = ", "))
  d <- dplyr::ungroup(receiving_func_base)
  conds <- list(
    tgt       = function(x) x$tgt_cluster_name %in% sp$tgt_cluster_input,
    rte       = function(x) x$rte_cluster_name %in% sp$rte_cluster_input,
    align     = function(x) x$align_cluster_name %in% sp$align_cluster_input,
    pos_grp   = function(x) x$final_position_group %in% sp$position_group_input,
    pos_rank  = function(x) x$pos_rank >= sp$pos_rank_vec[1] & x$pos_rank <= sp$pos_rank_vec[2],
    team_rank = function(x) x$team_rank >= sp$team_rank_vec[1] & x$team_rank <= sp$team_rank_vec[2],
    man_zone  = function(x) x$man_zone_grp_cluster %in% sp$man_zone_grp_input,
    man_z     = function(x) (x$z_score_percentile >= sp$man_z_vec_input[1] & x$z_score_percentile <= sp$man_z_vec_input[2]) |
      (sp$man_z_na & is.na(x$z_score_percentile)),
    xpass     = function(x) (x$xpass_percentile >= sp$xpass_vec_input[1] & x$xpass_percentile <= sp$xpass_vec_input[2]) |
      (sp$xpass_na & is.na(x$xpass_percentile)),
    xtd_grp   = function(x) x$td_grp_cluster %in% sp$xtd_grp_input,
    xtd       = function(x) (x$xtd_percentile >= sp$xtd_vec_input[1] & x$xtd_percentile <= sp$xtd_vec_input[2]) |
      (sp$xtd_grp_na & is.na(x$xtd_percentile)))
  asks <- list(
    tgt = sp$tgt_cluster_input, rte = sp$rte_cluster_input, align = sp$align_cluster_input,
    pos_grp = sp$position_group_input, pos_rank = sp$pos_rank_vec, team_rank = sp$team_rank_vec,
    man_zone = sp$man_zone_grp_input, man_z = sp$man_z_vec_input, xpass = sp$xpass_vec_input,
    xtd_grp = sp$xtd_grp_input, xtd = sp$xtd_vec_input)
  cat("== ", nm, " -- rows kept by each condition, alone and cumulative (base ", nrow(d), ") ==\n", sep = "")
  cat(sprintf("   %-10s %9s %11s   %s\n", "condition", "alone", "cumulative", "asks for"))
  cum <- d
  for (k in names(conds)) {
    alone <- sum(conds[[k]](d), na.rm = TRUE)
    cum   <- cum[which(conds[[k]](cum)), ]
    cat(sprintf("   %-10s %9d %11d   %s%s\n", k, alone, nrow(cum),
                paste(ifelse(is.na(asks[[k]]), "NA", asks[[k]]), collapse = " / "),
                if (alone == 0) "   <- base contains none of these" else ""))
  }
  invisible(cum)
}

# rec_base_values: what the base actually holds in the label columns, for
# the rows that align where the profile aligns. Read next to the trace: the
# ask that keeps zero rows alone is spelled differently here.
rec_base_values <- function(align = "RB") {
  d <- dplyr::ungroup(receiving_func_base)
  d <- d[which(d$align_cluster_name %in% align), ]
  cat("== base values where align_cluster_name in ", paste(align, collapse = "/"), " (", nrow(d), " rows) ==\n", sep = "")
  for (cl in c("final_position_group", "tgt_cluster_name", "rte_cluster_name",
               "man_zone_grp_cluster", "td_grp_cluster")) {
    t <- sort(table(d[[cl]], useNA = "ifany"), decreasing = TRUE)
    cat(sprintf("   %-22s %s\n", cl, paste(sprintf("%s=%d", ifelse(is.na(names(t)), "NA", names(t)), t), collapse = "  ")))
  }
  cat(sprintf("   %-22s pos_rank %g..%g  team_rank %g..%g\n", "ranks",
              min(d$pos_rank, na.rm = TRUE), max(d$pos_rank, na.rm = TRUE),
              min(d$team_rank, na.rm = TRUE), max(d$team_rank, na.rm = TRUE)))
}

# usage (nothing above fires except building the two spec lists):
#   sw <- rec_sweep(rec_specs_sea, rec_matchups_sea, tol_qb = xtds_tol_qb,  tol_def = xtds_tol_def)   # SEA offense, 1 matchup
#   sw <- rec_sweep(rec_specs_ne,  rec_matchups_ne,  tol_qb = stats_tol_qb, tol_def = stats_tol_def)  # NE offense, 6 matchups
#   sw %>% filter(delta > 0)                              # only the cells that needed it
#   sw %>% filter(!cleared)                               # not even at the cap
#   rec_specs_ne[["AJ Brown"]]                            # one parsed profile, eyeball it
#   rec_trace("Reggie Gilliam", rec_specs_ne)             # which condition zeroes a profile
#   rec_base_values("RB")                                 # what the base spells those labels (align RB = backs)
#   rec_sweep(rec_specs_ne, rec_matchups_ne, stats_tol_qb, stats_tol_def, side = "def")   # widen SEA only
#
# rec_floor_note: from the NE_v_SEA workbook at rec_func's hardcoded
# tolerances, the thin In cells are Arroyo less=3, Barner depth=6,
# Douglas depth/pressure=9, K.Williams depth=9, Hooper blitz/pressure=9.
# Stars sit at 11-27 (Shaheed pa=11, JSN pressure=12, Kupp 18, Diggs 27).
# Westover/Walker/Saubert run 94-199. The binding receivers are TE1s and
# WR3-4s with specific profiles, not the stars and not the fifth guys.

sw <- rec_sweep(rec_specs_ne,  rec_matchups_ne,  tol_qb = stats_tol_qb, tol_def = stats_tol_def)
