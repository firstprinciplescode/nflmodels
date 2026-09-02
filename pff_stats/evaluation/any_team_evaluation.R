# ============================================================
# TEAM REPORT -- one code in, the whole answer out.
# Andy, 2026-08-23: "for one team - it's an easy peasy lemon
# squeezy. like I insert 'SF' and i get something back" --
# "and ultimately I can't see the players from last year."
#
# WHAT THIS IS: a VIEW layer over the finished pipeline. The
# seven availability layers already ran their walls; every
# frame this file reads is league-wide. Nothing here derives
# anything new -- it filters, joins on verified keys, and
# prints. No canon object is touched, no stamp is re-ruled.
#
#   team_slate("SF")    -- SF's 2026 opponent units: faced
#                          2025 -> healthy 2026 -> priced 2026,
#                          one row per unit, all seven. Console
#                          board + a gt table in the secondary
#                          unit's zone-table shape.
#   team_own("SF")      -- SF's own players, before and after
#                          injury pricing (the mirror table,
#                          any team) + who moved in and out +
#                          THE AGGREGATE: one row per unit,
#                          before vs after, and the ALL UNITS
#                          bottom line.
#   team_yoy("SF")      -- SF's OWN roster, 2025 vs 2026 (Andy,
#                          2026-08-23: "I WANT NE 2025 VS NE
#                          2026. REAL PLAYING TIME, ADJUSTED
#                          PLAYING TIME VS NE 2026 REAL PLAYING
#                          TIME, ADJUSTED PLAYING TIME"): one
#                          row per unit; 2025 = real playing
#                          time (weighted by what they actually
#                          played), 2026 = projected playing
#                          time (misses priced to the backup
#                          level); raw + adjusted each year,
#                          deltas where both years carry the
#                          currency.
#   team_sched("SF")    -- the opponent SET: 2025 opponents
#                          vs 2026 opponents, twice-or-once,
#                          with the back/new/gone diff done
#                          for you (no week-by-week -- the
#                          aggregate is fine).
#   team_lastyear("SF") -- last year's players (2025), per
#                          unit, with their 2025 values and
#                          where they are now + the retention
#                          math (no more is/isn't-here
#                          eyeballing) + a gt board with
#                          departures in red, printed last.
#   team_report("SF")   -- all five: slate, sched, lastyear,
#                          yoy, own. The aggregate gt prints
#                          LAST (viewer law: the viewer ends on
#                          the bottom line; every gt persists
#                          as an object).
#
# DIRECTION, Block 1: every row is the OPPONENTS' unit. Higher
# = harder for YOUR team, whether you bring the ball or they
# do. delta = priced 2026 minus faced 2025, in percentile
# points. Positive = the slate got harder than last year.
#
# DIRECTION, Block 2: OUR side (whoever "we" are this call).
# after - before < 0 = injuries cost us. Colors flip from the
# opponent tables: red = the team loses quality.
#
# TEAM CODES (the AZ/ARZ law): canon frames ride PFF spellings
# (ARZ, LA, ...); the nflreadr roster route rides its own --
# this build says 'AZ' where canon says 'ARZ' (Andy's console,
# 2026-08-23: Budda Baker '-> AZ' while still a Cardinal, and
# team_own('ARZ') silently lost Receiving + Pass rush, the two
# units whose member frames are built through the roster
# route). The code bridge below is DERIVED from shared names,
# never hard-coded; enter either spelling, get the right team.
#
# LAWS CARRIED: PRINT LAW (plain English headers) | CROSSED-
# STREAMS LAW (reads unit-unique frames only, writes only
# _tr-suffixed names) | FULL-PRECISION LAW (no rounding in
# frames; x100 display happens at print) | COLUMN CONTRACTS
# (every consumed column is checked against its frame before a
# unit builds; a break prints frame + column and skips the
# unit, never the silent '--' costume) | gates print what is
# missing and SKIP the unit -- one absent unit never kills the
# report, inside team_own too | ASCII only | ONE SESSION LAW:
# re-source WHOLE. The viewer library()s dplyr/tidyr/gt so a
# bare session sources clean, same as the layers all season.
# The no-library law is on the old record; asked for a ruling
# 2026-08-23, Andy answered NO PREFERENCE -- the season-long
# working form stands, recorded, never forged as a signature.
# One word from him reverses it.
#
# SOURCE ORDER: the full pipeline first (all canons + all
#   seven availability layers, each green through its walls),
#   THEN this file.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(gt)
})

# ------------------------------------------------------------
# 0. GATES + HELPERS -- block-level requirements; a missing
#    frame prints its name and the block is skipped, never
#    silent, never fatal to the other blocks.
# ------------------------------------------------------------

cl_tr <- function(x) gsub("[^A-Z]", "", toupper(x))

# scalar helpers for the slate board: a swept value that is
# missing or multi-row becomes NA (prints as "--"), never dies
pick1_tr <- function(x) if (length(x) == 1) x else NA_real_
col_or_na_tr <- function(df, candidates) {
  for (cn in candidates) if (cn %in% names(df)) return(df[[cn]])
  NA_real_
}

# safe means for the viewer: an all-NA (or all-zero-weight)
# slice becomes NA (prints as "--"), never NaN, never dies
m_tr <- function(x) if (all(is.na(x))) NA_real_ else
  mean(x, na.rm = TRUE)
wm_tr <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}
wfill_tr <- function(x, w = NULL)
  if (is.null(w)) m_tr(x) else wm_tr(x, w)

# find the canon frame for a year/currency. Names are wired
# VERBATIM from the seven league_*_evaluating_currency_three
# files (Andy, 2026-08-23): prush_c3_pctl, rd_c3_pctl,
# ru_c3_pctl, pblk_c3_pctl, cov_c3_pctl, rec_c3_pctl,
# rblk_c3_pctl; raw receiving = rec_season_pctl_sos (wide
# man_/zone_ columns). The one unrecorded name: the Phase-1
# OL raw canon (run-block raw 2025) -- the currency file
# itself ships c1_obj_rblk = NULL, so that cell stays '--'
# until Andy names it. Prints which frame rode, or what was
# tried. NEVER fills silently.
# ROUND THREE (Andy, 2026-08-30): run block's scheme split is
# wired -- the own-table rows and the yoy / lastyear boards
# prefer gap_c3_pctl / zone_c3_pctl (2025 side, rblk_c3_pctl)
# and V_c3_gap(_av) / V_c3_zone(_av) (2026 side,
# slot_value_26_rb_build). Old frames in session -> blended
# fallback + a printed note, never a silent fill.
frame_pick_tr <- function(label, cands, cols) {
  for (fn in cands) {
    if (exists(fn) && all(cols %in% names(get(fn)))) {
      cat("   [", label, "] riding ", fn, "\n", sep = "")
      return(fn)
    }
  }
  cat("   [", label, "] none of: ",
      paste(cands, collapse = ", "),
      " -- cell stays '--'\n", sep = "")
  NA_character_
}
vpick_tr <- function(df, cands) {
  for (cn in cands) if (cn %in% names(df)) return(df[[cn]])
  NULL
}
wpick_tr <- function(df) vpick_tr(df, c("tps", "run_snaps",
                                        "pb_snaps", "atts",
                                        "q_snaps", "snaps",
                                        "qs", "sn", "q_atts",
                                        "g_snaps", "routes",
                                        "targets", "usage_w"))

have_tr <- function(objs) {
  miss <- objs[!vapply(objs, exists, logical(1))]
  if (length(miss)) {
    cat("   skipped -- session is missing:\n")
    print(miss)
    FALSE
  } else TRUE
}

# COLUMN CONTRACTS (the Claude-audit law, adopted): a unit
# builds only if its frame exists AND carries every column the
# block reads. A renamed or dropped column prints frame +
# missing names and skips the unit -- a broken contract never
# wears the '--' costume; the legend reserves '--' for canon
# boundaries, not bugs.
gate_tr <- function(frame, cols) {
  if (!exists(frame)) {
    cat("   skipped -- missing frame:", frame, "\n")
    return(FALSE)
  }
  miss <- cols[!cols %in% names(get(frame))]
  if (length(miss)) {
    cat("   skipped --", frame, "is missing columns:\n")
    print(miss)
    return(FALSE)
  }
  TRUE
}

# team code: any case, either vocabulary. 'AZ' and 'ARZ' are
# the same Cardinals -- the input is translated to the canon
# spelling; roster-route frames translate back via
# ros_code_tr(). Unknown codes stop with the valid list.
norm_code_tr <- function(code) {
  raw <- toupper(trimws(code))
  # TRANSLATE FIRST, validate second (Claude's regression
  # catch, 2026-08-23): a valid pool that ingests the roster-
  # riding frames (members_pa/members_rc) lets a roster
  # spelling skip translation and walk into the canon-keyed
  # filters as-is -- a half-empty report, no error. So the
  # bridge translates FIRST (identity for canon input), then
  # the membership check runs against CANON-side codes only:
  # exactly the frames the blocks filter on cd directly.
  cd <- pff_code_tr(raw)
  pool <- character(0)
  for (fn in c("sweep_ra", "sweep_pa", "sweep_unit_cv",
               "sweep_rc", "sweep_ru", "sweep_avail_pb",
               "sweep_avail_rb", "members_ra", "members_cv",
               "memb_lg_ru", "slot_value_26_pb_build",
               "slot_value_26_rb_build")) {
    if (exists(fn)) {
      d <- get(fn)
      cc <- intersect(c("focal", "team", "team_name"), names(d))
      if (length(cc)) pool <- c(pool, as.character(d[[cc[1]]]))
    }
  }
  valid <- sort(unique(pool))
  if (!length(valid))
    stop("no frames in session to learn team codes from -- ",
         "run the pipeline first")
  if (!cd %in% valid) {
    cat("valid team codes (canon spelling):\n"); print(valid)
    stop("unknown team code -- pick from the list above")
  }
  if (cd != raw &&
      (!exists("note_done_tr") || !isTRUE(note_done_tr))) {
    cat("code note: '", raw, "' is the roster-route spelling;",
        " canon says '", cd, "'. Translated, same team.\n",
        sep = "")
    note_done_tr <<- TRUE
  }
  cd
}

# no pipeline in session = nothing to view; say so in English
check_pipeline_tr <- function() {
  if (!any(vapply(c("sweep_ra", "members_ra", "members_pa"),
                  exists, logical(1)))) {
    stop("no pipeline frames in session -- run the canons and ",
         "the seven availability layers before this file")
  }
}

# ------------------------------------------------------------
# the CODE BRIDGE. Derived, never hard-coded: match 2026
# member names (canon side) to 2026 roster names (roster
# side); the modal code pair per team IS the vocabulary --
# movers pair their NEW team's two codes correctly, so only
# same-name-different-player collisions are noise, and the
# modal vote beats them. Divergent pairs print on first use.
# ------------------------------------------------------------
code_bridge_tr <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    mp <- tibble(pff = character(0), ros = character(0),
                 n = integer(0))
    ros <- tryCatch(
      nflreadr::load_rosters(2026) %>%
        transmute(nm = cl_tr(full_name), ros = team) %>%
        filter(!is.na(nm), !is.na(ros)) %>% distinct(),
      error = function(e) NULL)
    can <- NULL
    for (fn in c("members_ra", "members_cv", "memb_lg_ru")) {
      if (exists(fn)) {
        d <- get(fn)
        if (all(c("roster_name", "team") %in% names(d)))
          can <- bind_rows(can, d %>% transmute(
            nm = cl_tr(roster_name), pff = team))
      }
    }
    if (!is.null(ros) && !is.null(can) && nrow(can)) {
      mp <- inner_join(can %>% distinct(nm, pff),
                       ros %>% distinct(nm, ros), by = "nm") %>%
        count(pff, ros) %>% group_by(pff) %>%
        slice_max(n, n = 1, with_ties = FALSE) %>% ungroup()
      dv <- mp %>% filter(pff != ros)
      if (nrow(dv)) {
        cat("code bridge, canon <-> roster route",
            "(n = names voting):\n")
        print(as.data.frame(dv))
      }
    }
    cache <<- mp
    mp
  }
})

# roster-route spelling of a canon code (identity if unbridged)
ros_code_tr <- function(cd) {
  mp <- code_bridge_tr()
  hit <- mp$ros[mp$pff == cd]
  if (length(hit)) hit[1] else cd
}
# canon spelling of a roster-route code (identity if unbridged)
pff_code_tr <- function(rc) {
  mp <- code_bridge_tr()
  hit <- mp$pff[mp$ros == rc]
  if (length(hit)) hit[1] else rc
}
# a frame can ride either spelling; use the one it carries
code_in_tr <- function(vals, cd) {
  if (cd %in% vals) return(cd)
  alt <- ros_code_tr(cd)
  if (alt %in% vals) return(alt)
  # THIRD spelling (Andy's probe, 2026-08-30): members_rc /
  # members_pa carry AZ, but the name-vote bridge only learns
  # ARZ <-> ARI from its training frames. Without this hop ARZ
  # filters to zero rows in those frames and the unit prints
  # '--' (the receiving / pass-rush 2026 holes on the ARZ yoy
  # board). Scoped to the ARZ family -- the probe showed every
  # other team agrees across feeds.
  fam <- c("ARZ", "ARI", "AZ")
  if (cd %in% fam) {
    hit <- intersect(fam, vals)
    if (length(hit)) return(hit[1])
  }
  cd
}

# 2026 whereabouts of a 2025 player: roster route, cleaned-name
# match (the churn-board law). status = still here / -> TEAM /
# no 2026 roster.
status26_tr <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      ros <- nflreadr::load_rosters(2026)
      cache <<- setNames(ros$team, cl_tr(ros$full_name))
    }
    cache
  }
})
where_now_tr <- function(player_names, home_code) {
  lk <- status26_tr()
  dest <- unname(lk[cl_tr(player_names)])
  home_ros <- ros_code_tr(home_code)  # dests are roster codes
  dplyr::case_when(
    is.na(dest)     ~ "no 2026 roster",
    dest == home_ros ~ "still here",
    TRUE            ~ paste0("-> ", dest))
}

cat("\n== TEAM REPORT VIEWS loaded ==\n")
cat("team_report(\"SF\") runs all five blocks. Pieces:\n")
cat("team_slate() | team_sched() | team_lastyear() |",
    "team_yoy() | team_own()\n")

# ------------------------------------------------------------
# 1. TEAM_SLATE -- the focal's 2026 opponent units, one row
#    per unit: faced 2025 -> healthy 2026 -> priced 2026, raw
#    and adjusted. Every row is the OTHER guys' unit; higher
#    = harder for you. Reads the seven walled sweep frames.
# ------------------------------------------------------------

team_slate <- function(code) {
  cd <- norm_code_tr(code)
  cat("\n=====================================================\n")
  cat(" ", cd, "2026 OPPONENT SLATE -- before and after the\n")
  cat("  injury bill. Every row = the opponents' unit.\n")
  cat("  Higher = harder for", cd, ". Deltas in percentile\n")
  cat("  points: priced 2026 minus faced 2025.\n")
  cat("======================================================\n")
  
  rows_tr <- list()
  
  # -- opponents' RUN DEFENSE (you're on offense) --
  if (gate_tr("sweep_ra", c("focal", "grade_25", "grade_26",
                            "grade_26p", "c3_25f", "c3_26p",
                            "fill_share_25"))) {
    r <- sweep_ra %>% filter(focal == cd)
    if (nrow(r) == 1) rows_tr$rd <- tibble(
      unit = "Run defense", side = "your O",
      faced25 = r$grade_25, healthy26 = r$grade_26,
      priced26 = r$grade_26p,
      adj_faced25 = r$c3_25f, adj_priced26 = r$c3_26p,
      fill25 = r$fill_share_25,
      interp26 = if (exists("slate_rd_lg") &&
                     all(c("focal", "interp_share") %in%
                         names(slate_rd_lg)))
        pick1_tr(slate_rd_lg$interp_share[slate_rd_lg$focal ==
                                            cd]) else NA_real_)
  }
  # -- opponents' PASS RUSH (you're on offense) --
  if (gate_tr("sweep_pa", c("focal", "g25", "v26", "v26p",
                            "a25", "v26ap", "fill_share",
                            "prior_share"))) {
    r <- sweep_pa %>% filter(focal == cd)
    if (nrow(r) == 1) rows_tr$pa <- tibble(
      unit = "Pass rush", side = "your O",
      faced25 = r$g25, healthy26 = r$v26, priced26 = r$v26p,
      adj_faced25 = r$a25, adj_priced26 = r$v26ap,
      fill25 = r$fill_share, interp26 = r$prior_share)
  }
  # -- opponents' SECONDARY, both lenses (you're on offense)
  if (gate_tr("sweep_unit_cv", c("focal", "split", "unit_25",
                                 "healthy_u", "priced_u",
                                 "priced_a_u", "fill_share_25",
                                 "interp_raw"))) {
    for (sp in c("man", "zone")) {
      r <- sweep_unit_cv %>% filter(focal == cd, split == sp)
      if (nrow(r) == 1) rows_tr[[paste0("cv_", sp)]] <- tibble(
        unit = paste0("Secondary (", sp, ")"), side = "your O",
        faced25 = r$unit_25, healthy26 = r$healthy_u,
        priced26 = r$priced_u,
        adj_faced25 = NA_real_, adj_priced26 = r$priced_a_u,
        fill25 = r$fill_share_25, interp26 = r$interp_raw)
    }
  }
  # -- opponents' RECEIVING, both lenses (you're on defense)
  if (gate_tr("sweep_rc", c("focal", "split", "faced25",
                            "v26", "v26p", "a25", "v26ap",
                            "unscored_share"))) {
    for (sp in c("man", "zone")) {
      r <- sweep_rc %>% filter(focal == cd, split == sp)
      if (nrow(r) == 1) rows_tr[[paste0("rc_", sp)]] <- tibble(
        unit = paste0("Receiving (", sp, ")"), side = "your D",
        faced25 = r$faced25, healthy26 = r$v26, priced26 = r$v26p,
        adj_faced25 = r$a25, adj_priced26 = r$v26ap,
        fill25 = r$unscored_share,
        interp26 = if ("prior_share" %in% names(r))
          r$prior_share else NA_real_)
    }
  }
  # -- opponents' RUSHING (you're on defense) --
  if (gate_tr("sweep_ru", c("focal", "faced_grun",
                            "healthy_grun", "priced_grun",
                            "adj_25_m", "priced_adj"))) {
    r <- sweep_ru %>% filter(focal == cd)
    if (nrow(r) == 1) rows_tr$ru <- tibble(
      unit = "Rushing", side = "your D",
      faced25 = r$faced_grun, healthy26 = r$healthy_grun,
      priced26 = r$priced_grun,
      adj_faced25 = r$adj_25_m, adj_priced26 = r$priced_adj,
      fill25 = col_or_na_tr(r, c("bkup_share", "fill_share",
                                 "bkup25")),
      interp26 = NA_real_)
  }
  # -- opponents' PASS BLOCK, flat across five slots (you're
  #    on defense; OL layers round to 3 -- their law) --
  if (gate_tr("sweep_avail_pb", c("focal", "faced_r",
                                  "paper_r", "av_r", "av_a"))) {
    r <- sweep_avail_pb %>% filter(focal == cd)
    fa_pb <- if ("faced_a" %in% names(r)) mean(r$faced_a,
                                               na.rm = TRUE) else NA_real_
    if (nrow(r) > 0) rows_tr$pb <- tibble(
      unit = "Pass block", side = "your D",
      faced25 = mean(r$faced_r, na.rm = TRUE),
      healthy26 = mean(r$paper_r, na.rm = TRUE),
      priced26 = mean(r$av_r, na.rm = TRUE),
      adj_faced25 = fa_pb, adj_priced26 = mean(r$av_a,
                                               na.rm = TRUE),
      fill25 = NA_real_, interp26 = NA_real_)
  }
  # -- opponents' RUN BLOCK, both schemes (you're on defense)
  if (gate_tr("sweep_avail_rb", c("focal", "faced_gap",
                                  "healthy_gap", "priced_gap",
                                  "faced_zone", "healthy_zone",
                                  "priced_zone", "faced_adj",
                                  "priced_adj"))) {
    r <- sweep_avail_rb %>% filter(focal == cd)
    if (nrow(r) > 0) {
      rows_tr$rb_gap <- tibble(
        unit = "Run block (gap)", side = "your D",
        faced25 = mean(r$faced_gap, na.rm = TRUE),
        healthy26 = mean(r$healthy_gap, na.rm = TRUE),
        priced26 = mean(r$priced_gap, na.rm = TRUE),
        adj_faced25 = mean(r$faced_adj, na.rm = TRUE),
        adj_priced26 = mean(r$priced_adj, na.rm = TRUE),
        fill25 = NA_real_, interp26 = NA_real_)
      rows_tr$rb_zone <- tibble(
        unit = "Run block (zone)", side = "your D",
        faced25 = mean(r$faced_zone, na.rm = TRUE),
        healthy26 = mean(r$healthy_zone, na.rm = TRUE),
        priced26 = mean(r$priced_zone, na.rm = TRUE),
        adj_faced25 = mean(r$faced_adj, na.rm = TRUE),
        adj_priced26 = mean(r$priced_adj, na.rm = TRUE),
        fill25 = NA_real_, interp26 = NA_real_)
    }
  }
  
  if (!length(rows_tr)) {
    cat("no sweep frames in session -- run the availability",
        "layers first.\n")
    return(invisible(NULL))
  }
  
  slate_tr <- bind_rows(rows_tr) %>%
    mutate(d_priced = priced26 - faced25,
           d_adj    = adj_priced26 - adj_faced25)
  
  print(slate_tr %>%
          transmute(unit,
                    side,
                    faced25   = round(100 * faced25, 1),
                    healthy26 = round(100 * healthy26, 1),
                    priced26  = round(100 * priced26, 1),
                    d_priced_pts = round(100 * d_priced, 1),
                    adj_faced25  = round(100 * adj_faced25, 1),
                    adj_priced26 = round(100 * adj_priced26, 1),
                    d_adj_pts    = round(100 * d_adj, 1)) %>%
          as.data.frame())
  cat("\nread: positive d = the 2026 slate is HARDER than what\n")
  cat("was faced in 2025. '--'/NA = that unit's faced side has\n")
  cat("no adjusted currency (canon boundary, not a bug).\n")
  
  # the table form -- grouped BY YEAR (Andy's 2026-08-23
  # ruling): faced 2025 (real-time rosters: raw + adjusted)
  # left, projected 2026 (priced rosters: raw + adjusted)
  # right, deltas after, honesty last. '26 = the PRICED slate
  # (the whole point of the pipeline). Red = harder for the
  # focal -- opponent direction, like every opponent table.
  gt_slate_tr <- slate_tr %>%
    mutate(side = factor(
      dplyr::if_else(side == "your O",
                     "your OFFENSE on the field",
                     "your DEFENSE on the field"),
      levels = c("your OFFENSE on the field",
                 "your DEFENSE on the field")),
      unit = factor(unit,
                    levels = c("Run defense", "Pass rush",
                               "Secondary (man)",
                               "Secondary (zone)",
                               "Receiving (man)",
                               "Receiving (zone)", "Rushing",
                               "Pass block",
                               "Run block (gap)",
                               "Run block (zone)"))) %>%
    arrange(side, unit) %>%
    # Andy, 2026-08-23: group the columns BY YEAR -- faced
    # 2025 (raw + adjusted) on the left, projected 2026 (raw +
    # adjusted) on the right, deltas after, honesty last.
    select(side, unit,
           r25 = faced25, a25 = adj_faced25,
           r26 = priced26, a26 = adj_priced26,
           d = d_priced, ad = d_adj,
           fill25, interp26) %>%
    gt(groupname_col = "side", rowname_col = "unit") %>%
    tab_spanner(label = "faced 2025 (real-time rosters)",
                columns = c(r25, a25)) %>%
    tab_spanner(label = "projected 2026 (priced rosters)",
                columns = c(r26, a26)) %>%
    tab_spanner(label = "\u0394 (26 minus 25)",
                columns = c(d, ad)) %>%
    tab_spanner(label = "honesty",
                columns = c(fill25, interp26)) %>%
    cols_label(r25 = "raw", a25 = "adjusted",
               r26 = "raw", a26 = "adjusted",
               d = "raw", ad = "adjusted",
               fill25 = "fill %", interp26 = "interp %") %>%
    fmt_percent(columns = c(r25, a25, r26, a26),
                decimals = 0) %>%
    fmt_percent(columns = c(d, ad), decimals = 1,
                force_sign = TRUE) %>%
    fmt_percent(columns = c(fill25, interp26), decimals = 0) %>%
    sub_missing(missing_text = "--") %>%
    data_color(columns = c(d, ad),
               fn = scales::col_numeric(
                 palette = c("#6baed6", "#f7f7f7", "#C60C30"),
                 domain = NULL, na.color = "#f7f7f7"),
               autocolor_text = TRUE) %>%
    tab_header(
      title = paste0("How hard is ", cd, "'s 2026 slate, unit ",
                     "by unit? -- faced 2025 vs priced 2026"),
      subtitle = paste0(
        "every row is the OPPONENTS' unit -- higher = harder ",
        "for ", cd, " | delta = priced 2026 minus faced 2025, ",
        "same slate laws and walls as the unit layers | '--' = ",
        "that unit carries no adjusted faced currency (canon ",
        "boundary) | fill = share of 2025 faced evidence priced ",
        "at a band prior | interp = share of the 2026 slate on ",
        "interpolated talent | both lenses shown for split ",
        "units; run block's adjusted value is scheme-neutral ",
        "and repeats on both rows")) %>%
    tab_options(table.font.size = px(12),
                data_row.padding = px(3),
                column_labels.font.weight = "bold",
                row_group.font.weight = "bold")
  print(gt_slate_tr)
  
  # ------------------------------------------------------------
  # THE SIMPLE ONE (Andy, 2026-08-23: "THERE IS NO SIMPLE 2025
  # VS 2026 VISUAL. HOW IS THIS SO HARD."): one number per unit
  # per year, one delta. Raw currency -- the only one with no
  # canon holes. Adjusted currencies and the fill/interp
  # honesty columns live in the detail table above. Money
  # position: printed LAST in the block.
  # ------------------------------------------------------------
  simp_tr <- slate_tr %>%
    mutate(side = factor(
      dplyr::if_else(side == "your O",
                     "your OFFENSE on the field",
                     "your DEFENSE on the field"),
      levels = c("your OFFENSE on the field",
                 "your DEFENSE on the field")),
      unit = factor(unit,
                    levels = c("Run defense", "Pass rush",
                               "Secondary (man)",
                               "Secondary (zone)",
                               "Receiving (man)",
                               "Receiving (zone)", "Rushing",
                               "Pass block",
                               "Run block (gap)",
                               "Run block (zone)"))) %>%
    arrange(side, unit) %>%
    select(side, unit, y25 = faced25, y26 = priced26,
           d = d_priced)
  all_tr <- simp_tr %>%
    summarise(y25 = mean(y25, na.rm = TRUE),
              y26 = mean(y26, na.rm = TRUE), .groups = "drop") %>%
    mutate(d = y26 - y25)
  gt_simp_tr <- simp_tr %>%
    gt(groupname_col = "side", rowname_col = "unit") %>%
    tab_header(
      title = paste0(cd, " 2025 vs 2026 -- the whole slate, ",
                     "in one breath"),
      subtitle = paste0(
        "one number per unit per year, raw currency (the only ",
        "one with no canon holes) | red = 2026 harder than ",
        "2025 for ", cd, " | adjusted currencies + honesty ",
        "columns: the detail table above")) %>%
    cols_label(y25 = "2025", y26 = "2026", d = "\u0394") %>%
    fmt_percent(columns = c(y25, y26), decimals = 0) %>%
    fmt_percent(columns = d, decimals = 1, force_sign = TRUE) %>%
    sub_missing(missing_text = "--") %>%
    data_color(columns = d,
               fn = scales::col_numeric(
                 palette = c("#6baed6", "#f7f7f7", "#C60C30"),
                 domain = NULL, na.color = "#f7f7f7"),
               autocolor_text = TRUE) %>%
    grand_summary_rows(
      columns = c(y25, y26, d),
      fns = list(`ALL UNITS (avg of rows)` ~ mean(., na.rm = TRUE)),
      fmt = ~ fmt_percent(., columns = c(y25, y26),
                          decimals = 0) %>%
        fmt_percent(., columns = d, decimals = 1,
                    force_sign = TRUE)) %>%
    tab_options(table.font.size = px(12),
                data_row.padding = px(3),
                column_labels.font.weight = "bold",
                row_group.font.weight = "bold",
                grand_summary_row.background.color = "#f0f0f0",
                grand_summary_row.text_transform = "inherit")
  print(gt_simp_tr)
  cat(sprintf(paste0("\nslate in one number: 2025 %.0f%% -> ",
                     "2026 %.0f%% (%+.1f pts raw, avg of the ",
                     "unit rows)\n"),
              100 * all_tr$y25, 100 * all_tr$y26,
              100 * all_tr$d))
  invisible(list(board = slate_tr, gt = gt_slate_tr,
                 gt_simple = gt_simp_tr))
}

# ------------------------------------------------------------
# 1b. TEAM_SCHED -- the opponent SET, not the calendar (Andy,
#     2026-08-23: "I DON'T NEED WEEK BY WEEK. THE AGGREGATE IS
#     FINE"). 2025 opponents (real-time rosters, as faced) vs
#     2026 opponents (projected rosters) as a SET: who, twice
#     or once, back/new/gone. The strength compare is the
#     slate table -- healthy26 is the paper-roster ceiling
#     (the overestimate Andy flagged), priced26 has the
#     expected missed games already priced in, and the delta
#     law reads priced - faced. REG both years (canon rulings:
#     2026 playoff opponents unknowable; 2025 REG-only to
#     match the slate side).
# ------------------------------------------------------------

# nflverse schedule codes -> canon (PFF) spelling. Canon's own
# translator rides when in session; the table is the fixed
# PUBLIC nflverse vocabulary for the four divergent codes (the
# custom 2026 roster file's 'AZ' is a different file and a
# different job -- the name bridge owns that side)
to_canon_sched_tr <- function(v) {
  if (exists("to_pff_rd")) return(to_pff_rd(v))
  al <- c(ARI = "ARZ", BAL = "BLT", CLE = "CLV", HOU = "HST")
  hit <- unname(al[v])
  dplyr::if_else(is.na(hit), v, hit)
}

# same acceptance test canon runs on its schedule loads
load_sched_tr <- function(season) {
  s <- tryCatch(nflreadr::load_schedules(season),
                error = function(e) NULL)
  if (is.data.frame(s) &&
      all(c("season", "week", "home_team", "away_team") %in%
          names(s)) && nrow(s) > 200) return(s)
  cat("   schedule load for", season,
      "failed -- that side prints all '--'\n")
  NULL
}

team_sched <- function(code) {
  cd <- norm_code_tr(code)
  cat("\n=====================================================\n")
  cat(" ", cd, "SLATE -- the opponent SET, 2025 vs 2026.\n")
  cat("  Who, twice or once, back/new/gone. Strength is the\n")
  cat("  slate table's job. REG only, both years.\n")
  cat("======================================================\n")
  if (!requireNamespace("nflreadr", quietly = TRUE)) {
    cat("nflreadr not available -- block skipped\n")
    return(invisible(NULL))
  }
  s25 <- load_sched_tr(2025)
  s26 <- if (exists("sch26_rd")) sch26_rd else
    load_sched_tr(2026)
  if (is.null(s25) && is.null(s26)) {
    cat("no schedule frame either year -- block skipped\n")
    return(invisible(NULL))
  }
  
  one_side <- function(s) {
    if (is.null(s)) return(NULL)
    s %>% filter(week <= 18) %>%
      mutate(home = to_canon_sched_tr(home_team),
             away = to_canon_sched_tr(away_team)) %>%
      filter(home == cd | away == cd) %>%
      transmute(opp = if_else(home == cd, away, home))
  }
  o25 <- one_side(s25)
  o26 <- one_side(s26)
  
  cnt <- function(d, tag) {
    z <- tibble(opp = character(0), n = integer(0))
    if (is.null(d)) return(setNames(z, c("opp", tag)))
    setNames(d %>% count(opp), c("opp", tag))
  }
  sets <- full_join(cnt(o25, "n25"), cnt(o26, "n26"),
                    by = "opp") %>%
    mutate(y25 = case_when(is.na(n25) ~ "--", n25 == 2L ~ "x2",
                           TRUE ~ "x1"),
           y26 = case_when(is.na(n26) ~ "--", n26 == 2L ~ "x2",
                           TRUE ~ "x1"),
           st = case_when(!is.na(n25) & !is.na(n26) ~ "back",
                          is.na(n25) ~ "new", TRUE ~ "gone")) %>%
    arrange(factor(st, levels = c("back", "new", "gone")), opp)
  
  g25 <- sum(sets$n25, na.rm = TRUE)
  g26 <- sum(sets$n26, na.rm = TRUE)
  lst <- function(n) {
    d <- sets %>% filter(!is.na(.data[[n]]))
    if (!nrow(d)) return("--")
    paste(ifelse(d[[n]] == 2L, paste0(d$opp, " x2"), d$opp),
          collapse = ", ")
  }
  cat("\n2025 slate (", g25, " games): ", lst("n25"),
      "\n", sep = "")
  cat("2026 slate (", g26, " games): ", lst("n26"),
      "\n", sep = "")
  if (!is.null(o25) && !is.null(o26)) {
    cat("\n-- the diff:\n")
    cat("   back (", sum(sets$st == "back"), "): ",
        paste(sets$opp[sets$st == "back"], collapse = ", "),
        "\n", sep = "")
    cat("   new  (", sum(sets$st == "new"), "): ",
        paste(sets$opp[sets$st == "new"], collapse = ", "),
        "\n", sep = "")
    cat("   gone (", sum(sets$st == "gone"), "): ",
        paste(sets$opp[sets$st == "gone"], collapse = ", "),
        "\n", sep = "")
  } else cat("\n(one schedule side missing -- no diff)\n")
  print(sets %>% select(opp, y25, y26) %>% as.data.frame())
  
  both <- !is.null(o25) && !is.null(o26)
  new_ix  <- if (both) which(sets$st == "new") else integer(0)
  gone_ix <- if (both) which(sets$st == "gone") else integer(0)
  gt_sched_tr <- sets %>%
    select(opp, y25, y26) %>%
    gt(rowname_col = "opp") %>%
    tab_header(
      title = paste0(cd, " slate -- WHO: 2025 opponents vs ",
                     "2026"),
      subtitle = paste0(
        sum(sets$st == "back"), " back, ", sum(sets$st == "new"),
        " new, ", sum(sets$st == "gone"), " gone | x2 = ",
        "home-and-home | colors are churn, not strength: blue ",
        "= new in 2026, red = gone after 2025 -- strength per ",
        "unit is the slate table | REG both years: 2026 ",
        "playoff opponents unknowable, 2025 REG-only to match ",
        "| codes are canon (PFF) spelling")) %>%
    cols_label(y25 = "2025", y26 = "2026") %>%
    tab_options(table.font.size = px(12),
                data_row.padding = px(3),
                column_labels.font.weight = "bold")
  if (length(new_ix))
    gt_sched_tr <- gt_sched_tr %>%
    tab_style(style = cell_text(color = "#6baed6"),
              locations = cells_body(columns = y26,
                                     rows = new_ix))
  if (length(gone_ix))
    gt_sched_tr <- gt_sched_tr %>%
    tab_style(style = cell_text(color = "#C60C30"),
              locations = cells_body(columns = y25,
                                     rows = gone_ix))
  print(gt_sched_tr)
  invisible(list(sets = sets, gt = gt_sched_tr))
}

# ------------------------------------------------------------
# 2. TEAM_OWN -- the mirror table for ANY team: that team's
#    own players, healthy (before) vs injury-priced (after),
#    per unit. Same extraction law as the NE own-units table
#    (v3): pure filters of the seven walled member frames.
#    Console first: who moved in and out (roster route).
# ------------------------------------------------------------

team_own <- function(code) {
  cd <- norm_code_tr(code)
  ros_cd <- ros_code_tr(cd)  # the roster route's spelling
  cat("\n=====================================================\n")
  cat(" ", cd, "OWN UNITS -- before and after injury pricing\n")
  cat("  before = plays to his history | after = priced by\n")
  cat("  that history (misses route to the measured backup\n")
  cat("  level). after - before < 0 = injuries cost us.\n")
  cat("======================================================\n")
  
  # churn console: arrivals and departures, roster route
  arr_tr <- NULL
  if (requireNamespace("nflreadr", quietly = TRUE)) {
    ros25_tr <- nflreadr::load_rosters(2025) %>%
      filter(team == ros_cd)
    ros26_tr <- nflreadr::load_rosters(2026) %>%
      filter(team == ros_cd)
    ros25_all <- nflreadr::load_rosters(2025)
    arr_tr <- ros26_tr %>%
      filter(!cl_tr(full_name) %in% cl_tr(ros25_tr$full_name)) %>%
      mutate(from = unname(setNames(ros25_all$team,
                                    cl_tr(ros25_all$full_name))[
                                      cl_tr(full_name)]),
             from = dplyr::coalesce(from, "rookie/out"))
    dep_tr <- ros25_tr %>%
      filter(!cl_tr(full_name) %in% cl_tr(ros26_tr$full_name)) %>%
      mutate(now = where_now_tr(full_name, cd))
    cat("\n-- who moved IN (", cd, " 2026):\n", sep = "")
    print(arr_tr %>% select(full_name, position, from) %>%
            arrange(position) %>% as.data.frame())
    cat("-- who moved OUT:\n")
    print(dep_tr %>% select(full_name, position, now) %>%
            arrange(position) %>% as.data.frame())
  }
  
  # star = new to this team in 2026
  new26_keys_tr <- if (!is.null(arr_tr)) cl_tr(arr_tr$full_name) else
    character(0)
  
  # per-unit gates, same law as team_lastyear: one absent unit
  # never kills the other six (audit point -- the old all-or-
  # nothing gate broke the header's own promise, and REC_BANDS
  # rode the gate without ever being read; it is gone)
  parts_tr <- list()
  
  ok_u <- gate_tr("members_rc", c("team_name", "roster_name",
                                  "band", "avail", "mg_f", "mg_p", "c3mg_f",
                                  "c3mg_p", "zg_f", "zg_p", "c3zg_f", "c3zg_p",
                                  "usage_w"))
  if (ok_u) {
    rec_cd <- code_in_tr(members_rc$team_name, cd)
    rec_ne <- members_rc %>% filter(team_name == rec_cd)
    if (!nrow(rec_ne)) cat("   [Receiving] no", rec_cd,
                           "rows in members_rc -- unit skipped\n")
    if (nrow(rec_ne)) {
      ord <- rec_ne %>% arrange(desc(zg_f)) %>% pull(roster_name)
      parts_tr$rec <- bind_rows(
        rec_ne %>% transmute(unit = "Receiving",
                             player = roster_name, lens = "man",
                             role = band, avail,
                             bef = mg_f, aft = mg_p,
                             abef = c3mg_f, aaft = c3mg_p,
                             uw = usage_w),
        rec_ne %>% transmute(unit = "Receiving",
                             player = roster_name, lens = "zone",
                             role = band, avail,
                             bef = zg_f, aft = zg_p,
                             abef = c3zg_f, aaft = c3zg_p,
                             uw = usage_w)) %>%
        mutate(pord = match(player, ord))
    }
  }
  ok_u <- gate_tr("members_cv", c("team", "roster_name",
                                  "band", "split", "avail", "grade_f",
                                  "grade_p", "c3_f", "adj_p", "usage_w"))
  if (ok_u) {
    sec_cd <- code_in_tr(members_cv$team, cd)
    sec_ne <- members_cv %>% filter(team == sec_cd)
    if (!nrow(sec_ne)) cat("   [Secondary] no", sec_cd,
                           "rows in members_cv -- unit skipped\n")
    if (nrow(sec_ne)) {
      ord <- sec_ne %>% filter(split == "zone") %>%
        arrange(desc(grade_f)) %>% pull(roster_name)
      ord <- c(ord, sec_ne %>% filter(split == "man",
                                      !roster_name %in% ord) %>%
                 arrange(desc(grade_f)) %>% pull(roster_name))
      parts_tr$sec <- sec_ne %>%
        transmute(unit = "Secondary", player = roster_name,
                  lens = split, role = band, avail,
                  bef = grade_f, aft = grade_p,
                  abef = c3_f, aaft = adj_p, uw = usage_w) %>%
        mutate(pord = match(player, ord))
    }
  }
  ok_u <- gate_tr("slot_value_26_pb_build", c("team_name",
                                              "roster_name", "slot", "avail", "gf_raw",
                                              "gf_raw_av", "gf_adj", "gf_adj_av")) &&
    have_tr("ol_pos_levels")
  if (ok_u) {
    pb_cd <- code_in_tr(slot_value_26_pb_build$team_name, cd)
    pb_ne <- slot_value_26_pb_build %>% filter(team_name == pb_cd)
    if (!nrow(pb_ne)) cat("   [Pass block] no", pb_cd,
                          "rows in slot_value_26_pb_build -- unit skipped\n")
    if (nrow(pb_ne)) parts_tr$pb <- pb_ne %>%
      transmute(unit = "Pass block", player = roster_name,
                lens = "overall", role = slot, avail,
                bef = gf_raw, aft = gf_raw_av,
                abef = gf_adj, aaft = gf_adj_av,
                uw = NA_real_) %>%
      mutate(pord = match(role, ol_pos_levels))
  }
  rb_split_tr <- exists("slot_value_26_rb_build") &&
    all(c("V_c3_gap", "V_c3_gap_av", "V_c3_zone",
          "V_c3_zone_av") %in% names(slot_value_26_rb_build))
  ok_u <- gate_tr("slot_value_26_rb_build", c("team_name",
                                              "roster_name", "slot", "avail", "V_gap",
                                              "V_gap_av", "V_zone", "V_zone_av", "V_c3",
                                              "V_c3_av")) && have_tr("ol_pos_levels")
  if (ok_u) {
    rb_cd <- code_in_tr(slot_value_26_rb_build$team_name, cd)
    rb_ne <- slot_value_26_rb_build %>% filter(team_name == rb_cd)
    if (!nrow(rb_ne)) cat("   [Run block] no", rb_cd,
                          "rows in slot_value_26_rb_build -- unit skipped\n")
    if (nrow(rb_ne)) {
      if (!rb_split_tr)
        cat("   [Run block] V_c3_gap/V_c3_zone not on",
            "slot_value_26_rb_build -- adjusted repeats blended;",
            "re-source the availability layer for the split\n")
      rb_ne <- rb_ne %>% mutate(
        abef_g = if (rb_split_tr) V_c3_gap     else V_c3,
        aaft_g = if (rb_split_tr) V_c3_gap_av  else V_c3_av,
        abef_z = if (rb_split_tr) V_c3_zone    else V_c3,
        aaft_z = if (rb_split_tr) V_c3_zone_av else V_c3_av)
      parts_tr$rb <- bind_rows(
        rb_ne %>% transmute(unit = "Run block", player = roster_name,
                            lens = "gap", role = slot, avail,
                            bef = V_gap, aft = V_gap_av,
                            abef = abef_g, aaft = aaft_g,
                            uw = NA_real_),
        rb_ne %>% transmute(unit = "Run block", player = roster_name,
                            lens = "zone", role = slot, avail,
                            bef = V_zone, aft = V_zone_av,
                            abef = abef_z, aaft = aaft_z,
                            uw = NA_real_)) %>%
        mutate(pord = match(role, ol_pos_levels))
    }
  }
  rb_note_own_tr <- if (rb_split_tr)
    "run block's adjusted value is scheme-split (gap/zone currency)"
  else
    "run block's adjusted value is scheme-neutral and repeats on both rows"
  ok_u <- gate_tr("memb_lg_ru", c("team", "roster_name",
                                  "avail", "gf", "gf_av", "V_c3", "V_c3_av",
                                  "uw"))
  if (ok_u) {
    ru_cd <- code_in_tr(memb_lg_ru$team, cd)
    ru_ne <- memb_lg_ru %>% filter(team == ru_cd)
    if (!nrow(ru_ne)) cat("   [Rushing] no", ru_cd,
                          "rows in memb_lg_ru -- unit skipped\n")
    if (nrow(ru_ne)) {
      ord <- ru_ne %>% arrange(desc(gf)) %>% pull(roster_name)
      parts_tr$ru <- ru_ne %>%
        transmute(unit = "Rushing", player = roster_name,
                  lens = "overall", role = "committee", avail,
                  bef = gf, aft = gf_av,
                  abef = V_c3, aaft = V_c3_av, uw = uw) %>%
        mutate(pord = match(player, ord))
    }
  }
  ok_u <- gate_tr("members_pa", c("team", "roster_name",
                                  "band", "avail", "gf", "gf_p", "c3_f",
                                  "c3_p", "uw"))
  if (ok_u) {
    pa_cd <- code_in_tr(members_pa$team, cd)
    pa_ne <- members_pa %>% filter(team == pa_cd)
    if (!nrow(pa_ne)) cat("   [Pass rush] no", pa_cd,
                          "rows in members_pa -- unit skipped\n")
    if (nrow(pa_ne)) {
      ord <- pa_ne %>% arrange(desc(gf)) %>% pull(roster_name)
      parts_tr$pa <- pa_ne %>%
        transmute(unit = "Pass rush", player = roster_name,
                  lens = "overall", role = band, avail,
                  bef = gf, aft = gf_p,
                  abef = c3_f, aaft = c3_p, uw = uw) %>%
        mutate(pord = match(player, ord))
    }
  }
  ok_u <- gate_tr("members_ra", c("team", "roster_name",
                                  "band", "avail", "grade_f", "grade_p",
                                  "c3_f", "c3_p", "usage_w"))
  if (ok_u) {
    ra_cd <- code_in_tr(members_ra$team, cd)
    ra_ne <- members_ra %>% filter(team == ra_cd)
    if (!nrow(ra_ne)) cat("   [Run defense] no", ra_cd,
                          "rows in members_ra -- unit skipped\n")
    if (nrow(ra_ne)) {
      ord <- ra_ne %>% arrange(desc(grade_f)) %>% pull(roster_name)
      parts_tr$ra <- ra_ne %>%
        transmute(unit = "Run defense", player = roster_name,
                  lens = "overall", role = band, avail,
                  bef = grade_f, aft = grade_p,
                  abef = c3_f, aaft = c3_p, uw = usage_w) %>%
        mutate(pord = match(player, ord))
    }
  }
  
  if (!length(parts_tr)) {
    cat("no", cd, "rows in any member frame -- every unit",
        "gate above printed its reason; run the missing",
        "layers, then re-source this file.\n")
    return(invisible(NULL))
  }
  
  own_tr <- bind_rows(parts_tr) %>%
    mutate(unit = factor(unit,
                         levels = c("Receiving", "Rushing",
                                    "Pass block", "Run block",
                                    "Pass rush", "Secondary",
                                    "Run defense")),
           lens = factor(lens, levels = c("man", "gap",
                                          "overall", "zone")),
           emiss = (1 - avail) * 17,
           d  = aft  - bef,
           ad = aaft - abef) %>%
    arrange(unit, pord, lens) %>%
    mutate(new26 = cl_tr(player) %in% new26_keys_tr,
           player = dplyr::if_else(new26, paste0(player, " *"),
                                   player))
  
  # viewer receipt (Claude's proposal, adopted as a printed
  # warning -- Andy rules stops, and this layer's law is
  # skip-never-die): avail outside [0,1] should be impossible
  # post-cap; if it arrives, it prints, it never hides.
  oob_tr <- own_tr %>%
    filter(!is.na(avail) & (avail < 0 | avail > 1))
  if (nrow(oob_tr)) {
    cat("!! avail outside [0,1] reached the viewer -- the",
        "owning unit layer's wall missed this:\n")
    print(oob_tr %>% select(unit, player, avail) %>%
            as.data.frame())
  }
  
  # console summary, one line per unit
  cat("\n-- what injuries cost each", cd,
      "unit (usage-weighted where the unit carries weight):\n")
  sum_tr <- function(df, w = NULL, label) {
    b <- if (is.null(w)) mean(df$bef, na.rm = TRUE) else
      weighted.mean(df$bef, w = df[[w]], na.rm = TRUE)
    a <- if (is.null(w)) mean(df$aft, na.rm = TRUE) else
      weighted.mean(df$aft, w = df[[w]], na.rm = TRUE)
    cat(sprintf("   %-16s before %.3f -> after %.3f | exp backup gms %.1f\n",
                label, b, a, sum(17 * (1 - df$avail), na.rm = TRUE)))
  }
  if (!is.null(parts_tr$rec))
    sum_tr(rec_ne %>% mutate(bef = zg_f, aft = zg_p),
           "usage_w", "Receiving (zone)")
  if (!is.null(parts_tr$ru))
    sum_tr(ru_ne %>% mutate(bef = gf, aft = gf_av), "uw",
           "Rushing")
  if (!is.null(parts_tr$pb))
    sum_tr(pb_ne %>% mutate(bef = gf_adj, aft = gf_adj_av),
           NULL, "Pass block (adj)")
  if (!is.null(parts_tr$rb))
    sum_tr(rb_ne %>% mutate(bef = V_zone, aft = V_zone_av),
           NULL, "Run block (zone)")
  if (!is.null(parts_tr$pa))
    sum_tr(pa_ne %>% mutate(bef = gf, aft = gf_p), "uw",
           "Pass rush")
  if (!is.null(parts_tr$sec))
    sum_tr(sec_ne %>% filter(split == "zone") %>%
             mutate(bef = grade_f, aft = grade_p), "usage_w",
           "Secondary (zone)")
  if (!is.null(parts_tr$ra))
    sum_tr(ra_ne %>% mutate(bef = grade_f, aft = grade_p),
           "usage_w", "Run defense")
  
  gt_tr <- own_tr %>%
    select(unit, player, lens, role, avail, emiss,
           bef, aft, d, abef, aaft, ad) %>%
    gt(groupname_col = "unit", rowname_col = "player") %>%
    tab_header(
      title = paste0("What do injuries cost ", cd,
                     "'s own units in 2026? -- before and after"),
      subtitle = paste0(
        "before = everyone plays to his own history | after = ",
        "priced by that history (misses route to the measured ",
        "backup level) | OUR side now: red = ", cd, " loses ",
        "quality, the colors flip from the opponent tables | ",
        "'--' = canon box-score hole | ", rb_note_own_tr, " | ",
        "* = new to ", cd, " in 2026")) %>%
    tab_spanner(label = "availability",
                columns = c(avail, emiss)) %>%
    tab_spanner(label = "Raw value", columns = c(bef, aft, d)) %>%
    tab_spanner(label = "Adjusted (same-slate)",
                columns = c(abef, aaft, ad)) %>%
    cols_label(lens = "lens", role = "role",
               avail = "avail", emiss = "exp miss (g)",
               bef = "before", aft = "after", d = "\u0394",
               abef = "before", aaft = "after",
               ad = "\u0394") %>%
    fmt_percent(columns = c(avail, bef, aft, d, abef, aaft, ad),
                decimals = 0) %>%
    fmt_percent(columns = c(d, ad), decimals = 1,
                force_sign = TRUE) %>%
    fmt_number(columns = emiss, decimals = 1) %>%
    sub_missing(missing_text = "--") %>%
    data_color(columns = c(d, ad),
               fn = scales::col_numeric(
                 palette = c("#C60C30", "#f7f7f7", "#6baed6"),
                 domain = c(-0.3, 0.3), na.color = "#f7f7f7"),
               autocolor_text = TRUE) %>%
    tab_options(table.font.size = px(12),
                data_row.padding = px(3),
                column_labels.font.weight = "bold",
                row_group.font.weight = "bold")
  # ------------------------------------------------------------
  # THE AGGREGATE -- one row per unit, before -> after, each
  # unit's own weight law (usage where carried, flat across the
  # five OL slots). The single-number bottom line the opponent
  # slate always had; the own side has it now too.
  # ------------------------------------------------------------
  
  agg_row_tr <- function(df, w, label, bef, aft, abef, aaft) {
    wv <- if (is.null(w)) NULL else df[[w]]
    g <- function(v) {
      x <- df[[v]]
      if (all(is.na(x))) return(NA_real_)
      if (is.null(wv)) mean(x, na.rm = TRUE) else
        weighted.mean(x, w = wv, na.rm = TRUE)
    }
    tibble(unit = label, bef = g(bef), aft = g(aft),
           abef = g(abef), aaft = g(aaft),
           emiss = sum(17 * (1 - df$avail), na.rm = TRUE))
  }
  agg_rows_tr <- list()
  if (!is.null(parts_tr$rec)) {
    agg_rows_tr$recm <- agg_row_tr(rec_ne, "usage_w",
                                   "Receiving (man)", "mg_f", "mg_p", "c3mg_f", "c3mg_p")
    agg_rows_tr$recz <- agg_row_tr(rec_ne, "usage_w",
                                   "Receiving (zone)", "zg_f", "zg_p", "c3zg_f", "c3zg_p")
  }
  if (!is.null(parts_tr$ru))
    agg_rows_tr$ru <- agg_row_tr(ru_ne, "uw",
                                 "Rushing", "gf", "gf_av", "V_c3", "V_c3_av")
  if (!is.null(parts_tr$pb))
    agg_rows_tr$pb <- agg_row_tr(pb_ne, NULL,
                                 "Pass block", "gf_raw", "gf_raw_av", "gf_adj", "gf_adj_av")
  if (!is.null(parts_tr$rb)) {
    agg_rows_tr$rbg <- agg_row_tr(rb_ne, NULL,
                                  "Run block (gap)", "V_gap", "V_gap_av",
                                  "abef_g", "aaft_g")
    agg_rows_tr$rbz <- agg_row_tr(rb_ne, NULL,
                                  "Run block (zone)", "V_zone", "V_zone_av",
                                  "abef_z", "aaft_z")
  }
  if (!is.null(parts_tr$pa))
    agg_rows_tr$pa <- agg_row_tr(pa_ne, "uw",
                                 "Pass rush", "gf", "gf_p", "c3_f", "c3_p")
  if (!is.null(parts_tr$sec)) {
    agg_rows_tr$secm <- agg_row_tr(
      sec_ne %>% filter(split == "man"), "usage_w",
      "Secondary (man)", "grade_f", "grade_p", "c3_f", "adj_p")
    agg_rows_tr$secz <- agg_row_tr(
      sec_ne %>% filter(split == "zone"), "usage_w",
      "Secondary (zone)", "grade_f", "grade_p", "c3_f", "adj_p")
  }
  if (!is.null(parts_tr$ra))
    agg_rows_tr$ra <- agg_row_tr(ra_ne, "usage_w",
                                 "Run defense", "grade_f", "grade_p", "c3_f", "c3_p")
  
  agg_tr <- bind_rows(agg_rows_tr) %>%
    mutate(d = aft - bef, ad = aaft - abef)
  overall_tr <- agg_tr %>%
    summarise(bef = mean(bef, na.rm = TRUE),
              aft = mean(aft, na.rm = TRUE),
              abef = mean(abef, na.rm = TRUE),
              aaft = mean(aaft, na.rm = TRUE),
              emiss = sum(emiss, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(unit = "ALL UNITS (avg of rows)",
           d = aft - bef, ad = aaft - abef)
  agg_show_tr <- bind_rows(agg_tr, overall_tr) %>%
    mutate(unit = factor(unit,
                         levels = c("Receiving (man)",
                                    "Receiving (zone)", "Rushing",
                                    "Pass block",
                                    "Run block (gap)",
                                    "Run block (zone)",
                                    "Pass rush",
                                    "Secondary (man)",
                                    "Secondary (zone)",
                                    "Run defense",
                                    "ALL UNITS (avg of rows)"))) %>%
    arrange(unit)
  
  cat(sprintf(paste0("\n== %s AGGREGATE: before %.3f -> after %.3f",
                     " | raw delta %+.1f pts | adjusted %+.1f pts",
                     " ==\n"),
              cd, overall_tr$bef, overall_tr$aft,
              100 * overall_tr$d, 100 * overall_tr$ad))
  
  print(gt_tr)
  
  gt_agg_tr <- agg_show_tr %>%
    select(unit, bef, aft, d, abef, aaft, ad, emiss) %>%
    gt(rowname_col = "unit") %>%
    tab_spanner(label = "Raw value",
                columns = c(bef, aft, d)) %>%
    tab_spanner(label = "Adjusted (same-slate)",
                columns = c(abef, aaft, ad)) %>%
    tab_spanner(label = "availability", columns = emiss) %>%
    cols_label(bef = "before", aft = "after", d = "\u0394",
               abef = "before", aaft = "after", ad = "\u0394",
               emiss = "exp miss (g)") %>%
    fmt_percent(columns = c(bef, aft, d, abef, aaft, ad),
                decimals = 0) %>%
    fmt_percent(columns = c(d, ad), decimals = 1,
                force_sign = TRUE) %>%
    fmt_number(columns = emiss, decimals = 1) %>%
    sub_missing(missing_text = "--") %>%
    data_color(columns = c(d, ad),
               fn = scales::col_numeric(
                 palette = c("#C60C30", "#f7f7f7", "#6baed6"),
                 domain = NULL, na.color = "#f7f7f7"),
               autocolor_text = TRUE) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(rows = nrow(agg_show_tr))) %>%
    tab_header(
      title = paste0(cd, " 2026 -- the aggregate injury bill: ",
                     "before vs after"),
      subtitle = paste0(
        "one row per unit-lens, each unit's own weight law | ",
        "both lenses shown for split units; ", rb_note_own_tr,
        " | OUR ",
        "side: red = quality lost once injuries are ",
        "priced, colors flip from the opponent tables | ALL ",
        "UNITS row = simple average of the unit-lens rows shown")) %>%
    tab_options(table.font.size = px(12),
                data_row.padding = px(3),
                column_labels.font.weight = "bold")
  print(gt_agg_tr)
  invisible(list(data = own_tr, gt = gt_tr, gt_agg = gt_agg_tr))
}

# ------------------------------------------------------------
# 2b. TEAM_YOY -- the OWN-roster year compare (Andy,
#     2026-08-23: "I WANT NE 2025 VS NE 2026. REAL PLAYING
#     TIME, ADJUSTED PLAYING TIME VS NE 2026 REAL PLAYING
#     TIME, ADJUSTED PLAYING TIME."). One row per unit-lens.
#     2025 = REAL playing time: the season that happened,
#     players weighted by the snaps/games/attempts they
#     actually played (the same joins team_lastyear runs).
#     2026 = PROJECTED playing time: the priced roster,
#     expected misses routed to the measured backup level
#     (the same "after" columns team_own aggregates). Raw +
#     adjusted each year; deltas only where both years carry
#     the currency. '--' = canon carries no such currency for
#     that unit-year (boundary, not a bug). OUR side: higher
#     = better for the focal; red = quality lost vs 2025.
# ------------------------------------------------------------

team_yoy <- function(code) {
  cd <- norm_code_tr(code)
  cat("\n=====================================================\n")
  cat(" ", cd, "2025 vs 2026 -- OWN ROSTER, playing time in\n")
  cat("  the number. 2025 = real playing time (weighted by\n")
  cat("  what they actually played). 2026 = projected playing\n")
  cat("  time (misses priced to the backup level). raw and\n")
  cat("  adjusted, both years. OUR side: higher = better.\n")
  cat("======================================================\n")
  
  rows_yt <- list()
  yt_row <- function(unit, r25, a25, r26, a26)
    tibble(unit = unit, r25 = r25, a25 = a25,
           r26 = r26, a26 = a26)
  
  # -- RECEIVING, both lenses. 2026 = members_rc priced;
  #    2025 = the corps' stamped currency (canon boundary:
  #    receiving's 2025 value IS the adjusted one, and no
  #    playing-time weights ride in session -> flat corps
  #    mean, labeled) --
  r25m <- a25m <- r26m <- a26m <- NA_real_
  r25z <- a25z <- r26z <- a26z <- NA_real_
  if (gate_tr("corps25_rc", c("player_id", "team_c")) &
      gate_tr("rec_c3_pctl", c("player_id", "season", "split",
                               "c3_grade_pctl",
                               "c3_yprr_pctl"))) {
    v <- rec_c3_pctl %>% filter(season == 2025,
                                !is.na(player_id)) %>%
      select(player_id, split, c3_grade_pctl) %>%
      tidyr::pivot_wider(names_from = split,
                         values_from = c3_grade_pctl,
                         names_glue = "c3_{split}")
    b <- corps25_rc %>% filter(team_c == cd) %>%
      left_join(v, by = "player_id")
    if (!nrow(b)) cat("   [Receiving] no", cd,
                      "rows in corps25_rc -- 2025 side skipped\n")
    if (nrow(b)) {
      a25m <- if ("c3_man" %in% names(b)) m_tr(b$c3_man) else
        NA_real_
      a25z <- if ("c3_zone" %in% names(b)) m_tr(b$c3_zone) else
        NA_real_
    }
    fr <- frame_pick_tr("Receiving raw 2025",
                        c("rec_season_pctl_sos"),
                        c("player_id", "season",
                          "man_grade_pctl",
                          "zone_grade_pctl"))
    if (!is.na(fr)) {
      v25 <- get(fr) %>% filter(season == 2025) %>%
        inner_join(corps25_rc %>% filter(team_c == cd) %>%
                     distinct(player_id), by = "player_id")
      if (nrow(v25)) {
        w25 <- wpick_tr(v25)
        r25m <- wfill_tr(v25$man_grade_pctl, w25)
        r25z <- wfill_tr(v25$zone_grade_pctl, w25)
      }
    }
  }
  if (gate_tr("members_rc", c("team_name", "roster_name",
                              "band", "avail", "mg_f", "mg_p", "c3mg_f",
                              "c3mg_p", "zg_f", "zg_p", "c3zg_f", "c3zg_p",
                              "usage_w"))) {
    rc_cd <- code_in_tr(members_rc$team_name, cd)
    d <- members_rc %>% filter(team_name == rc_cd)
    if (!nrow(d)) cat("   [Receiving] no", rc_cd,
                      "rows in members_rc -- 2026 side skipped\n")
    if (nrow(d)) {
      r26m <- wm_tr(d$mg_p, d$usage_w)
      a26m <- wm_tr(d$c3mg_p, d$usage_w)
      r26z <- wm_tr(d$zg_p, d$usage_w)
      a26z <- wm_tr(d$c3zg_p, d$usage_w)
    }
  }
  if (any(!is.na(c(r25m, a25m, r26m, a26m))))
    rows_yt$recm <- yt_row("Receiving (man)",
                           r25m, a25m, r26m, a26m)
  if (any(!is.na(c(r25z, a25z, r26z, a26z))))
    rows_yt$recz <- yt_row("Receiving (zone)",
                           r25z, a25z, r26z, a26z)
  
  # -- RUSHING. 2026 = memb_lg_ru priced; 2025 = the stamped
  #    committee values, attempt-weighted --
  r25 <- a25 <- r26 <- a26 <- NA_real_
  if (gate_tr("starters_all_ru", c("season", "team",
                                   "player_id", "rb_rank")) &
      gate_tr("rush_season_pctl_sos", c("player_id", "season",
                                        "player", "q_games", "q_atts",
                                        "grun_pctl", "mtf_pctl"))) {
    b <- starters_all_ru %>% filter(season == 2025,
                                    team == cd) %>%
      left_join(rush_season_pctl_sos %>% filter(season == 2025) %>%
                  select(player_id, q_atts, grun_pctl),
                by = "player_id")
    if (!nrow(b)) cat("   [Rushing] no", cd,
                      "2025 rows -- 2025 side skipped\n")
    if (nrow(b)) r25 <- wm_tr(b$grun_pctl, b$q_atts)
    fr <- frame_pick_tr("Rushing adj 2025", c("ru_c3_pctl"),
                        c("player_id", "season", "c3_pctl"))
    if (!is.na(fr)) {
      v25 <- get(fr) %>% filter(season == 2025) %>%
        inner_join(starters_all_ru %>%
                     filter(season == 2025, team == cd) %>%
                     distinct(player_id), by = "player_id")
      if (nrow(v25)) a25 <- wfill_tr(v25$c3_pctl,
                                     wpick_tr(v25))
    }
  }
  if (gate_tr("memb_lg_ru", c("team", "roster_name", "avail",
                              "gf", "gf_av", "V_c3", "V_c3_av", "uw"))) {
    ru_cd <- code_in_tr(memb_lg_ru$team, cd)
    d <- memb_lg_ru %>% filter(team == ru_cd)
    if (!nrow(d)) cat("   [Rushing] no", ru_cd,
                      "rows in memb_lg_ru -- 2026 side skipped\n")
    if (nrow(d)) {
      r26 <- wm_tr(d$gf_av, d$uw)
      a26 <- wm_tr(d$V_c3_av, d$uw)
    }
  }
  if (any(!is.na(c(r25, a25, r26, a26))))
    rows_yt$ru <- yt_row("Rushing", r25, a25, r26, a26)
  
  # -- PASS BLOCK. 2026 = slot builds priced; 2025 = modal
  #    starters' tps, flat across the five slots (OL law);
  #    adjusted 2025 rides pblk_c3_pctl (pb_snaps-weighted) --
  r25 <- a25 <- r26 <- a26 <- NA_real_
  if (gate_tr("starters_all_av", c("season", "team_name",
                                   "player_id", "det_position", "sn")) &
      gate_tr("ol_season_pctl", c("player_id",
                                  "det_position"))) {
    b <- starters_all_av %>% filter(season == 2025,
                                    team_name == cd) %>%
      left_join(ol_season_pctl,
                by = c("player_id", "det_position"))
    if (!nrow(b)) cat("   [Pass block] no", cd,
                      "2025 rows -- 2025 side skipped\n")
    if (nrow(b)) r25 <- if ("tps_grade" %in% names(b))
      m_tr(b$tps_grade) else NA_real_
    fr <- frame_pick_tr("Pass block adj 2025",
                        c("pblk_c3_pctl"),
                        c("player_id", "season", "c3_pctl"))
    if (!is.na(fr)) {
      v25 <- get(fr) %>% filter(season == 2025) %>%
        inner_join(starters_all_av %>%
                     filter(season == 2025,
                            team_name == cd) %>%
                     distinct(player_id), by = "player_id")
      if (nrow(v25)) a25 <- wfill_tr(v25$c3_pctl,
                                     wpick_tr(v25))
    }
  }
  if (gate_tr("slot_value_26_pb_build", c("team_name",
                                          "roster_name", "slot", "avail", "gf_raw",
                                          "gf_raw_av", "gf_adj", "gf_adj_av"))) {
    pb_cd <- code_in_tr(slot_value_26_pb_build$team_name, cd)
    d <- slot_value_26_pb_build %>% filter(team_name == pb_cd)
    if (!nrow(d)) cat("   [Pass block] no", pb_cd,
                      "rows in slot_value_26_pb_build --",
                      "2026 side skipped\n")
    if (nrow(d)) {
      r26 <- m_tr(d$gf_raw_av)
      a26 <- m_tr(d$gf_adj_av)
    }
  }
  if (any(!is.na(c(r25, a25, r26, a26))))
    rows_yt$pb <- yt_row("Pass block", r25, a25, r26, a26)
  
  # -- RUN BLOCK, both schemes. 2026 = slot builds priced;
  #    2025 = modal starters' adjusted c3 (the unit's stamped
  #    2025 currency). ROUND THREE (Andy, 2026-08-30): each row
  #    prefers ITS OWN scheme currency -- gap_c3_pctl /
  #    zone_c3_pctl on the 2025 side, V_c3_gap_av / V_c3_zone_av
  #    on the 2026 side. Honest holes drop out of the 2025 means,
  #    never filled. Missing columns (old frames in session) fall
  #    back to blended + a printed note --
  rb_c3_split_tr <- exists("rblk_c3_pctl") &&
    all(c("gap_c3_pctl", "zone_c3_pctl") %in% names(rblk_c3_pctl))
  rb_split26_tr <- exists("slot_value_26_rb_build") &&
    all(c("V_c3_gap_av", "V_c3_zone_av") %in%
          names(slot_value_26_rb_build))
  r25g <- a25g <- r26g <- a26g <- NA_real_
  r25z <- a25z <- r26z <- a26z <- NA_real_
  if (gate_tr("starters_all_rb", c("season", "team",
                                   "player_id", "det_position", "sn")) &
      gate_tr("rblk_c3_pctl", c("player_id", "season",
                                "c3_pctl"))) {
    if (!rb_c3_split_tr)
      cat("   [Run block] gap_c3_pctl/zone_c3_pctl not on",
          "rblk_c3_pctl -- 2025 adjusted repeats blended;",
          "re-source the currency file for the split\n")
    b <- starters_all_rb %>% filter(season == 2025,
                                    team == cd) %>%
      left_join(rblk_c3_pctl %>% filter(season == 2025) %>%
                  select(player_id, c3_pctl,
                         dplyr::any_of(c("gap_c3_pctl",
                                         "zone_c3_pctl"))),
                by = "player_id")
    if (!nrow(b)) cat("   [Run block] no", cd,
                      "2025 rows -- 2025 side skipped\n")
    if (nrow(b)) {
      if (rb_c3_split_tr) {
        hg <- sum(is.na(b$gap_c3_pctl))
        hz <- sum(is.na(b$zone_c3_pctl))
        if (hg > 0)
          cat(sprintf(paste0("   [Run block] %d of %d modal starters",
                             " carry no 2025 gap currency -- they drop",
                             " out of the adjusted mean\n"), hg, nrow(b)))
        if (hz > 0)
          cat(sprintf(paste0("   [Run block] %d of %d modal starters",
                             " carry no 2025 zone currency -- they drop",
                             " out of the adjusted mean\n"), hz, nrow(b)))
        a25g <- m_tr(b$gap_c3_pctl)
        a25z <- m_tr(b$zone_c3_pctl)
      } else {
        a25g <- m_tr(b$c3_pctl)
        a25z <- a25g
      }
    }
    # the Phase-1 OL raw canon name is unrecorded (the
    # currency file ships c1_obj_rblk = NULL) -- a miss here
    # is the one EXPECTED '--' on this board
    fr <- frame_pick_tr("Run block raw 2025",
                        c("rblk_season_pctl_sos",
                          "rb_season_pctl_sos",
                          "ol_season_pctl"),
                        c("player_id", "season"))
    if (!is.na(fr)) {
      v25 <- get(fr) %>% filter(season == 2025) %>%
        inner_join(starters_all_rb %>%
                     filter(season == 2025, team == cd) %>%
                     distinct(player_id), by = "player_id")
      if (nrow(v25)) {
        g25 <- vpick_tr(v25, c("gap_pctl", "gap_grade_pctl",
                               "grade_gap_pctl"))
        z25 <- vpick_tr(v25, c("zone_pctl",
                               "zone_grade_pctl",
                               "grade_zone_pctl"))
        o25 <- vpick_tr(v25, c("grade_sos_pctl",
                               "grade_pctl", "g_pctl"))
        w25 <- wpick_tr(v25)
        if (!is.null(g25)) r25g <- wfill_tr(g25, w25)
        if (!is.null(z25)) r25z <- wfill_tr(z25, w25)
        if (is.null(g25) && is.null(z25) &&
            !is.null(o25)) {
          r25g <- wfill_tr(o25, w25)
          r25z <- r25g
        }
        if (is.null(g25) && is.null(z25) && is.null(o25))
          cat("   [Run block raw 2025] no value column I",
              "recognize in", fr, "-- cell stays '--'\n")
      }
    }
  }
  if (gate_tr("slot_value_26_rb_build", c("team_name",
                                          "roster_name", "slot", "avail", "V_gap",
                                          "V_gap_av", "V_zone", "V_zone_av", "V_c3",
                                          "V_c3_av"))) {
    rb_cd <- code_in_tr(slot_value_26_rb_build$team_name, cd)
    d <- slot_value_26_rb_build %>% filter(team_name == rb_cd)
    if (!nrow(d)) cat("   [Run block] no", rb_cd,
                      "rows in slot_value_26_rb_build --",
                      "2026 side skipped\n")
    if (nrow(d)) {
      r26g <- m_tr(d$V_gap_av)
      r26z <- m_tr(d$V_zone_av)
      if (rb_split26_tr) {
        a26g <- m_tr(d$V_c3_gap_av)
        a26z <- m_tr(d$V_c3_zone_av)
      } else {
        cat("   [Run block] V_c3_gap_av/V_c3_zone_av not on",
            "slot_value_26_rb_build -- 2026 adjusted repeats",
            "blended; re-source the availability layer for the",
            "split\n")
        a26g <- m_tr(d$V_c3_av)
        a26z <- a26g
      }
    }
  }
  if (any(!is.na(c(r25g, a25g, r26g, a26g))))
    rows_yt$rbg <- yt_row("Run block (gap)",
                          r25g, a25g, r26g, a26g)
  if (any(!is.na(c(r25z, a25z, r26z, a26z))))
    rows_yt$rbz <- yt_row("Run block (zone)",
                          r25z, a25z, r26z, a26z)
  rb_yoy_note_tr <-
    if (rb_c3_split_tr && rb_split26_tr)
      "run block's adjusted value is scheme-split (gap/zone currency)"
  else if (!rb_c3_split_tr && !rb_split26_tr)
    "run block's adjusted value is scheme-neutral and repeats on both rows"
  else paste0("run block's adjusted value is scheme-split on the ",
              if (rb_c3_split_tr) "2025 side only"
              else "2026 side only",
              " (re-source the other file for both)")
  
  # -- PASS RUSH. 2026 = members_pa priced; 2025 = qualified
  #    stints, snaps-weighted raw grade; adjusted 2025
  #    rides prush_c3_pctl (tps-weighted) --
  r25 <- a25 <- r26 <- a26 <- NA_real_
  if (gate_tr("stint25_pa", c("player_id", "team", "g",
                              "qs")) &
      gate_tr("cur_lg", c("player_id", "season", "band", "qg",
                          "qs", "g_pctl"))) {
    b <- stint25_pa %>% filter(team == cd) %>%
      select(player_id) %>%
      inner_join(cur_lg %>% filter(season == 2025) %>%
                   select(player_id, qs, g_pctl),
                 by = "player_id")
    if (!nrow(b)) cat("   [Pass rush] no", cd,
                      "2025 stints -- 2025 side skipped\n")
    if (nrow(b)) r25 <- wm_tr(b$g_pctl, b$qs)
    fr <- frame_pick_tr("Pass rush adj 2025",
                        c("prush_c3_pctl"),
                        c("player_id", "season", "c3_pctl"))
    if (!is.na(fr)) {
      v25 <- get(fr) %>% filter(season == 2025) %>%
        inner_join(stint25_pa %>% filter(team == cd) %>%
                     distinct(player_id), by = "player_id")
      if (nrow(v25)) a25 <- wfill_tr(v25$c3_pctl,
                                     wpick_tr(v25))
    }
  }
  if (gate_tr("members_pa", c("team", "roster_name", "band",
                              "avail", "gf", "gf_p", "c3_f", "c3_p",
                              "uw"))) {
    pa_cd <- code_in_tr(members_pa$team, cd)
    d <- members_pa %>% filter(team == pa_cd)
    if (!nrow(d)) cat("   [Pass rush] no", pa_cd,
                      "rows in members_pa -- 2026 side skipped\n")
    if (nrow(d)) {
      r26 <- wm_tr(d$gf_p, d$uw)
      a26 <- wm_tr(d$c3_p, d$uw)
    }
  }
  if (any(!is.na(c(r25, a25, r26, a26))))
    rows_yt$pa <- yt_row("Pass rush", r25, a25, r26, a26)
  
  # -- SECONDARY, both lenses. 2026 = members_cv priced; 2025
  #    = qualified seasons, snaps-weighted raw grade per
  #    split; adjusted 2025 rides cov_c3_pctl per split --
  r25m <- a25m <- r26m <- a26m <- NA_real_
  r25z <- a25z <- r26z <- a26z <- NA_real_
  if (gate_tr("cov_pctl_sec_lg", c("player_id", "season",
                                   "band", "split", "q_games", "q_snaps",
                                   "grade_pctl", "supp_pctl")) &
      gate_tr("sec_qbgrp_lg", c("player_id", "player",
                                "season", "team"))) {
    key <- sec_qbgrp_lg %>% filter(season == 2025) %>%
      distinct(player_id, tm = team)
    b <- cov_pctl_sec_lg %>% filter(season == 2025) %>%
      inner_join(key, by = "player_id",
                 relationship = "many-to-many") %>%
      filter(tm == cd)
    if (!nrow(b)) cat("   [Secondary] no", cd,
                      "2025 rows -- 2025 side skipped\n")
    if (nrow(b)) {
      bm <- b %>% filter(split == "man")
      bz <- b %>% filter(split == "zone")
      r25m <- wm_tr(bm$grade_pctl, bm$q_snaps)
      r25z <- wm_tr(bz$grade_pctl, bz$q_snaps)
    }
    fr <- frame_pick_tr("Secondary adj 2025",
                        c("cov_c3_pctl"),
                        c("player_id", "season", "split",
                          "c3_pctl"))
    if (!is.na(fr)) {
      v25 <- get(fr) %>% filter(season == 2025) %>%
        inner_join(key %>% filter(tm == cd) %>%
                     distinct(player_id), by = "player_id")
      if (nrow(v25)) {
        vm <- v25 %>% filter(split == "man")
        vz <- v25 %>% filter(split == "zone")
        if (nrow(vm)) a25m <- wfill_tr(vm$c3_pctl,
                                       wpick_tr(vm))
        if (nrow(vz)) a25z <- wfill_tr(vz$c3_pctl,
                                       wpick_tr(vz))
      }
    }
  }
  if (gate_tr("members_cv", c("team", "roster_name", "band",
                              "split", "avail", "grade_f", "grade_p",
                              "c3_f", "adj_p", "usage_w"))) {
    sec_cd <- code_in_tr(members_cv$team, cd)
    d <- members_cv %>% filter(team == sec_cd)
    if (!nrow(d)) cat("   [Secondary] no", sec_cd,
                      "rows in members_cv -- 2026 side skipped\n")
    if (nrow(d)) {
      dm <- d %>% filter(split == "man")
      dz <- d %>% filter(split == "zone")
      r26m <- wm_tr(dm$grade_p, dm$usage_w)
      a26m <- wm_tr(dm$adj_p, dm$usage_w)
      r26z <- wm_tr(dz$grade_p, dz$usage_w)
      a26z <- wm_tr(dz$adj_p, dz$usage_w)
    }
  }
  if (any(!is.na(c(r25m, a25m, r26m, a26m))))
    rows_yt$secm <- yt_row("Secondary (man)",
                           r25m, a25m, r26m, a26m)
  if (any(!is.na(c(r25z, a25z, r26z, a26z))))
    rows_yt$secz <- yt_row("Secondary (zone)",
                           r25z, a25z, r26z, a26z)
  
  # -- RUN DEFENSE. 2026 = members_ra priced; 2025 = qualified
  #    stints, snaps-weighted raw grade; adjusted 2025 rides
  #    rd_c3_pctl (run_snaps-weighted) --
  r25 <- a25 <- r26 <- a26 <- NA_real_
  if (gate_tr("stint25_ra", c("player_id", "team", "g",
                              "qs")) &
      gate_tr("cur_rd_lg", c("player_id", "season", "player",
                             "band", "q_games", "q_snaps",
                             "grade_pctl", "stop_pctl"))) {
    b <- stint25_ra %>% filter(team == cd) %>%
      select(player_id) %>%
      inner_join(cur_rd_lg %>% filter(season == 2025) %>%
                   select(player_id, q_snaps, grade_pctl),
                 by = "player_id")
    if (!nrow(b)) cat("   [Run defense] no", cd,
                      "2025 stints -- 2025 side skipped\n")
    if (nrow(b)) r25 <- wm_tr(b$grade_pctl, b$q_snaps)
    fr <- frame_pick_tr("Run defense adj 2025",
                        c("rd_c3_pctl"),
                        c("player_id", "season", "c3_pctl"))
    if (!is.na(fr)) {
      v25 <- get(fr) %>% filter(season == 2025) %>%
        inner_join(stint25_ra %>% filter(team == cd) %>%
                     distinct(player_id), by = "player_id")
      if (nrow(v25)) a25 <- wfill_tr(v25$c3_pctl,
                                     wpick_tr(v25))
    }
  }
  if (gate_tr("members_ra", c("team", "roster_name", "band",
                              "avail", "grade_f", "grade_p", "c3_f",
                              "c3_p", "usage_w"))) {
    ra_cd <- code_in_tr(members_ra$team, cd)
    d <- members_ra %>% filter(team == ra_cd)
    if (!nrow(d)) cat("   [Run defense] no", ra_cd,
                      "rows in members_ra -- 2026 side skipped\n")
    if (nrow(d)) {
      r26 <- wm_tr(d$grade_p, d$usage_w)
      a26 <- wm_tr(d$c3_p, d$usage_w)
    }
  }
  if (any(!is.na(c(r25, a25, r26, a26))))
    rows_yt$ra <- yt_row("Run defense", r25, a25, r26, a26)
  
  if (!length(rows_yt)) {
    cat("no own-roster frames in session for", cd,
        "-- every gate above printed its reason.\n")
    return(invisible(NULL))
  }
  
  yoy_tr <- bind_rows(rows_yt) %>%
    mutate(d = r26 - r25, ad = a26 - a25,
           unit = factor(unit,
                         levels = c("Receiving (man)",
                                    "Receiving (zone)",
                                    "Rushing", "Pass block",
                                    "Run block (gap)",
                                    "Run block (zone)",
                                    "Pass rush",
                                    "Secondary (man)",
                                    "Secondary (zone)",
                                    "Run defense"))) %>%
    arrange(unit)
  
  cat("\n", cd, " own roster, 2025 vs 2026 (percentiles):\n",
      sep = "")
  print(yoy_tr %>%
          transmute(unit,
                    y25_real = round(100 * r25, 1),
                    y25_adj  = round(100 * a25, 1),
                    y26_proj = round(100 * r26, 1),
                    y26_adj  = round(100 * a26, 1),
                    d_raw_pts = round(100 * d, 1),
                    d_adj_pts = round(100 * ad, 1)) %>%
          as.data.frame())
  cat("\nread: 2025 = real playing time (weighted by what\n")
  cat("they actually played); 2026 = projected playing time\n")
  cat("(misses priced to the backup level). positive d = the\n")
  cat("unit is STRONGER in 2026 than the real 2025 season.\n")
  cat("'--' = canon carries no such currency for that\n")
  cat("unit-year (boundary, not a bug): adjusted 2025 exists\n")
  cat("only for receiving + run block; receiving's 2025 rides\n")
  cat("the adjusted currency.\n")
  
  ov_tr <- yoy_tr %>%
    summarise(r25 = m_tr(r25), a25 = m_tr(a25),
              r26 = m_tr(r26), a26 = m_tr(a26),
              .groups = "drop") %>%
    mutate(d = r26 - r25, ad = a26 - a25)
  if (!is.na(ov_tr$d))
    cat(sprintf(paste0("\nyoy in one number, raw: 2025 %.0f%%",
                       " -> 2026 %.0f%% (%+.1f pts, avg of ",
                       "the unit rows)\n"),
                100 * ov_tr$r25, 100 * ov_tr$r26,
                100 * ov_tr$d))
  if (!is.na(ov_tr$ad))
    cat(sprintf(paste0("yoy in one number, adjusted: 2025 ",
                       "%.0f%% -> 2026 %.0f%% (%+.1f pts, ",
                       "avg of the unit rows)\n"),
                100 * ov_tr$a25, 100 * ov_tr$a26,
                100 * ov_tr$ad))
  
  # the visual -- money position, printed last in the block.
  # Same year-grouped shape as the slate table (the visual
  # Andy picked), own-side colors: red = quality LOST vs 2025.
  gt_yoy_tr <- yoy_tr %>%
    select(unit, r25, a25, r26, a26, d, ad) %>%
    gt(rowname_col = "unit") %>%
    tab_spanner(label = "2025 -- real playing time",
                columns = c(r25, a25)) %>%
    tab_spanner(label = "2026 -- projected playing time",
                columns = c(r26, a26)) %>%
    tab_spanner(label = "\u0394 (26 minus 25)",
                columns = c(d, ad)) %>%
    cols_label(r25 = "raw", a25 = "adjusted",
               r26 = "raw", a26 = "adjusted",
               d = "raw", ad = "adjusted") %>%
    fmt_percent(columns = c(r25, a25, r26, a26),
                decimals = 0) %>%
    fmt_percent(columns = c(d, ad), decimals = 1,
                force_sign = TRUE) %>%
    sub_missing(missing_text = "--") %>%
    data_color(columns = c(d, ad),
               fn = scales::col_numeric(
                 palette = c("#C60C30", "#f7f7f7", "#6baed6"),
                 domain = NULL, na.color = "#f7f7f7"),
               autocolor_text = TRUE) %>%
    tab_header(
      title = paste0(cd, " 2025 vs 2026 -- own roster, real ",
                     "vs projected playing time"),
      subtitle = paste0(
        "2025 = the real season, weighted by the snaps/",
        "games/attempts they actually played | 2026 = the ",
        "projected roster, expected misses priced to the ",
        "measured backup level (the same 'after' currency as ",
        "the injury table) | OUR side: red = ", cd, " lost ",
        "quality vs 2025, colors flip from the opponent ",
        "tables | '--' = that year/currency frame was not in ",
        "session (the console names what rode and what was ",
        "tried); with the currency-three chain sourced the ",
        "one expected '--' is run block raw 2025, whose ",
        "Phase-1 OL canon name is unrecorded | ",
        "deltas only where both years carry the currency | ",
        "receiving + run block 2025 are flat means (corps + ",
        "OL laws), the rest playing-time-weighted | ",
        rb_yoy_note_tr)) %>%
    grand_summary_rows(
      columns = c(r25, a25, r26, a26, d, ad),
      fns = list(`ALL UNITS (avg of rows)` ~ m_tr(.)),
      fmt = ~ fmt_percent(., columns = c(r25, a25, r26, a26),
                          decimals = 0) %>%
        fmt_percent(., columns = c(d, ad), decimals = 1,
                    force_sign = TRUE)) %>%
    tab_options(table.font.size = px(12),
                data_row.padding = px(3),
                column_labels.font.weight = "bold",
                grand_summary_row.background.color = "#f0f0f0",
                grand_summary_row.text_transform = "inherit")
  print(gt_yoy_tr)
  invisible(list(data = yoy_tr, gt = gt_yoy_tr))
}

# ------------------------------------------------------------
# 3. TEAM_LASTYEAR -- the 2025 players, unit by unit: who
#    held the jobs, their 2025 values, and where they are
#    now (roster route). Each unit soft-gates on its own
#    frames -- one missing frame skips one board, prints why.
# ------------------------------------------------------------

# name plumbing: bridges differ per unit. Use the unit's own
# bridge to gsis, then the 2025 roster for display names.
ros25_nm_tr <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      ros <- nflreadr::load_rosters(2025) %>%
        filter(!is.na(gsis_id)) %>%
        distinct(gsis_id, .keep_all = TRUE)
      cache <<- setNames(ros$full_name, ros$gsis_id)
    }
    cache
  }
})
# name plumbing: one merged player_id -> display-name lookup,
# built from whichever bridges are in session (defense xw
# frames, receiving rec_id_bridge). First use prints which
# bridges rode; a player with no bridge match shows his id.
nm_pool_tr <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      pool <- NULL
      rode <- character(0)
      for (bnm in c("xw_rd_lg", "xw_lg", "rec_id_bridge",
                    "combined_ids")) {
        if (exists(bnm)) {
          br <- get(bnm)
          if (all(c("player_id", "gsis_id") %in% names(br))) {
            pool <- bind_rows(pool,
                              br %>% filter(!is.na(player_id),
                                            !is.na(gsis_id)) %>%
                                select(player_id, gsis_id))
            rode <- c(rode, bnm)
          }
        }
      }
      if (is.null(pool)) {
        # skip-never-die (audit point): bridge-less session =
        # names print as player ids on this board, never a stop
        cat("   (no id bridge in session -- names print as",
            "player ids)\n")
        pool <- tibble(player_id = numeric(0),
                       gsis_id = character(0))
      }
      pool <- pool %>% distinct(player_id, .keep_all = TRUE)
      cache <<- setNames(
        unname(ros25_nm_tr()[as.character(pool$gsis_id)]),
        as.character(pool$player_id))
      if (length(rode))
        cat("   (name lookup built on:", paste(rode,
                                               collapse = ", "), ")\n")
    }
    cache
  }
})
nm_via_bridge_tr <- function(player_ids) {
  lk <- nm_pool_tr()
  nm <- unname(lk[as.character(player_ids)])
  ifelse(is.na(nm), as.character(player_ids), nm)
}

team_lastyear <- function(code) {
  cd <- norm_code_tr(code)
  cat("\n=====================================================\n")
  cat(" ", cd, "2025 -- LAST YEAR'S PLAYERS, unit by unit.\n")
  cat("  Values are 2025 qualified-season percentiles.\n")
  cat("  'where now' = 2026 roster route.\n")
  cat("======================================================\n")
  
  ly_rows <- list()   # collected per unit for the gt board
  
  # -- RUN DEFENSE (names ride in cur_rd_lg) --
  if (gate_tr("stint25_ra", c("player_id", "team", "g",
                              "qs")) &
      gate_tr("cur_rd_lg", c("player_id", "season", "player",
                             "band", "q_games", "q_snaps",
                             "grade_pctl", "stop_pctl"))) {
    b <- stint25_ra %>% filter(team == cd) %>%
      select(player_id, team, stint_g = g, stint_qs = qs) %>%
      inner_join(cur_rd_lg %>% filter(season == 2025),
                 by = "player_id") %>%
      mutate(where = where_now_tr(player, cd)) %>%
      arrange(match(band, c("DI", "ED", "LB", "S")),
              desc(q_snaps))
    ly_rows$rd <- b %>% transmute(unit = "Run defense",
                                  player, lens = NA_character_, detail = band, g = q_games,
                                  snaps = q_snaps, v1 = grade_pctl, v2 = stop_pctl, where)
    cat("\n-- 2025 run defenders (qualified stints):\n")
    print(b %>% transmute(player, band, games = q_games,
                          run_snaps = q_snaps,
                          grade_pctl = round(grade_pctl, 3),
                          stop_pctl = round(stop_pctl, 3),
                          where) %>% as.data.frame())
  }
  # -- PASS RUSH (names via the xw_lg bridge) --
  # names ride the nm pool, so xw_lg is no longer a hard gate
  if (gate_tr("stint25_pa", c("player_id", "team", "g",
                              "qs")) &
      gate_tr("cur_lg", c("player_id", "season", "band", "qg",
                          "qs", "g_pctl"))) {
    b <- stint25_pa %>% filter(team == cd) %>%
      select(player_id, team, stint_g = g, stint_qs = qs) %>%
      inner_join(cur_lg %>% filter(season == 2025),
                 by = "player_id") %>%
      mutate(player = unname(nm_via_bridge_tr(player_id)),
             where = where_now_tr(player, cd)) %>%
      arrange(match(band, c("ED", "DI")), desc(qs))
    ly_rows$pa <- b %>% transmute(unit = "Pass rush",
                                  player, lens = NA_character_, detail = band, g = qg,
                                  snaps = qs, v1 = g_pctl, v2 = NA_real_, where)
    cat("\n-- 2025 pass rushers (qualified stints):\n")
    print(b %>% transmute(player, band, games = qg,
                          rush_snaps = qs,
                          grade_pctl = round(g_pctl, 3),
                          where) %>% as.data.frame())
  }
  # -- SECONDARY (names + team ride in sec_qbgrp_lg; renamed
  #    at key construction so a name column on the value frame
  #    can never collide the join into player.x/player.y) --
  if (gate_tr("cov_pctl_sec_lg", c("player_id", "season",
                                   "band", "split", "q_games",
                                   "q_snaps", "grade_pctl",
                                   "supp_pctl")) &
      gate_tr("sec_qbgrp_lg", c("player_id", "player",
                                "season", "team"))) {
    key <- sec_qbgrp_lg %>% filter(season == 2025) %>%
      distinct(player_id, plyr = player, tm = team)
    b <- cov_pctl_sec_lg %>% filter(season == 2025) %>%
      inner_join(key, by = "player_id",
                 relationship = "many-to-many") %>%
      filter(tm == cd) %>%
      mutate(where = where_now_tr(plyr, cd)) %>%
      arrange(band, split, desc(q_snaps))
    ly_rows$sec <- b %>% transmute(unit = "Secondary",
                                   player = plyr, lens = split, detail = band, g = q_games,
                                   snaps = q_snaps, v1 = grade_pctl, v2 = supp_pctl, where)
    cat("\n-- 2025 secondary (qualified seasons, per split):\n")
    print(b %>% transmute(player = plyr, band, split,
                          games = q_games, snaps = q_snaps,
                          grade_pctl = round(grade_pctl, 3),
                          supp_pctl = round(supp_pctl, 3),
                          where) %>% as.data.frame())
  }
  # -- RECEIVING (corps membership + adjusted 2025 values) --
  if (gate_tr("corps25_rc", c("player_id", "team_c")) &
      gate_tr("rec_band_season", c("player_id", "season",
                                   "band")) &
      gate_tr("rec_c3_pctl", c("player_id", "season", "split",
                               "c3_grade_pctl",
                               "c3_yprr_pctl"))) {
    v <- rec_c3_pctl %>% filter(season == 2025,
                                !is.na(player_id)) %>%
      select(player_id, split, c3_grade_pctl, c3_yprr_pctl) %>%
      tidyr::pivot_wider(names_from = split,
                         values_from = c(c3_grade_pctl,
                                         c3_yprr_pctl),
                         names_glue = "{.value}_{split}")
    b <- corps25_rc %>% filter(team_c == cd) %>%
      left_join(rec_band_season %>% filter(season == 2025) %>%
                  select(player_id, band),
                by = "player_id") %>%
      left_join(v, by = "player_id") %>%
      mutate(player = unname(nm_via_bridge_tr(player_id)),
             where = where_now_tr(player, cd)) %>%
      arrange(band, desc(c3_grade_pctl_zone))
    ly_rows$rec <- bind_rows(
      b %>% transmute(unit = "Receiving", player, lens = "man",
                      detail = band, g = NA_real_, snaps = NA_real_,
                      v1 = c3_grade_pctl_man, v2 = c3_yprr_pctl_man, where),
      b %>% transmute(unit = "Receiving", player, lens = "zone",
                      detail = band, g = NA_real_, snaps = NA_real_,
                      v1 = c3_grade_pctl_zone, v2 = c3_yprr_pctl_zone, where))
    cat("\n-- 2025 receiving corps (top-8 corps law; values are\n")
    cat("   the adjusted currency, labeled):\n")
    print(b %>% transmute(player, band,
                          adj_grade_man = round(c3_grade_pctl_man,
                                                3),
                          adj_grade_zone = round(c3_grade_pctl_zone,
                                                 3),
                          adj_yprr_man = round(c3_yprr_pctl_man,
                                               3),
                          adj_yprr_zone = round(c3_yprr_pctl_zone,
                                                3),
                          where) %>% as.data.frame())
  }
  # -- RUSHING (names ride in rush_season_pctl_sos) --
  if (gate_tr("starters_all_ru", c("season", "team",
                                   "player_id", "rb_rank")) &
      gate_tr("rush_season_pctl_sos", c("player_id", "season",
                                        "player", "q_games",
                                        "q_atts", "grun_pctl",
                                        "mtf_pctl"))) {
    b <- starters_all_ru %>% filter(season == 2025, team == cd) %>%
      left_join(rush_season_pctl_sos %>% filter(season == 2025) %>%
                  select(player_id, player, q_games, q_atts,
                         grun_pctl, mtf_pctl),
                by = "player_id") %>%
      mutate(player = dplyr::coalesce(
        player, unname(nm_via_bridge_tr(player_id))),
        where = where_now_tr(player, cd)) %>%
      arrange(rb_rank)
    ly_rows$ru <- b %>% transmute(unit = "Rushing", player,
                                  lens = NA_character_, detail = paste0("RB", rb_rank),
                                  g = q_games, snaps = q_atts, v1 = grun_pctl, v2 = mtf_pctl,
                                  where)
    cat("\n-- 2025 rushing committee (job-holders):\n")
    print(b %>% transmute(player, games = q_games,
                          attempts = q_atts,
                          grade_pctl = round(grun_pctl, 3),
                          mtf_pctl = round(mtf_pctl, 3),
                          where) %>% as.data.frame())
  }
  # -- PASS BLOCK (modal starters; canon single-season value
  #    frame is 2025-only per its own comment) --
  if (gate_tr("starters_all_av", c("season", "team_name",
                                   "player_id", "det_position",
                                   "sn")) &
      gate_tr("ol_season_pctl", c("player_id",
                                  "det_position")) &
      have_tr("ol_pos_levels")) {
    b <- starters_all_av %>% filter(season == 2025,
                                    team_name == cd) %>%
      left_join(ol_season_pctl, by = c("player_id",
                                       "det_position")) %>%
      mutate(player = unname(nm_via_bridge_tr(player_id)),
             where = where_now_tr(player, cd)) %>%
      arrange(match(det_position, ol_pos_levels))
    ly_rows$pb <- b %>%
      mutate(v1 = if ("tps_grade" %in% names(b)) tps_grade else
        NA_real_) %>%
      transmute(unit = "Pass block", player, lens = NA_character_,
                detail = det_position, g = NA_real_, snaps = sn, v1,
                v2 = NA_real_, where)
    cat("\n-- 2025 pass-block starters (season-modal per slot):\n")
    print(b %>% select(player, slot = det_position, snaps = sn,
                       dplyr::any_of("tps_grade"), where) %>%
            as.data.frame())
  }
  # -- RUN BLOCK (modal starters; adjusted 2025 value) --
  #    ROUND THREE: the board carries the scheme columns (v1 =
  #    gap, v2 = zone) when the currency frame has them; blended
  #    stays on the console print. '--' = never earned that
  #    scheme's currency -- honest hole, never filled. Fallback:
  #    blended board + a printed note.
  rb_ly_split_tr <- exists("rblk_c3_pctl") &&
    all(c("gap_c3_pctl", "zone_c3_pctl") %in% names(rblk_c3_pctl))
  if (gate_tr("starters_all_rb", c("season", "team",
                                   "player_id", "det_position",
                                   "sn")) &
      gate_tr("rblk_c3_pctl", c("player_id", "season",
                                "c3_pctl")) &
      have_tr("ol_pos_levels")) {
    b <- starters_all_rb %>% filter(season == 2025,
                                    team == cd) %>%
      left_join(rblk_c3_pctl %>% filter(season == 2025) %>%
                  select(player_id, c3_pctl,
                         dplyr::any_of(c("gap_c3_pctl", "zone_c3_pctl",
                                         "gap_qual_g", "zone_qual_g"))),
                by = "player_id") %>%
      mutate(player = unname(nm_via_bridge_tr(player_id)),
             where = where_now_tr(player, cd)) %>%
      arrange(match(det_position, ol_pos_levels))
    if (rb_ly_split_tr) {
      ly_rows$rb <- b %>% transmute(unit = "Run block", player,
                                    lens = NA_character_, detail = det_position,
                                    g = NA_real_, snaps = sn,
                                    v1 = gap_c3_pctl, v2 = zone_c3_pctl,
                                    where)
      cat("\n-- 2025 run-block starters (season-modal per slot;\n")
      cat("   value = adjusted currency; the board carries gap /\n")
      cat("   zone, blended below; '--' = never earned that scheme's\n")
      cat("   currency, honest hole; *_g = qualifying games):\n")
      print(b %>% transmute(player, slot = det_position,
                            snaps = sn,
                            blended = round(c3_pctl, 3),
                            gap = round(gap_c3_pctl, 3),
                            zone = round(zone_c3_pctl, 3),
                            gap_g = gap_qual_g, zone_g = zone_qual_g,
                            where) %>% as.data.frame())
    } else {
      cat("   [Run block] gap_c3_pctl/zone_c3_pctl not on",
          "rblk_c3_pctl -- board shows blended; re-source the",
          "currency file for the split\n")
      ly_rows$rb <- b %>% transmute(unit = "Run block", player,
                                    lens = NA_character_, detail = det_position,
                                    g = NA_real_, snaps = sn,
                                    v1 = c3_pctl, v2 = NA_real_, where)
      cat("\n-- 2025 run-block starters (season-modal per slot;\n")
      cat("   value = adjusted currency):\n")
      print(b %>% transmute(player, slot = det_position,
                            snaps = sn,
                            adj_pctl = round(c3_pctl, 3),
                            where) %>% as.data.frame())
    }
  }
  rb_ly_note_tr <- if (rb_ly_split_tr)
    "adjusted gap/zone" else "adjusted"
  cat("\nnote: a traded player appears under each 2025 team he\n")
  cat("qualified for; his season value is whole-season.\n")
  
  if (!length(ly_rows)) return(invisible(NULL))
  ly_all <- bind_rows(ly_rows)
  
  # the retention math, DONE (Andy, 2026-08-23: "i have to do
  # some mental math like he is / isn't here. what the fuck")
  # -- players per unit; snaps too where the unit carries one
  # row per player
  ret <- ly_all %>% distinct(unit, player, where) %>%
    group_by(unit) %>%
    summarise(k = sum(where == "still here"), n = n(),
              .groups = "drop")
  snp <- ly_all %>%
    filter(unit %in% c("Run defense", "Pass rush", "Rushing",
                       "Pass block", "Run block")) %>%
    group_by(unit) %>%
    summarise(sn = if (all(is.na(snaps))) NA_real_ else
      sum(snaps[where == "still here"], na.rm = TRUE) /
        sum(snaps, na.rm = TRUE), .groups = "drop")
  cat("\n-- 2025 -> 2026 retention:\n")
  for (u in ret$unit) {
    s <- snp$sn[snp$unit == u]
    extra <- if (length(s) && !is.na(s))
      sprintf(" | %.0f%% of snaps", 100 * s) else ""
    cat(sprintf("   %-12s %d/%d still here%s\n",
                u, ret$k[ret$unit == u], ret$n[ret$unit == u],
                extra))
  }
  cat(sprintf("   ALL          %d/%d still here\n",
              sum(ret$k), sum(ret$n)))
  
  # the visual: one board, units as row groups, departures in
  # color so 'who left' needs zero eyeballing. Printed LAST in
  # the block (viewer law)
  ly_ord <- ly_all %>%
    mutate(unit = factor(unit, levels = c("Receiving",
                                          "Rushing", "Pass block", "Run block", "Pass rush",
                                          "Secondary", "Run defense"))) %>%
    arrange(unit)
  gone_ix <- which(grepl("^->", ly_ord$where))
  out_ix  <- which(ly_ord$where == "no 2026 roster")
  gt_ly_tr <- ly_ord %>%
    gt(groupname_col = "unit", rowname_col = "player") %>%
    tab_header(
      title = paste0(cd, " 2025 -- last year's players, ",
                     "unit by unit"),
      subtitle = paste0(
        "value currencies -- run D: grade + stop rate | pass ",
        "rush: grade | secondary: grade + suppression | ",
        "receiving: ADJUSTED grade + YPRR (canon boundary: ",
        "receiving's stamped 2025 currency is the adjusted ",
        "one) | rushing: grade + missed-tackle | pass block: ",
        "tps | run block: ", rb_ly_note_tr, " | red = moved on ",
        "in 2026, ",
        "grey = no 2026 roster | '--' = the unit doesn't ",
        "carry that column")) %>%
    tab_spanner(label = "2025 season",
                columns = c(g, snaps, v1, v2)) %>%
    cols_label(lens = "lens", detail = "band/slot", g = "g",
               snaps = "snaps", v1 = "value", v2 = "2nd value",
               where = "where now") %>%
    fmt_percent(columns = c(v1, v2), decimals = 0) %>%
    fmt_number(columns = c(g, snaps), decimals = 0) %>%
    sub_missing(missing_text = "--") %>%
    tab_options(table.font.size = px(12),
                data_row.padding = px(3),
                column_labels.font.weight = "bold",
                row_group.font.weight = "bold")
  if (length(gone_ix))
    gt_ly_tr <- gt_ly_tr %>%
    tab_style(style = cell_text(color = "#C60C30"),
              locations = cells_body(columns = where,
                                     rows = gone_ix))
  if (length(out_ix))
    gt_ly_tr <- gt_ly_tr %>%
    tab_style(style = cell_text(color = "#8a8a8a",
                                style = "italic"),
              locations = cells_body(columns = where,
                                     rows = out_ix))
  print(gt_ly_tr)
  invisible(list(data = ly_all, gt = gt_ly_tr))
}

# ------------------------------------------------------------
# 4. TEAM_REPORT -- the easy button. Console blocks first,
#    the money table LAST (viewer law).
# ------------------------------------------------------------

team_report <- function(code) {
  cd <- norm_code_tr(code)
  team_slate(cd)
  team_sched(cd)
  team_lastyear(cd)
  team_yoy(cd)
  invisible(team_own(cd))
}

# ------------------------------------------------------------
# Andy, 2026-08-23: "PLEASE INCLUDE team_slate('SEA') ... AT
# THE END. I DON'T WANT TO HAVE TO UNCOMMENT THAT GODDAMN
# SHIT EVERY TIME." -- the four calls, live, his order.
# ------------------------------------------------------------

check_pipeline_tr()
team_slate("SEA")
team_own("SEA")
team_lastyear("SEA")
team_report("SEA")
# Andy: "I WANT THIS AS PART OF THE FILE" -- the year board
# rides the tail too, last on screen (money position).
team_yoy("SEA")
