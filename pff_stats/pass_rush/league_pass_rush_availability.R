# ============================================================
# PASS-RUSH AVAILABILITY LAYER -- opponent pass rushers, 2026
# slates priced by availability (single grade lens, bands ED /
# DI). The sixth and last unit layer.
# Built 2026-08-22 after the file Andy kept sending turned out
# to be the 08-17 adjusted-edition lookup three times running
# -- the availability layer for this unit never existed.
#
# WHAT THIS ASKS: if an opponent's rushers miss time in 2026,
# how much pass-rush quality does NE's pass protection
# actually dodge? Healthy slate vs injury-priced slate, raw
# and adjusted, plus the league sweep and the NE visual.
#
# DIRECTION (offense side): higher pctl = better rusher =
# HARDER for NE's protection. A negative real jump means the
# slate got softer once injuries are priced in.
#
# STAMPS CARRIED (unchanged laws):
#   - PRICING LAW: value_priced = avail * value_healthy +
#     (1 - avail) * repl. avail judged ONLY on job-holding
#     seasons. Zero job seasons -> observed first-year rate
#     (rookie/backup lines, 2024-25 first-year holders, 2023
#     excluded for left-censoring, empty cells -> pooled unit
#     line with an evidence column). repl = empirical 2025
#     backup level (qualified non-rotation stints).
#   - THREE CURRENCIES + firewall: raw season pctls and c3
#     adjusted, priced always labeled, FACED-2025 untouched.
#   - FULL-PRECISION LAW: compute unrounded; rounding is
#     display at print only, never before a wall. Walls at
#     1e-8 against unrounded canon objects.
#   - PRINT LAW: short English phrases; every table opens
#     with its question; display renames at print; internals
#     keep contract names.
#   - CROSSED-STREAMS LAW: consumes unit-unique canon names
#     ONLY (the _lg league frames, rot_2026 guarded by a band
#     identity stop, prush_* c3 frames); canons never edited
#     or re-assigned; runs in any session order after SOURCE
#     ORDER.
#   - SEQUENTIAL-MASKING LAW: counts in summarise, ratios in
#     mutate, uniquely named intermediates.
#   - NA-KEY JOIN INSURANCE: every right-side id-keyed frame
#     is filtered !is.na(player_id) first.
#   - unname() is LOAD-BEARING on named lookup vectors.
#   - WALLS print offenders and evidence BEFORE stopping.
#   - ZERO ATHENA in this file. nflreadr roster pulls only
#     (the canon league pattern).
#   - SCORED-WEIGHT LAW (the 2026-08-22 receiving receipt):
#     any two-step re-pool carries per-metric scored weights,
#     so weighted means re-compose EXACTLY, not by luck.
#
# UNIT ADAPTATIONS (this unit's shape -- Andy can veto):
#   1. ONE availability per player (pass rush has no splits).
#      played = the canon gate true_pass_set_snaps >= X_LG
#      (3) in a REG week (week <= 18). Denominator 17.
#   2. job = a QUALIFIED season (qg >= G_LG = 6 -- the canon's
#      own rotation floor), 2023-2025 window. (Receiving used
#      a top-N corps; this unit's canon defines rotation by
#      the floor -- flagged for veto.)
#   3. Snap-weighted everywhere (usage_w / uw).
#   4. FACED 2025 follows canon's FACED FILL LAW (a game with
#      no season value prices at the band prior; honesty =
#      fill share). Adjusted faced fills at the c3 band prior,
#      same game set (faced_games_lg + season c3 join).
#   5. Raw fills = canon's band priors (prior_lg). Adjusted
#      fills = the c3 entry prior per band, rebuilt here with
#      the adjusted edition's exact construction.
#   6. PHANTOMS ARE CANON on this unit (unlike receiving):
#      phantom slots carry the band prior value and price at
#      the backup line. Canon values untouched.
#   7. League healthy rebuild walled vs team26_lg at 1e-8 and
#      the sweep's healthy side vs league_prush at 1e-8,
#      BEFORE pricing enters.
#   8. Rookies enter NE-opponent rotations only via canon's
#      rot_2026 -- this layer adds nobody.
#
# SOURCE ORDER: new_england_opp_pass_rush_schedule.R (rot_2026,
#   cmp_prush_slate, sched_2026, opp_2026_teams) ->
#   league_opp_pass_rush_schedule.R (full_pass_rush_qbgrp, the
#   _lg frames, faced_games_lg, league_prush) ->
#   league_pass_rush_evaluating_currency_three.R (prush_c3_pctl)
#   -> THIS FILE. (in_season / blend2 / pff_team_lookup ride in
#   from any canon; gates verify.)
#   NAME-COLLISION WARNING: rot_2026 is a SHARED canon scratch
#   name -- the run-defense canon (sourced 2026-08-22 for the
#   run-defense layer) rebuilds rot_2026 with DI/ED/LB/S bands.
#   The pass-rush canon must be the LAST canon sourced before
#   this file. This file's own products are all _pa-suffixed
#   and stay safe no matter what is sourced after them.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(gt)
})

# ------------------------------------------------------------
# 0. GATES -- frames, column receipts, unit identity, knobs.
# ------------------------------------------------------------

needed_pa <- c("full_pass_rush_qbgrp", "rot_2026", "cmp_prush_slate",
               "sched_2026", "opp_2026_teams", "qual_lg", "cur_lg",
               "modal_lg", "rot_lg", "ph_lg", "team26_lg",
               "faced_games_lg", "faced_lg", "league_prush",
               "ros_lg", "use_lg", "band_lg", "slot_lg",
               "slot_med_lg", "prior_lg", "xw_lg", "fb_lg",
               "prush_c3_pctl", "entry_years_def", "opp26_lg",
               "pff32_lg", "blend2", "in_season", "pff_team_lookup",
               "X_LG", "G_LG", "NB_LG", "BANDS_LG")
missing_pa <- needed_pa[!vapply(needed_pa, exists, logical(1))]
if (length(missing_pa)) {
  cat("missing session objects:\n"); print(missing_pa)
  stop("see SOURCE ORDER in header")
}

req_cols_pa <- list(
  full_pass_rush_qbgrp = c("player_id", "season", "week", "def_ssn",
                           "qbgrp_ssn", "position",
                           "true_pass_set_snap_counts_pass_rush"),
  rot_2026   = c("team_name", "roster_name", "band", "status",
                 "prior_used", "usage_w", "grade_f"),
  cmp_prush_slate = c("band", "grade_25", "fill_share_25",
                      "grade_26", "d_grade"),
  qual_lg    = c("player_id", "season", "week", "def_ssn",
                 "position", "true_pass_set_snap_counts_pass_rush"),
  cur_lg     = c("player_id", "band", "season", "qg", "qs",
                 "g_pctl"),
  modal_lg   = c("player_id", "season", "band"),
  rot_lg     = c("team", "band", "gf", "uw"),
  ph_lg      = c("team", "band", "gf", "uw"),
  team26_lg  = c("team", "g26"),
  faced_games_lg = c("player_id", "week", "focal", "team", "band",
                     "sn", "gp", "fill"),
  faced_lg   = c("focal", "g25", "fill_share"),
  league_prush = c("focal", "g26", "g25", "d_grade"),
  ros_lg     = c("gsis_id", "team", "entry_year", "player_id"),
  use_lg     = c("player_id", "qs_2024", "qs_2025",
                 "qg_2024", "qg_2025", "g_pctl_2024", "g_pctl_2025"),
  band_lg    = c("player_id", "band"),
  slot_lg    = c("team", "band", "rk", "sn_slot"),
  slot_med_lg = c("band", "rk", "sn_med"),
  prior_lg   = c("band", "prg"),
  xw_lg      = c("player_id", "gsis_id"),
  prush_c3_pctl = c("player_id", "season", "band", "qual_g",
                    "c3_pctl"),
  entry_years_def = c("player_id", "entry_year"),
  opp26_lg   = c("focal", "opp"))
for (nm_pa in names(req_cols_pa)) {
  fr_pa <- get(nm_pa)
  miss_pa <- setdiff(req_cols_pa[[nm_pa]], names(fr_pa))
  if (length(miss_pa)) {
    cat("frame:", nm_pa, "-- missing columns:\n"); print(miss_pa)
    cat("actual columns:\n"); print(names(fr_pa))
    stop("column receipt printed above -- crossed-streams guard")
  }
}

# unit identity: pass-rush bands only on both membership frames.
# rot_2026 is a SHARED canon scratch name (the run-defense canon
# builds its own rot_2026 with DI/ED/LB/S bands) -- if that canon
# was sourced after the pass-rush canon, the run-defense version
# is squatting on the name. Print what we FOUND before stopping.
if (!all(rot_2026$band %in% c("ED", "DI"))) {
  cat("rot_2026 band census -- expected ED/DI only, found:\n")
  print(sort(unique(rot_2026$band)))
  cat("rows:", nrow(rot_2026), "\n")
  cat("read: another canon rebuilt rot_2026 after the pass-rush\n")
  cat("canon (run defense carries LB/S bands). Fix = source\n")
  cat("order: re-source new_england_opp_pass_rush_schedule.R and\n")
  cat("league_opp_pass_rush_schedule.R, then THIS file.\n")
  stop("rot_2026 identity stop -- squatter on a shared name")
}
stopifnot(all(rot_lg$band %in% c("ED", "DI")))
stopifnot(all(BANDS_LG %in% c("ED", "DI")))

# knobs -- consumed from the league canon, never re-pinned here
AVAIL_SEASONS_PA <- 2023:2025
AVAIL_DENOM_PA   <- 17L
cat("\n== PASS-RUSH AVAILABILITY LAYER ==\n")
cat("gates:", X_LG, "snaps = played |", G_LG, "games = qualified",
    "| rotation", NB_LG, "deep per band | denom",
    AVAIL_DENOM_PA, "\n")

# season coverage receipt + stop (the availability window must
# exist in the weekly base)
ssn_cov_pa <- sort(unique(qual_lg$season))
cat("weekly base seasons:", paste(ssn_cov_pa, collapse = " "), "\n")
if (!all(AVAIL_SEASONS_PA %in% ssn_cov_pa)) {
  cat("window seasons missing:", setdiff(AVAIL_SEASONS_PA, ssn_cov_pa), "\n")
  stop("availability window not covered by full_pass_rush_qbgrp")
}

# helpers (layer-local names; never the shared session ones)
denan_pa <- function(x) dplyr::if_else(is.nan(x), NA_real_, x)
wall_pa <- function(tag, chk_df, diff_col, tol = 1e-8) {
  d_vec <- abs(chk_df[[diff_col]]); d_vec <- d_vec[!is.na(d_vec)]
  if (!length(d_vec)) stop(tag, " -- all-NA diff column, nothing compared -- report")
  d <- max(d_vec)
  cat(sprintf("%-56s max |d| = %.3g -> %s\n", tag, d,
              ifelse(d <= tol, "OK", "FAIL")))
  if (d > tol) {
    print(chk_df %>% filter(abs(.data[[diff_col]]) > tol), n = Inf)
    stop(tag, " wall breach -- evidence printed above")
  }
  invisible(d)
}

# anti-staleness marker: if the run dies, the viewer shows THIS,
# never a previous unit's table
gt_marker_pa <- tibble::tibble(
  status = paste0("PASS-RUSH availability layer sourced ",
                  format(Sys.time(), "%H:%M:%S"),
                  " -- the table builds in section 8."),
  if_you_see_this = "the run stopped early -- read the console tail") %>%
  gt()
print(gt_marker_pa)

# ------------------------------------------------------------
# 0b. c3 ENTRY PRIOR per band -- the adjusted edition's exact
#     construction, rebuilt so this layer does not depend on it
# ------------------------------------------------------------

c3_entry_prior_pa <- prush_c3_pctl %>%
  inner_join(entry_years_def %>% filter(!is.na(player_id)),
             by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2017) %>%
  group_by(band) %>%
  summarise(pr_c3 = median(c3_pctl, na.rm = TRUE),
            n_entry = dplyr::n(), .groups = "drop")

cat("\n--- c3 entry prior by band (entry-season median, 2017+) ---\n")
print(c3_entry_prior_pa)
stopifnot(!any(is.na(c3_entry_prior_pa$pr_c3)))

# ------------------------------------------------------------
# 1. HISTORY -- played weeks and job seasons, 2023-2025.
#    played = canon gate (snaps >= X_LG) in a REG week.
#    job    = qualified season (qg >= G_LG), the canon's own
#             rotation floor.
# ------------------------------------------------------------

# played = DISTINCT REG weeks over the gate. qual_lg is one row
# per player-week-position; a rusher qualifying at ED and DI in
# the same week is still ONE played week (the stamped
# receiving-layer law: distinct weeks, then count).
games_pa <- qual_lg %>%
  filter(season %in% AVAIL_SEASONS_PA, week <= 18) %>%
  distinct(player_id, season, week) %>%
  count(player_id, season, name = "played_g")

hist_pa <- cur_lg %>%
  filter(season %in% AVAIL_SEASONS_PA) %>%
  left_join(games_pa %>% filter(!is.na(player_id)),
            by = c("player_id", "season")) %>%
  mutate(played_g = dplyr::coalesce(played_g, 0L),
         # capped at 1: 18 distinct REG weeks via mid-season trade =
         # a fully available season (the 28075 case). Same pmin as
         # the stamped pass-block layer.
         avail_s  = pmin(played_g / AVAIL_DENOM_PA, 1))

avail_pa <- hist_pa %>%
  group_by(player_id) %>%
  summarise(yrs       = dplyr::n(),
            job_yrs   = dplyr::n(),
            avail_job = mean(avail_s),
            .groups = "drop")

# availability bounds wall at birth: played_g counts DISTINCT REG
# weeks; a multi-position week is ONE played week. If a
# double-counted week ever sneaks back in, the offender prints
# HERE, at the unit that owns the number.
if (any(avail_pa$avail_job > 1 + 1e-8) ||
    any(avail_pa$avail_job < 0)) {
  cat("availability outside [0,1] -- offenders:\n")
  print(avail_pa %>% filter(avail_job > 1 | avail_job < 0),
        n = Inf)
  stop("availability bounds breach -- evidence above")
}

cat("\n--- durability among rushers who HELD a rotation job ---\n")
cat("job seasons:", nrow(hist_pa), "player-seasons |",
    dplyr::n_distinct(hist_pa$player_id), "players\n")

# ------------------------------------------------------------
# 2. BENCHMARK -- how durable is an ESTABLISHED rotation rusher?
# ------------------------------------------------------------

bench_pa <- avail_pa %>%
  filter(job_yrs >= 2) %>%
  summarise(n = dplyr::n(),
            mean_avail   = mean(avail_job),
            median_avail = median(avail_job),
            .groups = "drop")
cat("\n--- how much of a season does an established rotation",
    "rusher play? (2+ job seasons) ---\n")
print(bench_pa)

# ------------------------------------------------------------
# 2b. FIRST-YEAR LINES -- the observed rate when a NEW rotation
#     member holds a job. 2024-25 first job seasons (2023
#     excluded: the window edge left-censors it). rookie line =
#     entry-year holders; backup line = veterans' first job.
#     Empty cell -> pooled unit line, evidence column carried.
# ------------------------------------------------------------

first_job_pa <- hist_pa %>%
  group_by(player_id) %>%
  slice_min(season, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(season >= 2024) %>%
  left_join(entry_years_def %>% filter(!is.na(player_id)),
            by = "player_id") %>%
  mutate(is_rookie = !is.na(entry_year) & entry_year == season)

lines_pool_pa <- first_job_pa %>%
  summarise(line_rookie_pool = mean(avail_s[is_rookie]),
            line_backup_pool = mean(avail_s[!is_rookie]),
            n_ro_pool = sum(is_rookie), n_bk_pool = sum(!is_rookie),
            .groups = "drop")

lines_pa <- first_job_pa %>%
  group_by(band) %>%
  summarise(lr_n = sum(is_rookie),
            lr   = mean(avail_s[is_rookie]),
            lb_n = sum(!is_rookie),
            lb   = mean(avail_s[!is_rookie]),
            .groups = "drop") %>%
  mutate(line_rookie = dplyr::if_else(lr_n > 0, lr,
                                      lines_pool_pa$line_rookie_pool),
         line_backup = dplyr::if_else(lb_n > 0, lb,
                                      lines_pool_pa$line_backup_pool),
         rookie_from = dplyr::if_else(lr_n > 0, "band", "pooled"),
         backup_from = dplyr::if_else(lb_n > 0, "band", "pooled")) %>%
  select(band, line_rookie, line_backup, rookie_from, backup_from,
         n_rookie_cell = lr_n, n_backup_cell = lb_n)

cat("\n--- first-year lines per band (evidence + fill source) ---\n")
print(lines_pa)
if (any(is.na(lines_pa$line_rookie)) ||
    any(is.na(lines_pa$line_backup))) {
  print(lines_pool_pa)
  stop("first-year line NA even after pooling -- report")
}

# ------------------------------------------------------------
# 3. REPLACEMENT LEVEL -- what does a team get when a rotation
#    rusher is OUT? Empirical 2025: qualified stints (g >= G_LG)
#    OUTSIDE their team's top-NB_LG at the band, snap-weighted.
#    Empty cell -> pooled across bands, evidence column carried.
# ------------------------------------------------------------

stint25_pa <- qual_lg %>%
  filter(season == 2025) %>%
  group_by(def_ssn, player_id) %>%
  summarise(g  = dplyr::n_distinct(week),   # DISTINCT weeks --
            qs = sum(true_pass_set_snap_counts_pass_rush),   # snaps sum
            .groups = "drop") %>%
  filter(g >= G_LG) %>%
  mutate(team = stringr::str_remove(def_ssn, "2025$")) %>%
  inner_join(modal_lg %>% filter(season == 2025) %>%
               select(player_id, band), by = "player_id") %>%
  group_by(team, band) %>%
  arrange(desc(qs), .by_group = TRUE) %>%
  mutate(rk = dplyr::row_number()) %>%
  ungroup()

back25_pa <- stint25_pa %>%
  filter(rk > NB_LG) %>%
  left_join(cur_lg %>% filter(season == 2025) %>%
              select(player_id, band, g_pctl),
            by = c("player_id", "band")) %>%
  left_join(prush_c3_pctl %>% filter(season == 2025,
                                     !is.na(player_id)) %>%
              select(player_id, a25 = c3_pctl),
            by = "player_id")

repl_pool_pa <- back25_pa %>%
  summarise(repl_g_pool  = weighted.mean(g_pctl, w = qs, na.rm = TRUE),
            repl_c3_pool = weighted.mean(a25,    w = qs, na.rm = TRUE),
            .groups = "drop")

repl_pa <- back25_pa %>%
  group_by(band) %>%
  summarise(n_stints = dplyr::n(),
            rg  = weighted.mean(g_pctl, w = qs, na.rm = TRUE),
            rc3 = weighted.mean(a25,    w = qs, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(repl_g  = dplyr::if_else(n_stints > 0 & !is.na(rg),
                                  rg, repl_pool_pa$repl_g_pool),
         repl_c3 = dplyr::if_else(n_stints > 0 & !is.na(rc3),
                                  rc3, repl_pool_pa$repl_c3_pool),
         repl_from = dplyr::if_else(n_stints > 0 & !is.na(rg),
                                    "band", "pooled")) %>%
  select(band, repl_g, repl_c3, repl_from, n_stints)

cat("\n--- when the rotation rusher is out, what do you get?",
    "(2025 backup stints, measured) ---\n")
print(repl_pa)
if (any(is.na(repl_pa$repl_g))) {
  print(repl_pool_pa)
  stop("replacement level NA at a band even after pooling -- report")
}

# ------------------------------------------------------------
# 4. LEAGUE MEMBERSHIP (32 teams) -- the canon's rot_lg build,
#    replicated STEP FOR STEP with player ids and names kept,
#    then WALLED against rot_lg at 1e-8. Canon values and
#    weights untouched; availability and pricing attach below.
# ------------------------------------------------------------

ros_names_pa <- nflreadr::load_rosters(2026) %>%
  transmute(gsis_id, roster_name = full_name)
stopifnot(anyDuplicated(ros_names_pa$gsis_id) == 0)

members_real_pa <- ros_lg %>%
  left_join(use_lg,  by = "player_id") %>%
  left_join(band_lg, by = "player_id") %>%
  filter(!is.na(band)) %>%
  mutate(uo = pmax(dplyr::coalesce(qs_2025, 0),
                   dplyr::coalesce(qs_2024, 0))) %>%
  group_by(team, band) %>%
  arrange(desc(uo), .by_group = TRUE) %>%
  mutate(rk = dplyr::row_number()) %>%
  filter(rk <= NB_LG) %>%
  ungroup() %>%
  left_join(slot_lg,     by = c("team", "band", "rk")) %>%
  left_join(slot_med_lg, by = c("band", "rk")) %>%
  left_join(prior_lg,    by = "band") %>%
  mutate(w25 = pmin(dplyr::coalesce(qg_2025, 0L) / 10, 1),
         gf_bl = blend2(g_pctl_2025, g_pctl_2024, w25),
         prior_g = is.na(gf_bl),
         gf = dplyr::coalesce(gf_bl, prg),
         uw = dplyr::if_else(uo > 0, uo,
                             dplyr::coalesce(sn_slot, sn_med,
                                             fb_lg))) %>%
  left_join(ros_names_pa, by = "gsis_id") %>%
  mutate(status = dplyr::if_else(entry_year == 2026, "rookie",
                                 "veteran"))

# WALL: the id-carrying rebuild IS rot_lg (set compare on the
# canon contract columns, both sides sorted, full precision)
a_pa <- members_real_pa %>%
  select(team, band, gf, uw) %>% arrange(team, band, gf, uw)
b_pa <- rot_lg %>% arrange(team, band, gf, uw)
if (nrow(a_pa) != nrow(b_pa) ||
    !identical(a_pa$team, b_pa$team) ||
    !identical(a_pa$band, b_pa$band) ||
    !isTRUE(all.equal(as.data.frame(a_pa[, c("gf", "uw")]),
                      as.data.frame(b_pa[, c("gf", "uw")]),
                      tolerance = 1e-8))) {
  cat("\nrows:", nrow(a_pa), "vs rot_lg:", nrow(b_pa), "\n")
  print(anti_join(a_pa %>% mutate(across(where(is.numeric),
                                         round, 6)),
                  b_pa %>% mutate(across(where(is.numeric),
                                         round, 6)),
                  by = c("team", "band", "gf", "uw")))
  stop("league membership rebuild != rot_lg -- evidence above")
}
cat("\n--- league membership rebuild == rot_lg (",
    nrow(a_pa), "real members) ---\n", sep = "")

# phantoms: canon slots, unnamed jobs, band-prior value, priced
# at the backup line (adaptation 6)
members_pa <- bind_rows(
  members_real_pa %>%
    select(team, roster_name, band, status, player_id,
           entry_year, gf, uw, prior_g),
  ph_lg %>%
    mutate(roster_name = paste0("PHANTOM ", band, " slot"),
           status = "phantom", player_id = NA,
           entry_year = NA_integer_, prior_g = TRUE) %>%
    select(team, roster_name, band, status, player_id,
           entry_year, gf, uw, prior_g))

# availability attaches (layer-only columns below this line)
members_pa <- members_pa %>%
  left_join(avail_pa %>% filter(!is.na(player_id)),
            by = "player_id") %>%
  mutate(avail_src = dplyr::case_when(
    !is.na(avail_job)                    ~ "starter_history",
    status == "phantom"                  ~ "backup_line",
    dplyr::coalesce(entry_year, 0L) == 2026 ~ "rookie_line",
    TRUE                                 ~ "backup_line")) %>%
  left_join(lines_pa %>% select(band, line_rookie, line_backup),
            by = "band") %>%
  mutate(avail = dplyr::case_when(
    avail_src == "starter_history" ~ avail_job,
    avail_src == "rookie_line"     ~ line_rookie,
    TRUE                           ~ line_backup)) %>%
  left_join(repl_pa %>% select(band, repl_g, repl_c3),
            by = "band")

if (any(is.na(members_pa$avail))) {
  print(members_pa %>% filter(is.na(avail)) %>%
          select(team, roster_name, band, status, avail_src,
                 job_yrs))
  stop("members with NA avail after the line ladder -- report")
}

# adjusted value chain: 2025/2024 c3 blend at the canon weight,
# entry-prior fill
members_pa <- members_pa %>%
  left_join(prush_c3_pctl %>% filter(season == 2025,
                                     !is.na(player_id)) %>%
              select(player_id, a25 = c3_pctl, qg25a = qual_g),
            by = "player_id") %>%
  left_join(prush_c3_pctl %>% filter(season == 2024,
                                     !is.na(player_id)) %>%
              select(player_id, a24 = c3_pctl),
            by = "player_id") %>%
  left_join(c3_entry_prior_pa %>% select(band, pr_c3),
            by = "band") %>%
  mutate(w25a = pmin(dplyr::coalesce(qg25a, 0L) / 10, 1),
         c3_bl = blend2(a25, a24, w25a),
         prior_c3 = is.na(c3_bl),
         c3_f = dplyr::coalesce(c3_bl, pr_c3),
         gf_p = avail * gf   + (1 - avail) * repl_g,
         c3_p = avail * c3_f + (1 - avail) * repl_c3) %>%
  select(-a25, -a24, -qg25a, -w25a, -c3_bl, -pr_c3)

stopifnot(!any(is.na(members_pa$gf)), !any(is.na(members_pa$gf_p)))
if (any(is.na(members_pa$c3_f))) {
  cat("-- c3 value NA receipt (canon NA law; interp carries it):\n")
  print(members_pa %>% filter(is.na(c3_f)) %>%
          select(team, roster_name, band, status) %>%
          as.data.frame())
}

cat("members priced:", nrow(members_pa), "(",
    sum(members_pa$status == "phantom"), "phantom slots) |",
    "avail sources:",
    paste(names(table(members_pa$avail_src)),
          as.integer(table(members_pa$avail_src)),
          sep = "=", collapse = ", "), "\n")

# ------------------------------------------------------------
# 4b. LEAGUE WALL -- healthy re-aggregation must equal
#     team26_lg EXACTLY (same members, same weights, same
#     expression as the canon line) BEFORE pricing enters
# ------------------------------------------------------------

lg_chk_pa <- members_pa %>%
  group_by(team) %>%
  summarise(g26 = weighted.mean(gf, w = uw), .groups = "drop") %>%
  inner_join(team26_lg, by = "team", suffix = c("", "_canon")) %>%
  mutate(d = g26 - g26_canon)
stopifnot(nrow(lg_chk_pa) == 32L)
wall_pa("league healthy rebuild == team26_lg", lg_chk_pa, "d")

# ------------------------------------------------------------
# 5. NE SLATE -- the 14 opponents, injury-priced. Player ids
#    recovered through the roster route (canon handoff dropped
#    them); membership / values / weights byte-identical to
#    canon's rot_2026 -- provenance wall below. Healthy slate
#    walled vs cmp_prush_slate BEFORE pricing enters.
# ------------------------------------------------------------

ids_pa <- nflreadr::load_rosters(2026) %>%
  transmute(team = dplyr::coalesce(unname(pff_team_lookup[team]),
                                   team),
            roster_name = full_name, gsis_id) %>%
  left_join(xw_lg %>% filter(!is.na(gsis_id)), by = "gsis_id") %>%
  select(team, roster_name, player_id)
stopifnot(anyDuplicated(ids_pa[, c("team", "roster_name")]) == 0)

ne_members_pa <- rot_2026 %>%
  left_join(ids_pa %>% filter(!is.na(player_id)),
            by = c("team_name" = "team", "roster_name"))

# id-recovery wall: every non-phantom member lands exactly one
# roster id; phantoms stay NA by design (the pass-rush law)
unmatched_pa <- ne_members_pa %>%
  filter(status != "phantom", is.na(player_id))
if (nrow(unmatched_pa) > 0) {
  cat("rotation members with no roster id (team / name / band):\n")
  print(unmatched_pa %>%
          select(team_name, roster_name, band, status) %>%
          as.data.frame())
  stop("id recovery incomplete -- report, do not proceed")
}

ne_members_pa <- ne_members_pa %>%
  left_join(avail_pa %>% filter(!is.na(player_id)) %>%
              select(player_id, job_yrs, avail_job),
            by = "player_id") %>%
  mutate(avail_src = dplyr::case_when(
    !is.na(avail_job)       ~ "starter_history",
    status == "phantom"     ~ "backup_line",
    status == "rookie"      ~ "rookie_line",
    TRUE                    ~ "backup_line")) %>%
  left_join(lines_pa %>% select(band, line_rookie, line_backup),
            by = "band") %>%
  mutate(avail = dplyr::case_when(
    avail_src == "starter_history" ~ avail_job,
    avail_src == "rookie_line"     ~ line_rookie,
    TRUE                           ~ line_backup)) %>%
  left_join(repl_pa %>% select(band, repl_g, repl_c3),
            by = "band") %>%
  left_join(prush_c3_pctl %>% filter(season == 2025,
                                     !is.na(player_id)) %>%
              select(player_id, a25 = c3_pctl, qg25a = qual_g),
            by = "player_id") %>%
  left_join(prush_c3_pctl %>% filter(season == 2024,
                                     !is.na(player_id)) %>%
              select(player_id, a24 = c3_pctl),
            by = "player_id") %>%
  left_join(c3_entry_prior_pa %>% select(band, pr_c3),
            by = "band") %>%
  mutate(w25a = pmin(dplyr::coalesce(qg25a, 0L) / 10, 1),
         c3_bl = blend2(a25, a24, w25a),
         c3_f = dplyr::coalesce(c3_bl, pr_c3),
         grade_p = avail * grade_f + (1 - avail) * repl_g,
         c3_p    = avail * c3_f    + (1 - avail) * repl_c3) %>%
  select(-a25, -a24, -qg25a, -w25a, -c3_bl, -pr_c3)

if (any(is.na(ne_members_pa$avail))) {
  print(ne_members_pa %>% filter(is.na(avail)) %>%
          select(team_name, roster_name, band, status, avail_src))
  stop("NE-slate members with NA avail -- report")
}

# provenance wall: membership / values / weights identical to
# canon's rot_2026 (the layer ADDED columns, changed none)
prov_a_pa <- ne_members_pa %>%
  select(team_name, roster_name, band, status, prior_used,
         usage_w, grade_f) %>%
  arrange(team_name, band, roster_name)
prov_b_pa <- rot_2026 %>%
  select(team_name, roster_name, band, status, prior_used,
         usage_w, grade_f) %>%
  arrange(team_name, band, roster_name)
if (nrow(prov_a_pa) != nrow(prov_b_pa) ||
    !identical(as.data.frame(prov_a_pa[, c("team_name",
                                           "roster_name", "band",
                                           "status", "prior_used")]),
               as.data.frame(prov_b_pa[, c("team_name",
                                           "roster_name", "band",
                                           "status", "prior_used")])) ||
    !isTRUE(all.equal(prov_a_pa$usage_w, prov_b_pa$usage_w,
                      tolerance = 1e-8)) ||
    !isTRUE(all.equal(prov_a_pa$grade_f, prov_b_pa$grade_f,
                      tolerance = 1e-8))) {
  stop("NE membership provenance breach vs rot_2026 -- report")
}
cat("\n--- NE-slate membership == rot_2026 (",
    nrow(prov_a_pa), " members) ---\n", sep = "")

# team x band values, then the schedule-weighted slate (canon
# law: plain mean over the 17 known schedule rows)
tb26_pa <- ne_members_pa %>%
  group_by(team_name, band) %>%
  summarise(interp_share = sum(usage_w[prior_used]) / sum(usage_w),
            grade_26  = weighted.mean(grade_f, w = usage_w),
            grade_26p = weighted.mean(grade_p, w = usage_w),
            c3_26     = weighted.mean(c3_f, w = usage_w,
                                      na.rm = TRUE),
            c3_26p    = weighted.mean(c3_p, w = usage_w,
                                      na.rm = TRUE),
            .groups = "drop") %>%
  mutate(across(c(c3_26, c3_26p), denan_pa))

slate_pa <- tibble(team_name = sched_2026) %>%
  left_join(tb26_pa, by = "team_name",
            relationship = "many-to-many") %>%
  group_by(band) %>%
  summarise(grade_26  = mean(grade_26),
            grade_26p = mean(grade_26p),
            c3_26     = mean(c3_26, na.rm = TRUE),
            c3_26p    = mean(c3_26p, na.rm = TRUE),
            interp_share = mean(interp_share),
            .groups = "drop")

# THE NE WALL: healthy slate == cmp_prush_slate grade_26, 1e-8
slate_chk_pa <- slate_pa %>%
  inner_join(cmp_prush_slate %>%
               select(band, g_canon = grade_26), by = "band") %>%
  mutate(d = grade_26 - g_canon)
stopifnot(nrow(slate_chk_pa) == 2L)
wall_pa("NE healthy slate == cmp_prush_slate grade_26",
        slate_chk_pa, "d")

cat("\n== NE 2026 OPPONENT PASS-RUSH SLATE, PRICED ==\n")
cat("-- what does NE's protection face, before and after",
    "pricing injuries? (faced 2025 is OBSERVED canon --\n")
cat("   untouched by pricing; fill share = canon's honesty",
    "column) --\n")
cmp_priced_pa <- cmp_prush_slate %>%
  select(band, grade_25, fill_share_25) %>%
  inner_join(slate_pa, by = "band") %>%
  mutate(d_healthy = grade_26  - grade_25,
         d_priced  = grade_26p - grade_25,
         d_priced_adj = c3_26p - c3_26)
print(cmp_priced_pa %>%
        transmute(band,
                  "faced '25"      = grade_25,
                  "fill share"     = fill_share_25,
                  "healthy '26"    = grade_26,
                  "priced '26"     = grade_26p,
                  "priced - faced" = d_priced,
                  "interp share"   = interp_share) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
        as.data.frame())

# ------------------------------------------------------------
# 6. LEAGUE SWEEP -- every focal's 2026 slate vs what they
#    faced in 2025, healthy and priced. Healthy side walled vs
#    league_prush at 1e-8 BEFORE pricing enters. Adjusted faced
#    on the SAME game set as raw faced (canon faced fill law:
#    no season c3 -> band c3 prior), NE cross-wall direct vs
#    league grain.
# ------------------------------------------------------------

team26p_pa <- members_pa %>%
  group_by(team) %>%
  summarise(g26h  = weighted.mean(gf,   w = uw),
            g26p  = weighted.mean(gf_p, w = uw),
            g26a  = weighted.mean(c3_f, w = uw, na.rm = TRUE),
            g26ap = weighted.mean(c3_p, w = uw, na.rm = TRUE),
            prior_share = sum(uw[prior_g]) / sum(uw),
            .groups = "drop") %>%
  mutate(across(c(g26a, g26ap), denan_pa))

sweep_pa <- opp26_lg %>%
  left_join(team26p_pa, by = c("opp" = "team"),
            relationship = "many-to-many") %>%
  group_by(focal) %>%
  summarise(v26   = mean(g26h, na.rm = TRUE),
            v26p  = mean(g26p, na.rm = TRUE),
            v26a  = mean(g26a, na.rm = TRUE),
            v26ap = mean(g26ap, na.rm = TRUE),
            prior_share = mean(prior_share, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(faced_lg %>% select(focal, g25, fill_share),
            by = "focal") %>%
  mutate(d_h = v26 - g25, d_p = v26p - g25)

# WALLS: healthy side reproduces league_prush exactly
sw_chk_pa <- league_prush %>%
  select(focal, g26_c = g26, g25_c = g25) %>%
  inner_join(sweep_pa, by = "focal") %>%
  mutate(d26 = v26 - g26_c, d25 = g25 - g25_c)
stopifnot(nrow(sw_chk_pa) == 32L)
wall_pa("sweep healthy slate == league_prush g26", sw_chk_pa, "d26")
wall_pa("sweep faced == league_prush g25",        sw_chk_pa, "d25")

# adjusted faced per focal (same game set, canon fill law)
faced_a_pa <- faced_games_lg %>%
  left_join(prush_c3_pctl %>% filter(season == 2025,
                                     !is.na(player_id)) %>%
              select(player_id, a25 = c3_pctl),
            by = "player_id") %>%
  left_join(c3_entry_prior_pa %>% select(band, pr_c3),
            by = "band") %>%
  mutate(a_gp = dplyr::coalesce(a25, pr_c3)) %>%
  group_by(focal) %>%
  summarise(a25_n   = sum(sn),
            a25_f_n = sum(sn[is.na(a25)]),
            a25     = weighted.mean(a_gp, w = sn, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(fill_share_a = a25_f_n / a25_n) %>%
  select(focal, a25, fill_share_a)

# NE cross-wall (same-mask law): league grain vs direct rebuild
ne_a_pa <- faced_games_lg %>%
  filter(focal == "NE") %>%
  left_join(prush_c3_pctl %>% filter(season == 2025,
                                     !is.na(player_id)) %>%
              select(player_id, a25 = c3_pctl),
            by = "player_id") %>%
  left_join(c3_entry_prior_pa %>% select(band, pr_c3),
            by = "band") %>%
  mutate(a_gp = dplyr::coalesce(a25, pr_c3)) %>%
  summarise(a25 = weighted.mean(a_gp, w = sn, na.rm = TRUE))
fa_chk_pa <- faced_a_pa %>% filter(focal == "NE") %>%
  mutate(d = a25 - ne_a_pa$a25)
wall_pa("NE adjusted faced: league grain == direct", fa_chk_pa, "d")

sweep_pa <- sweep_pa %>%
  left_join(faced_a_pa, by = "focal") %>%
  mutate(d_ha = v26a - a25, d_pa = v26ap - a25)

cat("\n== LEAGUE SWEEP -- pass rush, injury-priced ==\n")
cat("-- whose 2026 slate of opposing rushers got harder, whose\n")
cat("   got softer, once injuries are priced? (faced 2025 is\n")
cat("   observed canon, untouched; priced = avail-weighted with\n")
cat("   the measured backup level) --\n")
ss_pa <- sweep_pa %>%
  mutate(rank_priced = rank(-d_p)) %>%
  arrange(desc(d_p))
cat("hardest priced jumps (raw grade lens):\n")
print(ss_pa %>% select(focal, g25, v26, v26p, d_h, d_p,
                       fill_share, prior_share) %>%
        mutate(ne = if_else(focal == "NE", "<-- NE", "")) %>%
        head(8) %>% mutate(across(where(is.numeric),
                                  ~ round(.x, 3))) %>%
        as.data.frame())
cat("easiest priced jumps:\n")
print(ss_pa %>% select(focal, g25, v26, v26p, d_h, d_p,
                       fill_share, prior_share) %>%
        mutate(ne = if_else(focal == "NE", "<-- NE", "")) %>%
        tail(8) %>% mutate(across(where(is.numeric),
                                  ~ round(.x, 3))) %>%
        as.data.frame())
cat(sprintf("league mean d: healthy %+.3f -> priced %+.3f | NE: healthy %+.3f -> priced %+.3f (rank %d of 32)\n",
            mean(sweep_pa$d_h), mean(sweep_pa$d_p),
            sweep_pa$d_h[sweep_pa$focal == "NE"],
            sweep_pa$d_p[sweep_pa$focal == "NE"],
            as.integer(ss_pa$rank_priced[ss_pa$focal == "NE"])))
cat("adjusted lens (same-slate ranks) -- NE: healthy",
    round(sweep_pa$d_ha[sweep_pa$focal == "NE"], 3),
    "-> priced", round(sweep_pa$d_pa[sweep_pa$focal == "NE"], 3),
    "\n")

# ------------------------------------------------------------
# 7. BOARDS -- who is fragile, who is a line-priced member,
#    where the expected backup games pile up
# ------------------------------------------------------------

b1_pa <- members_pa %>%
  filter(status != "phantom") %>%
  mutate(exp_miss = (1 - avail) * 17) %>%
  arrange(avail) %>%
  select(team, roster_name, band, avail, exp_miss, gf, gf_p,
         avail_src, job_yrs)

cat("\n-- board 1: the fragile rotation rushers (who misses the\n")
cat("   most, and what his absence costs his team) --\n")
print(b1_pa %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
        head(12), n = 12)