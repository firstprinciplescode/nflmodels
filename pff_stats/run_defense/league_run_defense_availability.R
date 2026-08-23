# ============================================================
# RUN-DEFENSE AVAILABILITY LAYER -- opponent run defenders,
# 2026 slates priced by availability (bands DI / ED / LB / S,
# grade anchor + stop rate riding with the canon caveat,
# adjusted grade via c3). The seventh unit -- the last one.
# Built 2026-08-22 on Andy's word: "THIS IS THE RUN DEFENS
# SHIT. THIS ILL BE A LOT. GO." Canon (bands / gates / faced
# fill law) already stamped 2026-08-14..18 -- this file
# consumes it, never re-derives it.
#
# WHAT THIS ASKS: if an opponent's run defenders miss time in
# 2026, how much run-defense quality does NE's ground game
# actually dodge? Healthy slate vs injury-priced slate, raw
# (grade + stop) and adjusted (grade), plus the league sweep
# and the NE visual.
#
# DIRECTION (offense side): higher pctl = better run defender
# = HARDER for NE's run game. A negative real jump means the
# slate got softer once injuries are priced in.
#
# STAMPS CARRIED (unchanged laws): PRICING LAW (avail judged
# only on job-holding seasons; zero job seasons -> observed
# first-year rate, 2024-25 holders, 2023 left-censored, empty
# cells -> pooled with evidence; repl = empirical 2025 backup
# level) | THREE CURRENCIES + firewall (FACED-2025 untouched)
# | FULL-PRECISION LAW (walls 1e-8, unrounded) | PRINT LAW |
# CROSSED-STREAMS LAW | SEQUENTIAL-MASKING LAW | NA-KEY JOIN
# INSURANCE | unname() load-bearing on named lookups (N_RD_LG
# is a NAMED vector -- unname() every time it is indexed) |
# WALLS print evidence BEFORE stopping | ZERO ATHENA |
# SCORED-WEIGHT LAW (2026-08-22 receiving receipt) |
# SLATE-LAW NOTE: this unit's canon slate = plain mean of
# team-band values over the 17 schedule rows (pass-rush /
# run-defense law) -- NOT the receiving unit's pooled
# member-weighted slate. The re-pool walls below mirror the
# canon law of THIS unit.
#
# UNIT ADAPTATIONS (this unit's shape -- Andy can veto):
#   1. ONE availability per player (no splits). played = canon
#      gate snap_counts_run >= X_RD_LG (3) in a REG week
#      (week <= 18). Denominator 17.
#   2. job = a QUALIFIED season (q_games >= G_RD_LG = 6, the
#      canon floor), 2023-2025 window.
#   3. Snap-weighted everywhere (usage_w).
#   4. FACED 2025 follows canon's FACED FILL LAW (band entry
#      priors; fill_share = the honesty number). Adjusted
#      faced fills at the c3 band prior on the SAME game set.
#   5. Raw fills = canon's band priors (prior_rd_lg).
#      Adjusted fills = in-file c3 entry prior per band (the
#      canon prior construction applied to rd_c3_pctl).
#   6. PHANTOMS ARE CANON: phantom slots carry band-prior
#      values and price at the backup line.
#   7. League healthy rebuild walled vs team_band_rd_lg /
#      team26_rd_lg and the sweep's healthy side vs
#      league_rundef at 1e-8 BEFORE pricing enters.
#   8. Rookies / adds / drops enter only via canon's
#      rot_2026_rd_lg (opp_rundef_2026_deltas is canon's
#      ruling layer -- this file adds nobody).
#
# SOURCE ORDER: pff_run_defense_AWS (run_defense_qbgrp in
#   session, with the prush pipeline objects it needs) ->
#   league_opp_run_defense_schedule.R (the whole canon: NE
#   build + league engine) ->
#   league_run_defense_evaluating_currency_three.R
#   (rd_c3_pctl) -> THIS FILE.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(gt)
})

# ------------------------------------------------------------
# 0. GATES -- frames, column receipts, unit identity, knobs.
# ------------------------------------------------------------

needed_ra <- c("qual_rd_lg", "cur_rd_lg", "modal_band_rd_lg",
               "prior_rd_lg", "rot_2026_rd_lg", "team_band_rd_lg",
               "team26_rd_lg", "faced_games_rd_lg", "faced_rd_lg",
               "slate_rd_lg", "league_rundef", "ent_rd_lg",
               "xw_rd_lg", "rot_2026_rundef", "cmp_rundef_slate",
               "rd_c3_pctl", "sched_2026", "opp_2026_teams",
               "opp26_rd", "X_RD_LG", "G_RD_LG", "N_RD_LG",
               "BANDS_RD_LG", "pff32_rd")
missing_ra <- needed_ra[!vapply(needed_ra, exists, logical(1))]
if (length(missing_ra)) {
  cat("missing session objects:\n"); print(missing_ra)
  stop("see SOURCE ORDER in header")
}

req_cols_ra <- list(
  qual_rd_lg  = c("player_id", "season", "week", "def_ssn",
                  "qbgrp_ssn", "position", "snap_counts_run"),
  cur_rd_lg   = c("player_id", "band", "season", "q_games",
                  "q_snaps", "grade_pctl", "stop_pctl"),
  modal_band_rd_lg = c("player_id", "season", "band"),
  prior_rd_lg = c("band", "pr_grade", "pr_stop"),
  rot_2026_rd_lg = c("team", "roster_name", "band", "status",
                     "prior_used", "usage_w", "grade_f", "stop_f"),
  team_band_rd_lg = c("team", "band", "n_members", "interp_share",
                      "grade_26", "stop_26"),
  team26_rd_lg = c("team", "interp_share", "grade_26", "stop_26"),
  faced_games_rd_lg = c("player_id", "week", "focal", "team",
                        "band", "sn", "gp", "sp", "fill"),
  faced_rd_lg = c("focal", "grade_25", "stop_25", "fill_share_25"),
  slate_rd_lg = c("focal", "grade_26", "stop_26", "interp_share"),
  league_rundef = c("focal", "grade_25", "stop_25", "grade_26",
                    "stop_26", "d_grade", "d_stop"),
  ent_rd_lg   = c("player_id", "entry_year"),
  xw_rd_lg    = c("player_id", "gsis_id"),
  rot_2026_rundef = c("team_name", "roster_name", "band", "status",
                      "prior_used", "usage_w", "grade_f", "stop_f"),
  cmp_rundef_slate = c("band", "grade_25", "stop_25",
                       "fill_share_25", "grade_26", "stop_26",
                       "interp_share", "d_grade", "d_stop"),
  rd_c3_pctl  = c("player_id", "season", "band", "qual_g",
                  "c3_pctl"),
  opp26_rd    = c("focal", "opp"))
for (nm_ra in names(req_cols_ra)) {
  fr_ra <- get(nm_ra)
  miss_ra <- setdiff(req_cols_ra[[nm_ra]], names(fr_ra))
  if (length(miss_ra)) {
    cat("frame:", nm_ra, "-- missing columns:\n"); print(miss_ra)
    cat("actual columns:\n"); print(names(fr_ra))
    stop("column receipt printed above -- crossed-streams guard")
  }
}

# unit identity: run-defense bands only on both membership frames
stopifnot(all(rot_2026_rd_lg$band %in% BANDS_RD_LG),
          all(rot_2026_rundef$band %in% BANDS_RD_LG),
          all(rot_2026_rundef$team_name %in% opp_2026_teams),
          all(BANDS_RD_LG %in% c("DI", "ED", "LB", "S")))

AVAIL_SEASONS_RA <- 2023:2025
AVAIL_DENOM_RA   <- 17L
cat("\n== RUN-DEFENSE AVAILABILITY LAYER ==\n")
cat("gates:", X_RD_LG, "run snaps = played |", G_RD_LG,
    "games = qualified | rotation N:", paste(names(N_RD_LG),
                                             N_RD_LG, sep = "=", collapse = " "), "| denom",
    AVAIL_DENOM_RA, "\n")

ssn_cov_ra <- sort(unique(qual_rd_lg$season))
cat("weekly base seasons:", paste(ssn_cov_ra, collapse = " "), "\n")
if (!all(AVAIL_SEASONS_RA %in% ssn_cov_ra)) {
  cat("window seasons missing:", setdiff(AVAIL_SEASONS_RA,
                                         ssn_cov_ra), "\n")
  stop("availability window not covered by the weekly base")
}

denan_ra <- function(x) dplyr::if_else(is.nan(x), NA_real_, x)
wall_ra <- function(tag, chk_df, diff_col, tol = 1e-8) {
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
gt_marker_ra <- tibble::tibble(
  status = paste0("RUN-DEFENSE availability layer sourced ",
                  format(Sys.time(), "%H:%M:%S"),
                  " -- the table builds in section 8."),
  if_you_see_this = "the run stopped early -- read the console tail") %>%
  gt()
print(gt_marker_ra)

# ------------------------------------------------------------
# 0b. c3 ENTRY PRIOR per band -- canon's prior construction
#     (entry-season median, 2017+) applied to rd_c3_pctl
# ------------------------------------------------------------

c3_entry_prior_ra <- rd_c3_pctl %>%
  inner_join(ent_rd_lg %>% filter(!is.na(player_id)),
             by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2017) %>%
  group_by(band) %>%
  summarise(pr_c3 = median(c3_pctl, na.rm = TRUE),
            n_entry = dplyr::n(), .groups = "drop")

cat("\n--- c3 entry prior by band (entry-season median, 2017+) ---\n")
print(c3_entry_prior_ra)
stopifnot(!any(is.na(c3_entry_prior_ra$pr_c3)))

# ------------------------------------------------------------
# 1. HISTORY -- played weeks and job seasons, 2023-2025.
#    played = canon gate (snap_counts_run >= X_RD_LG) in a REG
#    week. job = qualified season (q_games >= G_RD_LG).
# ------------------------------------------------------------

# played = DISTINCT REG weeks over the gate. qual_rd_lg is one
# row per player-week-POSITION, so a defender qualifying at two
# positions in the same week must still count ONE played week
# (the stamped receiving-layer law: distinct weeks, then count).
games_ra <- qual_rd_lg %>%
  filter(season %in% AVAIL_SEASONS_RA, week <= 18) %>%
  distinct(player_id, season, week) %>%
  count(player_id, season, name = "played_g")

hist_ra <- cur_rd_lg %>%
  filter(season %in% AVAIL_SEASONS_RA) %>%
  left_join(games_ra %>% filter(!is.na(player_id)),
            by = c("player_id", "season")) %>%
  mutate(played_g = dplyr::coalesce(played_g, 0L),
         # capped at 1: a mid-season trade can yield 18 DISTINCT REG
         # weeks (both byes already spent) -- real games, a FULLY
         # AVAILABLE season. Availability for a 17-game slate cannot
         # exceed 1. The 28075 case. Same pmin the pass-block layer
         # has carried since its stamp.
         avail_s  = pmin(played_g / AVAIL_DENOM_RA, 1))

avail_ra <- hist_ra %>%
  group_by(player_id) %>%
  summarise(yrs       = dplyr::n(),
            job_yrs   = dplyr::n(),
            avail_job = mean(avail_s),
            .groups = "drop")

# bounds wall at birth: availability lives in [0,1]. If a
# double-counted week ever sneaks back in, the offender prints
# HERE, at the unit that owns the number -- not downstream.
if (any(avail_ra$avail_job > 1 + 1e-8) ||
    any(avail_ra$avail_job < 0)) {
  cat("availability outside [0,1] -- offenders:\n")
  print(avail_ra %>% filter(avail_job > 1 | avail_job < 0),
        n = Inf)
  stop("availability bounds breach -- evidence above")
}

cat("\n--- durability among run defenders who HELD a rotation",
    "job ---\n")
cat("job seasons:", nrow(hist_ra), "player-seasons |",
    dplyr::n_distinct(hist_ra$player_id), "players\n")

# ------------------------------------------------------------
# 2. BENCHMARK -- how durable is an ESTABLISHED rotation
#    run defender?
# ------------------------------------------------------------

bench_ra <- avail_ra %>%
  filter(job_yrs >= 2) %>%
  summarise(n = dplyr::n(),
            mean_avail   = mean(avail_job),
            median_avail = median(avail_job),
            .groups = "drop")
cat("\n--- how much of a season does an established rotation",
    "run defender play? (2+ job seasons) ---\n")
print(bench_ra)

# ------------------------------------------------------------
# 2b. FIRST-YEAR LINES -- observed rate when a NEW rotation
#     member holds a job. 2024-25 first job seasons (2023
#     left-censored). rookie line = entry-year holders; backup
#     line = veterans' first job. Empty cell -> pooled unit
#     line, evidence column carried.
# ------------------------------------------------------------

first_job_ra <- hist_ra %>%
  group_by(player_id) %>%
  slice_min(season, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(season >= 2024) %>%
  left_join(ent_rd_lg %>% filter(!is.na(player_id)),
            by = "player_id") %>%
  mutate(is_rookie = !is.na(entry_year) & entry_year == season)

lines_pool_ra <- first_job_ra %>%
  summarise(line_rookie_pool = mean(avail_s[is_rookie]),
            line_backup_pool = mean(avail_s[!is_rookie]),
            n_ro_pool = sum(is_rookie), n_bk_pool = sum(!is_rookie),
            .groups = "drop")

lines_ra <- first_job_ra %>%
  group_by(band) %>%
  summarise(lr_n = sum(is_rookie),
            lr   = mean(avail_s[is_rookie]),
            lb_n = sum(!is_rookie),
            lb   = mean(avail_s[!is_rookie]),
            .groups = "drop") %>%
  mutate(line_rookie = dplyr::if_else(lr_n > 0, lr,
                                      lines_pool_ra$line_rookie_pool),
         line_backup = dplyr::if_else(lb_n > 0, lb,
                                      lines_pool_ra$line_backup_pool),
         rookie_from = dplyr::if_else(lr_n > 0, "band", "pooled"),
         backup_from = dplyr::if_else(lb_n > 0, "band", "pooled")) %>%
  select(band, line_rookie, line_backup, rookie_from, backup_from,
         n_rookie_cell = lr_n, n_backup_cell = lb_n)

cat("\n--- first-year lines per band (evidence + fill source) ---\n")
print(lines_ra)
if (any(is.na(lines_ra$line_rookie)) ||
    any(is.na(lines_ra$line_backup))) {
  print(lines_pool_ra)
  stop("first-year line NA even after pooling -- report")
}

# ------------------------------------------------------------
# 3. REPLACEMENT LEVEL -- 2025 qualified stints OUTSIDE the
#    rotation (rank > N_RD_LG[band] within team x band, snaps),
#    snap-weighted. Empty cell -> pooled, evidence carried.
# ------------------------------------------------------------

stint25_ra <- qual_rd_lg %>%
  filter(season == 2025) %>%
  group_by(def_ssn, player_id) %>%
  summarise(g  = dplyr::n_distinct(week),   # DISTINCT weeks --
            qs = sum(snap_counts_run), .groups = "drop") %>%   # snaps sum
  filter(g >= G_RD_LG) %>%
  mutate(team = stringr::str_remove(def_ssn, "2025$")) %>%
  inner_join(modal_band_rd_lg %>% filter(season == 2025) %>%
               select(player_id, band), by = "player_id") %>%
  group_by(team, band) %>%
  arrange(desc(qs), .by_group = TRUE) %>%
  mutate(rk       = dplyr::row_number(),
         target_n = unname(N_RD_LG[band])) %>%
  ungroup()

back25_ra <- stint25_ra %>%
  filter(rk > target_n) %>%
  left_join(cur_rd_lg %>% filter(season == 2025) %>%
              select(player_id, band, grade_pctl, stop_pctl),
            by = c("player_id", "band")) %>%
  left_join(rd_c3_pctl %>% filter(season == 2025,
                                  !is.na(player_id)) %>%
              select(player_id, a25 = c3_pctl),
            by = "player_id")

repl_pool_ra <- back25_ra %>%
  summarise(repl_g_pool   = weighted.mean(grade_pctl, w = qs,
                                          na.rm = TRUE),
            repl_s_pool   = weighted.mean(stop_pctl,  w = qs,
                                          na.rm = TRUE),
            repl_c3_pool  = weighted.mean(a25,        w = qs,
                                          na.rm = TRUE),
            .groups = "drop")

repl_ra <- back25_ra %>%
  group_by(band) %>%
  summarise(n_stints = dplyr::n(),
            rg  = weighted.mean(grade_pctl, w = qs, na.rm = TRUE),
            rs  = weighted.mean(stop_pctl,  w = qs, na.rm = TRUE),
            rc3 = weighted.mean(a25,        w = qs, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(repl_grade = dplyr::if_else(n_stints > 0 & !is.na(rg),
                                     rg, repl_pool_ra$repl_g_pool),
         repl_stop  = dplyr::if_else(n_stints > 0 & !is.na(rs),
                                     rs, repl_pool_ra$repl_s_pool),
         repl_c3    = dplyr::if_else(n_stints > 0 & !is.na(rc3),
                                     rc3, repl_pool_ra$repl_c3_pool),
         repl_from  = dplyr::if_else(n_stints > 0 & !is.na(rg),
                                     "band", "pooled")) %>%
  select(band, repl_grade, repl_stop, repl_c3, repl_from, n_stints)

cat("\n--- when the rotation run defender is out, what do you",
    "get? (2025 backup stints, measured) ---\n")
print(repl_ra)
if (any(is.na(repl_ra$repl_grade)) || any(is.na(repl_ra$repl_stop))) {
  print(repl_pool_ra)
  stop("replacement level NA at a band even after pooling -- report")
}

# ------------------------------------------------------------
# 4. MEMBERS -- canon rotation rot_2026_rd_lg (already stamped:
#    roster spots filled to N_RD_LG[band], PHANTOMs carry band
#    priors). This layer re-attaches ids, availability, lines,
#    replacement level, then prices. Canon values are WALL-
#    CHECKED, never re-derived.
# ------------------------------------------------------------

# id recovery: nflreadr 2026 rosters -> gsis bridge -> player_id
ros_ra <- nflreadr::load_rosters(2026) %>%
  filter(!is.na(gsis_id)) %>%
  select(team, roster_name = full_name, gsis_id)

ids_ra <- ros_ra %>%
  inner_join(xw_rd_lg %>% filter(!is.na(gsis_id),
                                 !is.na(player_id)) %>%
               select(gsis_id, player_id),
             by = "gsis_id") %>%
  select(team, roster_name, player_id) %>%
  distinct(team, roster_name, .keep_all = TRUE)

dup_ids_ra <- ids_ra %>% count(team, roster_name) %>% filter(n > 1)
if (nrow(dup_ids_ra)) {
  print(dup_ids_ra)
  stop("duplicate (team, roster_name) in the id bridge -- report")
}

members_real_ra <- rot_2026_rd_lg %>%
  left_join(ids_ra, by = c("team", "roster_name")) %>%
  mutate(is_phantom = grepl("^PHANTOM_", roster_name))

# code-vocabulary insurance: if canon's team codes and the
# roster feed's team codes ever disagree (the ARZ/BLT/LA
# family), the (team, name) join misses. Rescue on UNIQUE
# roster names only, and print the receipt -- never silent.
ids_unq_ra <- ids_ra %>%
  distinct(roster_name, player_id) %>%
  group_by(roster_name) %>%
  filter(dplyr::n() == 1) %>%
  ungroup() %>%
  rename(player_id_unq = player_id)
rescue_ra <- members_real_ra %>%
  filter(!is_phantom, is.na(player_id)) %>%
  left_join(ids_unq_ra, by = "roster_name") %>%
  filter(!is.na(player_id_unq))
if (nrow(rescue_ra)) {
  cat("id rescue (unique-name match, team code mismatch",
      "bypassed) -- receipt:\n")
  print(rescue_ra %>% select(team, roster_name, band,
                             player_id_unq), n = Inf)
  members_real_ra <- members_real_ra %>%
    left_join(ids_unq_ra, by = "roster_name") %>%
    mutate(player_id = dplyr::coalesce(player_id,
                                       player_id_unq)) %>%
    select(-player_id_unq)
}

# every non-phantom member must carry an id
unmatched_ra <- members_real_ra %>%
  filter(!is_phantom, is.na(player_id))
if (nrow(unmatched_ra)) {
  cat("non-phantom rotation members with no id match:\n")
  print(unmatched_ra, n = Inf)
  stop("id recovery hole -- report before trusting prices")
}

# provenance wall: this frame must BE the canon frame
chk_prov_ra <- members_real_ra %>%
  select(team, roster_name, band, usage_w, grade_f, stop_f,
         prior_used) %>%
  arrange(team, band, roster_name)
canon_prov_ra <- rot_2026_rd_lg %>%
  select(team, roster_name, band, usage_w, grade_f, stop_f,
         prior_used) %>%
  arrange(team, band, roster_name)
stopifnot(nrow(chk_prov_ra) == nrow(canon_prov_ra),
          identical(chk_prov_ra$team, canon_prov_ra$team),
          identical(chk_prov_ra$roster_name,
                    canon_prov_ra$roster_name),
          identical(chk_prov_ra$band, canon_prov_ra$band))
wall_ra("members vs canon rot_2026_rd_lg: grade_f",
        tibble(d = chk_prov_ra$grade_f - canon_prov_ra$grade_f), "d")
wall_ra("members vs canon rot_2026_rd_lg: stop_f",
        tibble(d = chk_prov_ra$stop_f - canon_prov_ra$stop_f), "d")
wall_ra("members vs canon rot_2026_rd_lg: usage_w",
        tibble(d = chk_prov_ra$usage_w - canon_prov_ra$usage_w), "d")

# the pricing chain: availability + lines + replacement + c3
members_ra <- members_real_ra %>%
  left_join(avail_ra %>% select(player_id, avail_job),
            by = "player_id") %>%
  left_join(lines_ra, by = "band") %>%
  left_join(repl_ra, by = "band") %>%
  left_join(rd_c3_pctl %>% filter(season == 2025,
                                  !is.na(player_id)) %>%
              select(player_id, c3_25 = c3_pctl),
            by = "player_id") %>%
  left_join(c3_entry_prior_ra %>% select(band, pr_c3),
            by = "band") %>%
  mutate(
    c3_f   = dplyr::coalesce(c3_25, pr_c3),
    avail  = dplyr::case_when(
      is_phantom                ~ line_backup,
      status == "rookie"        ~ line_rookie,
      !is.na(avail_job)         ~ avail_job,
      TRUE                      ~ line_backup),
    avail_src = dplyr::case_when(
      is_phantom                ~ "phantom -> backup line",
      status == "rookie"        ~ paste0("rookie line (",
                                         rookie_from, ")"),
      !is.na(avail_job)         ~ "job history",
      TRUE                      ~ paste0("backup line (",
                                         backup_from, ")")),
    grade_p = avail * grade_f + (1 - avail) * repl_grade,
    stop_p  = avail * stop_f  + (1 - avail) * repl_stop,
    c3_p    = avail * c3_f    + (1 - avail) * repl_c3)

cat("\n--- pricing chain attached:", nrow(members_ra),
    "rotation slots,", sum(members_ra$is_phantom),
    "phantoms ---\n")
if (any(is.na(members_ra$avail)) || any(is.na(members_ra$grade_p)) ||
    any(is.na(members_ra$stop_p)) || any(is.na(members_ra$c3_p))) {
  print(members_ra %>% filter(is.na(avail) | is.na(grade_p) |
                                is.na(stop_p) | is.na(c3_p)), n = Inf)
  stop("NA in the pricing chain -- evidence above")
}

# ------------------------------------------------------------
# 4b. LEAGUE WALLS -- healthy rebuild must equal canon
#     team_band_rd_lg / team26_rd_lg at 1e-8 BEFORE any price
#     enters. grade/stop = usage-weighted means; interp_share =
#     usage-weighted share of prior_used slots.
# ------------------------------------------------------------

tb_chk_ra <- members_ra %>%
  group_by(team, band) %>%
  summarise(n_members   = dplyr::n(),
            interp_share = weighted.mean(as.numeric(prior_used),
                                         w = usage_w),
            grade_26    = weighted.mean(grade_f, w = usage_w),
            stop_26     = weighted.mean(stop_f,  w = usage_w),
            .groups = "drop")

lg_chk_ra <- team_band_rd_lg %>%
  select(team, band, n_members_c = n_members,
         interp_c = interp_share, grade_c = grade_26,
         stop_c = stop_26) %>%
  inner_join(tb_chk_ra, by = c("team", "band")) %>%
  mutate(d_n = n_members - n_members_c,
         d_i = interp_share - interp_c,
         d_g = grade_26 - grade_c,
         d_s = stop_26 - stop_c)
stopifnot(nrow(lg_chk_ra) == nrow(team_band_rd_lg))
wall_ra("league bands vs canon: n_members", lg_chk_ra, "d_n")
wall_ra("league bands vs canon: interp_share", lg_chk_ra, "d_i")
wall_ra("league bands vs canon: grade_26", lg_chk_ra, "d_g")
wall_ra("league bands vs canon: stop_26", lg_chk_ra, "d_s")

# team level: bands pooled, usage-weighted, straight off the
# member frame (canon's "league screen" construction)
t26_chk_ra <- members_ra %>%
  group_by(team) %>%
  summarise(interp_share = weighted.mean(as.numeric(prior_used),
                                         w = usage_w),
            grade_26 = weighted.mean(grade_f, w = usage_w),
            stop_26  = weighted.mean(stop_f,  w = usage_w),
            .groups = "drop") %>%
  inner_join(team26_rd_lg %>%
               select(team, interp_c = interp_share,
                      grade_c = grade_26, stop_c = stop_26),
             by = "team") %>%
  mutate(d_i = interp_share - interp_c,
         d_g = grade_26 - grade_c,
         d_s = stop_26 - stop_c)
stopifnot(nrow(t26_chk_ra) == 32L)
wall_ra("league teams vs canon: interp_share", t26_chk_ra, "d_i")
wall_ra("league teams vs canon: grade_26", t26_chk_ra, "d_g")
wall_ra("league teams vs canon: stop_26", t26_chk_ra, "d_s")
cat("league healthy rebuild == canon. pricing may enter.\n")

# ------------------------------------------------------------
# 5. NEW ENGLAND SLATE -- 17 opponents, canon rot_2026_rundef,
#    plain-mean law (this unit's stamped slate law: the NE
#    slate value = the plain mean of the 17 opponent team-band
#    values -- NOT member-pooled; that is the receiving unit's
#    law). Walled vs canon cmp_rundef_slate at 1e-8.
# ------------------------------------------------------------

ne_members_ra <- rot_2026_rundef %>%
  left_join(ids_ra, by = c("team_name" = "team",
                           "roster_name")) %>%
  mutate(is_phantom = grepl("^PHANTOM_", roster_name)) %>%
  left_join(ids_unq_ra, by = "roster_name") %>%
  mutate(player_id = dplyr::coalesce(player_id, player_id_unq)) %>%
  select(-player_id_unq)

unmatched_ne_ra <- ne_members_ra %>%
  filter(!is_phantom, is.na(player_id))
if (nrow(unmatched_ne_ra)) {
  cat("NE-slate opponents' members with no id match:\n")
  print(unmatched_ne_ra, n = Inf)
  stop("id recovery hole on the NE slate -- report")
}

# provenance wall vs the canon NE frame
chk_ne_ra <- ne_members_ra %>%
  select(team_name, roster_name, band, usage_w, grade_f, stop_f,
         prior_used) %>%
  arrange(team_name, band, roster_name)
canon_ne_ra <- rot_2026_rundef %>%
  select(team_name, roster_name, band, usage_w, grade_f, stop_f,
         prior_used) %>%
  arrange(team_name, band, roster_name)
stopifnot(nrow(chk_ne_ra) == nrow(canon_ne_ra),
          identical(chk_ne_ra$team_name, canon_ne_ra$team_name),
          identical(chk_ne_ra$roster_name, canon_ne_ra$roster_name))
wall_ra("NE members vs canon rot_2026_rundef: grade_f",
        tibble(d = chk_ne_ra$grade_f - canon_ne_ra$grade_f), "d")
wall_ra("NE members vs canon rot_2026_rundef: stop_f",
        tibble(d = chk_ne_ra$stop_f - canon_ne_ra$stop_f), "d")

# price the NE slate members (same chain as the league)
ne_members_ra <- ne_members_ra %>%
  left_join(avail_ra %>% select(player_id, avail_job),
            by = "player_id") %>%
  left_join(lines_ra, by = "band") %>%
  left_join(repl_ra, by = "band") %>%
  left_join(rd_c3_pctl %>% filter(season == 2025,
                                  !is.na(player_id)) %>%
              select(player_id, c3_25 = c3_pctl),
            by = "player_id") %>%
  left_join(c3_entry_prior_ra %>% select(band, pr_c3),
            by = "band") %>%
  mutate(
    c3_f   = dplyr::coalesce(c3_25, pr_c3),
    avail  = dplyr::case_when(
      is_phantom        ~ line_backup,
      status == "rookie" ~ line_rookie,
      !is.na(avail_job)  ~ avail_job,
      TRUE               ~ line_backup),
    grade_p = avail * grade_f + (1 - avail) * repl_grade,
    stop_p  = avail * stop_f  + (1 - avail) * repl_stop,
    c3_p    = avail * c3_f    + (1 - avail) * repl_c3)

tb26_ra <- ne_members_ra %>%
  group_by(team_name, band) %>%
  summarise(interp_share = weighted.mean(as.numeric(prior_used),
                                         w = usage_w),
            grade_26 = weighted.mean(grade_f, w = usage_w),
            stop_26  = weighted.mean(stop_f,  w = usage_w),
            c3_26    = weighted.mean(c3_f,    w = usage_w),
            grade_26p = weighted.mean(grade_p, w = usage_w),
            stop_26p  = weighted.mean(stop_p,  w = usage_w),
            c3_26p    = weighted.mean(c3_p,    w = usage_w),
            .groups = "drop")

# plain-mean slate: one row per schedule opponent, band values
# averaged flat across the 17 rows
slate_ra <- tibble(team_name = sched_2026) %>%
  left_join(tb26_ra, by = "team_name",
            relationship = "many-to-many") %>%
  group_by(band) %>%
  summarise(interp_share = mean(interp_share, na.rm = TRUE),
            grade_26 = mean(grade_26, na.rm = TRUE),
            stop_26  = mean(stop_26,  na.rm = TRUE),
            c3_26    = mean(c3_26,    na.rm = TRUE),
            grade_26p = mean(grade_26p, na.rm = TRUE),
            stop_26p  = mean(stop_26p,  na.rm = TRUE),
            c3_26p    = mean(c3_26p,    na.rm = TRUE),
            .groups = "drop")
stopifnot(nrow(slate_ra) == length(BANDS_RD_LG),
          !any(is.na(slate_ra$grade_26)))

slate_chk_ra <- slate_ra %>%
  inner_join(cmp_rundef_slate %>%
               select(band, interp_c = interp_share,
                      grade_c = grade_26, stop_c = stop_26),
             by = "band") %>%
  mutate(d_i = interp_share - interp_c,
         d_g = grade_26 - grade_c,
         d_s = stop_26 - stop_c)
stopifnot(nrow(slate_chk_ra) == length(BANDS_RD_LG))
wall_ra("NE slate vs canon cmp_rundef_slate: interp_share",
        slate_chk_ra, "d_i")
wall_ra("NE slate vs canon cmp_rundef_slate: grade_26",
        slate_chk_ra, "d_g")
wall_ra("NE slate vs canon cmp_rundef_slate: stop_26",
        slate_chk_ra, "d_s")

cmp_priced_ra <- slate_ra %>%
  transmute(band,
            healthy_grade = round(grade_26, 4),
            priced_grade  = round(grade_26p, 4),
            d_grade = round(grade_26p - grade_26, 4),
            healthy_stop = round(stop_26, 4),
            priced_stop  = round(stop_26p, 4),
            d_stop = round(stop_26p - stop_26, 4),
            healthy_c3 = round(c3_26, 4),
            priced_c3  = round(c3_26p, 4),
            d_c3 = round(c3_26p - c3_26, 4))
cat("\n--- NE 2026 run-defense slate: healthy vs priced ---\n")
print(cmp_priced_ra)

# ------------------------------------------------------------
# 6. LEAGUE SWEEP -- every focal's 2026 opponent run-defense
#    slate, healthy vs priced, against what they FACED in 2025.
#    Healthy + faced sides walled vs canon league_rundef at
#    1e-8 before any priced number is trusted.
# ------------------------------------------------------------

# priced team level (league-wide)
team26p_ra <- members_ra %>%
  group_by(team) %>%
  summarise(grade_26p = weighted.mean(grade_p, w = usage_w),
            stop_26p  = weighted.mean(stop_p,  w = usage_w),
            c3_26p    = weighted.mean(c3_p,    w = usage_w),
            .groups = "drop")

# adjusted faced frame: every gated defender-game carries a c3
# value -- earned 2025 c3 if present, else the band c3 entry
# prior. FACED FILL LAW: nobody drops from the denominator.
prc3_lk_ra <- setNames(c3_entry_prior_ra$pr_c3,
                       c3_entry_prior_ra$band)
faced_games_a_ra <- faced_games_rd_lg %>%
  left_join(rd_c3_pctl %>% filter(season == 2025,
                                  !is.na(player_id)) %>%
              select(player_id, c3_25 = c3_pctl),
            by = "player_id") %>%
  mutate(fill_a = is.na(c3_25),
         a_gp   = dplyr::coalesce(c3_25,
                                  unname(prc3_lk_ra[band])))
if (any(is.na(faced_games_a_ra$a_gp))) {
  print(faced_games_a_ra %>% filter(is.na(a_gp)), n = Inf)
  stop("adjusted faced fill failed -- evidence above")
}

faced_a_ra <- faced_games_a_ra %>%
  group_by(focal) %>%
  summarise(c3_25f = weighted.mean(a_gp, w = sn),
            fill_share_a = sum(sn[fill_a]) / sum(sn),
            .groups = "drop")

# focal sweep: healthy = plain mean of the 32 opponents' team
# values over the canon opp26_rd pairs (mirror canon exactly)
sweep_ra <- opp26_rd %>%
  left_join(team26_rd_lg %>% select(team, grade_26, stop_26),
            by = c("opp" = "team")) %>%
  left_join(team26p_ra, by = c("opp" = "team")) %>%
  group_by(focal) %>%
  summarise(grade_26  = mean(grade_26),
            stop_26   = mean(stop_26),
            grade_26p = mean(grade_26p),
            stop_26p  = mean(stop_26p),
            c3_26p    = mean(c3_26p),
            .groups = "drop") %>%
  left_join(faced_rd_lg %>% select(focal, grade_25, stop_25,
                                   fill_share_25),
            by = "focal") %>%
  left_join(faced_a_ra, by = "focal")
stopifnot(nrow(sweep_ra) == 32L)

# walls vs canon league_rundef (healthy + faced, pre-pricing)
sw_chk_ra <- sweep_ra %>%
  inner_join(league_rundef %>%
               select(focal, g25_c = grade_25, s25_c = stop_25,
                      g26_c = grade_26, s26_c = stop_26),
             by = "focal") %>%
  mutate(d_g25 = grade_25 - g25_c, d_s25 = stop_25 - s25_c,
         d_g26 = grade_26 - g26_c, d_s26 = stop_26 - s26_c)
stopifnot(nrow(sw_chk_ra) == 32L)
wall_ra("sweep vs canon league_rundef: grade_25", sw_chk_ra, "d_g25")
wall_ra("sweep vs canon league_rundef: stop_25", sw_chk_ra, "d_s25")
wall_ra("sweep vs canon league_rundef: grade_26", sw_chk_ra, "d_g26")
wall_ra("sweep vs canon league_rundef: stop_26", sw_chk_ra, "d_s26")

# deltas: what changes when the injury bill comes due
sweep_ra <- sweep_ra %>%
  mutate(d_h  = grade_26  - grade_25,
         d_p  = grade_26p - grade_25,
         d_hs = stop_26  - stop_25,
         d_ps = stop_26p - stop_25,
         d_pa = c3_26p   - c3_25f)

cat("\n--- league sweep: 2026 opponent run defenses vs 2025",
    "faced (raw grade; deltas in percentile points) ---\n")
print(sweep_ra %>%
        transmute(focal,
                  faced25 = round(grade_25, 3),
                  healthy26 = round(grade_26, 3),
                  priced26 = round(grade_26p, 3),
                  d_healthy_pts = round(100 * d_h, 1),
                  d_priced_pts = round(100 * d_p, 1),
                  fill25 = round(fill_share_25, 2)) %>%
        arrange(desc(d_priced_pts)), n = 32)
cat("\n--- same, adjusted (c3 grade, same-slate; points) ---\n")
print(sweep_ra %>%
        transmute(focal,
                  faced25_adj = round(c3_25f, 3),
                  priced26_adj = round(c3_26p, 3),
                  d_priced_adj_pts = round(100 * d_pa, 1),
                  fill_adj = round(fill_share_a, 2)) %>%
        arrange(desc(d_priced_adj_pts)), n = 32)

ne_line_ra <- sweep_ra %>% filter(focal == "NE")
cat("\n--- NE callout ---\n")
cat("NE's 2026 opponents' run defense, healthy slate grade:",
    round(ne_line_ra$grade_26, 3), "| priced:",
    round(ne_line_ra$grade_26p, 3), "| 2025 faced:",
    round(ne_line_ra$grade_25, 3), "\n")
cat("raw delta priced vs faced:", round(100 * ne_line_ra$d_p, 1),
    "pts | adjusted:", round(100 * ne_line_ra$d_pa, 1),
    "pts (negative = softer for the NE run game)\n")

# ------------------------------------------------------------
# 7. BOARDS -- fragility, line-priced slots, exposure
# ------------------------------------------------------------

b1_ra <- members_ra %>%
  filter(!is_phantom) %>%
  arrange(avail) %>%
  select(team, roster_name, band, avail, avail_src, grade_f) %>%
  head(15)
cat("\n--- B1: thinnest availability in the 2026 run-defense",
    "rotation (league-wide) ---\n")
print(b1_ra)

b2_ra <- members_ra %>%
  filter(avail_src != "job history") %>%
  count(team, band, avail_src, name = "slots") %>%
  arrange(desc(slots))
cat("\n--- B2: slots priced off a LINE, not a history ---\n")
print(b2_ra, n = 40)

b4_ra <- members_ra %>%
  mutate(bup_gms = (1 - avail) * 17) %>%
  group_by(team, band) %>%
  summarise(bup_gms = sum(bup_gms),
            repl_grade = dplyr::first(repl_grade),
            .groups = "drop") %>%
  group_by(team) %>%
  slice_max(bup_gms, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(ne_opp = team %in% opp_2026_teams) %>%
  arrange(desc(bup_gms))
cat("\n--- B4: biggest expected backup burden per team",
    "(backup games, run defense) ---\n")
print(b4_ra, n = 32)

# ------------------------------------------------------------
# 8. THE VISUAL -- NE's 14 opponents x 4 bands, faced-2025 vs
#    healthy-2026 vs priced-2026. Re-pool walls FIRST: the
#    cells must re-compose to the canon slate (plain-mean law
#    on the healthy side; snap-weighted on the faced side).
# ------------------------------------------------------------

cells_h_ra <- ne_members_ra %>%
  group_by(team_name, band) %>%
  summarise(sw = sum(usage_w),
            interp = weighted.mean(as.numeric(prior_used),
                                   w = usage_w),
            h_g = weighted.mean(grade_f, w = usage_w),
            p_g = weighted.mean(grade_p, w = usage_w),
            h_s = weighted.mean(stop_f,  w = usage_w),
            p_s = weighted.mean(stop_p,  w = usage_w),
            h_a = weighted.mean(c3_f,    w = usage_w),
            p_a = weighted.mean(c3_p,    w = usage_w),
            .groups = "drop")

cells_f_ra <- faced_games_a_ra %>%
  filter(focal == "NE") %>%
  group_by(team, band) %>%
  summarise(fr = sum(sn),
            fill_n = sum(sn[fill]),
            f_g = weighted.mean(gp, w = sn),
            f_s = weighted.mean(sp, w = sn),
            f_a = weighted.mean(a_gp, w = sn),
            .groups = "drop") %>%
  mutate(fill_sh = fill_n / fr) %>%
  select(-fill_n)

# healthy re-pool: PLAIN MEAN of team-band cells over the 17
# schedule rows must equal the canon slate (this unit's law)
repool_h_ra <- tibble(team_name = sched_2026) %>%
  left_join(cells_h_ra, by = "team_name",
            relationship = "many-to-many") %>%
  group_by(band) %>%
  summarise(r_g = mean(h_g, na.rm = TRUE),
            r_s = mean(h_s, na.rm = TRUE),
            r_a = mean(h_a, na.rm = TRUE), .groups = "drop") %>%
  inner_join(slate_ra %>% select(band, grade_26, stop_26, c3_26),
             by = "band") %>%
  mutate(d_g = r_g - grade_26, d_s = r_s - stop_26,
         d_a = r_a - c3_26)
wall_ra("healthy cells -> NE slate (plain mean): grade",
        repool_h_ra, "d_g")
wall_ra("healthy cells -> NE slate (plain mean): stop",
        repool_h_ra, "d_s")
wall_ra("healthy cells -> NE slate (plain mean): c3",
        repool_h_ra, "d_a")

# faced re-pool: snap-weighted across the faced cells must
# equal canon cmp_rundef_slate 2025 columns (scored-weight law)
repool_f_ra <- cells_f_ra %>%
  group_by(band) %>%
  summarise(r_g = weighted.mean(f_g, w = fr),
            r_s = weighted.mean(f_s, w = fr),
            r_a = weighted.mean(f_a, w = fr),
            .groups = "drop") %>%
  inner_join(cmp_rundef_slate %>%
               select(band, g25 = grade_25, s25 = stop_25),
             by = "band") %>%
  mutate(d_g = r_g - g25, d_s = r_s - s25)
wall_ra("faced cells -> canon faced slate: grade",
        repool_f_ra, "d_g")
wall_ra("faced cells -> canon faced slate: stop",
        repool_f_ra, "d_s")

# adjusted faced self-wall: two-step (cells) vs one-step (raw
# games) -- the scored weights make this exact, not lucky
a_dir_ra <- faced_games_a_ra %>%
  filter(focal == "NE") %>%
  group_by(band) %>%
  summarise(a_dir = weighted.mean(a_gp, w = sn), .groups = "drop")
a_two_ra <- cells_f_ra %>%
  group_by(band) %>%
  summarise(a_two = weighted.mean(f_a, w = fr), .groups = "drop")
a_chk_ra <- a_dir_ra %>%
  inner_join(a_two_ra, by = "band") %>%
  mutate(d_a = a_two - a_dir)
wall_ra("faced adjusted: two-step vs one-step", a_chk_ra, "d_a")

vis_ra <- expand_grid(team_name = unique(sched_2026),
                      band = BANDS_RD_LG) %>%
  left_join(cells_h_ra, by = c("team_name", "band")) %>%
  left_join(cells_f_ra, by = c("team_name" = "team", "band")) %>%
  mutate(d_h  = h_g - f_g,  d_r  = p_g - f_g,
         d_ha = h_a - f_a,  d_ra = p_a - f_a,
         d_hs = h_s - f_s,  d_rs = p_s - f_s)
stopifnot(nrow(vis_ra) == length(unique(sched_2026)) *
            length(BANDS_RD_LG),
          !any(is.na(vis_ra$h_g)))
# NOTE: f_g / f_s / f_a are NA for 2026 opponents NE did NOT
# face in 2025 -- that is honest, the table shows "--".

team_ord_ra <- vis_ra %>%
  group_by(team_name) %>%
  summarise(ord = mean(d_r, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(ord)) %>%
  pull(team_name)

gt_ra <- vis_ra %>%
  mutate(team_name = factor(team_name, levels = team_ord_ra),
         band = factor(band, levels = BANDS_RD_LG),
         # display scale: deltas print in percentile POINTS
         # (values stay 0-1 fractions for fmt_percent)
         d_r = 100 * d_r, d_ra = 100 * d_ra, d_rs = 100 * d_rs) %>%
  arrange(team_name, band) %>%
  select(team_name, band,
         f_g, h_g, p_g, d_r,
         f_a, h_a, p_a, d_ra,
         f_s, h_s, p_s, d_rs,
         fill_sh, interp) %>%
  gt(groupname_col = "team_name", rowname_col = "band") %>%
  tab_spanner(label = "Run-defense grade (raw)",
              columns = c(f_g, h_g, p_g, d_r)) %>%
  tab_spanner(label = "Adjusted (same-slate c3)",
              columns = c(f_a, h_a, p_a, d_ra)) %>%
  tab_spanner(label = "Stop rate (raw)",
              columns = c(f_s, h_s, p_s, d_rs)) %>%
  tab_spanner(label = "Honesty",
              columns = c(fill_sh, interp)) %>%
  cols_label(f_g = "faced 25", h_g = "healthy 26",
             p_g = "priced 26", d_r = "\u0394",
             f_a = "faced 25", h_a = "healthy 26",
             p_a = "priced 26", d_ra = "\u0394",
             f_s = "faced 25", h_s = "healthy 26",
             p_s = "priced 26", d_rs = "\u0394",
             fill_sh = "fill 25", interp = "interp 26") %>%
  fmt_percent(columns = c(f_g, h_g, p_g, f_a, h_a, p_a,
                          f_s, h_s, p_s), decimals = 0) %>%
  fmt_percent(columns = c(fill_sh, interp), decimals = 0) %>%
  fmt_number(columns = c(d_r, d_ra, d_rs), decimals = 1,
             force_sign = TRUE) %>%
  sub_missing(missing_text = "--") %>%
  data_color(columns = c(d_r, d_ra, d_rs),
             colors = scales::col_numeric(
               palette = c("#6baed6", "#f7f7f7", "#C60C30"),
               domain = NULL)) %>%
  tab_header(
    title = "What NE's ground game runs into in 2026",
    subtitle = paste0(
      "Opponent run defenses: faced 2025 vs healthy 2026 vs ",
      "injury-priced 2026. Red = harder for NE, blue = softer. ",
      "fill = share of 2025 faced snaps priced at a band prior; ",
      "interp = share of 2026 snaps on interpolated (prior) ",
      "talent. Stop rate rides with the canon talent caveat.")) %>%
  tab_options(table.font.size = px(12),
              data_row.padding = px(3),
              column_labels.font.weight = "bold",
              row_group.font.weight = "bold")
print(gt_ra)

cat("\n== RUN-DEFENSE AVAILABILITY LAYER: DONE ==\n")
cat("objects: members_ra, ne_members_ra, slate_ra, sweep_ra,",
    "cells_h_ra, cells_f_ra, vis_ra, gt_ra\n")
cat("next: source ne_own_units_before_after.txt v3 -- it folds",
    "unit seven (members_ra) into the NE mirror table.\n")