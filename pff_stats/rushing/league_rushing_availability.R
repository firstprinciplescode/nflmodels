# ============================================================
# AVAILABILITY LAYER -- OPPOSING RUNNERS (RB RUSHING UNIT)
#
# CARRIES THE STAMP (same law as pass block / pass rush / run
# block -- do not re-litigate):
#   - value_26_priced = avail x value_26 + (1 - avail) x repl.
#   - avail judged ONLY on seasons the player HELD the job.
#     Zero job seasons -> observed FIRST-YEAR rate for his
#     population, measured from 2024-25 first-year job holders:
#       backup_line = in the league, never held a job (phantoms
#                     and no-window vets ride this);
#       rookie_line = 2026 entry class (roster truth:
#                     entry_year == 2026 / canon status == "rookie").
#     2023 first-year holders excluded (window edge). Empty cell
#     falls back to the pooled unit line, evidence column shown.
#   - repl = empirical 2025 backup level, signed fill law per
#     lens: earned season pctl else pool prior (box) / game c3
#     else pool c3 prior (adjusted).
#   - FACED-2025 IS UNTOUCHED (realized football).
#   - FULL PRECISION inside; rounding is display, at print only,
#     NEVER before a wall. Canon targets here are UNROUNDED
#     (cmp_rush_slate, league_rush) -> walls at 1e-8.
#   - PRINT LAW: short English phrases, one lens per table,
#     every table opens with its question, display renames at
#     print time, internals keep contract names.
#
# UNIT ADAPTATIONS (PROPOSED -- Andy stamps or corrects):
#   1. played = >= 4 rushing attempts that week. Agrees with the
#      canon currency gate (X_QUAL = X_RB = 4) -- same pattern as
#      run block's 8 snaps agreeing with X_QUAL_RBLK.
#   2. job = season TOP-2 per team by qualifying attempts
#      (mirrors the N_RB = 2 committee law; membership usage is
#      pmax('25,'24) attempts, history here is per-season).
#   3. THREE lenses, one per table: RUN GRADE (grun),
#      TACKLE-BREAKING (mtf), ADJUSTED (c3 on grades_run).
#      Breakaway stays OUT (canon: NOISE at every viable gate);
#      yco / ypa stay OUT (canon: table-only, ypa OL-contaminated).
#   4. ATTEMPTS-WEIGHTED EVERYWHERE. Unit law: canon weights team
#      committee values by usage and faced values by that game's
#      attempts. The OL equal-game-weights law does NOT transfer;
#      the pass-rush snap weighting was that unit's adaptation too.
#   5. SINGLE POOL. band = "HB" constant: ONE repl line per lens,
#      ONE backup/rookie line pair. No band split anywhere.
#   6. Rookies ARE visible in membership on this unit (the ledger
#      keeps status == "rookie"; the league rotation frame carries
#      entry_year == 2026 bodies at zero usage). No blind spot.
#   7. History window is REGULAR SEASON (week <= 18) for played /
#      job / repl -- the 17-game denominator law. Canon's faced
#      side keeps playoffs IN (untouched, stamped 2026-08-17).
#
# OPERATIVE-CANON NOTE: the league file re-runs the whole NE
#   section in-session with the STAMPED faced fill law (fill at
#   the entry-year prior, nobody drops; the standalone NE file
#   still prints the superseded observed-only version). This layer
#   walls against those operative objects.
#
# CROSSED-STREAMS LAW (ruled into this file 2026-08-20, after the
#   faced_games_2025 collision fired in Andy's session):
#   SIX canon files across TWO units assign the generic name
#   faced_games_2025 -- the pass-rush files (NE + league) with
#   band/band_row/g_snaps columns, the rushing files with
#   team/g_att. One global session: last writer wins, and the
#   pass-rush availability work ran after the rushing canon.
#   THEREFORE: this layer consumes UNIT-UNIQUE names only.
#     - NE faced cells read faced_games_rb_lg (league file,
#       rushing-unique) filtered to focal == "NE" -- certified
#       identical to the NE faced side by canon's own 1e-8 wall
#       inside the league file. The generic faced_games_2025 is
#       NEVER read.
#     - entry years read ent_rb_lg (league tail, rushing-unique),
#       never the generic entry_years_off (the receiving files
#       assign that name too).
#     - the generic gate scalars X_QUAL / G_MIN are NEVER read
#       (every unit file re-declares them with its own values);
#       this layer reads the rushing-suffixed X_RB / G_RB / N_RB.
#     - the generic name "ledger" is NEVER read either -- FIVE
#       files across the pass-rush and rushing units assign it
#       (same ruling-ledger template, different schemas; the
#       att_2025/att_2024 fingerprint gate caught the pass-rush
#       copy in Andy's session 2026-08-20). Member id recovery
#       reads ros_rb_lg (rushing-unique) instead.
#     - shared primitives sched_2026 / opp_2026_teams / blend2 /
#       in_season are identical by construction across units;
#       they are the only generics consumed.
#   Consequence: no re-source of any canon file is needed to run
#   this layer, even if another unit's files ran more recently.
#
# SUFFIX LAW: this layer uses _ru everywhere. Canon rushing owns
#   _rb (rot_2026_rb, cmp_rush_slate, league_rush); the run-block
#   layer's objects also carry _rb. Nothing here overwrites either
#   namespace.
#
# SOURCE ORDER (re-source WHOLE files, in order):
#   (1) step-0: pff_rushing_stats_build_one_AWS.R  (rushing_qbgrp)
#   (2) new_england_opp_rushing_schedule.R
#   (3) league_rushing_evaluating_currency_three.R (ru_c3_pctl)
#   (4) league_opp_rushing_schedule(1).R           (league + walls)
#   (5) this file, top to bottom.
#   The split-build files (two/three/four/five) and the receiving
#   exploration file are upstream of step-0 or Phase-5 scope --
#   not needed for this layer.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(tibble)
})

needed_ru_av <- c("rushing_qbgrp", "rush_qual_games",
                  "rush_season_pctl_sos", "ru_c3_pctl",
                  "rush_rookie_prior", "prior_rb_lg",
                  "rot_2026_rb", "team_rb_2026",
                  "cmp_rush_slate",
                  "rot_2026_rb_lg", "team26_rb_lg",
                  "faced_games_rb_lg", "faced_rb_lg", "league_rush",
                  "opp26_rb", "ros_rb_lg", "cur_rb_lg",
                  "ent_rb_lg", "sched_2026", "opp_2026_teams",
                  "in_season", "blend2",
                  "X_RB", "G_RB", "N_RB")
missing_ru_av <- needed_ru_av[!vapply(needed_ru_av, exists, logical(1))]
if (length(missing_ru_av)) {
  cat("missing session objects:", paste(missing_ru_av, collapse = ", "), "\n")
  stop("source the canon files first -- see SOURCE ORDER in header")
}

# names() receipt walls -- print actual columns, then stop loud.
req_cols_ru <- list(
  rushing_qbgrp       = c("player", "player_id", "season", "week",
                          "team", "def_ssn", "attempts", "grades_run",
                          "elu_rush_mtf"),
  rush_qual_games     = c("player_id", "season", "week", "team",
                          "attempts"),
  rush_season_pctl_sos = c("player_id", "season", "grun_pctl",
                           "mtf_pctl"),
  ru_c3_pctl          = c("player_id", "season", "band", "qual_g",
                          "c3_pctl", "player"),
  rush_rookie_prior   = c("pr_grun", "pr_mtf"),
  prior_rb_lg         = c("pr_grun", "pr_mtf"),
  rot_2026_rb         = c("team_name", "roster_name", "status",
                          "prior_used", "usage_w", "grun_f", "mtf_f"),
  rot_2026_rb_lg      = c("team", "roster_name", "prior_used",
                          "gf", "mf", "uw"),
  ros_rb_lg           = c("team", "roster_name", "player_id",
                          "entry_year"),
  faced_games_rb_lg   = c("player_id", "week", "focal", "team",
                          "attempts", "grun_pctl", "mtf_pctl", "fill"),
  faced_rb_lg         = c("focal", "grun_25", "mtf_25", "att_faced",
                          "fill_share_25"),
  league_rush         = c("focal", "grun_25", "mtf_25", "grun_26",
                          "mtf_26", "fill_share_25"),
  cmp_rush_slate      = c("grun_25", "mtf_25", "fill_share_25",
                          "grun_26", "mtf_26", "interp_share"),
  opp26_rb            = c("focal", "opp"),
  team_rb_2026        = c("team_name", "grun_26", "mtf_26",
                          "interp_share"),
  team26_rb_lg        = c("team", "grun_26", "mtf_26", "interp_share"),
  cur_rb_lg           = c("player_id", "season", "grun_pctl",
                          "mtf_pctl"),
  ent_rb_lg           = c("player_id", "entry_year"))
for (nm_ru in names(req_cols_ru)) {
  fr_ru <- get(nm_ru)
  miss_ru <- setdiff(req_cols_ru[[nm_ru]], names(fr_ru))
  if (length(miss_ru)) {
    cat("frame:", nm_ru, "-- missing columns:\n"); print(miss_ru)
    cat("actual columns:\n"); print(names(fr_ru))
    stop("column receipt printed above -- no-placeholder law")
  }
}

# (generic gate scalars X_QUAL / G_MIN are deliberately never read --
#  CROSSED-STREAMS LAW in the header; X_RB / G_RB / N_RB are the
#  rushing-suffixed canon gates)
stopifnot(X_RB == 4L, G_RB == 6L, N_RB == 2L)

# ------------------------------------------------------------
# knobs (unit-suffixed; stamped pattern, PROPOSED values)
# ------------------------------------------------------------
AVAIL_SEASONS_RU  <- 2023:2025   # history window
AVAIL_ATT_MIN_RU  <- 4           # played = >= 4 attempts (canon X)
AVAIL_DENOM_RU    <- 17          # REG games

cat("\n== AVAILABILITY LAYER -- OPPOSING RUNNERS == knobs: seasons",
    paste(range(AVAIL_SEASONS_RU), collapse = "-"),
    "| played = attempts >=", AVAIL_ATT_MIN_RU,
    "| committee", N_RB, "deep | denominator", AVAIL_DENOM_RU,
    "REG games ==\n")

# ------------------------------------------------------------
# 1. AVAILABILITY HISTORY (2023-2025, REG only)
#    played = >= AVAIL_ATT_MIN_RU attempts that week.
#    job    = season top-2 per team by qualifying attempts
#             (the N_RB = 2 committee law, applied to history).
# ------------------------------------------------------------

played_wk_ru <- rushing_qbgrp %>%
  filter(season %in% AVAIL_SEASONS_RU, week <= 18) %>%
  group_by(player_id, season, week, team) %>%
  summarise(g_att = sum(attempts, na.rm = TRUE), .groups = "drop") %>%
  mutate(played = g_att >= AVAIL_ATT_MIN_RU)

starters_all_ru <- played_wk_ru %>%
  filter(played) %>%
  group_by(season, team, player_id) %>%
  summarise(att = sum(g_att), g = dplyr::n(), .groups = "drop") %>%
  group_by(season, team) %>%
  arrange(desc(att), player_id, .by_group = TRUE) %>%
  mutate(rb_rank = dplyr::row_number()) %>%
  ungroup() %>%
  mutate(has_job = rb_rank <= N_RB)

# collapse to player-season before any join: a traded back can hold
# a job at BOTH teams -- team-grain rows would double-count him.
job_flag_ru <- starters_all_ru %>%
  group_by(player_id, season) %>%
  summarise(has_job = any(has_job), g_tot = sum(g),
            .groups = "drop")

avail_ru <- played_wk_ru %>%
  group_by(player_id, season) %>%
  summarise(gp = sum(played), .groups = "drop") %>%
  left_join(job_flag_ru %>% select(player_id, season, has_job),
            by = c("player_id", "season")) %>%
  mutate(has_job = dplyr::coalesce(has_job, FALSE)) %>%
  group_by(player_id) %>%
  summarise(yrs       = dplyr::n(),
            job_yrs   = sum(has_job),
            avail_all = sum(gp) / (AVAIL_DENOM_RU * yrs),
            avail_job = if (any(has_job))
              sum(gp[has_job]) / (AVAIL_DENOM_RU * job_yrs)
            else NA_real_,
            .groups = "drop")

cat("\n--- durability among backs who HELD a committee job at least once ---\n")
print(quantile(avail_ru$avail_job, na.rm = TRUE,
               probs = c(0, .1, .25, .5, .75, .9, 1)) %>% round(3))

# ------------------------------------------------------------
# 2. BENCHMARK -- how durable is an ESTABLISHED committee back?
#    (2025 job holders; context only -- not the first-year fill)
# ------------------------------------------------------------

bench_ru <- avail_ru %>%
  filter(job_yrs > 0) %>%
  summarise(starters  = sum(job_yrs),
            mean_avail = mean(avail_job, na.rm = TRUE))

cat("\n--- how much of a season does an established committee back",
    "keep? (benchmark for context) ---\n")
print(bench_ru %>% transmute("job-holder seasons" = starters,
                             "kept rate" = round(mean_avail, 3)))

# ------------------------------------------------------------
# 0b. c3 ENTRY-YEAR PRIOR (adjusted currency) -- single pool.
#     Mirror of the stamped pattern: season == entry_year,
#     entry_year >= 2017, median of the season c3 percentiles.
# ------------------------------------------------------------

stopifnot(all(ru_c3_pctl$band == "HB"))

c3_rookie_prior_ru <- ru_c3_pctl %>%
  inner_join(ent_rb_lg, by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2017) %>%
  summarise(pr_c3   = median(c3_pctl, na.rm = TRUE),
            n_entry = dplyr::n(), .groups = "drop")
stopifnot(nrow(c3_rookie_prior_ru) == 1L, !is.na(c3_rookie_prior_ru$pr_c3))

cat("\n--- c3 entry-year prior (adjusted currency, single HB pool) ---\n")
print(c3_rookie_prior_ru)

# ------------------------------------------------------------
# 2b. FIRST-YEAR LINES -- when a NEW committee member holds a job
#     for the first time, how much of that season does he keep?
#     Populations: backup (in the league, never held a job) vs
#     rookie (entry_year == season). 2024-25 first-year holders;
#     2023 excluded (window edge). Empty cell -> pooled unit
#     line, evidence column shown. Single pool: ONE pair of
#     lines, no slot split.
# ------------------------------------------------------------

seen_2023_ru  <- played_wk_ru %>%
  filter(season == 2023, played) %>% distinct(player_id)
seen_2324_ru  <- played_wk_ru %>%
  filter(season %in% 2023:2024, played) %>% distinct(player_id)

first_year_ru <- bind_rows(
  job_flag_ru %>% filter(season == 2024, has_job) %>%
    anti_join(seen_2023_ru,  by = "player_id"),
  job_flag_ru %>% filter(season == 2025, has_job) %>%
    anti_join(seen_2324_ru, by = "player_id")) %>%
  left_join(ent_rb_lg, by = "player_id") %>%
  mutate(pop  = dplyr::case_when(entry_year == season ~ "rookie",
                                 TRUE                 ~ "backup"),
         rate = pmin(g_tot / AVAIL_DENOM_RU, 1))

first_year_cells_ru <- first_year_ru %>%
  group_by(pop) %>%
  summarise(n    = dplyr::n(),
            line = mean(rate), .groups = "drop")

unit_line_ru <- mean(first_year_ru$rate)
unit_n_ru    <- nrow(first_year_ru)

lines_ru <- tidyr::expand_grid(pop = c("backup", "rookie")) %>%
  left_join(first_year_cells_ru, by = "pop") %>%
  mutate(evidence = dplyr::case_when(
    !is.na(n) ~ paste0(n, " first-year job holders"),
    TRUE      ~ paste0("empty cell -- pooled unit line (",
                       unit_n_ru, " holders)")),
    n    = dplyr::coalesce(n, 0L),
    line = dplyr::coalesce(line, unit_line_ru))
if (any(is.na(lines_ru$line))) {
  print(lines_ru); stop("first-year lines produced NA -- report")
}

cat("\n--- the first-year lines: when a NEW committee member holds",
    "a job for the first time, how much of that season does he",
    "keep? ---\n")
print(lines_ru %>% transmute(population = pop,
                             "kept rate" = round(line, 3),
                             evidence))

line_backup_ru <- lines_ru$line[lines_ru$pop == "backup"]
line_rookie_ru <- lines_ru$line[lines_ru$pop == "rookie"]

# ------------------------------------------------------------
# 3. repl -- the empirical 2025 backup-runner level, REG, one
#    line per lens (single pool). "Who you actually get when a
#    committee back is out": qualifying runner-games by backs
#    OUTSIDE their team's season top-2. ATTEMPTS-WEIGHTED (unit
#    law). Signed fill law per lens: earned 2025 season level,
#    else pool prior.
# ------------------------------------------------------------

starters_25_ru <- starters_all_ru %>%
  filter(season == 2025, has_job) %>%
  select(team, player_id)

repl_games_ru <- played_wk_ru %>%
  filter(season == 2025, played) %>%
  anti_join(starters_25_ru, by = c("team", "player_id")) %>%
  left_join(rush_season_pctl_sos %>% filter(season == 2025) %>%
              select(player_id, grun25 = grun_pctl, mtf25 = mtf_pctl),
            by = "player_id") %>%
  left_join(ru_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, c3_25 = c3_pctl),
            by = "player_id") %>%
  mutate(grun_val = dplyr::coalesce(grun25, rush_rookie_prior$pr_grun),
         mtf_val  = dplyr::coalesce(mtf25,  rush_rookie_prior$pr_mtf),
         adj_val  = dplyr::coalesce(c3_25,  c3_rookie_prior_ru$pr_c3))

repl_level_ru <- repl_games_ru %>%
  summarise(n_backup  = dplyr::n(),
            att_backup = sum(g_att),
            repl_grun = weighted.mean(grun_val, w = g_att),
            repl_mtf  = weighted.mean(mtf_val,  w = g_att),
            repl_adj  = weighted.mean(adj_val,  w = g_att))
if (any(is.na(repl_level_ru))) {
  print(repl_level_ru); stop("repl level produced NA -- report")
}

cat("\n--- when a committee back is out, what do you get? (2025",
    "backup runner-games, attempts-weighted, per lens) ---\n")
print(repl_level_ru %>%
        transmute("backup gms seen" = n_backup,
                  "backup carries"  = att_backup,
                  "fill lvl (grun)" = round(repl_grun, 3),
                  "fill lvl (mtf)"  = round(repl_mtf, 3),
                  "fill lvl (adj)"  = round(repl_adj, 3)))

# ------------------------------------------------------------
# 4. LEAGUE MEMBERSHIP + PRICING (32 teams x 2)
#    player_id / entry_year recovered from ros_rb_lg by
#    (team, roster_name) -- canon rot_2026_rb_lg drops them in
#    its final select; the roster frame walls uniqueness, so the
#    recovery join is exact. Values (gf / mf) are canon's,
#    UNTOUCHED. The adjusted lens (V_c3) is built here from
#    ru_c3_pctl with the same blend2(w25) + prior shape canon
#    uses for the box lenses.
# ------------------------------------------------------------

memb_lg_ru <- rot_2026_rb_lg %>%
  left_join(ros_rb_lg %>%
              select(team, roster_name, player_id, entry_year),
            by = c("team", "roster_name")) %>%
  mutate(phantom = grepl("^PHANTOM", roster_name))
stopifnot(nrow(memb_lg_ru) == 32L * N_RB,
          anyDuplicated(memb_lg_ru[, c("team", "roster_name")]) == 0)

memb_lg_ru <- memb_lg_ru %>%
  left_join(ru_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, a25 = c3_pctl, qg_a = qual_g),
            by = "player_id") %>%
  left_join(ru_c3_pctl %>% filter(season == 2024) %>%
              select(player_id, a24 = c3_pctl),
            by = "player_id") %>%
  mutate(w25a = pmin(dplyr::coalesce(qg_a, 0L) / 10, 1),
         V_c3 = dplyr::coalesce(blend2(a25, a24, w25a),
                                c3_rookie_prior_ru$pr_c3)) %>%
  # --- layer-only joins below this line; canon values untouched ---
  left_join(avail_ru %>%
              select(player_id, yrs, job_yrs, avail_all, avail_job),
            by = "player_id") %>%
  mutate(avail_src = dplyr::case_when(
    !is.na(avail_job)                    ~ "starter_history",
    phantom                              ~ "backup_line",
    dplyr::coalesce(entry_year, 0L) == 2026 ~ "rookie_line",
    TRUE                                 ~ "backup_line"),
    avail = dplyr::case_when(
      avail_src == "starter_history" ~ avail_job,
      avail_src == "rookie_line"     ~ line_rookie_ru,
      TRUE                           ~ line_backup_ru),
    gf_av   = round(avail * gf   + (1 - avail) *
                      repl_level_ru$repl_grun, 4),
    mf_av   = round(avail * mf   + (1 - avail) *
                      repl_level_ru$repl_mtf, 4),
    V_c3_av = round(avail * V_c3 + (1 - avail) *
                      repl_level_ru$repl_adj, 4))

if (any(is.na(memb_lg_ru$gf))  || any(is.na(memb_lg_ru$mf)) ||
    any(is.na(memb_lg_ru$V_c3)) || any(is.na(memb_lg_ru$avail))) {
  print(memb_lg_ru %>%
          filter(is.na(gf) | is.na(mf) | is.na(V_c3) | is.na(avail)))
  stop("league membership produced NA value or availability -- report")
}

# ------------------------------------------------------------
# THE LEAGUE WALL: league_rush rebuilt from this membership and
# these faced cells -- all 32 focals, both box lenses, the faced
# side and the 2026 side, 1e-8. league_rush is UNROUNDED, so the
# wall is full precision. Offenders + focal setdiffs print
# BEFORE the stop; a real mismatch cannot hide.
# ------------------------------------------------------------

team26_chk_ru <- memb_lg_ru %>%
  group_by(team) %>%
  summarise(grun_26_m = weighted.mean(gf, w = uw),
            mtf_26_m  = weighted.mean(mf, w = uw), .groups = "drop")

# team-level wall vs canon's team26_rb_lg: the focal-mean wall
# below cannot see a same-league team swap; this one can.
chk_team26_ru <- team26_rb_lg %>%
  select(team, grun_26, mtf_26) %>%
  full_join(team26_chk_ru, by = "team")
bad_team26_ru <- chk_team26_ru %>%
  filter(dplyr::if_any(everything(), is.na) |
           abs(grun_26 - grun_26_m) > 1e-8 | abs(mtf_26 - mtf_26_m) > 1e-8)
if (nrow(chk_team26_ru) != 32L || nrow(bad_team26_ru) > 0L) {
  cat("team-level wall failure -- rows:", nrow(chk_team26_ru),
      "(need 32)\n")
  print(bad_team26_ru)
  stop("rushing committee values drifted from team26_rb_lg -- report")
}
cat("\n--- team wall passed: all 32 committee values identical to",
    "team26_rb_lg (1e-8) ---\n")

slate_chk_ru <- opp26_rb %>%
  left_join(team26_chk_ru, by = c("opp" = "team")) %>%
  group_by(focal) %>%
  summarise(grun_26_m = mean(grun_26_m),
            mtf_26_m  = mean(mtf_26_m), .groups = "drop")

faced_chk_ru <- faced_games_rb_lg %>%
  group_by(focal) %>%
  summarise(grun_25_m = weighted.mean(grun_pctl, w = attempts),
            mtf_25_m  = weighted.mean(mtf_pctl,  w = attempts),
            fill_m    = sum(attempts[fill]) / sum(attempts),
            .groups = "drop")

chk_lg_ru <- league_rush %>%
  select(focal, grun_25, mtf_25, grun_26, mtf_26, fill_share_25) %>%
  full_join(slate_chk_ru, by = "focal") %>%
  full_join(faced_chk_ru, by = "focal")

bad_lg_ru <- chk_lg_ru %>%
  filter(dplyr::if_any(c(grun_25, mtf_25, grun_26, mtf_26, fill_share_25,
                         grun_25_m, mtf_25_m, grun_26_m, mtf_26_m,
                         fill_m), is.na) |
           abs(grun_25 - grun_25_m) > 1e-8 |
           abs(mtf_25  - mtf_25_m)  > 1e-8 |
           abs(grun_26 - grun_26_m) > 1e-8 |
           abs(mtf_26  - mtf_26_m)  > 1e-8 |
           abs(fill_share_25 - fill_m) > 1e-8)

if (nrow(chk_lg_ru) != 32L || nrow(bad_lg_ru) > 0L) {
  cat("league wall failure -- rows:", nrow(chk_lg_ru),
      "(need 32); bad rows:", nrow(bad_lg_ru), "\n")
  cat("focals in canon but not my 2026 side:\n")
  print(setdiff(league_rush$focal, slate_chk_ru$focal))
  cat("focals in my 2026 side but not canon:\n")
  print(setdiff(slate_chk_ru$focal, league_rush$focal))
  cat("focals in canon but not my faced side:\n")
  print(setdiff(league_rush$focal, faced_chk_ru$focal))
  cat("focals in my faced side but not canon:\n")
  print(setdiff(faced_chk_ru$focal, league_rush$focal))
  print(bad_lg_ru)
  stop("rushing membership or faced side drifted from league_rush",
       " -- report")
}
cat("\n--- league wall passed: committee membership + faced rebuild",
    "identical to league_rush, both box lenses + fill share,",
    "32 focals ---\n")

memb_2026_ru <- memb_lg_ru

# ------------------------------------------------------------
# 5. NE SLATE -- the 14 opposing committees, priced.
#    player_id / entry_year recovered from ros_rb_lg by
#    (team_name, roster_name) -- canon rot_2026_rb drops them in
#    its final select. ros_rb_lg is the league tail's 32-team
#    roster frame, rushing-unique (the generic "ledger" name is
#    shared with the pass-rush files and is never read here --
#    CROSSED-STREAMS LAW). Phantom members correctly recover NA.
#    Canon values (grun_f / mtf_f / usage_w / prior_used)
#    UNTOUCHED.
# ------------------------------------------------------------

memb_ne_ru <- rot_2026_rb %>%
  left_join(ros_rb_lg %>%
              select(team_name = team, roster_name,
                     player_id, entry_year),
            by = c("team_name", "roster_name")) %>%
  mutate(phantom = status == "phantom")
stopifnot(nrow(memb_ne_ru) == length(opp_2026_teams) * N_RB,
          anyDuplicated(memb_ne_ru[, c("team_name", "roster_name")]) == 0)

memb_ne_ru <- memb_ne_ru %>%
  left_join(ru_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, a25 = c3_pctl, qg_a = qual_g),
            by = "player_id") %>%
  left_join(ru_c3_pctl %>% filter(season == 2024) %>%
              select(player_id, a24 = c3_pctl),
            by = "player_id") %>%
  mutate(w25a = pmin(dplyr::coalesce(qg_a, 0L) / 10, 1),
         c3_f = dplyr::coalesce(blend2(a25, a24, w25a),
                                c3_rookie_prior_ru$pr_c3)) %>%
  left_join(avail_ru %>%
              select(player_id, yrs, job_yrs, avail_all, avail_job),
            by = "player_id") %>%
  mutate(avail_src = dplyr::case_when(
    !is.na(avail_job)  ~ "starter_history",
    phantom            ~ "backup_line",
    status == "rookie" ~ "rookie_line",
    TRUE               ~ "backup_line"),
    avail = dplyr::case_when(
      avail_src == "starter_history" ~ avail_job,
      avail_src == "rookie_line"     ~ line_rookie_ru,
      TRUE                           ~ line_backup_ru),
    grun_av = round(avail * grun_f + (1 - avail) *
                      repl_level_ru$repl_grun, 4),
    mtf_av  = round(avail * mtf_f  + (1 - avail) *
                      repl_level_ru$repl_mtf, 4),
    c3_av   = round(avail * c3_f   + (1 - avail) *
                      repl_level_ru$repl_adj, 4))

if (any(is.na(memb_ne_ru$grun_f)) || any(is.na(memb_ne_ru$mtf_f)) ||
    any(is.na(memb_ne_ru$c3_f))   || any(is.na(memb_ne_ru$avail))) {
  print(memb_ne_ru %>%
          filter(is.na(grun_f) | is.na(mtf_f) | is.na(c3_f) |
                   is.na(avail)))
  stop("NE committee produced NA value or availability -- report")
}

# -- NE faced cells from the COLLISION-PROOF source: the league
#    file's all-32 faced frame (rushing-unique name), filtered to
#    NE. Canon's own in-file wall already certified this frame
#    reproduces cmp_rush_slate's faced side at 1e-8. The generic
#    faced_games_2025 is never read (CROSSED-STREAMS LAW). -------
faced_ne_cells_ru <- faced_games_rb_lg %>%
  filter(focal == "NE") %>%
  transmute(player_id, week, team, g_att = attempts,
            g25 = grun_pctl, m25 = mtf_pctl, fill)

# -- NE WALL 1: the faced side, rebuilt from these cells, must
#    equal cmp_rush_slate exactly (1e-8) ------------------------
chk_faced_ne_ru <- faced_ne_cells_ru %>%
  summarise(grun_25_m = weighted.mean(g25, w = g_att),
            mtf_25_m  = weighted.mean(m25, w = g_att),
            fill_m    = sum(g_att[fill]) / sum(g_att))
if (!isTRUE(all.equal(chk_faced_ne_ru$grun_25_m,
                      cmp_rush_slate$grun_25, tolerance = 1e-8)) ||
    !isTRUE(all.equal(chk_faced_ne_ru$mtf_25_m,
                      cmp_rush_slate$mtf_25, tolerance = 1e-8)) ||
    !isTRUE(all.equal(chk_faced_ne_ru$fill_m,
                      cmp_rush_slate$fill_share_25, tolerance = 1e-8))) {
  print(chk_faced_ne_ru); print(cmp_rush_slate)
  stop("NE faced rebuild drifted from cmp_rush_slate -- report")
}

# -- NE WALL 2: the healthy 2026 slate, rebuilt from committee
#    members, must equal cmp_rush_slate exactly (1e-8) ----------
team_ne_chk_ru <- memb_ne_ru %>%
  group_by(team_name) %>%
  summarise(interp_m = sum(usage_w[prior_used]) / sum(usage_w),
            grun_26_m = weighted.mean(grun_f, w = usage_w),
            mtf_26_m  = weighted.mean(mtf_f,  w = usage_w),
            .groups = "drop")
# team-level wall vs canon's team_rb_2026 first: the slate-mean
# wall cannot see a same-slate team swap; this one can.
chk_team_ne_ru <- team_rb_2026 %>%
  select(team_name, grun_26, mtf_26) %>%
  full_join(team_ne_chk_ru %>% select(team_name, grun_26_m, mtf_26_m),
            by = "team_name")
bad_team_ne_ru <- chk_team_ne_ru %>%
  filter(dplyr::if_any(everything(), is.na) |
           abs(grun_26 - grun_26_m) > 1e-8 | abs(mtf_26 - mtf_26_m) > 1e-8)
if (nrow(chk_team_ne_ru) != length(opp_2026_teams) ||
    nrow(bad_team_ne_ru) > 0L) {
  cat("NE team-level wall failure -- rows:", nrow(chk_team_ne_ru),
      "(need", length(opp_2026_teams), ")\n")
  print(bad_team_ne_ru)
  stop("NE committee values drifted from team_rb_2026 -- report")
}

chk_slate_ne_ru <- tibble(team_name = sched_2026) %>%
  left_join(team_ne_chk_ru, by = "team_name",
            relationship = "many-to-many") %>%
  summarise(grun_26_m   = mean(grun_26_m),
            mtf_26_m    = mean(mtf_26_m),
            interp_m    = mean(interp_m))
if (!isTRUE(all.equal(chk_slate_ne_ru$grun_26_m,
                      cmp_rush_slate$grun_26, tolerance = 1e-8)) ||
    !isTRUE(all.equal(chk_slate_ne_ru$mtf_26_m,
                      cmp_rush_slate$mtf_26, tolerance = 1e-8)) ||
    !isTRUE(all.equal(chk_slate_ne_ru$interp_m,
                      cmp_rush_slate$interp_share, tolerance = 1e-8))) {
  print(chk_slate_ne_ru); print(cmp_rush_slate)
  stop("NE healthy slate rebuild drifted from cmp_rush_slate -- report")
}
cat("\n--- NE walls passed: faced side + healthy slate identical",
    "to cmp_rush_slate (1e-8) ---\n")

# -- NE faced ADJUSTED lens (layer-only; canon has no faced c3).
#    Same fill law: earned 2025 c3 if season-gated, else the
#    entry-year prior. Attempts-weighted, playoffs IN. ---------
faced_adj_ne_ru <- faced_ne_cells_ru %>%
  left_join(ru_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, c3_25 = c3_pctl),
            by = "player_id") %>%
  mutate(fill_c3  = is.na(c3_25),
         fill_why = dplyr::case_when(!fill_c3 ~ "earned",
                                     TRUE     ~ "no_c3_2025"),
         c3_25    = dplyr::coalesce(c3_25, c3_rookie_prior_ru$pr_c3))

cat("\n--- NE faced fill tiers (adjusted lens, by runner-game) ---\n")
print(faced_adj_ne_ru %>% count(fill_why, name = "games") %>%
        mutate(share = round(games / sum(games), 3)))

adj_25_ne <- faced_adj_ne_ru %>%
  summarise(adj_25 = weighted.mean(c3_25, w = g_att)) %>%
  pull(adj_25)

# -- priced slate scalars: same construction as the healthy wall,
#    priced values instead of canon values ----------------------
team_ne_priced_ru <- memb_ne_ru %>%
  group_by(team_name) %>%
  summarise(grun_26p = weighted.mean(grun_av, w = usage_w),
            mtf_26p  = weighted.mean(mtf_av,  w = usage_w),
            c3_26p   = weighted.mean(c3_av,   w = usage_w),
            .groups = "drop")
slate_ne_priced_ru <- tibble(team_name = sched_2026) %>%
  left_join(team_ne_priced_ru, by = "team_name",
            relationship = "many-to-many") %>%
  summarise(grun_26p = mean(grun_26p),
            mtf_26p  = mean(mtf_26p),
            c3_26p   = mean(c3_26p))
bkup_gms_slate_ru <- tibble(team_name = sched_2026) %>%
  left_join(memb_ne_ru, by = "team_name",
            relationship = "many-to-many") %>%
  summarise(backup_gms = round(sum(1 - avail), 1)) %>%
  pull(backup_gms)

# healthy ADJUSTED slate scalar: same slate law as the box lenses
# (team usage-weighted mean first, then the 17-game grid mean --
# division opponents count twice). Never a flat member mean.
slate_ne_healthy_c3_ru <- tibble(team_name = sched_2026) %>%
  left_join(memb_ne_ru %>%
              group_by(team_name) %>%
              summarise(c3_26h = weighted.mean(c3_f, w = usage_w),
                        .groups = "drop"),
            by = "team_name", relationship = "many-to-many") %>%
  summarise(c3_26h = mean(c3_26h)) %>%
  pull(c3_26h)

# -- THE THREE NE TABLES -- one lens per table ------------------
cat("\n== NE 2026 OPPOSING-RUNNER SLATE -- availability-priced ==\n")

cat("\n--- RUN-GRADE LENS: what do the 2026 opposing backfields",
    "look like once you price in missed games? ---\n")
print(tibble(
  `faced '25`   = round(cmp_rush_slate$grun_25, 3),
  `26 healthy`  = round(cmp_rush_slate$grun_26, 3),
  `healthy jump` = round(cmp_rush_slate$grun_26 -
                           cmp_rush_slate$grun_25, 3),
  `26 priced`   = round(slate_ne_priced_ru$grun_26p, 3),
  `real jump`   = round(slate_ne_priced_ru$grun_26p -
                          cmp_rush_slate$grun_25, 3),
  `backup gms`  = bkup_gms_slate_ru))

cat("\n--- TACKLE-BREAKING LENS: same question, missed-tackles",
    "rate ---\n")
print(tibble(
  `faced '25`   = round(cmp_rush_slate$mtf_25, 3),
  `26 healthy`  = round(cmp_rush_slate$mtf_26, 3),
  `healthy jump` = round(cmp_rush_slate$mtf_26 -
                           cmp_rush_slate$mtf_25, 3),
  `26 priced`   = round(slate_ne_priced_ru$mtf_26p, 3),
  `real jump`   = round(slate_ne_priced_ru$mtf_26p -
                          cmp_rush_slate$mtf_25, 3),
  `backup gms`  = bkup_gms_slate_ru))

cat("\n--- ADJUSTED LENS: same question, opponent-adjusted run",
    "grade ---\n")
print(tibble(
  `faced '25`   = round(adj_25_ne, 3),
  `26 healthy`  = round(slate_ne_healthy_c3_ru, 3),
  `healthy jump` = round(slate_ne_healthy_c3_ru - adj_25_ne, 3),
  `26 priced`   = round(slate_ne_priced_ru$c3_26p, 3),
  `real jump`   = round(slate_ne_priced_ru$c3_26p - adj_25_ne, 3),
  `backup gms`  = bkup_gms_slate_ru))

cat("\n--- the 14 opposing committees, member by member ---\n")
print(memb_ne_ru %>%
        arrange(match(team_name, opp_2026_teams), desc(usage_w)) %>%
        transmute(tm = team_name, back = roster_name, status,
                  "kept rate"  = round(avail, 3),
                  "priced at"  = avail_src,
                  "exp misses" = round((1 - avail) * 17, 1),
                  "lvl (grun)" = round(grun_f, 3),
                  "lvl (adj)"  = round(c3_f, 3)),
      n = Inf)

# ------------------------------------------------------------
# 6. LEAGUE SWEEP -- every 2026 defense's opposing-runner diet,
#    healthy vs priced. Two-step like canon: team committee means
#    (usage-weighted) first, then the focal's 17-game grid mean.
#    Faced side reads canon's faced cells; the adjusted faced
#    lens is layer-only (same fill law).
# ------------------------------------------------------------

team26_av_ru <- memb_2026_ru %>%
  group_by(team) %>%
  summarise(g_h  = weighted.mean(gf,   w = uw),
            g_p  = weighted.mean(gf_av, w = uw),
            m_h  = weighted.mean(mf,   w = uw),
            m_p  = weighted.mean(mf_av, w = uw),
            c_h  = weighted.mean(V_c3, w = uw),
            c_p  = weighted.mean(V_c3_av, w = uw),
            .groups = "drop")

# backup games at MEMBER-grid level -- same construction as the
# other units' slate boards: sum of (1 - avail) over the focal's
# 17-game x 2-member grid (division opponents counted twice).
bkup_focal_ru <- opp26_rb %>%
  left_join(memb_2026_ru %>% select(team, avail),
            by = c("opp" = "team"), relationship = "many-to-many") %>%
  group_by(focal) %>%
  summarise(backup_gms = sum(1 - avail), .groups = "drop")

faced_adj_lg_ru <- faced_games_rb_lg %>%
  left_join(ru_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, c3_25 = c3_pctl),
            by = "player_id") %>%
  mutate(c3_25 = dplyr::coalesce(c3_25, c3_rookie_prior_ru$pr_c3)) %>%
  group_by(focal) %>%
  summarise(adj_25_m = weighted.mean(c3_25, w = attempts),
            .groups = "drop")

sweep_ru <- opp26_rb %>%
  left_join(team26_av_ru, by = c("opp" = "team"),
            relationship = "many-to-many") %>%
  group_by(focal) %>%
  summarise(healthy_grun = mean(g_h), priced_grun = mean(g_p),
            healthy_mtf  = mean(m_h), priced_mtf  = mean(m_p),
            healthy_adj  = mean(c_h), priced_adj  = mean(c_p),
            .groups = "drop") %>%
  left_join(bkup_focal_ru, by = "focal") %>%
  left_join(league_rush %>%
              select(focal, faced_grun = grun_25, faced_mtf = mtf_25),
            by = "focal") %>%
  left_join(faced_adj_lg_ru, by = "focal") %>%
  mutate(d_grun    = healthy_grun - faced_grun,
         d_av_grun = priced_grun  - faced_grun,
         d_mtf     = healthy_mtf  - faced_mtf,
         d_av_mtf  = priced_mtf   - faced_mtf,
         d_adj     = healthy_adj  - adj_25_m,
         d_av_adj  = priced_adj   - adj_25_m)

d_cols_ru <- c("d_grun", "d_av_grun", "d_mtf", "d_av_mtf",
               "d_adj", "d_av_adj")
if (any(is.na(sweep_ru %>% select(dplyr::all_of(d_cols_ru))))) {
  print(sweep_ru %>%
          filter(dplyr::if_any(dplyr::all_of(d_cols_ru), is.na)))
  stop("sweep produced NA -- report")
}

cat("\n== LEAGUE-WIDE -- once you price in missed games, did 2026",
    "backfields really get scarier? ==\n")
league_mean_ru <- sweep_ru %>%
  summarise(across(c(healthy_grun, priced_grun, faced_grun,
                     healthy_mtf, priced_mtf, faced_mtf,
                     healthy_adj, priced_adj, adj_25_m),
                   mean))
print(tibble(
  lens          = c("run grade", "tackle-breaking", "adjusted"),
  `healthy jump` = round(c(league_mean_ru$healthy_grun -
                             league_mean_ru$faced_grun,
                           league_mean_ru$healthy_mtf -
                             league_mean_ru$faced_mtf,
                           league_mean_ru$healthy_adj -
                             league_mean_ru$adj_25_m), 3),
  `real jump`    = round(c(league_mean_ru$priced_grun -
                             league_mean_ru$faced_grun,
                           league_mean_ru$priced_mtf -
                             league_mean_ru$faced_mtf,
                           league_mean_ru$priced_adj -
                             league_mean_ru$adj_25_m), 3)))

cat("\n--- biggest SOFTENINGS once priced (adjusted lens; the",
    "healthy jump was partly a mirage) ---\n")
print(sweep_ru %>%
        mutate(swing = d_av_adj - d_adj) %>%
        arrange(swing) %>%
        transmute(focal, "healthy jump" = round(d_adj, 3),
                  "real jump" = round(d_av_adj, 3),
                  "priced away" = round(-swing, 3),
                  "backup gms" = round(backup_gms, 1)) %>%
        head(10))
cat("\n--- biggest HARDENINGS once priced (the jump was real",
    "or understated) ---\n")
print(sweep_ru %>%
        mutate(swing = d_av_adj - d_adj) %>%
        arrange(desc(swing)) %>%
        transmute(focal, "healthy jump" = round(d_adj, 3),
                  "real jump" = round(d_av_adj, 3),
                  "priced up" = round(swing, 3),
                  "backup gms" = round(backup_gms, 1)) %>%
        head(5))

# -- slate difficulty rank board (adjusted lens): does pricing
#    move NE? ----------------------------------------------------
focal_slate_ru <- sweep_ru %>%
  transmute(focal,
            backup_gms = round(backup_gms, 1),
            healthy = healthy_adj, priced = priced_adj) %>%
  mutate(rk_healthy = min_rank(desc(healthy)),
         rk_priced  = min_rank(desc(priced)),
         chg = rk_priced - rk_healthy,
         ne  = dplyr::if_else(focal == "NE", "<-- NE", "")) %>%
  arrange(rk_priced)

cat("\n--- who faces the hardest 2026 runner diet? (adjusted",
    "lens; healthy rank vs priced rank) ---\n")
print(focal_slate_ru %>%
        transmute(focal, "backup gms" = backup_gms,
                  "healthy lvl" = round(healthy, 3),
                  "priced lvl"  = round(priced, 3),
                  "rk healthy" = rk_healthy, "rk priced" = rk_priced,
                  "rank chg" = chg, ne),
      n = 32)

# ------------------------------------------------------------
# 7. BOARDS
# ------------------------------------------------------------

# -- board 1: the most fragile 2026 committees ------------------
team_exposure_ru <- memb_2026_ru %>%
  group_by(team) %>%
  summarise(exp_lost = round(sum((1 - avail) * 17), 1),
            soft = roster_name[which.min(avail)],
            soft_avail = min(avail),
            .groups = "drop") %>%
  mutate(ne_opp = dplyr::if_else(team %in% sched_2026,
                                 "<-- on NE slate", "")) %>%
  arrange(desc(exp_lost))

cat("\n-- board 1: which 2026 backfields are most exposed to",
    "missed games? (both committee spots, expected games lost) --\n")
print(team_exposure_ru %>%
        transmute(tm = team, "exp committee gms lost" = exp_lost,
                  "soft spot" = soft,
                  "his kept rate" = round(soft_avail, 3),
                  "NE opp" = ne_opp),
      n = 32)

# -- board 2: established backs with the worst durability -------
board_job_ru <- memb_2026_ru %>%
  filter(avail_src == "starter_history") %>%
  arrange(avail) %>%
  transmute(tm = team, back = roster_name,
            "exp misses"  = round((1 - avail) * 17, 1),
            "job yrs"     = job_yrs,
            "lvl (adj)"   = round(V_c3, 3),
            "fill lvl"    = round(repl_level_ru$repl_adj, 3),
            "hit (adj)"   = round((1 - avail) *
                                    (V_c3 - repl_level_ru$repl_adj), 3),
            "hit (grun)"  = round((1 - avail) *
                                    (gf - repl_level_ru$repl_grun), 3),
            "hit (mtf)"   = round((1 - avail) *
                                    (mf - repl_level_ru$repl_mtf), 3))

cat("\n-- board 2: established committee backs with the worst",
    "durability (hit = what their absence costs, per lens) --\n")
print(board_job_ru, n = 15)

# -- board 3: everyone priced at the backup line ----------------
board_bkup_ru <- memb_2026_ru %>%
  filter(avail_src == "backup_line") %>%
  arrange(desc(uw)) %>%
  transmute(tm = team, back = roster_name,
            "entry yr" = dplyr::coalesce(entry_year, NA_integer_),
            "priced at" = round(avail, 3),
            "lvl (adj)" = round(V_c3, 3))

cat("\n-- board 3: committee members with no job history -- priced",
    " at the backup line (", nrow(board_bkup_ru), " of ",
    nrow(memb_2026_ru), ") --\n", sep = "")
print(board_bkup_ru, n = Inf)

# -- board 4: true 2026 rookies in the committees ---------------
board_rook_ru <- memb_2026_ru %>%
  filter(avail_src == "rookie_line") %>%
  arrange(desc(uw)) %>%
  transmute(tm = team, back = roster_name,
            "priced at"  = round(avail, 3),
            "exp misses" = round((1 - avail) * 17, 1),
            "lvl (adj)"  = round(V_c3, 3))

cat("\n-- board 4: true 2026 rookies in 2026 committees --",
    nrow(board_rook_ru), "of", nrow(memb_2026_ru),
    "members --\n")
print(board_rook_ru, n = Inf)
cat("note: on this unit the machine DOES see rookies -- the ledger\n")
cat("keeps status == 'rookie' and the league rotation carries\n")
cat("entry_year == 2026 bodies at zero usage. No blind spot here.\n")
cat("one divergence on file: the NE rebuild's ledger proposed LV\n")
cat("rookie Washington; the league carry-count law picked Collier.\n")
cat("NE-side tables use the NE build (walled vs cmp_rush_slate).\n")

# ------------------------------------------------------------
# 8. THE TEAM-SPLIT VISUAL -- raw vs adjusted, per 2026 opponent.
#    The pass-rush adjusted edition's per-team table (the one Andy
#    stamped by screenshot, 2026-08-20), ported to this unit's
#    laws: attempts-weighted everywhere, single HB pool. The
#    adjusted currency lives on RUN GRADE only, so run grade gets
#    the raw + adjusted table and tackle-breaking gets a raw-only
#    companion. Faced side = the canon faced frame itself grouped
#    by team (the stamped earned-else-prior fills are already
#    inside it); 2026 side = the walled committee members, same
#    men, same attempt weights. The injury-priced family stays
#    OUT of these tables -- it has its own section above.
# ------------------------------------------------------------

suppressPackageStartupMessages(library(gt))

# -- per-team 2026: the walled members, attempts law ------------
team26_split_ru <- memb_ne_ru %>%
  mutate(prior_c3 = is.na(blend2(a25, a24, w25a))) %>%
  group_by(team_name) %>%
  summarise(raw26_grun = weighted.mean(grun_f, w = usage_w),
            raw26_mtf  = weighted.mean(mtf_f,  w = usage_w),
            adj26      = weighted.mean(c3_f,   w = usage_w),
            interp_adj = sum(usage_w[prior_c3]) / sum(usage_w),
            interp_raw = sum(usage_w[prior_used]) / sum(usage_w),
            .groups = "drop")

# -- per-team faced 2025: the canon faced cells, grouped --------
faced_team_ru <- faced_adj_ne_ru %>%
  group_by(team) %>%
  summarise(att        = sum(g_att),
            raw25_grun = weighted.mean(g25,   w = g_att),
            raw25_mtf  = weighted.mean(m25,   w = g_att),
            adj25      = weighted.mean(c3_25, w = g_att),
            fill_pct   = sum(g_att[fill]) / sum(g_att),
            .groups = "drop")

# walls: grid-averaged over the 17-game slate, the 2026 columns
# must equal the canon slate scalars; the faced teams' attempts
# must rebuild the faced total exactly.
chk_grid_ru <- tibble(team_name = sched_2026) %>%
  left_join(team26_split_ru, by = "team_name",
            relationship = "many-to-many")
stopifnot(abs(mean(chk_grid_ru$raw26_grun) -
                cmp_rush_slate$grun_26) < 1e-8,
          abs(mean(chk_grid_ru$raw26_mtf) -
                cmp_rush_slate$mtf_26) < 1e-8,
          abs(mean(chk_grid_ru$adj26) -
                slate_ne_healthy_c3_ru) < 1e-8,
          abs(sum(faced_team_ru$att) -
                sum(faced_ne_cells_ru$g_att)) < 1e-8)
cat("\n[wall] team-split 2026 columns rebuild the slate scalars",
    "and the faced attempt total (1e-8)\n")

cmp_team_ru <- team26_split_ru %>%
  left_join(faced_team_ru, by = c("team_name" = "team")) %>%
  mutate(d_grun = raw26_grun - raw25_grun,
         d_adj  = adj26      - adj25,
         d_mtf  = raw26_mtf  - raw25_mtf)

ord_adj_ru <- cmp_team_ru %>%
  arrange(desc(dplyr::coalesce(d_adj, -Inf))) %>% pull(team_name)
ord_mtf_ru <- cmp_team_ru %>%
  arrange(desc(dplyr::coalesce(d_mtf, -Inf))) %>% pull(team_name)

cat("\n-- the team split: what does each 2026 opponent's backfield",
    "look like, vs what NE faced from it in 2025? --\n")
print(cmp_team_ru %>%
        select(team_name, raw25_grun, raw26_grun, d_grun,
               adj25, adj26, d_adj, raw25_mtf, raw26_mtf, d_mtf,
               fill_pct, interp_adj, interp_raw) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))), n = Inf)

# -- table 1 of 2: TACKLE-BREAKING lens (raw only -- no adjusted
#    variant exists for this metric). Printed FIRST on purpose:
#    the viewer shows one gt at a time and each print replaces
#    the last, so the run-grade table (the one WITH adjusted)
#    prints last, below. ----------------------------------------
gt_mtf_team_ru <- cmp_team_ru %>%
  mutate(team_name = factor(team_name, levels = ord_mtf_ru)) %>%
  arrange(team_name) %>%
  select(team_name, raw25_mtf, raw26_mtf, d_mtf,
         fill_pct, interp_raw) %>%
  gt(rowname_col = "team_name") %>%
  tab_spanner(label = "Raw",
              columns = c(raw25_mtf, raw26_mtf, d_mtf)) %>%
  cols_label(raw25_mtf = "'25", raw26_mtf = "'26",
             d_mtf = "\u0394",
             fill_pct = "fill %", interp_raw = "interp %") %>%
  fmt_percent(columns = c(raw25_mtf, raw26_mtf,
                          fill_pct, interp_raw), decimals = 0) %>%
  fmt_percent(columns = d_mtf, decimals = 0, force_sign = TRUE) %>%
  sub_missing(missing_text = "--") %>%
  data_color(columns = d_mtf,
             fn = scales::col_numeric(
               c("#6baed6", "#f7f7f7", "#C60C30"),
               domain = c(-0.3, 0.3), na.color = "#f7f7f7"),
             autocolor_text = TRUE) %>%
  tab_header(
    title = "Opposing runners, per team \u2014 tackle-breaking",
    subtitle = "'--' = not faced in 2025 | teams sorted by raw \u0394, hardest first | attempts-weighted | raw only \u2014 the adjusted currency lives on run grade") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold")
print(gt_mtf_team_ru)

# -- table 2 of 2: RUN-GRADE lens (raw + adjusted) -- prints
#    LAST so the viewer ends on the table with adjusted --------
gt_grun_team_ru <- cmp_team_ru %>%
  mutate(team_name = factor(team_name, levels = ord_adj_ru)) %>%
  arrange(team_name) %>%
  select(team_name, raw25_grun, raw26_grun, d_grun,
         adj25, adj26, d_adj, fill_pct, interp_adj) %>%
  gt(rowname_col = "team_name") %>%
  tab_spanner(label = "Raw",
              columns = c(raw25_grun, raw26_grun, d_grun)) %>%
  tab_spanner(label = "Adjusted (same-slate)",
              columns = c(adj25, adj26, d_adj)) %>%
  cols_label(raw25_grun = "'25", raw26_grun = "'26",
             d_grun = "\u0394",
             adj25 = "'25", adj26 = "'26", d_adj = "\u0394",
             fill_pct = "fill %", interp_adj = "interp %") %>%
  fmt_percent(columns = c(raw25_grun, raw26_grun, adj25, adj26,
                          fill_pct, interp_adj), decimals = 0) %>%
  fmt_percent(columns = c(d_grun, d_adj), decimals = 0,
              force_sign = TRUE) %>%
  sub_missing(missing_text = "--") %>%
  data_color(columns = c(d_grun, d_adj),
             fn = scales::col_numeric(
               c("#6baed6", "#f7f7f7", "#C60C30"),
               domain = c(-0.3, 0.3), na.color = "#f7f7f7"),
             autocolor_text = TRUE) %>%
  tab_header(
    title = "Opposing runners, per team \u2014 raw vs adjusted",
    subtitle = "'--' = not faced in 2025 | teams sorted by adjusted \u0394, hardest first | same committee members and attempt weights in both lenses | run-grade lens only") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold")
print(gt_grun_team_ru)

cat("\nnote: the viewer shows ONE gt table at a time -- each new",
    "table replaces the last.\n  both tables stay in session;",
    "bring either back with one line:\n",
    '  print(gt_grun_team_ru)   # run grade: raw + adjusted\n',
    '  print(gt_mtf_team_ru)    # tackle-breaking: raw only\n',
    sep = "")

# Checkpoint: gtsave("rush_team_split_grun.png", gt_grun_team_ru,
#                    vwidth = 900, vheight = 640)

# ------------------------------------------------------------
# CHECKPOINT -- pending stamps and next steps
# ------------------------------------------------------------
# - team-split visual (raw vs adjusted, per 2026 opponent) added
#   2026-08-20 as section 8 -- the stamped pass-rush table,
#   ported to this unit's laws. The slate-level GT and the
#   canon-style final evaluation still await their stamps.
# - knob stamp pending: AVAIL_ATT_MIN_RU = 4 attempts (unit
#   adaptation 1; agrees with canon X_QUAL = X_RB = 4).
# - unit law carried: attempts-weighted everywhere (adaptation 4)
#   -- the OL equal-game-weights law did not transfer.
# - canon has no run-side cmp_adj answer table for rushing; the
#   adjusted lens here is layer-built (same blend2 + prior shape).
#   A canon-style league_rush final evaluation (adjusted family +
#   injury-priced family + GT spanners + slate plot) awaits its
#   own stamp.
# - remaining units after this one: RUN DEFENSE, SECONDARY,
#   RECEIVING (split-aware; receiving two-metric grade + yprr).
# ------------------------------------------------------------

cat("\n== AVAILABILITY LAYER -- OPPOSING RUNNERS: done ==\n")
