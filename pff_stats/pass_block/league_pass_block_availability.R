# ============================================================
# PASS-BLOCK SLATE -- AVAILABILITY LAYER, v3
# Andy directive 2026-08-19: "ONLY THING - INJURIES. 2026 SLATE
# DOESN'T FACTOR INJURIES THAT WILL INEVITABLY HAPPEN." Prices
# EXPECTED injuries into the 2026 side so both sides of the slate
# carry the same health expectation.
#
# v3 PRINT LAW (Andy 2026-08-19, "DO FUCKING BETTER"):
#   1. Printed column names are SHORT ENGLISH PHRASES with spaces
#      ("faced '25", "26 priced", "backup gms"). No suffix algebra
#      (no _r/_a, no av_), no gaming slang (frag is dead). Object
#      internals keep their contract names; renames happen only at
#      print time.
#   2. ONE LENS PER TABLE. Box-score lens (raw season percentiles)
#      and adjusted lens (same-slate skill) print as separate small
#      tables, never interleaved in one row.
#   3. Every printed table opens with the question it answers.
#
# THE PRICING LAW (unchanged):
#   value_26_avail = avail * value_26 + (1 - avail) * repl(slot)
#   avail = games-played rate 0-1, judged ONLY on seasons the player
#           HELD a starting job (season-modal at a slot, REG weeks)
#           -- Andy 2026-08-19 board-split stamp. Players with zero
#           starter seasons get the observed FIRST-YEAR-STARTER rate
#           for THEIR population (Andy 2026-08-19 ruling: "career
#           backups get priced at the backup thing; rookies get the
#           rookie line"), measured from 2024-25 first-year starters
#           at that slot:
#             backup_line = in the league but never held a job, and
#               phantom slots (an open job is usually won by a backup)
#             rookie_line = no 2023-25 rows at all -- true rookies
#           (2023 first-year starters are excluded from the
#           measurement -- the window cannot tell their population.
#           A slot cell with no evidence falls back to that
#           population's unit line, and says so in the evidence
#           column.)
#   repl  = empirical 2025 BACKUP-GAME level by slot, per lens, REG
#           weeks, signed fill law (box: earned else slot prior else
#           unit scalar; adj: game-slot c3 else slot c3 prior).
#   FACED-2025 IS UNTOUCHED: its injuries are already realized in
#           the membership. Discounting it would double-count.
#   FIREWALL INTACT: box blends with the box replacement level, adj
#           with the adj replacement level; the avail family stays
#           its own always-labeled family.
#   NOT SEEN: current 2026 injuries beyond roster membership (the
#           roster-change protocol drops known season-enders via
#           load_rosters; PUP/short-term IR not priced here).
# KNOBS still PROPOSED -- Kimi, UNSIGNED (Andy has not spoken to
# their values): AVAIL_SEASONS, AVAIL_SNAP_MIN, AVAIL_DENOM.
#
# SOURCE ORDER: the full adjusted stack (pff_pass_block_AWS, canon
# NE OL file, league c3 file, league tail, league_pass_block_final_
# evaluation) then this file. Needs rot26_full_ol + slate_view_pb
# (built at source time when the league tail is present).
# ============================================================

needed_av <- c("all_pass_block_summary", "ol_season_pctl",
               "rookie_prior", "pblk_c3_pctl", "c3_rookie_prior_ol",
               "slots_full", "rot26_full_ol", "opp_map_ol", "pff32_ol",
               "sched_2026", "cmp_adj_pb", "proj_c3_pb", "in_season",
               "blend2", "ol_pos_levels", "slate_view_pb")
missing_av <- needed_av[!vapply(needed_av, exists, logical(1))]
if (length(missing_av)) stop("missing session objects: ",
                             paste(missing_av, collapse = ", "),
                             " -- see SOURCE ORDER in header")

req_cols_av <- list(
  slots_full = c("team_name", "slot", "roster_name", "player_id",
                 "status", "phantom", "tps_grade_bl", "tps_grade",
                 "pr_tps"),
  rot26_full_ol = c("team", "slot", "gf_raw", "gf_adj"),
  opp_map_ol = c("focal", "opp"),
  ol_season_pctl = c("player_id", "det_position", "tps_grade"),
  proj_c3_pb = c("team_name", "det_position", "roster_name",
                 "player_id", "status", "tps_f", "c3_f",
                 "prior_used_c3"),
  cmp_adj_pb = c("det_position", "raw_25", "raw_26", "d_raw",
                 "adj_25", "adj_26", "d_adj"))
for (nm_av in names(req_cols_av)) {
  miss_av <- setdiff(req_cols_av[[nm_av]], names(get(nm_av)))
  if (length(miss_av)) {
    cat("\n", nm_av, "-- columns present:\n")
    print(names(get(nm_av)))
    stop(nm_av, " is missing columns: ", paste(miss_av, collapse = ", "),
         " -- report, do not proceed")
  }
}

# ------------------------------------------------------------
# 0. KNOBS (PROPOSED -- Kimi, UNSIGNED). Printed at source.
# ------------------------------------------------------------
AVAIL_SEASONS  <- 2023:2025   # availability history window
AVAIL_SNAP_MIN <- 8           # pb snaps that week to count as played
AVAIL_DENOM    <- 17          # REG games per season

cat("\n== AVAILABILITY LAYER v3 == knobs: seasons",
    paste(range(AVAIL_SEASONS), collapse = "-"), "| played = pb snaps >=",
    AVAIL_SNAP_MIN, "| denominator", AVAIL_DENOM, "REG games ==\n")

# ------------------------------------------------------------
# 1. avail per player -- REG games played / AVAIL_DENOM. Two rates
#    (Andy 2026-08-19): avail_all = every season with data;
#    avail_job = only seasons he HELD a starting job (season-modal
#    at a slot that year). The price uses avail_job; players with
#    none fall to the slot starter mean in section 2.
#    Feed = all_pass_block_summary (UNFILTERED; the c3 game frame's
#    16-snap floor would hide real availability).
# ------------------------------------------------------------

# who HELD the job: season-modal starter per team x slot, REG weeks
starters_all_av <- all_pass_block_summary %>%
  filter(season %in% AVAIL_SEASONS, week <= 18,
         det_position %in% ol_pos_levels) %>%
  group_by(season, team_name, det_position, player_id) %>%
  summarise(sn = sum(snap_counts_pass_block, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(season, team_name, det_position) %>%
  slice_max(sn, n = 1, with_ties = FALSE) %>%
  ungroup()

starter_map_av <- starters_all_av %>%
  distinct(season, player_id) %>%          # trades: one row either way
  mutate(had_job = TRUE)

avail_pb <- all_pass_block_summary %>%
  filter(season %in% AVAIL_SEASONS, week <= 18,
         snap_counts_pass_block >= AVAIL_SNAP_MIN) %>%
  distinct(player_id, season, week) %>%
  count(player_id, season, name = "g_p") %>%
  mutate(avail_s = pmin(g_p / AVAIL_DENOM, 1)) %>%
  left_join(starter_map_av, by = c("player_id", "season")) %>%
  mutate(had_job = !is.na(had_job)) %>%
  group_by(player_id) %>%
  summarise(yrs       = dplyr::n(),
            job_yrs   = sum(had_job),
            avail_all = round(mean(avail_s), 3),
            avail_job = if (any(had_job))
              round(mean(avail_s[had_job]), 3) else NA_real_,
            .groups = "drop")

cat("\n--- durability among players who HELD a job at least once ---\n")
print(quantile(avail_pb$avail_job, na.rm = TRUE,
               probs = c(0, .1, .25, .5, .75, .9, 1)) %>% round(3))

# ------------------------------------------------------------
# 2. slot-level ESTABLISHED-STARTER availability -- the incumbent
#    benchmark (2025 season-modal starters, REG weeks). Printed for
#    reference only; it is NOT the first-time-starter fill anymore
#    (Andy 2026-08-19 ruling) -- the fills are the lines in 2b.
# ------------------------------------------------------------

g25 <- all_pass_block_summary %>%
  filter(season == 2025, week <= 18,
         snap_counts_pass_block >= AVAIL_SNAP_MIN) %>%
  distinct(player_id, week) %>%
  count(player_id, name = "g_p25")

slot_starter_avail_pb <- starters_all_av %>%
  filter(season == 2025) %>%
  distinct(player_id, .keep_all = TRUE) %>%   # kicked-inside seasons: one row
  left_join(g25, by = "player_id") %>%
  mutate(avail_s = pmin(g_p25 / AVAIL_DENOM, 1)) %>%
  group_by(det_position) %>%
  summarise(slot_starter_avail = round(mean(avail_s, na.rm = TRUE), 3),
            n_starters = dplyr::n(),
            .groups = "drop")

cat("\n--- how durable is an ESTABLISHED starter at each slot?",
    "(benchmark for context -- not the first-year fill) ---\n")
print(slot_starter_avail_pb %>%
        rename(slot = det_position, "starter avail" = slot_starter_avail,
               starters = n_starters))
if (any(is.na(slot_starter_avail_pb$slot_starter_avail))) {
  stop("slot starter availability NA -- report")
}

# ------------------------------------------------------------
# 2b. THE FIRST-YEAR LINES (Andy 2026-08-19: "career backups get
#     priced at the backup thing; rookies get the rookie line").
#     Measured from 2024-25 first-year starters: when a guy holds a
#     starting job for the FIRST time, what share of that season
#     does he actually keep -- split by population, because a career
#     backup earning a job and a rookie earning a job are different
#     bets. 2023 first-year starters are excluded: the window
#     cannot tell their population.
# ------------------------------------------------------------

seen_2023_av <- all_pass_block_summary %>%
  filter(season == 2023, week <= 18) %>%
  distinct(player_id) %>% pull(player_id)
seen_2324_av <- all_pass_block_summary %>%
  filter(season %in% 2023:2024, week <= 18) %>%
  distinct(player_id) %>% pull(player_id)
nfl_seen_av <- all_pass_block_summary %>%
  filter(season %in% AVAIL_SEASONS, week <= 18) %>%
  distinct(player_id) %>% pull(player_id)

starter_seasons_av <- starters_all_av %>% distinct(player_id, season)

first_year_av <- bind_rows(
  starters_all_av %>%
    filter(season == 2024) %>%
    anti_join(starter_seasons_av %>% filter(season == 2023),
              by = "player_id"),
  starters_all_av %>%
    filter(season == 2025) %>%
    anti_join(starter_seasons_av %>% filter(season %in% 2023:2024),
              by = "player_id")) %>%
  distinct(player_id, season, .keep_all = TRUE) %>%
  mutate(pop = case_when(
    season == 2024 & player_id %in% seen_2023_av ~ "backup",
    season == 2025 & player_id %in% seen_2324_av ~ "backup",
    TRUE ~ "rookie")) %>%
  left_join(all_pass_block_summary %>%
              filter(season %in% 2024:2025, week <= 18,
                     snap_counts_pass_block >= AVAIL_SNAP_MIN) %>%
              distinct(player_id, season, week) %>%
              count(player_id, season, name = "g_p"),
            by = c("player_id", "season")) %>%
  mutate(g_p  = dplyr::coalesce(g_p, 0L),
         rate = pmin(g_p / AVAIL_DENOM, 1))

first_year_cells_pb <- first_year_av %>%
  group_by(det_position, pop) %>%
  summarise(n = dplyr::n(), line = round(mean(rate), 3),
            .groups = "drop")

first_year_unit_pb <- first_year_av %>%
  group_by(pop) %>%
  summarise(n_unit = dplyr::n(), unit_line = round(mean(rate), 3),
            .groups = "drop")

first_year_lines_pb <- tidyr::expand_grid(
  det_position = ol_pos_levels, pop = c("backup", "rookie")) %>%
  left_join(first_year_cells_pb, by = c("det_position", "pop")) %>%
  left_join(first_year_unit_pb, by = "pop") %>%
  mutate(line = dplyr::coalesce(line, unit_line),
         evidence = if_else(is.na(n),
                            paste0("no slot evidence -- ", pop,
                                   " unit line (n=", n_unit, ")"),
                            paste0(n, " first-year starters"))) %>%
  select(det_position, pop, line, evidence)
if (any(is.na(first_year_lines_pb$line))) {
  print(first_year_lines_pb)
  stop("first-year lines NA -- report")
}

cat("\n--- the first-year lines: when a NEW starter holds a job for",
    "the first time, how much of that season does he keep? ---\n")
print(first_year_lines_pb %>%
        transmute(slot = det_position, population = pop,
                  "kept rate" = line, evidence))

lines_wide_av <- first_year_lines_pb %>%
  select(det_position, pop, line) %>%
  tidyr::pivot_wider(names_from = pop, values_from = line,
                     names_prefix = "line_")

# ------------------------------------------------------------
# 3. repl(slot) -- empirical 2025 backup-game level, REG, signed
#    fill law. This is "who you actually get when the starter is
#    out", measured from real 2025 backup starts.
# ------------------------------------------------------------

starters_25_reg_av <- all_pass_block_summary %>%
  filter(season == 2025, week <= 18, det_position %in% ol_pos_levels) %>%
  group_by(team_name, det_position, player_id) %>%
  summarise(sn = sum(snap_counts_pass_block, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(team_name, det_position) %>%
  slice_max(sn, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(team_name, det_position, starter_id = player_id)

repl_games_av <- all_pass_block_summary %>%
  filter(season == 2025, week <= 18, det_position %in% ol_pos_levels) %>%
  group_by(team_name, week, det_position) %>%
  slice_max(snap_counts_pass_block, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(starters_25_reg_av, by = c("team_name", "det_position")) %>%
  filter(player_id != starter_id) %>%
  left_join(ol_season_pctl %>%               # canon: single-season frame,
              select(player_id, det_position, raw_g = tps_grade),
            by = c("player_id", "det_position")) %>%
  left_join(pblk_c3_pctl %>% filter(season == 2025),
            by = c("player_id", "det_position" = "band")) %>%
  mutate(
    adj_val = case_when(!is.na(c3_pctl) ~ c3_pctl,
                        !is.na(player_id) ~ c3_rookie_prior_ol$pr_c3[
                          match(det_position, c3_rookie_prior_ol$band)],
                        TRUE ~ NA_real_),
    raw_val = case_when(!is.na(raw_g) ~ raw_g,
                        !is.na(player_id) ~ rookie_prior$pr_tps[
                          match(det_position, rookie_prior$det_position)],
                        TRUE ~ NA_real_))

repl_level_pb <- repl_games_av %>%
  group_by(det_position) %>%
  summarise(n_backup = dplyr::n(),
            repl_raw = round(mean(raw_val, na.rm = TRUE), 3),
            repl_adj = round(mean(adj_val, na.rm = TRUE), 3),
            .groups = "drop")

cat("\n--- when the starter is out, what do you get? (2025 backup",
    "starts, measured) ---\n")
print(repl_level_pb %>%
        rename(slot = det_position, "backup gms seen" = n_backup,
               "fill lvl (box)" = repl_raw, "fill lvl (adj)" = repl_adj))
if (any(is.na(repl_level_pb$repl_raw)) ||
    any(is.na(repl_level_pb$repl_adj))) {
  stop("replacement level NA at a slot -- report")
}

# ------------------------------------------------------------
# 4. slot_value_26_pb -- every projected 2026 starter carrying the
#    final-file value plus the injury-priced value. The gf chain
#    mirrors rot26_full_ol (final file section 7) EXACTLY, walled
#    cell-by-cell below. avail = avail_job when he has starter
#    seasons, else the slot starter mean (board-split stamp).
# ------------------------------------------------------------

prior_unit_tps_av <- mean(rookie_prior$pr_tps, na.rm = TRUE)

slot_value_26_pb_build <- slots_full %>%
  filter(slot %in% ol_pos_levels) %>%
  mutate(gf_raw = dplyr::case_when(
    phantom ~ dplyr::coalesce(pr_tps, prior_unit_tps_av),
    status == "rookie" ~ dplyr::coalesce(tps_grade_bl, pr_tps,
                                         prior_unit_tps_av),
    TRUE ~ dplyr::coalesce(tps_grade_bl, tps_grade, pr_tps,
                           prior_unit_tps_av))) %>%
  left_join(pblk_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, a25 = c3_pctl, qg25a = qual_g),
            by = "player_id") %>%
  left_join(pblk_c3_pctl %>% filter(season == 2024) %>%
              select(player_id, a24 = c3_pctl),
            by = "player_id") %>%
  left_join(c3_rookie_prior_ol %>% select(band, pr_c3),
            by = c("slot" = "band")) %>%
  mutate(w25a = pmin(dplyr::coalesce(qg25a, 0L) / 10, 1),
         gf_adj = dplyr::coalesce(blend2(a25, a24, w25a), pr_c3)) %>%
  left_join(avail_pb %>%
              select(player_id, yrs, job_yrs, avail_all, avail_job),
            by = "player_id") %>%
  mutate(avail_src = dplyr::case_when(
    !is.na(avail_job) ~ "starter_history",
    phantom ~ "backup_line",
    player_id %in% nfl_seen_av ~ "backup_line",
    TRUE ~ "rookie_line")) %>%
  left_join(lines_wide_av, by = c("slot" = "det_position")) %>%
  mutate(avail = dplyr::case_when(
    avail_src == "starter_history" ~ avail_job,
    avail_src == "backup_line" ~ line_backup,
    TRUE ~ line_rookie)) %>%
  left_join(repl_level_pb, by = c("slot" = "det_position")) %>%
  mutate(gf_raw_av = if_else(is.na(gf_raw), NA_real_,
                             round(avail * gf_raw +
                                     (1 - avail) * repl_raw, 4)),
         gf_adj_av = round(avail * gf_adj + (1 - avail) * repl_adj, 4))

chk_rot26 <- slot_value_26_pb_build %>%
  full_join(rot26_full_ol, by = c("team_name" = "team", "slot"),
            suffix = c("", "_fe"))
if (nrow(chk_rot26) != 160 ||
    !isTRUE(all.equal(chk_rot26$gf_raw, chk_rot26$gf_raw_fe,
                      tolerance = 1e-8)) ||
    !isTRUE(all.equal(chk_rot26$gf_adj, chk_rot26$gf_adj_fe,
                      tolerance = 1e-8))) {
  chk_rot26 %>%
    filter(is.na(gf_raw_fe) | is.na(gf_raw) |
             abs(gf_raw - gf_raw_fe) > 1e-8 |
             abs(gf_adj - gf_adj_fe) > 1e-8) %>%
    print()
  stop("slot_value_26_pb drifted from the final-file chain -- report")
}
slot_value_26_pb <- slot_value_26_pb_build
stopifnot(!any(is.na(slot_value_26_pb$avail)))

# ------------------------------------------------------------
# 5. NE slate, injury-priced -- CANON membership off proj_c3_pb.
#    Canon leaves tps_f NA where neither 2025 nor 2024 pctl exists;
#    the layer inherits the holes exactly (walled), never invents.
# ------------------------------------------------------------

ne_frame_av <- proj_c3_pb %>%
  left_join(avail_pb %>% select(player_id, avail_job), by = "player_id") %>%
  mutate(avail_src = dplyr::case_when(
    !is.na(avail_job) ~ "starter_history",
    is.na(player_id) ~ "backup_line",
    status == "rookie" ~ "rookie_line",
    !(player_id %in% nfl_seen_av) ~ "rookie_line",
    TRUE ~ "backup_line")) %>%
  left_join(lines_wide_av, by = "det_position") %>%
  mutate(avail = dplyr::case_when(
    avail_src == "starter_history" ~ avail_job,
    avail_src == "backup_line" ~ line_backup,
    TRUE ~ line_rookie)) %>%
  left_join(repl_level_pb, by = "det_position") %>%
  mutate(raw_f_av = if_else(is.na(tps_f), NA_real_,
                            round(avail * tps_f + (1 - avail) * repl_raw, 4)),
         adj_f_av = round(avail * c3_f + (1 - avail) * repl_adj, 4))

if (!identical(is.na(ne_frame_av$raw_f_av), is.na(ne_frame_av$tps_f))) {
  ne_frame_av %>%
    filter(is.na(raw_f_av) != is.na(tps_f)) %>%
    select(team_name, det_position, roster_name, status, tps_f) %>%
    print()
  stop("layer invented or dropped a canon NA -- report")
}
if (any(is.na(ne_frame_av$adj_f_av))) stop("adj_f_av NA -- report")

cat("\n--- canon holes: these slate members carry no box-score value",
    "(the adjusted side covers them) ---\n")
print(ne_frame_av %>%
        filter(is.na(raw_f_av)) %>%
        transmute(tm = team_name, slot = det_position,
                  player = roster_name, status, "adj lvl" = c3_f))

slate_rows_ne_av <- tibble::tibble(opp = sched_2026) %>%
  left_join(ne_frame_av, by = c("opp" = "team_name"),
            relationship = "many-to-many")
if (nrow(slate_rows_ne_av) != length(sched_2026) * 5L) {
  stop("NE slate rows broken -- report")
}

# 2026-side means recomputed from the slate rows (walled vs canon
# below); the FACED side is canon itself, pulled from cmp_adj_pb --
# the layer never touches it, so it never rebuilds it.
ne_slate_avail_pb <- slate_rows_ne_av %>%
  group_by(det_position) %>%
  summarise(avail_mean   = round(mean(avail), 3),
            n_raw_priced = sum(!is.na(raw_f_av)),
            backup_gms   = round(sum(1 - avail), 1),
            raw_26_paper = round(mean(tps_f, na.rm = TRUE), 3),
            raw_26_av    = round(mean(raw_f_av, na.rm = TRUE), 3),
            adj_26_paper = round(mean(c3_f), 3),
            adj_26_av    = round(mean(adj_f_av), 3),
            .groups = "drop") %>%
  left_join(cmp_adj_pb %>%
              select(det_position, raw_25, adj_25, d_raw, d_adj,
                     raw_26_c = raw_26, adj_26_c = adj_26),
            by = "det_position") %>%
  mutate(d_raw_av = round(raw_26_av - raw_25, 3),
         d_adj_av = round(adj_26_av - adj_25, 3)) %>%
  arrange(match(det_position, ol_pos_levels))

chk_ne <- ne_slate_avail_pb %>%
  select(det_position, raw_26_paper, adj_26_paper, raw_26_c, adj_26_c)
if (!isTRUE(all.equal(chk_ne$raw_26_paper, chk_ne$raw_26_c,
                      tolerance = 5e-4)) ||
    !isTRUE(all.equal(chk_ne$adj_26_paper, chk_ne$adj_26_c,
                      tolerance = 5e-4))) {
  print(chk_ne)
  stop("NE availability table drifted from canon -- report")
}

cat("\n== NE 2026 OPPONENT PASS PRO -- how hard will each slot be to",
    "rush, once injuries happen? ==\n")
cat("BOX-SCORE LENS (raw season percentiles)\n")
cat("  26 healthy = everyone plays 17 | 26 priced = expected injuries",
    "in\n  real jump = priced - faced '25 | backup gms = expected games",
    "NE catches a\n  backup at that slot | rows w/ box = of 17 slate",
    "rows, how many carry a canon\n  box-score value\n")
print(ne_slate_avail_pb %>%
        transmute(slot = det_position,
                  "faced '25" = raw_25,
                  "26 healthy" = raw_26_paper,
                  "healthy jump" = round(d_raw, 3),
                  "26 priced" = raw_26_av,
                  "real jump" = d_raw_av,
                  "backup gms" = backup_gms,
                  "rows w/ box" = n_raw_priced))

cat("\nADJUSTED LENS (same-slate skill, the evaluation currency)\n")
print(ne_slate_avail_pb %>%
        transmute(slot = det_position,
                  "faced '25" = adj_25,
                  "26 healthy" = adj_26_paper,
                  "healthy jump" = round(d_adj, 3),
                  "26 priced" = adj_26_av,
                  "real jump" = d_adj_av,
                  "backup gms" = backup_gms))

# ------------------------------------------------------------
# 6. LEAGUE SWEEP -- every focal's slate, healthy vs priced.
#    MACHINE-ONLY: machine membership + slate_view_pb's machine
#    faced law (fills at priors), walled against slate_view_pb("NE").
# ------------------------------------------------------------

faced_league_pb <- all_pass_block_summary %>%
  filter(season == 2025, in_season(week),
         det_position %in% ol_pos_levels) %>%
  group_by(def_ssn, week, team_name, det_position) %>%
  slice_max(snap_counts_pass_block, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(ol_season_pctl %>%
              select(player_id, det_position, raw = tps_grade),
            by = c("player_id", "det_position")) %>%
  left_join(rookie_prior %>% select(det_position, prg = pr_tps),
            by = "det_position") %>%
  left_join(pblk_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, band, adj = c3_pctl),
            by = c("player_id", "det_position" = "band")) %>%
  left_join(c3_rookie_prior_ol %>% select(band, pr_c3),
            by = c("det_position" = "band")) %>%
  mutate(raw = dplyr::coalesce(raw, prg, prior_unit_tps_av),
         adj = dplyr::coalesce(adj, pr_c3),
         focal = sub("2025$", "", def_ssn)) %>%
  group_by(focal, det_position) %>%
  summarise(faced_r = round(mean(raw), 3),
            faced_a = round(mean(adj), 3),
            .groups = "drop")

chk_capture <- capture.output(sv_chk <- slate_view_pb("NE"),
                              type = "output")
chk_sv <- faced_league_pb %>%
  filter(focal == "NE") %>%
  left_join(sv_chk %>% select(det_position, raw_25, adj_25),
            by = "det_position")
if (nrow(chk_sv) != 5L ||
    !isTRUE(all.equal(chk_sv$faced_r, chk_sv$raw_25,
                      tolerance = 5e-4)) ||
    !isTRUE(all.equal(chk_sv$faced_a, chk_sv$adj_25,
                      tolerance = 5e-4))) {
  print(chk_sv)
  stop("league faced rebuild drifted from slate_view_pb -- report")
}

sweep_avail_pb <- opp_map_ol %>%
  left_join(slot_value_26_pb %>%
              select(team_name, slot, avail, gf_raw, gf_adj,
                     gf_raw_av, gf_adj_av),
            by = c("opp" = "team_name"),
            relationship = "many-to-many") %>%
  group_by(focal, slot) %>%
  summarise(paper_r = round(mean(gf_raw), 3),
            av_r    = round(mean(gf_raw_av), 3),
            paper_a = round(mean(gf_adj), 3),
            av_a    = round(mean(gf_adj_av), 3),
            .groups = "drop") %>%
  left_join(faced_league_pb, by = c("focal", "slot" = "det_position")) %>%
  mutate(d_r    = round(paper_r - faced_r, 3),
         d_av_r = round(av_r - faced_r, 3),
         d_a    = round(paper_a - faced_a, 3),
         d_av_a = round(av_a - faced_a, 3))
if (any(is.na(sweep_avail_pb$faced_a)) ||
    any(is.na(sweep_avail_pb$av_a))) {
  sweep_avail_pb %>% filter(is.na(faced_a) | is.na(av_a)) %>% print()
  stop("league sweep has NA -- report")
}

cat("\n== HOW MUCH OF THE 2026 SLATE JUMP WAS THE INJURY ASYMMETRY? ==\n")
cat("League mean jump by slot. Positive = opponents block better than\n")
cat("what the rush faced in '25. 'real jump' is what survives once\n")
cat("expected injuries are priced in.\n")
cat("\nADJUSTED LENS\n")
print(sweep_avail_pb %>%
        group_by(slot) %>%
        summarise("healthy jump" = round(mean(d_a), 3),
                  "real jump"    = round(mean(d_av_a), 3),
                  .groups = "drop") %>%
        arrange(match(slot, ol_pos_levels)))
cat("\nBOX-SCORE LENS\n")
print(sweep_avail_pb %>%
        group_by(slot) %>%
        summarise("healthy jump" = round(mean(d_r), 3),
                  "real jump"    = round(mean(d_av_r), 3),
                  .groups = "drop") %>%
        arrange(match(slot, ol_pos_levels)))

cat("\n== WHOSE SLATE MOVES MOST ONCE INJURIES ARE PRICED? ==\n")
cat("(adjusted lens; swing = priced - healthy at that slot.\n")
cat(" Softens first, then the five whose slates get HARDER --\n")
cat(" their opponents are the durable ones.)\n")
cat("\nSOFTENS MOST\n")
print(sweep_avail_pb %>%
        mutate(swing = round(d_av_a - d_a, 3)) %>%
        arrange(swing) %>%
        transmute(tm = focal, slot, "faced '25" = faced_a,
                  healthy = paper_a, priced = av_a, swing) %>%
        head(10))
cat("\nHARDENS MOST\n")
print(sweep_avail_pb %>%
        mutate(swing = round(d_av_a - d_a, 3)) %>%
        arrange(desc(swing)) %>%
        transmute(tm = focal, slot, "faced '25" = faced_a,
                  healthy = paper_a, priced = av_a, swing) %>%
        head(5))

focal_slate_pb <- opp_map_ol %>%
  left_join(slot_value_26_pb %>%
              select(team_name, slot, avail, gf_adj, gf_adj_av),
            by = c("opp" = "team_name"),
            relationship = "many-to-many") %>%
  group_by(focal) %>%
  summarise(backup_gms = round(sum(1 - avail), 1),
            healthy    = round(mean(gf_adj), 3),
            priced     = round(mean(gf_adj_av), 3),
            .groups = "drop") %>%
  mutate(rk_healthy = min_rank(desc(healthy)),
         rk_priced  = min_rank(desc(priced)),
         chg        = rk_priced - rk_healthy,
         ne         = if_else(focal == "NE", "<-- NE", "")) %>%
  arrange(rk_priced)

cat("\n== DOES INJURY-PRICING REORDER WHO HAS THE HARDEST SLATE? ==\n")
cat("(adjusted lens, mean across the five slots. rk 1 = HARDEST slate\n")
cat(" to rush. chg = priced rank - healthy rank; positive = the slate\n")
cat(" got HARDER relative to the league once injuries are priced.\n")
cat(" backup gms = expected opponent-backup games across all 85\n")
cat(" opponent-line slots on the slate.)\n")
print(focal_slate_pb %>%
        select(tm = focal, "backup gms" = backup_gms,
               "hard rk (healthy)" = rk_healthy,
               "hard rk (priced)" = rk_priced, chg, ne), n = 32)

# ------------------------------------------------------------
# 7. PLAYER BOARDS (Andy 2026-08-19 split: judged only on seasons
#    holding the job; first-time starters price at the slot mean)
#    plus the team exposure board.
# ------------------------------------------------------------

cat("\n== BOARD 1 -- STARTERS WHO MISS GAMES ==\n")
cat("(the injury-proneness board. exp misses = expected games missed\n")
cat(" per 17, from starter seasons only. lvl = his value; fill lvl =\n")
cat(" what his team dips to when he is out; hit = what injury-pricing\n")
cat(" costs his slot, per lens. Worst 15.)\n")
board_job_pb <- slot_value_26_pb %>%
  filter(avail_src == "starter_history") %>%
  arrange(avail) %>%
  transmute(tm = team_name, slot, player = roster_name,
            "exp misses" = round((1 - avail) * 17, 1),
            "starter yrs" = job_yrs,
            "lvl (adj)" = gf_adj, "fill lvl" = repl_adj,
            "hit (adj)" = round(gf_adj - gf_adj_av, 3),
            "hit (box)" = round(gf_raw - gf_raw_av, 3))
print(head(board_job_pb, 15))

cat("\n== BOARD 2 -- CAREER BACKUPS NOW STARTING ==\n")
cat("(in the league '23-'25 but never held a starting job -- plus\n")
cat(" phantom slots. Priced at the BACKUP first-year line: what\n")
cat(" backups historically keep of year one as a starter, which\n")
cat(" includes the flame-out risk. 'bench app' = share of weeks they\n")
cat(" appeared at all as a backup: context, NOT an injury flag.)\n")
board_bkup_pb <- slot_value_26_pb %>%
  filter(avail_src == "backup_line") %>%
  arrange(avail_all) %>%
  transmute(tm = team_name, slot, player = roster_name, status,
            "lg yrs" = yrs, "bench app" = avail_all,
            "priced at" = avail, "lvl (adj)" = gf_adj)
print(board_bkup_pb, n = Inf)
cat("career backups / open slots on 2026 lines:", nrow(board_bkup_pb),
    "of 160\n")

cat("\n== BOARD 3 -- TRUE ROOKIES: NO NFL RECORD ==\n")
cat("(no '23-'25 rows at all. Priced at the ROOKIE first-year line:\n")
cat(" what rookies historically keep of year one as a starter.)\n")
board_rook_pb <- slot_value_26_pb %>%
  filter(avail_src == "rookie_line") %>%
  transmute(tm = team_name, slot, player = roster_name,
            "priced at" = avail, "lvl (adj)" = gf_adj)
print(board_rook_pb, n = Inf)
cat("true rookies on 2026 lines:", nrow(board_rook_pb), "of 160\n")

cat("\n== BOARD 4 -- WHICH LINES CAN LEAST AFFORD AN INJURY ==\n")
cat("(team-level exposure: starter games expected lost across the\n")
cat(" five slots, the softest slot on the line, and what a backup\n")
cat(" there looks like. NE's 2026 opponents flagged.)\n")
team_exposure_pb <- slot_value_26_pb %>%
  group_by(team_name) %>%
  summarise("exp starter gms lost" = round(sum((1 - avail) * 17), 1),
            .groups = "drop") %>%
  left_join(slot_value_26_pb %>%
              group_by(team_name) %>%
              slice_min(avail, n = 1, with_ties = FALSE) %>%
              select(team_name, soft = slot, fill_there = repl_adj),
            by = "team_name") %>%
  mutate(ne_opp = if_else(team_name %in% sched_2026,
                          "<-- on NE slate", "")) %>%
  arrange(desc(.data[["exp starter gms lost"]]))
print(team_exposure_pb %>%
        rename(tm = team_name, "soft spot" = soft,
               "fill there" = fill_there, "NE opp" = ne_opp), n = 32)

# ------------------------------------------------------------
# 8. THE TEAM-SPLIT VISUAL -- raw vs adjusted, per 2026 opponent
#    and slot. The pass-rush adjusted edition's per-team table
#    (Andy's stamped screenshot, 2026-08-20), ported to OL law:
#    one member per slot, equal-game means, raw faced earned-only,
#    adjusted faced fills at the slot prior. ONE box lens on this
#    unit (pass-pro grade), so ONE table. Canon holes print '--'
#    in raw '26 -- the adjusted side covers them. The injury-
#    priced family stays OUT of this table -- it has its own
#    section above.
# ------------------------------------------------------------

suppressPackageStartupMessages(library(gt))

# the faced cells come from the final evaluation (same stack --
# it built and walled them); the membership wall below re-proves
# them per team x slot anyway.
for (obj_pb2 in c("faced_c3_pb", "ne_2025_opp_ol_games")) {
  if (!exists(obj_pb2)) {
    stop("needs ", obj_pb2, " in session -- run the full adjusted",
         " stack (SOURCE ORDER): the canon NE OL file and",
         " league_pass_block_final_evaluation.R build it")
  }
}
stopifnot(all(c("team_name", "det_position", "player_id",
                "c3_25", "fill_c3") %in% names(faced_c3_pb)))

# -- per-team-x-slot faced 2025: raw earned means (canon law),
#    adj + fill from the walled faced c3 cells ------------------
faced_team_pb <- faced_c3_pb %>%
  left_join(ol_season_pctl %>%
              select(player_id, det_position, tps_grade),
            by = c("player_id", "det_position")) %>%
  group_by(team_name, det_position) %>%
  summarise(raw25    = dplyr::na_if(mean(tps_grade, na.rm = TRUE),
                                    NaN),
            adj25    = mean(c3_25),
            fill_pct = mean(fill_c3),
            .groups = "drop")

# membership wall: faced cells per team x slot must equal canon's
# faced frame exactly -- the final-evaluation R1 pattern.
chk_mem_pb2 <- ne_2025_opp_ol_games %>%
  count(team_name, det_position, name = "n_canon") %>%
  full_join(faced_c3_pb %>%
              count(team_name, det_position, name = "n_mine"),
            by = c("team_name", "det_position")) %>%
  mutate(same = !is.na(n_canon) & !is.na(n_mine) &
           n_canon == n_mine)
if (!all(chk_mem_pb2$same)) {
  cat("\n--- team-split faced membership mismatch (canon vs",
      "faced_c3_pb) ---\n")
  print(chk_mem_pb2 %>% filter(!same), n = Inf)
  stop("team-split faced membership != canon -- report, do not",
       " proceed")
}

# -- per-team-x-slot 2026: canon members, holes inherited -------
team26_split_pb <- proj_c3_pb %>%
  transmute(team_name, det_position,
            raw26  = tps_f,
            adj26  = c3_f,
            interp = as.numeric(prior_used_c3))

cmp_team_pb <- team26_split_pb %>%
  left_join(faced_team_pb, by = c("team_name", "det_position")) %>%
  mutate(d_raw = raw26 - raw25,
         d_adj = adj26 - adj25)

team_ord_pb <- cmp_team_pb %>%
  group_by(team_name) %>%
  summarise(o = dplyr::na_if(mean(d_adj, na.rm = TRUE), NaN),
            .groups = "drop") %>%
  arrange(desc(dplyr::coalesce(o, -Inf))) %>% pull(team_name)

cmp_team_pb <- cmp_team_pb %>%
  mutate(team_name = factor(team_name, levels = team_ord_pb)) %>%
  arrange(team_name, match(det_position, ol_pos_levels))

cat("\n-- the team split: what does each 2026 opponent's pass pro",
    "look like, vs what NE faced from it in 2025? --\n")
print(cmp_team_pb %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))),
      n = Inf)

# -- the table: PASS-PRO GRADE lens (raw + adjusted) ------------
gt_pb_team_pb <- cmp_team_pb %>%
  select(team_name, det_position, raw25, raw26, d_raw,
         adj25, adj26, d_adj, fill_pct, interp) %>%
  gt(groupname_col = "team_name") %>%
  tab_spanner(label = "Raw",
              columns = c(raw25, raw26, d_raw)) %>%
  tab_spanner(label = "Adjusted (same-slate)",
              columns = c(adj25, adj26, d_adj)) %>%
  cols_label(det_position = "",
             raw25 = "'25", raw26 = "'26", d_raw = "\u0394",
             adj25 = "'25", adj26 = "'26", d_adj = "\u0394",
             fill_pct = "fill %", interp = "interp %") %>%
  fmt_percent(columns = c(raw25, raw26, adj25, adj26,
                          fill_pct, interp), decimals = 0) %>%
  fmt_percent(columns = c(d_raw, d_adj), decimals = 0,
              force_sign = TRUE) %>%
  sub_missing(missing_text = "--") %>%
  data_color(columns = c(d_raw, d_adj),
             fn = scales::col_numeric(
               c("#6baed6", "#f7f7f7", "#C60C30"),
               domain = c(-0.3, 0.3), na.color = "#f7f7f7"),
             autocolor_text = TRUE) %>%
  tab_header(
    title = "Opposing pass pro, per team \u2014 raw vs adjusted",
    subtitle = "'--' = not faced in 2025 ('25 side) or canon hole (raw '26) | teams sorted by adjusted \u0394, hardest first | one member per slot, equal-game law | pass-pro grade lens only") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold",
              row_group.font.weight = "bold")
print(gt_pb_team_pb)

# Checkpoint: gtsave("pass_block_team_split.png", gt_pb_team_pb,
#                    vwidth = 900, vheight = 900)

# ------------------------------------------------------------
# Checkpoint (when Andy stamps the fold-in): the priced columns
# become a labeled third family inside
# league_pass_block_final_evaluation.R -- cmp_adj_pb gains priced
# columns, the GT gains an "Injury-priced" spanner next to "Raw"
# and "Adjusted (same-slate)", and plot_pblk_slate_adj draws the
# priced point as a triangle between healthy and faced. Until then
# this file stays the priced family's home.
# (the team-split visual landed 2026-08-20 as section 8 above.)
# ------------------------------------------------------------