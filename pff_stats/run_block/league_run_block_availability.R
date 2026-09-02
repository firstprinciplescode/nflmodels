# ============================================================
# RUN-BLOCK SLATE -- AVAILABILITY LAYER, v1
# Andy directive 2026-08-19: "ONLY THING - INJURIES" (OL stamp),
# extended to this unit 2026-08-20: "let's generate those run
# blocking whatever." Prices EXPECTED injuries into the 2026 side
# so both sides of the slate carry the same health expectation.
#
# WHAT CARRIES THE OL STAMP (Andy 2026-08-19, "THESE ASSUMPTIONS ARE
# ALL ACCEPTABLE"):
#   - the pricing law, the print law, the faced-side-untouched law,
#     the firewall, the first-year-lines fill, the population split
#   - knob VALUES: window 2023-2025, played = >= 8 unit snaps that
#     week, denominator 17 REG games
#   - the job rule: season-modal starter per team x slot, REG weeks
#   - repl = empirical 2025 backup-game level by slot, EQUAL GAME
#     WEIGHTS (snap weighting was the pass-rush adaptation only)
#
# UNIT ADAPTATIONS (Kimi, flagged -- Andy can veto):
#   1. "played" = >= 8 RUN-BLOCK snaps that week (unit snap type;
#      the pass-block layer used pass-block snaps, the pass-rush
#      layer TPS rush snaps). Agrees with the c3 canon file's
#      unsigned X_QUAL_RBLK = 8. PROPOSED.
#   2. "held the job" = season-modal starter per team x slot by
#      RUN-BLOCK snaps. Same rule, unit snaps. PROPOSED.
#   3. THREE lenses, one lens per table (print law): GAP (box),
#      ZONE (box), ADJUSTED (c3 rank on the overall run-block
#      grade). Canon carries two scheme lenses on the box side;
#      the adjusted currency is single.
#   4. The adjusted side is BUILT IN-FILE -- canon has no run-block
#      final evaluation. c3 rookie prior by slot, c3 24/25 blends,
#      and the faced/2026 adj frames mirror the pass-block final
#      evaluation text exactly; every step that HAS a canon target
#      is walled against it (cmp_ol_slate on the NE raw side,
#      league_sys_rb on the league raw side).
#   5. Names ride the membership frames (roster_name on both) --
#      no lookup needed on this unit. Rookies DO appear in machine
#      membership (the flex/modal rungs carry status) -- the
#      pass-rush rookie blind spot does not exist here.
#
# THE PRICING LAW:
#   value_26_priced = avail * value_26 + (1 - avail) * repl(slot)
#   avail = games-played rate 0-1, judged ONLY on seasons the player
#           HELD a starting job (season-modal at a slot that year).
#           Zero starter seasons -> the observed FIRST-YEAR-STARTER
#           rate for HIS population (Andy 2026-08-19 ruling: "career
#           backups get priced at the backup thing; rookies get the
#           rookie line"), measured from 2024-25 first-year starters
#           at that slot:
#             backup_line = in the league but never held a job, and
#               phantom slots (an open job is usually won by a
#               backup); a vet with no window rows is a backup,
#               not a rookie
#             rookie_line = the 2026 entry class (canon roster
#               status == "rookie", i.e. entry_year == 2026) --
#               true rookies
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
#   FIREWALL INTACT: gap blends with the gap replacement level, zone
#           with the zone level, adj with the adj level; the priced
#           family stays its own always-labeled family.
#   SCHEME SPLIT (round two, stamped 2026-08-30, same signature as
#           the currency file's section 4b): V_c3_gap / V_c3_zone
#           (+ their _av priced forms) built per scheme from the
#           currency file's gap_c3_pctl / zone_c3_pctl, same blend
#           law as blended V_c3, which stays untouched.
#   FACED-SIDE SPLIT (round three, stamped 2026-08-30, Andy:
#           "Yes -- honest '--' holes"): the section-8 per-team
#           tables carry THEIR OWN adjusted columns per scheme --
#           faced cells valued on gap_c3_pctl / zone_c3_pctl at the
#           game slot, the 2026 side on per-scheme 24/25 blends
#           with NO prior fill. A scheme neither season earned
#           prints '--'; holes never fill on this side. Sections
#           5/6/7 keep the blended adjusted lens.
#   NOT SEEN: current 2026 injuries beyond roster membership (the
#           roster-change protocol drops known season-enders via
#           load_rosters; PUP/short-term IR not priced here).
#
# PRINT LAW (Andy 2026-08-19): short English phrases with spaces;
# no suffix algebra; one lens per table; every table opens with the
# question it answers. Internals keep contract names. FULL
# PRECISION inside; rounding happens at print, never before a wall.
#
# SOURCE ORDER: pff_run_block_AWS, new_england_opp_ol_schedule(2),
# league_run_block_evaluating_currency_three,
# league_opp_run_blocking_schedule, then this file. Needs
# slots_full + league_sys_rb (built when the league tail is in).
# ============================================================

needed_rb <- c("run_block_summary_qbgrp", "all_pass_block_summary",
               "ol_season_pctl", "rookie_prior", "rblk_c3_pctl",
               "entry_years", "opp_ol_2026_final", "slots_full",
               "league_sys_rb", "opp_map_ol", "pff32_ol", "sched_2026",
               "cmp_ol_slate", "ne_2025_opp_ol_games", "in_season",
               "blend2", "ol_pos_levels")
missing_rb <- needed_rb[!vapply(needed_rb, exists, logical(1))]
if (length(missing_rb)) stop("missing session objects: ",
                             paste(missing_rb, collapse = ", "),
                             " -- see SOURCE ORDER in header")

req_cols_rb <- list(
  run_block_summary_qbgrp = c("player_id", "season", "week", "team",
                              "det_position", "snap_counts_run_block"),
  all_pass_block_summary  = c("player_id", "season", "week",
                              "team_name", "def_ssn", "det_position",
                              "snap_counts_pass_block"),
  ol_season_pctl   = c("player_id", "det_position", "gap", "zone"),
  rookie_prior     = c("det_position", "pr_gap", "pr_zone"),
  rblk_c3_pctl     = c("player_id", "player", "season", "band",
                       "qual_g", "c3_pctl", "gap_c3_pctl",
                       "zone_c3_pctl", "gap_qual_g",
                       "zone_qual_g"),  # round-two split (2026-08-30):
  # if this wall fires, re-source
  # league_run_block_evaluating_currency_three.R
  # (with section 4b) first
  entry_years      = c("player_id", "entry_year"),
  opp_ol_2026_final = c("team_name", "det_position", "roster_name",
                        "player_id", "status", "gap_f", "zone_f"),
  slots_full       = c("team_name", "slot", "roster_name", "player_id",
                       "status", "phantom", "entry_year", "gap_bl",
                       "zone_bl", "gap", "zone", "pr_gap", "pr_zone"),
  league_sys_rb    = c("focal", "lens", "v26", "faced25"),
  opp_map_ol       = c("focal", "opp"),
  cmp_ol_slate     = c("det_position", "metric", "slate_2025",
                       "slate_2026", "delta"),
  ne_2025_opp_ol_games = c("team_name", "det_position", "gap_f",
                           "zone_f"))
for (nm_rb in names(req_cols_rb)) {
  miss_rb <- setdiff(req_cols_rb[[nm_rb]], names(get(nm_rb)))
  if (length(miss_rb)) {
    cat("\n", nm_rb, "-- columns present:\n")
    print(names(get(nm_rb)))
    stop(nm_rb, " is missing columns: ", paste(miss_rb, collapse = ", "),
         " -- report, do not proceed")
  }
}

# ------------------------------------------------------------
# 0. KNOBS. Window/denominator carry the OL stamp; the snap type
#    is the unit adaptation (PROPOSED). Unit-suffixed so every
#    unit's layer coexists in one session.
# ------------------------------------------------------------
AVAIL_SEASONS_RB  <- 2023:2025  # availability history window
AVAIL_SNAP_MIN_RB <- 8          # run-block snaps that week = played
AVAIL_DENOM_RB    <- 17         # REG games per season

cat("\n== AVAILABILITY LAYER -- RUN BLOCKING == knobs: seasons",
    paste(range(AVAIL_SEASONS_RB), collapse = "-"),
    "| played = run-block snaps >=", AVAIL_SNAP_MIN_RB,
    "| denominator", AVAIL_DENOM_RB, "REG games ==\n")

# ------------------------------------------------------------
# 1. avail per player -- REG games played / AVAIL_DENOM_RB. Two
#    rates (Andy 2026-08-19): avail_all = every season with data;
#    avail_job = only seasons he HELD a starting job (season-modal
#    at a slot that year). The price uses avail_job; players with
#    none fall to the first-year lines in section 2b.
#    Feed = run_block_summary_qbgrp, collapsed to player-week
#    before any floor (the qbgrp-grain law -- one player-week is
#    one week, however the feed is keyed). The frame is UNFILTERED
#    here: the c3 game frame's 8-snap qualification gate would
#    hide real availability.
# ------------------------------------------------------------

# who HELD the job: season-modal starter per team x slot, REG weeks,
# by run-block snaps
starters_all_rb <- run_block_summary_qbgrp %>%
  filter(season %in% AVAIL_SEASONS_RB, week <= 18,
         det_position %in% ol_pos_levels) %>%
  group_by(season, team, det_position, player_id) %>%
  summarise(sn = sum(snap_counts_run_block, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(season, team, det_position) %>%
  slice_max(sn, n = 1, with_ties = FALSE) %>%
  ungroup()

starter_map_rb <- starters_all_rb %>%
  distinct(season, player_id) %>%          # trades: one row either way
  mutate(had_job = TRUE)

played_wk_rb <- run_block_summary_qbgrp %>%
  filter(season %in% AVAIL_SEASONS_RB, week <= 18) %>%
  group_by(player_id, season, week) %>%
  summarise(rsnaps = sum(snap_counts_run_block, na.rm = TRUE),
            .groups = "drop") %>%
  filter(rsnaps >= AVAIL_SNAP_MIN_RB)

avail_rb <- played_wk_rb %>%
  count(player_id, season, name = "g_p") %>%
  mutate(avail_s = pmin(g_p / AVAIL_DENOM_RB, 1)) %>%
  left_join(starter_map_rb, by = c("player_id", "season")) %>%
  mutate(had_job = !is.na(had_job)) %>%
  group_by(player_id) %>%
  summarise(yrs       = dplyr::n(),
            job_yrs   = sum(had_job),
            avail_all = round(mean(avail_s), 3),
            avail_job = if (any(had_job))
              round(mean(avail_s[had_job]), 3) else NA_real_,
            .groups = "drop")

cat("\n--- durability among players who HELD a job at least once ---\n")
print(quantile(avail_rb$avail_job, na.rm = TRUE,
               probs = c(0, .1, .25, .5, .75, .9, 1)) %>% round(3))

# ------------------------------------------------------------
# 2. slot-level ESTABLISHED-STARTER availability -- the incumbent
#    benchmark (2025 season-modal starters, REG weeks). Printed for
#    reference only; it is NOT the first-time-starter fill (Andy
#    2026-08-19 ruling) -- the fills are the lines in 2b.
# ------------------------------------------------------------

g25_rb <- played_wk_rb %>%
  filter(season == 2025) %>%
  count(player_id, name = "g_p25")

slot_starter_avail_rb <- starters_all_rb %>%
  filter(season == 2025) %>%
  distinct(player_id, .keep_all = TRUE) %>%   # kicked-inside seasons: one row
  left_join(g25_rb, by = "player_id") %>%
  mutate(avail_s = pmin(g_p25 / AVAIL_DENOM_RB, 1)) %>%
  group_by(det_position) %>%
  summarise(slot_starter_avail = round(mean(avail_s, na.rm = TRUE), 3),
            n_starters = dplyr::n(),
            .groups = "drop")

cat("\n--- how durable is an ESTABLISHED starter at each slot?",
    "(benchmark for context -- not the first-year fill) ---\n")
print(slot_starter_avail_rb %>%
        rename(slot = det_position, "starter avail" = slot_starter_avail,
               starters = n_starters))
if (any(is.na(slot_starter_avail_rb$slot_starter_avail))) {
  stop("slot starter availability NA -- report")
}

# ------------------------------------------------------------
# 0b. c3 entry-year prior by slot (fills rookies / no-c3 members,
#     mirroring the pass-block final evaluation's section 1 text
#     exactly -- canon has no run-block final evaluation, so this
#     layer builds its own from the canon c3 currency frame).
# ------------------------------------------------------------

c3_rookie_prior_rb <- rblk_c3_pctl %>%
  inner_join(entry_years, by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2017) %>%
  filter(band %in% ol_pos_levels) %>%
  group_by(band) %>%
  summarise(pr_c3 = median(c3_pctl, na.rm = TRUE),
            n_entry = dplyr::n(), .groups = "drop")

cat("\n--- c3 entry-year prior by slot (run-block currency) ---\n")
print(c3_rookie_prior_rb)

if (!setequal(c3_rookie_prior_rb$band, ol_pos_levels)) {
  cat("slots priced:", c3_rookie_prior_rb$band, "\n")
  stop("c3 prior must price all five slots -- every coalesce below ",
       "depends on it; report, do not proceed")
}

# ------------------------------------------------------------
# 0c. SCHEME-SPLIT c3 priors by slot (round two, stamped
#     2026-08-30 -- same signature as the currency file's 4b):
#     entry-year medians of gap_c3_pctl / zone_c3_pctl. Scheme
#     samples are thinner BY DESIGN, so a slot can lack a scheme
#     prior -- NO stop here; every chain below falls scheme
#     prior -> blended slot prior -> unit scalar.
# ------------------------------------------------------------

c3_scheme_prior_rb <- rblk_c3_pctl %>%
  inner_join(entry_years, by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2017) %>%
  filter(band %in% ol_pos_levels) %>%
  group_by(band) %>%
  summarise(pr_c3_gap  = median(gap_c3_pctl,  na.rm = TRUE),
            pr_c3_zone = median(zone_c3_pctl, na.rm = TRUE),
            n_entry = dplyr::n(), .groups = "drop")

cat("\n--- c3 entry-year prior by slot, per scheme (a hole falls",
    "to the blended prior in the chains below) ---\n")
print(c3_scheme_prior_rb)

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

seen_2023_rb <- run_block_summary_qbgrp %>%
  filter(season == 2023, week <= 18) %>%
  distinct(player_id) %>% pull(player_id)
seen_2324_rb <- run_block_summary_qbgrp %>%
  filter(season %in% 2023:2024, week <= 18) %>%
  distinct(player_id) %>% pull(player_id)

starter_seasons_rb <- starters_all_rb %>% distinct(player_id, season)

first_year_rb <- bind_rows(
  starters_all_rb %>%
    filter(season == 2024) %>%
    anti_join(starter_seasons_rb %>% filter(season == 2023),
              by = "player_id"),
  starters_all_rb %>%
    filter(season == 2025) %>%
    anti_join(starter_seasons_rb %>% filter(season %in% 2023:2024),
              by = "player_id")) %>%
  distinct(player_id, season, .keep_all = TRUE) %>%
  mutate(pop = case_when(
    season == 2024 & player_id %in% seen_2023_rb ~ "backup",
    season == 2025 & player_id %in% seen_2324_rb ~ "backup",
    TRUE ~ "rookie")) %>%
  left_join(played_wk_rb %>%
              filter(season %in% 2024:2025) %>%
              count(player_id, season, name = "g_p"),
            by = c("player_id", "season")) %>%
  mutate(g_p  = dplyr::coalesce(g_p, 0L),
         rate = pmin(g_p / AVAIL_DENOM_RB, 1))

first_year_cells_rb <- first_year_rb %>%
  group_by(det_position, pop) %>%
  summarise(n = dplyr::n(), line = round(mean(rate), 3),
            .groups = "drop")

first_year_unit_rb <- first_year_rb %>%
  group_by(pop) %>%
  summarise(n_unit = dplyr::n(), unit_line = round(mean(rate), 3),
            .groups = "drop")

first_year_lines_rb <- tidyr::expand_grid(
  det_position = ol_pos_levels, pop = c("backup", "rookie")) %>%
  left_join(first_year_cells_rb, by = c("det_position", "pop")) %>%
  left_join(first_year_unit_rb, by = "pop") %>%
  mutate(line = dplyr::coalesce(line, unit_line),
         evidence = if_else(is.na(n),
                            paste0("no slot evidence -- ", pop,
                                   " unit line (n=", n_unit, ")"),
                            paste0(n, " first-year starters"))) %>%
  select(det_position, pop, line, evidence)
if (any(is.na(first_year_lines_rb$line))) {
  print(first_year_lines_rb)
  stop("first-year lines NA -- report")
}

cat("\n--- the first-year lines: when a NEW starter holds a job for",
    "the first time, how much of that season does he keep? ---\n")
print(first_year_lines_rb %>%
        transmute(slot = det_position, population = pop,
                  "kept rate" = line, evidence))

lines_wide_rb <- first_year_lines_rb %>%
  select(det_position, pop, line) %>%
  tidyr::pivot_wider(names_from = pop, values_from = line,
                     names_prefix = "line_")

# ------------------------------------------------------------
# 3. repl(slot) -- empirical 2025 backup-game level, REG, equal
#    game weights (OL law), signed fill law per lens. This is "who
#    you actually get when the starter is out", measured from real
#    2025 backup games: the week's top run-block-snap blocker at a
#    team x slot who is NOT the season-modal starter there.
# ------------------------------------------------------------

starters_25_reg_rb <- starters_all_rb %>%
  filter(season == 2025) %>%
  select(team, det_position, starter_id = player_id)

# unit scalars -- the last rung of the signed fill law ("else unit
# scalar"). Defined here, before section 3; the league frame in
# section 4 reuses these same objects (single definition).
prior_unit_gap_rb <- mean(rookie_prior$pr_gap, na.rm = TRUE)
prior_unit_zone_rb <- mean(rookie_prior$pr_zone, na.rm = TRUE)
prior_unit_c3_rb  <- mean(c3_rookie_prior_rb$pr_c3)
stopifnot(!is.na(prior_unit_gap_rb), !is.na(prior_unit_zone_rb),
          !is.na(prior_unit_c3_rb))

repl_games_rb <- run_block_summary_qbgrp %>%
  filter(season == 2025, week <= 18, det_position %in% ol_pos_levels) %>%
  group_by(team, week, det_position) %>%
  slice_max(snap_counts_run_block, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(starters_25_reg_rb, by = c("team", "det_position")) %>%
  filter(player_id != starter_id) %>%
  left_join(ol_season_pctl %>%
              select(player_id, det_position, gap25 = gap,
                     zone25 = zone),
            by = c("player_id", "det_position")) %>%
  left_join(rblk_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, band, c3_pctl, gap_c3_pctl,
                     zone_c3_pctl),
            by = c("player_id", "det_position" = "band")) %>%
  mutate(
    # signed fill law per lens: earned 2025 level, else slot prior,
    # else unit scalar. coalesce chain -- a backup game can never
    # land NA, so equal game weights stay exact.
    gap_val  = dplyr::coalesce(
      gap25,
      rookie_prior$pr_gap[match(det_position, rookie_prior$det_position)],
      prior_unit_gap_rb),
    zone_val = dplyr::coalesce(
      zone25,
      rookie_prior$pr_zone[match(det_position, rookie_prior$det_position)],
      prior_unit_zone_rb),
    adj_val  = dplyr::coalesce(
      c3_pctl,
      c3_rookie_prior_rb$pr_c3[match(det_position,
                                     c3_rookie_prior_rb$band)],
      prior_unit_c3_rb),
    # round two: same signed fill law per scheme -- earned scheme
    # level, else slot scheme prior, else slot blended prior,
    # else unit scalar
    adj_gap_val = dplyr::coalesce(
      gap_c3_pctl,
      c3_scheme_prior_rb$pr_c3_gap[match(det_position,
                                         c3_scheme_prior_rb$band)],
      c3_rookie_prior_rb$pr_c3[match(det_position,
                                     c3_rookie_prior_rb$band)],
      prior_unit_c3_rb),
    adj_zone_val = dplyr::coalesce(
      zone_c3_pctl,
      c3_scheme_prior_rb$pr_c3_zone[match(det_position,
                                          c3_scheme_prior_rb$band)],
      c3_rookie_prior_rb$pr_c3[match(det_position,
                                     c3_rookie_prior_rb$band)],
      prior_unit_c3_rb))

repl_level_rb <- repl_games_rb %>%
  group_by(det_position) %>%
  summarise(n_backup  = dplyr::n(),
            repl_gap  = round(mean(gap_val,  na.rm = TRUE), 3),
            repl_zone = round(mean(zone_val, na.rm = TRUE), 3),
            repl_adj  = round(mean(adj_val,  na.rm = TRUE), 3),
            repl_adj_gap  = round(mean(adj_gap_val,  na.rm = TRUE), 3),
            repl_adj_zone = round(mean(adj_zone_val, na.rm = TRUE), 3),
            .groups = "drop")

cat("\n--- when the starter is out, what do you get? (2025 backup",
    "games at the slot, per lens) ---\n")
print(repl_level_rb %>%
        rename(slot = det_position, "backup gms seen" = n_backup,
               "fill lvl (gap)" = repl_gap, "fill lvl (zone)" = repl_zone,
               "fill lvl (adj)" = repl_adj,
               "fill lvl (adj gap)" = repl_adj_gap,
               "fill lvl (adj zone)" = repl_adj_zone))
if (!setequal(repl_level_rb$det_position, ol_pos_levels) ||
    any(is.na(repl_level_rb$repl_gap)) ||
    any(is.na(repl_level_rb$repl_zone)) ||
    any(is.na(repl_level_rb$repl_adj)) ||
    any(is.na(repl_level_rb$repl_adj_gap)) ||
    any(is.na(repl_level_rb$repl_adj_zone))) {
  stop("replacement level NA or slot missing -- report")
}

# ------------------------------------------------------------
# 4. slot_value_26_rb -- every 2026 slot (all 32 teams, machine
#    membership) carrying the league-tail values plus the
#    injury-priced values, per lens. The V_gap / V_zone chains
#    mirror the league tail's league_one_lens() case_when
#    VERBATIM; V_c3 mirrors the pass-block final evaluation's
#    gf_adj chain. THE LEAGUE WALL below rebuilds canon's
#    league_sys_rb (all 32 focals, both box lenses) from this
#    exact membership and faced frame -- identical or stop.
#
#    The faced cells are built ONCE here (the league tail's
#    faced law: one blocker per week x team x slot by PASS-BLOCK
#    snaps -- canon's blocker-pick rule for this unit -- valued
#    per lens, fill at slot prior else unit scalar / slot c3
#    prior); the sweep in section 6 reads this same frame.
# ------------------------------------------------------------

# (prior_unit_gap_rb / prior_unit_zone_rb defined above section 3 --
#  single definition, reused here)

faced_cells_rb <- all_pass_block_summary %>%
  filter(season == 2025, in_season(week),
         det_position %in% ol_pos_levels) %>%
  mutate(def_t = stringr::str_remove(def_ssn, "2025$")) %>%
  group_by(def_t, week, team_name, det_position) %>%
  slice_max(snap_counts_pass_block, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(def_t, week, team_name, det_position, player, player_id) %>%
  left_join(ol_season_pctl %>%
              select(player_id, det_position, F_gap = gap,
                     F_zone = zone),
            by = c("player_id", "det_position")) %>%
  left_join(rookie_prior %>%
              select(det_position, PR_gap = pr_gap, PR_zone = pr_zone),
            by = "det_position") %>%
  left_join(rblk_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, band, F_c3 = c3_pctl),
            by = c("player_id", "det_position" = "band")) %>%
  left_join(c3_rookie_prior_rb %>% select(band, pr_c3),
            by = c("det_position" = "band")) %>%
  mutate(F_gap  = dplyr::coalesce(F_gap,  PR_gap,  prior_unit_gap_rb),
         F_zone = dplyr::coalesce(F_zone, PR_zone, prior_unit_zone_rb),
         F_c3   = dplyr::coalesce(F_c3,   pr_c3))
stopifnot(!any(is.na(faced_cells_rb$F_gap)),
          !any(is.na(faced_cells_rb$F_zone)),
          !any(is.na(faced_cells_rb$F_c3)))

cat("\n--- faced 2025 cells built:", nrow(faced_cells_rb),
    "blocker-weeks across", dplyr::n_distinct(faced_cells_rb$def_t),
    "defenses (playoffs IN; fills at priors) ---\n")

slot_value_26_rb_build <- slots_full %>%
  filter(slot %in% ol_pos_levels) %>%
  mutate(V_gap = dplyr::case_when(
    phantom ~ dplyr::coalesce(pr_gap, prior_unit_gap_rb),
    status == "rookie" ~ dplyr::coalesce(gap_bl, pr_gap,
                                         prior_unit_gap_rb),
    TRUE ~ dplyr::coalesce(gap_bl, gap, pr_gap,
                           prior_unit_gap_rb)),
    V_zone = dplyr::case_when(
      phantom ~ dplyr::coalesce(pr_zone, prior_unit_zone_rb),
      status == "rookie" ~ dplyr::coalesce(zone_bl, pr_zone,
                                           prior_unit_zone_rb),
      TRUE ~ dplyr::coalesce(zone_bl, zone, pr_zone,
                             prior_unit_zone_rb))) %>%
  left_join(rblk_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, a25 = c3_pctl, qg25a = qual_g,
                     g25 = gap_c3_pctl, qg25g = gap_qual_g,
                     z25 = zone_c3_pctl, qg25z = zone_qual_g),
            by = "player_id") %>%
  left_join(rblk_c3_pctl %>% filter(season == 2024) %>%
              select(player_id, a24 = c3_pctl, g24 = gap_c3_pctl,
                     z24 = zone_c3_pctl),
            by = "player_id") %>%
  left_join(c3_rookie_prior_rb %>% select(band, pr_c3),
            by = c("slot" = "band")) %>%
  left_join(c3_scheme_prior_rb %>% select(band, pr_c3_gap,
                                          pr_c3_zone),
            by = c("slot" = "band")) %>%
  mutate(w25a = pmin(dplyr::coalesce(qg25a, 0L) / 10, 1),
         V_c3 = dplyr::coalesce(blend2(a25, a24, w25a), pr_c3),
         # round two: the scheme split -- same blend law per scheme;
         # fill chain scheme prior -> blended slot prior -> unit
         w25g = pmin(dplyr::coalesce(qg25g, 0L) / 10, 1),
         w25z = pmin(dplyr::coalesce(qg25z, 0L) / 10, 1),
         V_c3_gap = dplyr::coalesce(blend2(g25, g24, w25g),
                                    pr_c3_gap, pr_c3,
                                    prior_unit_c3_rb),
         V_c3_zone = dplyr::coalesce(blend2(z25, z24, w25z),
                                     pr_c3_zone, pr_c3,
                                     prior_unit_c3_rb)) %>%
  # --- layer-only joins below this line; V values untouched ---
  left_join(avail_rb %>%
              select(player_id, yrs, job_yrs, avail_all, avail_job),
            by = "player_id") %>%
  mutate(avail_src = dplyr::case_when(
    !is.na(avail_job) ~ "starter_history",
    phantom ~ "backup_line",
    status == "rookie" ~ "rookie_line",
    TRUE ~ "backup_line")) %>%
  left_join(lines_wide_rb, by = c("slot" = "det_position")) %>%
  mutate(avail = dplyr::case_when(
    avail_src == "starter_history" ~ avail_job,
    avail_src == "backup_line" ~ line_backup,
    TRUE ~ line_rookie)) %>%
  left_join(repl_level_rb, by = c("slot" = "det_position")) %>%
  mutate(V_gap_av  = round(avail * V_gap  + (1 - avail) * repl_gap, 4),
         V_zone_av = round(avail * V_zone + (1 - avail) * repl_zone, 4),
         V_c3_av   = round(avail * V_c3   + (1 - avail) * repl_adj, 4),
         V_c3_gap_av  = round(avail * V_c3_gap +
                                (1 - avail) * repl_adj_gap, 4),
         V_c3_zone_av = round(avail * V_c3_zone +
                                (1 - avail) * repl_adj_zone, 4))
stopifnot(!any(is.na(slot_value_26_rb_build$V_gap)),
          !any(is.na(slot_value_26_rb_build$V_zone)),
          !any(is.na(slot_value_26_rb_build$V_c3)),
          !any(is.na(slot_value_26_rb_build$V_c3_gap)),
          !any(is.na(slot_value_26_rb_build$V_c3_zone)),
          !any(is.na(slot_value_26_rb_build$V_c3_gap_av)),
          !any(is.na(slot_value_26_rb_build$V_c3_zone_av)),
          !any(is.na(slot_value_26_rb_build$avail)))

cat("\n--- round-two receipt: 2026 slots priced, scheme split live ---\n")
print(slot_value_26_rb_build %>%
        summarise(slots = dplyr::n(),
                  gap_earned = sum(!is.na(g25) | !is.na(g24)),
                  zone_earned = sum(!is.na(z25) | !is.na(z24)),
                  prior_filled_gap = sum(is.na(g25) & is.na(g24)),
                  prior_filled_zone = sum(is.na(z25) & is.na(z24))))

# THE LEAGUE WALL: league_sys_rb rebuilt from this membership and
# these faced cells, all 32 focals, both box lenses, 1e-8.
team26_rb <- slot_value_26_rb_build %>%
  group_by(team_name) %>%
  summarise(v26_gap_m = mean(V_gap), v26_zone_m = mean(V_zone),
            .groups = "drop")
focal26_rb <- opp_map_ol %>%
  left_join(team26_rb, by = c("opp" = "team_name")) %>%
  group_by(focal) %>%
  summarise(v26_gap_m = mean(v26_gap_m),
            v26_zone_m = mean(v26_zone_m), .groups = "drop")
faced_focal_rb <- faced_cells_rb %>%
  group_by(def_t) %>%
  summarise(f_gap_m = mean(F_gap), f_zone_m = mean(F_zone),
            f_adj_m = mean(F_c3), .groups = "drop")

# widen ONLY the two compared columns: league_sys_rb also carries
# d / prior_share / fill_share, which differ by lens -- left in, the
# wide frame doubles to 64 rows and the wall fires on its own shadow.
chk_lg_rb <- league_sys_rb %>%
  select(focal, lens, v26, faced25) %>%
  tidyr::pivot_wider(names_from = lens,
                     values_from = c(v26, faced25)) %>%
  full_join(focal26_rb, by = "focal") %>%
  full_join(faced_focal_rb %>%
              select(def_t, f_gap_m, f_zone_m),
            by = c("focal" = "def_t"))

bad_lg_rb <- chk_lg_rb %>%
  filter(dplyr::if_any(c(v26_gap, v26_zone, faced25_gap, faced25_zone,
                         v26_gap_m, v26_zone_m, f_gap_m, f_zone_m),
                       is.na) |
           abs(v26_gap  - v26_gap_m)  > 1e-8 |
           abs(v26_zone - v26_zone_m) > 1e-8 |
           abs(faced25_gap  - f_gap_m)  > 1e-8 |
           abs(faced25_zone - f_zone_m) > 1e-8)

if (nrow(chk_lg_rb) != 32L || nrow(bad_lg_rb) > 0L) {
  cat("league wall failure -- wide rows:", nrow(chk_lg_rb),
      "(need 32); bad rows:", nrow(bad_lg_rb), "\n")
  cat("focals in canon but not my 2026 side:\n")
  print(setdiff(league_sys_rb$focal, focal26_rb$focal))
  cat("focals in my 2026 side but not canon:\n")
  print(setdiff(focal26_rb$focal, league_sys_rb$focal))
  cat("focals in canon but not my faced side:\n")
  print(setdiff(league_sys_rb$focal, faced_focal_rb$def_t))
  cat("focals in my faced side but not canon:\n")
  print(setdiff(faced_focal_rb$def_t, league_sys_rb$focal))
  print(bad_lg_rb)
  stop("run-block membership or faced side drifted from league_sys_rb",
       " -- report")
}
cat("\n--- league wall passed: membership + faced rebuild identical",
    "to league_sys_rb, both lenses, 32 focals ---\n")

slot_value_26_rb <- slot_value_26_rb_build

# ------------------------------------------------------------
# 5. NE slate, injury-priced -- CANON membership off
#    opp_ol_2026_final (70 rows). The adjusted side is built
#    in-file, mirroring the pass-block final evaluation:
#    proj_c3_rb (canon membership + c3 blend, prior fills by
#    slot) and the faced-2025 adj cells (game-slot c3, fill at
#    the slot c3 prior, off-modal fills flagged). Canon leaves
#    gap_f/zone_f NA where neither 2025 nor 2024 pctl exists;
#    the layer inherits the holes exactly (walled), never
#    invents -- the adjusted side covers them.
# ------------------------------------------------------------

proj_c3_rb <- opp_ol_2026_final %>%
  left_join(rblk_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, c3_25p = c3_pctl, qg25_c3 = qual_g),
            by = "player_id") %>%
  left_join(rblk_c3_pctl %>% filter(season == 2024) %>%
              select(player_id, c3_24p = c3_pctl),
            by = "player_id") %>%
  left_join(rblk_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, c3_25g = gap_c3_pctl, qg25_g = gap_qual_g,
                     c3_25z = zone_c3_pctl, qg25_z = zone_qual_g),
            by = "player_id") %>%
  left_join(rblk_c3_pctl %>% filter(season == 2024) %>%
              select(player_id, c3_24g = gap_c3_pctl,
                     c3_24z = zone_c3_pctl),
            by = "player_id") %>%
  left_join(c3_rookie_prior_rb %>% select(band, pr_c3),
            by = c("det_position" = "band")) %>%
  mutate(w25_c3 = pmin(dplyr::coalesce(qg25_c3, 0L) / 10, 1),
         c3_bl  = blend2(c3_25p, c3_24p, w25_c3),
         prior_used_c3 = is.na(c3_bl),
         c3_f   = dplyr::coalesce(c3_bl, pr_c3),
         # round three: per-scheme 2026 values, SAME blend law as
         # blended -- but NO prior fill (Andy 2026-08-30, honest
         # '--' holes). A scheme neither season earned stays NA.
         w25_g = pmin(dplyr::coalesce(qg25_g, 0L) / 10, 1),
         w25_z = pmin(dplyr::coalesce(qg25_z, 0L) / 10, 1),
         c3_f_gap  = blend2(c3_25g, c3_24g, w25_g),
         c3_f_zone = blend2(c3_25z, c3_24z, w25_z))
stopifnot(nrow(proj_c3_rb) == 70L,
          anyDuplicated(proj_c3_rb[, c("team_name", "det_position")]) == 0,
          !any(is.na(proj_c3_rb$c3_f)))

cat("\n--- 2026 slate scheme holes (of the 70 opponent-line slots):",
    "no scheme currency in EITHER 2025 or 2024 -- they print '--'",
    "in the per-team tables; nothing fills them ---\n")
print(proj_c3_rb %>%
        summarise(slots = dplyr::n(),
                  gap_holes = sum(is.na(c3_f_gap)),
                  zone_holes = sum(is.na(c3_f_zone))))

# NE faced cells rebuilt once here for the adjusted side (canon's
# faced frame drops player_id in its final select). Raw side of
# the rebuild walled against canon per-slot means at 1e-8 first --
# the pass-block final evaluation's rebuild check, mirrored.
faced_ne_rb <- all_pass_block_summary %>%
  filter(season == 2025, def_ssn == "NE2025", in_season(week),
         det_position %in% ol_pos_levels) %>%
  group_by(week, team_name, det_position) %>%
  slice_max(snap_counts_pass_block, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(week, team_name, det_position, player, player_id)

chk_val_rb <- ne_2025_opp_ol_games %>%
  group_by(det_position) %>%
  summarise(canon_gap  = mean(gap_f,  na.rm = TRUE),
            canon_zone = mean(zone_f, na.rm = TRUE),
            .groups = "drop") %>%
  full_join(faced_ne_rb %>%
              left_join(ol_season_pctl %>%
                          select(player_id, det_position, gap_m = gap,
                                 zone_m = zone),
                        by = c("player_id", "det_position")) %>%
              group_by(det_position) %>%
              summarise(mine_gap  = mean(gap_m,  na.rm = TRUE),
                        mine_zone = mean(zone_m, na.rm = TRUE),
                        .groups = "drop"),
            by = "det_position") %>%
  mutate(ok = abs(canon_gap - mine_gap)   < 1e-8 &
           abs(canon_zone - mine_zone) < 1e-8)
cat("\n--- faced rebuild check: per-slot earned means, canon vs",
    "rebuild (must be identical) ---\n")
print(chk_val_rb %>% mutate(across(where(is.numeric), ~ round(.x, 4))))
stopifnot(all(chk_val_rb$ok))

# Adj valuation at the GAME slot: c3 keeps one modal band per
# player-season, so the join key is (player_id, game slot == band).
# Misses split into never-qualified vs qualified-at-another-band;
# both fill at the slot prior. ROUND THREE (Andy 2026-08-30): the
# scheme cells c3_25_gap / c3_25_zone ride the same join but are
# earned-or-HOLE -- no prior fill, ever.
faced_c3_rb <- faced_ne_rb %>%
  left_join(rblk_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, band, c3_25 = c3_pctl),
            by = c("player_id", "det_position" = "band")) %>%
  left_join(rblk_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, band, c3_25_gap = gap_c3_pctl,
                     c3_25_zone = zone_c3_pctl),
            by = c("player_id", "det_position" = "band")) %>%
  mutate(has_c3_25 = player_id %in%
           (rblk_c3_pctl %>% filter(season == 2025) %>%
              pull(player_id))) %>%
  left_join(c3_rookie_prior_rb %>% select(band, pr_c3),
            by = c("det_position" = "band")) %>%
  mutate(fill_c3  = is.na(c3_25),
         fill_why = dplyr::case_when(!fill_c3   ~ "earned",
                                     !has_c3_25 ~ "no_c3_2025",
                                     TRUE       ~ "off_modal_slot"),
         c3_25    = dplyr::coalesce(c3_25, pr_c3))
if (any(is.na(faced_c3_rb$c3_25))) {
  print(faced_c3_rb %>% filter(is.na(c3_25)))
  stop("faced adj produced NA after the prior fill -- report")
}
cat("\n--- NE faced fill tiers (adj, by cell count) ---\n")
print(faced_c3_rb %>% count(fill_why, name = "cells") %>%
        mutate(share = round(cells / sum(cells), 3)))

cat("\n--- NE faced SCHEME holes (of the faced cells above): the",
    "faced player never earned that scheme's 2025 currency at the",
    "game slot. Honest holes -- they print '--' in the per-team",
    "tables and drop out of the adjusted '25 means ---\n")
print(faced_c3_rb %>%
        summarise(cells = dplyr::n(),
                  gap_holes = sum(is.na(c3_25_gap)),
                  zone_holes = sum(is.na(c3_25_zone))))

faced_band_c3_rb <- faced_c3_rb %>%
  group_by(det_position) %>%
  summarise(fill_share = round(mean(fill_c3), 3),
            adj_25     = mean(c3_25),
            .groups = "drop")

ne_frame_rb <- proj_c3_rb %>%
  left_join(avail_rb %>% select(player_id, avail_job),
            by = "player_id") %>%
  mutate(avail_src = dplyr::case_when(
    !is.na(avail_job) ~ "starter_history",
    is.na(player_id) ~ "backup_line",
    status == "rookie" ~ "rookie_line",
    TRUE ~ "backup_line")) %>%
  left_join(lines_wide_rb, by = "det_position") %>%
  mutate(avail = dplyr::case_when(
    avail_src == "starter_history" ~ avail_job,
    avail_src == "backup_line" ~ line_backup,
    TRUE ~ line_rookie)) %>%
  left_join(repl_level_rb, by = "det_position") %>%
  mutate(gap_f_av  = if_else(is.na(gap_f), NA_real_,
                             round(avail * gap_f +
                                     (1 - avail) * repl_gap, 4)),
         zone_f_av = if_else(is.na(zone_f), NA_real_,
                             round(avail * zone_f +
                                     (1 - avail) * repl_zone, 4)),
         adj_f_av  = round(avail * c3_f + (1 - avail) * repl_adj, 4))

if (!identical(is.na(ne_frame_rb$gap_f_av), is.na(ne_frame_rb$gap_f)) ||
    !identical(is.na(ne_frame_rb$zone_f_av), is.na(ne_frame_rb$zone_f))) {
  ne_frame_rb %>%
    filter(is.na(gap_f_av) != is.na(gap_f) |
             is.na(zone_f_av) != is.na(zone_f)) %>%
    select(team_name, det_position, roster_name, status, gap_f,
           zone_f) %>%
    print()
  stop("layer invented or dropped a canon NA -- report")
}
if (any(is.na(ne_frame_rb$adj_f_av)) || any(is.na(ne_frame_rb$avail))) {
  stop("adj_f_av or avail NA -- report")
}

cat("\n--- canon holes: these slate members carry no box-score value",
    "(the adjusted side covers them) ---\n")
holes_rb <- ne_frame_rb %>%
  filter(is.na(gap_f) | is.na(zone_f))
if (nrow(holes_rb)) {
  print(holes_rb %>%
          transmute(tm = team_name, slot = det_position,
                    player = roster_name, status,
                    "gap hole" = is.na(gap_f),
                    "zone hole" = is.na(zone_f),
                    "adj lvl" = round(c3_f, 3)))
} else cat("  (none -- every slate member carries both box lenses)\n")

cmp_wide_rb <- cmp_ol_slate %>%
  filter(metric %in% c("gap_f", "zone_f")) %>%
  tidyr::pivot_wider(names_from = metric,
                     values_from = c(slate_2025, slate_2026, delta))

slate_rows_rb <- tibble::tibble(team_name = sched_2026) %>%
  left_join(ne_frame_rb, by = "team_name",
            relationship = "many-to-many")
if (nrow(slate_rows_rb) != length(sched_2026) * 5L) {
  stop("NE slate rows broken -- report")
}

# 2026-side healthy means recomputed from the slate rows (walled vs
# canon below); the FACED raw side is canon itself, pulled from
# cmp_ol_slate -- the layer never touches it, so it never rebuilds
# it. FULL PRECISION inside; rounding is print-time only.
ne_slate_avail_rb <- slate_rows_rb %>%
  group_by(det_position) %>%
  summarise(backup_gms     = round(sum(1 - avail), 1),
            n_gap          = sum(!is.na(gap_f)),
            gap_26_healthy  = mean(gap_f, na.rm = TRUE),
            gap_26_priced   = mean(gap_f_av, na.rm = TRUE),
            n_zone         = sum(!is.na(zone_f)),
            zone_26_healthy = mean(zone_f, na.rm = TRUE),
            zone_26_priced  = mean(zone_f_av, na.rm = TRUE),
            adj_26_healthy  = mean(c3_f),
            adj_26_priced   = mean(adj_f_av),
            .groups = "drop") %>%
  left_join(cmp_wide_rb, by = "det_position") %>%
  left_join(faced_band_c3_rb %>% select(det_position, adj_25),
            by = "det_position") %>%
  mutate(d_gap_av  = gap_26_priced  - slate_2025_gap_f,
         d_zone_av = zone_26_priced - slate_2025_zone_f,
         d_adj_av   = adj_26_priced  - adj_25) %>%
  arrange(match(det_position, ol_pos_levels))

chk_ne_rb <- ne_slate_avail_rb %>%
  select(det_position, gap_26_healthy, zone_26_healthy,
         slate_2026_gap_f, slate_2026_zone_f)
if (!isTRUE(all.equal(chk_ne_rb$gap_26_healthy,
                      chk_ne_rb$slate_2026_gap_f, tolerance = 5e-4)) ||
    !isTRUE(all.equal(chk_ne_rb$zone_26_healthy,
                      chk_ne_rb$slate_2026_zone_f, tolerance = 5e-4))) {
  print(chk_ne_rb)
  stop("NE run-block availability table drifted from canon -- report")
}

cat("\n== NE 2026 OPPONENT RUN BLOCKING -- how hard will each slot",
    "be to move, once injuries happen? ==\n")
cat("  26 healthy = everyone plays 17 | 26 priced = expected injuries",
    "in\n  real jump = priced - faced '25 | backup gms = expected games",
    "NE catches a\n  backup at that slot | rows w/ box = of 17 slate",
    "rows, how many carry a canon\n  box-score value\n")

cat("\nGAP LENS (box score, gap-scheme grade percentiles)\n")
print(ne_slate_avail_rb %>%
        transmute(slot = det_position,
                  "faced '25" = round(slate_2025_gap_f, 3),
                  "26 healthy" = round(gap_26_healthy, 3),
                  "healthy jump" = round(delta_gap_f, 3),
                  "26 priced" = round(gap_26_priced, 3),
                  "real jump" = round(d_gap_av, 3),
                  "backup gms" = backup_gms,
                  "rows w/ box" = n_gap))

cat("\nZONE LENS (box score, zone-scheme grade percentiles)\n")
print(ne_slate_avail_rb %>%
        transmute(slot = det_position,
                  "faced '25" = round(slate_2025_zone_f, 3),
                  "26 healthy" = round(zone_26_healthy, 3),
                  "healthy jump" = round(delta_zone_f, 3),
                  "26 priced" = round(zone_26_priced, 3),
                  "real jump" = round(d_zone_av, 3),
                  "backup gms" = backup_gms,
                  "rows w/ box" = n_zone))

cat("\nADJUSTED LENS (same-slate skill on the overall run-block",
    "grade, the evaluation currency)\n")
print(ne_slate_avail_rb %>%
        transmute(slot = det_position,
                  "faced '25" = round(adj_25, 3),
                  "26 healthy" = round(adj_26_healthy, 3),
                  "healthy jump" = round(adj_26_healthy - adj_25, 3),
                  "26 priced" = round(adj_26_priced, 3),
                  "real jump" = round(d_adj_av, 3),
                  "backup gms" = backup_gms))

# ------------------------------------------------------------
# 6. LEAGUE SWEEP -- every focal's slate, healthy vs priced, per
#    slot and lens. MACHINE-ONLY: machine membership + the faced
#    cells from section 4. The league wall above already proved
#    both box lenses against league_sys_rb at 1e-8 (all 32
#    focals, faced AND 2026 sides); the adjusted lens has no
#    canon counterpart -- internal NA walls only.
# ------------------------------------------------------------

faced_slot_rb <- faced_cells_rb %>%
  group_by(def_t, det_position) %>%
  summarise(faced_gap = mean(F_gap), faced_zone = mean(F_zone),
            faced_adj = mean(F_c3), .groups = "drop")

sweep_avail_rb <- opp_map_ol %>%
  left_join(slot_value_26_rb %>%
              select(team_name, slot, avail, V_gap, V_gap_av,
                     V_zone, V_zone_av, V_c3, V_c3_av),
            by = c("opp" = "team_name"),
            relationship = "many-to-many") %>%
  group_by(focal, slot) %>%
  summarise(healthy_gap  = mean(V_gap),   priced_gap  = mean(V_gap_av),
            healthy_zone = mean(V_zone),  priced_zone = mean(V_zone_av),
            healthy_adj  = mean(V_c3),    priced_adj  = mean(V_c3_av),
            .groups = "drop") %>%
  left_join(faced_slot_rb,
            by = c("focal" = "def_t", "slot" = "det_position")) %>%
  mutate(d_gap    = healthy_gap  - faced_gap,
         d_av_gap = priced_gap   - faced_gap,
         d_zone    = healthy_zone - faced_zone,
         d_av_zone = priced_zone  - faced_zone,
         d_adj     = healthy_adj  - faced_adj,
         d_av_adj  = priced_adj   - faced_adj)
if (any(is.na(sweep_avail_rb$faced_adj)) ||
    any(is.na(sweep_avail_rb$priced_adj)) ||
    any(is.na(sweep_avail_rb$priced_gap)) ||
    any(is.na(sweep_avail_rb$priced_zone))) {
  sweep_avail_rb %>%
    filter(is.na(faced_adj) | is.na(priced_adj) |
             is.na(priced_gap) | is.na(priced_zone)) %>% print()
  stop("league sweep has NA -- report")
}

cat("\n== HOW MUCH OF THE 2026 SLATE JUMP WAS THE INJURY ASYMMETRY? ==\n")
cat("League mean jump by slot. Positive = opponents block better than\n")
cat("what the run game faced in '25. 'real jump' is what survives\n")
cat("once expected injuries are priced in.\n")
cat("\nGAP LENS\n")
print(sweep_avail_rb %>%
        group_by(slot) %>%
        summarise("healthy jump" = round(mean(d_gap), 3),
                  "real jump"    = round(mean(d_av_gap), 3),
                  .groups = "drop") %>%
        arrange(match(slot, ol_pos_levels)))
cat("\nZONE LENS\n")
print(sweep_avail_rb %>%
        group_by(slot) %>%
        summarise("healthy jump" = round(mean(d_zone), 3),
                  "real jump"    = round(mean(d_av_zone), 3),
                  .groups = "drop") %>%
        arrange(match(slot, ol_pos_levels)))
cat("\nADJUSTED LENS\n")
print(sweep_avail_rb %>%
        group_by(slot) %>%
        summarise("healthy jump" = round(mean(d_adj), 3),
                  "real jump"    = round(mean(d_av_adj), 3),
                  .groups = "drop") %>%
        arrange(match(slot, ol_pos_levels)))

cat("\n== WHOSE SLATE MOVES MOST ONCE INJURIES ARE PRICED? ==\n")
cat("(adjusted lens; swing = priced - healthy at that slot.\n")
cat(" Softens first, then the five whose slates get HARDER --\n")
cat(" their opponents are the durable ones.)\n")
cat("\nSOFTENS MOST\n")
print(sweep_avail_rb %>%
        mutate(swing = round(d_av_adj - d_adj, 3)) %>%
        arrange(swing) %>%
        transmute(tm = focal, slot,
                  "faced '25" = round(faced_adj, 3),
                  healthy = round(healthy_adj, 3),
                  priced = round(priced_adj, 3), swing) %>%
        head(10))
cat("\nHARDENS MOST\n")
print(sweep_avail_rb %>%
        mutate(swing = round(d_av_adj - d_adj, 3)) %>%
        arrange(desc(swing)) %>%
        transmute(tm = focal, slot,
                  "faced '25" = round(faced_adj, 3),
                  healthy = round(healthy_adj, 3),
                  priced = round(priced_adj, 3), swing) %>%
        head(5))

focal_slate_rb <- opp_map_ol %>%
  left_join(slot_value_26_rb %>%
              select(team_name, slot, avail, V_c3, V_c3_av),
            by = c("opp" = "team_name"),
            relationship = "many-to-many") %>%
  group_by(focal) %>%
  summarise(backup_gms = round(sum(1 - avail), 1),
            healthy    = mean(V_c3),
            priced     = mean(V_c3_av),
            .groups = "drop") %>%
  mutate(rk_healthy = min_rank(desc(healthy)),
         rk_priced  = min_rank(desc(priced)),
         chg        = rk_priced - rk_healthy,
         ne         = if_else(focal == "NE", "<-- NE", "")) %>%
  arrange(rk_priced)

cat("\n== DOES INJURY-PRICING REORDER WHO HAS THE HARDEST SLATE? ==\n")
cat("(adjusted lens, mean across the five slots. rk 1 = HARDEST slate\n")
cat(" to run at. chg = priced rank - healthy rank; positive = the\n")
cat(" slate got HARDER relative to the league once injuries are\n")
cat(" priced. backup gms = expected opponent-backup games across all\n")
cat(" 85 opponent-line slots on the slate.)\n")
print(focal_slate_rb %>%
        mutate(healthy = round(healthy, 3), priced = round(priced, 3)) %>%
        select(tm = focal, "backup gms" = backup_gms,
               "hard rk (healthy)" = rk_healthy,
               "hard rk (priced)" = rk_priced, chg, ne), n = 32)

# ------------------------------------------------------------
# 7. PLAYER BOARDS (Andy 2026-08-19 split: judged only on seasons
#    holding the job; first-time starters price at their
#    population line) plus the team exposure board.
# ------------------------------------------------------------

cat("\n== BOARD 1 -- STARTERS WHO MISS GAMES ==\n")
cat("(the injury-proneness board. exp misses = expected games missed\n")
cat(" per 17, from starter seasons only. lvl = his value; fill lvl =\n")
cat(" what his team dips to when he is out; hit = what injury-pricing\n")
cat(" costs his slot, per lens. Worst 15.)\n")
board_job_rb <- slot_value_26_rb %>%
  filter(avail_src == "starter_history") %>%
  arrange(avail) %>%
  transmute(tm = team_name, slot, player = roster_name,
            "exp misses" = round((1 - avail) * 17, 1),
            "starter yrs" = job_yrs,
            "lvl (adj)" = round(V_c3, 3), "fill lvl" = repl_adj,
            "hit (adj)"  = round(V_c3   - V_c3_av, 3),
            "hit (gap)"  = round(V_gap  - V_gap_av, 3),
            "hit (zone)" = round(V_zone - V_zone_av, 3))
print(head(board_job_rb, 15))

cat("\n== BOARD 2 -- CAREER BACKUPS NOW STARTING ==\n")
cat("(in the league '23-'25 but never held a starting job -- plus\n")
cat(" phantom slots. Priced at the BACKUP first-year line: what\n")
cat(" backups historically keep of year one as a starter, which\n")
cat(" includes the flame-out risk. 'bench app' = share of weeks they\n")
cat(" appeared at all as a backup: context, NOT an injury flag.)\n")
board_bkup_rb <- slot_value_26_rb %>%
  filter(avail_src == "backup_line") %>%
  arrange(avail_all) %>%
  transmute(tm = team_name, slot,
            player = dplyr::coalesce(roster_name, "PHANTOM"), status,
            "lg yrs" = yrs, "bench app" = avail_all,
            "priced at" = avail, "lvl (adj)" = round(V_c3, 3))
print(board_bkup_rb, n = Inf)
cat("career backups / open slots on 2026 lines:", nrow(board_bkup_rb),
    "of", nrow(slot_value_26_rb), "\n")

cat("\n== BOARD 3 -- TRUE ROOKIES: NO NFL RECORD ==\n")
cat("(canon status rookie, i.e. the 2026 entry class. Priced at the\n")
cat(" ROOKIE first-year line: what rookies historically keep of year\n")
cat(" one as a starter. Machine membership DOES see rookies on this\n")
cat(" unit -- the flex/modal rungs carry them.)\n")
board_rook_rb <- slot_value_26_rb %>%
  filter(avail_src == "rookie_line") %>%
  transmute(tm = team_name, slot, player = roster_name,
            "priced at" = avail, "lvl (adj)" = round(V_c3, 3))
print(board_rook_rb, n = Inf)
cat("true rookies on 2026 lines:", nrow(board_rook_rb), "of",
    nrow(slot_value_26_rb), "\n")

cat("\n== BOARD 4 -- WHICH LINES CAN LEAST AFFORD AN INJURY ==\n")
cat("(team-level exposure: starter games expected lost across the\n")
cat(" five slots, the softest slot on the line, the weakest link in\n")
cat(" it, and what a backup there looks like. NE's 2026 opponents\n")
cat(" flagged.)\n")
team_exposure_rb <- slot_value_26_rb %>%
  group_by(team_name) %>%
  summarise(exp_lost = round(sum((1 - avail) * 17), 1),
            .groups = "drop") %>%
  left_join(slot_value_26_rb %>%
              group_by(team_name) %>%
              slice_min(avail, n = 1, with_ties = FALSE) %>%
              mutate(weakest = dplyr::coalesce(roster_name,
                                               "PHANTOM")) %>%
              select(team_name, soft = slot, weakest,
                     fill_there = repl_adj),
            by = "team_name") %>%
  mutate(ne_opp = if_else(team_name %in% sched_2026,
                          "<-- on NE slate", "")) %>%
  arrange(desc(exp_lost))
print(team_exposure_rb %>%
        transmute(tm = team_name, "exp starter gms lost" = exp_lost,
                  "soft spot" = soft, weakest,
                  "fill there" = fill_there, "NE opp" = ne_opp),
      n = 32)

# ------------------------------------------------------------
# 8. THE TEAM-SPLIT VISUAL -- raw vs adjusted, per 2026 opponent
#    and slot. The pass-rush adjusted edition's per-team table
#    (Andy's stamped screenshot, 2026-08-20), ported to OL law:
#    one member per slot, equal-game means, raw faced earned-only.
#    TWO tables -- gap lens and zone lens -- and each carries ITS
#    OWN adjusted columns (round three, Andy 2026-08-30: "Yes --
#    honest '--' holes"): faced '25 valued on that scheme's c3 at
#    the game slot, '26 on that scheme's 24/25 blend with NO prior
#    fill. A player who never earned the scheme currency prints
#    '--' -- holes never fill on this side, and adjusted '25 means
#    run over earned games only (the 'earned' columns show how
#    much of each cell is real). Canon holes print '--' in raw '26
#    as before; each table sorts by ITS OWN adjusted delta,
#    hardest first. The injury-priced family stays OUT of these
#    tables -- it has its own section above; sections 5-7 keep
#    the blended adjusted lens.
# ------------------------------------------------------------

suppressPackageStartupMessages(library(gt))

# -- per-team-x-slot faced 2025: raw earned means (canon law)
#    from the layer's walled rebuild; scheme adj means over EARNED
#    games only + earned shares from the layer's faced c3 cells
#    (round-three honest-hole law) ---------------------------------
faced_team_rb <- faced_ne_rb %>%
  left_join(ol_season_pctl %>%
              select(player_id, det_position, g_gap = gap,
                     g_zone = zone),
            by = c("player_id", "det_position")) %>%
  group_by(team_name, det_position) %>%
  summarise(raw25_gap  = dplyr::na_if(mean(g_gap,  na.rm = TRUE),
                                      NaN),
            raw25_zone = dplyr::na_if(mean(g_zone, na.rm = TRUE),
                                      NaN),
            .groups = "drop") %>%
  full_join(faced_c3_rb %>%
              group_by(team_name, det_position) %>%
              summarise(adj25_gap  = dplyr::na_if(
                mean(c3_25_gap,  na.rm = TRUE), NaN),
                adj25_zone = dplyr::na_if(
                  mean(c3_25_zone, na.rm = TRUE), NaN),
                earned25_gap  = mean(!is.na(c3_25_gap)),
                earned25_zone = mean(!is.na(c3_25_zone)),
                .groups = "drop"),
            by = c("team_name", "det_position"))

# membership wall: faced cells per team x slot must equal canon's
# faced frame exactly -- the final-evaluation R1 pattern.
chk_mem_rb2 <- ne_2025_opp_ol_games %>%
  count(team_name, det_position, name = "n_canon") %>%
  full_join(faced_ne_rb %>%
              count(team_name, det_position, name = "n_mine"),
            by = c("team_name", "det_position")) %>%
  mutate(same = !is.na(n_canon) & !is.na(n_mine) &
           n_canon == n_mine)
if (!all(chk_mem_rb2$same)) {
  cat("\n--- team-split faced membership mismatch (canon vs",
      "rebuild) ---\n")
  print(chk_mem_rb2 %>% filter(!same), n = Inf)
  stop("team-split faced membership != canon -- report, do not",
       " proceed")
}

# -- per-team-x-slot 2026: canon members; raw holes inherited,
#    adjusted cells earned-or-hole (round-three law) -------------
team26_split_rb <- proj_c3_rb %>%
  transmute(team_name, det_position,
            raw26_gap  = gap_f,
            raw26_zone = zone_f,
            adj26_gap  = c3_f_gap,
            adj26_zone = c3_f_zone,
            earned26_gap  = as.numeric(!is.na(c3_f_gap)),
            earned26_zone = as.numeric(!is.na(c3_f_zone)))

cmp_team_rb <- team26_split_rb %>%
  left_join(faced_team_rb, by = c("team_name", "det_position")) %>%
  mutate(d_gap      = raw26_gap  - raw25_gap,
         d_zone     = raw26_zone - raw25_zone,
         d_adj_gap  = adj26_gap  - adj25_gap,
         d_adj_zone = adj26_zone - adj25_zone)

# each table sorts by ITS OWN adjusted delta, hardest first
team_ord_gap_rb <- cmp_team_rb %>%
  group_by(team_name) %>%
  summarise(o = dplyr::na_if(mean(d_adj_gap, na.rm = TRUE), NaN),
            .groups = "drop") %>%
  arrange(desc(dplyr::coalesce(o, -Inf))) %>% pull(team_name)
team_ord_zone_rb <- cmp_team_rb %>%
  group_by(team_name) %>%
  summarise(o = dplyr::na_if(mean(d_adj_zone, na.rm = TRUE), NaN),
            .groups = "drop") %>%
  arrange(desc(dplyr::coalesce(o, -Inf))) %>% pull(team_name)

cat("\n-- the team split: what does each 2026 opponent's run",
    "blocking look like, vs what NE faced from it in 2025? --\n")
print(cmp_team_rb %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))),
      n = Inf)

# -- table 1: GAP lens (raw + GAP adjusted) ----------------------
gt_gap_team_rb <- cmp_team_rb %>%
  mutate(team_name = factor(team_name, levels = team_ord_gap_rb)) %>%
  arrange(team_name, match(det_position, ol_pos_levels)) %>%
  select(team_name, det_position, raw25_gap, raw26_gap, d_gap,
         adj25_gap, adj26_gap, d_adj_gap, earned25_gap, earned26_gap) %>%
  gt(groupname_col = "team_name") %>%
  tab_spanner(label = "Raw",
              columns = c(raw25_gap, raw26_gap, d_gap)) %>%
  tab_spanner(label = "Adjusted (same-slate, gap)",
              columns = c(adj25_gap, adj26_gap, d_adj_gap)) %>%
  cols_label(det_position = "",
             raw25_gap = "'25", raw26_gap = "'26",
             d_gap = "\u0394",
             adj25_gap = "'25", adj26_gap = "'26",
             d_adj_gap = "\u0394",
             earned25_gap = "earned '25",
             earned26_gap = "earned '26") %>%
  fmt_percent(columns = c(raw25_gap, raw26_gap, adj25_gap, adj26_gap,
                          earned25_gap, earned26_gap), decimals = 0) %>%
  fmt_percent(columns = c(d_gap, d_adj_gap), decimals = 0,
              force_sign = TRUE) %>%
  sub_missing(missing_text = "--") %>%
  data_color(columns = c(d_gap, d_adj_gap),
             fn = scales::col_numeric(
               c("#6baed6", "#f7f7f7", "#C60C30"),
               domain = c(-0.3, 0.3), na.color = "#f7f7f7"),
             autocolor_text = TRUE) %>%
  tab_header(
    title = "Opposing run-blocking, per team \u2014 raw vs adjusted",
    subtitle = "'--' = not faced in 2025 ('25 side), canon hole (raw '26), or no earned gap currency (adjusted side \u2014 holes never fill) | adjusted '25 = mean over earned games | teams sorted by adjusted \u0394, hardest first | gap lens") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold",
              row_group.font.weight = "bold")
print(gt_gap_team_rb)

# -- table 2: ZONE lens (raw + ZONE adjusted) ---------------------
gt_zone_team_rb <- cmp_team_rb %>%
  mutate(team_name = factor(team_name, levels = team_ord_zone_rb)) %>%
  arrange(team_name, match(det_position, ol_pos_levels)) %>%
  select(team_name, det_position, raw25_zone, raw26_zone, d_zone,
         adj25_zone, adj26_zone, d_adj_zone, earned25_zone, earned26_zone) %>%
  gt(groupname_col = "team_name") %>%
  tab_spanner(label = "Raw",
              columns = c(raw25_zone, raw26_zone, d_zone)) %>%
  tab_spanner(label = "Adjusted (same-slate, zone)",
              columns = c(adj25_zone, adj26_zone, d_adj_zone)) %>%
  cols_label(det_position = "",
             raw25_zone = "'25", raw26_zone = "'26",
             d_zone = "\u0394",
             adj25_zone = "'25", adj26_zone = "'26",
             d_adj_zone = "\u0394",
             earned25_zone = "earned '25",
             earned26_zone = "earned '26") %>%
  fmt_percent(columns = c(raw25_zone, raw26_zone, adj25_zone, adj26_zone,
                          earned25_zone, earned26_zone), decimals = 0) %>%
  fmt_percent(columns = c(d_zone, d_adj_zone), decimals = 0,
              force_sign = TRUE) %>%
  sub_missing(missing_text = "--") %>%
  data_color(columns = c(d_zone, d_adj_zone),
             fn = scales::col_numeric(
               c("#6baed6", "#f7f7f7", "#C60C30"),
               domain = c(-0.3, 0.3), na.color = "#f7f7f7"),
             autocolor_text = TRUE) %>%
  tab_header(
    title = "Opposing run-blocking, per team \u2014 raw vs adjusted",
    subtitle = "'--' = not faced in 2025 ('25 side), canon hole (raw '26), or no earned zone currency (adjusted side \u2014 holes never fill) | adjusted '25 = mean over earned games | teams sorted by adjusted \u0394, hardest first | zone lens") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold",
              row_group.font.weight = "bold")
print(gt_zone_team_rb)

cat("\nnote: the viewer shows ONE gt table at a time -- each new",
    "table replaces the last.\n  both tables stay in session;",
    "bring either back with one line:\n",
    '  print(gt_gap_team_rb)    # gap lens: raw + gap adjusted\n',
    '  print(gt_zone_team_rb)   # zone lens: raw + zone adjusted\n',
    sep = "")

# Checkpoint: gtsave("run_block_team_split_gap.png", gt_gap_team_rb,
#                    vwidth = 900, vheight = 900)

# ------------------------------------------------------------
# Checkpoint (after eyeball):
# - team-split visual (raw vs adjusted, per opponent and slot)
#   added 2026-08-20 as section 8 -- the stamped pass-rush table,
#   ported to OL law.
# - canon has NO run-block final evaluation; this file currently
#   IS the adjusted + priced run-block home. If Andy wants the
#   full final-evaluation treatment (cmp_adj-style answer table,
#   GT with Raw / Adjusted / Injury-priced spanners, slate plot),
#   that is its own stamp -- say the word and it gets built as
#   league_run_block_final_evaluation.R canon-style, with this
#   layer folded in as the third family.
# - knob stamp pending: AVAIL_SNAP_MIN_RB = 8 run-block snaps
#   (unit adaptation 1-2). Everything else carries the OL stamp.
# ------------------------------------------------------------
