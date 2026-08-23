# ============================================================
# SECONDARY SLATE -- AVAILABILITY LAYER, v1
# Andy directive 2026-08-21: "overall -- most / all of the decisions
# you made are good ... man / zone should be in the same file maybe
# ... let's give this a start." ONE FILE, BOTH SPLITS, NEVER
# RECOMBINED. Prices EXPECTED injuries into the 2026 side so both
# sides of the slate carry the same health expectation.
#
# WHAT CARRIES THE OL/PASS-RUSH STAMP (Andy 2026-08-19, "THESE
# ASSUMPTIONS ARE ALL ACCEPTABLE"):
#   - the pricing law, the print law, the faced-side-untouched law,
#     the firewall, the first-year-lines fill, the population split
#   - knob VALUES: window 2023-2025, denominator 17 REG games.
# SIX UNIT ADAPTATIONS (Kimi, STAMPED by Andy 2026-08-21):
#   1. "played" = the canon weekly SPLIT floors: >= 6 man coverage
#      snaps that week (man split), >= 13 zone coverage snaps (zone
#      split). The canon gates X_MAN_SEC / X_ZONE_SEC ARE the floors
#      -- this file references them, it never re-strikes the numbers.
#      Availability HISTORY stays REG (week <= 18, denominator 17) --
#      stamped layer law, distinct from canon's currency law.
#   2. "held the job" = the season committee: top N_SEC_LG[band]
#      (CB 3, SCB 1, S 3, LB 2 -- canon depth) per team x band x
#      split by qualifying split snaps, band = season-modal band
#      (canon band law, modal_band_sec_lg). A traded player who made
#      a committee at ANY team held a job that season (collapse per
#      player x season x split).
#   3. SNAP-WEIGHTED everywhere (split snaps) -- the pass-rush
#      weighting, not rushing's attempts, not OL equal-vote.
#   4. THREE RAW LENSES + ADJUSTED, per split, printed separately,
#      NEVER recombined: coverage grade / suppression / pbu (raw
#      family) and within-opponent rank (adjusted family). The
#      adjusted slate for this unit did not exist in canon -- the
#      2026-08-21 stamp IS the design turn (handoff section 4
#      deferral lifted).
#   5. repl = empirical 2025 BACKUP defender-games per band x split,
#      snap-weighted, with ARM-PRIOR fills (rookie if entry_year ==
#      2025, else vet) from canon's prior law; first-year lines per
#      band x split with pooled same-split fallback + evidence
#      column (2023 first-year members excluded -- window edge).
#   6. History 2023-2025, denominator 17, FACED-2025 UNTOUCHED
#      (its injuries are realized in the membership; pricing faced
#      would double-count).
# PRIORS: the adjusted family gets a per-split c3 ENTRY PRIOR
#   (median entry-season c3, entry >= 2017, per band x split) --
#   the pass-rush pattern, one cell per band x split, with a
#   same-split pooled rung if a cell is empty (splits never
#   recombined; bands pool within a split, which is legal).
# FACET LAW (canon, printed with each table): grade seats
#   everywhere; man supp is NOISE-flagged (printed, never seated);
#   zone supp seats for CB/S, is dead for SCB, context for LB;
#   pbu is context everywhere.
#
# THE PRICING LAW:
#   value_26_priced = avail * value_26 + (1 - avail) * repl(band, split)
#   avail = games-played rate 0-1, judged ONLY on seasons the player
#           HELD a committee job (top N_SEC_LG at his band, that
#           split, that year). Zero committee seasons -> the observed
#           FIRST-YEAR-COMMITTEE rate for HIS population (Andy
#           2026-08-19 ruling: career backups priced at the backup
#           thing; rookies at the rookie line), measured from 2024-25
#           first-year committee members at that band x split.
#   FIREWALL: raw blends with the raw replacement level, adjusted
#           with the adjusted replacement level; the priced family
#           stays its own always-labeled family.
#   NOT SEEN: current 2026 injuries beyond roster membership
#           (canon's load_rosters handles known season-enders).
#
# PRINT LAW (Andy 2026-08-19): short English phrases with spaces;
# no suffix algebra; one lens per table; every table opens with the
# question it answers. Internals keep contract names.
#
# CROSSED-STREAMS (ruled 2026-08-20): this file reads UNIT-UNIQUE
# names only (_mz / _sec_lg / cov_c3_* / session canon). It never
# reads another unit's faced/ledger/rot objects, never edits canon,
# and works in any session order once SOURCE ORDER below has run.
# Player ids for 2026 members are recovered through ros_sec_lg
# (canon's own roster route, team + roster_name) -- NEVER ledger_mz.
#
# SOURCE ORDER: phase6_step0_secondary_gate_ritual (cache), then
# league_opp_secondary_schedule (full -- NE sections plus league
# tail), then league_secondary_evaluating_currency_three (cov_c3_*),
# then this file.
# ============================================================

suppressPackageStartupMessages(library(dplyr))

needed_cv <- c("sec_qbgrp_lg", "modal_band_sec_lg", "cov_pctl_sec_lg",
               "prior_law_sec_lg", "rot_2026_mz", "rot_2026_sec_lg",
               "cmp_slate_mz", "faced_games_sec_lg", "faced_band_sec_lg",
               "faced_unit_sec_lg", "team_band_sec_lg", "team_unit_sec_lg",
               "band_wt_sec_lg", "slate_band_sec_lg", "opp26_sec",
               "league_cov", "ent_sec_lg", "ros_sec_lg",
               "cov_c3_pctl", "sched_2026", "blend2", "in_season",
               "percent_rank_avg", "X_MAN_SEC", "X_ZONE_SEC",
               "G_MAN_SEC", "G_ZONE_SEC", "N_SEC_LG", "BANDS_SEC_LG",
               "pff32_sec", "CM", "CZ")
missing_cv <- needed_cv[!vapply(needed_cv, exists, logical(1))]
if (length(missing_cv)) stop("missing session objects: ",
                             paste(missing_cv, collapse = ", "),
                             " -- see SOURCE ORDER in header")

# column receipts (crossed-streams guard): shapes, not just names.
# On a miss: print the actual columns, stop loud, report -- do not
# proceed. Split columns are resolved LIVE from canon's card
# (CM / CZ), never hardcoded.
req_cols_cv <- list(
  sec_qbgrp_lg = c("player_id", "player", "season", "week", "team",
                   "band4", "qbgrp_ssn",
                   unname(CM["snaps"]), unname(CZ["snaps"])),
  modal_band_sec_lg = c("player_id", "season", "band"),
  cov_pctl_sec_lg = c("player_id", "band", "season", "split",
                      "q_games", "q_snaps", "grade_pctl", "supp_pctl",
                      "pbu_pctl"),
  prior_law_sec_lg = c("split", "band", "rookie_grade", "vet_grade",
                       "pr_supp", "pr_pbu"),
  rot_2026_mz = c("team_name", "roster_name", "band", "status",
                  "prior_used", "usage_w", "grade_f", "supp_f",
                  "pbu_f", "split"),
  rot_2026_sec_lg = c("team", "roster_name", "band", "status",
                      "prior_used", "usage_w", "grade_f", "supp_f",
                      "pbu_f", "split"),
  cmp_slate_mz = c("split", "band", "grade_25", "supp_25", "pbu_25",
                   "fill_share_25", "grade_26", "supp_26", "pbu_26",
                   "interp_share"),
  faced_games_sec_lg = c("player_id", "week", "team", "band",
                         "g_snaps", "g25", "s25", "fill", "split",
                         "focal"),
  faced_band_sec_lg = c("focal", "split", "band", "grade_25",
                        "supp_25", "fill_share_25"),
  faced_unit_sec_lg = c("focal", "split", "unit_25", "fill_share_25"),
  team_band_sec_lg = c("split", "team", "band", "interp_share",
                       "grade_26", "supp_26", "pbu_26"),
  team_unit_sec_lg = c("split", "team", "unit_26", "interp_share"),
  band_wt_sec_lg = c("split", "band", "wt"),
  slate_band_sec_lg = c("focal", "split", "band", "grade_26",
                        "supp_26", "interp_share"),
  opp26_sec = c("focal", "opp"),
  league_cov = c("focal", "split", "unit_25", "unit_26",
                 "interp_share", "d_grade"),
  ent_sec_lg = c("player_id", "entry_year"),
  ros_sec_lg = c("gsis_id", "roster_name", "team", "entry_year",
                 "roster_pos", "player_id"),
  cov_c3_pctl = c("player_id", "season", "band", "split", "c3_pctl",
                  "qual_g"))
for (nm_cv in names(req_cols_cv)) {
  fr_cv <- get(nm_cv)
  miss_cv2 <- setdiff(req_cols_cv[[nm_cv]], names(fr_cv))
  if (length(miss_cv2)) {
    cat("\nframe:", nm_cv, "-- missing columns:\n"); print(miss_cv2)
    cat("actual columns:\n"); print(names(fr_cv))
    stop("column receipt printed above -- crossed-streams guard")
  }
}

# unit identity: secondary bands + the two splits only. If another
# unit's rot/canon frame is squatting on one of these names, this
# fires before any join.
stopifnot(all(rot_2026_mz$band %in% BANDS_SEC_LG),
          all(rot_2026_sec_lg$band %in% BANDS_SEC_LG),
          all(rot_2026_mz$split %in% c("man", "zone")),
          all(rot_2026_sec_lg$split %in% c("man", "zone")),
          nrow(rot_2026_sec_lg) == 32L * sum(N_SEC_LG) * 2L)

# intake note: every frame above is already a tibble -- canon
# coerces at its own intake (coverage_qbgrp_mz is as_tibble'd at
# birth and every downstream frame is dplyr-built). This layer
# NEVER re-assigns a canon name (crossed-streams); if a frame
# arrives as a bare data.frame the wall below stops loud.
not_tibble_cv <- names(req_cols_cv)[
  !vapply(names(req_cols_cv),
          function(nm) inherits(get(nm), "tbl_df"), logical(1))]
if (length(not_tibble_cv)) {
  cat("frames arriving non-tibble:", paste(not_tibble_cv,
                                           collapse = ", "), "\n")
  stop("intake class check failed -- report, do not proceed")
}

# ------------------------------------------------------------
# 0. KNOBS. Window + denominator carry the OL stamp; the played
#    floors are canon's own split gates, referenced not re-struck.
# ------------------------------------------------------------
AVAIL_SEASONS_CV <- 2023:2025  # availability history window
AVAIL_DENOM_CV   <- 17L        # REG games per season

SPLIT_FLOOR_CV <- c(man = X_MAN_SEC, zone = X_ZONE_SEC)

cat("\n== SECONDARY AVAILABILITY LAYER v1 == knobs: seasons",
    paste(range(AVAIL_SEASONS_CV), collapse = "-"),
    "| played = split snaps >=", X_MAN_SEC, "(man) /",
    X_ZONE_SEC, "(zone) -- canon's own gates | denominator",
    AVAIL_DENOM_CV, "REG games ==\n")
cat("one file, both splits, never recombined\n")

# ------------------------------------------------------------
# 0b. THE c3 ENTRY PRIOR per band x split (adjusted family's fill
#     price): median entry-season c3 among defenders whose entry
#     season is 2017+. Rung law: band x split cell; an EMPTY cell
#     falls to the same-split pooled entry median (bands pool
#     within a split -- splits never recombined) and says so in
#     the evidence column. Thin cells (n < 10) stand but print.
# ------------------------------------------------------------

c3_entry_pool_cv <- cov_c3_pctl %>%
  inner_join(ent_sec_lg, by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2017)

c3_entry_cell_cv <- c3_entry_pool_cv %>%
  group_by(split, band) %>%
  summarise(pr_c3 = median(c3_pctl, na.rm = TRUE),
            n_entry = dplyr::n(), .groups = "drop")

c3_entry_split_cv <- c3_entry_pool_cv %>%
  group_by(split) %>%
  summarise(pr_c3_split = median(c3_pctl, na.rm = TRUE),
            n_split = dplyr::n(), .groups = "drop")

c3_entry_prior_cv <- tidyr::expand_grid(
  split = c("man", "zone"), band = BANDS_SEC_LG) %>%
  left_join(c3_entry_cell_cv, by = c("split", "band")) %>%
  left_join(c3_entry_split_cv, by = "split") %>%
  mutate(evidence = if_else(is.na(pr_c3),
                            paste0("no band cell -- same-split pooled (n=",
                                   n_split, ")"),
                            paste0(n_entry, " entry seasons")),
         pr_c3 = dplyr::coalesce(pr_c3, pr_c3_split)) %>%
  select(split, band, pr_c3, n_entry, evidence)
if (any(is.na(c3_entry_prior_cv$pr_c3))) {
  print(c3_entry_prior_cv)
  stop("c3 entry prior NA even after the same-split pooled rung -- report")
}

cat("\n--- the adjusted family's entry price: what does a defender's",
    "first gated season look like, per band x split? ---\n")
print(c3_entry_prior_cv %>%
        transmute(split, band, "entry price (adj)" = round(pr_c3, 3),
                  n_entry, evidence))

# ------------------------------------------------------------
# 1. avail per player PER SPLIT. Played week = the canon split
#    floor (6 man / 13 zone snaps), REG weeks only. Job = season
#    committee: top N_SEC_LG[band] per team x band x split by
#    qualifying split snaps, band = season-modal band. A traded
#    player who made ANY team's committee held a job that season.
#    Two rates: avail_all = every season with a played week;
#    avail_job = only seasons he HELD a committee job.
# ------------------------------------------------------------

reg_cv <- sec_qbgrp_lg %>%
  filter(season %in% AVAIL_SEASONS_CV, week <= 18)

one_split_hist_cv <- function(sp) {
  C <- if (sp == "man") CM else CZ
  x <- SPLIT_FLOOR_CV[[sp]]
  played <- reg_cv %>%
    filter(.data[[C["snaps"]]] >= x) %>%
    distinct(player_id, season, week)
  committee <- reg_cv %>%
    filter(.data[[C["snaps"]]] >= x) %>%
    group_by(season, team, player_id) %>%
    summarise(qg = dplyr::n(),
              qsn = sum(.data[[C["snaps"]]], na.rm = TRUE),
              .groups = "drop") %>%
    inner_join(modal_band_sec_lg, by = c("player_id", "season")) %>%
    group_by(season, team, band) %>%
    arrange(desc(qsn), desc(qg), .by_group = TRUE) %>%
    mutate(rk = dplyr::row_number()) %>%
    filter(rk <= N_SEC_LG[band]) %>%
    ungroup()
  job_map <- committee %>% distinct(season, player_id) %>%
    mutate(had_job = TRUE)
  avail <- played %>%
    count(player_id, season, name = "g_p") %>%
    mutate(avail_s = pmin(g_p / AVAIL_DENOM_CV, 1)) %>%
    left_join(job_map, by = c("player_id", "season")) %>%
    mutate(had_job = !is.na(had_job)) %>%
    group_by(player_id) %>%
    summarise(yrs       = dplyr::n(),
              job_yrs   = sum(had_job),
              avail_all = mean(avail_s),
              avail_job = if (any(had_job)) mean(avail_s[had_job])
              else NA_real_,
              .groups = "drop")
  list(played = played, committee = committee, avail = avail)
}

hist_man_cv  <- one_split_hist_cv("man")
hist_zone_cv <- one_split_hist_cv("zone")

played_wk_cv <- bind_rows(hist_man_cv$played %>% mutate(split = "man"),
                          hist_zone_cv$played %>% mutate(split = "zone"))
committee_cv <- bind_rows(hist_man_cv$committee %>% mutate(split = "man"),
                          hist_zone_cv$committee %>% mutate(split = "zone"))
avail_cv <- bind_rows(hist_man_cv$avail %>% mutate(split = "man"),
                      hist_zone_cv$avail %>% mutate(split = "zone"))

cat("\n--- durability among defenders who HELD a committee job at",
    "least once, per split ---\n")
for (sp in c("man", "zone")) {
  cat("[", sp, "]\n", sep = "")
  print(quantile(avail_cv$avail_job[avail_cv$split == sp],
                 probs = c(0, .1, .25, .5, .75, .9, 1),
                 na.rm = TRUE) %>% round(3))
}

# ------------------------------------------------------------
# 2. ESTABLISHED committee-member availability by band x split --
#    the incumbent benchmark, measured over every 2023-25 job
#    season. Reference only; NOT the first-year fill (Andy
#    2026-08-19 ruling) -- the fills are the lines in 2b.
# ------------------------------------------------------------

bench_cv <- committee_cv %>%
  left_join(played_wk_cv %>%
              count(player_id, season, split, name = "g_p"),
            by = c("player_id", "season", "split")) %>%
  mutate(avail_s = pmin(g_p / AVAIL_DENOM_CV, 1)) %>%
  group_by(split, band) %>%
  summarise(job_seasons = dplyr::n(),
            bench_avail = mean(avail_s, na.rm = TRUE),
            .groups = "drop")

# sequential-masking law: counts in summarise, ratios in mutate,
# uniquely named intermediates -- a later summarise() expression
# would see the JUST-CREATED job_seasons column, not the upstream
# vector (the 2026-08-21 bench_split receipt).
bench_split_cv <- bench_cv %>%
  mutate(bs_wsum = bench_avail * job_seasons) %>%
  group_by(split) %>%
  summarise(job_seasons = sum(job_seasons),
            bs_wsum = sum(bs_wsum),
            .groups = "drop") %>%
  mutate(bench_avail = bs_wsum / job_seasons) %>%
  select(split, job_seasons, bench_avail)

cat("\n--- how durable is an ESTABLISHED committee defender at each",
    "band, per split? (benchmark -- not the first-year fill) ---\n")
print(bench_cv %>%
        transmute(split, band, "job seasons" = job_seasons,
                  "committee avail" = round(bench_avail, 3)))
cat("\nsplit-level benchmark (all job seasons pooled within split):\n")
print(bench_split_cv %>%
        transmute(split, "job seasons" = job_seasons,
                  "committee avail" = round(bench_avail, 3)))
if (any(is.na(bench_cv$bench_avail))) {
  stop("committee benchmark availability NA -- report")
}

# ------------------------------------------------------------
# 2b. THE FIRST-YEAR LINES (Andy 2026-08-19 ruling), per band x
#     split. Measured from 2024-25 first-year committee members:
#     when a defender holds a committee job in this split for the
#     FIRST time, what share of that season does he keep -- split
#     by population. 2023 first-year members excluded: the window
#     cannot tell their population. An empty band x split cell
#     falls to that population's pooled SAME-SPLIT line, and says
#     so in the evidence column.
# ------------------------------------------------------------

seen_2023_cv <- reg_cv %>% filter(season == 2023) %>%
  distinct(player_id) %>% pull(player_id)
seen_2324_cv <- reg_cv %>% filter(season %in% 2023:2024) %>%
  distinct(player_id) %>% pull(player_id)

comm_seasons_cv <- committee_cv %>% distinct(player_id, season, split)

first_year_cv <- bind_rows(
  committee_cv %>%
    filter(season == 2024) %>%
    anti_join(comm_seasons_cv %>% filter(season == 2023),
              by = c("player_id", "split")),
  committee_cv %>%
    filter(season == 2025) %>%
    anti_join(comm_seasons_cv %>% filter(season %in% 2023:2024),
              by = c("player_id", "split"))) %>%
  distinct(player_id, season, split, .keep_all = TRUE) %>%
  mutate(pop = case_when(
    season == 2024 & player_id %in% seen_2023_cv ~ "backup",
    season == 2025 & player_id %in% seen_2324_cv ~ "backup",
    TRUE ~ "rookie")) %>%
  left_join(played_wk_cv %>%
              filter(season %in% 2024:2025) %>%
              count(player_id, season, split, name = "g_p"),
            by = c("player_id", "season", "split")) %>%
  mutate(g_p  = dplyr::coalesce(g_p, 0L),
         rate = pmin(g_p / AVAIL_DENOM_CV, 1))

first_year_cells_cv <- first_year_cv %>%
  group_by(split, band, pop) %>%
  summarise(n = dplyr::n(), line = mean(rate), .groups = "drop")

first_year_unit_cv <- first_year_cv %>%
  group_by(split, pop) %>%
  summarise(n_unit = dplyr::n(), unit_line = mean(rate),
            .groups = "drop")

first_year_lines_cv <- tidyr::expand_grid(
  split = c("man", "zone"), band = BANDS_SEC_LG,
  pop = c("backup", "rookie")) %>%
  left_join(first_year_cells_cv, by = c("split", "band", "pop")) %>%
  left_join(first_year_unit_cv, by = c("split", "pop")) %>%
  mutate(line = dplyr::coalesce(line, unit_line),
         evidence = if_else(is.na(n),
                            paste0("no band evidence -- ", pop,
                                   " same-split line (n=", n_unit, ")"),
                            paste0(n, " first-year members"))) %>%
  select(split, band, pop, line, evidence)
if (any(is.na(first_year_lines_cv$line))) {
  print(first_year_lines_cv)
  stop("first-year lines NA -- report")
}

cat("\n--- the first-year lines: when a defender holds a committee",
    "job in this split for the first time, how much of that season",
    "does he keep? ---\n")
print(first_year_lines_cv %>%
        transmute(split, band, population = pop,
                  "kept rate" = round(line, 3), evidence), n = Inf)

lines_wide_cv <- first_year_lines_cv %>%
  select(split, band, pop, line) %>%
  tidyr::pivot_wider(names_from = pop, values_from = line,
                     names_prefix = "line_")

# ------------------------------------------------------------
# 3. repl(band, split) -- empirical 2025 BACKUP-defender level,
#    REG weeks, per band x split: played-floor games by defenders
#    OUTSIDE their team's 2025 committee in that split.
#    SNAP-WEIGHTED by split snaps (unit-consistent, stamped).
#    Signed fill law, per lens:
#      grade = earned 2025 grade_pctl (that split) else arm prior
#              (entry_year == 2025 -> rookie_grade, else vet_grade)
#      supp  = earned supp_pctl else pr_supp
#      pbu   = earned pbu_pctl else pr_pbu
#      adj   = earned 2025 c3_pctl (that split) else the band x
#              split c3 entry prior (0b)
# ------------------------------------------------------------

one_split_repl_cv <- function(sp) {
  C <- if (sp == "man") CM else CZ
  x <- SPLIT_FLOOR_CV[[sp]]
  comm25 <- committee_cv %>%
    filter(split == sp, season == 2025) %>%
    select(season, team, player_id)
  reg_cv %>%
    filter(season == 2025, .data[[C["snaps"]]] >= x) %>%
    transmute(player_id, week, team,
              g_snaps = .data[[C["snaps"]]]) %>%
    anti_join(comm25, by = c("team", "player_id")) %>%
    inner_join(modal_band_sec_lg %>% filter(season == 2025),
               by = c("player_id")) %>%
    select(-season) %>%
    left_join(cov_pctl_sec_lg %>%
                filter(season == 2025, split == sp) %>%
                select(player_id, g25 = grade_pctl, s25 = supp_pctl,
                       p25 = pbu_pctl),
              by = "player_id") %>%
    left_join(cov_c3_pctl %>%
                filter(season == 2025, split == sp) %>%
                select(player_id, a25 = c3_pctl),
              by = "player_id") %>%
    left_join(ent_sec_lg, by = "player_id") %>%
    left_join(prior_law_sec_lg %>% filter(split == sp) %>%
                select(band, rookie_grade, vet_grade, pr_supp, pr_pbu),
              by = "band") %>%
    left_join(c3_entry_prior_cv %>% filter(split == sp) %>%
                select(band, pr_c3),
              by = "band") %>%
    mutate(arm = if_else(!is.na(entry_year) & entry_year == 2025,
                         "rookie", "vet"),
           raw_g = dplyr::coalesce(g25, if_else(arm == "rookie",
                                                rookie_grade,
                                                vet_grade)),
           raw_s = dplyr::coalesce(s25, pr_supp),
           raw_p = dplyr::coalesce(p25, pr_pbu),
           adj_v = dplyr::coalesce(a25, pr_c3)) %>%
    group_by(band) %>%
    summarise(n_backup = dplyr::n(),
              repl_g = weighted.mean(raw_g, w = g_snaps),
              repl_s = weighted.mean(raw_s, w = g_snaps),
              repl_p = weighted.mean(raw_p, w = g_snaps),
              repl_a = weighted.mean(adj_v, w = g_snaps),
              .groups = "drop") %>%
    mutate(split = sp)
}

repl_level_cv <- bind_rows(one_split_repl_cv("man"),
                           one_split_repl_cv("zone")) %>%
  select(split, band, n_backup, repl_g, repl_s, repl_p, repl_a)

cat("\n--- when a committee defender is out, what do you get? (2025",
    "backup defender-games, snap-weighted, per band x split) ---\n")
print(repl_level_cv %>%
        transmute(split, band, "backup gms seen" = n_backup,
                  "fill lvl (grade)" = round(repl_g, 3),
                  "fill lvl (supp)"  = round(repl_s, 3),
                  "fill lvl (pbu)"   = round(repl_p, 3),
                  "fill lvl (adj)"   = round(repl_a, 3)), n = Inf)
chk_repl_cv <- tidyr::expand_grid(split = c("man", "zone"),
                                  band = BANDS_SEC_LG) %>%
  anti_join(repl_level_cv, by = c("split", "band"))
if (nrow(chk_repl_cv) > 0 ||
    any(is.na(repl_level_cv$repl_g)) ||
    any(is.na(repl_level_cv$repl_a)) ||
    any(repl_level_cv$n_backup == 0L)) {
  cat("empty or NA band x split replacement cells:\n")
  print(chk_repl_cv)
  print(repl_level_cv %>% filter(is.na(repl_g) | is.na(repl_a)))
  stop("replacement level incomplete -- report, do not proceed")
}

# ------------------------------------------------------------
# 4. members_cv -- every 2026 rotation member, ALL 32 TEAMS, both
#    splits: canon's own league rotation frame (rot_2026_sec_lg,
#    ledger-driven, phantom-armed, depth-walled) with the layer
#    appended: player ids recovered through ros_sec_lg (canon's
#    roster route -- NEVER ledger_mz), the adjusted chain (blend2
#    of '25/'24 c3, weighted by gated games, filled at the band x
#    split entry prior), availability tiers, and the priced
#    values. Membership, raw values, and weights are canon's,
#    byte for byte -- the provenance wall below proves the layer
#    changed nothing.
# ------------------------------------------------------------

id_route_cv <- ros_sec_lg %>%
  select(team, roster_name, player_id, entry_year_ros = entry_year)

members_cv_build <- rot_2026_sec_lg %>%
  left_join(id_route_cv, by = c("team", "roster_name"))

# id-recovery wall: every non-phantom member lands exactly one
# roster id (ros_sec_lg is walled unique on team x roster_name at
# birth). Phantoms stay NA by design -> prior, the pass-rush law.
unmatched_cv <- members_cv_build %>%
  filter(status != "phantom", is.na(player_id))
if (nrow(unmatched_cv) > 0) {
  cat("rotation members with no roster id (team / name / split):\n")
  print(unmatched_cv %>%
          select(team, roster_name, band, split, status) %>%
          as.data.frame())
  stop("id recovery incomplete -- report, do not proceed")
}

members_cv <- members_cv_build %>%
  left_join(cov_c3_pctl %>% filter(season == 2025, !is.na(player_id)) %>%
              select(player_id, split, a25 = c3_pctl, qg25a = qual_g),
            by = c("player_id", "split")) %>%
  left_join(cov_c3_pctl %>% filter(season == 2024, !is.na(player_id)) %>%
              select(player_id, split, a24 = c3_pctl),
            by = c("player_id", "split")) %>%
  left_join(c3_entry_prior_cv %>% select(split, band, pr_c3),
            by = c("split", "band")) %>%
  mutate(w25a = pmin(dplyr::coalesce(qg25a, 0L) / 10, 1),
         c3_bl = blend2(a25, a24, w25a),
         prior_c3 = is.na(c3_bl),
         c3_f = dplyr::coalesce(c3_bl, pr_c3)) %>%
  # --- layer-only joins below this line; canon values untouched ---
  left_join(avail_cv %>% filter(!is.na(player_id)) %>%
              select(player_id, split, yrs, job_yrs, avail_all,
                     avail_job),
            by = c("player_id", "split")) %>%
  mutate(avail_src = dplyr::case_when(
    !is.na(avail_job) ~ "starter_history",
    status == "phantom" ~ "backup_line",
    dplyr::coalesce(entry_year_ros, 0) == 2026 |
      status == "rookie" ~ "rookie_line",
    TRUE ~ "backup_line")) %>%
  left_join(lines_wide_cv, by = c("split", "band")) %>%
  mutate(avail = dplyr::case_when(
    avail_src == "starter_history" ~ avail_job,
    avail_src == "backup_line" ~ line_backup,
    TRUE ~ line_rookie)) %>%
  left_join(repl_level_cv, by = c("split", "band")) %>%
  mutate(grade_p = avail * grade_f + (1 - avail) * repl_g,
         supp_p  = avail * supp_f  + (1 - avail) * repl_s,
         pbu_p   = avail * pbu_f   + (1 - avail) * repl_p,
         adj_p   = avail * c3_f    + (1 - avail) * repl_a)

# provenance wall: membership / raw values / weights identical to
# canon's rot frame -- FULL PRECISION both sides, 1e-8.
a_cv <- members_cv %>%
  select(split, team, roster_name, band, prior_used, usage_w,
         grade_f, supp_f, pbu_f) %>%
  arrange(split, team, band, roster_name)
b_cv <- rot_2026_sec_lg %>%
  select(split, team, roster_name, band, prior_used, usage_w,
         grade_f, supp_f, pbu_f) %>%
  arrange(split, team, band, roster_name)
if (nrow(a_cv) != nrow(b_cv) ||
    !identical(a_cv$split, b_cv$split) ||
    !identical(a_cv$team, b_cv$team) ||
    !identical(a_cv$roster_name, b_cv$roster_name) ||
    !identical(a_cv$band, b_cv$band) ||
    !identical(a_cv$prior_used, b_cv$prior_used) ||
    !isTRUE(all.equal(as.data.frame(a_cv[, c("usage_w", "grade_f",
                                             "supp_f", "pbu_f")]),
                      as.data.frame(b_cv[, c("usage_w", "grade_f",
                                             "supp_f", "pbu_f")]),
                      tolerance = 1e-8))) {
  cat("\nrows:", nrow(a_cv), "vs rot_2026_sec_lg:", nrow(b_cv), "\n")
  print(anti_join(a_cv %>% mutate(across(where(is.numeric), round, 6)),
                  b_cv %>% mutate(across(where(is.numeric), round, 6)),
                  by = c("split", "team", "roster_name", "band",
                         "usage_w")))
  stop("members_cv drifted from the canon rotation frame -- report")
}
stopifnot(!any(is.na(members_cv$avail)),
          !any(is.na(members_cv$grade_p)),
          !any(is.na(members_cv$adj_p)))

cat("\nleague rotation priced:", nrow(members_cv),
    "member-rows (32 teams x 9 slots x 2 splits) | tiers:\n")
print(members_cv %>% count(split, avail_src) %>%
        tidyr::pivot_wider(names_from = avail_src, values_from = n) %>%
        as.data.frame())

# ------------------------------------------------------------
# 5. NE SLATE, injury-priced -- CANON NE membership (rot_2026_mz:
#    the ledger build, named rookies kept visible). Ids recovered
#    through ros_sec_lg, same route as section 4. Walls prove the
#    healthy side reproduces cmp_slate_mz at 1e-8 (grade / supp /
#    pbu / interp, 8 cells = 2 splits x 4 bands) and the faced
#    side reproduces canon's faced bands at 1e-8. The tail faced
#    frame carries grade + supp only, so faced pbu is PULLED from
#    cmp_slate_mz (canon), never rebuilt. The adjusted faced side
#    has no canon object (the 2026-08-21 stamp is its design
#    turn): rebuilt here from sec_qbgrp_lg + cov_c3_pctl with the
#    same gates and arm-prior fill law as canon's faced build,
#    and a membership wall proves it stands on exactly canon's
#    NE-faced row set.
# ------------------------------------------------------------

ne_frame_cv <- rot_2026_mz %>%
  left_join(id_route_cv, by = c("team_name" = "team", "roster_name"))

unmatched_ne_cv <- ne_frame_cv %>%
  filter(status != "phantom", is.na(player_id))
if (nrow(unmatched_ne_cv) > 0) {
  cat("NE-slate members with no roster id:\n")
  print(unmatched_ne_cv %>%
          select(team_name, roster_name, band, split, status) %>%
          as.data.frame())
  stop("NE id recovery incomplete -- report, do not proceed")
}

ne_frame_cv <- ne_frame_cv %>%
  left_join(cov_c3_pctl %>% filter(season == 2025, !is.na(player_id)) %>%
              select(player_id, split, a25 = c3_pctl, qg25a = qual_g),
            by = c("player_id", "split")) %>%
  left_join(cov_c3_pctl %>% filter(season == 2024, !is.na(player_id)) %>%
              select(player_id, split, a24 = c3_pctl),
            by = c("player_id", "split")) %>%
  left_join(c3_entry_prior_cv %>% select(split, band, pr_c3),
            by = c("split", "band")) %>%
  mutate(w25a = pmin(dplyr::coalesce(qg25a, 0L) / 10, 1),
         c3_bl = blend2(a25, a24, w25a),
         prior_c3 = is.na(c3_bl),
         c3_f = dplyr::coalesce(c3_bl, pr_c3)) %>%
  left_join(avail_cv %>% filter(!is.na(player_id)) %>%
              select(player_id, split, avail_job),
            by = c("player_id", "split")) %>%
  mutate(avail_src = dplyr::case_when(
    !is.na(avail_job) ~ "starter_history",
    status == "phantom" ~ "backup_line",
    dplyr::coalesce(entry_year_ros, 0) == 2026 |
      status == "rookie" ~ "rookie_line",
    TRUE ~ "backup_line")) %>%
  left_join(lines_wide_cv, by = c("split", "band")) %>%
  mutate(avail = dplyr::case_when(
    avail_src == "starter_history" ~ avail_job,
    avail_src == "backup_line" ~ line_backup,
    TRUE ~ line_rookie)) %>%
  left_join(repl_level_cv, by = c("split", "band")) %>%
  mutate(grade_p = avail * grade_f + (1 - avail) * repl_g,
         supp_p  = avail * supp_f  + (1 - avail) * repl_s,
         pbu_p   = avail * pbu_f   + (1 - avail) * repl_p,
         adj_p   = avail * c3_f    + (1 - avail) * repl_a)
stopifnot(!any(is.na(ne_frame_cv$avail)),
          !any(is.na(ne_frame_cv$grade_p)),
          !any(is.na(ne_frame_cv$adj_p)))

# team x band x split grain, then the equal game mean over the
# 17-game slate -- canon's two-step slate law, mirrored exactly.
tb_ne_cv <- ne_frame_cv %>%
  group_by(team_name, split, band) %>%
  summarise(healthy_g = weighted.mean(grade_f, w = usage_w),
            healthy_s = weighted.mean(supp_f,  w = usage_w),
            healthy_p = weighted.mean(pbu_f,   w = usage_w),
            healthy_a = weighted.mean(c3_f,    w = usage_w),
            priced_g  = weighted.mean(grade_p, w = usage_w),
            priced_s  = weighted.mean(supp_p,  w = usage_w),
            priced_p  = weighted.mean(pbu_p,   w = usage_w),
            priced_a  = weighted.mean(adj_p,   w = usage_w),
            interp_raw = sum(usage_w[prior_used]) / sum(usage_w),
            interp_adj = sum(usage_w[prior_c3]) / sum(usage_w),
            .groups = "drop")

slate_band_ne_cv <- tibble::tibble(team_name = sched_2026) %>%
  left_join(tb_ne_cv, by = "team_name",
            relationship = "many-to-many") %>%
  group_by(split, band) %>%
  summarise(healthy_g = mean(healthy_g), healthy_s = mean(healthy_s),
            healthy_p = mean(healthy_p), healthy_a = mean(healthy_a),
            priced_g  = mean(priced_g),  priced_s  = mean(priced_s),
            priced_p  = mean(priced_p),  priced_a  = mean(priced_a),
            interp_raw = mean(interp_raw),
            interp_adj = mean(interp_adj),
            .groups = "drop")

slate_rows_ne_cv <- tibble::tibble(team_name = sched_2026) %>%
  left_join(ne_frame_cv, by = "team_name",
            relationship = "many-to-many")
if (nrow(slate_rows_ne_cv) !=
    length(sched_2026) * sum(N_SEC_LG) * 2L) {
  cat("expected", length(sched_2026) * sum(N_SEC_LG) * 2L,
      "slate rows, got", nrow(slate_rows_ne_cv), "\n")
  stop("NE slate rows broken -- report")
}
backup_gms_ne_cv <- slate_rows_ne_cv %>%
  group_by(split, band) %>%
  summarise(backup_gms = sum(1 - avail), .groups = "drop")

# THE HEALTHY WALL: layer healthy == cmp_slate_mz, 1e-8, 8 cells.
gate_ne26_cv <- slate_band_ne_cv %>%
  inner_join(cmp_slate_mz %>%
               select(split, band, g_c = grade_26, s_c = supp_26,
                      p_c = pbu_26, i_c = interp_share),
             by = c("split", "band")) %>%
  mutate(dg = abs(healthy_g - g_c), ds = abs(healthy_s - s_c),
         dp = abs(healthy_p - p_c), di = abs(interp_raw - i_c))
stopifnot(nrow(gate_ne26_cv) == 2L * length(BANDS_SEC_LG))
cat("NE healthy wall: max |d grade| =", signif(max(gate_ne26_cv$dg), 3),
    "| max |d supp| =", signif(max(gate_ne26_cv$ds), 3),
    "| max |d pbu| =", signif(max(gate_ne26_cv$dp), 3),
    "| max |d interp| =", signif(max(gate_ne26_cv$di), 3), "\n")
if (max(gate_ne26_cv$dg, gate_ne26_cv$ds, gate_ne26_cv$dp,
        gate_ne26_cv$di) > 1e-8) {
  print(as.data.frame(gate_ne26_cv))
  stop("NE healthy wall failed -- layer does not reproduce canon")
}

# THE FACED WALL: canon tail faced (focal == NE) == cmp_slate_mz
# faced side, grade / supp / fill, 1e-8, 8 cells. pbu faced rides
# cmp_slate_mz directly (the tail faced frame has no pbu).
faced_ne_cv <- faced_band_sec_lg %>%
  filter(focal == "NE")
gate_ne25_cv <- faced_ne_cv %>%
  inner_join(cmp_slate_mz %>%
               select(split, band, g_c = grade_25, s_c = supp_25,
                      f_c = fill_share_25),
             by = c("split", "band")) %>%
  mutate(dg = abs(grade_25 - g_c), ds = abs(supp_25 - s_c),
         df = abs(fill_share_25 - f_c))
stopifnot(nrow(gate_ne25_cv) == 2L * length(BANDS_SEC_LG))
cat("NE faced wall: max |d grade| =", signif(max(gate_ne25_cv$dg), 3),
    "| max |d supp| =", signif(max(gate_ne25_cv$ds), 3),
    "| max |d fill| =", signif(max(gate_ne25_cv$df), 3), "\n")
if (max(gate_ne25_cv$dg, gate_ne25_cv$ds, gate_ne25_cv$df) > 1e-8) {
  print(as.data.frame(gate_ne25_cv))
  stop("NE faced wall failed -- faced side does not reproduce canon")
}

# the adjusted faced side, layer-built on canon's exact row set
faced_c3_one_cv <- function(sp) {
  C <- if (sp == "man") CM else CZ
  x <- SPLIT_FLOOR_CV[[sp]]
  sec_qbgrp_lg %>%
    filter(season == 2025, in_season(week),
           startsWith(qbgrp_ssn, "NE"),
           .data[[C["snaps"]]] >= x) %>%
    transmute(player_id, week, team, band_row = band4,
              g_snaps = .data[[C["snaps"]]]) %>%
    left_join(cov_c3_pctl %>%
                filter(season == 2025, split == .env$sp,
                       !is.na(player_id)) %>%
                select(player_id, band_cur = band, c3 = c3_pctl),
              by = "player_id") %>%
    mutate(band = dplyr::coalesce(band_cur, band_row)) %>%
    left_join(ent_sec_lg, by = "player_id") %>%
    left_join(c3_entry_prior_cv %>% filter(split == .env$sp) %>%
                select(band, pr_c3),
              by = "band") %>%
    mutate(fill = is.na(c3),
           c3 = dplyr::coalesce(c3, pr_c3),
           split = sp)
}
faced_c3_ne_cv <- bind_rows(faced_c3_one_cv("man"),
                            faced_c3_one_cv("zone"))

# membership wall: same player-week-split rows as canon's faced
keys_l_cv <- faced_c3_ne_cv %>% distinct(player_id, week, split)
keys_c_cv <- faced_games_sec_lg %>% filter(focal == "NE") %>%
  distinct(player_id, week, split)
cat("adj faced membership: layer rows", nrow(keys_l_cv),
    "| canon rows", nrow(keys_c_cv), "\n")
if (nrow(dplyr::anti_join(keys_l_cv, keys_c_cv,
                          by = c("player_id", "week", "split"))) > 0 ||
    nrow(dplyr::anti_join(keys_c_cv, keys_l_cv,
                          by = c("player_id", "week", "split"))) > 0) {
  stop("adjusted faced rebuild stands on a different row set -- report")
}

faced_c3_band_ne_cv <- faced_c3_ne_cv %>%
  group_by(split, band) %>%
  summarise(adj_25 = weighted.mean(c3, w = g_snaps),
            fill_adj_25 = sum(g_snaps[fill]) / sum(g_snaps),
            .groups = "drop")

# THE ANSWER FRAME, per split x band
ne_slate_avail_cv <- slate_band_ne_cv %>%
  left_join(faced_ne_cv %>%
              select(split, band, grade_25, supp_25, fill_share_25),
            by = c("split", "band")) %>%
  left_join(cmp_slate_mz %>%
              select(split, band, pbu_25),
            by = c("split", "band")) %>%
  left_join(faced_c3_band_ne_cv, by = c("split", "band")) %>%
  left_join(backup_gms_ne_cv, by = c("split", "band")) %>%
  mutate(d_g    = healthy_g - grade_25,
         d_g_av = priced_g  - grade_25,
         d_s    = healthy_s - supp_25,
         d_s_av = priced_s  - supp_25,
         d_p    = healthy_p - pbu_25,
         d_p_av = priced_p  - pbu_25,
         d_a    = healthy_a - adj_25,
         d_a_av = priced_a  - adj_25) %>%
  arrange(split, match(band, BANDS_SEC_LG))

for (sp in c("man", "zone")) {
  cat("\n== NE 2026 OPPONENT COVERAGE --", toupper(sp),
      "-- how hard will each band be for Maye, once injuries",
      "happen? ==\n")
  cat("(26 healthy = everyone plays 17 | 26 priced = expected",
      "injuries in\n real jump = priced - faced '25 | backup gms =",
      "expected defender-games\n played by backups in that band",
      "across NE's 17 games)\n")
  sl_cv <- ne_slate_avail_cv %>% filter(split == sp)
  
  cat("\nCOVERAGE-GRADE LENS (raw season percentiles)\n")
  print(sl_cv %>%
          transmute(band,
                    "faced '25" = round(grade_25, 3),
                    "26 healthy" = round(healthy_g, 3),
                    "healthy jump" = round(d_g, 3),
                    "26 priced" = round(priced_g, 3),
                    "real jump" = round(d_g_av, 3),
                    "backup gms" = round(backup_gms, 1)))
  
  cat("\nSUPPRESSION LENS (completion-rate percentile, inverted)",
      if (sp == "man")
        "-- facet law: NOISE for man, printed, never seated\n"
      else
        "-- facet law: seated for CB/S, dead for SCB, context for LB\n",
      sep = "")
  print(sl_cv %>%
          transmute(band,
                    "faced '25" = round(supp_25, 3),
                    "26 healthy" = round(healthy_s, 3),
                    "healthy jump" = round(d_s, 3),
                    "26 priced" = round(priced_s, 3),
                    "real jump" = round(d_s_av, 3)))
  
  cat("\nPBU LENS -- facet law: context everywhere\n")
  print(sl_cv %>%
          transmute(band,
                    "faced '25" = round(pbu_25, 3),
                    "26 healthy" = round(healthy_p, 3),
                    "healthy jump" = round(d_p, 3),
                    "26 priced" = round(priced_p, 3),
                    "real jump" = round(d_p_av, 3)))
  
  cat("\nADJUSTED LENS (same-slate skill, the evaluation currency)\n")
  print(sl_cv %>%
          transmute(band,
                    "faced '25" = round(adj_25, 3),
                    "26 healthy" = round(healthy_a, 3),
                    "healthy jump" = round(d_a, 3),
                    "26 priced" = round(priced_a, 3),
                    "real jump" = round(d_a_av, 3),
                    "backup gms" = round(backup_gms, 1)))
}

# ------------------------------------------------------------
# 6. LEAGUE SWEEP -- every focal's slate, healthy vs priced, per
#    split. Canon's two-step law throughout: usage-weighted member
#    means per team x band x split, then the equal mean over the
#    focal's 17 games; unit grain = the canon band-share weighted
#    composite of the grade lens. Walls prove the healthy side
#    reproduces slate_band_sec_lg (256 cells) and league_cov
#    (64 unit rows) at 1e-8. The faced side is canon's
#    faced_unit_sec_lg -- untouched, by law.
# ------------------------------------------------------------

tb_lg_cv <- members_cv %>%
  group_by(team, split, band) %>%
  summarise(healthy_g = weighted.mean(grade_f, w = usage_w),
            healthy_s = weighted.mean(supp_f,  w = usage_w),
            healthy_p = weighted.mean(pbu_f,   w = usage_w),
            healthy_a = weighted.mean(c3_f,    w = usage_w),
            priced_g  = weighted.mean(grade_p, w = usage_w),
            priced_s  = weighted.mean(supp_p,  w = usage_w),
            priced_p  = weighted.mean(pbu_p,   w = usage_w),
            priced_a  = weighted.mean(adj_p,   w = usage_w),
            interp_raw = sum(usage_w[prior_used]) / sum(usage_w),
            interp_adj = sum(usage_w[prior_c3]) / sum(usage_w),
            .groups = "drop")

sweep_band_cv <- opp26_sec %>%
  inner_join(tb_lg_cv, by = c("opp" = "team"),
             relationship = "many-to-many") %>%
  group_by(focal, split, band) %>%
  summarise(healthy_g = mean(healthy_g), priced_g = mean(priced_g),
            healthy_s = mean(healthy_s), priced_s = mean(priced_s),
            healthy_p = mean(healthy_p), priced_p = mean(priced_p),
            healthy_a = mean(healthy_a), priced_a = mean(priced_a),
            interp_raw = mean(interp_raw),
            interp_adj = mean(interp_adj),
            .groups = "drop")

# band-grain wall vs canon's league slate frame
gate_sw_cv <- sweep_band_cv %>%
  inner_join(slate_band_sec_lg %>%
               select(focal, split, band, g_c = grade_26,
                      s_c = supp_26, i_c = interp_share),
             by = c("focal", "split", "band")) %>%
  mutate(dg = abs(healthy_g - g_c), ds = abs(healthy_s - s_c),
         di = abs(interp_raw - i_c))
stopifnot(nrow(gate_sw_cv) == 32L * 2L * length(BANDS_SEC_LG))
cat("\nleague band wall: max |d grade| =", signif(max(gate_sw_cv$dg), 3),
    "| max |d supp| =", signif(max(gate_sw_cv$ds), 3),
    "| max |d interp| =", signif(max(gate_sw_cv$di), 3),
    "over", nrow(gate_sw_cv), "cells\n")
if (max(gate_sw_cv$dg, gate_sw_cv$ds, gate_sw_cv$di) > 1e-8) {
  print(as.data.frame(gate_sw_cv %>%
                        filter(dg > 1e-8 | ds > 1e-8 | di > 1e-8)))
  stop("league band wall failed -- sweep does not reproduce canon")
}

# unit grain: canon chain -- team composite first (band-share
# weighted, grade lens), then the schedule mean
team_unit_cv <- tb_lg_cv %>%
  left_join(band_wt_sec_lg, by = c("split", "band")) %>%
  group_by(team, split) %>%
  summarise(healthy_u = weighted.mean(healthy_g, w = wt),
            priced_u  = weighted.mean(priced_g,  w = wt),
            healthy_a_u = weighted.mean(healthy_a, w = wt),
            priced_a_u  = weighted.mean(priced_a,  w = wt),
            interp_raw = weighted.mean(interp_raw, w = wt),
            .groups = "drop")

sweep_unit_cv <- opp26_sec %>%
  inner_join(team_unit_cv, by = c("opp" = "team"),
             relationship = "many-to-many") %>%
  group_by(focal, split) %>%
  summarise(healthy_u = mean(healthy_u), priced_u = mean(priced_u),
            healthy_a_u = mean(healthy_a_u),
            priced_a_u = mean(priced_a_u),
            interp_raw = mean(interp_raw), .groups = "drop") %>%
  inner_join(faced_unit_sec_lg %>%
               select(focal, split, unit_25, fill_share_25),
             by = c("focal", "split")) %>%
  mutate(d_healthy = healthy_u - unit_25,
         d_priced  = priced_u  - unit_25) %>%
  group_by(split) %>%
  mutate(rk_healthy = rank(-d_healthy, ties.method = "min"),
         rk_priced  = rank(-d_priced,  ties.method = "min")) %>%
  ungroup()

# unit-grain wall vs league_cov: healthy unit + faced unit, 64 rows
gate_su_cv <- sweep_unit_cv %>%
  inner_join(league_cov %>%
               select(focal, split, u26_c = unit_26, u25_c = unit_25,
                      i_c = interp_share),
             by = c("focal", "split")) %>%
  mutate(d26 = abs(healthy_u - u26_c), d25 = abs(unit_25 - u25_c),
         di  = abs(interp_raw - i_c))
stopifnot(nrow(gate_su_cv) == 64L)
cat("league unit wall: max |d unit26| =", signif(max(gate_su_cv$d26), 3),
    "| max |d unit25| =", signif(max(gate_su_cv$d25), 3),
    "| max |d interp| =", signif(max(gate_su_cv$di), 3), "\n")
if (max(gate_su_cv$d26, gate_su_cv$d25, gate_su_cv$di) > 1e-8) {
  print(as.data.frame(gate_su_cv %>%
                        filter(d26 > 1e-8 | d25 > 1e-8 | di > 1e-8)))
  stop("league unit wall failed -- sweep does not reproduce canon")
}

cat("\n== HOW MUCH OF THE 2026 SLATE JUMP WAS THE INJURY ASYMMETRY? ==\n")
cat("League mean jump by band, per split. Positive = the focal's '26\n")
cat("opponents cover better than what the focal's pass game faced in\n")
cat("'25 (playoffs included). 'real jump' is what survives once\n")
cat("expected injuries are priced in.\n")
# the league-wide ADJUSTED faced side: canon's own faced rows
# (faced_games_sec_lg, every focal) valued in c3 with the same
# entry-prior fill law -- no identity parsing here, canon owns the
# focal column. NE cells walled against the section-5 rebuild below.
faced_c3_lg_cv <- faced_games_sec_lg %>%
  left_join(cov_c3_pctl %>%
              filter(season == 2025, !is.na(player_id)) %>%
              select(player_id, split, c3 = c3_pctl),
            by = c("player_id", "split")) %>%
  left_join(c3_entry_prior_cv %>% select(split, band, pr_c3),
            by = c("split", "band")) %>%
  mutate(c3 = dplyr::coalesce(c3, pr_c3)) %>%
  group_by(focal, split, band) %>%
  summarise(adj_25 = weighted.mean(c3, w = g_snaps),
            .groups = "drop")
stopifnot(!any(is.na(faced_c3_lg_cv$adj_25)))

gate_fc3_cv <- faced_c3_lg_cv %>%
  filter(focal == "NE") %>%
  inner_join(faced_c3_band_ne_cv %>%
               select(split, band, a_l = adj_25),
             by = c("split", "band")) %>%
  mutate(da = abs(adj_25 - a_l))
cat("adj faced cross-wall (NE cells, two build routes): max |d| =",
    signif(max(gate_fc3_cv$da), 3), "\n")
if (max(gate_fc3_cv$da) > 1e-8) {
  print(as.data.frame(gate_fc3_cv))
  stop("adjusted faced routes disagree on NE -- report")
}

sweep_band_faced_cv <- sweep_band_cv %>%
  left_join(faced_band_sec_lg %>%
              select(focal, split, band, grade_25, supp_25),
            by = c("focal", "split", "band")) %>%
  left_join(faced_c3_lg_cv, by = c("focal", "split", "band")) %>%
  mutate(d_g    = healthy_g - grade_25,
         d_g_av = priced_g  - grade_25,
         d_a    = healthy_a - adj_25,
         d_a_av = priced_a  - adj_25)
for (sp in c("man", "zone")) {
  cat("\n[", sp, "] GRADE LENS\n", sep = "")
  print(sweep_band_faced_cv %>%
          filter(split == sp) %>%
          group_by(band) %>%
          summarise("healthy jump" = round(mean(d_g, na.rm = TRUE), 3),
                    "real jump"    = round(mean(d_g_av, na.rm = TRUE), 3),
                    .groups = "drop") %>%
          arrange(match(band, BANDS_SEC_LG)))
  cat("[", sp, "] ADJUSTED LENS\n", sep = "")
  print(sweep_band_faced_cv %>%
          filter(split == sp) %>%
          group_by(band) %>%
          summarise("healthy jump" = round(mean(d_a, na.rm = TRUE), 3),
                    "real jump"    = round(mean(d_a_av, na.rm = TRUE), 3),
                    .groups = "drop") %>%
          arrange(match(band, BANDS_SEC_LG)))
}

for (sp in c("man", "zone")) {
  cat("\n== ", toupper(sp),
      " -- WHOSE COVERAGE SLATE MOVES MOST ONCE INJURIES ARE PRICED?",
      " ==\n", sep = "")
  cat("(unit grain, grade lens. swing = priced - healthy; negative =\n")
  cat(" the slate softens -- that focal's opponents are the fragile\n")
  cat(" ones. rk 1 = hardest slate to throw on. chg = priced rank -\n")
  cat(" healthy rank; positive = HARDER relative to the league once\n")
  cat(" injuries are priced. backup gms = expected opponent-backup\n")
  cat(" coverage defender-games across the slate.)\n")
  bup_cv <- opp26_sec %>%
    inner_join(members_cv %>% filter(split == sp) %>%
                 select(team, avail),
               by = c("opp" = "team"),
               relationship = "many-to-many") %>%
    group_by(focal) %>%
    summarise(backup_gms = round(sum(1 - avail), 1), .groups = "drop")
  print(sweep_unit_cv %>%
          filter(split == sp) %>%
          left_join(bup_cv, by = "focal") %>%
          mutate(swing = round(d_priced - d_healthy, 3),
                 ne = if_else(focal == "NE", "<-- NE", "")) %>%
          arrange(rk_priced) %>%
          transmute(tm = focal, "faced '25" = round(unit_25, 3),
                    "26 healthy" = round(healthy_u, 3),
                    "26 priced" = round(priced_u, 3),
                    "real jump" = round(d_priced, 3), swing,
                    "backup gms" = backup_gms,
                    "hard rk (healthy)" = rk_healthy,
                    "hard rk (priced)" = rk_priced,
                    chg = rk_priced - rk_healthy, ne),
        n = 32)
}

# ------------------------------------------------------------
# 7. PLAYER BOARDS (Andy 2026-08-19 split) plus team exposure,
#    PER SPLIT. A defender can sit in both splits' committees --
#    he then counts in both splits' boards, by design.
# ------------------------------------------------------------

for (sp in c("man", "zone")) {
  mb_cv <- members_cv %>% filter(split == sp)
  
  cat("\n== BOARD 1 (", sp, ") -- COMMITTEE DEFENDERS WHO MISS GAMES ==\n",
      sep = "")
  cat("(the injury-proneness board. exp misses = expected games\n")
  cat(" missed per 17, from committee seasons only. lvl = his value;\n")
  cat(" fill lvl = what his team dips to when he is out; hit = what\n")
  cat(" injury-pricing costs his spot, per lens. Worst 15.)\n")
  print(mb_cv %>%
          filter(avail_src == "starter_history") %>%
          arrange(avail) %>%
          transmute(tm = team, band, player = roster_name,
                    "exp misses" = round((1 - avail) * 17, 1),
                    "committee yrs" = job_yrs,
                    "lvl (adj)" = round(c3_f, 3),
                    "fill lvl" = round(repl_a, 3),
                    "hit (adj)" = round(c3_f - adj_p, 3),
                    "hit (grade)" = round(grade_f - grade_p, 3)) %>%
          head(15))
  
  cat("\n== BOARD 2 (", sp, ") -- CAREER BACKUPS NOW IN COMMITTEES ==\n",
      sep = "")
  cat("(in the league '23-'25 but never held a committee job in this\n")
  cat(" split -- plus phantom slots. Priced at the BACKUP first-year\n")
  cat(" line: what backups historically keep of year one in a\n")
  cat(" committee, flame-out risk included. 'bench app' = share of\n")
  cat(" weeks they appeared at all: context, NOT an injury flag.)\n")
  board_bkup_cv <- mb_cv %>%
    filter(avail_src == "backup_line") %>%
    arrange(avail_all) %>%
    transmute(tm = team, band, player = roster_name,
              "lg yrs" = yrs, "bench app" = round(avail_all, 3),
              "priced at" = round(avail, 3),
              "lvl (adj)" = round(c3_f, 3))
  print(board_bkup_cv, n = Inf)
  cat("career backups / phantom slots in 2026", sp, "committees:",
      nrow(board_bkup_cv), "of", nrow(mb_cv), "\n")
  
  cat("\n== BOARD 3 (", sp, ") -- TRUE ROOKIES: NO NFL RECORD ==\n",
      sep = "")
  cat("(no '23-'25 rows at all. Priced at the ROOKIE first-year\n")
  cat(" line: what rookies historically keep of year one in a\n")
  cat(" committee. This unit's canon membership is ledger-driven, so\n")
  cat(" named rookies ARE the committee rows here.)\n")
  board_rook_cv <- mb_cv %>%
    filter(avail_src == "rookie_line") %>%
    transmute(tm = team, band, player = roster_name,
              "priced at" = round(avail, 3),
              "lvl (adj)" = round(c3_f, 3))
  print(board_rook_cv, n = Inf)
  cat("true rookies in 2026", sp, "committees:", nrow(board_rook_cv),
      "of", nrow(mb_cv), "\n")
  
  cat("\n== BOARD 4 (", sp,
      ") -- WHICH SECONDARIES CAN LEAST AFFORD AN INJURY ==\n",
      sep = "")
  cat("(team-level exposure: committee-member games expected lost\n")
  cat(" across the 9 slots, the softest spot in the secondary, and\n")
  cat(" what a backup there looks like. NE's 2026 opponents flagged.)\n")
  print(mb_cv %>%
          group_by(team) %>%
          summarise(exp_lost = round(sum((1 - avail) * 17), 1),
                    .groups = "drop") %>%
          left_join(mb_cv %>%
                      group_by(team) %>%
                      slice_min(avail, n = 1, with_ties = FALSE) %>%
                      transmute(team, soft = band,
                                weakest = roster_name,
                                fill_there = round(repl_a, 3)),
                    by = "team") %>%
          mutate(ne_opp = if_else(team %in% sched_2026,
                                  "<-- on NE slate", "")) %>%
          arrange(desc(exp_lost)) %>%
          transmute(tm = team, "exp committee gms lost" = exp_lost,
                    "soft spot" = soft, weakest,
                    "fill there" = fill_there, "NE opp" = ne_opp),
        n = 32)
}

# ------------------------------------------------------------
# 8. THE TEAM-SPLIT VISUAL (Andy 2026-08-20 stamp, ported from the
#    pass-rush adjusted edition; rushing/run-block/pass-block carry
#    the same section): per opponent team x band, the coverage unit
#    NE faced in 2025 vs the 2026 unit NE will face -- raw and
#    adjusted side by side, ONE TABLE PER SPLIT, grade lens only.
#    Teams = NE's 14 unique 2026 opponents; groupname rows per
#    team, one row per band (56 rows per table). Splits never
#    recombined. Walls: the healthy side reproduces
#    team_band_sec_lg at 1e-8 for ALL 32 teams (not just the 14);
#    the faced side re-pooled to split x band reproduces canon's
#    NE faced bands at 1e-8 (components must sum to the walled
#    whole); the adjusted faced side passes the same components
#    wall against its own game rows.
# ------------------------------------------------------------

suppressPackageStartupMessages(library(gt))

opp_teams_cv <- sort(unique(sched_2026))

# healthy side, team x band x split (full league -- the wall wants
# all 32, the tables keep the 14)
team26_split_cv <- members_cv %>%
  group_by(team, split, band) %>%
  summarise(raw26 = weighted.mean(grade_f, w = usage_w),
            adj26 = weighted.mean(c3_f,    w = usage_w),
            interp_raw = sum(usage_w[prior_used]) / sum(usage_w),
            interp_adj = sum(usage_w[prior_c3]) / sum(usage_w),
            .groups = "drop")

gate_tb_cv <- team26_split_cv %>%
  inner_join(team_band_sec_lg %>%
               select(split, team, band, g_c = grade_26,
                      i_c = interp_share),
             by = c("split", "team", "band")) %>%
  mutate(dg = abs(raw26 - g_c), di = abs(interp_raw - i_c))
stopifnot(nrow(gate_tb_cv) == 32L * 2L * length(BANDS_SEC_LG))
cat("\nteam healthy wall (visual): max |d grade| =",
    signif(max(gate_tb_cv$dg), 3), "| max |d interp| =",
    signif(max(gate_tb_cv$di), 3), "over", nrow(gate_tb_cv),
    "cells\n")
if (max(gate_tb_cv$dg, gate_tb_cv$di) > 1e-8) {
  print(as.data.frame(gate_tb_cv %>% filter(dg > 1e-8 | di > 1e-8)))
  stop("team healthy wall failed -- visual does not reproduce canon")
}

# faced side, per opponent team: that team's defenders' games vs NE
faced_team_cv <- faced_games_sec_lg %>%
  filter(focal == "NE") %>%
  group_by(team, split, band) %>%
  summarise(raw25 = weighted.mean(g25, w = g_snaps, na.rm = TRUE),
            fill_pct = sum(g_snaps[fill]) / sum(g_snaps),
            .groups = "drop")

# components wall: re-pooled to split x band, the per-team cells
# must reproduce canon's NE faced bands exactly
gate_ft_cv <- faced_games_sec_lg %>%
  filter(focal == "NE") %>%
  group_by(split, band) %>%
  summarise(raw25 = weighted.mean(g25, w = g_snaps, na.rm = TRUE),
            fill_pct = sum(g_snaps[fill]) / sum(g_snaps),
            .groups = "drop") %>%
  inner_join(faced_band_sec_lg %>%
               filter(focal == "NE") %>%
               select(split, band, g_c = grade_25, f_c = fill_share_25),
             by = c("split", "band")) %>%
  mutate(dg = abs(raw25 - g_c), df = abs(fill_pct - f_c))
if (max(gate_ft_cv$dg, gate_ft_cv$df) > 1e-8) {
  print(as.data.frame(gate_ft_cv))
  stop("faced components wall failed -- report")
}

# adjusted faced side, per opponent team (layer-built rows, walled
# in section 5 to stand on canon's exact NE-faced set)
faced_team_c3_cv <- faced_c3_ne_cv %>%
  group_by(team, split, band) %>%
  summarise(adj25 = weighted.mean(c3, w = g_snaps),
            .groups = "drop")

# membership wall: adjusted per-team cells live exactly where the
# raw per-team cells live
keys_r_cv <- faced_team_cv %>% distinct(team, split, band)
keys_a_cv <- faced_team_c3_cv %>% distinct(team, split, band)
if (nrow(dplyr::anti_join(keys_r_cv, keys_a_cv,
                          by = c("team", "split", "band"))) > 0 ||
    nrow(dplyr::anti_join(keys_a_cv, keys_r_cv,
                          by = c("team", "split", "band"))) > 0) {
  stop("faced cell grids disagree between lenses -- report")
}

cmp_team_cv <- tidyr::expand_grid(
  team = opp_teams_cv, split = c("man", "zone"),
  band = BANDS_SEC_LG) %>%
  left_join(team26_split_cv,
            by = c("team", "split", "band")) %>%
  left_join(faced_team_cv,
            by = c("team", "split", "band")) %>%
  left_join(faced_team_c3_cv,
            by = c("team", "split", "band")) %>%
  mutate(d_raw = raw26 - raw25,
         d_adj = adj26 - adj25)
# healthy side can never be NA (canon depth is exact); faced '--'
# is legitimate: not faced, or no defender of that band met the
# split floor in those weeks
if (any(is.na(cmp_team_cv$raw26)) || any(is.na(cmp_team_cv$adj26))) {
  print(cmp_team_cv %>% filter(is.na(raw26) | is.na(adj26)))
  stop("healthy grid has holes -- report")
}
stopifnot(nrow(cmp_team_cv) ==
            length(opp_teams_cv) * 2L * length(BANDS_SEC_LG))

# team order: mean adjusted delta, hardest first, per split
ord_one_cv <- function(sp) {
  cmp_team_cv %>%
    filter(split == sp) %>%
    group_by(team) %>%
    summarise(md = mean(d_adj, na.rm = TRUE), .groups = "drop") %>%
    mutate(md = dplyr::coalesce(md, -Inf)) %>%
    arrange(desc(md)) %>% pull(team)
}
ord_man_cv  <- ord_one_cv("man")
ord_zone_cv <- ord_one_cv("zone")

gt_one_cv <- function(sp, ord_cv, tag_txt) {
  cmp_team_cv %>%
    filter(split == sp) %>%
    mutate(team = factor(team, levels = ord_cv),
           band = factor(band, levels = BANDS_SEC_LG)) %>%
    arrange(team, band) %>%
    select(team, band, raw25, raw26, d_raw, adj25, adj26, d_adj,
           fill_pct, interp_adj) %>%
    gt(groupname_col = "team", rowname_col = "band") %>%
    tab_spanner(label = "Raw",
                columns = c(raw25, raw26, d_raw)) %>%
    tab_spanner(label = "Adjusted (same-slate)",
                columns = c(adj25, adj26, d_adj)) %>%
    cols_label(raw25 = "'25", raw26 = "'26", d_raw = "\u0394",
               adj25 = "'25", adj26 = "'26", d_adj = "\u0394",
               fill_pct = "fill %", interp_adj = "interp %") %>%
    fmt_percent(columns = c(raw25, raw26, adj25, adj26,
                            fill_pct, interp_adj), decimals = 0) %>%
    fmt_percent(columns = c(d_raw, d_adj), decimals = 0,
                force_sign = TRUE) %>%
    sub_missing(missing_text = "--") %>%
    data_color(columns = c(d_raw, d_adj),
               fn = scales::col_numeric(
                 c("#6baed6", "#f7f7f7", "#C60C30"),
                 domain = c(-0.3, 0.3), na.color = "#f7f7f7"),
               autocolor_text = TRUE) %>%
    tab_header(
      title = paste0("Opposing coverage, per team \u2014 ",
                     toupper(sp), " (", tag_txt, ")"),
      subtitle = "'--' = not faced in 2025 (or nobody at that band met the split floor vs NE) | teams sorted by adjusted \u0394, hardest first | higher = better coverage = harder for Maye | same rotation members and coverage-snap weights in both lenses | coverage-grade lens only") %>%
    tab_options(table.font.size = px(12), data_row.padding = px(3),
                column_labels.font.weight = "bold",
                row_group.font.weight = "bold")
}

# viewer law (the 2026-08-20 lesson): the RStudio viewer shows ONE
# gt at a time -- each print replaces the last. Zone is the
# majority diet (canon denominator law), so zone prints LAST and
# the viewer ends on it. Both tables persist in session; recall
# any time with print().
gt_man_team_cv <- gt_one_cv("man", ord_man_cv, "table 1 of 2")
gt_zone_team_cv <- gt_one_cv("zone", ord_zone_cv,
                             "table 2 of 2 -- prints LAST so the viewer ends on the zone diet")

cat("\n== THE PER-TEAM VISUAL, one table per split ==\n")
cat("man prints first, zone last (the viewer shows one table at a\n")
cat("time and ends on the last print). Recall any time:\n")
cat("  print(gt_man_team_cv)    # man coverage, per team x band\n")
cat("  print(gt_zone_team_cv)   # zone coverage, per team x band\n")

# gtsave checkpoints (uncomment to write):
# gt::gtsave(gt_man_team_cv,  "coverage_team_split_man.png")
# gt::gtsave(gt_zone_team_cv, "coverage_team_split_zone.png")

print(gt_man_team_cv)
print(gt_zone_team_cv)

# ------------------------------------------------------------
# Checkpoint: SECONDARY AVAILABILITY LAYER v1 green.
#   - the six unit adaptations carried Andy's 2026-08-21 stamp;
#     window/denominator/pricing-law carry the 2026-08-19 OL stamp
#   - walls passed: NE healthy + NE faced (cmp_slate_mz, 1e-8,
#     8 cells each), adjusted-faced membership (canon's exact NE
#     row set), league band (256 cells) + unit (64 rows) vs
#     slate_band_sec_lg / league_cov at 1e-8, team healthy (256
#     cells) vs team_band_sec_lg, faced components, provenance
#     vs rot_2026_sec_lg, id recovery (zero unmatched non-phantoms)
#   - the fold-in (priced columns inside the secondary final
#     evaluation, GT spanner, slate plot) awaits its own stamp,
#     same as pass-rush / run-block / rushing
# ------------------------------------------------------------
