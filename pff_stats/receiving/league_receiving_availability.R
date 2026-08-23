# ============================================================
# RECEIVING AVAILABILITY LAYER -- opponent pass catchers,
# 2026 slates priced by availability (man / zone, one file,
# never recombined)
# Built 2026-08-21 on Andy's green light: "this ... this
# should be enough for receiving. i thin. let's give it a
# shot."
#
# WHAT THIS ASKS: if an opponent's receivers miss time in
# 2026, how much pass-catcher quality does NE's pass defense
# actually dodge? Healthy slate vs injury-priced slate, per
# split, both anchors (grade, YPRR), both currencies.
#
# DIRECTION (defense side): higher pctl = better receiver =
# HARDER for NE. A negative real jump means the slate got
# softer once injuries are priced in.
#
# STAMPS CARRIED (unchanged laws):
#   - PRICING LAW: value_priced = avail * value_healthy +
#     (1 - avail) * repl. avail judged ONLY on job-holding
#     seasons. Zero job seasons -> observed first-year rate
#     (rookie/backup lines, 2024-25 first-year holders, 2023
#     excluded for left-censoring, empty cells -> pooled unit
#     line with an evidence column). repl = empirical 2025
#     backup level, signed fill law per lens.
#   - THREE CURRENCIES + firewall: c1 raw percentiles, c3
#     adjusted within-opponent game ranks, priced always
#     labeled. FACED-2025 untouched by pricing.
#   - FULL-PRECISION LAW: compute unrounded; rounding is
#     display at print only, never before a wall. Walls at
#     1e-8 against unrounded canon objects.
#   - PRINT LAW: short English phrases; one lens per table;
#     every table opens with its question; display renames at
#     print, internals keep contract names.
#   - CROSSED-STREAMS LAW: this layer consumes unit-unique
#     canon names ONLY; canons are never edited or
#     re-assigned; the layer runs in any session order after
#     its SOURCE ORDER.
#   - SEQUENTIAL-MASKING LAW: counts in summarise, ratios in
#     mutate, uniquely named intermediates -- a later
#     summarise() expression would see the JUST-CREATED
#     column (the 2026-08-21 bench_split receipt). New
#     columns referenced inside one mutate are canon-legal
#     (the canon rot build does it); inside one summarise,
#     never.
#   - NA-KEY JOIN INSURANCE: dplyr left_join matches NA==NA;
#     every right-side id-keyed frame is filtered
#     !is.na(player_id) first.
#   - unname() is LOAD-BEARING on named lookup vectors
#     (all_of / [ on named input eats renames).
#   - WALLS print offenders and evidence BEFORE stopping.
#   - ZERO ATHENA in this file. One nflreadr roster pull in
#     section 4 (the canon league pattern). No live lookups.
#
# UNIT ADAPTATIONS (this unit's shape -- Andy can veto):
#   1. ONE availability per player (split-neutral). The
#      weekly base has no per-split routes (canon-documented
#      asymmetry), so "played" = the canon overall gate:
#      routes >= X_QUAL_RC (8, pinned in section 0 -- the
#      shared session name X_QUAL is never consumed) in a REG
#      week. Splits live in
#      the VALUES, never in the availability number.
#   2. Job = canon membership law: top N_CORPS (8) per
#      (team, season) by pmax(this team's routes, prior-
#      season total routes) -- the traded-player clause
#      mirrors the 2026 corps usage_ord law.
#   3. Route-weighted everywhere (canon weight law: weights
#      are overall routes on both splits).
#   4. FACED 2025 stays OBSERVED-ONLY -- this unit's canon
#      law, unlike the secondary's 08-17 fill law. Honesty
#      column = unscored share, on BOTH currencies.
#   5. Value fills = canon's single-arm band entry priors
#      (rec_rookie_prior) for the raw side; the layer builds
#      the matching c3 entry prior per (band, split) in-file
#      with a pooled same-split fallback + evidence column
#      (the secondary section-0b pattern).
#   6. NO PHANTOMS (canon v1 law). Members without job
#      history price at the observed first-year lines.
#   7. League sweep mirrors league_one_split EXACTLY
#      (2-rung weight ladder: blend -> real pmax; no
#      corps-median fill; zero-evidence members carry w = 0)
#      and walls against league_sys_both at 1e-8 BEFORE
#      pricing enters.
#   8. Rookies enter the NE corps only through Andy's delta
#      tribble (canon law; currently empty = full sign-off),
#      so board 3 is structural, same as pass-rush.
#
# SOURCE ORDER (one session, in order):
#   1. receiving workspace build (receiving_stats_build_AWS.R):
#      receiving_func_base, receiver_scheme_final, combined_ids
#   2. new_england_opp_ol_schedule(2).R: sched_2026,
#      opp_2026_teams, in_season, blend2, pff_team_lookup
#   3. new_england_opp_receiving_schedule(2).R  (Phase 5 canon)
#   4. league_opp_receiving_schedule(2).R       (league tail)
#   5. league_receiving_evaluating_currency_three.R (c3 canon)
#   6. THIS FILE.
#
# ASCII only. Re-source WHOLE file after any edit.
# ============================================================

suppressPackageStartupMessages(library(dplyr))

# ------------------------------------------------------------
# GATES -- session contract, column receipts, unit identity,
# tibble classes (non-destructive: canon names never touched).
# ------------------------------------------------------------

needed_rc <- c(
  # receiving workspace + Phase 5 NE canon
  "receiving_func_base", "rec_season_pctl_sos", "rec_rookie_prior",
  "rec_band_season", "rec_2026",
  "entry_years_off", "usage_rec", "band_26", "p25", "p24",
  "rot_rec_2026", "faced_band_rec_2025", "slate_band_rec_2026",
  "cmp_rec_slate", "rec_id_bridge", "pos_keep",
  "REC_BANDS", "N_CORPS",
  # OL canon session law
  "sched_2026", "in_season", "blend2", "pff_team_lookup",
  # league tail
  "league_sys_both", "opp_map",
  # currency three
  "rec_c3_pctl", "REC_TEAM_PATCH")
missing_rc <- needed_rc[!vapply(needed_rc, exists, logical(1))]
if (length(missing_rc)) {
  stop("missing session objects: ", paste(missing_rc, collapse = ", "),
       " -- run the SOURCE ORDER block in the header")
}

req_cols_rc <- list(
  receiving_func_base = c("player_id", "season", "week", "posteam",
                          "def_ssn", "routes", "align_cluster_name"),
  rec_season_pctl_sos = c("player_id", "season", "band", "qual_g",
                          "man_grade_pctl", "man_yprr_pctl",
                          "zone_grade_pctl", "zone_yprr_pctl"),
  rec_rookie_prior    = c("band", "pr_grade", "pr_man_grade",
                          "pr_man_yprr", "pr_zone_grade",
                          "pr_zone_yprr", "n_entry"),
  rec_band_season     = c("player_id", "season", "band"),
  rec_2026            = c("team_name", "roster_name", "player_id",
                          "entry_year"),
  entry_years_off     = c("player_id", "entry_year"),
  usage_rec           = c("player_id", "routes_2024", "routes_2025"),
  band_26             = c("player_id", "band"),
  p25                 = c("player_id", "qual_g25", "mg25", "my25",
                          "zg25", "zy25"),
  p24                 = c("player_id", "mg24", "my24", "zg24", "zy24"),
  rot_rec_2026        = c("team_name", "roster_name", "band", "status",
                          "prior_used", "usage_w", "mg_f", "my_f",
                          "zg_f", "zy_f"),
  faced_band_rec_2025 = c("band", "tot_routes", "unscored_share",
                          "man_grade_25", "man_yprr_25",
                          "zone_grade_25", "zone_yprr_25"),
  slate_band_rec_2026 = c("band", "interp_share", "man_grade_26",
                          "man_yprr_26", "zone_grade_26",
                          "zone_yprr_26"),
  cmp_rec_slate       = c("band", "man_grade_25", "man_grade_26",
                          "zone_grade_25", "zone_grade_26",
                          "d_man_grade", "d_zone_grade",
                          "interp_share"),
  league_sys_both     = c("focal", "split", "v26", "faced25", "d",
                          "prior_share", "unscored_share"),
  opp_map             = c("focal", "opp"),
  rec_c3_pctl         = c("player_id", "season", "band", "split",
                          "pool_g", "c3_grade_pctl", "c3_yprr_pctl"))
for (nm in names(req_cols_rc)) {
  miss_cols <- setdiff(req_cols_rc[[nm]], names(get(nm)))
  if (length(miss_cols)) {
    stop("column receipt failed on ", nm, ": ",
         paste(miss_cols, collapse = ", "))
  }
}
cat("gates: session contract + column receipts OK (",
    length(needed_rc), "objects,", length(req_cols_rc),
    "frames checked)\n")

# unit identity walls
stopifnot(all(rot_rec_2026$band %in% REC_BANDS),
          all(rec_c3_pctl$split %in% c("man", "zone")),
          setequal(league_sys_both$split, c("man", "zone")),
          nrow(league_sys_both) == 64L,
          setequal(cmp_rec_slate$band, REC_BANDS),
          setequal(faced_band_rec_2025$band, REC_BANDS),
          setequal(unique(as.character(rot_rec_2026$team_name)),
                   unique(sched_2026)))

# tibble-class wall (non-destructive: a layer never re-assigns a
# canon name -- it dies loud here instead)
tib_check_rc <- c("receiving_func_base", "rec_season_pctl_sos",
                  "rec_c3_pctl", "rot_rec_2026", "cmp_rec_slate",
                  "league_sys_both", "rec_2026", "rec_rookie_prior")
bad_tib <- tib_check_rc[!vapply(tib_check_rc,
                                function(nm) inherits(get(nm), "tbl_df"),
                                logical(1))]
if (length(bad_tib)) {
  stop("canon frames lost tibble class (re-source their files, do ",
       "NOT re-assign here): ", paste(bad_tib, collapse = ", "))
}

# ANTI-STALENESS MARKER (the 2026-08-21 viewer receipt): the
# RStudio viewer shows ONE gt at a time and KEEPS it until
# another gt prints over it. A run that dies before section 8
# used to leave the PREVIOUS unit's table on screen, reading
# like fresh output. This marker replaces whatever is sitting
# there the moment this file is sourced: if the marker is what
# you see, the run stopped before section 8 -- the console tail
# names the stop. The real tables (bands WWR/WSWR/SWR/ITE/STE/
# RB) replace this marker at the end of a green run.
suppressPackageStartupMessages(library(gt))
gt_marker_rc <- tibble::tibble(
  status = paste0("RECEIVING availability layer sourced ",
                  format(Sys.time(), "%H:%M:%S"),
                  " -- tables build in section 8."),
  if_you_see_this = "the run stopped early -- read the console tail for the stop") %>%
  gt()
print(gt_marker_rc)

# ------------------------------------------------------------
# INTAKE -- the doubled-rows mine (the Akers x4 family:
# cluster_join fan-out in the canon workspace build). Collapse
# law, mirrored from the c3 canon: identical pairs on every
# carried column -> distinct() is lossless; same-team pairs
# that DIFFER die loud. Canon names untouched -- the layer
# works on its own intake frame.
# ------------------------------------------------------------

rcv_base_rc <- receiving_func_base %>%
  filter(!is.na(player_id)) %>%
  mutate(team_c = dplyr::coalesce(unname(REC_TEAM_PATCH[posteam]),
                                  posteam)) %>%
  select(player_id, season, week, team_c, def_ssn, routes)

dup_rc <- rcv_base_rc %>%
  count(player_id, season, week, team_c) %>%
  filter(n > 1)
if (nrow(dup_rc)) {
  cat("BASE DUP RECEIPT: doubled player-game rows in",
      "receiving_func_base (canon build fan-out):\n")
  chk_rc <- rcv_base_rc %>%
    inner_join(dup_rc %>% select(player_id, season, week, team_c),
               by = c("player_id", "season", "week", "team_c"))
  print(chk_rc %>% arrange(player_id, season, week), n = 40)
  non_ident <- chk_rc %>%
    distinct() %>%
    count(player_id, season, week, team_c) %>%
    filter(n > 1)
  if (nrow(non_ident)) {
    cat("SAME-TEAM PAIRS THAT DIFFER on carried columns:\n")
    print(non_ident)
    stop("dup player-game rows differ -- report, do not collapse")
  }
  cat("all", nrow(dup_rc), "pairs identical on carried columns --",
      "distinct() lossless\n")
  rcv_base_rc <- rcv_base_rc %>% distinct()
} else {
  cat("intake: no doubled player-game rows in receiving_func_base\n")
}

# align-label map (faced-side band fallback for unscored rows,
# canon faced law). Identical-label dups collapse; a label
# CONFLICT inside one player-game dies loud for Andy's ruling.
lab_rc <- receiving_func_base %>%
  filter(!is.na(player_id)) %>%
  mutate(team_c = dplyr::coalesce(unname(REC_TEAM_PATCH[posteam]),
                                  posteam)) %>%
  distinct(player_id, season, week, team_c,
           band_row = align_cluster_name)
lab_dup <- lab_rc %>%
  count(player_id, season, week, team_c) %>%
  filter(n > 1)
if (nrow(lab_dup)) {
  cat("ALIGN-LABEL CONFLICTS inside a player-game:\n")
  print(lab_rc %>%
          inner_join(lab_dup %>% select(-n),
                     by = c("player_id", "season", "week", "team_c")),
        n = 40)
  stop("align label conflict -- report, do not pick one")
}

# ------------------------------------------------------------
# 0. KNOBS
# ------------------------------------------------------------

AVAIL_SEASONS_RC <- 2023:2025   # judged availability window (REG)
AVAIL_DENOM_RC   <- 17L         # REG games per season
X_QUAL_RC        <- 8L          # the receiving canon's played gate
# (Phase 5 header law: 8 routes). PINNED, never read from the
# session name X_QUAL: that name is shared across canons and the
# rushing canons overwrite it with 4 (their own collision note at
# league_opp_rushing_schedule(1).R:614). The 2026-08-21 receipt:
# a session carrying X_QUAL = 4 failed the faced wall (canon
# faced was built at 8; the STE/RB bands moved). The receiving
# canon files themselves still read the shared name -- pinning
# them canon-side is Andy's ruling, not this layer's.

if (exists("X_QUAL") && !identical(as.integer(X_QUAL), X_QUAL_RC)) {
  cat("\n!! GATE COLLISION RECEIPT: session X_QUAL =", X_QUAL,
      "but the receiving gate is", X_QUAL_RC, "routes. The layer\n")
  cat("!! uses its pinned", X_QUAL_RC, "throughout. If you re-source\n")
  cat("!! the receiving canon files in this session state they\n")
  cat("!! would rebuild at", X_QUAL, "-- re-run the receiving\n")
  cat("!! workspace build first, or rule a canon-side pin.\n")
}

cat("\n== RECEIVING AVAILABILITY LAYER ==\n")
cat("knobs: window", min(AVAIL_SEASONS_RC), "-", max(AVAIL_SEASONS_RC),
    "| REG denominator", AVAIL_DENOM_RC,
    "| played floor = receiving gate routes >=", X_QUAL_RC,
    "| corps size = canon N_CORPS", N_CORPS, "\n")

# ------------------------------------------------------------
# 0b. c3 ENTRY PRIOR per (band, split) -- median entry-season
#     c3 pctl, entry >= 2018 (canon prior window). Empty/thin
#     cells fall back to the pooled same-split line, evidence
#     column kept. This is the fill for adjusted values with no
#     c3 history (secondary section-0b pattern; splits never
#     recombine -- pooling is across BANDS within one split).
# ------------------------------------------------------------

c3_ent_pool_src <- rec_c3_pctl %>%
  inner_join(entry_years_off, by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2018)

c3_ent_cell <- c3_ent_pool_src %>%
  group_by(band, split) %>%
  summarise(n_entry = dplyr::n(),
            pr_c3g_b = suppressWarnings(
              median(c3_grade_pctl, na.rm = TRUE)),
            pr_c3y_b = suppressWarnings(
              median(c3_yprr_pctl, na.rm = TRUE)),
            .groups = "drop")

c3_ent_pool <- c3_ent_pool_src %>%
  group_by(split) %>%
  summarise(pr_c3g_p = suppressWarnings(
    median(c3_grade_pctl, na.rm = TRUE)),
    pr_c3y_p = suppressWarnings(
      median(c3_yprr_pctl, na.rm = TRUE)),
    .groups = "drop")

c3_entry_prior_rc <- tidyr::expand_grid(band = REC_BANDS,
                                        split = c("man", "zone")) %>%
  left_join(c3_ent_cell, by = c("band", "split")) %>%
  left_join(c3_ent_pool, by = "split") %>%
  mutate(n_entry = dplyr::coalesce(n_entry, 0L),
         pr_c3g = dplyr::coalesce(pr_c3g_b, pr_c3g_p),
         pr_c3y = dplyr::coalesce(pr_c3y_b, pr_c3y_p),
         ev_c3 = dplyr::if_else(!is.na(pr_c3g_b) & !is.na(pr_c3y_b),
                                "band_pool", "split_pool")) %>%
  select(band, split, pr_c3g, pr_c3y, n_entry, ev_c3)

if (any(is.na(c3_entry_prior_rc$pr_c3g)) ||
    any(is.na(c3_entry_prior_rc$pr_c3y))) {
  print(c3_entry_prior_rc %>% filter(is.na(pr_c3g) | is.na(pr_c3y)))
  stop("c3 entry prior has NA after pooled fallback -- report")
}

cat("\n-- c3 entry prior per band x split (evidence: which pool) --\n")
print(c3_entry_prior_rc %>%
        mutate(across(c(pr_c3g, pr_c3y), ~ round(.x, 3))), n = 12)

# ------------------------------------------------------------
# 1. AVAILABILITY HISTORY -- one number per player (split-
#    neutral, adaptation 1). played = routes >= X_QUAL_RC in a
#    REG week. Job = top N_CORPS per (team, season) by
#    pmax(this team's routes, prior-season total routes).
# ------------------------------------------------------------

reg_rc <- rcv_base_rc %>% filter(week <= 18)

routes_tm_rc <- reg_rc %>%
  filter(season >= min(AVAIL_SEASONS_RC) - 1L) %>%
  group_by(player_id, season, team_c) %>%
  summarise(routes_tm = sum(routes), .groups = "drop")

routes_prev_rc <- routes_tm_rc %>%
  group_by(player_id, season) %>%
  summarise(routes_tot = sum(routes_tm), .groups = "drop") %>%
  mutate(season = season + 1L) %>%
  select(player_id, season, routes_prev = routes_tot)

jobs_rc <- routes_tm_rc %>%
  filter(season %in% AVAIL_SEASONS_RC) %>%
  left_join(routes_prev_rc, by = c("player_id", "season")) %>%
  mutate(usage_ord = pmax(routes_tm,
                          dplyr::coalesce(routes_prev, 0))) %>%
  group_by(season, team_c) %>%
  arrange(desc(usage_ord), .by_group = TRUE) %>%
  mutate(corps_rank = dplyr::row_number()) %>%
  ungroup() %>%
  mutate(is_corps = corps_rank <= N_CORPS)

job_map_rc <- jobs_rc %>%
  filter(is_corps) %>%
  distinct(player_id, season) %>%
  mutate(had_job = TRUE)

played_rc <- reg_rc %>%
  filter(season %in% AVAIL_SEASONS_RC, routes >= X_QUAL_RC) %>%
  count(player_id, season, name = "played_g")

hist_rc <- reg_rc %>%
  filter(season %in% AVAIL_SEASONS_RC) %>%
  distinct(player_id, season) %>%
  left_join(played_rc, by = c("player_id", "season")) %>%
  mutate(played_g = dplyr::coalesce(played_g, 0L),
         avail = played_g / AVAIL_DENOM_RC) %>%
  left_join(job_map_rc, by = c("player_id", "season")) %>%
  mutate(had_job = dplyr::coalesce(had_job, FALSE)) %>%
  left_join(rec_band_season, by = c("player_id", "season"))

cat("\n== SECTION 1 -- who held a receiving job, and how often\n")
cat("   did they suit up? (a job = top-8 corps routes for his\n")
cat("   team that season; played = 8+ routes in a REG week)\n")
cat("-- corps size check: teams x seasons vs rows kept --\n")
print(jobs_rc %>%
        group_by(season) %>%
        summarise(teams = dplyr::n_distinct(team_c),
                  corps_rows = sum(is_corps),
                  .groups = "drop") %>%
        mutate(mean_per_team = round(corps_rows / teams, 2)))
cat("-- availability across corps seasons, by band --\n")
print(hist_rc %>%
        filter(had_job, !is.na(band)) %>%
        group_by(band) %>%
        summarise(job_seasons = dplyr::n(),
                  q10 = round(quantile(avail, .10), 3),
                  med = round(median(avail), 3),
                  mean = round(mean(avail), 3),
                  q90 = round(quantile(avail, .90), 3),
                  .groups = "drop") %>%
        arrange(match(band, REC_BANDS)))
cat("-- pooled across bands (all corps seasons) --\n")
print(hist_rc %>%
        filter(had_job) %>%
        summarise(job_seasons = dplyr::n(),
                  q10 = round(quantile(avail, .10), 3),
                  med = round(median(avail), 3),
                  mean = round(mean(avail), 3),
                  q90 = round(quantile(avail, .90), 3)))

# ------------------------------------------------------------
# 2. BENCHMARK -- the expected suit-up rate of a corps member,
#    per band, job seasons only (pricing law: avail judged
#    ONLY on job-holding seasons).
# ------------------------------------------------------------

bench_band_rc <- hist_rc %>%
  filter(had_job, !is.na(band)) %>%
  group_by(band) %>%
  summarise(job_seasons = dplyr::n(),
            bench_avail = mean(avail),
            .groups = "drop") %>%
  arrange(match(band, REC_BANDS))

bench_unit_rc <- hist_rc %>%
  filter(had_job) %>%
  summarise(job_seasons = dplyr::n(),
            bench_avail = mean(avail))

cat("\n== SECTION 2 -- what does a corps receiver's season\n")
cat("   usually look like? (benchmark suit-up rates)\n")
print(bench_band_rc %>% mutate(bench_avail = round(bench_avail, 3)))
cat("pooled unit line:", round(bench_unit_rc$bench_avail, 3),
    "over", bench_unit_rc$job_seasons, "corps seasons\n")

# ------------------------------------------------------------
# 2b. FIRST-YEAR LINES -- the observed suit-up rate of a
#     first-year corps member, per band x population. Pop:
#     rookie = entry season; backup = entered earlier. 2024-25
#     first-years only (2023 excluded: left-censored window).
#     Empty cells -> pooled unit line for that pop, evidence
#     column kept (pricing law).
# ------------------------------------------------------------

first_job_rc <- job_map_rc %>%
  group_by(player_id) %>%
  summarise(first_job = min(season), .groups = "drop")

fy_rc <- hist_rc %>%
  filter(season %in% c(2024, 2025), had_job) %>%
  left_join(first_job_rc, by = "player_id") %>%
  filter(first_job == season) %>%
  left_join(entry_years_off %>% filter(!is.na(player_id)),
            by = "player_id") %>%
  mutate(pop = dplyr::if_else(!is.na(entry_year) &
                                entry_year == season,
                              "rookie", "backup"))

fy_cell <- fy_rc %>%
  filter(!is.na(band)) %>%
  group_by(band, pop) %>%
  summarise(n_fy = dplyr::n(),
            line_b = mean(avail),
            .groups = "drop")

fy_pool <- fy_rc %>%
  group_by(pop) %>%
  summarise(n_pool = dplyr::n(),
            line_p = mean(avail),
            .groups = "drop")

fy_lines_rc <- tidyr::expand_grid(band = REC_BANDS,
                                  pop = c("rookie", "backup")) %>%
  left_join(fy_cell, by = c("band", "pop")) %>%
  left_join(fy_pool, by = "pop") %>%
  mutate(n_fy = dplyr::coalesce(n_fy, 0L),
         line = dplyr::coalesce(line_b, line_p),
         ev_fy = dplyr::if_else(!is.na(line_b), "band", "unit_pool")) %>%
  select(band, pop, line, n_fy, ev_fy)

if (any(is.na(fy_lines_rc$line))) {
  print(fy_lines_rc %>% filter(is.na(line)))
  stop("first-year lines have NA after pooled fallback -- report")
}

cat("\n== SECTION 2b -- when a receiver holds a corps job for\n")
cat("   the first time, how much of the season does he play?\n")
cat("   (this is the suit-up rate priced into members with no\n")
cat("   job history; 'unit_pool' = band cell empty, pooled line\n")
cat("   used)\n")
print(fy_lines_rc %>% mutate(line = round(line, 3)), n = 12)

# ------------------------------------------------------------
# 3. REPLACEMENT LEVEL -- the 2025 REG backup receiver games
#    (played the floor, NOT in his team's 2025 corps), route-
#    weighted per band. Fill law per lens: earned 2025 pctl
#    where it exists, else canon's single-arm band entry prior
#    (raw) / the section-0b c3 entry prior (adjusted). A band
#    with no backup games falls back to the pooled unit repl,
#    evidence column kept.
# ------------------------------------------------------------

corps25_rc <- jobs_rc %>%
  filter(season == 2025, is_corps) %>%
  select(player_id, team_c) %>%
  mutate(in_corps = TRUE)

c3_25_w <- rec_c3_pctl %>%
  filter(season == 2025, !is.na(player_id)) %>%
  select(player_id, split, c3_grade_pctl, c3_yprr_pctl) %>%
  tidyr::pivot_wider(names_from = split,
                     values_from = c(c3_grade_pctl, c3_yprr_pctl),
                     names_glue = "{.value}_{split}")

c3_pr_w <- c3_entry_prior_rc %>%
  select(band, split, pr_c3g, pr_c3y) %>%
  tidyr::pivot_wider(names_from = split,
                     values_from = c(pr_c3g, pr_c3y),
                     names_glue = "{.value}_{split}")

bup_games_rc <- rcv_base_rc %>%
  filter(season == 2025, week <= 18, routes >= X_QUAL_RC) %>%
  left_join(corps25_rc, by = c("player_id", "team_c")) %>%
  filter(is.na(in_corps)) %>%
  inner_join(rec_band_season %>% filter(season == 2025),
             by = c("player_id", "season")) %>%
  filter(band %in% REC_BANDS)

cat("\n== SECTION 3 -- who actually fills in when a corps\n")
cat("   receiver sits? (2025 backup receiver-games by band)\n")
print(bup_games_rc %>%
        count(band, name = "backup_games") %>%
        arrange(match(band, REC_BANDS)))

bup_val_rc <- bup_games_rc %>%
  left_join(p25 %>% filter(!is.na(player_id)) %>%
              select(player_id, mg25, my25, zg25, zy25),
            by = "player_id") %>%
  left_join(rec_rookie_prior %>%
              select(band, pr_man_grade, pr_man_yprr,
                     pr_zone_grade, pr_zone_yprr),
            by = "band") %>%
  left_join(c3_25_w, by = "player_id") %>%
  left_join(c3_pr_w, by = "band") %>%
  mutate(v_mg   = dplyr::coalesce(mg25, pr_man_grade),
         v_my   = dplyr::coalesce(my25, pr_man_yprr),
         v_zg   = dplyr::coalesce(zg25, pr_zone_grade),
         v_zy   = dplyr::coalesce(zy25, pr_zone_yprr),
         v_c3mg = dplyr::coalesce(c3_grade_pctl_man,  pr_c3g_man),
         v_c3my = dplyr::coalesce(c3_yprr_pctl_man,   pr_c3y_man),
         v_c3zg = dplyr::coalesce(c3_grade_pctl_zone, pr_c3g_zone),
         v_c3zy = dplyr::coalesce(c3_yprr_pctl_zone,  pr_c3y_zone))

repl_cell <- bup_val_rc %>%
  group_by(band) %>%
  summarise(n_games = dplyr::n(),
            repl_mg   = weighted.mean(v_mg,   w = routes),
            repl_my   = weighted.mean(v_my,   w = routes),
            repl_zg   = weighted.mean(v_zg,   w = routes),
            repl_zy   = weighted.mean(v_zy,   w = routes),
            repl_c3mg = weighted.mean(v_c3mg, w = routes),
            repl_c3my = weighted.mean(v_c3my, w = routes),
            repl_c3zg = weighted.mean(v_c3zg, w = routes),
            repl_c3zy = weighted.mean(v_c3zy, w = routes),
            .groups = "drop")

repl_pool <- bup_val_rc %>%
  summarise(n_games_pool = dplyr::n(),
            pool_mg   = weighted.mean(v_mg,   w = routes),
            pool_my   = weighted.mean(v_my,   w = routes),
            pool_zg   = weighted.mean(v_zg,   w = routes),
            pool_zy   = weighted.mean(v_zy,   w = routes),
            pool_c3mg = weighted.mean(v_c3mg, w = routes),
            pool_c3my = weighted.mean(v_c3my, w = routes),
            pool_c3zg = weighted.mean(v_c3zg, w = routes),
            pool_c3zy = weighted.mean(v_c3zy, w = routes))

repl_rc <- tibble(band = REC_BANDS) %>%
  left_join(repl_cell, by = "band") %>%
  mutate(n_games = dplyr::coalesce(n_games, 0L),
         repl_mg   = dplyr::coalesce(repl_mg,   repl_pool$pool_mg),
         repl_my   = dplyr::coalesce(repl_my,   repl_pool$pool_my),
         repl_zg   = dplyr::coalesce(repl_zg,   repl_pool$pool_zg),
         repl_zy   = dplyr::coalesce(repl_zy,   repl_pool$pool_zy),
         repl_c3mg = dplyr::coalesce(repl_c3mg, repl_pool$pool_c3mg),
         repl_c3my = dplyr::coalesce(repl_c3my, repl_pool$pool_c3my),
         repl_c3zg = dplyr::coalesce(repl_c3zg, repl_pool$pool_c3zg),
         repl_c3zy = dplyr::coalesce(repl_c3zy, repl_pool$pool_c3zy),
         ev_repl = dplyr::if_else(n_games > 0L, "band", "unit_pool"))

if (any(is.na(repl_rc %>% select(starts_with("repl_"))))) {
  # canon-tolerated, NOT a stop: a band x metric entry prior can
  # be NA (thin split-floor cells -- the RB man-routes family).
  # Canon's own rot keeps those values NA and lets na.rm drop
  # them; the interp/unscored columns carry the honesty. Print
  # the cells so the exposure is visible.
  cat("-- repl NA receipt (canon-tolerated: NA band priors\n")
  cat("   stay NA, na.rm drops them downstream) --\n")
  print(repl_rc %>%
          filter(if_any(starts_with("repl_"), is.na)))
}

cat("-- the fill-in level, by band (route-weighted; raw = band\n")
cat("   percentiles, adj = within-opponent game-rank\n")
cat("   percentiles)\n")
print(repl_rc %>%
        mutate(across(starts_with("repl_"), ~ round(.x, 3))) %>%
        select(band, n_games, ev_repl, repl_mg, repl_zg,
               repl_c3mg, repl_c3zg))

# ------------------------------------------------------------
# 4. LEAGUE MEMBERS -- the canon league corps machine,
#    rebuilt verbatim (2-rung weight ladder, no corps-median
#    fill, membership split-neutral by canon law), extended
#    with availability + pricing. Walled against
#    league_sys_both at 1e-8 BEFORE pricing enters: if the
#    healthy chain reproduces canon's v26 and prior_share for
#    all 32 x 2 cells, membership and weights are canon's.
# ------------------------------------------------------------

wall_rc <- function(tag, chk_df, diff_col, tol = 1e-8) {
  d_vec <- abs(chk_df[[diff_col]])
  d_vec <- d_vec[!is.na(d_vec)]
  if (!length(d_vec)) {
    stop(tag, " -- all-NA diff column, nothing compared -- report")
  }
  d <- max(d_vec)
  cat(sprintf("%-56s max |d| = %.3g -> %s\n",
              tag, d, ifelse(d <= tol, "OK", "FAIL")))
  if (d > tol) {
    print(chk_df %>% filter(abs(.data[[diff_col]]) > tol), n = Inf)
    stop(tag, " wall breach -- evidence printed above")
  }
  invisible(d)
}

denan_rc <- function(x) dplyr::if_else(is.nan(x), NA_real_, x)

# adjusted-currency chain inputs, wide by split x season
c3_w <- rec_c3_pctl %>%
  filter(season %in% c(2024, 2025), !is.na(player_id)) %>%
  select(player_id, season, split, pool_g,
         c3_grade_pctl, c3_yprr_pctl) %>%
  tidyr::pivot_wider(names_from = c(split, season),
                     values_from = c(pool_g, c3_grade_pctl,
                                     c3_yprr_pctl),
                     names_glue = "{.value}_{split}_{season}")

# player-level availability resume (sequential-masking law:
# sums in summarise, ratios in mutate)
av_rc <- hist_rc %>%
  group_by(player_id) %>%
  summarise(job_seasons = sum(had_job),
            avail_job_sum = sum(avail[had_job]),
            played_seasons = sum(played_g > 0),
            avail_all_sum = sum(avail[played_g > 0]),
            .groups = "drop") %>%
  mutate(avail_job = dplyr::if_else(job_seasons > 0,
                                    avail_job_sum / job_seasons,
                                    NA_real_),
         avail_all = dplyr::if_else(played_seasons > 0,
                                    avail_all_sum / played_seasons,
                                    NA_real_)) %>%
  select(player_id, job_seasons, avail_job, played_seasons,
         avail_all)

fy_w <- fy_lines_rc %>%
  select(band, pop, line) %>%
  tidyr::pivot_wider(names_from = pop, values_from = line,
                     names_prefix = "line_")

members_rc <- nflreadr::load_rosters(2026) %>%
  filter(position %in% pos_keep) %>%
  transmute(gsis_id,
            roster_name = full_name,
            team_name = dplyr::coalesce(unname(pff_team_lookup[team]),
                                        team),
            entry_year) %>%
  left_join(rec_id_bridge, by = "gsis_id") %>%
  left_join(usage_rec, by = "player_id") %>%
  left_join(band_26, by = "player_id") %>%
  left_join(p25 %>% filter(!is.na(player_id)), by = "player_id") %>%
  left_join(p24 %>% filter(!is.na(player_id)) %>%
              select(player_id, mg24, my24, zg24, zy24),
            by = "player_id") %>%
  filter(!is.na(band)) %>%
  mutate(usage_ord = pmax(dplyr::coalesce(routes_2025, 0),
                          dplyr::coalesce(routes_2024, 0))) %>%
  group_by(team_name) %>%
  arrange(desc(usage_ord), .by_group = TRUE) %>%
  mutate(corps_rank = dplyr::row_number()) %>%
  filter(corps_rank <= N_CORPS) %>%
  ungroup() %>%
  left_join(c3_w, by = "player_id") %>%
  left_join(rec_rookie_prior %>%
              select(band, pr_man_grade, pr_man_yprr,
                     pr_zone_grade, pr_zone_yprr),
            by = "band") %>%
  left_join(c3_pr_w, by = "band") %>%
  mutate(w25  = pmin(dplyr::coalesce(qual_g25, 0L) / 10, 1),
         w25u = pmin(dplyr::coalesce(qual_g25, 0L) / 18, 1),
         mg_bl = blend2(mg25, mg24, w25),
         my_bl = blend2(my25, my24, w25),
         zg_bl = blend2(zg25, zg24, w25),
         zy_bl = blend2(zy25, zy24, w25),
         prior_mg = is.na(mg_bl),
         prior_my = is.na(my_bl),
         prior_zg = is.na(zg_bl),
         prior_zy = is.na(zy_bl),
         mg_f = dplyr::coalesce(mg_bl, pr_man_grade),
         my_f = dplyr::coalesce(my_bl, pr_man_yprr),
         zg_f = dplyr::coalesce(zg_bl, pr_zone_grade),
         zy_f = dplyr::coalesce(zy_bl, pr_zone_yprr),
         w25a_m = pmin(dplyr::coalesce(pool_g_man_2025, 0) / 10, 1),
         w25a_z = pmin(dplyr::coalesce(pool_g_zone_2025, 0) / 10, 1),
         c3mg_bl = blend2(c3_grade_pctl_man_2025,
                          c3_grade_pctl_man_2024, w25a_m),
         c3my_bl = blend2(c3_yprr_pctl_man_2025,
                          c3_yprr_pctl_man_2024, w25a_m),
         c3zg_bl = blend2(c3_grade_pctl_zone_2025,
                          c3_grade_pctl_zone_2024, w25a_z),
         c3zy_bl = blend2(c3_yprr_pctl_zone_2025,
                          c3_yprr_pctl_zone_2024, w25a_z),
         prior_c3mg = is.na(c3mg_bl),
         prior_c3my = is.na(c3my_bl),
         prior_c3zg = is.na(c3zg_bl),
         prior_c3zy = is.na(c3zy_bl),
         c3mg_f = dplyr::coalesce(c3mg_bl, pr_c3g_man),
         c3my_f = dplyr::coalesce(c3my_bl, pr_c3y_man),
         c3zg_f = dplyr::coalesce(c3zg_bl, pr_c3g_zone),
         c3zy_f = dplyr::coalesce(c3zy_bl, pr_c3y_zone),
         u_bl = blend2(dplyr::coalesce(routes_2025, 0),
                       dplyr::coalesce(routes_2024, 0), w25u),
         # 2-rung ladder (league law): blend -> real pmax; no
         # corps-median fill; zero-evidence members keep w = 0
         usage_w = dplyr::if_else(u_bl > 0, u_bl, usage_ord)) %>%
  left_join(av_rc %>% filter(!is.na(player_id)), by = "player_id") %>%
  left_join(fy_w, by = "band") %>%
  mutate(avail_src = case_when(
    dplyr::coalesce(job_seasons, 0L) > 0 ~ "starter_history",
    entry_year == 2026                   ~ "rookie_line",
    TRUE                                 ~ "backup_line"),
    avail = case_when(
      avail_src == "starter_history" ~ avail_job,
      avail_src == "rookie_line"     ~ line_rookie,
      TRUE                           ~ line_backup)) %>%
  left_join(repl_rc %>% select(band, starts_with("repl_")),
            by = "band") %>%
  mutate(mg_p   = avail * mg_f   + (1 - avail) * repl_mg,
         my_p   = avail * my_f   + (1 - avail) * repl_my,
         zg_p   = avail * zg_f   + (1 - avail) * repl_zg,
         zy_p   = avail * zy_f   + (1 - avail) * repl_zy,
         c3mg_p = avail * c3mg_f + (1 - avail) * repl_c3mg,
         c3my_p = avail * c3my_f + (1 - avail) * repl_c3my,
         c3zg_p = avail * c3zg_f + (1 - avail) * repl_c3zg,
         c3zy_p = avail * c3zy_f + (1 - avail) * repl_c3zy)

if (any(is.na(members_rc$avail))) {
  print(members_rc %>% filter(is.na(avail)) %>%
          select(team_name, roster_name, band, entry_year,
                 avail_src, job_seasons))
  stop("members with NA avail after the line ladder -- report")
}
# values follow canon NA law: a NA band entry prior stays NA and
# na.rm drops it from weighted means -- receipt, not a stop
na_val_n <- members_rc %>%
  summarise(across(c(mg_p, my_p, zg_p, zy_p,
                     c3mg_p, c3my_p, c3zg_p, c3zy_p),
                   ~ sum(is.na(.x))))
if (any(unlist(na_val_n) > 0)) {
  cat("-- member value NA receipt (canon NA law; see section 3\n")
  cat("   receipt for the band priors behind this) --\n")
  print(na_val_n)
}

cat("\n== SECTION 4 -- league corps, availability attached ==\n")
cat("-- corps rows:", nrow(members_rc), "across",
    dplyr::n_distinct(members_rc$team_name), "teams (top",
    N_CORPS, "per team by canon pmax law)\n")
cat("-- where each member's suit-up rate comes from --\n")
print(members_rc %>% count(avail_src))

# THE REPRODUCTION WALLS -- healthy chain vs league_sys_both
team26_rc <- bind_rows(
  members_rc %>%
    group_by(team_name) %>%
    summarise(v26  = weighted.mean(mg_f,   w = usage_w, na.rm = TRUE),
              v26p = weighted.mean(mg_p,   w = usage_w, na.rm = TRUE),
              v26a = weighted.mean(c3mg_f, w = usage_w, na.rm = TRUE),
              v26ap = weighted.mean(c3mg_p, w = usage_w, na.rm = TRUE),
              prior_share   = sum(usage_w[prior_mg]) / sum(usage_w),
              prior_share_a = sum(usage_w[prior_c3mg]) / sum(usage_w),
              .groups = "drop") %>%
    mutate(split = "man"),
  members_rc %>%
    group_by(team_name) %>%
    summarise(v26  = weighted.mean(zg_f,   w = usage_w, na.rm = TRUE),
              v26p = weighted.mean(zg_p,   w = usage_w, na.rm = TRUE),
              v26a = weighted.mean(c3zg_f, w = usage_w, na.rm = TRUE),
              v26ap = weighted.mean(c3zg_p, w = usage_w, na.rm = TRUE),
              prior_share   = sum(usage_w[prior_zg]) / sum(usage_w),
              prior_share_a = sum(usage_w[prior_c3zg]) / sum(usage_w),
              .groups = "drop") %>%
    mutate(split = "zone"))

# schedule/roster code coverage receipt -- canon swallows a
# whole-team code miss through na.rm (the pass-rush AZ/ARZ
# family); the layer mirrors canon's exact computation so the
# reproduction wall below still proves identity, and names the
# hole here for Andy's ruling (fix is canon-side)
code_hole_rc <- setdiff(unique(opp_map$opp),
                        unique(team26_rc$team_name))
if (length(code_hole_rc)) {
  cat("\n-- CODE HOLE: schedule team(s) with no corps match --\n")
  print(code_hole_rc)
  cat("   their games price off the other opponents only,\n")
  cat("   exactly as canon computed v26 (na.rm). Fix is a\n")
  cat("   canon-side ruling -- report to Andy.\n")
}

# per-split slate build -- mirrors canon's league_one_split
# shape exactly (single-split team frame joined to the
# schedule). Joining the long two-split frame in one step lets
# an unmatched opponent spawn a phantom split = NA group (the
# 2026-08-21 nrow(lg_chk) receipt).
slate26_one_rc <- function(sp) {
  opp_map %>%
    left_join(team26_rc %>% filter(split == sp) %>% select(-split),
              by = c("opp" = "team_name"),
              relationship = "many-to-many") %>%
    group_by(focal) %>%
    summarise(v26  = mean(v26,  na.rm = TRUE),
              v26p = mean(v26p, na.rm = TRUE),
              v26a = mean(v26a, na.rm = TRUE),
              v26ap = mean(v26ap, na.rm = TRUE),
              prior_share   = mean(prior_share,   na.rm = TRUE),
              prior_share_a = mean(prior_share_a, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(split = sp)
}
slate26_rc <- bind_rows(slate26_one_rc("man"),
                        slate26_one_rc("zone"))

lg_chk <- slate26_rc %>%
  left_join(league_sys_both %>%
              select(focal, split, v26_c = v26, ps_c = prior_share),
            by = c("focal", "split")) %>%
  mutate(d_v26 = v26 - v26_c, d_ps = prior_share - ps_c)

cat("\n-- WALL: healthy corps chain reproduces the canon league\n")
cat("   engine (64 cells; membership + weights are canon's iff\n")
cat("   this is 0)\n")
if (nrow(lg_chk) != 64L || any(is.na(lg_chk$v26_c))) {
  cat("join diagnostic -- rows per split (want 32/32):\n")
  print(lg_chk %>% count(split))
  cat("cells with no canon match:\n")
  print(lg_chk %>% filter(is.na(v26_c)) %>% select(focal, split),
        n = Inf)
  stop("league reproduction wall row census -- evidence above")
}
wall_rc("league v26 (healthy, both splits)", lg_chk, "d_v26")
wall_rc("league prior_share (both splits)", lg_chk, "d_ps")

# ------------------------------------------------------------
# 5. NE SLATE -- the 14 opponents on NE's 2026 schedule,
#    members from canon rot_rec_2026 (player_id dropped by
#    canon handoff law -> recovered via rec_2026 on
#    team+roster_name, the unit-unique roster route).
#    The valuation chain (blends, priors, weight ladder incl.
#    the 3rd corps-median rung) is REBUILT from p25/p24 and
#    walled against canon's rot values at 1e-8 -- after that,
#    availability + pricing enter, and the healthy/faced
#    slates are walled against cmp_rec_slate cell by cell.
# ------------------------------------------------------------

ne_ids <- rec_2026 %>%
  select(team_name, roster_name, player_id, entry_year)
stopifnot(anyDuplicated(ne_ids[, c("team_name", "roster_name")]) == 0)

ne_missing <- rot_rec_2026 %>%
  anti_join(ne_ids, by = c("team_name", "roster_name"))
if (nrow(ne_missing)) {
  print(ne_missing)
  stop("rot_rec_2026 members missing from rec_2026 -- report")
}

ne_frame_rc <- rot_rec_2026 %>%
  left_join(ne_ids, by = c("team_name", "roster_name")) %>%
  left_join(usage_rec, by = "player_id") %>%
  left_join(p25 %>% filter(!is.na(player_id)) %>%
              select(player_id, qual_g25, mg25, my25, zg25, zy25),
            by = "player_id") %>%
  left_join(p24 %>% filter(!is.na(player_id)) %>%
              select(player_id, mg24, my24, zg24, zy24),
            by = "player_id") %>%
  left_join(rec_rookie_prior %>%
              select(band, pr_man_grade, pr_man_yprr,
                     pr_zone_grade, pr_zone_yprr),
            by = "band") %>%
  mutate(w25  = pmin(dplyr::coalesce(qual_g25, 0L) / 10, 1),
         w25u = pmin(dplyr::coalesce(qual_g25, 0L) / 18, 1),
         mg_bl = blend2(mg25, mg24, w25),
         my_bl = blend2(my25, my24, w25),
         zg_bl = blend2(zg25, zg24, w25),
         zy_bl = blend2(zy25, zy24, w25),
         prior_mg = is.na(mg_bl),
         prior_my = is.na(my_bl),
         prior_zg = is.na(zg_bl),
         prior_zy = is.na(zy_bl),
         mg_f2 = dplyr::coalesce(mg_bl, pr_man_grade),
         my_f2 = dplyr::coalesce(my_bl, pr_man_yprr),
         zg_f2 = dplyr::coalesce(zg_bl, pr_zone_grade),
         zy_f2 = dplyr::coalesce(zy_bl, pr_zone_yprr),
         prior2 = prior_mg & prior_zg,
         u_bl = blend2(dplyr::coalesce(routes_2025, 0),
                       dplyr::coalesce(routes_2024, 0), w25u),
         u_pre = dplyr::if_else(u_bl > 0, u_bl,
                                pmax(dplyr::coalesce(routes_2025, 0),
                                     dplyr::coalesce(routes_2024, 0)))) %>%
  group_by(team_name) %>%
  mutate(u_fill = suppressWarnings(median(u_pre[u_pre > 0]))) %>%
  ungroup() %>%
  mutate(usage_w2 = dplyr::if_else(u_pre > 0, u_pre, u_fill))

# THE PROVENANCE WALL -- rebuilt values vs canon's rot values
prov_chk <- ne_frame_rc %>%
  transmute(team_name, roster_name, band,
            d_mg = mg_f2 - mg_f, d_my = my_f2 - my_f,
            d_zg = zg_f2 - zg_f, d_zy = zy_f2 - zy_f,
            d_uw = usage_w2 - usage_w,
            prior_mismatch = (prior2 != prior_used),
            na_mismatch = (is.na(usage_w2) != is.na(usage_w)))
cat("\n== SECTION 5 -- NE opponent slate ==\n")
cat("-- WALL: rot valuation chain reproduced from canon inputs\n")
wall_rc("rot man grade (mg_f)",   prov_chk, "d_mg")
wall_rc("rot man yprr (my_f)",    prov_chk, "d_my")
wall_rc("rot zone grade (zg_f)",  prov_chk, "d_zg")
wall_rc("rot zone yprr (zy_f)",   prov_chk, "d_zy")
wall_rc("rot usage weights",      prov_chk, "d_uw")
if (any(prov_chk$prior_mismatch) || any(prov_chk$na_mismatch)) {
  print(prov_chk %>% filter(prior_mismatch | na_mismatch), n = Inf)
  stop("prior_used / NA-pattern mismatch vs canon rot -- report")
}
cat("   prior_used flag + NA pattern: exact match\n")

# adjusted-currency chain + availability + pricing
ne_frame_rc <- ne_frame_rc %>%
  select(-ends_with("2"), -u_bl, -u_pre, -u_fill) %>%
  left_join(c3_w, by = "player_id") %>%
  left_join(c3_pr_w, by = "band") %>%
  mutate(w25a_m = pmin(dplyr::coalesce(pool_g_man_2025, 0) / 10, 1),
         w25a_z = pmin(dplyr::coalesce(pool_g_zone_2025, 0) / 10, 1),
         c3mg_bl = blend2(c3_grade_pctl_man_2025,
                          c3_grade_pctl_man_2024, w25a_m),
         c3my_bl = blend2(c3_yprr_pctl_man_2025,
                          c3_yprr_pctl_man_2024, w25a_m),
         c3zg_bl = blend2(c3_grade_pctl_zone_2025,
                          c3_grade_pctl_zone_2024, w25a_z),
         c3zy_bl = blend2(c3_yprr_pctl_zone_2025,
                          c3_yprr_pctl_zone_2024, w25a_z),
         prior_c3mg = is.na(c3mg_bl),
         prior_c3my = is.na(c3my_bl),
         prior_c3zg = is.na(c3zg_bl),
         prior_c3zy = is.na(c3zy_bl),
         c3mg_f = dplyr::coalesce(c3mg_bl, pr_c3g_man),
         c3my_f = dplyr::coalesce(c3my_bl, pr_c3y_man),
         c3zg_f = dplyr::coalesce(c3zg_bl, pr_c3g_zone),
         c3zy_f = dplyr::coalesce(c3zy_bl, pr_c3y_zone)) %>%
  left_join(av_rc %>% filter(!is.na(player_id)), by = "player_id") %>%
  left_join(fy_w, by = "band") %>%
  mutate(avail_src = case_when(
    dplyr::coalesce(job_seasons, 0L) > 0 ~ "starter_history",
    entry_year == 2026                   ~ "rookie_line",
    TRUE                                 ~ "backup_line"),
    avail = case_when(
      avail_src == "starter_history" ~ avail_job,
      avail_src == "rookie_line"     ~ line_rookie,
      TRUE                           ~ line_backup)) %>%
  left_join(repl_rc %>% select(band, starts_with("repl_")),
            by = "band") %>%
  mutate(mg_p   = avail * mg_f   + (1 - avail) * repl_mg,
         my_p   = avail * my_f   + (1 - avail) * repl_my,
         zg_p   = avail * zg_f   + (1 - avail) * repl_zg,
         zy_p   = avail * zy_f   + (1 - avail) * repl_zy,
         c3mg_p = avail * c3mg_f + (1 - avail) * repl_c3mg,
         c3my_p = avail * c3my_f + (1 - avail) * repl_c3my,
         c3zg_p = avail * c3zg_f + (1 - avail) * repl_c3zg,
         c3zy_p = avail * c3zy_f + (1 - avail) * repl_c3zy)

if (any(is.na(ne_frame_rc$avail))) {
  print(ne_frame_rc %>% filter(is.na(avail)) %>%
          select(team_name, roster_name, band, entry_year,
                 avail_src, job_seasons))
  stop("NE members with NA avail after the line ladder -- report")
}

cat("-- NE-slate corps:", nrow(ne_frame_rc), "members across",
    dplyr::n_distinct(ne_frame_rc$team_name), "opponents;",
    "player_id recovered on",
    sum(!is.na(ne_frame_rc$player_id)), "of",
    nrow(ne_frame_rc), "rows (rest: no bridged pff id ->\n")
cat("   their suit-up rate comes from the first-year lines)\n")
print(ne_frame_rc %>% count(avail_src))

# ---- the slate (two-step canon law: 17 game rows x members) ----
slate_rows_rc <- tibble(team_name = sched_2026) %>%
  left_join(ne_frame_rc, by = "team_name",
            relationship = "many-to-many")

slate_healthy_rc <- slate_rows_rc %>%
  group_by(band) %>%
  summarise(interp_share = sum(usage_w[prior_used]) / sum(usage_w),
            interp_m = sum(usage_w[prior_mg]) / sum(usage_w),
            interp_z = sum(usage_w[prior_zg]) / sum(usage_w),
            man_grade_26  = weighted.mean(mg_f, w = usage_w, na.rm = TRUE),
            man_yprr_26   = weighted.mean(my_f, w = usage_w, na.rm = TRUE),
            zone_grade_26 = weighted.mean(zg_f, w = usage_w, na.rm = TRUE),
            zone_yprr_26  = weighted.mean(zy_f, w = usage_w, na.rm = TRUE),
            h_c3mg = weighted.mean(c3mg_f, w = usage_w, na.rm = TRUE),
            h_c3my = weighted.mean(c3my_f, w = usage_w, na.rm = TRUE),
            h_c3zg = weighted.mean(c3zg_f, w = usage_w, na.rm = TRUE),
            h_c3zy = weighted.mean(c3zy_f, w = usage_w, na.rm = TRUE),
            .groups = "drop")

slate_priced_rc <- slate_rows_rc %>%
  group_by(band) %>%
  summarise(bup_g = sum(1 - avail),
            p_mg   = weighted.mean(mg_p,   w = usage_w, na.rm = TRUE),
            p_my   = weighted.mean(my_p,   w = usage_w, na.rm = TRUE),
            p_zg   = weighted.mean(zg_p,   w = usage_w, na.rm = TRUE),
            p_zy   = weighted.mean(zy_p,   w = usage_w, na.rm = TRUE),
            p_c3mg = weighted.mean(c3mg_p, w = usage_w, na.rm = TRUE),
            p_c3my = weighted.mean(c3my_p, w = usage_w, na.rm = TRUE),
            p_c3zg = weighted.mean(c3zg_p, w = usage_w, na.rm = TRUE),
            p_c3zy = weighted.mean(c3zy_p, w = usage_w, na.rm = TRUE),
            .groups = "drop")

# ---- faced side (OBSERVED-ONLY, adaptation 4; canon currency:
#      in_season includes the playoff weeks) ----
faced_rows_rc <- rcv_base_rc %>%
  filter(def_ssn == "NE2025", in_season(week), routes >= X_QUAL_RC) %>%
  left_join(lab_rc, by = c("player_id", "season", "week", "team_c")) %>%
  left_join(rec_season_pctl_sos %>%
              filter(season == 2025, !is.na(player_id)) %>%
              select(player_id, band,
                     mg25 = man_grade_pctl, my25 = man_yprr_pctl,
                     zg25 = zone_grade_pctl, zy25 = zone_yprr_pctl),
            by = "player_id") %>%
  mutate(band = dplyr::coalesce(band, band_row)) %>%
  filter(band %in% REC_BANDS)

faced_ne_rc <- faced_rows_rc %>%
  group_by(band) %>%
  summarise(tot_routes = sum(routes),
            unsc_n = sum(routes[is.na(mg25) & is.na(zg25)]),
            man_grade_25  = weighted.mean(mg25, w = routes, na.rm = TRUE),
            man_yprr_25   = weighted.mean(my25, w = routes, na.rm = TRUE),
            zone_grade_25 = weighted.mean(zg25, w = routes, na.rm = TRUE),
            zone_yprr_25  = weighted.mean(zy25, w = routes, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(unscored_share = unsc_n / tot_routes) %>%
  select(-unsc_n)

# THE HEALTHY + FACED WALLS vs canon cmp_rec_slate, cell by cell
cmp_chk <- cmp_rec_slate %>%
  select(band, starts_with("man_"), starts_with("zone_"),
         interp_share, tot_routes, unscored_share,
         starts_with("d_")) %>%
  left_join(slate_healthy_rc, by = "band",
            suffix = c("_c", "")) %>%
  left_join(faced_ne_rc %>%
              select(band, tot_routes, unscored_share,
                     man_grade_25, man_yprr_25,
                     zone_grade_25, zone_yprr_25) %>%
              rename_with(~ paste0(.x, "_l"),
                          -band),
            by = "band") %>%
  mutate(dd_man_grade_26  = man_grade_26  - man_grade_26_c,
         dd_man_yprr_26   = man_yprr_26   - man_yprr_26_c,
         dd_zone_grade_26 = zone_grade_26 - zone_grade_26_c,
         dd_zone_yprr_26  = zone_yprr_26  - zone_yprr_26_c,
         dd_interp        = interp_share  - interp_share_c,
         dd_man_grade_25  = man_grade_25_l  - man_grade_25,
         dd_man_yprr_25   = man_yprr_25_l   - man_yprr_25,
         dd_zone_grade_25 = zone_grade_25_l - zone_grade_25,
         dd_zone_yprr_25  = zone_yprr_25_l  - zone_yprr_25,
         dd_routes        = tot_routes_l    - tot_routes,
         dd_unsc          = unscored_share_l - unscored_share)

cat("\n-- WALL: NE healthy + faced slates reproduce canon\n")
cat("   cmp_rec_slate (6 bands x 11 cells)\n")
for (cc in paste0("dd_", c("man_grade_26", "man_yprr_26",
                           "zone_grade_26", "zone_yprr_26",
                           "interp",
                           "man_grade_25", "man_yprr_25",
                           "zone_grade_25", "zone_yprr_25",
                           "routes", "unsc"))) {
  wall_rc(paste0("cmp cell ", cc), cmp_chk, cc)
}

# ---- adjusted faced (observed-only; honesty = c3 unscored) ----
faced_c3_rc <- faced_rows_rc %>%
  left_join(rec_c3_pctl %>%
              filter(season == 2025, split == "man",
                     !is.na(player_id)) %>%
              select(player_id,
                     c3g_m = c3_grade_pctl, c3y_m = c3_yprr_pctl),
            by = "player_id") %>%
  left_join(rec_c3_pctl %>%
              filter(season == 2025, split == "zone",
                     !is.na(player_id)) %>%
              select(player_id,
                     c3g_z = c3_grade_pctl, c3y_z = c3_yprr_pctl),
            by = "player_id")

# membership wall: the adjusted frame must be the SAME game rows
# as the raw faced frame (observed-only law -- no new bodies)
mm1 <- faced_c3_rc %>%
  anti_join(faced_rows_rc, by = c("player_id", "week", "team_c"))
mm2 <- faced_rows_rc %>%
  anti_join(faced_c3_rc, by = c("player_id", "week", "team_c"))
if (nrow(mm1) || nrow(mm2)) {
  print(mm1); print(mm2)
  stop("adjusted/raw faced membership mismatch -- report")
}
cat("-- adjusted faced membership wall: same game rows as raw\n")
cat("   faced (observed-only law) OK\n")

faced_ne_c3_rc <- faced_c3_rc %>%
  group_by(band) %>%
  summarise(c3_routes = sum(routes),
            unsc_m_n = sum(routes[is.na(c3g_m)]),
            unsc_z_n = sum(routes[is.na(c3g_z)]),
            f_c3mg = weighted.mean(c3g_m, w = routes, na.rm = TRUE),
            f_c3my = weighted.mean(c3y_m, w = routes, na.rm = TRUE),
            f_c3zg = weighted.mean(c3g_z, w = routes, na.rm = TRUE),
            f_c3zy = weighted.mean(c3y_z, w = routes, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(unsc_c3m = unsc_m_n / c3_routes,
         unsc_c3z = unsc_z_n / c3_routes,
         across(c(f_c3mg, f_c3my, f_c3zg, f_c3zy), denan_rc)) %>%
  select(-unsc_m_n, -unsc_z_n)

# ---- the comparison frame: faced vs healthy vs priced, both
#      currencies, one row per band ----
cmp_priced_rc <- faced_ne_rc %>%
  select(band, tot_routes, unscored_share,
         f_mg = man_grade_25, f_my = man_yprr_25,
         f_zg = zone_grade_25, f_zy = zone_yprr_25) %>%
  left_join(slate_healthy_rc %>%
              select(band, interp_share, interp_m, interp_z,
                     h_mg = man_grade_26, h_my = man_yprr_26,
                     h_zg = zone_grade_26, h_zy = zone_yprr_26,
                     h_c3mg, h_c3my, h_c3zg, h_c3zy),
            by = "band") %>%
  left_join(slate_priced_rc, by = "band") %>%
  left_join(faced_ne_c3_rc %>%
              select(band, f_c3mg, f_c3my, f_c3zg, f_c3zy,
                     unsc_c3m, unsc_c3z),
            by = "band") %>%
  arrange(match(band, REC_BANDS))

# ---- per-split lens tables (one lens per table; every table
#      opens with its question) ----
show_lens_rc <- function(sp, cur, lens) {
  pre <- if (cur == "raw") {
    paste0(substr(sp, 1, 1), substr(lens, 1, 1))
  } else {
    paste0("c3", substr(sp, 1, 1), substr(lens, 1, 1))
  }
  interp_col <- if (sp == "man") "interp_m" else "interp_z"
  unsc_col <- if (cur == "raw") {
    "unscored_share"
  } else if (sp == "man") "unsc_c3m" else "unsc_c3z"
  q <- paste0(
    "\n-- ", toupper(sp), " | ", toupper(lens),
    if (cur == "raw") " (raw percentiles)" else " (adjusted: within-opponent game ranks)",
    " --\n-- which pass-catcher groups got harder or easier\n",
    "-- for NE's defense in 2026, and how much of that\n",
    "-- survives once injuries are priced in?\n")
  cat(q)
  tab <- cmp_priced_rc %>%
    transmute(band,
              faced_25   = .data[[paste0("f_", pre)]],
              healthy_26 = .data[[paste0("h_", pre)]],
              priced_26  = .data[[paste0("p_", pre)]],
              backup_gms = bup_g,
              interp_pct = .data[[interp_col]],
              unscored_pct = .data[[unsc_col]]) %>%
    mutate(d_healthy = healthy_26 - faced_25,
           d_real    = priced_26 - faced_25) %>%
    select(band, faced_25, healthy_26, d_healthy,
           priced_26, d_real, backup_gms, interp_pct,
           unscored_pct) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  print(tab, n = Inf)
  invisible(tab)
}

cat("\n== NE 2026 OPPONENT RECEIVING SLATE ==\n")
cat("higher = better receiver = harder for NE's pass defense.\n")
cat("d_healthy = 2026 healthy minus 2025 faced. d_real = 2026\n")
cat("injury-priced minus 2025 faced (the number that matters).\n")
cat("backup_gms = expected backup receiver-games on the slate\n")
cat("(split-neutral: availability is one number per player).\n")
cat("interp % = corps weight riding entry-year priors (that\n")
cat("split). unscored % = faced routes with no score (honesty).\n")

t_mg_raw <- show_lens_rc("man",  "raw", "grade")
t_my_raw <- show_lens_rc("man",  "raw", "yprr")
t_mg_adj <- show_lens_rc("man",  "adj", "grade")
t_my_adj <- show_lens_rc("man",  "adj", "yprr")
t_zg_raw <- show_lens_rc("zone", "raw", "grade")
t_zy_raw <- show_lens_rc("zone", "raw", "yprr")
t_zg_adj <- show_lens_rc("zone", "adj", "grade")
t_zy_adj <- show_lens_rc("zone", "adj", "yprr")

# ------------------------------------------------------------
# 6. LEAGUE SWEEP -- every defense's 2026 receiver slate,
#    healthy vs injury-priced, grade lens (the canon league
#    tail is grade-only) + the adjusted lens (layer-new).
#    faced25 stays canon's (observed-only); healthy v26 and
#    prior_share were already walled in section 4.
# ------------------------------------------------------------

# adjusted faced per defense (observed-only; unit grain like
# the canon league engine -- no band filter here)
faced_c3_lg <- rcv_base_rc %>%
  filter(season == 2025, in_season(week), routes >= X_QUAL_RC) %>%
  mutate(def_t = stringr::str_remove(def_ssn, "2025$")) %>%
  left_join(c3_25_w, by = "player_id") %>%
  group_by(def_t) %>%
  summarise(rt = sum(routes),
            unsc_a_m_n = sum(routes[is.na(c3_grade_pctl_man)]),
            unsc_a_z_n = sum(routes[is.na(c3_grade_pctl_zone)]),
            a25_m = weighted.mean(c3_grade_pctl_man, w = routes,
                                  na.rm = TRUE),
            a25_z = weighted.mean(c3_grade_pctl_zone, w = routes,
                                  na.rm = TRUE),
            .groups = "drop") %>%
  # final names carry _m / _z so the pivot below can split them
  mutate(unsc_a_m = unsc_a_m_n / rt,
         unsc_a_z = unsc_a_z_n / rt,
         across(c(a25_m, a25_z), denan_rc)) %>%
  select(def_t, a25_m, a25_z, unsc_a_m, unsc_a_z)

# NE cross-wall (same-mask law): the NE cell of the league-wide
# adjusted faced frame must equal a direct NE rebuild
ne_a25_unit <- rcv_base_rc %>%
  filter(def_ssn == "NE2025", in_season(week), routes >= X_QUAL_RC) %>%
  left_join(c3_25_w, by = "player_id") %>%
  summarise(a25_m = weighted.mean(c3_grade_pctl_man, w = routes,
                                  na.rm = TRUE),
            a25_z = weighted.mean(c3_grade_pctl_zone, w = routes,
                                  na.rm = TRUE))
fc3_ne_chk <- faced_c3_lg %>%
  filter(def_t == "NE") %>%
  mutate(d_m = a25_m - ne_a25_unit$a25_m,
         d_z = a25_z - ne_a25_unit$a25_z)
cat("\n== SECTION 6 -- league sweep ==\n")
cat("-- WALL: NE cells of the league adjusted-faced frame match\n")
cat("   the direct NE rebuild\n")
wall_rc("NE adjusted faced, man split",  fc3_ne_chk, "d_m")
wall_rc("NE adjusted faced, zone split", fc3_ne_chk, "d_z")

faced_c3_long <- faced_c3_lg %>%
  # columns a25_m/a25_z/unsc_a_m/unsc_a_z -> values a25/unsc_a x split m/z
  tidyr::pivot_longer(-def_t,
                      names_to = c(".value", "split"),
                      names_pattern = "(.+)_(m|z)$") %>%
  mutate(split = dplyr::if_else(split == "m", "man", "zone")) %>%
  rename(focal = def_t)

sweep_rc <- slate26_rc %>%
  left_join(league_sys_both %>%
              select(focal, split, faced25, unscored_share),
            by = c("focal", "split")) %>%
  left_join(faced_c3_long, by = c("focal", "split")) %>%
  mutate(d_h  = v26  - faced25,
         d_p  = v26p - faced25,
         d_ha = v26a - a25,
         d_pa = v26ap - a25)

stopifnot(nrow(sweep_rc) == 64L,
          !any(is.na(sweep_rc$faced25)))
# v26p follows canon NA law (a fully-NA value cell would mean
# every weighted member of a corps rode a NA band prior -- the
# section 3/4 receipts would have shown it upstream)

cat("-- adjusted-faced coverage per split (focals with no c3\n")
cat("   score at all -- want 0; observed-only honesty) --\n")
print(sweep_rc %>%
        group_by(split) %>%
        summarise(focals_missing_a25 = sum(is.na(a25)),
                  mean_unscored_adj = round(mean(unsc_a), 3),
                  .groups = "drop"))

cat("\n-- whose 2026 receiver slate got harder, league-wide, once\n")
cat("   injuries are priced in? (d_real = priced 2026 minus\n")
cat("   faced 2025; positive = harder for the defense)\n")
for (sp in c("man", "zone")) {
  ss <- sweep_rc %>%
    filter(split == sp) %>%
    mutate(rank_healthy = rank(-d_h), rank_priced = rank(-d_p)) %>%
    arrange(desc(d_p))
  cat("\n-- ", toupper(sp), " | GRADE raw: hardest priced slates --\n", sep = "")
  print(ss %>%
          select(focal, faced25, v26, v26p, d_h, d_p,
                 rank_healthy, rank_priced, prior_share,
                 unscored_share) %>%
          mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
          head(8))
  cat("-- ", toupper(sp), " | easiest priced slates --\n", sep = "")
  print(ss %>%
          select(focal, faced25, v26, v26p, d_h, d_p,
                 rank_healthy, rank_priced) %>%
          mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
          tail(8))
  cat("-- ", toupper(sp),
      " | league mean jump: healthy", sep = "")
  cat(" ", round(mean(ss$d_h), 3),
      " -> priced ", round(mean(ss$d_p), 3),
      " (pricing moves the mean by ",
      round(mean(ss$d_p) - mean(ss$d_h), 3), ")\n", sep = "")
  cat("-- ", toupper(sp), " | NE row: healthy d ",
      round(ss$d_h[ss$focal == "NE"], 3), " (#",
      ss$rank_healthy[ss$focal == "NE"], ") -> priced d ",
      round(ss$d_p[ss$focal == "NE"], 3), " (#",
      ss$rank_priced[ss$focal == "NE"], " of 32)\n", sep = "")
  cat("-- ", toupper(sp),
      " | GRADE adjusted (within-opponent ranks): hardest priced --\n",
      sep = "")
  print(ss %>%
          arrange(desc(d_pa)) %>%
          select(focal, a25, v26a, v26ap, d_ha, d_pa, unsc_a) %>%
          mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
          head(8))
  cat("-- ", toupper(sp), " | adjusted league mean: healthy ",
      round(mean(ss$d_ha, na.rm = TRUE), 3), " -> priced ",
      round(mean(ss$d_pa, na.rm = TRUE), 3), "\n", sep = "")
}

# ------------------------------------------------------------
# 7. BOARDS -- per-player answers (availability is split-
#    neutral; values shown per split, never recombined).
# ------------------------------------------------------------

cat("\n== SECTION 7 -- the boards ==\n")

b1_rc <- members_rc %>%
  filter(avail_src == "starter_history") %>%
  mutate(exp_miss = (1 - avail) * AVAIL_DENOM_RC,
         hit_m = mg_p - mg_f, hit_z = zg_p - zg_f) %>%
  arrange(avail, desc(usage_w)) %>%
  head(15) %>%
  select(roster_name, team_name, band, avail, job_seasons,
         exp_miss, mg_f, zg_f, repl_mg, repl_zg, hit_m, hit_z)

cat("\n-- BOARD 1 -- which projected corps receivers are the\n")
cat("   biggest injury bets? (job-holders with the thinnest\n")
cat("   suit-up record; hit = priced minus healthy, negative =\n")
cat("   quality his team probably loses)\n")
print(b1_rc %>% mutate(across(where(is.numeric), ~ round(.x, 3))),
      n = 15)

b2_rc <- members_rc %>%
  filter(avail_src == "backup_line") %>%
  arrange(desc(usage_w)) %>%
  head(15) %>%
  select(roster_name, team_name, band, entry_year, avail,
         usage_w, mg_f, zg_f)

cat("\n-- BOARD 2 -- corps members with no job history, priced at\n")
cat("   the backup first-year line (no phantom rows in this\n")
cat("   unit -- canon v1 law). count:",
    sum(members_rc$avail_src == "backup_line"), "\n")
print(b2_rc %>% mutate(across(where(is.numeric), ~ round(.x, 3))),
      n = 15)

n_rookie_ne <- sum(ne_frame_rc$entry_year == 2026)
n_rookie_lg <- sum(members_rc$entry_year == 2026)
cat("\n-- BOARD 3 -- rookies: NE-slate corps:", n_rookie_ne,
    "| league corps:", n_rookie_lg, "\n")
cat("   STRUCTURAL, same as pass-rush: rookies enter the NE\n")
cat("   corps only through Andy's delta tribble (currently\n")
cat("   empty = full sign-off), and the league machine corps\n")
cat("   requires a band label (canon filter drops no-history\n")
cat("   bodies). Added rookies price at the rookie first-year\n")
cat("   line automatically.\n")

tb4_rc <- members_rc %>%
  group_by(team_name, band) %>%
  summarise(bup_gms = sum(1 - avail) * AVAIL_DENOM_RC,
            .groups = "drop")
b4_rc <- tb4_rc %>%
  group_by(team_name) %>%
  summarise(exp_backup_gms = sum(bup_gms), .groups = "drop") %>%
  left_join(tb4_rc %>%
              group_by(team_name) %>%
              slice_max(bup_gms, n = 1, with_ties = FALSE) %>%
              select(team_name, soft_band = band,
                     soft_band_gms = bup_gms),
            by = "team_name") %>%
  left_join(repl_rc %>%
              select(band, fill_mg = repl_mg, fill_zg = repl_zg),
            by = c("soft_band" = "band")) %>%
  mutate(ne_opp = team_name %in% sched_2026) %>%
  arrange(desc(exp_backup_gms))

cat("\n-- BOARD 4 -- which teams are most exposed to receiver\n")
cat("   injuries in 2026? (expected backup receiver-games =\n")
cat("   17 x sum of (1 - avail) over the corps; soft band =\n")
cat("   the group carrying the most expected miss; fill = the\n")
cat("   replacement level there)\n")
print(b4_rc %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
        head(12), n = 12)
cat("   NE opponents flagged; the full 32-team frame is b4_rc\n")

# ------------------------------------------------------------
# 8. THE VISUAL -- one gt table per split, grade lens, raw +
#    adjusted. 14 opponents x 6 bands = 84 rows each. Cells:
#    per-(team, band) faced vs healthy vs priced; walls prove
#    the cells re-pool to the walled section-5 band slates.
# ------------------------------------------------------------

suppressPackageStartupMessages(library(gt))

cells_h_rc <- ne_frame_rc %>%
  group_by(team_name, band) %>%
  # sw_* = usage weight behind SCORED values only, one per walled
  # metric. The re-pool below must drop NA values exactly the way
  # the direct band slate does, or weighted means do not re-compose
  summarise(sw = sum(usage_w),
            sw_mg   = sum(usage_w[!is.na(mg_f)]),
            sw_zg   = sum(usage_w[!is.na(zg_f)]),
            sw_c3mg = sum(usage_w[!is.na(c3mg_f)]),
            sw_c3zg = sum(usage_w[!is.na(c3zg_f)]),
            interp_m = sum(usage_w[prior_mg]) / sum(usage_w),
            interp_z = sum(usage_w[prior_zg]) / sum(usage_w),
            h_mg   = weighted.mean(mg_f,   w = usage_w, na.rm = TRUE),
            p_mg   = weighted.mean(mg_p,   w = usage_w, na.rm = TRUE),
            h_zg   = weighted.mean(zg_f,   w = usage_w, na.rm = TRUE),
            p_zg   = weighted.mean(zg_p,   w = usage_w, na.rm = TRUE),
            h_c3mg = weighted.mean(c3mg_f, w = usage_w, na.rm = TRUE),
            p_c3mg = weighted.mean(c3mg_p, w = usage_w, na.rm = TRUE),
            h_c3zg = weighted.mean(c3zg_f, w = usage_w, na.rm = TRUE),
            p_c3zg = weighted.mean(c3zg_p, w = usage_w, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(across(c(interp_m, interp_z, starts_with(c("h_", "p_"))),
                denan_rc))

cells_f_rc <- faced_c3_rc %>%
  group_by(team_c, band) %>%
  # fr_* = routes behind SCORED values only, one per walled metric
  # (same re-composition law as the healthy side above)
  summarise(fr = sum(routes),
            fr_mg   = sum(routes[!is.na(mg25)]),
            fr_zg   = sum(routes[!is.na(zg25)]),
            fr_c3mg = sum(routes[!is.na(c3g_m)]),
            fr_c3zg = sum(routes[!is.na(c3g_z)]),
            unsc_n   = sum(routes[is.na(mg25) & is.na(zg25)]),
            unsc_m_n = sum(routes[is.na(c3g_m)]),
            unsc_z_n = sum(routes[is.na(c3g_z)]),
            f_mg   = weighted.mean(mg25,  w = routes, na.rm = TRUE),
            f_zg   = weighted.mean(zg25,  w = routes, na.rm = TRUE),
            f_c3mg = weighted.mean(c3g_m, w = routes, na.rm = TRUE),
            f_c3zg = weighted.mean(c3g_z, w = routes, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(unsc   = unsc_n / fr,
         unsc_m = unsc_m_n / fr,
         unsc_z = unsc_z_n / fr) %>%
  # denan in its own mutate: across() picks its columns from the
  # pre-verb frame, so freshly-made shares are not guaranteed to
  # be visible to it inside the same call
  mutate(across(c(unsc, unsc_m, unsc_z,
                  starts_with("f_")), denan_rc)) %>%
  select(-unsc_n, -unsc_m_n, -unsc_z_n)

# RE-POOL WALLS -- cells over the 17 slate rows must rebuild
# the (already canon-walled) band slates at 1e-8
repool_h <- tibble(team_name = sched_2026) %>%
  left_join(cells_h_rc, by = "team_name",
            relationship = "many-to-many") %>%
  group_by(band) %>%
  summarise(r_mg   = weighted.mean(h_mg,   w = sw_mg,   na.rm = TRUE),
            r_zg   = weighted.mean(h_zg,   w = sw_zg,   na.rm = TRUE),
            r_c3mg = weighted.mean(h_c3mg, w = sw_c3mg, na.rm = TRUE),
            r_c3zg = weighted.mean(h_c3zg, w = sw_c3zg, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(slate_healthy_rc, by = "band") %>%
  mutate(d1 = r_mg   - man_grade_26,
         d2 = r_zg   - zone_grade_26,
         d3 = r_c3mg - h_c3mg,
         d4 = r_c3zg - h_c3zg)

repool_f <- cells_f_rc %>%
  group_by(band) %>%
  summarise(r_mg   = weighted.mean(f_mg,   w = fr_mg,   na.rm = TRUE),
            r_zg   = weighted.mean(f_zg,   w = fr_zg,   na.rm = TRUE),
            r_c3mg = weighted.mean(f_c3mg, w = fr_c3mg, na.rm = TRUE),
            r_c3zg = weighted.mean(f_c3zg, w = fr_c3zg, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(faced_ne_rc, by = "band") %>%
  left_join(faced_ne_c3_rc %>% select(band, f_c3mg, f_c3zg),
            by = "band", suffix = c("", "_canon")) %>%
  mutate(d1 = r_mg   - man_grade_25,
         d2 = r_zg   - zone_grade_25,
         d3 = r_c3mg - f_c3mg,
         d4 = r_c3zg - f_c3zg)

cat("\n== SECTION 8 -- the visual ==\n")
cat("-- WALLS: visual cells re-pool to the walled band slates\n")
for (cc in paste0("d", 1:4)) {
  wall_rc(paste0("healthy cells -> band slate ", cc), repool_h, cc)
  wall_rc(paste0("faced cells -> band slate ", cc),   repool_f, cc)
}

vis_rc <- tidyr::expand_grid(team = unique(sched_2026),
                             band = REC_BANDS) %>%
  left_join(cells_h_rc, by = c("team" = "team_name", "band")) %>%
  left_join(cells_f_rc, by = c("team" = "team_c", "band")) %>%
  mutate(dh_mg  = h_mg   - f_mg,
         dr_mg  = p_mg   - f_mg,
         dh_zg  = h_zg   - f_zg,
         dr_zg  = p_zg   - f_zg,
         dra_mg = p_c3mg - f_c3mg,
         dra_zg = p_c3zg - f_c3zg)

stopifnot(nrow(vis_rc) == 84L)

# team order: hardest priced adjusted zone-grade jump first
# (zone is the headline lens for this unit -- the canon league
# receipt is a zone receipt); teams with no cell sort last
team_ord_rc <- vis_rc %>%
  group_by(team) %>%
  summarise(o = mean(dra_zg, na.rm = TRUE), .groups = "drop") %>%
  mutate(o = dplyr::if_else(is.nan(o), -1, o)) %>%
  arrange(desc(o)) %>%
  pull(team)

gt_rec_one <- function(sp) {
  f   <- if (sp == "man") "f_mg"   else "f_zg"
  h   <- if (sp == "man") "h_mg"   else "h_zg"
  p   <- if (sp == "man") "p_mg"   else "p_zg"
  fa  <- if (sp == "man") "f_c3mg" else "f_c3zg"
  pa  <- if (sp == "man") "p_c3mg" else "p_c3zg"
  dh  <- if (sp == "man") "dh_mg"  else "dh_zg"
  dr  <- if (sp == "man") "dr_mg"  else "dr_zg"
  dra <- if (sp == "man") "dra_mg" else "dra_zg"
  ua  <- if (sp == "man") "unsc_m" else "unsc_z"
  ic  <- if (sp == "man") "interp_m" else "interp_z"
  tt  <- if (sp == "man") "Man" else "Zone"
  vis_rc %>%
    mutate(team = factor(team, levels = team_ord_rc),
           band = factor(band, levels = REC_BANDS)) %>%
    arrange(team, band) %>%
    select(team, band,
           f25 = .data[[f]],  h26 = .data[[h]],
           dh  = .data[[dh]], p26 = .data[[p]],
           dr  = .data[[dr]],
           fa25 = .data[[fa]], pa26 = .data[[pa]],
           dra = .data[[dra]],
           unsc = .data[[ua]], interp = .data[[ic]]) %>%
    gt(groupname_col = "team", rowname_col = "band") %>%
    tab_spanner(label = "Raw",
                columns = c(f25, h26, dh, p26, dr)) %>%
    tab_spanner(label = "Adjusted (same-slate)",
                columns = c(fa25, pa26, dra)) %>%
    cols_label(f25 = "'25", h26 = "'26", dh = "\u0394",
               p26 = "priced '26", dr = "real \u0394",
               fa25 = "'25", pa26 = "priced '26",
               dra = "real \u0394",
               unsc = "unscored %", interp = "interp %") %>%
    fmt_percent(columns = c(f25, h26, p26, fa25, pa26,
                            unsc, interp),
                decimals = 0) %>%
    fmt_percent(columns = c(dh, dr, dra), decimals = 0,
                force_sign = TRUE) %>%
    sub_missing(missing_text = "--") %>%
    data_color(columns = c(dr, dra),
               fn = scales::col_numeric(
                 c("#6baed6", "#f7f7f7", "#C60C30"),
                 domain = c(-0.3, 0.3), na.color = "#f7f7f7"),
               autocolor_text = TRUE) %>%
    tab_header(
      title = paste0("2026 opponent pass catchers vs NE's ",
                     tt, " coverage \u2014 grade lens"),
      subtitle = paste0(
        "higher = better receiver = harder for NE's defense. ",
        "raw = band percentiles; adjusted = within-opponent ",
        "game ranks (same-slate). priced = availability-",
        "weighted. unscored % = faced routes with no score on ",
        "this currency (honesty). interp % = corps weight on ",
        "entry-year priors. -- = no faced routes or no corps ",
        "member in that cell.")) %>%
    tab_options(table.font.size = px(12),
                data_row.padding = px(3),
                column_labels.font.weight = "bold",
                row_group.font.weight = "bold")
}

gt_rec_man  <- gt_rec_one("man")
gt_rec_zone <- gt_rec_one("zone")

# viewer law: one gt at a time, each print replaces the last --
# the money table prints LAST. Recall either with print(obj).
# save checkpoints (uncomment to use):
# gtsave(gt_rec_man,  "gt_rec_man.html")
# gtsave(gt_rec_zone, "gt_rec_zone.html")
cat("\nrecall tables: print(gt_rec_man)  |  print(gt_rec_zone)\n")
print(gt_rec_man)
print(gt_rec_zone)

# ------------------------------------------------------------
# CHECKPOINT
# ------------------------------------------------------------

cat("\n== CHECKPOINT -- receiving availability layer ==\n")
cat("deliverables in session:\n")
cat("  cmp_priced_rc   NE slate, band x lens, faced/healthy/\n")
cat("                  priced, both currencies\n")
cat("  sweep_rc        64-row league sweep (focal x split),\n")
cat("                  healthy + priced, raw + adjusted\n")
cat("  b1_rc..b4_rc    boards (fragile starters / backup-line\n")
cat("                  members / rookies structural / team\n")
cat("                  exposure)\n")
cat("  gt_rec_man, gt_rec_zone  the two visual tables\n")
cat("walls run: canon league reproduction (v26 + prior_share,\n")
cat("64 cells), rot valuation provenance (4 values + weights +\n")
cat("flags), NE healthy/faced vs cmp_rec_slate (66 cells),\n")
cat("adjusted-faced membership, NE cross-wall, visual re-pool\n")
cat("(16 cells). All at 1e-8 on unrounded values.\n")
cat("canon objects: consumed, never modified. Re-source WHOLE\n")
cat("file after any edit. ASCII only.\n")
