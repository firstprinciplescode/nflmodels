# ============================================================
# NE 2026 -- OPPONENT SECONDARY SoS -- PHASE 6b BUILD FILE
# THE SPLIT SLATES (man-hardness / zone-hardness, SCB un-folded)
# v1, written 2026-08-16, on Andy's full-menu sign-off
# ("those sound good to me, all of it. let's let it run").
#
# STAMPS (ruled 2026-08-16 off the v1.1 ritual run -- fixed,
#   do not re-litigate):
#   GATES:  X_MAN = 6 man snaps / G_MAN = 6 games.
#           X_ZONE = 13 zone snaps / G_ZONE = 6 games.
#           (G law ports: spot-duty dies at G >= 6 -- the cal-room
#           gm4->gm6 drops, Emmanwori 8->5, Williams 11->9.)
#   N PER BAND, PER CURRENCY: CB 3 / SCB 1 / S 3 / LB 2 (the
#           sweep's 15/6 verdict; handoff's CB_out 2 expectation
#           lost). EACH slate fills to N -- man slate and zone
#           slate separately (per-currency ruling). SCB-man runs
#           ~40% phantom: prior-heavy by design, flagged, legal.
#   SLATE MEMBERSHIP: modal band (UN-GATED frame, law) ->
#           per-split gate -> top-N by split usage (pmax '25/'24,
#           the Ed Oliver receipt) -> phantom fill to N.
#   FACETS (menu item 5b -- receipt-first loop): block 3 prints
#           the full luck card at the stamped floors FIRST. The
#           FACET LAW below seats: grade everywhere a pool
#           exists; zone supp (cmp_rt inverted) seated CB + S,
#           context LB, dead SCB; man supp NOISE-FLAGGED every
#           band (printed, never seated -- it floors only at
#           man 15/6, which costs 7 zero-teams); pbu + all else
#           context pending the block-3 receipt. Andy vetoes by
#           editing FACET LAW + re-sourcing WHOLE file.
#   PRIOR LAW: the promotion audit RUNS IN-FILE (block 5) per
#           split -- method ports verbatim from the combined
#           audit, births the SCB class (v2 has no SCB row, so
#           "carry v2" was unavailable), re-centers phantom
#           prices per split pool (man pbu base .0977 vs zone
#           .0658 -- the pools genuinely differ). Fallback rungs
#           printed per cell. Override = edit + re-source.
#
# RECEIPTS CARRIED (discovery v2 + ritual v1.1, 2026-08-16):
#   All 11 split facets resolve SCHEME-SIDE (nfl_data.coverage_
#   scheme), both splits, all numeric; grade NA share 0.000.
#   Join BY NAME on (player_id, week, season): 69,250 = 69,250,
#   no fan-out. Denominators: man .321 / zone .679 of man+zone.
#   split_vs_comb = .888 -- ~11% of coverage snaps are NEITHER
#   man nor zone. LAW: splits are priced separately and NEVER
#   recombine into one number; the other bucket lives only in
#   the Phase 6 combined currency (checkpointed, stands as-is).
#   Man-share season drift .358 (2016) -> .279 (2025); within-
#   season pctls absorb it. Band4 vocab: LB 24,279 / S 16,948 /
#   CB 14,503 / SCB 5,686. Band-identity tripwire: 979 multi-
#   band player-seasons consolidated to modal (combined snaps,
#   un-gated). EDGE RECEIPT 0/0: the un-gated modal map drops
#   nobody who gates a split. NE faced = NE own = the same 21
#   weeks of 2025. Base rates: man p_cmp .606 / p_pbu .0977;
#   zone p_cmp .723 / p_pbu .0658 -- the splits PRICE
#   DIFFERENTLY; never recombine.
#   CAL ROOMS (v1.1, na.rm-fixed): SEA -- Witherspoon gates
#   BOTH currencies deep (gm6 9, gz13 13), modal CB: SEA's slot
#   job is a CB-modal two-currency object; SEA SCB phantom-
#   fills; Kendrick never gates; Love is full-time combined
#   (g15c 16) but spot-duty WITHIN each split (gm6 3 / gz8 11).
#   NE -- Marcus Jones gates both (the SCB anchor); Williams
#   both; Davis ZONE-ONLY (gz13 14, gm6 3 -- his scouting
#   profile in one row); Mapu/Tavai zone-only; Woodson both;
#   edge droppers (Nwosu/Mafe/Hall) 0/0 everywhere.
#
# TOWER LANDMINES carried (do not rebuild the tower): index
#   renames never touched; columns resolve BY NAME with fuzzy
#   candidates on failure; the cache frame is team-fixed.
#
# OE CANDIDATE: NOT PORTED to 6b. receiving_coverage_versus has
#   no stamped scheme split; the combined build's candidate
#   posture stands. If a man/zone OE column ever surfaces, the
#   orientation block re-runs on request.
#
# SOURCE ORDER:
#   (1) session canon: combined_grade_epa_summary (qbgrp join
#       runs LIVE every run -- ONE SESSION LAW), combined_ids_
#       defense, pff_team_lookup, opp_2026_teams, sched_2026,
#       blend2, in_season
#   (2) coverage_raw_build_cache_cov.rds in getwd() (absent ->
#       run the Phase 6 step-0 file first)
#   (3) coverage_scheme -- in-session object preferred, else
#       coverage_scheme_cache_mz.rds, else a raw pull (paid
#       once, cached). Grain tripwire STOPS the file.
#   Re-source WHOLE file after any edit. ASCII only.
#
# Direction: opposing COVERAGE defenders vs NE's PASSING game.
#   higher pctl = better defender = HARDER for Maye's pass game.
#   Faced side keys off startsWith(qbgrp_ssn, "NE"); NE's own
#   unit = def_ssn == "NE2025" (calibration eyeball).
#
# SUFFIX LAW: split-neutral objects end _mz. Per-split column
#   lists are CM / CZ. Per-split results carry a `split` column
#   and NEVER recombine into one number. dplyr::n() only inside
#   verbs. No index renames, ever.
# ============================================================

library(dplyr); library(tidyr); library(stringr)

needed_mz <- c("combined_grade_epa_summary", "combined_ids_defense",
               "pff_team_lookup", "opp_2026_teams", "sched_2026",
               "blend2", "in_season")
missing_mz <- needed_mz[!vapply(needed_mz, exists, logical(1))]
if (length(missing_mz)) stop("missing session objects: ",
                             paste(missing_mz, collapse = ", "),
                             " -- see SOURCE ORDER in header")

if (!exists("percent_rank_avg"))
  percent_rank_avg <- function(x) {
    r <- rank(x, ties.method = "average", na.last = "keep")
    r / sum(!is.na(x))
  }

if (!exists("run_athena_query")) {
  run_athena_query <- function(sql, max_wait = 120) {
    start_cmd <- sprintf(
      'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
      gsub('"', '\\"', sql)
    )
    query_id <- system(start_cmd, intern = TRUE)
    status <- "RUNNING"
    elapsed <- 0
    while (status %in% c("RUNNING", "QUEUED") && elapsed < max_wait) {
      Sys.sleep(2)
      elapsed <- elapsed + 2
      status_cmd <- sprintf(
        'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.State" --output text',
        query_id
      )
      status <- trimws(system(status_cmd, intern = TRUE))
    }
    if (status != "SUCCEEDED") {
      error_cmd <- sprintf(
        'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.StateChangeReason" --output text',
        query_id
      )
      error_msg <- system(error_cmd, intern = TRUE)
      stop(sprintf("Query failed with status %s: %s", status, error_msg))
    }
    result_cmd <- sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.ResultConfiguration.OutputLocation" --output text',
      query_id
    )
    s3_path <- system(result_cmd, intern = TRUE)
    read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
  }
}

if (!exists("resolve_col"))
  resolve_col <- function(prefs, pool, label, required = TRUE) {
    hit <- prefs[prefs %in% pool][1]
    if (is.na(hit)) {
      fuzz <- grep(paste(prefs, collapse = "|"), pool,
                   value = TRUE, ignore.case = TRUE)
      msg <- paste0(label, ": no exact match. candidates: ",
                    paste(fuzz, collapse = ", "))
      if (required) stop(msg, " -- stamp the right name and rerun")
      cat(msg, "  (optional, skipped)\n")
      return(NA_character_)
    }
    cat(label, "->", hit, "\n")
    hit
  }

safe_sd <- function(x) if (sum(!is.na(x)) < 2) NA_real_ else sd(x, na.rm = TRUE)

# ------------------------------------------------------------
# 0. FEED 1 -- the Phase 6 cache + the live qbgrp join.
# ------------------------------------------------------------

CACHE_COV <- "coverage_raw_build_cache_cov.rds"   # lives in getwd()
if (!file.exists(CACHE_COV))
  stop("cache absent -- run phase6_step0_secondary_gate_ritual.txt first")

coverage_built_mz <- readRDS(CACHE_COV)
cat("feed 1: cached raw build ->", normalizePath(CACHE_COV),
    " | rows:", nrow(coverage_built_mz), "\n")

coverage_qbgrp_mz <- tibble::as_tibble(coverage_built_mz) %>%
  left_join(combined_grade_epa_summary %>%
              select(opp, week, season, qbgrp_ssn, def_ssn),
            by = c("team_name" = "opp", "week" = "week", "season" = "season"))

cat("\nqbgrp join completeness (unmatched rows, want ~0):\n")
print(sum(is.na(coverage_qbgrp_mz$qbgrp_ssn)))

bad_tm <- c("ARI", "BAL", "CLE", "HOU")
stopifnot(!any(coverage_qbgrp_mz$team %in% bad_tm),
          !any(coverage_qbgrp_mz$team_name %in% bad_tm))

# ------------------------------------------------------------
# 0a. FEED 2 -- coverage_scheme. In-session object preferred,
#     else the cache, else a raw pull (paid once, cached).
#     Grain tripwire: unique on (player_id, week, season) or
#     this file STOPS.
# ------------------------------------------------------------

SCHEME_CACHE <- "coverage_scheme_cache_mz.rds"
if (exists("coverage_scheme")) {
  cat("feed 2: in-session coverage_scheme (the tower pull)\n")
  coverage_scheme_mz <- tibble::as_tibble(coverage_scheme)
} else if (file.exists(SCHEME_CACHE)) {
  cat("feed 2: cached scheme ->", normalizePath(SCHEME_CACHE), "\n")
  coverage_scheme_mz <- readRDS(SCHEME_CACHE)
} else {
  cat("feed 2: RAW ATHENA PULL -- nfl_data.coverage_scheme (paid once, cached)\n")
  coverage_scheme_mz <- tibble::as_tibble(run_athena_query("
      SELECT  *
      FROM    nfl_data.coverage_scheme
  "))
  saveRDS(coverage_scheme_mz, SCHEME_CACHE)
  cat("feed 2: cached ->", normalizePath(SCHEME_CACHE), "\n")
}

sch_dup_mz <- coverage_scheme_mz %>%
  count(player_id, week, season) %>%
  filter(n > 1)
if (nrow(sch_dup_mz) > 0) {
  print(head(sch_dup_mz, 10))
  stop("scheme grain NOT unique on (player_id, week, season) -- ",
       "the 3-key join would fan out. Stamp the tower 12-key join by hand.")
}
cat("scheme grain: unique on (player_id, week, season) -- OK\n")

# ------------------------------------------------------------
# 0b. COLUMN RESOLUTION -- combined anchors off the cache; the
#     split card TWO-STAGE (cache pool, then scheme pool). The
#     scheme-side columns join BY NAME with a no-fan-out
#     stopifnot. Core facets (grade / snaps / tgt / rec / pbu)
#     unresolved in BOTH pools = hard stop.
# ------------------------------------------------------------

CN <- list()
CN$grade <- resolve_col(c("grades_coverage_defense", "grades_coverage"),
                        names(coverage_qbgrp_mz), "combined coverage grade")
CN$snaps <- resolve_col(c("snap_counts_coverage", "coverage_snaps"),
                        names(coverage_qbgrp_mz), "combined coverage snaps")

split_prefs_mz <- list(
  grade = c("S_grades_coverage_defense", "S_coverage_grade",
            "S_grades_coverage", "grades_coverage_defense_S"),
  snaps = c("S_snap_counts_coverage", "snap_counts_coverage_S"),
  tgt   = c("S_targets", "S_targets_against", "targets_S"),
  rec   = c("S_receptions", "S_receptions_allowed", "receptions_S"),
  pbu   = c("S_pass_break_ups", "S_pass_breakups", "S_pbu"),
  yards = c("S_yards_allowed", "S_yards", "yards_allowed_S"),
  yac   = c("S_yards_after_catch", "S_yac"),
  int   = c("S_interceptions", "S_ints", "interceptions_S"),
  qbr   = c("S_qb_rating_against", "qb_rating_against_S"),
  adot  = c("S_avg_depth_of_target", "S_adot"),
  mt    = c("S_missed_tackle_rate", "missed_tackle_rate_S"))

resolve_split_mz <- function(facet, split) {
  prefs <- gsub("S", split, split_prefs_mz[[facet]], fixed = TRUE)
  hit <- prefs[prefs %in% names(coverage_qbgrp_mz)][1]
  if (!is.na(hit)) return(list(col = hit, src = "summary"))
  hit <- prefs[prefs %in% names(coverage_scheme_mz)][1]
  if (!is.na(hit)) return(list(col = hit, src = "scheme"))
  fuzz <- grep(split, c(names(coverage_qbgrp_mz), names(coverage_scheme_mz)),
               value = TRUE, ignore.case = TRUE)
  fuzz <- unique(grep(facet, fuzz, value = TRUE, ignore.case = TRUE))
  cat(split, "/", facet, ": unresolved. fuzzy candidates: ",
      paste(fuzz, collapse = ", "), "  (optional, skipped)\n", sep = "")
  list(col = NA_character_, src = NA_character_)
}

cat("\n================ SPLIT CARD RESOLUTION ================\n")
CN_MAN  <- lapply(names(split_prefs_mz), function(f) resolve_split_mz(f, "man"))
CN_ZONE <- lapply(names(split_prefs_mz), function(f) resolve_split_mz(f, "zone"))
names(CN_MAN) <- names(split_prefs_mz); names(CN_ZONE) <- names(split_prefs_mz)

card_print_mz <- bind_rows(lapply(names(split_prefs_mz), function(f)
  tibble(facet = f,
         man_col  = CN_MAN[[f]]$col,  man_src  = CN_MAN[[f]]$src,
         zone_col = CN_ZONE[[f]]$col, zone_src = CN_ZONE[[f]]$src)))
cat("\n--- split card coverage matrix (NA = facet dead at the split) ---\n")
print(card_print_mz, n = Inf)

core_facets_mz <- c("grade", "snaps", "tgt", "rec", "pbu")
core_miss_mz <- card_print_mz %>%
  filter(facet %in% core_facets_mz,
         is.na(man_col) | is.na(zone_col))
if (nrow(core_miss_mz)) {
  cat("\nCORE SPLIT FACETS UNRESOLVED:\n"); print(core_miss_mz)
  stop("core split facets missing in both tables -- paste back to Andy")
}

sch_need_mz <- unique(na.omit(c(
  unname(sapply(CN_MAN,  function(x) if (x$src == "scheme") x$col else NA_character_)),
  unname(sapply(CN_ZONE, function(x) if (x$src == "scheme") x$col else NA_character_)))))

if (length(sch_need_mz)) {
  cat("\n--- scheme columns entering the 6b feed (BY NAME) ---\n")
  print(sch_need_mz)
  n_pre <- nrow(coverage_qbgrp_mz)
  coverage_qbgrp_mz <- coverage_qbgrp_mz %>%
    left_join(coverage_scheme_mz %>%
                select(player_id, week, season, all_of(sch_need_mz)),
              by = c("player_id", "week", "season"))
  cat("feed rows post-join:", nrow(coverage_qbgrp_mz),
      " (pre-join:", n_pre, " -- equal = no fan-out)\n")
  stopifnot(nrow(coverage_qbgrp_mz) == n_pre)
}

CM <- sapply(CN_MAN,  `[[`, "col")
CZ <- sapply(CN_ZONE, `[[`, "col")

cat("\n--- resolved split classes ---\n")
for (cc in unique(c(CM, CZ)))
  cat(cc, ":", class(coverage_qbgrp_mz[[cc]]), "\n")

# ------------------------------------------------------------
# 1. THE CANONICAL CHAIN -- FOUR BANDS, SCB un-folded at birth.
#    One row per player-week is the law (fan-out tripwire).
#    MODAL BAND MAP on the UN-GATED frame (design law): band
#    identity is one per player-season, modal band4 by COMBINED
#    coverage snaps. CURRENCY ENTRY gates per split. Identity
#    vs currency, two different questions, two different gates.
# ------------------------------------------------------------

SEC_BANDS_MZ <- c("CB", "SCB", "S", "LB")

secondary_qbgrp_mz <- coverage_qbgrp_mz %>%
  mutate(band4 = dplyr::case_when(
    final_position == "SCB"            ~ "SCB",
    final_position %in% c("MLB", "LB") ~ "LB",
    TRUE ~ final_position)) %>%
  filter(band4 %in% SEC_BANDS_MZ,
         !is.na(.data[[CN$grade]]),
         .data[[CN$snaps]] > 0)

dup_check_mz <- secondary_qbgrp_mz %>%
  count(player_id, week, season) %>%
  filter(n > 1)
if (nrow(dup_check_mz)) {
  print(head(dup_check_mz, 10))
  stop("fan-out: ", nrow(dup_check_mz), " player-weeks with >1 row")
}

cat("\n--- coverage FACED by NE offense 2025 (expect the 21 weeks) ---\n")
print(secondary_qbgrp_mz %>%
        filter(startsWith(qbgrp_ssn, "NE"), season == 2025) %>%
        distinct(week) %>% arrange(week), n = Inf)
cat("\n--- NE's OWN coverage unit 2025 (expect the same 21) ---\n")
print(secondary_qbgrp_mz %>% filter(def_ssn == "NE2025") %>%
        distinct(week) %>% arrange(week), n = Inf)

modal_band_mz <- secondary_qbgrp_mz %>%
  count(player_id, season, band4, wt = .data[[CN$snaps]], name = "sn") %>%
  group_by(player_id, season) %>%
  slice_max(sn, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, season, band = band4)

multi_band_mz <- secondary_qbgrp_mz %>%
  distinct(player_id, season, band4) %>%
  count(player_id, season) %>%
  filter(n > 1)
cat("\n--- band-identity tripwire: player-seasons spanning >1 band4\n")
cat("    (consolidated to modal band by combined coverage snaps):",
    nrow(multi_band_mz), "---\n")

# ------------------------------------------------------------
# 1b. DUMMY FILL -- optional facets unresolved get zero-filled
#     dummies, flagged off; the currency builder posts them
#     back to NA. Per split.
# ------------------------------------------------------------

split_setup_mz <- function(C_flat, sp, data) {
  opt <- c("yards", "yac", "int", "qbr", "adot", "mt")
  OK <- setNames(rep(TRUE, length(opt)), opt)
  for (f in opt) {
    if (is.na(C_flat[f])) {
      d <- paste0(".__", sp, "_", f, "_na__")
      data[[d]] <- 0
      C_flat[f] <- d
      OK[f] <- FALSE
    }
  }
  list(C = C_flat, OK = OK, data = data)
}

setup_man_mz  <- split_setup_mz(CM, "man", secondary_qbgrp_mz)
secondary_qbgrp_mz <- setup_man_mz$data; CM <- setup_man_mz$C; OK_MAN <- setup_man_mz$OK
setup_zone_mz <- split_setup_mz(CZ, "zone", secondary_qbgrp_mz)
secondary_qbgrp_mz <- setup_zone_mz$data; CZ <- setup_zone_mz$C; OK_ZONE <- setup_zone_mz$OK

cat("\n--- optional-facet flags ---\n")
cat("man : "); print(OK_MAN)
cat("zone: "); print(OK_ZONE)

# ------------------------------------------------------------
# 2. THE SPLIT CURRENCY BUILDER -- season metrics as a function
#    of the split game gate. Percentiles arrive in block 4,
#    within band x season x split -- never recombined.
# ------------------------------------------------------------

season_currency_mz <- function(x_qual, g_min, split, data = secondary_qbgrp_mz) {
  C  <- if (split == "man") CM else CZ
  OK <- if (split == "man") OK_MAN else OK_ZONE
  s <- data %>%
    filter(.data[[C["snaps"]]] >= x_qual) %>%
    inner_join(modal_band_mz, by = c("player_id", "season")) %>%
    group_by(player, player_id, band, season) %>%
    summarise(
      q_games = dplyr::n(),
      q_snaps = sum(.data[[C["snaps"]]], na.rm = TRUE),
      q_tgts  = sum(.data[[C["tgt"]]],   na.rm = TRUE),
      q_rec   = sum(.data[[C["rec"]]],   na.rm = TRUE),
      grade   = weighted.mean(.data[[C["grade"]]], w = .data[[C["snaps"]]], na.rm = TRUE),
      pbu_rt  = sum(.data[[C["pbu"]]],   na.rm = TRUE) / pmax(q_tgts, 1),
      cmp_rt  = q_rec / pmax(q_tgts, 1),
      ypt_alw = sum(.data[[C["yards"]]], na.rm = TRUE) / pmax(q_tgts, 1),
      yac_alw = sum(.data[[C["yac"]]],   na.rm = TRUE) / pmax(q_rec, 1),
      ypcs_alw = sum(.data[[C["yards"]]], na.rm = TRUE) / pmax(q_snaps, 1),
      int_rt  = sum(.data[[C["int"]]],   na.rm = TRUE) / pmax(q_tgts, 1),
      sn_per_tgt = q_snaps / pmax(q_tgts, 1),
      sn_per_rec = q_snaps / pmax(q_rec, 1),
      qbr_alw = weighted.mean(.data[[C["qbr"]]], w = .data[[C["tgt"]]], na.rm = TRUE),
      adot    = weighted.mean(.data[[C["adot"]]], w = .data[[C["tgt"]]], na.rm = TRUE),
      mt_rt   = weighted.mean(.data[[C["mt"]]], w = .data[[C["snaps"]]], na.rm = TRUE),
      .groups = "drop") %>%
    filter(q_games >= g_min)
  # honesty pass: dummies back to NA; NaN (all-NA weighted means) to NA
  if (!OK["yards"]) { s$ypt_alw <- NA_real_; s$ypcs_alw <- NA_real_ }
  if (!OK["int"])     s$int_rt  <- NA_real_
  if (!OK["adot"])    s$adot    <- NA_real_
  if (!OK["yac"])     s$yac_alw <- NA_real_
  if (!OK["qbr"])     s$qbr_alw <- NA_real_
  if (!OK["mt"])      s$mt_rt   <- NA_real_
  for (cc in c("grade", "qbr_alw", "adot", "mt_rt"))
    s[[cc]][is.nan(s[[cc]])] <- NA_real_
  s
}

# ------------------------------------------------------------
# 3. CONSTANTS (ruled 2026-08-16) + FIRST RECEIPT (menu 5b):
#    the full luck card AT THE STAMPED FLOORS, printed BEFORE
#    any currency is seated. Andy vetoes facets off this
#    receipt: edit FACET LAW + re-source WHOLE file.
# ------------------------------------------------------------

X_MAN  <- 6;   G_MAN  <- 6L
X_ZONE <- 13;  G_ZONE <- 6L
N_BAND_MZ <- c(CB = 3L, SCB = 1L, S = 3L, LB = 2L)  # PER CURRENCY

yardstick_stamp_mz <- bind_rows(lapply(c("man", "zone"), function(sp) {
  x <- if (sp == "man") X_MAN else X_ZONE
  g <- if (sp == "man") G_MAN else G_ZONE
  s <- season_currency_mz(x, g, sp)
  s %>%
    group_by(band) %>%
    summarise(
      n_seasons = dplyr::n(),
      per_team  = dplyr::n() / dplyr::n_distinct(s$season) / 32,
      tgts_p10  = quantile(q_tgts, .1, na.rm = TRUE),
      sd_grade  = safe_sd(grade),
      sd_pbu    = safe_sd(pbu_rt),
      sd_cmp    = safe_sd(cmp_rt),
      sd_ypt    = safe_sd(ypt_alw),
      sd_qbr    = safe_sd(qbr_alw),
      sd_snt    = safe_sd(sn_per_tgt),
      .groups = "drop") %>%
    mutate(split = sp, X = x, G = g, .before = 1)
}))

base_rt_mz <- bind_rows(lapply(c("man", "zone"), function(sp) {
  C  <- if (sp == "man") CM else CZ
  OK <- if (sp == "man") OK_MAN else OK_ZONE
  fl <- if (sp == "man") 8 else 13
  secondary_qbgrp_mz %>%
    filter(.data[[C["snaps"]]] >= fl) %>%
    summarise(
      p_pbu = sum(.data[[C["pbu"]]], na.rm = TRUE) /
        sum(.data[[C["tgt"]]], na.rm = TRUE),
      p_cmp = sum(.data[[C["rec"]]], na.rm = TRUE) /
        sum(.data[[C["tgt"]]], na.rm = TRUE),
      p_int = if (OK["int"]) sum(.data[[C["int"]]], na.rm = TRUE) /
        sum(.data[[C["tgt"]]], na.rm = TRUE) else NA_real_) %>%
    mutate(split = sp, ref_floor = fl, .before = 1)
}))

cat("\n--- split base rates (ritual receipts: man .606/.0977, zone .723/.0658) ---\n")
print(base_rt_mz)

luck_stamp_mz <- yardstick_stamp_mz %>%
  left_join(base_rt_mz %>% select(split, p_pbu, p_cmp), by = "split") %>%
  mutate(luck_pbu = sqrt(p_pbu * (1 - p_pbu) / tgts_p10),
         luck_cmp = sqrt(p_cmp * (1 - p_cmp) / tgts_p10),
         pbu_verdict = dplyr::case_when(
           luck_pbu <= sd_pbu     ~ "FLOOR MET",
           luck_pbu <= sd_pbu / 2 ~ "LUXURY",
           TRUE ~ "NOISE"),
         cmp_verdict = dplyr::case_when(
           luck_cmp <= sd_cmp     ~ "FLOOR MET",
           luck_cmp <= sd_cmp / 2 ~ "LUXURY",
           TRUE ~ "NOISE"))

cat("\n================ FIRST RECEIPT (menu 5b) ================\n")
cat("--- the full luck card AT THE STAMPED FLOORS (man 6/6, zone 13/6) ---\n")
print(luck_stamp_mz %>%
        select(split, band, n_seasons, per_team, tgts_p10,
               sd_grade, sd_pbu, luck_pbu, pbu_verdict,
               sd_cmp, luck_cmp, cmp_verdict), n = Inf)
cat("\n--- context sds at the stamped floors (ypt / qbr / snt) ---\n")
print(yardstick_stamp_mz %>%
        select(split, band, tgts_p10, sd_ypt, sd_qbr, sd_snt), n = Inf)

grade_deciles_mz <- bind_rows(lapply(c("man", "zone"), function(sp) {
  x <- if (sp == "man") X_MAN else X_ZONE
  g <- if (sp == "man") G_MAN else G_ZONE
  season_currency_mz(x, g, sp) %>%
    group_by(band) %>%
    summarise(n   = dplyr::n(),
              p10 = quantile(grade, .1,  na.rm = TRUE),
              p25 = quantile(grade, .25, na.rm = TRUE),
              p50 = quantile(grade, .5,  na.rm = TRUE),
              p75 = quantile(grade, .75, na.rm = TRUE),
              p90 = quantile(grade, .9,  na.rm = TRUE),
              .groups = "drop") %>%
    mutate(split = sp, ref = paste0(x, "/", g), .before = 1)
}))
cat("\n--- GRADE LEVELS per split at the stamped floors (deciles) ---\n")
print(grade_deciles_mz, n = Inf)

# --- FACET LAW (stamped defaults off the v1.1 grid; the receipt
#     above is the veto window). Seats: "seated" = currency;
#     "noise" = printed, never seated; "context" = respect stat;
#     "dead" = not computed into pctls.
facet_law_mz <- tribble(
  ~split, ~band, ~grade_seat, ~supp_seat, ~pbu_seat,
  "man",  "CB",  "seated", "noise",   "context",
  "man",  "SCB", "seated", "noise",   "context",
  "man",  "S",   "seated", "noise",   "context",
  "man",  "LB",  "seated", "noise",   "context",
  "zone", "CB",  "seated", "seated",  "context",
  "zone", "SCB", "seated", "dead",    "context",
  "zone", "S",   "seated", "seated",  "context",
  "zone", "LB",  "seated", "context", "context")
cat("\n--- FACET LAW (edit + re-source to veto, off the receipt above) ---\n")
print(facet_law_mz, n = Inf)

# ------------------------------------------------------------
# 4. THE SPLIT CURRENCIES -- FINAL. Gates: the stamped cells.
#    Percentiles within band x season x split. INVERSIONS
#    (defense side): supp = pctl of -cmp_rt; ypt / yac / ypcs /
#    qbr / mt same law (LOWER = harder). pbu / int / snt run
#    straight. NEVER recombined across splits.
# ------------------------------------------------------------

cov_season_pctl_mz <- bind_rows(
  season_currency_mz(X_MAN,  G_MAN,  "man")  %>% mutate(split = "man"),
  season_currency_mz(X_ZONE, G_ZONE, "zone") %>% mutate(split = "zone")) %>%
  group_by(split, band, season) %>%
  mutate(
    grade_pctl     = percent_rank_avg(grade),
    supp_pctl      = percent_rank_avg(-cmp_rt),     # INVERTED
    pbu_pctl       = percent_rank_avg(pbu_rt),
    ypt_supp_pctl  = percent_rank_avg(-ypt_alw),    # INVERTED (context)
    yac_supp_pctl  = percent_rank_avg(-yac_alw),    # INVERTED (context)
    ypcs_supp_pctl = percent_rank_avg(-ypcs_alw),   # INVERTED (context)
    int_pctl       = percent_rank_avg(int_rt),
    snt_pctl       = percent_rank_avg(sn_per_tgt),  # respect stat (context)
    qbr_supp_pctl  = percent_rank_avg(-qbr_alw),    # INVERTED (context)
    mt_supp_pctl   = percent_rank_avg(-mt_rt)       # INVERTED (context)
  ) %>%
  ungroup()

cat("\n--- currency validation: gated player-seasons, split x band x year ---\n")
print(cov_season_pctl_mz %>% count(season, split, band) %>%
        arrange(season, split, band), n = Inf)
cat("\n--- 2025 pools per split x band ---\n")
print(cov_season_pctl_mz %>% filter(season == 2025) %>%
        count(split, band), n = Inf)

cat("\n--- NE's OWN 2025 room per split, currency + pctls (answer key) ---\n")
cat("    (expect: Davis zone >> man; Jones both; Mapu/Tavai zone-only)\n")
print(secondary_qbgrp_mz %>%
        filter(def_ssn == "NE2025") %>%
        count(player_id, name = "g") %>%
        inner_join(cov_season_pctl_mz %>% filter(season == 2025),
                   by = "player_id") %>%
        arrange(split, band, desc(q_snaps)) %>%
        select(split, band, player, q_games, q_snaps, q_tgts,
               grade, grade_pctl, supp_pctl, pbu_pctl), n = Inf)

# ------------------------------------------------------------
# 4b. ORIENTATION RITUAL PER SPLIT -- cor matrices + known-elite
#     eyeball. Pruning rule (prush law): |r| >= ~.85 vs grade ->
#     rides to the table, not the facets. OE: not ported (header).
# ------------------------------------------------------------

for (sp in c("man", "zone")) for (b in SEC_BANDS_MZ) {
  cor_mat_mz <- cov_season_pctl_mz %>%
    filter(season == 2025, split == sp, band == b) %>%
    select(grade, cmp_rt, pbu_rt, ypt_alw, sn_per_tgt, qbr_alw) %>%
    cor(use = "pairwise.complete.obs")
  cat("\n--- metric cor matrix, 2025 gated,", sp, "/", b,
      "(raw values; cmp/ypt/qbr: LOW = good) ---\n")
  print(round(cor_mat_mz, 3))
}

cat("\n--- known-elite eyeball: top 10 by grade_pctl, 2025, split x band ---\n")
print(cov_season_pctl_mz %>%
        filter(season == 2025) %>%
        group_by(split, band) %>%
        slice_max(grade_pctl, n = 10, with_ties = FALSE) %>%
        arrange(split, band, desc(grade_pctl)) %>%
        select(split, band, player, q_games, q_snaps, q_tgts,
               grade_pctl, supp_pctl, pbu_pctl),
      n = Inf)

cat("\n--- top 10 ZONE CBs by supp_pctl (the FLOORED facet), 2025 ---\n")
print(cov_season_pctl_mz %>%
        filter(season == 2025, split == "zone", band == "CB") %>%
        slice_max(supp_pctl, n = 10, with_ties = FALSE) %>%
        arrange(desc(supp_pctl)) %>%
        select(player, q_games, q_tgts, cmp_rt, supp_pctl, grade_pctl, pbu_pctl),
      n = Inf)

cat("\n--- top 10 MAN CBs by supp_pctl (NOISE-FLAGGED -- eyeball only) ---\n")
print(cov_season_pctl_mz %>%
        filter(season == 2025, split == "man", band == "CB") %>%
        slice_max(supp_pctl, n = 10, with_ties = FALSE) %>%
        arrange(desc(supp_pctl)) %>%
        select(player, q_games, q_tgts, cmp_rt, supp_pctl, grade_pctl, pbu_pctl),
      n = Inf)

# ------------------------------------------------------------
# 5. PRIOR AUDIT PER SPLIT -- the v3 birth (stamp 7). Method
#    ports verbatim from the combined audit: the promotion
#    class = priced committee members (top-N by split usage
#    within team x band x season, N = the stamped 3/1/3/2)
#    whose price got REALIZED (gated in t) with NO gated split
#    season in t-1 or t-2. Their realized season-t grade pctl
#    is the no-data price. Arms: rookie (entry_year == t) vs
#    vet. Phantom = pooled class median. SCB is BORN here (v2
#    has no SCB row). Fallback chain per band x split x arm,
#    printed per cell:
#      rung 1: arm median, n >= 10
#      rung 2: pooled band x split median, n_pool >= 10
#      rung 3: v2 combined price (CB/S/LB); SCB rides the
#              same-split CB cell (the un-fold's emergency
#              rung -- SCB was CB once, stamped here)
#    pr_supp / pr_pbu: pooled realized medians (context; thin
#    pools fall to neutral .5, flagged). Override = edit +
#    re-source WHOLE file.
# ------------------------------------------------------------

def_xwalk <- combined_ids_defense %>%
  filter(!is.na(gsis_id)) %>%
  distinct(player_id, gsis_id)
stopifnot(anyDuplicated(def_xwalk$gsis_id)   == 0,
          anyDuplicated(def_xwalk$player_id) == 0)

entry_years_mz <- nflreadr::load_rosters(2017:2025) %>%
  filter(!is.na(gsis_id), !is.na(entry_year)) %>%
  group_by(gsis_id) %>%
  summarise(entry_year = min(entry_year), .groups = "drop") %>%
  inner_join(def_xwalk, by = "gsis_id") %>%
  select(player_id, entry_year)

committee_hist_one_mz <- function(sp) {
  C <- if (sp == "man") CM else CZ
  x <- if (sp == "man") X_MAN else X_ZONE
  secondary_qbgrp_mz %>%
    filter(.data[[C["snaps"]]] >= x) %>%
    inner_join(modal_band_mz, by = c("player_id", "season")) %>%
    group_by(team, season, band, player_id) %>%
    summarise(qg = dplyr::n(),
              sn = sum(.data[[C["snaps"]]], na.rm = TRUE),
              .groups = "drop") %>%
    group_by(team, season, band) %>%
    arrange(desc(sn), .by_group = TRUE) %>%
    mutate(band_rank = dplyr::row_number()) %>%
    filter(band_rank <= N_BAND_MZ[band]) %>%
    ungroup() %>%
    mutate(split = sp)
}
committee_hist_mz <- bind_rows(committee_hist_one_mz("man"),
                               committee_hist_one_mz("zone"))

gated_ps_mz <- cov_season_pctl_mz %>% distinct(split, player_id, season)

promo_mz <- committee_hist_mz %>%
  filter(season >= 2018) %>%
  inner_join(cov_season_pctl_mz %>%
               select(split, player_id, season, band,
                      grade_pctl, supp_pctl, pbu_pctl),
             by = c("split", "player_id", "season", "band")) %>%
  left_join(gated_ps_mz %>%
              transmute(split, player_id, season = season + 1L, g1 = TRUE),
            by = c("split", "player_id", "season")) %>%
  left_join(gated_ps_mz %>%
              transmute(split, player_id, season = season + 2L, g2 = TRUE),
            by = c("split", "player_id", "season")) %>%
  filter(is.na(g1), is.na(g2)) %>%
  select(split, band, player_id, season, grade_pctl, supp_pctl, pbu_pctl) %>%
  left_join(entry_years_mz, by = "player_id") %>%
  mutate(arm = if_else(!is.na(entry_year) & entry_year == season,
                       "rookie", "vet"))

cat("\n================ PRIOR AUDIT PER SPLIT ================\n")
cat("--- promotion class sizes, 2018-2025 (priced committee members,\n")
cat("    realized in t, no gated t-1/t-2), split x band x arm ---\n")
print(promo_mz %>% count(split, band, arm) %>%
        arrange(split, band, arm), n = Inf)

promo_med_mz <- promo_mz %>%
  group_by(split, band, arm) %>%
  summarise(n = dplyr::n(),
            med_grade = median(grade_pctl, na.rm = TRUE),
            .groups = "drop")

promo_pool_mz <- promo_mz %>%
  group_by(split, band) %>%
  summarise(n_pool    = dplyr::n(),
            pool_grade = median(grade_pctl, na.rm = TRUE),
            pool_supp  = median(supp_pctl,  na.rm = TRUE),
            pool_pbu   = median(pbu_pctl,   na.rm = TRUE),
            .groups = "drop")

cat("\n--- pooled realized medians per split x band (the phantom center) ---\n")
print(promo_pool_mz, n = Inf)

V2_LAW_MZ <- tribble(   # combined-currency prices, RUNG 3 only
  ~band, ~v2_rookie, ~v2_vet, ~v2_phantom,
  "CB", 0.433, 0.359, 0.408,
  "LB", 0.456, 0.338, 0.400,
  "S",  0.471, 0.465, 0.468)

N_ARM_MIN <- 10L

prior_rows_mz <- list()
cb_price <- list()
for (sp in c("man", "zone")) {
  for (b in SEC_BANDS_MZ) {   # CB first by construction (SEC_BANDS_MZ order)
    pool <- promo_pool_mz %>% filter(split == sp, band == b)
    rook <- promo_med_mz %>% filter(split == sp, band == b, arm == "rookie")
    vet  <- promo_med_mz %>% filter(split == sp, band == b, arm == "vet")
    n_pool <- if (nrow(pool)) pool$n_pool else 0L
    n_rook <- if (nrow(rook)) rook$n else 0L
    n_vet  <- if (nrow(vet))  vet$n  else 0L
    pool_g <- if (nrow(pool)) pool$pool_grade else NA_real_
    v2r <- if (b == "SCB") NA_real_ else V2_LAW_MZ$v2_rookie[V2_LAW_MZ$band == b]
    v2v <- if (b == "SCB") NA_real_ else V2_LAW_MZ$v2_vet[V2_LAW_MZ$band == b]
    v2p <- if (b == "SCB") NA_real_ else V2_LAW_MZ$v2_phantom[V2_LAW_MZ$band == b]
    # rung-3 SCB rides the same-split CB cell (already computed)
    r3r <- if (b == "SCB") cb_price[[sp]]$rook else v2r
    r3v <- if (b == "SCB") cb_price[[sp]]$vet  else v2v
    r3p <- if (b == "SCB") cb_price[[sp]]$phan else v2p
    r3l <- if (b == "SCB") "split CB (rung 3)" else "v2 combined (rung 3)"
    if (n_rook >= N_ARM_MIN) { g_r <- rook$med_grade; rung_r <- "arm (n>=10)" }
    else if (n_pool >= N_ARM_MIN) { g_r <- pool_g;  rung_r <- "pooled band x split" }
    else { g_r <- r3r; rung_r <- r3l }
    if (n_vet >= N_ARM_MIN)  { g_v <- vet$med_grade;  rung_v <- "arm (n>=10)" }
    else if (n_pool >= N_ARM_MIN) { g_v <- pool_g;  rung_v <- "pooled band x split" }
    else { g_v <- r3v; rung_v <- r3l }
    if (n_pool >= N_ARM_MIN) {
      g_p <- pool_g; rung_p <- "pooled band x split"
      s_p <- pool$pool_supp; b_p <- pool$pool_pbu
      supp_src <- "pooled"; pbu_src <- "pooled"
    } else {
      g_p <- r3p; rung_p <- r3l
      s_p <- 0.5; b_p <- 0.5
      supp_src <- "neutral .5 (thin pool)"; pbu_src <- "neutral .5 (thin pool)"
    }
    if (b == "CB") cb_price[[sp]] <- list(rook = g_r, vet = g_v, phan = g_p)
    prior_rows_mz[[paste(sp, b)]] <- tibble(
      split = sp, band = b,
      rookie_grade = g_r, vet_grade = g_v, phantom_grade = g_p,
      pr_supp = s_p, pr_pbu = b_p,
      n_rook = n_rook, n_vet = n_vet, n_pool = n_pool,
      rung_rookie = rung_r, rung_vet = rung_v, rung_phantom = rung_p,
      supp_src = supp_src, pbu_src = pbu_src)
  }
}
cov_prior_law_mz <- bind_rows(prior_rows_mz)

cat("\n--- PRIOR LAW v3 (computed in-file, per split; rungs printed) ---\n")
print(cov_prior_law_mz %>%
        select(split, band, rookie_grade, vet_grade, phantom_grade,
               n_rook, n_vet, n_pool, rung_rookie, rung_vet, rung_phantom),
      n = Inf)
cat("\n--- context priors (pr_supp / pr_pbu) + their sources ---\n")
print(cov_prior_law_mz %>%
        select(split, band, pr_supp, supp_src, pr_pbu, pbu_src), n = Inf)

cat("\n--- v3 vs v2, phantom price, CB/S/LB (the re-centering receipt) ---\n")
print(cov_prior_law_mz %>%
        filter(band != "SCB") %>%
        select(split, band, phantom_grade) %>%
        left_join(V2_LAW_MZ %>% select(band, v2_phantom), by = "band") %>%
        mutate(d_v3_v2 = phantom_grade - v2_phantom) %>%
        arrange(band, split), n = Inf)

# ------------------------------------------------------------
# 6. defense crosswalk + 2026 coverage rosters (split-neutral)
#    + PER-SPLIT usage, pctls, status flags, and committee
#    proposals. Band law (prush canon, Andy 08f): band = modal
#    PFF position from coverage history (block 1, UN-GATED
#    frame). nflverse roster strings NEVER assign bands --
#    rookie bands are tribble calls. DE/DT/NT/EDGE not pulled;
#    the LB family is pulled and history sorts off-ball from
#    edge (edge modals read DL -> never proposed).
#    usage_ord = pmax('25,'24 split snaps), NEVER coalesce
#    (the Ed Oliver receipt). A vet with ZERO split-gated
#    games in '25 AND '24 is excluded from THAT split's ledger
#    (no_split_history -- a pure zone player never crowds the
#    man slate; phantom fills the body). Rookies stay in both
#    ledgers, never auto-proposed.
# ------------------------------------------------------------

cov_roster_pos <- c("CB", "S", "FS", "SS", "SAF", "DB", "NB", "SCB",
                    "LB", "ILB", "MLB", "OLB")

def_2026_cov_mz <- nflreadr::load_rosters(2026) %>%
  filter(position %in% cov_roster_pos) %>%
  transmute(gsis_id,
            roster_name = full_name,
            team_name = dplyr::coalesce(pff_team_lookup[team], team),
            entry_year,
            roster_pos = position) %>%
  filter(team_name %in% opp_2026_teams) %>%
  left_join(def_xwalk, by = "gsis_id")

stopifnot(anyDuplicated(def_2026_cov_mz[, c("team_name", "roster_name")]) == 0)

cat("\n--- roster positions pulled (verify nothing weird) ---\n")
print(def_2026_cov_mz %>% count(roster_pos))

cat("\n--- coverage defenders pulled + matched to PFF ids (per team) ---\n")
print(def_2026_cov_mz %>% group_by(team_name) %>%
        summarise(n_cov = dplyr::n(), matched = sum(!is.na(player_id)),
                  .groups = "drop") %>%
        arrange(match(team_name, opp_2026_teams)), n = Inf)

cov_usage_one_mz <- function(sp) {
  C <- if (sp == "man") CM else CZ
  x <- if (sp == "man") X_MAN else X_ZONE
  secondary_qbgrp_mz %>%
    filter(.data[[C["snaps"]]] >= x, season %in% c(2024, 2025)) %>%
    group_by(player_id, season) %>%
    summarise(cov_snaps = sum(.data[[C["snaps"]]], na.rm = TRUE),
              qg = dplyr::n(), .groups = "drop") %>%
    pivot_wider(names_from = season, values_from = c(cov_snaps, qg)) %>%
    mutate(split = sp)
}
cov_usage_mz <- bind_rows(cov_usage_one_mz("man"), cov_usage_one_mz("zone"))

band_map_mz <- modal_band_mz %>% filter(season == 2025) %>%
  select(player_id, band_25 = band) %>%
  full_join(modal_band_mz %>% filter(season == 2024) %>%
              select(player_id, band_24 = band),
            by = "player_id") %>%
  mutate(band = dplyr::coalesce(band_25, band_24)) %>%
  select(player_id, band)

ledger_base_one_mz <- function(sp) {
  u   <- cov_usage_mz %>% filter(split == .env$sp)
  p25 <- cov_season_pctl_mz %>% filter(season == 2025, split == .env$sp) %>%
    select(player_id, grade_25 = grade_pctl, supp_25 = supp_pctl,
           pbu_25 = pbu_pctl)
  p24 <- cov_season_pctl_mz %>% filter(season == 2024, split == .env$sp) %>%
    select(player_id, grade_24 = grade_pctl, supp_24 = supp_pctl,
           pbu_24 = pbu_pctl)
  def_2026_cov_mz %>%
    left_join(u,   by = "player_id") %>%
    left_join(band_map_mz, by = "player_id") %>%
    left_join(p25, by = "player_id") %>%
    left_join(p24, by = "player_id") %>%
    mutate(band = if_else(band %in% SEC_BANDS_MZ, band, NA_character_),
           status = case_when(
             entry_year == 2026                            ~ "rookie",
             is.na(player_id)                              ~ "no_pff_id",
             !is.na(grade_25)                              ~ "has_2025_pctl",
             !is.na(grade_24)                              ~ "data_2024_only",
             dplyr::coalesce(cov_snaps_2025, 0) > 0 |
               dplyr::coalesce(cov_snaps_2024, 0) > 0      ~ "usage_no_pctl",
             TRUE                                          ~ "no_split_history"
           )) %>%
    mutate(split = sp)
}

ledger_base_mz <- bind_rows(ledger_base_one_mz("man"),
                            ledger_base_one_mz("zone"))

cat("\n--- zero-split-usage vets excluded per team x split (FYI) ---\n")
print(ledger_base_mz %>% filter(status == "no_split_history") %>%
        count(team_name, split), n = Inf)

ledger_mz <- ledger_base_mz %>%
  filter(status != "no_split_history") %>%
  mutate(usage_ord = pmax(dplyr::coalesce(cov_snaps_2025, 0),
                          dplyr::coalesce(cov_snaps_2024, 0))) %>%
  group_by(split, team_name, band) %>%
  arrange(desc(usage_ord), .by_group = TRUE) %>%
  mutate(band_rank = dplyr::row_number(),
         proposed  = dplyr::coalesce(
           !is.na(band) & band_rank <= N_BAND_MZ[band], FALSE)) %>%
  ungroup()

cat("\n--- 2025 SPLIT-gated shapes, per team x band, both splits (FYI) ---\n")
for (sp in c("man", "zone")) {
  C <- if (sp == "man") CM else CZ
  x <- if (sp == "man") X_MAN else X_ZONE
  g <- if (sp == "man") G_MAN else G_ZONE
  cat("\n  [", sp, " slate -- gated at ", x, "/", g, " ]\n", sep = "")
  print(secondary_qbgrp_mz %>%
          filter(.data[[C["snaps"]]] >= x, season == 2025) %>%
          inner_join(modal_band_mz %>% filter(season == 2025),
                     by = c("player_id", "season")) %>%
          group_by(team, band, player_id) %>%
          summarise(qg = dplyr::n(), .groups = "drop") %>%
          filter(qg >= g) %>%
          count(team, band, name = "n_2025") %>%
          filter(team %in% opp_2026_teams) %>%
          pivot_wider(names_from = band, values_from = n_2025) %>%
          arrange(match(team, opp_2026_teams)), n = Inf)
}

# ------------------------------------------------------------
# 7. THE RULING LEDGERS -- one per slate. proposed = TRUE ->
#    auto-committee (top N of the band by '25/'24 SPLIT usage;
#    N = CB 3 / SCB 1 / S 3 / LB 2 PER CURRENCY, ruled
#    2026-08-16). Andy rules ALL deltas via tribble: adds
#    (rookies/signings, WITH band), drops (injuries/cuts).
#    band = PFF modal history only.
# ------------------------------------------------------------

for (sp in c("man", "zone")) {
  cat("\n================ STEP 0 -- THE RULING LEDGER:", toupper(sp),
      "SLATE ================\n")
  print(ledger_mz %>%
          filter(split == sp) %>%
          arrange(match(team_name, opp_2026_teams), band, desc(usage_ord)) %>%
          select(team_name, band, roster_name, status, proposed,
                 sn25 = cov_snaps_2025, sn24 = cov_snaps_2024,
                 grade_25, supp_25, pbu_25, grade_24, supp_24, pbu_24),
        n = Inf)
}

cat("\n--- calls needed per team x split ---\n")
print(ledger_mz %>% group_by(team_name, split) %>%
        summarise(proposed      = sum(proposed),
                  rookies       = sum(status == "rookie"),
                  data_24_only  = sum(status == "data_2024_only"),
                  usage_no_pctl = sum(status == "usage_no_pctl"),
                  no_pff_id     = sum(status == "no_pff_id"),
                  no_band       = sum(is.na(band)),
                  .groups = "drop") %>%
        arrange(match(team_name, opp_2026_teams), split), n = Inf)

# ------------------------------------------------------------
# STOP -- ruling boundary. Deltas happen in section 8.
# ------------------------------------------------------------

# ------------------------------------------------------------
# 8. ANDY'S DELTAS -- the only judgment layer. EMPTY = full
#    sign-off (legal + complete). Actions: add / drop. An add
#    seats into BOTH split committees (one player, two
#    currencies), subject to each split's own N cap by usage --
#    the cap can cut an add from a full committee (the 4th
#    corner lives in the pool). Adds MUST carry a band unless
#    the player's history supplies one (coalesce law). Names
#    must match def_2026_cov_mz spelling exactly -- the gate
#    fails loudly. Phantoms are AUTOMATIC per split (block 10
#    fills every (team, band, split) to its N at the v3 prior).
# ------------------------------------------------------------

opp_cov_2026_deltas_mz <- tribble(
  ~team_name, ~roster_name, ~band, ~action, ~note
  # "MIA", "<name>", "CB", "add", "example - rookie into the committee",
)

d_miss <- opp_cov_2026_deltas_mz %>%
  anti_join(def_2026_cov_mz, by = c("team_name", "roster_name"))
if (nrow(d_miss) > 0) {
  print(d_miss)
  stop("delta names not on 2026 rosters - fix spelling vs the ledger")
}

cat("\n--- deltas registered:", nrow(opp_cov_2026_deltas_mz), "row(s) ---\n")
if (nrow(opp_cov_2026_deltas_mz) > 0) print(opp_cov_2026_deltas_mz, n = Inf)

# ------------------------------------------------------------
# 9. FACED 2025 SIDE PER SPLIT -- observed only, no imputation
#    of the past. Every SPLIT-gated coverage game vs NE's
#    PASSING game (qbgrp_ssn), weighted by that game's SPLIT
#    snaps. unscored_share = the honesty number, per split.
#    PLUS the scheme-share by-product (stamp: informational
#    only, NEVER a recombination): each team's 2025 man/zone
#    split of coverage snaps, season-long AND as actually
#    faced by NE.
# ------------------------------------------------------------

faced_games_one_mz <- function(sp) {
  C <- if (sp == "man") CM else CZ
  x <- if (sp == "man") X_MAN else X_ZONE
  secondary_qbgrp_mz %>%
    filter(startsWith(qbgrp_ssn, "NE"), season == 2025, in_season(week),
           .data[[C["snaps"]]] >= x) %>%
    transmute(player_id, player, team, week, band_row = band4,
              g_snaps = .data[[C["snaps"]]]) %>%
    left_join(cov_season_pctl_mz %>% filter(season == 2025, split == .env$sp) %>%
                select(player_id, band, g25 = grade_pctl,
                       s25 = supp_pctl, p25 = pbu_pctl),
              by = "player_id") %>%
    mutate(band = dplyr::coalesce(band, band_row), split = sp)
}
faced_games_mz <- bind_rows(faced_games_one_mz("man"),
                            faced_games_one_mz("zone")) %>%
  # FACED FILL LAW (Andy 08-16, supersedes observed-only): every faced
  # participant carries a pctl. Unscored defender-games price at the
  # v3 VET rung for their (split, band) -- the Echols rule. Refinement
  # available: 2025-rookie faced games at rookie_grade via entry years.
  left_join(cov_prior_law_mz %>%
              select(split, band, vet_grade, pr_supp, pr_pbu),
            by = c("split", "band")) %>%
  mutate(g25_f = dplyr::coalesce(g25, vet_grade),
         s25_f = dplyr::coalesce(s25, pr_supp),
         p25_f = dplyr::coalesce(p25, pr_pbu),
         price_basis = dplyr::if_else(is.na(g25), "vet prior (v3)",
                                      "earned '25"))

cat("\n--- faced 2025: weeks present per split (expect the 21 incl. playoffs) ---\n")
print(faced_games_mz %>% distinct(split, week) %>% arrange(split, week), n = Inf)

faced_band_mz <- faced_games_mz %>%
  group_by(split, band) %>%
  summarise(defender_games = dplyr::n(),
            tot_snaps      = sum(g_snaps),
            unscored_share = sum(g_snaps[is.na(g25)]) / tot_snaps,
            grade_25 = weighted.mean(g25_f, w = g_snaps, na.rm = TRUE),
            supp_25  = weighted.mean(s25_f, w = g_snaps, na.rm = TRUE),
            pbu_25   = weighted.mean(p25_f, w = g_snaps, na.rm = TRUE),
            .groups = "drop")

cat("\n--- faced 2025 band profile PER SPLIT (unscored_share = exposure priced at vet prior, not dropped) ---\n")
print(faced_band_mz, n = Inf)

faced_team_band_mz <- faced_games_mz %>%
  group_by(split, team_name = team, band) %>%
  summarise(tot_snaps = sum(g_snaps),
            grade_25  = if (all(is.na(g25_f))) NA_real_
            else weighted.mean(g25_f, w = g_snaps, na.rm = TRUE),
            supp_25   = if (all(is.na(s25_f))) NA_real_
            else weighted.mean(s25_f, w = g_snaps, na.rm = TRUE),
            pbu_25    = if (all(is.na(p25_f))) NA_real_
            else weighted.mean(p25_f, w = g_snaps, na.rm = TRUE),
            .groups = "drop")

cat("\n--- faced 2025 per team x band PER SPLIT (ghost layer; NA = not faced) ---\n")
print(faced_team_band_mz %>% arrange(split, band, desc(grade_25)), n = Inf)

# --- the SCHEME-SHARE by-product (informational, never recombined) ---
scheme_share_mz <- secondary_qbgrp_mz %>%
  filter(season == 2025) %>%
  group_by(team_name = team) %>%
  summarise(man_sn  = sum(.data[[CM["snaps"]]],  na.rm = TRUE),
            zone_sn = sum(.data[[CZ["snaps"]]],  na.rm = TRUE),
            .groups = "drop") %>%
  mutate(man_share  = man_sn / (man_sn + zone_sn),
         zone_share = 1 - man_share,
         opp_2026   = team_name %in% opp_2026_teams)

scheme_share_faced_mz <- secondary_qbgrp_mz %>%
  filter(startsWith(qbgrp_ssn, "NE"), season == 2025, in_season(week)) %>%
  group_by(team_name = team) %>%
  summarise(man_sn_faced  = sum(.data[[CM["snaps"]]],  na.rm = TRUE),
            zone_sn_faced = sum(.data[[CZ["snaps"]]],  na.rm = TRUE),
            .groups = "drop") %>%
  mutate(man_share_faced = man_sn_faced / (man_sn_faced + zone_sn_faced))

cat("\n--- SCHEME SHARE 2025, season-long vs AS FACED BY NE (by-product;\n")
cat("    which currency matters vs each opponent -- NOT a recombination) ---\n")
print(scheme_share_mz %>%
        filter(opp_2026 | team_name == "NE") %>%
        left_join(scheme_share_faced_mz %>%
                    select(team_name, man_share_faced), by = "team_name") %>%
        mutate(is_NE = team_name == "NE") %>%
        arrange(desc(opp_2026), is_NE, desc(man_share)) %>%
        select(team_name, opp_2026, is_NE, man_share, zone_share,
               man_share_faced), n = Inf)
cat("\nleague mean man share 2025:",
    round(mean(scheme_share_mz$man_share), 3), "\n")

# ------------------------------------------------------------
# 10. 2026 PROJECTION PER SPLIT -- signed committees + deltas +
#     v3 priors + phantoms. Values: blend2('25,'24 pctl, w25 =
#     q_games25/10) per split. No-pctl members (rookie /
#     under-gate vet / phantom) = band x split prior, flagged.
#     Weights: pmax('25,'24 SPLIT snaps) -> slot-weight
#     inheritance within (team, band, split). Every (team,
#     band, split) is exactly its N deep, no exceptions.
# ------------------------------------------------------------

slot_usage_one_mz <- function(sp) {
  C <- if (sp == "man") CM else CZ
  x <- if (sp == "man") X_MAN else X_ZONE
  gmin <- if (sp == "man") G_MAN else G_ZONE
  secondary_qbgrp_mz %>%
    filter(.data[[C["snaps"]]] >= x, season == 2025) %>%
    group_by(team, player_id) %>%
    summarise(g  = dplyr::n(),
              sn = sum(.data[[C["snaps"]]], na.rm = TRUE), .groups = "drop") %>%
    filter(g >= gmin) %>%
    inner_join(modal_band_mz %>% filter(season == 2025) %>%
                 select(player_id, band), by = "player_id") %>%
    group_by(team, band) %>%
    arrange(desc(sn), .by_group = TRUE) %>%
    mutate(slot_rk = dplyr::row_number()) %>%
    ungroup() %>%
    mutate(split = sp) %>%
    select(split, team_name = team, band, slot_rk, sn_slot = sn)
}
slot_usage_2025_mz <- bind_rows(slot_usage_one_mz("man"),
                                slot_usage_one_mz("zone"))

slot_usage_med_mz <- slot_usage_2025_mz %>%
  group_by(split, band, slot_rk) %>%
  summarise(sn_med = median(sn_slot), .groups = "drop")

fallback_sn_mz <- slot_usage_2025_mz %>%
  group_by(split) %>%
  summarise(fb = min(sn_slot), .groups = "drop")

proj_one_mz <- function(sp) {
  p25 <- cov_season_pctl_mz %>% filter(season == 2025, split == .env$sp) %>%
    select(player_id, g25 = grade_pctl, s25 = supp_pctl, p25 = pbu_pctl)
  p24 <- cov_season_pctl_mz %>% filter(season == 2024, split == .env$sp) %>%
    select(player_id, g24 = grade_pctl, s24 = supp_pctl, p24 = pbu_pctl)
  law <- cov_prior_law_mz %>% filter(split == .env$sp) %>%
    select(band, rookie_grade, vet_grade, phantom_grade, pr_supp, pr_pbu)
  slots <- slot_usage_2025_mz %>% filter(split == .env$sp) %>% select(-split)
  meds  <- slot_usage_med_mz  %>% filter(split == .env$sp) %>% select(-split)
  fb    <- fallback_sn_mz %>% filter(split == .env$sp) %>% pull(fb)
  
  adds_prepped <- opp_cov_2026_deltas_mz %>%
    filter(action == "add") %>%
    left_join(ledger_base_mz %>% filter(split == .env$sp) %>%
                select(team_name, roster_name, player_id, band_hist = band,
                       cov_snaps_2025, cov_snaps_2024, qg_2025, status),
              by = c("team_name", "roster_name")) %>%
    mutate(band = dplyr::coalesce(band, band_hist)) %>%
    select(team_name, roster_name, player_id, band,
           cov_snaps_2025, cov_snaps_2024, qg_2025, status)
  
  rot_real <- ledger_mz %>%
    filter(split == .env$sp, proposed) %>%
    select(team_name, roster_name, player_id, band,
           cov_snaps_2025, cov_snaps_2024, qg_2025, status) %>%
    anti_join(opp_cov_2026_deltas_mz %>% filter(action == "drop"),
              by = c("team_name", "roster_name")) %>%
    bind_rows(adds_prepped)
  
  # (team, name) dup law catches the proposed-CB + added-S collision class
  stopifnot(!any(is.na(rot_real$band)),
            all(rot_real$band %in% SEC_BANDS_MZ),
            anyDuplicated(rot_real[, c("team_name", "roster_name")]) == 0)
  
  rot_real <- rot_real %>%
    left_join(p25, by = "player_id") %>%
    left_join(p24, by = "player_id") %>%
    left_join(law, by = "band") %>%
    mutate(w25      = pmin(dplyr::coalesce(qg_2025, 0L) / 10, 1),
           grade_bl = blend2(g25, g24, w25),
           supp_bl  = blend2(s25, s24, w25),
           pbu_bl   = blend2(p25, p24, w25),
           prior_used = is.na(grade_bl),
           grade_f = dplyr::coalesce(grade_bl,
                                     if_else(status == "rookie", rookie_grade, vet_grade)),
           supp_f  = dplyr::coalesce(supp_bl,  pr_supp),
           pbu_f   = dplyr::coalesce(pbu_bl,   pr_pbu),
           usage_w = pmax(dplyr::coalesce(cov_snaps_2025, 0),
                          dplyr::coalesce(cov_snaps_2024, 0))) %>%
    select(team_name, roster_name, band, status, prior_used,
           usage_w, grade_f, supp_f, pbu_f)
  
  # cap at the band's N (THIS currency), then slot-weight inheritance
  rot_real <- rot_real %>%
    mutate(usage_w = if_else(usage_w > 0, usage_w, NA_real_)) %>%
    group_by(team_name, band) %>%
    arrange(desc(dplyr::coalesce(usage_w, 0)), .by_group = TRUE) %>%
    mutate(band_rank = dplyr::row_number()) %>%
    filter(band_rank <= N_BAND_MZ[band]) %>%
    ungroup() %>%
    left_join(slots, by = c("team_name", "band", "band_rank" = "slot_rk")) %>%
    left_join(meds,  by = c("band", "band_rank" = "slot_rk")) %>%
    mutate(usage_w = dplyr::coalesce(usage_w, sn_slot, sn_med, fb)) %>%
    select(team_name, roster_name, band, status, prior_used,
           usage_w, grade_f, supp_f, pbu_f)
  
  auto_phantoms <- rot_real %>%
    count(team_name, band, name = "n_real") %>%
    tidyr::complete(team_name = opp_2026_teams, band = SEC_BANDS_MZ,
                    fill = list(n_real = 0L)) %>%
    mutate(n_target = N_BAND_MZ[band]) %>%
    filter(n_real < n_target) %>%
    mutate(slot_rk = purrr::map2(n_real, n_target, ~ seq(.x + 1L, .y))) %>%
    tidyr::unnest(slot_rk) %>%
    left_join(slots, by = c("team_name", "band", "slot_rk")) %>%
    left_join(meds,  by = c("band", "slot_rk")) %>%
    left_join(law,   by = "band") %>%
    transmute(team_name,
              roster_name = paste0("PHANTOM_", band, slot_rk),
              band, status = "phantom", prior_used = TRUE,
              usage_w = dplyr::coalesce(sn_slot, sn_med, fb),
              grade_f = phantom_grade, supp_f = pr_supp, pbu_f = pr_pbu)
  
  bind_rows(rot_real, auto_phantoms) %>% mutate(split = sp)
}

rot_2026_mz <- bind_rows(proj_one_mz("man"), proj_one_mz("zone"))

# every (team, band, split) is exactly its N deep, no exceptions
depth_check_mz <- dplyr::count(rot_2026_mz, split, team_name, band)
stopifnot(all(depth_check_mz$n == N_BAND_MZ[depth_check_mz$band]),
          nrow(rot_2026_mz) == length(opp_2026_teams) * sum(N_BAND_MZ) * 2)

cat("\n--- 2026 coverage committees PER SPLIT, final (prior_used = prior) ---\n")
print(rot_2026_mz %>%
        arrange(split, match(team_name, opp_2026_teams), band, desc(usage_w)) %>%
        select(split, team_name, band, roster_name, status, prior_used,
               usage_w, grade_f, supp_f, pbu_f), n = Inf)

team_band_2026_mz <- rot_2026_mz %>%
  group_by(split, team_name, band) %>%
  summarise(n_members    = dplyr::n(),
            interp_share = sum(usage_w[prior_used]) / sum(usage_w),
            grade_26 = weighted.mean(grade_f, w = usage_w),
            supp_26  = weighted.mean(supp_f,  w = usage_w),
            pbu_26   = weighted.mean(pbu_f,   w = usage_w),
            .groups = "drop")

cat("\n--- team x band 2026 values PER SPLIT + interp_share honesty ---\n")
print(team_band_2026_mz %>%
        arrange(split, match(team_name, opp_2026_teams), band), n = Inf)

# band weights per split: league 2025 gated SPLIT-snap shares
# (earned, printed -- not magical). Same weights both sides.
band_wt_mz <- cov_season_pctl_mz %>%
  filter(season == 2025) %>%
  group_by(split, band) %>%
  summarise(sn = sum(q_snaps), .groups = "drop") %>%
  group_by(split) %>%
  mutate(wt = sn / sum(sn)) %>%
  ungroup() %>%
  select(split, band, wt)

cat("\n--- unit-composite band weights PER SPLIT (2025 gated split-snap\n")
cat("    shares -- note SCB's man weight is earned-thin by design) ---\n")
print(band_wt_mz, n = Inf)

team_unit_2026_mz <- team_band_2026_mz %>%
  left_join(band_wt_mz, by = c("split", "band")) %>%
  group_by(split, team_name) %>%
  summarise(unit_26 = weighted.mean(grade_26, w = wt),
            interp_share = weighted.mean(interp_share, w = wt),
            .groups = "drop")

# schedule-weighted slate PER SPLIT (division x2, same as prush)
slate_band_2026_mz <- tibble(team_name = sched_2026) %>%
  left_join(team_band_2026_mz, by = "team_name",
            relationship = "many-to-many") %>%
  group_by(split, band) %>%
  summarise(grade_26 = mean(grade_26), supp_26 = mean(supp_26),
            pbu_26 = mean(pbu_26), interp_share = mean(interp_share),
            .groups = "drop")

slate_unit_2026_mz <- tibble(team_name = sched_2026) %>%
  left_join(team_unit_2026_mz, by = "team_name",
            relationship = "many-to-many") %>%
  group_by(split) %>%
  summarise(unit_26 = mean(unit_26),
            interp_share = mean(interp_share), .groups = "drop")

cmp_slate_mz <- faced_band_mz %>%
  select(split, band, grade_25, supp_25, pbu_25,
         unscored_share_25 = unscored_share) %>%
  left_join(slate_band_2026_mz, by = c("split", "band")) %>%
  mutate(d_grade = grade_26 - grade_25,
         d_supp  = supp_26  - supp_25,
         d_pbu   = pbu_26   - pbu_25)

faced_unit_mz <- faced_band_mz %>%
  left_join(band_wt_mz, by = c("split", "band")) %>%
  group_by(split) %>%
  summarise(unit_25 = if (all(is.na(grade_25))) NA_real_
            else weighted.mean(grade_25, w = wt, na.rm = TRUE),
            .groups = "drop")

cmp_unit_mz <- slate_unit_2026_mz %>%
  left_join(faced_unit_mz, by = "split") %>%
  mutate(d_grade = unit_26 - unit_25)

cat("\n================ THE ANSWER, PER SPLIT (never recombined) ================\n")
for (sp in c("man", "zone")) {
  seated_bands <- facet_law_mz %>%
    filter(split == sp, supp_seat == "seated") %>% pull(band)
  seated_txt <- if (length(seated_bands)) paste(seated_bands, collapse = "/") else "none"
  cat("\n--- ", toupper(sp), " SLATE: 2026 vs 2025 opposing-coverage, by band ---\n", sep = "")
  cat("(facet law: supp seated ", seated_txt,
      "; man supp is NOISE-flagged -- printed, never seated)\n")
  print(cmp_slate_mz %>% filter(split == sp), n = Inf)
  cat("\n--- ", toupper(sp), " UNIT, split-snap-share weighted ---\n", sep = "")
  print(cmp_unit_mz %>% filter(split == sp), n = Inf)
}

# --- the two-slates headline, one row per opponent: man and
#     zone side by side as COLUMNS (never averaged together).
faced_team_unit_mz <- faced_team_band_mz %>%
  left_join(band_wt_mz, by = c("split", "band")) %>%
  group_by(split, team_name) %>%
  summarise(unit_25 = if (all(is.na(grade_25))) NA_real_
            else weighted.mean(grade_25, w = wt, na.rm = TRUE),
            .groups = "drop")

answer_mz <- team_unit_2026_mz %>%
  pivot_wider(names_from = split,
              values_from = c(unit_26, interp_share)) %>%
  left_join(faced_team_unit_mz %>%
              pivot_wider(names_from = split, values_from = unit_25,
                          names_prefix = "unit_25_"),
            by = "team_name") %>%
  left_join(scheme_share_mz %>%
              select(team_name, man_share, zone_share), by = "team_name") %>%
  left_join(scheme_share_faced_mz %>%
              select(team_name, man_share_faced), by = "team_name") %>%
  mutate(d_man  = unit_26_man  - unit_25_man,
         d_zone = unit_26_zone - unit_25_zone) %>%
  arrange(match(team_name, opp_2026_teams)) %>%
  select(team_name, man_share, man_share_faced,
         unit_25_man, unit_26_man, d_man, interp_share_man,
         unit_25_zone, unit_26_zone, d_zone, interp_share_zone)

cat("\n--- THE TWO SLATES, one row per 2026 opponent (columns, never\n")
cat("    recombined; man_share = the opponent's 2025 scheme split) ---\n")
print(answer_mz, n = Inf)

# ------------------------------------------------------------
# 11. FIGS -- one per split (two slates, two figs; never one
#     panel mixing currencies). Grammar ports Phase 6: ghost
#     open circle = what NE's pass game faced from THAT team in
#     2025 (absent = not faced, THIS currency); solid navy =
#     2026 projected committee. Team order = THIS split's unit
#     composite. Division x2 lives in the slate rows, not here.
# ------------------------------------------------------------

library(ggplot2)

team_cmp_mz <- team_unit_2026_mz %>%
  left_join(faced_team_unit_mz, by = c("split", "team_name"))

fig_one_mz <- function(sp) {
  pd <- team_band_2026_mz %>%
    filter(split == .env$sp) %>%
    select(team_name, band, grade_26) %>%
    left_join(faced_team_band_mz %>%
                filter(split == .env$sp) %>%
                select(team_name, band, grade_25),
              by = c("team_name", "band")) %>%
    mutate(team_name = factor(team_name,
                              levels = team_cmp_mz %>%
                                filter(split == .env$sp) %>%
                                arrange(unit_26) %>% pull(team_name)),
           band = factor(band, levels = SEC_BANDS_MZ))
  supp_note <- if (sp == "man")
    "supp pctls NOISE-FLAGGED at man 6/6 -- grade is the only seated facet" else
      "supp seated CB/S, context LB, dead SCB (zone 13/6 floors)"
  ggplot(pd, aes(y = team_name)) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey45") +
    geom_segment(data = pd %>% filter(!is.na(grade_25)),
                 aes(x = grade_25, xend = grade_26, yend = team_name),
                 arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
                 linewidth = 1, color = "grey60") +
    geom_point(aes(x = grade_25), shape = 1, size = 3.2, stroke = 1.2,
               color = "grey55", na.rm = TRUE) +
    geom_point(aes(x = grade_26), shape = 16, size = 2.6, color = "#002244") +
    facet_wrap(~ band, nrow = 1) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = paste0("NE offense - 2026 vs 2025 opposing-coverage slate, by band -- ",
                        toupper(sp), " currency"),
         subtitle = paste0("open circle = 2025 defenders faced (split-snap-weighted; absent = not faced in ",
                           sp, ") | solid navy = 2026 projected committee | higher = HARDER for Maye | ",
                           supp_note),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "grey40", size = 8.5),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(face = "bold", size = 9),
          panel.spacing.x = unit(1.1, "lines"))
}

plot_cov_slate_man  <- fig_one_mz("man")
plot_cov_slate_man
plot_cov_slate_zone <- fig_one_mz("zone")
plot_cov_slate_zone

# ------------------------------------------------------------
# 12. FIG-COMPANION TABLES -- one per split. One row per
#     opponent: grade '25/'26/D per band + the split UNIT +
#     interp % + the scheme-share by-product column (which
#     currency matters vs this opponent). gt attached HERE,
#     not inherited.
# ------------------------------------------------------------

library(gt)

cov_gt_one_mz <- function(sp) {
  pd <- team_band_2026_mz %>%
    filter(split == .env$sp) %>%
    select(team_name, band, grade_26) %>%
    pivot_wider(names_from = band, values_from = grade_26,
                names_prefix = "g26_") %>%
    left_join(faced_team_band_mz %>%
                filter(split == .env$sp) %>%
                select(team_name, band, grade_25) %>%
                pivot_wider(names_from = band, values_from = grade_25,
                            names_prefix = "g25_"),
              by = "team_name") %>%
    mutate(d_CB  = g26_CB  - g25_CB,
           d_SCB = g26_SCB - g25_SCB,
           d_S   = g26_S   - g25_S,
           d_LB  = g26_LB  - g25_LB) %>%
    left_join(team_unit_2026_mz %>% filter(split == .env$sp),
              by = "team_name") %>%
    left_join(faced_team_unit_mz %>% filter(split == .env$sp) %>%
                select(team_name, unit_25), by = "team_name") %>%
    left_join(scheme_share_mz %>%
                select(team_name, man_share), by = "team_name") %>%
    mutate(d_unit = unit_26 - unit_25) %>%
    arrange(desc(unit_26)) %>%
    select(team_name,
           g25_CB, g26_CB, d_CB, g25_SCB, g26_SCB, d_SCB,
           g25_S, g26_S, d_S, g25_LB, g26_LB, d_LB,
           unit_25, unit_26, d_unit, interp_share, man_share)
  
  d_dom <- max(abs(c(pd$d_CB, pd$d_SCB, pd$d_S, pd$d_LB, pd$d_unit)),
               na.rm = TRUE)
  
  gate_txt <- if (sp == "man") "6 man snaps / 6 games" else "13 zone snaps / 6 games"
  law_txt  <- if (sp == "man")
    "supp NOISE-flagged (floors only at 15/6) -- grade-only currency" else
      "supp seated CB/S; context LB; dead SCB"
  
  pd %>%
    gt() %>%
    tab_spanner(label = "CB (N=3)",  columns = c(g25_CB, g26_CB, d_CB)) %>%
    tab_spanner(label = "SCB (N=1)", columns = c(g25_SCB, g26_SCB, d_SCB)) %>%
    tab_spanner(label = "S (N=3)",   columns = c(g25_S, g26_S, d_S)) %>%
    tab_spanner(label = "LB (N=2)",  columns = c(g25_LB, g26_LB, d_LB)) %>%
    tab_spanner(label = "UNIT",      columns = c(unit_25, unit_26, d_unit)) %>%
    cols_label(team_name = "",
               g25_CB = "'25", g26_CB = "'26", d_CB = "D",
               g25_SCB = "'25", g26_SCB = "'26", d_SCB = "D",
               g25_S  = "'25", g26_S  = "'26", d_S  = "D",
               g25_LB = "'25", g26_LB = "'26", d_LB = "D",
               unit_25 = "'25", unit_26 = "'26", d_unit = "D",
               interp_share = "interp %",
               man_share = "man shr") %>%
    fmt_percent(columns = c(g25_CB, g26_CB, g25_SCB, g26_SCB,
                            g25_S, g26_S, g25_LB, g26_LB,
                            unit_25, unit_26, interp_share, man_share),
                decimals = 0) %>%
    fmt_percent(columns = c(d_CB, d_SCB, d_S, d_LB, d_unit), decimals = 0,
                force_sign = TRUE) %>%
    sub_missing(columns = everything(), missing_text = "--") %>%
    data_color(columns = c(d_CB, d_SCB, d_S, d_LB, d_unit),
               fn = scales::col_numeric(c("#6baed6", "#f7f7f7", "#C60C30"),
                                        domain = c(-d_dom, d_dom)),
               autocolor_text = TRUE) %>%
    tab_header(title = paste0("2026 opposing-coverage committees vs the 2025 secondaries NE faced -- ",
                              toupper(sp), " currency"),
               subtitle = paste0("usage-weighted band means at the ruled gates (", gate_txt,
                                 "; N = 3/1/3/2 per currency) | UNIT = split-snap-share-weighted grade composite | higher = harder for Maye's pass game | D red = harder | '--' = not faced in 2025 (this currency) | interp % = share of projected snaps on v3 priors | man shr = opponent 2025 scheme split | ",
                                 law_txt,
                                 " | slate row (division x2) prints in console")) %>%
    tab_options(table.font.size = px(12), data_row.padding = px(3),
                column_labels.font.weight = "bold")
}

cov_slate_gt_man  <- cov_gt_one_mz("man")
cov_slate_gt_man
cov_slate_gt_zone <- cov_gt_one_mz("zone")
cov_slate_gt_zone

# ------------------------------------------------------------
# STOP 2 -- player panels (v2 grammar) + 3-season table build
# AFTER the fig eyeball, on request. Phase 6 combined stands
# checkpointed; 6b objects coexist via the _mz suffix law.
# Checkpoint objects for the six-part rollup workspace:
#   cov_season_pctl_mz, cov_prior_law_mz, ledger_mz,
#   rot_2026_mz, faced_band_mz, faced_team_band_mz,
#   team_band_2026_mz, team_unit_2026_mz, cmp_slate_mz,
#   cmp_unit_mz, answer_mz, scheme_share_mz,
#   scheme_share_faced_mz, band_wt_mz, luck_stamp_mz,
#   grade_deciles_mz, facet_law_mz
# ------------------------------------------------------------
# ggsave("cov_fig1_slate_man.png",  plot_cov_slate_man,  width = 12, height = 6, dpi = 200)
# ggsave("cov_fig1_slate_zone.png", plot_cov_slate_zone, width = 12, height = 6, dpi = 200)
# gtsave(cov_slate_gt_man,  "cov_slate_table_man.png",  vwidth = 1300)
# gtsave(cov_slate_gt_zone, "cov_slate_table_zone.png", vwidth = 1300)
# save.image("~/ne_coverage_sos_workspace.RData")
# system('aws s3 cp ~/ne_coverage_sos_workspace.RData s3://nfl-pff-data-lucas/workspaces/')



# ============================================================
# PHASE 6b -- CELL DRILL-DOWN (audit tool, no new machinery)
# Source AFTER phase6b_build_split_slates.txt (needs its
# session objects). Prints the FULL arithmetic behind one
# (team, band, split) cell of THE ANSWER:
#   faced '25  = which defender-games vs NE, which pctls,
#                which snap weights (and how many unscored)
#   room       = the full ledger for that team x band x split
#                (proposed flag + status -- a name MISSING from
#                the roster shows nowhere; a name present but
#                ungated shows as usage_no_pctl)
#   committee  = the N members with every blend component
#                (g25 / g24 / w25 / blend / prior / weight)
# Usage: cell_drill_mz("NYJ", "CB", "zone") -- then any cell.
# ASCII only. No new law -- this file only READS the receipts.
# ============================================================

cell_drill_mz <- function(tm, b, sp) {
  x <- if (sp == "man") X_MAN else X_ZONE
  cat("\n================ CELL DRILL:", tm, "/", b, "/",
      toupper(sp), "================\n")
  
  # ---- faced side (the '25 number)
  fg <- faced_games_mz %>% filter(team == tm, band == b, split == sp)
  cat("\n-- faced 2025: defender-games vs NE passing, >=", x, sp,
      "snaps --\n")
  print(fg %>% arrange(week, desc(g_snaps)) %>%
          select(week, player, g_snaps, g25, g25_f, price_basis), n = Inf)
  if (nrow(fg)) {
    cat("\n-- faced, per defender (snap-weighted across the NE games) --\n")
    print(fg %>% group_by(player) %>%
            summarise(games = dplyr::n(), sn = sum(g_snaps),
                      pctl = weighted.mean(g25_f, w = g_snaps, na.rm = TRUE),
                      .groups = "drop") %>%
            arrange(desc(sn)), n = Inf)
    cat("=> faced '25 = snap-weighted mean pctl =",
        round(weighted.mean(fg$g25_f, w = fg$g_snaps, na.rm = TRUE), 3),
        " over", sum(fg$g_snaps), sp, "snaps |",
        sum(is.na(fg$g25)), "defender-games priced at vet prior\n")
  } else cat("   (not faced in this currency -- the '--' cell)\n")
  
  # ---- the room (who was eligible, who was proposed)
  cat("\n-- the 2026 room, ledger order (proposed = in the committee) --\n")
  print(ledger_mz %>%
          filter(team_name == tm, band == b, split == sp) %>%
          arrange(desc(usage_ord)) %>%
          select(roster_name, status, proposed, usage_ord,
                 sn25 = cov_snaps_2025, sn24 = cov_snaps_2024,
                 grade_25, grade_24), n = Inf)
  
  # ---- the committee (the '26 number), every component
  rr <- rot_2026_mz %>% filter(team_name == tm, band == b, split == sp)
  out <- rr %>%
    left_join(ledger_mz %>%
                filter(team_name == tm, band == b, split == sp) %>%
                select(roster_name, grade_25, grade_24, qg_2025),
              by = "roster_name") %>%
    mutate(w25 = pmin(dplyr::coalesce(qg_2025, 0L) / 10, 1),
           grade_bl = blend2(grade_25, grade_24, w25),
           price_basis = case_when(
             status == "phantom"                      ~ "phantom prior (v3)",
             prior_used & status == "rookie"          ~ "rookie prior (v3)",
             prior_used                               ~ "vet prior (v3)",
             !is.na(grade_25) & !is.na(grade_24)      ~ "blend '25/'24",
             !is.na(grade_25)                         ~ "'25 pctl only",
             TRUE                                     ~ "'24 pctl only")) %>%
    arrange(desc(usage_w)) %>%
    select(roster_name, status, price_basis, usage_w,
           grade_25, grade_24, w25, grade_bl, grade_f)
  cat("\n-- 2026 committee,", sp, "slate (N =", N_BAND_MZ[b],
      "), the arithmetic --\n")
  print(out, n = Inf)
  cat("=> projected '26 = usage-weighted mean grade_f =",
      round(weighted.mean(rr$grade_f, w = rr$usage_w), 3), "\n")
  cat("   D =", round(weighted.mean(rr$grade_f, w = rr$usage_w), 3), "-",
      if (nrow(fg)) round(weighted.mean(fg$g25, w = fg$g_snaps,
                                        na.rm = TRUE), 3) else "NA",
      "= the cell\n")
  invisible(list(faced = fg, committee = out))
}

# ------------------------------------------------------------
# the cell under audit + the contrast cell:
cell_drill_mz("PIT", "CB", "zone")
cell_drill_mz("PIT", "CB", "man")
# ------------------------------------------------------------
# more cells on request: cell_drill_mz("<TM>", "<band>", "<split>")
# e.g. cell_drill_mz("BUF", "CB", "man")   -- the other big red
#      cell_drill_mz("MIA", "CB", "zone")  -- the big blue one