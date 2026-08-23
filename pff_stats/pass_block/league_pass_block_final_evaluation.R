# ============================================================
# NE 2026 -- OPPONENT PASS BLOCK, ADJUSTED EDITION (currency three)
# Andy directive 2026-08-17: one place to look things up -- player
# lookup, team boards, and the slate framework valued in the
# schedule-adjusted currency, AS AN OPTION beside the standard slate.
# Pass-blocking port of league_pass_rush_final_evaluation.R, built
# 2026-08-19 against the OL contracts (new_england_opp_ol_schedule.R,
# league_pass_block_evaluating_currency_three.R).
#
# FIREWALL AMENDMENT (Andy directive, 08-17): currency three still
# never enters any CURRENCY-ONE artifact. Adjusted editions are their
# own, always-labeled family. Membership, phantoms, and weights are
# taken UNCHANGED from the canon OL build -- same starters, same
# games, same equal-game slot means -- so standard-vs-adjusted
# differences are valuation only.
#
# VOCABULARY (Andy 08-17): printed labels are raw (= currency one)
# and adj (= currency three). Session OBJECT names keep c1/c3
# provenance so cross-file contracts stay intact.
#
# GRADE LENS ONLY: currency three exists for TPS pass-block grade
# (canon dropped the pb lens; tps_press stays out per the 2026-08-08
# ruling). DIRECTION: higher = better protection = HARDER for NE's
# pass rush.
#
# IMPORTED GATES (not re-derived): pblk_c3_pctl carries the c3 file's
# gates inside it -- X_QUAL_PBLK = 16 (PROPOSED -- Kimi, UNSIGNED,
# awaiting Andy's stamp), G_MIN_PBLK = 6, pools = def_ssn x
# det_position, MEDIAN season aggregation.
#
# SOURCE ORDER: pff_pass_block_AWS.R, then new_england_opp_ol_
# schedule.R (needs opp_ol_2026_final, ne_2025_opp_ol_games,
# cmp_ol_slate, rookie_prior, entry_years, ol_season_pctl,
# all_pass_block_summary, tps_pass_block_player_season_summary,
# sched_2026, blend2, in_season, pctl_year, ol_pos_levels, id_xwalk,
# pff_team_lookup), then league_pass_block_evaluating_currency_
# three.R (needs pblk_game_c3, pblk_modal_band_c3, pblk_c3_pctl),
# then this file. slate_view_pb additionally needs the OL league
# tail (league_opp_pass_blocking_schedule.R run end to end:
# opp_map_ol, slots_full, pff32_ol); without it the file sources
# fine and slate_view_pb declines politely, naming what is missing.
#
# PROPOSED -- Kimi, UNSIGNED (adaptations from the prush template,
# awaiting Andy's stamp):
#   P-A. FACED REBUILD WITH IDS: canon's ne_2025_opp_ol_games drops
#       player_id and week in its final transmute (the faced-side
#       rot-mine). The adj faced side rebuilds the rows from
#       all_pass_block_summary with the IDENTICAL construction (the
#       league file's R1 check proved it equal on per-slot earned
#       means to 1e-8) and walls membership against the canon frame
#       before any valuation happens. Canon's raw side is untouched.
#   P-B. ALL-YEARS RAW HISTORY: canon keeps three pctl years in
#       session (ol_season_pctl/_24/_23). The lookups' raw columns
#       come from tps_raw_hist_pb, built LAZILY on the first lookup
#       call (canon's own pctl_year machinery over
#       tps_pass_block_player_season_summary, every season present)
#       and cached in session. Raw is joined at the player's band for
#       that season, so raw and adj are valued in the same pool.
#   P-C. slate_view_pb WIRING: the OL league tail is league_opp_pass_
#       blocking_schedule.R (opp_map_ol / slots_full / pff32_ol), NOT
#       the prush _lg fleet. The 2026 side mirrors that file's signed
#       tps value chain (blend / raw25 / slot prior / unit scalar,
#       with the rookie and phantom forks) inline -- slots_full
#       carries every ingredient -- and every pick's adj value joins
#       by player_id. MACHINE-ONLY: no hand deltas, so
#       slate_view_pb("NE") can differ slightly from the signed NE
#       artifacts (cmp_ol_slate / cmp_adj_pb) -- those remain canon.
#   P-D. GAME-SLOT vs MODAL BAND on the faced side: canon raw faced
#       prices each game at THAT game's det_position (ol_season_pctl
#       is player x det_position). c3 keeps ONE modal band per
#       player-season, so a game played off his modal band falls to
#       the slot prior; those cells are counted separately
#       (off_modal_share) from never-qualified fills (no_c3_2025).
#   P-E. COEXISTENCE NAMING: every object here carries a _pb / _ol
#       suffix (player_c3_pb, c3_rookie_prior_ol, cmp_adj_pb, ...) so
#       all six adjusted files can live in one session; the pass-rush
#       file's generic names stay its own.
# ============================================================

needed_adj_pb <- c("sched_2026", "opp_ol_2026_final",
                   "ne_2025_opp_ol_games", "cmp_ol_slate",
                   "rookie_prior", "entry_years", "ol_season_pctl",
                   "all_pass_block_summary",
                   "tps_pass_block_player_season_summary",
                   "pblk_game_c3", "pblk_modal_band_c3", "pblk_c3_pctl",
                   "in_season", "blend2", "pctl_year", "ol_pos_levels",
                   "id_xwalk", "pff_team_lookup")
missing_adj_pb <- needed_adj_pb[!vapply(needed_adj_pb, exists, logical(1))]
if (length(missing_adj_pb)) stop("missing session objects: ",
                                 paste(missing_adj_pb, collapse = ", "),
                                 " -- see SOURCE ORDER in header")

# names() walls -- never guess columns (house law). The frame's actual
# names print BEFORE the stop, so the fix is one paste away.
req_cols_pb <- list(
  pblk_c3_pctl = c("player_id", "player", "season", "band", "qual_g",
                   "pb_snaps", "med_vsopp", "c3_pctl"),
  pblk_game_c3 = c("player_id", "player", "season", "week", "team",
                   "def_ssn", "det_position", "pb_snaps"),
  pblk_modal_band_c3 = c("player_id", "season", "band"),
  opp_ol_2026_final = c("team_name", "det_position", "roster_name",
                        "player_id", "status", "tps_f"),
  ne_2025_opp_ol_games = c("team_name", "det_position", "tps_f"),
  cmp_ol_slate = c("det_position", "metric", "slate_2025",
                   "slate_2026", "delta"),
  rookie_prior = c("det_position", "pr_tps"),
  entry_years = c("player_id", "entry_year"),
  ol_season_pctl = c("player_id", "det_position", "tps_grade"),
  all_pass_block_summary = c("season", "week", "def_ssn", "team_name",
                             "det_position", "player", "player_id",
                             "snap_counts_pass_block"),
  tps_pass_block_player_season_summary = c("player_id", "det_position",
                                           "season",
                                           "grade_season_pctl"))
for (nm_pb in names(req_cols_pb)) {
  miss_pb <- setdiff(req_cols_pb[[nm_pb]], names(get(nm_pb)))
  if (length(miss_pb)) {
    cat("\n", nm_pb, "-- columns present:\n")
    print(names(get(nm_pb)))
    stop(nm_pb, " is missing columns: ", paste(miss_pb, collapse = ", "),
         " -- report, do not proceed")
  }
}

# ------------------------------------------------------------
# 1. c3 entry-year prior by slot (fills rookies / no-c3 members,
#    mirroring the currency-one rookie_prior law)
# ------------------------------------------------------------

c3_rookie_prior_ol <- pblk_c3_pctl %>%
  inner_join(entry_years, by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2017) %>%
  filter(band %in% ol_pos_levels) %>%
  group_by(band) %>%
  summarise(pr_c3 = median(c3_pctl, na.rm = TRUE),
            n_entry = dplyr::n(), .groups = "drop")

cat("\n--- c3 entry-year prior by slot ---\n")
print(c3_rookie_prior_ol)

if (!setequal(c3_rookie_prior_ol$band, ol_pos_levels)) {
  cat("slots priced:", c3_rookie_prior_ol$band, "\n")
  stop("c3 prior must price all five slots -- every coalesce below ",
       "depends on it; report, do not proceed")
}

# ------------------------------------------------------------
# 2. LOOKUPS -- the "where is Zeitler" layer
# ------------------------------------------------------------

# pblk_game_c3$team code space -> PFF space (never startsWith; the
# league-mode law). The coalesce is a no-op when the feed is already
# PFF-coded; the receipt below prints the space either way.
OL_TEAM_PATCH <- c("ARI" = "ARZ", "AZ" = "ARZ", "BAL" = "BLT",
                   "CLE" = "CLV", "HOU" = "HST", "LAR" = "LA",
                   "JAC" = "JAX", "WSH" = "WAS")

team_codes_pb <- sort(unique(dplyr::coalesce(
  OL_TEAM_PATCH[pblk_game_c3$team], pblk_game_c3$team)))
cat("\n--- pblk_game_c3 team codes, patched to PFF space ---\n")
cat(team_codes_pb, sep = " ", "\n")

# PROPOSED P-B: lazy all-years raw history. Canon's own pctl_year
# machinery over tps_pass_block_player_season_summary, every season
# present, built on the FIRST lookup call and cached in session. The
# raw currency is identical to canon's tps_grade column by
# construction (same feed, same rename, same stint weighting).
build_tps_raw_hist_pb <- function() {
  yrs_pb <- sort(unique(tps_pass_block_player_season_summary$season))
  out_pb <- purrr::map_dfr(yrs_pb, function(y) {
    suppressMessages(
      pctl_year(tps_pass_block_player_season_summary,
                c(raw = "grade_season_pctl"), y)) %>%
      mutate(season = y)
  })
  assign("tps_raw_hist_pb", out_pb, envir = .GlobalEnv)
  cat("\ntps_raw_hist_pb built once:", nrow(out_pb),
      "player x band x season rows over", length(yrs_pb),
      "seasons (cached in session)\n")
  invisible(out_pb)
}
get_tps_raw_hist_pb <- function() {
  if (!exists("tps_raw_hist_pb", envir = .GlobalEnv)) build_tps_raw_hist_pb()
  get("tps_raw_hist_pb", envir = .GlobalEnv)
}

player_c3_pb <- function(who) {
  if (is.numeric(who)) {
    d <- pblk_c3_pctl %>% filter(player_id %in% who)
  } else {
    d <- pblk_c3_pctl %>%
      filter(tolower(player) == tolower(who))        # full name first
    if (nrow(d) == 0) {
      d <- pblk_c3_pctl %>%
        filter(grepl(who, player, ignore.case = TRUE))
    }
  }
  if (nrow(d) == 0) { cat("no match\n"); return(invisible(NULL)) }
  ids <- d %>% group_by(player_id, player) %>%
    summarise(seasons = paste0(min(season), "-", max(season)),
              career_pb_snaps = sum(pb_snaps), .groups = "drop")
  if (nrow(ids) > 1) {
    if (dplyr::n_distinct(ids$player) > 1) {
      cat("\n--", nrow(ids), "players match. Re-run with the full name: --\n")
      print(ids %>% select(player, seasons, career_pb_snaps), n = Inf)
    } else {
      cat("\n-- same name, different players (rare). Re-run",
          "player_c3_pb(<player_id>): --\n")
      print(ids, n = Inf)
    }
    return(invisible(ids))
  }
  d <- d %>% arrange(season) %>% rename(adj = c3_pctl) %>%
    left_join(get_tps_raw_hist_pb() %>%
                select(player_id, season, band = det_position, raw),
              by = c("player_id", "season", "band")) %>%
    mutate(d = round(adj - raw, 3))
  print(d %>% select(player, season, band, qual_g, pb_snaps, med_vsopp,
                     raw, adj, d), n = Inf)
  invisible(d)
}

team_c3_pb <- function(team, ssn = 2025) {
  yrs <- (ssn - 2):ssn
  team_p <- unname(dplyr::coalesce(OL_TEAM_PATCH[team], team))
  ids <- pblk_game_c3 %>%
    mutate(team_pff = unname(dplyr::coalesce(OL_TEAM_PATCH[team], team))) %>%
    filter(season == ssn) %>%
    count(player_id, team_pff, wt = pb_snaps, name = "sn") %>%
    group_by(player_id) %>%
    slice_max(sn, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    filter(team_pff == team_p) %>%
    select(player_id)
  if (nrow(ids) == 0) {
    cat("no", ssn, "pass-block games for", team_p,
        "-- the code list printed at source shows the space\n")
    return(invisible(NULL))
  }
  hist <- pblk_c3_pctl %>%
    filter(season %in% yrs) %>%
    semi_join(ids, by = "player_id")
  wide_adj <- hist %>%
    select(player_id, season, c3_pctl) %>%
    tidyr::pivot_wider(names_from = season, values_from = c3_pctl,
                       names_prefix = "adj_", names_sort = TRUE)
  b <- hist %>%
    filter(season == ssn) %>%
    select(player_id, player, band, qual_g, pb_snaps) %>%
    left_join(wide_adj, by = "player_id")
  hist_raw <- hist %>%
    select(player_id, season, band) %>%
    inner_join(get_tps_raw_hist_pb() %>%
                 select(player_id, season, band = det_position, raw),
               by = c("player_id", "season", "band")) %>%
    select(player_id, season, raw)
  b <- b %>%
    left_join(hist_raw %>%
                tidyr::pivot_wider(names_from = season,
                                   values_from = raw,
                                   names_prefix = "raw_",
                                   names_sort = TRUE),
              by = "player_id")
  rw <- paste0("raw_", ssn)
  if (rw %in% names(b)) {
    b <- b %>% mutate(d = round(.data[[paste0("adj_", ssn)]] -
                                  .data[[rw]], 3))
  }
  b <- b %>% arrange(match(band, ol_pos_levels),
                     desc(.data[[paste0("adj_", ssn)]]))
  cat("\n==", team_p, ssn, "-- pass blockers, raw + adj,", min(yrs),
      "-", ssn, "(values travel with the player) ==\n")
  print(b %>% select(player, band, qual_g, pb_snaps,
                     dplyr::starts_with("raw_"),
                     dplyr::starts_with("adj_"),
                     dplyr::any_of("d")), n = Inf)
  cat("\n-- slot means per season, pb-snap-weighted, this", ssn,
      "room (band = that season's modal band) --\n")
  mm <- hist %>%
    group_by(band, season) %>%
    summarise(adj = round(weighted.mean(c3_pctl, w = pb_snaps), 3),
              .groups = "drop") %>%
    left_join(hist %>%
                inner_join(hist_raw, by = c("player_id", "season")) %>%
                group_by(band, season) %>%
                summarise(raw = round(weighted.mean(raw, w = pb_snaps),
                                      3),
                          .groups = "drop"),
              by = c("band", "season")) %>%
    select(band, season, raw, adj) %>%
    arrange(match(band, ol_pos_levels), season)
  print(mm, n = Inf)
  invisible(b)
}

team_c3_26_pb <- function(tm) {
  # 2026 roster view, any of the 32 teams (the roster route -- canon
  # roster-change protocol: transactions automatic via load_rosters).
  # History travels with the player; adj_proj = the same blend2 +
  # prior the slate uses.
  ros <- nflreadr::load_rosters(2026) %>%
    filter(position %in% c("T", "G", "C", "OL", "OT", "OG")) %>%
    transmute(gsis_id, player = full_name, entry_year,
              team_name = dplyr::coalesce(pff_team_lookup[team], team)) %>%
    filter(team_name == tm) %>%
    left_join(id_xwalk %>% filter(!is.na(gsis_id)) %>%
                select(player_id, gsis_id),
              by = "gsis_id")
  if (nrow(ros) == 0) {
    stop("no 2026 roster rows for '", tm, "' -- PFF code space ",
         "(ARZ not ARI, LA not LAR, BLT/CLV/HST/WAS)")
  }
  yrs <- 2023:2025
  band26 <- pblk_c3_pctl %>%
    filter(season %in% c(2024, 2025)) %>%
    semi_join(ros %>% filter(!is.na(player_id)), by = "player_id") %>%
    select(player_id, season, band) %>%
    tidyr::pivot_wider(names_from = season, values_from = band,
                       names_prefix = "b") %>%
    mutate(band = dplyr::coalesce(b2025, b2024)) %>%
    select(player_id, band)
  wide_adj <- pblk_c3_pctl %>%
    filter(season %in% yrs) %>%
    semi_join(ros %>% filter(!is.na(player_id)), by = "player_id") %>%
    select(player_id, season, c3_pctl) %>%
    tidyr::pivot_wider(names_from = season, values_from = c3_pctl,
                       names_prefix = "adj_", names_sort = TRUE)
  wide_raw <- pblk_modal_band_c3 %>%
    filter(season %in% yrs) %>%
    semi_join(ros %>% filter(!is.na(player_id)), by = "player_id") %>%
    inner_join(get_tps_raw_hist_pb(),
               by = c("player_id", "season", "band" = "det_position")) %>%
    select(player_id, season, raw) %>%
    tidyr::pivot_wider(names_from = season, values_from = raw,
                       names_prefix = "raw_", names_sort = TRUE)
  proj <- pblk_c3_pctl %>%
    filter(season == 2025) %>%
    select(player_id, a25 = c3_pctl, qg25 = qual_g) %>%
    full_join(pblk_c3_pctl %>% filter(season == 2024) %>%
                select(player_id, a24 = c3_pctl),
              by = "player_id")
  b <- ros %>%
    select(player, player_id, entry_year) %>%
    left_join(band26, by = "player_id") %>%
    left_join(proj,   by = "player_id") %>%
    left_join(c3_rookie_prior_ol %>% select(band, pr_c3), by = "band") %>%
    mutate(w25 = pmin(dplyr::coalesce(qg25, 0L) / 10, 1),
           adj_proj = round(dplyr::coalesce(blend2(a25, a24, w25),
                                            pr_c3), 3),
           note = dplyr::case_when(
             entry_year == 2026 ~ "rookie",
             is.na(player_id)   ~ "no_pff_id",
             is.na(band)        ~ "no_24_25_data",
             TRUE ~ "")) %>%
    left_join(wide_adj, by = "player_id") %>%
    left_join(wide_raw, by = "player_id")
  b <- b %>% arrange(match(band, ol_pos_levels), desc(adj_proj))
  cat("\n==", tm, "2026 roster -- raw + adj history 2023-2025,",
      "adj_proj = slate valuation ==\n")
  print(b %>% select(player, band, entry_year,
                     dplyr::starts_with("raw_"),
                     dplyr::starts_with("adj_"),
                     adj_proj, note), n = Inf)
  invisible(b)
}

# ------------------------------------------------------------
# 3. FACED 2025 -- adjusted valuation. PROPOSED P-A rebuild: same
#    defender-games (membership walled against canon below), same
#    equal-game slot means (the OL slate law -- one game, one vote;
#    pair with n_games), playoffs IN. Only the currency swaps.
#    Canon's raw faced is earned-only; the adj side fills at the slot
#    prior, and every fill is counted.
# ------------------------------------------------------------

faced_rows_pb <- all_pass_block_summary %>%
  filter(def_ssn == "NE2025", in_season(week),
         det_position %in% ol_pos_levels) %>%
  group_by(week, team_name, det_position) %>%
  slice_max(snap_counts_pass_block, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(week, team_name, det_position, player, player_id)

# MEMBERSHIP WALL: the rebuild must reproduce canon's frame exactly --
# cells per team x slot, then per-slot earned means to 1e-8 (the
# league file's R1 pattern, proven green). Evidence BEFORE the stop.
chk_mem_pb <- ne_2025_opp_ol_games %>%
  count(team_name, det_position, name = "n_canon") %>%
  full_join(faced_rows_pb %>%
              count(team_name, det_position, name = "n_mine"),
            by = c("team_name", "det_position")) %>%
  mutate(same = !is.na(n_canon) & !is.na(n_mine) & n_canon == n_mine)
if (!all(chk_mem_pb$same)) {
  cat("\n--- faced rebuild membership mismatch (canon vs rebuild) ---\n")
  print(chk_mem_pb %>% filter(!same), n = Inf)
  stop("faced rebuild membership != canon -- report, do not proceed")
}

chk_val_pb <- ne_2025_opp_ol_games %>%
  group_by(det_position) %>%
  summarise(canon_tps = mean(tps_f, na.rm = TRUE), .groups = "drop") %>%
  full_join(faced_rows_pb %>%
              left_join(ol_season_pctl %>%
                          select(player_id, det_position, tps_grade),
                        by = c("player_id", "det_position")) %>%
              group_by(det_position) %>%
              summarise(mine_tps = mean(tps_grade, na.rm = TRUE),
                        .groups = "drop"),
            by = "det_position") %>%
  mutate(ok_tps = abs(canon_tps - mine_tps) < 1e-8)
cat("\n--- faced rebuild check: per-slot earned means, canon vs",
    "rebuild (must be identical) ---\n")
print(chk_val_pb %>% mutate(across(where(is.numeric), ~ round(.x, 4))))
stopifnot(all(chk_val_pb$ok_tps))

# Adj valuation at the GAME slot (P-D): c3 keeps one modal band per
# player-season, so the join key is (player_id, game slot == band).
# Misses split into never-qualified (no_c3_2025) vs qualified-at-
# another-band (off_modal_slot); both fill at the slot prior.
faced_c3_pb <- faced_rows_pb %>%
  left_join(pblk_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, band, c3_25 = c3_pctl),
            by = c("player_id", "det_position" = "band")) %>%
  mutate(has_c3_25 = player_id %in%
           (pblk_c3_pctl %>% filter(season == 2025) %>%
              pull(player_id))) %>%
  left_join(c3_rookie_prior_ol %>% select(band, pr_c3),
            by = c("det_position" = "band")) %>%
  mutate(fill_c3  = is.na(c3_25),
         fill_why = dplyr::case_when(!fill_c3   ~ "earned",
                                     !has_c3_25 ~ "no_c3_2025",
                                     TRUE       ~ "off_modal_slot"),
         c3_25    = dplyr::coalesce(c3_25, pr_c3))

if (any(is.na(faced_c3_pb$c3_25))) {
  print(faced_c3_pb %>% filter(is.na(c3_25)))
  stop("faced adj produced NA after the prior fill -- a slot is ",
       "missing from c3_rookie_prior_ol; report")
}

cat("\n--- faced fill tiers (adj, by cell count) ---\n")
print(faced_c3_pb %>% count(fill_why, name = "cells") %>%
        mutate(share = round(cells / sum(cells), 3)))

faced_band_c3_pb <- faced_c3_pb %>%
  group_by(det_position) %>%
  summarise(n_games = dplyr::n(),
            fill_share = round(mean(fill_c3), 3),
            off_modal_share = round(mean(fill_why == "off_modal_slot"),
                                    3),
            adj_25 = round(mean(c3_25), 3),
            .groups = "drop")

cat("\n--- faced 2025, adj valuation by slot (equal-game means,",
    "playoffs IN) ---\n")
print(faced_band_c3_pb)

# ------------------------------------------------------------
# 4. 2026 PROJECTION -- adjusted valuation. opp_ol_2026_final
#    UNCHANGED: same members, same slots, same 70 rows. Unlike the
#    pass-rush unit, player_id SURVIVES canon's final select here --
#    no ledger recovery needed on this unit. adj never falls back to
#    raw: the value is blend2(c3_25, c3_24) else the slot prior.
# ------------------------------------------------------------

proj_c3_pb <- opp_ol_2026_final %>%
  left_join(pblk_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, c3_25p = c3_pctl, qg25_c3 = qual_g),
            by = "player_id") %>%
  left_join(pblk_c3_pctl %>% filter(season == 2024) %>%
              select(player_id, c3_24p = c3_pctl),
            by = "player_id") %>%
  left_join(c3_rookie_prior_ol %>% select(band, pr_c3),
            by = c("det_position" = "band")) %>%
  mutate(w25_c3 = pmin(dplyr::coalesce(qg25_c3, 0L) / 10, 1),
         c3_bl  = blend2(c3_25p, c3_24p, w25_c3),
         prior_used_c3 = is.na(c3_bl),
         c3_f   = dplyr::coalesce(c3_bl, pr_c3))

stopifnot(nrow(proj_c3_pb) == nrow(opp_ol_2026_final),
          anyDuplicated(proj_c3_pb[, c("team_name", "det_position")]) == 0,
          !any(is.na(proj_c3_pb$c3_f)))

cat("\n--- 2026 projection, adj valuation (prior fills by slot) ---\n")
print(proj_c3_pb %>%
        select(team_name, det_position, roster_name, status,
               c3_25p, c3_24p, c3_f, prior_used_c3) %>%
        arrange(team_name, match(det_position, ol_pos_levels)), n = 70)

slate_rows_c3_pb <- tibble::tibble(team_name = sched_2026) %>%
  left_join(proj_c3_pb %>%
              select(team_name, det_position, c3_f, prior_used_c3),
            by = "team_name", relationship = "many-to-many")

stopifnot(nrow(slate_rows_c3_pb) == length(sched_2026) * 5L)

slate_band_c3_pb <- slate_rows_c3_pb %>%
  group_by(det_position) %>%
  summarise(interp_share = round(mean(prior_used_c3), 3),
            adj_26 = round(mean(c3_f), 3),
            .groups = "drop")

# ------------------------------------------------------------
# 5. THE ANSWER, BOTH EDITIONS SIDE BY SIDE -- does the slate verdict
#    survive skill-cleaning? raw columns pulled from the live
#    cmp_ol_slate (tps_f rows); adj columns from sections 3-4. raw '25
#    is canon's earned-only mean; adj '25 fills at the slot prior
#    (fill % shown, off-modal fills shown separately).
# ------------------------------------------------------------

cmp_adj_pb <- cmp_ol_slate %>%
  filter(metric == "tps_f") %>%
  select(det_position, raw_25 = slate_2025, raw_26 = slate_2026,
         d_raw = delta) %>%
  left_join(faced_band_c3_pb %>%
              select(det_position, adj_25, fill_share, off_modal_share),
            by = "det_position") %>%
  left_join(slate_band_c3_pb %>%
              select(det_position, adj_26, interp_share),
            by = "det_position") %>%
  mutate(d_adj = adj_26 - adj_25) %>%
  arrange(match(det_position, ol_pos_levels))

cat("\n--- SLATE, raw vs adj (tps grade lens; higher = harder for",
    "NE's rush) ---\n")
print(cmp_adj_pb, n = Inf)

pblk_adj_gt <- cmp_adj_pb %>%
  select(det_position, raw_25, raw_26, d_raw, adj_25, adj_26, d_adj,
         fill_share, interp_share) %>%
  gt() %>%
  tab_spanner(label = "Raw",
              columns = c(raw_25, raw_26, d_raw)) %>%
  tab_spanner(label = "Adjusted (same-slate)",
              columns = c(adj_25, adj_26, d_adj)) %>%
  cols_label(det_position = "", raw_25 = "'25", raw_26 = "'26",
             d_raw = "\u0394", adj_25 = "'25", adj_26 = "'26",
             d_adj = "\u0394",
             fill_share = "fill %", interp_share = "interp %") %>%
  fmt_percent(columns = c(raw_25, raw_26, adj_25, adj_26,
                          fill_share, interp_share), decimals = 0) %>%
  fmt_percent(columns = c(d_raw, d_adj), decimals = 0,
              force_sign = TRUE) %>%
  data_color(columns = c(d_raw, d_adj),
             fn = scales::col_numeric(c("#6baed6", "#f7f7f7", "#C60C30"),
                                      domain = c(-0.08, 0.08)),
             autocolor_text = TRUE) %>%
  tab_header(
    title = "Opposing pass-pro slate \u2014 standard vs adjusted",
    subtitle = "same starters, games, and equal-game weights in both editions | adjusted = currency three (same-slate skill), tps grade lens only | higher = better protection = harder for NE's pass rush") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold")
pblk_adj_gt

rec_fig_pd_adj_pb <- cmp_adj_pb %>%
  select(det_position, adj_25, adj_26) %>%
  mutate(det_position = factor(det_position, levels = ol_pos_levels))

plot_pblk_slate_adj <- ggplot(rec_fig_pd_adj_pb, aes(y = det_position)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey45") +
  geom_segment(aes(x = adj_25, xend = adj_26, yend = det_position),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               linewidth = 1, color = "grey60") +
  geom_point(aes(x = adj_25), shape = 1, size = 3.2, stroke = 1.2,
             color = "grey55") +
  geom_point(aes(x = adj_26), shape = 16, size = 2.6, color = "#002244") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(title = "NE defense - opposing pass-pro slate, ADJUSTED edition",
       subtitle = "adj = same-slate skill | open = 2025 faced, solid = 2026 projected | same members + weights as the raw slate | tps grade lens only | right = harder for NE's rush",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey40", size = 8.5),
        panel.grid.major.y = element_blank())
plot_pblk_slate_adj

# ------------------------------------------------------------
# 6. lookup usage printed at source (visible on dark themes);
#    nothing executes a lookup.
# ------------------------------------------------------------
cat("\nlookups ready (console tools, full names always):\n",
    '  player_c3_pb("Kevin Zeitler")\n',
    '  team_c3_pb("DEN")          # 2025 room, 3yr history\n',
    '  team_c3_26_pb("NE")        # 2026 roster, history + slate valuation\n',
    '  slate_view_pb("KC")        # any team\'s slate, raw + adj, machine-only\n',
    "PFF code space for team args: ARZ BLT CLV HST LA WAS",
    "(not ARI BAL CLE HOU LAR WSH)\n",
    sep = "")

# ------------------------------------------------------------
# 7. slate_view_pb(focal) -- the slate question pointed at ANY team.
#    Identity via def_ssn on the faced side and opp_map_ol on the
#    projection side (PFF space; never startsWith -- the league-mode
#    law). MACHINE-ONLY: no hand deltas/tribbles for any focal, so
#    slate_view_pb("NE") can differ slightly from the signed NE
#    artifacts (cmp_ol_slate / cmp_adj_pb) -- those remain canon.
#    PROPOSED P-C: wired to the OL league tail (league_opp_pass_
#    blocking_schedule.R run end to end), with that file's signed tps
#    value chain mirrored inline. Without the tail this file still
#    sources; slate_view_pb declines politely.
# ------------------------------------------------------------

lg_needed_ol <- c("opp_map_ol", "slots_full", "pff32_ol")
if (all(vapply(lg_needed_ol, exists, logical(1)))) {
  # slots_full carries every ingredient: player_id, roster_name,
  # status, phantom, slot (assigned), tps_grade_bl, tps_grade, and
  # pr_tps (rookie_prior joined at the ASSIGNED slot).
  prior_unit_tps_pb <- mean(rookie_prior$pr_tps, na.rm = TRUE)
  rot26_full_ol <- slots_full %>%
    filter(slot %in% ol_pos_levels) %>%
    mutate(gf_raw = dplyr::case_when(
      phantom ~ dplyr::coalesce(pr_tps, prior_unit_tps_pb),
      status == "rookie" ~ dplyr::coalesce(tps_grade_bl, pr_tps,
                                           prior_unit_tps_pb),
      TRUE ~ dplyr::coalesce(tps_grade_bl, tps_grade, pr_tps,
                             prior_unit_tps_pb))) %>%
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
    select(team = team_name, slot, gf_raw, gf_adj)
  stopifnot(!any(is.na(rot26_full_ol$gf_raw)),
            !any(is.na(rot26_full_ol$gf_adj)))
  cat("\nrot26_full_ol built:", nrow(rot26_full_ol),
      "member rows across 32 teams (machine-only)\n")
} else {
  cat("\n[slate_view_pb disabled] league tail objects missing:",
      paste(lg_needed_ol[!vapply(lg_needed_ol, exists, logical(1))],
            collapse = ", "),
      "-- run league_opp_pass_blocking_schedule.R end to end\n")
}

slate_view_pb <- function(focal) {
  if (!exists("rot26_full_ol")) {
    stop("slate_view_pb needs the league tail in session -- run ",
         "league_opp_pass_blocking_schedule.R end to end first")
  }
  if (!(focal %in% pff32_ol)) {
    stop("unknown focal '", focal, "' -- PFF codes: ",
         paste(sort(pff32_ol), collapse = " "))
  }
  prior_unit_tps_pb <- mean(rookie_prior$pr_tps, na.rm = TRUE)
  f <- all_pass_block_summary %>%
    filter(season == 2025, def_ssn == paste0(focal, "2025"),
           in_season(week), det_position %in% ol_pos_levels) %>%
    group_by(week, team_name, det_position) %>%
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
    mutate(raw = dplyr::coalesce(raw, prg, prior_unit_tps_pb),
           adj = dplyr::coalesce(adj, pr_c3)) %>%
    group_by(det_position) %>%
    summarise(n_games = dplyr::n(),
              raw_25 = round(mean(raw), 3),
              adj_25 = round(mean(adj), 3),
              .groups = "drop")
  p <- opp_map_ol %>%
    filter(focal == !!focal) %>%
    left_join(rot26_full_ol, by = c("opp" = "team"),
              relationship = "many-to-many") %>%
    group_by(slot) %>%
    summarise(raw_26 = round(mean(gf_raw), 3),
              adj_26 = round(mean(gf_adj), 3),
              .groups = "drop")
  out <- f %>%
    left_join(p, by = c("det_position" = "slot")) %>%
    mutate(d_raw = raw_26 - raw_25, d_adj = adj_26 - adj_25) %>%
    select(det_position, n_games, raw_25, raw_26, d_raw,
           adj_25, adj_26, d_adj) %>%
    arrange(match(det_position, ol_pos_levels))
  cat("\n==", focal,
      "-- opposing pass-pro slate, raw + adj (MACHINE-ONLY:",
      "no hand deltas; playoffs in faced; fill at priors) ==\n")
  print(out, n = Inf)
  invisible(out)
}

# ------------------------------------------------------------
# Checkpoint (after eyeball):
# ------------------------------------------------------------
# ggsave("pblk_slate_adjusted.png", plot_pblk_slate_adj,
#        width = 8, height = 4.5, dpi = 200)