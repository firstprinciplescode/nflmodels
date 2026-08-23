# ============================================================
# NE 2026 -- OPPONENT RUN-DEFENSE SoS -- BUILD FILE (Phase 3)
# Gate ruled by Andy 2026-08-14: X_QUAL = 3 run snaps, G_MIN = 6
#   qualifying games. N per band: DI 5 / ED 5 / LB 3 / S 4.
#   S IS IN as a band (SEA eyeball: Bryant/Okada/Love/Bell/Emmanwori
#   = run-game safeties). Box-safety double-count vs Phase 6 noted.
#
# RECEIPTS (step 0 / 0b, 2026-08-14):
#   raw pull 102,126 rows (session table was filtered >= 14 snaps,
#   ~48% of games deleted -- old gate priced: X14-G10 sees 1.03
#   ED/team; dead). qbgrp join via combined_grade_epa_summary:
#   0 unmatched rows. Faced 2025 = 21 weeks, matches scraped
#   `games` table week-for-week incl LAC/HST/DEN/SEA playoff run.
#   talent_sd plateau: stop_rt .022 DI / .020 ED / .019 LB / .017 S.
#   base p: .069 DI / .062 ED / .073 LB / .031 S (computed, not
#   imported). luck_p10 never <= talent_sd at coverage-viable gates
#   (prush verdict): grade anchors, stop rate rides with caveat.
#   N grid: G6 = knee (G4 fill-in inflation, G8 rotation censorship);
#   LB pinned at 3 across all viable gates.
#
# AUTOPSY RECEIPT (2026-08-14): S band returned 100% phantoms on
#   first run. Cause: nflreadr 2026 rosters use a COARSENED position
#   set (defense = DL / LB / DB only). Safeties live under "DB";
#   "S"/"SS"/"FS" do not exist in the 2026 file, so the roster pull
#   returned zero safeties. Crosswalk coverage of gated S player_ids
#   = 471/471 (combined_ids_defense innocent). Fix: "DB" added to the
#   roster pull; pure CBs self-filter (no SOS-band history -> the
#   no-history filter kills them before the ledger).
#
# SOURCE ORDER:
#   (1) step 0 v3 in session: run_defense_qbgrp (raw + qbgrp/def ids)
#   (2) prush pipeline objects: combined_ids_defense, pff_team_lookup,
#       opp_2026_teams, sched_2026, percent_rank_avg, blend2, in_season
#   (3) this file, top to bottom. Re-source WHOLE file after any edit.
#
# Direction: opposing RUN DEFENDERS vs NE's RUN GAME.
#   higher pctl = better run defender = HARDER for NE's ground game.
# ============================================================

library(dplyr); library(tidyr); library(stringr)

needed_p3 <- c("run_defense_qbgrp", "combined_ids_defense",
               "pff_team_lookup", "opp_2026_teams", "sched_2026",
               "blend2", "in_season")
missing_p3 <- needed_p3[!vapply(needed_p3, exists, logical(1))]
if (length(missing_p3)) stop("missing session objects: ",
                             paste(missing_p3, collapse = ", "),
                             " -- see SOURCE ORDER in header")

# tibble-at-birth (RAQ returns base data.frames; print(n=) dies on them)
run_defense_qbgrp      <- tibble::as_tibble(run_defense_qbgrp)
combined_ids_defense   <- tibble::as_tibble(combined_ids_defense)

if (!exists("percent_rank_avg"))
  percent_rank_avg <- function(x) {          # fallback only; upstream wins
    r <- rank(x, ties.method = "average", na.last = "keep")
    r / sum(!is.na(x))
  }

# ------------------------------------------------------------
# 1. constants (ruled 2026-08-14)
# ------------------------------------------------------------

X_QUAL    <- 3
G_MIN     <- 6
SOS_BANDS <- c("DI", "ED", "LB", "S")
N_BAND    <- c(DI = 5L, ED = 5L, LB = 3L, S = 4L)

# ------------------------------------------------------------
# 2. SoS RUN-DEFENSE CURRENCY
#    qualifying games (>= X_QUAL run snaps), modal band by run
#    snaps, snap-weighted season values (count rates = ratio-of-sums),
#    percentiles within band-season. Direction law: higher pctl =
#    better defender -- mt_rt and adot are NEGATED (low is good).
# ------------------------------------------------------------

rundef_qual_games <- run_defense_qbgrp %>%
  filter(position %in% SOS_BANDS, snap_counts_run >= X_QUAL)

rundef_modal_band <- rundef_qual_games %>%
  count(player_id, season, position, wt = snap_counts_run, name = "sn") %>%
  group_by(player_id, season) %>%
  slice_max(sn, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, season, band = position)

rundef_season_pctl_sos <- rundef_qual_games %>%
  inner_join(rundef_modal_band, by = c("player_id", "season")) %>%
  filter(position == band) %>%   # qualifying games at the modal band only
  group_by(player, player_id, band, season) %>%
  summarise(
    q_games   = dplyr::n(),
    q_snaps   = sum(snap_counts_run, na.rm = TRUE),
    q_opps    = sum(run_stop_opp, na.rm = TRUE),
    rd_grade  = weighted.mean(grades_run_defense, w = snap_counts_run, na.rm = TRUE),
    tck_grade = weighted.mean(grades_tackle,      w = snap_counts_run, na.rm = TRUE),
    stop_rt   = sum(stops, na.rm = TRUE) / sum(run_stop_opp, na.rm = TRUE),
    mt_rt     = sum(missed_tackles, na.rm = TRUE) /
      sum(missed_tackles + tackles, na.rm = TRUE),
    adot      = weighted.mean(avg_depth_of_tackle, w = tackles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(q_games >= G_MIN) %>%
  group_by(band, season) %>%
  mutate(
    grade_pctl = percent_rank_avg(rd_grade),
    tck_pctl   = percent_rank_avg(tck_grade),
    stop_pctl  = percent_rank_avg(stop_rt),
    mt_pctl    = percent_rank_avg(-mt_rt),
    adot_pctl  = percent_rank_avg(-adot)
  ) %>%
  ungroup()

cat("\n--- currency validation: gated player-seasons per year ---\n")
print(rundef_season_pctl_sos %>% count(season) %>% arrange(season), n = Inf)
cat("\n--- 2025 pools per band (expect ~ DI 165 / ED 146 / LB 105 / S 105) ---\n")
print(rundef_season_pctl_sos %>% filter(season == 2025) %>% count(band))

# ------------------------------------------------------------
# 2b. ORIENTATION RITUAL -- cor matrix + known-elite eyeball.
#     Pruning rule (prush law): |r| >= ~.85 -> one rides to the
#     table, not the facets. Facet default: grade + stop rate.
# ------------------------------------------------------------

cor_mat <- rundef_season_pctl_sos %>%
  filter(season == 2025) %>%
  select(rd_grade, tck_grade, stop_rt, mt_rt, adot) %>%
  cor(use = "pairwise.complete.obs")
cat("\n--- metric cor matrix, 2025 gated (raw values; mt/adot: LOW = good) ---\n")
print(round(cor_mat, 3))

cat("\n--- known-elite eyeball: top 8 per band by grade_pctl, 2025 ---\n")
print(rundef_season_pctl_sos %>%
        filter(season == 2025) %>%
        group_by(band) %>%
        slice_max(grade_pctl, n = 8, with_ties = FALSE) %>%
        arrange(band, desc(grade_pctl)) %>%
        select(band, player, q_games, q_snaps,
               grade_pctl, stop_pctl, tck_pctl, mt_pctl, adot_pctl),
      n = Inf)
cat("\n--- same, top 8 per band by stop_pctl (the noisy count rate) ---\n")
print(rundef_season_pctl_sos %>%
        filter(season == 2025) %>%
        group_by(band) %>%
        slice_max(stop_pctl, n = 8, with_ties = FALSE) %>%
        arrange(band, desc(stop_pctl)) %>%
        select(band, player, q_games, q_opps, stop_pctl, grade_pctl),
      n = Inf)

# ------------------------------------------------------------
# 3. defense crosswalk + 2026 rosters (front seven + safeties)
#    nflreadr 2026 coarsens defense to DL / LB / DB -- safeties
#    AND corners both live under "DB". The band law sorts them:
#    pure CBs have no SOS-band history and die at the no-history
#    filter; real safeties get band "S" from modal PFF position.
# ------------------------------------------------------------

def_xwalk <- combined_ids_defense %>%
  filter(!is.na(gsis_id)) %>%
  distinct(player_id, gsis_id)

stopifnot(anyDuplicated(def_xwalk$gsis_id)   == 0,
          anyDuplicated(def_xwalk$player_id) == 0)

rundef_roster_pos <- c("DL", "DE", "DT", "NT", "EDGE",
                       "OLB", "LB", "ILB", "MLB",
                       "S", "SS", "FS", "SAF", "DB")

def_2026 <- nflreadr::load_rosters(2026) %>%
  filter(position %in% rundef_roster_pos) %>%
  transmute(gsis_id,
            roster_name = full_name,
            team_name = dplyr::coalesce(pff_team_lookup[team], team),
            entry_year,
            roster_pos = position) %>%
  filter(team_name %in% opp_2026_teams) %>%
  left_join(def_xwalk, by = "gsis_id")

stopifnot(anyDuplicated(def_2026[, c("team_name", "roster_name")]) == 0)

cat("\n--- roster positions pulled (verify nothing weird) ---\n")
print(def_2026 %>% count(roster_pos))

# ------------------------------------------------------------
# 4. usage, band, pctls, status flags
#    band law: modal PFF position from run-D history. Roster
#    strings NEVER assign bands. Rookie bands = tribble calls.
# ------------------------------------------------------------

rundef_usage <- rundef_qual_games %>%
  filter(season %in% c(2024, 2025)) %>%
  group_by(player_id, season) %>%
  summarise(rd_snaps = sum(snap_counts_run),
            qg = dplyr::n(), .groups = "drop") %>%
  pivot_wider(names_from = season, values_from = c(rd_snaps, qg))

band_map <- rundef_modal_band %>% filter(season == 2025) %>%
  select(player_id, band_25 = band) %>%
  full_join(rundef_modal_band %>% filter(season == 2024) %>%
              select(player_id, band_24 = band),
            by = "player_id") %>%
  mutate(band = dplyr::coalesce(band_25, band_24)) %>%
  select(player_id, band)

pctl_25 <- rundef_season_pctl_sos %>% filter(season == 2025) %>%
  select(player_id, grade_25 = grade_pctl, stop_25 = stop_pctl)
pctl_24 <- rundef_season_pctl_sos %>% filter(season == 2024) %>%
  select(player_id, grade_24 = grade_pctl, stop_24 = stop_pctl)

ledger_base <- def_2026 %>%
  left_join(rundef_usage, by = "player_id") %>%
  left_join(band_map,     by = "player_id") %>%
  left_join(pctl_25,      by = "player_id") %>%
  left_join(pctl_24,      by = "player_id") %>%
  mutate(status = case_when(
    entry_year == 2026                          ~ "rookie",
    is.na(player_id)                            ~ "no_pff_id",
    !is.na(grade_25)                            ~ "has_2025_pctl",
    !is.na(grade_24)                            ~ "data_2024_only",
    dplyr::coalesce(rd_snaps_2025, 0) > 0 |
      dplyr::coalesce(rd_snaps_2024, 0) > 0     ~ "usage_no_pctl",
    TRUE                                        ~ "no_rundef_history"
  ))

cat("\n--- no-history players excluded from ledger (per team) ---\n")
print(ledger_base %>% filter(status == "no_rundef_history") %>%
        count(team_name), n = Inf)

ledger <- ledger_base %>% filter(status != "no_rundef_history")

# ------------------------------------------------------------
# 5. rotation proposals: top N_BAND[band] per (team, band) by
#    pmax('25,'24) usage. N per band ruled 2026-08-14.
# ------------------------------------------------------------

ledger <- ledger %>%
  mutate(usage_ord = pmax(dplyr::coalesce(rd_snaps_2025, 0),
                          dplyr::coalesce(rd_snaps_2024, 0))) %>%
  group_by(team_name, band) %>%
  arrange(desc(usage_ord), .by_group = TRUE) %>%
  mutate(band_rank = dplyr::row_number(),
         target_n  = N_BAND[band],
         proposed  = !is.na(band) & band_rank <= target_n) %>%
  ungroup()

# ------------------------------------------------------------
# 6. THE RULING LEDGER
# ------------------------------------------------------------

cat("\n================ STEP 0 -- THE RULING LEDGER ================\n")
cat("proposed = TRUE -> auto-rotation (top N_BAND[band] by '25/'24 usage)\n")
cat("Andy rules ALL deltas via tribble below: adds (rookies/signings),\n")
cat("drops (injuries/cuts). Rookie adds carry a band assignment.\n\n")

ledger_print <- ledger %>%
  arrange(match(team_name, opp_2026_teams), band, desc(usage_ord)) %>%
  select(team_name, band, roster_name, status, proposed,
         sn25 = rd_snaps_2025, sn24 = rd_snaps_2024,
         grade_25, stop_25, grade_24, stop_24)
print(ledger_print, n = Inf)

cat("\n--- calls needed per team ---\n")
print(ledger %>% group_by(team_name) %>%
        summarise(proposed      = sum(proposed),
                  rookies       = sum(status == "rookie"),
                  data_24_only  = sum(status == "data_2024_only"),
                  usage_no_pctl = sum(status == "usage_no_pctl"),
                  no_pff_id     = sum(status == "no_pff_id"),
                  .groups = "drop") %>%
        arrange(match(team_name, opp_2026_teams)), n = Inf)

cat("\n--- 2025 rotation shapes (FYI; membership is uniform N_BAND per band) ---\n")
n_band_2025 <- rundef_qual_games %>%
  filter(season == 2025) %>%
  count(def_ssn, player_id, name = "g") %>%
  filter(g >= G_MIN) %>%
  inner_join(rundef_modal_band %>% filter(season == 2025) %>%
               select(player_id, band),
             by = "player_id") %>%
  count(def_ssn, band, name = "n_2025") %>%
  mutate(team_name = stringr::str_remove(def_ssn, "2025$")) %>%
  filter(team_name %in% opp_2026_teams) %>%
  select(team_name, band, n_2025)
print(n_band_2025 %>%
        pivot_wider(names_from = band, values_from = n_2025) %>%
        arrange(match(team_name, opp_2026_teams)), n = Inf)

# ------------------------------------------------------------
# STOP -- ruling boundary. Deltas happen in section 7.
# ------------------------------------------------------------

# ------------------------------------------------------------
# 7. ANDY'S DELTAS -- the only judgment layer.
#    EMPTY = full sign-off (legal + complete). Actions: add / drop.
#    Phantoms are AUTOMATIC (section 9 fills every band to
#    N_BAND[band] at the entry-year prior). "add" rows for rookies
#    MUST carry band -- roster strings never assign bands.
#    Names must match def_2026 spelling exactly -- the gate fails loudly.
# ------------------------------------------------------------

opp_rundef_2026_deltas <- tribble(
  ~team_name, ~roster_name, ~band, ~action, ~note
  # "KC", "<name>", "ED", "add", "example - rookie into the rotation",
)

d_miss <- opp_rundef_2026_deltas %>%
  anti_join(def_2026, by = c("team_name", "roster_name"))
if (nrow(d_miss) > 0) {
  print(d_miss)
  stop("delta names not on 2026 rosters - fix spelling vs the ledger")
}

cat("\n--- deltas registered:", nrow(opp_rundef_2026_deltas), "row(s) ---\n")
if (nrow(opp_rundef_2026_deltas) > 0) print(opp_rundef_2026_deltas, n = Inf)

# ------------------------------------------------------------
# 8. FACED 2025 SIDE -- observed only, no imputation of the past.
#    Every gated run-defender game vs NE's offense, weighted by
#    that game's run snaps. unscored_share = the honesty number.
# ------------------------------------------------------------

faced_games_2025 <- run_defense_qbgrp %>%
  filter(startsWith(qbgrp_ssn, "NE"), season == 2025, in_season(week),
         position %in% SOS_BANDS, snap_counts_run >= X_QUAL) %>%
  transmute(player_id, player, week, band_row = position,
            g_snaps = snap_counts_run) %>%
  left_join(rundef_season_pctl_sos %>% filter(season == 2025) %>%
              select(player_id, band,
                     g25 = grade_pctl, s25 = stop_pctl),
            by = "player_id") %>%
  mutate(band = dplyr::coalesce(band, band_row))

cat("\n--- faced 2025: weeks present (expect 21 incl playoffs) ---\n")
print(faced_games_2025 %>% distinct(week) %>% arrange(week), n = Inf)

faced_band_2025 <- faced_games_2025 %>%
  group_by(band) %>%
  summarise(defender_games = dplyr::n(),
            tot_snaps      = sum(g_snaps),
            unscored_share = sum(g_snaps[is.na(g25)]) / tot_snaps,
            grade_25 = weighted.mean(g25, w = g_snaps, na.rm = TRUE),
            stop_25  = weighted.mean(s25, w = g_snaps, na.rm = TRUE),
            .groups = "drop")

cat("\n--- faced 2025 band profile (unscored_share = honesty number) ---\n")
print(faced_band_2025)

# ------------------------------------------------------------
# 9. 2026 PROJECTION -- signed rotations + deltas + priors + phantoms.
#    Values: blend2('25,'24 pctl, w25 = q_games25/10). No-pctl members
#    (rookie / under-gate vet / phantom) = band entry-year prior,
#    flagged. Weights: pmax('25,'24 snaps) -> renormalized shares.
# ------------------------------------------------------------

entry_years_def <- nflreadr::load_rosters(2017:2025) %>%
  filter(!is.na(gsis_id), !is.na(entry_year)) %>%
  group_by(gsis_id) %>%
  summarise(entry_year = min(entry_year), .groups = "drop") %>%
  inner_join(def_xwalk, by = "gsis_id") %>%
  select(player_id, entry_year)

rundef_rookie_prior <- rundef_season_pctl_sos %>%
  inner_join(entry_years_def, by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2017) %>%
  group_by(band) %>%
  summarise(pr_grade = median(grade_pctl, na.rm = TRUE),
            pr_stop  = median(stop_pctl,  na.rm = TRUE),
            .groups = "drop")

cat("\n--- entry-year prior by band (values for rookies/phantoms/no-pctl) ---\n")
print(rundef_rookie_prior)

# 2025 slot-usage curve: phantom weights inherit the team's own vacated
# slot; league median at that rank is the fallback
slot_usage_2025 <- rundef_qual_games %>%
  filter(season == 2025) %>%
  group_by(def_ssn, player_id) %>%
  summarise(g = dplyr::n(),
            sn = sum(snap_counts_run), .groups = "drop") %>%
  filter(g >= G_MIN) %>%
  inner_join(rundef_modal_band %>% filter(season == 2025) %>%
               select(player_id, band), by = "player_id") %>%
  mutate(team_name = stringr::str_remove(def_ssn, "2025$")) %>%
  group_by(team_name, band) %>%
  arrange(desc(sn), .by_group = TRUE) %>%
  mutate(slot_rk = dplyr::row_number()) %>%
  ungroup() %>%
  select(team_name, band, slot_rk, sn_slot = sn)

slot_usage_med <- slot_usage_2025 %>%
  group_by(band, slot_rk) %>%
  summarise(sn_med = median(sn_slot), .groups = "drop")
fallback_sn <- min(slot_usage_2025$sn_slot)

# real members: proposals minus drops plus named adds
p25_cur <- rundef_season_pctl_sos %>% filter(season == 2025) %>%
  select(player_id, g25 = grade_pctl, s25 = stop_pctl)
p24_cur <- rundef_season_pctl_sos %>% filter(season == 2024) %>%
  select(player_id, g24 = grade_pctl, s24 = stop_pctl)

adds_prepped <- opp_rundef_2026_deltas %>%
  filter(action == "add") %>%
  left_join(ledger_base %>%
              select(team_name, roster_name, player_id, band_hist = band,
                     rd_snaps_2025, rd_snaps_2024, qg_2025, status),
            by = c("team_name", "roster_name")) %>%
  mutate(band = dplyr::coalesce(band, band_hist)) %>%
  select(team_name, roster_name, player_id, band,
         rd_snaps_2025, rd_snaps_2024, qg_2025, status)

rot_real <- ledger %>%
  filter(proposed) %>%
  select(team_name, roster_name, player_id, band,
         rd_snaps_2025, rd_snaps_2024, qg_2025, status) %>%
  anti_join(opp_rundef_2026_deltas %>% filter(action == "drop"),
            by = c("team_name", "roster_name")) %>%
  bind_rows(adds_prepped)

stopifnot(!any(is.na(rot_real$band)),
          anyDuplicated(rot_real[, c("team_name", "roster_name")]) == 0)

rot_real <- rot_real %>%
  left_join(p25_cur, by = "player_id") %>%
  left_join(p24_cur, by = "player_id") %>%
  left_join(rundef_rookie_prior, by = "band") %>%
  mutate(w25      = pmin(dplyr::coalesce(qg_2025, 0L) / 10, 1),
         grade_bl = blend2(g25, g24, w25),
         stop_bl  = blend2(s25, s24, w25),
         prior_used = is.na(grade_bl),
         grade_f = dplyr::coalesce(grade_bl, pr_grade),
         stop_f  = dplyr::coalesce(stop_bl,  pr_stop),
         usage_w = pmax(dplyr::coalesce(rd_snaps_2025, 0),
                        dplyr::coalesce(rd_snaps_2024, 0))) %>%
  select(team_name, roster_name, band, status, prior_used,
         usage_w, grade_f, stop_f)

# cap at N_BAND[band], then slot-weight inheritance
rot_real <- rot_real %>%
  mutate(usage_w = if_else(usage_w > 0, usage_w, NA_real_)) %>%
  group_by(team_name, band) %>%
  arrange(desc(dplyr::coalesce(usage_w, 0)), .by_group = TRUE) %>%
  mutate(band_rank = dplyr::row_number(),
         target_n  = N_BAND[band]) %>%
  filter(band_rank <= target_n) %>%
  ungroup() %>%
  left_join(slot_usage_2025,
            by = c("team_name", "band", "band_rank" = "slot_rk")) %>%
  left_join(slot_usage_med, by = c("band", "band_rank" = "slot_rk")) %>%
  mutate(usage_w = dplyr::coalesce(usage_w, sn_slot, sn_med, fallback_sn)) %>%
  select(team_name, roster_name, band, status, prior_used,
         usage_w, grade_f, stop_f)

auto_phantoms <- rot_real %>%
  count(team_name, band, name = "n_real") %>%
  tidyr::complete(team_name = opp_2026_teams, band = SOS_BANDS,
                  fill = list(n_real = 0L)) %>%
  mutate(target_n = N_BAND[band]) %>%
  filter(n_real < target_n) %>%
  mutate(slot_rk = purrr::map2(n_real, target_n, ~ seq(.x + 1L, .y))) %>%
  tidyr::unnest(slot_rk) %>%
  left_join(slot_usage_2025, by = c("team_name", "band", "slot_rk")) %>%
  left_join(slot_usage_med,  by = c("band", "slot_rk")) %>%
  left_join(rundef_rookie_prior, by = "band") %>%
  transmute(team_name,
            roster_name = paste0("PHANTOM_", band, slot_rk),
            band, status = "phantom", prior_used = TRUE,
            usage_w = dplyr::coalesce(sn_slot, sn_med, fallback_sn),
            grade_f = pr_grade, stop_f = pr_stop)

rot_2026 <- bind_rows(rot_real, auto_phantoms)

# every (team, band) is exactly N_BAND[band] deep, no exceptions
stopifnot(all(dplyr::count(rot_2026, team_name, band)$n ==
                N_BAND[dplyr::count(rot_2026, team_name, band)$band]),
          nrow(rot_2026) == length(opp_2026_teams) * sum(N_BAND))

cat("\n--- 2026 rotations, final (prior_used = valued at band prior) ---\n")
print(rot_2026 %>%
        arrange(match(team_name, opp_2026_teams), band, desc(usage_w)) %>%
        select(team_name, band, roster_name, status, prior_used, usage_w,
               grade_f, stop_f), n = Inf)

team_band_2026 <- rot_2026 %>%
  group_by(team_name, band) %>%
  summarise(n_members    = dplyr::n(),
            interp_share = sum(usage_w[prior_used]) / sum(usage_w),
            grade_26 = weighted.mean(grade_f, w = usage_w),
            stop_26  = weighted.mean(stop_f,  w = usage_w),
            .groups = "drop")

cat("\n--- team-band 2026 values + interp_share honesty column ---\n")
print(team_band_2026 %>%
        arrange(match(team_name, opp_2026_teams), band), n = Inf)

# schedule-weighted slate (division x2, same as OL/prush Fig 1)
slate_band_2026 <- tibble(team_name = sched_2026) %>%
  left_join(team_band_2026, by = "team_name", relationship = "many-to-many") %>%
  group_by(band) %>%
  summarise(grade_26 = mean(grade_26), stop_26 = mean(stop_26),
            interp_share = mean(interp_share), .groups = "drop")

cmp_rundef_slate <- faced_band_2025 %>%
  select(band, grade_25, stop_25, unscored_share_25 = unscored_share) %>%
  left_join(slate_band_2026, by = "band") %>%
  mutate(d_grade = grade_26 - grade_25,
         d_stop  = stop_26  - stop_25)

cat("\n--- THE ANSWER: 2026 vs 2025 run-defense slate, by band ---\n")
print(cmp_rundef_slate, n = Inf)

# ------------------------------------------------------------
# 10. FIG 1 -- bands x facets (grade + stop rate per default ruling;
#     cor matrix printed in 2b -- Andy can veto a facet). Mirrors
#     the prush Fig 1.
# ------------------------------------------------------------

rundef_fig1_pd <- cmp_rundef_slate %>%
  select(band, grade_25, grade_26, stop_25, stop_26) %>%
  pivot_longer(-band, names_to = c("metric", "yr"),
               names_pattern = "(grade|stop)_(25|26)", values_to = "pctl") %>%
  pivot_wider(names_from = yr, values_from = pctl, names_prefix = "y") %>%
  mutate(band   = factor(band, levels = rev(SOS_BANDS)),
         metric = factor(metric, levels = c("grade", "stop"),
                         labels = c("Run-defense grade", "Stop rate")))

plot_rundef_slate <- ggplot(rundef_fig1_pd, aes(y = band)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey45") +
  geom_segment(aes(x = y25, xend = y26, yend = band),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               linewidth = 1, color = "grey60") +
  geom_point(aes(x = y25), shape = 1, size = 3.2, stroke = 1.2, color = "grey55") +
  geom_point(aes(x = y26), shape = 16, size = 2.6, color = "#002244") +
  facet_wrap(~ metric, nrow = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(title = "NE offense - 2026 vs 2025 opposing run-defense slate, by band",
       subtitle = "open circle = 2025 run defenders faced (snap-weighted) | solid navy = 2026 projected rotations | higher = HARDER for NE's run game | dashed = league median",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey40", size = 8.5),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", size = 9),
        panel.spacing.x = unit(1.1, "lines"))

plot_rundef_slate

# ------------------------------------------------------------
# 11. FIG-1 COMPANION TABLE. interp_share = the honesty column.
#     gt attached HERE, not inherited: save.image restores objects,
#     never library() state.
# ------------------------------------------------------------

library(gt)

rundef_d_dom <- max(abs(c(cmp_rundef_slate$d_grade, cmp_rundef_slate$d_stop)),
                    na.rm = TRUE)

rundef_slate_gt <- cmp_rundef_slate %>%
  transmute(band,
            g25 = grade_25, g26 = grade_26, gd = d_grade,
            s25 = stop_25, s26 = stop_26, sd = d_stop,
            interp_share) %>%
  gt() %>%
  tab_spanner(label = "Run-D grade", columns = c(g25, g26, gd)) %>%
  tab_spanner(label = "Stop rate",   columns = c(s25, s26, sd)) %>%
  cols_label(band = "", g25 = "'25", g26 = "'26", gd = "D",
             s25 = "'25", s26 = "'26", sd = "D",
             interp_share = "interp %") %>%
  fmt_percent(columns = c(g25, g26, s25, s26, interp_share), decimals = 0) %>%
  fmt_percent(columns = c(gd, sd), decimals = 0, force_sign = TRUE) %>%
  data_color(columns = c(gd, sd),
             fn = scales::col_numeric(c("#6baed6", "#f7f7f7", "#C60C30"),
                                      domain = c(-rundef_d_dom, rundef_d_dom)),
             autocolor_text = TRUE) %>%
  tab_header(title = "2026 vs 2025 opposing run-defense slate -- the numbers",
             subtitle = "snap-share-weighted band means, division counted twice | higher = harder for NE's run game | D red = harder | interp % = share of projected snaps riding on interpolated (entry-year prior) values") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold")

rundef_slate_gt

# ------------------------------------------------------------
# STOP 2 -- player panels (v2 grammar) + 3-season table build AFTER
# the Fig-1 eyeball, on request. Checkpoint:
# ------------------------------------------------------------
# ggsave("rundef_fig1_slate.png", plot_rundef_slate, width = 9, height = 4.5, dpi = 200)
# gtsave(rundef_slate_gt, "rundef_slate_table.png", vwidth = 800)
# save.image("~/ne_rundef_sos_workspace.RData")
# system('aws s3 cp ~/ne_rundef_sos_workspace.RData s3://nfl-pff-data-lucas/workspaces/')



# ============================================================
# NE 2026 -- RUN-DEFENSE SoS, PER TEAM (Phase 3 add-on) + PERMANENT NAMES
# Same treatment as phase2b (pass rush): rebuilds the run-D
#   projection chain into *_rundef names, proves the rebuild ==
#   the original build via equality receipt vs live cmp_rundef_slate
#   (1e-8, stops loudly on drift), then rebuilds the per-team table
#   + heatmap on the suffixed objects. After this + phase2b, all
#   three units sit at permanent per-team grain and no generic-name
#   regime can silently lose one again.
#   If phase2b's section-0 snapshot already saved a
#   team_band_2026_rundef, this overwrites it with receipt-verified
#   values -- strictly better.
#
# Run-D receipts (Phase 3 header): gate X=3 run snaps, G=6; bands
#   DI/ED/LB/S; N_BAND = 5/5/3/4; facets grade + stop rate (stop
#   rides with the noise caveat); roster pull includes "DB" (the
#   coarsened-roster S fix); priors by band; usage_ord = pmax.
#
# SOURCE ORDER: all build files already sourced this session.
#   Re-source WHOLE file after any edit.
# ============================================================

library(dplyr); library(tidyr); library(stringr)
library(ggplot2); library(gt)

needed_p3b <- c("run_defense_qbgrp", "rundef_qual_games", "rundef_modal_band",
                "rundef_season_pctl_sos", "rundef_usage", "rundef_rookie_prior",
                "cmp_rundef_slate", "opp_rundef_2026_deltas",
                "combined_ids_defense", "pff_team_lookup", "opp_2026_teams",
                "sched_2026", "blend2", "in_season", "percent_rank_avg")
missing_p3b <- needed_p3b[!vapply(needed_p3b, exists, logical(1))]
if (length(missing_p3b)) stop("missing session objects: ",
                              paste(missing_p3b, collapse = ", "),
                              " -- re-source the Phase 3 build file first")

run_defense_qbgrp      <- tibble::as_tibble(run_defense_qbgrp)
rundef_qual_games      <- tibble::as_tibble(rundef_qual_games)
rundef_modal_band      <- tibble::as_tibble(rundef_modal_band)
rundef_season_pctl_sos <- tibble::as_tibble(rundef_season_pctl_sos)
rundef_usage           <- tibble::as_tibble(rundef_usage)
rundef_rookie_prior    <- tibble::as_tibble(rundef_rookie_prior)
cmp_rundef_slate       <- tibble::as_tibble(cmp_rundef_slate)
combined_ids_defense   <- tibble::as_tibble(combined_ids_defense)

# ------------------------------------------------------------
# 1. RUN-D PROJECTION CHAIN, REBUILT SUFFIXED (mirror of the Phase 3
#    build file sections 3-9; same code path, permanent names).
# ------------------------------------------------------------

X_QUAL_RD <- 3; G_MIN_RD <- 6
SOS_BANDS_RD <- c("DI", "ED", "LB", "S")
N_BAND_RD <- c(DI = 5L, ED = 5L, LB = 3L, S = 4L)

def_xwalk_rundef <- combined_ids_defense %>%
  filter(!is.na(gsis_id)) %>%
  distinct(player_id, gsis_id)
stopifnot(anyDuplicated(def_xwalk_rundef$gsis_id)   == 0,
          anyDuplicated(def_xwalk_rundef$player_id) == 0)

# the S fix: "DB" must be in the pull (coarsened 2026 rosters)
rundef_roster_pos <- c("DL", "DE", "DT", "NT", "EDGE",
                       "OLB", "LB", "ILB", "MLB",
                       "S", "SS", "FS", "SAF", "DB")

def_2026_rundef <- nflreadr::load_rosters(2026) %>%
  filter(position %in% rundef_roster_pos) %>%
  transmute(gsis_id,
            roster_name = full_name,
            team_name = dplyr::coalesce(pff_team_lookup[team], team),
            entry_year,
            roster_pos = position) %>%
  filter(team_name %in% opp_2026_teams) %>%
  left_join(def_xwalk_rundef, by = "gsis_id")

band_map_rundef <- rundef_modal_band %>% filter(season == 2025) %>%
  select(player_id, band_25 = band) %>%
  full_join(rundef_modal_band %>% filter(season == 2024) %>%
              select(player_id, band_24 = band),
            by = "player_id") %>%
  mutate(band = dplyr::coalesce(band_25, band_24)) %>%
  select(player_id, band)

pctl_25_rundef <- rundef_season_pctl_sos %>% filter(season == 2025) %>%
  select(player_id, grade_25 = grade_pctl, stop_25 = stop_pctl)
pctl_24_rundef <- rundef_season_pctl_sos %>% filter(season == 2024) %>%
  select(player_id, grade_24 = grade_pctl, stop_24 = stop_pctl)

status_rundef <- function(df) {
  df %>% mutate(status = case_when(
    entry_year == 2026                          ~ "rookie",
    is.na(player_id)                            ~ "no_pff_id",
    !is.na(grade_25)                            ~ "has_2025_pctl",
    !is.na(grade_24)                            ~ "data_2024_only",
    dplyr::coalesce(rd_snaps_2025, 0) > 0 |
      dplyr::coalesce(rd_snaps_2024, 0) > 0     ~ "usage_no_pctl",
    TRUE                                        ~ "no_rundef_history"
  ))
}

ledger_base_rundef <- def_2026_rundef %>%
  left_join(rundef_usage,   by = "player_id") %>%
  left_join(band_map_rundef, by = "player_id") %>%
  left_join(pctl_25_rundef, by = "player_id") %>%
  left_join(pctl_24_rundef, by = "player_id") %>%
  status_rundef()

ledger_rundef <- ledger_base_rundef %>%
  filter(status != "no_rundef_history") %>%
  mutate(usage_ord = pmax(dplyr::coalesce(rd_snaps_2025, 0),
                          dplyr::coalesce(rd_snaps_2024, 0))) %>%
  group_by(team_name, band) %>%
  arrange(desc(usage_ord), .by_group = TRUE) %>%
  mutate(band_rank = dplyr::row_number(),
         target_n  = N_BAND_RD[band],
         proposed  = !is.na(band) & band_rank <= target_n) %>%
  ungroup()

entry_years_def_rundef <- nflreadr::load_rosters(2017:2025) %>%
  filter(!is.na(gsis_id), !is.na(entry_year)) %>%
  group_by(gsis_id) %>%
  summarise(entry_year = min(entry_year), .groups = "drop") %>%
  inner_join(def_xwalk_rundef, by = "gsis_id") %>%
  select(player_id, entry_year)

slot_usage_2025_rundef <- rundef_qual_games %>%
  filter(season == 2025) %>%
  group_by(def_ssn, player_id) %>%
  summarise(g = dplyr::n(),
            sn = sum(snap_counts_run), .groups = "drop") %>%
  filter(g >= G_MIN_RD) %>%
  inner_join(rundef_modal_band %>% filter(season == 2025) %>%
               select(player_id, band), by = "player_id") %>%
  mutate(team_name = stringr::str_remove(def_ssn, "2025$")) %>%
  group_by(team_name, band) %>%
  arrange(desc(sn), .by_group = TRUE) %>%
  mutate(slot_rk = dplyr::row_number()) %>%
  ungroup() %>%
  select(team_name, band, slot_rk, sn_slot = sn)

slot_usage_med_rundef <- slot_usage_2025_rundef %>%
  group_by(band, slot_rk) %>%
  summarise(sn_med = median(sn_slot), .groups = "drop")
fallback_sn_rundef <- min(slot_usage_2025_rundef$sn_slot)

p25_cur_rundef <- rundef_season_pctl_sos %>% filter(season == 2025) %>%
  select(player_id, g25 = grade_pctl, s25 = stop_pctl)
p24_cur_rundef <- rundef_season_pctl_sos %>% filter(season == 2024) %>%
  select(player_id, g24 = grade_pctl, s24 = stop_pctl)

adds_prepped_rundef <- opp_rundef_2026_deltas %>%
  filter(action == "add") %>%
  left_join(ledger_base_rundef %>%
              select(team_name, roster_name, player_id, band_hist = band,
                     rd_snaps_2025, rd_snaps_2024, qg_2025, status),
            by = c("team_name", "roster_name")) %>%
  mutate(band = dplyr::coalesce(band, band_hist)) %>%
  select(team_name, roster_name, player_id, band,
         rd_snaps_2025, rd_snaps_2024, qg_2025, status)

rot_real_rundef <- ledger_rundef %>%
  filter(proposed) %>%
  select(team_name, roster_name, player_id, band,
         rd_snaps_2025, rd_snaps_2024, qg_2025, status) %>%
  anti_join(opp_rundef_2026_deltas %>% filter(action == "drop"),
            by = c("team_name", "roster_name")) %>%
  bind_rows(adds_prepped_rundef)

stopifnot(!any(is.na(rot_real_rundef$band)),
          anyDuplicated(rot_real_rundef[, c("team_name", "roster_name")]) == 0)

rot_real_rundef <- rot_real_rundef %>%
  left_join(p25_cur_rundef, by = "player_id") %>%
  left_join(p24_cur_rundef, by = "player_id") %>%
  left_join(rundef_rookie_prior, by = "band") %>%
  mutate(w25      = pmin(dplyr::coalesce(qg_2025, 0L) / 10, 1),
         grade_bl = blend2(g25, g24, w25),
         stop_bl  = blend2(s25, s24, w25),
         prior_used = is.na(grade_bl),
         grade_f = dplyr::coalesce(grade_bl, pr_grade),
         stop_f  = dplyr::coalesce(stop_bl,  pr_stop),
         usage_w = pmax(dplyr::coalesce(rd_snaps_2025, 0),
                        dplyr::coalesce(rd_snaps_2024, 0))) %>%
  select(team_name, roster_name, band, status, prior_used,
         usage_w, grade_f, stop_f)

rot_real_rundef <- rot_real_rundef %>%
  mutate(usage_w = if_else(usage_w > 0, usage_w, NA_real_)) %>%
  group_by(team_name, band) %>%
  arrange(desc(dplyr::coalesce(usage_w, 0)), .by_group = TRUE) %>%
  mutate(band_rank = dplyr::row_number(),
         target_n  = N_BAND_RD[band]) %>%
  filter(band_rank <= target_n) %>%
  ungroup() %>%
  left_join(slot_usage_2025_rundef,
            by = c("team_name", "band", "band_rank" = "slot_rk")) %>%
  left_join(slot_usage_med_rundef, by = c("band", "band_rank" = "slot_rk")) %>%
  mutate(usage_w = dplyr::coalesce(usage_w, sn_slot, sn_med,
                                   fallback_sn_rundef)) %>%
  select(team_name, roster_name, band, status, prior_used,
         usage_w, grade_f, stop_f)

auto_phantoms_rundef <- rot_real_rundef %>%
  count(team_name, band, name = "n_real") %>%
  tidyr::complete(team_name = opp_2026_teams, band = SOS_BANDS_RD,
                  fill = list(n_real = 0L)) %>%
  mutate(target_n = N_BAND_RD[band]) %>%
  filter(n_real < target_n) %>%
  mutate(slot_rk = purrr::map2(n_real, target_n, ~ seq(.x + 1L, .y))) %>%
  tidyr::unnest(slot_rk) %>%
  left_join(slot_usage_2025_rundef, by = c("team_name", "band", "slot_rk")) %>%
  left_join(slot_usage_med_rundef,  by = c("band", "slot_rk")) %>%
  left_join(rundef_rookie_prior, by = "band") %>%
  transmute(team_name,
            roster_name = paste0("PHANTOM_", band, slot_rk),
            band, status = "phantom", prior_used = TRUE,
            usage_w = dplyr::coalesce(sn_slot, sn_med, fallback_sn_rundef),
            grade_f = pr_grade, stop_f = pr_stop)

rot_2026_rundef <- bind_rows(rot_real_rundef, auto_phantoms_rundef)

stopifnot(all(dplyr::count(rot_2026_rundef, team_name, band)$n ==
                N_BAND_RD[dplyr::count(rot_2026_rundef, team_name, band)$band]),
          nrow(rot_2026_rundef) == length(opp_2026_teams) * sum(N_BAND_RD))

team_band_2026_rundef <- rot_2026_rundef %>%
  group_by(team_name, band) %>%
  summarise(n_members    = dplyr::n(),
            interp_share = sum(usage_w[prior_used]) / sum(usage_w),
            grade_26 = weighted.mean(grade_f, w = usage_w),
            stop_26  = weighted.mean(stop_f,  w = usage_w),
            .groups = "drop")

# ------------------------------------------------------------
# 2. THE RECEIPT: rebuilt slate must EQUAL the live cmp_rundef_slate.
# ------------------------------------------------------------

slate_check_rd <- tibble(team_name = sched_2026) %>%
  left_join(team_band_2026_rundef, by = "team_name",
            relationship = "many-to-many") %>%
  group_by(band) %>%
  summarise(grade_26 = mean(grade_26), stop_26 = mean(stop_26),
            .groups = "drop") %>%
  arrange(band)

cmp_check_rd <- cmp_rundef_slate %>%
  select(band, grade_26, stop_26) %>% arrange(band)

cat("\n--- EQUALITY RECEIPT: rebuilt vs live cmp_rundef_slate ---\n")
print(slate_check_rd); print(cmp_check_rd)
stopifnot(isTRUE(all.equal(slate_check_rd$grade_26, cmp_check_rd$grade_26,
                           tolerance = 1e-8)),
          isTRUE(all.equal(slate_check_rd$stop_26, cmp_check_rd$stop_26,
                           tolerance = 1e-8)))
cat("receipt holds: rebuild == original build (1e-8).\n")

# ------------------------------------------------------------
# 3. FACED 2025, PER TEAM x BAND -- observed only, run-D gate
#    literals (3 run snaps / 6 games), snap-weighted.
# ------------------------------------------------------------

faced_rundef_team_2025 <- run_defense_qbgrp %>%
  filter(startsWith(qbgrp_ssn, "NE"), season == 2025, in_season(week),
         position %in% SOS_BANDS_RD, snap_counts_run >= X_QUAL_RD) %>%
  transmute(player_id,
            team_name = stringr::str_remove(def_ssn, "2025$"),
            band_row = position, g_snaps = snap_counts_run) %>%
  left_join(rundef_season_pctl_sos %>% filter(season == 2025) %>%
              select(player_id, band, g25 = grade_pctl, s25 = stop_pctl),
            by = "player_id") %>%
  mutate(band = dplyr::coalesce(band, band_row)) %>%
  group_by(team_name, band) %>%
  summarise(tot_snaps = sum(g_snaps),
            unscored_share = sum(g_snaps[is.na(g25)]) / tot_snaps,
            grade_25 = if (all(is.na(g25))) NA_real_
            else weighted.mean(g25, w = g_snaps, na.rm = TRUE),
            stop_25  = if (all(is.na(s25))) NA_real_
            else weighted.mean(s25, w = g_snaps, na.rm = TRUE),
            .groups = "drop")

rundef_team_cmp <- team_band_2026_rundef %>%
  left_join(faced_rundef_team_2025 %>%
              select(team_name, band, grade_25, stop_25, unscored_share),
            by = c("team_name", "band")) %>%
  mutate(d_grade = grade_26 - grade_25,
         d_stop  = stop_26  - stop_25) %>%
  group_by(team_name) %>%
  mutate(team_d = mean(d_grade, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(team_d), match(band, SOS_BANDS_RD))

cat("\n--- RUN-D PER TEAM x BAND (sorted by mean grade delta) ---\n")
print(rundef_team_cmp %>% select(team_name, band, grade_25, grade_26, d_grade,
                                 stop_25, stop_26, d_stop, unscored_share,
                                 interp_share), n = Inf)

# ------------------------------------------------------------
# 4. GT TABLE (grouped by team) + HEATMAP (team x band, delta fill)
# ------------------------------------------------------------

rundef_d_dom_team <- max(abs(c(rundef_team_cmp$d_grade, rundef_team_cmp$d_stop)),
                         na.rm = TRUE)

rundef_team_gt <- rundef_team_cmp %>%
  transmute(team_name, band,
            g25 = grade_25, g26 = grade_26, gd = d_grade,
            s25 = stop_25, s26 = stop_26, sd = d_stop,
            interp_share) %>%
  gt(groupname_col = "team_name") %>%
  tab_spanner(label = "Run-D grade", columns = c(g25, g26, gd)) %>%
  tab_spanner(label = "Stop rate",   columns = c(s25, s26, sd)) %>%
  cols_label(band = "", g25 = "'25", g26 = "'26", gd = "D",
             s25 = "'25", s26 = "'26", sd = "D",
             interp_share = "interp %") %>%
  fmt_percent(columns = c(g25, g26, s25, s26, interp_share), decimals = 0) %>%
  fmt_percent(columns = c(gd, sd), decimals = 0, force_sign = TRUE) %>%
  sub_missing(columns = everything(), missing_text = "--") %>%
  data_color(columns = c(gd, sd),
             fn = scales::col_numeric(c("#6baed6", "#f7f7f7", "#C60C30"),
                                      domain = c(-rundef_d_dom_team,
                                                 rundef_d_dom_team)),
             autocolor_text = TRUE) %>%
  tab_header(title = "2026 vs 2025 opposing run defense -- per team, per band",
             subtitle = "snap-share-weighted band means | higher = harder for NE's run game | D red = harder | '--' = not faced in 2025 | interp % = share of projected snaps on entry-year priors | teams sorted by mean grade delta") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold")

rundef_team_gt

rundef_hm_pd <- rundef_team_cmp %>%
  select(team_name, band, d_grade, d_stop, team_d) %>%
  pivot_longer(c(d_grade, d_stop), names_to = "metric", values_to = "delta") %>%
  mutate(team_name = factor(team_name,
                            levels = rundef_team_cmp %>% arrange(team_d) %>%
                              distinct(team_name) %>% pull(team_name)),
         band   = factor(band, levels = SOS_BANDS_RD),
         metric = factor(metric, levels = c("d_grade", "d_stop"),
                         labels = c("Run-D grade delta", "Stop rate delta")),
         lab = if_else(is.na(delta), "--",
                       scales::percent(delta, accuracy = 1)))

plot_rundef_team_hm <- ggplot(rundef_hm_pd, aes(x = band, y = team_name,
                                                fill = delta)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = lab), size = 2.8) +
  facet_wrap(~ metric, nrow = 1) +
  scale_fill_gradient2(low = "#6baed6", mid = "#f7f7f7", high = "#C60C30",
                       midpoint = 0,
                       limits = c(-rundef_d_dom_team, rundef_d_dom_team),
                       na.value = "grey88", labels = scales::percent_format()) +
  labs(title = "Run defense, per team x band -- 2026 vs 2025 delta",
       subtitle = "red = harder for NE's run game in 2026 | '--' = band not faced in 2025 | teams ordered by mean grade delta",
       x = NULL, y = NULL, fill = "delta") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey40", size = 8.5),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 9),
        legend.position = "right")

plot_rundef_team_hm

# ------------------------------------------------------------
# Checkpoint (after eyeball). Permanent per-unit objects now live:
#   team_band_2026_rundef, rot_2026_rundef, faced_rundef_team_2025,
#   rundef_team_cmp (+ prush + rb sets from phase2b / phase4 build).
# ------------------------------------------------------------
# ggsave("rundef_perteam_heatmap.png", plot_rundef_team_hm, width = 10, height = 5.5, dpi = 200)
# gtsave(rundef_team_gt, "rundef_perteam_table.png", vwidth = 900)
# save.image("~/ne_rundef_sos_workspace.RData")
# system('aws s3 cp ~/ne_rundef_sos_workspace.RData s3://nfl-pff-data-lucas/workspaces/')