# ============================================================
# NE 2026 — OPPONENT PASS-RUSH SoS — BUILD FILE (Phase 2)
# STEP 0 shipped 2026-08-08e: sections 1-6 = THE RULING LEDGER.
# Slate + figures sections land AFTER Andy's deltas tribble.
# Filename provisional — Andy renames per canon.
#
# SOURCE ORDER:
#   (1) prush pipeline objects in session (pff_stats/pff_pass_rush_AWS.R)
#   (2) new_england_opp_ol_schedule.R  (opp_2026_teams, pff_team_lookup
#       via data_build/pff_ids_validate_cross.R, percent_rank_avg)
#   (3) this file, top to bottom. Re-source WHOLE file after any edit.
#
# Direction: opposing RUSHERS vs NE's PROTECTION.
#   higher pctl = better rusher = HARDER for Maye's pocket.
# ============================================================

needed_p2 <- c("full_pass_rush_qbgrp", "combined_ids_defense",
               "pff_team_lookup", "opp_2026_teams", "percent_rank_avg",
               "blend2", "in_season", "sched_2026")
missing_p2 <- needed_p2[!vapply(needed_p2, exists, logical(1))]
if (length(missing_p2)) stop("missing session objects: ",
                             paste(missing_p2, collapse = ", "),
                             " — see SOURCE ORDER in header")

# ------------------------------------------------------------
# 1. constants (rulings 2026-08-08d)
# ------------------------------------------------------------

X_QUAL    <- 3            # qualifying game = >= X tps rush snaps
G_MIN     <- 6            # gate = >= G qualifying games (one game, one vote)
SOS_BANDS <- c("ED", "DI")

# RAQ returns base data.frames (read.csv); print(n=) crashes on them.
# Stopgap until session_helpers RAQ lands (idempotent):
full_pass_rush_qbgrp <- tibble::as_tibble(full_pass_rush_qbgrp)

# ------------------------------------------------------------
# 2. SoS TPS currency — FINAL (receipts 2026-08-08d/e)
# Gate: >= G_MIN games of >= X_QUAL tps rush snaps. X=3/G=6: 14-opponent
#   ledger eyeballed by name + approved; league mean 7.4/team, floor 5;
#   SEA 3DI+4ED = 7/7; NE = 9; KC = 6 (2 DI = real structure).
#   Card's (11,7) rejected: Reed g11=6 on 149 snaps.
# Metrics: snap-weighted over qualifying games (count rates = ratio-of-sums).
#   Noise: talent_sd .041-.048; luck_p10 .044 -> count rates ~parity noise;
#   grade anchors (every-snap scoring). Facets default grade + PRP
#   (r_grade_win = .857 -> win rate to the table; Andy can veto).
# Validated 08e: 226-243 player-seasons/yr 2016-2025; 2025 pools DI 118 /
#   ED 116; NE scored 10 (was 4); "only 1 in 2024" dead (235 in 2024).
# Card table + its gates UNTOUCHED.
# ------------------------------------------------------------

prush_qual_games <- full_pass_rush_qbgrp %>%
  filter(position %in% SOS_BANDS,
         true_pass_set_snap_counts_pass_rush >= X_QUAL)

prush_modal_band <- prush_qual_games %>%
  count(player_id, season, position,
        wt = true_pass_set_snap_counts_pass_rush, name = "sn") %>%
  group_by(player_id, season) %>%
  slice_max(sn, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, season, band = position)

prush_tps_season_pctl_sos <- prush_qual_games %>%
  inner_join(prush_modal_band, by = c("player_id", "season")) %>%
  group_by(player, player_id, band, season) %>%
  summarise(
    q_games = dplyr::n(),
    q_snaps = sum(true_pass_set_snap_counts_pass_rush),
    tps_grade = weighted.mean(true_pass_set_grades_pass_rush_defense,
                              w = true_pass_set_snap_counts_pass_rush, na.rm = TRUE),
    tps_win   = weighted.mean(true_pass_set_pass_rush_win_rate,
                              w = true_pass_set_snap_counts_pass_rush, na.rm = TRUE),
    tps_prp   = weighted.mean(true_pass_set_prp,
                              w = true_pass_set_snap_counts_pass_rush, na.rm = TRUE),
    tps_pressure_rate = sum(true_pass_set_total_pressures, na.rm = TRUE) / q_snaps,
    tps_hurry_rate    = sum(true_pass_set_hurries, na.rm = TRUE) / q_snaps,
    .groups = "drop"
  ) %>%
  filter(q_games >= G_MIN) %>%
  group_by(band, season) %>%
  mutate(
    tps_grade_sos_pctl    = percent_rank_avg(tps_grade),
    tps_win_sos_pctl      = percent_rank_avg(tps_win),
    tps_prp_sos_pctl      = percent_rank_avg(tps_prp),
    tps_pressure_sos_pctl = percent_rank_avg(tps_pressure_rate),
    tps_hurry_sos_pctl    = percent_rank_avg(tps_hurry_rate)
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 3. defense crosswalk (player grain) + 2026 front rosters
#    xwalk receipts 08e: 8 NA-gsis players dropped, 0 collisions
# ------------------------------------------------------------

def_xwalk <- combined_ids_defense %>%
  filter(!is.na(gsis_id)) %>%
  distinct(player_id, gsis_id)

stopifnot(anyDuplicated(def_xwalk$gsis_id)   == 0,
          anyDuplicated(def_xwalk$player_id) == 0)

front_roster_pos <- c("DL", "DE", "DT", "NT", "EDGE", "OLB", "LB", "ILB", "MLB")

def_2026 <- nflreadr::load_rosters(2026) %>%
  filter(position %in% front_roster_pos) %>%
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
#    band law: modal PFF position from prush history; roster strings
#    NEVER assign bands (det_position 99.5% NA for rushers — tested 08e).
#    Rookie bands = tribble-assigned by Andy.
# ------------------------------------------------------------

prush_usage <- prush_qual_games %>%
  filter(season %in% c(2024, 2025)) %>%
  group_by(player_id, season) %>%
  summarise(tps_snaps = sum(true_pass_set_snap_counts_pass_rush),
            qg = dplyr::n(), .groups = "drop") %>%
  pivot_wider(names_from = season, values_from = c(tps_snaps, qg))

band_map <- prush_modal_band %>% filter(season == 2025) %>%
  select(player_id, band_25 = band) %>%
  full_join(prush_modal_band %>% filter(season == 2024) %>%
              select(player_id, band_24 = band),
            by = "player_id") %>%
  mutate(band = dplyr::coalesce(band_25, band_24)) %>%
  select(player_id, band)

pctl_25 <- prush_tps_season_pctl_sos %>% filter(season == 2025) %>%
  select(player_id, grade_25 = tps_grade_sos_pctl, prp_25 = tps_prp_sos_pctl)
pctl_24 <- prush_tps_season_pctl_sos %>% filter(season == 2024) %>%
  select(player_id, grade_24 = tps_grade_sos_pctl, prp_24 = tps_prp_sos_pctl)

ledger_base <- def_2026 %>%
  left_join(prush_usage, by = "player_id") %>%
  left_join(band_map,    by = "player_id") %>%
  left_join(pctl_25,     by = "player_id") %>%
  left_join(pctl_24,     by = "player_id") %>%
  mutate(status = case_when(
    entry_year == 2026                         ~ "rookie",
    is.na(player_id)                           ~ "no_pff_id",
    !is.na(grade_25)                           ~ "has_2025_pctl",
    !is.na(grade_24)                           ~ "data_2024_only",
    dplyr::coalesce(tps_snaps_2025, 0) > 0 |
      dplyr::coalesce(tps_snaps_2024, 0) > 0   ~ "usage_no_pctl",
    TRUE                                       ~ "no_prush_history"
  ))

cat("\n--- off-ball / no-history vets excluded from ledger (per team) ---\n")
print(ledger_base %>% filter(status == "no_prush_history") %>%
        count(team_name), n = Inf)

ledger <- ledger_base %>% filter(status != "no_prush_history")

# ------------------------------------------------------------
# 5. rotation-size proposal: n per (team, band) = that team's OWN 2025
#    gated-rotation count. 2026 proposal = top-n of the band by usage.
#    Rookies have no usage -> never auto-proposed -> Andy tribble-adds.
# ------------------------------------------------------------

n_band_2025 <- prush_qual_games %>%
  filter(season == 2025) %>%
  count(def_ssn, player_id, name = "g") %>%
  filter(g >= G_MIN) %>%
  inner_join(prush_modal_band %>% filter(season == 2025) %>%
               select(player_id, band),
             by = "player_id") %>%
  count(def_ssn, band, name = "n_2025") %>%
  mutate(team_name = stringr::str_remove(def_ssn, "2025$")) %>%
  filter(team_name %in% opp_2026_teams) %>%
  select(team_name, band, n_2025)

stopifnot(nrow(n_band_2025) == length(opp_2026_teams) * 2)

# usage_ord = pmax of the two seasons, NOT coalesce. Found on first ledger
# run: coalesce buried Ed Oliver (37 injury snaps '25 vs 211 + .897 grade
# '24) below DeWayne Carter — the AVT/injury-return class. pmax surfaces
# returners for Andy's eyeball; benched-role inflation is exactly what the
# tribble exists to catch. Seven proposals flip vs coalesce; one matters
# (Oliver), six are fringe coin-flips (Sweat/Onyemata, B.Jones/Hemingway,
# Cox/Mosby, Wingo/Tufele, Nowaske/Turner, Loudermilk/Taimani).
# N_BAND ruling (Andy 08g): uniform top-4 per band, ALL teams. Per-team
# shape inheritance retired — it assumed scheme continuity and DC churn
# breaks that yearly ("we're trying to overfit"). DEN's 5th DI capped
# ("fuck it"). Shortfalls auto-phantom in section 9 at band prior with
# slot-rank weights — the KC ED4 / MIN ED3 (Chambliss-class) hand rows
# from 08f, generalized into a rule. n_band_2025 stays as an FYI print.
N_BAND <- 4

ledger <- ledger %>%
  mutate(usage_ord = pmax(dplyr::coalesce(tps_snaps_2025, 0),
                          dplyr::coalesce(tps_snaps_2024, 0))) %>%
  group_by(team_name, band) %>%
  arrange(desc(usage_ord), .by_group = TRUE) %>%
  mutate(band_rank = dplyr::row_number(),
         proposed  = !is.na(band) & band_rank <= N_BAND) %>%
  ungroup()

# ------------------------------------------------------------
# 6. THE RULING LEDGER
# ------------------------------------------------------------

cat("\n================ STEP 0 — THE RULING LEDGER ================\n")
cat("proposed = TRUE -> auto-rotation (top N_BAND = 4 of the band by '25/'24 usage)\n")
cat("Andy rules ALL deltas via tribble: adds (rookies/signings), drops\n")
cat("(injuries/cuts), share overrides, rookie band assignments.\n\n")

ledger_print <- ledger %>%
  arrange(match(team_name, opp_2026_teams), band, desc(usage_ord)) %>%
  # band = PFF modal position, THE position column (Andy 08f). nflverse
  # roster_pos (DL/LB only) dropped — its lone residual use is a weak hint
  # on rookie rows, and rookie bands are tribble calls by law.
  select(team_name, band, roster_name, status, proposed,
         sn25 = tps_snaps_2025, sn24 = tps_snaps_2024,
         grade_25, prp_25, grade_24, prp_24)
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

cat("\n--- 2025 rotation shapes (FYI only - membership is uniform N_BAND = 4) ---\n")
print(n_band_2025 %>%
        pivot_wider(names_from = band, values_from = n_2025) %>%
        arrange(match(team_name, opp_2026_teams)), n = Inf)

# ------------------------------------------------------------
# STOP — 2026-08-08e boundary. Rulings happen in section 7 below.
# ------------------------------------------------------------

# ------------------------------------------------------------
# 7. ANDY'S DELTAS — the only judgment layer.
#    EMPTY = full sign-off (legal + complete). Actions: "add" / "drop".
#    Phantoms are AUTOMATIC as of 08g (section 9 fills every band to
#    N_BAND at the entry-year prior) — no phantom rows needed.
#    08f notes preserved: KC ED — Gillotte likely ED3, 4th body unknown;
#    MIN ED — Chambliss-class rookie most likely (name not yet rostered).
#    Both now covered by auto-fill; upgrade to a named "add" when real.
#    Names must match def_2026 spelling exactly — the gate fails loudly.
# ------------------------------------------------------------

opp_prush_2026_deltas <- tribble(
  ~team_name, ~roster_name, ~band, ~action, ~note
  # "MIN", "<name>", "ED", "add", "example - rookie into the rotation",
)

d_miss <- opp_prush_2026_deltas %>%
  anti_join(def_2026, by = c("team_name", "roster_name"))
if (nrow(d_miss) > 0) {
  print(d_miss)
  stop("delta names not on 2026 rosters - fix spelling vs the ledger")
}

cat("\n--- deltas registered:", nrow(opp_prush_2026_deltas), "row(s) ---\n")
if (nrow(opp_prush_2026_deltas) > 0) print(opp_prush_2026_deltas, n = Inf)

# ------------------------------------------------------------
# 8. FACED 2025 SIDE — observed only, no imputation of the past.
#    Every gated rusher-game vs NE's offense, weighted by that game's
#    TPS rush snaps. Unscored share printed as the honesty number.
# ------------------------------------------------------------

faced_games_2025 <- full_pass_rush_qbgrp %>%
  filter(startsWith(qbgrp_ssn, "NE"), season == 2025, in_season(week),
         position %in% SOS_BANDS,
         true_pass_set_snap_counts_pass_rush >= X_QUAL) %>%
  transmute(player_id, player, week, band_row = position,
            g_snaps = true_pass_set_snap_counts_pass_rush) %>%
  left_join(prush_tps_season_pctl_sos %>% filter(season == 2025) %>%
              select(player_id, band,
                     g25 = tps_grade_sos_pctl, p25 = tps_prp_sos_pctl,
                     w25v = tps_win_sos_pctl),
            by = "player_id") %>%
  mutate(band = dplyr::coalesce(band, band_row))

cat("\n--- faced 2025: weeks present (expect 21 incl. playoffs) ---\n")
print(faced_games_2025 %>% distinct(week) %>% arrange(week), n = Inf)

faced_band_2025 <- faced_games_2025 %>%
  group_by(band) %>%
  summarise(rusher_games   = dplyr::n(),
            tot_snaps      = sum(g_snaps),
            unscored_share = sum(g_snaps[is.na(g25)]) / tot_snaps,
            grade_25 = weighted.mean(g25,  w = g_snaps, na.rm = TRUE),
            prp_25   = weighted.mean(p25,  w = g_snaps, na.rm = TRUE),
            win_25   = weighted.mean(w25v, w = g_snaps, na.rm = TRUE),
            .groups = "drop")

cat("\n--- faced 2025 band profile (unscored_share = honesty number) ---\n")
print(faced_band_2025)

# ------------------------------------------------------------
# 9. 2026 PROJECTION — signed rotations + deltas + priors + phantoms.
#    Values: blend2('25,'24 pctl, w25 = q_games25/10). No-pctl members
#    (rookie / under-gate vet / phantom) = band entry-year prior, flagged
#    (Andy's median-interpolate ruling, 08f). Weights: pmax('25,'24 snaps)
#    -> renormalized shares within (team, band).
# ------------------------------------------------------------

entry_years_def <- nflreadr::load_rosters(2017:2025) %>%
  filter(!is.na(gsis_id), !is.na(entry_year)) %>%
  group_by(gsis_id) %>%
  summarise(entry_year = min(entry_year), .groups = "drop") %>%
  inner_join(def_xwalk, by = "gsis_id") %>%
  select(player_id, entry_year)

prush_rookie_prior <- prush_tps_season_pctl_sos %>%
  inner_join(entry_years_def, by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2017) %>%
  group_by(band) %>%
  summarise(pr_grade = median(tps_grade_sos_pctl, na.rm = TRUE),
            pr_prp   = median(tps_prp_sos_pctl,   na.rm = TRUE),
            pr_win   = median(tps_win_sos_pctl,   na.rm = TRUE),
            .groups = "drop")

cat("\n--- entry-year prior by band (values for rookies/phantoms/no-pctl) ---\n")
print(prush_rookie_prior)

# 2025 slot-usage curve: phantom weights inherit the team's own vacated
# slot; league median at that rank is the fallback
slot_usage_2025 <- prush_qual_games %>%
  filter(season == 2025) %>%
  group_by(def_ssn, player_id) %>%
  summarise(g = dplyr::n(),
            sn = sum(true_pass_set_snap_counts_pass_rush), .groups = "drop") %>%
  filter(g >= G_MIN) %>%
  inner_join(prush_modal_band %>% filter(season == 2025) %>%
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
p25_cur <- prush_tps_season_pctl_sos %>% filter(season == 2025) %>%
  select(player_id, g25 = tps_grade_sos_pctl, p25 = tps_prp_sos_pctl,
         w25v = tps_win_sos_pctl)
p24_cur <- prush_tps_season_pctl_sos %>% filter(season == 2024) %>%
  select(player_id, g24 = tps_grade_sos_pctl, p24 = tps_prp_sos_pctl,
         w24v = tps_win_sos_pctl)

adds_prepped <- opp_prush_2026_deltas %>%
  filter(action == "add") %>%
  left_join(ledger_base %>%
              select(team_name, roster_name, player_id, band_hist = band,
                     tps_snaps_2025, tps_snaps_2024, qg_2025, status),
            by = c("team_name", "roster_name")) %>%
  mutate(band = dplyr::coalesce(band, band_hist)) %>%
  select(team_name, roster_name, player_id, band,
         tps_snaps_2025, tps_snaps_2024, qg_2025, status)

rot_real <- ledger %>%
  filter(proposed) %>%
  select(team_name, roster_name, player_id, band,
         tps_snaps_2025, tps_snaps_2024, qg_2025, status) %>%
  anti_join(opp_prush_2026_deltas %>% filter(action == "drop"),
            by = c("team_name", "roster_name")) %>%
  bind_rows(adds_prepped)

stopifnot(!any(is.na(rot_real$band)),
          anyDuplicated(rot_real[, c("team_name", "roster_name")]) == 0)

rot_real <- rot_real %>%
  left_join(p25_cur, by = "player_id") %>%
  left_join(p24_cur, by = "player_id") %>%
  left_join(prush_rookie_prior, by = "band") %>%
  mutate(w25      = pmin(dplyr::coalesce(qg_2025, 0L) / 10, 1),
         grade_bl = blend2(g25,  g24,  w25),
         prp_bl   = blend2(p25,  p24,  w25),
         win_bl   = blend2(w25v, w24v, w25),
         prior_used = is.na(grade_bl),
         grade_f = dplyr::coalesce(grade_bl, pr_grade),
         prp_f   = dplyr::coalesce(prp_bl,   pr_prp),
         win_f   = dplyr::coalesce(win_bl,   pr_win),
         usage_w = pmax(dplyr::coalesce(tps_snaps_2025, 0),
                        dplyr::coalesce(tps_snaps_2024, 0))) %>%
  select(team_name, roster_name, band, status, prior_used,
         usage_w, grade_f, prp_f, win_f)

# 08g: enforce uniform bands. Cap overages at N_BAND (lowest-usage guys
# out, DEN's 5th DI included), then AUTO-PHANTOM every shortfall — each
# missing slot is an unnamed job at the band entry-year prior, weighted
# by that team's own 2025 usage at that slot rank (league-median rank
# usage fallback, then the global floor). Named adds compete on usage
# like everyone else; a zero-usage rookie add inherits its slot weight.
rot_real <- rot_real %>%
  mutate(usage_w = if_else(usage_w > 0, usage_w, NA_real_)) %>%
  group_by(team_name, band) %>%
  arrange(desc(dplyr::coalesce(usage_w, 0)), .by_group = TRUE) %>%
  mutate(band_rank = dplyr::row_number()) %>%
  filter(band_rank <= N_BAND) %>%
  ungroup() %>%
  left_join(slot_usage_2025,
            by = c("team_name", "band", "band_rank" = "slot_rk")) %>%
  left_join(slot_usage_med, by = c("band", "band_rank" = "slot_rk")) %>%
  mutate(usage_w = dplyr::coalesce(usage_w, sn_slot, sn_med, fallback_sn)) %>%
  select(team_name, roster_name, band, status, prior_used,
         usage_w, grade_f, prp_f, win_f)

auto_phantoms <- rot_real %>%
  count(team_name, band, name = "n_real") %>%
  tidyr::complete(team_name = opp_2026_teams, band = SOS_BANDS,
                  fill = list(n_real = 0L)) %>%
  filter(n_real < N_BAND) %>%
  mutate(slot_rk = purrr::map2(n_real, N_BAND, ~ seq(.x + 1L, .y))) %>%
  tidyr::unnest(slot_rk) %>%
  left_join(slot_usage_2025, by = c("team_name", "band", "slot_rk")) %>%
  left_join(slot_usage_med,  by = c("band", "slot_rk")) %>%
  left_join(prush_rookie_prior, by = "band") %>%
  transmute(team_name,
            roster_name = paste0("PHANTOM_", band, slot_rk),
            band, status = "phantom", prior_used = TRUE,
            usage_w = dplyr::coalesce(sn_slot, sn_med, fallback_sn),
            grade_f = pr_grade, prp_f = pr_prp, win_f = pr_win)

rot_2026 <- bind_rows(rot_real, auto_phantoms)

# every band is exactly N_BAND deep, no exceptions
stopifnot(all(dplyr::count(rot_2026, team_name, band)$n == N_BAND),
          nrow(rot_2026) == length(opp_2026_teams) * 2 * N_BAND)

cat("\n--- 2026 rotations, final (prior_used = valued at band prior) ---\n")
print(rot_2026 %>%
        arrange(match(team_name, opp_2026_teams), band, desc(usage_w)) %>%
        select(team_name, band, roster_name, status, prior_used, usage_w,
               grade_f, prp_f), n = Inf)

team_band_2026 <- rot_2026 %>%
  group_by(team_name, band) %>%
  summarise(n_members    = dplyr::n(),
            interp_share = sum(usage_w[prior_used]) / sum(usage_w),
            grade_26 = weighted.mean(grade_f, w = usage_w),
            prp_26   = weighted.mean(prp_f,   w = usage_w),
            win_26   = weighted.mean(win_f,   w = usage_w),
            .groups = "drop")

cat("\n--- team-band 2026 values + interp_share honesty column ---\n")
print(team_band_2026 %>%
        arrange(match(team_name, opp_2026_teams), band), n = Inf)

# schedule-weighted slate (division x2, same as OL Fig 1)
slate_band_2026 <- tibble(team_name = sched_2026) %>%
  left_join(team_band_2026, by = "team_name", relationship = "many-to-many") %>%
  group_by(band) %>%
  summarise(grade_26 = mean(grade_26), prp_26 = mean(prp_26),
            win_26 = mean(win_26),
            interp_share = mean(interp_share), .groups = "drop")

cmp_prush_slate <- faced_band_2025 %>%
  select(band, grade_25, prp_25, win_25, unscored_share_25 = unscored_share) %>%
  left_join(slate_band_2026, by = "band") %>%
  mutate(d_grade = grade_26 - grade_25,
         d_prp   = prp_26   - prp_25,
         d_win   = win_26   - win_25)

cat("\n--- THE ANSWER: 2026 vs 2025 pass-rush slate, by band ---\n")
print(cmp_prush_slate, n = Inf)

# ------------------------------------------------------------
# 10. FIG 1 — bands x facets (grade + PRP per default ruling; win rate
#     computed above, table-only). Style mirrors the OL Fig 1.
# ------------------------------------------------------------

prush_fig1_pd <- cmp_prush_slate %>%
  select(band, grade_25, grade_26, prp_25, prp_26) %>%
  pivot_longer(-band, names_to = c("metric", "yr"),
               names_pattern = "(grade|prp)_(25|26)", values_to = "pctl") %>%
  pivot_wider(names_from = yr, values_from = pctl, names_prefix = "y") %>%
  mutate(band   = factor(band, levels = rev(SOS_BANDS)),
         metric = factor(metric, levels = c("grade", "prp"),
                         labels = c("TPS pass-rush grade", "TPS PRP")))

plot_prush_slate <- ggplot(prush_fig1_pd, aes(y = band)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey45") +
  geom_segment(aes(x = y25, xend = y26, yend = band),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               linewidth = 1, color = "grey60") +
  geom_point(aes(x = y25), shape = 1, size = 3.2, stroke = 1.2, color = "grey55") +
  geom_point(aes(x = y26), shape = 16, size = 2.6, color = "#002244") +
  facet_wrap(~ metric, nrow = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(title = "NE offense - 2026 vs 2025 opposing pass-rush slate, by band",
       subtitle = "open circle = 2025 rushers faced (snap-weighted) | solid navy = 2026 projected rotations | higher = HARDER for Maye's pocket | dashed = league median",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey40", size = 8.5),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", size = 9),
        panel.spacing.x = unit(1.1, "lines"))

plot_prush_slate

# ------------------------------------------------------------
# 11. FIG-1 COMPANION TABLE (10b pattern). interp_share = the honesty
#     column (Andy 08h rename); flat sensitivity RETIRED 08h.
#     gt attached HERE, not inherited from the OL file: save.image
#     restores objects, never library() state — the 08g tab_options crash.
# ------------------------------------------------------------

library(gt)

prush_d_dom <- max(abs(c(cmp_prush_slate$d_grade, cmp_prush_slate$d_prp)),
                   na.rm = TRUE)

prush_slate_gt <- cmp_prush_slate %>%
  transmute(band,
            g25 = grade_25, g26 = grade_26, gd = d_grade,
            p25c = prp_25, p26c = prp_26, pd = d_prp,
            interp_share) %>%
  gt() %>%
  tab_spanner(label = "TPS grade", columns = c(g25, g26, gd)) %>%
  tab_spanner(label = "TPS PRP",   columns = c(p25c, p26c, pd)) %>%
  cols_label(band = "", g25 = "'25", g26 = "'26", gd = "\u0394",
             p25c = "'25", p26c = "'26", pd = "\u0394",
             interp_share = "interp %") %>%
  fmt_percent(columns = c(g25, g26, p25c, p26c, interp_share), decimals = 0) %>%
  fmt_percent(columns = c(gd, pd), decimals = 0, force_sign = TRUE) %>%
  data_color(columns = c(gd, pd),
             fn = scales::col_numeric(c("#6baed6", "#f7f7f7", "#C60C30"),
                                      domain = c(-prush_d_dom, prush_d_dom)),
             autocolor_text = TRUE) %>%
  tab_header(title = "2026 vs 2025 opposing pass-rush slate \u2014 the numbers",
             subtitle = "snap-share-weighted band means, division counted twice | higher = harder for Maye's pocket | \u0394 red = harder | interp % = share of projected snaps riding on interpolated (entry-year prior) values") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold")

prush_slate_gt

# ------------------------------------------------------------
# STOP 2 — sections 12+ (one-row-per-rusher panels in the v2 grammar,
# 3-season gt table) build AFTER the Fig-1 eyeball. Checkpoint:
# ------------------------------------------------------------
# ggsave("prush_fig1_slate.png", plot_prush_slate, width = 9, height = 4, dpi = 200)
# gtsave(prush_slate_gt, "prush_slate_table.png", vwidth = 800)
# save.image("~/ne_prush_sos_workspace.RData")
# system('aws s3 cp ~/ne_prush_sos_workspace.RData s3://nfl-pff-data-lucas/workspaces/')



# ============================================================
# NE 2026 — OPPONENT PASS-RUSH SoS — BUILD FILE (Phase 2)
# STEP 0 shipped 2026-08-08e: sections 1-6 = THE RULING LEDGER.
# Slate + figures sections land AFTER Andy's deltas tribble.
# Filename provisional — Andy renames per canon.
#
# SOURCE ORDER:
#   (1) prush pipeline objects in session (pff_stats/pff_pass_rush_AWS.R)
#   (2) new_england_opp_ol_schedule.R  (opp_2026_teams, pff_team_lookup
#       via data_build/pff_ids_validate_cross.R, percent_rank_avg)
#   (3) this file, top to bottom. Re-source WHOLE file after any edit.
#
# Direction: opposing RUSHERS vs NE's PROTECTION.
#   higher pctl = better rusher = HARDER for Maye's pocket.
# ============================================================

needed_p2 <- c("full_pass_rush_qbgrp", "combined_ids_defense",
               "pff_team_lookup", "opp_2026_teams", "percent_rank_avg",
               "blend2", "in_season", "sched_2026")
missing_p2 <- needed_p2[!vapply(needed_p2, exists, logical(1))]
if (length(missing_p2)) stop("missing session objects: ",
                             paste(missing_p2, collapse = ", "),
                             " — see SOURCE ORDER in header")

# ------------------------------------------------------------
# 1. constants (rulings 2026-08-08d)
# ------------------------------------------------------------

X_QUAL    <- 3            # qualifying game = >= X tps rush snaps
G_MIN     <- 6            # gate = >= G qualifying games (one game, one vote)
SOS_BANDS <- c("ED", "DI")

# RAQ returns base data.frames (read.csv); print(n=) crashes on them.
# Stopgap until session_helpers RAQ lands (idempotent):
full_pass_rush_qbgrp <- tibble::as_tibble(full_pass_rush_qbgrp)

# ------------------------------------------------------------
# 2. SoS TPS currency — FINAL (receipts 2026-08-08d/e)
# Gate: >= G_MIN games of >= X_QUAL tps rush snaps. X=3/G=6: 14-opponent
#   ledger eyeballed by name + approved; league mean 7.4/team, floor 5;
#   SEA 3DI+4ED = 7/7; NE = 9; KC = 6 (2 DI = real structure).
#   Card's (11,7) rejected: Reed g11=6 on 149 snaps.
# Metrics: snap-weighted over qualifying games (count rates = ratio-of-sums).
#   Noise: talent_sd .041-.048; luck_p10 .044 -> count rates ~parity noise;
#   grade anchors (every-snap scoring). Facets default grade + PRP
#   (r_grade_win = .857 -> win rate to the table; Andy can veto).
# Validated 08e: 226-243 player-seasons/yr 2016-2025; 2025 pools DI 118 /
#   ED 116; NE scored 10 (was 4); "only 1 in 2024" dead (235 in 2024).
# Card table + its gates UNTOUCHED.
# ------------------------------------------------------------

prush_qual_games <- full_pass_rush_qbgrp %>%
  filter(position %in% SOS_BANDS,
         true_pass_set_snap_counts_pass_rush >= X_QUAL)

prush_modal_band <- prush_qual_games %>%
  count(player_id, season, position,
        wt = true_pass_set_snap_counts_pass_rush, name = "sn") %>%
  group_by(player_id, season) %>%
  slice_max(sn, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, season, band = position)

prush_tps_season_pctl_sos <- prush_qual_games %>%
  inner_join(prush_modal_band, by = c("player_id", "season")) %>%
  group_by(player, player_id, band, season) %>%
  summarise(
    q_games = dplyr::n(),
    q_snaps = sum(true_pass_set_snap_counts_pass_rush),
    tps_grade = weighted.mean(true_pass_set_grades_pass_rush_defense,
                              w = true_pass_set_snap_counts_pass_rush, na.rm = TRUE),
    tps_win   = weighted.mean(true_pass_set_pass_rush_win_rate,
                              w = true_pass_set_snap_counts_pass_rush, na.rm = TRUE),
    tps_prp   = weighted.mean(true_pass_set_prp,
                              w = true_pass_set_snap_counts_pass_rush, na.rm = TRUE),
    tps_pressure_rate = sum(true_pass_set_total_pressures, na.rm = TRUE) / q_snaps,
    tps_hurry_rate    = sum(true_pass_set_hurries, na.rm = TRUE) / q_snaps,
    .groups = "drop"
  ) %>%
  filter(q_games >= G_MIN) %>%
  group_by(band, season) %>%
  mutate(
    tps_grade_sos_pctl    = percent_rank_avg(tps_grade),
    tps_win_sos_pctl      = percent_rank_avg(tps_win),
    tps_prp_sos_pctl      = percent_rank_avg(tps_prp),
    tps_pressure_sos_pctl = percent_rank_avg(tps_pressure_rate),
    tps_hurry_sos_pctl    = percent_rank_avg(tps_hurry_rate)
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 3. defense crosswalk (player grain) + 2026 front rosters
#    xwalk receipts 08e: 8 NA-gsis players dropped, 0 collisions
# ------------------------------------------------------------

def_xwalk <- combined_ids_defense %>%
  filter(!is.na(gsis_id)) %>%
  distinct(player_id, gsis_id)

stopifnot(anyDuplicated(def_xwalk$gsis_id)   == 0,
          anyDuplicated(def_xwalk$player_id) == 0)

front_roster_pos <- c("DL", "DE", "DT", "NT", "EDGE", "OLB", "LB", "ILB", "MLB")

def_2026 <- nflreadr::load_rosters(2026) %>%
  filter(position %in% front_roster_pos) %>%
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
#    band law: modal PFF position from prush history; roster strings
#    NEVER assign bands (det_position 99.5% NA for rushers — tested 08e).
#    Rookie bands = tribble-assigned by Andy.
# ------------------------------------------------------------

prush_usage <- prush_qual_games %>%
  filter(season %in% c(2024, 2025)) %>%
  group_by(player_id, season) %>%
  summarise(tps_snaps = sum(true_pass_set_snap_counts_pass_rush),
            qg = dplyr::n(), .groups = "drop") %>%
  pivot_wider(names_from = season, values_from = c(tps_snaps, qg))

band_map <- prush_modal_band %>% filter(season == 2025) %>%
  select(player_id, band_25 = band) %>%
  full_join(prush_modal_band %>% filter(season == 2024) %>%
              select(player_id, band_24 = band),
            by = "player_id") %>%
  mutate(band = dplyr::coalesce(band_25, band_24)) %>%
  select(player_id, band)

pctl_25 <- prush_tps_season_pctl_sos %>% filter(season == 2025) %>%
  select(player_id, grade_25 = tps_grade_sos_pctl, prp_25 = tps_prp_sos_pctl)
pctl_24 <- prush_tps_season_pctl_sos %>% filter(season == 2024) %>%
  select(player_id, grade_24 = tps_grade_sos_pctl, prp_24 = tps_prp_sos_pctl)

ledger_base <- def_2026 %>%
  left_join(prush_usage, by = "player_id") %>%
  left_join(band_map,    by = "player_id") %>%
  left_join(pctl_25,     by = "player_id") %>%
  left_join(pctl_24,     by = "player_id") %>%
  mutate(status = case_when(
    entry_year == 2026                         ~ "rookie",
    is.na(player_id)                           ~ "no_pff_id",
    !is.na(grade_25)                           ~ "has_2025_pctl",
    !is.na(grade_24)                           ~ "data_2024_only",
    dplyr::coalesce(tps_snaps_2025, 0) > 0 |
      dplyr::coalesce(tps_snaps_2024, 0) > 0   ~ "usage_no_pctl",
    TRUE                                       ~ "no_prush_history"
  ))

cat("\n--- off-ball / no-history vets excluded from ledger (per team) ---\n")
print(ledger_base %>% filter(status == "no_prush_history") %>%
        count(team_name), n = Inf)

ledger <- ledger_base %>% filter(status != "no_prush_history")

# ------------------------------------------------------------
# 5. rotation-size proposal: n per (team, band) = that team's OWN 2025
#    gated-rotation count. 2026 proposal = top-n of the band by usage.
#    Rookies have no usage -> never auto-proposed -> Andy tribble-adds.
# ------------------------------------------------------------

n_band_2025 <- prush_qual_games %>%
  filter(season == 2025) %>%
  count(def_ssn, player_id, name = "g") %>%
  filter(g >= G_MIN) %>%
  inner_join(prush_modal_band %>% filter(season == 2025) %>%
               select(player_id, band),
             by = "player_id") %>%
  count(def_ssn, band, name = "n_2025") %>%
  mutate(team_name = stringr::str_remove(def_ssn, "2025$")) %>%
  filter(team_name %in% opp_2026_teams) %>%
  select(team_name, band, n_2025)

stopifnot(nrow(n_band_2025) == length(opp_2026_teams) * 2)

# usage_ord = pmax of the two seasons, NOT coalesce. Found on first ledger
# run: coalesce buried Ed Oliver (37 injury snaps '25 vs 211 + .897 grade
# '24) below DeWayne Carter — the AVT/injury-return class. pmax surfaces
# returners for Andy's eyeball; benched-role inflation is exactly what the
# tribble exists to catch. Seven proposals flip vs coalesce; one matters
# (Oliver), six are fringe coin-flips (Sweat/Onyemata, B.Jones/Hemingway,
# Cox/Mosby, Wingo/Tufele, Nowaske/Turner, Loudermilk/Taimani).
# N_BAND ruling (Andy 08g): uniform top-4 per band, ALL teams. Per-team
# shape inheritance retired — it assumed scheme continuity and DC churn
# breaks that yearly ("we're trying to overfit"). DEN's 5th DI capped
# ("fuck it"). Shortfalls auto-phantom in section 9 at band prior with
# slot-rank weights — the KC ED4 / MIN ED3 (Chambliss-class) hand rows
# from 08f, generalized into a rule. n_band_2025 stays as an FYI print.
N_BAND <- 4

ledger <- ledger %>%
  mutate(usage_ord = pmax(dplyr::coalesce(tps_snaps_2025, 0),
                          dplyr::coalesce(tps_snaps_2024, 0))) %>%
  group_by(team_name, band) %>%
  arrange(desc(usage_ord), .by_group = TRUE) %>%
  mutate(band_rank = dplyr::row_number(),
         proposed  = !is.na(band) & band_rank <= N_BAND) %>%
  ungroup()

# ------------------------------------------------------------
# 6. THE RULING LEDGER
# ------------------------------------------------------------

cat("\n================ STEP 0 — THE RULING LEDGER ================\n")
cat("proposed = TRUE -> auto-rotation (top N_BAND = 4 of the band by '25/'24 usage)\n")
cat("Andy rules ALL deltas via tribble: adds (rookies/signings), drops\n")
cat("(injuries/cuts), share overrides, rookie band assignments.\n\n")

ledger_print <- ledger %>%
  arrange(match(team_name, opp_2026_teams), band, desc(usage_ord)) %>%
  # band = PFF modal position, THE position column (Andy 08f). nflverse
  # roster_pos (DL/LB only) dropped — its lone residual use is a weak hint
  # on rookie rows, and rookie bands are tribble calls by law.
  select(team_name, band, roster_name, status, proposed,
         sn25 = tps_snaps_2025, sn24 = tps_snaps_2024,
         grade_25, prp_25, grade_24, prp_24)
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

cat("\n--- 2025 rotation shapes (FYI only - membership is uniform N_BAND = 4) ---\n")
print(n_band_2025 %>%
        pivot_wider(names_from = band, values_from = n_2025) %>%
        arrange(match(team_name, opp_2026_teams)), n = Inf)

# ------------------------------------------------------------
# STOP — 2026-08-08e boundary. Rulings happen in section 7 below.
# ------------------------------------------------------------

# ------------------------------------------------------------
# 7. ANDY'S DELTAS — the only judgment layer.
#    EMPTY = full sign-off (legal + complete). Actions: "add" / "drop".
#    Phantoms are AUTOMATIC as of 08g (section 9 fills every band to
#    N_BAND at the entry-year prior) — no phantom rows needed.
#    08f notes preserved: KC ED — Gillotte likely ED3, 4th body unknown;
#    MIN ED — Chambliss-class rookie most likely (name not yet rostered).
#    Both now covered by auto-fill; upgrade to a named "add" when real.
#    Names must match def_2026 spelling exactly — the gate fails loudly.
# ------------------------------------------------------------

opp_prush_2026_deltas <- tribble(
  ~team_name, ~roster_name, ~band, ~action, ~note
  # "MIN", "<name>", "ED", "add", "example - rookie into the rotation",
)

d_miss <- opp_prush_2026_deltas %>%
  anti_join(def_2026, by = c("team_name", "roster_name"))
if (nrow(d_miss) > 0) {
  print(d_miss)
  stop("delta names not on 2026 rosters - fix spelling vs the ledger")
}

cat("\n--- deltas registered:", nrow(opp_prush_2026_deltas), "row(s) ---\n")
if (nrow(opp_prush_2026_deltas) > 0) print(opp_prush_2026_deltas, n = Inf)

# ------------------------------------------------------------
# 8. FACED 2025 SIDE — observed only, no imputation of the past.
#    Every gated rusher-game vs NE's offense, weighted by that game's
#    TPS rush snaps. Unscored share printed as the honesty number.
# ------------------------------------------------------------

faced_games_2025 <- full_pass_rush_qbgrp %>%
  filter(startsWith(qbgrp_ssn, "NE"), season == 2025, in_season(week),
         position %in% SOS_BANDS,
         true_pass_set_snap_counts_pass_rush >= X_QUAL) %>%
  transmute(player_id, player, week, band_row = position,
            g_snaps = true_pass_set_snap_counts_pass_rush) %>%
  left_join(prush_tps_season_pctl_sos %>% filter(season == 2025) %>%
              select(player_id, band,
                     g25 = tps_grade_sos_pctl, p25 = tps_prp_sos_pctl,
                     w25v = tps_win_sos_pctl),
            by = "player_id") %>%
  mutate(band = dplyr::coalesce(band, band_row))

cat("\n--- faced 2025: weeks present (expect 21 incl. playoffs) ---\n")
print(faced_games_2025 %>% distinct(week) %>% arrange(week), n = Inf)

faced_band_2025 <- faced_games_2025 %>%
  group_by(band) %>%
  summarise(rusher_games   = dplyr::n(),
            tot_snaps      = sum(g_snaps),
            unscored_share = sum(g_snaps[is.na(g25)]) / tot_snaps,
            grade_25 = weighted.mean(g25,  w = g_snaps, na.rm = TRUE),
            prp_25   = weighted.mean(p25,  w = g_snaps, na.rm = TRUE),
            win_25   = weighted.mean(w25v, w = g_snaps, na.rm = TRUE),
            .groups = "drop")

cat("\n--- faced 2025 band profile (unscored_share = honesty number) ---\n")
print(faced_band_2025)

# ------------------------------------------------------------
# 9. 2026 PROJECTION — signed rotations + deltas + priors + phantoms.
#    Values: blend2('25,'24 pctl, w25 = q_games25/10). No-pctl members
#    (rookie / under-gate vet / phantom) = band entry-year prior, flagged
#    (Andy's median-interpolate ruling, 08f). Weights: pmax('25,'24 snaps)
#    -> renormalized shares within (team, band).
# ------------------------------------------------------------

entry_years_def <- nflreadr::load_rosters(2017:2025) %>%
  filter(!is.na(gsis_id), !is.na(entry_year)) %>%
  group_by(gsis_id) %>%
  summarise(entry_year = min(entry_year), .groups = "drop") %>%
  inner_join(def_xwalk, by = "gsis_id") %>%
  select(player_id, entry_year)

prush_rookie_prior <- prush_tps_season_pctl_sos %>%
  inner_join(entry_years_def, by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2017) %>%
  group_by(band) %>%
  summarise(pr_grade = median(tps_grade_sos_pctl, na.rm = TRUE),
            pr_prp   = median(tps_prp_sos_pctl,   na.rm = TRUE),
            pr_win   = median(tps_win_sos_pctl,   na.rm = TRUE),
            .groups = "drop")

cat("\n--- entry-year prior by band (values for rookies/phantoms/no-pctl) ---\n")
print(prush_rookie_prior)

# 2025 slot-usage curve: phantom weights inherit the team's own vacated
# slot; league median at that rank is the fallback
slot_usage_2025 <- prush_qual_games %>%
  filter(season == 2025) %>%
  group_by(def_ssn, player_id) %>%
  summarise(g = dplyr::n(),
            sn = sum(true_pass_set_snap_counts_pass_rush), .groups = "drop") %>%
  filter(g >= G_MIN) %>%
  inner_join(prush_modal_band %>% filter(season == 2025) %>%
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
p25_cur <- prush_tps_season_pctl_sos %>% filter(season == 2025) %>%
  select(player_id, g25 = tps_grade_sos_pctl, p25 = tps_prp_sos_pctl,
         w25v = tps_win_sos_pctl)
p24_cur <- prush_tps_season_pctl_sos %>% filter(season == 2024) %>%
  select(player_id, g24 = tps_grade_sos_pctl, p24 = tps_prp_sos_pctl,
         w24v = tps_win_sos_pctl)

adds_prepped <- opp_prush_2026_deltas %>%
  filter(action == "add") %>%
  left_join(ledger_base %>%
              select(team_name, roster_name, player_id, band_hist = band,
                     tps_snaps_2025, tps_snaps_2024, qg_2025, status),
            by = c("team_name", "roster_name")) %>%
  mutate(band = dplyr::coalesce(band, band_hist)) %>%
  select(team_name, roster_name, player_id, band,
         tps_snaps_2025, tps_snaps_2024, qg_2025, status)

rot_real <- ledger %>%
  filter(proposed) %>%
  select(team_name, roster_name, player_id, band,
         tps_snaps_2025, tps_snaps_2024, qg_2025, status) %>%
  anti_join(opp_prush_2026_deltas %>% filter(action == "drop"),
            by = c("team_name", "roster_name")) %>%
  bind_rows(adds_prepped)

stopifnot(!any(is.na(rot_real$band)),
          anyDuplicated(rot_real[, c("team_name", "roster_name")]) == 0)

rot_real <- rot_real %>%
  left_join(p25_cur, by = "player_id") %>%
  left_join(p24_cur, by = "player_id") %>%
  left_join(prush_rookie_prior, by = "band") %>%
  mutate(w25      = pmin(dplyr::coalesce(qg_2025, 0L) / 10, 1),
         grade_bl = blend2(g25,  g24,  w25),
         prp_bl   = blend2(p25,  p24,  w25),
         win_bl   = blend2(w25v, w24v, w25),
         prior_used = is.na(grade_bl),
         grade_f = dplyr::coalesce(grade_bl, pr_grade),
         prp_f   = dplyr::coalesce(prp_bl,   pr_prp),
         win_f   = dplyr::coalesce(win_bl,   pr_win),
         usage_w = pmax(dplyr::coalesce(tps_snaps_2025, 0),
                        dplyr::coalesce(tps_snaps_2024, 0))) %>%
  select(team_name, roster_name, band, status, prior_used,
         usage_w, grade_f, prp_f, win_f)

# 08g: enforce uniform bands. Cap overages at N_BAND (lowest-usage guys
# out, DEN's 5th DI included), then AUTO-PHANTOM every shortfall — each
# missing slot is an unnamed job at the band entry-year prior, weighted
# by that team's own 2025 usage at that slot rank (league-median rank
# usage fallback, then the global floor). Named adds compete on usage
# like everyone else; a zero-usage rookie add inherits its slot weight.
rot_real <- rot_real %>%
  mutate(usage_w = if_else(usage_w > 0, usage_w, NA_real_)) %>%
  group_by(team_name, band) %>%
  arrange(desc(dplyr::coalesce(usage_w, 0)), .by_group = TRUE) %>%
  mutate(band_rank = dplyr::row_number()) %>%
  filter(band_rank <= N_BAND) %>%
  ungroup() %>%
  left_join(slot_usage_2025,
            by = c("team_name", "band", "band_rank" = "slot_rk")) %>%
  left_join(slot_usage_med, by = c("band", "band_rank" = "slot_rk")) %>%
  mutate(usage_w = dplyr::coalesce(usage_w, sn_slot, sn_med, fallback_sn)) %>%
  select(team_name, roster_name, band, status, prior_used,
         usage_w, grade_f, prp_f, win_f)

auto_phantoms <- rot_real %>%
  count(team_name, band, name = "n_real") %>%
  tidyr::complete(team_name = opp_2026_teams, band = SOS_BANDS,
                  fill = list(n_real = 0L)) %>%
  filter(n_real < N_BAND) %>%
  mutate(slot_rk = purrr::map2(n_real, N_BAND, ~ seq(.x + 1L, .y))) %>%
  tidyr::unnest(slot_rk) %>%
  left_join(slot_usage_2025, by = c("team_name", "band", "slot_rk")) %>%
  left_join(slot_usage_med,  by = c("band", "slot_rk")) %>%
  left_join(prush_rookie_prior, by = "band") %>%
  transmute(team_name,
            roster_name = paste0("PHANTOM_", band, slot_rk),
            band, status = "phantom", prior_used = TRUE,
            usage_w = dplyr::coalesce(sn_slot, sn_med, fallback_sn),
            grade_f = pr_grade, prp_f = pr_prp, win_f = pr_win)

rot_2026 <- bind_rows(rot_real, auto_phantoms)

# every band is exactly N_BAND deep, no exceptions
stopifnot(all(dplyr::count(rot_2026, team_name, band)$n == N_BAND),
          nrow(rot_2026) == length(opp_2026_teams) * 2 * N_BAND)

cat("\n--- 2026 rotations, final (prior_used = valued at band prior) ---\n")
print(rot_2026 %>%
        arrange(match(team_name, opp_2026_teams), band, desc(usage_w)) %>%
        select(team_name, band, roster_name, status, prior_used, usage_w,
               grade_f, prp_f), n = Inf)

team_band_2026 <- rot_2026 %>%
  group_by(team_name, band) %>%
  summarise(n_members    = dplyr::n(),
            interp_share = sum(usage_w[prior_used]) / sum(usage_w),
            grade_26 = weighted.mean(grade_f, w = usage_w),
            prp_26   = weighted.mean(prp_f,   w = usage_w),
            win_26   = weighted.mean(win_f,   w = usage_w),
            .groups = "drop")

cat("\n--- team-band 2026 values + interp_share honesty column ---\n")
print(team_band_2026 %>%
        arrange(match(team_name, opp_2026_teams), band), n = Inf)

# schedule-weighted slate (division x2, same as OL Fig 1)
slate_band_2026 <- tibble(team_name = sched_2026) %>%
  left_join(team_band_2026, by = "team_name", relationship = "many-to-many") %>%
  group_by(band) %>%
  summarise(grade_26 = mean(grade_26), prp_26 = mean(prp_26),
            win_26 = mean(win_26),
            interp_share = mean(interp_share), .groups = "drop")

cmp_prush_slate <- faced_band_2025 %>%
  select(band, grade_25, prp_25, win_25, unscored_share_25 = unscored_share) %>%
  left_join(slate_band_2026, by = "band") %>%
  mutate(d_grade = grade_26 - grade_25,
         d_prp   = prp_26   - prp_25,
         d_win   = win_26   - win_25)

cat("\n--- THE ANSWER: 2026 vs 2025 pass-rush slate, by band ---\n")
print(cmp_prush_slate, n = Inf)

# ------------------------------------------------------------
# 10. FIG 1 — bands x facets (grade + PRP per default ruling; win rate
#     computed above, table-only). Style mirrors the OL Fig 1.
# ------------------------------------------------------------

prush_fig1_pd <- cmp_prush_slate %>%
  select(band, grade_25, grade_26, prp_25, prp_26) %>%
  pivot_longer(-band, names_to = c("metric", "yr"),
               names_pattern = "(grade|prp)_(25|26)", values_to = "pctl") %>%
  pivot_wider(names_from = yr, values_from = pctl, names_prefix = "y") %>%
  mutate(band   = factor(band, levels = rev(SOS_BANDS)),
         metric = factor(metric, levels = c("grade", "prp"),
                         labels = c("TPS pass-rush grade", "TPS PRP")))

plot_prush_slate <- ggplot(prush_fig1_pd, aes(y = band)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey45") +
  geom_segment(aes(x = y25, xend = y26, yend = band),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               linewidth = 1, color = "grey60") +
  geom_point(aes(x = y25), shape = 1, size = 3.2, stroke = 1.2, color = "grey55") +
  geom_point(aes(x = y26), shape = 16, size = 2.6, color = "#002244") +
  facet_wrap(~ metric, nrow = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(title = "NE offense - 2026 vs 2025 opposing pass-rush slate, by band",
       subtitle = "open circle = 2025 rushers faced (snap-weighted) | solid navy = 2026 projected rotations | higher = HARDER for Maye's pocket | dashed = league median",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey40", size = 8.5),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", size = 9),
        panel.spacing.x = unit(1.1, "lines"))

plot_prush_slate

# ------------------------------------------------------------
# 11. FIG-1 COMPANION TABLE (10b pattern). interp_share = the honesty
#     column (Andy 08h rename); flat sensitivity RETIRED 08h.
#     gt attached HERE, not inherited from the OL file: save.image
#     restores objects, never library() state — the 08g tab_options crash.
# ------------------------------------------------------------

library(gt)

prush_d_dom <- max(abs(c(cmp_prush_slate$d_grade, cmp_prush_slate$d_prp)),
                   na.rm = TRUE)

prush_slate_gt <- cmp_prush_slate %>%
  transmute(band,
            g25 = grade_25, g26 = grade_26, gd = d_grade,
            p25c = prp_25, p26c = prp_26, pd = d_prp,
            interp_share) %>%
  gt() %>%
  tab_spanner(label = "TPS grade", columns = c(g25, g26, gd)) %>%
  tab_spanner(label = "TPS PRP",   columns = c(p25c, p26c, pd)) %>%
  cols_label(band = "", g25 = "'25", g26 = "'26", gd = "\u0394",
             p25c = "'25", p26c = "'26", pd = "\u0394",
             interp_share = "interp %") %>%
  fmt_percent(columns = c(g25, g26, p25c, p26c, interp_share), decimals = 0) %>%
  fmt_percent(columns = c(gd, pd), decimals = 0, force_sign = TRUE) %>%
  data_color(columns = c(gd, pd),
             fn = scales::col_numeric(c("#6baed6", "#f7f7f7", "#C60C30"),
                                      domain = c(-prush_d_dom, prush_d_dom)),
             autocolor_text = TRUE) %>%
  tab_header(title = "2026 vs 2025 opposing pass-rush slate \u2014 the numbers",
             subtitle = "snap-share-weighted band means, division counted twice | higher = harder for Maye's pocket | \u0394 red = harder | interp % = share of projected snaps riding on interpolated (entry-year prior) values") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold")

prush_slate_gt

# ------------------------------------------------------------
# STOP 2 — sections 12+ (one-row-per-rusher panels in the v2 grammar,
# 3-season gt table) build AFTER the Fig-1 eyeball. Checkpoint:
# ------------------------------------------------------------
# ggsave("prush_fig1_slate.png", plot_prush_slate, width = 9, height = 4, dpi = 200)
# gtsave(prush_slate_gt, "prush_slate_table.png", vwidth = 800)
# save.image("~/ne_prush_sos_workspace.RData")
# system('aws s3 cp ~/ne_prush_sos_workspace.RData s3://nfl-pff-data-lucas/workspaces/')



# ============================================================
# NE 2026 -- PASS-RUSH SoS, PER TEAM (Phase 2 add-on) + COLLISION FIX
# Andy ruling 2026-08-14: banded units get per-team views (run-D
#   done in phase4b; pass rush here).
#
# COLLISION AUTOPSY (why this file looks the way it does):
#   Phase 2 and Phase 3 build files write the SAME generic names
#   (team_band_2026, rot_2026, faced_band_2025, ledger, def_2026,
#   slot_usage_2025...). Last-sourced wins. Slate answers survived
#   (cmp_prush_slate / cmp_rundef_slate are unique). The run-D
#   per-team table from phase4b is PROVABLY valid: had
#   team_band_2026 been the prush version when it ran, it crashes
#   on stop_26; it ran clean -> run-D regime was live.
#   THE LAW going forward: phase-suffixed names at birth (Phase 4
#   onward already complies). This file (a) snapshots whichever
#   regime is currently live into *_rundef / *_prush_live names,
#   (b) rebuilds the prush projection chain into *_prush names,
#   (c) proves the rebuild == the original build by an equality
#   receipt vs the live cmp_prush_slate (tolerance 1e-8, stops
#   loudly on drift). NO re-runs of the old build files needed.
#
# Prush receipts (Phase 2 header, 08d/e/g): gate X=3 TPS rush snaps,
#   G=6; bands ED/DI; uniform N_BAND=4; facets grade + PRP (win
#   tabled, r_grade_win = .857); priors by band; usage_ord = pmax.
#
# SOURCE ORDER: Phase 2 + Phase 3 + Phase 4 builds already sourced
#   this session (Andy rerun-all 2026-08-14). Re-source WHOLE file.
# ============================================================

library(dplyr); library(tidyr); library(stringr)
library(ggplot2); library(gt)

needed_p2b <- c("full_pass_rush_qbgrp", "prush_qual_games", "prush_modal_band",
                "prush_tps_season_pctl_sos", "prush_usage", "prush_rookie_prior",
                "cmp_prush_slate", "opp_prush_2026_deltas",
                "combined_ids_defense", "pff_team_lookup", "opp_2026_teams",
                "sched_2026", "blend2", "in_season", "percent_rank_avg")
missing_p2b <- needed_p2b[!vapply(needed_p2b, exists, logical(1))]
if (length(missing_p2b)) stop("missing session objects: ",
                              paste(missing_p2b, collapse = ", "),
                              " -- re-source the Phase 2 build file first")

full_pass_rush_qbgrp     <- tibble::as_tibble(full_pass_rush_qbgrp)
prush_qual_games         <- tibble::as_tibble(prush_qual_games)
prush_modal_band         <- tibble::as_tibble(prush_modal_band)
prush_tps_season_pctl_sos <- tibble::as_tibble(prush_tps_season_pctl_sos)
prush_usage              <- tibble::as_tibble(prush_usage)
prush_rookie_prior       <- tibble::as_tibble(prush_rookie_prior)
cmp_prush_slate          <- tibble::as_tibble(cmp_prush_slate)
combined_ids_defense     <- tibble::as_tibble(combined_ids_defense)

# ------------------------------------------------------------
# 0. REGIME SNAPSHOT -- whichever phase's generic objects are live
#    right now gets copied to its permanent suffixed name. Nothing
#    is overwritten; detection is by column fingerprint.
# ------------------------------------------------------------

if (exists("team_band_2026")) {
  tb_fp <- colnames(team_band_2026)
  if ("stop_26" %in% tb_fp) {
    team_band_2026_rundef <- tibble::as_tibble(team_band_2026)
    if (exists("rot_2026") && "stop_f" %in% colnames(rot_2026))
      rot_2026_rundef <- tibble::as_tibble(rot_2026)
    if (exists("faced_band_2025") && "stop_25" %in% colnames(faced_band_2025))
      faced_band_2025_rundef <- tibble::as_tibble(faced_band_2025)
    cat("\n--- regime snapshot: RUN-D was live -> team_band_2026_rundef (+ rot/faced) saved ---\n")
  } else if ("prp_26" %in% tb_fp) {
    team_band_2026_prush_live <- tibble::as_tibble(team_band_2026)
    cat("\n--- regime snapshot: PRUSH was live -> team_band_2026_prush_live saved (rebuild still runs for the receipt) ---\n")
  } else {
    cat("\n--- regime snapshot: team_band_2026 fingerprint unknown; printed below ---\n")
    print(tb_fp)
  }
} else {
  cat("\n--- regime snapshot: no generic team_band_2026 live; nothing to save ---\n")
}

# ------------------------------------------------------------
# 1. PRUSH PROJECTION CHAIN, REBUILT SUFFIXED (mirror of the Phase 2
#    build file sections 3-9; same code path, permanent names).
# ------------------------------------------------------------

X_QUAL_PR <- 3; G_MIN_PR <- 6
SOS_BANDS_PR <- c("ED", "DI"); N_BAND_PR <- 4L

def_xwalk_prush <- combined_ids_defense %>%
  filter(!is.na(gsis_id)) %>%
  distinct(player_id, gsis_id)
stopifnot(anyDuplicated(def_xwalk_prush$gsis_id)   == 0,
          anyDuplicated(def_xwalk_prush$player_id) == 0)

front_roster_pos <- c("DL", "DE", "DT", "NT", "EDGE", "OLB", "LB", "ILB", "MLB")

def_2026_prush <- nflreadr::load_rosters(2026) %>%
  filter(position %in% front_roster_pos) %>%
  transmute(gsis_id,
            roster_name = full_name,
            team_name = dplyr::coalesce(pff_team_lookup[team], team),
            entry_year,
            roster_pos = position) %>%
  filter(team_name %in% opp_2026_teams) %>%
  left_join(def_xwalk_prush, by = "gsis_id")

band_map_prush <- prush_modal_band %>% filter(season == 2025) %>%
  select(player_id, band_25 = band) %>%
  full_join(prush_modal_band %>% filter(season == 2024) %>%
              select(player_id, band_24 = band),
            by = "player_id") %>%
  mutate(band = dplyr::coalesce(band_25, band_24)) %>%
  select(player_id, band)

pctl_25_prush <- prush_tps_season_pctl_sos %>% filter(season == 2025) %>%
  select(player_id, grade_25 = tps_grade_sos_pctl, prp_25 = tps_prp_sos_pctl)
pctl_24_prush <- prush_tps_season_pctl_sos %>% filter(season == 2024) %>%
  select(player_id, grade_24 = tps_grade_sos_pctl, prp_24 = tps_prp_sos_pctl)

ledger_prush <- def_2026_prush %>%
  left_join(prush_usage,    by = "player_id") %>%
  left_join(band_map_prush, by = "player_id") %>%
  left_join(pctl_25_prush,  by = "player_id") %>%
  left_join(pctl_24_prush,  by = "player_id") %>%
  mutate(status = case_when(
    entry_year == 2026                         ~ "rookie",
    is.na(player_id)                           ~ "no_pff_id",
    !is.na(grade_25)                           ~ "has_2025_pctl",
    !is.na(grade_24)                           ~ "data_2024_only",
    dplyr::coalesce(tps_snaps_2025, 0) > 0 |
      dplyr::coalesce(tps_snaps_2024, 0) > 0   ~ "usage_no_pctl",
    TRUE                                       ~ "no_prush_history"
  )) %>%
  filter(status != "no_prush_history") %>%
  mutate(usage_ord = pmax(dplyr::coalesce(tps_snaps_2025, 0),
                          dplyr::coalesce(tps_snaps_2024, 0))) %>%
  group_by(team_name, band) %>%
  arrange(desc(usage_ord), .by_group = TRUE) %>%
  mutate(band_rank = dplyr::row_number(),
         proposed  = !is.na(band) & band_rank <= N_BAND_PR) %>%
  ungroup()

entry_years_def_prush <- nflreadr::load_rosters(2017:2025) %>%
  filter(!is.na(gsis_id), !is.na(entry_year)) %>%
  group_by(gsis_id) %>%
  summarise(entry_year = min(entry_year), .groups = "drop") %>%
  inner_join(def_xwalk_prush, by = "gsis_id") %>%
  select(player_id, entry_year)

slot_usage_2025_prush <- prush_qual_games %>%
  filter(season == 2025) %>%
  group_by(def_ssn, player_id) %>%
  summarise(g = dplyr::n(),
            sn = sum(true_pass_set_snap_counts_pass_rush), .groups = "drop") %>%
  filter(g >= G_MIN_PR) %>%
  inner_join(prush_modal_band %>% filter(season == 2025) %>%
               select(player_id, band), by = "player_id") %>%
  mutate(team_name = stringr::str_remove(def_ssn, "2025$")) %>%
  group_by(team_name, band) %>%
  arrange(desc(sn), .by_group = TRUE) %>%
  mutate(slot_rk = dplyr::row_number()) %>%
  ungroup() %>%
  select(team_name, band, slot_rk, sn_slot = sn)

slot_usage_med_prush <- slot_usage_2025_prush %>%
  group_by(band, slot_rk) %>%
  summarise(sn_med = median(sn_slot), .groups = "drop")
fallback_sn_prush <- min(slot_usage_2025_prush$sn_slot)

p25_cur_prush <- prush_tps_season_pctl_sos %>% filter(season == 2025) %>%
  select(player_id, g25 = tps_grade_sos_pctl, p25 = tps_prp_sos_pctl,
         w25v = tps_win_sos_pctl)
p24_cur_prush <- prush_tps_season_pctl_sos %>% filter(season == 2024) %>%
  select(player_id, g24 = tps_grade_sos_pctl, p24 = tps_prp_sos_pctl,
         w24v = tps_win_sos_pctl)

ledger_base_prush <- def_2026_prush %>%
  left_join(prush_usage,    by = "player_id") %>%
  left_join(band_map_prush, by = "player_id") %>%
  left_join(pctl_25_prush,  by = "player_id") %>%
  left_join(pctl_24_prush,  by = "player_id") %>%
  mutate(status = case_when(
    entry_year == 2026                         ~ "rookie",
    is.na(player_id)                           ~ "no_pff_id",
    !is.na(grade_25)                           ~ "has_2025_pctl",
    !is.na(grade_24)                           ~ "data_2024_only",
    dplyr::coalesce(tps_snaps_2025, 0) > 0 |
      dplyr::coalesce(tps_snaps_2024, 0) > 0   ~ "usage_no_pctl",
    TRUE                                       ~ "no_prush_history"
  ))

adds_prepped_prush <- opp_prush_2026_deltas %>%
  filter(action == "add") %>%
  left_join(ledger_base_prush %>%
              select(team_name, roster_name, player_id, band_hist = band,
                     tps_snaps_2025, tps_snaps_2024, qg_2025, status),
            by = c("team_name", "roster_name")) %>%
  mutate(band = dplyr::coalesce(band, band_hist)) %>%
  select(team_name, roster_name, player_id, band,
         tps_snaps_2025, tps_snaps_2024, qg_2025, status)

rot_real_prush <- ledger_prush %>%
  filter(proposed) %>%
  select(team_name, roster_name, player_id, band,
         tps_snaps_2025, tps_snaps_2024, qg_2025, status) %>%
  anti_join(opp_prush_2026_deltas %>% filter(action == "drop"),
            by = c("team_name", "roster_name")) %>%
  bind_rows(adds_prepped_prush)

stopifnot(!any(is.na(rot_real_prush$band)),
          anyDuplicated(rot_real_prush[, c("team_name", "roster_name")]) == 0)

rot_real_prush <- rot_real_prush %>%
  left_join(p25_cur_prush, by = "player_id") %>%
  left_join(p24_cur_prush, by = "player_id") %>%
  left_join(prush_rookie_prior, by = "band") %>%
  mutate(w25      = pmin(dplyr::coalesce(qg_2025, 0L) / 10, 1),
         grade_bl = blend2(g25,  g24,  w25),
         prp_bl   = blend2(p25,  p24,  w25),
         win_bl   = blend2(w25v, w24v, w25),
         prior_used = is.na(grade_bl),
         grade_f = dplyr::coalesce(grade_bl, pr_grade),
         prp_f   = dplyr::coalesce(prp_bl,   pr_prp),
         win_f   = dplyr::coalesce(win_bl,   pr_win),
         usage_w = pmax(dplyr::coalesce(tps_snaps_2025, 0),
                        dplyr::coalesce(tps_snaps_2024, 0))) %>%
  select(team_name, roster_name, band, status, prior_used,
         usage_w, grade_f, prp_f, win_f)

rot_real_prush <- rot_real_prush %>%
  mutate(usage_w = if_else(usage_w > 0, usage_w, NA_real_)) %>%
  group_by(team_name, band) %>%
  arrange(desc(dplyr::coalesce(usage_w, 0)), .by_group = TRUE) %>%
  mutate(band_rank = dplyr::row_number()) %>%
  filter(band_rank <= N_BAND_PR) %>%
  ungroup() %>%
  left_join(slot_usage_2025_prush,
            by = c("team_name", "band", "band_rank" = "slot_rk")) %>%
  left_join(slot_usage_med_prush, by = c("band", "band_rank" = "slot_rk")) %>%
  mutate(usage_w = dplyr::coalesce(usage_w, sn_slot, sn_med, fallback_sn_prush)) %>%
  select(team_name, roster_name, band, status, prior_used,
         usage_w, grade_f, prp_f, win_f)

auto_phantoms_prush <- rot_real_prush %>%
  count(team_name, band, name = "n_real") %>%
  tidyr::complete(team_name = opp_2026_teams, band = SOS_BANDS_PR,
                  fill = list(n_real = 0L)) %>%
  filter(n_real < N_BAND_PR) %>%
  mutate(slot_rk = purrr::map2(n_real, N_BAND_PR, ~ seq(.x + 1L, .y))) %>%
  tidyr::unnest(slot_rk) %>%
  left_join(slot_usage_2025_prush, by = c("team_name", "band", "slot_rk")) %>%
  left_join(slot_usage_med_prush,  by = c("band", "slot_rk")) %>%
  left_join(prush_rookie_prior, by = "band") %>%
  transmute(team_name,
            roster_name = paste0("PHANTOM_", band, slot_rk),
            band, status = "phantom", prior_used = TRUE,
            usage_w = dplyr::coalesce(sn_slot, sn_med, fallback_sn_prush),
            grade_f = pr_grade, prp_f = pr_prp, win_f = pr_win)

rot_2026_prush <- bind_rows(rot_real_prush, auto_phantoms_prush)

stopifnot(all(dplyr::count(rot_2026_prush, team_name, band)$n == N_BAND_PR),
          nrow(rot_2026_prush) == length(opp_2026_teams) * 2 * N_BAND_PR)

team_band_2026_prush <- rot_2026_prush %>%
  group_by(team_name, band) %>%
  summarise(n_members    = dplyr::n(),
            interp_share = sum(usage_w[prior_used]) / sum(usage_w),
            grade_26 = weighted.mean(grade_f, w = usage_w),
            prp_26   = weighted.mean(prp_f,   w = usage_w),
            .groups = "drop")

# ------------------------------------------------------------
# 2. THE RECEIPT: rebuilt slate must EQUAL the live cmp_prush_slate
#    (same empty deltas tribble, same machinery -> same numbers).
# ------------------------------------------------------------

slate_check <- tibble(team_name = sched_2026) %>%
  left_join(team_band_2026_prush, by = "team_name",
            relationship = "many-to-many") %>%
  group_by(band) %>%
  summarise(grade_26 = mean(grade_26), prp_26 = mean(prp_26),
            .groups = "drop") %>%
  arrange(band)

cmp_check <- cmp_prush_slate %>%
  select(band, grade_26, prp_26) %>% arrange(band)

cat("\n--- EQUALITY RECEIPT: rebuilt vs live cmp_prush_slate ---\n")
print(slate_check); print(cmp_check)
stopifnot(isTRUE(all.equal(slate_check$grade_26, cmp_check$grade_26,
                           tolerance = 1e-8)),
          isTRUE(all.equal(slate_check$prp_26, cmp_check$prp_26,
                           tolerance = 1e-8)))
cat("receipt holds: rebuild == original build (1e-8).\n")

# ------------------------------------------------------------
# 3. FACED 2025, PER TEAM x BAND -- observed only, prush gate
#    literals (3 TPS snaps / 6 games), snap-weighted.
# ------------------------------------------------------------

faced_prush_team_2025 <- full_pass_rush_qbgrp %>%
  filter(startsWith(qbgrp_ssn, "NE"), season == 2025, in_season(week),
         position %in% SOS_BANDS_PR,
         true_pass_set_snap_counts_pass_rush >= X_QUAL_PR) %>%
  transmute(player_id,
            team_name = stringr::str_remove(def_ssn, "2025$"),
            band_row = position,
            g_snaps = true_pass_set_snap_counts_pass_rush) %>%
  left_join(prush_tps_season_pctl_sos %>% filter(season == 2025) %>%
              select(player_id, band,
                     g25 = tps_grade_sos_pctl, p25 = tps_prp_sos_pctl),
            by = "player_id") %>%
  mutate(band = dplyr::coalesce(band, band_row)) %>%
  group_by(team_name, band) %>%
  summarise(tot_snaps = sum(g_snaps),
            unscored_share = sum(g_snaps[is.na(g25)]) / tot_snaps,
            grade_25 = if (all(is.na(g25))) NA_real_
            else weighted.mean(g25, w = g_snaps, na.rm = TRUE),
            prp_25   = if (all(is.na(p25))) NA_real_
            else weighted.mean(p25, w = g_snaps, na.rm = TRUE),
            .groups = "drop")

prush_team_cmp <- team_band_2026_prush %>%
  left_join(faced_prush_team_2025 %>%
              select(team_name, band, grade_25, prp_25, unscored_share),
            by = c("team_name", "band")) %>%
  mutate(d_grade = grade_26 - grade_25,
         d_prp   = prp_26   - prp_25) %>%
  group_by(team_name) %>%
  mutate(team_d = mean(d_grade, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(team_d), match(band, SOS_BANDS_PR))

cat("\n--- PASS RUSH PER TEAM x BAND (sorted by mean grade delta) ---\n")
print(prush_team_cmp %>% select(team_name, band, grade_25, grade_26, d_grade,
                                prp_25, prp_26, d_prp, unscored_share,
                                interp_share), n = Inf)

# ------------------------------------------------------------
# 4. GT TABLE (grouped by team) + HEATMAP (team x band, delta fill)
# ------------------------------------------------------------

prush_d_dom_team <- max(abs(c(prush_team_cmp$d_grade, prush_team_cmp$d_prp)),
                        na.rm = TRUE)

prush_team_gt <- prush_team_cmp %>%
  transmute(team_name, band,
            g25 = grade_25, g26 = grade_26, gd = d_grade,
            p25 = prp_25, p26 = prp_26, pd = d_prp,
            interp_share) %>%
  gt(groupname_col = "team_name") %>%
  tab_spanner(label = "TPS grade", columns = c(g25, g26, gd)) %>%
  tab_spanner(label = "TPS PRP",   columns = c(p25, p26, pd)) %>%
  cols_label(band = "", g25 = "'25", g26 = "'26", gd = "D",
             p25 = "'25", p26 = "'26", pd = "D",
             interp_share = "interp %") %>%
  fmt_percent(columns = c(g25, g26, p25, p26, interp_share), decimals = 0) %>%
  fmt_percent(columns = c(gd, pd), decimals = 0, force_sign = TRUE) %>%
  sub_missing(columns = everything(), missing_text = "--") %>%
  data_color(columns = c(gd, pd),
             fn = scales::col_numeric(c("#6baed6", "#f7f7f7", "#C60C30"),
                                      domain = c(-prush_d_dom_team,
                                                 prush_d_dom_team)),
             autocolor_text = TRUE) %>%
  tab_header(title = "2026 vs 2025 opposing pass rush -- per team, per band",
             subtitle = "snap-share-weighted band means | higher = harder for Maye's pocket | D red = harder | '--' = not faced in 2025 | interp % = share of projected snaps on entry-year priors | teams sorted by mean grade delta") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold")

prush_team_gt

prush_hm_pd <- prush_team_cmp %>%
  select(team_name, band, d_grade, d_prp, team_d) %>%
  pivot_longer(c(d_grade, d_prp), names_to = "metric", values_to = "delta") %>%
  mutate(team_name = factor(team_name,
                            levels = prush_team_cmp %>% arrange(team_d) %>%
                              distinct(team_name) %>% pull(team_name)),
         band   = factor(band, levels = SOS_BANDS_PR),
         metric = factor(metric, levels = c("d_grade", "d_prp"),
                         labels = c("TPS grade delta", "TPS PRP delta")),
         lab = if_else(is.na(delta), "--",
                       scales::percent(delta, accuracy = 1)))

plot_prush_team_hm <- ggplot(prush_hm_pd, aes(x = band, y = team_name,
                                              fill = delta)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = lab), size = 2.8) +
  facet_wrap(~ metric, nrow = 1) +
  scale_fill_gradient2(low = "#6baed6", mid = "#f7f7f7", high = "#C60C30",
                       midpoint = 0,
                       limits = c(-prush_d_dom_team, prush_d_dom_team),
                       na.value = "grey88", labels = scales::percent_format()) +
  labs(title = "Pass rush, per team x band -- 2026 vs 2025 delta",
       subtitle = "red = harder for Maye's pocket in 2026 | '--' = band not faced in 2025 | teams ordered by mean grade delta",
       x = NULL, y = NULL, fill = "delta") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey40", size = 8.5),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 9),
        legend.position = "right")

plot_prush_team_hm

# ------------------------------------------------------------
# Checkpoint (after eyeball). Permanent per-team objects now live:
#   team_band_2026_prush, rot_2026_prush, faced_prush_team_2025,
#   prush_team_cmp (+ whatever regime snapshot section 0 saved).
# ------------------------------------------------------------
# ggsave("prush_perteam_heatmap.png", plot_prush_team_hm, width = 8, height = 5.5, dpi = 200)
# gtsave(prush_team_gt, "prush_perteam_table.png", vwidth = 900)
# save.image("~/ne_prush_sos_workspace.RData")
# system('aws s3 cp ~/ne_prush_sos_workspace.RData s3://nfl-pff-data-lucas/workspaces/')