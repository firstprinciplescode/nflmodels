# ============================================================
# NE 2026 -- OPPONENT RB (RUSHING) SoS -- BUILD FILE (Phase 4)
# Gate ruled by Andy 2026-08-14: X_QUAL = 4 attempts, G_MIN = 6
#   qualifying games. Single band (HB). N_RB = 2 per team.
#   Facets: run grade anchors + MTF rate (floored at ALL 16 gate
#   cells). Breakaway rate rides with the noise caveat (NOISE at
#   every coverage-viable gate; only clears at G10/G12 = coverage-
#   dead). yco / ypa TABLE-only (ypa = OL-contaminated control).
#
# RECEIPTS (step 0, 2026-08-14):
#   rushing_summary = PFF player-game, HB filter kills QB kneels
#   and 140 junk rows (P/S/CB/LB/K/DI/ED/C/G = return/trick plays).
#   10 seasons 2016-2025 (~1.3k HB games/yr). qbgrp join via
#   combined_grade_epa_summary: stopifnot-clean; faced AND NE-own
#   2025 both = the canonical 21 weeks. Yardstick FLAT across X at
#   G6 (sd_grun 3.93-4.08) -- gate does not distort talent spread.
#   base p computed: mtf .168 / brk .045. Coverage corner X in
#   {4,6} x G6; X4G6 = ZERO unpriceable team-seasons 2016-2025,
#   64 backs in 2025 = exactly 2.0/team. HB-only committee mode =
#   2 at 15% (207/320), 20% (207/320), 25% (166 vs 148). N sweep:
#   N=2 across the whole coverage corner, no close-up. Calibration:
#   NE Henderson 210 / Rhamondre 188 clear EVERY gate, Gibson /
#   Jennings / D'Ernest die at every gate; SEA Walker 286 /
#   Charbonnet 189 clear all, Holani/Velus/Wright/Gaskin die.
#
# TOWER LANDMINES CARRIED (do not rebuild the tower):
#   rank_grp is GAME-GRAIN BY DESIGN (role = what a back shoulders
#   THAT game; season identity = aggregated carry share -- which is
#   exactly this currency). rush_proportion = run-SNAP share (drove
#   A/B/C letters); rush_share = CARRY share (rush_stats_high .4
#   cut). This file recomputes carry share at season grain; NEITHER
#   legacy column is imported. part_* dead for 2025. Index renames
#   fragile. Cluster labels seed-dependent. Receiving cols live in
#   rushing_summary (yprr/routes/targets) -- Phase 5 scope ruling,
#   NOT used here.
#
# FACED FILL LAW (ruled 2026-08-17, supersedes "observed only,
#   no imputation"): every gated runner who PLAYED vs NE's defense
#   gets a percentile -- earned 2025 currency pctl if season-gated,
#   else the entry-year prior price (pr_grun / pr_mtf). NOBODY drops
#   from the faced denominator. fill_share = share of faced attempts
#   priced at the prior -- the honesty number (replaces
#   unscored_share). The Echols ruling, uniform across units.
#   ALL PLAYOFF WEEKS ARE INCLUDED. Currency data STAYS NA -- the
#   fill lives only in the comparison frames.
#
# SOURCE ORDER:
#   (1) step-0 file in session: rushing_qbgrp (raw + qbgrp/def ids,
#       HB-only, canonical team fixes incl LAC-2016 -> SD,
#       LV <= 2019 -> OAK)
#   (2) prush pipeline objects: combined_ids (offense crosswalk),
#       pff_team_lookup, opp_2026_teams, sched_2026, blend2,
#       in_season, percent_rank_avg
#   (3) this file, top to bottom. Re-source WHOLE file after any edit.
#
# Direction: opposing RUNNERS vs NE's RUN DEFENSE.
#   higher pctl = better runner = HARDER for NE's run defense.
#   First OFFENSE-side asset in the series. All five metrics are
#   "higher = better for the runner" -- NOTHING is negated.
#   Object names carry _rb suffixes so Phase 3 + Phase 4
#   workspaces can coexist in the six-part rollup.
# ============================================================

library(dplyr); library(tidyr); library(stringr)

needed_p4 <- c("rushing_qbgrp", "combined_ids", "pff_team_lookup",
               "opp_2026_teams", "sched_2026", "blend2", "in_season")
missing_p4 <- needed_p4[!vapply(needed_p4, exists, logical(1))]
if (length(missing_p4)) stop("missing session objects: ",
                             paste(missing_p4, collapse = ", "),
                             " -- see SOURCE ORDER in header")

# tibble-at-birth (RAQ returns base data.frames; print(n=) dies on them)
rushing_qbgrp <- tibble::as_tibble(rushing_qbgrp)
combined_ids  <- tibble::as_tibble(combined_ids)

if (!exists("percent_rank_avg"))
  percent_rank_avg <- function(x) {          # fallback only; upstream wins
    r <- rank(x, ties.method = "average", na.last = "keep")
    r / sum(!is.na(x))
  }

# ------------------------------------------------------------
# 1. constants (ruled 2026-08-14)
# ------------------------------------------------------------

X_QUAL <- 4
G_MIN  <- 6
N_RB   <- 2L

# ------------------------------------------------------------
# 2. SoS RUSHING CURRENCY
#    qualifying games (>= X_QUAL attempts), single HB pool,
#    count rates = ratio-of-sums, grade = attempt-weighted,
#    percentiles within season. Direction law: higher = better
#    runner = HARDER for NE's run defense.
# ------------------------------------------------------------

rush_qual_games <- rushing_qbgrp %>%
  filter(attempts >= X_QUAL)

rush_season_pctl_sos <- rush_qual_games %>%
  group_by(player, player_id, season) %>%
  summarise(
    q_games = dplyr::n(),
    q_atts  = sum(attempts, na.rm = TRUE),
    grun    = weighted.mean(grades_run, w = attempts, na.rm = TRUE),
    ypa     = sum(yards, na.rm = TRUE) / q_atts,
    yco     = sum(yards_after_contact, na.rm = TRUE) / q_atts,
    mtf_rt  = sum(elu_rush_mtf, na.rm = TRUE) / q_atts,
    brk_rt  = sum(breakaway_attempts, na.rm = TRUE) / q_atts,
    .groups = "drop"
  ) %>%
  filter(q_games >= G_MIN) %>%
  group_by(season) %>%
  mutate(
    grun_pctl = percent_rank_avg(grun),
    ypa_pctl  = percent_rank_avg(ypa),
    yco_pctl  = percent_rank_avg(yco),
    mtf_pctl  = percent_rank_avg(mtf_rt),
    brk_pctl  = percent_rank_avg(brk_rt)
  ) %>%
  ungroup()

cat("\n--- currency validation: gated player-seasons per year ---\n")
print(rush_season_pctl_sos %>% count(season) %>% arrange(season), n = Inf)
cat("\n--- 2025 pool (expect 64 = 2.0 per team) ---\n")
print(rush_season_pctl_sos %>% filter(season == 2025) %>% nrow())

# ------------------------------------------------------------
# 2b. ORIENTATION RITUAL -- cor matrix + known-elite eyeball.
#     Pruning rule (prush law): |r| >= ~.85 -> one rides to the
#     table, not the facets. Facet default: grade + MTF rate
#     (MTF floored at all 16 gate cells in step 0; breakaway
#     rides with the noise caveat; yco/ypa table-only).
# ------------------------------------------------------------

cor_mat_rb <- rush_season_pctl_sos %>%
  filter(season == 2025) %>%
  select(grun, yco, mtf_rt, brk_rt, ypa) %>%
  cor(use = "pairwise.complete.obs")
cat("\n--- metric cor matrix, 2025 gated (raw values; all: HIGH = good) ---\n")
print(round(cor_mat_rb, 3))

cat("\n--- known-elite eyeball: top 10 by grun_pctl, 2025 ---\n")
print(rush_season_pctl_sos %>%
        filter(season == 2025) %>%
        slice_max(grun_pctl, n = 10, with_ties = FALSE) %>%
        arrange(desc(grun_pctl)) %>%
        select(player, q_games, q_atts,
               grun_pctl, mtf_pctl, yco_pctl, brk_pctl, ypa_pctl),
      n = Inf)
cat("\n--- same, top 10 by mtf_pctl (the floored count rate) ---\n")
print(rush_season_pctl_sos %>%
        filter(season == 2025) %>%
        slice_max(mtf_pctl, n = 10, with_ties = FALSE) %>%
        arrange(desc(mtf_pctl)) %>%
        select(player, q_games, q_atts, mtf_pctl, grun_pctl, brk_pctl),
      n = Inf)

# ------------------------------------------------------------
# 3. offense crosswalk + 2026 rosters (backs)
#    nflreadr 2026 coarsens offense to QB / RB / WR / TE / OL --
#    backs are "RB" (213 of them; no HB/FB strings exist). FBs
#    ride under "RB" if present; usage ordering kills pure FBs.
# ------------------------------------------------------------

off_xwalk <- combined_ids %>%
  filter(!is.na(gsis_id)) %>%
  distinct(player_id, gsis_id)

stopifnot(anyDuplicated(off_xwalk$gsis_id)   == 0,
          anyDuplicated(off_xwalk$player_id) == 0)

off_2026 <- nflreadr::load_rosters(2026) %>%
  filter(position == "RB") %>%
  transmute(gsis_id,
            roster_name = full_name,
            team_name = dplyr::coalesce(pff_team_lookup[team], team),
            entry_year,
            roster_pos = position) %>%
  filter(team_name %in% opp_2026_teams) %>%
  left_join(off_xwalk, by = "gsis_id")

stopifnot(anyDuplicated(off_2026[, c("team_name", "roster_name")]) == 0)

cat("\n--- roster RBs pulled + matched to PFF ids (per team) ---\n")
print(off_2026 %>% group_by(team_name) %>%
        summarise(n_rb = dplyr::n(), matched = sum(!is.na(player_id)),
                  .groups = "drop") %>%
        arrange(match(team_name, opp_2026_teams)), n = Inf)

# ------------------------------------------------------------
# 4. usage, pctls, status flags (single pool -- no band law)
# ------------------------------------------------------------

rush_usage <- rush_qual_games %>%
  filter(season %in% c(2024, 2025)) %>%
  group_by(player_id, season) %>%
  summarise(att = sum(attempts),
            qg = dplyr::n(), .groups = "drop") %>%
  pivot_wider(names_from = season, values_from = c(att, qg))

pctl_25 <- rush_season_pctl_sos %>% filter(season == 2025) %>%
  select(player_id, grun_25 = grun_pctl, mtf_25 = mtf_pctl)
pctl_24 <- rush_season_pctl_sos %>% filter(season == 2024) %>%
  select(player_id, grun_24 = grun_pctl, mtf_24 = mtf_pctl)

ledger_base <- off_2026 %>%
  left_join(rush_usage, by = "player_id") %>%
  left_join(pctl_25,    by = "player_id") %>%
  left_join(pctl_24,    by = "player_id") %>%
  mutate(status = case_when(
    entry_year == 2026                          ~ "rookie",
    is.na(player_id)                            ~ "no_pff_id",
    !is.na(grun_25)                             ~ "has_2025_pctl",
    !is.na(grun_24)                             ~ "data_2024_only",
    dplyr::coalesce(att_2025, 0) > 0 |
      dplyr::coalesce(att_2024, 0) > 0          ~ "usage_no_pctl",
    TRUE                                        ~ "no_rush_history"
  ))

cat("\n--- no-history players excluded from ledger (per team) ---\n")
print(ledger_base %>% filter(status == "no_rush_history") %>%
        count(team_name), n = Inf)

ledger <- ledger_base %>% filter(status != "no_rush_history")

# ------------------------------------------------------------
# 5. committee proposals: top N_RB per team by pmax('25,'24)
#    qualifying attempts. N_RB = 2 ruled 2026-08-14.
# ------------------------------------------------------------

ledger <- ledger %>%
  mutate(usage_ord = pmax(dplyr::coalesce(att_2025, 0),
                          dplyr::coalesce(att_2024, 0))) %>%
  group_by(team_name) %>%
  arrange(desc(usage_ord), .by_group = TRUE) %>%
  mutate(rb_rank  = dplyr::row_number(),
         proposed = rb_rank <= N_RB) %>%
  ungroup()

# ------------------------------------------------------------
# 6. THE RULING LEDGER
# ------------------------------------------------------------

cat("\n================ STEP 0 -- THE RULING LEDGER ================\n")
cat("proposed = TRUE -> auto-committee (top 2 by '25/'24 attempts)\n")
cat("Andy rules ALL deltas via tribble below: adds (rookies/signings),\n")
cat("drops (injuries/cuts). Single band -- adds carry NO band call.\n\n")

ledger_print <- ledger %>%
  arrange(match(team_name, opp_2026_teams), desc(usage_ord)) %>%
  select(team_name, roster_name, status, proposed,
         att25 = att_2025, att24 = att_2024,
         grun_25, mtf_25, grun_24, mtf_24)
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

cat("\n--- 2025 committee shapes (FYI; membership is uniform N_RB) ---\n")
n_rb_2025 <- rush_qual_games %>%
  filter(season == 2025) %>%
  count(team, player_id, name = "g") %>%
  filter(g >= G_MIN) %>%
  count(team, name = "n_2025") %>%
  rename(team_name = team) %>%
  filter(team_name %in% opp_2026_teams) %>%
  arrange(match(team_name, opp_2026_teams))
print(n_rb_2025, n = Inf)

# ------------------------------------------------------------
# STOP -- ruling boundary. Deltas happen in section 7.
# ------------------------------------------------------------

# ------------------------------------------------------------
# 7. ANDY'S DELTAS -- the only judgment layer.
#    EMPTY = full sign-off (legal + complete). Actions: add / drop.
#    Phantoms are AUTOMATIC (section 9 fills every team to N_RB
#    at the entry-year prior). Single band: no band column.
#    Names must match off_2026 spelling exactly -- the gate fails loudly.
# ------------------------------------------------------------

opp_rush_2026_deltas <- tribble(
  ~team_name, ~roster_name, ~action, ~note
  # "MIA", "<name>", "add", "example - rookie into the committee",
)

d_miss <- opp_rush_2026_deltas %>%
  anti_join(off_2026, by = c("team_name", "roster_name"))
if (nrow(d_miss) > 0) {
  print(d_miss)
  stop("delta names not on 2026 rosters - fix spelling vs the ledger")
}

cat("\n--- deltas registered:", nrow(opp_rush_2026_deltas), "row(s) ---\n")
if (nrow(opp_rush_2026_deltas) > 0) print(opp_rush_2026_deltas, n = Inf)

# ------------------------------------------------------------
# 8. FACED 2025 SIDE -- FACED FILL LAW (uniform, ruled 2026-08-17).
#    Every gated runner game vs NE's DEFENSE (def_ssn), weighted by
#    that game's attempts. Currency pctls STAY NA in the data; at
#    COMPARISON time every runner who played gets a price -- earned
#    2025 pctl if season-gated, else the entry-year prior. fill_share
#    = share of faced attempts priced at the prior (the honesty
#    number, replaces unscored_share). ALL PLAYOFF WEEKS INCLUDED.
#    Priors live HERE now (moved up from section 9) so the fill can
#    use them; the projection below reuses the same objects.
# ------------------------------------------------------------

entry_years_off <- nflreadr::load_rosters(2017:2025) %>%
  filter(!is.na(gsis_id), !is.na(entry_year)) %>%
  group_by(gsis_id) %>%
  summarise(entry_year = min(entry_year), .groups = "drop") %>%
  inner_join(off_xwalk, by = "gsis_id") %>%
  select(player_id, entry_year)

rush_rookie_prior <- rush_season_pctl_sos %>%
  inner_join(entry_years_off, by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2017) %>%
  summarise(pr_grun = median(grun_pctl, na.rm = TRUE),
            pr_mtf  = median(mtf_pctl,  na.rm = TRUE))

cat("\n--- entry-year prior (fill prices for no-pctl runners) ---\n")
print(rush_rookie_prior)

faced_games_2025 <- rushing_qbgrp %>%
  filter(startsWith(def_ssn, "NE"), season == 2025, in_season(week),
         attempts >= X_QUAL) %>%
  transmute(player_id, player, team, week, g_att = attempts) %>%
  left_join(rush_season_pctl_sos %>% filter(season == 2025) %>%
              select(player_id, g25 = grun_pctl, m25 = mtf_pctl),
            by = "player_id") %>%
  mutate(fill = is.na(g25),
         g25  = dplyr::coalesce(g25, rush_rookie_prior$pr_grun),
         m25  = dplyr::coalesce(m25, rush_rookie_prior$pr_mtf))

cat("\n--- faced 2025: weeks present (expect 21 incl playoffs) ---\n")
print(faced_games_2025 %>% distinct(week) %>% arrange(week), n = Inf)

faced_rb_2025 <- faced_games_2025 %>%
  summarise(runner_games  = dplyr::n(),
            tot_att       = sum(g_att),
            fill_share    = sum(g_att[fill]) / tot_att,
            grun_25 = weighted.mean(g25, w = g_att, na.rm = TRUE),
            mtf_25  = weighted.mean(m25, w = g_att, na.rm = TRUE))

cat("\n--- faced 2025 profile (fill_share = honesty number) ---\n")
print(faced_rb_2025)

faced_rb_team_2025 <- faced_games_2025 %>%
  group_by(team_name = team) %>%
  summarise(tot_att = sum(g_att),
            fill_share = sum(g_att[fill]) / tot_att,
            grun_25 = weighted.mean(g25, w = g_att, na.rm = TRUE),
            mtf_25  = weighted.mean(m25, w = g_att, na.rm = TRUE),
            .groups = "drop")

cat("\n--- faced 2025 per-team (ghost layer for Fig 1; NA = not faced) ---\n")
print(faced_rb_team_2025 %>% arrange(desc(grun_25)), n = Inf)

# ------------------------------------------------------------
# 9. 2026 PROJECTION -- signed committees + deltas + priors + phantoms.
#    Values: blend2('25,'24 pctl, w25 = q_games25/10). No-pctl members
#    (rookie / under-gate vet / phantom) = entry-year prior (computed
#    in section 8 above for the fill law), flagged.
#    Weights: pmax('25,'24 attempts) -> renormalized carry shares.
# ------------------------------------------------------------

# 2025 slot-usage curve: phantom weights inherit the team's own vacated
# slot; league median at that rank is the fallback
slot_usage_2025 <- rush_qual_games %>%
  filter(season == 2025) %>%
  group_by(team, player_id) %>%
  summarise(g = dplyr::n(),
            att = sum(attempts), .groups = "drop") %>%
  filter(g >= G_MIN) %>%
  group_by(team) %>%
  arrange(desc(att), .by_group = TRUE) %>%
  mutate(slot_rk = dplyr::row_number()) %>%
  ungroup() %>%
  select(team_name = team, slot_rk, att_slot = att)

slot_usage_med <- slot_usage_2025 %>%
  group_by(slot_rk) %>%
  summarise(att_med = median(att_slot), .groups = "drop")
fallback_att <- min(slot_usage_2025$att_slot)

# real members: proposals minus drops plus named adds
p25_cur <- rush_season_pctl_sos %>% filter(season == 2025) %>%
  select(player_id, g25 = grun_pctl, m25 = mtf_pctl)
p24_cur <- rush_season_pctl_sos %>% filter(season == 2024) %>%
  select(player_id, g24 = grun_pctl, m24 = mtf_pctl)

adds_prepped <- opp_rush_2026_deltas %>%
  filter(action == "add") %>%
  left_join(ledger_base %>%
              select(team_name, roster_name, player_id,
                     att_2025, att_2024, qg_2025, status),
            by = c("team_name", "roster_name")) %>%
  select(team_name, roster_name, player_id,
         att_2025, att_2024, qg_2025, status)

rot_real <- ledger %>%
  filter(proposed) %>%
  select(team_name, roster_name, player_id,
         att_2025, att_2024, qg_2025, status) %>%
  anti_join(opp_rush_2026_deltas %>% filter(action == "drop"),
            by = c("team_name", "roster_name")) %>%
  bind_rows(adds_prepped)

stopifnot(anyDuplicated(rot_real[, c("team_name", "roster_name")]) == 0)

rot_real <- rot_real %>%
  left_join(p25_cur, by = "player_id") %>%
  left_join(p24_cur, by = "player_id") %>%
  mutate(w25      = pmin(dplyr::coalesce(qg_2025, 0L) / 10, 1),
         grun_bl  = blend2(g25, g24, w25),
         mtf_bl   = blend2(m25, m24, w25),
         prior_used = is.na(grun_bl),
         grun_f   = dplyr::coalesce(grun_bl, rush_rookie_prior$pr_grun),
         mtf_f    = dplyr::coalesce(mtf_bl,  rush_rookie_prior$pr_mtf),
         usage_w  = pmax(dplyr::coalesce(att_2025, 0),
                         dplyr::coalesce(att_2024, 0))) %>%
  select(team_name, roster_name, status, prior_used,
         usage_w, grun_f, mtf_f)

# cap at N_RB, then slot-weight inheritance
rot_real <- rot_real %>%
  mutate(usage_w = if_else(usage_w > 0, usage_w, NA_real_)) %>%
  group_by(team_name) %>%
  arrange(desc(dplyr::coalesce(usage_w, 0)), .by_group = TRUE) %>%
  mutate(rb_rank = dplyr::row_number()) %>%
  filter(rb_rank <= N_RB) %>%
  ungroup() %>%
  left_join(slot_usage_2025,
            by = c("team_name", "rb_rank" = "slot_rk")) %>%
  left_join(slot_usage_med, by = c("rb_rank" = "slot_rk")) %>%
  mutate(usage_w = dplyr::coalesce(usage_w, att_slot, att_med, fallback_att)) %>%
  select(team_name, roster_name, status, prior_used,
         usage_w, grun_f, mtf_f)

auto_phantoms <- rot_real %>%
  count(team_name, name = "n_real") %>%
  tidyr::complete(team_name = opp_2026_teams, fill = list(n_real = 0L)) %>%
  filter(n_real < N_RB) %>%
  mutate(slot_rk = purrr::map2(n_real, N_RB, ~ seq(.x + 1L, .y))) %>%
  tidyr::unnest(slot_rk) %>%
  left_join(slot_usage_2025, by = c("team_name", "slot_rk")) %>%
  left_join(slot_usage_med,  by = "slot_rk") %>%
  transmute(team_name,
            roster_name = paste0("PHANTOM_RB", slot_rk),
            status = "phantom", prior_used = TRUE,
            usage_w = dplyr::coalesce(att_slot, att_med, fallback_att),
            grun_f = rush_rookie_prior$pr_grun,
            mtf_f  = rush_rookie_prior$pr_mtf)

rot_2026_rb <- bind_rows(rot_real, auto_phantoms)

# every team is exactly N_RB deep, no exceptions
stopifnot(all(dplyr::count(rot_2026_rb, team_name)$n == N_RB),
          nrow(rot_2026_rb) == length(opp_2026_teams) * N_RB)

cat("\n--- 2026 committees, final (prior_used = valued at entry-year prior) ---\n")
print(rot_2026_rb %>%
        arrange(match(team_name, opp_2026_teams), desc(usage_w)) %>%
        select(team_name, roster_name, status, prior_used, usage_w,
               grun_f, mtf_f), n = Inf)

team_rb_2026 <- rot_2026_rb %>%
  group_by(team_name) %>%
  summarise(n_members    = dplyr::n(),
            interp_share = sum(usage_w[prior_used]) / sum(usage_w),
            grun_26 = weighted.mean(grun_f, w = usage_w),
            mtf_26  = weighted.mean(mtf_f,  w = usage_w),
            .groups = "drop")

cat("\n--- team 2026 committee values + interp_share honesty column ---\n")
print(team_rb_2026 %>%
        arrange(match(team_name, opp_2026_teams)), n = Inf)

# schedule-weighted slate (division x2, same as run-D / prush)
slate_rb_2026 <- tibble(team_name = sched_2026) %>%
  left_join(team_rb_2026, by = "team_name", relationship = "many-to-many") %>%
  summarise(grun_26 = mean(grun_26), mtf_26 = mean(mtf_26),
            interp_share = mean(interp_share))

cmp_rush_slate <- faced_rb_2025 %>%
  select(grun_25, mtf_25, fill_share_25 = fill_share) %>%
  bind_cols(slate_rb_2026) %>%
  mutate(d_grun = grun_26 - grun_25,
         d_mtf  = mtf_26  - mtf_25)

cat("\n--- THE ANSWER: 2026 vs 2025 opposing-RB slate (division x2) ---\n")
print(cmp_rush_slate, n = Inf)

# ------------------------------------------------------------
# 10. FIG 1 -- opponents x facets (grade + MTF per the ruling;
#     cor matrix printed in 2b -- Andy can veto a facet). Single
#     band, so the band axis becomes the opponent axis; ghost =
#     what NE faced from THAT team in 2025 (absent = not faced).
#     Division x2 lives in the slate row above, not this panel.
# ------------------------------------------------------------

library(ggplot2)

team_cmp_rb <- team_rb_2026 %>%
  left_join(faced_rb_team_2025 %>% select(team_name, grun_25, mtf_25),
            by = "team_name") %>%
  mutate(d_grun = grun_26 - grun_25,
         d_mtf  = mtf_26  - mtf_25) %>%
  arrange(desc(grun_26))

rush_fig1_pd <- team_cmp_rb %>%
  select(team_name, grun_25, grun_26, mtf_25, mtf_26) %>%
  pivot_longer(-team_name, names_to = c("metric", "yr"),
               names_pattern = "(grun|mtf)_(25|26)", values_to = "pctl") %>%
  pivot_wider(names_from = yr, values_from = pctl, names_prefix = "y") %>%
  mutate(team_name = factor(team_name,
                            levels = team_cmp_rb %>% arrange(grun_26) %>%
                              pull(team_name)),
         metric = factor(metric, levels = c("grun", "mtf"),
                         labels = c("Run grade", "Missed-tackle rate")))

plot_rush_slate <- ggplot(rush_fig1_pd, aes(y = team_name)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey45") +
  geom_segment(data = rush_fig1_pd %>% filter(!is.na(y25)),
               aes(x = y25, xend = y26, yend = team_name),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               linewidth = 1, color = "grey60") +
  geom_point(aes(x = y25), shape = 1, size = 3.2, stroke = 1.2, color = "grey55",
             na.rm = TRUE) +
  geom_point(aes(x = y26), shape = 16, size = 2.6, color = "#002244") +
  facet_wrap(~ metric, nrow = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(title = "NE defense - 2026 vs 2025 opposing-RB slate, by opponent",
       subtitle = "open circle = 2025 backs faced (attempt-weighted; absent = not faced) | solid navy = 2026 projected committee | higher = HARDER for NE's run defense | dashed = league median",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey40", size = 8.5),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", size = 9),
        panel.spacing.x = unit(1.1, "lines"))

plot_rush_slate

# ------------------------------------------------------------
# 11. FIG-1 COMPANION TABLE. interp_share = the honesty column.
#     gt attached HERE, not inherited: save.image restores objects,
#     never library() state. Per-team view (single band); the
#     division-x2 slate row is the console print above.
# ------------------------------------------------------------

library(gt)

rush_d_dom <- max(abs(c(team_cmp_rb$d_grun, team_cmp_rb$d_mtf)), na.rm = TRUE)

rush_slate_gt <- team_cmp_rb %>%
  transmute(team_name,
            g25 = grun_25, g26 = grun_26, gd = d_grun,
            m25 = mtf_25, m26 = mtf_26, md = d_mtf,
            interp_share) %>%
  gt() %>%
  tab_spanner(label = "Run grade", columns = c(g25, g26, gd)) %>%
  tab_spanner(label = "MTF rate",  columns = c(m25, m26, md)) %>%
  cols_label(team_name = "", g25 = "'25", g26 = "'26", gd = "D",
             m25 = "'25", m26 = "'26", md = "D",
             interp_share = "interp %") %>%
  fmt_percent(columns = c(g25, g26, m25, m26, interp_share), decimals = 0) %>%
  fmt_percent(columns = c(gd, md), decimals = 0, force_sign = TRUE) %>%
  sub_missing(columns = everything(), missing_text = "--") %>%
  data_color(columns = c(gd, md),
             fn = scales::col_numeric(c("#6baed6", "#f7f7f7", "#C60C30"),
                                      domain = c(-rush_d_dom, rush_d_dom)),
             autocolor_text = TRUE) %>%
  tab_header(title = "2026 opposing-RB committees vs the 2025 backs NE faced",
             subtitle = "attempt-share-weighted committee means, sorted by '26 run grade | higher = harder for NE's run defense | D red = harder | '--' = not faced in 2025 | interp % = share of projected carries riding on entry-year priors | slate row (division x2) prints in console") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold")

rush_slate_gt

# ------------------------------------------------------------
# STOP 2 -- player panels (v2 grammar) + 3-season table build AFTER
# the Fig-1 eyeball, on request. Checkpoint (objects for the
# six-part rollup: rush_season_pctl_sos, rot_2026_rb, faced_rb_2025,
# faced_rb_team_2025, team_rb_2026, cmp_rush_slate):
# ------------------------------------------------------------
# ggsave("rush_fig1_slate.png", plot_rush_slate, width = 9, height = 5.5, dpi = 200)
# gtsave(rush_slate_gt, "rush_slate_table.png", vwidth = 800)
# save.image("~/ne_rush_sos_workspace.RData")
# system('aws s3 cp ~/ne_rush_sos_workspace.RData s3://nfl-pff-data-lucas/workspaces/')



# ============================================================
# NE 2026 -- SoS PER-TEAM + COMPOSITE PACK (Phases 3/4 add-on)
# Andy ruling 2026-08-14: (1) RB slate gets a COMPOSITE per team
#   (equal-weight mean of run-grade pctl + MTF pctl -- both facets
#   are floored/anchored, so no weighting games); (2) the banded
#   units get PER-TEAM views too -- run defense here, pass rush
#   after the object hunt at the bottom prints.
#
# COLLISION NOTE: the Phase 4 build overwrote session X_QUAL/G_MIN
#   (now 4/6). The run-D faced rebuild below uses the run-D gate
#   LITERALS (3 run snaps / 6 games). Do not "parameterize" this.
#
# SOURCE ORDER: Phase 3 build + Phase 4 build already sourced in
#   this session. Needed: team_rb_2026, faced_rb_team_2025,
#   cmp_rush_slate, team_cmp_rb (rebuilt below anyway),
#   run_defense_qbgrp, rundef_season_pctl_sos, team_band_2026,
#   opp_2026_teams, in_season, percent_rank_avg.
#   Re-source WHOLE file after any edit.
# ============================================================

library(dplyr); library(tidyr); library(stringr)
library(ggplot2); library(gt)

needed_pk <- c("team_rb_2026", "faced_rb_team_2025", "cmp_rush_slate",
               "run_defense_qbgrp", "rundef_season_pctl_sos",
               "team_band_2026", "opp_2026_teams", "in_season")
missing_pk <- needed_pk[!vapply(needed_pk, exists, logical(1))]
if (length(missing_pk)) stop("missing session objects: ",
                             paste(missing_pk, collapse = ", "),
                             " -- re-source the Phase 3 / Phase 4 build files first")

run_defense_qbgrp      <- tibble::as_tibble(run_defense_qbgrp)
rundef_season_pctl_sos <- tibble::as_tibble(rundef_season_pctl_sos)
team_band_2026         <- tibble::as_tibble(team_band_2026)

# ------------------------------------------------------------
# 1. RB COMPOSITE -- per team + slate row.
#    comp = mean(grun_pctl, mtf_pctl). Both facets required
#    (NA if either missing -- not-faced teams stay NA).
# ------------------------------------------------------------

team_cmp_rb <- team_rb_2026 %>%
  left_join(faced_rb_team_2025 %>% select(team_name, grun_25, mtf_25),
            by = "team_name") %>%
  mutate(comp_25 = (grun_25 + mtf_25) / 2,
         comp_26 = (grun_26 + mtf_26) / 2,
         d_comp  = comp_26 - comp_25) %>%
  arrange(desc(comp_26))

cat("\n--- RB COMPOSITE, per team (comp = mean of grade + MTF pctls) ---\n")
print(team_cmp_rb %>% select(team_name, comp_25, comp_26, d_comp,
                             interp_share), n = Inf)

cat("\n--- RB COMPOSITE, slate row (division x2 via cmp_rush_slate) ---\n")
cmp_rush_slate_comp <- cmp_rush_slate %>%
  mutate(comp_25 = (grun_25 + mtf_25) / 2,
         comp_26 = (grun_26 + mtf_26) / 2,
         d_comp  = comp_26 - comp_25) %>%
  select(comp_25, comp_26, d_comp, interp_share, fill_share_25)
print(cmp_rush_slate_comp)

# composite dumbbell (ghost = faced '25, navy = '26 committee)
rush_comp_pd <- team_cmp_rb %>%
  select(team_name, y25 = comp_25, y26 = comp_26) %>%
  mutate(team_name = factor(team_name,
                            levels = team_cmp_rb %>% arrange(comp_26) %>%
                              pull(team_name)))

plot_rush_comp <- ggplot(rush_comp_pd, aes(y = team_name)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey45") +
  geom_segment(data = rush_comp_pd %>% filter(!is.na(y25)),
               aes(x = y25, xend = y26, yend = team_name),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               linewidth = 1, color = "grey60") +
  geom_point(aes(x = y25), shape = 1, size = 3.2, stroke = 1.2,
             color = "grey55", na.rm = TRUE) +
  geom_point(aes(x = y26), shape = 16, size = 2.6, color = "#002244") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(title = "NE defense - 2026 vs 2025 opposing-RB slate, COMPOSITE",
       subtitle = "composite = mean of run-grade and missed-tackle-rate percentiles | open circle = 2025 backs faced (absent = not faced) | solid navy = 2026 committee | higher = HARDER",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey40", size = 8.5),
        panel.grid.major.y = element_blank())

plot_rush_comp

rush_comp_gt <- team_cmp_rb %>%
  transmute(team_name, c25 = comp_25, c26 = comp_26, cd = d_comp,
            interp_share) %>%
  gt() %>%
  cols_label(team_name = "", c25 = "'25", c26 = "'26", cd = "D",
             interp_share = "interp %") %>%
  fmt_percent(columns = c(c25, c26, interp_share), decimals = 0) %>%
  fmt_percent(columns = cd, decimals = 0, force_sign = TRUE) %>%
  sub_missing(columns = everything(), missing_text = "--") %>%
  data_color(columns = cd,
             fn = scales::col_numeric(c("#6baed6", "#f7f7f7", "#C60C30"),
                                      domain = c(-max(abs(team_cmp_rb$d_comp), na.rm = TRUE),
                                                 max(abs(team_cmp_rb$d_comp), na.rm = TRUE))),
             autocolor_text = TRUE) %>%
  tab_header(title = "2026 opposing-RB slate -- the composite number",
             subtitle = "mean of run-grade and MTF percentiles, attempt-share-weighted committees, sorted by '26 | higher = harder for NE's run defense | D red = harder | '--' = not faced in 2025 | interp % = share of projected carries riding on entry-year priors") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold")

rush_comp_gt

# ------------------------------------------------------------
# Checkpoint (after eyeball):
# ------------------------------------------------------------
# ggsave("rush_fig_composite.png", plot_rush_comp, width = 7, height = 5.5, dpi = 200)
# gtsave(rush_comp_gt, "rush_composite_table.png", vwidth = 640)
# save.image("~/ne_rush_sos_workspace.RData")
# system('aws s3 cp ~/ne_rush_sos_workspace.RData s3://nfl-pff-data-lucas/workspaces/')

##
## ============================================================================
## BUILT-IN TEST -- RUSHING LEAGUE DIFFERENTIAL + NE REPRODUCTION WALL
##   The standalone league screen, folded into the build file
##   (2026-08-17): ONE file per unit, the tests ship inside. FACED
##   FILL LAW in force; ALL PLAYOFF WEEKS included on the faced side;
##   slate side REG-only (the canon asymmetry). Everything below
##   carries engine suffixes -- zero collision with the build above.
## ============================================================================

needed_rb <- c("rushing_qbgrp", "combined_ids", "pff_team_lookup",
               "blend2", "percent_rank_avg", "cmp_rush_slate")
missing_rb <- needed_rb[!vapply(needed_rb, exists, logical(1))]
if (length(missing_rb)) stop("missing: ", paste(missing_rb, collapse = ", "))

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(tibble)
})

rushing_qbgrp <- tibble::as_tibble(rushing_qbgrp)
combined_ids  <- tibble::as_tibble(combined_ids)

X_RB <- 4; G_RB <- 6; N_RB <- 2L

# nflverse codes the canon lookup predates or never carried (same scar as
# prush): new-style AZ/LAR/JAC/WSH AND old-style ARI/BAL/CLE/HOU -- the raw
# 2026 nflverse file uses the old style. canon lookup wins first; patch
# fills gaps.
lookup_patch_rb <- c("ARI" = "ARZ", "AZ" = "ARZ", "BAL" = "BLT",
                     "CLE" = "CLV", "HOU" = "HST", "LAR" = "LA",
                     "JAC" = "JAX", "WSH" = "WAS")
to_pff_rb <- function(x) unname(dplyr::coalesce(pff_team_lookup[x],
                                                lookup_patch_rb[x], x))

# ---- currency, rebuilt locally (grade anchor + MTF facet) ----
qual_rb_lg <- rushing_qbgrp %>%
  filter(attempts >= X_RB)

cur_rb_lg <- qual_rb_lg %>%
  group_by(player, player_id, season) %>%
  summarise(q_games = dplyr::n(),
            q_atts  = sum(attempts, na.rm = TRUE),
            grun    = weighted.mean(grades_run, w = attempts, na.rm = TRUE),
            mtf_rt  = sum(elu_rush_mtf, na.rm = TRUE) / q_atts,
            .groups = "drop") %>%
  filter(q_games >= G_RB) %>%
  group_by(season) %>%
  mutate(grun_pctl = percent_rank_avg(grun),
         mtf_pctl  = percent_rank_avg(mtf_rt)) %>%
  ungroup()

cat("[0] gated HB player-seasons per year:\n")
print(cur_rb_lg %>% dplyr::count(season) %>% arrange(season) %>% as.data.frame())
cat("[0] 2025 pool (canon receipt: 64 = 2.0/team):",
    cur_rb_lg %>% filter(season == 2025) %>% nrow(), "\n")

# ---- crosswalk + 32-team rosters (backs) ----
xw_rb_lg <- combined_ids %>%
  filter(!is.na(gsis_id)) %>%
  distinct(player_id, gsis_id)
stopifnot(anyDuplicated(xw_rb_lg$gsis_id)   == 0,
          anyDuplicated(xw_rb_lg$player_id) == 0)

ros_rb_lg <- nflreadr::load_rosters(2026) %>%
  filter(position == "RB") %>%
  transmute(gsis_id,
            roster_name = full_name,
            team = to_pff_rb(team),
            entry_year) %>%
  left_join(xw_rb_lg, by = "gsis_id")
stopifnot(anyDuplicated(ros_rb_lg[, c("team", "roster_name")]) == 0)

# ---- usage / pctls / prior / slot curve (single pool -- no band law) ----
use_rb_lg <- qual_rb_lg %>%
  filter(season %in% c(2024, 2025)) %>%
  group_by(player_id, season) %>%
  summarise(att = sum(attempts), qg = dplyr::n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = season, values_from = c(att, qg))

p_rb_lg <- cur_rb_lg %>%
  filter(season %in% c(2024, 2025)) %>%
  select(player_id, season, grun_pctl, mtf_pctl) %>%
  tidyr::pivot_wider(names_from = season,
                     values_from = c(grun_pctl, mtf_pctl))

ent_rb_lg <- nflreadr::load_rosters(2017:2025) %>%
  filter(!is.na(gsis_id), !is.na(entry_year)) %>%
  group_by(gsis_id) %>%
  summarise(entry_year = min(entry_year), .groups = "drop") %>%
  inner_join(xw_rb_lg, by = "gsis_id") %>%
  select(player_id, entry_year)

prior_rb_lg <- cur_rb_lg %>%
  inner_join(ent_rb_lg, by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2017) %>%
  summarise(pr_grun = median(grun_pctl, na.rm = TRUE),
            pr_mtf  = median(mtf_pctl,  na.rm = TRUE))
cat("[0] entry-year prior (single pool):\n"); print(as.data.frame(prior_rb_lg))

slot_rb_lg <- qual_rb_lg %>%
  filter(season == 2025) %>%
  group_by(team, player_id) %>%
  summarise(g = dplyr::n(), att = sum(attempts), .groups = "drop") %>%
  filter(g >= G_RB) %>%
  group_by(team) %>%
  arrange(desc(att), .by_group = TRUE) %>%
  mutate(slot_rk = dplyr::row_number()) %>%
  ungroup() %>%
  select(team, slot_rk, att_slot = att)

slot_med_rb_lg <- slot_rb_lg %>%
  group_by(slot_rk) %>%
  summarise(att_med = median(att_slot), .groups = "drop")
fb_rb_lg <- min(slot_rb_lg$att_slot)

pff32_rb <- sort(unique(slot_rb_lg$team))
stopifnot(length(pff32_rb) == 32L)

# roster code wall BEFORE any filtering
ros_teams <- sort(unique(ros_rb_lg$team))
if (!setequal(ros_teams, pff32_rb)) {
  cat("roster-vs-currency code mismatch:\n")
  cat("roster-only:", setdiff(ros_teams, pff32_rb), "\n")
  cat("currency-only:", setdiff(pff32_rb, ros_teams), "\n")
  stop("team code alignment failed (rosters)")
}

# ---- 32-team committees: top-2 by pmax attempts + auto-phantoms ----
rot_rb_lg <- ros_rb_lg %>%
  left_join(use_rb_lg, by = "player_id") %>%
  left_join(p_rb_lg,  by = "player_id") %>%
  mutate(uo = pmax(dplyr::coalesce(att_2025, 0),
                   dplyr::coalesce(att_2024, 0))) %>%
  group_by(team) %>%
  arrange(desc(uo), .by_group = TRUE) %>%
  mutate(rk = dplyr::row_number()) %>%
  filter(rk <= N_RB) %>%
  ungroup() %>%
  left_join(slot_rb_lg,     by = c("team", "rk" = "slot_rk")) %>%
  left_join(slot_med_rb_lg, by = c("rk" = "slot_rk")) %>%
  mutate(w25 = pmin(dplyr::coalesce(qg_2025, 0L) / 10, 1),
         grun_f = dplyr::coalesce(blend2(grun_pctl_2025, grun_pctl_2024, w25),
                                  prior_rb_lg$pr_grun),
         mtf_f  = dplyr::coalesce(blend2(mtf_pctl_2025, mtf_pctl_2024, w25),
                                  prior_rb_lg$pr_mtf),
         prior_used = is.na(blend2(grun_pctl_2025, grun_pctl_2024, w25)),
         uw  = dplyr::if_else(uo > 0, uo,
                              dplyr::coalesce(att_slot, att_med, fb_rb_lg))) %>%
  select(team, roster_name, prior_used, gf = grun_f, mf = mtf_f, uw)

ph_rb_lg <- rot_rb_lg %>%
  count(team, name = "nr") %>%
  tidyr::complete(team = pff32_rb, fill = list(nr = 0L)) %>%
  filter(nr < N_RB) %>%
  mutate(slot_rk = purrr::map2(nr, N_RB, ~ seq(.x + 1L, .y))) %>%
  tidyr::unnest(slot_rk) %>%
  left_join(slot_rb_lg,     by = c("team", "slot_rk")) %>%
  left_join(slot_med_rb_lg, by = "slot_rk") %>%
  transmute(team, roster_name = paste0("PHANTOM_RB", slot_rk),
            prior_used = TRUE,
            gf = prior_rb_lg$pr_grun, mf = prior_rb_lg$pr_mtf,
            uw = dplyr::coalesce(att_slot, att_med, fb_rb_lg))

rot_2026_rb_lg <- bind_rows(rot_rb_lg, ph_rb_lg)
stopifnot(all(dplyr::count(rot_2026_rb_lg, team)$n == N_RB),
          nrow(rot_2026_rb_lg) == 32L * N_RB)
cat("[3] rot_2026_rb_lg:", nrow(rot_2026_rb_lg),
    "rows | every team exactly", N_RB, "deep | phantoms:",
    sum(rot_2026_rb_lg$prior_used & grepl("PHANTOM", rot_2026_rb_lg$roster_name)), "\n")

team26_rb_lg <- rot_2026_rb_lg %>%
  group_by(team) %>%
  summarise(interp_share = sum(uw[prior_used]) / sum(uw),
            grun_26 = weighted.mean(gf, w = uw),
            mtf_26  = weighted.mean(mf, w = uw),
            .groups = "drop")
stopifnot(nrow(team26_rb_lg) == 32)

## ============================================================================
## 9. FACED 2025, INCLUDING PLAYOFFS -- PFF-NATIVE (rebuilt 2026-08-17)
##    identity: runner faced focal F's defense in week w <=> the row's
##    def_ssn == F2025 -- the same ssn parsing canon uses, extended to all
##    32 focals. week = PFF week; in_season() (the canon function) includes
##    the playoff weeks (PFF 28/29/30/32). NO nflverse bridge on the faced
##    side: nothing to download, nothing to mistranslate. The NE wall below
##    certifies the construction against cmp_rush_slate at 1e-8.
## ============================================================================

## -- 9e. faced games: every qualifying 2025 runner-game, attempts-weighted,
##    FACED FILL LAW at comparison time (fill = no season pctl -> entry-year
##    prior price; the currency data itself stays NA) -----------------------
faced_games_rb_lg <- qual_rb_lg %>%
  filter(season == 2025, in_season(week)) %>%
  transmute(player_id, week,
            focal = stringr::str_remove(def_ssn, "2025$"),
            team, attempts) %>%
  left_join(cur_rb_lg %>% filter(season == 2025) %>%
              select(player_id, grun_pctl, mtf_pctl),
            by = "player_id") %>%
  mutate(fill = is.na(grun_pctl),
         grun_pctl = dplyr::coalesce(grun_pctl, prior_rb_lg$pr_grun),
         mtf_pctl  = dplyr::coalesce(mtf_pctl,  prior_rb_lg$pr_mtf))

## -- WALL: every focal is a real PFF code ---------------------------------
stopifnot(all(unique(faced_games_rb_lg$focal) %in% pff32_rb))

## -- 9f. per-focal faced: grun_25 / mtf_25 + fill share (FILL LAW) ----------
##    fill = runner-game had no season pctl -> priced at the entry-year
##    prior at COMPARISON time; the currency data itself stays NA
faced_rb_lg <- faced_games_rb_lg %>%
  group_by(focal) %>%
  summarise(
    grun_25 = weighted.mean(grun_pctl, attempts, na.rm = TRUE),
    mtf_25  = weighted.mean(mtf_pctl,  attempts, na.rm = TRUE),
    att_faced     = sum(attempts),
    fill_share_25 = sum(attempts[fill]) / att_faced,
    n_weeks   = n_distinct(week),
    po_weeks  = n_distinct(week[week > 18]),
    .groups = "drop"
  )
stopifnot(nrow(faced_rb_lg) == 32L)
## -- WALL: NE faced exactly 21 weeks (canonical 2025 slate incl. playoffs) ---
stopifnot(faced_rb_lg %>% filter(focal == "NE") %>% pull(n_weeks) == 21L)
cat("playoff weeks in each focal's faced diet:\n")
print(faced_rb_lg %>% arrange(desc(po_weeks), focal) %>%
        select(focal, n_weeks, po_weeks) %>% as.data.frame())

## -- 9g. NE wall: reproduction of the canon faced side -----------------------
ne_rb_lg <- faced_rb_lg %>% filter(focal == "NE")
cat("NE faced side, league engine (incl. playoffs):\n")
print(ne_rb_lg %>% as.data.frame())
stopifnot(abs(ne_rb_lg$grun_25 - cmp_rush_slate$grun_25) < 1e-8,
          abs(ne_rb_lg$mtf_25  - cmp_rush_slate$mtf_25)  < 1e-8,
          abs(ne_rb_lg$fill_share_25 - cmp_rush_slate$fill_share_25) < 1e-8)
cat("NE wall passed: league faced side reproduces cmp_rush_slate within 1e-8\n")

## -- 9h. RECEIPT: what the REG-only convention was costing -------------------
ne_rb_reg <- faced_games_rb_lg %>%
  filter(week <= 18) %>%
  group_by(focal) %>%
  summarise(grun_25 = weighted.mean(grun_pctl, attempts, na.rm = TRUE),
            mtf_25  = weighted.mean(mtf_pctl, attempts, na.rm = TRUE),
            .groups = "drop") %>%
  filter(focal == "NE")
cat("NE faced side BEFORE/AFTER the playoff repair:\n")
cat(sprintf("  REG-only (WRONG):  grun_25=%.4f  mtf_25=%.4f\n",
            ne_rb_reg$grun_25, ne_rb_reg$mtf_25))
cat(sprintf("  incl. playoffs (canon): grun_25=%.4f  mtf_25=%.4f  (playoff weeks faced: %d)\n",
            ne_rb_lg$grun_25, ne_rb_lg$mtf_25, ne_rb_lg$po_weeks))

## ============================================================================
## 10. SLATE 2026 (REG ONLY -- 2026 playoff opponents are unknowable)
##     for each focal: the schedule-mean of opponents' team26 rushing units
## ============================================================================

## -- robust schedule load: nflreadr first; if its post-processing returns
##    junk (the 2026-08-17 "roof" stub), pull the raw nflverse assets direct
##    from the CURRENT repo -- nflverse-data, not nfldata (the old URL 404s
##    -- 2026-08-18 failure); games.csv mirror as the last resort ----------
load_sched_rb <- function(season) {
  ok <- function(df) is.data.frame(df) &&
    all(c("season", "week", "home_team", "away_team") %in% names(df)) &&
    nrow(df) > 200
  s <- tryCatch(nflreadr::load_schedules(season), error = function(e) NULL)
  if (!ok(s)) {
    cat("nflreadr::load_schedules(", season,
        ") returned a bad frame -- pulling raw nflverse games.rds direct\n", sep = "")
    s <- tryCatch(readRDS(url(
      "https://github.com/nflverse/nflverse-data/releases/download/schedules/games.rds",
      "rb")), error = function(e) NULL)
    if (is.data.frame(s) && "season" %in% names(s))
      s <- s %>% filter(season == .env$season)
  }
  if (!ok(s)) {
    cat("games.rds unreachable -- trying the games.csv mirror\n")
    s <- tryCatch(utils::read.csv(url(
      "https://github.com/nflverse/nflverse-data/releases/download/schedules/games.csv")),
      error = function(e) NULL)
    if (is.data.frame(s) && "season" %in% names(s))
      s <- s %>% filter(season == .env$season)
  }
  if (!ok(s)) {
    cat("schedule load failed all three ways; last attempt class/names:\n")
    print(class(s)); print(names(s))
    stop("no usable 2026 schedule -- slate blocked (faced side + NE wall above already certified)")
  }
  s
}

sched26_rb <- load_sched_rb(2026) %>%
  filter(week <= 18) %>%  # REG weeks (2026 playoff opponents unknowable)
  transmute(week, home = to_pff_rb(home_team), away = to_pff_rb(away_team))

## code wall BEFORE any use: mapped 2026 codes must land in the PFF 32
sched26_teams <- sort(unique(c(sched26_rb$home, sched26_rb$away)))
if (!setequal(sched26_teams, pff32_rb)) {
  cat("2026 schedule-vs-currency code mismatch:\n")
  cat("schedule-only:", setdiff(sched26_teams, pff32_rb), "\n")
  cat("currency-only:", setdiff(pff32_rb, sched26_teams), "\n")
  stop("2026 schedule code alignment failed")
}

opp26_rb <- bind_rows(
  sched26_rb %>% transmute(focal = home, opp = away),
  sched26_rb %>% transmute(focal = away, opp = home)
)
opp_n_rb <- opp26_rb %>% count(focal, name = "n_games")
stopifnot(nrow(opp26_rb) == 544L, all(opp_n_rb$n_games == 17L))

slate_rb_lg <- opp26_rb %>%
  inner_join(team26_rb_lg, by = c("opp" = "team")) %>%
  group_by(focal) %>%
  summarise(
    grun_26      = mean(grun_26),
    mtf_26       = mean(mtf_26),
    interp_share = mean(interp_share),
    .groups = "drop"
  )
stopifnot(nrow(slate_rb_lg) == 32L)

## NE slate wall vs canon
ne_slate_lg <- slate_rb_lg %>% filter(focal == "NE")
cat("NE slate side, league engine (REG 2026):\n")
print(ne_slate_lg %>% as.data.frame())
stopifnot(abs(ne_slate_lg$grun_26      - cmp_rush_slate$grun_26)      < 1e-8,
          abs(ne_slate_lg$mtf_26       - cmp_rush_slate$mtf_26)       < 1e-8,
          abs(ne_slate_lg$interp_share - cmp_rush_slate$interp_share) < 1e-8)
cat("NE wall passed: league slate side reproduces cmp_rush_slate within 1e-8\n")

## ============================================================================
## 11. LEAGUE FRAME + PRINTS
##     d > 0 = opponents' 2026 backfields are STRONGER than the 2025 backfields
##     this defense actually faced (incl. playoffs) = harder schedule
## ============================================================================

league_rush <- faced_rb_lg %>%
  select(focal, grun_25, mtf_25, fill_share_25, n_weeks, po_weeks) %>%
  inner_join(slate_rb_lg, by = "focal") %>%
  mutate(
    d_grun = grun_26 - grun_25,
    d_mtf  = mtf_26  - mtf_25,
    rank_grun = rank(-d_grun, ties.method = "min"),
    rank_mtf  = rank(-d_mtf,  ties.method = "min")
  ) %>%
  arrange(desc(d_grun))

cat("\n================ RUSHING SoS DIFFERENTIAL LADDER (sorted by d_grun) ================\n")
cat("d > 0: 2026 opposing backfields STRONGER than 2025 faced = HARDER for run defense\n\n")
print(league_rush %>% as.data.frame(), row.names = FALSE)

cat("\n---- summary ----\n")
summ_rb <- league_rush %>%
  summarise(
    grun_25_mean = mean(grun_25), grun_25_sd = sd(grun_25),
    mtf_25_mean  = mean(mtf_25),  mtf_25_sd  = sd(mtf_25),
    grun_26_mean = mean(grun_26), grun_26_sd = sd(grun_26),
    mtf_26_mean  = mean(mtf_26),  mtf_26_sd  = sd(mtf_26),
    d_grun_mean  = mean(d_grun),  d_grun_sd  = sd(d_grun),
    d_mtf_mean   = mean(d_mtf),   d_mtf_sd   = sd(d_mtf),
    fill_mean = mean(fill_share_25), fill_sd = sd(fill_share_25),
    interp_mean   = mean(interp_share),      interp_sd   = sd(interp_share))
print(summ_rb %>% as.data.frame())

ne_row_rb <- league_rush %>% filter(focal == "NE")
cat(sprintf("\nNE: d_grun=%+.4f (rank %d/32)   d_mtf=%+.4f (rank %d/32)\n",
            ne_row_rb$d_grun, ne_row_rb$rank_grun,
            ne_row_rb$d_mtf,  ne_row_rb$rank_mtf))
cat(sprintf("cor(d_grun, d_mtf) = %.3f\n", cor(league_rush$d_grun, league_rush$d_mtf)))

cat("\n---- AFC East (ranks are league-wide, computed before filtering) ----\n")
print(league_rush %>% filter(focal %in% c("NE", "BUF", "MIA", "NYJ")) %>%
        as.data.frame(), row.names = FALSE)

cat("\n---- league mean sanity: differentials redistribute, never inflate ----\n")
cat(sprintf("mean(d_grun)=%+.5f  mean(d_mtf)=%+.5f  (expect ~0 up to pool composition)\n",
            mean(league_rush$d_grun), mean(league_rush$d_mtf)))

cat("\nDone. Objects created (all *_rb_lg suffixed except league_rush):\n")
cat("  cur_rb_lg xw_rb_lg ros_rb_lg use_rb_lg p_rb_lg ent_rb_lg prior_rb_lg\n")
cat("  slot_rb_lg slot_med_rb_lg fb_rb_lg rot_rb_lg ph_rb_lg rot_2026_rb_lg team26_rb_lg\n")
cat("  faced_games_rb_lg faced_rb_lg slate_rb_lg league_rush\n")
