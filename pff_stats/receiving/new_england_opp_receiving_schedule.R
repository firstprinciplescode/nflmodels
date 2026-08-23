# ============================================================
# NE 2026 -- OPPONENT RECEIVING SoS -- BUILD FILE (Phase 5)
# Written 2026-08-15 on Andy's ship-it ("season is soon").
#
# STAMPED RULINGS (do not re-litigate):
#   BANDS: the six alignment clusters ARE the bands, NO MERGES EVER
#     (Andy 08-15, twice). RB = sixth band (rec riding unless vetoed:
#     drop "RB" from REC_BANDS and everything downstream adjusts).
#   GATE: X_QUAL = 8 routes / G_MIN = 6 qualifying games. Genealogy:
#     G>=6 is the ported spot-duty law (Phase 6 receipts); X=8 was the
#     loosest coverage-viable grid cell (X=12 took ITE 33->19).
#   POOLS: percentiles rank within (band, split) over a ROLLING
#     3-SEASON window (receipts 08-15: single-season SWR=27/ITE=33
#     broke the >=50 floor; 2023-25 window = ITE 88 / SWR 90 floor-
#     clear, stable across rolls). Header caveat: pctls shift slightly
#     when the window rolls -- fine for a static preseason artifact.
#   METRICS: grade + YPRR anchor (graded/denominated every route).
#     ypa_oe DEMOTED by yardstick: talent_sd ~1.05 asymptote, observed
#     sd 1.42 at [40,60) tgts -> luck ~0.95 = PARITY at gated samples.
#     OE is OVERALL-ONLY by construction (Andy 08-15): expectations are
#     pbp-side (play grain, no coverage labels), man/zone is PFF-side
#     (player-week aggregates, no play ids) -- per-split OE cannot be
#     built from current data. Any future OE context column = overall,
#     flagged. Do not invent man_pbp_xypa.
#     OE fork DEAD: r(part, pbp) = .992 -> pbp_ family only (future-
#     proof for in-season); part_ retired to the comp layer.
#     xtds bug HEALED (zero_share receipt 08-15) -- available, unused.
#   SPLITS: man / zone slate pair, mirroring Phase 6 across the ball.
#     Split lives in the METRICS; weights are overall routes on BOTH
#     sides (weekly base has no per-split routes -- documented
#     asymmetry, resolved symmetrically).
#   LABELS: band = receiver_scheme_final align_cluster_name, collapsed
#     one-per-player-season by routes-weighted slice_max (cluster_join
#     is multi-row per player-season -- the Akers x4 catch, 08-15).
#     2026 band = coalesce('25, '24) + tribble overrides (drift stay-
#     rate .732 is a contaminated FLOOR; movement adjacent).
#   MEMBERSHIP: top N_CORPS = 8 per team by pmax('25,'24) routes,
#     cross-band (a corps is a committee, not positional slots).
#     NO PHANTOMS in v1: receiver routes concentrate hard -- the
#     capture print below receipts N_CORPS; unknown WR6s at prior
#     weight would be noise. Rookies enter via tribble adds only.
#   PROPORTIONS (Andy 08-16): projection weights = blend2('25,'24
#     routes, w25) -- pmax stacks mutually-exclusive peak seasons in
#     the denominator and DILUTES concentrated stars (the JSN effect;
#     one-sided vs the realized-route faced side). pmax = ledger
#     surfacing only. Zero-usage adds enter at corps-median weight.
#   BRIDGE: combined_ids gsis<->pff, gated; weekly base carries
#     player_id natively (bridge needed for 2026 rosters only).
#     filter(!is.na(receiver_id/player_id)) law: the one "unbridged"
#     was an NA row class, not a human.
#
# SOLO CALLS flagged for Andy's eyeball on first run (all print
# receipts): N_CORPS = 8 (capture share prints); SPLIT_RTE_MIN = 50
# season routes per split for that split's pctl (aligned to the
# [40,60) noise-measured bin; coverage prints); faced weekly gate =
# X_QUAL on total routes.
#
# SOURCE ORDER: receiving workspace in session (receiving_func_base,
# receiver_scheme_final, combined_ids), OL file for opp_2026_teams /
# sched_2026 / in_season / blend2 / pff_team_lookup, then this file,
# top to bottom. Re-source WHOLE file after any edit.
#
# Direction: their PASS CATCHERS vs NE's PASS DEFENSE.
#   higher pctl = better receiver = HARDER for NE's secondary.
# ============================================================

needed_p5 <- c("receiving_func_base", "receiver_scheme_final", "combined_ids",
               "opp_2026_teams", "sched_2026", "in_season", "blend2",
               "pff_team_lookup")
missing_p5 <- needed_p5[!vapply(needed_p5, exists, logical(1))]
if (length(missing_p5)) stop("missing session objects: ",
                             paste(missing_p5, collapse = ", "),
                             " -- see SOURCE ORDER in header")

if (!exists("percent_rank_avg")) {          # fallback only; upstream wins
  percent_rank_avg <- function(x) {
    (rank(x, ties.method = "average", na.last = "keep") - 1) /
      (sum(!is.na(x)) - 1)
  }
}

library(gt)

# ------------------------------------------------------------
# 1. constants (rulings 2026-08-15)
# ------------------------------------------------------------

X_QUAL    <- 8       # qualifying game = >= 8 routes (one game, one vote)
G_MIN     <- 6       # gate = >= 6 qualifying games (ported spot-duty law)
POOL_W    <- 3       # rolling percentile window, seasons
N_CORPS   <- 8       # members per team, cross-band; capture print receipts
SPLIT_RTE_MIN <- 50  # season routes in a split for that split's pctl
REC_BANDS <- c("WWR", "WSWR", "SWR", "ITE", "STE", "RB")

# read.csv / sqldf landmine law: tibble at intake
receiving_func_base   <- tibble::as_tibble(receiving_func_base)
receiver_scheme_final <- tibble::as_tibble(receiver_scheme_final)

# ------------------------------------------------------------
# 2. SoS RECEIVING CURRENCY -- per split, rolling pools.
#    Stint collapse: route-weighted across team rows (traded players).
#    GRAIN LAWS (the Derby + name-drift catches, 08-15):
#      (a) scheme rows dedupe to ONE per (player_id, team, season)
#          before any sums -- z_source-style variants fan out joins
#          and double-count stats (collapsed count prints below);
#      (b) season grain groups by (player_id, season) ONLY -- PFF name
#          strings drift across stints and split the group into
#          partial seasons (the list-col pivot crash); display name
#          attaches from the career-max-routes stint;
#      (c) grades compute FIRST inside summarise -- later sum lines
#          SHADOW routes/man_routes/zone_routes and R recycles the
#          scalar SILENTLY. Do not reorder.
# ------------------------------------------------------------

scheme_p5_dups <- receiver_scheme_final %>%
  count(player_id, team_abbreviation, season) %>%
  filter(n > 1)
cat("\n--- scheme stint-grain dup keys collapsed:", nrow(scheme_p5_dups), "---\n")

scheme_p5 <- receiver_scheme_final %>%
  group_by(player_id, team_abbreviation, season) %>%
  slice_max(routes, n = 1, with_ties = FALSE) %>%
  ungroup()

rec_name <- scheme_p5 %>%
  group_by(player_id) %>%
  slice_max(routes, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, player)

rec_band_season <- scheme_p5 %>%
  filter(align_cluster_name %in% REC_BANDS) %>%
  group_by(player_id, season) %>%
  slice_max(routes, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, season, band = align_cluster_name)

rec_season <- scheme_p5 %>%
  group_by(player_id, season) %>%
  summarise(
    # grades FIRST -- law (c) above. Sum-product with denominators
    # subset to non-NA grade stints (= weighted.mean's x/w pair drop).
    grade_all  = sum(weighted_avg_grade * routes, na.rm = TRUE) /
      sum(routes[!is.na(weighted_avg_grade)], na.rm = TRUE),
    man_grade  = sum(weighted_avg_man_grade * man_routes, na.rm = TRUE) /
      sum(man_routes[!is.na(weighted_avg_man_grade)], na.rm = TRUE),
    zone_grade = sum(weighted_avg_zone_grade * zone_routes, na.rm = TRUE) /
      sum(zone_routes[!is.na(weighted_avg_zone_grade)], na.rm = TRUE),
    routes      = sum(routes, na.rm = TRUE),
    targets     = sum(targets, na.rm = TRUE),
    man_routes  = sum(man_routes, na.rm = TRUE),
    zone_routes = sum(zone_routes, na.rm = TRUE),
    man_targets  = sum(man_targets, na.rm = TRUE),
    zone_targets = sum(zone_targets, na.rm = TRUE),
    man_yards    = sum(man_yards, na.rm = TRUE),
    zone_yards   = sum(zone_yards, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(man_yprr      = ifelse(man_routes  > 0, man_yards  / man_routes,  NA_real_),
         zone_yprr     = ifelse(zone_routes > 0, zone_yards / zone_routes, NA_real_),
         man_tgt_rate  = ifelse(man_routes  > 0, man_targets  / man_routes,  NA_real_),
         zone_tgt_rate = ifelse(zone_routes > 0, zone_targets / zone_routes, NA_real_)) %>%
  inner_join(rec_name,        by = "player_id") %>%
  inner_join(rec_band_season, by = c("player_id", "season"))

# the wall: this grain violation dies HERE, loudly, not as list-cols
# in section 4
stopifnot(anyDuplicated(rec_season[, c("player_id", "season")]) == 0)

# gate: qualifying games from the WEEKLY base (native player_id)
rec_qual <- receiving_func_base %>%
  filter(!is.na(player_id), routes >= X_QUAL) %>%
  count(player_id, season, name = "qual_g") %>%
  filter(qual_g >= G_MIN)

rec_gated <- rec_season %>% inner_join(rec_qual, by = c("player_id", "season"))

# split-level floors: a split pctl exists only above SPLIT_RTE_MIN
rec_gated <- rec_gated %>%
  mutate(man_ok  = man_routes  >= SPLIT_RTE_MIN,
         zone_ok = zone_routes >= SPLIT_RTE_MIN)

cat("\n--- split-floor coverage receipt (2025 gated pool) ---\n")
print(rec_gated %>% filter(season == 2025) %>%
        group_by(band) %>%
        summarise(n = dplyr::n(),
                  man_ok  = sum(man_ok),
                  zone_ok = sum(zone_ok), .groups = "drop"))

# rolling 3-season pctls, ranked within (band x window), kept for season s
pool_pctls <- function(s) {
  rec_gated %>%
    filter(season %in% (s - POOL_W + 1):s) %>%
    group_by(band) %>%
    mutate(
      grade_pctl     = percent_rank_avg(grade_all),
      man_grade_pctl = percent_rank_avg(ifelse(man_ok,  man_grade,  NA_real_)),
      man_yprr_pctl  = percent_rank_avg(ifelse(man_ok,  man_yprr,   NA_real_)),
      man_tgt_pctl   = percent_rank_avg(ifelse(man_ok,  man_tgt_rate,  NA_real_)),
      zone_grade_pctl = percent_rank_avg(ifelse(zone_ok, zone_grade, NA_real_)),
      zone_yprr_pctl  = percent_rank_avg(ifelse(zone_ok, zone_yprr,  NA_real_)),
      zone_tgt_pctl   = percent_rank_avg(ifelse(zone_ok, zone_tgt_rate, NA_real_))
    ) %>%
    ungroup() %>%
    filter(season == s)
}

rec_season_pctl_sos <- purrr::map_dfr(2018:2025, pool_pctls)

cat("\n--- currency coverage by season (expect stable) ---\n")
print(rec_season_pctl_sos %>% count(season), n = 12)
cat("\n--- 2025 pooled-band sizes at the gate ---\n")
print(rec_season_pctl_sos %>% filter(season == 2025) %>% count(band))

# ------------------------------------------------------------
# 2b. ORIENTATION + COR RITUAL per split (receipts print; all three
#     candidates run higher = better receiver = harder for NE).
#     Eyeball: top-6 by man grade -- names from data, never from Claude.
# ------------------------------------------------------------

cat("\n--- cor matrix, man split (2025) ---\n")
print(rec_season_pctl_sos %>% filter(season == 2025) %>%
        summarise(r_grade_yprr = cor(man_grade_pctl, man_yprr_pctl, use = "complete.obs"),
                  r_grade_tgt  = cor(man_grade_pctl, man_tgt_pctl,  use = "complete.obs"),
                  r_yprr_tgt   = cor(man_yprr_pctl,  man_tgt_pctl,  use = "complete.obs")))
cat("\n--- cor matrix, zone split (2025) ---\n")
print(rec_season_pctl_sos %>% filter(season == 2025) %>%
        summarise(r_grade_yprr = cor(zone_grade_pctl, zone_yprr_pctl, use = "complete.obs"),
                  r_grade_tgt  = cor(zone_grade_pctl, zone_tgt_pctl,  use = "complete.obs"),
                  r_yprr_tgt   = cor(zone_yprr_pctl,  zone_tgt_pctl,  use = "complete.obs")))
cat("\n--- eyeball: 2025 top-6 man-grade pctl (should read like a WR1 list) ---\n")
print(rec_season_pctl_sos %>% filter(season == 2025) %>%
        arrange(desc(man_grade_pctl)) %>%
        select(player, band, man_routes, man_grade_pctl, man_yprr_pctl) %>%
        head(6))

# ------------------------------------------------------------
# 3. offense crosswalk + 2026 rosters + entry years
# ------------------------------------------------------------

rec_id_bridge <- combined_ids %>%
  filter(!is.na(gsis_id)) %>%
  distinct(player_id, gsis_id)
stopifnot(anyDuplicated(rec_id_bridge$gsis_id)   == 0,
          anyDuplicated(rec_id_bridge$player_id) == 0)

pos_keep <- c("WR", "TE", if ("RB" %in% REC_BANDS) "RB")

rec_2026 <- nflreadr::load_rosters(2026) %>%
  filter(position %in% pos_keep) %>%
  transmute(gsis_id,
            roster_name = full_name,
            team_name = dplyr::coalesce(pff_team_lookup[team], team),
            entry_year,
            roster_pos = position) %>%
  filter(team_name %in% opp_2026_teams) %>%
  left_join(rec_id_bridge, by = "gsis_id")

stopifnot(anyDuplicated(rec_2026[, c("team_name", "roster_name")]) == 0)

entry_years_off <- nflreadr::load_rosters(2017:2025) %>%
  filter(!is.na(gsis_id), !is.na(entry_year)) %>%
  group_by(gsis_id) %>%
  summarise(entry_year = min(entry_year), .groups = "drop") %>%
  inner_join(rec_id_bridge, by = "gsis_id") %>%
  select(player_id, entry_year)

# entry-year priors per (band, metric) from the pooled currency
rec_rookie_prior <- rec_season_pctl_sos %>%
  inner_join(entry_years_off, by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2018) %>%
  group_by(band) %>%
  summarise(pr_grade      = median(grade_pctl, na.rm = TRUE),
            pr_man_grade  = median(man_grade_pctl,  na.rm = TRUE),
            pr_man_yprr   = median(man_yprr_pctl,   na.rm = TRUE),
            pr_zone_grade = median(zone_grade_pctl, na.rm = TRUE),
            pr_zone_yprr  = median(zone_yprr_pctl,  na.rm = TRUE),
            n_entry = dplyr::n(), .groups = "drop")

cat("\n--- entry-year priors by band (n_entry = pool behind each) ---\n")
print(rec_rookie_prior)

# ------------------------------------------------------------
# 4. usage, band ('25 -> '24 coalesce), pctls, status flags
# ------------------------------------------------------------

usage_rec <- rec_season %>%
  filter(season %in% c(2024, 2025)) %>%
  select(player_id, season, routes) %>%
  tidyr::pivot_wider(names_from = season, values_from = routes,
                     names_prefix = "routes_")

band_26 <- rec_band_season %>%
  filter(season %in% c(2024, 2025)) %>%
  tidyr::pivot_wider(names_from = season, values_from = band,
                     names_prefix = "band_") %>%
  mutate(band = dplyr::coalesce(band_2025, band_2024)) %>%
  select(player_id, band)

p25 <- rec_season_pctl_sos %>% filter(season == 2025) %>%
  select(player_id, qual_g25 = qual_g,
         mg25 = man_grade_pctl,  my25 = man_yprr_pctl,  mt25 = man_tgt_pctl,
         zg25 = zone_grade_pctl, zy25 = zone_yprr_pctl, zt25 = zone_tgt_pctl)
p24 <- rec_season_pctl_sos %>% filter(season == 2024) %>%
  select(player_id,
         mg24 = man_grade_pctl,  my24 = man_yprr_pctl,  mt24 = man_tgt_pctl,
         zg24 = zone_grade_pctl, zy24 = zone_yprr_pctl, zt24 = zone_tgt_pctl)

ledger_base_rec <- rec_2026 %>%
  left_join(usage_rec, by = "player_id") %>%
  left_join(band_26,   by = "player_id") %>%
  left_join(p25,       by = "player_id") %>%
  left_join(p24,       by = "player_id") %>%
  mutate(status = case_when(
    entry_year == 2026                        ~ "rookie",
    is.na(player_id)                          ~ "no_pff_id",
    !is.na(mg25) | !is.na(zg25)               ~ "has_2025_pctl",
    !is.na(mg24) | !is.na(zg24)               ~ "data_2024_only",
    dplyr::coalesce(routes_2025, 0) > 0 |
      dplyr::coalesce(routes_2024, 0) > 0     ~ "usage_no_pctl",
    TRUE                                      ~ "no_rec_history"
  ))

cat("\n--- no-history bodies excluded from ledger (per team) ---\n")
print(ledger_base_rec %>% filter(status == "no_rec_history") %>%
        count(team_name), n = Inf)

ledger_rec <- ledger_base_rec %>% filter(status != "no_rec_history")

# ------------------------------------------------------------
# 5. corps proposal: top N_CORPS per team by pmax('25,'24) routes,
#    cross-band. Capture print = the N_CORPS receipt.
# ------------------------------------------------------------

# usage_ord = pmax: SURFACING statistic only (the Oliver law -- injury
# returners must rank visibly). Projection WEIGHTS are blend2 (section
# 9, proportions ruling 08-16). Two jobs, two statistics -- do not unify.
ledger_rec <- ledger_rec %>%
  mutate(usage_ord = pmax(dplyr::coalesce(routes_2025, 0),
                          dplyr::coalesce(routes_2024, 0))) %>%
  group_by(team_name) %>%
  arrange(desc(usage_ord), .by_group = TRUE) %>%
  mutate(corps_rank = dplyr::row_number(),
         proposed   = !is.na(band) & corps_rank <= N_CORPS) %>%
  ungroup()

cat("\n--- N_CORPS receipt: share of 2025 team routes captured by top-8 ---\n")
print(ledger_rec %>% group_by(team_name) %>%
        summarise(capture = round(sum(routes_2025[proposed], na.rm = TRUE) /
                                    sum(routes_2025, na.rm = TRUE), 3),
                  .groups = "drop") %>%
        arrange(capture), n = Inf)

# ------------------------------------------------------------
# 6. THE RULING LEDGER
# ------------------------------------------------------------

cat("\n================ STEP 0 -- THE RULING LEDGER ================\n")
cat("proposed = TRUE -> corps (top N_CORPS = 8 by '25/'24 routes)\n")
cat("Andy rules deltas: adds (rookies -- band REQUIRED), drops,\n")
cat("band overrides for known role changes (the movers list).\n\n")

print(ledger_rec %>%
        arrange(match(team_name, opp_2026_teams), desc(usage_ord)) %>%
        select(team_name, band, roster_name, status, proposed,
               r25 = routes_2025, r24 = routes_2024,
               mg25, zg25, my25, zy25),
      n = Inf)

cat("\n--- calls needed per team ---\n")
print(ledger_rec %>% group_by(team_name) %>%
        summarise(proposed      = sum(proposed),
                  rookies       = sum(status == "rookie"),
                  data_24_only  = sum(status == "data_2024_only"),
                  usage_no_pctl = sum(status == "usage_no_pctl"),
                  no_pff_id     = sum(status == "no_pff_id"),
                  band_na       = sum(is.na(band)),
                  .groups = "drop") %>%
        arrange(match(team_name, opp_2026_teams)), n = Inf)

# ------------------------------------------------------------
# STOP -- ruling boundary. Deltas happen in section 7.
# ------------------------------------------------------------

# ------------------------------------------------------------
# 7. ANDY'S DELTAS -- the only judgment layer.
#    EMPTY = full sign-off. actions: "add" (band REQUIRED for
#    rookies/no-history), "drop", "band" (override a label -- the
#    drift movers). Names gate vs rec_2026, fails loudly.
# ------------------------------------------------------------

opp_rec_2026_deltas <- tribble(
  ~team_name, ~roster_name, ~band, ~action, ~note
  # "SEA", "<rookie name>", "WWR", "add",  "first-round X target",
  # "MIN", "<name>",        "SWR", "band", "new OC moved him inside",
)

d_miss_rec <- opp_rec_2026_deltas %>%
  anti_join(rec_2026, by = c("team_name", "roster_name"))
if (nrow(d_miss_rec) > 0) {
  print(d_miss_rec)
  stop("delta names not on 2026 rosters -- fix spelling vs the ledger")
}
cat("\n--- deltas registered:", nrow(opp_rec_2026_deltas), "row(s) ---\n")
if (nrow(opp_rec_2026_deltas) > 0) print(opp_rec_2026_deltas, n = Inf)

# ------------------------------------------------------------
# 8. FACED 2025 SIDE -- observed only. Weight = weekly routes;
#    split lives in the METRIC (his man/zone season pctls).
#    Band fallback = the weekly row's own align label.
# ------------------------------------------------------------

faced_rec_2025 <- receiving_func_base %>%
  filter(def_ssn == "NE2025", in_season(week),
         !is.na(player_id), routes >= X_QUAL) %>%
  transmute(player_id, player, week, g_routes = routes,
            band_row = align_cluster_name) %>%
  left_join(rec_season_pctl_sos %>% filter(season == 2025) %>%
              select(player_id, band, mg25 = man_grade_pctl,
                     my25 = man_yprr_pctl, zg25 = zone_grade_pctl,
                     zy25 = zone_yprr_pctl),
            by = "player_id") %>%
  mutate(band = dplyr::coalesce(band, band_row)) %>%
  filter(band %in% REC_BANDS)

cat("\n--- faced 2025: weeks present (expect 21 incl playoffs) ---\n")
print(faced_rec_2025 %>% distinct(week) %>% arrange(week), n = Inf)

cat("\n--- faced audit: duplicate player-weeks (want 0) ---\n")
print(faced_rec_2025 %>% count(player_id, week) %>% filter(n > 1) %>% nrow())

faced_band_rec_2025 <- faced_rec_2025 %>%
  group_by(band) %>%
  summarise(tot_routes = sum(g_routes),
            unscored_share = sum(g_routes[is.na(mg25) & is.na(zg25)]) / tot_routes,
            man_grade_25  = weighted.mean(mg25, w = g_routes, na.rm = TRUE),
            man_yprr_25   = weighted.mean(my25, w = g_routes, na.rm = TRUE),
            zone_grade_25 = weighted.mean(zg25, w = g_routes, na.rm = TRUE),
            zone_yprr_25  = weighted.mean(zy25, w = g_routes, na.rm = TRUE),
            .groups = "drop")

cat("\n--- faced 2025 band profile (unscored_share = honesty) ---\n")
print(faced_band_rec_2025)

# ------------------------------------------------------------
# 9. 2026 PROJECTION -- signed corps + deltas + priors.
#    blend2 w25 = qual_g25/10; NO phantoms (header rationale);
#    prior fill for rookies/no-pctl members, flagged; interp_share
#    per (band, split) = the honesty column.
# ------------------------------------------------------------

drops_rec <- opp_rec_2026_deltas %>% filter(action == "drop")
band_over <- opp_rec_2026_deltas %>% filter(action == "band") %>%
  select(team_name, roster_name, band_new = band)
adds_rec <- opp_rec_2026_deltas %>% filter(action == "add") %>%
  left_join(ledger_base_rec %>%
              select(team_name, roster_name, player_id, band_hist = band,
                     routes_2025, routes_2024, qual_g25, status,
                     mg25, my25, mt25, zg25, zy25, zt25,
                     mg24, my24, mt24, zg24, zy24, zt24),
            by = c("team_name", "roster_name")) %>%
  mutate(band = dplyr::coalesce(band, band_hist)) %>%
  select(-band_hist, -action, -note)
if (nrow(adds_rec)) stopifnot(!any(is.na(adds_rec$band)))

rot_rec_2026 <- ledger_rec %>%
  filter(proposed) %>%
  select(team_name, roster_name, player_id, band, status,
         routes_2025, routes_2024, qual_g25,
         mg25, my25, mt25, zg25, zy25, zt25,
         mg24, my24, mt24, zg24, zy24, zt24) %>%
  anti_join(drops_rec, by = c("team_name", "roster_name")) %>%
  bind_rows(adds_rec) %>%
  left_join(band_over, by = c("team_name", "roster_name")) %>%
  mutate(band = dplyr::coalesce(band_new, band)) %>%
  select(-dplyr::any_of("band_new"))

stopifnot(!any(is.na(rot_rec_2026$band)),
          anyDuplicated(rot_rec_2026[, c("team_name", "roster_name")]) == 0)

rot_rec_2026 <- rot_rec_2026 %>%
  left_join(rec_rookie_prior, by = "band") %>%
  mutate(w25 = pmin(dplyr::coalesce(qual_g25, 0L) / 10, 1),
         # w25u (08-16c): USAGE evidence scales with season COVERAGE,
         # not the quality cap. /10 saturates at 10 gated games, so a
         # 10-game injury year imported its availability shortfall as
         # projected role shrink (the Wilson/Kraft receipt rows). /18
         # lets '24 carry the missing weeks; full-season role-losers
         # (Tolbert-class) stay correctly discounted. VALUES keep /10.
         w25u = pmin(dplyr::coalesce(qual_g25, 0L) / 18, 1),
         mg_bl = blend2(mg25, mg24, w25),
         my_bl = blend2(my25, my24, w25),
         zg_bl = blend2(zg25, zg24, w25),
         zy_bl = blend2(zy25, zy24, w25),
         prior_used = is.na(mg_bl) & is.na(zg_bl),
         mg_f = dplyr::coalesce(mg_bl, pr_man_grade),
         my_f = dplyr::coalesce(my_bl, pr_man_yprr),
         zg_f = dplyr::coalesce(zg_bl, pr_zone_grade),
         zy_f = dplyr::coalesce(zy_bl, pr_zone_yprr),
         # PROPORTIONS RULING (Andy 08-16): weights = blend2, NOT pmax.
         # pmax stacks mutually-exclusive peak seasons in the corps
         # denominator (two guys who split a role across years both
         # carry their peak), diluting concentrated stars -- the JSN
         # effect. pmax survives ONLY as the ledger surfacing statistic.
         # WEIGHT FALLBACK LADDER (08-16b -- the RB interp 4->8 tell):
         #  1) blend (gated members -- the JSN fix intact)
         #  2) real pmax routes: gate-failed fringe keep their TRUE
         #     small weights (w25 = 0 zeroed them through blend, and
         #     median-fill inflated a 30-route back to a WR4's weight)
         #  3) corps-median: TRUE zero-evidence members (rookie adds)
         usage_bl = blend2(dplyr::coalesce(routes_2025, 0),
                           dplyr::coalesce(routes_2024, 0), w25u),
         usage_w  = dplyr::if_else(usage_bl > 0, usage_bl,
                                   pmax(dplyr::coalesce(routes_2025, 0),
                                        dplyr::coalesce(routes_2024, 0)))) %>%
  select(team_name, roster_name, band, status, prior_used, usage_w,
         mg_f, my_f, zg_f, zy_f) %>%
  group_by(team_name) %>%
  mutate(usage_w = dplyr::if_else(usage_w > 0, usage_w,
                                  stats::median(usage_w[usage_w > 0]))) %>%
  ungroup()

cat("\n--- 2026 corps, final (prior_used = valued at band prior) ---\n")
print(rot_rec_2026 %>%
        arrange(match(team_name, opp_2026_teams), band, desc(usage_w)) %>%
        select(team_name, band, roster_name, status, prior_used,
               usage_w, mg_f, zg_f), n = Inf)

# schedule-weighted band slate (division x2); interp per band x split
slate_rows_rec <- tibble(team_name = sched_2026) %>%
  left_join(rot_rec_2026, by = "team_name", relationship = "many-to-many")

slate_band_rec_2026 <- slate_rows_rec %>%
  group_by(band) %>%
  summarise(interp_share  = sum(usage_w[prior_used]) / sum(usage_w),
            man_grade_26  = weighted.mean(mg_f, w = usage_w, na.rm = TRUE),
            man_yprr_26   = weighted.mean(my_f, w = usage_w, na.rm = TRUE),
            zone_grade_26 = weighted.mean(zg_f, w = usage_w, na.rm = TRUE),
            zone_yprr_26  = weighted.mean(zy_f, w = usage_w, na.rm = TRUE),
            .groups = "drop")

cmp_rec_slate <- faced_band_rec_2025 %>%
  left_join(slate_band_rec_2026, by = "band") %>%
  mutate(d_man_grade  = man_grade_26  - man_grade_25,
         d_man_yprr   = man_yprr_26   - man_yprr_25,
         d_zone_grade = zone_grade_26 - zone_grade_25,
         d_zone_yprr  = zone_yprr_26  - zone_yprr_25) %>%
  arrange(match(band, REC_BANDS))

cat("\n--- THE ANSWER: 2026 vs 2025 receiving slate, band x split ---\n")
print(cmp_rec_slate, n = Inf)

# ------------------------------------------------------------
# 10. FIGS -- one per split (two slates, two figs; never one).
# ------------------------------------------------------------

rec_fig_pd <- function(sp) {
  cmp_rec_slate %>%
    # explicit named all_of, not starts_with + positional set_names --
    # the d_-prefixed deltas broke that pattern once (08-15)
    select(band, dplyr::all_of(c(
      grade_25 = paste0(sp, "_grade_25"), yprr_25 = paste0(sp, "_yprr_25"),
      grade_26 = paste0(sp, "_grade_26"), yprr_26 = paste0(sp, "_yprr_26")))) %>%
    tidyr::pivot_longer(-band, names_to = c("metric", "yr"),
                        names_pattern = "(grade|yprr)_(25|26)",
                        values_to = "pctl") %>%
    tidyr::pivot_wider(names_from = yr, values_from = pctl,
                       names_prefix = "y") %>%
    mutate(band = factor(band, levels = rev(REC_BANDS)),
           metric = factor(metric, levels = c("grade", "yprr"),
                           labels = c("Receiving grade", "YPRR")))
}

plot_rec_split <- function(sp, title) {
  ggplot(rec_fig_pd(sp), aes(y = band)) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey45") +
    geom_segment(aes(x = y25, xend = y26, yend = band),
                 arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
                 linewidth = 1, color = "grey60") +
    geom_point(aes(x = y25), shape = 1, size = 3.2, stroke = 1.2, color = "grey55") +
    geom_point(aes(x = y26), shape = 16, size = 2.6, color = "#002244") +
    facet_wrap(~ metric, nrow = 1) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = title,
         subtitle = "open = 2025 receivers faced (route-weighted) | solid navy = 2026 projected corps | higher = HARDER for NE's secondary | dashed = league median",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "grey40", size = 8.5),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(face = "bold", size = 9),
          panel.spacing.x = unit(1.1, "lines"))
}

plot_rec_man  <- plot_rec_split("man",
                                "NE defense - 2026 vs 2025 opposing receivers, MAN coverage lens")
plot_rec_man
plot_rec_zone <- plot_rec_split("zone",
                                "NE defense - 2026 vs 2025 opposing receivers, ZONE coverage lens")
plot_rec_zone

# ------------------------------------------------------------
# 11. FIG-COMPANION TABLES -- one per split, interp % honesty column.
# ------------------------------------------------------------

rec_split_gt <- function(sp, title) {
  d <- cmp_rec_slate %>%
    select(band, interp_share, dplyr::all_of(c(
      g25 = paste0(sp, "_grade_25"), y25c = paste0(sp, "_yprr_25"),
      g26 = paste0(sp, "_grade_26"), y26c = paste0(sp, "_yprr_26"),
      gd  = paste0("d_", sp, "_grade"), yd = paste0("d_", sp, "_yprr"))))
  ddom <- max(abs(c(d$gd, d$yd)), na.rm = TRUE)
  d %>%
    select(band, g25, g26, gd, y25c, y26c, yd, interp_share) %>%
    gt() %>%
    tab_spanner(label = "Grade", columns = c(g25, g26, gd)) %>%
    tab_spanner(label = "YPRR",  columns = c(y25c, y26c, yd)) %>%
    cols_label(band = "", g25 = "'25", g26 = "'26", gd = "\u0394",
               y25c = "'25", y26c = "'26", yd = "\u0394",
               interp_share = "interp %") %>%
    fmt_percent(columns = c(g25, g26, y25c, y26c, interp_share), decimals = 0) %>%
    fmt_percent(columns = c(gd, yd), decimals = 0, force_sign = TRUE) %>%
    data_color(columns = c(gd, yd),
               fn = scales::col_numeric(c("#6baed6", "#f7f7f7", "#C60C30"),
                                        domain = c(-ddom, ddom)),
               autocolor_text = TRUE) %>%
    tab_header(title = title,
               subtitle = "route-share-weighted band means, division counted twice | higher = harder for NE's secondary | \u0394 red = harder | interp % = projected routes on entry-year priors") %>%
    tab_options(table.font.size = px(12), data_row.padding = px(3),
                column_labels.font.weight = "bold")
}

rec_man_gt  <- rec_split_gt("man",  "2026 vs 2025 receiving slate \u2014 MAN lens")
rec_man_gt
rec_zone_gt <- rec_split_gt("zone", "2026 vs 2025 receiving slate \u2014 ZONE lens")
rec_zone_gt

# ------------------------------------------------------------
# STOP 2 -- player panels (v2 grammar) + 3-season table + per-team
# views build AFTER the fig eyeball. Checkpoint:
# ------------------------------------------------------------
# ggsave("rec_fig_man.png",  plot_rec_man,  width = 9, height = 5.5, dpi = 200)
# ggsave("rec_fig_zone.png", plot_rec_zone, width = 9, height = 5.5, dpi = 200)
# gtsave(rec_man_gt,  "rec_slate_man.png",  vwidth = 820)
# gtsave(rec_zone_gt, "rec_slate_zone.png", vwidth = 820)
# save.image("~/ne_rec_sos_workspace.RData")
# system('aws s3 cp ~/ne_rec_sos_workspace.RData s3://nfl-pff-data-lucas/workspaces/')