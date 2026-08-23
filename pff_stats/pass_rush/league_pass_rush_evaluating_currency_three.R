# ============================================================
# PASS RUSH -- CURRENCY THREE, ROUND ONE (Andy go: 2026-08-16)
#
# RULINGS (Andy, this session):
#   - RANK SPACE, not residuals: no expectation model, no OE columns.
#     Blowout compression is a FEATURE (a 5-sack demolition and a
#     barely-won day both count as "a good day"; nothing drags).
#   - NO NEW STATISTICS: one new column only, built from Andy's own
#     pattern (percent_rank_avg + singleton guard + qbgrp grouping),
#     applied to a grade column that already exists.
#   - MEDIAN season aggregation (skew resistance, doubled).
#   - Gates = canon (X=3 tps snaps / G=6 games) -- NOT re-derived.
#     One law, one place.
#   - FIREWALL: currency three NEVER feeds any SoS slate. The SoS
#     measures schedules with UNadjusted quality; feeding adjusted
#     pctls back would subtract schedule from the schedule measure.
#
# WHAT THE NEW COLUMN MEANS: vsopp_rank = this game's TPS grade,
# ranked among every same-position rusher-game against the SAME
# offense (qbgrp_ssn) that season. "Your day, vs everyone who faced
# the same OL." Opponent adjustment by construction -- the common-
# opponent comparison, no model anywhere.
#
# SOURCE ORDER: prush pipeline workspace in session (needs only
# full_pass_rush_qbgrp), then this file, top to bottom.
# Optional soft joins (print extra context IF present, skipped
# silently if not): prush_tps_season_pctl_sos (currency one, for the
# comparison + NE block), pass_rush_all_opp_percentile (Andy's
# existing OL-difficulty table, line-217 construction).
# ============================================================

needed_c3 <- c("full_pass_rush_qbgrp")
missing_c3 <- needed_c3[!vapply(needed_c3, exists, logical(1))]
if (length(missing_c3)) stop("missing session objects: ",
                             paste(missing_c3, collapse = ", "))

if (!exists("percent_rank_avg")) {          # fallback only; upstream wins
  percent_rank_avg <- function(x) {
    n_valid <- sum(!is.na(x))
    if (n_valid <= 1) return(rep(0.5, length(x)))
    (rank(x, ties.method = "average", na.last = "keep") - 1) / (n_valid - 1)
  }
}

full_pass_rush_qbgrp <- tibble::as_tibble(full_pass_rush_qbgrp)

# ------------------------------------------------------------
# 1. constants -- canon gates, imported not re-derived
# ------------------------------------------------------------

X_QUAL_C3 <- 3          # qualifying game = >= 3 tps rush snaps (canon)
G_MIN_C3  <- 6          # gate = >= 6 qualifying games (canon)
BANDS_C3  <- c("ED", "DI")

# ------------------------------------------------------------
# 2. THE NEW COLUMN -- within-opponent game rank (Andy's pattern:
#    percent_rank_avg + n()==1 -> 0.5 singleton guard)
# ------------------------------------------------------------

prush_game_c3 <- full_pass_rush_qbgrp %>%
  filter(position %in% BANDS_C3,
         true_pass_set_snap_counts_pass_rush >= X_QUAL_C3) %>%
  select(player, player_id, position, season, week,
         qbgrp_ssn, def_ssn,
         tps_snaps = true_pass_set_snap_counts_pass_rush,
         tps_grade = true_pass_set_grades_pass_rush_defense) %>%
  group_by(qbgrp_ssn, position) %>%
  mutate(n_pool = dplyr::n(),
         vsopp_rank = dplyr::case_when(
           dplyr::n() == 1 ~ 0.5,
           TRUE ~ percent_rank_avg(tps_grade))) %>%
  ungroup()

cat("\n--- honesty: sub-gate games excluded from ranking (by season) ---\n")
print(full_pass_rush_qbgrp %>%
        filter(position %in% BANDS_C3) %>%
        group_by(season) %>%
        summarise(games = dplyr::n(),
                  excluded = round(mean(
                    true_pass_set_snap_counts_pass_rush < X_QUAL_C3,
                    na.rm = TRUE), 3),
                  .groups = "drop"), n = 12)

cat("\n--- honesty: within-opponent pool sizes (rusher-games per pool) ---\n")
print(prush_game_c3 %>%
        distinct(qbgrp_ssn, position, n_pool) %>%
        group_by(position) %>%
        summarise(pools = dplyr::n(),
                  p10 = quantile(n_pool, .10),
                  med = median(n_pool),
                  p90 = quantile(n_pool, .90), .groups = "drop"))

# ------------------------------------------------------------
# 3. season grain -- MEDIAN of vsopp ranks, canon gate, modal band
# ------------------------------------------------------------

modal_band_c3 <- prush_game_c3 %>%
  count(player_id, season, position, wt = tps_snaps, name = "sn") %>%
  group_by(player_id, season) %>%
  slice_max(sn, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, season, band = position)

# grain law (the name-drift lesson from receiving): group by player_id
# ONLY -- PFF name strings drift across stints and split seasons; the
# wall below exists to catch exactly that. Display name attaches from
# the career-max-snaps row.
prush_name_c3 <- prush_game_c3 %>%
  group_by(player_id) %>%
  slice_max(tps_snaps, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, player)

prush_c3_season <- prush_game_c3 %>%
  group_by(player_id, season) %>%
  summarise(qual_g = dplyr::n(),
            tps    = sum(tps_snaps),
            med_vsopp = median(vsopp_rank, na.rm = TRUE),
            .groups = "drop") %>%
  filter(qual_g >= G_MIN_C3) %>%
  inner_join(modal_band_c3, by = c("player_id", "season")) %>%
  inner_join(prush_name_c3, by = "player_id")

stopifnot(anyDuplicated(prush_c3_season[, c("player_id", "season")]) == 0)

# ------------------------------------------------------------
# 4. THE CURRENCY -- band x season pools of the season medians
# ------------------------------------------------------------

prush_c3_pctl <- prush_c3_season %>%
  group_by(band, season) %>%
  mutate(c3_pctl = percent_rank_avg(med_vsopp)) %>%
  ungroup()

cat("\n--- currency coverage: gated players per band x season ---\n")
print(prush_c3_pctl %>% count(season, band) %>%
        tidyr::pivot_wider(names_from = band, values_from = n), n = 12)

cat("\n--- eyeball: 2025 top-10 by c3 (should read like an All-Pro list) ---\n")
print(prush_c3_pctl %>% filter(season == 2025) %>%
        arrange(desc(c3_pctl)) %>%
        select(player, band, qual_g, tps, med_vsopp, c3_pctl) %>%
        head(10))

cat("\n--- eyeball: 2025 bottom-10 (gated, so real rotation players) ---\n")
print(prush_c3_pctl %>% filter(season == 2025) %>%
        arrange(c3_pctl) %>%
        select(player, band, qual_g, tps, med_vsopp, c3_pctl) %>%
        head(10))

# ------------------------------------------------------------
# 5. SOFT CONTEXT -- lights up only if the objects are in session
# ------------------------------------------------------------

if (exists("prush_tps_season_pctl_sos")) {
  c1_c3 <- prush_c3_pctl %>%
    inner_join(prush_tps_season_pctl_sos %>%
                 select(player_id, season, band,
                        c1_pctl = tps_grade_sos_pctl),
               by = c("player_id", "season", "band"))
  
  cat("\n--- c1 vs c3: how much does slate-adjustment reorder? ---\n")
  print(c1_c3 %>% group_by(season) %>%
          summarise(n = dplyr::n(),
                    r = round(cor(c1_pctl, c3_pctl,
                                  use = "complete.obs"), 3),
                    .groups = "drop"), n = 12)
  
  cat("\n--- 2025 biggest movers, c3 - c1 (slate-flattered < 0 < slate-punished) ---\n")
  print(c1_c3 %>% filter(season == 2025) %>%
          mutate(d = round(c3_pctl - c1_pctl, 3)) %>%
          arrange(desc(abs(d))) %>%
          select(player, band, c1_pctl, c3_pctl, d) %>%
          head(12))
  
  cat("\n--- THE CHAISSON QUESTION: NE2025 rushers, both lenses ---\n")
  ne_ids_c3 <- prush_game_c3 %>%
    filter(season == 2025, def_ssn == "NE2025") %>%
    distinct(player_id)
  print(c1_c3 %>% filter(season == 2025) %>%
          semi_join(ne_ids_c3, by = "player_id") %>%
          mutate(d = round(c3_pctl - c1_pctl, 3)) %>%
          arrange(band, desc(c3_pctl)) %>%
          select(player, band, qual_g, c1_pctl, c3_pctl, d))
} else {
  cat("\n[soft skip] prush_tps_season_pctl_sos not in session --",
      "source the canon prush file for the c1 comparison + NE block\n")
}

if (exists("pass_rush_all_opp_percentile")) {
  apo_c3 <- tibble::as_tibble(pass_rush_all_opp_percentile)
  ed_col_c3 <- grep("grade", grep("ED", names(apo_c3), value = TRUE),
                    value = TRUE)[1]
  if (!is.na(ed_col_c3)) {
    cat("\n--- context: hardest / easiest OLs (Andy's line-217 table) ---\n")
    print(apo_c3 %>% arrange(.data[[ed_col_c3]]) %>%
            select(qbgrp_ssn, dplyr::contains("grade")) %>% head(5))
    print(apo_c3 %>% arrange(desc(.data[[ed_col_c3]])) %>%
            select(qbgrp_ssn, dplyr::contains("grade")) %>% head(5))
  } else {
    cat("\n[soft skip] difficulty table present but ED grade column",
        "not recognized -- pivot naming differs; harmless\n")
  }
} else {
  cat("\n[soft skip] pass_rush_all_opp_percentile not in session --",
      "difficulty context available after the pipeline file runs\n")
}

# ------------------------------------------------------------
# Checkpoint (after eyeball):
# ------------------------------------------------------------
# save.image("~/prush_c3_workspace.RData")
# system('aws s3 cp ~/prush_c3_workspace.RData s3://nfl-pff-data-lucas/workspaces/')