# ============================================================
# CURRENCY THREE -- RUN DEFENSE (standalone unit file)
#
# Ported from the signed prush currency-three template
# (league_pass_rush_evaluating_currency_three.R; round one signed
# 2026-08-16). Same skeleton: needs gate -> constants ->
# within-opponent rank -> honesty prints -> player_id-only season
# grain + wall -> band x season pools -> eyeballs -> soft c1.
# Self-contained: no other currency-three file needed.
#
# WHAT THE COLUMN MEANS: vsopp_rank = this game's run-defense grade,
# ranked among every same-position defender-game against the SAME
# offense (qbgrp_ssn) that season. "Your day, vs everyone who faced
# the same offense." Opponent adjustment by construction -- no model.
#
# RULINGS IMPORTED (do not re-litigate):
#   - RANK SPACE, not residuals. No expectation model, no OE columns.
#   - NO NEW STATISTICS: one new column (vsopp_rank), Andy's pattern
#     (percent_rank_avg + n()==1 -> 0.5 singleton guard + the
#     opponent grouping key) on an EXISTING grade column.
#   - MEDIAN season aggregation.
#   - Gates = canon, imported from the phase-3 header (ruled
#     2026-08-14): X = 3 run snaps, G = 6 games, bands DI/ED/LB/S.
#   - Opponent-key symmetry law: defensive units rank within
#     qbgrp_ssn (the offense faced).
#   - FIREWALL: currency three NEVER feeds any SoS slate.
#   - Playoffs IN: rankable games are floor-gated, never week-gated.
#   - Name drift: season grain groups by player_id ONLY; display
#     name via career-max-snaps slice_max; grain wall stays.
#   - tibble-at-birth; no startsWith anywhere; keys are opaque.
#
# SOURCE ORDER: phase-3 build in session (needs run_defense_qbgrp),
# then this file top to bottom. Soft context lights up if present:
# rundef_season_pctl_sos (c1, for the comparison + NE block),
# run_defense_opp_position_percentile (difficulty table).
#
# THE COLUMN LEGEND prints at the end of every run.
# ============================================================

needed_rd <- c("run_defense_qbgrp")
missing_rd <- needed_rd[!vapply(needed_rd, exists, logical(1))]
if (length(missing_rd)) stop("missing session objects: ",
                             paste(missing_rd, collapse = ", "))

if (!exists("percent_rank_avg")) {          # fallback only; upstream wins
  percent_rank_avg <- function(x) {
    n_valid <- sum(!is.na(x))
    if (n_valid <= 1) return(rep(0.5, length(x)))
    (rank(x, ties.method = "average", na.last = "keep") - 1) / (n_valid - 1)
  }
}

run_defense_qbgrp <- tibble::as_tibble(run_defense_qbgrp)

# names() receipt wall (handoff law: confirm grade/snap/key columns
# against the live frame; if a frame lacks a column, STOP and report --
# no substitution).
req_rd <- c("player", "player_id", "position", "season", "week",
            "qbgrp_ssn", "snap_counts_run", "grades_run_defense")
miss_rd <- setdiff(req_rd, names(run_defense_qbgrp))
if (length(miss_rd)) {
  cat("STOP -- run_defense_qbgrp lacks required columns:\n")
  print(miss_rd)
  cat("frame columns:\n"); print(names(run_defense_qbgrp))
  stop("column receipt printed above -- no-placeholder law")
}
cat("--- intake: grade/snap columns on run_defense_qbgrp ---\n")
print(grep("grade|snap", names(run_defense_qbgrp), value = TRUE))

# ------------------------------------------------------------
# 1. constants -- canon gates, imported not re-derived
# ------------------------------------------------------------

X_QUAL_RD <- 3          # qualifying game = >= 3 run snaps (canon)
G_MIN_RD  <- 6          # gate = >= 6 qualifying games (canon)
BANDS_RD  <- c("DI", "ED", "LB", "S")

# ------------------------------------------------------------
# 2. THE NEW COLUMN -- within-opponent game rank
# ------------------------------------------------------------

rd_game_c3 <- run_defense_qbgrp %>%
  filter(position %in% BANDS_RD,
         snap_counts_run >= X_QUAL_RD) %>%
  select(player, player_id, position, season, week,
         qbgrp_ssn, def_ssn,
         run_snaps = snap_counts_run,
         run_grade = grades_run_defense)

cat("\n--- honesty: NA opponent-key rows among floor-passers ---\n")
print(rd_game_c3 %>% summarise(na_key = sum(is.na(qbgrp_ssn)),
                               rows = dplyr::n()))
rd_game_c3 <- rd_game_c3 %>%
  filter(!is.na(qbgrp_ssn)) %>%
  group_by(qbgrp_ssn, position) %>%
  mutate(n_pool = dplyr::n(),
         vsopp_rank = dplyr::case_when(
           dplyr::n() == 1 ~ 0.5,
           TRUE ~ percent_rank_avg(run_grade))) %>%
  ungroup()

cat("\n--- honesty: sub-gate games excluded (by season) ---\n")
print(run_defense_qbgrp %>%
        filter(position %in% BANDS_RD) %>%
        group_by(season) %>%
        summarise(games = dplyr::n(),
                  excluded = round(mean(snap_counts_run < X_QUAL_RD,
                                        na.rm = TRUE), 3),
                  .groups = "drop"), n = 12)

cat("\n--- honesty: within-opponent pool sizes ---\n")
print(rd_game_c3 %>%
        distinct(qbgrp_ssn, position, n_pool) %>%
        group_by(position) %>%
        summarise(pools = dplyr::n(),
                  p10 = quantile(n_pool, .10),
                  med = median(n_pool),
                  p90 = quantile(n_pool, .90), .groups = "drop"))

# ------------------------------------------------------------
# 3. season grain -- MEDIAN of vsopp ranks, canon gate, modal band
# ------------------------------------------------------------

rd_modal_band_c3 <- rd_game_c3 %>%
  count(player_id, season, position, wt = run_snaps, name = "sn") %>%
  group_by(player_id, season) %>%
  slice_max(sn, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, season, band = position)

rd_name_c3 <- rd_game_c3 %>%
  group_by(player_id) %>%
  slice_max(run_snaps, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, player)

rd_c3_season <- rd_game_c3 %>%
  group_by(player_id, season) %>%
  summarise(qual_g = dplyr::n(),
            run_snaps = sum(run_snaps),
            med_vsopp = median(vsopp_rank, na.rm = TRUE),
            .groups = "drop") %>%
  filter(qual_g >= G_MIN_RD) %>%
  inner_join(rd_modal_band_c3, by = c("player_id", "season")) %>%
  inner_join(rd_name_c3, by = "player_id")

stopifnot(anyDuplicated(rd_c3_season[, c("player_id", "season")]) == 0)

# ------------------------------------------------------------
# 4. THE CURRENCY -- band x season pools of the season medians
# ------------------------------------------------------------

rd_c3_pctl <- rd_c3_season %>%
  group_by(band, season) %>%
  mutate(c3_pctl = percent_rank_avg(med_vsopp)) %>%
  ungroup()

cat("\n--- coverage: gated players per band x season ---\n")
print(rd_c3_pctl %>% count(season, band) %>%
        tidyr::pivot_wider(names_from = band, values_from = n), n = 12)

cat("\n--- eyeball: 2025 top-10 by c3 (should read like an All-Pro list) ---\n")
print(rd_c3_pctl %>% filter(season == 2025) %>%
        arrange(desc(c3_pctl)) %>%
        select(player, band, qual_g, run_snaps, med_vsopp, c3_pctl) %>%
        head(10))

cat("\n--- eyeball: 2025 bottom-10 (gated, so real rotation players) ---\n")
print(rd_c3_pctl %>% filter(season == 2025) %>%
        arrange(c3_pctl) %>%
        select(player, band, qual_g, run_snaps, med_vsopp, c3_pctl) %>%
        head(10))

# ------------------------------------------------------------
# 5. SOFT CONTEXT -- lights up only if the objects are in session
# ------------------------------------------------------------

if (exists("rundef_season_pctl_sos")) {
  c1_c3_rd <- rd_c3_pctl %>%
    inner_join(rundef_season_pctl_sos %>%
                 select(player_id, season, band,
                        c1_pctl = grade_pctl),
               by = c("player_id", "season", "band"))
  
  cat("\n--- c1 vs c3: how much does slate-adjustment reorder? ---\n")
  print(c1_c3_rd %>% group_by(season) %>%
          summarise(n = dplyr::n(),
                    r = round(cor(c1_pctl, c3_pctl,
                                  use = "complete.obs"), 3),
                    .groups = "drop"), n = 12)
  
  cat("\n--- 2025 biggest movers, c3 - c1 (slate-flattered < 0 < slate-punished) ---\n")
  print(c1_c3_rd %>% filter(season == 2025) %>%
          mutate(d = round(c3_pctl - c1_pctl, 3)) %>%
          arrange(desc(abs(d))) %>%
          select(player, band, c1_pctl, c3_pctl, d) %>%
          head(12))
  
  cat("\n--- NE-2025 block: NE run defenders, both lenses ---\n")
  ne_ids_rd <- rd_game_c3 %>%
    filter(season == 2025, def_ssn == "NE2025") %>%
    distinct(player_id)
  print(c1_c3_rd %>% filter(season == 2025) %>%
          semi_join(ne_ids_rd, by = "player_id") %>%
          mutate(d = round(c3_pctl - c1_pctl, 3)) %>%
          arrange(band, desc(c3_pctl)) %>%
          select(player, band, qual_g, c1_pctl, c3_pctl, d))
} else {
  cat("\n[soft skip] rundef_season_pctl_sos not in session --",
      "source the phase-3 file for the c1 comparison + NE block\n")
}

if (exists("run_defense_opp_position_percentile")) {
  apo_rd <- tibble::as_tibble(run_defense_opp_position_percentile)
  gr_rd <- grep("grade", names(apo_rd), value = TRUE)[1]
  if (!is.na(gr_rd)) {
    cat("\n--- context: toughest / softest offenses to play run-D",
        "against (Andy's opp-percentile table) ---\n")
    print(apo_rd %>% arrange(.data[[gr_rd]]) %>% head(5))
    print(apo_rd %>% arrange(desc(.data[[gr_rd]])) %>% head(5))
  } else {
    cat("\n[soft skip] difficulty table present but grade column",
        "not recognized -- harmless\n")
  }
} else {
  cat("\n[soft skip] run_defense_opp_position_percentile not in",
      "session -- difficulty context after the exploration file runs\n")
}

# ------------------------------------------------------------
# THE COLUMN LEGEND -- prints at the end of every run
# ------------------------------------------------------------

legend_rd <- c(
  "",
  "============================================================",
  "HOW TO READ WHAT YOU JUST BUILT -- RUN DEFENSE C3",
  "============================================================",
  "GAME FRAME (rd_game_c3) -- one row per defender-game >= 3 run snaps:",
  "  qbgrp_ssn   the OFFENSE faced (team + QB group, e.g. NEMaye-2025).",
  "              An opaque label -- it IS the rank pool.",
  "  n_pool      same-position defender-games vs this offense all",
  "              season (playoffs included). QB changes splinter pools",
  "              (p10 ~ 3-4) -- the median eats that.",
  "  vsopp_rank  THIS GAME's run-defense grade ranked inside the pool:",
  "              0 = worst day any same-position defender had vs this",
  "              offense all year; 1 = best; 0.5 = only game in the",
  "              pool (singleton guard). Blowout compression is a",
  "              FEATURE: a demolition and a barely-won day both count",
  "              as 'a good day' -- nothing drags.",
  "",
  "CURRENCY (rd_c3_pctl) -- one row per gated player-season",
  "(>= 6 qualifying games):",
  "  band        modal position by run snaps (pool assignment only --",
  "              the median runs over ALL qualifying games).",
  "  qual_g      qualifying games. Low qual_g = noisier median.",
  "  run_snaps   total qualifying run snaps = THE VOLUME COLUMN. c3 is",
  "              one-game-one-vote; always pair the pctl with this.",
  "  med_vsopp   median of vsopp_rank = 'your typical day, vs every",
  "              same-position defender who faced that offense.'",
  "              0.5 = league-median day vs those offenses.",
  "  c3_pctl     where that median ranks among all gated players at",
  "              the band that season. THE CURRENCY -- for cards /",
  "              comps / models. NEVER feeds an SoS slate (FIREWALL).",
  "",
  "WORKED EXAMPLE (the 2025 run, real numbers):",
  "  Will Anderson Jr., ED: 19 qual games, 248 run snaps,",
  "  med_vsopp = 0.805 -> his median day vs each offense topped ~80%",
  "  of the ED days that offense saw all season.",
  "  c3_pctl = 0.993 -> that median sits top ~2 of the 146 gated EDs.",
  "",
  "  d = c3_pctl - c1_pctl (when the c1 block is lit):",
  "  d > 0 = slate-PUNISHED (his raw c1 percentile undersold him -- he",
  "  drew a hard diet of offenses); d < 0 = slate-FLATTERED.",
  "============================================================"
)
cat(legend_rd, sep = "\n")

# ------------------------------------------------------------
# Checkpoint (after eyeball):
# ------------------------------------------------------------
# save.image("~/rundef_c3_workspace.RData")
# system('aws s3 cp ~/rundef_c3_workspace.RData s3://nfl-pff-data-lucas/workspaces/')