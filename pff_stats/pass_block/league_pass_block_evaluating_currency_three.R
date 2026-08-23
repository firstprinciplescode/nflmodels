# ============================================================
# CURRENCY THREE -- PASS BLOCKING (standalone unit file)
#
# Ported from the signed prush currency-three template
# (league_pass_rush_evaluating_currency_three.R; round one signed
# 2026-08-16). Same skeleton. Self-contained.
#
# WHAT THE COLUMN MEANS: vsopp_rank = this game's TPS pass-block
# grade, ranked among every same-slot OL-game against the SAME
# defense (def_ssn) that season. "Your day, vs every lineman at
# your slot who faced the same pass rush." Opponent adjustment by
# construction -- no model.
#
# !! UNSIGNED GATE IN FORCE !!
# X_QUAL_PBLK is PROPOSED -- Kimi, UNSIGNED: X = 16 TPS pass-block
# snaps = Andy's own exploration floor (pff_pass_block_AWS.R line
# 113); the Phase-1 OL canon X is not in the session record Kimi
# holds. G = 6 is the house universal. Andy stamps or corrects,
# then re-source whole file.
# 2025 smell test under the proposed gate (from the port run):
# Zeitler / O'Neill / Parham / Wirfs / Beebe / Humphrey /
# Eluemunor / Steen / Thuney / Cross -- reads like an All-Pro
# list. That is the stamp evidence.
#
# THE ~50% EXCLUSION, explained: the frame carries every OL
# player-game with a single pass-block snap. X = 16 TPS snaps is
# spot-duty repellent -- half the rows are backups playing
# partial games, and the floor removes them. Receipt from the
# port run: gated coverage lands at ~1 starter per slot per team
# (25-32 per slot per season), and the pools are the cleanest of
# any unit (320 pools per slot = 32 defenses x 10 seasons
# exactly; p10/med/p90 = 6-7 / 10 / 13-14 games). The floor is
# doing its job -- the number to be comfortable with before
# stamping.
#
# RULINGS IMPORTED (do not re-litigate):
#   - RANK SPACE, not residuals. MEDIAN season aggregation.
#   - NO NEW STATISTICS: one new column (vsopp_rank), Andy's
#     pattern on an EXISTING grade column
#     (true_pass_set_grades_pass_block -- the TPS variant, handoff
#     table stanza 4).
#   - Opponent-key symmetry law: offensive units rank within
#     def_ssn (the defense faced).
#   - Pools = OL slots per canon: det_position (LT/LG/C/RG/RT).
#   - FIREWALL: currency three NEVER feeds any SoS slate.
#   - Playoffs IN: floor-gated, never week-gated.
#   - Name drift: player_id ONLY; display name via slice_max;
#     grain wall stays. tibble-at-birth; no startsWith.
#
# SOURCE ORDER: pff_pass_block_AWS.R in session (needs
# pass_block_summary_qbgrp), then this file top to bottom. Soft
# context lights up if present: tps_pass_block_opp_position_
# percentile / all_pass_block_opp_position_percentile (difficulty
# tables). The c1 block is NULL-guarded: set c1_obj_pblk to the
# Phase-1 OL canon currency name to light it (PROPOSED -- Kimi,
# UNSIGNED until named).
#
# THE COLUMN LEGEND prints at the end of every run.
# ============================================================

needed_pblk <- c("pass_block_summary_qbgrp")
missing_pblk <- needed_pblk[!vapply(needed_pblk, exists, logical(1))]
if (length(missing_pblk)) stop("missing session objects: ",
                               paste(missing_pblk, collapse = ", "))

if (!exists("percent_rank_avg")) {          # fallback only; upstream wins
  percent_rank_avg <- function(x) {
    n_valid <- sum(!is.na(x))
    if (n_valid <= 1) return(rep(0.5, length(x)))
    (rank(x, ties.method = "average", na.last = "keep") - 1) / (n_valid - 1)
  }
}

pass_block_summary_qbgrp <- tibble::as_tibble(pass_block_summary_qbgrp)

# names() receipt wall (STOP and report; no substitution).
req_pblk <- c("player", "player_id", "season", "week", "team",
              "def_ssn", "det_position",
              "true_pass_set_snap_counts_pass_block",
              "true_pass_set_grades_pass_block")
miss_pblk <- setdiff(req_pblk, names(pass_block_summary_qbgrp))
if (length(miss_pblk)) {
  cat("STOP -- pass_block_summary_qbgrp lacks required columns:\n")
  print(miss_pblk)
  cat("frame columns:\n"); print(names(pass_block_summary_qbgrp))
  stop("column receipt printed above -- no-placeholder law")
}
cat("--- intake: grade/snap columns on pass_block_summary_qbgrp ---\n")
print(grep("grade|snap", names(pass_block_summary_qbgrp), value = TRUE))

# ------------------------------------------------------------
# 1. constants -- X PROPOSED -- Kimi, UNSIGNED; G house universal
# ------------------------------------------------------------

X_QUAL_PBLK <- 16       # PROPOSED: qualifying game = >= 16 TPS
# pass-block snaps (Andy's exploration floor).
G_MIN_PBLK  <- 6        # house universal (every canon unit runs 6).

# ------------------------------------------------------------
# 2. THE NEW COLUMN -- within-opponent game rank
# ------------------------------------------------------------

pblk_game_c3 <- pass_block_summary_qbgrp %>%
  filter(true_pass_set_snap_counts_pass_block >= X_QUAL_PBLK) %>%
  select(player, player_id, det_position, season, week, team,
         def_ssn,
         pb_snaps = true_pass_set_snap_counts_pass_block,
         pb_grade = true_pass_set_grades_pass_block)

cat("\n--- honesty: NA keys / NA det_position among floor-passers ---\n")
print(pblk_game_c3 %>% summarise(na_key = sum(is.na(def_ssn)),
                                 na_det = sum(is.na(det_position)),
                                 rows = dplyr::n()))
pblk_game_c3 <- pblk_game_c3 %>%
  filter(!is.na(def_ssn), !is.na(det_position))

cat("\n--- receipt: det_position census among ranked games",
    "(pools = OL slots per canon; eyeball the labels) ---\n")
print(pblk_game_c3 %>% count(det_position, name = "games") %>%
        arrange(desc(games)))

pblk_game_c3 <- pblk_game_c3 %>%
  group_by(def_ssn, det_position) %>%
  mutate(n_pool = dplyr::n(),
         vsopp_rank = dplyr::case_when(
           dplyr::n() == 1 ~ 0.5,
           TRUE ~ percent_rank_avg(pb_grade))) %>%
  ungroup()

cat("\n--- honesty: sub-gate games excluded (by season) ---\n")
print(pass_block_summary_qbgrp %>%
        group_by(season) %>%
        summarise(games = dplyr::n(),
                  excluded = round(mean(
                    true_pass_set_snap_counts_pass_block < X_QUAL_PBLK,
                    na.rm = TRUE), 3),
                  .groups = "drop"), n = 12)

cat("\n--- honesty: within-opponent pool sizes ---\n")
print(pblk_game_c3 %>%
        distinct(def_ssn, det_position, n_pool) %>%
        group_by(det_position) %>%
        summarise(pools = dplyr::n(),
                  p10 = quantile(n_pool, .10),
                  med = median(n_pool),
                  p90 = quantile(n_pool, .90), .groups = "drop"))

# ------------------------------------------------------------
# 3. season grain -- MEDIAN of vsopp ranks, gate, modal slot
# ------------------------------------------------------------

pblk_modal_band_c3 <- pblk_game_c3 %>%
  count(player_id, season, det_position, wt = pb_snaps, name = "sn") %>%
  group_by(player_id, season) %>%
  slice_max(sn, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, season, band = det_position)

pblk_name_c3 <- pblk_game_c3 %>%
  group_by(player_id) %>%
  slice_max(pb_snaps, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, player)

pblk_c3_season <- pblk_game_c3 %>%
  group_by(player_id, season) %>%
  summarise(qual_g = dplyr::n(),
            pb_snaps = sum(pb_snaps),
            med_vsopp = median(vsopp_rank, na.rm = TRUE),
            .groups = "drop") %>%
  filter(qual_g >= G_MIN_PBLK) %>%
  inner_join(pblk_modal_band_c3, by = c("player_id", "season")) %>%
  inner_join(pblk_name_c3, by = "player_id")

stopifnot(anyDuplicated(pblk_c3_season[, c("player_id", "season")]) == 0)

# ------------------------------------------------------------
# 4. THE CURRENCY -- slot x season pools of the season medians
# ------------------------------------------------------------

pblk_c3_pctl <- pblk_c3_season %>%
  group_by(band, season) %>%
  mutate(c3_pctl = percent_rank_avg(med_vsopp)) %>%
  ungroup()

cat("\n--- coverage: gated players per slot x season ---\n")
print(pblk_c3_pctl %>% count(season, band) %>%
        tidyr::pivot_wider(names_from = band, values_from = n), n = 12)

cat("\n--- eyeball: 2025 top-10 by c3",
    "(UNSIGNED gate -- smell test before stamp) ---\n")
print(pblk_c3_pctl %>% filter(season == 2025) %>%
        arrange(desc(c3_pctl)) %>%
        select(player, band, qual_g, pb_snaps, med_vsopp, c3_pctl) %>%
        head(10))

cat("\n--- eyeball: 2025 bottom-10 ---\n")
print(pblk_c3_pctl %>% filter(season == 2025) %>%
        arrange(c3_pctl) %>%
        select(player, band, qual_g, pb_snaps, med_vsopp, c3_pctl) %>%
        head(10))

# ------------------------------------------------------------
# 5. SOFT CONTEXT
# ------------------------------------------------------------

# PROPOSED -- Kimi, UNSIGNED: the Phase-1 OL canon currency object
# name is not in the session record. Set c1_obj_pblk to the canon
# name to light the comparison up. Skips loudly until then.
c1_obj_pblk <- NULL
if (!is.null(c1_obj_pblk) && exists(c1_obj_pblk)) {
  c1_pblk <- tibble::as_tibble(get(c1_obj_pblk))
  cat("\n--- c1 object columns (pick the pctl column) ---\n")
  print(names(c1_pblk))
} else {
  cat("\n[soft skip] c1 comparison dark --",
      "set c1_obj_pblk to the Phase-1 OL currency name to light it\n")
}

cat("\n--- NE-2025 block: NE pass protectors, c3 lens ---\n")
ne_ids_pblk <- pblk_game_c3 %>%
  filter(season == 2025, team == "NE") %>%
  distinct(player_id)
print(pblk_c3_pctl %>% filter(season == 2025) %>%
        semi_join(ne_ids_pblk, by = "player_id") %>%
        arrange(band, desc(c3_pctl)) %>%
        select(player, band, qual_g, pb_snaps, med_vsopp, c3_pctl))

if (exists("tps_pass_block_opp_position_percentile")) {
  cat("\n--- context: defenses, TPS pass-pro difficulty",
      "(Andy's opp-percentile table) ---\n")
  print(tibble::as_tibble(tps_pass_block_opp_position_percentile) %>%
          head(8))
} else {
  cat("\n[soft skip] tps_pass_block_opp_position_percentile not in",
      "session\n")
}
if (exists("all_pass_block_opp_position_percentile")) {
  cat("\n--- context: defenses, all-snap pass-pro difficulty",
      "(Andy's opp-percentile table) ---\n")
  print(tibble::as_tibble(all_pass_block_opp_position_percentile) %>%
          head(8))
} else {
  cat("\n[soft skip] all_pass_block_opp_position_percentile not in",
      "session\n")
}

# ------------------------------------------------------------
# THE COLUMN LEGEND -- prints at the end of every run
# ------------------------------------------------------------

legend_pblk <- c(
  "",
  "============================================================",
  "HOW TO READ WHAT YOU JUST BUILT -- PASS BLOCKING C3",
  "============================================================",
  "GAME FRAME (pblk_game_c3) -- one row per OL-game >= 16 TPS",
  "pass-block snaps (PROPOSED floor -- UNSIGNED until Andy stamps):",
  "  def_ssn     the DEFENSE faced (e.g. NE2025). An opaque label --",
  "              it IS the rank pool. Playoffs included.",
  "  det_position the OL slot that game (LT/LG/C/RG/RT, from",
  "              play_counts). Pools are slot x defense.",
  "  n_pool      same-slot OL-games vs this defense all season. These",
  "              pools are the cleanest of any unit: exactly 320 per",
  "              slot (32 defenses x 10 seasons), median 10 games.",
  "  pb_grade    the TPS pass-block grade -- true pass sets only",
  "              (screens, play-action rollouts, spikes are out), so",
  "              the grade is the honest pass-pro rep.",
  "  vsopp_rank  THIS GAME's TPS grade ranked inside the pool:",
  "              0 = worst protection day any same-slot lineman had",
  "              vs this defense all year; 1 = best; 0.5 = singleton.",
  "",
  "CURRENCY (pblk_c3_pctl) -- one row per gated OL-season",
  "(>= 6 qualifying games):",
  "  band        modal slot by TPS snaps (pool assignment only).",
  "  qual_g      qualifying games. Low qual_g = noisier median.",
  "  pb_snaps    total qualifying TPS snaps = THE VOLUME COLUMN. c3",
  "              is one-game-one-vote; pair the pctl with this.",
  "  med_vsopp   median of vsopp_rank = 'your typical day, vs every",
  "              same-slot lineman who faced that pass rush.'",
  "  c3_pctl     where that median ranks among all gated players at",
  "              the slot that season. THE CURRENCY -- for cards /",
  "              comps / models. NEVER feeds an SoS slate (FIREWALL).",
  "",
  "WORKED EXAMPLE (the 2025 port run, real numbers):",
  "  Kevin Zeitler, RG: 9 qual games, 216 pb_snaps,",
  "  med_vsopp = 0.857 -> his median day vs each defense topped ~86%",
  "  of the RG days that defense saw; c3_pctl = 1.000 -> best among",
  "  the 28 gated RGs. Note the 9 games -- pair with pb_snaps",
  "  (volume law) before quoting.",
  "  Creed Humphrey, C: 11 games, 239 snaps, med_vsopp = 0.800,",
  "  c3_pctl = 0.967 of 31 gated centers.",
  "",
  "  d = c3_pctl - c1_pctl (once c1_obj_pblk is set):",
  "  d > 0 = slate-PUNISHED (raw c1 undersold him -- hard diet of",
  "  pass rushes); d < 0 = slate-FLATTERED.",
  "============================================================"
)
cat(legend_pblk, sep = "\n")

# ------------------------------------------------------------
# Checkpoint (after eyeball + stamps):
# ------------------------------------------------------------
# save.image("~/pass_blocking_c3_workspace.RData")
# system('aws s3 cp ~/pass_blocking_c3_workspace.RData s3://nfl-pff-data-lucas/workspaces/')