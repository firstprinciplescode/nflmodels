# ============================================================
# CURRENCY THREE -- RUN BLOCKING (standalone unit file)
#
# Ported from the signed prush currency-three template
# (league_pass_rush_evaluating_currency_three.R; round one signed
# 2026-08-16). Same skeleton. Self-contained.
#
# WHAT THE COLUMN MEANS: vsopp_rank = this game's run-block grade,
# ranked among every same-slot OL-game against the SAME defense
# (def_ssn) that season. "Your day, vs every lineman at your slot
# who faced the same defense." Opponent adjustment by construction
# -- no model.
#
# GATES STAMPED 2026-08-30. The blended entry gates sat PROPOSED
# through the port run; the 2025 smell test read like an All-Pro
# list (Brewer / Lindstrom / Wirfs / Sewell / T. Smith / A. Thomas
# / McKivitz / Bortolini / Nelson / Meinerz), and Andy's answer to
# the execution list was "i'm in agree for all of them" (via the
# Claude paste) -- that signature covers X_QUAL_RBLK = 8 and
# G_MIN_RBLK = 6 AND the round-two scheme split in section 4b:
# gap games enter at >= 7 gap run-block snaps, zone at >= 9 (his
# own exploration floors, pff_run_block_AWS.R), six qualifying
# games per scheme for a scheme season number (the house minimum
# every unit runs). One place, per the law.
#
# RULINGS IMPORTED (do not re-litigate):
#   - RANK SPACE, not residuals. MEDIAN season aggregation.
#   - NO NEW STATISTICS: one new column (vsopp_rank), Andy's pattern
#     on an EXISTING grade column (grades_run_block, the overall
#     run-block grade).
#   - Opponent-key symmetry law: offensive units rank within
#     def_ssn (the defense faced).
#   - Pools = OL slots per canon: det_position (LT/LG/C/RG/RT) --
#     det_position LIVES for offense (roadmap law).
#   - FIREWALL: currency three NEVER feeds any SoS slate.
#   - Playoffs IN: rankable games are floor-gated, never week-gated.
#   - Name drift: player_id ONLY; display name via slice_max;
#     grain wall stays. tibble-at-birth; no startsWith.
#   - SCHEME SPLIT (round two, stamped 2026-08-30): gap and zone
#     priced separately, the same way receiving prices man/zone --
#     never recombined inside the scheme numbers. The blended
#     c3_pctl column stays untouched; the scheme columns are added
#     next to it, so nothing that reads the old column breaks.
#
# COLUMN NOTE: Andy's exploration of this frame (pff_run_block_AWS.R)
# uses ONLY the gap_/zone_ grade+snap variants. This currency targets
# the overall run-block grade/snap pair for the blended column
# (confirmed present on the frame -- the intake wall checked). If a
# future frame ever lacks them, the wall prints every run_block
# column and STOPs -- no substitution. The scheme-split (gap c3 /
# zone c3) was the round-two knob; the ruling landed 2026-08-30 and
# the split ships in section 4b.
#
# CENSUS NOTE (2026-08-18 port run): one det_position == "TE-L" game
# in ten seasons (1 of 29,157). It forms a one-game pool, the
# singleton guard prices it 0.5, and the median absorbs it.
# Harmless by construction -- on the record.
#
# SOURCE ORDER: pff_run_block_AWS.R in session (needs
# run_block_summary_qbgrp), then this file top to bottom. Soft
# context lights up if present: gap_opp_position_percentile /
# zone_opp_position_percentile (difficulty tables). The c1 block is
# NULL-guarded: set c1_obj_rblk to the Phase-1 OL canon currency
# name to light it (PROPOSED -- Kimi, UNSIGNED until named).
#
# THE COLUMN LEGEND prints at the end of every run.
# ============================================================

needed_rblk <- c("run_block_summary_qbgrp")
missing_rblk <- needed_rblk[!vapply(needed_rblk, exists, logical(1))]
if (length(missing_rblk)) stop("missing session objects: ",
                               paste(missing_rblk, collapse = ", "))

if (!exists("percent_rank_avg")) {          # fallback only; upstream wins
  percent_rank_avg <- function(x) {
    n_valid <- sum(!is.na(x))
    if (n_valid <= 1) return(rep(0.5, length(x)))
    (rank(x, ties.method = "average", na.last = "keep") - 1) / (n_valid - 1)
  }
}

run_block_summary_qbgrp <- tibble::as_tibble(run_block_summary_qbgrp)

cat("--- intake: every run_block column on the frame ---\n")
print(grep("run_block", names(run_block_summary_qbgrp), value = TRUE))

# names() receipt wall (STOP and report; no substitution).
req_rblk <- c("player", "player_id", "season", "week", "team",
              "def_ssn", "det_position",
              "snap_counts_run_block", "grades_run_block")
miss_rblk <- setdiff(req_rblk, names(run_block_summary_qbgrp))
if (length(miss_rblk)) {
  cat("STOP -- run_block_summary_qbgrp lacks required columns:\n")
  print(miss_rblk)
  cat("frame columns:\n"); print(names(run_block_summary_qbgrp))
  cat("(the exploration file only ever used the gap_/zone_ variants --\n")
  cat(" if those are all that exist, the grade choice is Andy's ruling)\n")
  stop("column receipt printed above -- no-placeholder law")
}

# ------------------------------------------------------------
# 1. constants -- STAMPED 2026-08-30 (see header)
# ------------------------------------------------------------

X_QUAL_RBLK <- 8        # STAMPED: qualifying game = >= 8 overall
# run-block snaps. Basis: midpoint of Andy's
# exploration floors (gap >= 7 / zone >= 9).
G_MIN_RBLK  <- 6        # house universal (every canon unit
# runs G = 6); same stamp.
X_QUAL_GAP  <- 7        # STAMPED: a game enters the GAP pool at
# >= 7 gap run-block snaps (Andy's exploration
# floor, pff_run_block_AWS.R).
X_QUAL_ZONE <- 9        # STAMPED: a game enters the ZONE pool at
# >= 9 zone run-block snaps (Andy's exploration
# floor, pff_run_block_AWS.R).

# ------------------------------------------------------------
# 2. THE NEW COLUMN -- within-opponent game rank
# ------------------------------------------------------------

rblk_game_c3 <- run_block_summary_qbgrp %>%
  filter(snap_counts_run_block >= X_QUAL_RBLK) %>%
  select(player, player_id, det_position, season, week, team,
         def_ssn,
         rb_snaps = snap_counts_run_block,
         rb_grade = grades_run_block)

cat("\n--- honesty: NA keys / NA det_position among floor-passers ---\n")
print(rblk_game_c3 %>% summarise(na_key = sum(is.na(def_ssn)),
                                 na_det = sum(is.na(det_position)),
                                 rows = dplyr::n()))
rblk_game_c3 <- rblk_game_c3 %>%
  filter(!is.na(def_ssn), !is.na(det_position))

cat("\n--- receipt: det_position census among ranked games",
    "(pools = OL slots per canon; eyeball the labels) ---\n")
print(rblk_game_c3 %>% count(det_position, name = "games") %>%
        arrange(desc(games)))

rblk_game_c3 <- rblk_game_c3 %>%
  group_by(def_ssn, det_position) %>%
  mutate(n_pool = dplyr::n(),
         vsopp_rank = dplyr::case_when(
           dplyr::n() == 1 ~ 0.5,
           TRUE ~ percent_rank_avg(rb_grade))) %>%
  ungroup()

cat("\n--- honesty: sub-gate games excluded (by season) ---\n")
print(run_block_summary_qbgrp %>%
        group_by(season) %>%
        summarise(games = dplyr::n(),
                  excluded = round(mean(
                    snap_counts_run_block < X_QUAL_RBLK,
                    na.rm = TRUE), 3),
                  .groups = "drop"), n = 12)

cat("\n--- honesty: within-opponent pool sizes ---\n")
print(rblk_game_c3 %>%
        distinct(def_ssn, det_position, n_pool) %>%
        group_by(det_position) %>%
        summarise(pools = dplyr::n(),
                  p10 = quantile(n_pool, .10),
                  med = median(n_pool),
                  p90 = quantile(n_pool, .90), .groups = "drop"))

# ------------------------------------------------------------
# 3. season grain -- MEDIAN of vsopp ranks, gate, modal slot
# ------------------------------------------------------------

rblk_modal_band_c3 <- rblk_game_c3 %>%
  count(player_id, season, det_position, wt = rb_snaps, name = "sn") %>%
  group_by(player_id, season) %>%
  slice_max(sn, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, season, band = det_position)

rblk_name_c3 <- rblk_game_c3 %>%
  group_by(player_id) %>%
  slice_max(rb_snaps, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, player)

rblk_c3_season <- rblk_game_c3 %>%
  group_by(player_id, season) %>%
  summarise(qual_g = dplyr::n(),
            rb_snaps = sum(rb_snaps),
            med_vsopp = median(vsopp_rank, na.rm = TRUE),
            .groups = "drop") %>%
  filter(qual_g >= G_MIN_RBLK) %>%
  inner_join(rblk_modal_band_c3, by = c("player_id", "season")) %>%
  inner_join(rblk_name_c3, by = "player_id")

stopifnot(anyDuplicated(rblk_c3_season[, c("player_id", "season")]) == 0)

# ------------------------------------------------------------
# 4. THE CURRENCY -- slot x season pools of the season medians
# ------------------------------------------------------------

rblk_c3_pctl <- rblk_c3_season %>%
  group_by(band, season) %>%
  mutate(c3_pctl = percent_rank_avg(med_vsopp)) %>%
  ungroup()

cat("\n--- coverage: gated players per slot x season ---\n")
print(rblk_c3_pctl %>% count(season, band) %>%
        tidyr::pivot_wider(names_from = band, values_from = n), n = 12)

cat("\n--- eyeball: 2025 top-10 by c3 ---\n")
print(rblk_c3_pctl %>% filter(season == 2025) %>%
        arrange(desc(c3_pctl)) %>%
        select(player, band, qual_g, rb_snaps, med_vsopp, c3_pctl) %>%
        head(10))

cat("\n--- eyeball: 2025 bottom-10 ---\n")
print(rblk_c3_pctl %>% filter(season == 2025) %>%
        arrange(c3_pctl) %>%
        select(player, band, qual_g, rb_snaps, med_vsopp, c3_pctl) %>%
        head(10))

# ------------------------------------------------------------
# 4b. THE SCHEME SPLIT -- gap c3 / zone c3 (round two; ruling
#     landed 2026-08-30: "i'm in agree for all of them"). Same
#     skeleton, run twice -- once on the gap grade, once on the
#     zone grade. MEDIAN stays (signed template law): the split
#     numbers will NOT equal the exploration frame's means --
#     same player, same lens, different season math. Pools stay
#     def_ssn x det_position. A scheme pctl exists only where
#     the player passes that scheme's game floor >= G_MIN times
#     -- honest holes, printed, never filled. The thinning cuts
#     both ways, on the record: fewer pool members per def_ssn x
#     slot (more singleton 0.5s), and a balanced lineman (say 6
#     gap + 6 zone snaps a game) qualifies BLENDED (12 >= 8) yet
#     fails BOTH scheme floors (7 / 9) -- he shows a blended
#     number and two scheme holes.
# ------------------------------------------------------------

# intake wall, scheme columns (same no-substitution law)
req_rblk_sc <- c("gap_grades_run_block", "gap_snap_counts_run_block",
                 "zone_grades_run_block", "zone_snap_counts_run_block")
miss_rblk_sc <- setdiff(req_rblk_sc, names(run_block_summary_qbgrp))
if (length(miss_rblk_sc)) {
  cat("STOP -- run_block_summary_qbgrp lacks the scheme columns:\n")
  print(miss_rblk_sc)
  cat("frame columns:\n"); print(names(run_block_summary_qbgrp))
  stop("column receipt printed above -- no-placeholder law")
}

# one skeleton, two schemes; the tag keeps the receipts apart
scheme_c3_rblk <- function(grade_col, snap_col, x_qual, tag) {
  g <- run_block_summary_qbgrp %>%
    filter(.data[[snap_col]] >= x_qual) %>%
    select(player, player_id, det_position, season, week, team,
           def_ssn,
           rb_snaps = .data[[snap_col]],
           rb_grade = .data[[grade_col]]) %>%
    filter(!is.na(def_ssn), !is.na(det_position)) %>%
    group_by(def_ssn, det_position) %>%
    mutate(n_pool = dplyr::n(),
           vsopp_rank = dplyr::case_when(
             dplyr::n() == 1 ~ 0.5,
             TRUE ~ percent_rank_avg(rb_grade))) %>%
    ungroup()
  
  cat("\n--- scheme split [", tag, "] sub-floor games excluded,",
      "by season ---\n", sep = "")
  print(run_block_summary_qbgrp %>%
          group_by(season) %>%
          summarise(games = dplyr::n(),
                    excluded = round(mean(
                      .data[[snap_col]] < x_qual,
                      na.rm = TRUE), 3),
                    .groups = "drop"), n = 12)
  
  cat("\n--- scheme split [", tag, "] within-opponent pool sizes",
      "---\n", sep = "")
  print(g %>% distinct(def_ssn, det_position, n_pool) %>%
          group_by(det_position) %>%
          summarise(pools = dplyr::n(),
                    p10 = quantile(n_pool, .10),
                    med = median(n_pool),
                    p90 = quantile(n_pool, .90), .groups = "drop"))
  
  s <- g %>%
    group_by(player_id, season) %>%
    summarise(qual_g = dplyr::n(),
              sc_snaps = sum(rb_snaps),
              med_vsopp = median(vsopp_rank, na.rm = TRUE),
              .groups = "drop") %>%
    filter(qual_g >= G_MIN_RBLK) %>%
    inner_join(rblk_modal_band_c3, by = c("player_id", "season")) %>%
    inner_join(rblk_name_c3, by = "player_id") %>%
    group_by(band, season) %>%
    mutate(c3_pctl = percent_rank_avg(med_vsopp)) %>%
    ungroup()
  
  cat("\n--- scheme split [", tag, "] gated player-seasons,",
      "slot x season ---\n", sep = "")
  print(s %>% count(season, band) %>%
          tidyr::pivot_wider(names_from = band, values_from = n),
        n = 12)
  cat("\n--- scheme split [", tag, "] 2025 top-10 ---\n", sep = "")
  print(s %>% filter(season == 2025) %>%
          arrange(desc(c3_pctl)) %>%
          select(player, band, qual_g, sc_snaps, med_vsopp,
                 c3_pctl) %>% head(10))
  s
}

c3_gap_rblk  <- scheme_c3_rblk("gap_grades_run_block",
                               "gap_snap_counts_run_block",
                               X_QUAL_GAP, "gap")
c3_zone_rblk <- scheme_c3_rblk("zone_grades_run_block",
                               "zone_snap_counts_run_block",
                               X_QUAL_ZONE, "zone")

# honesty: scheme seasons with NO blended row (a scheme-heavy
# player can pass a scheme floor six times yet never pass the
# blended 8-snap overall floor six times). Receipt only -- the
# currency stays blended-master.
orp_g_rblk <- anti_join(c3_gap_rblk, rblk_c3_pctl,
                        by = c("player_id", "season"))
orp_z_rblk <- anti_join(c3_zone_rblk, rblk_c3_pctl,
                        by = c("player_id", "season"))
cat("\n--- scheme seasons with no blended row (receipt only):",
    nrow(orp_g_rblk), "gap |", nrow(orp_z_rblk), "zone ---\n")

rblk_c3_pctl <- rblk_c3_pctl %>%
  left_join(c3_gap_rblk %>%
              select(player_id, season,
                     gap_qual_g = qual_g,
                     gap_snaps = sc_snaps,
                     gap_med_vsopp = med_vsopp,
                     gap_c3_pctl = c3_pctl),
            by = c("player_id", "season")) %>%
  left_join(c3_zone_rblk %>%
              select(player_id, season,
                     zone_qual_g = qual_g,
                     zone_snaps = sc_snaps,
                     zone_med_vsopp = med_vsopp,
                     zone_c3_pctl = c3_pctl),
            by = c("player_id", "season"))

stopifnot(anyDuplicated(rblk_c3_pctl[, c("player_id", "season")]) == 0)

# ------------------------------------------------------------
# 5. SOFT CONTEXT
# ------------------------------------------------------------

# PROPOSED -- Kimi, UNSIGNED: the Phase-1 OL canon currency object
# name is not in the session record. Set c1_obj_rblk to the canon
# name (a *_season_pctl_sos-style frame with player_id, season, and
# a grade-pctl column) to light the comparison up. Skips loudly
# until then; nothing substitutes.
c1_obj_rblk <- NULL
if (!is.null(c1_obj_rblk) && exists(c1_obj_rblk)) {
  c1_rblk <- tibble::as_tibble(get(c1_obj_rblk))
  cat("\n--- c1 object columns (pick the pctl column) ---\n")
  print(names(c1_rblk))
} else {
  cat("\n[soft skip] c1 comparison dark --",
      "set c1_obj_rblk to the Phase-1 OL currency name to light it\n")
}

cat("\n--- NE-2025 block: NE run blockers, c3 lens ---\n")
ne_ids_rblk <- rblk_game_c3 %>%
  filter(season == 2025, team == "NE") %>%
  distinct(player_id)
print(rblk_c3_pctl %>% filter(season == 2025) %>%
        semi_join(ne_ids_rblk, by = "player_id") %>%
        arrange(band, desc(c3_pctl)) %>%
        select(player, band, qual_g, rb_snaps, med_vsopp, c3_pctl,
               gap_c3_pctl, zone_c3_pctl))

if (exists("gap_opp_position_percentile")) {
  cat("\n--- context: defenses, gap-scheme difficulty",
      "(Andy's opp-percentile table) ---\n")
  print(tibble::as_tibble(gap_opp_position_percentile) %>% head(8))
} else {
  cat("\n[soft skip] gap_opp_position_percentile not in session\n")
}
if (exists("zone_opp_position_percentile")) {
  cat("\n--- context: defenses, zone-scheme difficulty",
      "(Andy's opp-percentile table) ---\n")
  print(tibble::as_tibble(zone_opp_position_percentile) %>% head(8))
} else {
  cat("\n[soft skip] zone_opp_position_percentile not in session\n")
}

# ------------------------------------------------------------
# THE COLUMN LEGEND -- prints at the end of every run
# ------------------------------------------------------------

legend_rblk <- c(
  "",
  "============================================================",
  "HOW TO READ WHAT YOU JUST BUILT -- RUN BLOCKING C3",
  "============================================================",
  "GAME FRAME (rblk_game_c3) -- one row per OL-game >= 8 run-block",
  "snaps:",
  "  def_ssn     the DEFENSE faced (e.g. NE2025). An opaque label --",
  "              it IS the rank pool. Playoffs included.",
  "  det_position the OL slot that game (LT/LG/C/RG/RT, from",
  "              play_counts). Pools are slot x defense: a guard's day",
  "              is only ever ranked against guards.",
  "  n_pool      same-slot OL-games vs this defense all season.",
  "  vsopp_rank  THIS GAME's overall run-block grade ranked inside",
  "              the pool: 0 = worst day any same-slot lineman had vs",
  "              this defense all year; 1 = best; 0.5 = only game in",
  "              the pool (singleton guard -- that is also how the",
  "              lone TE-L game in ten seasons got priced).",
  "",
  "CURRENCY (rblk_c3_pctl) -- one row per gated OL-season",
  "(>= 6 qualifying games):",
  "  band        modal slot by run-block snaps (pool assignment only",
  "              -- the median runs over ALL qualifying games).",
  "  qual_g      qualifying games. Low qual_g = noisier median.",
  "  rb_snaps    total qualifying run-block snaps = THE VOLUME COLUMN.",
  "              c3 is one-game-one-vote; pair the pctl with this.",
  "  med_vsopp   median of vsopp_rank = 'your typical day, vs every",
  "              same-slot lineman who faced that defense.'",
  "  c3_pctl     where that median ranks among all gated players at",
  "              the slot that season. THE CURRENCY -- for cards /",
  "              comps / models. NEVER feeds an SoS slate (FIREWALL).",
  "",
  "  THE SCHEME SPLIT (round two, stamped 2026-08-30):",
  "  gap_c3_pctl / zone_c3_pctl",
  "              the same currency computed on gap-scheme plays only",
  "              / zone-scheme plays only (game floors >= 7 / >= 9",
  "              scheme snaps, six qualifying games per scheme).",
  "              MEDIAN law -- these will NOT equal the exploration",
  "              frame's mean-based numbers; same player, same lens,",
  "              different season math. Pair each with its volume:",
  "              gap_qual_g / gap_snaps, zone_qual_g / zone_snaps.",
  "              '--' (NA) = the player never passed that scheme's",
  "              floors enough times -- an honest hole, never filled.",
  "",
  "WORKED EXAMPLE (the 2025 port run, real numbers):",
  "  Chris Lindstrom, RG: 17 qual games, 463 rb_snaps,",
  "  med_vsopp = 0.938 -> his median day vs each defense topped ~94%",
  "  of the RG days that defense saw all season.",
  "  c3_pctl = 1.000 -> the best median among the 38 gated RGs of 2025.",
  "",
  "  d = c3_pctl - c1_pctl (once c1_obj_rblk is set):",
  "  d > 0 = slate-PUNISHED (raw c1 undersold him -- hard diet of",
  "  run defenses); d < 0 = slate-FLATTERED.",
  "============================================================"
)
cat(legend_rblk, sep = "\n")

# ------------------------------------------------------------
# Checkpoint (after eyeball + stamps):
# ------------------------------------------------------------
# save.image("~/run_blocking_c3_workspace.RData")
