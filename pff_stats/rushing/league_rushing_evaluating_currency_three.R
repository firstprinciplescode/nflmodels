# ============================================================
# CURRENCY THREE -- RB RUSHING (standalone unit file)
#
# Ported from the signed prush currency-three template
# (league_pass_rush_evaluating_currency_three.R; round one signed
# 2026-08-16). Same skeleton. Self-contained.
#
# WHAT THE COLUMN MEANS: vsopp_rank = this game's rushing grade,
# ranked among every back-game against the SAME defense (def_ssn)
# that season. "Your day, vs every back who faced the same run
# defense." Opponent adjustment by construction -- no model.
#
# RULINGS IMPORTED (do not re-litigate):
#   - RANK SPACE, not residuals. MEDIAN season aggregation.
#   - NO NEW STATISTICS: one new column (vsopp_rank), Andy's pattern
#     on an EXISTING grade column (grades_run).
#   - Gates = canon, imported from the phase-4 header (ruled
#     2026-08-14): X = 4 attempts, G = 6 games, single HB pool.
#     Canon currency law: the qualifying-game filter is
#     attempts-only -- no position filter on the currency side
#     ("HB filter kills QB kneels" lives upstream in the feed).
#     band = "HB" constant holds the stanza shape; it is not a
#     real band split.
#   - Opponent-key symmetry law: offensive units rank within
#     def_ssn (the defense faced).
#   - FIREWALL: currency three NEVER feeds any SoS slate.
#   - Playoffs IN: rankable games are floor-gated, never week-gated.
#   - Name drift: season grain groups by player_id ONLY; display
#     name via career-max-snaps slice_max; grain wall stays.
#   - tibble-at-birth; no startsWith anywhere; keys are opaque.
#
# SOURCE ORDER: phase-4 build in session (needs rushing_qbgrp),
# then this file top to bottom. Soft context lights up if present:
# rush_season_pctl_sos (c1, for the comparison + NE block).
# No rushing opponent-difficulty frame exists in the exploration
# set -- this unit has no line-217 analog (soft skip, on record).
#
# THE COLUMN LEGEND prints at the end of every run.
# ============================================================

needed_ru <- c("rushing_qbgrp")
missing_ru <- needed_ru[!vapply(needed_ru, exists, logical(1))]
if (length(missing_ru)) stop("missing session objects: ",
                             paste(missing_ru, collapse = ", "))

if (!exists("percent_rank_avg")) {          # fallback only; upstream wins
  percent_rank_avg <- function(x) {
    n_valid <- sum(!is.na(x))
    if (n_valid <= 1) return(rep(0.5, length(x)))
    (rank(x, ties.method = "average", na.last = "keep") - 1) / (n_valid - 1)
  }
}

rushing_qbgrp <- tibble::as_tibble(rushing_qbgrp)

# names() receipt wall (STOP and report; no substitution).
req_ru <- c("player", "player_id", "season", "week", "team",
            "def_ssn", "attempts", "grades_run")
miss_ru <- setdiff(req_ru, names(rushing_qbgrp))
if (length(miss_ru)) {
  cat("STOP -- rushing_qbgrp lacks required columns:\n")
  print(miss_ru)
  cat("frame columns:\n"); print(names(rushing_qbgrp))
  stop("column receipt printed above -- no-placeholder law")
}
cat("--- intake: grade/snap columns on rushing_qbgrp ---\n")
print(grep("grade|snap|attempt", names(rushing_qbgrp), value = TRUE))

# ------------------------------------------------------------
# 1. constants -- canon gates, imported not re-derived
# ------------------------------------------------------------

X_QUAL_RU <- 4          # qualifying game = >= 4 attempts (canon)
G_MIN_RU  <- 6          # gate = >= 6 qualifying games (canon)
BAND_RU   <- "HB"       # single pool (phase-4 law; shape-keeping only)

# ------------------------------------------------------------
# 2. THE NEW COLUMN -- within-opponent game rank
# ------------------------------------------------------------

ru_game_c3 <- rushing_qbgrp %>%
  filter(attempts >= X_QUAL_RU) %>%
  mutate(band = BAND_RU) %>%
  select(player, player_id, band, season, week, team,
         def_ssn,
         atts = attempts,
         run_grade = grades_run)

cat("\n--- honesty: NA opponent-key rows among floor-passers ---\n")
print(ru_game_c3 %>% summarise(na_key = sum(is.na(def_ssn)),
                               rows = dplyr::n()))
ru_game_c3 <- ru_game_c3 %>%
  filter(!is.na(def_ssn)) %>%
  group_by(def_ssn, band) %>%
  mutate(n_pool = dplyr::n(),
         vsopp_rank = dplyr::case_when(
           dplyr::n() == 1 ~ 0.5,
           TRUE ~ percent_rank_avg(run_grade))) %>%
  ungroup()

cat("\n--- honesty: sub-gate games excluded (by season) ---\n")
print(rushing_qbgrp %>%
        group_by(season) %>%
        summarise(games = dplyr::n(),
                  excluded = round(mean(attempts < X_QUAL_RU,
                                        na.rm = TRUE), 3),
                  .groups = "drop"), n = 12)

cat("\n--- honesty: within-opponent pool sizes ---\n")
print(ru_game_c3 %>%
        distinct(def_ssn, n_pool) %>%
        summarise(pools = dplyr::n(),
                  p10 = quantile(n_pool, .10),
                  med = median(n_pool),
                  p90 = quantile(n_pool, .90)))

# ------------------------------------------------------------
# 3. season grain -- MEDIAN of vsopp ranks, canon gate, single pool
# ------------------------------------------------------------

ru_name_c3 <- ru_game_c3 %>%
  group_by(player_id) %>%
  slice_max(atts, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, player)

ru_c3_season <- ru_game_c3 %>%
  group_by(player_id, season) %>%
  summarise(qual_g = dplyr::n(),
            atts = sum(atts),
            med_vsopp = median(vsopp_rank, na.rm = TRUE),
            band = BAND_RU,
            .groups = "drop") %>%
  filter(qual_g >= G_MIN_RU) %>%
  inner_join(ru_name_c3, by = "player_id")

stopifnot(anyDuplicated(ru_c3_season[, c("player_id", "season")]) == 0)

# ------------------------------------------------------------
# 4. THE CURRENCY -- season pools of the season medians (single pool)
# ------------------------------------------------------------

ru_c3_pctl <- ru_c3_season %>%
  group_by(band, season) %>%
  mutate(c3_pctl = percent_rank_avg(med_vsopp)) %>%
  ungroup()

cat("\n--- coverage: gated players per season",
    "(phase-4 receipt: 2025 = 64 = 2.0 per team) ---\n")
print(ru_c3_pctl %>% count(season), n = 12)

cat("\n--- eyeball: 2025 top-10 by c3 (should read like the rushing leaderboard) ---\n")
print(ru_c3_pctl %>% filter(season == 2025) %>%
        arrange(desc(c3_pctl)) %>%
        select(player, qual_g, atts, med_vsopp, c3_pctl) %>%
        head(10))

cat("\n--- eyeball: 2025 bottom-10 ---\n")
print(ru_c3_pctl %>% filter(season == 2025) %>%
        arrange(c3_pctl) %>%
        select(player, qual_g, atts, med_vsopp, c3_pctl) %>%
        head(10))

# ------------------------------------------------------------
# 5. SOFT CONTEXT -- lights up only if the objects are in session
# ------------------------------------------------------------

if (exists("rush_season_pctl_sos")) {
  c1_c3_ru <- ru_c3_pctl %>%
    inner_join(rush_season_pctl_sos %>%
                 select(player_id, season,
                        c1_pctl = grun_pctl),
               by = c("player_id", "season"))
  
  cat("\n--- c1 vs c3: how much does slate-adjustment reorder? ---\n")
  print(c1_c3_ru %>% group_by(season) %>%
          summarise(n = dplyr::n(),
                    r = round(cor(c1_pctl, c3_pctl,
                                  use = "complete.obs"), 3),
                    .groups = "drop"), n = 12)
  
  cat("\n--- 2025 biggest movers, c3 - c1 (slate-flattered < 0 < slate-punished) ---\n")
  print(c1_c3_ru %>% filter(season == 2025) %>%
          mutate(d = round(c3_pctl - c1_pctl, 3)) %>%
          arrange(desc(abs(d))) %>%
          select(player, c1_pctl, c3_pctl, d) %>%
          head(12))
  
  cat("\n--- NE-2025 block: NE backs, both lenses ---\n")
  ne_ids_ru <- ru_game_c3 %>%
    filter(season == 2025, team == "NE") %>%
    distinct(player_id)
  print(c1_c3_ru %>% filter(season == 2025) %>%
          semi_join(ne_ids_ru, by = "player_id") %>%
          mutate(d = round(c3_pctl - c1_pctl, 3)) %>%
          arrange(desc(c3_pctl)) %>%
          select(player, qual_g, c1_pctl, c3_pctl, d))
} else {
  cat("\n[soft skip] rush_season_pctl_sos not in session --",
      "source the phase-4 file for the c1 comparison + NE block\n")
}

cat("\n[soft skip] no rushing opponent-difficulty frame exists in the",
    "exploration set -- this unit has no line-217 analog\n")

# ------------------------------------------------------------
# THE COLUMN LEGEND -- prints at the end of every run
# ------------------------------------------------------------

legend_ru <- c(
  "",
  "============================================================",
  "HOW TO READ WHAT YOU JUST BUILT -- RB RUSHING C3",
  "============================================================",
  "GAME FRAME (ru_game_c3) -- one row per back-game >= 4 attempts:",
  "  def_ssn     the DEFENSE faced (e.g. NE2025). An opaque label --",
  "              it IS the rank pool. Playoffs included.",
  "  n_pool      back-games vs this defense all season. def_ssn pools",
  "              do NOT splinter (a defense is itself all year) --",
  "              unlike qbgrp pools on the defensive units.",
  "  vsopp_rank  THIS GAME's rushing grade ranked inside the pool:",
  "              0 = worst day any back had vs this defense all year;",
  "              1 = best; 0.5 = only game in the pool (singleton",
  "              guard). Blowout compression is a FEATURE.",
  "",
  "CURRENCY (ru_c3_pctl) -- one row per gated back-season",
  "(>= 6 qualifying games; single HB pool, no band split):",
  "  qual_g      qualifying games. Committee backs live at 6-10 --",
  "              noisier medians; that is why the c1-vs-c3 r runs",
  "              looser here (.70-.86) than on prush (.82-.88).",
  "  atts        total qualifying attempts = THE VOLUME COLUMN. c3 is",
  "              one-game-one-vote; always pair the pctl with this.",
  "  med_vsopp   median of vsopp_rank = 'your typical day, vs every",
  "              back who faced that defense.' 0.5 = league-median.",
  "  c3_pctl     where that median ranks among all gated backs that",
  "              season. THE CURRENCY -- for cards / comps / models.",
  "              NEVER feeds an SoS slate (FIREWALL).",
  "",
  "WORKED EXAMPLES (the 2025 run, real numbers):",
  "  De'Von Achane: 16 qual games, 238 atts, med_vsopp = 0.818 ->",
  "  his median day topped ~82% of the back-days each defense saw;",
  "  c3_pctl = 0.984 -> 2nd of the 64 gated backs.",
  "  Cam Skattebo tops the board (1.000) on 6 games / 96 atts --",
  "  legal, but pair it with atts before quoting (volume law).",
  "  Christian McCaffrey sits bottom-board (0.000) at 337 atts:",
  "  this stat reads his TYPICAL day, not his volume -- the volume",
  "  caveat from the handoff applies in both directions.",
  "",
  "  d = c3_pctl - c1_pctl (when the c1 block is lit):",
  "  d > 0 = slate-PUNISHED (raw c1 undersold him -- hard diet of",
  "  run defenses); d < 0 = slate-FLATTERED.",
  "============================================================"
)
cat(legend_ru, sep = "\n")

# ------------------------------------------------------------
# Checkpoint (after eyeball):
# ------------------------------------------------------------
# save.image("~/rushing_c3_workspace.RData")
# system('aws s3 cp ~/rushing_c3_workspace.RData s3://nfl-pff-data-lucas/workspaces/')