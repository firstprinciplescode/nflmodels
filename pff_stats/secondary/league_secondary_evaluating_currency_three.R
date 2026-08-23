# ============================================================
# COVERAGE (SECONDARY) -- CURRENCY THREE, ROUND ONE PORT
# Written 2026-08-18 on Andy's go: "HERE IS SECONDARY ... THE
# CURRENCY_THREE SHIT."
#
# RULINGS CARRIED (the signed round-one template,
# league_pass_rush_evaluating_currency_three.R):
#   - RANK SPACE, not residuals. Blowout compression is a feature.
#   - NO NEW STATISTICS: one new column only -- percent_rank_avg +
#     n()==1 -> 0.5 singleton guard + opponent grouping, applied to
#     grade columns that already exist.
#   - MEDIAN season aggregation.
#   - Gates = canon, imported not re-derived: X_MAN = 6 man snaps /
#     G_MAN = 6 games; X_ZONE = 13 zone snaps / G_ZONE = 6 games
#     (stamped 2026-08-16 off the v1.1 ritual run; do not
#     re-litigate).
#   - FIREWALL: currency three NEVER feeds any SoS slate.
#
# COVERAGE LAW CARRIED (canon 6b, new_england_opp_secondary_schedule.R):
#   - TWO SPLITS, NEVER RECOMBINED: man and zone price separately,
#     per split per band per season. ~11% of coverage snaps are
#     neither man nor zone; that bucket lives only in the Phase 6
#     combined currency and never enters here.
#   - BAND IDENTITY: one band per player-season -- modal band4 by
#     COMBINED coverage snaps on the un-gated frame (canon
#     modal_band_mz). Bands: CB / SCB / S / LB (SCB un-folded at
#     birth). Identity vs currency are two different questions with
#     two different gates -- the modal map is identity; the X gates
#     below are currency entry.
#   - OPPONENT KEY: coverage is a DEFENSIVE unit, so game ranks are
#     within qbgrp_ssn (the PASSING GAME faced), per the opponent-
#     key symmetry law. "Your day vs every same-band defender who
#     faced the same passing game that season."
#
# PROPOSED -- Kimi, UNSIGNED (one coverage-specific extension of the
#   signed template): pools are qbgrp_ssn x MODAL BAND per split
#   (the template pooled qbgrp_ssn x position for ED/DI; coverage's
#   canon identity is the four-band modal map). Everything else is
#   the signed template verbatim.
#
# SOURCE ORDER: source new_england_opp_secondary_schedule.R FIRST
#   (needs its session objects: secondary_qbgrp_mz, modal_band_mz,
#   CM, CZ, and the stamped gate constants), then this file, top to
#   bottom. Soft joins (skipped silently if absent):
#   cov_season_pctl_mz (currency one, for the c1 comparison + NE
#   block), league_cov (the league-lens differential; context only,
#   c3 never feeds it).
#   Re-source WHOLE file after any edit. ASCII only.
# ============================================================

needed_c3 <- c("secondary_qbgrp_mz", "modal_band_mz", "CM", "CZ",
               "X_MAN", "G_MAN", "X_ZONE", "G_ZONE")
missing_c3 <- needed_c3[!vapply(needed_c3, exists, logical(1))]
if (length(missing_c3)) stop("missing session objects: ",
                             paste(missing_c3, collapse = ", "),
                             " -- source new_england_opp_secondary_schedule.R first")

if (!exists("percent_rank_avg")) {          # fallback only; upstream wins
  percent_rank_avg <- function(x) {
    n_valid <- sum(!is.na(x))
    if (n_valid <= 1) return(rep(0.5, length(x)))
    (rank(x, ties.method = "average", na.last = "keep") - 1) / (n_valid - 1)
  }
}

library(dplyr)

secondary_qbgrp_mz <- tibble::as_tibble(secondary_qbgrp_mz)
modal_band_mz      <- tibble::as_tibble(modal_band_mz)

# ------------------------------------------------------------
# 0. COLUMN WALL -- the split card was resolved live by canon 6b;
#    this file never hardcodes split column names. Verify the four
#    columns c3 needs exist, print the resolution, stop loud if not.
# ------------------------------------------------------------

key_cols_c3 <- c("player", "player_id", "season", "week",
                 "qbgrp_ssn", "def_ssn")
need_cols_c3 <- c(key_cols_c3,
                  unname(CM[c("grade", "snaps")]),
                  unname(CZ[c("grade", "snaps")]))
col_miss_c3 <- need_cols_c3[!need_cols_c3 %in% names(secondary_qbgrp_mz)]
cat("--- c3 column resolution (live, from canon's split card) ---\n")
cat("man  grade:", unname(CM["grade"]), "| man  snaps:", unname(CM["snaps"]), "\n")
cat("zone grade:", unname(CZ["grade"]), "| zone snaps:", unname(CZ["snaps"]), "\n")
if (length(col_miss_c3)) {
  cat("MISSING COLUMNS:", paste(col_miss_c3, collapse = ", "), "\n")
  cat("frame columns:\n"); print(names(secondary_qbgrp_mz))
  stop("c3 coverage column wall failed -- do not substitute; report")
}

# ------------------------------------------------------------
# 1. THE STANZA -- one split at a time. Template-faithful:
#    game floor -> modal band -> rank within qbgrp_ssn x band ->
#    median season -> canon G gate -> band x season pctls.
# ------------------------------------------------------------

c3_one_split <- function(sp) {
  X <- if (sp == "man") X_MAN else X_ZONE
  G <- if (sp == "man") G_MAN else G_ZONE
  C <- if (sp == "man") CM else CZ
  
  # THE NEW COLUMN -- within-opponent game rank, per split
  game <- secondary_qbgrp_mz %>%
    filter(.data[[C["snaps"]]] >= X, !is.na(.data[[C["grade"]]])) %>%
    # unname() is LOAD-BEARING: canon's split card (CM/CZ) is a NAMED
    # character vector, and all_of() on a named input lets the inner
    # name eat the rename -- sp_grade never materializes. Plain string
    # selection + outer rename is the bulletproof form.
    select(player, player_id, season, week, qbgrp_ssn, def_ssn,
           sp_snaps = unname(C["snaps"]),
           sp_grade = unname(C["grade"])) %>%
    inner_join(modal_band_mz, by = c("player_id", "season")) %>%
    group_by(qbgrp_ssn, band) %>%
    mutate(n_pool = dplyr::n(),
           vsopp_rank = dplyr::case_when(
             dplyr::n() == 1 ~ 0.5,
             TRUE ~ percent_rank_avg(sp_grade))) %>%
    ungroup()
  
  # honesty: the game floor at work. Two different questions, two
  # prints: (a) structural zeros (played, but no snaps in this
  # scheme -- the man .321 / zone .679 denominator law), (b) of the
  # rows that DID play the scheme, the sub-X spot-duty share.
  # two-step, no same-summarise shadowing: a later summarise()
  # expression can see a JUST-CREATED column instead of the upstream
  # vector (the zero_scheme trap: !0.13 is FALSE, pmax(0,1) = 1, and
  # the "shares" collapse to raw counts). Counts in summarise, ratios
  # in mutate, uniquely named intermediates.
  uni <- secondary_qbgrp_mz %>%
    mutate(zero_scheme = is.na(.data[[C["snaps"]]]) |
             .data[[C["snaps"]]] == 0,
           sub_floor = !zero_scheme & .data[[C["snaps"]]] < X,
           grade_na  = !zero_scheme & .data[[C["snaps"]]] >= X &
             is.na(.data[[C["grade"]]])) %>%
    group_by(season) %>%
    summarise(rows = dplyr::n(),
              n_zero = sum(zero_scheme),
              n_played = sum(!zero_scheme),
              n_sub = sum(sub_floor),
              n_qual = sum(!zero_scheme & !sub_floor),
              n_grade_na = sum(grade_na),
              .groups = "drop") %>%
    mutate(zero_scheme = round(n_zero / rows, 3),
           sub_floor_of_played = round(n_sub / pmax(n_played, 1), 3),
           grade_na_among_quals = round(n_grade_na / pmax(n_qual, 1), 4)) %>%
    select(season, rows, zero_scheme, sub_floor_of_played,
           grade_na_among_quals)
  cat(sprintf("--- honesty (%s): scheme-zero / sub-floor / grade-NA shares by season ---\n", sp))
  print(uni, n = 12)
  
  # honesty: within-opponent pool sizes + the singleton share
  # (SCB pools run thin by design -- canon 6b receipt)
  cat(sprintf("--- honesty (%s): within-opponent pool sizes (defender-games per pool) ---\n", sp))
  print(game %>% distinct(qbgrp_ssn, band, n_pool) %>%
          group_by(band) %>%
          summarise(pools = dplyr::n(),
                    singleton_share = round(mean(n_pool == 1), 3),
                    p10 = quantile(n_pool, .10),
                    med = median(n_pool),
                    p90 = quantile(n_pool, .90), .groups = "drop"))
  
  # season grain -- MEDIAN of vsopp ranks, canon G gate.
  # grain law: group by player_id ONLY; display name attaches from
  # the career-max-snaps row (the name-drift lesson).
  nm <- game %>% group_by(player_id) %>%
    slice_max(sp_snaps, n = 1, with_ties = FALSE) %>% ungroup() %>%
    select(player_id, player)
  ssn <- game %>%
    group_by(player_id, season) %>%
    summarise(qual_g = dplyr::n(),
              sp_snaps = sum(sp_snaps),
              med_vsopp = median(vsopp_rank, na.rm = TRUE),
              .groups = "drop") %>%
    filter(qual_g >= G) %>%
    # template law: the season band comes from canon's modal map,
    # re-joined AFTER the aggregation -- summarise() drops
    # non-grouping columns, and band must come home here
    inner_join(modal_band_mz, by = c("player_id", "season")) %>%
    inner_join(nm, by = "player_id")
  stopifnot(anyDuplicated(ssn[, c("player_id", "season")]) == 0)
  
  # THE CURRENCY -- band x season pools of the season medians,
  # within the split. Never recombined.
  pctl <- ssn %>%
    group_by(band, season) %>%
    mutate(c3_pctl = percent_rank_avg(med_vsopp)) %>%
    ungroup()
  
  cat(sprintf("--- currency coverage (%s): gated players per band x season ---\n",
              sp))
  print(pctl %>% count(season, band) %>%
          tidyr::pivot_wider(names_from = band, values_from = n), n = 12)
  
  cat(sprintf("--- eyeball (%s): 2025 top-10 by c3 (should read like an All-Pro list) ---\n", sp))
  print(pctl %>% filter(season == 2025) %>% arrange(desc(c3_pctl)) %>%
          select(player, band, qual_g, sp_snaps, med_vsopp, c3_pctl) %>%
          head(10))
  cat(sprintf("--- eyeball (%s): 2025 bottom-10 (gated, so real rotation defenders) ---\n", sp))
  print(pctl %>% filter(season == 2025) %>% arrange(c3_pctl) %>%
          select(player, band, qual_g, sp_snaps, med_vsopp, c3_pctl) %>%
          head(10))
  
  list(game = game %>% mutate(split = sp),
       season = ssn %>% mutate(split = sp),
       pctl = pctl %>% mutate(split = sp))
}

run_man  <- c3_one_split("man")
run_zone <- c3_one_split("zone")

cov_c3_game   <- bind_rows(run_man$game,   run_zone$game)
cov_c3_season <- bind_rows(run_man$season, run_zone$season)
cov_c3_pctl   <- bind_rows(run_man$pctl,   run_zone$pctl)

# split-law wall: the long frames carry `split`; a player-season can
# appear once per split (gates both ways) and NEVER twice within one
stopifnot(anyDuplicated(cov_c3_pctl[, c("player_id", "season",
                                        "split")]) == 0)

# ------------------------------------------------------------
# 2. SOFT CONTEXT -- lights up only if the objects are in session
# ------------------------------------------------------------

if (exists("cov_season_pctl_mz")) {
  c1_c3 <- cov_c3_pctl %>%
    inner_join(tibble::as_tibble(cov_season_pctl_mz) %>%
                 select(player_id, season, split, band,
                        c1_pctl = grade_pctl),
               by = c("player_id", "season", "split", "band"))
  
  cat("\n--- c1 vs c3: how much does slate-adjustment reorder?",
      "(per split -- never recombined) ---\n")
  print(c1_c3 %>% group_by(split, season) %>%
          summarise(n = dplyr::n(),
                    r = round(cor(c1_pctl, c3_pctl,
                                  use = "complete.obs"), 3),
                    .groups = "drop"), n = 24)
  
  cat("\n--- 2025 biggest movers per split, c3 - c1",
      "(slate-flattered < 0 < slate-punished) ---\n")
  for (sp in c("man", "zone")) {
    cat(sprintf("  [%s]\n", sp))
    print(c1_c3 %>% filter(season == 2025, split == sp) %>%
            mutate(d = round(c3_pctl - c1_pctl, 3)) %>%
            arrange(desc(abs(d))) %>%
            select(player, band, c1_pctl, c3_pctl, d) %>%
            head(10))
  }
  
  # THE NE ROOM, both lenses -- canon's answer key (6b header):
  #   Davis ZONE >> man; Marcus Jones gates both; Williams both;
  #   Mapu / Tavai zone-only; Woodson both. If this block does not
  #   rhyme with that key, STOP and report.
  cat("\n--- NE's OWN 2025 room, c1 vs c3, per split ---\n")
  ne_ids_c3 <- cov_c3_game %>%
    filter(season == 2025, def_ssn == "NE2025") %>%
    distinct(player_id)
  print(c1_c3 %>% filter(season == 2025) %>%
          semi_join(ne_ids_c3, by = "player_id") %>%
          mutate(d = round(c3_pctl - c1_pctl, 3)) %>%
          arrange(split, band, desc(c3_pctl)) %>%
          select(split, player, band, qual_g, c1_pctl, c3_pctl, d),
        n = Inf)
} else {
  cat("\n[soft skip] cov_season_pctl_mz not in session --",
      "source canon 6b for the c1 comparison + NE block\n")
}

if (exists("league_cov")) {
  cat("\n--- context only: the league-lens secondary differential",
      "(the SoS slate -- c3 NEVER feeds it) ---\n")
  lc <- tibble::as_tibble(league_cov)
  if (all(c("focal", "split", "d_grade") %in% names(lc))) {
    for (sp in c("man", "zone")) {
      cat(sprintf("  [%s] hardest slate moves ahead (top 5) / easiest (bottom 5):\n", sp))
      lsp <- lc %>% filter(split == sp) %>% select(focal, d_grade)
      print(lsp %>% arrange(desc(d_grade)) %>% head(5))
      print(lsp %>% arrange(d_grade) %>% head(5))
    }
  } else {
    cat("[soft skip] league_cov present but focal/split/d_grade not",
        "recognized -- harmless\n")
  }
} else {
  cat("\n[soft skip] league_cov not in session -- league-lens context",
      "after league_opp_secondary_schedule.R runs\n")
}

# ------------------------------------------------------------
# THE COLUMN LEGEND -- prints at the end of every run
# ------------------------------------------------------------

legend_c3 <- c(
  "",
  "============================================================",
  "HOW TO READ WHAT YOU JUST BUILT -- COVERAGE CURRENCY THREE",
  "============================================================",
  "cov_c3_pctl: one row per player_id x season x split x band.",
  "Splits NEVER recombine (canon 6b law). Columns:",
  "",
  "  qual_g      qualifying games: split coverage snaps >= X",
  "              (man X=6 / zone X=13, canon). Season enters the",
  "              currency at qual_g >= 6 (canon G).",
  "  sp_snaps    total split-scheme coverage snaps that season.",
  "  med_vsopp   MEDIAN of the game's within-opponent ranks:",
  "              each qualifying game ranked among every same-band",
  "              defender-game against the SAME passing game",
  "              (qbgrp_ssn) that season -- your day vs everyone who",
  "              faced that offense. Singleton pools score 0.5.",
  "  c3_pctl     the currency: percent_rank of med_vsopp within",
  "              band x season, per split. 1.00 = the most",
  "              slate-punished-worthy coverage season in the pool.",
  "              Higher = better defender; no inversions here",
  "              (the grade itself already points the right way).",
  "",
  "Reading the c1 vs c3 movers: d = c3 - c1. d > 0 = slate-PUNISHED",
  "(his raw percentile was held down by a hard slate of passing",
  "games; the common-opponent comparison hands it back). d < 0 =",
  "slate-flattered. Worked example to anchor the smell test: canon's",
  "own NE answer key says Davis is zone >> man, Marcus Jones gates",
  "both, Mapu/Tavai zone-only -- the NE block should rhyme.",
  "",
  "FIREWALL: currency three measures PLAYERS, slate-adjusted. It",
  "never feeds any SoS slate -- the SoS measures schedules with",
  "UNadjusted quality. league_cov context prints are read-only.",
  "============================================================"
)
cat(legend_c3, sep = "\n")

# ------------------------------------------------------------
# Checkpoint (after eyeball):
# ------------------------------------------------------------
# save.image("~/coverage_c3_workspace.RData")
# system('aws s3 cp ~/coverage_c3_workspace.RData s3://nfl-pff-data-lucas/workspaces/')