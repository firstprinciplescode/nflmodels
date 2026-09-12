# ============================================================
# SHARED NE 2026 CONSTANTS -- standalone (added 2026-09-12)
#
# WHAT THIS MAKES: opp_2026_teams, sched_2026, in_season, blend2,
#   ol_pos_levels -- the objects every new_england_opp_*_schedule.R
#   and league_*_schedule.R file lists under "prush pipeline objects".
#
# WHY THIS FILE EXISTS: canon defines these inside
#   pff_stats/pass_block/new_england_opp_ol_schedule.R (lines 39-45
#   and 121-126), but that file walls on the pass-block / run-block
#   AWS frames BEFORE it reaches them. Any chain other than OL
#   therefore could not start in a fresh session without hand-running
#   those lines. This file is a VERBATIM copy of them, nothing more.
#
# CANON LAW: new_england_opp_ol_schedule.R stays the source of truth.
#   If the slate or the blend changes there, change it here the same
#   day. Sourcing the OL schedule after this file simply re-assigns
#   identical values.
#
# SOURCE ORDER: after data_build (combined_ids, combined_ids_defense,
#   pff_team_lookup), before any unit's step-0 / schedule file.
# ============================================================

library(dplyr)

# --- new_england_opp_ol_schedule.R lines 39-45, verbatim ---
ol_pos_levels  <- c("LT","LG","C","RG","RT")
opp_2026_teams <- c("SEA","PIT","JAX","BUF","LV","NYJ","CHI","MIA",
                    "GB","DET","LAC","MIN","KC","DEN")
sched_2026 <- c("SEA","PIT","JAX","BUF","LV","NYJ","CHI","MIA","GB","DET",
                "LAC","BUF","MIN","KC","NYJ","DEN","MIA")

in_season <- function(w) w <= 18 | w >= 28   # reg + playoffs, all games

# --- new_england_opp_ol_schedule.R lines 121-126, verbatim ---
blend2 <- function(x25, x24, w) case_when(
  is.na(x25) & is.na(x24) ~ NA_real_,
  is.na(x24)              ~ x25,
  is.na(x25)              ~ x24,
  TRUE                    ~ w * x25 + (1 - w) * x24
)

cat("shared NE 2026 constants in session: opp_2026_teams (",
    length(opp_2026_teams), "), sched_2026 (", length(sched_2026),
    " games), in_season, blend2, ol_pos_levels\n", sep = "")
