#### #######################################################################
#### ###  THIS IS A TEST HARNESS — EVERY NUMBER BELOW IS FAKE.           ###
#### ###  Invented players, invented teams, a handful of toy seasons.    ###
#### ###  It exists to check the code's math. It says NOTHING            ###
#### ###  about Waddle, Sneed, the Chiefs, the Broncos, or anyone real.  ###
#### ###                                                                 ###
#### ###  REAL numbers come from running impact_bridge.R in your         ###
#### ###  RStudio session with your data loaded. Not here.               ###
#### #######################################################################
suppressMessages(library(dplyr))
source("C:/Users/vflre/Downloads/nflmodels_UPDATE/pff_stats/evaluation/impact_bridge.R")

#### SAFETY: toy frames named like real ones are stashed and ALWAYS restored,
#### even if a check fails mid-run (tryCatch/finally below).
.br_test_names <- c("combined_pbp", "coverage_summary", "all_pass_block_summary",
                    "tps_pass_block_player_season_summary", "receiving_func_base",
                    "coverage_man_player_season_summary",
                    "coverage_zone_player_season_summary", "final_coverage_df_qbgrp",
                    "pass_rush_tps_player_season_summary", "run_defense_player_season_summary",
                    "rush_season_pctl_sos", "rec_season_pctl_sos",
                    "combined_grade_epa_summary", "run_athena_query")
.br_stash <- new.env()
for (f in .br_test_names)
  if (exists(f, envir = .GlobalEnv)) assign(f, get(f, envir = .GlobalEnv), envir = .br_stash)
.br_restore <- function() {
  for (f in .br_test_names) {
    if (exists(f, envir = .br_stash)) assign(f, get(f, envir = .br_stash), envir = .GlobalEnv)
    else if (exists(f, envir = .GlobalEnv)) rm(list = f, envir = .GlobalEnv)
  }
  if (exists(".br_memo", envir = .GlobalEnv))          # fake team totals out of the memo
    rm(list = ls(get(".br_memo", envir = .GlobalEnv), all.names = TRUE),
       envir = get(".br_memo", envir = .GlobalEnv))
  cat("[test harness restored your real frames]\n")
}

tryCatch({

#### combined_pbp toy: pbp-style abbreviated names, ids are the join key
mk_pbp <- function(team, def, ssn, qb, rid, rname, yds_per, td_n, int_n) {
  tibble(posteam = team, defteam = def, season = ssn, game_id = paste0(team, ssn, "g1"),
         week = 1, pass_attempt = 1, sack = 0, qb_dropback = 1,
         complete_pass = rep(c(1, 0), c(70, 30)),
         passing_yards = yds_per, pass_touchdown = 0, interception = 0,
         epa = 0.1, yards_gained = yds_per,
         receiver_id = rid, receiver_player_name = rname, qbgrp_ssn = qb,
         pbp_predicted_after_run_xtd = NA_real_,
         pbp_predicted_after_pass_xtd = 0.05,
         pbp_predicted_after_scramble_xtd = NA_real_)
}
den24 <- mk_pbp("DEN", "KC", 2024, "JNix-2024", "00-1", "J.Receiver1", 7.5, 5, 2)
den24$pass_touchdown[1:5] <- 1; den24$interception[6:7] <- 1
den25 <- mk_pbp("DEN", "KC", 2025, "JNix-2025", "00-2", "J.Receiver2", 8.0, 6, 2)
den25$pass_touchdown[1:6] <- 1; den25$interception[7:8] <- 1
kc24  <- mk_pbp("KC", "DEN", 2024, "PMahomes-2024", "00-3", "J.Receiver3", 7.0, 4, 3)
kc24$pass_touchdown[1:4] <- 1; kc24$interception[5:7] <- 1
kc25  <- mk_pbp("KC", "DEN", 2025, "PMahomes-2025", "00-3", "J.Receiver3", 7.1, 4, 3)
kc25$pass_touchdown[1:4] <- 1; kc25$interception[5:7] <- 1
mia24w2 <- mk_pbp("MIA", "BUF", 2024, "TTagovailoa-2024", "00-2", "J.Receiver2", 9.0, 5, 1)
mia24w2$pass_touchdown[1:5] <- 1; mia24w2$interception[6] <- 1

mk_rush <- function(def, ssn, ypc_val) {
  tibble(posteam = "OPP", defteam = def, season = ssn, game_id = paste0("r", def, ssn, "g1"),
         week = 1, pass_attempt = 0, sack = 0, qb_dropback = 0,
         complete_pass = 0, passing_yards = 0, pass_touchdown = 0, interception = 0,
         epa = 0.05, yards_gained = ypc_val, rush_attempt = 1, qb_scramble = 0,
         receiver_id = NA, receiver_player_name = NA, qbgrp_ssn = "OPPQB-2025",
         pbp_predicted_after_run_xtd = 0.02,
         pbp_predicted_after_pass_xtd = NA_real_,
         pbp_predicted_after_scramble_xtd = NA_real_)[rep(1, 40), ]
}
rush_rows <- bind_rows(mk_rush("KC", 2024, 4.2), mk_rush("KC", 2025, 4.6),
                       mk_rush("DEN", 2024, 4.0), mk_rush("DEN", 2025, 4.1))
combined_pbp <- bind_rows(den24, den25, kc24, kc25, mia24w2, rush_rows)

#### coverage_summary toy — raw frame: `position`, NOT final_position; includes an "S"
coverage_summary <- bind_rows(
  tibble(player = "Trent McDuffie", player_id = 101, position = "CB", team = "KC", season = 2024,
         week = 1:4, snap_counts_coverage = 60, grades_coverage_defense = 85,
         targets = 10, receptions = 5, yards = 60, qb_rating_against = 70, draft_season = 2022L),
  tibble(player = "Jaylen Watson",  player_id = 102, position = "CB", team = "KC", season = 2024,
         week = 1:4, snap_counts_coverage = 55, grades_coverage_defense = 70,
         targets = 9, receptions = 5, yards = 70, qb_rating_against = 85, draft_season = 2022L),
  tibble(player = "L'Jarius Sneed", player_id = 201, position = "CB", team = "KC", season = 2025,
         week = 1:4, snap_counts_coverage = 40, grades_coverage_defense = 55,
         targets = 8, receptions = 5, yards = 65, qb_rating_against = 100, draft_season = 2020L),
  # Sneed 2024: BELOW-GATE season — in the raw frame only, not in the summaries
  tibble(player = "L'Jarius Sneed", player_id = 201, position = "CB", team = "TEN", season = 2024,
         week = 1, snap_counts_coverage = 30, grades_coverage_defense = 48,
         targets = 6, receptions = 4, yards = 70, qb_rating_against = 120, draft_season = 2020L),
  tibble(player = "Rookie CB",      player_id = 202, position = "CB", team = "KC", season = 2025,
         week = 1:4, snap_counts_coverage = 55, grades_coverage_defense = 50,
         targets = 8, receptions = 6, yards = 75, qb_rating_against = 110, draft_season = 2025L),
  tibble(player = "Pat Surtain",    player_id = 301, position = "CB", team = "DEN", season = 2024,
         week = 1:4, snap_counts_coverage = 60, grades_coverage_defense = 90,
         targets = 10, receptions = 4, yards = 50, qb_rating_against = 60, draft_season = 2021L),
  tibble(player = "Pat Surtain",    player_id = 301, position = "CB", team = "DEN", season = 2025,
         week = 1:4, snap_counts_coverage = 60, grades_coverage_defense = 90,
         targets = 10, receptions = 4, yards = 50, qb_rating_against = 60, draft_season = 2021L),
  tibble(player = "Bryan Cook",     player_id = 401, position = "S",  team = "KC", season = 2025,
         week = 1:4, snap_counts_coverage = 45, grades_coverage_defense = 78,
         targets = 6, receptions = 3, yards = 40, qb_rating_against = 65, draft_season = 2022L)
)

#### man/zone season summaries (schema mirrors pff_pass_coverage_AWS.R:426+)
#### McDuffie gets 3 seasons so the two-season card window can be tested
mk_fam <- function(fam) {
  d <- tibble(
    player = c("Trent McDuffie", "Trent McDuffie", "Trent McDuffie",
               "L'Jarius Sneed", "Rookie CB", "Pat Surtain"),
    player_id = c(101, 101, 101, 201, 202, 301),
    final_position = "CB",
    def_ssn = c("KC2022", "KC2023", "KC2024", "KC2025", "KC2025", "DEN2025"),
    season = c(2022, 2023, 2024, 2025, 2025, 2025),
    n = c(4, 4, 4, 4, 4, 4)
  )
  p <- list(grade_cov = c(.91, .92, .90, .40, .30, .95),
            cov_snaps_per_target = c(.80, .81, .80, .50, .40, .90),
            catch_rate = c(.79, .80, .80, .40, .30, .90),
            pass_break_up_rate = c(.70, .71, .70, .40, .30, .80),
            yards_per_cov_snap = c(.84, .85, .85, .35, .30, .90),
            qb_rating_against = c(.87, .88, .88, .30, .25, .92))
  for (nm in names(p)) d[[paste0(fam, "_", nm, "_season_pctl")]] <- p[[nm]]
  d
}
coverage_man_player_season_summary  <- mk_fam("man")
coverage_zone_player_season_summary <- mk_fam("zone")

#### final_coverage_df_qbgrp toy: room-membership snaps + grades (the coverage
#### room builder now reads THIS frame, with your final_position labels)
final_coverage_df_qbgrp <- bind_rows(
  tibble(
    player = rep(c("Trent McDuffie", "L'Jarius Sneed", "Rookie CB", "Pat Surtain", "Pat Surtain"), each = 4),
    player_id = rep(c(101, 201, 202, 301, 301), each = 4),
    final_position = "CB",
    team = rep(c("KC", "KC", "KC", "DEN", "DEN"), each = 4),
    def_ssn = rep(c("KC2024", "KC2025", "KC2025", "DEN2025", "DEN2024"), each = 4),
    season = rep(c(2024, 2025, 2025, 2025, 2024), each = 4),
    week = rep(1:4, 5),
    snap_counts_coverage = rep(c(50, 30, 45, 55, 52), each = 4),
    grades_coverage_defense = rep(c(85, 55, 50, 90, 90), each = 4)
  ),
  tibble(
    player = "Bryan Cook", player_id = 401, final_position = "S", team = "KC",
    def_ssn = "KC2025", season = 2025, week = 1:4,
    snap_counts_coverage = 45, grades_coverage_defense = 78
  ),
  # team-map rows for the v7 cov_pctl_sec_lg players
  tibble(
    player = paste0("P", 1:12),
    player_id = c(1001:1009, 1101:1103),
    final_position = c(rep("CB", 4), "SCB", "S", "S", "LB", "LB", "CB", "CB", "CB"),
    team = c(rep("KC", 9), rep("DEN", 3)),
    def_ssn = c(rep("KC2025", 9), rep("DEN2025", 3)),
    season = 2025, week = 1,
    snap_counts_coverage = 50, grades_coverage_defense = 60
  )
)

#### pass-block toys — tps carries det_position (stints collapse WITHIN position);
#### Moore swings RT/LG to prove positions never blend; Benson missing from tps
all_pass_block_summary <- bind_rows(
  tibble(player = "Jawaan Taylor", player_id = 401, det_position = "RT", team_name = "KC",
         season = 2024, week = 1:4, snap_counts_pass_block = 40,
         grades_pass_block = 70, pressures_allowed = 2, hurries_allowed = 1),
  tibble(player = "Josh Simmons",  player_id = 402, det_position = "LT", team_name = "KC",
         season = 2024, week = 1:4, snap_counts_pass_block = 40,
         grades_pass_block = 65, pressures_allowed = 2, hurries_allowed = 1),
  tibble(player = "Jaylon Moore",  player_id = 403, det_position = "RT", team_name = "KC",
         season = 2025, week = 1:2, snap_counts_pass_block = 40,
         grades_pass_block = 60, pressures_allowed = 3, hurries_allowed = 2),
  tibble(player = "Jaylon Moore",  player_id = 403, det_position = "LG", team_name = "KC",
         season = 2025, week = 3:4, snap_counts_pass_block = 40,
         grades_pass_block = 58, pressures_allowed = 3, hurries_allowed = 2),
  tibble(player = "Kahlil Benson", player_id = 404, det_position = "LT", team_name = "KC",
         season = 2025, week = 1:4, snap_counts_pass_block = 40,
         grades_pass_block = 52, pressures_allowed = 4, hurries_allowed = 2),
  tibble(player = "Creed Humphrey", player_id = 405, det_position = "C", team_name = "KC",
         season = 2025, week = 1:4, snap_counts_pass_block = 38,
         grades_pass_block = 75, pressures_allowed = 1, hurries_allowed = 1)
)
tps_pass_block_player_season_summary <- tibble(
  player_id = c(401, 401, 402, 403, 403, 405),
  det_position = c("RT", "RT", "LT", "RT", "LG", "C"),
  season = c(2024, 2024, 2024, 2025, 2025, 2025),
  n = c(3, 1, 4, 2, 2, 4),
  grade_season_pctl = c(.75, .55, .65, .55, .50, .80),
  pressure_season_pctl = c(.40, .50, .45, .70, .72, .30),
  hurries_season_pctl = c(.40, .50, .45, .70, .72, .30)
)

#### receiving_func_base toy: FULL names + OE inputs + cluster columns
receiving_func_base <- tibble(
  abbreviation = rep(c("DEN", "KC"), each = 4),
  posteam = rep(c("DEN", "KC"), each = 4),
  season = rep(rep(c(2024, 2025), each = 2), times = 2),
  receiver_id = c("00-1", "00-2", "00-2", "00-1", "00-3", "00-4", "00-3", "00-4"),
  player = c("Jalen Receiver1", "Jaylen Receiver2", "Jaylen Receiver2", "Jalen Receiver1",
             "Jamarr Receiver3", "J Receiver4", "Jamarr Receiver3", "J Receiver4"),
  player_id = c(1, 2, 2, 1, 3, 4, 3, 4),
  final_position_group = "WR",
  tgt_share = c(.25, .20, .28, .22, .30, .25, .31, .24),
  tgt_cluster_name = c("ML", "ML", "ML", "ML", "RB", "RB", "RB", "RB"),
  align_cluster_name = "WWR",
  targets = c(10, 8, 12, 9, 6, 5, 7, 5),
  snap_counts_pass_route = 30,
  acc_rate = c(.70, .72, .75, .68, .66, .64, .67, .63),
  pbp_cp   = c(.68, .66, .66, .67, .65, .65, .66, .64),
  ypa      = c(8.0, 9.0, 9.5, 7.8, 6.5, 6.2, 6.7, 6.1),
  pbp_xypa = c(7.6, 7.4, 7.4, 7.5, 7.0, 7.1, 7.0, 7.1),
  yac      = c(5.0, 6.0, 6.5, 4.8, 4.0, 3.8, 4.2, 3.7),
  pbp_yac  = c(4.6, 4.5, 4.5, 4.5, 4.3, 4.3, 4.3, 4.3),
  adot     = c(9, 11, 11.5, 9.2, 2, 2.5, 2.2, 2.4),
  week = 1
)

#### v5 toy frames: pass rush / run defense / rushing / machine receiving
pass_rush_tps_player_season_summary <- tibble(
  player = c("George Karlaftis", "Felix Anudike", "Nik Bonitto"),
  player_id = c(501, 502, 503),
  position = "ED",
  def_ssn = c("KC2025", "KC2025", "DEN2025"),
  season = 2025, n = c(4, 4, 4),
  tps_grade_pass_rush_season_pctl = c(.70, .40, .88),
  tps_prp_season_pctl = c(.72, .38, .90),
  tps_pass_rush_win_rate_season_pctl = c(.68, .42, .87),
  tps_pressure_rate_season_pctl = c(.71, .39, .89),
  tps_hit_rate_season_pctl = c(.60, .45, .80),
  tps_hurry_rate_season_pctl = c(.66, .41, .85)
)
pass_rush_tps_player_season_summary <- bind_rows(
  pass_rush_tps_player_season_summary,
  tibble(player = "Chris Jones", player_id = 504, position = "DI", def_ssn = "KC2024",
         season = 2024, n = 4, tps_grade_pass_rush_season_pctl = .90,
         tps_prp_season_pctl = .91, tps_pass_rush_win_rate_season_pctl = .89,
         tps_pressure_rate_season_pctl = .92, tps_hit_rate_season_pctl = .85,
         tps_hurry_rate_season_pctl = .88)
)

run_defense_player_season_summary <- bind_rows(
  tibble(player = "Chris Jones", player_id = 504, position = "DI", def_ssn = "KC2024",
         season = 2024, n = 4,
         grade_run_def_season_pctl = .88, grade_tackle_season_pctl = .80,
         stop_pct_season_pctl = .85, tackle_pct_season_pctl = .70,
         assists_pct_season_pctl = .50,
         missed_tackle_rate_season_pctl = .20,
         avg_depth_of_tackle_season_pctl = .30),
  tibble(player = "Nick Bolton", player_id = 505, position = "LB", def_ssn = "KC2025",
         season = 2025, n = 4,
         grade_run_def_season_pctl = .55, grade_tackle_season_pctl = .50,
         stop_pct_season_pctl = .52, tackle_pct_season_pctl = .60,
         assists_pct_season_pctl = .65,
         missed_tackle_rate_season_pctl = .80,
         avg_depth_of_tackle_season_pctl = .60),
  tibble(player = "Zach Allen", player_id = 506, position = "DI", def_ssn = "DEN2025",
         season = 2025, n = 4,
         grade_run_def_season_pctl = .75, grade_tackle_season_pctl = .70,
         stop_pct_season_pctl = .72, tackle_pct_season_pctl = .65,
         assists_pct_season_pctl = .55,
         missed_tackle_rate_season_pctl = .40,
         avg_depth_of_tackle_season_pctl = .45)
)

rush_season_pctl_sos <- tibble(
  player = c("Isiah Pacheco", "RJ Harvey"),
  player_id = c(601, 602), season = 2025, band = "HB",
  grun_pctl = c(.70, .55), ypa_pctl = c(.65, .50), yco_pctl = c(.72, .45),
  mtf_pctl = c(.68, .52), brk_pctl = c(.60, .48),
  q_games = c(8, 8), q_atts = c(120, 100)
)

rec_season_pctl_sos <- tibble(
  player = "Jaylen Receiver2", player_id = 2, season = 2025, band = "WWR",
  grade_pctl = .78, man_grade_pctl = .80, man_yprr_pctl = .82, man_tgt_pctl = .60,
  zone_grade_pctl = .75, zone_yprr_pctl = .77, zone_tgt_pctl = .65,
  routes = 400, qual_g = 10
)

#### toy id table + toy "Athena" (the outcomes layer now pulls PFF team totals
#### through run_athena_query + combined_grade_epa_summary)
combined_grade_epa_summary <- tibble(
  posteam = c("DEN", "DEN", "KC",  "KC",  "MIA"),
  opp     = c("KC",  "KC",  "DEN", "DEN", "BUF"),
  week    = c(1L, 1L, 1L, 1L, 1L),
  season  = c(2024L, 2025L, 2024L, 2025L, 2024L)
)

run_athena_query <- function(q) {
  if (grepl("passing_pressure", q)) {
    # returns the QUERY's aliased columns, not raw table columns
    mk <- function(tm, ssn, att, cmp, yds, td, int, sk) {
      tibble(team_name = tm, week = 1L, season = ssn,
             dropbacks = att + sk, pressured = round((att + sk) * 0.4),
             att = att, comp_n = cmp, yds = yds, td_n = td, int_n = int, sacks = sk)
    }
    # DEN 2024: 70/100, 750 yds, 5 td, 2 int -> rating exactly 100
    bind_rows(
      mk("DEN", 2024, 100, 70, 750, 5, 2, 4),
      mk("DEN", 2025, 100, 70, 800, 6, 2, 3),
      mk("KC",  2024, 100, 66, 700, 4, 3, 5),
      mk("KC",  2025, 100, 67, 710, 4, 3, 5),
      mk("MIA", 2024, 100, 70, 900, 5, 1, 2)
    )
  } else if (grepl("rushing_summary", q)) {
    tibble(team_name = c("DEN", "DEN", "KC", "KC", "MIA"),
           week = 1L, season = c(2024L, 2025L, 2024L, 2025L, 2024L),
           rushes = 40, ryds = c(168, 164, 184, 160, 150), rtd = 2)
  } else stop("toy run_athena_query: unknown query")
}

for (f in .br_test_names)
  assign(f, get(f), envir = .GlobalEnv)

cat("=== outcomes sanity: DEN 2024 rating should be 100 ===\n")
o <- .br_outcomes("offense", 2024:2025)
stopifnot(abs(o$rating[o$team == "DEN" & o$season == 2024] - 100) < 1e-6)
cat("rating OK\n")

cat("\n=== v6: safety position mapping (raw 'S' -> 'S', not 'OTHER') ===\n")
stopifnot(.br_ensure_final_position(tibble(position = "S"))$final_position == "S")
cook <- lab_cov_player("Bryan Cook")
stopifnot(cook$final_position[1] == "S")
cat("position mapping OK\n")

cat("\n=== v6 cov_card: two-season window + below-gate rows + per-position cards ===\n")
cc <- cov_card("Trent McDuffie")
# default window = two most recent seasons (2023-2024), not 2022
stopifnot(abs(cc[["man CB"]]$card$weighted["Grade"] - 91) < 0.2)   # 2023+2024 only
out_txt <- paste(capture.output(cov_card("L'Jarius Sneed")), collapse = "\n")
stopifnot(grepl("BELOW THE GATE", out_txt), grepl("2024", out_txt))
cat("cov_card window + below-gate OK\n")

cat("\n=== OT: stints collapse within position; swing player keeps both slots ===\n")
otps <- .br_ot_player_seasons(2024:2025)
stopifnot(nrow(otps[otps$player_id == 401 & otps$season == 2024, ]) == 1,
          abs(otps$grade_season_pctl[otps$player_id == 401] - 0.70) < 1e-9)
moore <- ot_card("Jaylon Moore")
stopifnot(length(moore) == 2, all(c("RT", "LG") %in% names(moore)))
cat("position-separated OL card OK\n")

cat("\n=== ot_card raw fallback + a CENTER ===\n")
oc <- ot_card("Kahlil Benson")
stopifnot(is.null(oc$card), "raw_pressure_rate" %in% names(oc$seasons))
oc3 <- ot_card("Creed Humphrey")
stopifnot(!is.null(oc3$C))
cat("fallback + G/C OK\n")

cat("\n=== V8: retired bridges stop and name the replacement ===\n")
for (fn in c("bridge_fit", "bridge_delta", "bridge_fit_stats", "bridge_stats_delta", "bridge_wr1_add")) {
  msg <- tryCatch({ get(fn)("cb"); "no error" }, error = function(e) conditionMessage(e))
  stopifnot(grepl("retired", msg), grepl("Use instead", msg))
}
cat("retired bridges OK\n")

cat("\n=== V8 wr_card: role-aware pool gated, thin seasons marked ===\n")
wc2 <- wr_card("Jaylen Receiver2", seasons = 2024:2025, min_tgt_pool = 1)
stopifnot("role_aware" %in% names(wc2),
          all(c("cp_oe_pctl", "pool_n", "below_pool") %in% names(wc2$role_aware$seasons)),
          wc2$role_aware$seasons$tgt_cluster[1] %in% c("ML", "RB"))
cat("wr_card role-aware OK\n")

cat("\n=== v5 cards intact ===\n")
pc <- pr_card("Karlaftis"); stopifnot(abs(pc$card$weighted["Grade"] - 70) < 0.2)
rd <- rd_card("Nick Bolton"); stopifnot(abs(rd$card$weighted["MT%"] - 20) < 0.2)
rc <- rb_card("Pacheco");    stopifnot(abs(rc$card$weighted["RunGrade"] - 70) < 0.2)
invisible(capture.output(rookie_anchor("cb")))   # prints your priors; toy session has none
cat("v5 cards intact OK\n")

cat("\nALL V8 SMOKE TESTS PASSED\n")

}, finally = .br_restore())
