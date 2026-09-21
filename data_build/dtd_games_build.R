# dtd_games_build.R
# PROPOSED -- Claude, UNSIGNED.  NEW file (2026-09-20). Changes nothing that exists.
#
# WHY: combined_pbp holds pass and run plays only -- no punts, kickoffs, field goals, kneels or
# spikes. Everything that is NOT a real pass or run lives here instead, as ONE game-level side frame
# with the same grain and keys as qb_stats_df_final (one row per offense-game: season, week, posteam,
# qbgrp_ssn, def_ssn). It is built from the same nflverse play-by-play pbp_nfl_base.R starts from
# (nflreadr::load_pbp), so there is no second play-by-play to maintain.
#
# WHAT A ROW SAYS: "in this game, THIS offense's team gave up these non-offensive touchdowns to the
# OTHER team (def_ssn's team)", plus the exposure they come from and the kneel-downs this team took.
#   int_ret, fum_ret (fum_ret_sack = the strip-sack ones), punt_ret, kick_ret, blk_punt_ret, blk_fg_ret,
#   st_recovery (other team recovered a special-teams fumble for a TD)
#   dtd_def = int_ret + fum_ret      dtd_st = the special-teams kinds      dtd = dtd_def + dtd_st
#   exposure: off_plays, dropbacks, pass_att, ints, sacks_taken, qb_hits_taken, fumbles, fumbles_lost,
#             sack_fumbles_lost, punts, punts_returned, punts_blocked, kickoffs, kickoffs_returned, fg_att, fg_blocked
#   kneels, kneel_yards, spikes (this team's own), home, fav_pts (closing spread from this team's side,
#   +3 = favoured by 3), total_line, pts, opp_pts, roof
# To get what a team SCORED, read the rows where it is the defense (def_team / def_ssn).
#
# USE (RStudio console, repo root as working directory):
#   source("data_build/dtd_games_build.R")
#   dtd_games_build()            # ~2 min, downloads nflverse pbp, writes cache/dtd_games.rds + cache/dtd_plays.rds
# Nothing runs when this file is sourced. Creates no objects in the session except this one function.
# Session needs: dplyr, tidyr, nflreadr installed. Uses qb_stats_df_final from the session if it is
# there, otherwise reads cache/qb_stats_df_final.rds into a LOCAL variable only.

dtd_games_build <- function(seasons = 2016:2025, qb = NULL, save = TRUE, dir = "cache") {
  `%>%` <- dplyr::`%>%`
  if (is.null(qb)) qb <- if (exists("qb_stats_df_final")) get("qb_stats_df_final") else readRDS(file.path(dir, "qb_stats_df_final.rds"))
  q <- dplyr::ungroup(qb)
  extra <- setdiff(unique(q$season), seasons)                          # checked BEFORE the download: every season in qb_stats_df_final must be covered
  if (length(extra)) stop("qb_stats_df_final has season(s) ", paste(sort(extra), collapse = ", "), " that `seasons` does not cover. Run dtd_games_build(seasons = ", min(q$season), ":", max(q$season), ")")
  pbp <- as.data.frame(nflreadr::load_pbp(seasons))
  real <- pbp[!is.na(pbp$play_type) & pbp$play_type != "no_play", ]

  # ---- 1. every touchdown, classified. On a kickoff nflverse posteam = the RECEIVING team. ----
  td <- real[real$touchdown %in% 1 & !is.na(real$td_team), ]
  scrim <- td$play_type %in% c("pass", "run", "qb_kneel", "qb_spike")
  td$kind <- dplyr::case_when(
    scrim & td$interception %in% 1 & td$td_team == td$defteam             ~ "int_ret",
    scrim & td$td_team == td$defteam                                     ~ "fum_ret",
    td$play_type == "punt" & td$td_team == td$defteam & td$punt_blocked %in% 1 ~ "blk_punt_ret",
    td$play_type == "punt" & td$td_team == td$defteam                     ~ "punt_ret",
    td$play_type == "punt" & td$td_team == td$posteam                     ~ "st_recovery",
    td$play_type == "kickoff" & td$td_team == td$posteam                  ~ "kick_ret",
    td$play_type == "kickoff" & td$td_team == td$defteam                  ~ "st_recovery",
    td$play_type == "field_goal" & td$td_team == td$defteam               ~ "blk_fg_ret",
    scrim & td$td_team == td$posteam                                     ~ "OFFENSE",
    TRUE ~ "OTHER")
  if (any(td$kind == "OTHER")) stop("touchdowns the classifier does not know: ", sum(td$kind == "OTHER"), " -- look at them before going on")
  td$fum_how <- ifelse(td$kind == "fum_ret", ifelse(td$sack %in% 1, "sack", ifelse(td$complete_pass %in% 1, "after_catch", ifelse(td$rush_attempt %in% 1, "rush", "other"))), NA_character_)
  td$scorer <- td$td_team; td$victim <- ifelse(td$td_team == td$home_team, td$away_team, td$home_team)
  kinds <- c("int_ret", "fum_ret", "punt_ret", "kick_ret", "blk_punt_ret", "blk_fg_ret", "st_recovery")
  nod <- td[td$kind %in% kinds, c("game_id", "play_id", "season", "week", "season_type", "home_team", "away_team", "posteam", "defteam", "play_type",
                                  "qtr", "score_differential", "kind", "fum_how", "scorer", "victim", "td_player_name", "desc")]

  # ---- 2. exposure per game x team ----
  sc <- real[!is.na(real$posteam) & real$play_type %in% c("pass", "run", "qb_kneel", "qb_spike") & !(real$two_point_attempt %in% 1), ]
  off <- sc %>% dplyr::group_by(game_id, team = posteam) %>%
    dplyr::summarise(off_plays = sum(play_type %in% c("pass", "run")), dropbacks = sum(qb_dropback == 1, na.rm = TRUE), pass_att = sum(pass_attempt == 1 & sack == 0, na.rm = TRUE),
                     ints = sum(interception == 1, na.rm = TRUE), sacks_taken = sum(sack == 1, na.rm = TRUE), qb_hits_taken = sum(qb_hit == 1, na.rm = TRUE),
                     fumbles = sum(fumble == 1, na.rm = TRUE), fumbles_lost = sum(fumble_lost == 1, na.rm = TRUE), sack_fumbles_lost = sum(fumble_lost == 1 & sack == 1, na.rm = TRUE),
                     kneels = sum(play_type == "qb_kneel"), kneel_yards = sum(yards_gained[play_type == "qb_kneel"], na.rm = TRUE), spikes = sum(play_type == "qb_spike"), .groups = "drop")
  pnt <- real[real$play_type == "punt", ] %>% dplyr::group_by(game_id, team = posteam) %>%
    dplyr::summarise(punts = dplyr::n(), punts_returned = sum(!is.na(punt_returner_player_id) & punt_fair_catch %in% 0 & touchback %in% 0 & punt_out_of_bounds %in% 0 & punt_downed %in% 0),
                     punts_blocked = sum(punt_blocked == 1, na.rm = TRUE), .groups = "drop")
  ko <- real[real$play_type == "kickoff", ] %>% dplyr::group_by(game_id, team = defteam) %>%          # defteam on a kickoff = the KICKING team
    dplyr::summarise(kickoffs = dplyr::n(), kickoffs_returned = sum(!is.na(kickoff_returner_player_id) & touchback %in% 0 & kickoff_fair_catch %in% 0 & kickoff_out_of_bounds %in% 0 & kickoff_downed %in% 0), .groups = "drop")
  fga <- real[real$play_type == "field_goal", ] %>% dplyr::group_by(game_id, team = posteam) %>%
    dplyr::summarise(fg_att = dplyr::n(), fg_blocked = sum(field_goal_result == "blocked", na.rm = TRUE), .groups = "drop")
  gm <- pbp %>% dplyr::distinct(game_id, season, week, season_type, home_team, away_team, spread_line, total_line, home_score, away_score, roof) %>%
    dplyr::group_by(game_id) %>% dplyr::slice(1) %>% dplyr::ungroup()
  tg <- dplyr::bind_rows(
    gm %>% dplyr::transmute(game_id, season, week, season_type, team = home_team, opp = away_team, home = 1L, fav_pts = spread_line, total_line, pts = home_score, opp_pts = away_score, roof),
    gm %>% dplyr::transmute(game_id, season, week, season_type, team = away_team, opp = home_team, home = 0L, fav_pts = -spread_line, total_line, pts = away_score, opp_pts = home_score, roof))
  cnt <- nod %>% dplyr::count(game_id, scorer, victim, kind) %>% tidyr::pivot_wider(names_from = kind, values_from = n, values_fill = 0)
  for (k in kinds) if (!k %in% names(cnt)) cnt[[k]] <- 0L
  strip <- nod[nod$kind == "fum_ret" & nod$fum_how %in% "sack", ] %>% dplyr::count(game_id, scorer, victim, name = "fum_ret_sack")
  cnt <- cnt %>% dplyr::left_join(strip, by = c("game_id", "scorer", "victim"))
  ints0 <- c(kinds, "fum_ret_sack", "punts", "punts_returned", "punts_blocked", "kickoffs", "kickoffs_returned", "fg_att", "fg_blocked", "kneels", "spikes")
  g <- tg %>% dplyr::left_join(off, by = c("game_id", "team")) %>% dplyr::left_join(pnt, by = c("game_id", "team")) %>% dplyr::left_join(ko, by = c("game_id", "team")) %>%
    dplyr::left_join(fga, by = c("game_id", "team")) %>% dplyr::left_join(dplyr::rename(cnt, team = victim, opp = scorer), by = c("game_id", "team", "opp")) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(ints0), ~ dplyr::coalesce(as.integer(.x), 0L)), kneel_yards = dplyr::coalesce(kneel_yards, 0),
                  dtd_def = int_ret + fum_ret, dtd_st = punt_ret + kick_ret + blk_punt_ret + blk_fg_ret + st_recovery, dtd = dtd_def + dtd_st)

  # ---- 3. nflverse codes and weeks -> the ones qb_stats_df_final uses. Season-aware: OAK through 2019, SD in 2016;
  #         playoffs are weeks 28 / 29 / 30 / 32 (wild card / divisional / conference / Super Bowl). ----
  his_code <- function(tm, season) {
    out <- dplyr::recode(tm, ARI = "ARZ", BAL = "BLT", CLE = "CLV", HOU = "HST")
    out <- ifelse(out == "LV" & season <= 2019, "OAK", out); ifelse(out == "LAC" & season == 2016, "SD", out)
  }
  g <- g %>% dplyr::group_by(season) %>% dplyr::mutate(first_post = suppressWarnings(min(week[season_type == "POST"]))) %>% dplyr::ungroup() %>%
    dplyr::mutate(nflverse_team = team, nflverse_opp = opp, nflverse_week = week, team = his_code(team, season), opp = his_code(opp, season),
                  week = ifelse(season_type == "POST", c(28L, 29L, 30L, 32L)[pmin(pmax(week - first_post + 1L, 1L), 4L)], week)) %>% dplyr::select(-first_post)

  # ---- 4. line it up with qb_stats_df_final. It writes posteam in its own codes (BLT) but defteam in nflverse
  #         codes (BAL), so the defense is read off def_ssn. THE WALL: every qb_stats row must find its game. ----
  qk <- q %>% dplyr::select(season, week, posteam, qbgrp_ssn, def_ssn, dplyr::any_of(c("rain_ind", "snow_ind", "wind", "temp"))) %>%
    dplyr::mutate(def_team = sub("[0-9]{4}$", "", def_ssn))
  out <- qk %>% dplyr::left_join(dplyr::rename(g, posteam = team, def_team = opp), by = c("season", "week", "posteam", "def_team"))
  if (any(is.na(out$dtd))) stop(sum(is.na(out$dtd)), " qb_stats_df_final rows found no game -- team code or week mismatch, e.g. ",
                                paste(utils::head(unique(paste(out$qbgrp_ssn, out$def_ssn, out$week)[is.na(out$dtd)]), 5), collapse = "; "))
  if (anyDuplicated(out[, c("season", "week", "posteam")])) stop("duplicate offense-games after the join")
  out <- as.data.frame(out); attr(out, "built") <- format(Sys.time(), "%Y-%m-%d %H:%M"); attr(out, "seasons") <- range(out$season)

  cat("dtd_games: ", nrow(out), " offense-games, ", sum(out$dtd), " non-offensive TDs (", sum(out$int_ret), " int ret, ", sum(out$fum_ret), " fumble ret of which ",
      sum(out$fum_ret_sack), " strip-sacks, ", sum(out$dtd_st), " special teams).  Per offense-game ", round(mean(out$dtd), 4), ".\n", sep = "")
  if (save) {
    saveRDS(out, file.path(dir, "dtd_games.rds")); saveRDS(as.data.frame(nod), file.path(dir, "dtd_plays.rds"))
    cat("wrote ", file.path(dir, "dtd_games.rds"), " and ", file.path(dir, "dtd_plays.rds"), "\n", sep = "")
  }
  invisible(list(games = out, plays = as.data.frame(nod)))
}
