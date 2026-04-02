
combined_pbp

conflicts_prefer(nflfastR::load_pbp)

conflict_prefer_all("dplyr", quiet = TRUE)

# Option 1: Specify what to KEEP, remove everything else
keep_objects <- c("combined_pbp", "qb_stats_df_base", "real_scores", "run_athena_query", "con")
rm(list = setdiff(ls(), keep_objects))

'%ni%' <- Negate('%in%')

run_athena_query <- function(sql, max_wait = 120) {
  # Start query
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  
  # Poll for completion
  status <- "RUNNING"
  elapsed <- 0
  while (status %in% c("RUNNING", "QUEUED") && elapsed < max_wait) {
    Sys.sleep(2)
    elapsed <- elapsed + 2
    
    status_cmd <- sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.State" --output text',
      query_id
    )
    status <- trimws(system(status_cmd, intern = TRUE))
  }
  
  if (status != "SUCCEEDED") {
    # Get error message
    error_cmd <- sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.StateChangeReason" --output text',
      query_id
    )
    error_msg <- system(error_cmd, intern = TRUE)
    stop(sprintf("Query failed with status %s: %s", status, error_msg))
  }
  
  # Get results location
  result_cmd <- sprintf(
    'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.ResultConfiguration.OutputLocation" --output text',
    query_id
  )
  s3_path <- system(result_cmd, intern = TRUE)
  
  # Read CSV result
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}

pbp_base <- load_pbp(c(2016:2025))

pbp_base$posteam[which(pbp_base$posteam == "ARI")] = "ARZ"
pbp_base$posteam[which(pbp_base$posteam == "BAL")] = "BLT"
pbp_base$posteam[which(pbp_base$posteam == "CLE")] = "CLV"
pbp_base$posteam[which(pbp_base$posteam == "HOU")] = "HST"
pbp_base$posteam[which(pbp_base$posteam == "LAC" & pbp_base$season == 2016)] = "SD"
pbp_base$posteam[which(pbp_base$posteam == "LV" & pbp_base$season <= 2019)] = "OAK"

pbp_base$defteam[which(pbp_base$defteam == "ARI")] = "ARZ"
pbp_base$defteam[which(pbp_base$posteam == "BAL")] = "BLT"
pbp_base$defteam[which(pbp_base$defteam == "CLE")] = "CLV"
pbp_base$defteam[which(pbp_base$defteam == "HOU")] = "HST"
pbp_base$defteam[which(pbp_base$defteam == "LAC" & pbp_base$season == 2016)] = "SD"
pbp_base$defteam[which(pbp_base$defteam == "LV" & pbp_base$season <= 2019)] = "OAK"

pbp_base$td_team[which(pbp_base$td_team == "ARI")] = "ARZ"
pbp_base$td_team[which(pbp_base$td_team == "BAL")] = "BLT"
pbp_base$td_team[which(pbp_base$td_team == "CLE")] = "CLV"
pbp_base$td_team[which(pbp_base$td_team == "HOU")] = "HST"
pbp_base$td_team[which(pbp_base$td_team == "LAC" & pbp_base$season == 2016)] = "SD"
pbp_base$td_team[which(pbp_base$td_team == "LV" & pbp_base$season <= 2019)] = "OAK"

pbp_base$td_side = ifelse((!is.na(pbp_base$td_team) & pbp_base$posteam == pbp_base$td_team), 1, 0)

pbp_base$week <- ifelse(pbp_base$week == 18 & pbp_base$season <= 2020, 28, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 19 & pbp_base$season <= 2020, 29, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 19 & pbp_base$season > 2020, 28, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 20 & pbp_base$season <= 2020, 30, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 20 & pbp_base$season > 2020, 29, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 21 & pbp_base$season <= 2020, 32, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 21 & pbp_base$season > 2020, 30, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 22, 32, pbp_base$week)


real_scores <- pbp_base %>% group_by(posteam, week, season) %>% 
  dplyr::summarise(tds = sum(touchdown, na.rm = T), 
                   fgs = sum(field_goal_attempt, na.rm = T))

rm(pbp_base)


qb_stats_df_base <- combined_pbp %>% 
  group_by(qbgrp_ssn, def_ssn, posteam, defteam, week, season, wind, temp, rain_ind, snow_ind) %>% 
  dplyr::summarise(
    pass_rate = sum(pass_attempt, na.rm = T) / sum(play, na.rm = T),
    fastr_xpass_rate = sum(xpass, na.rm = T) / sum(play, na.rm = T),
    pbp_xpass_rate = sum(pbp_predicted_xpass, na.rm = T) / sum(play, na.rm = T),
    part_xpass_rate = sum(part_predicted_xpass, na.rm = T) / sum(play, na.rm = T),
    scr_rate = (sum(qb_scramble, na.rm = TRUE) - sum(ifelse(!is.na(air_yards) & qb_scramble == 1, 1, 0))) / sum(qb_dropback, na.rm = TRUE),
    scr_ypc = sum(ifelse(qb_scramble == 1 & is.na(air_yards), yards_gained, 0), na.rm = TRUE) / (sum(qb_scramble, na.rm = TRUE) - sum(ifelse(!is.na(air_yards) & qb_scramble == 1, 1, 0))),
    pbp_scr_xypc = sum(ifelse(qb_scramble == 1 & is.na(air_yards), pbp_predicted_scramble_ypc, 0), na.rm = TRUE) / (sum(qb_scramble, na.rm = TRUE) - sum(ifelse(!is.na(air_yards) & qb_scramble == 1, 1, 0))),
    part_scr_xypc = sum(ifelse(qb_scramble == 1 & is.na(air_yards), part_predicted_scramble_ypc, 0), na.rm = TRUE) / (sum(qb_scramble, na.rm = TRUE) - sum(ifelse(!is.na(air_yards) & qb_scramble == 1, 1, 0))),
    acc_rate = sum(complete_pass, na.rm = TRUE) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    fastr_cp = sum(cp, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    pbp_cp = sum(pbp_predicted_cp, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    part_cp = sum(part_predicted_cp, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    pbp_pressure = sum(pbp_predicted_pbp_pressure, na.rm = T) / sum(qb_dropback, na.rm = T),
    part_pressure_before = sum(part_predicted_pressure_before, na.rm = T) / sum(qb_dropback, na.rm = T),
    part_pressure_after = (sum(part_predicted_pressure_after_pass, na.rm = T) + sum(part_predicted_pressure_scramble, na.rm = T)) / sum(ifelse(is.na(part_predicted_pressure_after_pass) & is.na(part_predicted_pressure_scramble), 0, 1)),
    sack_rate = sum(sack, na.rm = TRUE) / sum(qb_dropback, na.rm = TRUE),
    pbp_sack_rate = sum(pbp_predicted_sack, na.rm = T) / sum(qb_dropback, na.rm = TRUE),
    part_sack_rate = sum(part_predicted_sack, na.rm = T) / sum(qb_dropback, na.rm = TRUE),
    ypa = sum(passing_yards, na.rm = TRUE) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    pbp_xypa = sum(pbp_predicted_ypa, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    part_xypa = sum(part_predicted_ypa, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    ypc = sum(ifelse(rush_attempt == 1 & qb_scramble == 0, yards_gained, 0)) / (sum(rush_attempt, na.rm = T) - sum(qb_scramble, na.rm = T)),
    pbp_xypc = mean(pbp_predicted_ypc, na.rm = T),
    part_xypc = mean(part_predicted_ypc, na.rm = T),
    yac = sum(yards_after_catch, na.rm = TRUE) / sum(complete_pass, na.rm = T),
    pbp_yac = sum(pbp_predicted_yds_after_catch, na.rm = T) / sum(complete_pass, na.rm = T),
    part_yac = sum(part_predicted_yds_after_catch, na.rm = T) / sum(complete_pass, na.rm = T),
    
    adot = sum(air_yards, na.rm = T) / (sum(pass_attempt, na.rm = TRUE) - sum(sack, na.rm = TRUE)),
    plays = sum(play, na.rm = TRUE),
    no_huddle = sum(no_huddle, na.rm = TRUE) / sum(play, na.rm = TRUE),
    pbp_xtds = sum(pbp_after_new_xtd, na.rm = T),
    part_xtds = sum(part_after_new_xtd, na.rm = T)
  ) 

rm(combined_pbp)

qb_stats_df_base <- left_join(qb_stats_df_base, real_scores, by = c("posteam", "week", "season"))


qb_stats_df_base$part_sack_rate[which(qb_stats_df_base$part_xpass_rate == 0)] <- NA
# GOTTA START WITH PART_SACK_RATE
qb_stats_df_base$part_scr_xypc[which(qb_stats_df_base$part_xpass_rate == 0)] <- NA
qb_stats_df_base$part_pressure_after[which(qb_stats_df_base$part_pressure_after == 0)] <- NA
qb_stats_df_base$part_pressure_before[which(qb_stats_df_base$part_pressure_before == 0)] <- NA
qb_stats_df_base$part_cp[which(qb_stats_df_base$part_cp == 0)] <- NA
qb_stats_df_base$part_xypa[which(qb_stats_df_base$part_xypa == 0)] <- NA
qb_stats_df_base$part_xypc[which(qb_stats_df_base$part_xypc == 0)] <- NA
qb_stats_df_base$part_yac[which(qb_stats_df_base$part_yac == 0)] <- NA
qb_stats_df_base$part_xtds[which(qb_stats_df_base$part_xtds == 0)] <- NA

qb_stats_df_base$part_xpass_rate[which(qb_stats_df_base$part_xpass_rate == 0)] <- NA

qb_stats_df_base$temp[which(is.na(qb_stats_df_base$temp))] <- 71
qb_stats_df_base$wind[which(is.na(qb_stats_df_base$wind))] <- 0


###
###


# Option 1: Specify what to KEEP, remove everything else
keep_objects <- c("combined_pbp", "qb_stats_df_base", "real_scores")
rm(list = setdiff(ls(), keep_objects))


run_athena_query <- function(sql, max_wait = 120) {
  # Start query
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  cat("Query ID:", query_id, "\n")
  
  # Poll for completion
  status <- "RUNNING"
  elapsed <- 0
  while (status %in% c("RUNNING", "QUEUED") && elapsed < max_wait) {
    Sys.sleep(2)
    elapsed <- elapsed + 2
    
    status_cmd <- sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.State" --output text',
      query_id
    )
    status <- trimws(system(status_cmd, intern = TRUE))
  }
  
  cat("Final status:", status, "\n")
  
  if (status != "SUCCEEDED") {
    error_cmd <- sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.StateChangeReason" --output text',
      query_id
    )
    error_msg <- system(error_cmd, intern = TRUE)
    stop(sprintf("Query failed with status %s: %s", status, error_msg))
  }
  
  # Get results location
  result_cmd <- sprintf(
    'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.ResultConfiguration.OutputLocation" --output text',
    query_id
  )
  s3_path <- trimws(system(result_cmd, intern = TRUE))
  cat("S3 path:", s3_path, "\n")
  
  # Check if file exists
  Sys.sleep(2)  # Give S3 a moment
  check_cmd <- sprintf("aws s3 ls %s", s3_path)
  cat("File check:", system(check_cmd, intern = TRUE), "\n")
  
  # Read CSV result
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}


query_less <- run_athena_query("
      SELECT  
          epa.qbgrp_ssn,
          epa.good_off_ind,
          pa.*
      FROM nfl_data.vw_passing_tip pa
      INNER JOIN nfl_data.combined_grade_epa_summary epa 
          ON pa.team_name = epa.posteam
          AND pa.week = epa.week 
          AND pa.season = epa.season 
  ")

query_pa <- run_athena_query("
      SELECT  
          epa.qbgrp_ssn,
          epa.good_off_ind,
          pa.*
      FROM nfl_data.vw_combined_pa pa
      INNER JOIN nfl_data.combined_grade_epa_summary epa 
          ON pa.team_name = epa.posteam
          AND pa.week = epa.week 
          AND pa.season = epa.season 
  ")

query_depth <- run_athena_query("
      SELECT  
          epa.qbgrp_ssn,
          epa.good_off_ind,
          depth.*
      FROM nfl_data.vw_combined_depth depth
      INNER JOIN nfl_data.combined_grade_epa_summary epa 
          ON depth.team_name = epa.posteam
          AND depth.week = epa.week 
          AND depth.season = epa.season 
  ")

query_blitz <- run_athena_query("
      SELECT  
          epa.def_ssn,
          epa.good_def_ind,
          blitz.*
      FROM nfl_data.vw_combined_blitz blitz
      INNER JOIN nfl_data.combined_grade_epa_summary epa 
          ON blitz.team_name = epa.posteam
          AND blitz.week = epa.week 
          AND blitz.season = epa.season 
  ")

query_pressure <- run_athena_query("
      SELECT  
          epa.def_ssn,
          epa.good_epa_def_ind,
          pressure.*
      FROM nfl_data.vw_combined_pressure pressure
      INNER JOIN nfl_data.combined_grade_epa_summary epa 
          ON pressure.team_name = epa.posteam
          AND pressure.week = epa.week 
          AND pressure.season = epa.season 
  ")



qb_stats_df_intermediary <- left_join(qb_stats_df_base, 
                              query_blitz %>% dplyr::select(team_name:season, blitz_rate, blitz_grade, no_blitz_grade, blitz_qbr, no_blitz_qbr), 
                              by = c("posteam" = "team_name", "week" = "week", "season" = "season"))

qb_stats_df_intermediary <- left_join(qb_stats_df_intermediary, 
                              query_depth %>% dplyr::select(team_name:season, behind_los_rate:deep_rate, behind_los_grade, short_grade, medium_grade, deep_grade, behind_los_qbr, short_qbr, medium_qbr, deep_qbr), 
                              by = c("posteam" = "team_name", "week" = "week", "season" = "season"))

qb_stats_df_intermediary <- left_join(qb_stats_df_intermediary, 
                              query_less %>% dplyr::select(team_name:season, less_rate, less_grade, more_grade, less_qbr, more_qbr), 
                              by = c("posteam" = "team_name", "week" = "week", "season" = "season"))

qb_stats_df_intermediary <- left_join(qb_stats_df_intermediary, 
                              query_pa %>% dplyr::select(team_name:season, pa_rate, npa_grade, pa_grade, npa_qbr, pa_qbr), 
                              by = c("posteam" = "team_name", "week" = "week", "season" = "season"))

qb_stats_df_final <- left_join(qb_stats_df_intermediary, 
                              query_pressure %>% dplyr::select(team_name:season, pressure_rate, no_pressure_grade, pressure_grade, no_pressure_qbr, pressure_qbr), 
                              by = c("posteam" = "team_name", "week" = "week", "season" = "season"))


df_twp_agg <- query_less %>% group_by(team_name, week, season) %>% 
  dplyr::summarise(less_rate = sum(less_snaps, na.rm = T) / (sum(less_snaps, na.rm = T) + sum(more_snaps, na.rm = T)),
                   less_twp_rate = sum(less_snaps * less_twp_rate, na.rm = TRUE) / sum(less_snaps, na.rm = TRUE) / 100,
                   more_twp_rate = sum(more_snaps * more_twp_rate, na.rm = TRUE) / sum(more_snaps, na.rm = TRUE) / 100,
                   less_int_rate = sum(less_snaps * less_int_rate, na.rm = TRUE) / sum(less_snaps, na.rm = T),
                   more_int_rate = sum(more_snaps * more_int_rate, na.rm = TRUE) / sum(more_snaps, na.rm = T))


df_twp_agg_stats <- df_twp_agg %>% 
  mutate(twp_rate = less_rate * less_twp_rate + (1 - less_rate) * more_twp_rate,
         int_rate = less_rate * less_int_rate + (1 - less_rate) * more_int_rate) %>% 
  dplyr::select(team_name, week, season, twp_rate, int_rate)


qb_stats_df_final <- left_join(qb_stats_df_final, 
                              df_twp_agg_stats, by = c("posteam" = "team_name", "week" = "week", "season" = "season"))


####
####


qb_stats_df_final <- qb_stats_df_final %>% ungroup()


qb_stats_df_final <- qb_stats_df_final %>%
  group_by(qbgrp_ssn) %>%
  mutate(
    pass_rate_rank = (rank(pass_rate, ties.method = "average") - 1) / (n() - 1),
    fastr_xpass_rate_rank = (rank(fastr_xpass_rate, ties.method = "average") - 1) / (n() - 1),
    pbp_xpass_rate_rank = (rank(pbp_xpass_rate, ties.method = "average") - 1) / (n() - 1),
    part_xpass_rate_rank = (rank(part_xpass_rate, ties.method = "average") - 1) / (n() - 1),
    scr_rate_rank = (rank(scr_rate, ties.method = "average") - 1) / (n() - 1),
    scr_ypc_rank = (rank(scr_ypc, ties.method = "average") - 1) / (n() - 1),
    pbp_scr_xypc_rank = (rank(pbp_scr_xypc, ties.method = "average", na.last = "keep") - 1) / (n() - 1),
    part_scr_xypc_rank = (rank(part_scr_xypc, ties.method = "average", na.last = "keep") - 1) / (n() - 1),
    acc_rate_rank = (rank(acc_rate, ties.method = "average") - 1) / (n() - 1),
    fastr_cp_rank = (rank(fastr_cp, ties.method = "average") - 1) / (n() - 1),
    pbp_cp_rank = (rank(pbp_cp, ties.method = "average") - 1) / (n() - 1),
    part_cp_rank = (rank(part_cp, ties.method = "average") - 1) / (n() - 1),
    pbp_pressure_rank = (n() - rank(pbp_pressure, ties.method = "average")) / (n() - 1),
    part_pressure_before_rank = (n() - rank(part_pressure_before, ties.method = "average")) / (n() - 1),
    part_pressure_after_rank = (n() - rank(part_pressure_after, ties.method = "average") - 1) / (n() - 1),
    
    sack_rate_rank = (n() - rank(sack_rate, ties.method = "average")) / (n() - 1),
    pbp_sack_rate_rank = (n() - rank(pbp_sack_rate, ties.method = "average")) / (n() - 1),
    part_sack_rate_rank = (n() - rank(part_sack_rate, ties.method = "average")) / (n() - 1),
    ypa_rank = (rank(ypa, ties.method = "average") - 1) / (n() - 1),
    pbp_xypa_rank = (rank(pbp_xypa, ties.method = "average") - 1) / (n() - 1),
    part_xypa_rank = (rank(part_xypa, ties.method = "average") - 1) / (n() - 1),
    yac_rank = (rank(yac, ties.method = "average") - 1) / (n() - 1),
    pbp_yac_rank = (rank(pbp_yac, ties.method = "average") - 1) / (n() - 1),
    part_yac_rank = (rank(part_yac, ties.method = "average") - 1) / (n() - 1),
    ypc_rank = (rank(ypc, ties.method = "average") - 1) / (n() - 1),
    pbp_xypc_rank = (rank(pbp_xypc, ties.method = "average") - 1) / (n() - 1),
    part_xypc_rank = (rank(part_xypc, ties.method = "average") - 1) / (n() - 1),
    adot_rank = (rank(adot, ties.method = "average") - 1) / (n() - 1),
    plays_rank = (rank(plays, ties.method = "average") - 1) / (n() - 1),
    no_huddle_rank = (rank(no_huddle, ties.method = "average") - 1) / (n() - 1),
    pbp_xtds_rank = (rank(pbp_xtds, ties.method = "average") - 1) / (n() - 1),
    part_xtds_rank = (rank(part_xtds, ties.method = "average") - 1) / (n() - 1),
    
    blitz_rate_rank = (rank(blitz_rate, ties.method = "average") - 1) / (n() - 1),
    no_blitz_gr_rank = (rank(no_blitz_grade, ties.method = "average") - 1) / (n() - 1),
    blitz_gr_rank = (rank(blitz_grade, ties.method = "average") - 1) / (n() - 1),
    no_blitz_qbr_rank = (rank(no_blitz_qbr, ties.method = "average") - 1) / (n() - 1),
    blitz_qbr_rank = (rank(blitz_qbr, ties.method = "average") - 1) / (n() - 1),
    
    behind_los_rate_rank = (rank(behind_los_rate, ties.method = "average") - 1) / (n() - 1),
    short_rate_rank = (rank(short_rate, ties.method = "average") - 1) / (n() - 1),
    medium_rate_rank = (rank(medium_rate, ties.method = "average") - 1) / (n() - 1),
    deep_rate_rank = (rank(deep_rate, ties.method = "average") - 1) / (n() - 1),    
    
    behind_los_gr_rank = (rank(behind_los_grade, ties.method = "average") - 1) / (n() - 1),
    short_gr_rank = (rank(short_grade, ties.method = "average") - 1) / (n() - 1),
    medium_gr_rank = (rank(medium_grade, ties.method = "average") - 1) / (n() - 1),
    deep_gr_rank = (rank(deep_grade, ties.method = "average") - 1) / (n() - 1),
    behind_los_qbr_rank = (rank(behind_los_qbr, ties.method = "average") - 1) / (n() - 1),
    short_qbr_rank = (rank(short_qbr, ties.method = "average") - 1) / (n() - 1),
    medium_qbr_rank = (rank(medium_qbr, ties.method = "average") - 1) / (n() - 1),
    deep_qbr_rank = (rank(deep_qbr, ties.method = "average") - 1) / (n() - 1),
    
    less_rate_rank = (rank(less_rate, ties.method = "average")) / (n() - 1),
    less_gr_rank = (rank(less_grade, ties.method = "average") - 1) / (n() - 1),
    more_gr_rank = (rank(more_grade, ties.method = "average") - 1) / (n() - 1),
    less_qbr_rank = (rank(less_qbr, ties.method = "average") - 1) / (n() - 1),
    more_qbr_rank = (rank(more_qbr, ties.method = "average") - 1) / (n() - 1),    
    
    pa_rate_rank = (rank(pa_rate, ties.method = "average") - 1) / (n() - 1),
    npa_gr_rank = (rank(npa_grade, ties.method = "average") - 1) / (n() - 1),
    pa_gr_rank = (rank(pa_grade, ties.method = "average") - 1) / (n() - 1),
    npa_qbr_rank = (rank(npa_qbr, ties.method = "average") - 1) / (n() - 1),
    pa_qbr_rank = (rank(pa_qbr, ties.method = "average") - 1) / (n() - 1),     
    
    pressure_rate_rank = (n() - rank(pressure_rate, ties.method = "average")) / (n() - 1),
    no_pressure_gr_rank = (rank(no_pressure_grade, ties.method = "average") - 1) / (n() - 1),
    pressure_gr_rank = (rank(pressure_grade, ties.method = "average") - 1) / (n() - 1),
    no_pressure_qbr_rank = (rank(no_pressure_qbr, ties.method = "average") - 1) / (n() - 1),
    pressure_qbr_rank = (rank(pressure_qbr, ties.method = "average") - 1) / (n() - 1), 
    
    twp_rate_rank = (n() - rank(twp_rate, ties.method = "average")) / (n() - 1),
    int_rate_rank = (n() - rank(int_rate, ties.method = "average")) / (n() - 1)
  ) %>%
  ungroup()

qb_stats_df_final <- qb_stats_df_final %>%
  group_by(def_ssn) %>%
  mutate(
    pass_rate_rank_def = (rank(pass_rate, ties.method = "average") - 1) / (n() - 1),
    fastr_xpass_rate_rank_def = (rank(fastr_xpass_rate, ties.method = "average") - 1) / (n() - 1),
    pbp_xpass_rate_rank_def = (rank(pbp_xpass_rate, ties.method = "average") - 1) / (n() - 1),
    part_xpass_rate_rank_def = (rank(part_xpass_rate, ties.method = "average") - 1) / (n() - 1),
    scr_rate_rank_def = (rank(scr_rate, ties.method = "average") - 1) / (n() - 1),
    scr_ypc_rank_def = (rank(scr_ypc, ties.method = "average") - 1) / (n() - 1),
    pbp_scr_xypc_rank_def = (rank(pbp_scr_xypc, ties.method = "average", na.last = "keep") - 1) / (n() - 1),
    part_scr_xypc_rank_def = (rank(part_scr_xypc, ties.method = "average", na.last = "keep") - 1) / (n() - 1),
    acc_rate_rank_def = (rank(acc_rate, ties.method = "average") - 1) / (n() - 1),
    fastr_cp_rank_def = (rank(fastr_cp, ties.method = "average") - 1) / (n() - 1),
    pbp_cp_rank_def = (rank(pbp_cp, ties.method = "average") - 1) / (n() - 1),
    part_cp_rank_def = (rank(part_cp, ties.method = "average") - 1) / (n() - 1),
    
    pbp_pressure_rank_def = (n() - rank(pbp_pressure, ties.method = "average")) / (n() - 1),
    part_pressure_before_rank_def = (n() - rank(part_pressure_before, ties.method = "average")) / (n() - 1),
    part_pressure_after_rank_def = (n() - rank(part_pressure_after, ties.method = "average")) / (n() - 1),    
    
    sack_rate_rank_def = (n() - rank(sack_rate, ties.method = "average")) / (n() - 1),
    pbp_sack_rate_rank_def = (n() - rank(pbp_sack_rate, ties.method = "average")) / (n() - 1),
    part_sack_rate_rank_def = (n() - rank(part_sack_rate, ties.method = "average")) / (n() - 1),
    ypa_rank_def = (rank(ypa, ties.method = "average") - 1) / (n() - 1),
    pbp_xypa_rank_def = (rank(pbp_xypa, ties.method = "average") - 1) / (n() - 1),
    part_xypa_rank_def = (rank(part_xypa, ties.method = "average") - 1) / (n() - 1),
    yac_rank_def = (rank(yac, ties.method = "average") - 1) / (n() - 1),
    pbp_yac_rank_def = (rank(pbp_yac, ties.method = "average") - 1) / (n() - 1),
    part_yac_rank_def = (rank(part_yac, ties.method = "average") - 1) / (n() - 1),
    ypc_rank_def = (rank(ypc, ties.method = "average") - 1) / (n() - 1),
    pbp_xypc_rank_def = (rank(pbp_xypc, ties.method = "average") - 1) / (n() - 1),
    part_xypc_rank_def = (rank(part_xypc, ties.method = "average") - 1) / (n() - 1),    
    adot_rank_def = (rank(adot, ties.method = "average") - 1) / (n() - 1),
    plays_rank_def = (rank(plays, ties.method = "average") - 1) / (n() - 1),
    no_huddle_rank_def = (rank(no_huddle, ties.method = "average") - 1) / (n() - 1),
    pbp_xtds_rank_def = (rank(pbp_xtds, ties.method = "average") - 1) / (n() - 1),
    part_xtds_rank_def = (rank(part_xtds, ties.method = "average") - 1) / (n() - 1),
    
    blitz_rate_rank_def = (rank(blitz_rate, ties.method = "average") - 1) / (n() - 1),
    no_blitz_gr_rank_def = (rank(no_blitz_grade, ties.method = "average") - 1) / (n() - 1),
    blitz_gr_rank_def = (rank(blitz_grade, ties.method = "average") - 1) / (n() - 1),
    no_blitz_qbr_rank_def = (rank(no_blitz_qbr, ties.method = "average") - 1) / (n() - 1),
    blitz_qbr_rank_def = (rank(blitz_qbr, ties.method = "average") - 1) / (n() - 1),
    
    behind_los_rate_rank_def = (rank(behind_los_rate, ties.method = "average") - 1) / (n() - 1),
    short_rate_rank_def = (rank(short_rate, ties.method = "average") - 1) / (n() - 1),
    medium_rate_rank_def = (rank(medium_rate, ties.method = "average") - 1) / (n() - 1),
    deep_rate_rank_def = (rank(deep_rate, ties.method = "average") - 1) / (n() - 1),  
    
    behind_los_gr_rank_def = (rank(behind_los_grade, ties.method = "average") - 1) / (n() - 1),
    short_gr_rank_def = (rank(short_grade, ties.method = "average") - 1) / (n() - 1),
    medium_gr_rank_def = (rank(medium_grade, ties.method = "average") - 1) / (n() - 1),
    deep_gr_rank_def = (rank(deep_grade, ties.method = "average") - 1) / (n() - 1),
    behind_los_qbr_rank_def = (rank(behind_los_qbr, ties.method = "average") - 1) / (n() - 1),
    short_qbr_rank_def = (rank(short_qbr, ties.method = "average") - 1) / (n() - 1),
    medium_qbr_rank_def = (rank(medium_qbr, ties.method = "average") - 1) / (n() - 1),
    deep_qbr_rank_def = (rank(deep_qbr, ties.method = "average") - 1) / (n() - 1),
    
    less_rate_rank_def = (rank(less_rate, ties.method = "average")) / (n() - 1),
    less_gr_rank_def = (rank(less_grade, ties.method = "average") - 1) / (n() - 1),
    more_gr_rank_def = (rank(more_grade, ties.method = "average") - 1) / (n() - 1),
    less_qbr_rank_def = (rank(less_qbr, ties.method = "average") - 1) / (n() - 1),
    more_qbr_rank_def = (rank(more_qbr, ties.method = "average") - 1) / (n() - 1),    
    
    pa_rate_rank_def = (rank(pa_rate, ties.method = "average") - 1) / (n() - 1),
    npa_gr_rank_def = (rank(npa_grade, ties.method = "average") - 1) / (n() - 1),
    pa_gr_rank_def = (rank(pa_grade, ties.method = "average") - 1) / (n() - 1),
    npa_qbr_rank_def = (rank(npa_qbr, ties.method = "average") - 1) / (n() - 1),
    pa_qbr_rank_def = (rank(pa_qbr, ties.method = "average") - 1) / (n() - 1),     
    
    pressure_rate_rank_def = (n() - rank(pressure_rate, ties.method = "average")) / (n() - 1),
    no_pressure_gr_rank_def = (rank(no_pressure_grade, ties.method = "average") - 1) / (n() - 1),
    pressure_gr_rank_def = (rank(pressure_grade, ties.method = "average") - 1) / (n() - 1),
    no_pressure_qbr_rank_def = (rank(no_pressure_qbr, ties.method = "average") - 1) / (n() - 1),
    pressure_qbr_rank_def = (rank(pressure_qbr, ties.method = "average") - 1) / (n() - 1), 
    
    twp_rate_rank_def = (n() - rank(twp_rate, ties.method = "average")) / (n() - 1),
    int_rate_rank_def = (n() - rank(int_rate, ties.method = "average")) / (n() - 1)
  ) %>%
  ungroup()


qb_stats_df_final$part_xpass_rate_rank[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_scr_xypc_rank[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_cp_rank[which(qb_stats_df_final$season == 2025)] <- NA

qb_stats_df_final$part_pressure_after_rank_def[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_pressure_after_rank[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_pressure_before_rank_def[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_pressure_before_rank[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_sack_rate_rank[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_xypa_rank[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_xypc_rank[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_yac_rank[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_xtds_rank[which(qb_stats_df_final$season == 2025)] <- NA

qb_stats_df_final$part_xpass_rate_rank_def[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_scr_xypc_rank_def[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_cp_rank_def[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_sack_rate_rank_def[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_xypc_rank_def[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_xypa_rank_def[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_yac_rank_def[which(qb_stats_df_final$season == 2025)] <- NA
qb_stats_df_final$part_xtds_rank_def[which(qb_stats_df_final$season == 2025)] <- NA

View(qb_stats_df_final)

qb_stats_df_final$temp[which(is.na(qb_stats_df_final$temp))] <- 71
qb_stats_df_final$wind[which(is.na(qb_stats_df_final$wind))] <- 0


receiving_func_base <- left_join(receiving_func_base,
          qb_stats_df_final %>% select(qbgrp_ssn, def_ssn, week, season, temp, wind, rain_ind, snow_ind), 
          by = c("qbgrp_ssn", "def_ssn","week","season"))


# View(qb_stats_df_final %>% filter(qbgrp_ssn == "PITRodgers-2025"))


qb_stats_df_final %>% filter(posteam == "TB", season == 2025) %>%
  mutate(rattler_ind = ifelse(week %in% c(1,2,3,15,16,17),1,0)) %>%
  group_by(rattler_ind) %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank_def, na.rm = T),
                   n = n())

qb_stats_df_final %>% filter(posteam == "TB", season == 2025) %>%
  group_by(qbgrp_ssn) %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank_def, na.rm = T),
                   n = n())

qb_stats_df_final %>% filter(defteam == "TB", season == 2025) %>%
  mutate(rattler_ind = ifelse(week %in% c(1,2,3,15,16,17),1,0)) %>%
  group_by(rattler_ind) %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank, na.rm = T),
                   n = n())

qb_stats_df_final %>% filter(posteam == "TB", season == 2024) %>%
  group_by(qbgrp_ssn) %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank_def, na.rm = T),
                   n = n())

qb_stats_df_final %>% filter(defteam == "TB", season == 2024) %>%
  group_by(def_ssn) %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank, na.rm = T),
                   n = n())

qb_stats_df_final %>% filter(posteam == "OAK", season == 2017) %>%
  group_by(qbgrp_ssn) %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank_def, na.rm = T),
                   n = n())

qb_stats_df_final %>% filter(posteam == "TB", season == 2024) %>%
  group_by(qbgrp_ssn) %>%
  dplyr::summarise(behind_rate_rank = mean(behind_los_qbr_rank_def, na.rm = T),
                   short_rate_rank = mean(behind_los_qbr_rank_def, na.rm = T),
                   medium_rate_rank = mean(medium_qbr_rank_def, na.rm = T),
                   deep_rate_rank = mean(deep_qbr_rank_def, na.rm = T),
                   pressure_rate_rank = mean(pressure_rate_rank_def, na.rm = T),
                   n = n())

qb_stats_df_final %>% filter(posteam == "TB", season == 2025) %>%
  mutate(rattler_ind = ifelse(week %in% c(1,2,3,15,16,17),1,0)) %>%
  group_by(rattler_ind) %>%
  dplyr::summarise(behind_rate_rank = mean(behind_los_qbr_rank_def, na.rm = T),
                   short_rate_rank = mean(behind_los_qbr_rank_def, na.rm = T),
                   medium_rate_rank = mean(medium_qbr_rank_def, na.rm = T),
                   deep_rate_rank = mean(deep_qbr_rank_def, na.rm = T),
                   pressure_rate_rank = mean(pressure_rate_rank_def, na.rm = T),
                   n = n())
