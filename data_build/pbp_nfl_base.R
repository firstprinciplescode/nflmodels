
options(scipen = 999)

conflicts_prefer(nflreadr::load_pbp)

conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise, dplyr::mutate, dplyr::desc, dplyr::summarize)

pbp_base <- load_pbp(c(2016:2025))


'%ni%' <- Negate('%in%')

# LET'S KEEP THINGS SEPARATE UNTIL THE END TO AVOID CROSS-CONTAMINATION OF INFO

pbp_base <- pbp_base %>% mutate(mod_ydstogo = ifelse(goal_to_go == 1, yardline_100, ydstogo))
pbp_base <- pbp_base %>% mutate(lead_timeout = lag(timeout_team, 1), last_timeout_ind = case_when(lead_timeout == posteam ~ 1, is.na(lead_timeout) ~ 0, (lead_timeout != posteam & !is.na(lead_timeout)) ~ -1))
pbp_base$td_side = ifelse((!is.na(pbp_base$td_team) & pbp_base$posteam == pbp_base$td_team), 1, 0)
pbp_base <- pbp_base %>% mutate(posteam_ind = ifelse(home_team == posteam, 1, 0))

pbp_base$week <- ifelse(pbp_base$week == 18 & pbp_base$season <= 2020, 28, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 19 & pbp_base$season <= 2020, 29, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 19 & pbp_base$season > 2020, 28, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 20 & pbp_base$season <= 2020, 30, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 20 & pbp_base$season > 2020, 29, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 21 & pbp_base$season <= 2020, 32, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 21 & pbp_base$season > 2020, 30, pbp_base$week)
pbp_base$week <- ifelse(pbp_base$week == 22, 32, pbp_base$week)


team_ssn_combo <- sqldf("SELECT DISTINCT posteam, season
                          FROM  pbp_base
                          WHERE posteam IS NOT NULL
                          GROUP BY  posteam, season") %>%
  mutate(combo = paste0(posteam, season))
#  UNIQUE TEAM / SEASON ABBREVIATIONS IN THE PBP


###
###
###


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

# Use it
athena_teams <- run_athena_query("SELECT season, abbreviation FROM team_ids_tbl")


head(team_ssn_combo)
head(athena_teams)


team_ssn_combo$combo[which(team_ssn_combo$combo %ni% athena_teams$combo)]
athena_teams$combo[which(athena_teams$combo %ni% team_ssn_combo$combo)]

# AS A RULE - WE TRANSFORM PBP TO FIT PFF, NOT VICE VERSA

# ARI TO ARZ - ALL YEARS
# BAL TO BLT - ALL YEARS
# CLE TO CLV - ALL YEARS
# HOU TO HST - ALL YEARS
# LAC TO SD - 2016
# LV TO OAK - 2016 TO 2019


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



pbp_base$air_yards[which(pbp_base$air_yards <= -54)] = 0

pbp_base$down_one_ind = ifelse(pbp_base$down == 1, 1, 0)
pbp_base$down_two_ind = ifelse(pbp_base$down == 2, 1, 0)
pbp_base$down_three_ind = ifelse(pbp_base$down == 3, 1, 0)
pbp_base$down_four_ind = ifelse(pbp_base$down == 4, 1, 0)

pbp_base$distance_to_sticks = pbp_base$mod_ydstogo - pbp_base$air_yards

pbp_base$season_type <- ifelse(pbp_base$season_type == "REG", 0, 1)
pbp_base$roof <- ifelse(pbp_base$roof %in% c('dome', 'closed'), 1, 0)
pbp_base$surface <- ifelse(pbp_base$surface == "grass", 0, 1)

pbp_base <- pbp_base %>% filter(play_type %in% c("pass", "run")) %>% filter(special_teams_play == 0 & play_type %ni% c('no_play', 'qb_kneel', 'qb_spike', 'punt')) %>% filter(!is.na(play_type)) %>% filter(!is.na(posteam)) %>% filter(is.na(two_point_conv_result))

pbp_base <- pbp_base %>% filter(!grepl("TWO-POINT CONVERSION", desc))

pbp_base$season_type <- ifelse(pbp_base$season_type == "REG", 0, 1)
pbp_base$roof <- ifelse(pbp_base$roof %in% c('dome', 'closed'), 1, 0)
pbp_base$surface <- ifelse(pbp_base$surface == "grass", 0, 1)
pbp_base$run_location <- case_when(is.na(pbp_base$run_location) ~ 2, pbp_base$run_location %in% c("left", "right") ~ 1, pbp_base$run_location == "middle" ~ 0)
pbp_base$run_gap <- case_when(is.na(pbp_base$run_gap) ~ 0, pbp_base$run_gap == "guard" ~ 1, pbp_base$run_gap == "tackle" ~ 2, pbp_base$run_gap == "end" ~ 3)

pbp_base$center_ind = ifelse(pbp_base$run_gap == 0, 1, 0)
pbp_base$guard_ind = ifelse(pbp_base$run_gap == 1, 1, 0)
pbp_base$tackle_ind = ifelse(pbp_base$run_gap == 2, 1, 0)
pbp_base$end_ind = ifelse(pbp_base$run_gap == 3, 1, 0)

pbp_base$pass_length <- case_when(is.na(pbp_base$pass_length) ~ 0, pbp_base$pass_length == "short" ~ 1, pbp_base$pass_length == "deep" ~ 2)
pbp_base$pass_location <- case_when(is.na(pbp_base$pass_location) ~ 0, pbp_base$pass_location == "middle" ~ 1, pbp_base$pass_location %in% c('left', 'right') ~ 2)

pbp_base$middle_ind = ifelse(pbp_base$pass_location == 1, 1, 0)
pbp_base$outside_ind = ifelse(pbp_base$pass_location == 2, 1, 0)


pbp_base <- pbp_base %>% filter(qb_spike == 0 & qb_kneel == 0)
pbp_base <- pbp_base %>% filter(two_point_attempt == 0)

### THIS IS BIG - WILL TRANSFORM ALL SCRAMBLE TO PASS IN A NEW FIELD - PLAY_TYPE_SCR

pbp_base <- pbp_base %>% mutate(play_type_scr = ifelse(qb_scramble == 1 & rush_attempt == 1, "pass", play_type))

pbp_base$half_ind <- case_when(pbp_base$game_half == "Half1" ~ 1, pbp_base$game_half == "Half2" ~ 2, pbp_base$game_half == "Overtime" ~ 3)

pbp_base <- pbp_base %>%
  mutate(
    rain_ind = ifelse(grepl("Rain|Showers|Rainy|Raining", weather, ignore.case = TRUE), 1, 0),
    snow_ind = ifelse(grepl("Snow", weather, ignore.case = TRUE), 1, 0),
    precip_ind = ifelse(rain_ind == 1 | snow_ind == 1, 1, 0)
  )

# ============ WEATHER BACKFILL ============
weather_backfill <- readRDS("~/weather_backfill.rds")
for (i in seq_len(nrow(weather_backfill))) {
  gid <- weather_backfill$nflverse_game_id[i]
  idx <- which(pbp_base$nflverse_game_id == gid & (is.na(pbp_base$temp) | is.na(pbp_base$wind)))
  if (length(idx) > 0 && !is.na(weather_backfill$temp[i])) {
    pbp_base$temp[idx] <- weather_backfill$temp[i]
    pbp_base$wind[idx] <- weather_backfill$wind[i]
    pbp_base$rain_ind[idx] <- as.integer(weather_backfill$precip[i] > 0.1)
    pbp_base$snow_ind[idx] <- as.integer(weather_backfill$precip[i] > 0.1 & weather_backfill$temp[i] <= 35)
  }
}

# Blanket impute remaining (dome/closed only)
pbp_base$wind[which(is.na(pbp_base$wind))] <- 0
pbp_base$temp[which(is.na(pbp_base$temp))] <- 70

# Quick check
table(pbp_base$rain_ind, useNA = "ifany")
table(pbp_base$snow_ind, useNA = "ifany")
table(pbp_base$precip_ind, useNA = "ifany")

nfl_epa_wk <-  sqldf("SELECT  posteam, 
                              week, 
                              season, 
                              AVG(CASE WHEN play_type = 'run' THEN epa END) AS epa_run,
                              AVG(CASE WHEN play_type = 'pass' THEN epa END) AS epa_pass
                        FROM  pbp_base
                        WHERE qb_spike = 0
                        AND   qb_kneel = 0
                        AND   penalty = 0
                        AND   field_goal_attempt = 0
                        GROUP BY  posteam,
                                  week,
                                  season
                      ")

pff_grade_summary <- run_athena_query("SELECT * FROM vw_team_passing_summary")


# Convert both to regular integers before joining
combined_grade_epa_summary <- full_join(
  nfl_epa_wk %>% mutate(week = as.integer(week)),
  pff_grade_summary %>% mutate(week = as.integer(week)),
  by = c("posteam" = "team", "week" = "week", "season" = "season")
) %>% 
  arrange(season, posteam, starting_qb, passing_grade)
# CREATE QBGRP_SSN, DEF_SSN SHIT
# GOOD / BAD FOR THE NFL EPA SHIT

quantile(combined_grade_epa_summary$epa_pass, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE)
quantile(combined_grade_epa_summary$epa_run, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE)

epa_pass_tiebreaker <- median(combined_grade_epa_summary$epa_pass)
epa_run_tiebreaker <- median(combined_grade_epa_summary$epa_run)


combined_grade_epa_summary <- combined_grade_epa_summary %>%
  group_by(posteam, starting_qb_id, season) %>%
  mutate(
    rank_epa_off = row_number(desc(epa_pass)),
    total_games_off = n(),
    good_epa_off_ind = case_when(
      # Top half
      rank_epa_off <= floor(total_games_off / 2) ~ "Good",
      # Bottom half
      rank_epa_off > ceiling(total_games_off / 2) ~ "Bad",
      # Middle game tiebreaker (odd number of games)
      epa_pass >= epa_pass_tiebreaker ~ "Good",
      TRUE ~ "Bad"
    )
  ) %>%
  ungroup() %>%
  group_by(opp, season) %>%
  mutate(
    rank_epa_def = row_number(epa_pass),  # ASC order - lowest EPA = best defense
    total_games_def = n(),
    good_epa_def_ind = case_when(
      # Top half of worst performances = good defense = bad for offense
      rank_epa_def <= floor(total_games_def / 2) ~ "Bad",
      # Bottom half = bad defense = good for offense
      rank_epa_def > ceiling(total_games_def / 2) ~ "Good",
      # Middle game tiebreaker: <0.036 is good defense (bad for offense)
      epa_pass < epa_pass_tiebreaker ~ "Bad",
      TRUE ~ "Good"
    )
  ) %>%
  ungroup() %>%
  select(-rank_epa_off, -total_games_off, -rank_epa_def, -total_games_def)

combined_grade_epa_summary <- combined_grade_epa_summary %>%
  group_by(posteam, starting_qb_id, season) %>%
  mutate(
    rank_epa_off = row_number(desc(epa_run)),
    total_games_off = n(),
    good_epa_run_off_ind = case_when(
      # Top half
      rank_epa_off <= floor(total_games_off / 2) ~ "Good",
      # Bottom half
      rank_epa_off > ceiling(total_games_off / 2) ~ "Bad",
      # Middle game tiebreaker (odd number of games)
      epa_run >= epa_run_tiebreaker ~ "Good",
      TRUE ~ "Bad"
    )
  ) %>%
  ungroup() %>%
  group_by(opp, season) %>%
  mutate(
    rank_epa_def = row_number(epa_run),  # ASC order - lowest EPA = best defense
    total_games_def = n(),
    good_epa_run_def_ind = case_when(
      # Top half of worst performances = good defense = bad for offense
      rank_epa_def <= floor(total_games_def / 2) ~ "Bad",
      # Bottom half = bad defense = good for offense
      rank_epa_def > ceiling(total_games_def / 2) ~ "Good",
      # Middle game tiebreaker: <0.036 is good defense (bad for offense)
      epa_run < epa_run_tiebreaker ~ "Bad",
      TRUE ~ "Good"
    )
  ) %>%
  ungroup() %>%
  select(-rank_epa_off, -total_games_off, -rank_epa_def, -total_games_def)


combined_grade_epa_summary <- combined_grade_epa_summary %>%
  mutate(
    # Extract last name from starting_qb
    last_name = case_when(
      # If there are 3 words, take the middle one
      str_count(starting_qb, "\\S+") == 3 ~ word(starting_qb, 2),
      # Otherwise take the last word
      TRUE ~ word(starting_qb, -1)
    ),
    # Create qbgrp_ssn
    qbgrp_ssn = paste0(posteam, last_name, "-", season),
    # Create def_ssn
    def_ssn = paste0(opp, season)
  ) %>%
  select(-last_name) 


combined_grade_epa_summary <- combined_grade_epa_summary %>%
  group_by(qbgrp_ssn) %>%
  mutate(
    off_epa_run_perc = percent_rank(epa_run),
    off_epa_pass_perc = percent_rank(epa_pass),
    off_pass_gr_perc = percent_rank(passing_grade)
  ) %>%
  ungroup() %>%
  group_by(def_ssn) %>%
  mutate(
    # For defense: invert so lowest EPA allowed = best defense = highest percentile
    # But we want consistency where 1 = best for offense, so we DON'T invert
    def_epa_run_perc = percent_rank(epa_run),
    def_epa_pass_perc = percent_rank(epa_pass),
    def_pass_gr_perc = percent_rank(passing_grade)
  ) %>%
  ungroup()


# OFF PERCENTILES FOR 2025
combined_grade_epa_summary %>% 
  filter(season == 2025) %>%
  group_by(qbgrp_ssn) %>%
  dplyr::summarise(mn_epa_run = mean(def_epa_run_perc, na.rm = T),
                   mn_epa_pass = mean(def_epa_pass_perc, na.rm = T),
                   mn_pass_gr = mean(def_pass_gr_perc, na.rm = T),
                   n = n()) %>%
  filter(n >= 4)


# DEF PERCENTILES FOR 2025
combined_grade_epa_summary %>% 
  filter(season == 2025) %>%
  group_by(def_ssn) %>%
  dplyr::summarise(mn_epa_run = mean(off_epa_run_perc, na.rm = T),
                   mn_epa_pass = mean(off_epa_pass_perc, na.rm = T),
                   mn_pass_gr = mean(off_pass_gr_perc, na.rm = T),
                   n = n())



# Write locally as parquet
write_parquet(combined_grade_epa_summary, "combined_grade_epa_summary.parquet")

# Upload to S3 (overwrites existing)
put_object(
  file = "combined_grade_epa_summary.parquet",
  object = "data/combined_grade_epa_summary/data.parquet",
  bucket = "nfl-pff-data-lucas"
)