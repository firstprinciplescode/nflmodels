
# WATCH THESE COLNAMES HERE

colnames(pbp_base)[c(394:404, 406:410)]
colnames(pbp_base)[c(394:404, 406:410)] <- paste0("pbp_", colnames(pbp_base)[c(394:404, 406:410)])

colnames(part_nfl)[c(477:489, 491:495)] 
colnames(part_nfl)[c(477:489, 491:495)] <- paste0("part_", colnames(part_nfl)[c(477:489, 491:495)])

part_nfl <- part_nfl %>% 
  rename_with(~ gsub("^part_part_", "part_", .x))

pbp_base <- pbp_base %>% 
  rename_with(~ gsub("^pbp_pbp_", "pbp_", .x))

keep_objects <- c("part_nfl", "con", "pbp_base")
rm(list = setdiff(ls(), keep_objects))

'%ni%' <- Negate('%in%')

part_nfl_join <- part_nfl %>%
  select(nflverse_game_id:old_game_id, pressure_ind, is_st_play:punt_ind_def, part_predicted_before_xtd:part_predicted_yds_after_catch, part_predicted_after_xtd:part_after_xtd_new)

combined_pbp <- left_join(pbp_base, part_nfl_join, by = c("game_id" = "nflverse_game_id", "play_id" = "play_id"))


pbp_part_model <- s3read_using(
  readRDS,
  object = "models/xtd_normalization/pbp_part_model.rds",
  bucket = "nfl-pff-data-lucas"
)

pbp_only_model <- s3read_using(
  readRDS,
  object = "models/xtd_normalization/pbp_only_model.rds",
  bucket = "nfl-pff-data-lucas"
)

names(pbp_part_model$coefficients)

names(pbp_only_model$coefficients)

# part_before_xtd_new / part_after_xtd_new --> part_before_new_xtd / part_after_new_xtd
# part_predicted_before_xtd --> part_before_old_xtd
# part_predicted_after_xtd --> part_after_old_xtd

# pbp_predicted_before_xtd --> DOES NOT MATTER
# pbp_predicted_after_xtd --> pbp_after_old_xtd
# pbp_before_xtd_new --> pbp_before_new_xtd
# pbp_after_xtd_new --> pbp_after_new_xtd

colnames(combined_pbp)[which(colnames(combined_pbp) == "part_before_xtd_new")] <- "part_before_new_xtd"
colnames(combined_pbp)[which(colnames(combined_pbp) == "part_after_xtd_new")] <- "part_after_new_xtd"
colnames(combined_pbp)[which(colnames(combined_pbp) == "part_predicted_before_xtd")] <- "part_before_old_xtd"
colnames(combined_pbp)[which(colnames(combined_pbp) == "part_predicted_after_xtd")] <- "part_after_old_xtd"

colnames(combined_pbp)[which(colnames(combined_pbp) == "pbp_predicted_after_xtd")] <- "pbp_after_old_xtd"
colnames(combined_pbp)[which(colnames(combined_pbp) == "pbp_before_xtd_new")] <- "pbp_before_new_xtd"
colnames(combined_pbp)[which(colnames(combined_pbp) == "pbp_after_xtd_new")] <- "pbp_after_new_xtd"


run_athena_query <- function(sql) {
  # Start query
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  
  # Wait for completion
  Sys.sleep(2)
  
  # Get results location
  result_cmd <- sprintf('aws athena get-query-execution --query-execution-id %s --query "QueryExecution.ResultConfiguration.OutputLocation" --output text', query_id)
  s3_path <- system(result_cmd, intern = TRUE)
  
  # Read CSV result
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}


combined_grade_epa_summary <- run_athena_query("SELECT * FROM combined_grade_epa_summary")


combined_pbp <- left_join(combined_pbp, 
          combined_grade_epa_summary %>% 
  select(posteam, week, season, qbgrp_ssn, def_ssn), 
          by = c("posteam", "week", "season"))


### JUMP TO XPASS NORMALIZATOIN HERE


combined_pbp <- combined_pbp %>% 
  mutate(mixed_xpass = case_when(
    # 2016-2023: Full model
    !is.na(part_predicted_xpass) & !is.na(pbp_predicted_xpass) & !is.na(xpass) ~ 
      plogis(-3.08345 + 1.68837 * xpass + .2213 * pbp_predicted_xpass + 4.06614 * part_predicted_xpass),
    
    # 2024+: No-part model  
    !is.na(pbp_predicted_xpass) & !is.na(xpass) ~ 
      plogis(-2.7559 + 1.2588 * xpass + 4.15996 * pbp_predicted_xpass),
    
    # Fallbacks
    !is.na(part_predicted_xpass) ~ part_predicted_xpass,
    !is.na(pbp_predicted_xpass) ~ pbp_predicted_xpass,
    !is.na(xpass) ~ xpass,
    
    TRUE ~ NA_real_
  ))
