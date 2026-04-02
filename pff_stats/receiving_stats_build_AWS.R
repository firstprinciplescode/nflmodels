
# START WITH PFF_RECEIVING_MAN_ZONE_EXPLORATION WORKSPACE
# UPLOAD / LOAD THE IDS / PBP_PART COMBINED WORKSPACES

conflict_prefer_all("dplyr", quiet = TRUE)

# Add some debugging to your function
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

# Option 1: Specify what to KEEP, remove everything else
keep_objects <- c("combined_pbp", "con", "cluster_join", "receiver_scheme_final", "combined_ids", "pbp_receiver_stats_one", "pbp_receiver_stats", "run_athena_query", "vw_receiving_enriched", "receiver_xtd_final_join", "receiver_xtd_final", "receiver_xpass_final_join", "receiver_xpass_final", "receiving_func_base")
rm(list = setdiff(ls(), keep_objects))

'%ni%' <- Negate('%in%')


pbp_receiver_stats_one <- combined_pbp %>% 
  filter(!is.na(yards_gained) & play_type == "pass" & pass_attempt == 1 & sack == 0 & !is.na(air_yards)) %>%
  group_by(receiver_id, receiver_player_name, posteam, week, season, qbgrp_ssn, def_ssn, game_id, old_game_id.x) %>%
  dplyr::summarise(pbp_rec_xtds = sum(pbp_after_new_xtd, na.rm = T),
                   part_rec_xtds = sum(part_after_new_xtd, na.rm = T),
                   acc_rate = mean(complete_pass, na.rm = T),
                   fastr_cp = mean(cp, na.rm = T),
                   pbp_cp = mean(pbp_predicted_cp, na.rm = T),
                   part_cp = mean(part_predicted_cp, na.rm = T),
                   ypa = mean(yards_gained, na.rm = T),
                   pbp_xypa = mean(pbp_predicted_ypa, na.rm = T),
                   part_xypa = mean(part_predicted_ypa, na.rm = T),
                   yac = mean(yards_after_catch, na.rm = T),
                   pbp_yac = mean(pbp_predicted_yds_after_catch, na.rm = T),
                   part_yac = mean(part_predicted_yds_after_catch, na.rm = T),
                   adot = mean(air_yards, na.rm = T)
  )


pbp_receiver_stats_one$receiver_id[which(pbp_receiver_stats_one$receiver_player_name == "A.Mattison" & pbp_receiver_stats_one$posteam %in% c("MIN", "LV", "MIA"))] <- "00-0035236"

pbp_receiver_stats_one$receiver_id[which(pbp_receiver_stats_one$receiver_player_name == "M.Woods" & pbp_receiver_stats_one$posteam %in% c("CLV") & pbp_receiver_stats_one$season %in% c(2022, 2024))] <- "00-0037300"

pbp_receiver_stats_one$receiver_id[which(pbp_receiver_stats_one$receiver_player_name == "D.Johnson" & pbp_receiver_stats_one$posteam == "HST" & pbp_receiver_stats_one$season == 2020 & pbp_receiver_stats_one$week %in% c(15,16,17))] <- "00-0032187"
pbp_receiver_stats_one$receiver_player_name[which(pbp_receiver_stats_one$receiver_id == "00-0032187" & pbp_receiver_stats_one$posteam == "HST" & pbp_receiver_stats_one$season == 2020 & pbp_receiver_stats_one$week %in% c(15,16,17))] <- "Da.Johnson"


pbp_receiver_stats <- pbp_receiver_stats_one %>%
  left_join(
    combined_ids %>% 
      select(-c(franchise_id, position)) %>% 
      filter(!is.na(gsis_id)) %>%
      distinct(),
    by = c("receiver_id" = "gsis_id", "posteam" = "team", "week" = "week", "season" = "season")
  )

pbp_receiver_stats$player[which(pbp_receiver_stats$receiver_id == "00-0037300
")] <- "Michael Woods"
pbp_receiver_stats$player_id[which(pbp_receiver_stats$receiver_id == "00-0037300")] <- 84117
pbp_receiver_stats$pos_group[which(pbp_receiver_stats$receiver_id == "00-0037300")] <- "REC"

pbp_receiver_stats$player[which(pbp_receiver_stats$receiver_id == "00-0032187
")] <- "David Johnson"
pbp_receiver_stats$player_id[which(pbp_receiver_stats$receiver_id == "00-0032187")] <- 9519
pbp_receiver_stats$pos_group[which(pbp_receiver_stats$receiver_id == "00-0032187")] <- "BACK"

pbp_receiver_stats$receiver_id[which(pbp_receiver_stats$receiver_player_name == "D.Johnson" & pbp_receiver_stats$posteam == "HST" & pbp_receiver_stats$week %in% c(15,16,17) & pbp_receiver_stats$season == 2020)] <- "00-0032187"
pbp_receiver_stats$player[which(pbp_receiver_stats$receiver_player_name == "D.Johnson" & pbp_receiver_stats$posteam == "HST" & pbp_receiver_stats$week %in% c(15,16,17) & pbp_receiver_stats$season == 2020)] <- "David Johnson"
pbp_receiver_stats$player_id[which(pbp_receiver_stats$receiver_player_name == "D.Johnson" & pbp_receiver_stats$posteam == "HST" & pbp_receiver_stats$week %in% c(15,16,17) & pbp_receiver_stats$season == 2020)] <- 9519
pbp_receiver_stats$pos_group[which(pbp_receiver_stats$receiver_player_name == "D.Johnson" & pbp_receiver_stats$posteam == "HST" & pbp_receiver_stats$week %in% c(15,16,17) & pbp_receiver_stats$season == 2020)] <- "BACK"


pbp_receiver_stats$pos_group[which(pbp_receiver_stats$pos_group %in% c("OL", "OTHER", "DB", "LB") | is.na(pbp_receiver_stats$pos_group))] <- "OTHER"


# pbp_receiver_stats

# RTE_SHARE
# TGT_SHARE
# XTD_SHARE

# I NEED THE SNAP COUNTS TOO


vw_receiving_enriched <- run_athena_query("
    SELECT  *
    FROM    nfl_data.vw_receiving_enriched
")

vw_play_counts <- run_athena_query("
    SELECT  *
    FROM    nfl_data.vw_play_counts_enriched
    WHERE   snap_counts_pass_route > 0
")


pbp_receiver_stats_snap_join <- left_join(pbp_receiver_stats, 
          vw_receiving_enriched %>% select(player_id, week, season, team_abbreviation, position, routes, targets),
          by = c("player_id" = "player_id", 
                 "week" = "week", 
                 "season" = "season",
                 "posteam" = "team_abbreviation"))

pbp_receiver_stats_final <- left_join(pbp_receiver_stats_snap_join, 
          cluster_join, 
          by = c("player_id" = "player_id",
                 "posteam" = "team_abbreviation",
                 "season" = "season")) %>%
  filter(!is.na(receiver_player_name) & !is.na(receiver_id))

pbp_receiver_stats_final %>%
  filter(is.na(position_group))
# THIS ONE HAS SOME ISSUES ... THAT I MISSED
# SEVERAL RECEIVERS, BACKS

# L.HUMPHREY - HE'S BACK ON THE BRONCOS NOW, SO NO REAL DATA ANYWHERE BASICALLY. FILL DOWN UP I GUESS. HE HAS SNAPS, BUT ... NOTHING IN MY DATA? HE'S LITERALLY NOT IN THE WEEK 13 / 2025 / DEN
# B.BERRIOS - CONFUSED. HE'S IN ... MIA / 28 / 2023

pbp_receiver_stats_final %>%
  filter(receiver_id == '00-0034419')

pbp_receiver_stats_final %>%
  filter(qbgrp_ssn == "MIATagovailoa-2023", week == 28)

pbp_receiver_stats %>%
  filter(receiver_id == '00-0034419', qbgrp_ssn == "MIATagovailoa-2023", week == 28)


# IF POS_GROUP --> BACK OR POSITION %IN% C(FB, HB) OR POSITION_GROUP == "HB" --> BACK
# IF POSITION %IN% C(TE, TE-L, TE-R) OR POSITION_GROUP == TE
# IF POS_GROUP --> REC AND POSITION %NI% C(TE, TE-L, TE-R) AND POSITION_GROUP %NI% TE, HB
# EVERYTHING ELSE IS OTHER


pbp_receiver_stats_final <- pbp_receiver_stats_final %>%
  mutate(
    final_position_group = case_when(
      pos_group == "BACK" | position %in% c("FB", "HB") | position_group == "HB" ~ "BACK",
      position %in% c("TE", "TE-L", "TE-R") | position_group == "TE" ~ "TE",
      pos_group == "REC" & position %ni% c("TE", "TE-L", "TE-R") & position_group %ni% c("TE", "HB") ~ "WR",
      TRUE ~ "OTHER"
    )
  ) %>%
  # Group for rankings and shares
  group_by(qbgrp_ssn, def_ssn, week, season) %>%
  mutate(
    # Team-level shares
    tgt_share = targets / sum(targets, na.rm = T),
    pbp_xtds_share = pbp_rec_xtds / sum(pbp_rec_xtds, na.rm = T),
    part_xtds_share = part_rec_xtds / sum(part_rec_xtds, na.rm = T)
  ) %>%
  ungroup()
# NOT DONE - I HAVE TO JOIN IN SNAPS AND SHIT DAMN IT. 



play_counts_receiving_stats_one <- 
  left_join(vw_play_counts %>% 
  select(game_id, player, player_id, abbreviation, week, season, snap_counts_pass_route, snap_counts_total_pass),
  pbp_receiver_stats_final, 
  by = c("player_id", "week", "season")
)


play_counts_receiving_stats_one %>%
  filter(week == 8, season == 2025, abbreviation == "PHI")
# SO ... GONNA HAVE TO LIKE "FILL DOWN" SO TO SPEAK THE NAs FOR ... 
# RECEIVER_ID THROUGH DERF_SSN, PLAYER.X:POSITION, PLAYER.Y:FINAL_POSITION_GROUP, , 
# PBP_REC_XTDS / PART_REC_XTDS / TGT_SHARE / PBP_XTDS_SHARE --> 0
# PART_REC_XTDS --> IF SEASON == 2025, NA


# If that returns 0 rows, safe to fill:
play_counts_receiving_stats_one <- play_counts_receiving_stats_one %>%
  group_by(abbreviation, week, season) %>%
  fill(qbgrp_ssn, def_ssn, posteam, .direction = "downup") %>%
  ungroup()

# Verify no NAs left
play_counts_receiving_stats_one %>%
  filter(is.na(qbgrp_ssn) | is.na(def_ssn)) %>%
  nrow()


play_counts_receiving_stats_two <- play_counts_receiving_stats_one %>%
  # Static player info (doesn't change across time)
  group_by(player_id) %>%
  fill(
    receiver_id, receiver_player_name, player.x, player.y,
    .direction = "downup"
  ) %>%
  ungroup() %>%
  # Season-level dimensions (clusters, position, z-scores change year to year)
  group_by(player_id, season) %>%
  fill(
    position_group, align_cluster_name, rte_cluster_name, 
    tgt_cluster_name, man_zone_grp_cluster, z_score_percentile, z_source,
    final_position_group, pos_group, position, posteam,
    .direction = "downup"
  ) %>%
  ungroup() %>%
  # Zero out counting/share stats
  mutate(
    targets = replace_na(targets, 0),
    routes = coalesce(routes, snap_counts_pass_route),
    pbp_rec_xtds = replace_na(pbp_rec_xtds, 0),
    tgt_share = replace_na(tgt_share, 0),
    pbp_xtds_share = replace_na(pbp_xtds_share, 0),
    part_rec_xtds = case_when(
      season == 2025 ~ NA_real_,
      is.na(part_rec_xtds) ~ 0,
      TRUE ~ part_rec_xtds
    ),
    part_xtds_share = case_when(
      season == 2025 ~ NA_real_,
      is.na(part_xtds_share) ~ 0,
      TRUE ~ part_xtds_share
    )
  )

# Check what's still NA
play_counts_receiving_stats_two %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "na_count") %>%
  filter(na_count > 0)

play_counts_receiving_stats_two <- 
  play_counts_receiving_stats_two %>%
  arrange(season, week, qbgrp_ssn, desc(snap_counts_total_pass))


play_counts_receiving_stats_two %>% 
  filter(week == 11, season == 2025, abbreviation == "CIN")

play_counts_receiving_stats_two <- play_counts_receiving_stats_two %>%
  # Team rank (all players)
  group_by(qbgrp_ssn, def_ssn, abbreviation, week, season) %>%
  mutate(
    team_rank = dense_rank(desc(snap_counts_pass_route) + desc(targets) * 0.0001)
  ) %>%
  ungroup() %>%
  # Position rank (within position group)
  group_by(qbgrp_ssn, def_ssn, abbreviation, week, season, final_position_group) %>%
  mutate(
    pos_rank = dense_rank(desc(snap_counts_pass_route) + desc(targets) * 0.0001)
  ) %>%
  ungroup()


total_snaps <- run_athena_query("
    SELECT  *
    FROM    nfl_data.vw_total_snaps 
")

play_counts_receiving_stats_two <- left_join(play_counts_receiving_stats_two, 
          total_snaps,
          by = c("abbreviation", "week", "season")
          )

play_counts_receiving_stats_two$onfield_perc = play_counts_receiving_stats_two$snap_counts_pass_route / 
play_counts_receiving_stats_two$total_pass_snaps

play_counts_receiving_stats_two %>%
  filter(week == 15, season == 2025, abbreviation == "PHI")


play_counts_receiving_stats_three <- left_join(play_counts_receiving_stats_two,
          receiver_xpass_final_join,
          by = c("receiver_id", "player_id", "posteam", "season"))

play_counts_receiving_stats_four <- left_join(play_counts_receiving_stats_three,
          receiver_xtd_final_join,
          by = c("receiver_id", "player_id", "posteam", "season"))

receiving_func_base <- play_counts_receiving_stats_four

# SHIT SOMEHOW WORKS OUT AT THE END ... SO YEAH. FUN. 


receiving_func_base %>%
  filter(week == 30, season == 2025, abbreviation == "SEA")

rm(combined_pbp)
rm(combined_ids)


left_join(receiving_func_base,
          qb_stats_df_final %>% select(qbgrp_ssn, def_ssn, week, season, temp, wind, rain_ind, snow_ind), 
          by = c("qbgrp_ssn", "def_ssn","week","season"))

          