
conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise, dplyr::mutate)

rm(combined_pbp)


bucket <- "nfl-pff-data-lucas"

# qbgrp_def_functions/ - all files
tmp <- tempfile(); save_object("qbgrp_def_functions/comparison_blitz_func.rds", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("qbgrp_def_functions/comparison_depth_func.rds", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("qbgrp_def_functions/comparison_less_func.rds", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("qbgrp_def_functions/comparison_pa_func.rds", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("qbgrp_def_functions/comparison_pressure_func.rds", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("qbgrp_def_functions/list_dependencies_blitz.RData", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("qbgrp_def_functions/list_dependencies_depth.RData", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("qbgrp_def_functions/list_dependencies_less.RData", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("qbgrp_def_functions/list_dependencies_pa.RData", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("qbgrp_def_functions/list_dependencies_pressure.RData", bucket = bucket, file = tmp); load(tmp)

# def_functions/ - all files
tmp <- tempfile(); save_object("def_functions/comparison_blitz_def_func.rds", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("def_functions/comparison_depth_def_func.rds", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("def_functions/comparison_less_def_func.rds", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("def_functions/comparison_pa_def_func.rds", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("def_functions/comparison_pressure_def_func.rds", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("def_functions/list_dependencies_blitz_def.RData", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("def_functions/list_dependencies_depth_def.RData", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("def_functions/list_dependencies_less_def.RData", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("def_functions/list_dependencies_pa_def.RData", bucket = bucket, file = tmp); load(tmp)
tmp <- tempfile(); save_object("def_functions/list_dependencies_pressure_def.RData", bucket = bucket, file = tmp); load(tmp)


importance_matrix_blitz <- list_dependencies_blitz[[2]]
df_blitz_scaled_z <- list_dependencies_blitz[[1]]

importance_matrix_blitz_def <- list_dependencies_blitz_def[[2]]
df_blitz_def_scaled_z <- list_dependencies_blitz_def[[1]]

importance_matrix_depth <- list_dependencies_depth[[2]]
df_depth_scaled_z <- list_dependencies_depth[[1]]

importance_matrix_depth_def <- list_dependencies_depth_def[[2]]
df_depth_def_scaled_z <- list_dependencies_depth_def[[1]]

importance_matrix_less <- list_dependencies_less[[2]]
df_less_scaled_z <- list_dependencies_less[[1]]

importance_matrix_less_def <- list_dependencies_less_def[[2]]
df_less_def_scaled_z <- list_dependencies_less_def[[1]]

importance_matrix_pa <- list_dependencies_pa[[2]]
df_pa_scaled_z <- list_dependencies_pa[[1]]

importance_matrix_pa_def <- list_dependencies_pa_def[[2]]
df_pa_def_scaled_z <- list_dependencies_pa_def[[1]]

importance_matrix_pressure <- list_dependencies_pressure[[2]]
df_pressure_scaled_z <- list_dependencies_pressure[[1]]

importance_matrix_pressure_def <- list_dependencies_pressure_def[[2]]
df_pressure_def_scaled_z <- list_dependencies_pressure_def[[1]]


####
####

comparison_blitz_func("HSTStroud-2025", 1.01) # 64
comparison_depth_func("HSTStroud-2025", .98) # 81
comparison_less_func("HSTStroud-2025", 1.06) # 32
comparison_pa_func("HSTStroud-2025", .98) # 74
comparison_pressure_func("HSTStroud-2025", 1.01) # 62


comparison_blitz_def_func("LAC2025", 1.05) # 45
comparison_depth_def_func("LAC2025", 1.06) # 38
comparison_less_def_func("LAC2025", 1.07) # 32
comparison_pa_def_func("LAC2025", 1.02) # 80
comparison_pressure_def_func("LAC2025", 1.06) # 39


receiving_func_base %>%
  filter(qbgrp_ssn %in% c("SEADarnold-2025"), position_group == "TE") %>%
  arrange(player, week)

receiving_func_base %>%
  filter(qbgrp_ssn == "TENWard-2025", position_group == "WR") %>%
  arrange(player, season, week)

receiving_func_base %>%
  filter(player_id == 115664) %>%
  select(season, man_zone_grp_cluster, z_score_percentile, xpass_percentile, td_grp_cluster, xtd_percentile) %>%
  distinct()


receiving_func_base <- 
  left_join(receiving_func_base,
            qb_stats_df_final %>% select(qbgrp_ssn, def_ssn, week, season, temp, wind, rain_ind, snow_ind),
            by = c("qbgrp_ssn", "def_ssn", "week", "season"))


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

align_centroids <- run_athena_query("
    SELECT  *
    FROM    nfl_data.align_centroids
")

rte_centroids <- run_athena_query("
    SELECT  *
    FROM    nfl_data.rte_centroids
")

tgt_centroids <- run_athena_query("
    SELECT  *
    FROM    nfl_data.tgt_centroids
")

rec_func <- function(qbgrp_one, defgrp_one, tgt_cluster_input, rte_cluster_input, align_cluster_input, position_group_input, pos_rank_vec, team_rank_vec, man_zone_grp_input, man_z_vec_input, xpass_na, xpass_vec_input, xtd_grp_input, xtd_grp_na, xtd_vec_input, player_name) {
  wb_tds <- createWorkbook()
  
  categories <- list(
    blitz    = list(qb_threshold = 1.02, def_threshold = 1,  qb_func = comparison_blitz_func,    def_func = comparison_blitz_def_func),
    depth    = list(qb_threshold = 1.01, def_threshold = 1.07,  qb_func = comparison_depth_func,    def_func = comparison_depth_def_func),
    less     = list(qb_threshold = .99,   def_threshold = .94,  qb_func = comparison_less_func,     def_func = comparison_less_def_func),
    pa       = list(qb_threshold = 1.07,   def_threshold = 1.32,  qb_func = comparison_pa_func,       def_func = comparison_pa_def_func),
    pressure = list(qb_threshold = 1.02, def_threshold = .98, qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func)
  )
  
  process_category <- function(category, qbgrp_one, defgrp_one) {
    qb_teams <- c(category$qb_func(qbgrp_one, category$qb_threshold)$QB, qbgrp_one)
    def_teams <- c(category$def_func(defgrp_one, category$def_threshold)$QB, defgrp_one)
    
    receiving_func_base %>%
      ungroup() %>%
      dplyr::filter(qbgrp_ssn %in% qb_teams) %>%
      mutate(
        ind = ifelse(def_ssn %in% def_teams, "In", "Out"),
        rec_ind = ifelse(
          tgt_cluster_name %in% tgt_cluster_input & 
            rte_cluster_name %in% rte_cluster_input & 
            align_cluster_name %in% align_cluster_input & 
            final_position_group %in% position_group_input & 
            pos_rank >= pos_rank_vec[1] & pos_rank <= pos_rank_vec[2] & 
            team_rank >= team_rank_vec[1] & team_rank <= team_rank_vec[2] & 
            man_zone_grp_cluster %in% man_zone_grp_input & 
            z_score_percentile >= man_z_vec_input[1] & z_score_percentile <= man_z_vec_input[2] & 
            (
              (xpass_percentile >= xpass_vec_input[1] & xpass_percentile <= xpass_vec_input[2]) |
                (xpass_na & is.na(xpass_percentile))
            ) &
            td_grp_cluster %in% xtd_grp_input & 
            (
              (xtd_percentile >= xtd_vec_input[1] & xtd_percentile <= xtd_vec_input[2]) |
                (xtd_grp_na & is.na(xtd_percentile))
            ),
          "In", "Out"
        )
      ) %>%
      mutate(has_rel_recs = any(rec_ind == "In")) %>%
      dplyr::filter(has_rel_recs) %>%
      group_by(qbgrp_ssn, def_ssn, week, season, ind) %>%
      dplyr::filter(sum(ifelse(rec_ind == "In", 1, 0), na.rm = TRUE) > 0) %>%
      dplyr::summarise(
        rel_players = sum(ifelse(rec_ind == "In", 1, 0), na.rm = TRUE),
        notrel_players = sum(ifelse(rec_ind != "In", 1, 0), na.rm = TRUE),   
        rel_targets = sum(ifelse(rec_ind == "In", targets, 0), na.rm = TRUE),
        notrel_targets = sum(ifelse(rec_ind != "In", targets, 0), na.rm = TRUE),
        rel_routes = sum(ifelse(rec_ind == "In", routes, 0), na.rm = TRUE),
        notrel_routes = sum(ifelse(rec_ind != "In", routes, 0), na.rm = TRUE),
        rel_pbp_xtds = sum(ifelse(rec_ind == "In", pbp_rec_xtds, 0), na.rm = TRUE),
        notrel_pbp_xtds = sum(ifelse(rec_ind != "In", pbp_rec_xtds, 0), na.rm = TRUE),     
        rel_part_xtds = sum(ifelse(rec_ind == "In", part_rec_xtds, 0), na.rm = TRUE),
        notrel_part_xtds = sum(ifelse(rec_ind != "In", part_rec_xtds, 0), na.rm = TRUE),  
        rel_ypa = mean(ifelse(rec_ind == "In", ypa, NA), na.rm = TRUE),
        notrel_ypa = mean(ifelse(rec_ind != "In", ypa, NA), na.rm = TRUE), 
        rel_pbp_xypa = mean(ifelse(rec_ind == "In", pbp_xypa, NA), na.rm = TRUE),
        notrel_pbp_xypa = mean(ifelse(rec_ind != "In", pbp_xypa, NA), na.rm = TRUE), 
        rel_part_xypa = mean(ifelse(rec_ind == "In", part_xypa, NA), na.rm = TRUE),
        notrel_part_xypa = mean(ifelse(rec_ind != "In", part_xypa, NA), na.rm = TRUE),    
        rel_acc_rate = mean(ifelse(rec_ind == "In", acc_rate, NA), na.rm = TRUE),
        notrel_acc_rate = mean(ifelse(rec_ind != "In", acc_rate, NA), na.rm = TRUE), 
        rel_fastr_cp = mean(ifelse(rec_ind == "In", fastr_cp, NA), na.rm = TRUE),
        notrel_fastr_cp = mean(ifelse(rec_ind != "In", fastr_cp, NA), na.rm = TRUE), 
        rel_pbp_cp = mean(ifelse(rec_ind == "In", pbp_cp, NA), na.rm = TRUE),
        notrel_pbp_cp = mean(ifelse(rec_ind != "In", pbp_cp, NA), na.rm = TRUE),         
        rel_part_cp = mean(ifelse(rec_ind == "In", part_cp, NA), na.rm = TRUE),
        notrel_part_cp = mean(ifelse(rec_ind != "In", part_cp, NA), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        tgt_shr = (rel_targets / (rel_targets + notrel_targets)) / rel_players,
        rte_shr = (rel_routes / (rel_routes + notrel_routes)) / rel_players,
        pbp_xtd_shr = (rel_pbp_xtds / (rel_pbp_xtds + notrel_pbp_xtds)) / rel_players,
        part_xtd_shr = (rel_part_xtds / (rel_part_xtds + notrel_part_xtds)) / rel_players
      ) %>%    
      ungroup() %>%
      group_by(ind) %>%
      dplyr::summarise(
        players = n(),
        tgt_shr = mean(tgt_shr, na.rm = TRUE),
        rte_shr = mean(rte_shr, na.rm = TRUE),      
        pbp_xtd_shr = mean(pbp_xtd_shr, na.rm = TRUE),
        part_xtd_shr = mean(part_xtd_shr, na.rm = TRUE),
        ypa = mean(rel_ypa, na.rm = TRUE),
        pbp_xypa = mean(rel_pbp_xypa, na.rm = TRUE),
        part_xypa = mean(rel_part_xypa, na.rm = TRUE),
        acc_rate = mean(rel_acc_rate, na.rm = TRUE),
        fastr_cp = mean(rel_fastr_cp, na.rm = TRUE),      
        pbp_cp = mean(rel_pbp_cp, na.rm = TRUE),
        part_cp = mean(rel_part_cp, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  category_results <- lapply(categories, function(cat) {
    process_category(cat, qbgrp_one, defgrp_one)
  })
  
  sheet_name <- substr(paste0("Rec - ", player_name), 1, 31)
  addWorksheet(wb_tds, sheet_name)
  
  start_rows <- c(1, 5, 9, 13, 17)
  names(category_results) <- names(categories)
  
  for (i in seq_along(category_results)) {
    writeData(wb_tds, sheet = sheet_name, x = data.frame(category_results[[i]]), startRow = start_rows[i])
  }
  
  # Save to S3
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb_tds, tmp, overwrite = TRUE)
  put_object(file = tmp, object = paste0("outputs/", sheet_name, ".xlsx"), bucket = "nfl-pff-data-lucas")
  
  return(paste0("Saved to s3://nfl-pff-data-lucas/outputs/", sheet_name, ".xlsx"))
}

rec_func(qbgrp_one = "TENWard-2025", 
         defgrp_one = "JAX2025", 
         tgt_cluster_input = c("RB","ST","LT","G"),
         rte_cluster_input = c("SMT","RB","G","LR"), 
         align_cluster_input = c("WWR"), 
         position_group_input = c("WR"), 
         pos_rank_vec = c(4,99), 
         team_rank_vec = c(8,99), 
         man_zone_grp_input = c("WR_LT","WR_DEEP"), 
         man_z_vec_input = c(0,100), 
         xpass_na = T,
         xpass_vec_input = c(0,100),
         xtd_grp_input = c("TD_LOW","REC_LT"), # TD_LOW, TD_HIGH, NA, REC_LT, HB_LT, OTH_LT
         xtd_grp_na = T,
         xtd_vec_input = c(0,100),
         player_name = "Bryce Oliver")


weather_rec_func <- function(qbgrp_one, defgrp_one, tgt_cluster_input, rte_cluster_input, align_cluster_input, position_group_input, pos_rank_vec, team_rank_vec, man_zone_grp_input, man_z_vec_input, xpass_na, xpass_vec_input, xtd_grp_input, xtd_grp_na, xtd_vec_input, player_name) {
  wb_tds <- createWorkbook()
  
  categories <- list(
    blitz    = list(qb_threshold = 1.065, def_threshold = 1.005, qb_func = comparison_blitz_func,    def_func = comparison_blitz_def_func),
    depth    = list(qb_threshold = 1.055, def_threshold = .995,  qb_func = comparison_depth_func,    def_func = comparison_depth_def_func),
    less     = list(qb_threshold = .99,   def_threshold = .95,   qb_func = comparison_less_func,     def_func = comparison_less_def_func),
    pa       = list(qb_threshold = 1.055, def_threshold = .975,  qb_func = comparison_pa_func,       def_func = comparison_pa_def_func),
    pressure = list(qb_threshold = 1.01,  def_threshold = .98,   qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func)
  )
  
  process_category <- function(category, qbgrp_one, defgrp_one) {
    qb_teams <- c(category$qb_func(qbgrp_one, category$qb_threshold)$QB, qbgrp_one)
    def_teams <- c(category$def_func(defgrp_one, category$def_threshold)$QB, defgrp_one)
    
    game_level <- receiving_func_base %>%
      ungroup() %>%
      dplyr::filter(qbgrp_ssn %in% qb_teams) %>%
      mutate(
        wind_na = ifelse(is.na(wind), 0, wind),
        weather = ifelse(wind_na >= 10 | temp <= 50, "1_Bad", "2_Good"),
        ind = ifelse(def_ssn %in% def_teams, "In", "Out"),
        rec_ind = ifelse(
          tgt_cluster_name %in% tgt_cluster_input & 
            rte_cluster_name %in% rte_cluster_input & 
            align_cluster_name %in% align_cluster_input & 
            final_position_group %in% position_group_input & 
            pos_rank >= pos_rank_vec[1] & pos_rank <= pos_rank_vec[2] & 
            team_rank >= team_rank_vec[1] & team_rank <= team_rank_vec[2] & 
            man_zone_grp_cluster %in% man_zone_grp_input & 
            z_score_percentile >= man_z_vec_input[1] & z_score_percentile <= man_z_vec_input[2] & 
            (
              (xpass_percentile >= xpass_vec_input[1] & xpass_percentile <= xpass_vec_input[2]) |
                (xpass_na & is.na(xpass_percentile))
            ) &
            td_grp_cluster %in% xtd_grp_input & 
            (
              (xtd_percentile >= xtd_vec_input[1] & xtd_percentile <= xtd_vec_input[2]) |
                (xtd_grp_na & is.na(xtd_percentile))
            ),
          "In", "Out"
        )
      ) %>%
      mutate(has_rel_recs = any(rec_ind == "In")) %>%
      dplyr::filter(has_rel_recs) %>%
      group_by(qbgrp_ssn, def_ssn, week, season, ind, weather) %>%
      dplyr::filter(sum(ifelse(rec_ind == "In", 1, 0), na.rm = TRUE) > 0) %>%
      dplyr::summarise(
        rel_players = sum(ifelse(rec_ind == "In", 1, 0), na.rm = TRUE),
        notrel_players = sum(ifelse(rec_ind != "In", 1, 0), na.rm = TRUE),   
        rel_targets = sum(ifelse(rec_ind == "In", targets, 0), na.rm = TRUE),
        notrel_targets = sum(ifelse(rec_ind != "In", targets, 0), na.rm = TRUE),
        rel_routes = sum(ifelse(rec_ind == "In", routes, 0), na.rm = TRUE),
        notrel_routes = sum(ifelse(rec_ind != "In", routes, 0), na.rm = TRUE),
        rel_pbp_xtds = sum(ifelse(rec_ind == "In", pbp_rec_xtds, 0), na.rm = TRUE),
        notrel_pbp_xtds = sum(ifelse(rec_ind != "In", pbp_rec_xtds, 0), na.rm = TRUE),     
        rel_part_xtds = sum(ifelse(rec_ind == "In", part_rec_xtds, 0), na.rm = TRUE),
        notrel_part_xtds = sum(ifelse(rec_ind != "In", part_rec_xtds, 0), na.rm = TRUE),  
        rel_ypa = mean(ifelse(rec_ind == "In", ypa, NA), na.rm = TRUE),
        rel_pbp_xypa = mean(ifelse(rec_ind == "In", pbp_xypa, NA), na.rm = TRUE),
        rel_part_xypa = mean(ifelse(rec_ind == "In", part_xypa, NA), na.rm = TRUE),
        rel_acc_rate = mean(ifelse(rec_ind == "In", acc_rate, NA), na.rm = TRUE),
        rel_fastr_cp = mean(ifelse(rec_ind == "In", fastr_cp, NA), na.rm = TRUE),
        rel_pbp_cp = mean(ifelse(rec_ind == "In", pbp_cp, NA), na.rm = TRUE),
        rel_part_cp = mean(ifelse(rec_ind == "In", part_cp, NA), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        tgt_shr = (rel_targets / (rel_targets + notrel_targets)) / rel_players,
        rte_shr = (rel_routes / (rel_routes + notrel_routes)) / rel_players,
        pbp_xtd_shr = (rel_pbp_xtds / (rel_pbp_xtds + notrel_pbp_xtds)) / rel_players,
        part_xtd_shr = (rel_part_xtds / (rel_part_xtds + notrel_part_xtds)) / rel_players
      ) %>%
      ungroup()
    
    selected_metrics <- c("tgt_shr", "rte_shr", "pbp_xtd_shr", "part_xtd_shr", 
                          "rel_ypa", "rel_acc_rate")
    
    result <- game_level %>%
      pivot_longer(cols = all_of(selected_metrics), names_to = "metric", values_to = "value") %>%
      group_by(ind, metric) %>%
      summarise(
        n_Bad = sum(weather == "1_Bad", na.rm = TRUE),
        n_Good = sum(weather == "2_Good", na.rm = TRUE),
        n_All = n(),
        
        mean_Bad = mean(value[weather == "1_Bad"], na.rm = TRUE),
        mean_Good = mean(value[weather == "2_Good"], na.rm = TRUE),
        mean_All = mean(value, na.rm = TRUE),
        
        overall_sd = sd(value, na.rm = TRUE),
        
        wilcox_pval = tryCatch({
          wilcox.test(value ~ weather, exact = FALSE)$p.value
        }, error = function(e) NA),
        
        .groups = "drop"
      ) %>%
      mutate(
        diff_Bad_vs_Good = mean_Bad - mean_Good,
        diff_Bad_vs_All = mean_Bad - mean_All,
        diff_Good_vs_All = mean_Good - mean_All,
        
        d_Bad_vs_Good = diff_Bad_vs_Good / overall_sd,
        d_Bad_vs_All = diff_Bad_vs_All / overall_sd,
        d_Good_vs_All = diff_Good_vs_All / overall_sd,
        
        sig = case_when(
          wilcox_pval < 0.01 ~ "***",
          wilcox_pval < 0.05 ~ "**",
          wilcox_pval < 0.10 ~ "*",
          TRUE ~ ""
        ),
        effect_size = case_when(
          abs(d_Bad_vs_Good) >= 0.8 ~ "large",
          abs(d_Bad_vs_Good) >= 0.5 ~ "medium",
          abs(d_Bad_vs_Good) >= 0.2 ~ "small",
          TRUE ~ "negligible"
        )
      ) %>%
      arrange(ind, metric)
    
    return(result)
  }
  
  category_results <- lapply(categories, function(cat) {
    process_category(cat, qbgrp_one, defgrp_one)
  })
  
  for (cat_name in names(category_results)) {
    addWorksheet(wb_tds, cat_name)
    writeData(wb_tds, sheet = cat_name, x = data.frame(category_results[[cat_name]]))
  }
  
  sheet_name <- substr(paste0("WeatherRec - ", player_name), 1, 31)
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb_tds, tmp, overwrite = TRUE)
  put_object(file = tmp, object = paste0("outputs/", sheet_name, ".xlsx"), bucket = "nfl-pff-data-lucas")
  
  return(paste0("Saved to s3://nfl-pff-data-lucas/outputs/", sheet_name, ".xlsx"))
}


precip_rec_func <- function(qbgrp_one, defgrp_one, tgt_cluster_input, rte_cluster_input, align_cluster_input, position_group_input, pos_rank_vec, team_rank_vec, man_zone_grp_input, man_z_vec_input, xpass_na, xpass_vec_input, xtd_grp_input, xtd_grp_na, xtd_vec_input, player_name) {
  wb_tds <- createWorkbook()
  
  categories <- list(
    blitz    = list(qb_threshold = 1.065, def_threshold = 1.005, qb_func = comparison_blitz_func,    def_func = comparison_blitz_def_func),
    depth    = list(qb_threshold = 1.055, def_threshold = .995,  qb_func = comparison_depth_func,    def_func = comparison_depth_def_func),
    less     = list(qb_threshold = .99,   def_threshold = .95,   qb_func = comparison_less_func,     def_func = comparison_less_def_func),
    pa       = list(qb_threshold = 1.055, def_threshold = .975,  qb_func = comparison_pa_func,       def_func = comparison_pa_def_func),
    pressure = list(qb_threshold = 1.01,  def_threshold = .98,   qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func)
  )
  
  process_category <- function(category, qbgrp_one, defgrp_one) {
    qb_teams <- c(category$qb_func(qbgrp_one, category$qb_threshold)$QB, qbgrp_one)
    def_teams <- c(category$def_func(defgrp_one, category$def_threshold)$QB, defgrp_one)
    
    game_level <- receiving_func_base %>%
      ungroup() %>%
      dplyr::filter(qbgrp_ssn %in% qb_teams) %>%
      mutate(
        precip = ifelse(snow_ind == 1 | rain_ind == 1, "1_Precip", "2_Clear"),
        ind = ifelse(def_ssn %in% def_teams, "In", "Out"),
        rec_ind = ifelse(
          tgt_cluster_name %in% tgt_cluster_input & 
            rte_cluster_name %in% rte_cluster_input & 
            align_cluster_name %in% align_cluster_input & 
            final_position_group %in% position_group_input & 
            pos_rank >= pos_rank_vec[1] & pos_rank <= pos_rank_vec[2] & 
            team_rank >= team_rank_vec[1] & team_rank <= team_rank_vec[2] & 
            man_zone_grp_cluster %in% man_zone_grp_input & 
            z_score_percentile >= man_z_vec_input[1] & z_score_percentile <= man_z_vec_input[2] & 
            (
              (xpass_percentile >= xpass_vec_input[1] & xpass_percentile <= xpass_vec_input[2]) |
                (xpass_na & is.na(xpass_percentile))
            ) &
            td_grp_cluster %in% xtd_grp_input & 
            (
              (xtd_percentile >= xtd_vec_input[1] & xtd_percentile <= xtd_vec_input[2]) |
                (xtd_grp_na & is.na(xtd_percentile))
            ),
          "In", "Out"
        )
      ) %>%
      mutate(has_rel_recs = any(rec_ind == "In")) %>%
      dplyr::filter(has_rel_recs) %>%
      group_by(qbgrp_ssn, def_ssn, week, season, ind, precip) %>%
      dplyr::filter(sum(ifelse(rec_ind == "In", 1, 0), na.rm = TRUE) > 0) %>%
      dplyr::summarise(
        rel_players = sum(ifelse(rec_ind == "In", 1, 0), na.rm = TRUE),
        notrel_players = sum(ifelse(rec_ind != "In", 1, 0), na.rm = TRUE),   
        rel_targets = sum(ifelse(rec_ind == "In", targets, 0), na.rm = TRUE),
        notrel_targets = sum(ifelse(rec_ind != "In", targets, 0), na.rm = TRUE),
        rel_routes = sum(ifelse(rec_ind == "In", routes, 0), na.rm = TRUE),
        notrel_routes = sum(ifelse(rec_ind != "In", routes, 0), na.rm = TRUE),
        rel_pbp_xtds = sum(ifelse(rec_ind == "In", pbp_rec_xtds, 0), na.rm = TRUE),
        notrel_pbp_xtds = sum(ifelse(rec_ind != "In", pbp_rec_xtds, 0), na.rm = TRUE),     
        rel_part_xtds = sum(ifelse(rec_ind == "In", part_rec_xtds, 0), na.rm = TRUE),
        notrel_part_xtds = sum(ifelse(rec_ind != "In", part_rec_xtds, 0), na.rm = TRUE),  
        rel_ypa = mean(ifelse(rec_ind == "In", ypa, NA), na.rm = TRUE),
        rel_pbp_xypa = mean(ifelse(rec_ind == "In", pbp_xypa, NA), na.rm = TRUE),
        rel_part_xypa = mean(ifelse(rec_ind == "In", part_xypa, NA), na.rm = TRUE),
        rel_acc_rate = mean(ifelse(rec_ind == "In", acc_rate, NA), na.rm = TRUE),
        rel_fastr_cp = mean(ifelse(rec_ind == "In", fastr_cp, NA), na.rm = TRUE),
        rel_pbp_cp = mean(ifelse(rec_ind == "In", pbp_cp, NA), na.rm = TRUE),
        rel_part_cp = mean(ifelse(rec_ind == "In", part_cp, NA), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        tgt_shr = (rel_targets / (rel_targets + notrel_targets)) / rel_players,
        rte_shr = (rel_routes / (rel_routes + notrel_routes)) / rel_players,
        pbp_xtd_shr = (rel_pbp_xtds / (rel_pbp_xtds + notrel_pbp_xtds)) / rel_players,
        part_xtd_shr = (rel_part_xtds / (rel_part_xtds + notrel_part_xtds)) / rel_players
      ) %>%
      ungroup()
    
    selected_metrics <- c("tgt_shr", "rte_shr", "pbp_xtd_shr", "part_xtd_shr", 
                          "rel_ypa", "rel_acc_rate")
    
    result <- game_level %>%
      pivot_longer(cols = all_of(selected_metrics), names_to = "metric", values_to = "value") %>%
      group_by(ind, metric) %>%
      summarise(
        n_Precip = sum(precip == "1_Precip", na.rm = TRUE),
        n_Clear = sum(precip == "2_Clear", na.rm = TRUE),
        n_All = n(),
        
        mean_Precip = mean(value[precip == "1_Precip"], na.rm = TRUE),
        mean_Clear = mean(value[precip == "2_Clear"], na.rm = TRUE),
        mean_All = mean(value, na.rm = TRUE),
        
        overall_sd = sd(value, na.rm = TRUE),
        
        wilcox_pval = tryCatch({
          wilcox.test(value ~ precip, exact = FALSE)$p.value
        }, error = function(e) NA),
        
        .groups = "drop"
      ) %>%
      mutate(
        diff_Precip_vs_Clear = mean_Precip - mean_Clear,
        diff_Precip_vs_All = mean_Precip - mean_All,
        diff_Clear_vs_All = mean_Clear - mean_All,
        
        d_Precip_vs_Clear = diff_Precip_vs_Clear / overall_sd,
        d_Precip_vs_All = diff_Precip_vs_All / overall_sd,
        d_Clear_vs_All = diff_Clear_vs_All / overall_sd,
        
        sig = case_when(
          wilcox_pval < 0.01 ~ "***",
          wilcox_pval < 0.05 ~ "**",
          wilcox_pval < 0.10 ~ "*",
          TRUE ~ ""
        ),
        effect_size = case_when(
          abs(d_Precip_vs_Clear) >= 0.8 ~ "large",
          abs(d_Precip_vs_Clear) >= 0.5 ~ "medium",
          abs(d_Precip_vs_Clear) >= 0.2 ~ "small",
          TRUE ~ "negligible"
        )
      ) %>%
      arrange(ind, metric)
    
    return(result)
  }
  
  category_results <- lapply(categories, function(cat) {
    process_category(cat, qbgrp_one, defgrp_one)
  })
  
  for (cat_name in names(category_results)) {
    addWorksheet(wb_tds, cat_name)
    writeData(wb_tds, sheet = cat_name, x = data.frame(category_results[[cat_name]]))
  }
  
  sheet_name <- substr(paste0("PrecipRec - ", player_name), 1, 31)
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb_tds, tmp, overwrite = TRUE)
  put_object(file = tmp, object = paste0("outputs/", sheet_name, ".xlsx"), bucket = "nfl-pff-data-lucas")
  
  return(paste0("Saved to s3://nfl-pff-data-lucas/outputs/", sheet_name, ".xlsx"))
}


# Route clusters - input order: c(behind_los, short, medium, deep)
rte_closest_func <- function(counts, centroids = rte_centroids) {
  pcts <- 100 * counts / sum(counts)
  
  centroids$distance <- apply(centroids[, c("behind_los_rte_rate", "short_rte_rate", "medium_rte_rate", "deep_rte_rate")], 1, function(row) {
    sqrt(sum((row - pcts)^2))
  })
  
  centroids %>%
    arrange(distance) %>%
    select(name, distance, everything())
}

# Target clusters - input order: c(behind_los, short, medium, deep)
tgt_closest_func <- function(counts, centroids = tgt_centroids) {
  pcts <- 100 * counts / sum(counts)
  
  centroids$distance <- apply(centroids[, c("behind_los_tgt_rate", "short_tgt_rate", "medium_tgt_rate", "deep_tgt_rate")], 1, function(row) {
    sqrt(sum((row - pcts)^2))
  })
  
  centroids %>%
    arrange(distance) %>%
    select(name, distance, everything())
}

# Alignment clusters - input order: c(wide, slot, inline, behind)
align_closest_func <- function(counts, centroids = align_centroids) {
  pcts <- 100 * counts / sum(counts)
  
  centroids$distance <- apply(centroids[, c("wide_rate", "slot_rate", "inline_rate", "behind_rate")], 1, function(row) {
    sqrt(sum((row - pcts)^2))
  })
  
  centroids %>%
    arrange(distance) %>%
    select(name, distance, everything())
}


rte_closest_func(c(13,60,22,11))

tgt_closest_func(c(22,22,14,11))

align_closest_func(c(71, 42, 1, 1))


# , "HSTStroud-2024"
receiving_func_base %>%
  filter(qbgrp_ssn %in% c("TENWard-2025"), 
         position_group %in% c("WR"), 
         pos_rank >= 4, pos_rank <= 99, team_rank >= 7, team_rank <= 99, 
         (tgt_cluster_name %in% c("RB","ST","LT") | rte_cluster_name %in% c("RB","SMT","G","LR")) , 
         ((onfield_perc >= 0 & onfield_perc <= .1) | (tgt_share <= .04 & tgt_share >= 0))
         ) %>%
  mutate(tgt_per_snaps = targets / snap_counts_pass_route,
         pbp_td_tgt_ratio = pbp_xtds_share / tgt_share,
         part_td_tgt_ratio = part_xtds_share / tgt_share) %>%
  filter(pbp_td_tgt_ratio != Inf) %>%
  dplyr::summarise(acc_rate = mean(acc_rate, na.rm = T),
                   fastr_cp = mean(fastr_cp, na.rm = T),
                   pbp_cp = mean(pbp_cp, na.rm = T),
                   part_cp = mean(part_cp, na.rm = T))

receiving_func_base %>%
  filter(position_group == "WR", tgt_cluster_name %in% c("MT","DT","ML"), pos_rank <= 3, season %in% c(2025), posteam == "TEN") %>%
  mutate(tgt_per_snaps = targets / snap_counts_pass_route,
         pbp_td_tgt_ratio = pbp_xtds_share / tgt_share,
         part_td_tgt_ratio = part_xtds_share / tgt_share) %>%
  arrange(pbp_xtds_share ) %>%
  pull(pbp_xtds_share )

receiving_func_base %>%
  filter(def_ssn == "JAX2025", 
         position_group %in% c("WR"), 
         pos_rank >= 4, pos_rank <= 99, team_rank >= 7, team_rank <= 99, 
         (tgt_cluster_name %in% c("RB","ST","LT") | rte_cluster_name %in% c("RB","SMT","G","LR")) , 
         ((onfield_perc >= 0 & onfield_perc <= .1) | (tgt_share <= .04 & tgt_share >= 0))     ) %>%
  mutate(tgt_per_snaps = targets / snap_counts_pass_route,
         pbp_td_tgt_ratio = pbp_xtds_share / tgt_share,
         part_td_tgt_ratio = part_xtds_share / tgt_share) %>%
  filter(pbp_td_tgt_ratio != Inf) %>%
  dplyr::summarise(acc_rate = mean(acc_rate, na.rm = T),
                   fastr_cp = mean(fastr_cp, na.rm = T),
                   pbp_cp = mean(pbp_cp, na.rm = T),
                   part_cp = mean(part_cp, na.rm = T))

::summarise(mn_tgt_rate = mean(tgt_per_snaps, na.rm = T))

dplyr::summarise(mn_pbp_td_tgt_ratio = mean(pbp_td_tgt_ratio, na.rm = T))

dplyr::summarise(ypa = mean(ypa, na.rm = T),
                 pbp_xypa = mean(pbp_xypa, na.rm = T),
                 part_xypa = mean(part_xypa, na.rm = T))

dplyr::summarise(acc_rate = mean(acc_rate, na.rm = T),
                 fastr_cp = mean(fastr_cp, na.rm = T),
                 pbp_cp = mean(pbp_cp, na.rm = T),
                 part_cp = mean(part_cp, na.rm = T))