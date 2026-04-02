
library(aws.s3)

bucket <- "nfl-pff-data-lucas"

# Option 1: Specify what to KEEP, remove everything else
keep_objects <- c("qb_stats_df_final", "qb_stats_df_final_filtered", "con", "bucket")
rm(list = setdiff(ls(), keep_objects))

'%ni%' <- Negate('%in%')

conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise, dplyr::mutate, dplyr::count)


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


comparison_blitz_func("CARYoung-2025", 1.035) # 36
comparison_depth_func("CARYoung-2025", 1.025) # 49
comparison_less_func("CARYoung-2025", .96) # 96
comparison_pa_func("CARYoung-2025", 1.025) # 53
comparison_pressure_func("CARYoung-2025", .98) # 81

comparison_blitz_def_func("LA2025", .975) # 60
comparison_depth_def_func("LA2025", .975) # 56
comparison_less_def_func("LA2025", .92) # 79
comparison_pa_def_func("LA2025", .945) # 77
comparison_pressure_def_func("LA2025", .95) # 67

View(qb_stats_df_final %>% filter(qbgrp_ssn == "CARYoung-2025"))
View(qb_stats_df_final %>% filter(def_ssn == "LA2025"))

xtds_func <- function(qbgrp_one, defgrp_one) {
  wb_tds <- createWorkbook()
  
  # Define categories with correct thresholds and column selections
  categories <- list(
    blitz = list(
      qb_threshold = 1.035, def_threshold = .975,
      qb_func = comparison_blitz_func, def_func = comparison_blitz_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def", 
                  "pressure_rate", "pressure_rate_rank", "pressure_rate_rank_def", 
                  "blitz_rate", "blitz_rate_rank", "blitz_rate_rank_def", 
                  "blitz_grade", "blitz_gr_rank", "blitz_gr_rank_def", 
                  "no_blitz_grade", "no_blitz_gr_rank", "no_blitz_gr_rank_def", 
                  "blitz_qbr", "blitz_qbr_rank", "blitz_qbr_rank_def", 
                  "no_blitz_qbr", "no_blitz_qbr_rank", "no_blitz_qbr_rank_def", 
                  "sack_rate", "sack_rate_rank", "sack_rate_rank_def")
    ),
    
    depth = list(
      qb_threshold = 1.025, def_threshold = .975,
      qb_func = comparison_depth_func, def_func = comparison_depth_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "behind_los_rate", "behind_los_rate_rank", "behind_los_rate_rank_def",
                  "behind_los_grade", "behind_los_gr_rank", "behind_los_gr_rank_def",
                  "behind_los_qbr", "behind_los_qbr_rank", "behind_los_qbr_rank_def",
                  "short_rate", "short_rate_rank", "short_rate_rank_def",
                  "short_grade", "short_gr_rank", "short_gr_rank_def",
                  "short_qbr", "short_qbr_rank", "short_qbr_rank_def",
                  "medium_rate", "medium_rate_rank", "medium_rate_rank_def",
                  "medium_grade", "medium_gr_rank", "medium_gr_rank_def",
                  "medium_qbr", "medium_qbr_rank", "medium_qbr_rank_def",
                  "deep_rate", "deep_rate_rank", "deep_rate_rank_def",
                  "deep_grade", "deep_gr_rank", "deep_gr_rank_def",
                  "deep_qbr", "deep_qbr_rank", "deep_qbr_rank_def")
    ),
    
    less = list(
      qb_threshold = .96, def_threshold = .92,
      qb_func = comparison_less_func, def_func = comparison_less_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "less_rate", "less_rate_rank", "less_rate_rank_def",
                  "less_grade", "less_gr_rank", "less_gr_rank_def",
                  "more_grade", "more_gr_rank", "more_gr_rank_def",
                  "less_qbr", "less_qbr_rank", "less_qbr_rank_def",
                  "more_qbr", "more_qbr_rank", "more_qbr_rank_def")
    ),
    
    pa = list(
      qb_threshold = 1.025, def_threshold = .945,
      qb_func = comparison_pa_func, def_func = comparison_pa_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "pressure_rate", "pressure_rate_rank", "pressure_rate_rank_def",
                  "pa_rate", "pa_rate_rank", "pa_rate_rank_def",
                  "pa_grade", "pa_gr_rank", "pa_gr_rank_def",
                  "npa_grade", "npa_gr_rank", "npa_gr_rank_def",
                  "pa_qbr", "pa_qbr_rank", "pa_qbr_rank_def",
                  "npa_qbr", "npa_qbr_rank", "npa_qbr_rank_def",
                  "sack_rate", "sack_rate_rank", "sack_rate_rank_def")
    ),
    
    pressure = list(
      qb_threshold = .98, def_threshold = .95,
      qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "acc_rate", "acc_rate_rank", "acc_rate_rank_def",
                  "pressure_rate", "pressure_rate_rank", "pressure_rate_rank_def",
                  "pressure_grade", "pressure_gr_rank", "pressure_gr_rank_def",
                  "no_pressure_grade", "no_pressure_gr_rank", "no_pressure_gr_rank_def",
                  "pressure_qbr", "pressure_qbr_rank", "pressure_qbr_rank_def",
                  "no_pressure_qbr", "no_pressure_qbr_rank", "no_pressure_qbr_rank_def",
                  "sack_rate", "sack_rate_rank", "sack_rate_rank_def")
    )
  )
  
  # Helper function to process each category
  process_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func, selected_columns) {
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    return(rbind(
      qb_stats_df_final %>%
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>%
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams) %>%
        dplyr::summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>%
        dplyr::filter(qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE))
    ))
  }
  
  # Process each category with specific columns
  category_results <- lapply(categories, function(cat) {
    process_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, cat$qb_func, cat$def_func, cat$columns)
  })
  
  # Define sheet name
  sheet_name <- substr(paste0("TDs - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
  addWorksheet(wb_tds, sheet_name)
  
  # Write data at specified row positions
  start_rows <- c(1, 6, 11, 16, 21)
  names(category_results) <- names(categories)
  
  for (i in seq_along(category_results)) {
    writeData(wb_tds, sheet = sheet_name, x = data.frame(category_results[[i]]), startRow = start_rows[i])
  }
  
  # Save to S3
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb_tds, tmp)
  put_object(tmp, bucket = "nfl-pff-data-lucas", object = paste0("outputs/", sheet_name, ".xlsx"))
  
  return(paste0("Saved to s3://nfl-pff-data-lucas/outputs/", sheet_name, ".xlsx"))
}

xtds_func("CARYoung-2025", "LA2025")


qb_stats_df_final %>% filter(qbgrp_ssn == "CARYoung-2025") %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank_def, na.rm = T),
                   mn_fgs = mean(fgs, na.rm = T),
                   n = n())

qb_stats_df_final %>% filter(def_ssn == "LA2025") %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank, na.rm = T),
                   mn_fgs = mean(fgs, na.rm = T))


qb_stats_df_final %>% filter(qbgrp_ssn == "JAXLawrence-2025") %>%
  select(week, pbp_xtds, tds, pbp_xtds_rank_def) %>%
  arrange(week)
  
qb_stats_df_final %>% filter(def_ssn == "TEN2025") %>%
  select(week, pbp_xtds, tds, pbp_xtds_rank) %>%
  arrange(week)


####
####
####

comparison_blitz_func("TENWard-2025", 1) # 77
comparison_depth_func("TENWard-2025", .99) # 82
comparison_less_func("TENWard-2025", .97) # 81
comparison_pa_func("TENWard-2025", 1.04) # 66
comparison_pressure_func("TENWard-2025", 1) # 71

comparison_blitz_def_func("JAX2025", .98) # 78
comparison_depth_def_func("JAX2025", 1.05) # 32
comparison_less_def_func("JAX2025", .92) # 113
comparison_pa_def_func("JAX2025", 1.29) # 14
comparison_pressure_def_func("JAX2025", .96) # 85

View(qb_stats_df_final %>% filter(qbgrp_ssn == "TENWard-2025"))
View(qb_stats_df_final %>% filter(def_ssn == "JAX2025"))

xtds_func <- function(qbgrp_one, defgrp_one) {
  wb_tds <- createWorkbook()
  
  # Define categories with correct thresholds and column selections
  categories <- list(
    blitz = list(
      qb_threshold = 1, def_threshold = .98,
      qb_func = comparison_blitz_func, def_func = comparison_blitz_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def", 
                  "pressure_rate", "pressure_rate_rank", "pressure_rate_rank_def", 
                  "blitz_rate", "blitz_rate_rank", "blitz_rate_rank_def", 
                  "blitz_grade", "blitz_gr_rank", "blitz_gr_rank_def", 
                  "no_blitz_grade", "no_blitz_gr_rank", "no_blitz_gr_rank_def", 
                  "blitz_qbr", "blitz_qbr_rank", "blitz_qbr_rank_def", 
                  "no_blitz_qbr", "no_blitz_qbr_rank", "no_blitz_qbr_rank_def", 
                  "sack_rate", "sack_rate_rank", "sack_rate_rank_def")
    ),
    
    depth = list(
      qb_threshold = .99, def_threshold = 1.05,
      qb_func = comparison_depth_func, def_func = comparison_depth_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "behind_los_rate", "behind_los_rate_rank", "behind_los_rate_rank_def",
                  "behind_los_grade", "behind_los_gr_rank", "behind_los_gr_rank_def",
                  "behind_los_qbr", "behind_los_qbr_rank", "behind_los_qbr_rank_def",
                  "short_rate", "short_rate_rank", "short_rate_rank_def",
                  "short_grade", "short_gr_rank", "short_gr_rank_def",
                  "short_qbr", "short_qbr_rank", "short_qbr_rank_def",
                  "medium_rate", "medium_rate_rank", "medium_rate_rank_def",
                  "medium_grade", "medium_gr_rank", "medium_gr_rank_def",
                  "medium_qbr", "medium_qbr_rank", "medium_qbr_rank_def",
                  "deep_rate", "deep_rate_rank", "deep_rate_rank_def",
                  "deep_grade", "deep_gr_rank", "deep_gr_rank_def",
                  "deep_qbr", "deep_qbr_rank", "deep_qbr_rank_def")
    ),
    
    less = list(
      qb_threshold = .97, def_threshold = .92,
      qb_func = comparison_less_func, def_func = comparison_less_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "less_rate", "less_rate_rank", "less_rate_rank_def",
                  "less_grade", "less_gr_rank", "less_gr_rank_def",
                  "more_grade", "more_gr_rank", "more_gr_rank_def",
                  "less_qbr", "less_qbr_rank", "less_qbr_rank_def",
                  "more_qbr", "more_qbr_rank", "more_qbr_rank_def")
    ),
    
    pa = list(
      qb_threshold = 1.04, def_threshold = 1.29,
      qb_func = comparison_pa_func, def_func = comparison_pa_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "pressure_rate", "pressure_rate_rank", "pressure_rate_rank_def",
                  "pa_rate", "pa_rate_rank", "pa_rate_rank_def",
                  "pa_grade", "pa_gr_rank", "pa_gr_rank_def",
                  "npa_grade", "npa_gr_rank", "npa_gr_rank_def",
                  "pa_qbr", "pa_qbr_rank", "pa_qbr_rank_def",
                  "npa_qbr", "npa_qbr_rank", "npa_qbr_rank_def",
                  "sack_rate", "sack_rate_rank", "sack_rate_rank_def")
    ),
    
    pressure = list(
      qb_threshold = 1, def_threshold = .96,
      qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "acc_rate", "acc_rate_rank", "acc_rate_rank_def",
                  "pressure_rate", "pressure_rate_rank", "pressure_rate_rank_def",
                  "pressure_grade", "pressure_gr_rank", "pressure_gr_rank_def",
                  "no_pressure_grade", "no_pressure_gr_rank", "no_pressure_gr_rank_def",
                  "pressure_qbr", "pressure_qbr_rank", "pressure_qbr_rank_def",
                  "no_pressure_qbr", "no_pressure_qbr_rank", "no_pressure_qbr_rank_def",
                  "sack_rate", "sack_rate_rank", "sack_rate_rank_def")
    )
  )
  
  # Helper function to process each category
  process_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func, selected_columns) {
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    return(rbind(
      qb_stats_df_final %>%
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>%
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams) %>%
        dplyr::summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>%
        dplyr::filter(qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE))
    ))
  }
  
  # Process each category with specific columns
  category_results <- lapply(categories, function(cat) {
    process_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, cat$qb_func, cat$def_func, cat$columns)
  })
  
  # Define sheet name
  sheet_name <- substr(paste0("TDs - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
  addWorksheet(wb_tds, sheet_name)
  
  # Write data at specified row positions
  start_rows <- c(1, 6, 11, 16, 21)
  names(category_results) <- names(categories)
  
  for (i in seq_along(category_results)) {
    writeData(wb_tds, sheet = sheet_name, x = data.frame(category_results[[i]]), startRow = start_rows[i])
  }
  
  # Save to S3
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb_tds, tmp)
  put_object(tmp, bucket = "nfl-pff-data-lucas", object = paste0("outputs/", sheet_name, ".xlsx"))
  
  return(paste0("Saved to s3://nfl-pff-data-lucas/outputs/", sheet_name, ".xlsx"))
}

xtds_func("TENWard-2025", "JAX2025")


qb_stats_df_final %>% filter(qbgrp_ssn == "TENWard-2025") %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank_def, na.rm = T),
                   mn_fgs = mean(fgs, na.rm = T))

qb_stats_df_final %>% filter(def_ssn == "JAX2025") %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank, na.rm = T),
                   mn_fgs = mean(fgs, na.rm = T))


qb_stats_df_final %>% filter(qbgrp_ssn == "TENWard-2025") %>%
  select(week, pbp_xtds, tds, pbp_xtds_rank_def) %>%
  arrange(week)

qb_stats_df_final %>% filter(def_ssn == "JAX2025") %>%
  select(week, pbp_xtds, tds, pbp_xtds_rank) %>%
  arrange(week)


####
####
####


comparison_blitz_func("JAXLawrence-2025", 1) # 78
comparison_depth_func("TBMayfield-2025", 1.05) # 23
comparison_less_func("TBMayfield-2025", .9) # 94
comparison_pa_func("TBMayfield-2025", .92) # 68
comparison_pressure_func("TBMayfield-2025", .92) # 65

comparison_blitz_def_func("CAR2025", .91) # 94
comparison_depth_def_func("CAR2025", 1.04) # 29
comparison_less_def_func("CAR2025", .96) # 56
comparison_pa_def_func("CAR2025", 1) # 35
comparison_pressure_def_func("CAR2025", .99) # 45


xtds_func <- function(qbgrp_one, defgrp_one) {
  wb_tds <- createWorkbook()
  
  # Define categories with correct thresholds and column selections
  categories <- list(
    blitz = list(
      qb_threshold = .92, def_threshold = .91,
      qb_func = comparison_blitz_func, def_func = comparison_blitz_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def", 
                  "pressure_rate", "pressure_rate_rank", "pressure_rate_rank_def", 
                  "blitz_rate", "blitz_rate_rank", "blitz_rate_rank_def", 
                  "blitz_grade", "blitz_gr_rank", "blitz_gr_rank_def", 
                  "no_blitz_grade", "no_blitz_gr_rank", "no_blitz_gr_rank_def", 
                  "blitz_qbr", "blitz_qbr_rank", "blitz_qbr_rank_def", 
                  "no_blitz_qbr", "no_blitz_qbr_rank", "no_blitz_qbr_rank_def", 
                  "sack_rate", "sack_rate_rank", "sack_rate_rank_def")
    ),
    
    depth = list(
      qb_threshold = 1.05, def_threshold = 1.04,
      qb_func = comparison_depth_func, def_func = comparison_depth_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "behind_los_rate", "behind_los_rate_rank", "behind_los_rate_rank_def",
                  "behind_los_grade", "behind_los_gr_rank", "behind_los_gr_rank_def",
                  "behind_los_qbr", "behind_los_qbr_rank", "behind_los_qbr_rank_def",
                  "short_rate", "short_rate_rank", "short_rate_rank_def",
                  "short_grade", "short_gr_rank", "short_gr_rank_def",
                  "short_qbr", "short_qbr_rank", "short_qbr_rank_def",
                  "medium_rate", "medium_rate_rank", "medium_rate_rank_def",
                  "medium_grade", "medium_gr_rank", "medium_gr_rank_def",
                  "medium_qbr", "medium_qbr_rank", "medium_qbr_rank_def",
                  "deep_rate", "deep_rate_rank", "deep_rate_rank_def",
                  "deep_grade", "deep_gr_rank", "deep_gr_rank_def",
                  "deep_qbr", "deep_qbr_rank", "deep_qbr_rank_def")
    ),
    
    less = list(
      qb_threshold = .9, def_threshold = .96,
      qb_func = comparison_less_func, def_func = comparison_less_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "less_rate", "less_rate_rank", "less_rate_rank_def",
                  "less_grade", "less_gr_rank", "less_gr_rank_def",
                  "more_grade", "more_gr_rank", "more_gr_rank_def",
                  "less_qbr", "less_qbr_rank", "less_qbr_rank_def",
                  "more_qbr", "more_qbr_rank", "more_qbr_rank_def")
    ),
    
    pa = list(
      qb_threshold = .92, def_threshold = 1,
      qb_func = comparison_pa_func, def_func = comparison_pa_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "pressure_rate", "pressure_rate_rank", "pressure_rate_rank_def",
                  "pa_rate", "pa_rate_rank", "pa_rate_rank_def",
                  "pa_grade", "pa_gr_rank", "pa_gr_rank_def",
                  "npa_grade", "npa_gr_rank", "npa_gr_rank_def",
                  "pa_qbr", "pa_qbr_rank", "pa_qbr_rank_def",
                  "npa_qbr", "npa_qbr_rank", "npa_qbr_rank_def",
                  "sack_rate", "sack_rate_rank", "sack_rate_rank_def")
    ),
    
    pressure = list(
      qb_threshold = .92, def_threshold = .99,
      qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "acc_rate", "acc_rate_rank", "acc_rate_rank_def",
                  "pressure_rate", "pressure_rate_rank", "pressure_rate_rank_def",
                  "pressure_grade", "pressure_gr_rank", "pressure_gr_rank_def",
                  "no_pressure_grade", "no_pressure_gr_rank", "no_pressure_gr_rank_def",
                  "pressure_qbr", "pressure_qbr_rank", "pressure_qbr_rank_def",
                  "no_pressure_qbr", "no_pressure_qbr_rank", "no_pressure_qbr_rank_def",
                  "sack_rate", "sack_rate_rank", "sack_rate_rank_def")
    )
  )
  
  # Helper function to process each category
  process_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func, selected_columns) {
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    return(rbind(
      qb_stats_df_final %>%
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>%
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams) %>%
        dplyr::summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>%
        dplyr::filter(qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE))
    ))
  }
  
  # Process each category with specific columns
  category_results <- lapply(categories, function(cat) {
    process_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, cat$qb_func, cat$def_func, cat$columns)
  })
  
  # Define sheet name
  sheet_name <- substr(paste0("TDs - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
  addWorksheet(wb_tds, sheet_name)
  
  # Write data at specified row positions
  start_rows <- c(1, 6, 11, 16, 21)
  names(category_results) <- names(categories)
  
  for (i in seq_along(category_results)) {
    writeData(wb_tds, sheet = sheet_name, x = data.frame(category_results[[i]]), startRow = start_rows[i])
  }
  
  # Save to S3
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb_tds, tmp)
  put_object(tmp, bucket = "nfl-pff-data-lucas", object = paste0("outputs/", sheet_name, ".xlsx"))
  
  return(paste0("Saved to s3://nfl-pff-data-lucas/outputs/", sheet_name, ".xlsx"))
}

xtds_func("TBMayfield-2025", "CAR2025")

View(qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2025"))
View(qb_stats_df_final %>% filter(def_ssn == "CAR2025"))

qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2025") %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank_def, na.rm = T),
                   mn_fgs = mean(fgs, na.rm = T))

qb_stats_df_final %>% filter(def_ssn == "CAR2025") %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank, na.rm = T),
                   mn_fgs = mean(fgs, na.rm = T))


qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2025") %>%
  select(week, pbp_xtds, tds, pbp_xtds_rank_def) %>%
  arrange(week)

qb_stats_df_final %>% filter(def_ssn == "CAR2025") %>%
  select(week, pbp_xtds, tds, pbp_xtds_rank) %>%
  arrange(week)


####
####


comparison_blitz_func("TBMayfield-2024", .93) # 111
comparison_depth_func("TBMayfield-2024", 1.095) # 21
comparison_less_func("TBMayfield-2024", .96) # 93
comparison_pa_func("TBMayfield-2024", 1) # 69
comparison_pressure_func("TBMayfield-2024", 1.01) # 53

comparison_blitz_def_func("CAR2025", .89) # 94
comparison_depth_def_func("CAR2025", 1.045) # 29
comparison_less_def_func("CAR2025", .96) # 56
comparison_pa_def_func("CAR2025", 1.01) # 35
comparison_pressure_def_func("CAR2025", 1) # 45

View(qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2024"))

xtds_func <- function(qbgrp_one, defgrp_one) {
  wb_tds <- createWorkbook()
  
  # Define categories with correct thresholds and column selections
  categories <- list(
    blitz = list(
      qb_threshold = .93, def_threshold = .89,
      qb_func = comparison_blitz_func, def_func = comparison_blitz_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def", 
                  "pressure_rate", "pressure_rate_rank", "pressure_rate_rank_def", 
                  "blitz_rate", "blitz_rate_rank", "blitz_rate_rank_def", 
                  "blitz_grade", "blitz_gr_rank", "blitz_gr_rank_def", 
                  "no_blitz_grade", "no_blitz_gr_rank", "no_blitz_gr_rank_def", 
                  "blitz_qbr", "blitz_qbr_rank", "blitz_qbr_rank_def", 
                  "no_blitz_qbr", "no_blitz_qbr_rank", "no_blitz_qbr_rank_def", 
                  "sack_rate", "sack_rate_rank", "sack_rate_rank_def")
    ),
    
    depth = list(
      qb_threshold = 1.095, def_threshold = 1.045,
      qb_func = comparison_depth_func, def_func = comparison_depth_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "behind_los_rate", "behind_los_rate_rank", "behind_los_rate_rank_def",
                  "behind_los_grade", "behind_los_gr_rank", "behind_los_gr_rank_def",
                  "behind_los_qbr", "behind_los_qbr_rank", "behind_los_qbr_rank_def",
                  "short_rate", "short_rate_rank", "short_rate_rank_def",
                  "short_grade", "short_gr_rank", "short_gr_rank_def",
                  "short_qbr", "short_qbr_rank", "short_qbr_rank_def",
                  "medium_rate", "medium_rate_rank", "medium_rate_rank_def",
                  "medium_grade", "medium_gr_rank", "medium_gr_rank_def",
                  "medium_qbr", "medium_qbr_rank", "medium_qbr_rank_def",
                  "deep_rate", "deep_rate_rank", "deep_rate_rank_def",
                  "deep_grade", "deep_gr_rank", "deep_gr_rank_def",
                  "deep_qbr", "deep_qbr_rank", "deep_qbr_rank_def")
    ),
    
    less = list(
      qb_threshold = .96, def_threshold = .96,
      qb_func = comparison_less_func, def_func = comparison_less_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "less_rate", "less_rate_rank", "less_rate_rank_def",
                  "less_grade", "less_gr_rank", "less_gr_rank_def",
                  "more_grade", "more_gr_rank", "more_gr_rank_def",
                  "less_qbr", "less_qbr_rank", "less_qbr_rank_def",
                  "more_qbr", "more_qbr_rank", "more_qbr_rank_def")
    ),
    
    pa = list(
      qb_threshold = 1, def_threshold = 1.01,
      qb_func = comparison_pa_func, def_func = comparison_pa_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "pressure_rate", "pressure_rate_rank", "pressure_rate_rank_def",
                  "pa_rate", "pa_rate_rank", "pa_rate_rank_def",
                  "pa_grade", "pa_gr_rank", "pa_gr_rank_def",
                  "npa_grade", "npa_gr_rank", "npa_gr_rank_def",
                  "pa_qbr", "pa_qbr_rank", "pa_qbr_rank_def",
                  "npa_qbr", "npa_qbr_rank", "npa_qbr_rank_def",
                  "sack_rate", "sack_rate_rank", "sack_rate_rank_def")
    ),
    
    pressure = list(
      qb_threshold = 1.01, def_threshold = 1,
      qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def",
                  "ypa", "pbp_xypa", "part_xypa", 
                  "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", 
                  "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def",
                  "acc_rate", "acc_rate_rank", "acc_rate_rank_def",
                  "pressure_rate", "pressure_rate_rank", "pressure_rate_rank_def",
                  "pressure_grade", "pressure_gr_rank", "pressure_gr_rank_def",
                  "no_pressure_grade", "no_pressure_gr_rank", "no_pressure_gr_rank_def",
                  "pressure_qbr", "pressure_qbr_rank", "pressure_qbr_rank_def",
                  "no_pressure_qbr", "no_pressure_qbr_rank", "no_pressure_qbr_rank_def",
                  "sack_rate", "sack_rate_rank", "sack_rate_rank_def")
    )
  )
  
  # Helper function to process each category
  process_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func, selected_columns) {
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    return(rbind(
      qb_stats_df_final %>%
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>%
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams) %>%
        dplyr::summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>%
        dplyr::filter(qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE))
    ))
  }
  
  # Process each category with specific columns
  category_results <- lapply(categories, function(cat) {
    process_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, cat$qb_func, cat$def_func, cat$columns)
  })
  
  # Define sheet name
  sheet_name <- substr(paste0("TDs - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
  addWorksheet(wb_tds, sheet_name)
  
  # Write data at specified row positions
  start_rows <- c(1, 6, 11, 16, 21)
  names(category_results) <- names(categories)
  
  for (i in seq_along(category_results)) {
    writeData(wb_tds, sheet = sheet_name, x = data.frame(category_results[[i]]), startRow = start_rows[i])
  }
  
  # Save to S3
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb_tds, tmp)
  put_object(tmp, bucket = "nfl-pff-data-lucas", object = paste0("outputs/", sheet_name, ".xlsx"))
  
  return(paste0("Saved to s3://nfl-pff-data-lucas/outputs/", sheet_name, ".xlsx"))
}

xtds_func("TBMayfield-2024", "CAR2025")


qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2024") %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank_def, na.rm = T),
                   mn_fgs = mean(fgs, na.rm = T))

qb_stats_df_final %>% filter(def_ssn == "CAR2025") %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank, na.rm = T),
                   mn_fgs = mean(fgs, na.rm = T))


qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2024") %>%
  select(week, pbp_xtds, tds, pbp_xtds_rank_def) %>%
  arrange(week)

qb_stats_df_final %>% filter(def_ssn == "CAR2025") %>%
  select(week, pbp_xtds, tds, pbp_xtds_rank) %>%
  arrange(week)




####
####


qb_stats_df_final %>% filter(posteam == "HST", season == 2024) %>%
  group_by(qbgrp_ssn) %>%
  dplyr::summarise(mn_tds = mean(tds, na.rm = T),
                   mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
                   mn_pbp_xtd_rank = mean(pbp_xtds_rank_def, na.rm = T),
                   n = n())


####
####


qb_summary <- qb_stats_df_final %>%
  group_by(qbgrp_ssn) %>%
  dplyr::summarise(
    mn_tds = mean(tds, na.rm = T),
    mn_pbp_xtds = mean(pbp_xtds, na.rm = T),
    mn_pbp_xtd_rank = mean(pbp_xtds_rank_def, na.rm = T),
    mn_fgs = mean(fgs, na.rm = T),
    n = n()
  ) %>%
  filter(n >= 5)

fit <- lm(mn_pbp_xtds ~ mn_pbp_xtd_rank, data = qb_summary)
coefs <- coef(fit)
r2 <- summary(fit)$r.squared

eq_label <- paste0("y = ", round(coefs[1], 2), " + ", round(coefs[2], 2), "x\nR² = ", round(r2, 3))

ggplot(qb_summary, aes(x = mn_pbp_xtd_rank, y = mn_pbp_xtds)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = coefs[1], slope = coefs[2], color = "red", linetype = "dashed") +
  geom_text(aes(label = qbgrp_ssn), size = 2, vjust = -0.5, check_overlap = TRUE) +
  annotate("text", x = 0.8, y = max(qb_summary$mn_pbp_xtds) - 0.2, 
           label = eq_label, hjust = 1, size = 4) +
  labs(
    x = "Mean Defensive Rank Faced (higher = easier)",
    y = "Mean Expected TDs (pbp_xtds)",
    title = "QB Expected TDs vs Schedule Difficulty"
  ) +
  theme_minimal()