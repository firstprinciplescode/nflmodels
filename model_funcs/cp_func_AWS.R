library(aws.s3)

bucket <- "nfl-pff-data-lucas"

# Option 1: Specify what to KEEP, remove everything else
keep_objects <- c("qb_stats_df_final", "con", "bucket")
rm(list = setdiff(ls(), keep_objects))

'%ni%' <- Negate('%in%')

conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise)


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


View(qb_stats_df_final %>% filter(qbgrp_ssn == "CARYoung-2024"))
View(qb_stats_df_final %>% filter(def_ssn == "LA2025"))


cp_func <- function(qbgrp_one, defgrp_one) {
  wb_tds <- createWorkbook()
  
  categories <- list(
    blitz    = list(qb_threshold = 1.035, def_threshold = .975,  qb_func = comparison_blitz_func,    def_func = comparison_blitz_def_func),
    depth    = list(qb_threshold = 1.025, def_threshold = .975,  qb_func = comparison_depth_func,    def_func = comparison_depth_def_func),
    less     = list(qb_threshold = .96,   def_threshold = .92,  qb_func = comparison_less_func,     def_func = comparison_less_def_func),
    pa       = list(qb_threshold = 1.025,   def_threshold = .945,  qb_func = comparison_pa_func,       def_func = comparison_pa_def_func),
    pressure = list(qb_threshold = .98, def_threshold = .95, qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func)
  )
  
  process_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func) {
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    return(rbind(
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(acc_rate, fastr_cp, pbp_cp, part_cp,
                                           acc_rate_rank, acc_rate_rank_def,
                                           fastr_cp_rank, fastr_cp_rank_def,
                                           pbp_cp_rank, pbp_cp_rank_def,
                                           part_cp_rank, part_cp_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams) %>%
        dplyr::summarise(n = n(), across(c(acc_rate, fastr_cp, pbp_cp, part_cp,
                                           acc_rate_rank, acc_rate_rank_def,
                                           fastr_cp_rank, fastr_cp_rank_def,
                                           pbp_cp_rank, pbp_cp_rank_def,
                                           part_cp_rank, part_cp_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(acc_rate, fastr_cp, pbp_cp, part_cp,
                                           acc_rate_rank, acc_rate_rank_def,
                                           fastr_cp_rank, fastr_cp_rank_def,
                                           pbp_cp_rank, pbp_cp_rank_def,
                                           part_cp_rank, part_cp_rank_def), mean, na.rm = TRUE))
    ))
  }
  
  category_results <- lapply(categories, function(cat) {
    process_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, cat$qb_func, cat$def_func)
  })
  
  sheet_name <- substr(paste0("CP - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
  addWorksheet(wb_tds, sheet_name)
  
  start_rows <- c(1, 6, 11, 16, 21)
  names(category_results) <- names(categories)
  
  for (i in seq_along(category_results)) {
    writeData(wb_tds, sheet = sheet_name, x = data.frame(category_results[[i]]), startRow = start_rows[i])
  }
  
  # Save to S3
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb_tds, tmp)
  put_object(file = tmp, object = paste0("outputs/", sheet_name, ".xlsx"), bucket = "nfl-pff-data-lucas")
  
  return(paste0("Saved to s3://nfl-pff-data-lucas/outputs/", sheet_name, ".xlsx"))
}

cp_func("CARYoung-2025", "LA2025")


qb_stats_df_final %>% filter(qbgrp_ssn == "CARYoung-2025") %>%
  dplyr::summarise(
    acc_rate = mean(acc_rate, na.rm = T),
    fastr_cp = mean(fastr_cp, na.rm = T),
    pbp_cp = mean(pbp_cp, na.rm = T),
    part_cp = mean(part_cp, na.rm = T)
  )

qb_stats_df_final %>% filter(def_ssn == "LA2025") %>%
  dplyr::summarise(
    acc_rate = mean(acc_rate, na.rm = T),
    fastr_cp = mean(fastr_cp, na.rm = T),
    pbp_cp = mean(pbp_cp, na.rm = T),
    part_cp = mean(part_cp, na.rm = T)
  )


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


cp_func <- function(qbgrp_one, defgrp_one) {
  wb_tds <- createWorkbook()
  
  categories <- list(
    blitz    = list(qb_threshold = 1, def_threshold = .98,  qb_func = comparison_blitz_func,    def_func = comparison_blitz_def_func),
    depth    = list(qb_threshold = .99, def_threshold = 1.05,  qb_func = comparison_depth_func,    def_func = comparison_depth_def_func),
    less     = list(qb_threshold = .97,   def_threshold = .92,  qb_func = comparison_less_func,     def_func = comparison_less_def_func),
    pa       = list(qb_threshold = 1.04,   def_threshold = 1.29,  qb_func = comparison_pa_func,       def_func = comparison_pa_def_func),
    pressure = list(qb_threshold = 1, def_threshold = .96, qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func)
  )
  
  process_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func) {
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    return(rbind(
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(acc_rate, fastr_cp, pbp_cp, part_cp,
                                           acc_rate_rank, acc_rate_rank_def,
                                           fastr_cp_rank, fastr_cp_rank_def,
                                           pbp_cp_rank, pbp_cp_rank_def,
                                           part_cp_rank, part_cp_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams) %>%
        dplyr::summarise(n = n(), across(c(acc_rate, fastr_cp, pbp_cp, part_cp,
                                           acc_rate_rank, acc_rate_rank_def,
                                           fastr_cp_rank, fastr_cp_rank_def,
                                           pbp_cp_rank, pbp_cp_rank_def,
                                           part_cp_rank, part_cp_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(acc_rate, fastr_cp, pbp_cp, part_cp,
                                           acc_rate_rank, acc_rate_rank_def,
                                           fastr_cp_rank, fastr_cp_rank_def,
                                           pbp_cp_rank, pbp_cp_rank_def,
                                           part_cp_rank, part_cp_rank_def), mean, na.rm = TRUE))
    ))
  }
  
  category_results <- lapply(categories, function(cat) {
    process_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, cat$qb_func, cat$def_func)
  })
  
  sheet_name <- substr(paste0("CP - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
  addWorksheet(wb_tds, sheet_name)
  
  start_rows <- c(1, 6, 11, 16, 21)
  names(category_results) <- names(categories)
  
  for (i in seq_along(category_results)) {
    writeData(wb_tds, sheet = sheet_name, x = data.frame(category_results[[i]]), startRow = start_rows[i])
  }
  
  # Save to S3
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb_tds, tmp)
  put_object(file = tmp, object = paste0("outputs/", sheet_name, ".xlsx"), bucket = "nfl-pff-data-lucas")
  
  return(paste0("Saved to s3://nfl-pff-data-lucas/outputs/", sheet_name, ".xlsx"))
}

cp_func("TENWard-2025", "JAX2025")


qb_stats_df_final %>% filter(qbgrp_ssn == "TENWard-2025") %>%
  dplyr::summarise(
    acc_rate = mean(acc_rate, na.rm = T),
    fastr_cp = mean(fastr_cp, na.rm = T),
    pbp_cp = mean(pbp_cp, na.rm = T),
    part_cp = mean(part_cp, na.rm = T)
  )

qb_stats_df_final %>% filter(def_ssn == "JAX2025") %>%
  dplyr::summarise(
    acc_rate = mean(acc_rate, na.rm = T),
    fastr_cp = mean(fastr_cp, na.rm = T),
    pbp_cp = mean(pbp_cp, na.rm = T),
    part_cp = mean(part_cp, na.rm = T)
  )


qb_stats_df_final %>% filter(qbgrp_ssn == "TENWard-2025") %>%
  select(week, acc_rate, acc_rate_rank_def, fastr_cp, fastr_cp_rank_def, pbp_cp, pbp_cp_rank_def) %>%
  arrange(week)

qb_stats_df_final %>% filter(def_ssn == "JAX2025") %>%
  select(week, acc_rate, acc_rate_rank, fastr_cp, fastr_cp_rank, pbp_cp, pbp_cp_rank) %>%
  arrange(week)


####
####


comparison_blitz_func("TBMayfield-2025", .92) # 78
comparison_depth_func("TBMayfield-2025", 1.05) # 23
comparison_less_func("TBMayfield-2025", .9) # 94
comparison_pa_func("TBMayfield-2025", .92) # 68
comparison_pressure_func("TBMayfield-2025", .92) # 65

comparison_blitz_def_func("CAR2025", .91) # 94
comparison_depth_def_func("CAR2025", 1.04) # 29
comparison_less_def_func("CAR2025", .96) # 56
comparison_pa_def_func("CAR2025", 1) # 35
comparison_pressure_def_func("CAR2025", .99) # 45


View(qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2025"))
View(qb_stats_df_final %>% filter(def_ssn == "CAR2025"))


cp_func <- function(qbgrp_one, defgrp_one) {
  wb_tds <- createWorkbook()
  
  categories <- list(
    blitz    = list(qb_threshold = .92, def_threshold = .91,  qb_func = comparison_blitz_func,    def_func = comparison_blitz_def_func),
    depth    = list(qb_threshold = 1.05, def_threshold = 1.04,  qb_func = comparison_depth_func,    def_func = comparison_depth_def_func),
    less     = list(qb_threshold = .9,   def_threshold = .96,  qb_func = comparison_less_func,     def_func = comparison_less_def_func),
    pa       = list(qb_threshold = .92,   def_threshold = 1,  qb_func = comparison_pa_func,       def_func = comparison_pa_def_func),
    pressure = list(qb_threshold = .92, def_threshold = .99, qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func)
  )
  
  process_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func) {
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    return(rbind(
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(acc_rate, fastr_cp, pbp_cp, part_cp,
                                           acc_rate_rank, acc_rate_rank_def,
                                           fastr_cp_rank, fastr_cp_rank_def,
                                           pbp_cp_rank, pbp_cp_rank_def,
                                           part_cp_rank, part_cp_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams) %>%
        dplyr::summarise(n = n(), across(c(acc_rate, fastr_cp, pbp_cp, part_cp,
                                           acc_rate_rank, acc_rate_rank_def,
                                           fastr_cp_rank, fastr_cp_rank_def,
                                           pbp_cp_rank, pbp_cp_rank_def,
                                           part_cp_rank, part_cp_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(acc_rate, fastr_cp, pbp_cp, part_cp,
                                           acc_rate_rank, acc_rate_rank_def,
                                           fastr_cp_rank, fastr_cp_rank_def,
                                           pbp_cp_rank, pbp_cp_rank_def,
                                           part_cp_rank, part_cp_rank_def), mean, na.rm = TRUE))
    ))
  }
  
  category_results <- lapply(categories, function(cat) {
    process_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, cat$qb_func, cat$def_func)
  })
  
  sheet_name <- substr(paste0("CP - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
  addWorksheet(wb_tds, sheet_name)
  
  start_rows <- c(1, 6, 11, 16, 21)
  names(category_results) <- names(categories)
  
  for (i in seq_along(category_results)) {
    writeData(wb_tds, sheet = sheet_name, x = data.frame(category_results[[i]]), startRow = start_rows[i])
  }
  
  # Save to S3
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb_tds, tmp)
  put_object(file = tmp, object = paste0("outputs/", sheet_name, ".xlsx"), bucket = "nfl-pff-data-lucas")
  
  return(paste0("Saved to s3://nfl-pff-data-lucas/outputs/", sheet_name, ".xlsx"))
}

cp_func("TBMayfield-2025", "CAR2025")


qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2025") %>%
  dplyr::summarise(
    acc_rate = mean(acc_rate, na.rm = T),
    fastr_cp = mean(fastr_cp, na.rm = T),
    pbp_cp = mean(pbp_cp, na.rm = T),
    part_cp = mean(part_cp, na.rm = T)
  )

qb_stats_df_final %>% filter(def_ssn == "CAR2025") %>%
  dplyr::summarise(
    acc_rate = mean(acc_rate, na.rm = T),
    fastr_cp = mean(fastr_cp, na.rm = T),
    pbp_cp = mean(pbp_cp, na.rm = T),
    part_cp = mean(part_cp, na.rm = T)
  )


qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2025") %>%
  select(week, acc_rate, acc_rate_rank_def, fastr_cp, fastr_cp_rank_def, pbp_cp, pbp_cp_rank_def) %>%
  arrange(week)

qb_stats_df_final %>% filter(def_ssn == "CAR2025") %>%
  select(week, acc_rate, acc_rate_rank, fastr_cp, fastr_cp_rank, pbp_cp, pbp_cp_rank) %>%
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
View(qb_stats_df_final %>% filter(def_ssn == "CAR2025"))


cp_func <- function(qbgrp_one, defgrp_one) {
  wb_tds <- createWorkbook()
  
  categories <- list(
    blitz    = list(qb_threshold = .93, def_threshold = .89,  qb_func = comparison_blitz_func,    def_func = comparison_blitz_def_func),
    depth    = list(qb_threshold = 1.095, def_threshold = 1.045,  qb_func = comparison_depth_func,    def_func = comparison_depth_def_func),
    less     = list(qb_threshold = .96,   def_threshold = .96,  qb_func = comparison_less_func,     def_func = comparison_less_def_func),
    pa       = list(qb_threshold = 1,   def_threshold = 1.01,  qb_func = comparison_pa_func,       def_func = comparison_pa_def_func),
    pressure = list(qb_threshold = 1.01, def_threshold = 1, qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func)
  )
  
  process_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func) {
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    return(rbind(
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(acc_rate, fastr_cp, pbp_cp, part_cp,
                                           acc_rate_rank, acc_rate_rank_def,
                                           fastr_cp_rank, fastr_cp_rank_def,
                                           pbp_cp_rank, pbp_cp_rank_def,
                                           part_cp_rank, part_cp_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams) %>%
        dplyr::summarise(n = n(), across(c(acc_rate, fastr_cp, pbp_cp, part_cp,
                                           acc_rate_rank, acc_rate_rank_def,
                                           fastr_cp_rank, fastr_cp_rank_def,
                                           pbp_cp_rank, pbp_cp_rank_def,
                                           part_cp_rank, part_cp_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(acc_rate, fastr_cp, pbp_cp, part_cp,
                                           acc_rate_rank, acc_rate_rank_def,
                                           fastr_cp_rank, fastr_cp_rank_def,
                                           pbp_cp_rank, pbp_cp_rank_def,
                                           part_cp_rank, part_cp_rank_def), mean, na.rm = TRUE))
    ))
  }
  
  category_results <- lapply(categories, function(cat) {
    process_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, cat$qb_func, cat$def_func)
  })
  
  sheet_name <- substr(paste0("CP - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
  addWorksheet(wb_tds, sheet_name)
  
  start_rows <- c(1, 6, 11, 16, 21)
  names(category_results) <- names(categories)
  
  for (i in seq_along(category_results)) {
    writeData(wb_tds, sheet = sheet_name, x = data.frame(category_results[[i]]), startRow = start_rows[i])
  }
  
  # Save to S3
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb_tds, tmp)
  put_object(file = tmp, object = paste0("outputs/", sheet_name, ".xlsx"), bucket = "nfl-pff-data-lucas")
  
  return(paste0("Saved to s3://nfl-pff-data-lucas/outputs/", sheet_name, ".xlsx"))
}

cp_func("TBMayfield-2024", "CAR2025")


qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2024") %>%
  dplyr::summarise(
    acc_rate = mean(acc_rate, na.rm = T),
    fastr_cp = mean(fastr_cp, na.rm = T),
    pbp_cp = mean(pbp_cp, na.rm = T),
    part_cp = mean(part_cp, na.rm = T)
  )

qb_stats_df_final %>% filter(def_ssn == "CAR2025") %>%
  dplyr::summarise(
    acc_rate = mean(acc_rate, na.rm = T),
    fastr_cp = mean(fastr_cp, na.rm = T),
    pbp_cp = mean(pbp_cp, na.rm = T),
    part_cp = mean(part_cp, na.rm = T)
  )


qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2024") %>%
  select(week, acc_rate, acc_rate_rank_def, fastr_cp, fastr_cp_rank_def, pbp_cp, pbp_cp_rank_def) %>%
  arrange(week)

qb_stats_df_final %>% filter(def_ssn == "CAR2025") %>%
  select(week, acc_rate, acc_rate_rank, fastr_cp, fastr_cp_rank, pbp_cp, pbp_cp_rank) %>%
  arrange(week)



