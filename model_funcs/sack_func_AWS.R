library(aws.s3)

bucket <- "nfl-pff-data-lucas"

# Option 1: Specify what to KEEP, remove everything else
keep_objects <- c("qb_stats_df_final", "con", "bucket")
rm(list = setdiff(ls(), keep_objects))

'%ni%' <- Negate('%in%')

conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise, dplyr::mutate)


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


sack_rate_func <- function(qbgrp_one, defgrp_one) {
  wb_tds <- createWorkbook()
  
  # Define categories with ORIGINAL thresholds
  categories <- list(
    blitz    = list(qb_threshold = 1.035, def_threshold = .975,  qb_func = comparison_blitz_func,    def_func = comparison_blitz_def_func),
    depth    = list(qb_threshold = 1.025, def_threshold = .975,  qb_func = comparison_depth_func,    def_func = comparison_depth_def_func),
    less     = list(qb_threshold = .96,   def_threshold = .92,  qb_func = comparison_less_func,     def_func = comparison_less_def_func),
    pa       = list(qb_threshold = 1.025,   def_threshold = .945,  qb_func = comparison_pa_func,       def_func = comparison_pa_def_func),
    pressure = list(qb_threshold = .98, def_threshold = .95, qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func)
  )
  
  # Helper function to process each category
  process_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func) {
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    return(rbind(
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(sack_rate, pbp_sack_rate, part_sack_rate, sack_rate_rank, sack_rate_rank_def, pbp_sack_rate_rank, pbp_sack_rate_rank_def, part_sack_rate_rank, part_sack_rate_rank_def, pressure_rate, pbp_pressure, part_pressure_before, pressure_rate_rank, pressure_rate_rank_def, pbp_pressure_rank, pbp_pressure_rank_def, part_pressure_before_rank, part_pressure_before_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams) %>%
        dplyr::summarise(n = n(), across(c(sack_rate, pbp_sack_rate, part_sack_rate, sack_rate_rank, sack_rate_rank_def, pbp_sack_rate_rank, pbp_sack_rate_rank_def, part_sack_rate_rank, part_sack_rate_rank_def, pressure_rate, pbp_pressure, part_pressure_before, pressure_rate_rank, pressure_rate_rank_def, pbp_pressure_rank, pbp_pressure_rank_def, part_pressure_before_rank, part_pressure_before_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(sack_rate, pbp_sack_rate, part_sack_rate, sack_rate_rank, sack_rate_rank_def, pbp_sack_rate_rank, pbp_sack_rate_rank_def, part_sack_rate_rank, part_sack_rate_rank_def, pressure_rate, pbp_pressure, part_pressure_before, pressure_rate_rank, pressure_rate_rank_def, pbp_pressure_rank, pbp_pressure_rank_def, part_pressure_before_rank, part_pressure_before_rank_def), mean, na.rm = TRUE))
    ))
  }
  
  # Process each category
  category_results <- lapply(categories, function(cat) {
    process_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, cat$qb_func, cat$def_func)
  })
  
  # Define sheet name
  sheet_name <- substr(paste0("SackRt - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
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

sack_rate_func("CARYoung-2025", "LA2025")

weather_sack_rate_func <- function(qbgrp_one, defgrp_one) {
  
  wb <- createWorkbook()
  
  categories <- list(
    blitz    = list(qb_threshold = 1.035, def_threshold = .975,  qb_func = comparison_blitz_func,    def_func = comparison_blitz_def_func),
    depth    = list(qb_threshold = 1.025, def_threshold = .975,  qb_func = comparison_depth_func,    def_func = comparison_depth_def_func),
    less     = list(qb_threshold = .96,   def_threshold = .92,   qb_func = comparison_less_func,     def_func = comparison_less_def_func),
    pa       = list(qb_threshold = 1.025, def_threshold = .945,  qb_func = comparison_pa_func,       def_func = comparison_pa_def_func),
    pressure = list(qb_threshold = .98,   def_threshold = .95,   qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func)
  )
  
  selected_columns <- c("sack_rate", "pbp_sack_rate", "part_sack_rate", 
                        "sack_rate_rank", "pbp_sack_rate_rank", "part_sack_rate_rank",
                        "pressure_rate", "pbp_pressure", "part_pressure_before", 
                        "pressure_rate_rank", "pbp_pressure_rank", "part_pressure_before_rank")
  
  process_weather_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func) {
    
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    comp_weather_df <- qb_stats_df_final %>%
      mutate(
        wind_na = ifelse(is.na(wind), 0, wind),
        weather = case_when(
          wind_na >= 10 & temp <= 50 ~ "1_Both",
          wind_na >= 10 & temp > 50 ~ "2_Wind",
          wind_na < 10 & temp <= 50 ~ "3_Cold",
          TRUE ~ "4_Good"
        ),
        comp_bucket = case_when(
          qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams ~ "1_Both",
          qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams ~ "2_Off_only",
          qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams ~ "3_Def_only",
          TRUE ~ "Neither"
        )
      ) %>%
      filter(comp_bucket != "Neither")
    
    result <- comp_weather_df %>%
      pivot_longer(cols = all_of(selected_columns), names_to = "metric", values_to = "value") %>%
      group_by(comp_bucket, metric) %>%
      summarise(
        n_1_Both = sum(weather == "1_Both"),
        n_2_Wind = sum(weather == "2_Wind"),
        n_3_Cold = sum(weather == "3_Cold"),
        n_4_Good = sum(weather == "4_Good"),
        n_5_All = n(),
        
        mean_1_Both = mean(value[weather == "1_Both"], na.rm = T),
        mean_2_Wind = mean(value[weather == "2_Wind"], na.rm = T),
        mean_3_Cold = mean(value[weather == "3_Cold"], na.rm = T),
        mean_4_Good = mean(value[weather == "4_Good"], na.rm = T),
        mean_5_All = mean(value, na.rm = T),
        
        overall_sd = sd(value, na.rm = T),
        
        anova_pval = tryCatch({
          aov_fit <- aov(value ~ weather)
          summary(aov_fit)[[1]][["Pr(>F)"]][1]
        }, error = function(e) NA),
        
        eta_sq = tryCatch({
          aov_fit <- aov(value ~ weather)
          ss <- summary(aov_fit)[[1]][["Sum Sq"]]
          ss[1] / sum(ss)
        }, error = function(e) NA),
        
        .groups = "drop"
      ) %>%
      mutate(
        diff_Both_vs_All = mean_1_Both - mean_5_All,
        diff_Wind_vs_All = mean_2_Wind - mean_5_All,
        diff_Cold_vs_All = mean_3_Cold - mean_5_All,
        diff_Good_vs_All = mean_4_Good - mean_5_All,
        
        d_Both_vs_All = diff_Both_vs_All / overall_sd,
        d_Wind_vs_All = diff_Wind_vs_All / overall_sd,
        d_Cold_vs_All = diff_Cold_vs_All / overall_sd,
        d_Good_vs_All = diff_Good_vs_All / overall_sd,
        
        anova_sig = case_when(
          anova_pval < 0.01 ~ "***",
          anova_pval < 0.05 ~ "**",
          anova_pval < 0.10 ~ "*",
          TRUE ~ ""
        ),
        effect_size = case_when(
          eta_sq >= 0.14 ~ "large",
          eta_sq >= 0.06 ~ "medium",
          eta_sq >= 0.01 ~ "small",
          TRUE ~ "negligible"
        )
      ) %>%
      arrange(comp_bucket, metric)
    
    return(result)
  }
  
  category_results <- lapply(categories, function(cat) {
    process_weather_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, 
                             cat$qb_func, cat$def_func)
  })
  
  for (cat_name in names(category_results)) {
    addWorksheet(wb, cat_name)
    writeData(wb, sheet = cat_name, x = data.frame(category_results[[cat_name]]))
  }
  
  sheet_name <- substr(paste0("WeatherSack - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb, tmp)
  put_object(tmp, bucket = bucket, object = paste0("outputs/", sheet_name, ".xlsx"))
  
  return(list(
    file = paste0("Saved to s3://nfl-pff-data-lucas/outputs/", sheet_name, ".xlsx"),
    results = category_results
  ))
}

weather_sack_rate_func("CARYoung-2025", "LA2025")

precip_sack_rate_func <- function(qbgrp_one, defgrp_one) {
  
  wb <- createWorkbook()
  
  categories <- list(
    blitz    = list(qb_threshold = 1.035, def_threshold = .975,  qb_func = comparison_blitz_func,    def_func = comparison_blitz_def_func),
    depth    = list(qb_threshold = 1.025, def_threshold = .975,  qb_func = comparison_depth_func,    def_func = comparison_depth_def_func),
    less     = list(qb_threshold = .96,   def_threshold = .92,   qb_func = comparison_less_func,     def_func = comparison_less_def_func),
    pa       = list(qb_threshold = 1.025, def_threshold = .945,  qb_func = comparison_pa_func,       def_func = comparison_pa_def_func),
    pressure = list(qb_threshold = .98,   def_threshold = .95,   qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func)
  )
  
  selected_columns <- c("sack_rate", "pbp_sack_rate", "part_sack_rate", 
                        "sack_rate_rank", "pbp_sack_rate_rank", "part_sack_rate_rank",
                        "pressure_rate", "pbp_pressure", "part_pressure_before", 
                        "pressure_rate_rank", "pbp_pressure_rank", "part_pressure_before_rank")
  
  process_precip_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func) {
    
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    comp_precip_df <- qb_stats_df_final %>%
      mutate(
        precip = case_when(
          snow_ind == 1 ~ "1_Snow",
          rain_ind == 1 ~ "2_Rain",
          TRUE ~ "3_Clear"
        ),
        comp_bucket = case_when(
          qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams ~ "1_Both",
          qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams ~ "2_Off_only",
          qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams ~ "3_Def_only",
          TRUE ~ "Neither"
        )
      ) %>%
      filter(comp_bucket != "Neither")
    
    result <- comp_precip_df %>%
      pivot_longer(cols = all_of(selected_columns), names_to = "metric", values_to = "value") %>%
      group_by(comp_bucket, metric) %>%
      summarise(
        n_1_Snow = sum(precip == "1_Snow"),
        n_2_Rain = sum(precip == "2_Rain"),
        n_3_Clear = sum(precip == "3_Clear"),
        n_4_All = n(),
        
        mean_1_Snow = mean(value[precip == "1_Snow"], na.rm = T),
        mean_2_Rain = mean(value[precip == "2_Rain"], na.rm = T),
        mean_3_Clear = mean(value[precip == "3_Clear"], na.rm = T),
        mean_4_All = mean(value, na.rm = T),
        
        overall_sd = sd(value, na.rm = T),
        
        anova_pval = tryCatch({
          aov_fit <- aov(value ~ precip)
          summary(aov_fit)[[1]][["Pr(>F)"]][1]
        }, error = function(e) NA),
        
        eta_sq = tryCatch({
          aov_fit <- aov(value ~ precip)
          ss <- summary(aov_fit)[[1]][["Sum Sq"]]
          ss[1] / sum(ss)
        }, error = function(e) NA),
        
        .groups = "drop"
      ) %>%
      mutate(
        diff_Snow_vs_All = mean_1_Snow - mean_4_All,
        diff_Rain_vs_All = mean_2_Rain - mean_4_All,
        diff_Clear_vs_All = mean_3_Clear - mean_4_All,
        
        d_Snow_vs_All = diff_Snow_vs_All / overall_sd,
        d_Rain_vs_All = diff_Rain_vs_All / overall_sd,
        d_Clear_vs_All = diff_Clear_vs_All / overall_sd,
        
        diff_Snow_vs_Clear = mean_1_Snow - mean_3_Clear,
        diff_Rain_vs_Clear = mean_2_Rain - mean_3_Clear,
        d_Snow_vs_Clear = diff_Snow_vs_Clear / overall_sd,
        d_Rain_vs_Clear = diff_Rain_vs_Clear / overall_sd,
        
        anova_sig = case_when(
          anova_pval < 0.01 ~ "***",
          anova_pval < 0.05 ~ "**",
          anova_pval < 0.10 ~ "*",
          TRUE ~ ""
        ),
        effect_size = case_when(
          eta_sq >= 0.14 ~ "large",
          eta_sq >= 0.06 ~ "medium",
          eta_sq >= 0.01 ~ "small",
          TRUE ~ "negligible"
        )
      ) %>%
      arrange(comp_bucket, metric)
    
    return(result)
  }
  
  category_results <- lapply(categories, function(cat) {
    process_precip_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, 
                            cat$qb_func, cat$def_func)
  })
  
  for (cat_name in names(category_results)) {
    addWorksheet(wb, cat_name)
    writeData(wb, sheet = cat_name, x = data.frame(category_results[[cat_name]]))
  }
  
  sheet_name <- substr(paste0("PrecipSack - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
  tmp <- tempfile(fileext = ".xlsx")
  saveWorkbook(wb, tmp)
  put_object(tmp, bucket = bucket, object = paste0("outputs/", sheet_name, ".xlsx"))
  
  return(list(
    file = paste0("Saved to s3://nfl-pff-data-lucas/outputs/", sheet_name, ".xlsx"),
    results = category_results
  ))
}

precip_sack_rate_func("CARYoung-2025", "LA2025")


qb_stats_df_final %>% filter(qbgrp_ssn == "CARYoung-2025") %>%
  dplyr::summarise(
    sack_rate = mean(sack_rate, na.rm = T),
    pbp_sack_rate = mean(pbp_sack_rate, na.rm = T),
    part_sack_rate = mean(part_sack_rate, na.rm = T),
    pressure_rate = mean(pressure_rate, na.rm = T),
    pbp_pressure_before_rate = mean(pbp_pressure, na.rm = T)
  )

qb_stats_df_final %>% filter(def_ssn == "LA2025") %>%
  dplyr::summarise(
    sack_rate = mean(sack_rate, na.rm = T),
    pbp_sack_rate = mean(pbp_sack_rate, na.rm = T),
    part_sack_rate = mean(part_sack_rate, na.rm = T),
    pressure_rate = mean(pressure_rate, na.rm = T),
    pbp_pressure_before_rate = mean(pbp_pressure, na.rm = T)
  )

qb_stats_df_final %>% filter(qbgrp_ssn == "JAXLawrence-2025") %>%
  select(week, sack_rate, sack_rate_rank_def, pbp_sack_rate, pbp_sack_rate_rank_def) %>%
  arrange(week)

qb_stats_df_final %>% filter(def_ssn == "TEN2025") %>%
  select(week, sack_rate, sack_rate_rank, pbp_sack_rate, pbp_sack_rate_rank) %>%
  arrange(week)


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


sack_rate_func <- function(qbgrp_one, defgrp_one) {
  wb_tds <- createWorkbook()
  
  # Define categories with ORIGINAL thresholds
  categories <- list(
    blitz    = list(qb_threshold = 1, def_threshold = .98,  qb_func = comparison_blitz_func,    def_func = comparison_blitz_def_func),
    depth    = list(qb_threshold = .99, def_threshold = 1.05,  qb_func = comparison_depth_func,    def_func = comparison_depth_def_func),
    less     = list(qb_threshold = .97,   def_threshold = .92,  qb_func = comparison_less_func,     def_func = comparison_less_def_func),
    pa       = list(qb_threshold = 1.04,   def_threshold = 1.29,  qb_func = comparison_pa_func,       def_func = comparison_pa_def_func),
    pressure = list(qb_threshold = 1, def_threshold = .96, qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func)
  )
  
  # Helper function to process each category
  process_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func) {
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    return(rbind(
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(sack_rate, pbp_sack_rate, part_sack_rate, sack_rate_rank, sack_rate_rank_def, pbp_sack_rate_rank, pbp_sack_rate_rank_def, part_sack_rate_rank, part_sack_rate_rank_def, pressure_rate, pbp_pressure, part_pressure_before, pressure_rate_rank, pressure_rate_rank_def, pbp_pressure_rank, pbp_pressure_rank_def, part_pressure_before_rank, part_pressure_before_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams) %>%
        dplyr::summarise(n = n(), across(c(sack_rate, pbp_sack_rate, part_sack_rate, sack_rate_rank, sack_rate_rank_def, pbp_sack_rate_rank, pbp_sack_rate_rank_def, part_sack_rate_rank, part_sack_rate_rank_def, pressure_rate, pbp_pressure, part_pressure_before, pressure_rate_rank, pressure_rate_rank_def, pbp_pressure_rank, pbp_pressure_rank_def, part_pressure_before_rank, part_pressure_before_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(sack_rate, pbp_sack_rate, part_sack_rate, sack_rate_rank, sack_rate_rank_def, pbp_sack_rate_rank, pbp_sack_rate_rank_def, part_sack_rate_rank, part_sack_rate_rank_def, pressure_rate, pbp_pressure, part_pressure_before, pressure_rate_rank, pressure_rate_rank_def, pbp_pressure_rank, pbp_pressure_rank_def, part_pressure_before_rank, part_pressure_before_rank_def), mean, na.rm = TRUE))
    ))
  }
  
  # Process each category
  category_results <- lapply(categories, function(cat) {
    process_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, cat$qb_func, cat$def_func)
  })
  
  # Define sheet name
  sheet_name <- substr(paste0("SackRt - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
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

sack_rate_func("TENWard-2025", "JAX2025")


qb_stats_df_final %>% filter(qbgrp_ssn == "TENWard-2025") %>%
  dplyr::summarise(
    sack_rate = mean(sack_rate, na.rm = T),
    pbp_sack_rate = mean(pbp_sack_rate, na.rm = T),
    part_sack_rate = mean(part_sack_rate, na.rm = T),
    pressure_rate = mean(pressure_rate, na.rm = T),
    pbp_pressure_before_rate = mean(pbp_pressure, na.rm = T)
  )

qb_stats_df_final %>% filter(def_ssn == "JAX2025") %>%
  dplyr::summarise(
    sack_rate = mean(sack_rate, na.rm = T),
    pbp_sack_rate = mean(pbp_sack_rate, na.rm = T),
    part_sack_rate = mean(part_sack_rate, na.rm = T),
    pressure_rate = mean(pressure_rate, na.rm = T),
    pbp_pressure_before_rate = mean(pbp_pressure, na.rm = T)
  )

qb_stats_df_final %>% filter(qbgrp_ssn == "TENWard-2025") %>%
  select(week, sack_rate, sack_rate_rank_def, pbp_sack_rate, pbp_sack_rate_rank_def) %>%
  arrange(week)

qb_stats_df_final %>% filter(def_ssn == "JAX2025") %>%
  select(week, sack_rate, sack_rate_rank, pbp_sack_rate, pbp_sack_rate_rank) %>%
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


sack_rate_func <- function(qbgrp_one, defgrp_one) {
  wb_tds <- createWorkbook()
  
  # Define categories with ORIGINAL thresholds
  categories <- list(
    blitz    = list(qb_threshold = .92, def_threshold = .91,  qb_func = comparison_blitz_func,    def_func = comparison_blitz_def_func),
    depth    = list(qb_threshold = 1.05, def_threshold = 1.04,  qb_func = comparison_depth_func,    def_func = comparison_depth_def_func),
    less     = list(qb_threshold = .9,   def_threshold = .96,  qb_func = comparison_less_func,     def_func = comparison_less_def_func),
    pa       = list(qb_threshold = .92,   def_threshold = 1,  qb_func = comparison_pa_func,       def_func = comparison_pa_def_func),
    pressure = list(qb_threshold = .92, def_threshold = .99, qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func)
  )
  
  # Helper function to process each category
  process_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func) {
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    return(rbind(
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(sack_rate, pbp_sack_rate, part_sack_rate, sack_rate_rank, sack_rate_rank_def, pbp_sack_rate_rank, pbp_sack_rate_rank_def, part_sack_rate_rank, part_sack_rate_rank_def, pressure_rate, pbp_pressure, part_pressure_before, pressure_rate_rank, pressure_rate_rank_def, pbp_pressure_rank, pbp_pressure_rank_def, part_pressure_before_rank, part_pressure_before_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams) %>%
        dplyr::summarise(n = n(), across(c(sack_rate, pbp_sack_rate, part_sack_rate, sack_rate_rank, sack_rate_rank_def, pbp_sack_rate_rank, pbp_sack_rate_rank_def, part_sack_rate_rank, part_sack_rate_rank_def, pressure_rate, pbp_pressure, part_pressure_before, pressure_rate_rank, pressure_rate_rank_def, pbp_pressure_rank, pbp_pressure_rank_def, part_pressure_before_rank, part_pressure_before_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(sack_rate, pbp_sack_rate, part_sack_rate, sack_rate_rank, sack_rate_rank_def, pbp_sack_rate_rank, pbp_sack_rate_rank_def, part_sack_rate_rank, part_sack_rate_rank_def, pressure_rate, pbp_pressure, part_pressure_before, pressure_rate_rank, pressure_rate_rank_def, pbp_pressure_rank, pbp_pressure_rank_def, part_pressure_before_rank, part_pressure_before_rank_def), mean, na.rm = TRUE))
    ))
  }
  
  # Process each category
  category_results <- lapply(categories, function(cat) {
    process_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, cat$qb_func, cat$def_func)
  })
  
  # Define sheet name
  sheet_name <- substr(paste0("SackRt - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
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

sack_rate_func("TBMayfield-2025", "CAR2025")


qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2025") %>%
  dplyr::summarise(
    sack_rate = mean(sack_rate, na.rm = T),
    pbp_sack_rate = mean(pbp_sack_rate, na.rm = T),
    part_sack_rate = mean(part_sack_rate, na.rm = T),
    pressure_rate = mean(pressure_rate, na.rm = T),
    pbp_pressure_before_rate = mean(pbp_pressure, na.rm = T)
  )

qb_stats_df_final %>% filter(def_ssn == "CAR2025") %>%
  dplyr::summarise(
    sack_rate = mean(sack_rate, na.rm = T),
    pbp_sack_rate = mean(pbp_sack_rate, na.rm = T),
    part_sack_rate = mean(part_sack_rate, na.rm = T),
    pressure_rate = mean(pressure_rate, na.rm = T),
    pbp_pressure_before_rate = mean(pbp_pressure, na.rm = T)
  )

qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2025") %>%
  select(week, sack_rate, sack_rate_rank_def, pbp_sack_rate, pbp_sack_rate_rank_def) %>%
  arrange(week)

qb_stats_df_final %>% filter(def_ssn == "CAR2025") %>%
  select(week, sack_rate, sack_rate_rank, pbp_sack_rate, pbp_sack_rate_rank) %>%
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


sack_rate_func <- function(qbgrp_one, defgrp_one) {
  wb_tds <- createWorkbook()
  
  # Define categories with ORIGINAL thresholds
  categories <- list(
    blitz    = list(qb_threshold = .93, def_threshold = .89,  qb_func = comparison_blitz_func,    def_func = comparison_blitz_def_func),
    depth    = list(qb_threshold = 1.095, def_threshold = 1.045,  qb_func = comparison_depth_func,    def_func = comparison_depth_def_func),
    less     = list(qb_threshold = .96,   def_threshold = .96,  qb_func = comparison_less_func,     def_func = comparison_less_def_func),
    pa       = list(qb_threshold = 1,   def_threshold = 1.01,  qb_func = comparison_pa_func,       def_func = comparison_pa_def_func),
    pressure = list(qb_threshold = 1.01, def_threshold = 1, qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func)
  )
  
  # Helper function to process each category
  process_category <- function(qbgrp_one, defgrp_one, qb_threshold, def_threshold, qb_func, def_func) {
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$QB, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$QB, defgrp_one)
    
    return(rbind(
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(sack_rate, pbp_sack_rate, part_sack_rate, sack_rate_rank, sack_rate_rank_def, pbp_sack_rate_rank, pbp_sack_rate_rank_def, part_sack_rate_rank, part_sack_rate_rank_def, pressure_rate, pbp_pressure, part_pressure_before, pressure_rate_rank, pressure_rate_rank_def, pbp_pressure_rank, pbp_pressure_rank_def, part_pressure_before_rank, part_pressure_before_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams) %>%
        dplyr::summarise(n = n(), across(c(sack_rate, pbp_sack_rate, part_sack_rate, sack_rate_rank, sack_rate_rank_def, pbp_sack_rate_rank, pbp_sack_rate_rank_def, part_sack_rate_rank, part_sack_rate_rank_def, pressure_rate, pbp_pressure, part_pressure_before, pressure_rate_rank, pressure_rate_rank_def, pbp_pressure_rank, pbp_pressure_rank_def, part_pressure_before_rank, part_pressure_before_rank_def), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>% 
        dplyr::filter(qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams) %>%
        dplyr::summarise(n = n(), across(c(sack_rate, pbp_sack_rate, part_sack_rate, sack_rate_rank, sack_rate_rank_def, pbp_sack_rate_rank, pbp_sack_rate_rank_def, part_sack_rate_rank, part_sack_rate_rank_def, pressure_rate, pbp_pressure, part_pressure_before, pressure_rate_rank, pressure_rate_rank_def, pbp_pressure_rank, pbp_pressure_rank_def, part_pressure_before_rank, part_pressure_before_rank_def), mean, na.rm = TRUE))
    ))
  }
  
  # Process each category
  category_results <- lapply(categories, function(cat) {
    process_category(qbgrp_one, defgrp_one, cat$qb_threshold, cat$def_threshold, cat$qb_func, cat$def_func)
  })
  
  # Define sheet name
  sheet_name <- substr(paste0("SackRt - ", qbgrp_one, " vs ", defgrp_one), 1, 31)
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

sack_rate_func("TBMayfield-2024", "CAR2025")


qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2024") %>%
  dplyr::summarise(
    sack_rate = mean(sack_rate, na.rm = T),
    pbp_sack_rate = mean(pbp_sack_rate, na.rm = T),
    part_sack_rate = mean(part_sack_rate, na.rm = T),
    pressure_rate = mean(pressure_rate, na.rm = T),
    pbp_pressure_before_rate = mean(pbp_pressure, na.rm = T)
  )

qb_stats_df_final %>% filter(def_ssn == "CAR2025") %>%
  dplyr::summarise(
    sack_rate = mean(sack_rate, na.rm = T),
    pbp_sack_rate = mean(pbp_sack_rate, na.rm = T),
    part_sack_rate = mean(part_sack_rate, na.rm = T),
    pressure_rate = mean(pressure_rate, na.rm = T),
    pbp_pressure_before_rate = mean(pbp_pressure, na.rm = T)
  )

qb_stats_df_final %>% filter(qbgrp_ssn == "TBMayfield-2024") %>%
  select(week, sack_rate, sack_rate_rank_def, pbp_sack_rate, pbp_sack_rate_rank_def) %>%
  arrange(week)

qb_stats_df_final %>% filter(def_ssn == "CAR2025") %>%
  select(week, sack_rate, sack_rate_rank, pbp_sack_rate, pbp_sack_rate_rank) %>%
  arrange(week)




