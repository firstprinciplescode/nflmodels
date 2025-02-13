library(nflreadr)
library(nflfastR)
library(sqldf)
library(openxlsx)
library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(readxl)
library(flexclust)
library(factoextra)
library(NbClust)
library(xgboost)
library(caret)
library(caTools)
library(Metrics)
library(cvms)
library(ParBayesianOptimization)
library(doParallel)
library(stringr)
library(pROC)
library(olsrr)
library(stats)
library(ROCR)
library(gridExtra)
library(mgcv)


options(scipen = 999)

setwd("C:/Users/vflre/Downloads/NFL Models")

'%ni%' <- Negate('%in%')


comparison_blitz_func <- function(qbgrp_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_blitz_off$Feature
  
  # Filter the relevant columns in df_blitz_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_blitz_scaled_z %>%
    filter(qbgrp_ssn == qbgrp_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance
  
  all_others <- df_blitz_scaled_z %>%
    filter(qbgrp_ssn != qbgrp_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for comparison
  
  # Preserve the team identifiers
  all_others_tms <- df_blitz_scaled_z %>% filter(qbgrp_ssn != qbgrp_ssn2) %>% pull(qbgrp_ssn)
  
  # Calculate the absolute difference for each team (row-wise difference)
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  # Convert Gain to numeric weights
  weights <- importance_matrix_blitz_off$Gain
  
  # Calculate weighted column sums (each column weighted and summed across rows)
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")
  
  # Calculate weighted row sums for each team
  result_def <- rowSums(weighted_diff)
  
  # Convert to DataFrame
  result_def <- data.frame(Team = all_others_tms, defScore = result_def)
  
  
  # Apply year filter if specified
  if (!is.null(year)) {
    result_def <- result_def %>%
      filter(substr(Team, nchar(Team) - 3, nchar(Team)) == as.character(year))
  }
  
  # Apply threshold filter
  result_def <- result_def %>% filter(defScore <= threshold) %>% arrange(defScore)
  
  return(result_def)
}
comparison_blitz_def_func <- comparison_blitz_def_func <- function(def_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_blitz_def$Feature
  
  # Filter the relevant columns in df_blitz_def_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_blitz_def_scaled_z %>%
    filter(def_ssn == def_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance
  
  all_others <- df_blitz_def_scaled_z %>%
    filter(def_ssn != def_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for comblitzrison
  
  # Preserve the team identifiers
  all_others_tms <- df_blitz_def_scaled_z %>% filter(def_ssn != def_ssn2) %>% pull(def_ssn)
  
  # Calculate the absolute difference for each team (row-wise difference)
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  # Convert Gain to numeric weights
  weights <- importance_matrix_blitz_def$Gain
  
  # Calculate weighted column sums (each column weighted and summed across rows)
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")
  
  # Calculate weighted row sums for each team
  result_def <- rowSums(weighted_diff)
  
  # Convert to DataFrame
  result_def <- data.frame(Team = all_others_tms, defScore = result_def)
  
  
  # Apply year filter if specified
  if (!is.null(year)) {
    result_def <- result_def %>%
      filter(substr(Team, nchar(Team) - 3, nchar(Team)) == as.character(year))
  }
  
  # Apply threshold filter
  result_def <- result_def %>% filter(defScore <= threshold) %>% arrange(defScore)
  
  return(result_def)
}

comparison_depth_func <- function(qbgrp_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_depth_off$Feature

  # Filter the relevant columns in df_depth_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_depth_scaled_z %>%
    filter(qbgrp_ssn == qbgrp_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance

  all_others <- df_depth_scaled_z %>%
    filter(qbgrp_ssn != qbgrp_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for comparison

  # Preserve the team identifiers
  all_others_tms <- df_depth_scaled_z %>% filter(qbgrp_ssn != qbgrp_ssn2) %>% pull(qbgrp_ssn)

  # Calculate the absolute difference for each team (row-wise difference)
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))

  # Convert Gain to numeric weights
  weights <- importance_matrix_depth_off$Gain

  # Calculate weighted column sums (each column weighted and summed across rows)
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")

  # Calculate weighted row sums for each team
  result_def <- rowSums(weighted_diff)

  # Convert to DataFrame
  result_def <- data.frame(Team = all_others_tms, defScore = result_def)


  # Apply year filter if specified
  if (!is.null(year)) {
    result_def <- result_def %>%
      filter(substr(Team, nchar(Team) - 3, nchar(Team)) == as.character(year))
  }

  # Apply threshold filter
  result_def <- result_def %>% filter(defScore <= threshold) %>% arrange(defScore)

  return(result_def)
}
comparison_depth_def_func <- function(def_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_depth_def$Feature
  
  # Filter the relevant columns in df_depth_def_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_depth_def_scaled_z %>%
    filter(def_ssn == def_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance
  
  all_others <- df_depth_def_scaled_z %>%
    filter(def_ssn != def_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for comparison
  
  # Preserve the team identifiers
  all_others_tms <- df_depth_def_scaled_z %>% filter(def_ssn != def_ssn2) %>% pull(def_ssn)
  
  # Calculate the absolute difference for each team (row-wise difference)
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  # Convert Gain to numeric weights
  weights <- importance_matrix_depth_def$Gain
  
  # Calculate weighted column sums (each column weighted and summed across rows)
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")
  
  # Calculate weighted row sums for each team
  result_def <- rowSums(weighted_diff)
  
  # Convert to DataFrame
  result_def <- data.frame(Team = all_others_tms, defScore = result_def)
  
  
  # Apply year filter if specified
  if (!is.null(year)) {
    result_def <- result_def %>%
      filter(substr(Team, nchar(Team) - 3, nchar(Team)) == as.character(year))
  }
  
  # Apply threshold filter
  result_def <- result_def %>% filter(defScore <= threshold) %>% arrange(defScore)
  
  return(result_def)
}

comparison_less_func <- function(qbgrp_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_less_off$Feature
  
  # Filter the relevant columns in df_less_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_less_scaled_z %>%
    filter(qbgrp_ssn == qbgrp_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance
  
  all_others <- df_less_scaled_z %>%
    filter(qbgrp_ssn != qbgrp_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for comparison
  
  # Preserve the team identifiers
  all_others_tms <- df_less_scaled_z %>% filter(qbgrp_ssn != qbgrp_ssn2) %>% pull(qbgrp_ssn)
  
  # Calculate the absolute difference for each team (row-wise difference)
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  # Convert Gain to numeric weights
  weights <- importance_matrix_less_off$Gain
  
  # Calculate weighted column sums (each column weighted and summed across rows)
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")
  
  # Calculate weighted row sums for each team
  result_def <- rowSums(weighted_diff)
  
  # Convert to DataFrame
  result_def <- data.frame(Team = all_others_tms, defScore = result_def)
  
  
  # Apply year filter if specified
  if (!is.null(year)) {
    result_def <- result_def %>%
      filter(substr(Team, nchar(Team) - 3, nchar(Team)) == as.character(year))
  }
  
  # Apply threshold filter
  result_def <- result_def %>% filter(defScore <= threshold) %>% arrange(defScore)
  
  return(result_def)
}
comparison_less_def_func <- function(def_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_less_def$Feature
  
  # Filter the relevant columns in df_less_def_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_less_def_scaled_z %>%
    filter(def_ssn == def_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance
  
  all_others <- df_less_def_scaled_z %>%
    filter(def_ssn != def_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for comparison
  
  # Preserve the team identifiers
  all_others_tms <- df_less_def_scaled_z %>% filter(def_ssn != def_ssn2) %>% pull(def_ssn)
  
  # Calculate the absolute difference for each team (row-wise difference)
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  # Convert Gain to numeric weights
  weights <- importance_matrix_less_def$Gain
  
  # Calculate weighted column sums (each column weighted and summed across rows)
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")
  
  # Calculate weighted row sums for each team
  result_def <- rowSums(weighted_diff)
  
  # Convert to DataFrame
  result_def <- data.frame(Team = all_others_tms, defScore = result_def)
  
  
  # Apply year filter if specified
  if (!is.null(year)) {
    result_def <- result_def %>%
      filter(substr(Team, nchar(Team) - 3, nchar(Team)) == as.character(year))
  }
  
  # Apply threshold filter
  result_def <- result_def %>% filter(defScore <= threshold) %>% arrange(defScore)
  
  return(result_def)
}

comparison_pressure_func <- function(qbgrp_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_press_off$Feature
  
  # Filter the relevant columns in df_pressure_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_pressure_scaled_z %>%
    filter(qbgrp_ssn == qbgrp_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance
  
  all_others <- df_pressure_scaled_z %>%
    filter(qbgrp_ssn != qbgrp_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for comparison
  
  # Preserve the team identifiers
  all_others_tms <- df_pressure_scaled_z %>% filter(qbgrp_ssn != qbgrp_ssn2) %>% pull(qbgrp_ssn)
  
  # Calculate the absolute difference for each team (row-wise difference)
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  # Convert Gain to numeric weights
  weights <- importance_matrix_press_off$Gain
  
  # Calculate weighted column sums (each column weighted and summed across rows)
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")
  
  # Calculate weighted row sums for each team
  result_def <- rowSums(weighted_diff)
  
  # Convert to DataFrame
  result_def <- data.frame(Team = all_others_tms, defScore = result_def)
  
  
  # Apply year filter if specified
  if (!is.null(year)) {
    result_def <- result_def %>%
      filter(substr(Team, nchar(Team) - 3, nchar(Team)) == as.character(year))
  }
  
  # Apply threshold filter
  result_def <- result_def %>% filter(defScore <= threshold) %>% arrange(defScore)
  
  return(result_def)
}
comparison_pressure_def_func <- function(def_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_press_def$Feature
  
  # Filter the relevant columns in df_pressure_def_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_pressure_def_scaled_z %>%
    filter(def_ssn == def_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance
  
  all_others <- df_pressure_def_scaled_z %>%
    filter(def_ssn != def_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for compressurerison
  
  # Preserve the team identifiers
  all_others_tms <- df_pressure_def_scaled_z %>% filter(def_ssn != def_ssn2) %>% pull(def_ssn)
  
  # Calculate the absolute difference for each team (row-wise difference)
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  # Convert Gain to numeric weights
  weights <- importance_matrix_press_def$Gain
  
  # Calculate weighted column sums (each column weighted and summed across rows)
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")
  
  # Calculate weighted row sums for each team
  result_def <- rowSums(weighted_diff)
  
  # Convert to DataFrame
  result_def <- data.frame(Team = all_others_tms, defScore = result_def)
  
  
  # Apply year filter if specified
  if (!is.null(year)) {
    result_def <- result_def %>%
      filter(substr(Team, nchar(Team) - 3, nchar(Team)) == as.character(year))
  }
  
  # Apply threshold filter
  result_def <- result_def %>% filter(defScore <= threshold) %>% arrange(defScore)
  
  return(result_def)
}

comparison_pa_func <- function(qbgrp_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_pa_off$Feature
  
  # Filter the relevant columns in df_pa_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_pa_scaled_z %>%
    filter(qbgrp_ssn == qbgrp_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance
  
  all_others <- df_pa_scaled_z %>%
    filter(qbgrp_ssn != qbgrp_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for comparison
  
  # Preserve the team identifiers
  all_others_tms <- df_pa_scaled_z %>% filter(qbgrp_ssn != qbgrp_ssn2) %>% pull(qbgrp_ssn)
  
  # Calculate the absolute difference for each team (row-wise difference)
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  # Convert Gain to numeric weights
  weights <- importance_matrix_pa_off$Gain
  
  # Calculate weighted column sums (each column weighted and summed across rows)
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")
  
  # Calculate weighted row sums for each team
  result_def <- rowSums(weighted_diff)
  
  # Convert to DataFrame
  result_def <- data.frame(Team = all_others_tms, defScore = result_def)
  
  
  # Apply year filter if specified
  if (!is.null(year)) {
    result_def <- result_def %>%
      filter(substr(Team, nchar(Team) - 3, nchar(Team)) == as.character(year))
  }
  
  # Apply threshold filter
  result_def <- result_def %>% filter(defScore <= threshold) %>% arrange(defScore)
  
  return(result_def)
}
comparison_pa_def_func <- function(def_ssn2, threshold, year = NULL) {
  # Extract the relevant columns from importance_matrix_press_off and order them by importance
  relevant_features <- importance_matrix_pa_def$Feature
  
  # Filter the relevant columns in df_pa_def_scaled_z and ensure they are ordered as in importance_matrix_press_off
  relevant_team <- df_pa_def_scaled_z %>%
    filter(def_ssn == def_ssn2) %>%  # Corrected to match exactly
    select(all_of(relevant_features))  # Selecting and ordering columns by importance
  
  all_others <- df_pa_def_scaled_z %>%
    filter(def_ssn != def_ssn2) %>%  # Ensure the def_ssn logic is correct here
    select(all_of(relevant_features))  # Ensuring same order for comparison
  
  # Preserve the team identifiers
  all_others_tms <- df_pa_def_scaled_z %>% filter(def_ssn != def_ssn2) %>% pull(def_ssn)
  
  # Calculate the absolute difference for each team (row-wise difference)
  all_others_diff <- abs(sweep(as.matrix(all_others), 2, as.matrix(relevant_team), "-"))
  
  # Convert Gain to numeric weights
  weights <- importance_matrix_pa_def$Gain
  
  # Calculate weighted column sums (each column weighted and summed across rows)
  weighted_diff <- sweep(all_others_diff, 2, weights, "*")
  
  # Calculate weighted row sums for each team
  result_def <- rowSums(weighted_diff)
  
  # Convert to DataFrame
  result_def <- data.frame(Team = all_others_tms, defScore = result_def)
  
  
  # Apply year filter if specified
  if (!is.null(year)) {
    result_def <- result_def %>%
      filter(substr(Team, nchar(Team) - 3, nchar(Team)) == as.character(year))
  }
  
  # Apply threshold filter
  result_def <- result_def %>% filter(defScore <= threshold) %>% arrange(defScore)
  
  return(result_def)
}


attach("dependencies_blitz.RData")

importance_matrix_blitz_off <- dependencies_blitz[[1]]
df_blitz_scaled_z <- dependencies_blitz[[2]]

attach("dependencies_blitz_def.RData")

importance_matrix_blitz_def <- dependencies_blitz_def[[1]]
df_blitz_def_scaled_z <- dependencies_blitz_def[[2]]

attach("dependencies_depth.RData")

importance_matrix_depth_off <- list_dependencies_depth[[2]]
df_depth_scaled_z <- list_dependencies_depth[[1]]

attach("dependencies_depth_def.RData")

importance_matrix_depth_def <- list_dependencies_depth[[2]]
df_depth_def_scaled_z <- list_dependencies_depth[[1]]

attach("list_dependencies_less.RData")

importance_matrix_less_off <- list_dependencies_less[[1]]
df_less_scaled_z <- list_dependencies_less[[2]]

attach("dependencies_less_def.RData")

importance_matrix_less_def <- list_dependencies_less_def[[2]]
df_less_def_scaled_z <- list_dependencies_less_def[[1]]

attach("dependencies_pa.RData")

importance_matrix_pa_off <- list_dependencies_pa[[2]]
df_pa_scaled_z <- list_dependencies_pa[[1]]

attach("dependencies_pa_def.RData")

importance_matrix_pa_def <- list_dependencies_pa_def[[2]]
df_pa_def_scaled_z <- list_dependencies_pa_def[[1]]

attach("dependencies_pressure.RData")

importance_matrix_press_off <- dependencies_pressure[[1]]
df_pressure_scaled_z <- dependencies_pressure[[2]]

attach("dependencies_pressure_def.RData")

importance_matrix_press_def <- dependencies_pressure_def[[2]]
df_pressure_def_scaled_z <- dependencies_pressure_def[[1]]


comparison_blitz_func("PHIHurts-2024", .785)
comparison_blitz_def_func("KC2024", .86)

comparison_depth_func("PHIHurts-2024", .76)
comparison_depth_def_func("KC2024", .845)

comparison_less_func("PHIHurts-2024", .71)
comparison_less_def_func("KC2024", .8)

comparison_pa_func("PHIHurts-2024", .755)
comparison_pa_def_func("KC2024", .85)

comparison_pressure_func("PHIHurts-2024", .835)
comparison_pressure_def_func("KC2024", .88)

####
####

comparison_blitz_func("KCMahomes-2024", .715)
comparison_blitz_def_func("PHI2024", .815)

comparison_depth_func("KCMahomes-2024", .755)
comparison_depth_def_func("PHI2024", .895)

comparison_less_func("KCMahomes-2024", .715)
comparison_less_def_func("PHI2024", .825)

comparison_pa_func("KCMahomes-2024", .7)
comparison_pa_def_func("PHI2024", .82)

comparison_pressure_func("KCMahomes-2024", .72)
comparison_pressure_def_func("PHI2024", .85)

####
####

attach("NEW_qb_stats_df_build_workspace.RData")

qb_stats_df_final <- qb_stats_df_final

detach("file:NEW_qb_stats_df_build_workspace.RData")


  
####
####

View(qb_stats_df_final %>% filter(qbgrp_ssn == "KCMahomes-2024"))
View(qb_stats_df_final %>% filter(def_ssn == "PHI2024"))


##
##

xtds_func <- function(qbgrp_one, defgrp_one) {
  wb_tds <- createWorkbook()
  
  # Define categories with correct thresholds and column selections
  categories <- list(
    blitz = list(
      qb_threshold = .715, def_threshold = .815,
      qb_func = comparison_blitz_func, def_func = comparison_blitz_def_func,
      columns = c("tds", "fgs", "pbp_xtds", "part_xtds", 
                  "pbp_xtds_rank", "part_xtds_rank", "pbp_xtds_rank_def", "part_xtds_rank_def","ypa", "pbp_xypa", "part_xypa", "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", "ypa_rank_def", "pbp_xypa_rank_def", "part_xypa_rank_def", "pressure_rate", "pressure_rate_rank", "pressure_rate_rank_def", "blitz_rate", "blitz_rate_rank", "blitz_rate_rank_def", "blitz_grade", "blitz_gr_rank", "blitz_gr_rank_def", "no_blitz_grade", "no_blitz_gr_rank", "no_blitz_gr_rank_def", "blitz_qbr", "blitz_qbr_rank", "blitz_qbr_rank_def", "no_blitz_qbr", "no_blitz_qbr_rank", "no_blitz_qbr_rank_def", "sack_rate", "sack_rate_rank", "sack_rate_rank_def")
    ),
    
    depth = list(
      qb_threshold = .755, def_threshold = .895,
      qb_func = comparison_depth_func, def_func = comparison_depth_def_func,
      columns = c(
        "tds", "fgs", "pbp_xtds", "part_xtds", 
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
        "deep_qbr", "deep_qbr_rank", "deep_qbr_rank_def"
      )
    ),
    
    less = list(
      qb_threshold = .715, def_threshold = .825,
      qb_func = comparison_less_func, def_func = comparison_less_def_func,
      columns = c(
        "tds", "fgs", "pbp_xtds", "part_xtds", 
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
      qb_threshold = .7, def_threshold = .82,
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
      qb_threshold = .72, def_threshold = .85,
      qb_func = comparison_pressure_func, def_func = comparison_pressure_def_func,
      columns = c( "tds", "fgs", "pbp_xtds", "part_xtds", 
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
    qb_teams <- c(qb_func(qbgrp_one, qb_threshold)$Team, qbgrp_one)
    def_teams <- c(def_func(defgrp_one, def_threshold)$Team, defgrp_one)
    
    return(rbind(
      qb_stats_df_final %>%
        filter(qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams) %>%
        summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>%
        filter(qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams) %>%
        summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE)),
      
      qb_stats_df_final %>%
        filter(qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams) %>%
        summarise(n = n(), across(all_of(selected_columns), mean, na.rm = TRUE))
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
  
  # Save workbook
  saveWorkbook(wb_tds, file.path("C:/Users/vflre/Downloads/NFL Models", paste0(sheet_name, ".xlsx")))
}

xtds_func("KCMahomes-2024", "PHI2024")
