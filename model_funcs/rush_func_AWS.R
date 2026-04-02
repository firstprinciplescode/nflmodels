

rush_stats_final
# SO HERE ... I ACTUALLY ... KIND OF WANT GAP / ZONE SHIT
# ALSO LOT MORE NAs THAN I'D EXPECT 

conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise, dplyr::mutate)

rm(pbp_rush)
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


### A

# 1: LOW AF
# 2: BELLCOW 
# 3: MID
# 4: LONG YARDAGE ONLY

### B

# 1: BELLCOW
# 2: LONG YARDAGE
# 3: EARLY DOWN
# 4: MID
# 5: SHORT YARDAGE 

### C

# 1: MID
# 2: SHORT YARDAGE
# 3: MOSTLY EARLY DOWN
# 4: MOSTLY SHORT YARDAGE
# 5: LONG YARDAGE
# 6: BELLCOW


####
#### GAP
####

# 1 - GUARD
# 2 - GUARD
# 3 - OUTSIDE
# 4 - CENTER / TACKLE
# 5 - MID
# 6 - CENTER / OUTSIDE
# 7 - CENTER
# 8 - GUARD / TACKLE

###
### B
###


#######################
# RUN GAP CLUSTERING - RANK B
#######################


# 1 - TACKLE
# 2 - GUARD
# 3 - OUTSIDE
# 4 - CENTER


#######################
# RUN GAP CLUSTERING - RANK C
#######################

# 1 - OUTSIDE
# 2 - GUARD
# 3 - TACKLE
# 4 - CENTER


rush_stats_final %>%
  filter(week == 16)

View(rush_stats_final %>% filter(qbgrp_ssn %in% c("TENWard-2025")))

rush_stats_final %>%
  filter(player_id == 27126) %>%
  select(qbgrp_ssn, rank_grp, situation_cluster, gap_cluster, xtd_percentile, gap_z) %>%
  arrange(qbgrp_ssn) %>%
  distinct()


rush_stats_final <- 
  left_join(rush_stats_final,
            qb_stats_df_final %>% select(qbgrp_ssn, def_ssn, week, season, temp, wind, rain_ind, snow_ind),
            by = c("qbgrp_ssn", "def_ssn", "week", "season"))


comparison_blitz_func("JAXLawrence-2025", .96) # 99
comparison_depth_func("JAXLawrence-2025", 1.11) # 23
comparison_less_func("JAXLawrence-2025", 1.01) # 69
comparison_pa_func("JAXLawrence-2025", 1) # 75
comparison_pressure_func("JAXLawrence-2025", 1.05) # 55


comparison_blitz_def_func("DEN2025", 1.09) # 29
comparison_depth_def_func("DEN2025", 1.15) # 16
comparison_less_def_func("DEN2025", 1.12) # 19
comparison_pressure_def_func("DEN2025", 1.16) # 11
comparison_pa_def_func("DEN2025", 1.11) # 38

rush_func <- function(qbgrp_one, defgrp_one, rank_grp_input, situation_input, 
                      gap_input, gap_z_range, gap_z_NA = FALSE, 
                      xtd_vec_input, td_na = FALSE,
                      position_group_input, player_name) {
  
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
    
    rush_stats_final %>%
      ungroup() %>%
      dplyr::filter(qbgrp_ssn %in% qb_teams) %>%
      mutate(
        ind = ifelse(def_ssn %in% def_teams, "In", "Out"),
        rush_ind = ifelse(
          rank_grp %in% rank_grp_input & 
            position_group %in% position_group_input &
            situation_cluster %in% situation_input &
            gap_cluster %in% gap_input & 
            (
              (gap_z >= gap_z_range[1] & gap_z <= gap_z_range[2]) |
                (gap_z_NA & is.na(gap_z))
            ) &
            (
              (xtd_percentile >= xtd_vec_input[1] & xtd_percentile <= xtd_vec_input[2]) |
                (td_na & is.na(xtd_percentile))
            ),
          "In", "Out")
      ) %>%
      mutate(has_rel_rushers = any(rush_ind == "In")) %>%
      dplyr::filter(has_rel_rushers) %>%
      group_by(qbgrp_ssn, def_ssn, week, season, ind) %>%
      dplyr::filter(sum(ifelse(rush_ind == "In", 1, 0), na.rm = TRUE) > 0) %>%
      dplyr::summarise(
        rel_players = sum(ifelse(rush_ind == "In", 1, 0), na.rm = TRUE),
        notrel_players = sum(ifelse(rush_ind != "In", 1, 0), na.rm = TRUE),
        rel_rushes = sum(ifelse(rush_ind == "In", attempts, 0), na.rm = TRUE),
        notrel_rushes = sum(ifelse(rush_ind != "In", attempts, 0), na.rm = TRUE),
        rel_pbp_xtds = sum(ifelse(rush_ind == "In", pbp_xtd, 0), na.rm = TRUE),
        notrel_pbp_xtds = sum(ifelse(rush_ind != "In", pbp_xtd, 0), na.rm = TRUE),
        rel_part_xtds = sum(ifelse(rush_ind == "In", part_xtd, 0), na.rm = TRUE),
        notrel_part_xtds = sum(ifelse(rush_ind != "In", part_xtd, 0), na.rm = TRUE),
        rel_ypc = mean(ifelse(rush_ind == "In", ypc, NA), na.rm = TRUE),
        rel_pbp_ypc = mean(ifelse(rush_ind == "In", pbp_xypc, NA), na.rm = TRUE),
        rel_part_ypc = mean(ifelse(rush_ind == "In", part_xypc, NA), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        rush_shr = (rel_rushes / (rel_rushes + notrel_rushes)) / rel_players,
        pbp_xtd_shr = (rel_pbp_xtds / (rel_pbp_xtds + notrel_pbp_xtds)) / rel_players,
        part_xtd_shr = (rel_part_xtds / (rel_part_xtds + notrel_part_xtds)) / rel_players
      ) %>%
      ungroup() %>%
      group_by(ind) %>%
      dplyr::summarise(
        players = n(),
        rush_shr = mean(rush_shr, na.rm = TRUE),
        pbp_xtd_shr = mean(pbp_xtd_shr, na.rm = TRUE),
        part_xtd_shr = mean(part_xtd_shr, na.rm = TRUE),
        ypc = mean(rel_ypc, na.rm = TRUE),
        pbp_ypc = mean(rel_pbp_ypc, na.rm = TRUE),
        part_ypc = mean(rel_part_ypc, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  category_results <- lapply(categories, function(cat) {
    process_category(cat, qbgrp_one, defgrp_one)
  })
  
  sheet_name <- substr(paste0("Rush - ", player_name), 1, 31)
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

rush_func("TENWard-2025", 
          "JAX2025", 
          rank_grp_input = c("C"), 
          situation_input = c(3,NA), 
          gap_input = c(1,NA), 
          gap_z_range = c(-99,99),
          gap_z_NA = T, 
          xtd_vec_input = c(60,100),
          td_na = T,
          position_group_input = c("REC"), 
          player_name = 'TEN REC')


rush_stats_final %>%
  filter(qbgrp_ssn == "TENWard-2025", 
         (rank_grp == "C" & position_group == "QB" ), rush_proportion >= 0, rush_proportion <= 1) %>%
  mutate(pbp_xtd_ratio = pbp_xtd_share / rush_proportion,
         part_xtd_ratio = part_xtd_share / rush_proportion) %>%
  filter(pbp_xtd_ratio != Inf) %>%
  dplyr::summarise(mn_ypc = mean(ypc),
                   mn_pbp_xypc = mean(pbp_xypc))

::summarise(mn_pbp_xtd = mean(pbp_xtd_ratio))

dplyr::summarise(mn_ypc = mean(ypc),
                 mn_pbp_xypc = mean(pbp_xypc))

rush_stats_final %>%
  filter(def_ssn == "JAX2025", 
         (rank_grp == "C" & position_group == "QB" ), rush_proportion >= 0, rush_proportion <= 1) %>%
  mutate(pbp_xtd_ratio = pbp_xtd_share / rush_proportion,
         part_xtd_ratio = part_xtd_share / rush_proportion) %>%
  filter(pbp_xtd_ratio != Inf) %>%
  dplyr::summarise(mn_ypc = mean(ypc),
                   mn_pbp_xypc = mean(pbp_xypc))

rush_stats_final %>%
  filter(def_ssn == "JAX2025", ((rank_grp == "A" & gap_cluster %in% c(2)) | (rank_grp == "B" & gap_cluster %in% c(2,3)) ), rush_proportion >= 0, rush_proportion <= 1) %>%
  dplyr::summarise(mn_ypc = mean(ypc),
                   mn_pbp_xypc = mean(pbp_xypc))

rush_stats_final %>%
  filter(qbgrp_ssn == "TENWard-2025", player == "Tony Pollard", rush_proportion >= 0, rush_proportion <= 1) %>%
  arrange(pbp_xtd_share ) %>%
  pull(pbp_xtd_share )

# rush_share
# pbp_xtd_share