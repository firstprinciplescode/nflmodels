

rush_stats_final
# SO HERE ... I ACTUALLY ... KIND OF WANT GAP / ZONE SHIT
# ALSO LOT MORE NAs THAN I'D EXPECT 

conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise, dplyr::mutate)

# rm(pbp_rush)       # commented 2026-09-19: never delete a shared session frame
# rm(combined_pbp)   # commented 2026-09-19: never delete a shared session frame

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

# CLUSTER LABELS (rewritten 2026-09-20). ONE numbering: the build saved 2026-09-20 11:14 --
# cache/situation_cluster_df.rds and cache/rush_stats_final.rds ($situation_cluster). kmeans RENUMBERS on every rebuild
# (the June workspace, the 09-19 workspace and this build all carry different numbers), so after ANY rebuild check the shapes first:
#   situation_cluster_df %>% group_by(rank_grp, cluster) %>% summarise(rows = n(), first_10 = mean(First_10_Ratio, na.rm = TRUE),
#     third_1 = mean(Third_1_Ratio, na.rm = TRUE), third_rest = mean(Third_rest_Ratio, na.rm = TRUE))
# A (the lead in that game, > 50% of the team's rushes)
#   1: LOW AF          ~81 rows, ~48 carries. every xpass quantile low (q50 -.11, q80 +.06); smallest share of 1st-and-10 and of everything long
#   2: MID             ~282 rows, ~111 carries. the biggest group. early-down lead, gives up 3rd-and-3 and longer; narrow xpass range (-.15 .. +.12)
#   3: LONG YARDAGE    ~124 rows, ~69 carries. the long buckets, least 2nd-and-2 / 3rd-and-1; highest xpass (q50 +.02, q80 +.22)
#   4: BELLCOW         ~219 rows, ~124 carries. biggest share of almost every bucket, short yardage and 3rd downs included; widest xpass range (-.20 .. +.20)

### B

# B (15-50% of the team's rushes)
#   1: LATER/LONG      ~106 rows, ~21 carries. the 3rd-down back: about half the team's 3rd-and-4+ carries; xpass far above the rest (q50 +.16)
#   2: BELLCOW MID     ~219 rows, ~39 carries. an even ~30% of every bucket; widest xpass range (-.22 .. +.20)
#   3: SHORT YARDAGE   ~190 rows, ~28 carries. top on 3rd-and-1 / 4th-and-1, bottom on everything long; every xpass quantile negative (q50 -.14)
#   4: EARLY DOWN      ~376 rows, ~46 carries. the biggest group. 1st-and-10 and 2nd-and-up-to-10, least 3rd-and-long and 4th down; xpass q50 -.03
#   5: MID             ~271 rows, ~41 carries. leans long: 1st-and-long, 2nd-and-long, 3rd-and-3; xpass q50 +.03, q80 +.26

### C

# C (under 15% of the team's rushes)
#   1: EARLY DOWN      ~378 rows, ~13 carries. the biggest group. spot carries on 1st-and-10 and 2nd-and-medium; xpass q50 -.04
#   2: MID             ~276 rows, ~14 carries. leans later-down / longer; xpass q50 +.10, q80 +.32
#   3: SHORT YARDAGE   ~249 rows, ~10 carries. carries almost only on 2nd-and-2, 3rd-and-1, 4th-and-1; every xpass quantile negative (q50 -.18)
#   4: LONG YARDAGE    ~100 rows, ~12 carries. 2nd-and-6+ and the 3rd-and-longer buckets; xpass far above the rest (q50 +.27)


####
#### GAP
####

# GAP numbers redone 2026-09-20 for the build in cache/ (gap_cluster_df.rds) -- same eight shapes as before, new numbers.
# share of his carries: center / guard / tackle / end
# 1 - CENTER            .50 / .19 / .16 / .14
# 2 - CENTER / TACKLE   .33 / .15 / .39 / .14
# 3 - NOT CENTER        .15 / .27 / .32 / .25
# 4 - GUARD / TACKLE    .05 / .38 / .46 / .11
# 5 - NOT OUTSIDE       .29 / .34 / .24 / .13
# 6 - GUARD             .12 / .54 / .18 / .16
# 7 - OUTSIDE           .19 / .17 / .18 / .46
# 8 - MID               .30 / .21 / .22 / .26

###
### B
###


#######################
# RUN GAP CLUSTERING - RANK B
#######################


# 1 - OUTSIDE           .21 / .15 / .15 / .49
# 2 - CENTER            .48 / .20 / .17 / .16
# 3 - TACKLE-ISH        .23 / .20 / .37 / .21
# 4 - GUARD             .20 / .41 / .23 / .16


#######################
# RUN GAP CLUSTERING - RANK C
#######################

# 1 - TACKLE-ISH        .25 / .18 / .33 / .24
# 2 - OUTSIDE           .08 / .03 / .06 / .83
# 3 - CENTER / GUARD    .29 / .44 / .12 / .16
# 4 - CENTER            .67 / .09 / .05 / .18


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
  filter(position_group == "REC", qbgrp_ssn == "SEADarnold-2025") %>%
  select(season, rank_grp, gap_cluster, situation_cluster, gap_z, xtd_percentile) %>%
  arrange(season) %>%
  distinct()

rush_stats_final %>%
  filter(qbgrp_ssn %in% c("JAXLawrence-2025", "TBMayfield-2024"), 
         ( (rank_grp == "C" & situation_cluster %in% c(2,99,NA))  ), rush_proportion > 0, rush_proportion <= 1) %>%
  mutate(pbp_xtd_ratio = pbp_xtd_share / rush_proportion,
         part_xtd_ratio = part_xtd_share / rush_proportion) %>%
  filter(pbp_xtd_ratio != Inf) %>%
  dplyr::summarise(ypc = mean(ypc),
                   pbp_xypc = mean(pbp_xypc),
                   part_xypc = mean(part_xypc))

rush_stats_final %>%
  filter(def_ssn == "DEN2025", 
         ((rank_grp == "C" & situation_cluster %in% c(2,99,NA))  ), rush_proportion > 0, rush_proportion <= 1) %>%
  mutate(pbp_xtd_ratio = pbp_xtd_share / rush_proportion,
         part_xtd_ratio = part_xtd_share / rush_proportion) %>%  
  dplyr::summarise(mn_pbp_xtd = mean(pbp_xtd_ratio), mn_part_xtd = mean(part_xtd_ratio))

rush_stats_final %>%
  filter(def_ssn == "DEN2025",
         ((rank_grp == "C" & gap_cluster %in% c(1, 2, 3)) | (rank_grp == "B" & gap_cluster %in% c(1, 2, 4))),
         rush_proportion > 0, rush_proportion <= 1) %>%
  mutate(pbp_xtd_ratio = pbp_xtd_share / rush_proportion,
         part_xtd_ratio = part_xtd_share / rush_proportion) %>%
  dplyr::summarise(ypc = mean(ypc),
                   pbp_xypc = mean(pbp_xypc),
                   part_xypc = mean(part_xypc))


# dplyr::summarise(mn_rush_prop = mean(rush_proportion))

# ::summarise(mn_pbp_xtd = mean(pbp_xtd_ratio), mn_part_xtd = mean(part_xtd_ratio))          # 2026-09-20: orphan fragment (no data piped in) -- it stopped the whole file from parsing

# dplyr::summarise(mn_ypc = mean(ypc),                   # 2026-09-20: orphan fragment too (no data piped in) -- errors if the file is sourced
#                  mn_pbp_xypc = mean(pbp_xypc))


rush_stats_final %>%
  filter(player == "Trevor Lawrence", rush_proportion >= 0, rush_proportion <= 1) %>%
  dplyr::select(qbgrp_ssn, rank_grp, gap_cluster, situation_cluster, gap_z) %>%
  distinct()

rush_stats_final %>%
  filter(def_ssn %in% c("SEA2025"), 
         (rank_grp == "A" & position_group == "HB" & situation_cluster %in% c(3)), rush_proportion >= 0, rush_proportion <= 1) %>%
  mutate(pbp_xtd_ratio = pbp_xtd_share / rush_proportion,
         part_xtd_ratio = part_xtd_share / rush_proportion) %>%
  dplyr::summarise(mn_pbp_xtd = mean(pbp_xtd_ratio))
  
  filter(pbp_xtd_ratio != Inf) %>%
  dplyr::summarise(mn_ypc = mean(ypc),
                   mn_pbp_xypc = mean(pbp_xypc))

rush_stats_final %>%
  filter(def_ssn == "JAX2025", ((rank_grp == "A" & gap_cluster %in% c(2)) | (rank_grp == "B" & gap_cluster %in% c(2,3)) ), rush_proportion >= 0, rush_proportion <= 1) %>%
  dplyr::summarise(mn_ypc = mean(ypc),
                   mn_pbp_xypc = mean(pbp_xypc))

rush_stats_final %>%
  filter(qbgrp_ssn == "SEADarnold-2025", player == "Kenneth Walker III", rush_proportion >= 0, rush_proportion <= 1) %>%
  arrange(pbp_xtd_share ) %>%
  pull(pbp_xtd_share )

# rush_share
# pbp_xtd_share