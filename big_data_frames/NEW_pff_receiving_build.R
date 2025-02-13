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



receiver_scheme <- read.xlsx("df_receiver_scheme.xlsx") 
# HOW DO WE CLUSTER ZONE / MAN PLAYERS? IT WILL BE INTERESTING. PROBABLY WILL HAVE TO COMPARE TO TEAM AVERAGE IN THAT SPECIFIC GAME? OR AVERAGE VERSUS SEASON? I KNOW THAT PERSONNEL MATTERS A FUCKTON - E.G. DOLPHINS SANS BYRON JONES + (RAMSEY?) SWITCHED IT UP

receiver_scheme$team[which(receiver_scheme$team == "SD")] <- "LAC"
receiver_scheme$team[which(receiver_scheme$team == "OAK")] <- "LV"


receiver_depth <- read.xlsx("df_receiving_depth_all.xlsx") # BOTH BEHIND_LOS / SHORT / MEDIUM / DEEP BUT ALSO WIDE / CENTER - DOES LEFT / CENTER / RIGHT MATTER? LET'S INVESTIGATE SOME PLAYERS. PROBABLY MORE IMPORTANT IF I LOOK AT IMPORTANT RECEIVERS FIRST. 

receiver_depth$team[which(receiver_depth$team == "SD")] <- "LAC"
receiver_depth$team[which(receiver_depth$team == "OAK")] <- "LV"


receiving_stats <- read.xlsx("df_receiving_all.xlsx") # ALIGNMENT

receiving_stats$team[which(receiving_stats$team == "SD")] <- "LAC"
receiving_stats$team[which(receiving_stats$team == "OAK")] <- "LV"


### GIVEN VOLATILITY I SHOULD DEFINITELY DO SEASON-LONG STUFF FOR CLASSIFICATION / GROUPING


# DEPTH CLUSTERING

# ROUTE INFORMATION

# EDIT WAY AFTER THE FACT (10/23): THE DTs ARE JUST LOW TARGET PEOPLE - NEED WAY MORE TO NOT BE SO FUCKING WEIRD. 
# FOR NOW 18 IS A GOOD NUMBER OF ROUTES - DON'T WANT TO REMOVE ALL 4TH WRS, MEDIAN 15. FOR 2ND TEs / HBs 23, 

rec_depth_cluster_base <- receiver_depth %>% group_by(player, player_id, team, Season) %>% 
  dplyr::summarize(base_tgts = sum(base_targets, na.rm = T), 
                   behind_los_routes = sum(left_behind_los_routes, na.rm = T) + sum(center_behind_los_routes, na.rm = T) + sum(right_behind_los_routes, na.rm = T),
                   short_routes = sum(left_short_routes, na.rm = T) + sum(center_short_routes, na.rm = T) + sum(right_short_routes, na.rm = T),
                   medium_routes = sum(left_medium_routes, na.rm = T) + sum(center_medium_routes, na.rm = T) + sum(right_medium_routes, na.rm = T),
                   deep_routes = sum(left_deep_routes, na.rm = T) + sum(center_deep_routes, na.rm = T) + sum(right_deeproutes, na.rm = T)) %>% 
  mutate(base_routes = behind_los_routes + short_routes + medium_routes + deep_routes) %>% 
  filter(base_routes >= 18) %>%
  ungroup() %>% group_by(player, player_id, team, Season) %>% 
  dplyr::summarize(behind_pct = behind_los_routes / base_routes, short_pct = short_routes / base_routes, medium_pct = medium_routes / base_routes, deep_pct = deep_routes / base_routes) %>%
  arrange(Season)
### NOTE: EVEN THOUGH ROUTES HERE ACTUALLY ONLY GETS TARGETS, THIS PRODUCES THE VARIETY IN DATA THAT WE NEED TO GET SOLID CLUSTERS. NOTE THAT FOR THE FUTURE, WE CAN DO GREAT THINGS LIKE TARGET RATE - SUM(LEFT_BEHIND, CENTER_BEHIND ... TGTS) / SUM(LEFT_BEHIND, CENTER_BEHIND ... ROUTES) 


rec_depth_pre_cluster <- rec_depth_cluster_base %>% ungroup() %>% select(-c(player_id, player, team, Season))

k <- 7
kmeans_rec_depth_combined <- kmeans(rec_depth_pre_cluster, centers = k, nstart = 25)

kmeans_rec_depth_combined

# 3 - .434
# 4 - .513
# 5 - .565
# 6 - .605
# 7 - .637
# 8 - .663
# 9 - .684



rec_depth_post_cluster <- cbind(rec_depth_pre_cluster, kmeans_rec_depth_combined$cluster)
colnames(rec_depth_post_cluster)[5] <- "cluster"

cluster_sum_depth <- rec_depth_post_cluster %>% group_by(cluster) %>% dplyr::summarize(behind_pct = mean(behind_pct), short_pct = mean(short_pct), medium_pct = mean(medium_pct), deep_pct = mean(deep_pct))

cluster_sum_depth

# START HERE


cluster_sum_depth$name = c("SMT", "DT", "MT", "RB", "BT", "G", "ST")


rec_depth_post_cluster$cluster <- kmeans_rec_depth_combined$cluster

rec_depth_final <- left_join(rec_depth_post_cluster, cluster_sum_depth %>% select(cluster, name), by = "cluster")


rec_depth_final_two <- cbind(rec_depth_cluster_base %>% select(player, player_id, team, Season), rec_depth_final)


route_cluster <- rec_depth_final_two %>% ungroup() %>% select(player_id, team, Season, name)



#####
#####
#####


# ROUTE INFORMATION

# EDIT WAY AFTER THE FACT (10/23): THE DTs ARE JUST LOW TARGET PEOPLE - NEED WAY MORE TO NOT BE SO FUCKING WEIRD. 
# FOR NOW 4 IS A GOOD NUMBER OF TARGETS - 3 IS 4TH WR MEDIAN, 5 IS 2ND TE / HB MEDIANS
# NOPE - 5 PER SEASON / TEAM COMBO. - WANT MORE OVERLAP BETWEEN NO RTE_CLUSTER, NO TGT_CLUSTER


rec_depth_cluster_tgt_base <- receiver_depth %>% group_by(player, player_id, team, Season) %>% 
  dplyr::summarize(base_targets = sum(base_targets, na.rm = T), 
                   behind_los_targets = sum(left_behind_los_targets, na.rm = T) + sum(center_behind_los_targets, na.rm = T) + sum(right_behind_los_targets, na.rm = T),
                   short_targets = sum(left_short_targets, na.rm = T) + sum(center_short_targets, na.rm = T) + sum(right_short_targets, na.rm = T),
                   medium_targets = sum(left_medium_targets, na.rm = T) + sum(center_medium_targets, na.rm = T) + sum(right_medium_targets, na.rm = T),
                   deep_targets = sum(left_deep_targets, na.rm = T) + sum(center_deep_targets, na.rm = T) + sum(right_deeptargets, na.rm = T)) %>%  
  filter(base_targets >= 9) %>%
  ungroup() %>% group_by(player, player_id, team, Season) %>% 
  dplyr::summarize(behind_pct = behind_los_targets / base_targets, short_pct = short_targets / base_targets, medium_pct = medium_targets / base_targets, deep_pct = deep_targets / base_targets) %>%
  arrange(Season)


rec_depth_pre_cluster_tgt <- rec_depth_cluster_tgt_base %>% ungroup() %>% select(-c(player_id, player, team, Season))

k2 <- 8
kmeans_rec_depth_combined_tgt <- kmeans(rec_depth_pre_cluster_tgt, centers = k2, nstart = 25)

kmeans_rec_depth_combined_tgt

# 2 - .453 - .453
# 3 - .649 - .061
# 4 - .71 - .061
# 5 - .756 - .046
# 6 - .79 - .034
# 7 - .81 - .02
# 8 - .828 - .018
# 9 - 
# 10 - 
# 11 - 


rec_depth_post_cluster_tgt <- cbind(rec_depth_pre_cluster_tgt, kmeans_rec_depth_combined_tgt$cluster)
colnames(rec_depth_post_cluster_tgt)[5] <- "cluster"

cluster_sum_depth_tgt <- rec_depth_post_cluster_tgt %>% group_by(cluster) %>% dplyr::summarize(behind_pct = mean(behind_pct), short_pct = mean(short_pct), medium_pct = mean(medium_pct), deep_pct = mean(deep_pct))

cluster_sum_depth_tgt

cluster_sum_depth_tgt$name = c("MT", "SL", "BT", "ST", "ML", "SRB", "G", "DT")


rec_depth_post_cluster_tgt$cluster <- kmeans_rec_depth_combined_tgt$cluster

rec_depth_final_tgt <- left_join(rec_depth_post_cluster_tgt, cluster_sum_depth_tgt %>% select(cluster, name), by = "cluster")


rec_depth_final_tgt_two <- cbind(rec_depth_cluster_tgt_base %>% select(player, player_id, team, Season), rec_depth_final_tgt)


tgt_cluster <- rec_depth_final_tgt_two %>% ungroup() %>% select(player_id, team, Season, name)


###
###


# ALIGNMENT CLUSTERING

receiving_stats_alignment <- receiving_stats %>% select(player, player_id, team, position, Week, Season, inline_rate, slot_rate, wide_rate) %>% mutate(behind_rate = 100 - inline_rate - slot_rate - wide_rate)

receiving_stats_alignment_pre_cluster <- receiving_stats_alignment %>% 
  group_by(player, player_id, team, Season) %>% 
  dplyr::summarise(inline_rate = mean(inline_rate), slot_rate = mean(slot_rate), wide_rate = mean(wide_rate), behind_rate = mean(behind_rate))

receiving_stats_alignment_during_cluster <- receiving_stats_alignment_pre_cluster %>% ungroup() %>% select(inline_rate:behind_rate)

k3 = 6
kmeans_rec2 <- kmeans(receiving_stats_alignment_during_cluster, centers = k3, nstart = 25)
kmeans_rec2


# 3 - .782
# 4 - .894
# 5 - .916
# 6 - .933
# 7 - .945
# 8 - .953
# 9 - .959
# 10 - .963


receiving_stats_alignment_pre_cluster <- cbind(receiving_stats_alignment_pre_cluster, kmeans_rec2$cluster)
colnames(receiving_stats_alignment_pre_cluster)[9] <- "cluster"

cluster_sum_alignment <- receiving_stats_alignment_pre_cluster %>% group_by(kmeans_rec2$cluster) %>% dplyr::summarize(inline_rate = mean(inline_rate), slot_rate = mean(slot_rate), wide_rate = mean(wide_rate), behind_rate = mean(behind_rate))

cluster_sum_alignment


#### AGAIN - ALWAYS MAKE SURE TO CHECK THIS SHIT


cluster_sum_alignment$name <- c("WSWR", "ITE", "STE", "RB", "SWR", "WWR")


colnames(cluster_sum_alignment)[1] <- "cluster"

receiving_stats_alignment_pre_cluster$cluster <- kmeans_rec2$cluster

receiving_stats_alignment_post_cluster <- merge(receiving_stats_alignment_pre_cluster, cluster_sum_alignment %>% select(cluster, name), by = "cluster")


align_cluster <- receiving_stats_alignment_post_cluster %>% ungroup() %>% select(player_id, team, Season, name)


##### MAY NEED TO DO ANOTHER SCRIPT WITH PBP RECEIVING STATS. MIGHT NEED TO LEARN / PROBABLY WILL NEED TO JOIN THE COMBINED PBP DATA. THIS WITH RUSHING AS WELL, AND RUSHING IS DEFINITELY NEXT. 
# WE WILL LIKELY NEED TO SUM UP MAN, ZONE SHIT HERE, IF IT'S EVEN WORTH IT LMAO.


# HONESTLY MIGHT THINK THAT WE SHOULD PROBABLY LOOP IN THE OTHER CLUSTERS --> FIND WHAT THE MEAN RATES FOR EACH GROUP ARE IN TERMS OF MAN_RATE, ZONE_RATE, ETC. 


receiver_scheme_combined <- left_join(receiver_scheme, tgt_cluster, by = c("player_id", "team", "Season"))
receiver_scheme_combined <- left_join(receiver_scheme_combined, route_cluster, by = c("player_id", "team", "Season"))
receiver_scheme_combined <- left_join(receiver_scheme_combined, align_cluster, by = c("player_id" = "player_id", "team" = "team", "Season" = "Season"))


colnames(receiver_scheme_combined)[c(74,75,76)] <- c("tgt_cluster", "rte_cluster", "align_cluster")



# THIS SHIT ONLY HAS PASS_SNAPS AND SHIT FOR PLAYERS WITH 1+ TARGET. WE NEED EVERYTHING, ESP FOR PLAYERS VERSUS LOCKDOWN DEFENSES / COVERAGE
# JOIN IN ... IN THE SNAPS SHEET THERE IS NO TEAM_ID. WE'RE GOING TO NEED TO DO SOME FINAGLING
# SNAPS TO GAME DF, THEN JOIN TO ... (THINKING) ... IDS DATAFRAME? THAT IS THE SAFEST FOR SURE, OR AT LEAST (THEORETICALLY) THE MOST "THOROUGH" SHEET. LET'S TAKE THIS SOMEWHERE ELSE SO WE DON'T "PLAY AROUND" HERE.  


attach("NEW_pff_ids_build_workspace.RData")

combined_ids_final <- combined_ids_final

detach("file:NEW_pff_ids_build_workspace.RData")



game_id_df <- read.xlsx("nfl_game_ids.xlsx") %>% distinct()

snaps_df <- read.xlsx("nfl_plays.xlsx")

# THERE IS FRANCHISE_ID IN SNAPS_DF


away_id_df <- game_id_df %>% select(away_franchise_id, away_abbreviation)
home_id_df <- game_id_df %>% select(home_franchise_id, home_abbreviation)

colnames(away_id_df) <- c("franchise_id", "team")
colnames(home_id_df) <- c("franchise_id", "team")


team_id_df <- rbind(away_id_df, home_id_df) %>% distinct() %>% arrange(franchise_id)
team_id_df$team[which(team_id_df$franchise_id == 27)] <- 'LAC'
team_id_df$team[which(team_id_df$franchise_id == 23)] <- 'LV'

team_id_df <- team_id_df %>% distinct()


snaps_df <- left_join(snaps_df, team_id_df, by = c("franchise_id"))

snaps_df <- left_join(snaps_df, game_id_df %>% select(id, season, week), by = c("game_id" = "id"))


pass_snaps_df <- snaps_df %>% filter(snap_counts_pass_route > 0)

### JOIN IN THE SNAPS NOW
### ACTUALLY, JOIN USING THE SNAPS AS THE BASE WITH RECEIVER_SCHEME ON TOP. 
### TIMEOUT - THERE IS NO WEEK, SEASON IN SNAPS_DF. PROBABLY NEED TO JOIN IN GAME_ID_DF FIRST


combined_snaps_df <- left_join(pass_snaps_df, receiver_scheme_combined %>% ungroup() %>% select(-penalties, player, position, declined_penalties, franchise_id, jersey_number), by = c("player_id" = "player_id", "week" = "Week", "season" = "Season", "team" = "team"))


combined_snaps_df <- combined_snaps_df %>%
  group_by(player_id, season, team) %>%  # Group by player, season, and team
  fill(tgt_cluster, rte_cluster, align_cluster, .direction = "downup")  # Fill missing values based on other games for the same player/season/team


combined_snaps_df <- combined_snaps_df %>% ungroup() %>% select(-c(player.y, franchise_id.y, declined_penalties.y, position.y))

colnames(combined_snaps_df)[which(colnames(combined_snaps_df) == "player.x")] <- "player"
colnames(combined_snaps_df)[which(colnames(combined_snaps_df) == "franchise_id.x")] <- "franchise_id"
colnames(combined_snaps_df)[which(colnames(combined_snaps_df) == "declined_penalties.x")] <- "declined_penalties"
colnames(combined_snaps_df)[which(colnames(combined_snaps_df) == "position.x")] <- "position"
colnames(combined_snaps_df)[which(colnames(combined_snaps_df) == "jersey_number.x")] <- "jersey_number"


library(dplyr)
library(tidyr)


combined_snaps_df <- combined_snaps_df %>%
  # Create position groups
  mutate(position_group = case_when(
    position %in% c("TE-L", "TE-R") | player == "Taysom Hill" ~ "TE",
    position %in% c("HB") | (position %in% c("FB", "LT", "RT", "LG", "RG", "C") & align_cluster %in% c("BRB", "MRB", NA)) | (position == "QB" & player != "Taysom Hill") ~ "HB",
    position %in% c("SRWR", "RWR", "SLWR", "LWR") ~ "WR",
    TRUE ~ "HB"  # Assign any other positions to "Other"
  ))


# CAST POS_GROUP_ORDER, TOT_ORDER TOO
# WE'RE GOING TO HAVE TO CHANGE SOME SHIT .... THERE ISN'T THE RIGHT COLUMN FOR THIS SHIT FFS. WIDE_SNAPS + INLINE_SNAPS? BECAUSE THIS SHIT DOESN'T LINE UP WITH WHAT IT'S SUPPOSED TO BE. ROUTES ARE NA IF NO TARGETS IN THE GAME IT LOOKS LIKE


library(sqldf)

combined_snaps_df <- sqldf("
    SELECT *,
           ROW_NUMBER() OVER (
             PARTITION BY team, position_group, game_id
             ORDER BY snap_counts_pass_route DESC, snap_counts_pass_block DESC, snap_counts_total_run DESC
           ) AS pos_rank,
           ROW_NUMBER() OVER (
             PARTITION BY team, game_id
             ORDER BY snap_counts_pass_route DESC, snap_counts_pass_block DESC, snap_counts_total_run DESC
           ) AS team_rank
    FROM combined_snaps_df
")


# GOTTA FIND THOSE WHERE THERE ARE NULL IN RTE / ALIGN / TGT_CLUSTER
# MORE UNDERSTANDABLE FOR TGT - NO TARGETS ... YEAH. 


# ALIGN CLUSTER - HONESTLY JUST GOING TO INTERPOLATE FFS. 


align_cluster_blanks_df <- sqldf("
  SELECT player, 
         player_id, 
         align_cluster
  FROM (
    SELECT player, 
           player_id, 
           align_cluster, 
           COUNT(*) AS count_align, 
           ROW_NUMBER() OVER (
             PARTITION BY player, player_id 
             ORDER BY COUNT(*) DESC
           ) AS rank_align
    FROM combined_snaps_df
    WHERE align_cluster IS NOT NULL
    GROUP BY player, player_id, align_cluster
  ) AS ranked_align
  WHERE rank_align = 1
")


# Step 1: Create a temporary dataframe with the join
combined_snaps_df <- sqldf("
  SELECT a.*, 
         CASE 
           WHEN a.align_cluster IS NULL THEN b.align_cluster
           ELSE a.align_cluster
         END AS temp_align_cluster
  FROM combined_snaps_df AS a
  LEFT JOIN align_cluster_blanks_df AS b
    ON a.player = b.player AND a.player_id = b.player_id
")


# Step 3: Remove the temporary column
combined_snaps_df <- combined_snaps_df[,-95]
colnames(combined_snaps_df)[98] <- "align_cluster"


# EVERYONE THAT IS LEFT NOW IS JUST REALLY STRANGE OL, QB, WHATEVER ... FRANKLY DON'T WORRY ABOUT IT
# ANYONE WITH NULL ALIGN_CLUSTER FROM HERE ON OUT - NOTHING


combined_snaps_df %>% filter(!is.na(align_cluster) & is.na(rte_cluster)) %>% 
  select(player, player_id, team, week, season, base_targets) %>% arrange(player, week, season)


combined_snaps_df %>% filter(player_id == 9635) %>% select(season, align_cluster, rte_cluster, tgt_cluster)



rte_cluster_blanks_df <- sqldf("
  SELECT player, 
         player_id, 
         season,
         rte_cluster
  FROM (
    SELECT player, 
           player_id,
           season,
           rte_cluster, 
           COUNT(*) AS count_rte, 
           ROW_NUMBER() OVER (
             PARTITION BY player, player_id, season 
             ORDER BY COUNT(*) DESC
           ) AS rank_rte
    FROM  combined_snaps_df
    WHERE rte_cluster IS NOT NULL
    GROUP BY  player, player_id, season, rte_cluster
  ) AS ranked_rte
  WHERE rank_rte = 1
")
# WE'LL GO BY SEASON TO FILL IN BLANKS - MAKES SENSE TO ME

# Step 1: Create a temporary dataframe with the join
combined_snaps_df <- sqldf("
  SELECT a.*, 
         CASE 
           WHEN a.rte_cluster IS NULL THEN b.rte_cluster
           ELSE a.rte_cluster
         END AS temp_rte_cluster
  FROM combined_snaps_df AS a
  LEFT JOIN rte_cluster_blanks_df AS b
    ON a.player = b.player AND a.player_id = b.player_id AND a.season = b.season
")

combined_snaps_df <- combined_snaps_df %>% select(-rte_cluster)
colnames(combined_snaps_df)[98] <- "rte_cluster"


tgt_cluster_blanks_df <- sqldf("
  SELECT player, 
         player_id, 
         season,
         tgt_cluster
  FROM (
    SELECT player, 
           player_id,
           season,
           tgt_cluster, 
           COUNT(*) AS count_tgt, 
           ROW_NUMBER() OVER (
             PARTITION BY player, player_id, season 
             ORDER BY COUNT(*) DESC
           ) AS rank_tgt
    FROM  combined_snaps_df
    WHERE tgt_cluster IS NOT NULL
    GROUP BY  player, player_id, season, tgt_cluster
  ) AS ranked_rte
  WHERE rank_tgt = 1
")



combined_snaps_df %>% group_by(tgt_cluster) %>% 
  filter(position_group == "WR" & team_rank <= 5) %>%
  dplyr::summarise(players = n_distinct(player_id)
                   , routes = sum(man_routes, na.rm = T) + sum(zone_routes, na.rm = T)
                   , targets = sum(man_targets, na.rm = T) + sum(zone_targets, na.rm = T)
                   , man_routes = sum(man_routes, na.rm = T)
                   , zone_routes = sum(zone_routes, na.rm = T)
                   , man_targets = sum(man_targets, na.rm = T)
                   , zone_targets = sum(zone_targets, na.rm = T)
                   , man_receptions = sum(man_receptions, na.rm = T)
                   , zone_receptions = sum(zone_receptions, na.rm = T)
                   , man_yards = sum(man_yards, na.rm = T)
                   , zone_yards = sum(zone_yards, na.rm = T))
 

combined_snaps_df$rte_cluster[which(is.na(combined_snaps_df$rte_cluster))] <- "LR"
combined_snaps_df$tgt_cluster[which(is.na(combined_snaps_df$tgt_cluster))] <- "LT"


combined_snaps_df$player_season_id = paste0(combined_snaps_df$player_id, combined_snaps_df$team, combined_snaps_df$season)


combined_snaps_df %>% filter(team_rank <= 5) %>% 
  group_by(position_group, align_cluster, tgt_cluster, rte_cluster) %>% 
  dplyr::summarise(n = n()) %>% arrange(desc(n))

combined_snaps_df %>% filter(player == "D.K. Metcalf") %>% 
  select(season, position_group, align_cluster, tgt_cluster, rte_cluster) %>% distinct()

combined_snaps_df %>%
  filter(align_cluster == "WWR") %>% 
  group_by(rte_cluster) %>%
  dplyr::summarise(players = n_distinct(player_id),
                   man_plays = sum(man_pass_plays, na.rm = T),
                   zone_plays = sum(zone_pass_plays, na.rm = T),
                   man_tgts = sum(man_targets, na.rm = T),
                   zone_tgts = sum(zone_targets, na.rm = T),
                   man_yards = sum(man_yards, na.rm = T),
                   zone_yards = sum(zone_yards, na.rm = T),
                   man_comp = sum(man_receptions, na.rm = T),
                   zone_comp = sum(zone_receptions, na.rm = T),
                   man_tgt_rate = man_tgts / man_plays,
                   zone_tgt_rate = zone_tgts / zone_plays,
                   man_ypa = man_yards / man_tgts,
                   zone_ypa = zone_yards / zone_tgts,
                   man_cp = man_comp / man_tgts,
                   zone_cp = zone_comp / zone_tgts,
                   man_gr = mean(man_grades_pass_route, na.rm = T),
                   zone_gr = mean(zone_grades_pass_route, na.rm = T),
                   man_adot = mean(man_avg_depth_of_target, na.rm = T),
                   zone_adot = mean(zone_avg_depth_of_target, na.rm = T),
                   man_qbr = mean(man_targeted_qb_rating, na.rm = T),
                   zone_qbr = mean(zone_targeted_qb_rating, na.rm = T),
                   man_tds = mean(man_touchdowns, na.rm = T),
                   zone_tds = mean(zone_touchdowns, na.rm = T)) %>%
  filter(players > 1) %>%
  select(rte_cluster, players, man_tgt_rate:zone_qbr)


combined_snaps_df %>%
  filter(align_cluster == "WWR") %>% 
  group_by(tgt_cluster) %>%
  dplyr::summarise(players = n_distinct(player_id),
                   man_plays = sum(man_pass_plays, na.rm = T),
                   zone_plays = sum(zone_pass_plays, na.rm = T),
                   man_tgts = sum(man_targets, na.rm = T),
                   zone_tgts = sum(zone_targets, na.rm = T),
                   man_yards = sum(man_yards, na.rm = T),
                   zone_yards = sum(zone_yards, na.rm = T),
                   man_comp = sum(man_receptions, na.rm = T),
                   zone_comp = sum(zone_receptions, na.rm = T),
                   man_tgt_rate = man_tgts / man_plays,
                   zone_tgt_rate = zone_tgts / zone_plays,
                   man_ypa = man_yards / man_tgts,
                   zone_ypa = zone_yards / zone_tgts,
                   man_cp = man_comp / man_tgts,
                   zone_cp = zone_comp / zone_tgts,
                   man_gr = mean(man_grades_pass_route, na.rm = T),
                   zone_gr = mean(zone_grades_pass_route, na.rm = T),
                   man_adot = mean(man_avg_depth_of_target, na.rm = T),
                   zone_adot = mean(zone_avg_depth_of_target, na.rm = T),
                   man_qbr = mean(man_targeted_qb_rating, na.rm = T),
                   zone_qbr = mean(zone_targeted_qb_rating, na.rm = T),
                   man_tds = mean(man_touchdowns, na.rm = T),
                   zone_tds = mean(zone_touchdowns, na.rm = T)) %>%
  filter(players > 1) %>%
  select(tgt_cluster, players, man_tgt_rate:zone_qbr)




combined_snaps_df %>%
  filter(position_group == "WR" & align_cluster == "WWR" & tgt_cluster == "DT" & rte_cluster == "G") %>% 
  group_by(player, team, season) %>%
  dplyr::summarise(man_plays = sum(man_pass_plays, na.rm = T),
                   zone_plays = sum(zone_pass_plays, na.rm = T),
                   man_tgts = sum(man_targets, na.rm = T),
                   zone_tgts = sum(zone_targets, na.rm = T),
                   man_yards = sum(man_yards, na.rm = T),
                   zone_yards = sum(zone_yards, na.rm = T),
                   man_comp = sum(man_receptions, na.rm = T),
                   zone_comp = sum(zone_receptions, na.rm = T),
                   man_tgt_rate = man_tgts / man_plays,
                   zone_tgt_rate = zone_tgts / zone_plays,
                   man_ypa = man_yards / man_tgts,
                   zone_ypa = zone_yards / zone_tgts,
                   man_cp = man_comp / man_tgts,
                   zone_cp = zone_comp / zone_tgts,
                   man_gr = mean(man_grades_pass_route, na.rm = T),
                   zone_gr = mean(zone_grades_pass_route, na.rm = T),
                   man_adot = mean(man_avg_depth_of_target, na.rm = T),
                   zone_adot = mean(zone_avg_depth_of_target, na.rm = T),
                   man_qbr = mean(man_targeted_qb_rating, na.rm = T),
                   zone_qbr = mean(zone_targeted_qb_rating, na.rm = T),
                   man_tds = sum(man_touchdowns, na.rm = T),
                   zone_tds = sum(zone_touchdowns, na.rm = T)) %>%
  select(player:season, man_tgt_rate:zone_qbr)
# THERE IS DEFINITE CHANGES BASED ON WR, AND I HAVE TO IMAGINE IT DOESN'T HAVE EVERYTHING TO DO WITH MATCHUP BUT RECEIVER TRAIT TOO



#####
#####
#####


combined_rec_clusters_precluster <- combined_snaps_df %>%
  group_by(align_cluster, tgt_cluster, rte_cluster) %>%
  dplyr::summarise(
    players = n_distinct(player_season_id),
    man_plays = sum(man_pass_plays, na.rm = TRUE),
    zone_plays = sum(zone_pass_plays, na.rm = TRUE),
    man_tgts = sum(man_targets, na.rm = TRUE),
    zone_tgts = sum(zone_targets, na.rm = TRUE),
    man_yards = sum(man_yards, na.rm = TRUE),
    zone_yards = sum(zone_yards, na.rm = TRUE),
    man_comp = sum(man_receptions, na.rm = TRUE),
    zone_comp = sum(zone_receptions, na.rm = TRUE),
    man_tgt_rate = man_tgts / man_plays,
    zone_tgt_rate = zone_tgts / zone_plays,
    man_ypa = man_yards / man_tgts,
    zone_ypa = zone_yards / zone_tgts,
    man_cp = man_comp / man_tgts,
    zone_cp = zone_comp / zone_tgts,
    man_adot = mean(man_avg_depth_of_target, na.rm = TRUE),
    zone_adot = mean(zone_avg_depth_of_target, na.rm = TRUE),
    man_qbr = mean(man_targeted_qb_rating, na.rm = TRUE),
    zone_qbr = mean(zone_targeted_qb_rating, na.rm = TRUE),
    man_tds = mean(man_touchdowns, na.rm = TRUE),
    zone_tds = mean(zone_touchdowns, na.rm = TRUE)
  ) %>%
  filter(players >= 10) %>%
  filter(!is.na(man_qbr)) %>%
  ungroup()



combined_rec_clusters <- combined_rec_clusters_precluster %>%
  select(man_tgt_rate:zone_tds) %>% ungroup() %>%
  mutate(across(man_tgt_rate:zone_tds, ~ (. - mean(.)) / sd(.), .names = "z_{col}")) %>%
  select(z_man_tgt_rate:z_zone_tds)


k4 = 5
kmeans_rec_groups <- kmeans(combined_rec_clusters, centers = k4, nstart = 25)
kmeans_rec_groups

# 2 - .421
# 3 - .525 - .104 - 3
# 4 - .605 - .08 - 1 - NOT ENOUGH
# 5 - .655 - .05 - 2
# 6 - .688 - .033 - 6
# 7 - .714 - .026 - 10
# 8 - .737 - .023 - 7
# 9 - .755 - .018 - 9
# 10 - .769 - .014 - 4
# 11 - .783 - .014 - 5
# 12 - .797 - .014 - 8

combined_rec_clusters_post_cluster <- cbind(combined_rec_clusters_precluster, kmeans_rec_groups$cluster)
colnames(combined_rec_clusters_post_cluster)[25] <- "cluster"

View(combined_rec_clusters_post_cluster %>% filter((tgt_cluster %ni% c("DT", "DL", "MT", "ML", "LT") & rte_cluster != "G" & align_cluster %ni% c("STE", "MRB", "BRB", "ITE"))))


# MRB / BRB IS ONE GROUP
# TGT_CLUSTER = DT / DL / MT
# RTE_CLUSTER = G
# ALIGN_CLUSTER = ITE, STE
# TGT_CLUSTER = ML
# TGT_CLUSTER = LT, ALIGN_CLUSTER %IN% WWR/WSWR/SWR
# IS.NA(ALIGN_CLUSTER)
# EVERYTHING ELSE



combined_snaps_df <- combined_snaps_df %>% 
  mutate(man_zone_grp = case_when(align_cluster %in% c("MRB", "BRB", "RB") ~ "RB",
                                  tgt_cluster %in% c("DT", "DL", "MT") ~ "DEEP",
                                  rte_cluster == "G" ~ "G",
                                  align_cluster %in% c("ITE", "STE") ~ "TE",
                                  tgt_cluster == "ML" ~ "ML",
                                  tgt_cluster == "LT" & align_cluster %in% c("WWR", "WSWR", "SWR") ~ "LT WR",
                                  is.na(align_cluster) ~ "NON-REG",
                                  TRUE ~ "OTH"))


# BY FAR, LARGEST GROUP OF OTH --> NON-REGULAR PLAYERS (OL, QB, DEF) --> SHOULD BE SEPARATE


rb_man_zone_z_df <- combined_snaps_df %>% filter(man_zone_grp == "RB") %>% 
  group_by(player, player_season_id) %>%
  dplyr::summarise(man_plays = sum(man_pass_plays, na.rm = TRUE),
                   zone_plays = sum(zone_pass_plays, na.rm = TRUE),
                   man_tgts = sum(man_targets, na.rm = TRUE),
                   zone_tgts = sum(zone_targets, na.rm = TRUE),
                   man_yards = sum(man_yards, na.rm = TRUE),
                   zone_yards = sum(zone_yards, na.rm = TRUE),
                   man_comp = sum(man_receptions, na.rm = TRUE),
                   zone_comp = sum(zone_receptions, na.rm = TRUE),
                   man_tgt_rate = man_tgts / man_plays,
                   zone_tgt_rate = zone_tgts / zone_plays,
                   man_ypa = man_yards / man_tgts,
                   zone_ypa = zone_yards / zone_tgts,
                   man_cp = man_comp / man_tgts,
                   zone_cp = zone_comp / zone_tgts,
                   man_adot = mean(man_avg_depth_of_target, na.rm = TRUE),
                   zone_adot = mean(zone_avg_depth_of_target, na.rm = TRUE),
                   man_qbr = mean(man_targeted_qb_rating, na.rm = TRUE),
                   zone_qbr = mean(zone_targeted_qb_rating, na.rm = TRUE),
                   man_tds = sum(man_touchdowns, na.rm = TRUE) / sum(man_pass_plays, na.rm = TRUE),
                   zone_tds = sum(zone_touchdowns, na.rm = TRUE) / sum(zone_pass_plays, na.rm = T)) %>% filter(!is.na(man_plays) & !is.na(zone_plays)) %>% drop_na() %>%
  mutate(tgt_rate_diff = man_tgt_rate - zone_tgt_rate,
         ypa_diff = man_ypa - zone_ypa,
         cp_diff = man_cp - zone_cp,
         adot_diff = man_adot - zone_adot,
         qbr_diff = man_qbr - zone_qbr,
         td_diff = man_tds - zone_tds) %>%
  select(player, player_season_id, tgt_rate_diff:td_diff) %>% ungroup() %>%
  mutate(across(tgt_rate_diff:td_diff, ~ (. - mean(.)) / sd(.), .names = "z_{col}")) %>%
  select(player, player_season_id, z_tgt_rate_diff:z_td_diff) %>% 
  mutate(z_score = z_tgt_rate_diff + .85 * z_ypa_diff + .65 * z_cp_diff + .45 * z_adot_diff + .85 * z_qbr_diff + .2 * z_td_diff,
         z_score_percentile = round(100 * percent_rank(z_score), 0)
  )


deep_man_zone_z_df <- combined_snaps_df %>% filter(man_zone_grp == "DEEP") %>% 
  group_by(player, player_season_id) %>%
  dplyr::summarise(man_plays = sum(man_pass_plays, na.rm = TRUE),
                   zone_plays = sum(zone_pass_plays, na.rm = TRUE),
                   man_tgts = sum(man_targets, na.rm = TRUE),
                   zone_tgts = sum(zone_targets, na.rm = TRUE),
                   man_yards = sum(man_yards, na.rm = TRUE),
                   zone_yards = sum(zone_yards, na.rm = TRUE),
                   man_comp = sum(man_receptions, na.rm = TRUE),
                   zone_comp = sum(zone_receptions, na.rm = TRUE),
                   man_tgt_rate = man_tgts / man_plays,
                   zone_tgt_rate = zone_tgts / zone_plays,
                   man_ypa = man_yards / man_tgts,
                   zone_ypa = zone_yards / zone_tgts,
                   man_cp = man_comp / man_tgts,
                   zone_cp = zone_comp / zone_tgts,
                   man_adot = mean(man_avg_depth_of_target, na.rm = TRUE),
                   zone_adot = mean(zone_avg_depth_of_target, na.rm = TRUE),
                   man_qbr = mean(man_targeted_qb_rating, na.rm = TRUE),
                   zone_qbr = mean(zone_targeted_qb_rating, na.rm = TRUE),
                   man_tds = sum(man_touchdowns, na.rm = TRUE) / sum(man_pass_plays, na.rm = TRUE),
                   zone_tds = sum(zone_touchdowns, na.rm = TRUE) / sum(zone_pass_plays, na.rm = T)) %>% filter(!is.na(man_plays) & !is.na(zone_plays)) %>% drop_na() %>%
  mutate(tgt_rate_diff = man_tgt_rate - zone_tgt_rate,
         ypa_diff = man_ypa - zone_ypa,
         cp_diff = man_cp - zone_cp,
         adot_diff = man_adot - zone_adot,
         qbr_diff = man_qbr - zone_qbr,
         td_diff = man_tds - zone_tds) %>%
  select(player, player_season_id, tgt_rate_diff:td_diff) %>% ungroup() %>%
  mutate(across(tgt_rate_diff:td_diff, ~ (. - mean(.)) / sd(.), .names = "z_{col}")) %>%
  select(player, player_season_id, z_tgt_rate_diff:z_td_diff) %>% 
  mutate(z_score = z_tgt_rate_diff + .85 * z_ypa_diff + .65 * z_cp_diff + .45 * z_adot_diff + .85 * z_qbr_diff + .2 * z_td_diff,
         z_score_percentile = round(100 * percent_rank(z_score), 0)
  )
  

# TGT - 1
# YPA - .85
# CP - .65
# ADOT - .45
# QBR - .85
# TD - .2


g_man_zone_z_df <- combined_snaps_df %>% filter(man_zone_grp == "G") %>% 
  group_by(player, player_season_id) %>%
  dplyr::summarise(man_plays = sum(man_pass_plays, na.rm = TRUE),
                   zone_plays = sum(zone_pass_plays, na.rm = TRUE),
                   man_tgts = sum(man_targets, na.rm = TRUE),
                   zone_tgts = sum(zone_targets, na.rm = TRUE),
                   man_yards = sum(man_yards, na.rm = TRUE),
                   zone_yards = sum(zone_yards, na.rm = TRUE),
                   man_comp = sum(man_receptions, na.rm = TRUE),
                   zone_comp = sum(zone_receptions, na.rm = TRUE),
                   man_tgt_rate = man_tgts / man_plays,
                   zone_tgt_rate = zone_tgts / zone_plays,
                   man_ypa = man_yards / man_tgts,
                   zone_ypa = zone_yards / zone_tgts,
                   man_cp = man_comp / man_tgts,
                   zone_cp = zone_comp / zone_tgts,
                   man_adot = mean(man_avg_depth_of_target, na.rm = TRUE),
                   zone_adot = mean(zone_avg_depth_of_target, na.rm = TRUE),
                   man_qbr = mean(man_targeted_qb_rating, na.rm = TRUE),
                   zone_qbr = mean(zone_targeted_qb_rating, na.rm = TRUE),
                   man_tds = sum(man_touchdowns, na.rm = TRUE) / sum(man_pass_plays, na.rm = TRUE),
                   zone_tds = sum(zone_touchdowns, na.rm = TRUE) / sum(zone_pass_plays, na.rm = T)) %>% filter(!is.na(man_plays) & !is.na(zone_plays)) %>% drop_na() %>%
  mutate(tgt_rate_diff = man_tgt_rate - zone_tgt_rate,
         ypa_diff = man_ypa - zone_ypa,
         cp_diff = man_cp - zone_cp,
         adot_diff = man_adot - zone_adot,
         qbr_diff = man_qbr - zone_qbr,
         td_diff = man_tds - zone_tds) %>%
  select(player, player_season_id, tgt_rate_diff:td_diff) %>% ungroup() %>%
  mutate(across(tgt_rate_diff:td_diff, ~ (. - mean(.)) / sd(.), .names = "z_{col}")) %>%
  select(player, player_season_id, z_tgt_rate_diff:z_td_diff) %>% 
  mutate(z_score = z_tgt_rate_diff + .85 * z_ypa_diff + .65 * z_cp_diff + .45 * z_adot_diff + .85 * z_qbr_diff + .2 * z_td_diff,
         z_score_percentile = round(100 * percent_rank(z_score), 0)
  )


te_man_zone_z_df <- combined_snaps_df %>% filter(man_zone_grp == "TE") %>% 
  group_by(player, player_season_id) %>%
  dplyr::summarise(man_plays = sum(man_pass_plays, na.rm = TRUE),
                   zone_plays = sum(zone_pass_plays, na.rm = TRUE),
                   man_tgts = sum(man_targets, na.rm = TRUE),
                   zone_tgts = sum(zone_targets, na.rm = TRUE),
                   man_yards = sum(man_yards, na.rm = TRUE),
                   zone_yards = sum(zone_yards, na.rm = TRUE),
                   man_comp = sum(man_receptions, na.rm = TRUE),
                   zone_comp = sum(zone_receptions, na.rm = TRUE),
                   man_tgt_rate = man_tgts / man_plays,
                   zone_tgt_rate = zone_tgts / zone_plays,
                   man_ypa = man_yards / man_tgts,
                   zone_ypa = zone_yards / zone_tgts,
                   man_cp = man_comp / man_tgts,
                   zone_cp = zone_comp / zone_tgts,
                   man_adot = mean(man_avg_depth_of_target, na.rm = TRUE),
                   zone_adot = mean(zone_avg_depth_of_target, na.rm = TRUE),
                   man_qbr = mean(man_targeted_qb_rating, na.rm = TRUE),
                   zone_qbr = mean(zone_targeted_qb_rating, na.rm = TRUE),
                   man_tds = sum(man_touchdowns, na.rm = TRUE) / sum(man_pass_plays, na.rm = TRUE),
                   zone_tds = sum(zone_touchdowns, na.rm = TRUE) / sum(zone_pass_plays, na.rm = T)) %>% filter(!is.na(man_plays) & !is.na(zone_plays)) %>% drop_na() %>%
  mutate(tgt_rate_diff = man_tgt_rate - zone_tgt_rate,
         ypa_diff = man_ypa - zone_ypa,
         cp_diff = man_cp - zone_cp,
         adot_diff = man_adot - zone_adot,
         qbr_diff = man_qbr - zone_qbr,
         td_diff = man_tds - zone_tds) %>%
  select(player, player_season_id, tgt_rate_diff:td_diff) %>% ungroup() %>%
  mutate(across(tgt_rate_diff:td_diff, ~ (. - mean(.)) / sd(.), .names = "z_{col}")) %>%
  select(player, player_season_id, z_tgt_rate_diff:z_td_diff) %>% 
  mutate(z_score = z_tgt_rate_diff + .85 * z_ypa_diff + .65 * z_cp_diff + .45 * z_adot_diff + .85 * z_qbr_diff + .2 * z_td_diff,
         z_score_percentile = round(100 * percent_rank(z_score), 0)
  )

ml_man_zone_z_df <- combined_snaps_df %>% filter(man_zone_grp == "ML") %>% 
  group_by(player, player_season_id) %>%
  dplyr::summarise(man_plays = sum(man_pass_plays, na.rm = TRUE),
                   zone_plays = sum(zone_pass_plays, na.rm = TRUE),
                   man_tgts = sum(man_targets, na.rm = TRUE),
                   zone_tgts = sum(zone_targets, na.rm = TRUE),
                   man_yards = sum(man_yards, na.rm = TRUE),
                   zone_yards = sum(zone_yards, na.rm = TRUE),
                   man_comp = sum(man_receptions, na.rm = TRUE),
                   zone_comp = sum(zone_receptions, na.rm = TRUE),
                   man_tgt_rate = man_tgts / man_plays,
                   zone_tgt_rate = zone_tgts / zone_plays,
                   man_ypa = man_yards / man_tgts,
                   zone_ypa = zone_yards / zone_tgts,
                   man_cp = man_comp / man_tgts,
                   zone_cp = zone_comp / zone_tgts,
                   man_adot = mean(man_avg_depth_of_target, na.rm = TRUE),
                   zone_adot = mean(zone_avg_depth_of_target, na.rm = TRUE),
                   man_qbr = mean(man_targeted_qb_rating, na.rm = TRUE),
                   zone_qbr = mean(zone_targeted_qb_rating, na.rm = TRUE),
                   man_tds = sum(man_touchdowns, na.rm = TRUE) / sum(man_pass_plays, na.rm = TRUE),
                   zone_tds = sum(zone_touchdowns, na.rm = TRUE) / sum(zone_pass_plays, na.rm = T)) %>% filter(!is.na(man_plays) & !is.na(zone_plays)) %>% drop_na() %>%
  mutate(tgt_rate_diff = man_tgt_rate - zone_tgt_rate,
         ypa_diff = man_ypa - zone_ypa,
         cp_diff = man_cp - zone_cp,
         adot_diff = man_adot - zone_adot,
         qbr_diff = man_qbr - zone_qbr,
         td_diff = man_tds - zone_tds) %>%
  select(player, player_season_id, tgt_rate_diff:td_diff) %>% ungroup() %>%
  mutate(across(tgt_rate_diff:td_diff, ~ (. - mean(.)) / sd(.), .names = "z_{col}")) %>%
  select(player, player_season_id, z_tgt_rate_diff:z_td_diff) %>% 
  mutate(z_score = z_tgt_rate_diff + .85 * z_ypa_diff + .65 * z_cp_diff + .45 * z_adot_diff + .85 * z_qbr_diff + .2 * z_td_diff,
         z_score_percentile = round(100 * percent_rank(z_score), 0)
  )

lt_wr_man_zone_z_df <- combined_snaps_df %>% filter(man_zone_grp == "LT WR") %>% 
  group_by(player, player_season_id) %>%
  dplyr::summarise(man_plays = sum(man_pass_plays, na.rm = TRUE),
                   zone_plays = sum(zone_pass_plays, na.rm = TRUE),
                   man_tgts = sum(man_targets, na.rm = TRUE),
                   zone_tgts = sum(zone_targets, na.rm = TRUE),
                   man_yards = sum(man_yards, na.rm = TRUE),
                   zone_yards = sum(zone_yards, na.rm = TRUE),
                   man_comp = sum(man_receptions, na.rm = TRUE),
                   zone_comp = sum(zone_receptions, na.rm = TRUE),
                   man_tgt_rate = man_tgts / man_plays,
                   zone_tgt_rate = zone_tgts / zone_plays,
                   man_ypa = man_yards / man_tgts,
                   zone_ypa = zone_yards / zone_tgts,
                   man_cp = man_comp / man_tgts,
                   zone_cp = zone_comp / zone_tgts,
                   man_adot = mean(man_avg_depth_of_target, na.rm = TRUE),
                   zone_adot = mean(zone_avg_depth_of_target, na.rm = TRUE),
                   man_qbr = mean(man_targeted_qb_rating, na.rm = TRUE),
                   zone_qbr = mean(zone_targeted_qb_rating, na.rm = TRUE),
                   man_tds = sum(man_touchdowns, na.rm = TRUE) / sum(man_pass_plays, na.rm = TRUE),
                   zone_tds = sum(zone_touchdowns, na.rm = TRUE) / sum(zone_pass_plays, na.rm = T)) %>% filter(!is.na(man_plays) & !is.na(zone_plays)) %>% drop_na() %>%
  mutate(tgt_rate_diff = man_tgt_rate - zone_tgt_rate,
         ypa_diff = man_ypa - zone_ypa,
         cp_diff = man_cp - zone_cp,
         adot_diff = man_adot - zone_adot,
         qbr_diff = man_qbr - zone_qbr,
         td_diff = man_tds - zone_tds) %>%
  select(player, player_season_id, tgt_rate_diff:td_diff) %>% ungroup() %>%
  mutate(across(tgt_rate_diff:td_diff, ~ (. - mean(.)) / sd(.), .names = "z_{col}")) %>%
  select(player, player_season_id, z_tgt_rate_diff:z_td_diff) %>% 
  mutate(z_score = z_tgt_rate_diff + .85 * z_ypa_diff + .65 * z_cp_diff + .45 * z_adot_diff + .85 * z_qbr_diff + .2 * z_td_diff,
         z_score_percentile = round(100 * percent_rank(z_score), 0)
  )

oth_man_zone_z_df <- combined_snaps_df %>% filter(man_zone_grp == "OTH") %>% 
  group_by(player, player_season_id) %>%
  dplyr::summarise(man_plays = sum(man_pass_plays, na.rm = TRUE),
                   zone_plays = sum(zone_pass_plays, na.rm = TRUE),
                   man_tgts = sum(man_targets, na.rm = TRUE),
                   zone_tgts = sum(zone_targets, na.rm = TRUE),
                   man_yards = sum(man_yards, na.rm = TRUE),
                   zone_yards = sum(zone_yards, na.rm = TRUE),
                   man_comp = sum(man_receptions, na.rm = TRUE),
                   zone_comp = sum(zone_receptions, na.rm = TRUE),
                   man_tgt_rate = man_tgts / man_plays,
                   zone_tgt_rate = zone_tgts / zone_plays,
                   man_ypa = man_yards / man_tgts,
                   zone_ypa = zone_yards / zone_tgts,
                   man_cp = man_comp / man_tgts,
                   zone_cp = zone_comp / zone_tgts,
                   man_adot = mean(man_avg_depth_of_target, na.rm = TRUE),
                   zone_adot = mean(zone_avg_depth_of_target, na.rm = TRUE),
                   man_qbr = mean(man_targeted_qb_rating, na.rm = TRUE),
                   zone_qbr = mean(zone_targeted_qb_rating, na.rm = TRUE),
                   man_tds = sum(man_touchdowns, na.rm = TRUE) / sum(man_pass_plays, na.rm = TRUE),
                   zone_tds = sum(zone_touchdowns, na.rm = TRUE) / sum(zone_pass_plays, na.rm = T)) %>% filter(!is.na(man_plays) & !is.na(zone_plays)) %>% drop_na() %>%
  mutate(tgt_rate_diff = man_tgt_rate - zone_tgt_rate,
         ypa_diff = man_ypa - zone_ypa,
         cp_diff = man_cp - zone_cp,
         adot_diff = man_adot - zone_adot,
         qbr_diff = man_qbr - zone_qbr,
         td_diff = man_tds - zone_tds) %>%
  select(player, player_season_id, tgt_rate_diff:td_diff) %>% ungroup() %>%
  mutate(across(tgt_rate_diff:td_diff, ~ (. - mean(.)) / sd(.), .names = "z_{col}")) %>%
  select(player, player_season_id, z_tgt_rate_diff:z_td_diff) %>% 
  mutate(z_score = z_tgt_rate_diff + .85 * z_ypa_diff + .65 * z_cp_diff + .45 * z_adot_diff + .85 * z_qbr_diff + .2 * z_td_diff,
         z_score_percentile = round(100 * percent_rank(z_score), 0)
  )


man_zone_z_score_df <- 
      rbind(rb_man_zone_z_df %>% select(player_season_id, z_score_percentile), 
      deep_man_zone_z_df %>% select(player_season_id, z_score_percentile),
      g_man_zone_z_df %>% select(player_season_id, z_score_percentile),
      te_man_zone_z_df %>% select(player_season_id, z_score_percentile),
      ml_man_zone_z_df %>% select(player_season_id, z_score_percentile),
      lt_wr_man_zone_z_df %>% select(player_season_id, z_score_percentile),
      oth_man_zone_z_df %>% select(player_season_id, z_score_percentile))


combined_snaps_df <- left_join(combined_snaps_df, man_zone_z_score_df, by = "player_season_id")




#####
#####
#####


attach("NEW_pbp_part_combined_join_workspace.RData")

combined_pbp_info <- combined_info

detach("file:NEW_pbp_part_combined_join_workspace.RData")

# MIXED IS BROKEN - THE MISSING PART SHIT ARE ALL 0'D, NOT NA'D

pbp_receiver_stats <- combined_pbp_info %>% 
  filter(!is.na(yards_gained) & play_type == "pass" & pass_attempt == 1 & sack == 0 & !is.na(air_yards)) %>%
  group_by(receiver_id, receiver_player_name, posteam, week, season, qbgrp_ssn, def_ssn) %>%
  dplyr::summarise(pbp_rec_xtds = sum(after_xtd_new, na.rm = T),
                   part_rec_xtds = sum(part_after_xtd_new, na.rm = T),
                   fastr_cp = mean(cp, na.rm = T),
                   pbp_cp = mean(predicted_cp, na.rm = T),
                   part_cp = mean(part_predicted_cp, na.rm = T),
                   ypa = mean(yards_gained, na.rm = T),
                   pbp_xypa = mean(predicted_ypa, na.rm = T),
                   part_xypa = mean(part_predicted_ypa, na.rm = T),
                   yac = mean(yards_after_catch, na.rm = T),
                   pbp_yac = mean(predicted_yac, na.rm = T),
                   part_yac = mean(part_predicted_yac, na.rm = T),
                   adot = mean(air_yards, na.rm = T)
                   )
  
  
pbp_receiver_stats2 <- left_join(pbp_receiver_stats, combined_ids_final %>% select(player_id, team, Week, Season, player_name_join, gsis_id_final) %>% distinct(), by = c("receiver_id" = "gsis_id_final", "week" = "Week", "season" = "Season", "posteam" = "team"))


combined_snaps_df <- combined_snaps_df %>%
  group_by(game_id, team) %>%
  mutate(max_snaps = max(snap_counts_total, na.rm = TRUE)) %>%
  ungroup()


receiver_man_zone_stats <- combined_snaps_df %>% 
  group_by(player, player_id, week, team, season, tgt_cluster, rte_cluster, align_cluster, position_group, pos_rank, team_rank, man_zone_grp, z_score_percentile, max_snaps) %>%
  dplyr::summarise(man_routes = sum(man_routes, na.rm = T),
                   man_targets = sum(man_targets, na.rm = T),
                   man_yards = sum(man_yards, na.rm = T),
                   man_receptions = sum(man_receptions, na.rm = T),
                   man_yards_after_catch = sum(man_yards_after_catch, na.rm = T),
                   zone_routes = sum(zone_routes, na.rm = T),
                   zone_targets = sum(zone_targets, na.rm = T),
                   zone_yards = sum(zone_yards, na.rm = T),
                   zone_receptions = sum(zone_receptions, na.rm = T),
                   zone_yards_after_catch = sum(zone_yards_after_catch, na.rm = T)
                   ) %>%
  mutate(man_route_perc = man_routes / (man_routes + zone_routes),
         man_tgt_perc = man_targets / (man_targets + zone_targets),
         man_ypa = man_yards / man_targets,
         man_cp = man_receptions / man_targets,
         man_yac = man_yards_after_catch / man_receptions,
         zone_ypa = zone_yards / zone_targets,
         zone_cp = zone_receptions / zone_targets,
         zone_yac = zone_yards_after_catch / zone_receptions)


receiver_depth_stats <- receiver_depth %>% group_by(player, player_id, Week, team, Season) %>% 
  dplyr::summarise(behind_los_routes = sum(left_behind_los_routes, na.rm = T) + sum(center_behind_los_routes, na.rm = T) + sum(right_behind_los_routes, na.rm = T),
                   short_routes = sum(left_short_routes, na.rm = T) + sum(center_short_routes, na.rm = T) + sum(right_short_routes, na.rm = T),
                   medium_routes = sum(left_medium_routes, na.rm = T) + sum(center_medium_routes, na.rm = T) + sum(right_medium_routes, na.rm = T),
                   deep_routes = sum(left_deep_routes, na.rm = T) + sum(center_deep_routes, na.rm = T) + sum(right_deeproutes, na.rm = T),
                   base_routes = behind_los_routes + short_routes + medium_routes + deep_routes,
                   behind_los_targets = sum(left_behind_los_targets, na.rm = T) + sum(center_behind_los_targets, na.rm = T) + sum(right_behind_los_targets, na.rm = T),
                   short_targets = sum(left_short_targets, na.rm = T) + sum(center_short_targets, na.rm = T) + sum(right_short_targets, na.rm = T),
                   medium_targets = sum(left_medium_targets, na.rm = T) + sum(center_medium_targets, na.rm = T) + sum(right_medium_targets, na.rm = T),
                   deep_targets = sum(left_deep_targets, na.rm = T) + sum(center_deep_targets, na.rm = T) + sum(right_deeptargets, na.rm = T),
                   base_targets = sum(base_targets, na.rm = T), 
                   behind_los_yards = sum(behind_los_yards, na.rm = T),
                   short_yards = sum(short_yards, na.rm = T),
                   medium_yards = sum(medium_yards, na.rm = T),
                   deep_yards = sum(deep_yards, na.rm = T),
                   yards = behind_los_yards + short_yards + medium_yards + deep_yards,
                   behind_los_receptions = sum(behind_los_receptions, na.rm = T), 
                   short_receptions = sum(short_receptions, na.rm = T),
                   medium_receptions = sum(medium_receptions, na.rm = T),
                   deep_receptions = sum(deep_receptions, na.rm = T),
                   receptions = behind_los_receptions + short_receptions + medium_receptions + deep_receptions,
                   behind_los_yards_after_catch = sum(behind_los_yards_after_catch, na.rm = T),
                   short_yards_after_catch = sum(short_yards_after_catch, na.rm = T),
                   medium_yards_after_catch = sum(medium_yards_after_catch, na.rm = T),
                   deep_yards_after_catch = sum(deep_yards_after_catch, na.rm = T),
                   behind_route_pct = behind_los_routes / base_routes,
                   short_route_pct = short_routes / base_routes,
                   medium_route_pct = medium_routes / base_routes,
                   deep_route_pct = deep_routes / base_routes,
                   behind_tgt_pct = behind_los_targets / base_targets,
                   short_tgt_pct = short_targets / base_targets,
                   medium_tgt_pct = medium_targets / base_targets,
                   deep_tgt_pct = deep_targets / base_targets,
                   behind_los_ypa = behind_los_yards / behind_los_targets,
                   short_ypa = short_yards / short_targets,
                   medium_ypa = medium_yards / medium_targets,
                   deep_ypa = deep_yards / deep_targets,
                   behind_los_yac = behind_los_yards_after_catch / behind_los_targets,
                   short_yac = short_yards_after_catch / short_targets,
                   medium_yac = medium_yards_after_catch / medium_targets,
                   deep_yac = deep_yards_after_catch / deep_targets,
                   behind_cp = behind_los_receptions / behind_los_targets,
                   short_cp = short_receptions / short_targets,
                   medium_cp = medium_receptions / medium_targets,
                   deep_cp = deep_receptions / deep_targets) 



combined_rec_stats_join_one <- left_join(pbp_receiver_stats2, receiver_depth_stats, by = c("player_id" = "player_id", "posteam" = "team", "week" = "Week", "season" = "Season"))

combined_rec_stats_final <- left_join(combined_rec_stats_join_one, receiving_stats_alignment, by = c("player_id" = "player_id", "posteam" = "team", "week" = "Week", "season" = "Season"))

combined_rec_stats_final <- combined_rec_stats_final %>% select(-player.y)
colnames(combined_rec_stats_final)[19] <- "player"


combined_rec_stats_final <- left_join(combined_rec_stats_final, receiver_man_zone_stats, by = c("player_id" = "player_id", "week" = "week", "season" = "season"))

colnames(combined_rec_stats_final)[18] <- "part_yac"

# GO BACK ---> THE ONES THAT ARE 0 THAT SHOULD BE NA --> GOTTA MAKE THAT FIX. FIND SOMETHING WHERE THERE IS NO CHANCE OF 0, KEEP THAT AS INDEX, FIX THE REST


combined_rec_stats_final <- combined_rec_stats_final %>% filter(!is.na(receiver_id))


combined_rec_stats_final <- combined_rec_stats_final %>%
  group_by(qbgrp_ssn, def_ssn, posteam, week, season) %>%
  mutate(
    total_routes = sum(base_routes, na.rm = T),
    total_targets = sum(base_targets, na.rm = T),
    route_share = base_routes / sum(base_routes, na.rm = TRUE),
    target_share = base_targets / sum(base_targets, na.rm = TRUE),
    pbp_td_share = pbp_rec_xtds / sum(pbp_rec_xtds, na.rm = TRUE),
    part_td_share = part_rec_xtds / sum(part_rec_xtds, na.rm = T),
    acc_rate = sum(receptions, na.rm = T) / sum(base_targets, na.rm = T)
  ) %>%
  ungroup()

combined_rec_stats_final$game_id = paste0(combined_rec_stats_final$posteam, combined_rec_stats_final$week, combined_rec_stats_final$season)

