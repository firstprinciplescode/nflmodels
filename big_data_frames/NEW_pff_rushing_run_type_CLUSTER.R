
#### THIS SHIT WILL BE FOR GAP / ZONE TYPE SHIT

### LET'S WORK ON SOME SHIT TYPE SHIT


rushing_stats_rank_grp <- left_join(rushing_stats, rush_order_df %>% select(player_id, season, week, rank_grp), by = c("player_id" = "player_id", "Season" = "season", "Week" = "week"))


rushing_stats_rank_grp %>% filter(is.na(rank_grp)) %>% filter(designed_yards > 0)
# LOTS OF KNEELS - LET'S SEE WHAT > 0 YARDS IS
# ACTUALLY I DON'T EVEN KNOW ABOUT THAT POINT
# JACQUIZZ RODGERS WAS RB1 IN HIS GAME, WK 2 2017. NO ONE IN THAT GAME HAD A RANK_GRP
# THEY'RE ALL 0 GAP + 0 ZONE ATTEMPTS. HONESTLY WHO CARES?! I FORGET IF THIS SHIT IS HOLISTIC


sqldf("SELECT team, Week, Season, COUNT(DISTINCT player) AS players
       FROM   rushing_stats_rank_grp
       WHERE  rank_grp IS NULL
       GROUP BY   team, Week, Season
       HAVING players > 1
       ORDER BY Week, Season")



rushing_stats_rank_grp %>% filter(Week == 7 & Season == 2023 & team == "IND")
# TB 2 2017 NO ONE, SAME WITH CHI 2 2017, BUT QUITE SURE THIS IS ONLY GAME WITH 0 INFO
# WRONG - THERE IS ANOTHER ONE - TB / CIN 8 2018
# HOWEVER, THINK THOSE TWO ARE THE ONLY TWO GAMES

# SO FAR ... THIS IS DUE TO SHITTY PFF DATA PULLS. WE REGOT THIS DATA, THE SHIT WORKED I THINK
# QUITE SURE IT WORKED. LET'S MOVE ON. LOCATION DATA (GUARD / TACKLE / END), ZONE / GAP


## THE BASE DATA IS SHIT. SOMEHOW RUN COUNTS IN SNAPS IS NOT THE SAME AS IN THE RUSHING SUMMARY, EVEN FOR DESIGNED RUNS FFS. 

rushing_stats_rank_grp %>%
  filter(rank_grp == "A") %>%
  group_by(player, player_id, team, Season) %>%
  summarise(
    zone_attempts = sum(zone_attempts, na.rm = TRUE),
    gap_attempts = sum(gap_attempts, na.rm = TRUE),
    attempts = sum(attempts, na.rm = TRUE),
    .groups = "drop" # Avoid the grouping message
  ) %>%
  filter(attempts >= 25) %>%
  mutate(zone_perc = zone_attempts / (gap_attempts + zone_attempts)) %>%
  ggplot(aes(x = "", y = zone_perc)) +
  geom_violin(trim = FALSE) +
  labs(
    title = "Distribution of Zone Percentage (zone_perc)",
    x = "Zone Attempts",
    y = "Zone Percentage"
  ) +
  theme_minimal()


rushing_stats_rank_grp %>%
  filter(rank_grp == "A") %>%
  group_by(player, player_id, team, Season) %>%
  summarise(
    zone_attempts = sum(zone_attempts, na.rm = TRUE),
    gap_attempts = sum(gap_attempts, na.rm = TRUE),
    attempts = sum(attempts, na.rm = TRUE),
    .groups = "drop" # Avoid the grouping message
  ) %>%
  filter(attempts >= 25) %>%
  mutate(zone_perc = zone_attempts / (gap_attempts + zone_attempts)) %>% ungroup() %>%
  summarise(percentile_15 = quantile(zone_perc, probs = 0.15, na.rm = TRUE)
            ,percentile_25 = quantile(zone_perc, probs = 0.25, na.rm = TRUE)
            ,percentile_35 = quantile(zone_perc, probs = 0.35, na.rm = TRUE)
            ,percentile_45 = quantile(zone_perc, probs = 0.45, na.rm = TRUE)
            ,percentile_55 = quantile(zone_perc, probs = 0.55, na.rm = TRUE)
            ,percentile_65 = quantile(zone_perc, probs = 0.65, na.rm = TRUE)
            ,percentile_75 = quantile(zone_perc, probs = 0.75, na.rm = TRUE)
            ,percentile_85 = quantile(zone_perc, probs = 0.85, na.rm = TRUE))


rushing_stats_rank_grp %>% 
  group_by(team, Season) %>%
  summarise(
    zone_attempts = sum(zone_attempts, na.rm = TRUE),
    gap_attempts = sum(gap_attempts, na.rm = TRUE),
    attempts = sum(attempts, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(zone_perc = zone_attempts / (gap_attempts + zone_attempts)) %>%
  arrange(Season)


rushing_stats_rank_grp$team[which(rushing_stats_rank_grp$team == "SD")] <- "LAC"
rushing_stats_rank_grp$team_name[which(rushing_stats_rank_grp$team_name == "SD")] <- "LAC"

rushing_stats_rank_grp$team[which(rushing_stats_rank_grp$team == "OAK")] <- "LV"
rushing_stats_rank_grp$team_name[which(rushing_stats_rank_grp$team_name == "OAK")] <- "LV"



# DISTRIBUTION IS PRETTY EVEN - PROBABLY NOT BIGLY IMPORTANT ... 25TH PERCENTILE AT 47%, 75TH AT 68. VIOLIN PLOT VERY CENTER HEAVY. I WANT TO SEE IF IT'S "CONSISTENT" --> SAME OC FAMILY / HC FAMILY SHARE THE SAME GENERAL RANGE.
# NOTE THIS SHIT ISN'T "INCLUSIVE" - NOT ALL RUNS ARE ZONE / GAP ... IK KNEELS AREN'T INCLUDED BUT STILL THERE ARE GAPS.


rushing_stats_rank_grp <- left_join(rushing_stats_rank_grp, pbp_rush_info %>% ungroup() %>% select(posteam, week, season, qbgrp_ssn, def_ssn) %>% distinct(), by = c("team_name" = "posteam", "Week" = "week", "Season" = "season"))


rushing_stats_rank_grp %>% 
  group_by(qbgrp_ssn) %>%
  summarise(
    zone_attempts = sum(zone_attempts, na.rm = TRUE),
    gap_attempts = sum(gap_attempts, na.rm = TRUE),
    atts = zone_attempts + gap_attempts,
    .groups = "drop"
  ) %>%
  filter(atts > 70) %>%
  mutate(zone_perc = zone_attempts / (gap_attempts + zone_attempts)) %>%
  ggplot(aes(x = zone_perc)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1) +
  labs(
    title = "Distribution of Zone Percentage with Density",
    x = "Zone Percentage",
    y = "Density"
  ) +
  theme_minimal()
# OFC - NORMAL DISTRIBUTION


rushing_stats_rank_grp %>% 
  group_by(qbgrp_ssn) %>%
  summarise(
    zone_attempts = sum(zone_attempts, na.rm = TRUE),
    gap_attempts = sum(gap_attempts, na.rm = TRUE),
    atts = zone_attempts + gap_attempts,
    .groups = "drop"
  ) %>%
  filter(atts > 70) %>%
  mutate(zone_perc = zone_attempts / (gap_attempts + zone_attempts),
         zone_perc_z_score = scale(zone_perc, center = TRUE, scale = TRUE),
         mean_zone_perc = mean(zone_perc),
         sd_zone_perc = sd(zone_perc)) %>%
  arrange(desc(atts))


rushing_stats_rank_grp %>% 
  group_by(def_ssn) %>%
  summarise(
    zone_attempts = sum(zone_attempts, na.rm = TRUE),
    gap_attempts = sum(gap_attempts, na.rm = TRUE),
    atts = zone_attempts + gap_attempts,
    .groups = "drop"
  ) %>%
  filter(atts > 70) %>%
  mutate(zone_perc = zone_attempts / (gap_attempts + zone_attempts)) %>%
  summarise(mean_zone_perc = mean(zone_perc),
            sd_zone_perc = sd(zone_perc))


rushing_stats_rank_grp %>% filter(def_ssn == "LAC2021") %>% 
  group_by(Week) %>% 
  summarise(
    zone_attempts = sum(zone_attempts, na.rm = TRUE),
    gap_attempts = sum(gap_attempts, na.rm = TRUE),
    atts = zone_attempts + gap_attempts,
    .groups = "drop"
  ) %>%
  mutate(zone_perc = zone_attempts / (gap_attempts + zone_attempts),
         zone_perc_z_score = (zone_perc - .566) / .059) %>%
  arrange(zone_perc_z_score)


# 1.2 IS IT FOR INTERVAL
# +/- .6, GET THE Z-SCORE WITH THE ABOVE MEAN / SD


zone_gap_qbgrp_ssn <- rushing_stats_rank_grp %>% 
  group_by(qbgrp_ssn) %>%
  summarise(
    zone_attempts = sum(zone_attempts, na.rm = TRUE),
    gap_attempts = sum(gap_attempts, na.rm = TRUE),
    atts = zone_attempts + gap_attempts,
    .groups = "drop"
  ) %>%
  mutate(zone_perc = zone_attempts / (gap_attempts + zone_attempts),
         zone_perc_z_score = (zone_perc - .571) / .136)


rushing_stats_rank_grp # THIS SHIT IS THE BASE #1

pbp_rushing_stats_base <- pbp_rush_info %>% 
  group_by(rusher_player_id, rusher_player_name, posteam, week, season, rank_grp, qbgrp_ssn, def_ssn) %>%
  dplyr::summarise(rushes = n(),
                   ypc = mean(yards_gained, na.rm = T),
                   pbp_xypc = mean(predicted_ypc, na.rm = T),
                   part_xypc = mean(part_predicted_ypc, na.rm = T),
                   pbp_xtds = sum(after_xtd_new, na.rm = T),
                   part_xtds = sum(part_after_xtd_new, na.rm = T))

# MY PBP_YPC MODEL IS FUCKED. DON'T KNOW WHY BUT NOT GOOD FOR SURE. DO I GO BACK AND TRY TO FIX IT? I'M GOING TO HAVE TO LOOKS LIKE


combined_rush_summation_all_rank_grp # HERE - WHEN WE REGENERATE ALL THAT SHIT --> WE'RE GOING TO HAVE TO RENAME, THEN ATTACH THOSE NAMES IN PLACE / IN ADDITION TO THE CLUSTER NUMBERS


pbp_rushing_stats_base_intermediaryone <- left_join(pbp_rushing_stats_base %>% distinct(), combined_rush_summation_all_rank_grp %>% select(rusher_player_name:rank_grp, cluster) %>% distinct(), by = c("rusher_player_id", "rusher_player_name", "posteam", "season", "rank_grp")) %>% distinct()


pbp_rushing_stats_base_intermediarytwo <- left_join(pbp_rushing_stats_base_intermediaryone, post_base_run_gap_cluster %>% select(-total_rushes), by = c("rusher_player_id", "rusher_player_name", "posteam", "season"))


zone_gap_qbgrp_ssn

rushing_stats_final <- left_join(pbp_rushing_stats_base_intermediarytwo, zone_gap_qbgrp_ssn %>% select(qbgrp_ssn, zone_perc, zone_perc_z_score), by = c("qbgrp_ssn"))

colnames(rushing_stats_final)[15] <- "run_situation_cluster"
colnames(rushing_stats_final)[20] <- "run_gap_cluster"


rushing_stats_final$part_xtds[which(rushing_stats_final$season == 2024)] <- NA
rushing_stats_final$part_xypc[which(rushing_stats_final$season == 2024)] <- NA


# ABOVE ARE ALL THE JOINS --> WE AIN'T DOING SHIIT WITH DEFENSE. DOESN'T MATTER REGARDING Z/G


rushing_stats_final <- rushing_stats_final %>% group_by(posteam, week, season) %>% 
  mutate(team_rushes = sum(rushes, na.rm = T)) %>% ungroup()

rushing_stats_final <- rushing_stats_final %>% group_by(posteam, week, season) %>% 
  mutate(team_pbp_xtds = sum(pbp_xtds, na.rm = T)) %>% ungroup()

rushing_stats_final <- rushing_stats_final %>% group_by(posteam, week, season) %>% 
  mutate(team_part_xtds = sum(part_xtds, na.rm = T)) %>% ungroup()

rushing_stats_final$team_part_xtds[which(rushing_stats_final$season == 2024)] <- NA


rushing_stats_final$game_id <- paste0(rushing_stats_final$posteam, rushing_stats_final$week, rushing_stats_final$season)

