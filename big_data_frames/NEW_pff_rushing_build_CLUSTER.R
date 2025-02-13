rusher_xpass_diff_a_pre_clust <- rusher_xpass_diff_df %>% ungroup() %>% filter(rank_grp == "A") %>% select(q20_xpass_diff:q80_xpass_diff) %>% mutate(across(everything(), ~ scale(.)))

rusher_xpass_diff_b_pre_clust <- rusher_xpass_diff_df %>% ungroup() %>% filter(rank_grp == "B") %>% select(q20_xpass_diff:q80_xpass_diff) %>% mutate(across(everything(), ~ scale(.)))

rusher_xpass_diff_c_pre_clust <- rusher_xpass_diff_df %>% ungroup() %>% filter(rank_grp == "C") %>% select(q20_xpass_diff:q80_xpass_diff) %>% mutate(across(everything(), ~ scale(.)))


k <- 9
kmeans_rusher_xpass_diff_a_combined <- kmeans(rusher_xpass_diff_a_pre_clust, centers = k, nstart = 25)

kmeans_rusher_xpass_diff_a_combined

# 2 - .312 - .312
# 3 - .446 - .134
# 4 - .54 - .098
# 5 - .594 - .054
# 6 - .636 - .042
# 7 - .668 - .032
# 8 - .69 - .022
# 9 - .71 - .02
# 10 - .726 - .016
# 11 - .743 - .017


rusher_xpass_diff_a_post_cluster <- cbind(rusher_xpass_diff_df %>% ungroup() %>% filter(rank_grp == "A") %>% select(rusher_player_name, rusher_player_id, posteam, season), rusher_xpass_diff_a_pre_clust, kmeans_rusher_xpass_diff_a_combined$cluster)
colnames(rusher_xpass_diff_a_post_cluster)[10] <- "cluster"

rusher_xpass_diff_a_post_cluster_grps <- rusher_xpass_diff_a_post_cluster %>% group_by(cluster) %>% dplyr::summarize(q20 = mean(q20_xpass_diff), q35 = mean(q35_xpass_diff), q50 = mean(q50_xpass_diff), q65 = mean(q65_xpass_diff), q80 = mean(q80_xpass_diff), n = n())


rusher_xpass_diff_a_post_cluster_grps

rusher_xpass_diff_a_post_cluster %>% filter(cluster == 1)
rusher_xpass_diff_a_post_cluster %>% filter(cluster == 2)


# IT'S BEEN A MINUTE ... LET'S LOCK IN TO GET BACK TO THE SPOT WHERE I KNEW WTF I WANTED LMAO


colnames(pbp_rush_info)

unique(pbp_rush_info$ydstogo_group)

# THIS SHIT DON'T WORK - I NEED TO PROBABLY BREAK DOWN STEP BY STEP


player_rush_summations <- sqldf("SELECT rusher_player_name, rusher_player_id, posteam, week, season, game_id, rank_grp,
        COALESCE(SUM(CASE WHEN ydstogo_group == '1st9' THEN 1 END), 0) AS First9Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '1st10+' THEN 1 END), 0) AS First10Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '2nd1' THEN 1 END), 0) AS Second1Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '2nd2-4' THEN 1 END), 0) AS Second2to4Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '2nd5-7' THEN 1 END), 0) AS Second5to7Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '2nd8-10' THEN 1 END), 0) AS Second8to10Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '2ndLong' THEN 1 END), 0) AS SecondLongRushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '3rd1' THEN 1 END), 0) AS Third1Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '3rd2-3' THEN 1 END), 0) AS Third2to3Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '3rdRest' THEN 1 END), 0) AS ThirdRestRushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '4th1' THEN 1 END), 0) AS Fourth1Rushes,       
        COALESCE(SUM(CASE WHEN ydstogo_group == '4thLong' THEN 1 END), 0) AS FourthLongRushes
      FROM  pbp_rush_info
      GROUP BY rusher_player_name, rusher_player_id, posteam, week, season, game_id, rank_grp")


game_rush_summations <- sqldf("SELECT posteam, game_id,
        COALESCE(SUM(CASE WHEN ydstogo_group == '1st9' THEN 1 END), 0) AS First9Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '1st10+' THEN 1 END), 0) AS First10Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '2nd1' THEN 1 END), 0) AS Second1Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '2nd2-4' THEN 1 END), 0) AS Second2to4Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '2nd5-7' THEN 1 END), 0) AS Second5to7Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '2nd8-10' THEN 1 END), 0) AS Second8to10Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '2ndLong' THEN 1 END), 0) AS SecondLongRushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '3rd1' THEN 1 END), 0) AS Third1Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '3rd2-3' THEN 1 END), 0) AS Third2to3Rushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '3rdRest' THEN 1 END), 0) AS ThirdRestRushes,
        COALESCE(SUM(CASE WHEN ydstogo_group == '4th1' THEN 1 END), 0) AS Fourth1Rushes,       
        COALESCE(SUM(CASE WHEN ydstogo_group == '4thLong' THEN 1 END), 0) AS FourthLongRushes
      FROM  pbp_rush_info
      GROUP BY posteam, game_id")


combined_rush_summations <- sqldf("SELECT play.*,
                                          game.First9Rushes AS GameFirst9Rushes,
                                          game.First10Rushes AS GameFirst10Rushes,
                                          game.Second1Rushes AS GameSecond1Rushes,
                                          game.Second2to4Rushes AS GameSecond2to4Rushes,
                                          game.Second5to7Rushes AS GameSecond5to7Rushes,
                                          game.Second8to10Rushes AS GameSecond8to10Rushes,
                                          game.SecondLongRushes AS GameSecondLongRushes,
                                          game.Third1Rushes AS GameThird1Rushes,
                                          game.Third2to3Rushes AS GameThird2to3Rushes,
                                          game.ThirdRestRushes AS GameThirdRestRushes,
                                          game.Fourth1Rushes AS GameFourth1Rushes,
                                          game.FourthLongRushes AS GameFourthLongRushes
                                  FROM  player_rush_summations  play
                                  INNER JOIN  game_rush_summations  game
                                  ON  play.posteam = game.posteam
                                  AND play.game_id = game.game_id")

combined_rush_summation_final <- 
  sqldf("SELECT rusher_player_name, 
                rusher_player_id, 
                posteam,
                week,
                season,
                game_id,
                rank_grp,
                CASE WHEN GameFirst9Rushes = 0 THEN NULL ELSE First9Rushes / GameFirst9Rushes END AS First9Ratio,
                CASE WHEN GameFirst10Rushes = 0 THEN NULL ELSE First10Rushes / GameFirst10Rushes END AS First10Ratio,
                CASE WHEN GameSecond1Rushes = 0 THEN NULL ELSE Second1Rushes / GameSecond1Rushes END AS Second1Ratio,
                CASE WHEN GameSecond2to4Rushes = 0 THEN NULL ELSE Second2to4Rushes / GameSecond2to4Rushes END AS Second2to4Ratio,
                CASE WHEN GameSecond5to7Rushes = 0 THEN NULL ELSE Second5to7Rushes / GameSecond5to7Rushes END AS Second5to7Ratio,
                CASE WHEN GameSecond8to10Rushes = 0 THEN NULL ELSE Second8to10Rushes / GameSecond8to10Rushes END AS Second8to10Ratio,
                CASE WHEN GameSecondLongRushes = 0 THEN NULL ELSE SecondLongRushes / GameSecondLongRushes END AS SecondLongRatio,
                CASE WHEN GameThird1Rushes = 0 THEN NULL ELSE Third1Rushes / GameThird1Rushes END AS Third1Ratio,
                CASE WHEN GameThird2to3Rushes = 0 THEN NULL ELSE Third2to3Rushes / GameThird2to3Rushes END AS Third2to3Ratio,
                CASE WHEN GameThirdRestRushes = 0 THEN NULL ELSE ThirdRestRushes / GameThirdRestRushes END AS ThirdRestRatio,
                CASE WHEN GameFourth1Rushes = 0 THEN NULL ELSE Fourth1Rushes / GameFourth1Rushes END AS Fourth1Ratio,
                CASE WHEN GameFourthLongRushes = 0 THEN NULL ELSE FourthLongRushes / GameFourthLongRushes END AS FourthLongRatio
          FROM  combined_rush_summations
        ")


combined_rush_summation_game <- 
  sqldf("SELECT rusher_player_name, 
                rusher_player_id, 
                posteam,
                week,
                season,
                game_id,
                rank_grp,
                CASE WHEN GameFirst9Rushes = 0 THEN NULL ELSE CAST(First9Rushes AS FLOAT) / GameFirst9Rushes END AS First9Ratio,
                CASE WHEN GameFirst10Rushes = 0 THEN NULL ELSE CAST(First10Rushes AS FLOAT) / GameFirst10Rushes END AS First10Ratio,
                CASE WHEN GameSecond1Rushes = 0 THEN NULL ELSE CAST(Second1Rushes AS FLOAT) / GameSecond1Rushes END AS Second1Ratio,
                CASE WHEN GameSecond2to4Rushes = 0 THEN NULL ELSE CAST(Second2to4Rushes AS FLOAT) / GameSecond2to4Rushes END AS Second2to4Ratio,
                CASE WHEN GameSecond5to7Rushes = 0 THEN NULL ELSE CAST(Second5to7Rushes AS FLOAT) / GameSecond5to7Rushes END AS Second5to7Ratio,
                CASE WHEN GameSecond8to10Rushes = 0 THEN NULL ELSE CAST(Second8to10Rushes AS FLOAT) / GameSecond8to10Rushes END AS Second8to10Ratio,
                CASE WHEN GameSecondLongRushes = 0 THEN NULL ELSE CAST(SecondLongRushes AS FLOAT) / GameSecondLongRushes END AS SecondLongRatio,
                CASE WHEN GameThird1Rushes = 0 THEN NULL ELSE CAST(Third1Rushes AS FLOAT) / GameThird1Rushes END AS Third1Ratio,
                CASE WHEN GameThird2to3Rushes = 0 THEN NULL ELSE CAST(Third2to3Rushes AS FLOAT) / GameThird2to3Rushes END AS Third2to3Ratio,
                CASE WHEN GameThirdRestRushes = 0 THEN NULL ELSE CAST(ThirdRestRushes AS FLOAT) / GameThirdRestRushes END AS ThirdRestRatio,
                CASE WHEN GameFourth1Rushes = 0 THEN NULL ELSE CAST(Fourth1Rushes AS FLOAT) / GameFourth1Rushes END AS Fourth1Ratio,
                CASE WHEN GameFourthLongRushes = 0 THEN NULL ELSE CAST(FourthLongRushes AS FLOAT) / GameFourthLongRushes END AS FourthLongRatio
          FROM combined_rush_summations
        ")


combined_rush_summation_final <- 
  sqldf("SELECT rusher_player_name
                ,rusher_player_id
                ,posteam
                ,season
                ,rank_grp
                ,AVG(First9Ratio) AS First9Ratio
                ,AVG(First10Ratio) AS First10Ratio
                ,AVG(Second1Ratio) AS Second1Ratio
                ,AVG(Second2to4Ratio) AS Second2to4Ratio
                ,AVG(Second5to7Ratio) AS Second5to7Ratio
                ,AVG(Second8to10Ratio) AS Second8to10Ratio
                ,AVG(SecondLongRatio) AS SecondLongRatio
                ,AVG(Third1Ratio) AS Third1Ratio
                ,AVG(Third2to3Ratio) AS Third2to3Ratio
                ,AVG(ThirdRestRatio) AS ThirdRestRatio
                ,AVG(Fourth1Ratio) AS Fourth1Ratio  
                ,AVG(FourthLongRatio) AS FourthLongRatio                  
         FROM   combined_rush_summation_game 
         GROUP BY   rusher_player_name, rusher_player_id, posteam, season, rank_grp
        ")


combined_rush_summation_final_a <- combined_rush_summation_final %>% filter(rank_grp == "A")

combined_rush_summation_final_a <- left_join(combined_rush_summation_final_a, rusher_xpass_diff_a_post_cluster %>% select(rusher_player_name:season, cluster), by = c("rusher_player_name", "rusher_player_id", "posteam", "season"))


a_cluster_mean_values <- combined_rush_summation_final_a %>% ungroup() %>% 
  group_by(cluster) %>% 
  dplyr::summarise(mn_First9Ratio = mean(First9Ratio, na.rm = T),
                   mn_First10Ratio = mean(First10Ratio, na.rm = T),
                   mn_Second1Ratio = mean(Second1Ratio, na.rm = T),
                   mn_Second2to4Ratio = mean(Second2to4Ratio, na.rm = T),
                   mn_Second5to7Ratio = mean(Second5to7Ratio, na.rm = T),
                   mn_Second8to10Ratio = mean(Second8to10Ratio, na.rm = T),
                   mn_SecondLongRatio = mean(SecondLongRatio, na.rm = T),
                   mn_Third1Ratio = mean(Third1Ratio, na.rm = T),
                   mn_Third2to3Ratio = mean(Third2to3Ratio, na.rm = T),
                   mn_ThirdRestRatio = mean(ThirdRestRatio, na.rm = T),
                   mn_Fourth1Ratio = mean(Fourth1Ratio, na.rm = T),
                   mn_FourthLongRatio = mean(FourthLongRatio, na.rm = T),
                   n = n())

a_cluster_mean_percs <- a_cluster_mean_values %>%
  filter(!is.na(cluster)) %>%
  mutate(across(mn_First9Ratio:mn_FourthLongRatio, 
                ~ rank(.x, na.last = "keep", ties.method = "min") / n() - (1 / n())))




pbp_rush_info %>% group_by(ydstogo_group) %>% dplyr::summarise(n = n()) %>% mutate(proportion = n / sum(n))


combined_rush_summation_final_a %>% filter(cluster == 1) %>% select(First10Ratio) %>% filter(!is.na(First10Ratio))

combined_rush_summation_final_a %>% filter(cluster == 2) %>% select(First10Ratio) %>% filter(!is.na(First10Ratio))



# SOME CLEAR DIFFERENCES ... SOME NOT SO CLEAR. NEEDS A LOT OF INVESTIGATION
# SOME T-TEST TYPE SHIT LIKELY
# ALSO SOME SHIT IS NA? HOW THE FUCK DID THIS SHIT HAPPEN AGAIN


# E.G. OF NA - JALEN HURTS, 2021 - ONE GAME, BACK WHEN SIRIANNI NEVER RAN THE BALL LOL



ydstogo_groups <- colnames(combined_rush_summation_final_a)[c(6:17)]

a_t_test_results <- data.frame(cluster = numeric(), ydstogo_group = character(), p_value = numeric())

for (cl in seq(1:9)) {
  for (group in ydstogo_groups) {
    # Data for the current cluster
    cluster_data <- combined_rush_summation_final_a %>% 
      filter(cluster == cl) %>% 
      pull(!!sym(group)) %>% 
      na.omit()
    
    # Data for all other clusters
    non_cluster_data <- combined_rush_summation_final_a %>% 
      filter(cluster != cl) %>% 
      pull(!!sym(group)) %>% 
      na.omit()
    
    # Perform t-test
    if (length(cluster_data) > 1 && length(non_cluster_data) > 1) { # Ensure enough data for t-test
      t_test <- t.test(cluster_data, non_cluster_data)
      p_value <- t_test$p.value
    } else {
      p_value <- NA # Not enough data
    }
    
    # Append the result to the data frame
    a_t_test_results <- rbind(a_t_test_results, data.frame(cluster = cl, ydstogo_group = group, p_value = p_value))
  }
}

a_t_test_results <- a_t_test_results %>% 
  mutate(p_value = round(p_value, 3))

# Pivot the results for a cleaner view
a_t_test_results <- a_t_test_results %>% 
  pivot_wider(names_from = ydstogo_group, values_from = p_value)

# View the final results
print(a_t_test_results)


# CLUSTER 1 - SHORT-YARDAGE BACKS
# CLUSTER 2 - THIRD DOWN BACKS, SOME SHORT YARDAGE
# CLUSTER 3 - LONG-YARDAGE BACKS
# CLUSTER 4 - 2ND DOWN BACKS? KINDA AVERAGE
# CLUSTER 5 - LOW USAGE A
# CLUSTER 6 - ALPHA A
# CLUSTER 7 - MID LEVEL B HIGHER USAGE
# CLUSTER 8 - LONG-YARDAGE VARIATION 2
# CLUSTER 9 - MID LEVEL A 3RD + 1 LOWER USAGE

### NOTE - DOING AFTER - NOT GOING TO RENAME AS OF YET, BUT DO KNOW THAT THERE ARE DISTINCT CLUSTERS WHICH IS GOOD AND THAT I CAN NAME THEM 


#####
#####
#####


# THE ABOVE SHOWS THAT THERE IS VALIDITY IN WTF I'M DOING. SWEET. 


# ONTO THE Bs ... 


k2 <- 9
kmeans_rusher_xpass_diff_b_combined <- kmeans(rusher_xpass_diff_b_pre_clust, centers = k2, nstart = 25)

kmeans_rusher_xpass_diff_b_combined


# 2 - .422 - .422 - 1 - #6
# 3 - .57 - .148 - .351 - #9
# 4 - .635 - .065 - .439 - #8
# 5 - .685 - .05 - .769 - #3
# 6 - .723 - .038 - .76 - #4
# 7 - .751 - .028 - .737 - #5
# 8 - .77 - .019 - .679 - #7
# 9 - .786 - .016 - .842 - #1
# 10 - .798 - .012 - .75 - #6
# 11 - .811 - .013 - 1.083 - #2


rusher_xpass_diff_b_post_cluster <- cbind(rusher_xpass_diff_df %>% ungroup() %>% filter(rank_grp == "B") %>% select(rusher_player_name, rusher_player_id, posteam, season), rusher_xpass_diff_b_pre_clust, kmeans_rusher_xpass_diff_b_combined$cluster)
colnames(rusher_xpass_diff_b_post_cluster)[10] <- "cluster"

rusher_xpass_diff_b_post_cluster_grps <- rusher_xpass_diff_b_post_cluster %>% group_by(cluster) %>% dplyr::summarize(q20 = mean(q20_xpass_diff), q35 = mean(q35_xpass_diff), q50 = mean(q50_xpass_diff), q65 = mean(q65_xpass_diff), q80 = mean(q80_xpass_diff), n = n())



combined_rush_summation_final_b <- combined_rush_summation_final %>% filter(rank_grp == "B")

combined_rush_summation_final_b <- left_join(combined_rush_summation_final_b, rusher_xpass_diff_b_post_cluster %>% select(rusher_player_name:season, cluster), by = c("rusher_player_name", "rusher_player_id", "posteam", "season"))


b_cluster_mean_values <- combined_rush_summation_final_b %>% ungroup() %>% 
  group_by(cluster) %>% 
  dplyr::summarise(mn_First9Ratio = mean(First9Ratio, na.rm = T),
                   mn_First10Ratio = mean(First10Ratio, na.rm = T),
                   mn_Second1Ratio = mean(Second1Ratio, na.rm = T),
                   mn_Second2to4Ratio = mean(Second2to4Ratio, na.rm = T),
                   mn_Second5to7Ratio = mean(Second5to7Ratio, na.rm = T),
                   mn_Second8to10Ratio = mean(Second8to10Ratio, na.rm = T),
                   mn_SecondLongRatio = mean(SecondLongRatio, na.rm = T),
                   mn_Third1Ratio = mean(Third1Ratio, na.rm = T),
                   mn_Third2to3Ratio = mean(Third2to3Ratio, na.rm = T),
                   mn_ThirdRestRatio = mean(ThirdRestRatio, na.rm = T),
                   mn_Fourth1Ratio = mean(Fourth1Ratio, na.rm = T),
                   mn_FourthLongRatio = mean(FourthLongRatio, na.rm = T),
                   n = n())

b_cluster_mean_percs <- b_cluster_mean_values %>%
  filter(!is.na(cluster)) %>%
  mutate(across(mn_First9Ratio:mn_FourthLongRatio, 
                ~ rank(.x, na.last = "keep", ties.method = "min") / n() - (1 / n())))


b_t_test_results <- data.frame(cluster = numeric(), ydstogo_group = character(), p_value = numeric())

for (cl in seq(1:9)) {
  for (group in ydstogo_groups) {
    # Data for the current cluster
    cluster_data <- combined_rush_summation_final_b %>% 
      filter(cluster == cl) %>% 
      pull(!!sym(group)) %>% 
      na.omit()
    
    # Data for all other clusters
    non_cluster_data <- combined_rush_summation_final_b %>% 
      filter(cluster != cl) %>% 
      pull(!!sym(group)) %>% 
      na.omit()
    
    # Perform t-test
    if (length(cluster_data) > 1 && length(non_cluster_data) > 1) { # Ensure enough data for t-test
      t_test <- t.test(cluster_data, non_cluster_data)
      p_value <- t_test$p.value
    } else {
      p_value <- NA # Not enough data
    }
    
    # Append the result to the data frame
    b_t_test_results <- rbind(b_t_test_results, data.frame(cluster = cl, ydstogo_group = group, p_value = p_value))
  }
}

b_t_test_results <- b_t_test_results %>% 
  mutate(p_value = round(p_value, 3))

# Pivot the results for a cleaner view
b_t_test_results <- b_t_test_results %>% 
  pivot_wider(names_from = ydstogo_group, values_from = p_value)

# View the final results
print(b_t_test_results)


# 1 - SHORT-YARD BACKS
# 2 - EARLY-DOWN SHORT-YARD
# 3 - LOWER-USAGE BACKS
# 4 - EARLY-DOWN BACKS
# 5 - LATE-DOWN BACKS
# 6 - MID-USAGE LEAN EARLY-DOWN
# 7 - LONG-YARD BACKS
# 8 - LOWER-USAGE LEAN LONG DOWN
# 9 - MID-USAGE LEAN LATE-DOWN


#####
##### HONESTLY FOR C ... KINDA STUPID BUT LET'S SEE LMAO


k3 <- 8
kmeans_rusher_xpass_diff_c_combined <- kmeans(rusher_xpass_diff_c_pre_clust, centers = k3, nstart = 25)

kmeans_rusher_xpass_diff_c_combined


# 2 - .518 - .518 - 1 - #6
# 3 - .677 - .159 - .307 - #10
# 4 - .744 - .067 - .421 - #9
# 5 - .782 - .038 - .567 - #8
# 6 - .814 - .032 - .842 - #1
# 7 - .834 - .02 - .625 - #7
# 8 - .848 - .014 - .7 - #3
# 9 - .858 - .01 - .714 - #4
# 10 - .868 - .01 - 1 - #2
# 11 - .876 - .008 - .8 - #5


rusher_xpass_diff_c_post_cluster <- cbind(rusher_xpass_diff_df %>% ungroup() %>% filter(rank_grp == "C") %>% select(rusher_player_name, rusher_player_id, posteam, season), rusher_xpass_diff_c_pre_clust, kmeans_rusher_xpass_diff_c_combined$cluster)
colnames(rusher_xpass_diff_c_post_cluster)[10] <- "cluster"

rusher_xpass_diff_c_post_cluster_grps <- rusher_xpass_diff_c_post_cluster %>% group_by(cluster) %>% dplyr::summarize(q20 = mean(q20_xpass_diff), q35 = mean(q35_xpass_diff), q50 = mean(q50_xpass_diff), q65 = mean(q65_xpass_diff), q80 = mean(q80_xpass_diff), n = n())



combined_rush_summation_final_c <- combined_rush_summation_final %>% filter(rank_grp == "C")

combined_rush_summation_final_c <- left_join(combined_rush_summation_final_c, rusher_xpass_diff_c_post_cluster %>% select(rusher_player_name:season, cluster), by = c("rusher_player_name", "rusher_player_id", "posteam", "season"))


c_cluster_mean_values <- combined_rush_summation_final_c %>% ungroup() %>% 
  group_by(cluster) %>% 
  dplyr::summarise(mn_First9Ratio = mean(First9Ratio, na.rm = T),
                   mn_First10Ratio = mean(First10Ratio, na.rm = T),
                   mn_Second1Ratio = mean(Second1Ratio, na.rm = T),
                   mn_Second2to4Ratio = mean(Second2to4Ratio, na.rm = T),
                   mn_Second5to7Ratio = mean(Second5to7Ratio, na.rm = T),
                   mn_Second8to10Ratio = mean(Second8to10Ratio, na.rm = T),
                   mn_SecondLongRatio = mean(SecondLongRatio, na.rm = T),
                   mn_Third1Ratio = mean(Third1Ratio, na.rm = T),
                   mn_Third2to3Ratio = mean(Third2to3Ratio, na.rm = T),
                   mn_ThirdRestRatio = mean(ThirdRestRatio, na.rm = T),
                   mn_Fourth1Ratio = mean(Fourth1Ratio, na.rm = T),
                   mn_FourthLongRatio = mean(FourthLongRatio, na.rm = T),
                   n = n())

c_cluster_mean_percs <- c_cluster_mean_values %>%
  filter(!is.na(cluster)) %>%
  mutate(across(mn_First9Ratio:mn_FourthLongRatio, 
                ~ rank(.x, na.last = "keep", ties.method = "min") / n() - (1 / n())))


c_t_test_results <- data.frame(cluster = numeric(), ydstogo_group = character(), p_value = numeric())

for (cl in seq(1:8)) {
  for (group in ydstogo_groups) {
    # Data for the current cluster
    cluster_data <- combined_rush_summation_final_c %>% 
      filter(cluster == cl) %>% 
      pull(!!sym(group)) %>% 
      na.omit()
    
    # Data for all other clusters
    non_cluster_data <- combined_rush_summation_final_c %>% 
      filter(cluster != cl) %>% 
      pull(!!sym(group)) %>% 
      na.omit()
    
    # Perform t-test
    if (length(cluster_data) > 1 && length(non_cluster_data) > 1) { # Ensure enough data for t-test
      t_test <- t.test(cluster_data, non_cluster_data)
      p_value <- t_test$p.value
    } else {
      p_value <- NA # Not enough data
    }
    
    # Append the result to the data frame
    c_t_test_results <- rbind(c_t_test_results, data.frame(cluster = cl, ydstogo_group = group, p_value = p_value))
  }
}

c_t_test_results <- c_t_test_results %>% 
  mutate(p_value = round(p_value, 3))

# Pivot the results for a cleaner view
c_t_test_results <- c_t_test_results %>% 
  pivot_wider(names_from = ydstogo_group, values_from = p_value)

# View the final results
print(c_t_test_results)


# 1 - FIRST DOWN BACK
# 2 - HIGHER USAGE LEAN SHORT-YARD BACK
# 3 - LOWER USAGE SHORT-YARD BACK
# 4 - LONG-YARD BACK
# 5 - LATER DOWN LONG-YARD BACK (KINDA CLOSE TO 4)
# 6 - 1ST 10 / 2ND-MID BACK
# 7 - MID-USAGE LEAN LONG-YARD back
# 8 - EXTREME SHORT-YARD BACK


