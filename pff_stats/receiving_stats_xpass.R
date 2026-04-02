
# LOAD PBP_PART_COMBINED_JOIN, COMBINED_IDS

pbp_pass <- combined_pbp %>%
  filter(pass_attempt == 1 & !is.na(air_yards) & qb_scramble == 0 & play_type == "pass") %>%
  mutate(ydstogo_group = case_when(
    down == 1 & ydstogo < 10 ~ "1st9",
    down == 1 & ydstogo == 10 ~ "1st10",
    down == 1 & ydstogo >= 11 ~ "1stLong",
    down == 2 & ydstogo <= 2 ~ "2nd2",
    down == 2 & ydstogo <= 5 & ydstogo >= 3 ~ "2nd3-5",
    down == 2 & ydstogo <= 10 & ydstogo >= 6 ~ "2nd6-10",
    down == 2 & ydstogo >= 11 ~ "2ndLong", 
    down == 3 & ydstogo <= 1 ~ "3rd1",
    down == 3 & ydstogo == 2 ~ "3rd2",
    down == 3 & ydstogo == 3 ~ "3rd3",
    down == 3 & ydstogo >= 4 ~ "3rdRest",
    down == 4 & ydstogo <= 1 ~ "4th1",
    down == 4 & ydstogo > 1 ~ "4thLong",
    TRUE ~ NA_character_))

pbp_pass <- pbp_pass %>% group_by(game_id, posteam) %>% 
  mutate(med_pass_xpass = mean(mixed_xpass, na.rm = T),
         pass_xpass_diff = mixed_xpass - med_pass_xpass)

pbp_pass$receiver_id[which(pbp_pass$receiver_player_name == "A.Mattison" & pbp_pass$posteam %in% c("MIN", "LV", "MIA"))] <- "00-0035236"

pbp_pass$receiver_id[which(pbp_pass$receiver_player_name == "M.Woods" & pbp_pass$posteam %in% c("CLV") & pbp_pass$season %in% c(2022, 2024))] <- "00-0037300"

pbp_pass$receiver_id[which(pbp_pass$receiver_player_name == "D.Johnson" & pbp_pass$posteam == "HST" & pbp_pass$season == 2020 & pbp_pass$week %in% c(15,16,17))] <- "00-0032187"
pbp_pass$receiver_player_name[which(pbp_pass$receiver_id == "00-0032187" & pbp_pass$posteam == "HST" & pbp_pass$season == 2020 & pbp_pass$week %in% c(15,16,17))] <- "Da.Johnson"


pbp_pass <- pbp_pass %>%
  left_join(
    combined_ids %>% 
      select(-c(franchise_id, position)) %>% 
      filter(!is.na(gsis_id)) %>%
      distinct(),
    by = c("receiver_id" = "gsis_id", "posteam" = "team", "week" = "week", "season" = "season")
  )

pbp_pass$receiver_player_name[which(pbp_pass$receiver_id == "00-0037300")] <- "Michael Woods"
pbp_pass$player_id[which(pbp_pass$receiver_id == "00-0037300")] <- 84117
pbp_pass$pos_group[which(pbp_pass$receiver_id == "00-0037300")] <- "REC"

pbp_pass$player[which(pbp_pass$receiver_id == "00-0032187
")] <- "David Johnson"
pbp_pass$player_id[which(pbp_pass$receiver_id == "00-0032187")] <- 9519
pbp_pass$pos_group[which(pbp_pass$receiver_id == "00-0032187")] <- "BACK"

pbp_pass$player[which(pbp_pass$receiver_id == "00-0032187
")] <- "David Johnson"
pbp_pass$player_id[which(pbp_pass$receiver_id == "00-0032187")] <- 9519
pbp_pass$pos_group[which(pbp_pass$receiver_id == "00-0032187")] <- "BACK"

pbp_pass$receiver_id[which(pbp_pass$receiver_player_name == "D.Johnson" & pbp_pass$posteam == "HST" & pbp_pass$week %in% c(15,16,17) & pbp_pass$season == 2020)] <- "00-0032187"
pbp_pass$player[which(pbp_pass$receiver_player_name == "D.Johnson" & pbp_pass$posteam == "HST" & pbp_pass$week %in% c(15,16,17) & pbp_pass$season == 2020)] <- "David Johnson"
pbp_pass$player_id[which(pbp_pass$receiver_player_name == "D.Johnson" & pbp_pass$posteam == "HST" & pbp_pass$week %in% c(15,16,17) & pbp_pass$season == 2020)] <- 9519
pbp_pass$pos_group[which(pbp_pass$receiver_player_name == "D.Johnson" & pbp_pass$posteam == "HST" & pbp_pass$week %in% c(15,16,17) & pbp_pass$season == 2020)] <- "BACK"


pbp_pass$pos_group[which(pbp_pass$pos_group %in% c("OL", "OTHER", "DB", "LB") | is.na(pbp_pass$pos_group))] <- "OTHER"


receiver_xpass_diff_df <- pbp_pass %>%
  # filter(season == 2022, posteam == "BLT", !is.na(rusher_player_name), qb_kneel == 0) %>%
  group_by(receiver_id, player_id, posteam, season) %>%
  filter(!is.na(receiver_player_id)) %>%
  summarize(
    q20_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.2, na.rm = TRUE),
    q35_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.35, na.rm = TRUE),
    q50_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.5, na.rm = TRUE),
    q65_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.65, na.rm = TRUE),
    q80_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.8, na.rm = TRUE),
    q20_xpass_diff = quantile(pass_xpass_diff[qb_scramble == 0], 0.2, na.rm = TRUE),
    q35_xpass_diff = quantile(pass_xpass_diff[qb_scramble == 0], 0.35, na.rm = TRUE),
    q50_xpass_diff = quantile(pass_xpass_diff[qb_scramble == 0], 0.5, na.rm = TRUE),
    q65_xpass_diff = quantile(pass_xpass_diff[qb_scramble == 0], 0.65, na.rm = TRUE),
    q80_xpass_diff = quantile(pass_xpass_diff[qb_scramble == 0], 0.8, na.rm = TRUE),    
    n = n()
  ) %>% filter(n > 5)


receiver_xpass_diff_df %>%
  filter(n >= 15) %>%
  ungroup() %>%
  summarise(
    across(
      c(q20_xpass_diff, q35_xpass_diff, q50_xpass_diff, q65_xpass_diff, q80_xpass_diff, n),
      list(
        p10 = ~quantile(.x, 0.10, na.rm = TRUE),
        p20 = ~quantile(.x, 0.20, na.rm = TRUE),
        p30 = ~quantile(.x, 0.30, na.rm = TRUE),
        p40 = ~quantile(.x, 0.40, na.rm = TRUE),
        p50 = ~quantile(.x, 0.50, na.rm = TRUE),
        p60 = ~quantile(.x, 0.60, na.rm = TRUE),
        p70 = ~quantile(.x, 0.70, na.rm = TRUE),
        p80 = ~quantile(.x, 0.80, na.rm = TRUE),
        p90 = ~quantile(.x, 0.90, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(everything(), names_to = c("variable", "percentile"), names_sep = "_p") %>%
  pivot_wider(names_from = percentile, values_from = value, names_prefix = "p")


receiver_xpass_diff_df_cluster <- left_join(receiver_xpass_diff_df, 
                                      cluster_join, 
                                      by = c("player_id" = "player_id",
                                             "posteam" = "team_abbreviation",
                                             "season" = "season")) %>%
  filter(!is.na(receiver_id))


receiver_xpass_cluster <- receiver_xpass_diff_df_cluster %>%
  ungroup() %>%
  select(q20_xpass_diff:q80_xpass_diff) %>%
  scale()

colnames(receiver_xpass_cluster)



max_k <- 12
wss <- numeric(max_k)
sil <- numeric(max_k)

set.seed(42)
for (k in 2:max_k) {
  km <- kmeans(receiver_xpass_cluster, centers = k, nstart = 25, iter.max = 100)
  wss[k] <- km$tot.withinss
  sil[k] <- mean(silhouette(km$cluster, dist(receiver_xpass_cluster))[, 3])
}

# Plot both
par(mfrow = c(1, 2))
plot(1:max_k, c(NA, wss[2:max_k]), type = "b", pch = 19,
     xlab = "Number of Clusters", ylab = "Total Within SS", main = "Elbow Method")
plot(2:max_k, sil[2:max_k], type = "b", pch = 19,
     xlab = "Number of Clusters", ylab = "Avg Silhouette Width", main = "Silhouette Method")
par(mfrow = c(1, 1))

# Print silhouette scores
data.frame(k = 2:max_k, silhouette = sil[2:max_k]) %>% arrange(desc(silhouette))


km_final_xpass <- kmeans(receiver_xpass_cluster, centers = 2, nstart = 25, iter.max = 100)

receiver_xpass_tree_data <- receiver_xpass_diff_df_cluster %>% 
  ungroup() %>%
  select(q20_xpass_diff:q80_xpass_diff, position_group:man_zone_grp_cluster) %>%
  mutate(q20_xpass_diff_scaled = scale(q20_xpass_diff),
         q35_xpass_diff_scaled = scale(q35_xpass_diff),
         q50_xpass_diff_scaled = scale(q50_xpass_diff),
         q65_xpass_diff_scaled = scale(q65_xpass_diff),
         q80_xpass_diff_scaled = scale(q80_xpass_diff)) %>%
  select(-c(q20_xpass_diff:q80_xpass_diff))

receiver_xpass_tree_data$xpass_cluster = factor(km_final_xpass$cluster)

# Check missingness
sapply(receiver_xpass_tree_data, function(x) sum(is.na(x)))

# Run CART
tree_model_xpass <- rpart(
  xpass_cluster ~ position_group + align_cluster_name + 
    rte_cluster_name + tgt_cluster_name + man_zone_grp_cluster,
  data = receiver_xpass_tree_data,
  method = "class",
  control = rpart.control(cp = 0.01, minsplit = 20)
)

# Plot tree
rpart.plot(tree_model_xpass, type = 4, extra = 104, under = TRUE, 
           faclen = 0, cex = 0.8)

# Variable importance
tree_model_xpass$variable.importance

# Confusion matrix - how well do existing features explain xpass clusters?
pred <- predict(tree_model_xpass, type = "class")
# Confusion matrix - reference the correct data frame
table(Predicted = pred, Actual = receiver_xpass_tree_data$xpass_cluster)

# Accuracy - same fix
mean(pred == receiver_xpass_tree_data$xpass_cluster, na.rm = TRUE)


# Option 1: Text summary of rules
rpart.rules(tree_model_xpass, cover = TRUE)

# Option 3: More detailed path to each terminal node
path.rpart(tree_model_xpass, nodes = rownames(tree_model_xpass$frame)[tree_model_xpass$frame$var == "<leaf>"])

# SO HONESTLY ... IT'S ALL KIND OF THE SAME. HAS TO BE DOWN TO THE PLAYER LEVEL
# WOULD ... JUST AVERAGE THE PERCENTILES TOGETHER AND THEN PERCENTILE THAT
# LET'S DO IT FOR NEXT WEEK LMAO


receiver_xpass_final <- receiver_xpass_diff_df_cluster %>%
  ungroup() %>%
  mutate(
    # Z-score each percentile column
    z_q20_xpass_diff = as.numeric(scale(q20_xpass_diff)),
    z_q35_xpass_diff = as.numeric(scale(q35_xpass_diff)),
    z_q50_xpass_diff = as.numeric(scale(q50_xpass_diff)),
    z_q65_xpass_diff = as.numeric(scale(q65_xpass_diff)),
    z_q80_xpass_diff = as.numeric(scale(q80_xpass_diff)),
    
    # Average the z-scores
    avg_xpass_z = (z_q20_xpass_diff + z_q35_xpass_diff + z_q50_xpass_diff + 
                     z_q65_xpass_diff + z_q80_xpass_diff) / 5,
    
    # Percentile the average z-score
    xpass_percentile = percent_rank(avg_xpass_z) * 100
  )

# Check distribution
receiver_xpass_final %>%
  select(receiver_id, player_id, posteam, season, n, 
         q50_xpass_diff, avg_xpass_z, xpass_percentile) %>%
  arrange(desc(xpass_percentile)) %>%
  head(20)

# And bottom
receiver_xpass_final %>%
  select(receiver_id, player_id, posteam, season, n, 
         q50_xpass_diff, avg_xpass_z, xpass_percentile) %>%
  arrange(xpass_percentile) %>%
  head(20)


receiver_xpass_final %>% filter(posteam == "PHI", season == 2025)

receiver_xpass_final_join <- receiver_xpass_final %>%
  ungroup() %>%
  select(receiver_id:season, xpass_percentile)
