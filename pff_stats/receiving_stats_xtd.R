# EP OR TD_PROB OR WHATEVER THAT COLUMN IS. 
# SO IT'S pbp_after_old_xtd


pbp_pass <- pbp_pass %>% group_by(game_id, posteam) %>% 
  mutate(med_pass_xtd_prob = mean(pbp_after_old_xtd, na.rm = T),
         pass_xtd_diff = pbp_after_old_xtd - med_pass_xtd_prob)

receiver_xtd_diff_df <- pbp_pass %>%
  # filter(season == 2022, posteam == "BLT", !is.na(rusher_player_name), qb_kneel == 0) %>%
  group_by(receiver_id, player_id, posteam, season) %>%
  filter(!is.na(receiver_player_id)) %>%
  summarize(
    q20_xtd = quantile(pbp_after_old_xtd[qb_scramble == 0], 0.2, na.rm = TRUE),
    q35_xtd = quantile(pbp_after_old_xtd[qb_scramble == 0], 0.35, na.rm = TRUE),
    q50_xtd = quantile(pbp_after_old_xtd[qb_scramble == 0], 0.5, na.rm = TRUE),
    q65_xtd = quantile(pbp_after_old_xtd[qb_scramble == 0], 0.65, na.rm = TRUE),
    q80_xtd = quantile(pbp_after_old_xtd[qb_scramble == 0], 0.8, na.rm = TRUE),
    q20_xtd_diff = quantile(pass_xtd_diff[qb_scramble == 0], 0.2, na.rm = TRUE),
    q35_xtd_diff = quantile(pass_xtd_diff[qb_scramble == 0], 0.35, na.rm = TRUE),
    q50_xtd_diff = quantile(pass_xtd_diff[qb_scramble == 0], 0.5, na.rm = TRUE),
    q65_xtd_diff = quantile(pass_xtd_diff[qb_scramble == 0], 0.65, na.rm = TRUE),
    q80_xtd_diff = quantile(pass_xtd_diff[qb_scramble == 0], 0.8, na.rm = TRUE),    
    n = n()
  ) %>% filter(n > 5)


receiver_xtd_diff_df %>%
  filter(n >= 15) %>%
  ungroup() %>%
  summarise(
    across(
      c(q20_xtd_diff, q35_xtd_diff, q50_xtd_diff, q65_xtd_diff, q80_xtd_diff, n),
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

receiver_xtd_diff_df_cluster <- left_join(receiver_xtd_diff_df, 
                                            cluster_join, 
                                            by = c("player_id" = "player_id",
                                                   "posteam" = "team_abbreviation",
                                                   "season" = "season")) %>%
  filter(!is.na(receiver_id))


receiver_xtd_cluster <- receiver_xtd_diff_df_cluster %>%
  filter(rte_cluster_name %ni% c("LR") & tgt_cluster_name %ni% c("LT") & man_zone_grp_cluster %ni% c("HB_LT", "TE_LT", "WR_LT", "OTH")) %>%
  ungroup() %>%
  select(q20_xtd_diff:q80_xtd_diff) %>%
  scale()


max_k <- 12
wss <- numeric(max_k)
sil <- numeric(max_k)

set.seed(42)
for (k in 2:max_k) {
  km <- kmeans(receiver_xtd_cluster, centers = k, nstart = 25, iter.max = 100)
  wss[k] <- km$tot.withinss
  sil[k] <- mean(silhouette(km$cluster, dist(receiver_xtd_cluster))[, 3])
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


km_final_xtd <- kmeans(receiver_xtd_cluster, centers = 2, nstart = 25, iter.max = 100)

receiver_xtd_tree_data <- receiver_xtd_diff_df_cluster %>%
  filter(rte_cluster_name %ni% c("LR") & tgt_cluster_name %ni% c("LT") & man_zone_grp_cluster %ni% c("HB_LT", "TE_LT", "WR_LT", "OTH")) %>%
  ungroup() %>%
  select(q20_xtd_diff:q80_xtd_diff, position_group:man_zone_grp_cluster) %>%
  mutate(q20_xtd_diff_scaled = scale(q20_xtd_diff),
         q35_xtd_diff_scaled = scale(q35_xtd_diff),
         q50_xtd_diff_scaled = scale(q50_xtd_diff),
         q65_xtd_diff_scaled = scale(q65_xtd_diff),
         q80_xtd_diff_scaled = scale(q80_xtd_diff)) %>%
  select(-c(q20_xtd_diff:q80_xtd_diff))

receiver_xtd_tree_data$xtd_cluster = factor(km_final_xtd$cluster)

# Check missingness
sapply(receiver_xtd_tree_data, function(x) sum(is.na(x)))

# Run CART
tree_model_xtd <- rpart(
  xtd_cluster ~ position_group + align_cluster_name + 
    rte_cluster_name + tgt_cluster_name + man_zone_grp_cluster,
  data = receiver_xtd_tree_data,
  method = "class",
  control = rpart.control(cp = 0.01, minsplit = 20)
)

# Plot tree
rpart.plot(tree_model_xtd, type = 4, extra = 104, under = TRUE, 
           faclen = 0, cex = 0.8)

# Variable importance
tree_model_xtd$variable.importance

# Confusion matrix - how well do existing features explain xpass clusters?
pred <- predict(tree_model_xtd, type = "class")
# Confusion matrix - reference the correct data frame
table(Predicted = pred, Actual = receiver_xtd_tree_data$xtd_cluster)

# Accuracy - same fix
mean(pred == receiver_xtd_tree_data$xtd_cluster, na.rm = TRUE)


# Option 1: Text summary of rules
rpart.rules(tree_model_xtd, cover = TRUE)

# Option 3: More detailed path to each terminal node
path.rpart(tree_model_xtd, nodes = rownames(tree_model_xtd$frame)[tree_model_xtd$frame$var == "<leaf>"])


colnames(receiver_xtd_diff_df_cluster)


receiver_xtd_diff_df_cluster <- receiver_xtd_diff_df_cluster %>%
  mutate(td_grp_cluster = case_when(
    # LT rules first
    (tgt_cluster_name == "LT" | rte_cluster_name == "LR") & position_group == "HB" ~ "HB_LT",
    (tgt_cluster_name == "LT" | rte_cluster_name == "LR") & position_group %in% c("TE", "WR") ~ "REC_LT",
    man_zone_grp_cluster == "OTH" ~ "OTH_LT",
    
    # Node 2: HB
    position_group == "HB" ~ "TD_LOW",
    
    # Node 12: TE/WR with tgt in (BT,G,ML,SMT,ST) & rte in (BT,DT,MT,SMT)
    position_group %in% c("TE", "WR") &
      tgt_cluster_name %in% c("BT", "G", "ML", "SMT", "ST") &
      rte_cluster_name %in% c("BT", "DT", "MT", "SMT") ~ "TD_LOW",
    
    # Node 13: TE/WR with tgt in (BT,G,ML,SMT,ST) & rte in (RB,ST)
    position_group %in% c("TE", "WR") &
      tgt_cluster_name %in% c("BT", "G", "ML", "SMT", "ST") &
      rte_cluster_name %in% c("RB", "ST") ~ "TD_HIGH",
    
    # Node 7: TE/WR with tgt in (DT,MT,RB)
    position_group %in% c("TE", "WR") &
      tgt_cluster_name %in% c("DT", "MT", "RB") ~ "TD_HIGH",
    
    TRUE ~ NA_character_
  ))


receiver_xtd_diff_df_cluster %>%
  group_by(td_grp_cluster) %>%
  dplyr::summarise(mn_q20_xtd_diff = mean(q20_xtd_diff, na.rm = T),
                   mn_q35_xtd_diff = mean(q35_xtd_diff, na.rm = T),
                   mn_q50_xtd_diff = mean(q50_xtd_diff, na.rm = T),
                   mn_q65_xtd_diff = mean(q65_xtd_diff, na.rm = T),
                   mn_q80_xtd_diff = mean(q80_xtd_diff, na.rm = T),
                   n = n())


receiver_xtd_final <- receiver_xtd_diff_df_cluster %>%
  ungroup() %>%
  mutate(
      # Z-score each percentile column
      z_q20_xtd_diff = as.numeric(scale(q20_xtd_diff)),
      z_q35_xtd_diff = as.numeric(scale(q35_xtd_diff)),
      z_q50_xtd_diff = as.numeric(scale(q50_xtd_diff)),
      z_q65_xtd_diff = as.numeric(scale(q65_xtd_diff)),
      z_q80_xtd_diff = as.numeric(scale(q80_xtd_diff)),
      
      # Average the z-scores
      avg_xtd_z = (z_q20_xtd_diff + z_q35_xtd_diff + z_q50_xtd_diff + 
                       z_q65_xtd_diff + z_q80_xtd_diff) / 5
  ) %>%
  group_by(td_grp_cluster) %>%
  mutate(
    xtd_percentile = percent_rank(avg_xtd_z) * 100
  )


View(receiver_xtd_final %>% filter(posteam == "PHI", season == 2025))


receiver_xtd_final_join <- receiver_xtd_final %>%
  select(receiver_id, player_id, posteam, season, td_grp_cluster, xtd_percentile)
