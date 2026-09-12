# pff_rushing_stats_build_five_xtd_AWS.R -> THIS COPY IS PROPOSED -- Claude, UNSIGNED, 2026-09-09. Andy stamps or corrects.
# Canon file untouched. Change set: seed 100 before every kmeans(); index renames [529]/[530] -> rename(cluster) with loud fail; View() removed
# Diff against canon: only the lines listed in rush_pipeline_order.md section 5.

pbp_rush <- pbp_rush %>% group_by(game_id, posteam) %>% 
  mutate(med_run_xtd = mean(pbp_after_old_xtd, na.rm = T),
         run_xtd_diff = pbp_after_old_xtd - med_run_xtd)

pbp_rush <- left_join(pbp_rush, 
                      situation_cluster_df %>% select(rusher_player_id:rank_grp, cluster), 
                      by = c("rusher_player_id", "posteam", "season", "rank_grp")
)

if (!"cluster" %in% names(pbp_rush)) { print(names(pbp_rush)); stop("situation join did not add `cluster` -- names printed above") }
pbp_rush <- dplyr::rename(pbp_rush, sit_cluster = cluster)

pbp_rush <- left_join(pbp_rush, 
                      gap_cluster_df %>%
                        dplyr::select(rusher_player_id:rank_grp, cluster), 
                      by = c("rusher_player_id", "posteam", "season", "rank_grp")
)

if (!"cluster" %in% names(pbp_rush)) { print(names(pbp_rush)); stop("gap join did not add `cluster` -- names printed above") }
pbp_rush <- dplyr::rename(pbp_rush, gap_cluster = cluster)


rusher_xtd_diff_df <- pbp_rush %>%
  # filter(season == 2022, posteam == "BLT", !is.na(rusher_player_name), qb_kneel == 0) %>%
  group_by(rusher_player_id, rank_grp, sit_cluster, gap_cluster, posteam, season) %>%
  summarize(
    q20_xtd = quantile(pbp_after_old_xtd[qb_scramble == 0], 0.2, na.rm = TRUE),
    q35_xtd = quantile(pbp_after_old_xtd[qb_scramble == 0], 0.35, na.rm = TRUE),
    q50_xtd = quantile(pbp_after_old_xtd[qb_scramble == 0], 0.5, na.rm = TRUE),
    q65_xtd = quantile(pbp_after_old_xtd[qb_scramble == 0], 0.65, na.rm = TRUE),
    q80_xtd = quantile(pbp_after_old_xtd[qb_scramble == 0], 0.8, na.rm = TRUE),
    q20_xtd_diff = quantile(run_xtd_diff[qb_scramble == 0], 0.2, na.rm = TRUE),
    q35_xtd_diff = quantile(run_xtd_diff[qb_scramble == 0], 0.35, na.rm = TRUE),
    q50_xtd_diff = quantile(run_xtd_diff[qb_scramble == 0], 0.5, na.rm = TRUE),
    q65_xtd_diff = quantile(run_xtd_diff[qb_scramble == 0], 0.65, na.rm = TRUE),
    q80_xtd_diff = quantile(run_xtd_diff[qb_scramble == 0], 0.8, na.rm = TRUE),    
    n = n()
  ) %>% filter(n > 5)


rusher_xtd_cluster <- rusher_xtd_diff_df %>%
  ungroup() %>%
  select(q20_xtd_diff:q80_xtd_diff) %>%
  scale()


max_k <- 12
wss <- numeric(max_k)
sil <- numeric(max_k)

set.seed(100)
for (k in 2:max_k) {
  set.seed(100)
  km <- kmeans(rusher_xtd_cluster, centers = k, nstart = 25, iter.max = 100)
  wss[k] <- km$tot.withinss
  sil[k] <- mean(silhouette(km$cluster, dist(rusher_xtd_cluster))[, 3])
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


set.seed(100)
km_final_xtd <- kmeans(rusher_xtd_cluster, centers = 2, nstart = 25, iter.max = 100)

rusher_xtd_tree_data <- rusher_xtd_diff_df %>%
  ungroup() %>%
  select(q20_xtd_diff:q80_xtd_diff, rank_grp:gap_cluster) %>%
  mutate(q20_xtd_diff_scaled = scale(q20_xtd_diff),
         q35_xtd_diff_scaled = scale(q35_xtd_diff),
         q50_xtd_diff_scaled = scale(q50_xtd_diff),
         q65_xtd_diff_scaled = scale(q65_xtd_diff),
         q80_xtd_diff_scaled = scale(q80_xtd_diff)) %>%
  select(-c(q20_xtd_diff:q80_xtd_diff))

rusher_xtd_tree_data$xtd_cluster = factor(km_final_xtd$cluster)

# Check missingness
sapply(rusher_xtd_tree_data, function(x) sum(is.na(x)))

# Run CART
tree_model_xtd <- rpart(
  xtd_cluster ~ rank_grp + sit_cluster + gap_cluster,
  data = rusher_xtd_tree_data,
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
table(Predicted = pred, Actual = rusher_xtd_tree_data$xtd_cluster)

# Accuracy - same fix
mean(pred == rusher_xtd_tree_data$xtd_cluster, na.rm = TRUE)


# Option 1: Text summary of rules
rpart.rules(tree_model_xtd, cover = TRUE)

# Option 3: More detailed path to each terminal node
path.rpart(tree_model_xtd, nodes = rownames(tree_model_xtd$frame)[tree_model_xtd$frame$var == "<leaf>"])

# SO NOTHING


rusher_xtd_final <- rusher_xtd_diff_df %>%
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
  mutate(
    xtd_percentile = percent_rank(avg_xtd_z) * 100
  )


# View() removed -- no live demo calls in files (usage law)