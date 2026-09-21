# pff_rushing_stats_build_two_situation_AWS.R -> THIS COPY IS PROPOSED -- Claude, UNSIGNED, 2026-09-09. Andy stamps or corrects.
# Canon file untouched. Change set: seed 100 immediately before EVERY kmeans() call (12 edits)
# Diff against canon: only the lines listed in rush_pipeline_order.md section 5.


set.seed(100)

####
#### A
####

rusher_xpass_diff_a_full <- rusher_xpass_diff_df %>% 
  
  ungroup() %>% 
  filter(rank_grp == "A")

# Scale clustering columns, keep originals
rusher_xpass_diff_a_full <- rusher_xpass_diff_a_full %>%
  mutate(across(q20_xpass_diff:q80_xpass_diff, ~ as.numeric(scale(.)), .names = "{.col}_scaled"))

# Extract ONLY scaled columns for clustering
rusher_a_for_clustering <- rusher_xpass_diff_a_full %>%
  dplyr::select(ends_with("_scaled"))

# Sanity check
cat("Rows:", nrow(rusher_a_for_clustering), "\n")
cat("Any NAs:", sum(is.na(rusher_a_for_clustering)), "\n")


#######################
# STEP 2: FIND OPTIMAL K
#######################

library(cluster)

set.seed(100)
max_k <- 12

metrics <- data.frame(
  k = 2:max_k,
  wss = NA,
  bss_tss = NA,
  silhouette = NA
)

for (i in 1:nrow(metrics)) {
  k <- metrics$k[i]
  
  set.seed(100)
  km <- kmeans(rusher_a_for_clustering, centers = k, nstart = 25, iter.max = 50)
  
  metrics$wss[i] <- km$tot.withinss
  metrics$bss_tss[i] <- km$betweenss / km$totss
  
  sil <- silhouette(km$cluster, dist(rusher_a_for_clustering))
  metrics$silhouette[i] <- mean(sil[, 3])
}

# Add marginal improvement
metrics <- metrics %>%
  mutate(
    wss_decrease = lag(wss) - wss,
    wss_decrease_pct = round(wss_decrease / lag(wss), 3),
    bss_gain = round(bss_tss - lag(bss_tss), 3)
  )

print(metrics)


#######################
# STEP 2B: VISUALIZE
#######################

par(mfrow = c(1, 3))

plot(metrics$k, metrics$wss, type = "b", pch = 19,
     xlab = "k", ylab = "Within-cluster SS", main = "Elbow")

plot(metrics$k, metrics$bss_tss, type = "b", pch = 19,
     xlab = "k", ylab = "BSS/TSS", main = "Variance Explained")
abline(h = 0.5, lty = 2, col = "red")

plot(metrics$k, metrics$silhouette, type = "b", pch = 19,
     xlab = "k", ylab = "Avg Silhouette", main = "Silhouette")

par(mfrow = c(1, 1))

###

candidate_ks <- c(4, 5, 6, 7)

cat("\n=== COMPARING CANDIDATES ===\n")

for (k in candidate_ks) {
  set.seed(100)
  km <- kmeans(rusher_a_for_clustering, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(rusher_a_for_clustering))
  
  cat(sprintf("\nk = %d:\n", k))
  cat(sprintf("  BSS/TSS: %.3f\n", km$betweenss / km$totss))
  cat(sprintf("  Silhouette: %.3f\n", mean(sil[, 3])))
  cat(sprintf("  Cluster sizes: %s\n", paste(sort(table(km$cluster), decreasing = TRUE), collapse = ", ")))
}


set.seed(100)
km_a_final <- kmeans(rusher_a_for_clustering, centers = 4, nstart = 25)
rusher_xpass_diff_a_full$cluster <- km_a_final$cluster

# Check profiles
rusher_xpass_diff_a_full %>%
  group_by(cluster) %>%
  summarise(
    n_players = n(),
    avg_rushes = mean(n),
    q20 = mean(q20_xpass_diff),
    q35 = mean(q35_xpass_diff),
    q50 = mean(q50_xpass_diff),
    q65 = mean(q65_xpass_diff),
    q80 = mean(q80_xpass_diff),
    .groups = "drop"
  ) %>%
  arrange(q50)


#### 
#### B
####


rusher_xpass_diff_b_full <- rusher_xpass_diff_df %>% 
  
  ungroup() %>% 
  filter(rank_grp == "B")

# Scale clustering columns, keep originals
rusher_xpass_diff_b_full <- rusher_xpass_diff_b_full %>%
  mutate(across(q20_xpass_diff:q80_xpass_diff, ~ as.numeric(scale(.)), .names = "{.col}_scaled"))

# Extract ONLY scaled columns for clustering
rusher_b_for_clustering <- rusher_xpass_diff_b_full %>%
  dplyr::select(ends_with("_scaled"))

# Sanity check
cat("Rows:", nrow(rusher_b_for_clustering), "\n")
cat("Any NAs:", sum(is.na(rusher_b_for_clustering)), "\n")


#######################
# STEP 2: FIND OPTIMAL K
#######################

library(cluster)

set.seed(100)
max_k <- 12

metrics <- data.frame(
  k = 2:max_k,
  wss = NA,
  bss_tss = NA,
  silhouette = NA
)

for (i in 1:nrow(metrics)) {
  k <- metrics$k[i]
  
  set.seed(100)
  km <- kmeans(rusher_b_for_clustering, centers = k, nstart = 25, iter.max = 50)
  
  metrics$wss[i] <- km$tot.withinss
  metrics$bss_tss[i] <- km$betweenss / km$totss
  
  sil <- silhouette(km$cluster, dist(rusher_b_for_clustering))
  metrics$silhouette[i] <- mean(sil[, 3])
}

# Add marginal improvement
metrics <- metrics %>%
  mutate(
    wss_decrease = lag(wss) - wss,
    wss_decrease_pct = round(wss_decrease / lag(wss), 3),
    bss_gain = round(bss_tss - lag(bss_tss), 3)
  )

print(metrics)


#######################
# STEP 2B: VISUALIZE
#######################

par(mfrow = c(1, 3))

plot(metrics$k, metrics$wss, type = "b", pch = 19,
     xlab = "k", ylab = "Within-cluster SS", main = "Elbow")

plot(metrics$k, metrics$bss_tss, type = "b", pch = 19,
     xlab = "k", ylab = "BSS/TSS", main = "Variance Explained")
abline(h = 0.5, lty = 2, col = "red")

plot(metrics$k, metrics$silhouette, type = "b", pch = 19,
     xlab = "k", ylab = "Avg Silhouette", main = "Silhouette")

par(mfrow = c(1, 1))

###

candidate_ks <- c(2,3,4,5)

cat("\n=== COMPARING CANDIDATES ===\n")

for (k in candidate_ks) {
  set.seed(100)
  km <- kmeans(rusher_b_for_clustering, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(rusher_b_for_clustering))
  
  cat(sprintf("\nk = %d:\n", k))
  cat(sprintf("  BSS/TSS: %.3f\n", km$betweenss / km$totss))
  cat(sprintf("  Silhouette: %.3f\n", mean(sil[, 3])))
  cat(sprintf("  Cluster sizes: %s\n", paste(sort(table(km$cluster), decreasing = TRUE), collapse = ", ")))
}


set.seed(100)
km_b_final <- kmeans(rusher_b_for_clustering, centers = 5, nstart = 25)
rusher_xpass_diff_b_full$cluster <- km_b_final$cluster

# Check profiles
rusher_xpass_diff_b_full %>%
  group_by(cluster) %>%
  summarise(
    n_players = n(),
    avg_rushes = mean(n),
    q20 = mean(q20_xpass_diff),
    q35 = mean(q35_xpass_diff),
    q50 = mean(q50_xpass_diff),
    q65 = mean(q65_xpass_diff),
    q80 = mean(q80_xpass_diff),
    .groups = "drop"
  ) %>%
  arrange(q50)


#### 
#### C
####


rusher_xpass_diff_c_full <- rusher_xpass_diff_df %>% 
  
  ungroup() %>% 
  filter(rank_grp == "C")

# Scale clustering columns, keep originals
rusher_xpass_diff_c_full <- rusher_xpass_diff_c_full %>%
  mutate(across(q20_xpass_diff:q80_xpass_diff, ~ as.numeric(scale(.)), .names = "{.col}_scaled"))

# Extract ONLY scaled columns for clustering
rusher_c_for_clustering <- rusher_xpass_diff_c_full %>%
  dplyr::select(ends_with("_scaled"))

# Sanity check
cat("Rows:", nrow(rusher_c_for_clustering), "\n")
cat("Any NAs:", sum(is.na(rusher_c_for_clustering)), "\n")


#######################
# STEP 2: FIND OPTIMAL K
#######################

library(cluster)

set.seed(100)
max_k <- 12

metrics <- data.frame(
  k = 2:max_k,
  wss = NA,
  bss_tss = NA,
  silhouette = NA
)

for (i in 1:nrow(metrics)) {
  k <- metrics$k[i]
  
  set.seed(100)
  km <- kmeans(rusher_c_for_clustering, centers = k, nstart = 25, iter.max = 50)
  
  metrics$wss[i] <- km$tot.withinss
  metrics$bss_tss[i] <- km$betweenss / km$totss
  
  sil <- silhouette(km$cluster, dist(rusher_c_for_clustering))
  metrics$silhouette[i] <- mean(sil[, 3])
}

# Add marginal improvement
metrics <- metrics %>%
  mutate(
    wss_decrease = lag(wss) - wss,
    wss_decrease_pct = round(wss_decrease / lag(wss), 3),
    bss_gain = round(bss_tss - lag(bss_tss), 3)
  )

print(metrics)


#######################
# STEP 2B: VISUALIZE
#######################

par(mfrow = c(1, 3))

plot(metrics$k, metrics$wss, type = "b", pch = 19,
     xlab = "k", ylab = "Within-cluster SS", main = "Elbow")

plot(metrics$k, metrics$bss_tss, type = "b", pch = 19,
     xlab = "k", ylab = "BSS/TSS", main = "Variance Explained")
abline(h = 0.5, lty = 2, col = "red")

plot(metrics$k, metrics$silhouette, type = "b", pch = 19,
     xlab = "k", ylab = "Avg Silhouette", main = "Silhouette")

par(mfrow = c(1, 1))

###

candidate_ks <- c(4,5,6)

cat("\n=== COMPARING CANDIDATES ===\n")

for (k in candidate_ks) {
  set.seed(100)
  km <- kmeans(rusher_c_for_clustering, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(rusher_c_for_clustering))
  
  cat(sprintf("\nk = %d:\n", k))
  cat(sprintf("  BSS/TSS: %.3f\n", km$betweenss / km$totss))
  cat(sprintf("  Silhouette: %.3f\n", mean(sil[, 3])))
  cat(sprintf("  Cluster sizes: %s\n", paste(sort(table(km$cluster), decreasing = TRUE), collapse = ", ")))
}


set.seed(100)
km_c_final <- kmeans(rusher_c_for_clustering, centers = 4, nstart = 25)
rusher_xpass_diff_c_full$cluster <- km_c_final$cluster

# Check profiles
rusher_xpass_diff_c_full %>%
  group_by(cluster) %>%
  summarise(
    n_players = n(),
    avg_rushes = mean(n),
    q20 = mean(q20_xpass_diff),
    q35 = mean(q35_xpass_diff),
    q50 = mean(q50_xpass_diff),
    q65 = mean(q65_xpass_diff),
    q80 = mean(q80_xpass_diff),
    .groups = "drop"
  ) %>%
  arrange(q50)


####
####
####
####


#######################
# PLAYER RUSH SUMMATIONS
#######################

player_rush_summations <- pbp_rush %>%
  filter(!is.na(ydstogo_group)) %>%
  group_by(rusher_player_id, posteam, week, season, game_id, rank_grp) %>%
  summarise(
    First_9 = sum(ydstogo_group == "1st9", na.rm = TRUE),
    First_10 = sum(ydstogo_group == "1st10", na.rm = TRUE),
    First_long = sum(ydstogo_group == "1stLong", na.rm = TRUE),
    Second_2 = sum(ydstogo_group == "2nd2", na.rm = TRUE),
    Second_3_5 = sum(ydstogo_group == "2nd3-5", na.rm = TRUE),
    Second_6_10 = sum(ydstogo_group == "2nd6-10", na.rm = TRUE),
    Second_long = sum(ydstogo_group == "2ndLong", na.rm = TRUE),
    Third_1 = sum(ydstogo_group == "3rd1", na.rm = TRUE),
    Third_2 = sum(ydstogo_group == "3rd2", na.rm = TRUE),
    Third_3 = sum(ydstogo_group == "3rd3", na.rm = TRUE),
    Third_rest = sum(ydstogo_group == "3rdRest", na.rm = TRUE),
    Fourth_1 = sum(ydstogo_group == "4th1", na.rm = TRUE),
    Fourth_long = sum(ydstogo_group == "4thLong", na.rm = TRUE),
    .groups = "drop"
  )


#######################
# GAME RUSH SUMMATIONS
#######################

game_rush_summations <- pbp_rush %>%
  filter(!is.na(ydstogo_group)) %>%
  group_by(posteam, game_id) %>%
  summarise(
    Game_First_9 = sum(ydstogo_group == "1st9", na.rm = TRUE),
    Game_First_10 = sum(ydstogo_group == "1st10", na.rm = TRUE),
    Game_First_long = sum(ydstogo_group == "1stLong", na.rm = TRUE),
    Game_Second_2 = sum(ydstogo_group == "2nd2", na.rm = TRUE),
    Game_Second_3_5 = sum(ydstogo_group == "2nd3-5", na.rm = TRUE),
    Game_Second_6_10 = sum(ydstogo_group == "2nd6-10", na.rm = TRUE),
    Game_Second_long = sum(ydstogo_group == "2ndLong", na.rm = TRUE),
    Game_Third_1 = sum(ydstogo_group == "3rd1", na.rm = TRUE),
    Game_Third_2 = sum(ydstogo_group == "3rd2", na.rm = TRUE),
    Game_Third_3 = sum(ydstogo_group == "3rd3", na.rm = TRUE),
    Game_Third_rest = sum(ydstogo_group == "3rdRest", na.rm = TRUE),
    Game_Fourth_1 = sum(ydstogo_group == "4th1", na.rm = TRUE),
    Game_Fourth_long = sum(ydstogo_group == "4thLong", na.rm = TRUE),
    .groups = "drop"
  )


#######################
# JOIN + RATIOS
#######################

combined_rush_summations <- left_join(
  player_rush_summations,
  game_rush_summations,
  by = c("posteam", "game_id")
)

combined_rush_summation_game <- combined_rush_summations %>%
  mutate(
    First_9_Ratio = ifelse(Game_First_9 == 0, NA, First_9 / Game_First_9),
    First_10_Ratio = ifelse(Game_First_10 == 0, NA, First_10 / Game_First_10),
    First_long_Ratio = ifelse(Game_First_long == 0, NA, First_long / Game_First_long),
    Second_2_Ratio = ifelse(Game_Second_2 == 0, NA, Second_2 / Game_Second_2),
    Second_3_5_Ratio = ifelse(Game_Second_3_5 == 0, NA, Second_3_5 / Game_Second_3_5),
    Second_6_10_Ratio = ifelse(Game_Second_6_10 == 0, NA, Second_6_10 / Game_Second_6_10),
    Second_long_Ratio = ifelse(Game_Second_long == 0, NA, Second_long / Game_Second_long),
    Third_1_Ratio = ifelse(Game_Third_1 == 0, NA, Third_1 / Game_Third_1),
    Third_2_Ratio = ifelse(Game_Third_2 == 0, NA, Third_2 / Game_Third_2),
    Third_3_Ratio = ifelse(Game_Third_3 == 0, NA, Third_3 / Game_Third_3),
    Third_rest_Ratio = ifelse(Game_Third_rest == 0, NA, Third_rest / Game_Third_rest),
    Fourth_1_Ratio = ifelse(Game_Fourth_1 == 0, NA, Fourth_1 / Game_Fourth_1),
    Fourth_long_Ratio = ifelse(Game_Fourth_long == 0, NA, Fourth_long / Game_Fourth_long)
  )


#######################
# AGGREGATE TO SEASON
#######################

combined_rush_summation_final <- combined_rush_summation_game %>%
  group_by(rusher_player_id, posteam, season, rank_grp) %>%
  summarise(
    First_9_Ratio = mean(First_9_Ratio, na.rm = TRUE),
    First_10_Ratio = mean(First_10_Ratio, na.rm = TRUE),
    First_long_Ratio = mean(First_long_Ratio, na.rm = TRUE),
    Second_2_Ratio = mean(Second_2_Ratio, na.rm = TRUE),
    Second_3_5_Ratio = mean(Second_3_5_Ratio, na.rm = TRUE),
    Second_6_10_Ratio = mean(Second_6_10_Ratio, na.rm = TRUE),
    Second_long_Ratio = mean(Second_long_Ratio, na.rm = TRUE),
    Third_1_Ratio = mean(Third_1_Ratio, na.rm = TRUE),
    Third_2_Ratio = mean(Third_2_Ratio, na.rm = TRUE),
    Third_3_Ratio = mean(Third_3_Ratio, na.rm = TRUE),
    Third_rest_Ratio = mean(Third_rest_Ratio, na.rm = TRUE),
    Fourth_1_Ratio = mean(Fourth_1_Ratio, na.rm = TRUE),
    Fourth_long_Ratio = mean(Fourth_long_Ratio, na.rm = TRUE),
    .groups = "drop"
  )


#######################
# VALIDATE CLUSTERS FUNCTION
#######################

validate_clusters <- function(summation_df, cluster_df, rank_grp_filter, num_clusters) {
  
  # Filter to rank group
  summation_filtered <- summation_df %>% filter(rank_grp == rank_grp_filter)
  
  # Join clusters
  summation_clustered <- left_join(
    summation_filtered,
    cluster_df %>% dplyr::select(rusher_player_id, posteam, season, cluster),
    by = c("rusher_player_id", "posteam", "season")
  )
  
  cat(sprintf("\n========== RANK %s VALIDATION ==========\n", rank_grp_filter))
  cat("Cluster distribution:\n")
  print(table(summation_clustered$cluster, useNA = "ifany"))
  
  # Cluster means
  ratio_cols <- names(summation_clustered)[grep("_Ratio$", names(summation_clustered))]
  
  cluster_means <- summation_clustered %>%
    filter(!is.na(cluster)) %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      across(all_of(ratio_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  cat("\nCluster means:\n")
  print(cluster_means)
  
  # Percentile ranks
  cluster_percs <- cluster_means %>%
    mutate(across(all_of(ratio_cols), 
                  ~ (rank(.x, na.last = "keep", ties.method = "average") - 1) / (n() - 1)))
  
  cat("\nCluster percentile ranks:\n")
  print(cluster_percs)
  
  # T-tests
  t_test_results <- data.frame()
  
  for (cl in 1:num_clusters) {
    for (col in ratio_cols) {
      cluster_data <- summation_clustered %>% 
        filter(cluster == cl) %>% 
        pull(!!sym(col)) %>% 
        na.omit()
      
      non_cluster_data <- summation_clustered %>% 
        filter(cluster != cl, !is.na(cluster)) %>% 
        pull(!!sym(col)) %>% 
        na.omit()
      
      if (length(cluster_data) > 1 && length(non_cluster_data) > 1) {
        t_test <- t.test(cluster_data, non_cluster_data)
        p_val <- t_test$p.value
        dir <- ifelse(mean(cluster_data) > mean(non_cluster_data), "HIGH", "LOW")
      } else {
        p_val <- NA
        dir <- NA
      }
      
      t_test_results <- rbind(t_test_results, data.frame(
        cluster = cl, 
        situation = col, 
        p_value = round(p_val, 3),
        direction = dir
      ))
    }
  }
  
  # T-test summary with arrows
  t_test_wide <- t_test_results %>%
    mutate(label = case_when(
      is.na(p_value) ~ "",
      p_value > 0.05 ~ "",
      direction == "HIGH" ~ paste0("↑", round(p_value, 3)),
      direction == "LOW" ~ paste0("↓", round(p_value, 3))
    )) %>%
    dplyr::select(cluster, situation, label) %>%
    pivot_wider(names_from = situation, values_from = label)
  
  cat("\nT-test results (↑ = higher, ↓ = lower than others, p < 0.05):\n")
  print(t_test_wide)
  
  # Naming guide
  cat("\n=== NAMING GUIDE ===\n")
  
  for (cl in 1:num_clusters) {
    cat(sprintf("\n--- CLUSTER %d ---\n", cl))
    
    # xpass profile from cluster_df
    profile <- cluster_df %>%
      filter(cluster == cl) %>%
      summarise(
        n = n(), 
        rushes = round(mean(n), 0),
        q20 = round(mean(q20_xpass_diff), 3),
        q50 = round(mean(q50_xpass_diff), 3), 
        q80 = round(mean(q80_xpass_diff), 3)
      )
    cat(sprintf("  n=%d, avg_rushes=%d, xpass_diff: %.3f / %.3f / %.3f\n",
                profile$n, profile$rushes, profile$q20, profile$q50, profile$q80))
    
    # Significant situations
    sig <- t_test_results %>% filter(cluster == cl, p_value <= 0.05)
    high <- sig %>% filter(direction == "HIGH") %>% pull(situation)
    low <- sig %>% filter(direction == "LOW") %>% pull(situation)
    
    if (length(high) > 0) cat(sprintf("  ↑ HIGH: %s\n", paste(high, collapse = ", ")))
    if (length(low) > 0) cat(sprintf("  ↓ LOW: %s\n", paste(low, collapse = ", ")))
    
    # Sample players
    players <- cluster_df %>%
      filter(cluster == cl) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      mutate(lbl = paste0(rusher_player_id, " ", season)) %>%
      pull(lbl)
    cat(sprintf("  Players: %s\n", paste(players, collapse = ", ")))
  }
  
  # Return the clustered df for further use
  return(summation_clustered)
}


#######################
# RUN VALIDATION
#######################

# RANK A (k=4)
combined_rush_summation_final_a <- validate_clusters(
  summation_df = combined_rush_summation_final,
  cluster_df = rusher_xpass_diff_a_full,
  rank_grp_filter = "A",
  num_clusters = 4
)

# RANK B (k=5)
combined_rush_summation_final_b <- validate_clusters(
  summation_df = combined_rush_summation_final,
  cluster_df = rusher_xpass_diff_b_full,
  rank_grp_filter = "B",
  num_clusters = 5
)

# RANK C (k=4)
combined_rush_summation_final_c <- validate_clusters(
  summation_df = combined_rush_summation_final,
  cluster_df = rusher_xpass_diff_c_full,
  rank_grp_filter = "C",
  num_clusters = 4
)


####
####
####


cluster_dashboard <- function(summation_clustered, cluster_df, num_clusters) {
  
  ratio_cols <- names(summation_clustered)[grep("_Ratio$", names(summation_clustered))]
  
  # 1. CLUSTER MEANS (raw values)
  cluster_means <- summation_clustered %>%
    filter(!is.na(cluster)) %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      across(all_of(ratio_cols), ~ round(mean(.x, na.rm = TRUE), 3)),
      .groups = "drop"
    )
  
  # 2. PERCENTILE RANKS (0-1 scale, where each cluster ranks vs others)
  cluster_percs <- cluster_means %>%
    mutate(across(all_of(ratio_cols), 
                  ~ round((rank(.x, na.last = "keep", ties.method = "average") - 1) / (n() - 1), 2)))
  
  # 3. T-TEST P-VALUES
  t_test_pvals <- data.frame()
  for (cl in 1:num_clusters) {
    row <- data.frame(cluster = cl)
    for (col in ratio_cols) {
      cluster_data <- summation_clustered %>% filter(cluster == cl) %>% pull(!!sym(col)) %>% na.omit()
      non_cluster_data <- summation_clustered %>% filter(cluster != cl, !is.na(cluster)) %>% pull(!!sym(col)) %>% na.omit()
      
      if (length(cluster_data) > 1 && length(non_cluster_data) > 1) {
        p_val <- t.test(cluster_data, non_cluster_data)$p.value
      } else {
        p_val <- NA
      }
      row[[col]] <- round(p_val, 3)
    }
    t_test_pvals <- rbind(t_test_pvals, row)
  }
  
  # 4. DIRECTION (HIGH/LOW vs others)
  cluster_direction <- data.frame()
  for (cl in 1:num_clusters) {
    row <- data.frame(cluster = cl)
    for (col in ratio_cols) {
      cluster_mean <- cluster_means %>% filter(cluster == cl) %>% pull(!!sym(col))
      overall_mean <- summation_clustered %>% filter(!is.na(cluster)) %>% pull(!!sym(col)) %>% mean(na.rm = TRUE)
      row[[col]] <- ifelse(cluster_mean > overall_mean, "↑", "↓")
    }
    cluster_direction <- rbind(cluster_direction, row)
  }
  
  # 5. COMBINED VIEW - percentile + significance marker
  cluster_combined <- data.frame(cluster = 1:num_clusters)
  for (col in ratio_cols) {
    perc_vals <- cluster_percs[[col]]
    p_vals <- t_test_pvals[[col]]
    dir_vals <- cluster_direction[[col]]
    
    # Format: percentile with * if significant
    cluster_combined[[col]] <- sapply(1:num_clusters, function(i) {
      perc <- perc_vals[i]
      sig <- ifelse(!is.na(p_vals[i]) && p_vals[i] < 0.05, "*", "")
      dir <- dir_vals[i]
      paste0(sprintf("%.2f", perc), sig, dir)
    })
  }
  
  # 6. XPASS PROFILE
  xpass_profile <- cluster_df %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      avg_rushes = round(mean(n), 0),
      q20 = round(mean(q20_xpass_diff), 3),
      q50 = round(mean(q50_xpass_diff), 3),
      q80 = round(mean(q80_xpass_diff), 3),
      .groups = "drop"
    )
  
  # PRINT EVERYTHING
  cat("\n===== RAW MEANS =====\n")
  print(cluster_means)
  
  cat("\n===== PERCENTILE RANKS (0=lowest, 1=highest among clusters) =====\n")
  print(cluster_percs)
  
  cat("\n===== T-TEST P-VALUES (< 0.05 = significant) =====\n")
  print(t_test_pvals)
  
  cat("\n===== COMBINED VIEW (percentile + *=sig + ↑/↓=direction) =====\n")
  print(cluster_combined)
  
  cat("\n===== XPASS PROFILE =====\n")
  print(xpass_profile)
  
  # Return list for further use
  return(list(
    means = cluster_means,
    percs = cluster_percs,
    pvals = t_test_pvals,
    combined = cluster_combined,
    xpass = xpass_profile
  ))
}


# RUN IT
a_dashboard <- cluster_dashboard(combined_rush_summation_final_a, rusher_xpass_diff_a_full, 4)

a_dashboard$percs
a_dashboard$pvals

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



b_dashboard <- cluster_dashboard(combined_rush_summation_final_b, rusher_xpass_diff_b_full, 5)

b_dashboard$percs
b_dashboard$pvals

# B (15-50% of the team's rushes)
#   1: LATER/LONG      ~106 rows, ~21 carries. the 3rd-down back: about half the team's 3rd-and-4+ carries; xpass far above the rest (q50 +.16)
#   2: BELLCOW MID     ~219 rows, ~39 carries. an even ~30% of every bucket; widest xpass range (-.22 .. +.20)
#   3: SHORT YARDAGE   ~190 rows, ~28 carries. top on 3rd-and-1 / 4th-and-1, bottom on everything long; every xpass quantile negative (q50 -.14)
#   4: EARLY DOWN      ~376 rows, ~46 carries. the biggest group. 1st-and-10 and 2nd-and-up-to-10, least 3rd-and-long and 4th down; xpass q50 -.03
#   5: MID             ~271 rows, ~41 carries. leans long: 1st-and-long, 2nd-and-long, 3rd-and-3; xpass q50 +.03, q80 +.26


c_dashboard <- cluster_dashboard(combined_rush_summation_final_c, rusher_xpass_diff_c_full, 4)

c_dashboard$percs
c_dashboard$pvals

# C (under 15% of the team's rushes)
#   1: EARLY DOWN      ~378 rows, ~13 carries. the biggest group. spot carries on 1st-and-10 and 2nd-and-medium; xpass q50 -.04
#   2: MID             ~276 rows, ~14 carries. leans later-down / longer; xpass q50 +.10, q80 +.32
#   3: SHORT YARDAGE   ~249 rows, ~10 carries. carries almost only on 2nd-and-2, 3rd-and-1, 4th-and-1; every xpass quantile negative (q50 -.18)
#   4: LONG YARDAGE    ~100 rows, ~12 carries. 2nd-and-6+ and the 3rd-and-longer buckets; xpass far above the rest (q50 +.27)


####
####
####


situation_cluster_df <- 
  rbind(combined_rush_summation_final_a, combined_rush_summation_final_b, combined_rush_summation_final_c)