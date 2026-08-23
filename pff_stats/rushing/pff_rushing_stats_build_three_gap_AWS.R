
# pbp_rush$center_ind = ifelse(pbp_rush$run_gap == 0, 1, 0)

base_run_gap_cluster <- pbp_rush %>%
  filter(qb_kneel == 0) %>%
  group_by(rusher_player_id, posteam, season, rank_grp) %>%
  summarise(
    total_rushes = n(),
    center_perc = sum(center_ind == 1, na.rm = TRUE) / n(),
    guard_perc = sum(guard_ind == 1, na.rm = TRUE) / n(),
    tackle_perc = sum(tackle_ind == 1, na.rm = TRUE) / n(),
    end_perc = sum(end_ind == 1, na.rm = TRUE) / n(),
    .groups = "drop"
  ) %>%
  filter(total_rushes >= 6)

# RANK A
run_gap_a_full <- base_run_gap_cluster %>% filter(rank_grp == "A")
run_gap_a_full <- run_gap_a_full %>%
  mutate(across(center_perc:end_perc, ~ as.numeric(scale(.)), .names = "{.col}_scaled"))
run_gap_a_for_clustering <- run_gap_a_full %>% dplyr::select(ends_with("_scaled"))

# RANK B
run_gap_b_full <- base_run_gap_cluster %>% filter(rank_grp == "B")
run_gap_b_full <- run_gap_b_full %>%
  mutate(across(center_perc:end_perc, ~ as.numeric(scale(.)), .names = "{.col}_scaled"))
run_gap_b_for_clustering <- run_gap_b_full %>% dplyr::select(ends_with("_scaled"))

# RANK C
run_gap_c_full <- base_run_gap_cluster %>% filter(rank_grp == "C")
run_gap_c_full <- run_gap_c_full %>%
  mutate(across(center_perc:end_perc, ~ as.numeric(scale(.)), .names = "{.col}_scaled"))
run_gap_c_for_clustering <- run_gap_c_full %>% dplyr::select(ends_with("_scaled"))


####
####


#######################
# RUN GAP CLUSTERING - RANK A
#######################

run_gap_a_full <- base_run_gap_cluster %>% filter(rank_grp == "A")
run_gap_a_full <- run_gap_a_full %>%
  mutate(across(center_perc:end_perc, ~ as.numeric(scale(.)), .names = "{.col}_scaled"))
run_gap_a_for_clustering <- run_gap_a_full %>% dplyr::select(ends_with("_scaled"))

cat("Rows:", nrow(run_gap_a_for_clustering), "\n")
cat("Any NAs:", sum(is.na(run_gap_a_for_clustering)), "\n")

library(cluster)
set.seed(42)
max_k <- 10

metrics_a <- data.frame(k = 2:max_k, wss = NA, bss_tss = NA, silhouette = NA)

for (i in 1:nrow(metrics_a)) {
  k <- metrics_a$k[i]
  km <- kmeans(run_gap_a_for_clustering, centers = k, nstart = 25, iter.max = 50)
  metrics_a$wss[i] <- km$tot.withinss
  metrics_a$bss_tss[i] <- km$betweenss / km$totss
  sil <- silhouette(km$cluster, dist(run_gap_a_for_clustering))
  metrics_a$silhouette[i] <- mean(sil[, 3])
}

metrics_a <- metrics_a %>%
  mutate(
    wss_decrease = lag(wss) - wss,
    wss_decrease_pct = round(wss_decrease / lag(wss), 3),
    bss_gain = round(bss_tss - lag(bss_tss), 3)
  )

print(metrics_a)

par(mfrow = c(1, 3))
plot(metrics_a$k, metrics_a$wss, type = "b", pch = 19, xlab = "k", ylab = "WSS", main = "Elbow - A")
plot(metrics_a$k, metrics_a$bss_tss, type = "b", pch = 19, xlab = "k", ylab = "BSS/TSS", main = "Variance - A")
abline(h = 0.5, lty = 2, col = "red")
plot(metrics_a$k, metrics_a$silhouette, type = "b", pch = 19, xlab = "k", ylab = "Silhouette", main = "Silhouette - A")
par(mfrow = c(1, 1))

candidate_ks <- c(4,5,6,7,8)
cat("\n=== RANK A CANDIDATES ===\n")
for (k in candidate_ks) {
  set.seed(42)
  km <- kmeans(run_gap_a_for_clustering, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(run_gap_a_for_clustering))
  cat(sprintf("\nk = %d:\n", k))
  cat(sprintf("  BSS/TSS: %.3f\n", km$betweenss / km$totss))
  cat(sprintf("  Silhouette: %.3f\n", mean(sil[, 3])))
  cat(sprintf("  Cluster sizes: %s\n", paste(sort(table(km$cluster), decreasing = TRUE), collapse = ", ")))
}
# GONNA GO WITH 8

# APPLY CHOSEN K (update after seeing results)
km_gap_a_final <- kmeans(run_gap_a_for_clustering, centers = 8, nstart = 25)  # UPDATE K
run_gap_a_full$cluster <- km_gap_a_final$cluster

run_gap_a_full %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    avg_rushes = mean(total_rushes),
    center = round(mean(center_perc), 3),
    guard = round(mean(guard_perc), 3),
    tackle = round(mean(tackle_perc), 3),
    end = round(mean(end_perc), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(center))

# 1 - GUARD / TACKLE
# 2 - GUARD
# 3 - OUTSIDE
# 4 - CENTER / TACKLE
# 5 - TACKLE / OUTSIDE
# 6 - CENTER / OUTSIDE
# 7 - CENTER
# 8 - GUARD / TACKLE

###
### B
###


#######################
# RUN GAP CLUSTERING - RANK B
#######################

run_gap_b_full <- base_run_gap_cluster %>% filter(rank_grp == "B")
run_gap_b_full <- run_gap_b_full %>%
  mutate(across(center_perc:end_perc, ~ as.numeric(scale(.)), .names = "{.col}_scaled"))
run_gap_b_for_clustering <- run_gap_b_full %>% dplyr::select(ends_with("_scaled"))

cat("Rows:", nrow(run_gap_b_for_clustering), "\n")
cat("Any NAs:", sum(is.na(run_gap_b_for_clustering)), "\n")

set.seed(42)
metrics_b <- data.frame(k = 2:max_k, wss = NA, bss_tss = NA, silhouette = NA)

for (i in 1:nrow(metrics_b)) {
  k <- metrics_b$k[i]
  km <- kmeans(run_gap_b_for_clustering, centers = k, nstart = 25, iter.max = 50)
  metrics_b$wss[i] <- km$tot.withinss
  metrics_b$bss_tss[i] <- km$betweenss / km$totss
  sil <- silhouette(km$cluster, dist(run_gap_b_for_clustering))
  metrics_b$silhouette[i] <- mean(sil[, 3])
}

metrics_b <- metrics_b %>%
  mutate(
    wss_decrease = lag(wss) - wss,
    wss_decrease_pct = round(wss_decrease / lag(wss), 3),
    bss_gain = round(bss_tss - lag(bss_tss), 3)
  )

print(metrics_b)

par(mfrow = c(1, 3))
plot(metrics_b$k, metrics_b$wss, type = "b", pch = 19, xlab = "k", ylab = "WSS", main = "Elbow - B")
plot(metrics_b$k, metrics_b$bss_tss, type = "b", pch = 19, xlab = "k", ylab = "BSS/TSS", main = "Variance - B")
abline(h = 0.5, lty = 2, col = "red")
plot(metrics_b$k, metrics_b$silhouette, type = "b", pch = 19, xlab = "k", ylab = "Silhouette", main = "Silhouette - B")
par(mfrow = c(1, 1))

candidate_ks <- c(3,4)
cat("\n=== RANK B CANDIDATES ===\n")
for (k in candidate_ks) {
  km <- kmeans(run_gap_b_for_clustering, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(run_gap_b_for_clustering))
  cat(sprintf("\nk = %d:\n", k))
  cat(sprintf("  BSS/TSS: %.3f\n", km$betweenss / km$totss))
  cat(sprintf("  Silhouette: %.3f\n", mean(sil[, 3])))
  cat(sprintf("  Cluster sizes: %s\n", paste(sort(table(km$cluster), decreasing = TRUE), collapse = ", ")))
}

# APPLY CHOSEN K
km_gap_b_final <- kmeans(run_gap_b_for_clustering, centers = 4, nstart = 25)  # UPDATE K
run_gap_b_full$cluster <- km_gap_b_final$cluster

run_gap_b_full %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    avg_rushes = mean(total_rushes),
    center = round(mean(center_perc), 3),
    guard = round(mean(guard_perc), 3),
    tackle = round(mean(tackle_perc), 3),
    end = round(mean(end_perc), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(center))

# 1 - OUTSIDE
# 2 - TACKLE
# 3 - CENTER
# 4 - GUARD


#######################
# RUN GAP CLUSTERING - RANK C
#######################

run_gap_c_full <- base_run_gap_cluster %>% filter(rank_grp == "C")
run_gap_c_full <- run_gap_c_full %>%
  mutate(across(center_perc:end_perc, ~ as.numeric(scale(.)), .names = "{.col}_scaled"))
run_gap_c_for_clustering <- run_gap_c_full %>% dplyr::select(ends_with("_scaled"))

cat("Rows:", nrow(run_gap_c_for_clustering), "\n")
cat("Any NAs:", sum(is.na(run_gap_c_for_clustering)), "\n")

set.seed(42)
metrics_c <- data.frame(k = 2:max_k, wss = NA, bss_tss = NA, silhouette = NA)

for (i in 1:nrow(metrics_c)) {
  k <- metrics_c$k[i]
  km <- kmeans(run_gap_c_for_clustering, centers = k, nstart = 25, iter.max = 50)
  metrics_c$wss[i] <- km$tot.withinss
  metrics_c$bss_tss[i] <- km$betweenss / km$totss
  sil <- silhouette(km$cluster, dist(run_gap_c_for_clustering))
  metrics_c$silhouette[i] <- mean(sil[, 3])
}

metrics_c <- metrics_c %>%
  mutate(
    wss_decrease = lag(wss) - wss,
    wss_decrease_pct = round(wss_decrease / lag(wss), 3),
    bss_gain = round(bss_tss - lag(bss_tss), 3)
  )

print(metrics_c)

par(mfrow = c(1, 3))
plot(metrics_c$k, metrics_c$wss, type = "b", pch = 19, xlab = "k", ylab = "WSS", main = "Elbow - C")
plot(metrics_c$k, metrics_c$bss_tss, type = "b", pch = 19, xlab = "k", ylab = "BSS/TSS", main = "Variance - C")
abline(h = 0.5, lty = 2, col = "red")
plot(metrics_c$k, metrics_c$silhouette, type = "b", pch = 19, xlab = "k", ylab = "Silhouette", main = "Silhouette - C")
par(mfrow = c(1, 1))

candidate_ks <- c(3, 4, 5)
cat("\n=== RANK C CANDIDATES ===\n")
for (k in candidate_ks) {
  set.seed(42)
  km <- kmeans(run_gap_c_for_clustering, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(run_gap_c_for_clustering))
  cat(sprintf("\nk = %d:\n", k))
  cat(sprintf("  BSS/TSS: %.3f\n", km$betweenss / km$totss))
  cat(sprintf("  Silhouette: %.3f\n", mean(sil[, 3])))
  cat(sprintf("  Cluster sizes: %s\n", paste(sort(table(km$cluster), decreasing = TRUE), collapse = ", ")))
}

# APPLY CHOSEN K

km_gap_c_final <- kmeans(run_gap_c_for_clustering, centers = 4, nstart = 25)  # UPDATE K
run_gap_c_full$cluster <- km_gap_c_final$cluster

run_gap_c_full %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    avg_rushes = mean(total_rushes),
    center = round(mean(center_perc), 3),
    guard = round(mean(guard_perc), 3),
    tackle = round(mean(tackle_perc), 3),
    end = round(mean(end_perc), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(center))

# 1 - OUTSIDE
# 2 - GUARD
# 3 - TACKLE
# 4 - CENTER


###
###
###

gap_cluster_df <- 
  rbind(run_gap_a_full, run_gap_b_full, run_gap_c_full)

