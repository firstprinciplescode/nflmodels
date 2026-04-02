
options(scipen = 999)

'%ni%' <- Negate('%in%')

keep_objects <- c("con", "combined_ids", "combined_pbp")
rm(list = setdiff(ls(), keep_objects))

'%ni%' <- Negate('%in%')

run_athena_query <- function(sql, max_wait = 120) {
  # Start query
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  
  # Poll for completion
  status <- "RUNNING"
  elapsed <- 0
  while (status %in% c("RUNNING", "QUEUED") && elapsed < max_wait) {
    Sys.sleep(2)
    elapsed <- elapsed + 2
    
    status_cmd <- sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.State" --output text',
      query_id
    )
    status <- trimws(system(status_cmd, intern = TRUE))
  }
  
  if (status != "SUCCEEDED") {
    # Get error message
    error_cmd <- sprintf(
      'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.Status.StateChangeReason" --output text',
      query_id
    )
    error_msg <- system(error_cmd, intern = TRUE)
    stop(sprintf("Query failed with status %s: %s", status, error_msg))
  }
  
  # Get results location
  result_cmd <- sprintf(
    'aws athena get-query-execution --query-execution-id %s --query "QueryExecution.ResultConfiguration.OutputLocation" --output text',
    query_id
  )
  s3_path <- system(result_cmd, intern = TRUE)
  
  # Read CSV result
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}

conflicts_prefer(dplyr::count)

receiver_scheme <- run_athena_query("
    SELECT  *
    FROM    nfl_data.vw_receiving_enriched_scheme_agg
")
# ACTUALLY ... THIS IS NOT GOING TO BE EASY.
# WE'RE GOING TO GO HAVE TO GO THROUGH EACH VIEW - FIGURE OUT ... WHICH VIEW IS WHICH

# IN OUR CLUSTERING TOO ... IT'S ALMOST LIKE ... WE USED 20 ROUTES OVER AN ENTIRE SEASON --> LOW. I THINK WE USED 30 FOR 2025, 45 FOR OTHERS ... THAT'S ... WE'LL SEE HOW MUCH WE CUT OUT. LIKELY TOO MUCH. 
# MARKED DOWN TO 25 / 35 FOR ROUTES, TGTS STAY THE SAME

# ATTEMPT 1 FOR THIS SHIT: ITINERARY

# WE JUST BRING IN THE SEASON AGG (FOR NOW - FOR NOW)
# THEN WE BRING IN THE CLUSTERS
# THEN AFTER THAT - ... WE'LL HAVE TO THINK ABOUT THE LT WR AND SHIT LIKE THAT
# BUT THEN ... DO THAT RPART SHIT. 


receiver_scheme$team_abbreviation[which(receiver_scheme$team_abbreviation == "ARI")] = "ARZ"
receiver_scheme$team_abbreviation[which(receiver_scheme$team_abbreviation == "BAL")] = "BLT"
receiver_scheme$team_abbreviation[which(receiver_scheme$team_abbreviation == "CLE")] = "CLV"
receiver_scheme$team_abbreviation[which(receiver_scheme$team_abbreviation == "HOU")] = "HST"
receiver_scheme$team_abbreviation[which(receiver_scheme$team_abbreviation == "LAC" & receiver_scheme$season == 2016)] = "SD"
receiver_scheme$team_abbreviation[which(receiver_scheme$team_abbreviation == "LV" & receiver_scheme$season <= 2019)] = "OAK"

receiver_scheme$team[which(receiver_scheme$team == "ARI")] = "ARZ"
receiver_scheme$team[which(receiver_scheme$team == "BAL")] = "BLT"
receiver_scheme$team[which(receiver_scheme$team == "CLE")] = "CLV"
receiver_scheme$team[which(receiver_scheme$team == "HOU")] = "HST"
receiver_scheme$team[which(receiver_scheme$team == "LAC" & receiver_scheme$season == 2016)] = "SD"
receiver_scheme$team[which(receiver_scheme$team == "LV" & receiver_scheme$season <= 2019)] = "OAK"


align_cluster <- run_athena_query("
    SELECT  *
    FROM    nfl_data.receivers_alignment_cluster_raw
")

rte_cluster <- run_athena_query("
    SELECT  *
    FROM    nfl_data.receiving_rte_cluster_raw
")

tgt_cluster <- run_athena_query("
    SELECT  *
    FROM    nfl_data.receiving_tgt_cluster_raw
")


receiver_scheme_inter_align <- left_join(receiver_scheme, align_cluster, by = c("player", "player_id", "team_abbreviation", "season"))

receiver_scheme_inter_rte <- 
  left_join(receiver_scheme_inter_align, rte_cluster %>% select(-rte_cluster), by = c("player_id" = "player_id", "team_abbreviation" = "team", "season" = "season"))

receiver_scheme_inter_tgt <- 
  left_join(receiver_scheme_inter_rte, tgt_cluster %>% select(-tgt_cluster), by = c("player_id" = "player_id", "team_abbreviation" = "team", "season" = "season"))


# Define counting stat columns to convert NA -> 0
counting_cols <- c(
  "contested_target_rate", "contested_reception_rate",
  "man_pass_plays", "zone_pass_plays", 
  "man_pass_blocks", "zone_pass_blocks",
  "man_routes", "zone_routes",
  "man_targets", "zone_targets",
  "avg_man_target_share", "avg_zone_target_share",
  "man_receptions", "zone_receptions",
  "man_yards", "zone_yards",
  "man_yards_after_catch", "zone_yards_after_catch",
  "man_touchdowns", "zone_touchdowns"
)

# Apply the transformations
receiver_scheme_final <- receiver_scheme_inter_tgt %>%
  mutate(
    # NA -> 0 for counting stats
    across(all_of(counting_cols), ~ replace_na(., 0)),
    # NA -> 'LR' / 'LT' for cluster names
    rte_cluster_name = replace_na(rte_cluster_name, "LR"),
    tgt_cluster_name = replace_na(tgt_cluster_name, "LT")
  )

receiver_scheme_final$man_td_rate = receiver_scheme_final$man_touchdowns / receiver_scheme_final$man_targets
receiver_scheme_final$zone_td_rate = receiver_scheme_final$zone_touchdowns / receiver_scheme_final$zone_targets

receiver_scheme_final$man_tgt_rate = receiver_scheme_final$man_targets / receiver_scheme_final$man_routes
receiver_scheme_final$zone_tgt_rate = receiver_scheme_final$zone_targets / receiver_scheme_final$zone_routes

receiver_scheme_final$man_cp = receiver_scheme_final$man_receptions / receiver_scheme_final$man_targets
receiver_scheme_final$zone_cp = receiver_scheme_final$zone_receptions / receiver_scheme_final$zone_targets

receiver_scheme_final$man_ypa = receiver_scheme_final$man_yards / receiver_scheme_final$man_targets
receiver_scheme_final$zone_ypa = receiver_scheme_final$zone_yards / receiver_scheme_final$zone_targets


colnames(receiver_scheme_final)[which(colnames(receiver_scheme_final) == "cluster_name")] <- "align_cluster_name"

receiver_scheme_final$position_group[which(receiver_scheme_final$position_group == "FB")] <- "HB"


wr_man_zone <- receiver_scheme_final %>%
  filter(position_group == "WR") %>%
  mutate(
    # Differentials (positive = better vs man)
    tgt_rate_diff = man_tgt_rate - zone_tgt_rate,
    ypa_diff = man_ypa - zone_ypa,
    cp_diff = man_cp - zone_cp,
    qbr_diff = avg_qbr_man - avg_qbr_zone,
    td_diff = man_td_rate - zone_td_rate,
    adot_diff = avg_man_adot - avg_zone_adot
  ) %>%
  # Replace Inf/-Inf with NA (from 0/0 divisions)
  mutate(across(c(tgt_rate_diff, ypa_diff, cp_diff, qbr_diff, td_diff, adot_diff), 
                ~ ifelse(is.infinite(.), NA, .))) %>%
  # Factors for CART
  mutate(
    align_cluster = as.factor(align_cluster_name),
    tgt_cluster = as.factor(tgt_cluster_name),
    rte_cluster = as.factor(rte_cluster_name)
  )


# Check distributions
wr_man_zone %>% dplyr::count(align_cluster)
wr_man_zone %>% dplyr::count(tgt_cluster)
wr_man_zone %>% dplyr::count(rte_cluster)

cat("\nTotal WR player-seasons:", nrow(wr_man_zone), "\n")
cat("With valid differentials:", sum(complete.cases(wr_man_zone %>% select(tgt_rate_diff:adot_diff))), "\n")

# Check NA rates by cluster
wr_man_zone %>%
  mutate(has_valid_data = !is.na(tgt_rate_diff) & !is.na(ypa_diff) & 
           !is.na(cp_diff) & !is.na(qbr_diff) & !is.na(td_diff) & !is.na(adot_diff)) %>%
  group_by(tgt_cluster, rte_cluster) %>%
  dplyr::summarise(
    n = n(),
    valid = sum(has_valid_data),
    missing = sum(!has_valid_data),
    pct_missing = round(100 * missing / n, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_missing))
# IF LT AND LR --> YOU'RE YOUR OWN THING. 


# For CART/kmeans: exclude LT+LR and anyone with invalid data
wr_for_modeling <- wr_man_zone %>%
  filter(tgt_cluster != "LT" & rte_cluster != "LR") %>%  # Remove ALL LT, not just LT+LR
  filter(!is.na(tgt_rate_diff) & !is.na(ypa_diff) & !is.na(cp_diff) & 
           !is.na(qbr_diff) & !is.na(td_diff) & !is.na(adot_diff))

cat("WRs for CART/kmeans:", nrow(wr_for_modeling), "\n")


wr_for_modeling <- wr_for_modeling %>%
  mutate(
    z_tgt = (tgt_rate_diff - mean(tgt_rate_diff)) / sd(tgt_rate_diff),
    z_ypa = (ypa_diff - mean(ypa_diff)) / sd(ypa_diff),
    z_cp = (cp_diff - mean(cp_diff)) / sd(cp_diff),
    z_qbr = (qbr_diff - mean(qbr_diff)) / sd(qbr_diff),
    z_td = (td_diff - mean(td_diff)) / sd(td_diff),
    z_adot = (adot_diff - mean(adot_diff)) / sd(adot_diff),
    z_composite = z_tgt + 0.85 * z_ypa + .75 * z_adot + 0.65 * z_cp + 0.85 * z_qbr + 0.2 * z_td
  )

target_vars <- c("tgt_rate_diff", "ypa_diff", "cp_diff", "qbr_diff", "td_diff", "adot_diff", "z_composite")

cart_results <- list()
var_importance <- data.frame()

for (target in target_vars) {
  formula <- as.formula(paste(target, "~ align_cluster + tgt_cluster + rte_cluster"))
  
  tree <- rpart(formula, data = wr_for_modeling, method = "anova",
                control = rpart.control(minsplit = 20, minbucket = 10, cp = 0.01, maxdepth = 4))
  
  cart_results[[target]] <- tree
  
  if (length(tree$variable.importance) > 0) {
    var_importance <- bind_rows(var_importance, data.frame(
      target = target,
      variable = names(tree$variable.importance),
      importance = as.numeric(tree$variable.importance)
    ))
  }
}

# Variable importance
var_importance %>%
  group_by(variable) %>%
  dplyr::summarise(avg_importance = mean(importance), times_selected = n()) %>%
  arrange(desc(avg_importance))

# Visualize
par(mfrow = c(2, 3))
for (target in target_vars) {
  rpart.plot(cart_results[[target]], main = target)
}
par(mfrow = c(1, 1))


kmeans_data <- wr_for_modeling %>% 
  select(z_tgt:z_adot) %>%
  mutate(across(everything(), ~ (. - mean(.)) / sd(.)))


# Elbow + Silhouette analysis
elbow_sil_results <- data.frame()

for (k in 2:12) {
  km <- kmeans(kmeans_data, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(kmeans_data))
  
  elbow_sil_results <- rbind(elbow_sil_results, data.frame(
    k = k,
    bss_tss = km$betweenss / km$totss,
    avg_silhouette = mean(sil[, 3]),
    within_ss = km$tot.withinss
  ))
}

# Print results
elbow_sil_results %>%
  mutate(
    bss_tss_gain = bss_tss - lag(bss_tss),
    sil_change = avg_silhouette - lag(avg_silhouette)
  )

# Plot
par(mfrow = c(1, 3))
plot(elbow_sil_results$k, elbow_sil_results$within_ss, type = "b", 
     xlab = "k", ylab = "Within SS", main = "Elbow Plot")
plot(elbow_sil_results$k, elbow_sil_results$bss_tss, type = "b", 
     xlab = "k", ylab = "BSS/TSS", main = "Variance Explained")
plot(elbow_sil_results$k, elbow_sil_results$avg_silhouette, type = "b", 
     xlab = "k", ylab = "Avg Silhouette", main = "Silhouette")
par(mfrow = c(1, 1))


# SO IT'S 2

km_2 <- kmeans(kmeans_data, centers = 2, nstart = 25)
wr_for_modeling$kmeans_grp_2 <- km_2$cluster

wr_for_modeling %>%
  select(align_cluster, tgt_cluster, rte_cluster, kmeans_grp_2, z_composite) %>%
  arrange(kmeans_grp_2, desc(z_composite))

# Check if it matches tgt_cluster pattern
wr_for_modeling %>%
  group_by(kmeans_grp_2) %>%
  dplyr::summarise(
    n = n(),
    # Side by side comparisons
    man_tgt_share = mean(avg_man_target_share, na.rm = T),
    zone_tgt_share = mean(avg_zone_target_share, na.rm = T),
    
    man_tgt_rate = mean(man_tgt_rate, na.rm = T),
    zone_tgt_rate = mean(zone_tgt_rate, na.rm = T),
    
    man_qbr = mean(avg_qbr_man, na.rm = T),
    zone_qbr = mean(avg_qbr_zone, na.rm = T),
    
    man_cp = mean(man_cp, na.rm = T),
    zone_cp = mean(zone_cp, na.rm = T),
    
    man_ypa = mean(man_ypa, na.rm = T),
    zone_ypa = mean(zone_ypa, na.rm = T),
    
    man_adot = mean(avg_man_adot, na.rm = T),
    zone_adot = mean(avg_zone_adot, na.rm = T),
    
    # Z-scores
    z_tgt = mean(z_tgt, na.rm = T),
    z_ypa = mean(z_ypa, na.rm = T),
    z_cp = mean(z_cp, na.rm = T),
    z_qbr = mean(z_qbr, na.rm = T),
    z_td = mean(z_td, na.rm = T),
    z_adot = mean(z_adot, na.rm = T),
    z_composite = mean(z_composite, na.rm = T)
  )


# What % of each tgt_cluster lands in each kmeans group
wr_for_modeling %>%
  count(kmeans_grp_2, tgt_cluster_name) %>%
  group_by(tgt_cluster_name) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = kmeans_grp_2, values_from = pct, values_fill = 0)

# Same for rte_cluster
wr_for_modeling %>%
  count(kmeans_grp_2, rte_cluster_name) %>%
  group_by(rte_cluster_name) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = kmeans_grp_2, values_from = pct, values_fill = 0)

# Same for align_cluster
wr_for_modeling %>%
  count(kmeans_grp_2, align_cluster_name) %>%
  group_by(align_cluster_name) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = kmeans_grp_2, values_from = pct, values_fill = 0)


# Use CART to find rules that predict kmeans_grp_3
wr_for_modeling$kmeans_grp_2_factor <- as.factor(wr_for_modeling$kmeans_grp_2)

tree_rules <- rpart(
  kmeans_grp_2_factor ~ align_cluster + tgt_cluster + rte_cluster,
  data = wr_for_modeling,
  method = "class",
  control = rpart.control(minsplit = 20, minbucket = 10, cp = 0.01, maxdepth = 4)
)

rpart.plot(tree_rules, main = "Rules to predict kmeans groups")

# Variable importance
tree_rules$variable.importance

# See the actual rules
print(tree_rules)


# LT --> ITS OWN THING
# TGT_CLUSTER IS ST / G, EVERYTHING ELSE


receiver_scheme_final <- receiver_scheme_final %>%
  mutate(man_zone_grp_cluster = case_when(
    # Keep existing WR_LT rule
    (tgt_cluster_name == "LT" | rte_cluster_name == "LR") & position_group == "WR" ~ "WR_LT",
    
    # Class 1 from tree: tgt in regular group, align = SWR, rte in specific clusters
    position_group == "WR" & 
      tgt_cluster_name %in% c("G", "ST") ~ "WR_SHORT",
    
    # All other WR → class 2 (covers nodes 3, 5, 9)
    position_group == "WR" ~ "WR_DEEP",
    
    TRUE ~ NA_character_
  ))

receiver_scheme_final %>%
  filter(position_group == "WR", !(tgt_cluster_name == "LT" | rte_cluster_name == "LR")) %>%
  group_by(man_zone_grp_cluster) %>%
  dplyr::summarise(
    n = n(),
    # Side by side comparisons
    man_tgt_share = mean(avg_man_target_share, na.rm = T),
    zone_tgt_share = mean(avg_zone_target_share, na.rm = T),
    
    man_tgt_rate = mean(man_tgt_rate, na.rm = T),
    zone_tgt_rate = mean(zone_tgt_rate, na.rm = T),
    
    man_qbr = mean(avg_qbr_man, na.rm = T),
    zone_qbr = mean(avg_qbr_zone, na.rm = T),
    
    man_cp = mean(man_cp, na.rm = T),
    zone_cp = mean(zone_cp, na.rm = T),
    
    man_ypa = mean(man_ypa, na.rm = T),
    zone_ypa = mean(zone_ypa, na.rm = T),
    
    man_adot = mean(avg_man_adot, na.rm = T),
    zone_adot = mean(avg_zone_adot, na.rm = T)
  )


# LT IS LT REGARDLESS, PUT LR IN IT TOO
# SHORT - IS JUST "SHORT"
# GRP1 VS SHORTLONG ... THIS IS HARD

# IN GRP1 - BT / G / MT / SMT / ST
# GRP2 - ML / RB / DT

receiver_scheme_final %>%
  filter(position_group == "WR", tgt_cluster_name != "LT") %>%
  count(man_zone_grp_cluster, rte_cluster_name) %>%
  group_by(rte_cluster_name) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = man_zone_grp_cluster, values_from = pct, values_fill = 0)

# SO BASICALLY - LT / LR IS ITS OWN THING
# AFTER THAT - SHORT IS ITS DISTINCT GROUP. rte_cluster_name %in% c("G", "RB", "SMT") & tgt_cluster_name %in% c("G", "MT", "SMT", "ST")
# HONESTLY - OUTSIDE OF THE SHORT ONE - EVERYTHING ELSE IS ... SO IT'LL BE LT / SHORT / REG


####
#### TE
####


# TE setup
te_man_zone <- receiver_scheme_final %>%
  filter(position_group == "TE") %>%
  mutate(
    tgt_rate_diff = man_tgt_rate - zone_tgt_rate,
    ypa_diff = man_ypa - zone_ypa,
    cp_diff = man_cp - zone_cp,
    qbr_diff = avg_qbr_man - avg_qbr_zone,
    td_diff = man_td_rate - zone_td_rate,
    adot_diff = avg_man_adot - avg_zone_adot
  ) %>%
  mutate(across(c(tgt_rate_diff, ypa_diff, cp_diff, qbr_diff, td_diff, adot_diff), 
                ~ ifelse(is.infinite(.), NA, .))) %>%
  mutate(
    align_cluster = as.factor(align_cluster_name),
    tgt_cluster = as.factor(tgt_cluster_name),
    rte_cluster = as.factor(rte_cluster_name)
  )

# Check distributions
te_man_zone %>% count(tgt_cluster)
te_man_zone %>% count(rte_cluster)
te_man_zone %>% count(align_cluster)

cat("\nTotal TE player-seasons:", nrow(te_man_zone), "\n")
cat("With valid differentials:", sum(complete.cases(te_man_zone %>% select(tgt_rate_diff:adot_diff))), "\n")

# Check NA rates by cluster - is LT/LR a problem for TEs?
te_man_zone %>%
  mutate(has_valid_data = !is.na(tgt_rate_diff) & !is.na(ypa_diff) & 
           !is.na(cp_diff) & !is.na(qbr_diff) & !is.na(td_diff) & !is.na(adot_diff)) %>%
  group_by(tgt_cluster, rte_cluster) %>%
  dplyr::summarise(
    n = n(),
    valid = sum(has_valid_data),
    missing = sum(!has_valid_data),
    pct_missing = round(100 * missing / n, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_missing))
# FUCK IT - LT / LR ALWAYS LT / LR

# TE modeling - remove LT, keep valid data
te_for_modeling <- te_man_zone %>%
  filter(tgt_cluster != "LT" & rte_cluster != "LR") %>%
  filter(!is.na(tgt_rate_diff) & !is.na(ypa_diff) & !is.na(cp_diff) & 
           !is.na(qbr_diff) & !is.na(td_diff) & !is.na(adot_diff))

cat("TEs for CART/kmeans:", nrow(te_for_modeling), "\n")

# Z-scores
te_for_modeling <- te_for_modeling %>%
  mutate(
    z_tgt = (tgt_rate_diff - mean(tgt_rate_diff)) / sd(tgt_rate_diff),
    z_ypa = (ypa_diff - mean(ypa_diff)) / sd(ypa_diff),
    z_cp = (cp_diff - mean(cp_diff)) / sd(cp_diff),
    z_qbr = (qbr_diff - mean(qbr_diff)) / sd(qbr_diff),
    z_td = (td_diff - mean(td_diff)) / sd(td_diff),
    z_adot = (adot_diff - mean(adot_diff)) / sd(adot_diff),
    z_composite = z_tgt + 0.85 * z_ypa + 0.75 * z_adot + 0.65 * z_cp + 0.85 * z_qbr + 0.2 * z_td
  )

# Elbow + Silhouette
kmeans_data_te <- te_for_modeling %>% 
  select(z_tgt:z_adot) %>%
  mutate(across(everything(), ~ (. - mean(.)) / sd(.)))

elbow_sil_results_te <- data.frame()

for (k in 2:10) {
  km <- kmeans(kmeans_data_te, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(kmeans_data_te))
  
  elbow_sil_results_te <- rbind(elbow_sil_results_te, data.frame(
    k = k,
    bss_tss = km$betweenss / km$totss,
    avg_silhouette = mean(sil[, 3]),
    within_ss = km$tot.withinss
  ))
}

elbow_sil_results_te %>%
  mutate(
    bss_tss_gain = bss_tss - lag(bss_tss),
    sil_change = avg_silhouette - lag(avg_silhouette)
  )

par(mfrow = c(1, 3))
plot(elbow_sil_results_te$k, elbow_sil_results_te$within_ss, type = "b", 
     xlab = "k", ylab = "Within SS", main = "TE: Elbow Plot")
plot(elbow_sil_results_te$k, elbow_sil_results_te$bss_tss, type = "b", 
     xlab = "k", ylab = "BSS/TSS", main = "TE: Variance Explained")
plot(elbow_sil_results_te$k, elbow_sil_results_te$avg_silhouette, type = "b", 
     xlab = "k", ylab = "Avg Silhouette", main = "TE: Silhouette")
par(mfrow = c(1, 1))


# k=2 for TEs
km_2_te <- kmeans(kmeans_data_te, centers = 2, nstart = 25)
te_for_modeling$kmeans_grp_2 <- km_2_te$cluster

# What do the 2 groups look like?
te_for_modeling %>%
  group_by(kmeans_grp_2) %>%
  dplyr::summarise(
    n = n(),
    man_tgt_rate = mean(man_tgt_rate, na.rm = T),
    zone_tgt_rate = mean(zone_tgt_rate, na.rm = T),
    man_ypa = mean(man_ypa, na.rm = T),
    zone_ypa = mean(zone_ypa, na.rm = T),
    man_qbr = mean(avg_qbr_man, na.rm = T),
    zone_qbr = mean(avg_qbr_zone, na.rm = T),
    man_cp = mean(man_cp, na.rm = T),
    zone_cp = mean(zone_cp, na.rm = T),
    man_adot = mean(avg_man_adot, na.rm = T),
    zone_adot = mean(avg_zone_adot, na.rm = T),
    z_composite = mean(z_composite, na.rm = T)
  )

# What clusters land where?
te_for_modeling %>%
  count(kmeans_grp_2, tgt_cluster) %>%
  group_by(tgt_cluster) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = kmeans_grp_2, values_from = pct, values_fill = 0)

te_for_modeling %>%
  count(kmeans_grp_2, rte_cluster) %>%
  group_by(rte_cluster) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = kmeans_grp_2, values_from = pct, values_fill = 0)

te_for_modeling %>%
  count(kmeans_grp_2, align_cluster) %>%
  group_by(align_cluster) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = kmeans_grp_2, values_from = pct, values_fill = 0)

# CART rules
tree_rules_te <- rpart(
  as.factor(kmeans_grp_2) ~ align_cluster + tgt_cluster + rte_cluster,
  data = te_for_modeling,
  method = "class",
  control = rpart.control(minsplit = 20, minbucket = 10, cp = 0.01, maxdepth = 4)
)

tree_rules_te$variable.importance
print(tree_rules_te)

# Visualize - use basic plot since rpart.plot errored earlier
plot(tree_rules_te)
text(tree_rules_te, use.n = TRUE, cex = 0.8)

# Or try rpart.plot with simpler palette
library(rpart.plot)
rpart.plot(tree_rules_te, main = "TE: Rules for 2 groups", box.palette = "Blues")


# Add TE groups based on CART rules
receiver_scheme_final <- receiver_scheme_final %>%
  mutate(man_zone_grp_cluster = case_when(
    # WR rules
    (tgt_cluster_name == "LT" | rte_cluster_name == "LR") & position_group == "WR" ~ "WR_LT",
    # Class 1 from tree: tgt in regular group, align = SWR, rte in specific clusters
    position_group == "WR" & 
      tgt_cluster_name %in% c("G", "ST") ~ "WR_SHORT",
    # All other WR → class 2 (covers nodes 3, 5, 9)
    position_group == "WR" ~ "WR_DEEP",
    
    # TE_LT - catches long routes/targets first (keep as is)
    (tgt_cluster_name == "LT" | rte_cluster_name == "LR") & position_group == "TE" ~ "TE_LT",
    
    # TE_MAN (Class 1)
    
    # Node 2: align = STE
    # TE_GRP1: STE alignment + short/mid targets
    position_group == "TE" & 
      align_cluster_name == "STE" & 
      tgt_cluster_name %in% c("DT", "ML", "RB", "SMT", "ST") ~ "TE_SHORT",
    
    # TE_GRP1: Flexed + short routes + most targets
    position_group == "TE" & 
      align_cluster_name %in% c("ITE", "RB", "SWR", "WSWR") & 
      rte_cluster_name %in% c("BT", "MT", "RB", "ST") &
      tgt_cluster_name %in% c("DT", "G", "MT", "RB", "SMT", "ST") ~ "TE_SHORT",
    
    # TE_GRP2: Everything else
    position_group == "TE" ~ "TE_DEEP",
    
    TRUE ~ NA_character_
  ))

# Performance by TE group
receiver_scheme_final %>%
  filter(position_group == "TE" & !is.na(man_zone_grp_cluster)) %>%
  group_by(man_zone_grp_cluster) %>%
  dplyr::summarise(
    n = n(),
    man_tgt_rate = mean(man_tgt_rate, na.rm = T),
    zone_tgt_rate = mean(zone_tgt_rate, na.rm = T),
    man_ypa = mean(man_ypa, na.rm = T),
    zone_ypa = mean(zone_ypa, na.rm = T),
    man_qbr = mean(avg_qbr_man, na.rm = T),
    zone_qbr = mean(avg_qbr_zone, na.rm = T),
    man_cp = mean(man_cp, na.rm = T),
    zone_cp = mean(zone_cp, na.rm = T),
    man_adot = mean(avg_man_adot, na.rm = T),
    zone_adot = mean(avg_zone_adot, na.rm = T)
  )
# SO IT WORKS, I GUESS. 


### NOW - TESTING FOR 3
# AND THAT BROKE IT
# SO JUST 2



####
#### RB
####

# RB setup
rb_man_zone <- receiver_scheme_final %>%
  filter(position_group %in% c("HB", "FB")) %>%
  mutate(
    tgt_rate_diff = man_tgt_rate - zone_tgt_rate,
    ypa_diff = man_ypa - zone_ypa,
    cp_diff = man_cp - zone_cp,
    qbr_diff = avg_qbr_man - avg_qbr_zone,
    td_diff = man_td_rate - zone_td_rate,
    adot_diff = avg_man_adot - avg_zone_adot
  ) %>%
  mutate(across(c(tgt_rate_diff, ypa_diff, cp_diff, qbr_diff, td_diff, adot_diff), 
                ~ ifelse(is.infinite(.), NA, .))) %>%
  mutate(
    align_cluster = as.factor(align_cluster_name),
    tgt_cluster = as.factor(tgt_cluster_name),
    rte_cluster = as.factor(rte_cluster_name)
  )

# Check distributions
rb_man_zone %>% count(tgt_cluster)
rb_man_zone %>% count(rte_cluster)
rb_man_zone %>% count(align_cluster)

cat("\nTotal RB player-seasons:", nrow(rb_man_zone), "\n")
cat("With valid differentials:", sum(complete.cases(rb_man_zone %>% select(tgt_rate_diff:adot_diff))), "\n")


# Check NA rates by cluster
rb_man_zone %>%
  mutate(has_valid_data = !is.na(tgt_rate_diff) & !is.na(ypa_diff) & 
           !is.na(cp_diff) & !is.na(qbr_diff) & !is.na(td_diff) & !is.na(adot_diff)) %>%
  group_by(tgt_cluster, rte_cluster) %>%
  dplyr::summarise(
    n = n(),
    valid = sum(has_valid_data),
    missing = sum(!has_valid_data),
    pct_missing = round(100 * missing / n, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_missing))

# RB modeling - remove LT, keep valid data
rb_for_modeling <- rb_man_zone %>%
  filter(tgt_cluster != "LT" & rte_cluster != "LR") %>%
  filter(!is.na(tgt_rate_diff) & !is.na(ypa_diff) & !is.na(cp_diff) & 
           !is.na(qbr_diff) & !is.na(td_diff) & !is.na(adot_diff))

cat("RBs for CART/kmeans:", nrow(rb_for_modeling), "\n")


# Z-scores
rb_for_modeling <- rb_for_modeling %>%
  mutate(
    z_tgt = (tgt_rate_diff - mean(tgt_rate_diff)) / sd(tgt_rate_diff),
    z_ypa = (ypa_diff - mean(ypa_diff)) / sd(ypa_diff),
    z_cp = (cp_diff - mean(cp_diff)) / sd(cp_diff),
    z_qbr = (qbr_diff - mean(qbr_diff)) / sd(qbr_diff),
    z_td = (td_diff - mean(td_diff)) / sd(td_diff),
    z_adot = (adot_diff - mean(adot_diff)) / sd(adot_diff),
    z_composite = z_tgt + 0.85 * z_ypa + 0.75 * z_adot + 0.65 * z_cp + 0.85 * z_qbr + 0.2 * z_td
  )

# Elbow + Silhouette
kmeans_data_rb <- rb_for_modeling %>% 
  select(z_tgt:z_adot) %>%
  mutate(across(everything(), ~ (. - mean(.)) / sd(.)))

elbow_sil_results_rb <- data.frame()

for (k in 2:10) {
  km <- kmeans(kmeans_data_rb, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(kmeans_data_rb))
  
  elbow_sil_results_rb <- rbind(elbow_sil_results_rb, data.frame(
    k = k,
    bss_tss = km$betweenss / km$totss,
    avg_silhouette = mean(sil[, 3]),
    within_ss = km$tot.withinss
  ))
}

elbow_sil_results_rb %>%
  mutate(
    bss_tss_gain = bss_tss - lag(bss_tss),
    sil_change = avg_silhouette - lag(avg_silhouette)
  )

par(mfrow = c(1, 3))
plot(elbow_sil_results_rb$k, elbow_sil_results_rb$within_ss, type = "b", 
     xlab = "k", ylab = "Within SS", main = "RB: Elbow Plot")
plot(elbow_sil_results_rb$k, elbow_sil_results_rb$bss_tss, type = "b", 
     xlab = "k", ylab = "BSS/TSS", main = "RB: Variance Explained")
plot(elbow_sil_results_rb$k, elbow_sil_results_rb$avg_silhouette, type = "b", 
     xlab = "k", ylab = "Avg Silhouette", main = "RB: Silhouette")
par(mfrow = c(1, 1))


# k=3 for RBs
km_2_rb <- kmeans(kmeans_data_rb, centers = 2, nstart = 25)
rb_for_modeling$kmeans_grp_2 <- km_2_rb$cluster

# What do the 3 groups look like?
rb_for_modeling %>%
  group_by(kmeans_grp_2) %>%
  dplyr::summarise(
    n = n(),
    man_tgt_rate = mean(man_tgt_rate, na.rm = T),
    zone_tgt_rate = mean(zone_tgt_rate, na.rm = T),
    man_ypa = mean(man_ypa, na.rm = T),
    zone_ypa = mean(zone_ypa, na.rm = T),
    man_qbr = mean(avg_qbr_man, na.rm = T),
    zone_qbr = mean(avg_qbr_zone, na.rm = T),
    man_cp = mean(man_cp, na.rm = T),
    zone_cp = mean(zone_cp, na.rm = T),
    man_adot = mean(avg_man_adot, na.rm = T),
    zone_adot = mean(avg_zone_adot, na.rm = T),
    z_composite = mean(z_composite, na.rm = T)
  )

# What clusters land where?
rb_for_modeling %>%
  count(kmeans_grp_2, tgt_cluster) %>%
  group_by(tgt_cluster) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = kmeans_grp_2, values_from = pct, values_fill = 0)

rb_for_modeling %>%
  count(kmeans_grp_2, rte_cluster) %>%
  group_by(rte_cluster) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = kmeans_grp_2, values_from = pct, values_fill = 0)

rb_for_modeling %>%
  count(kmeans_grp_2, align_cluster) %>%
  group_by(align_cluster) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = kmeans_grp_2, values_from = pct, values_fill = 0)

# CART rules
tree_rules_rb <- rpart(
  as.factor(kmeans_grp_2) ~ align_cluster + tgt_cluster + rte_cluster,
  data = rb_for_modeling,
  method = "class",
  control = rpart.control(minsplit = 20, minbucket = 10, cp = 0.01, maxdepth = 4)
)

tree_rules_rb$variable.importance
print(tree_rules_rb)

library(rpart.plot)
rpart.plot(tree_rules_rb, main = "HB: Rules for 2 groups", box.palette = "Blues")

receiver_scheme_final <- receiver_scheme_final %>%
  mutate(man_zone_grp_cluster = case_when(
    # WR rules
    (tgt_cluster_name == "LT" | rte_cluster_name == "LR") & position_group == "WR" ~ "WR_LT",
    # Class 1 from tree: tgt in regular group, align = SWR, rte in specific clusters
    position_group == "WR" & 
      tgt_cluster_name %in% c("G", "ST") ~ "WR_SHORT",
    # All other WR → class 2 (covers nodes 3, 5, 9)
    position_group == "WR" ~ "WR_DEEP",
    
    # TE rules
    (tgt_cluster_name == "LT" | rte_cluster_name == "LR") & position_group == "TE" ~ "TE_LT",
    
    # Node 2: align = STE
    position_group == "TE" & 
      align_cluster_name == "STE" & 
      tgt_cluster_name %in% c("DT", "ML", "RB", "SMT", "ST") ~ "TE_SHORT",
    
    # TE_GRP1: Flexed + short routes + most targets
    position_group == "TE" & 
      align_cluster_name %in% c("ITE", "RB", "SWR", "WSWR") & 
      rte_cluster_name %in% c("BT", "MT", "RB", "ST") &
      tgt_cluster_name %in% c("DT", "G", "MT", "RB", "SMT", "ST") ~ "TE_SHORT",
    
    # TE_GRP2: Everything else
    position_group == "TE" ~ "TE_DEEP",
    
    # RB rules
    (tgt_cluster_name == "LT" | rte_cluster_name == "LR") & position_group %in% c("RB", "FB", "HB") ~ "HB_LT",
    
    position_group == "HB" & 
      tgt_cluster_name %in% c("RB", "ST") & 
      rte_cluster_name %in% c("BT", "DT", "RB", "SMT") ~ "HB_DEEP",
    
    # HB_GRP1: Other targets + mid/short routes
    position_group == "HB" & 
      tgt_cluster_name %in% c("BT", "G", "SMT") & 
      rte_cluster_name %in% c("MT", "SMT", "ST") ~ "HB_DEEP",
    
    # HB_GRP2: Everything else
    position_group == "HB" ~ "HB_SHORT",

    TRUE ~ NA_character_
  ))


# Performance by TE group
receiver_scheme_final %>%
  filter(position_group %in% c("HB", "FB") & !is.na(man_zone_grp_cluster)) %>%
  group_by(man_zone_grp_cluster) %>%
  dplyr::summarise(
    n = n(),
    man_tgt_rate = mean(man_tgt_rate, na.rm = T),
    zone_tgt_rate = mean(zone_tgt_rate, na.rm = T),
    man_ypa = mean(man_ypa, na.rm = T),
    zone_ypa = mean(zone_ypa, na.rm = T),
    man_qbr = mean(avg_qbr_man, na.rm = T),
    zone_qbr = mean(avg_qbr_zone, na.rm = T),
    man_cp = mean(man_cp, na.rm = T),
    zone_cp = mean(zone_cp, na.rm = T),
    man_adot = mean(avg_man_adot, na.rm = T),
    zone_adot = mean(avg_zone_adot, na.rm = T)
  )


receiver_scheme_final <- receiver_scheme_final %>%
  # First create the diff columns
  mutate(
    tgt_rate_diff = man_tgt_rate - zone_tgt_rate,
    ypa_diff = man_ypa - zone_ypa,
    cp_diff = man_cp - zone_cp,
    adot_diff = avg_man_adot - avg_zone_adot,
    qbr_diff = avg_qbr_man - avg_qbr_zone
  ) %>%
  # Handle Inf from 0/0 divisions
  mutate(across(c(tgt_rate_diff, ypa_diff, cp_diff, adot_diff, qbr_diff), 
                ~ ifelse(is.infinite(.), NA, .))) %>%
  # Z-scores WITHIN each group
  group_by(man_zone_grp_cluster) %>%
  mutate(
    z_tgt = (tgt_rate_diff - mean(tgt_rate_diff, na.rm = T)) / sd(tgt_rate_diff, na.rm = T),
    z_ypa = (ypa_diff - mean(ypa_diff, na.rm = T)) / sd(ypa_diff, na.rm = T),
    z_cp = (cp_diff - mean(cp_diff, na.rm = T)) / sd(cp_diff, na.rm = T),
    z_adot = (adot_diff - mean(adot_diff, na.rm = T)) / sd(adot_diff, na.rm = T),
    z_qbr = (qbr_diff - mean(qbr_diff, na.rm = T)) / sd(qbr_diff, na.rm = T)
  ) %>%
  # Composite (no td_rate)
  mutate(
    z_composite = z_tgt + 0.85 * z_ypa + 0.65 * z_cp + 0.45 * z_adot + 0.85 * z_qbr,
    z_score_percentile_calc = round(100 * percent_rank(z_composite), 0)
  ) %>%
  ungroup() %>%
  # Override for one-sided target players
  mutate(
    z_score_percentile = case_when(
      man_targets > 0 & (is.na(zone_targets) | zone_targets == 0) ~ 100,  # man only
      zone_targets > 0 & (is.na(man_targets) | man_targets == 0) ~ 0,     # zone only
      (is.na(man_targets) | man_targets == 0) & (is.na(zone_targets) | zone_targets == 0) ~ 50,  # no targets
      TRUE ~ z_score_percentile_calc  # use calculated
    )
  )

# Check how many fall into each bucket
receiver_scheme_final <- receiver_scheme_final %>%
  mutate(z_source = case_when(
    man_targets > 0 & (is.na(zone_targets) | zone_targets == 0) ~ "forced_100",
    zone_targets > 0 & (is.na(man_targets) | man_targets == 0) ~ "forced_0",
    (is.na(man_targets) | man_targets == 0) & (is.na(zone_targets) | zone_targets == 0) ~ "forced_50",
    TRUE ~ "calculated"
  ))

receiver_scheme_final$man_zone_grp_cluster[which(is.na(receiver_scheme_final$man_zone_grp_cluster))] <- "OTH"

cluster_join <- sqldf("SELECT player, 
              player_id, 
              team_abbreviation, 
              season, 
              position_group,
              align_cluster_name,
              rte_cluster_name,
              tgt_cluster_name,
              man_zone_grp_cluster,
              z_score_percentile,
              z_source
        FROM  receiver_scheme_final
      GROUP BY  player, 
                player_id, 
                team_abbreviation, 
                season, 
                position_group,
                align_cluster_name,
                rte_cluster_name,
                tgt_cluster_name,
                man_zone_grp_cluster,
                z_score_percentile,
                z_source
      ")


View(cluster_join %>% filter(season == 2025, team_abbreviation == "PHI"))
