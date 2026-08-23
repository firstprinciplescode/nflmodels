
# pbp_part_combined_join
# pff_ids

# Option 1: Specify what to KEEP, remove everything else
keep_objects <- c("combined_pbp", "con", "cluster_join", "combined_ids", "run_athena_query", "rushing_summary", "combined_ids")
rm(list = setdiff(ls(), keep_objects))

conflict_prefer_all("dplyr", quiet = TRUE)

'%ni%' <- Negate('%in%')

run_athena_query <- function(sql, max_wait = 120) {
  # Start query
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  cat("Query ID:", query_id, "\n")
  
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
  
  cat("Final status:", status, "\n")
  
  if (status != "SUCCEEDED") {
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
  s3_path <- trimws(system(result_cmd, intern = TRUE))
  cat("S3 path:", s3_path, "\n")
  
  # Check if file exists
  Sys.sleep(2)  # Give S3 a moment
  check_cmd <- sprintf("aws s3 ls %s", s3_path)
  cat("File check:", system(check_cmd, intern = TRUE), "\n")
  
  # Read CSV result
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}


rushing_summary <- run_athena_query("
    SELECT  *
    FROM    nfl_data.rushing_summary
")

run_snap_counts_base <- run_athena_query("
    SELECT  *
    FROM    nfl_data.vw_play_counts_enriched 
    WHERE   snap_counts_run > 0
")

run_snap_counts_base <- run_snap_counts_base %>% 
  mutate(position_group = case_when(
    position %in% c("HB", "FB", "LG", "LT", "C", "RG", "RT") ~ "HB",
    position == "QB" ~ "QB",
    grepl("WR|TE", position) ~ "REC",
    TRUE ~ "OTHER"
  ))

qb_scramble_pbp <- combined_pbp |> filter(qb_kneel == 0, qb_scramble == 1, rush_attempt == 1) |>
  group_by(rusher_player_name, rusher_player_id, posteam, week, season) |>
  summarize(
    scrambles = sum(qb_scramble, na.rm = TRUE)
  )

qb_scramble_counts <- left_join(qb_scramble_pbp, 
                                combined_ids %>% dplyr::select(player_id, team, week, season, player, gsis_id) %>% distinct(), by = c("rusher_player_id" = "gsis_id", "week" = "week", "season" = "season", "posteam" = "team"))

run_snap_counts <- left_join(run_snap_counts_base, qb_scramble_counts %>% ungroup() %>% dplyr::select(player_id, posteam, week, season, scrambles), by = c("player_id" = "player_id", "week" = "week", "season" = "season", "abbreviation" = "posteam"))

run_snap_counts <- left_join(run_snap_counts, combined_ids %>% ungroup() %>% dplyr::select(player_id, team, week, season, gsis_id) %>% distinct(), by = c("player_id" = "player_id", "week" = "week", "season" = "season", "abbreviation" = "team"))

run_snap_counts <- run_snap_counts %>%
  dplyr::group_by(player_id) %>%
  dplyr::arrange(player_id) %>%
  fill(gsis_id, .direction = "downup") %>%
  dplyr::ungroup()


run_snap_counts$scrambles[which(is.na(run_snap_counts$scrambles))] <- 0

run_snap_counts <- run_snap_counts %>%
  mutate(rushes_final = pmax(snap_counts_run - scrambles, 0))


run_snap_counts_rank <- sqldf("
    SELECT *,
           ROW_NUMBER() OVER (
             PARTITION BY abbreviation, position_group, game_id
             ORDER BY rushes_final DESC, snap_counts_run DESC, snap_counts_total_run DESC, snap_counts_total_run DESC, snap_counts_total DESC
           ) AS pos_rank,
           ROW_NUMBER() OVER (
             PARTITION BY abbreviation, game_id
             ORDER BY rushes_final DESC, snap_counts_run DESC, snap_counts_total_run DESC, snap_counts_total_run DESC, snap_counts_total DESC
           ) AS team_rank
    FROM run_snap_counts
")


run_snap_counts_rank <- run_snap_counts_rank %>% 
  ungroup() %>% 
  group_by(game_id, abbreviation, week, season) %>% 
  mutate(total_rushes = sum(rushes_final, na.rm = T)) %>% ungroup() %>%
  mutate(rush_proportion = rushes_final / total_rushes)

rush_order_df <- run_snap_counts_rank %>% 
  dplyr::select(player, player_id, position_group, abbreviation, week, season, status, pos_rank, team_rank, rush_proportion, gsis_id)


rush_order_df <- rush_order_df %>%
  mutate(rank_grp = case_when(
    rush_proportion > 0.5 ~ "A",
    rush_proportion >= .15 ~ "B",
    TRUE ~ "C"
  ))

rush_order_df <- rush_order_df %>%
  group_by(player_id) %>%
  arrange(player_id) %>%
  fill(gsis_id, .direction = "downup") %>%
  ungroup()


pbp_rush <- 
  left_join(combined_pbp %>% 
    filter(rush == 1, qb_scramble == 0, qb_kneel == 0), 
    rush_order_df %>% dplyr::select(gsis_id, position_group:rush_proportion, rank_grp) %>% 
      distinct(), 
    by = c("rusher_player_id" = "gsis_id", "posteam" = "abbreviation", "week" = "week", "season" = "season"))



####
####
####

fourth_down_ydstogo_explorer <- combined_pbp %>%
  filter(!is.na(mod_ydstogo), down == 4) %>%
  group_by(down, mod_ydstogo) %>%
  summarise(
    n = n(),
    pct_of_rushes = n() / nrow(combined_pbp %>% filter(!is.na(mod_ydstogo))),
    mean_xpass = mean(mixed_xpass, na.rm = TRUE),
    sd_xpass = sd(mixed_xpass, na.rm = TRUE),
    q25_xpass = quantile(mixed_xpass, 0.25, na.rm = TRUE),
    median_xpass = median(mixed_xpass, na.rm = TRUE),
    q75_xpass = quantile(mixed_xpass, 0.75, na.rm = TRUE),
    actual_pass_rate = mean(pass, na.rm = TRUE),  # if you have this column
    .groups = "drop"
  ) %>%
  arrange(desc(mean_xpass))

print(fourth_down_ydstogo_explorer, n = 15)

View(fourth_down_ydstogo_explorer)


fourth_down_ydstogo_explorer %>%
  ggplot(aes(x = mod_ydstogo, y = actual_pass_rate)) + 
  geom_line() + 
  theme_minimal()


####
####


#######################
# 1. EXPLORE BY DOWN - RAW YDSTOGO VALUES
#######################

# Function to explore any down
explore_down <- function(down_num) {
  combined_pbp %>%
    filter(!is.na(mod_ydstogo), down == down_num) %>%
    group_by(mod_ydstogo) %>%
    summarise(
      n = n(),
      pass_rate = mean(pass, na.rm = TRUE),
      mean_xpass = mean(mixed_xpass, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(mod_ydstogo) %>%
    mutate(
      cumulative_n = cumsum(n),
      pct_of_down = n / sum(n),
      # Change from previous ydstogo
      pass_rate_diff = pass_rate - lag(pass_rate),
      xpass_diff = mean_xpass - lag(mean_xpass)
    )
}

first_down <- explore_down(1)
second_down <- explore_down(2)
third_down <- explore_down(3)
fourth_down <- explore_down(4)

print(first_down, n = 25)
print(second_down, n = 25)
print(third_down, n = 25)
print(fourth_down, n = 25)


#######################
# 2. TEST ADJACENT YDSTOGO - WHERE ARE THE REAL BREAKS?
#######################

# Function to test all adjacent pairs within a down
test_adjacent_breaks <- function(down_num, max_ytg = 15) {
  
  down_data <- combined_pbp %>%
    filter(!is.na(mod_ydstogo), down == down_num, mod_ydstogo <= max_ytg)
  
  results <- data.frame(
    down = integer(),
    ytg_low = integer(),
    ytg_high = integer(),
    n_low = integer(),
    n_high = integer(),
    pass_rate_low = numeric(),
    pass_rate_high = numeric(),
    pass_rate_diff = numeric(),
    p_value = numeric()
  )
  
  ytg_values <- sort(unique(down_data$mod_ydstogo))
  
  for (i in 1:(length(ytg_values) - 1)) {
    ytg_low <- ytg_values[i]
    ytg_high <- ytg_values[i + 1]
    
    group_low <- down_data %>% filter(mod_ydstogo == ytg_low) %>% pull(pass)
    group_high <- down_data %>% filter(mod_ydstogo == ytg_high) %>% pull(pass)
    
    if (length(group_low) > 10 && length(group_high) > 10) {
      # Proportion test for binary outcome
      test <- prop.test(
        x = c(sum(group_low), sum(group_high)),
        n = c(length(group_low), length(group_high))
      )
      p_val <- test$p.value
    } else {
      p_val <- NA
    }
    
    results <- rbind(results, data.frame(
      down = down_num,
      ytg_low = ytg_low,
      ytg_high = ytg_high,
      n_low = length(group_low),
      n_high = length(group_high),
      pass_rate_low = mean(group_low, na.rm = TRUE),
      pass_rate_high = mean(group_high, na.rm = TRUE),
      pass_rate_diff = mean(group_high, na.rm = TRUE) - mean(group_low, na.rm = TRUE),
      p_value = p_val
    ))
  }
  
  results %>%
    mutate(
      significant = ifelse(p_value < 0.05, "***", ""),
      suggested_break = ifelse(p_value < 0.05 & abs(pass_rate_diff) > 0.02, "BREAK", "")
    )
}

breaks_1st <- test_adjacent_breaks(1)
breaks_2nd <- test_adjacent_breaks(2)
breaks_3rd <- test_adjacent_breaks(3)
breaks_4th <- test_adjacent_breaks(4)

cat("\n=== 1ST DOWN ADJACENT TESTS ===\n")
print(breaks_1st, n = 20)

cat("\n=== 2ND DOWN ADJACENT TESTS ===\n")
print(breaks_2nd, n = 20)

cat("\n=== 3RD DOWN ADJACENT TESTS ===\n")
print(breaks_3rd, n = 20)

cat("\n=== 4TH DOWN ADJACENT TESTS ===\n")
print(breaks_4th, n = 20)


#######################
# 3. TEST CANDIDATE GROUPINGS - ARE GROUPS INTERNALLY HOMOGENEOUS?
#######################

# Function to test if a range of ydstogo should be ONE group or split
test_group_homogeneity <- function(down_num, ytg_min, ytg_max) {
  
  group_data <- combined_pbp %>%
    filter(down == down_num, mod_ydstogo >= ytg_min, mod_ydstogo <= ytg_max)
  
  if (nrow(group_data) < 50) return(NULL)
  
  # ANOVA within the group - is there significant variation?
  if (length(unique(group_data$mod_ydstogo)) > 1) {
    aov_result <- aov(pass ~ factor(mod_ydstogo), data = group_data)
    p_val <- summary(aov_result)[[1]][["Pr(>F)"]][1]
  } else {
    p_val <- NA
  }
  
  data.frame(
    down = down_num,
    ytg_range = paste0(ytg_min, "-", ytg_max),
    n = nrow(group_data),
    pass_rate = mean(group_data$pass, na.rm = TRUE),
    sd_pass_rate = sd(group_data$pass, na.rm = TRUE),
    within_group_p = p_val,
    homogeneous = ifelse(is.na(p_val) | p_val > 0.05, "YES", "NO - CONSIDER SPLIT")
  )
}

# Test your current groupings
cat("\n=== TESTING CURRENT 2ND DOWN GROUPS ===\n")
rbind(
  test_group_homogeneity(2, 1, 1),
  test_group_homogeneity(2, 2, 4),
  test_group_homogeneity(2, 5, 7),
  test_group_homogeneity(2, 8, 10),
  test_group_homogeneity(2, 11, 20)
) %>% print()

cat("\n=== TESTING CURRENT 3RD DOWN GROUPS ===\n")
rbind(
  test_group_homogeneity(3, 1, 1),
  test_group_homogeneity(3, 2, 3),
  test_group_homogeneity(3, 4, 20)
) %>% print()


#######################
# 4. VISUALIZE - FIND NATURAL BREAKS
#######################

library(ggplot2)

# Plot pass rate by ydstogo for each down
combined_pbp %>%
  filter(!is.na(mod_ydstogo), mod_ydstogo <= 15) %>%
  group_by(down, mod_ydstogo) %>%
  summarise(pass_rate = mean(pass, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n > 50) %>%
  ggplot(aes(x = mod_ydstogo, y = pass_rate, color = factor(down))) +
  geom_line(size = 1) +
  geom_point(aes(size = n)) +
  scale_x_continuous(breaks = 1:15) +
  labs(
    title = "Pass Rate by Yards to Go (by Down)",
    x = "Yards to Go",
    y = "Pass Rate",
    color = "Down",
    size = "Sample Size"
  ) +
  theme_minimal()

# Faceted version - easier to see breaks
combined_pbp %>%
  filter(!is.na(mod_ydstogo), mod_ydstogo <= 15) %>%
  group_by(down, mod_ydstogo) %>%
  summarise(pass_rate = mean(pass, na.rm = TRUE), n = n(), .groups = "drop") %>%
  filter(n > 50) %>%
  ggplot(aes(x = mod_ydstogo, y = pass_rate)) +
  geom_line(size = 1) +
  geom_point(aes(size = n)) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = 1:15) +
  facet_wrap(~down, scales = "free_y", labeller = labeller(down = c("1" = "1st Down", "2" = "2nd Down", "3" = "3rd Down", "4" = "4th Down"))) +
  labs(
    title = "Pass Rate by Yards to Go",
    x = "Yards to Go",
    y = "Pass Rate"
  ) +
  theme_minimal()


#######################
# 5. SUGGEST OPTIMAL GROUPINGS ALGORITHMICALLY
#######################

# Change point detection - where do the biggest jumps occur?
suggest_breaks <- function(down_num, max_breaks = 4) {
  
  down_summary <- combined_pbp %>%
    filter(!is.na(mod_ydstogo), down == down_num, mod_ydstogo <= 15) %>%
    group_by(mod_ydstogo) %>%
    summarise(pass_rate = mean(pass, na.rm = TRUE), n = n(), .groups = "drop") %>%
    filter(n > 30) %>%
    arrange(mod_ydstogo)
  
  # Calculate absolute difference in pass rate
  down_summary <- down_summary %>%
    mutate(
      pass_rate_jump = abs(pass_rate - lag(pass_rate)),
      weighted_jump = pass_rate_jump * sqrt(pmin(n, lag(n)))  # weight by sample size
    )
  
  # Top breaks
  top_breaks <- down_summary %>%
    filter(!is.na(weighted_jump)) %>%
    arrange(desc(weighted_jump)) %>%
    head(max_breaks) %>%
    pull(mod_ydstogo)
  
  cat(sprintf("\nDown %d - Suggested break points (before these values): %s\n", 
              down_num, paste(sort(top_breaks), collapse = ", ")))
  
  down_summary
}

suggest_breaks(1)
suggest_breaks(2)
suggest_breaks(3)
suggest_breaks(4)


# 1ST<9, 1ST+10, 1ST>11
# 2ND1-2, 2ND3-5, 2ND6-10, 2ND11+
# 3RD1, 3RD2-3, 3RD4+
# 4TH1, 4TH2+


####
####
####


pbp_rush <- pbp_rush %>% 
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

pbp_rush$rank_grp[which(is.na(pbp_rush$rank_grp))] <- "C"

pbp_rush <- pbp_rush %>% group_by(game_id, posteam) %>% 
  mutate(med_run_xpass = mean(mixed_xpass, na.rm = T),
         run_xpass_diff = mixed_xpass - med_run_xpass)

rusher_xpass_diff_df <- pbp_rush %>%
  # filter(season == 2022, posteam == "BLT", !is.na(rusher_player_name), qb_kneel == 0) %>%
  group_by(rusher_player_id, rank_grp, posteam, season) %>%
  summarize(
    q20_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.2, na.rm = TRUE),
    q35_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.35, na.rm = TRUE),
    q50_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.5, na.rm = TRUE),
    q65_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.65, na.rm = TRUE),
    q80_xpass = quantile(mixed_xpass[qb_scramble == 0], 0.8, na.rm = TRUE),
    q20_xpass_diff = quantile(run_xpass_diff[qb_scramble == 0], 0.2, na.rm = TRUE),
    q35_xpass_diff = quantile(run_xpass_diff[qb_scramble == 0], 0.35, na.rm = TRUE),
    q50_xpass_diff = quantile(run_xpass_diff[qb_scramble == 0], 0.5, na.rm = TRUE),
    q65_xpass_diff = quantile(run_xpass_diff[qb_scramble == 0], 0.65, na.rm = TRUE),
    q80_xpass_diff = quantile(run_xpass_diff[qb_scramble == 0], 0.8, na.rm = TRUE),    
    n = n()
  ) %>% filter(n > 5)


rusher_xpass_diff_df %>%
  filter(n >= 20) %>%
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
