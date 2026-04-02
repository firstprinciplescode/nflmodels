# First, create wind/temp buckets (excluding NAs)
qb_stats_df_final_filtered <- qb_stats_df_final %>%
  filter(!is.na(wind)) %>%
  group_by(qbgrp_ssn) %>%
  filter(n() > 4) 

colnames(qb_stats_df_final_filtered)

qb_stats_df_final_filtered %>%
  mutate(wind_na = ifelse(is.na(wind), 0, wind)) %>%
  group_by(wind_na) %>%
  summarise(
    n = n(),
    across(ends_with("_rank_def"), ~mean(.x, na.rm = TRUE))
  )


# Write locally
write.csv(qb_stats_df_final_filtered %>%
            mutate(wind_na = ifelse(is.na(wind), 0, wind)) %>%
            group_by(wind_na) %>%
            summarise(
              n = n(),
              across(ends_with("_rank_def"), ~mean(.x, na.rm = TRUE))
            )
          , "wind_rank_summary.csv", row.names = FALSE)

# Upload to S3
system("aws s3 cp wind_rank_summary.csv s3://nfl-pff-data-lucas/analysis/wind_rank_summary.csv")



# Get all the metric names
metrics <- colnames(qb_stats_df_final_filtered)[grepl("_rank_def$", colnames(qb_stats_df_final_filtered))]

# Function to plot one metric
plot_wind_metric <- function(metric_name) {
  qb_stats_df_final_filtered %>%
    filter(!is.na(wind)) %>%
    ggplot(aes(x = wind, y = .data[[metric_name]])) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "loess", se = TRUE) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = metric_name, y = "Rank", x = "Wind (mph)")
}

# Cycle through one at a time - hit Enter to advance
cycle_plots <- function() {
  for (i in seq_along(metrics)) {
    print(plot_wind_metric(metrics[i]))
    cat(sprintf("\n[%d/%d] %s\nPress Enter for next...", i, length(metrics), metrics[i]))
    readline()
  }
}

# Run it
cycle_plots()

# Or just call one directly
plot_wind_metric("pbp_xypc_rank_def")


# Above/below 10mph comparison for your flagged metrics
flagged_metrics <- c("pass_rate_rank_def", "scr_rate_rank_def", "acc_rate_rank_def", 
                     "ypa_rank_def", "plays_rank_def", "pbp_xtds_rank_def",
                     "blitz_gr_rank_def", "blitz_qbr_rank_def", "short_qbr_rank_def",
                     "more_gr_rank_def", "more_qbr_rank_def", "pa_gr_rank_def",
                     "no_pressure_gr_rank_def", "no_pressure_qbr_rank_def")


qb_stats_df_final_filtered %>%
  mutate(wind_na = ifelse(is.na(wind), 0, wind)) %>%
  filter(!is.na(wind_na)) %>%
  mutate(wind_10 = ifelse(wind_na >= 10, "10+", "<10")) %>%
  group_by(wind_10) %>%
  summarise(n = n(),
            across(all_of(flagged_metrics), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-c(wind_10, n)) %>%
  select(-n) %>%  # drop n before the pivot
  pivot_wider(names_from = wind_10, values_from = value) %>%
  mutate(diff = `10+` - `<10`)



# Function to plot one metric
plot_wind_metric <- function(metric_name) {
  qb_stats_df_final_filtered %>%
    ggplot(aes(x = temp, y = .data[[metric_name]])) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "loess", se = TRUE) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = metric_name, y = "Rank", x = "Temp (mph)")
}

# Cycle through one at a time - hit Enter to advance
cycle_plots_temp <- function() {
  for (i in seq_along(metrics)) {
    print(plot_wind_metric(metrics[i]))
    cat(sprintf("\n[%d/%d] %s\nPress Enter for next...", i, length(metrics), metrics[i]))
    readline()
  }
}


cycle_plots_temp()



# Above/below 10mph comparison for your flagged metrics
flagged_metrics_temp <- c("pass_rate_rank_def", "fastr_xpass_rate_rank_def", "part_xpass_rate_rank_def", "acc_rate_rank_def", "medium_rate_rank_def")


qb_stats_df_final_filtered %>%
  mutate(temp_50 = ifelse(temp <= 50, "50-", "51+")) %>%
  group_by(temp_50) %>%
  summarise(n = n(),
            across(all_of(flagged_metrics_temp), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-c(temp_50, n)) %>%
  select(-n) %>%  # drop n temp_50 the pivot
  pivot_wider(names_from = temp_50, values_from = value) %>%
  mutate(diff = `51+` - `50-`)


qb_stats_df_final_filtered %>%
  mutate(
    wind_na = ifelse(is.na(wind), 0, wind),
    weather_bucket = case_when(
      wind_na >= 10 & temp <= 50 ~ "Bad (10+ wind, 50- temp)",
      wind_na >= 10 & temp > 50 ~ "Wind only (10+)",
      wind_na < 10 & temp <= 50 ~ "Cold only (50-)",
      TRUE ~ "Good (<10 wind, 51+ temp)"
    )
  ) %>%
  group_by(weather_bucket) %>%
  summarise(
    n = n(),
    across(all_of(c(flagged_metrics, flagged_metrics_temp)), ~mean(.x, na.rm = TRUE))
  ) %>%
  pivot_longer(-c(weather_bucket, n), names_to = "metric", values_to = "rank") %>%
  select(-n) %>%
  pivot_wider(names_from = weather_bucket, values_from = rank)



# Write locally
write.csv(qb_stats_df_final_filtered %>%
            mutate(
              wind_na = ifelse(is.na(wind), 0, wind),
              weather_bucket = case_when(
                wind_na >= 10 & temp <= 50 ~ "Bad (10+ wind, 50- temp)",
                wind_na >= 10 & temp > 50 ~ "Wind only (10+)",
                wind_na < 10 & temp <= 50 ~ "Cold only (50-)",
                TRUE ~ "Good (<10 wind, 51+ temp)"
              )
            ) %>%
            group_by(weather_bucket) %>%
            summarise(
              n = n(),
              across(all_of(c(flagged_metrics, flagged_metrics_temp)), ~mean(.x, na.rm = TRUE))
            ) %>%
            pivot_longer(-c(weather_bucket, n), names_to = "metric", values_to = "rank") %>%
            select(-n) %>%
            pivot_wider(names_from = weather_bucket, values_from = rank)
          , "wind_temp_rank_summary.csv", row.names = FALSE)

# Upload to S3
system("aws s3 cp wind_temp_rank_summary.csv s3://nfl-pff-data-lucas/analysis/wind_temp_rank_summary.csv")



# Get your comp teams
qb_teams <- c(comparison_less_func("CARYoung-2025", .96)$QB, "CARYoung-2025")
def_teams <- c(comparison_less_def_func("LA2025", .92)$QB, "LA2025")

qb_stats_df_final %>%
  mutate(
    wind_na = ifelse(is.na(wind), 0, wind),
    weather = case_when(
      wind_na >= 10 & temp <= 50 ~ "1_Both",
      wind_na >= 10 & temp > 50 ~ "2_Wind",
      wind_na < 10 & temp <= 50 ~ "3_Cold",
      TRUE ~ "4_Good"
    ),
    comp_bucket = case_when(
      qbgrp_ssn %in% qb_teams & def_ssn %in% def_teams ~ "Both",
      qbgrp_ssn %in% qb_teams & def_ssn %ni% def_teams ~ "Off only",
      qbgrp_ssn %ni% qb_teams & def_ssn %in% def_teams ~ "Def only",
      TRUE ~ "Neither"
    )
  ) %>%
  filter(comp_bucket != "Neither") %>%
  group_by(comp_bucket, weather) %>%
  summarise(
    n = n(),
    tds = mean(tds, na.rm = T),
    pbp_xtds = mean(pbp_xtds, na.rm = T),
    pbp_xtds_rank = mean(pbp_xtds_rank_def, na.rm = T),
    ypa = mean(ypa, na.rm = T),
    ypa_rank = mean(ypa_rank_def, na.rm = T),
    acc_rate = mean(acc_rate, na.rm = T),
    acc_rate_rank = mean(acc_rate_rank_def, na.rm = T),
    pressure_rate = mean(pressure_rate, na.rm = T),
    pressure_rate_rank = mean(pressure_rate_rank_def, na.rm = T),
    twp_rate = mean(twp_rate, na.rm = T),
    twp_rate_rank = mean(twp_rate_rank_def, na.rm = T),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = weather, 
    values_from = c(n, tds, pbp_xtds, pbp_xtds_rank, ypa, ypa_rank, acc_rate, acc_rate_rank, pressure_rate, pressure_rate_rank, twp_rate, twp_rate_rank)
  )

rm(qb_teams)
rm(def_teams)


run_all_comps <- function(qb, def) {
  list(
    blitz_off = comparison_blitz_func(qb, 1.035),
    depth_off = comparison_depth_func(qb, 1.025),
    less_off = comparison_less_func(qb, .96),
    pa_off = comparison_pa_func(qb, 1.025),
    pressure_off = comparison_pressure_func(qb, .98),
    blitz_def = comparison_blitz_def_func(def, .975),
    depth_def = comparison_depth_def_func(def, .975),
    less_def = comparison_less_def_func(def, .92),
    pa_def = comparison_pa_def_func(def, .945),
    pressure_def = comparison_pressure_def_func(def, .95)
  )
}

run_all_comps("CARYoung-2025", "LA2025")

library(ggpubr)

plot_precip_metric <- function(metric_name, precip_var = "rain_ind") {
  qb_stats_df_final_filtered %>%
    filter(!is.na(.data[[precip_var]])) %>%
    mutate(precip_factor = factor(.data[[precip_var]], levels = c(0, 1), labels = c("No", "Yes"))) %>%
    ggplot(aes(x = precip_factor, y = .data[[metric_name]], fill = precip_factor)) +
    geom_boxplot(alpha = 0.7) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral")) +
    stat_compare_means(method = "wilcox.test", label = "p.format") +  # or method = "t.test"
    theme_minimal() +
    labs(title = paste(metric_name, "-", precip_var), 
         y = "Rank", x = precip_var) +
    theme(legend.position = "none")
}
# Box plot version - shows distribution
plot_precip_metric <- function(metric_name, precip_var = "rain_ind") {
  qb_stats_df_final_filtered %>%
    filter(!is.na(.data[[precip_var]])) %>%
    mutate(precip_factor = factor(.data[[precip_var]], levels = c(0, 1), labels = c("No", "Yes"))) %>%
    ggplot(aes(x = precip_factor, y = .data[[metric_name]], fill = precip_factor)) +
    geom_boxplot(alpha = 0.7) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "coral")) +
    theme_minimal() +
    labs(title = paste(metric_name, "-", precip_var), 
         y = "Rank", x = precip_var) +
    theme(legend.position = "none")
}

# Cycle through for rain
cycle_plots_rain <- function() {
  for (i in seq_along(metrics)) {
    print(plot_precip_metric(metrics[i], "rain_ind"))
    cat(sprintf("\n[%d/%d] %s\nPress Enter for next...", i, length(metrics), metrics[i]))
    readline()
  }
}

# Cycle through for snow
cycle_plots_snow <- function() {
  for (i in seq_along(metrics)) {
    print(plot_precip_metric(metrics[i], "snow_ind"))
    cat(sprintf("\n[%d/%d] %s\nPress Enter for next...", i, length(metrics), metrics[i]))
    readline()
  }
}

# Run em
cycle_plots_rain()
cycle_plots_snow()

# Or single calls
plot_precip_metric("pbp_xtds_rank_def", "rain_ind")
plot_precip_metric("acc_rate_rank_def", "snow_ind")