# S3 Configuration
bucket <- "nfl-pff-data-lucas"
prefix <- "models/"

load_model_from_s3 <- function(model_name, bucket, prefix) {
  full_path <- paste0(prefix, model_name)
  temp_file <- tempfile(fileext = ".model")
  save_object(full_path, bucket = bucket, file = temp_file)
  model <- xgb.load(temp_file)
  unlink(temp_file)
  return(model)
}

load_artifacts_from_s3 <- function(artifact_name, bucket, prefix) {
  full_path <- paste0(prefix, artifact_name, "_artifacts.rds")
  local_path <- paste0(getwd(), "/", artifact_name, "_artifacts.rds")
  save_object(full_path, bucket = bucket, file = local_path)
  return(readRDS(local_path))
}

impute_weather <- function(data) {
  data %>%
    mutate(
      wind = ifelse(is.na(wind), 0, wind),
      temp = ifelse(is.na(temp), 70, temp),
      rain_ind = ifelse(is.na(rain_ind), 0, rain_ind),
      snow_ind = ifelse(is.na(snow_ind), 0, snow_ind)
    )
}

create_dmatrix <- function(data, artifacts, use_weather = FALSE) {
  if (use_weather) {
    data <- impute_weather(data)
  }
  X <- data %>% select(all_of(artifacts$feature_names)) %>% as.matrix()
  return(xgb.DMatrix(data = X))
}

# ============================================================================
# LOAD MODELS
# ============================================================================

xgb_part_xtd <- load_model_from_s3("xgb_part_xtd.model", bucket, prefix)
xgb_part_xtd_artifacts <- load_artifacts_from_s3("xgb_part_xtd", bucket, prefix)

xgb_part_rush_xtd <- load_model_from_s3("xgb_part_rush_xtd.model", bucket, prefix)
xgb_part_rush_xtd_artifacts <- load_artifacts_from_s3("xgb_part_rush_xtd", bucket, prefix)

xgb_part_after_pass_xtd <- load_model_from_s3("xgb_part_after_pass_xtd.model", bucket, prefix)
xgb_part_after_pass_xtd_artifacts <- load_artifacts_from_s3("xgb_part_after_pass_xtd", bucket, prefix)

xgb_part_xpass <- load_model_from_s3("xgb_part_xpass.model", bucket, prefix)
xgb_part_xpass_artifacts <- load_artifacts_from_s3("xgb_part_xpass", bucket, prefix)

xgb_part_cp <- load_model_from_s3("xgb_part_cp.model", bucket, prefix)
xgb_part_cp_artifacts <- load_artifacts_from_s3("xgb_part_cp", bucket, prefix)

xgb_part_yac <- load_model_from_s3("xgb_part_yac_temp.model", bucket, prefix)
xgb_part_yac_artifacts <- load_artifacts_from_s3("xgb_part_yac_temp", bucket, prefix)

xgb_part_ypc <- load_model_from_s3("xgb_part_ypc.model", bucket, prefix)
xgb_part_ypc_artifacts <- load_artifacts_from_s3("xgb_part_ypc", bucket, prefix)

xgb_part_scramble_ypc <- load_model_from_s3("xgb_part_scramble_ypc_temp.model", bucket, prefix)
xgb_part_scramble_ypc_artifacts <- load_artifacts_from_s3("xgb_part_scramble_ypc_temp", bucket, prefix)

xgb_part_sack <- load_model_from_s3("xgb_part_sack_no_temp.model", bucket, prefix)
xgb_part_sack_artifacts <- load_artifacts_from_s3("xgb_part_sack_no_temp", bucket, prefix)

xgb_part_ypa <- load_model_from_s3("xgb_part_ypa_no_temp.model", bucket, prefix)
xgb_part_ypa_artifacts <- load_artifacts_from_s3("xgb_part_ypa_no_temp", bucket, prefix)

xgb_part_pressure_before <- load_model_from_s3("xgb_part_pressure_before_no_temp.model", bucket, prefix)
xgb_part_pressure_before_artifacts <- load_artifacts_from_s3("xgb_part_pressure_before_no_temp", bucket, prefix)

xgb_part_pressure_after_pass <- load_model_from_s3("xgb_part_pressure_after_ttt_pass_att_no_temp.model", bucket, prefix)

xgb_part_pressure_scramble <- load_model_from_s3("xgb_part_pressure_scramble_no_temp.model", bucket, prefix)
xgb_part_pressure_scramble_artifacts <- load_artifacts_from_s3("xgb_part_pressure_scramble_no_temp", bucket, prefix)

cat("✓ All participation models loaded\n")


# ============================================================================
# BEFORE XTD
# ============================================================================

print(xgb_part_xtd_artifacts$feature_names)

part_nfl$predicted_before_xtd <- NA

eligible_rows <- which(
  part_nfl$two_point_attempt == 0 & 
    part_nfl$qb_kneel == 0 & 
    part_nfl$qb_spike == 0 & 
    part_nfl$play_type %in% c("pass", "run") &
    !is.na(part_nfl$defenders_in_box)
)

if(length(eligible_rows) > 0) {
  dtest <- create_dmatrix(part_nfl[eligible_rows, ], xgb_part_xtd_artifacts, use_weather = TRUE)
  part_nfl$predicted_before_xtd[eligible_rows] <- predict(xgb_part_xtd, newdata = dtest)
}

part_nfl %>%
  filter(!is.na(predicted_before_xtd)) %>%
  mutate(bin = cut(predicted_before_xtd, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_td = mean(td_side, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# AFTER RUN XTD
# ============================================================================

print(xgb_part_rush_xtd_artifacts$feature_names)

part_nfl$predicted_after_run_xtd <- NA

eligible_rows <- which(
  part_nfl$two_point_attempt == 0 & 
    part_nfl$qb_kneel == 0 & 
    part_nfl$fumble == 0 &
    !is.na(part_nfl$defenders_in_box) & 
    part_nfl$play_type == "run" &
    part_nfl$rush_attempt == 1 &
    part_nfl$qb_scramble == 0
)

if(length(eligible_rows) > 0) {
  dtest <- create_dmatrix(part_nfl[eligible_rows, ], xgb_part_rush_xtd_artifacts, use_weather = TRUE)
  part_nfl$predicted_after_run_xtd[eligible_rows] <- predict(xgb_part_rush_xtd, newdata = dtest)
}

part_nfl %>%
  filter(!is.na(predicted_after_run_xtd)) %>%
  mutate(bin = cut(predicted_after_run_xtd, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_td = mean(td_side, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# AFTER PASS XTD
# ============================================================================

print(xgb_part_after_pass_xtd_artifacts$feature_names)

part_nfl$predicted_after_pass_xtd <- NA

eligible_rows <- which(
  part_nfl$two_point_attempt == 0 & 
    part_nfl$qb_kneel == 0 & 
    part_nfl$qb_spike == 0 & 
    part_nfl$play_type == "pass" &
    !is.na(part_nfl$defenders_in_box) & 
    part_nfl$pass_attempt == 1 & 
    part_nfl$qb_scramble == 0 & 
    part_nfl$sack == 0 & 
    !is.na(part_nfl$air_yards)
)

if(length(eligible_rows) > 0) {
  dtest <- create_dmatrix(part_nfl[eligible_rows, ], xgb_part_after_pass_xtd_artifacts, use_weather = TRUE)
  part_nfl$predicted_after_pass_xtd[eligible_rows] <- predict(xgb_part_after_pass_xtd, newdata = dtest)
}

part_nfl %>%
  filter(!is.na(predicted_after_pass_xtd)) %>%
  mutate(bin = cut(predicted_after_pass_xtd, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_td = mean(td_side, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# XPASS
# ============================================================================

print(xgb_part_xpass_artifacts$feature_names)

part_nfl$predicted_xpass <- NA

eligible_rows <- which(
  part_nfl$two_point_attempt == 0 & 
    part_nfl$qb_kneel == 0 & 
    part_nfl$qb_spike == 0 & 
    part_nfl$play_type %in% c("pass", "run") &
    !is.na(part_nfl$defenders_in_box)
)

if(length(eligible_rows) > 0) {
  dtest <- create_dmatrix(part_nfl[eligible_rows, ], xgb_part_xpass_artifacts, use_weather = TRUE)
  part_nfl$predicted_xpass[eligible_rows] <- predict(xgb_part_xpass, newdata = dtest)
}

part_nfl %>%
  filter(!is.na(predicted_xpass)) %>%
  mutate(bin = cut(predicted_xpass, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_xpass = mean(pass_attempt, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# CP
# ============================================================================

print(xgb_part_cp_artifacts$feature_names)

part_nfl$predicted_cp <- NA

eligible_rows <- which(
  part_nfl$two_point_attempt == 0 & 
    part_nfl$qb_kneel == 0 & 
    part_nfl$qb_spike == 0 & 
    part_nfl$play_type == "pass" &
    !is.na(part_nfl$defenders_in_box) &
    part_nfl$sack == 0 & 
    part_nfl$pass_attempt == 1 &
    part_nfl$rush_attempt == 0 & 
    !is.na(part_nfl$air_yards)
)

if(length(eligible_rows) > 0) {
  dtest <- create_dmatrix(part_nfl[eligible_rows, ], xgb_part_cp_artifacts, use_weather = TRUE)
  part_nfl$predicted_cp[eligible_rows] <- predict(xgb_part_cp, newdata = dtest)
}

part_nfl %>%
  filter(!is.na(predicted_cp)) %>%
  mutate(bin = cut(predicted_cp, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_cp = mean(complete_pass, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# PRESSURE BEFORE (OLD - no weather)
# ============================================================================

print(xgb_part_pressure_before_artifacts$feature_names)

part_nfl$predicted_pressure_before <- NA

eligible_rows <- which(
  part_nfl$two_point_attempt == 0 & 
    part_nfl$qb_kneel == 0 & 
    part_nfl$qb_spike == 0 & 
    part_nfl$play_type == "pass" &
    !is.na(part_nfl$defenders_in_box)
)

if(length(eligible_rows) > 0) {
  dtest <- create_dmatrix(part_nfl[eligible_rows, ], xgb_part_pressure_before_artifacts, use_weather = FALSE)
  part_nfl$predicted_pressure_before[eligible_rows] <- predict(xgb_part_pressure_before, newdata = dtest)
}

part_nfl %>%
  filter(!is.na(predicted_pressure_before)) %>%
  mutate(bin = cut(predicted_pressure_before, breaks = seq(0, 1, by = 0.025), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_pressure = mean(pressure_ind, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# PRESSURE AFTER PASS (OLD - no weather)
# ============================================================================

# PRESSURE AFTER PASS - NO ARTIFACTS FILE EXISTS, HARDCODE FEATURES
xgb_part_pressure_after_pass <- load_model_from_s3("xgb_part_pressure_after_ttt_pass_att_no_temp.model", bucket, prefix)

create_dmatrix_pressure_after_pass <- function(data) {
  X <- data %>% select(
    qb_hit, yardline_100, season_type, half_seconds_remaining, down, down_one_ind, 
    down_two_ind, down_three_ind, defenders_in_box, number_of_pass_rushers, 
    mod_ydstogo, shotgun, no_huddle, score_differential, surface, posteam_ind, 
    n_ol, n_te, n_rb, n_wr, n_st, n_dl, n_lb, n_db, n_st_def, cover3_ind, 
    cover2_ind, cover0_ind, cover1_ind, cover4_ind, twoman_ind, cover6_ind, 
    time_to_throw, hitch_route_ind, go_route_ind, out_route_ind, slant_route_ind, 
    cross_route_ind, post_route_ind, corner_route_ind, in_route_ind, wheel_route_ind
  ) %>% as.matrix()
  return(xgb.DMatrix(data = X))
}

part_nfl$predicted_pressure_after_pass <- NA

eligible_rows <- which(
  part_nfl$two_point_attempt == 0 & 
    part_nfl$qb_kneel == 0 & 
    part_nfl$qb_spike == 0 & 
    part_nfl$play_type == "pass" &
    !is.na(part_nfl$defenders_in_box) &
    part_nfl$pass_attempt == 1 & 
    part_nfl$qb_scramble == 0 & 
    part_nfl$rush_attempt == 0 & 
    !is.na(part_nfl$time_to_throw) & 
    !is.na(part_nfl$air_yards)
)

if(length(eligible_rows) > 0) {
  dtest <- create_dmatrix_pressure_after_pass(part_nfl[eligible_rows, ])
  part_nfl$predicted_pressure_after_pass[eligible_rows] <- predict(xgb_part_pressure_after_pass, newdata = dtest)
}

part_nfl$predicted_pressure_after_pass[which(part_nfl$sack == 1)] <- 1


# ============================================================================
# PRESSURE SCRAMBLE (OLD - no weather)
# ============================================================================

print(xgb_part_pressure_scramble_artifacts$feature_names)

part_nfl$predicted_pressure_scramble <- NA

eligible_rows <- which(
  part_nfl$rush_attempt == 1 & 
    part_nfl$qb_spike == 0 & 
    part_nfl$qb_kneel == 0 & 
    part_nfl$two_point_attempt == 0 &
    !is.na(part_nfl$defenders_in_box) & 
    part_nfl$qb_scramble == 1 & 
    is.na(part_nfl$time_to_throw) & 
    is.na(part_nfl$air_yards)
)

if(length(eligible_rows) > 0) {
  dtest <- create_dmatrix(part_nfl[eligible_rows, ], xgb_part_pressure_scramble_artifacts, use_weather = FALSE)
  part_nfl$predicted_pressure_scramble[eligible_rows] <- predict(xgb_part_pressure_scramble, newdata = dtest)
}

part_nfl %>%
  filter(!is.na(predicted_pressure_scramble)) %>%
  mutate(bin = cut(predicted_pressure_scramble, breaks = seq(0, 1, by = 0.02), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_pressure = mean(pressure_ind, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# SACK (has rain_ind, snow_ind - needs weather)
# ============================================================================

print(xgb_part_sack_artifacts$feature_names)

part_nfl$predicted_sack <- NA

eligible_rows <- which(
  part_nfl$two_point_attempt == 0 & 
    part_nfl$qb_kneel == 0 & 
    part_nfl$qb_spike == 0 & 
    part_nfl$play_type == "pass" &
    !is.na(part_nfl$defenders_in_box)
)

if(length(eligible_rows) > 0) {
  dtest <- create_dmatrix(part_nfl[eligible_rows, ], xgb_part_sack_artifacts, use_weather = TRUE)
  part_nfl$predicted_sack[eligible_rows] <- predict(xgb_part_sack, newdata = dtest)
}

part_nfl %>%
  filter(!is.na(predicted_sack)) %>%
  mutate(bin = cut(predicted_sack, breaks = seq(0, 1, by = 0.02), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_sack = mean(sack, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# YPC
# ============================================================================

print(xgb_part_ypc_artifacts$feature_names)

part_nfl$predicted_ypc <- NA

eligible_rows <- which(
  part_nfl$two_point_attempt == 0 & 
    part_nfl$pass_attempt == 0 & 
    part_nfl$rush_attempt == 1 &
    part_nfl$play_type == "run" & 
    part_nfl$qb_kneel == 0 &
    part_nfl$qb_spike == 0 & 
    part_nfl$qb_scramble == 0
)

if(length(eligible_rows) > 0) {
  dtest <- create_dmatrix(part_nfl[eligible_rows, ], xgb_part_ypc_artifacts, use_weather = TRUE)
  part_nfl$predicted_ypc[eligible_rows] <- predict(xgb_part_ypc, newdata = dtest)
}

part_nfl %>%
  filter(!is.na(predicted_ypc)) %>%
  mutate(bin = cut(predicted_ypc, breaks = seq(-1, 9, by = 0.5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(actual_ypc = mean(yards_gained, na.rm = TRUE), predicted_avg = mean(predicted_ypc, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# SCRAMBLE YPC
# ============================================================================

print(xgb_part_scramble_ypc_artifacts$feature_names)

part_nfl$predicted_scramble_ypc <- NA

eligible_rows <- which(
  part_nfl$two_point_attempt == 0 & 
    part_nfl$rush_attempt == 1 &
    part_nfl$qb_kneel == 0 &
    part_nfl$qb_spike == 0 & 
    part_nfl$qb_scramble == 1 & 
    part_nfl$sack == 0 & 
    part_nfl$pass_attempt == 0
)

if(length(eligible_rows) > 0) {
  dtest <- create_dmatrix(part_nfl[eligible_rows, ], xgb_part_scramble_ypc_artifacts, use_weather = TRUE)
  part_nfl$predicted_scramble_ypc[eligible_rows] <- predict(xgb_part_scramble_ypc, newdata = dtest)
}

part_nfl %>%
  filter(!is.na(predicted_scramble_ypc)) %>%
  mutate(bin = cut(predicted_scramble_ypc, breaks = seq(-1, 12.5, by = 0.5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(actual_ypc = mean(yards_gained, na.rm = TRUE), predicted_avg = mean(predicted_scramble_ypc, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# YPA (OLD - no weather)
# ============================================================================

print(xgb_part_ypa_artifacts$feature_names)

part_nfl$predicted_ypa <- NA

eligible_rows <- which(
  part_nfl$play_type == "pass" & 
    part_nfl$rush_attempt == 0 &
    part_nfl$two_point_attempt == 0 &
    part_nfl$qb_kneel == 0 &
    part_nfl$qb_spike == 0 & 
    part_nfl$qb_scramble == 0 & 
    part_nfl$sack == 0 & 
    part_nfl$pass_attempt == 1 & 
    !is.na(part_nfl$air_yards)
)

if(length(eligible_rows) > 0) {
  dtest <- create_dmatrix(part_nfl[eligible_rows, ], xgb_part_ypa_artifacts, use_weather = FALSE)
  part_nfl$predicted_ypa[eligible_rows] <- predict(xgb_part_ypa, newdata = dtest)
}

part_nfl %>%
  filter(!is.na(predicted_ypa)) %>%
  mutate(bin = cut(predicted_ypa, breaks = seq(-2, 22.5, by = 0.5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(actual_ypa = mean(yards_gained, na.rm = TRUE), predicted_avg = mean(predicted_ypa, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# YAC
# ============================================================================

print(xgb_part_yac_artifacts$feature_names)

part_nfl$predicted_yds_after_catch <- NA

eligible_rows <- which(
  part_nfl$play_type == "pass" & 
    part_nfl$rush_attempt == 0 &
    part_nfl$two_point_attempt == 0 &
    part_nfl$qb_kneel == 0 &
    part_nfl$qb_spike == 0 & 
    part_nfl$qb_scramble == 0 & 
    part_nfl$sack == 0 & 
    part_nfl$pass_attempt == 1 & 
    !is.na(part_nfl$air_yards) & 
    part_nfl$complete_pass == 1
)

if(length(eligible_rows) > 0) {
  dtest <- create_dmatrix(part_nfl[eligible_rows, ], xgb_part_yac_artifacts, use_weather = TRUE)
  part_nfl$predicted_yds_after_catch[eligible_rows] <- predict(xgb_part_yac, newdata = dtest)
}

part_nfl %>%
  filter(!is.na(predicted_yds_after_catch)) %>%
  mutate(bin = cut(predicted_yds_after_catch, breaks = seq(-2, 22.5, by = 0.5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(actual_yac = mean(yards_after_catch, na.rm = TRUE), predicted_avg = mean(predicted_yds_after_catch, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# XTD CALCULATIONS
# ============================================================================

part_nfl$drive_id <- paste0(part_nfl$nflverse_game_id, "-", part_nfl$posteam, "-", part_nfl$fixed_drive)

part_nfl$predicted_before_xtd[which(is.na(part_nfl$predicted_before_xtd))] <- 0
part_nfl$predicted_after_xtd <- coalesce(part_nfl$predicted_after_run_xtd, part_nfl$predicted_after_pass_xtd)
part_nfl$predicted_after_xtd[which(is.na(part_nfl$predicted_after_xtd))] <- 0

part_nfl <- part_nfl %>%
  group_by(drive_id) %>%
  mutate(
    before_remaining_xtd = lag(cumprod(1 - predicted_before_xtd), default = 1),
    after_remaining_xtd = lag(cumprod(1 - predicted_after_xtd), default = 1),
    before_xtd_new = predicted_before_xtd * before_remaining_xtd,
    after_xtd_new = predicted_after_xtd * after_remaining_xtd
  ) %>%
  ungroup()

part_xtd <- part_nfl %>% 
  group_by(posteam, defteam, week, season) %>% 
  dplyr::summarise(
    before_old_xtd = sum(predicted_before_xtd),
    before_new_xtd = sum(before_xtd_new),
    after_old_xtd = sum(predicted_after_xtd),
    after_new_xtd = sum(after_xtd_new),
    actual_td = sum(touchdown, na.rm = T),
    actual_fg = sum(field_goal_attempt, na.rm = T),
    .groups = "drop"
  ) %>%
  filter(!is.na(posteam) & !is.na(defteam))

write.csv(part_xtd, "part_xtd.csv", row.names = FALSE)
put_object(file = "part_xtd.csv", object = "part_xtd/part_xtd.csv", bucket = "nfl-pff-data-lucas")