# S3 Configuration
bucket <- "nfl-pff-data-lucas"
prefix <- "models/"

# Function to load XGBoost model from S3
load_model_from_s3 <- function(model_name, bucket, prefix) {
  full_path <- paste0(prefix, model_name)
  temp_file <- tempfile(fileext = ".model")
  save_object(full_path, bucket = bucket, file = temp_file)
  model <- xgb.load(temp_file)
  unlink(temp_file)
  return(model)
}

# ============================================================================
# LOAD MODELS
# ============================================================================

xgb_pbp_before_xtd <- load_model_from_s3("xgb_pbp_before_td_no_temp.model", bucket, prefix)
xgb_pbp_after_run_xtd <- load_model_from_s3("xgb_pbp_run_after_td_no_temp.model", bucket, prefix)
xgb_pbp_after_pass_xtd <- load_model_from_s3("xgb_pbp_pass_after_td_no_temp.model", bucket, prefix)
xgb_pbp_pressure <- load_model_from_s3("xgb_pbp_pressure_no_temp.model", bucket, prefix)
xgb_pbp_xpass <- load_model_from_s3("xgb_pbp_xpass_no_temp.model", bucket, prefix)
xgb_pbp_sack <- load_model_from_s3("xgb_pbp_sack.model", bucket, prefix)
xgb_pbp_cp <- load_model_from_s3("xgb_pbp_cp_temp.model", bucket, prefix)  # FIXED NAME
xgb_pbp_ypc <- load_model_from_s3("xgb_pbp_ypc.model", bucket, prefix)
xgb_pbp_ypa <- load_model_from_s3("xgb_pbp_ypa.model", bucket, prefix)
xgb_pbp_yac <- load_model_from_s3("xgb_pbp_yac.model", bucket, prefix)
xgb_pbp_scramble_ypc <- load_model_from_s3("xgb_pbp_scramble_ypc.model", bucket, prefix)


# ============================================================================
# HELPER: Impute weather NAs
# ============================================================================

impute_weather <- function(data) {
  data %>%
    mutate(
      wind = ifelse(is.na(wind), 0, wind),
      temp = ifelse(is.na(temp), 70, temp),
      rain_ind = ifelse(is.na(rain_ind), 0, rain_ind),
      snow_ind = ifelse(is.na(snow_ind), 0, snow_ind)
    )
}


# ============================================================================
# BEFORE XTD
# ============================================================================

xgb.importance(model = xgb_pbp_before_xtd)

create_dmatrix_before_xtd <- function(data) {
  X <- data %>% select(
    yardline_100, half_seconds_remaining, mod_ydstogo, posteam_ind, down, 
    shotgun, no_huddle, score_differential, posteam_timeouts_remaining, 
    defteam_timeouts_remaining
  ) %>% as.matrix()
  return(xgb.DMatrix(data = X))
}

pbp_base$predicted_before_xtd <- NA
eligible_rows <- pbp_base$two_point_attempt == 0 & 
  pbp_base$qb_kneel == 0 & 
  pbp_base$qb_spike == 0 & 
  pbp_base$play_type %in% c("pass", "run")

if(sum(eligible_rows, na.rm = TRUE) > 0) {
  dtest <- create_dmatrix_before_xtd(pbp_base[eligible_rows, ])
  pbp_base$predicted_before_xtd[eligible_rows] <- predict(xgb_pbp_before_xtd, newdata = dtest)
}

pbp_base %>%
  filter(!is.na(predicted_before_xtd)) %>%
  mutate(bin = cut(predicted_before_xtd, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_td = mean(td_side, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# AFTER RUN XTD
# ============================================================================

xgb.importance(model = xgb_pbp_after_run_xtd)

create_dmatrix_after_run_xtd <- function(data) {
  X <- data %>% select(
    season_type, yardline_100, half_seconds_remaining, down, mod_ydstogo, 
    shotgun, no_huddle, last_timeout_ind, score_differential, roof, surface, 
    posteam_ind, run_location, guard_ind, tackle_ind, end_ind, 
    posteam_timeouts_remaining, defteam_timeouts_remaining
  ) %>% as.matrix()
  return(xgb.DMatrix(data = X))
}

pbp_base$predicted_after_run_xtd <- NA
eligible_rows <- pbp_base$two_point_attempt == 0 & 
  pbp_base$qb_kneel == 0 & 
  pbp_base$qb_spike == 0 & 
  pbp_base$play_type == "run"

if(sum(eligible_rows, na.rm = TRUE) > 0) {
  dtest <- create_dmatrix_after_run_xtd(pbp_base[eligible_rows, ])
  pbp_base$predicted_after_run_xtd[eligible_rows] <- predict(xgb_pbp_after_run_xtd, newdata = dtest)
}

pbp_base %>%
  filter(!is.na(predicted_after_run_xtd)) %>%
  mutate(bin = cut(predicted_after_run_xtd, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_td = mean(td_side, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# AFTER PASS XTD
# ============================================================================

xgb.importance(model = xgb_pbp_after_pass_xtd)

create_dmatrix_after_pass_xtd <- function(data) {
  X <- data %>% select(
    yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, 
    qb_dropback, air_yards, pass_length, middle_ind, outside_ind, 
    score_differential, qb_hit, roof, surface, 
    posteam_timeouts_remaining, defteam_timeouts_remaining
  ) %>% as.matrix()
  return(xgb.DMatrix(data = X))
}

pbp_base$predicted_after_pass_xtd <- NA
eligible_rows <- pbp_base$two_point_attempt == 0 & 
  pbp_base$qb_kneel == 0 & 
  pbp_base$qb_spike == 0 & 
  pbp_base$sack == 0 &
  pbp_base$pass_attempt == 1 &
  pbp_base$qb_scramble == 0 &
  pbp_base$play_type == "pass" & 
  !is.na(pbp_base$air_yards)

if(sum(eligible_rows, na.rm = TRUE) > 0) {
  dtest <- create_dmatrix_after_pass_xtd(pbp_base[eligible_rows, ])
  pbp_base$predicted_after_pass_xtd[eligible_rows] <- predict(xgb_pbp_after_pass_xtd, newdata = dtest)
}

pbp_base %>%
  filter(!is.na(predicted_after_pass_xtd)) %>%
  mutate(bin = cut(predicted_after_pass_xtd, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_td = mean(td_side, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# XPASS
# ============================================================================

xgb.importance(model = xgb_pbp_xpass)

create_dmatrix_xpass <- function(data) {
  X <- data %>% select(
    yardline_100, half_seconds_remaining, posteam_timeouts_remaining, 
    defteam_timeouts_remaining, mod_ydstogo, posteam_ind, down_one_ind, 
    down_two_ind, down_three_ind, down, shotgun, no_huddle, score_differential
  ) %>% as.matrix()
  return(xgb.DMatrix(data = X))
}

pbp_base$predicted_xpass <- NA
eligible_rows <- pbp_base$two_point_attempt == 0 & 
  (pbp_base$pass_attempt == 1 | pbp_base$rush_attempt == 1) &
  pbp_base$play_type %in% c("run", "pass") & 
  pbp_base$qb_kneel == 0 &
  pbp_base$qb_spike == 0

if(sum(eligible_rows, na.rm = TRUE) > 0) {
  dtest <- create_dmatrix_xpass(pbp_base[eligible_rows, ])
  pbp_base$predicted_xpass[eligible_rows] <- predict(xgb_pbp_xpass, newdata = dtest)
}

pbp_base %>%
  filter(!is.na(predicted_xpass)) %>%
  mutate(bin = cut(predicted_xpass, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_xpass = mean(pass_attempt, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# PRESSURE
# ============================================================================

xgb.importance(model = xgb_pbp_pressure)

create_dmatrix_pressure <- function(data) {
  X <- data %>% select(
    yardline_100, season_type, half_seconds_remaining, down, down_one_ind, 
    down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, 
    score_differential, surface, posteam_ind
  ) %>% as.matrix()
  return(xgb.DMatrix(data = X))
}

pbp_base$predicted_pbp_pressure <- NA
eligible_rows <- pbp_base$two_point_attempt == 0 & 
  pbp_base$pass_attempt == 1 &
  pbp_base$play_type == "pass" & 
  pbp_base$qb_kneel == 0 &
  pbp_base$qb_spike == 0 & 
  pbp_base$sack == 0 & 
  pbp_base$rush_attempt == 0

if(sum(eligible_rows, na.rm = TRUE) > 0) {
  dtest <- create_dmatrix_pressure(pbp_base[eligible_rows, ])
  pbp_base$predicted_pbp_pressure[eligible_rows] <- predict(xgb_pbp_pressure, newdata = dtest)
}


# ============================================================================
# SACK
# ============================================================================

xgb.importance(model = xgb_pbp_sack)

create_dmatrix_sack <- function(data) {
  X <- data %>% 
    impute_weather() %>%
    select(
      yardline_100, season_type, half_seconds_remaining, down, down_one_ind, 
      down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, 
      score_differential, surface, posteam_ind, temp, wind, rain_ind, snow_ind
    ) %>% as.matrix()
  return(xgb.DMatrix(data = X))
}

pbp_base$predicted_sack <- NA
eligible_rows <- pbp_base$play_type == "pass" & 
  pbp_base$qb_spike == 0 & 
  pbp_base$qb_kneel == 0 & 
  pbp_base$two_point_attempt == 0

if(sum(eligible_rows, na.rm = TRUE) > 0) {
  dtest <- create_dmatrix_sack(pbp_base[eligible_rows, ])
  pbp_base$predicted_sack[eligible_rows] <- predict(xgb_pbp_sack, newdata = dtest)
}

pbp_base %>%
  filter(!is.na(predicted_sack)) %>%
  mutate(bin = cut(predicted_sack, breaks = seq(0, 1, by = 0.02), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(sack_rate = mean(sack, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# CP - FIXED: removed qb_dropback, fixed model name
# ============================================================================

xgb.importance(model = xgb_pbp_cp)

create_dmatrix_cp <- function(data) {
  X <- data %>%
    select(
      yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo,
      air_yards, pass_length, middle_ind, outside_ind,
      score_differential, qb_hit, roof, surface, posteam_timeouts_remaining,
      defteam_timeouts_remaining, down_one_ind, down_two_ind, down_three_ind, down_four_ind,
      wind, temp
    ) %>% as.matrix()
  return(xgb.DMatrix(data = X))
}

pbp_base$predicted_cp <- NA
eligible_rows <- pbp_base$two_point_attempt == 0 & 
  pbp_base$pass_attempt == 1 &
  pbp_base$play_type == "pass" & 
  pbp_base$qb_kneel == 0 &
  pbp_base$qb_spike == 0 & 
  pbp_base$qb_scramble == 0 & 
  pbp_base$sack == 0 & 
  !is.na(pbp_base$air_yards)

if(sum(eligible_rows, na.rm = TRUE) > 0) {
  dtest <- create_dmatrix_cp(pbp_base[eligible_rows, ])
  pbp_base$predicted_cp[eligible_rows] <- predict(xgb_pbp_cp, newdata = dtest)
}

pbp_base %>%
  filter(!is.na(predicted_cp)) %>%
  mutate(bin = cut(predicted_cp, breaks = seq(0, 1, by = 0.04), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(mean_cp = mean(complete_pass, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# YPC - FIXED features to match training
# ============================================================================

xgb.importance(model = xgb_pbp_ypc)

create_dmatrix_ypc <- function(data) {
  X <- data %>% 
    impute_weather() %>%
    select(
      yards_gained, yardline_100, season_type, half_seconds_remaining, down,
      down_one_ind, down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle,
      score_differential, surface, posteam_ind, end_ind, guard_ind, tackle_ind,
      temp, wind, rain_ind, snow_ind
    ) %>% select(-yards_gained) %>% as.matrix()
  return(xgb.DMatrix(data = X))
}

pbp_base$predicted_ypc <- NA
eligible_rows <- pbp_base$two_point_attempt == 0 & 
  pbp_base$pass_attempt == 0 & 
  pbp_base$rush_attempt == 1 &
  pbp_base$play_type %in% c("run", "pass") & 
  pbp_base$qb_kneel == 0 &
  pbp_base$qb_spike == 0 & 
  pbp_base$qb_scramble == 0

if(sum(eligible_rows, na.rm = TRUE) > 0) {
  dtest <- create_dmatrix_ypc(pbp_base[eligible_rows, ])
  pbp_base$predicted_ypc[eligible_rows] <- predict(xgb_pbp_ypc, newdata = dtest)
}

pbp_base %>%
  filter(!is.na(predicted_ypc)) %>%
  mutate(bin = cut(predicted_ypc, breaks = seq(-1, 9, by = 0.5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(actual_ypc = mean(yards_gained, na.rm = TRUE), predicted_avg = mean(predicted_ypc, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# SCRAMBLE YPC - FIXED features to match training
# ============================================================================

xgb.importance(model = xgb_pbp_scramble_ypc)

create_dmatrix_scramble_ypc <- function(data) {
  X <- data %>% 
    impute_weather() %>%
    select(
      yardline_100, season_type, half_seconds_remaining, down, down_one_ind, 
      down_two_ind, down_three_ind, mod_ydstogo, shotgun, no_huddle, 
      score_differential, surface, posteam_ind, end_ind, guard_ind, tackle_ind, 
      outside_ind, temp, wind, rain_ind, snow_ind
    ) %>% as.matrix()
  return(xgb.DMatrix(data = X))
}

pbp_base$predicted_scramble_ypc <- NA
eligible_rows <- pbp_base$two_point_attempt == 0 & 
  pbp_base$rush_attempt == 1 &
  pbp_base$qb_kneel == 0 &
  pbp_base$qb_spike == 0 & 
  pbp_base$qb_scramble == 1 & 
  pbp_base$sack == 0 & 
  pbp_base$pass_attempt == 0

if(sum(eligible_rows, na.rm = TRUE) > 0) {
  dtest <- create_dmatrix_scramble_ypc(pbp_base[eligible_rows, ])
  pbp_base$predicted_scramble_ypc[eligible_rows] <- predict(xgb_pbp_scramble_ypc, newdata = dtest)
}

pbp_base %>%
  filter(!is.na(predicted_scramble_ypc)) %>%
  mutate(bin = cut(predicted_scramble_ypc, breaks = seq(-1, 12.5, by = 0.5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(actual_ypc = mean(yards_gained, na.rm = TRUE), predicted_avg = mean(predicted_scramble_ypc, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# YPA
# ============================================================================

xgb.importance(model = xgb_pbp_ypa)

create_dmatrix_ypa <- function(data) {
  X <- data %>% 
    impute_weather() %>%
    select(
      yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, 
      air_yards, pass_length, middle_ind, outside_ind, score_differential, 
      qb_hit, roof, surface, posteam_timeouts_remaining, defteam_timeouts_remaining,
      temp, wind, rain_ind, snow_ind
    ) %>% as.matrix()
  return(xgb.DMatrix(data = X))
}

pbp_base$predicted_ypa <- NA
eligible_rows <- pbp_base$play_type == "pass" & 
  pbp_base$rush_attempt == 0 &
  pbp_base$two_point_attempt == 0 &
  pbp_base$qb_kneel == 0 &
  pbp_base$qb_spike == 0 & 
  pbp_base$qb_scramble == 0 & 
  pbp_base$sack == 0 & 
  pbp_base$pass_attempt == 1 & 
  !is.na(pbp_base$air_yards)

if(sum(eligible_rows, na.rm = TRUE) > 0) {
  dtest <- create_dmatrix_ypa(pbp_base[eligible_rows, ])
  pbp_base$predicted_ypa[eligible_rows] <- predict(xgb_pbp_ypa, newdata = dtest)
}

pbp_base %>%
  filter(!is.na(predicted_ypa)) %>%
  mutate(bin = cut(predicted_ypa, breaks = seq(-2, 22.5, by = 0.5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(actual_ypa = mean(yards_gained, na.rm = TRUE), predicted_avg = mean(predicted_ypa, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# YAC
# ============================================================================

xgb.importance(model = xgb_pbp_yac)

create_dmatrix_yac <- function(data) {
  X <- data %>% 
    impute_weather() %>%
    select(
      yardline_100, half_seconds_remaining, shotgun, no_huddle, mod_ydstogo, 
      qb_dropback, air_yards, pass_length, middle_ind, outside_ind, 
      score_differential, qb_hit, roof, surface, posteam_timeouts_remaining, 
      defteam_timeouts_remaining, down_one_ind, down_two_ind, down_three_ind,
      temp, wind, rain_ind, snow_ind
    ) %>% as.matrix()
  return(xgb.DMatrix(data = X))
}

pbp_base$predicted_yds_after_catch <- NA
eligible_rows <- pbp_base$play_type == "pass" & 
  pbp_base$qb_spike == 0 & 
  pbp_base$qb_kneel == 0 & 
  pbp_base$two_point_attempt == 0 &
  pbp_base$complete_pass == 1 & 
  pbp_base$sack == 0 & 
  pbp_base$pass_attempt == 1 & 
  pbp_base$qb_scramble == 0 & 
  !is.na(pbp_base$air_yards) & 
  !is.na(pbp_base$yards_after_catch)

if(sum(eligible_rows, na.rm = TRUE) > 0) {
  dtest <- create_dmatrix_yac(pbp_base[eligible_rows, ])
  pbp_base$predicted_yds_after_catch[eligible_rows] <- predict(xgb_pbp_yac, newdata = dtest)
}

pbp_base %>%
  filter(!is.na(predicted_yds_after_catch)) %>%
  mutate(bin = cut(predicted_yds_after_catch, breaks = seq(-2, 22.5, by = 0.5), include.lowest = TRUE, right = FALSE)) %>%
  group_by(bin) %>%
  summarize(actual_yac = mean(yards_after_catch, na.rm = TRUE), predicted_avg = mean(predicted_yds_after_catch, na.rm = TRUE), n = n(), .groups = 'drop')


# ============================================================================
# XTD CALCULATIONS
# ============================================================================

pbp_base$drive_id <- paste0(pbp_base$game_id, "-", pbp_base$posteam, "-", pbp_base$fixed_drive)

pbp_base$predicted_before_xtd[which(is.na(pbp_base$predicted_before_xtd))] <- 0
pbp_base$predicted_after_xtd <- coalesce(pbp_base$predicted_after_run_xtd, pbp_base$predicted_after_pass_xtd)
pbp_base$predicted_after_xtd[which(is.na(pbp_base$predicted_after_xtd))] <- 0

pbp_base <- pbp_base %>%
  group_by(drive_id) %>%
  mutate(
    before_remaining_xtd = lag(cumprod(1 - predicted_before_xtd), default = 1),
    after_remaining_xtd = lag(cumprod(1 - predicted_after_xtd), default = 1),
    before_xtd_new = predicted_before_xtd * before_remaining_xtd,
    after_xtd_new = predicted_after_xtd * after_remaining_xtd
  ) %>%
  ungroup()

pbp_xtd <- pbp_base %>% 
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

write.csv(pbp_xtd, "pbp_xtd.csv", row.names = FALSE)
put_object(file = "pbp_xtd.csv", object = "pbp_xtd/pbp_xtd.csv", bucket = "nfl-pff-data-lucas")