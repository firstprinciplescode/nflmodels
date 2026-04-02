# ============================================================================
# BACKFILL MISSING WEATHER DATA FROM OPEN-METEO
# ============================================================================
# 
# Two tiers:
#   1) Retractable roof open (ARI, ATL, DAL, HOU, IND) -> impute 72F / 0 wind / no precip
#   2) Everything else (outdoor missing + neutral sites) -> pull from Open-Meteo API
#
# Open-Meteo returns hourly data. We interpolate across the ~3hr game window.
# start_time is in ET. We convert to local stadium timezone for correct hour mapping.
# ============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# ============================================================================
# STADIUM LAT/LON + TIMEZONE LOOKUP
# ============================================================================

stadium_lookup <- tribble(
  ~stadium, ~lat, ~lon, ~tz,
  # US Outdoor Stadiums
  "Acrisure Stadium",         40.4468, -80.0158, "America/New_York",
  "Arrowhead Stadium",        39.0489, -94.4839, "America/Chicago",
  "Bank of America Stadium",  35.2258, -80.8528, "America/New_York",
  "Caesars Superdome",        29.9511, -90.0812, "America/Chicago",
  "Cleveland Browns Stadium", 41.5061, -81.6995, "America/New_York",
  "Empower Field at Mile High", 39.7439, -105.0201, "America/Denver",
  "EverBank Stadium",         30.3240, -81.6373, "America/New_York",
  "FedExField",               38.9076, -76.8645, "America/New_York",
  "Ford Field",               42.3400, -83.0456, "America/Detroit",
  "Gillette Stadium",         42.0909, -71.2643, "America/New_York",
  "Hard Rock Stadium",        25.9580, -80.2389, "America/New_York",
  "Heinz Field",              40.4468, -80.0158, "America/New_York",
  "Highmark Stadium",         42.7738, -78.7870, "America/New_York",
  "Levi's Stadium",           37.4033, -121.9694, "America/Los_Angeles",
  "Lincoln Financial Field",  39.9008, -75.1674, "America/New_York",
  "Los Angeles Memorial Coliseum", 34.0141, -118.2879, "America/Los_Angeles",
  "Lumen Field",              47.5952, -122.3316, "America/Los_Angeles",
  "M&T Bank Stadium",         39.2780, -76.6227, "America/New_York",
  "MetLife Stadium",          40.8128, -74.0742, "America/New_York",
  "Nissan Stadium",           36.1665, -86.7713, "America/Chicago",
  "Northwest Stadium",        38.9076, -76.8645, "America/New_York",
  "Paycor Stadium",           39.0954, -84.5160, "America/New_York",
  "Raymond James Stadium",    27.9759, -82.5033, "America/New_York",
  "SoFi Stadium",             33.9535, -118.3392, "America/Los_Angeles",
  "Soldier Field",            41.8623, -87.6167, "America/Chicago",
  "TIAA Bank Field",          30.3240, -81.6373, "America/New_York",
  "U.S. Bank Stadium",        44.9736, -93.2575, "America/Chicago",
  "Lambeau Field",            44.5013, -88.0622, "America/Chicago",
  # Retractable roof (for reference, these get imputed not API'd)
  "AT&T Stadium",             32.7473, -97.0945, "America/Chicago",
  "Lucas Oil Stadium",        39.7601, -86.1639, "America/New_York",
  "Mercedes-Benz Stadium",    33.7554, -84.4010, "America/New_York",
  "NRG Stadium",              29.6847, -95.4107, "America/Chicago",
  "State Farm Stadium",       33.5276, -112.2626, "America/Phoenix",
  # International / Neutral
  "Tottenham Hotspur Stadium", 51.6043, -0.0662, "Europe/London",
  "Wembley Stadium",          51.5560, -0.2795, "Europe/London",
  "Allianz Arena",            48.2188, 11.6247, "Europe/Berlin",
  "Frankfurt Stadium",        50.0685, 8.6455, "Europe/Berlin",
  "Deutsche Bank Park",       50.0685, 8.6455, "Europe/Berlin",
  "Estadio Azteca (Mexico City)", 19.3029, -99.1505, "America/Mexico_City",
  "Arena Corinthians",       -23.5453, -46.4743, "America/Sao_Paulo",
  "Santiago Bernabeu Stadium", 40.4531, -3.6883, "Europe/Madrid"
)

# Aliases for stadium name mismatches
stadium_aliases <- tribble(
  ~raw_name, ~canonical_name,
  "TIAA Bank Field", "EverBank Stadium",
  "Heinz Field", "Acrisure Stadium",
  "FedEx Field", "FedExField",
  "Estadio Azteca", "Estadio Azteca (Mexico City)",
  "Deutsche Bank Park", "Frankfurt Stadium",
  "ROKiT Field - Dignity Health Sports Park", "SoFi Stadium",
  "GEHA Field at Arrowhead Stadium", "Arrowhead Stadium",
  "Commanders Field", "FedExField",
  "Cleveland Browns Stadium", "Cleveland Browns Stadium",
  "Levi's® Stadium", "Levi's Stadium"
)


# ============================================================================
# IDENTIFY MISSING WEATHER GAMES
# ============================================================================

get_missing_weather_games <- function(part_nfl_raw) {
  all_missing <- part_nfl_raw %>%
    filter(is.na(temp) | is.na(wind)) %>%
    filter(roof %ni% c('dome', 'closed')) %>%
    distinct(nflverse_game_id, season, week, home_team, away_team, 
             stadium, location, roof, start_time,
             game_date = as.Date(game_date))
  
  retractable <- data.frame(
    nflverse_game_id = character(), temp = numeric(), 
    wind = numeric(), source = character(), stringsAsFactors = FALSE
  )
  
  list(retractable = retractable, needs_api = all_missing)
}


# ============================================================================
# PARSE KICKOFF TIME
# ============================================================================

parse_kickoff <- function(start_time_str, game_date, local_tz) {
  # start_time is in ET like "9/11/16, 16:27:34"
  # Parse hour in ET, convert to local timezone
  
  parsed <- tryCatch({
    et_time <- mdy_hms(start_time_str, tz = "America/New_York")
    local_time <- with_tz(et_time, tzone = local_tz)
    hour(local_time)
  }, error = function(e) {
    # Fallback: assume 1pm local
    13
  })
  
  return(parsed)
}


# ============================================================================
# OPEN-METEO API CALL
# ============================================================================

fetch_hourly_weather <- function(lat, lon, date_str, tz) {
  # date_str in YYYY-MM-DD format
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", lat,
    "&longitude=", lon,
    "&start_date=", date_str,
    "&end_date=", date_str,
    "&hourly=temperature_2m,wind_speed_10m,precipitation",
    "&temperature_unit=fahrenheit",
    "&wind_speed_unit=mph",
    "&timezone=", URLencode(tz)
  )
  
  resp <- tryCatch({
    r <- GET(url, timeout(10))
    if (status_code(r) == 200) {
      content(r, as = "parsed", simplifyVector = TRUE)
    } else {
      cat("  HTTP", status_code(r), "for", date_str, "\n")
      NULL
    }
  }, error = function(e) {
    cat("  Error fetching weather for", date_str, ":", conditionMessage(e), "\n")
    NULL
  })
  
  return(resp)
}


# ============================================================================
# INTERPOLATE OVER GAME WINDOW
# ============================================================================

interpolate_game_weather <- function(hourly_data, kickoff_hour) {
  # Average over kickoff_hour to kickoff_hour + 3 (3-hour game window)
  if (is.null(hourly_data) || is.null(hourly_data$hourly)) {
    return(list(temp = NA, wind = NA, precip = NA))
  }
  
  hours <- 0:23
  game_hours <- kickoff_hour:(min(kickoff_hour + 3, 23))
  idx <- game_hours + 1  # 1-indexed
  
  temps <- hourly_data$hourly$temperature_2m[idx]
  winds <- hourly_data$hourly$wind_speed_10m[idx]
  precip <- hourly_data$hourly$precipitation[idx]
  
  list(
    temp = round(mean(temps, na.rm = TRUE), 1),
    wind = round(mean(winds, na.rm = TRUE), 1),
    precip = round(sum(precip, na.rm = TRUE), 2)
  )
}


# ============================================================================
# MAIN: RESOLVE STADIUM NAME AND FETCH
# ============================================================================

resolve_stadium <- function(raw_stadium, stadium_lookup, stadium_aliases) {
  # Check aliases first
  alias_match <- stadium_aliases %>% filter(raw_name == raw_stadium)
  if (nrow(alias_match) > 0) {
    raw_stadium <- alias_match$canonical_name[1]
  }
  
  # Fuzzy match on stadium_lookup
  match <- stadium_lookup %>% filter(stadium == raw_stadium)
  
  if (nrow(match) == 0) {
    # Try partial match
    match <- stadium_lookup %>% filter(grepl(raw_stadium, stadium, ignore.case = TRUE) |
                                         grepl(stadium, raw_stadium, ignore.case = TRUE))
  }
  
  if (nrow(match) > 0) {
    return(match[1, ])
  } else {
    return(NULL)
  }
}


backfill_weather <- function(part_nfl_raw) {
  
  games <- get_missing_weather_games(part_nfl_raw)
  
  cat("Retractable roof games (imputing 72/0):", nrow(games$retractable), "\n")
  cat("Games needing API weather:", nrow(games$needs_api), "\n\n")
  
  # Process API games
  api_results <- data.frame(
    nflverse_game_id = character(),
    temp = numeric(),
    wind = numeric(),
    precip = numeric(),
    source = character(),
    stringsAsFactors = FALSE
  )
  
  needs_api <- games$needs_api
  total <- nrow(needs_api)
  
  for (i in seq_len(total)) {
    row <- needs_api[i, ]
    
    if (i %% 10 == 0) cat("Progress:", i, "/", total, "\n")
    
    # Resolve stadium
    stadium_info <- resolve_stadium(row$stadium, stadium_lookup, stadium_aliases)
    
    if (is.null(stadium_info)) {
      cat("  WARNING: No stadium match for '", row$stadium, "' (", row$nflverse_game_id, ")\n")
      api_results <- rbind(api_results, data.frame(
        nflverse_game_id = row$nflverse_game_id,
        temp = NA, wind = NA, precip = NA, source = "missing_stadium"
      ))
      next
    }
    
    # Parse kickoff hour
    kickoff_hour <- parse_kickoff(row$start_time, row$game_date, stadium_info$tz)
    
    # Fetch hourly weather
    hourly <- fetch_hourly_weather(
      stadium_info$lat, stadium_info$lon,
      as.character(row$game_date), stadium_info$tz
    )
    
    # Interpolate
    weather <- interpolate_game_weather(hourly, kickoff_hour)
    
    api_results <- rbind(api_results, data.frame(
      nflverse_game_id = row$nflverse_game_id,
      temp = weather$temp,
      wind = weather$wind,
      precip = weather$precip,
      source = "open_meteo"
    ))
    
    # Rate limit: be nice to the API
    Sys.sleep(0.2)
  }
  
  # Combine retractable + API results
  retractable_results <- games$retractable %>%
    select(nflverse_game_id, temp, wind) %>%
    mutate(precip = 0, source = "imputed_retractable")
  
  all_weather <- rbind(retractable_results, api_results)
  
  cat("\n========================================\n")
  cat("BACKFILL SUMMARY\n")
  cat("========================================\n")
  cat("Retractable roof imputed:", nrow(retractable_results), "\n")
  cat("API fetched:", sum(api_results$source == "open_meteo"), "\n")
  cat("Missing stadium:", sum(api_results$source == "missing_stadium"), "\n")
  cat("API failures (NA):", sum(is.na(api_results$temp) & api_results$source == "open_meteo"), "\n")
  
  return(all_weather)
}


# ============================================================================
# USAGE
# ============================================================================
# 
weather_backfill <- backfill_weather(part_nfl_raw)
# 
# # Check for unmatched stadiums
weather_backfill %>% filter(source == "missing_stadium")
# 
# # Apply to part_nfl:
# # For each game in weather_backfill, update temp/wind in part_nfl
# # where the nflverse_game_id matches and temp/wind are NA
#
for (i in seq_len(nrow(weather_backfill))) {
 gid <- weather_backfill$nflverse_game_id[i]
 idx <- which(part_nfl$nflverse_game_id == gid & (is.na(part_nfl$temp) | is.na(part_nfl$wind)))
 if (length(idx) > 0) {
   part_nfl$temp[idx] <- weather_backfill$temp[i]
   part_nfl$wind[idx] <- weather_backfill$wind[i]
   part_nfl$rain_ind[idx] <- as.integer(weather_backfill$precip[i] > 0.1)
   part_nfl$snow_ind[idx] <- as.integer(weather_backfill$precip[i] > 0.1 & weather_backfill$temp[i] <= 35)
 }
}


###
###


# Get the 27 missing game IDs
missing_27 <- weather_backfill %>% filter(source == "missing_stadium")

# Re-fetch with the fixed aliases
refetch <- part_nfl %>%
  filter(nflverse_game_id %in% missing_27$nflverse_game_id) %>%
  distinct(nflverse_game_id, season, week, home_team, away_team,
           stadium, location, roof, start_time,
           game_date = as.Date(game_date))

# Loop and fetch
for (i in seq_len(nrow(refetch))) {
  row <- refetch[i, ]
  stadium_info <- resolve_stadium(row$stadium, stadium_lookup, stadium_aliases)
  if (is.null(stadium_info)) {
    cat("STILL MISSING:", row$stadium, "\n")
    next
  }
  kickoff_hour <- parse_kickoff(row$start_time, row$game_date, stadium_info$tz)
  hourly <- fetch_hourly_weather(stadium_info$lat, stadium_info$lon,
                                 as.character(row$game_date), stadium_info$tz)
  weather <- interpolate_game_weather(hourly, kickoff_hour)
  
  # Update weather_backfill
  idx <- which(weather_backfill$nflverse_game_id == row$nflverse_game_id)
  weather_backfill$temp[idx] <- weather$temp
  weather_backfill$wind[idx] <- weather$wind
  weather_backfill$precip[idx] <- weather$precip
  weather_backfill$source[idx] <- "open_meteo"
  
  Sys.sleep(0.2)
}

# Now apply those 27 to part_nfl
for (i in which(weather_backfill$nflverse_game_id %in% missing_27$nflverse_game_id)) {
  gid <- weather_backfill$nflverse_game_id[i]
  idx <- which(part_nfl$nflverse_game_id == gid & (is.na(part_nfl$temp) | is.na(part_nfl$wind)))
  if (length(idx) > 0 && !is.na(weather_backfill$temp[i])) {
    part_nfl$temp[idx] <- weather_backfill$temp[i]
    part_nfl$wind[idx] <- weather_backfill$wind[i]
    part_nfl$rain_ind[idx] <- as.integer(weather_backfill$precip[i] > 0.1)
    part_nfl$snow_ind[idx] <- as.integer(weather_backfill$precip[i] > 0.1 & weather_backfill$temp[i] <= 35)
  }
}

# Re-save the updated backfill
saveRDS(weather_backfill, "~/weather_backfill.rds")


missing_ids <- part_nfl %>%
  filter(is.na(temp) | is.na(wind), roof == 0) %>%
  distinct(nflverse_game_id) %>%
  pull(nflverse_game_id)

length(missing_ids)  # should be 27

for (gid in missing_ids) {
  row <- part_nfl %>% filter(nflverse_game_id == gid) %>% slice(1)
  
  stadium_info <- resolve_stadium(row$stadium, stadium_lookup, stadium_aliases)
  if (is.null(stadium_info)) { cat("STILL MISSING:", row$stadium, "\n"); next }
  
  kickoff_hour <- parse_kickoff(row$start_time, row$game_date, stadium_info$tz)
  hourly <- fetch_hourly_weather(stadium_info$lat, stadium_info$lon,
                                 as.character(as.Date(row$game_date)), stadium_info$tz)
  weather <- interpolate_game_weather(hourly, kickoff_hour)
  
  pn_idx <- which(part_nfl$nflverse_game_id == gid)
  part_nfl$temp[pn_idx] <- weather$temp
  part_nfl$wind[pn_idx] <- weather$wind
  part_nfl$rain_ind[pn_idx] <- as.integer(weather$precip > 0.1)
  part_nfl$snow_ind[pn_idx] <- as.integer(weather$precip > 0.1 & weather$temp <= 35)
  
  cat(gid, ":", weather$temp, "F /", weather$wind, "mph\n")
  Sys.sleep(0.2)
}


# Overwrite the 27 in weather_backfill from part_nfl
for (gid in missing_ids) {
  row <- part_nfl %>% filter(nflverse_game_id == gid) %>% slice(1)
  wb_idx <- which(weather_backfill$nflverse_game_id == gid)
  weather_backfill$temp[wb_idx] <- row$temp
  weather_backfill$wind[wb_idx] <- row$wind
  weather_backfill$precip[wb_idx] <- 0  # already applied rain/snow from API
  weather_backfill$source[wb_idx] <- "open_meteo"
}

# Verify
sum(weather_backfill$source == "missing_stadium")  # should be 0

# Save
saveRDS(weather_backfill, "~/weather_backfill.rds")