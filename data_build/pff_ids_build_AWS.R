
keep_objects <- c("con")
rm(list = setdiff(ls(), keep_objects))

ids_one <- load_rosters_weekly(season = c(2016:2025))


ids_one$team[which(ids_one$team == "ARI")] = "ARZ"
ids_one$team[which(ids_one$team == "BAL")] = "BLT"
ids_one$team[which(ids_one$team == "CLE")] = "CLV"
ids_one$team[which(ids_one$team == "HOU")] = "HST"
ids_one$team[which(ids_one$team == "LAC" & ids_one$season == 2016)] = "SD"
ids_one$team[which(ids_one$team == "LV" & ids_one$season <= 2019)] = "OAK"

ids_one$week <- ifelse(ids_one$week == 18 & ids_one$season <= 2020, 28, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 19 & ids_one$season <= 2020, 29, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 19 & ids_one$season > 2020, 28, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 20 & ids_one$season <= 2020, 30, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 20 & ids_one$season > 2020, 29, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 21 & ids_one$season <= 2020, 32, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 21 & ids_one$season > 2020, 30, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 22, 32, ids_one$week)


#

run_athena_query <- function(sql) {
  # Start query
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  
  # Wait for completion
  Sys.sleep(2)
  
  # Get results location
  result_cmd <- sprintf('aws athena get-query-execution --query-execution-id %s --query "QueryExecution.ResultConfiguration.OutputLocation" --output text', query_id)
  s3_path <- system(result_cmd, intern = TRUE)
  
  # Read CSV result
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}

# Query 1: pass_block_summary
pff_pass_block <- run_athena_query("
  SELECT DISTINCT player, player_id, team, franchise_id, week, season, position
  FROM pass_block_summary
")

# Query 2: passing_pressure
pff_passing_pressure <- run_athena_query("
  SELECT DISTINCT player, player_id, team, franchise_id, week, season, position
  FROM passing_pressure
")

# Query 3: passing_tip (time in pocket)
pff_passing_tip <- run_athena_query("
  SELECT DISTINCT player, player_id, team, franchise_id, week, season, position
  FROM passing_tip
")

# Query 4: receiver_scheme
pff_receiver_scheme <- run_athena_query("
  SELECT DISTINCT player, player_id, team, franchise_id, week, season, position
  FROM receiver_scheme
")

# Query 5: run_block_summary
pff_run_block <- run_athena_query("
  SELECT DISTINCT player, player_id, team, franchise_id, week, season, position
  FROM run_block_summary
")

# Query 6: rushing_summary
pff_rushing <- run_athena_query("
  SELECT DISTINCT player, player_id, team, franchise_id, week, season, position
  FROM rushing_summary
")

# Query 7: receiving_no_targets (needs join for team abbreviation)
pff_receiving_no_targets <- run_athena_query("
  SELECT DISTINCT 
    NULL AS player, 
    rec.player_id, 
    team.abbreviation AS team, 
    NULL AS franchise_id,
    CAST(game_week AS BIGINT) AS week,
    rec.season AS season,
    NULL AS position
  FROM receiving_no_targets rec 
  LEFT JOIN team_ids_tbl team 
    ON CAST(rec.player_franchise_id AS BIGINT) = team.teamid 
    AND rec.season = team.season
")

# Query 8: receiving_with_targets
pff_receiving_targets <- run_athena_query("
  SELECT DISTINCT player, player_id, team, franchise_id, week, season, position
  FROM receiving_with_targets
")

cat("All Athena queries completed.\n")


###
###


cat("Combining PFF data sources...\n")

# Standardize column names (Athena returns lowercase)
standardize_cols <- function(df) {
  colnames(df) <- tolower(colnames(df))
  # Ensure consistent types
  df %>%
    mutate(
      player = as.character(player),
      player_id = as.numeric(player_id),
      team = as.character(team),
      franchise_id = as.numeric(franchise_id),
      week = as.numeric(week),
      season = as.numeric(season),
      position = as.character(position)
    )
}

pff_ids_one <- bind_rows(
  standardize_cols(pff_pass_block),
  standardize_cols(pff_passing_pressure),
  standardize_cols(pff_passing_tip),
  standardize_cols(pff_receiver_scheme),
  standardize_cols(pff_run_block),
  standardize_cols(pff_rushing),
  standardize_cols(pff_receiving_no_targets),
  standardize_cols(pff_receiving_targets)
) %>%
  distinct() %>%
  arrange(player_id, season, week) %>%
  drop_na()  



####
####


# 1. MANUAL OVERRIDES TABLE - just add new players here each year
manual_gsis_ids <- tribble(
  ~player_name,          ~gsis_id,
  "Zach Ertz",           "00-0030061",
  "Matt Hazel",          "00-0031315",
  "KaVontae Turpin",     "00-0037801",
  "De'Marcus Ayers",     "00-0032365",
  "Walter Powell",       "00-0031268",
  "Rodney Williams",     "00-0037451",
  "Michael Woods",       "00-0037300",
  "Kyric McGowan",       "00-0037172",
  "De'Von Achane",       "00-0039040",
  "Chris Brooks",        "00-0038685",
  "Tank Dell",           "00-0038977",
  "Adonai Mitchell",     "00-0039890",
  "Alec Pierce",         "00-0037664",
  "Alex Bachman",        "00-0035602",
  "Alexander Mattison",  "00-0035236",  # NOTE: was wrong in original (had Bachman's ID)
  "Brian Thomas",        "00-0039893",
  "Brock Bowers",        "00-0039338",
  "Caleb Williams",      "00-0039918",
  "Jamari Thrash",       "00-0039379",
  "Kimani Vidal",        "00-0039391",
  "Ladd McConkey",       "00-0039915",
  "Rome Odunze",         "00-0039919",
  "Scott Matlock",       "00-0038614",
  "Tip Reiman",          "00-0039737",
  "Jaelon Darden",       "00-0036877",
  "Clayton Tune",        "00-0038582",
  "Michael Woods II",    "00-0037300"
)

# 2. POSITION MAPPING FUNCTION
normalize_position <- function(pos) {
  case_when(
    pos %in% c("HB", "RB", "FB") ~ "BACK",
    pos %in% c("WR", "TE") ~ "REC",
    pos %in% c("QB") ~ "QB",
    pos %in% c("T", "G", "C", "OL", "OT", "OG") ~ "OL",
    pos %in% c("CB", "DB", "S", "FS", "SS") ~ "DB",
    pos %in% c("LB", "ILB", "OLB", "MLB") ~ "LB",
    pos %in% c("DE", "DT", "DL", "NT", "EDGE") ~ "DL",
    TRUE ~ "OTHER"
  )
}

# 3. PREP IDS_ONE WITH ALL JOIN KEYS
ids_one_prepped <- ids_one %>%
  mutate(
    pff_id = as.numeric(pff_id),
    full_name_calc = paste0(first_name, " ", last_name),
    full_name_clean = trimws(gsub("(Jr\\.|Sr\\.|II|III|IV|V|VI)$", "", full_name)),
    full_name_calc_clean = trimws(gsub("(Jr\\.|Sr\\.|II|III|IV|V|VI)$", "", full_name_calc)),
    pos_group = normalize_position(position)
  ) %>%
  select(pff_id, team, week, season, gsis_id, full_name, full_name_calc, full_name_clean, full_name_calc_clean, position, pos_group)

# 4. PREP PFF DATA
pff_prepped <- pff_ids_one %>%
  mutate(
    player_clean = trimws(gsub("(Jr\\.|Sr\\.|II|III|IV|V|VI)$", "", player)),
    pos_group = normalize_position(position)
  )

# 5. SINGLE JOIN APPROACH - try all methods, coalesce
combined_ids <- pff_prepped %>%
  # Method 1: pff_id match (most reliable, no position needed)
  left_join(
    ids_one_prepped %>% select(pff_id, team, week, season, gsis_id_pff = gsis_id) %>% distinct(),
    by = c("player_id" = "pff_id", "team", "week" = "week", "season" = "season")
  ) %>%
  # Method 2: full_name_calc match (first + last) WITH position
  left_join(
    ids_one_prepped %>% select(full_name_calc, team, week, season, pos_group, gsis_id_calc = gsis_id) %>% distinct(),
    by = c("player" = "full_name_calc", "team", "week" = "week", "season" = "season", "pos_group" = "pos_group")
  ) %>%
  # Method 3: full_name match WITH position
  left_join(
    ids_one_prepped %>% select(full_name, team, week, season, pos_group, gsis_id_full = gsis_id) %>% distinct(),
    by = c("player" = "full_name", "team", "week" = "week", "season" = "season", "pos_group" = "pos_group")
  ) %>%
  # Method 4: cleaned name match (no Jr/Sr/III) WITH position
  left_join(
    ids_one_prepped %>% select(full_name_clean, team, week, season, pos_group, gsis_id_clean = gsis_id) %>% distinct(),
    by = c("player_clean" = "full_name_clean", "team", "week" = "week", "season" = "season", "pos_group" = "pos_group")
  ) %>%
  # Method 5: manual overrides
  left_join(
    manual_gsis_ids,
    by = c("player" = "player_name")
  ) %>%
  # COALESCE - priority order
  mutate(
    gsis_id = coalesce(gsis_id, gsis_id_pff, gsis_id_calc, gsis_id_full, gsis_id_clean)
  ) %>%
  select(-gsis_id_pff, -gsis_id_calc, -gsis_id_full, -gsis_id_clean, -player_clean)

# 6. FILL DOWN WITHIN PLAYER_ID (for weeks where one method worked)
combined_ids <- combined_ids %>%
  group_by(player_id) %>%
  fill(gsis_id, .direction = "downup") %>%
  ungroup()

# 7. CHECK WHAT'S STILL MISSING
missing <- combined_ids %>% 
  filter(is.na(gsis_id)) %>%
  select(player, player_id, team, season) %>%
  distinct()

if(nrow(missing) > 0) {
  cat("Players still missing GSIS_ID - add to manual_gsis_ids:\n")
  print(missing)
}


# 1. Check for duplicates (same player_id + team + week + season should be unique)
combined_ids %>%
  group_by(player_id, team, week, season) %>%
  filter(n() > 1) %>%
  arrange(player_id, season, week)

# 2. Check for multiple gsis_ids per player_id (should be 1:1)
combined_ids %>%
  group_by(player_id) %>%
  summarise(n_gsis = n_distinct(gsis_id, na.rm = TRUE)) %>%
  filter(n_gsis > 1)

# 3. Check for multiple player_ids per gsis_id (should be 1:1)
combined_ids %>%
  filter(!is.na(gsis_id)) %>%
  group_by(gsis_id) %>%
  summarise(n_pff = n_distinct(player_id), players = paste(unique(player), collapse = ", ")) %>%
  filter(n_pff > 1)

# 4. Spot check some known players
combined_ids %>%
  filter(player %in% c("Patrick Mahomes", "Josh Allen", "Derrick Henry", "Tyreek Hill", "Travis Kelce")) %>%
  select(player, player_id, gsis_id, team, season) %>%
  distinct() %>%
  arrange(player, season)

# 5. NA summary
combined_ids %>%
  summarise(
    total_rows = n(),
    na_gsis = sum(is.na(gsis_id)),
    pct_na = round(100 * sum(is.na(gsis_id)) / n(), 2)
  )

# 6. Coverage by season
combined_ids %>%
  group_by(season) %>%
  summarise(
    total = n(),
    matched = sum(!is.na(gsis_id)),
    pct = round(100 * matched / total, 1)
  )


