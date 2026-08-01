# ============================================================
# OFFENSE: PFF <-> NFLverse gsis_id reconciliation
# Output: combined_ids (one row per PFF player-week)
#
# 2026-08: ported to the defense-build architecture.
#   - uniqueness-gated 5-tier cascade replaces the old 4-method join;
#     pos_group is no longer a join key (gating handles disambiguation)
#   - manual overrides fill NAs only - a hand-typed value never beats
#     a matched one (the Mattison/Murphy bug was manual-first coalesce)
#   - stopifnot hard assertions replace print-only checks
#   - drop_na narrowed to join keys: an NA position/franchise_id must
#     not delete a row that method 1 (pff_id) could still match
#
# Run order: this file -> pff_ids_build_defense_AWS -> pff_ids_validate_cross.R
# ============================================================

keep_objects <- c("con")
rm(list = setdiff(ls(), keep_objects))

SEASONS <- 2016:2025

# === HELPERS ===

run_athena_query <- function(sql) {
  start_cmd <- sprintf(
    'aws athena start-query-execution --query-string "%s" --result-configuration OutputLocation=s3://nfl-pff-data-lucas/athena-results/ --query-execution-context Database=nfl_data --output text',
    gsub('"', '\\"', sql)
  )
  query_id <- system(start_cmd, intern = TRUE)
  Sys.sleep(2)
  result_cmd <- sprintf('aws athena get-query-execution --query-execution-id %s --query "QueryExecution.ResultConfiguration.OutputLocation" --output text', query_id)
  s3_path <- system(result_cmd, intern = TRUE)
  read.csv(pipe(sprintf('aws s3 cp %s -', s3_path)))
}

normalize_position <- function(pos) {
  case_when(
    pos %in% c("HB", "RB", "FB") ~ "BACK",
    pos %in% c("WR", "TE") ~ "REC",
    pos %in% c("QB") ~ "QB",
    pos %in% c("T", "G", "C", "OL", "OT", "OG") ~ "OL",
    pos %in% c("CB", "DB", "S", "FS", "SS") ~ "DB",
    pos %in% c("LB", "ILB", "OLB", "MLB") ~ "LB",
    pos %in% c("DE", "DT", "DL", "NT", "EDGE", "DI", "ED") ~ "DL",
    TRUE ~ "OTHER"
  )
}

standardize_cols <- function(df) {
  colnames(df) <- tolower(colnames(df))
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

clean_name <- function(x) {
  x %>%
    trimws() %>%
    gsub("\\s+(Jr\\.?|Sr\\.?|II|III|IV|V|VI)$", "", .) %>%
    gsub("\\.", "", .) %>%
    trimws() %>%
    tolower()
}

# === LOAD NFLVERSE ROSTERS ===
# recode blocks stay IN the build: they cover SD-2016 / OAK<=2019 and the
# historical window generally - the 2025-derived pff_team_lookup in
# validate_cross knows nothing about pre-2025 franchises

ids_one <- load_rosters_weekly(season = SEASONS)

ids_one$team[which(ids_one$team == "ARI")] <- "ARZ"
ids_one$team[which(ids_one$team == "BAL")] <- "BLT"
ids_one$team[which(ids_one$team == "CLE")] <- "CLV"
ids_one$team[which(ids_one$team == "HOU")] <- "HST"
ids_one$team[which(ids_one$team == "LAC" & ids_one$season == 2016)] <- "SD"
ids_one$team[which(ids_one$team == "LV" & ids_one$season <= 2019)] <- "OAK"

ids_one$week <- ifelse(ids_one$week == 18 & ids_one$season <= 2020, 28, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 19 & ids_one$season <= 2020, 29, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 19 & ids_one$season > 2020, 28, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 20 & ids_one$season <= 2020, 30, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 20 & ids_one$season > 2020, 29, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 21 & ids_one$season <= 2020, 32, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 21 & ids_one$season > 2020, 30, ids_one$week)
ids_one$week <- ifelse(ids_one$week == 22, 32, ids_one$week)

# === PULL PFF OFFENSE TABLES ===

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

# Query 7 (receiving_no_targets) REMOVED 2026-08:
# its SELECT emitted NULL player / franchise_id / position, so the old broad
# drop_na() deleted 100% of its rows. Ten seasons contributed exactly one
# orphan player_id not covered elsewhere. Pure Athena cost, zero value.

# Query 8: receiving_with_targets
pff_receiving_targets <- run_athena_query("
  SELECT DISTINCT player, player_id, team, franchise_id, week, season, position
  FROM receiving_with_targets
")

cat("All Athena queries completed.\n")

# === DISTINCT PFF PLAYER-WEEKS (modal position) ===
# drop_na narrowed to join keys; franchise_id is OUT of the group so an NA
# there cannot split one player-week into two rows (recovered first-non-NA)

pff_ids_one <- bind_rows(
  standardize_cols(pff_pass_block),
  standardize_cols(pff_passing_pressure),
  standardize_cols(pff_passing_tip),
  standardize_cols(pff_receiver_scheme),
  standardize_cols(pff_run_block),
  standardize_cols(pff_rushing),
  standardize_cols(pff_receiving_targets)
) %>%
  distinct() %>%
  drop_na(player, player_id, team, week, season) %>%
  group_by(player, player_id, team, week, season) %>%
  summarise(position = names(sort(table(position), decreasing = TRUE))[1],
            franchise_id = dplyr::first(franchise_id[!is.na(franchise_id)]),
            .groups = "drop") %>%
  arrange(player_id, season, week)

# === MANUAL OVERRIDES (fill-only) ===
# Applied AFTER the cascade, filling NAs only. Kept in full - the gated
# season/career tiers below will make many redundant; the fired-report
# at the bottom identifies prune candidates.

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
  "Alexander Mattison",  "00-0034972",  # fixed 2026-08: was 00-0035236 (Byron Murphy Jr.'s id)
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
  "Michael Woods II",    "00-0037300",
  "Michael Morgan",      "00-0028443",  # absent from 2016 weekly roster file - verified vs seasonal rosters
  "Zachary Orr",         "00-0030700"  # PFF "Zachary" vs nflverse "Zach" - name tiers can't bridge diminutives
)

# hand-typed entries are the one remaining ingress for a malformed id - gate them here
stopifnot(
  all(grepl("^\\d{2}-\\d{7}$", manual_gsis_ids$gsis_id)),
  anyDuplicated(manual_gsis_ids$player_name) == 0
)

# === PREP BOTH SIDES ===

ids_one_keyed <- ids_one %>%
  filter(!is.na(gsis_id)) %>%
  mutate(pff_id = as.numeric(pff_id),
         name_key = clean_name(full_name))

# pos_group kept for downstream analytics; no longer a join key -
# uniqueness gating replaces position disambiguation
pff_keyed <- pff_ids_one %>%
  mutate(name_key = clean_name(player),
         pos_group = normalize_position(position))

# === LOOKUPS (each uniqueness-gated) ===

lk_pff_id <- ids_one_keyed %>%
  filter(!is.na(pff_id)) %>%
  group_by(pff_id, team, week, season) %>%
  summarise(g = first(gsis_id), nu = n_distinct(gsis_id), .groups = "drop") %>%
  filter(nu == 1) %>% select(pff_id, team, week, season, gsis_id = g)

lk_name_tws <- ids_one_keyed %>%
  group_by(name_key, team, week, season) %>%
  summarise(g = first(gsis_id), nu = n_distinct(gsis_id), .groups = "drop") %>%
  filter(nu == 1) %>% select(name_key, team, week, season, gsis_id_a = g)

lk_name_ts <- ids_one_keyed %>%
  group_by(name_key, team, season) %>%
  summarise(g = first(gsis_id), nu = n_distinct(gsis_id), .groups = "drop") %>%
  filter(nu == 1) %>% select(name_key, team, season, gsis_id_b = g)

lk_name_s <- ids_one_keyed %>%
  group_by(name_key, season) %>%
  summarise(g = first(gsis_id), nu = n_distinct(gsis_id), .groups = "drop") %>%
  filter(nu == 1) %>% select(name_key, season, gsis_id_c = g)

lk_name_only <- ids_one_keyed %>%
  group_by(name_key) %>%
  summarise(g = first(gsis_id), nu = n_distinct(gsis_id), .groups = "drop") %>%
  filter(nu == 1) %>% select(name_key, gsis_id_d = g)

# === BUILD COMBINED_IDS ===

combined_ids <- pff_keyed %>%
  left_join(lk_pff_id, by = c("player_id" = "pff_id", "team", "week", "season")) %>%
  rename(gsis_id_pff = gsis_id) %>%
  left_join(lk_name_tws, by = c("name_key", "team", "week", "season")) %>%
  left_join(lk_name_ts, by = c("name_key", "team", "season")) %>%
  left_join(lk_name_s, by = c("name_key", "season")) %>%
  left_join(lk_name_only, by = "name_key") %>%
  mutate(gsis_cascade = coalesce(gsis_id_pff, gsis_id_a, gsis_id_b, gsis_id_c, gsis_id_d)) %>%
  left_join(manual_gsis_ids %>% rename(gsis_id_manual = gsis_id),
            by = c("player" = "player_name")) %>%
  mutate(gsis_id = coalesce(gsis_cascade, gsis_id_manual))

# soft report: manual entries still doing work (absent names = prune candidates)
cat("Manual overrides that fired (cascade produced NA):\n")
combined_ids %>%
  filter(is.na(gsis_cascade), !is.na(gsis_id_manual)) %>%
  distinct(player, gsis_id_manual) %>%
  print(n = Inf)

combined_ids <- combined_ids %>%
  group_by(player_id) %>%
  fill(gsis_id, .direction = "downup") %>%
  ungroup() %>%
  select(-name_key, -gsis_id_pff, -gsis_id_a, -gsis_id_b, -gsis_id_c,
         -gsis_id_d, -gsis_cascade, -gsis_id_manual)

# === INTEGRITY CHECKS ===

dupe_check <- combined_ids %>%
  group_by(player_id, week, season) %>%
  summarise(n = n(), .groups = "drop") %>% filter(n > 1)

check_pid_name <- combined_ids %>%
  group_by(player_id) %>%
  summarise(n_names = n_distinct(player), .groups = "drop") %>% filter(n_names > 1)

check_pid_gsis <- combined_ids %>%
  filter(!is.na(gsis_id)) %>%
  group_by(player_id) %>%
  summarise(n_gsis = n_distinct(gsis_id), .groups = "drop") %>% filter(n_gsis > 1)

check_gsis_pid <- combined_ids %>%
  filter(!is.na(gsis_id)) %>%
  group_by(gsis_id) %>%
  summarise(n_pids = n_distinct(player_id), .groups = "drop") %>% filter(n_pids > 1)

check_gsis_week <- combined_ids %>%
  filter(!is.na(gsis_id)) %>%
  group_by(gsis_id, week, season) %>%
  summarise(n = n(), .groups = "drop") %>% filter(n > 1)

check_critical_na <- combined_ids %>%
  filter(is.na(player) | is.na(player_id) | is.na(team) |
           is.na(week) | is.na(season))

# franchise_id may legitimately be NA post-narrowing - exclude before the consistency check
check_fid_team <- combined_ids %>%
  filter(!is.na(franchise_id)) %>%
  group_by(season, franchise_id) %>%
  summarise(n_teams = n_distinct(team), .groups = "drop") %>% filter(n_teams > 1)

check_gsis_format <- combined_ids %>%
  filter(!is.na(gsis_id), !grepl("^\\d{2}-\\d{7}$", gsis_id))

# bounds derive from SEASONS - next year, edit SEASONS at the top only
check_bounds <- combined_ids %>%
  filter(week < 1 | week > 32 |
           season < min(SEASONS) | season > max(SEASONS) |
           player_id <= 0)

stopifnot(
  nrow(dupe_check) == 0,
  nrow(check_pid_name) == 0,
  nrow(check_pid_gsis) == 0,
  nrow(check_gsis_pid) == 0,
  nrow(check_gsis_week) == 0,
  nrow(check_critical_na) == 0,
  nrow(check_fid_team) == 0,
  nrow(check_gsis_format) == 0,
  nrow(check_bounds) == 0
)

# === MATCH RATE BY SEASON (soft report - expected ~99.9%+) ===

combined_ids %>%
  group_by(season) %>%
  summarise(total = n(),
            matched = sum(!is.na(gsis_id)),
            pct = round(100 * matched / total, 1))

# unmatched residue - candidates for new manual entries
combined_ids %>%
  filter(is.na(gsis_id)) %>%
  select(player, player_id, team, season) %>%
  distinct()

# === CLEANUP ===

rm(
  ids_one_keyed, pff_keyed,
  lk_pff_id, lk_name_tws, lk_name_ts, lk_name_s, lk_name_only,
  pff_ids_one,
  pff_pass_block, pff_passing_pressure, pff_passing_tip,
  pff_receiver_scheme, pff_run_block, pff_rushing, pff_receiving_targets,
  dupe_check,
  check_pid_name, check_pid_gsis, check_gsis_pid,
  check_gsis_week, check_critical_na, check_fid_team,
  check_gsis_format, check_bounds
)
gc()