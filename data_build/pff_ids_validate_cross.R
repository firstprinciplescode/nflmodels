# ============================================================
# CROSS-TABLE VALIDATION
# Run AFTER pff_ids_build_AWS.R and pff_ids_build_defense_AWS.
#
# Each build script proves 1:1 within itself. Neither can see a
# collision that spans the two tables - a gsis_id wrongly assigned
# in offense and correctly assigned in defense passes both sets of
# checks. That is exactly how Mattison carried Byron Murphy Jr.'s
# id for six seasons.
#
# Also emits: id_xwalk, team_map, pff_team_lookup
# ============================================================

both_ids <- bind_rows(
  combined_ids %>% mutate(src = "off"),
  combined_ids_defense %>% mutate(src = "def")
) %>%
  filter(!is.na(gsis_id))

# one gsis_id claimed by two different PFF players
xt_gsis_collision <- both_ids %>%
  distinct(player_id, gsis_id, player, src) %>%
  group_by(gsis_id) %>%
  filter(n_distinct(player_id) > 1) %>%
  arrange(gsis_id, player_id) %>%
  ungroup()

# one PFF player carrying two gsis_ids across the tables
xt_pid_collision <- both_ids %>%
  distinct(player_id, gsis_id, player, src) %>%
  group_by(player_id) %>%
  filter(n_distinct(gsis_id) > 1) %>%
  arrange(player_id, gsis_id) %>%
  ungroup()

# resolve a disputed id against nflverse rosters - whoever the roster
# says owns it wins; null the loser's gsis_id in its build script
resolve_gsis <- function(g, seasons = SEASONS) {
  nflreadr::load_rosters(seasons) %>%
    filter(gsis_id == g) %>%
    distinct(full_name, position, team, season, gsis_id) %>%
    arrange(season)
}

if (nrow(xt_gsis_collision) > 0) {
  print(xt_gsis_collision, n = Inf)
  message("resolve each with: resolve_gsis(\"<gsis_id>\")")
}
if (nrow(xt_pid_collision) > 0) {
  print(xt_pid_collision, n = Inf)
}

stopifnot(
  nrow(xt_gsis_collision) == 0,
  nrow(xt_pid_collision) == 0
)

# === THE CROSSWALK EVERYTHING DOWNSTREAM USES ===

id_xwalk <- both_ids %>%
  group_by(player_id, gsis_id) %>%
  summarise(player = first(player),
            pos_group = names(sort(table(pos_group), decreasing = TRUE))[1],
            first_ssn = min(season),
            last_ssn = max(season),
            .groups = "drop")

stopifnot(
  n_distinct(id_xwalk$player_id) == nrow(id_xwalk),
  n_distinct(id_xwalk$gsis_id) == nrow(id_xwalk)
)

# === PFF <-> NFLVERSE TEAM ABBREVIATION MAP (derived, not hardcoded) ===

team_map <- both_ids %>%
  filter(season == max(SEASONS)) %>%
  distinct(gsis_id, pff_team = team) %>%
  inner_join(nflreadr::load_rosters(max(SEASONS)) %>% distinct(gsis_id, nfl_team = team),
             by = "gsis_id") %>%
  count(nfl_team, pff_team) %>%
  group_by(nfl_team) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

stopifnot(nrow(team_map) == 32)

pff_team_lookup <- tibble::deframe(team_map %>% select(nfl_team, pff_team))

# eyeball the disagreements once (expect ARZ/BLT/CLV/HST)
team_map %>% filter(nfl_team != pff_team)

rm(both_ids, xt_gsis_collision, xt_pid_collision)
