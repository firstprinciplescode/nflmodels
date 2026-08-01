target_cols <- c("ypa", "pbp_xypa", "part_xypa", "ypc", "pbp_xypc", "part_xypc", "tds",  "pbp_xtds", "part_xtds", "twp_rate", "ypa_rank", "pbp_xypa_rank", "part_xypa_rank", "ypc_rank", "pbp_xypc_rank", "part_xypc_rank", "pbp_xtds_rank", "part_xtds_rank", "twp_rate_rank")


ne_2025_def <- qb_stats_df_final %>% filter(qbgrp_ssn == "NEMaye-2025") %>% pull(def_ssn)
ne_2026_def <- paste0(c("SEA","PIT","JAX","BUF","LV","NYJ","CHI","MIA","GB","DET",
                        "LAC","BUF","MIN","KC","NYJ","DEN","MIA"), "2025")

only_2025 <- setdiff(unique(sub("2025$","", ne_2025_def)), unique(sub("2025$","", ne_2026_def)))
only_2026 <- setdiff(unique(sub("2025$","", ne_2026_def)), unique(sub("2025$","", ne_2025_def)))

def_profile_2025 <- qb_stats_df_final %>%
  filter(season == 2025) %>%
  group_by(def_ssn) %>%
  summarise(g = n(), across(all_of(target_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

keep_cols <- target_cols[colSums(!is.na(as.data.frame(def_profile_2025)[target_cols])) > 0]

cmp_diff <- def_profile_2025 %>%
  filter(def_ssn %in% c(paste0(only_2025,"2025"), paste0(only_2026,"2025"))) %>%
  mutate(grp = if_else(sub("2025$","", def_ssn) %in% only_2026, "new_2026", "dropped_2025")) %>%
  select(grp, def_ssn, g, all_of(keep_cols)) %>%
  arrange(grp, def_ssn)

cmp_diff_median <- cmp_diff %>%
  group_by(grp) %>%
  summarise(n_teams = n(), across(all_of(keep_cols), ~ median(.x, na.rm = TRUE)), .groups = "drop")

print(cmp_diff, n = Inf)
print(cmp_diff_median)

View(cmp_diff_median)

write.csv(cmp_diff_median, 'ne_schedule_comparison.csv')

# teardown
# rm(target_cols, ne_2025_def, ne_2026_def, only_2025, only_2026,
#   def_profile_2025, keep_cols, cmp_diff, cmp_diff_median)




#####
## RUSHING
#####



rush_target_cols <- c("ypc","ybc","yac","pbp_xypc","part_xypc","pbp_xtd","part_xtd","attempts", "ypc_rank","ybc_rank","yac_rank","pbp_xypc_rank","part_xypc_rank", "pbp_xtd_rank","part_xtd_rank","attempts_rank")

ne_2025_def <- rush_stats_high %>% filter(off_ssn == "NE2025") %>% pull(def_ssn)

# 2026 opps from ESPN schedule -> proxied to 2025 D. both NY = NYJ, LA = LAC, SEA = 17th
ne_2026_def <- paste0(c("SEA","PIT","JAX","BUF","LV","NYJ","CHI","MIA","GB","DET",
                        "LAC","BUF","MIN","KC","NYJ","DEN","MIA"), "2025")

only_2025 <- setdiff(unique(sub("2025$","", ne_2025_def)), unique(sub("2025$","", ne_2026_def)))
only_2026 <- setdiff(unique(sub("2025$","", ne_2026_def)), unique(sub("2025$","", ne_2025_def)))

# per-defense allowed profile to high-usage backs (regular season)
def_profile_2025 <- rush_stats_high %>%
  filter(season == 2025, week <= 18) %>%
  group_by(def_ssn) %>%
  summarise(g = n(), across(all_of(rush_target_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# abbrev sanity check (catch any KC/GB/JAX-style mismatch before trusting)
missing_2026 <- setdiff(paste0(only_2026,"2025"), def_profile_2025$def_ssn)
if (length(missing_2026) > 0) message("unmatched 2026 def_ssn: ", paste(missing_2026, collapse = ", "))

# diff-only: dropped (2025) vs new (2026), shared 8 excluded
cmp_diff <- def_profile_2025 %>%
  filter(def_ssn %in% c(paste0(only_2025,"2025"), paste0(only_2026,"2025"))) %>%
  mutate(grp = if_else(sub("2025$","", def_ssn) %in% only_2026, "new_2026", "dropped_2025")) %>%
  select(grp, def_ssn, g, all_of(rush_target_cols)) %>%
  arrange(grp, def_ssn)

cmp_diff_median <- cmp_diff %>%
  group_by(grp) %>%
  summarise(n_teams = n(), across(all_of(rush_target_cols), ~ median(.x, na.rm = TRUE)), .groups = "drop")

print(cmp_diff, n = Inf)
print(cmp_diff_median)

# teardown
rm(rush_target_cols, ne_2025_def, ne_2026_def, only_2025, only_2026,
   def_profile_2025, missing_2026, cmp_diff, cmp_diff_median)



#####
## PASS BLOCKING
#####


pressure_cols <- c("pbp_pressure","part_pressure_before","part_pressure_after",
                   "pressure_rate","sack_rate","pbp_sack_rate","part_sack_rate",
                   "less_pressure_rate","more_pressure_rate",
                   "npa_pressure_rate","pa_pressure_rate")

pressure_rank_cols <- c("pbp_pressure_rank","part_pressure_before_rank","part_pressure_after_rank",
                        "pressure_rate_rank","sack_rate_rank","pbp_sack_rate_rank","part_sack_rate_rank",
                        "less_pressure_rate_rank","more_pressure_rate_rank",
                        "npa_pressure_rate_rank","pa_pressure_rate_rank")

# NE 2025 opponents; distinct week+def to avoid double-count on 2-QB games
ne_2025_def <- qb_stats_df_final %>%
  filter(posteam == "NE", season == 2025) %>%
  distinct(week, def_ssn) %>% pull(def_ssn)

# 2026 opps from ESPN schedule -> proxied to 2025 D. both NY = NYJ, LA = LAC, SEA = 17th
ne_2026_def <- paste0(c("SEA","PIT","JAX","BUF","LV","NYJ","CHI","MIA","GB","DET",
                        "LAC","BUF","MIN","KC","NYJ","DEN","MIA"), "2025")

only_2025 <- setdiff(unique(sub("2025$","", ne_2025_def)), unique(sub("2025$","", ne_2026_def)))
only_2026 <- setdiff(unique(sub("2025$","", ne_2026_def)), unique(sub("2025$","", ne_2025_def)))

# per-defense pressure/sack profile
def_profile_2025 <- qb_stats_df_final %>%
  filter(season == 2025) %>%
  group_by(def_ssn) %>%
  summarise(g = n(),
            across(all_of(c(pressure_cols, pressure_rank_cols)), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

missing_2026 <- setdiff(paste0(only_2026,"2025"), def_profile_2025$def_ssn)
if (length(missing_2026) > 0) message("unmatched 2026 def_ssn: ", paste(missing_2026, collapse = ", "))

# diff-only: dropped (2025) vs new (2026), shared 8 excluded
cmp_diff <- def_profile_2025 %>%
  filter(def_ssn %in% c(paste0(only_2025,"2025"), paste0(only_2026,"2025"))) %>%
  mutate(grp = if_else(sub("2025$","", def_ssn) %in% only_2026, "new_2026", "dropped_2025")) %>%
  select(grp, def_ssn, g, all_of(pressure_cols), all_of(pressure_rank_cols)) %>%
  arrange(grp, def_ssn)

cmp_diff_median <- cmp_diff %>%
  group_by(grp) %>%
  summarise(n_teams = n(),
            across(all_of(c(pressure_cols, pressure_rank_cols)), ~ median(.x, na.rm = TRUE)),
            .groups = "drop")

print(cmp_diff, n = Inf)
print(cmp_diff_median)

# teardown
rm(pressure_cols, pressure_rank_cols, ne_2025_def, ne_2026_def, only_2025, only_2026,
   def_profile_2025, missing_2026, cmp_diff, cmp_diff_median)
