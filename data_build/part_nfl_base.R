
conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::arrange, dplyr::summarise, dplyr::mutate, dplyr::count)

part_nfl <- load_participation(seasons = T, include_pbp = T)

part_nfl$part_ind = 1

'%ni%' <- Negate('%in%')

options(scipen = 999)

part_nfl <- part_nfl %>% mutate(mod_ydstogo = ifelse(goal_to_go == 1, yardline_100, ydstogo))
part_nfl <- part_nfl %>% mutate(lead_timeout = lag(timeout_team, 1), last_timeout_ind = case_when(lead_timeout == posteam ~ 1, is.na(lead_timeout) ~ 0, (lead_timeout != posteam & !is.na(lead_timeout)) ~ -1))
part_nfl$td_side = ifelse((!is.na(part_nfl$td_team) & part_nfl$posteam == part_nfl$td_team), 1, 0)
part_nfl <- part_nfl %>% mutate(posteam_ind = ifelse(home_team == posteam, 1, 0))

part_nfl$week <- ifelse(part_nfl$week == 18 & part_nfl$season <= 2020, 28, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 19 & part_nfl$season <= 2020, 29, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 19 & part_nfl$season > 2020, 28, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 20 & part_nfl$season <= 2020, 30, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 20 & part_nfl$season > 2020, 29, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 21 & part_nfl$season <= 2020, 32, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 21 & part_nfl$season > 2020, 30, part_nfl$week)
part_nfl$week <- ifelse(part_nfl$week == 22, 32, part_nfl$week)


part_nfl$posteam[which(part_nfl$posteam == "ARI")] = "ARZ"
part_nfl$posteam[which(part_nfl$posteam == "BAL")] = "BLT"
part_nfl$posteam[which(part_nfl$posteam == "CLE")] = "CLV"
part_nfl$posteam[which(part_nfl$posteam == "HOU")] = "HST"
part_nfl$posteam[which(part_nfl$posteam == "LAC" & part_nfl$season == 2016)] = "SD"
part_nfl$posteam[which(part_nfl$posteam == "LV" & part_nfl$season <= 2019)] = "OAK"

part_nfl$defteam[which(part_nfl$defteam == "ARI")] = "ARZ"
part_nfl$defteam[which(part_nfl$posteam == "BAL")] = "BLT"
part_nfl$defteam[which(part_nfl$defteam == "CLE")] = "CLV"
part_nfl$defteam[which(part_nfl$defteam == "HOU")] = "HST"
part_nfl$defteam[which(part_nfl$defteam == "LAC" & part_nfl$season == 2016)] = "SD"
part_nfl$defteam[which(part_nfl$defteam == "LV" & part_nfl$season <= 2019)] = "OAK"

part_nfl$td_team[which(part_nfl$td_team == "ARI")] = "ARZ"
part_nfl$td_team[which(part_nfl$td_team == "BAL")] = "BLT"
part_nfl$td_team[which(part_nfl$td_team == "CLE")] = "CLV"
part_nfl$td_team[which(part_nfl$td_team == "HOU")] = "HST"
part_nfl$td_team[which(part_nfl$td_team == "LAC" & part_nfl$season == 2016)] = "SD"
part_nfl$td_team[which(part_nfl$td_team == "LV" & part_nfl$season <= 2019)] = "OAK"

part_nfl$possession_team[which(part_nfl$possession_team == "ARI")] = "ARZ"
part_nfl$possession_team[which(part_nfl$possession_team == "BAL")] = "BLT"
part_nfl$possession_team[which(part_nfl$possession_team == "CLE")] = "CLV"
part_nfl$possession_team[which(part_nfl$possession_team == "HOU")] = "HST"
part_nfl$possession_team[which(part_nfl$possession_team == "LAC" & part_nfl$season == 2016)] = "SD"
part_nfl$possession_team[which(part_nfl$possession_team == "LV" & part_nfl$season <= 2019)] = "OAK"


part_nfl$air_yards[which(part_nfl$air_yards <= -54)] = 0

part_nfl$down_one_ind = ifelse(part_nfl$down == 1, 1, 0)
part_nfl$down_two_ind = ifelse(part_nfl$down == 2, 1, 0)
part_nfl$down_three_ind = ifelse(part_nfl$down == 3, 1, 0)
part_nfl$down_four_ind = ifelse(part_nfl$down == 4, 1, 0)

part_nfl$distance_to_sticks = part_nfl$mod_ydstogo - part_nfl$air_yards

part_nfl$season_type <- ifelse(part_nfl$season_type == "REG", 0, 1)
part_nfl$roof <- ifelse(part_nfl$roof %in% c('dome', 'closed'), 1, 0)
part_nfl$surface <- ifelse(part_nfl$surface == "grass", 0, 1)

part_nfl$pass_length <- case_when(is.na(part_nfl$pass_length) ~ 0, part_nfl$pass_length == "short" ~ 1, part_nfl$pass_length == "deep" ~ 2)
part_nfl$pass_location <- case_when(is.na(part_nfl$pass_location) ~ 0, part_nfl$pass_location == "middle" ~ 1, part_nfl$pass_location %in% c('left', 'right') ~ 2)

part_nfl$middle_ind = ifelse(part_nfl$pass_location == 1, 1, 0)
part_nfl$outside_ind = ifelse(part_nfl$pass_location == 2, 1, 0)

part_nfl$pressure_ind = case_when(part_nfl$was_pressure == T ~ 1, part_nfl$was_pressure == F ~ 0, is.na(part_nfl$was_pressure) ~ NA)


extract_players <- function(data, pattern) {
  numbers <- str_extract(data, paste0("(\\d+)\\s*\\b", pattern, "\\b"))
  numbers <- ifelse(is.na(numbers), 0, as.integer(str_extract(numbers, "\\d+")))
  return(numbers)
}

# Step 1: Create era and play type indicators
part_nfl <- part_nfl %>%
  mutate(
    era = ifelse(season <= 2022, "pre2023", "post2023"),
    is_st_play = as.integer(
      str_detect(desc, regex("punt|field goal", ignore_case = TRUE)) |
        str_detect(offense_personnel, "\\bLS\\b|\\bP\\b|\\bK\\b")
    )
  )

# Step 2: Extract ALL base positions
part_nfl <- part_nfl %>%
  mutate(
    n_qb_listed = extract_players(offense_personnel, "QB"),
    
    # OL components
    n_c = extract_players(offense_personnel, "C"),
    n_g = extract_players(offense_personnel, "G"),
    n_t = extract_players(offense_personnel, "T"),
    n_ol_explicit = extract_players(offense_personnel, "OL"),
    
    # Skill positions
    n_rb_explicit = extract_players(offense_personnel, "RB"),
    n_hb_explicit = extract_players(offense_personnel, "HB"),
    n_fb_explicit = extract_players(offense_personnel, "FB"),
    n_te_explicit = extract_players(offense_personnel, "TE"),
    n_wr_explicit = extract_players(offense_personnel, "WR"),
    
    # Defensive positions
    n_cb = extract_players(offense_personnel, "CB"),
    n_db = extract_players(offense_personnel, "DB"),
    n_dt = extract_players(offense_personnel, "DT"),
    n_de = extract_players(offense_personnel, "DE"),
    n_ed = extract_players(offense_personnel, "ED"),
    n_fs = extract_players(offense_personnel, "FS"),
    n_ss = extract_players(offense_personnel, "SS"),
    
    # Plain S - only if FS/SS/LS not present
    n_s_plain = ifelse(!str_detect(offense_personnel, "FS|SS|LS") & 
                         str_detect(offense_personnel, "\\bS\\b"),
                       extract_players(offense_personnel, "S"), 0),
    
    # All LB variants
    n_lb_total = ifelse(str_detect(offense_personnel, "\\bLB\\b") & 
                          !str_detect(offense_personnel, "ILB|OLB|MLB"), 
                        extract_players(offense_personnel, "LB"), 0) +
      ifelse(str_detect(offense_personnel, "ILB"), 
             extract_players(offense_personnel, "ILB"), 0) +
      ifelse(str_detect(offense_personnel, "OLB"), 
             extract_players(offense_personnel, "OLB"), 0) +
      ifelse(str_detect(offense_personnel, "MLB"), 
             extract_players(offense_personnel, "MLB"), 0),
    
    # ST
    n_ls = extract_players(offense_personnel, "LS"),
    n_k = extract_players(offense_personnel, "K"),
    n_p = extract_players(offense_personnel, "P")
  )

# ===== PRE-2023, REGULAR PLAYS =====
# RB = RB/HB/FB + all defensive players (DT/DE/ED/FS/SS/S/LB)
# WR = WR + CB/DB
# TE = TE + extra QBs (if 2+ QBs, extras become TE)
# OL = 11 - 1 QB - (RB + WR + TE)
part_nfl <- part_nfl %>%
  mutate(
    n_rb_pre2023_reg = n_rb_explicit + n_hb_explicit + n_fb_explicit +
      n_dt + n_de + n_ed + n_fs + n_ss + n_s_plain + n_lb_total,
    
    n_wr_pre2023_reg = n_wr_explicit + n_cb + n_db,
    
    n_te_pre2023_reg = n_te_explicit + pmax(0, n_qb_listed - 1),
    
    n_ol_pre2023_reg = 11 - 1 - n_te_pre2023_reg - n_rb_pre2023_reg - n_wr_pre2023_reg
  )

# ===== PRE-2023, ST PLAYS =====
# OL = LS + DT/DE/ED + LB (all LB variants go to OL on ST plays)
# RB = RB/HB/FB + S (FS/SS/S) + extra QBs
# WR = WR + CB/DB
# TE = TE
# ST = K + P
# OL = 11 - (RB + WR + TE + ST)
part_nfl <- part_nfl %>%
  mutate(
    n_rb_pre2023_st = n_rb_explicit + n_hb_explicit + n_fb_explicit +
      n_fs + n_ss + n_s_plain + pmax(0, n_qb_listed - 1),
    
    n_wr_pre2023_st = n_wr_explicit + n_cb + n_db,
    
    n_te_pre2023_st = n_te_explicit,
    
    n_st_pre2023 = n_k + n_p,
    
    # OL gets everything else: LS, DL, LB, and whatever's left
    n_ol_pre2023_st = 11 - n_rb_pre2023_st - n_wr_pre2023_st - n_te_pre2023_st - n_st_pre2023
  )

# ===== POST-2023, REGULAR PLAYS =====
# HB = RB/HB/FB + defensive players (DT/DE/FS/SS/S/LB)
# WR = WR + CB/DB
# TE = TE + extra QBs
# OL = C + G + T + LS + (11 - 1 QB - HB - WR - TE - C - G - T - LS)
part_nfl <- part_nfl %>%
  mutate(
    n_hb_post2023_reg = n_rb_explicit + n_hb_explicit + n_fb_explicit +
      n_dt + n_de + n_fs + n_ss + n_s_plain + n_lb_total,
    
    n_wr_post2023_reg = n_wr_explicit + n_cb + n_db,
    
    n_te_post2023_reg = n_te_explicit + pmax(0, n_qb_listed - 1),
    
    n_ol_post2023_reg = 11 - 1 - n_hb_post2023_reg - n_te_post2023_reg - n_wr_post2023_reg
  )

# ===== POST-2023, ST PLAYS =====
# OL = C/G/T + LS + DT/DE/ED + LB (all LB variants)
# RB = RB/HB/FB + S (FS/SS/S) + extra QBs
# WR = WR + CB/DB
# TE = TE
# ST = K + P
# OL = 11 - (RB + WR + TE + ST)
part_nfl <- part_nfl %>%
  mutate(
    n_rb_post2023_st = n_rb_explicit + n_hb_explicit + n_fb_explicit +
      n_fs + n_ss + n_s_plain + pmax(0, n_qb_listed - 1),
    
    n_wr_post2023_st = n_wr_explicit + n_cb + n_db,
    
    n_te_post2023_st = n_te_explicit,
    
    n_st_post2023 = n_k + n_p,
    
    n_ol_post2023_st = 11 - n_rb_post2023_st - n_wr_post2023_st - n_te_post2023_st - n_st_post2023
  )

# ===== FINAL ASSIGNMENT =====
part_nfl <- part_nfl %>%
  mutate(
    n_rb = case_when(
      era == "pre2023" & is_st_play == 0 ~ n_rb_pre2023_reg,
      era == "pre2023" & is_st_play == 1 ~ n_rb_pre2023_st,
      era == "post2023" & is_st_play == 1 ~ n_rb_post2023_st,
      TRUE ~ 0
    ),
    
    n_hb = case_when(
      era == "post2023" & is_st_play == 0 ~ n_hb_post2023_reg,
      TRUE ~ 0
    ),
    
    n_fb = 0,
    
    n_te = case_when(
      era == "pre2023" & is_st_play == 0 ~ n_te_pre2023_reg,
      era == "pre2023" & is_st_play == 1 ~ n_te_pre2023_st,
      era == "post2023" & is_st_play == 0 ~ n_te_post2023_reg,
      era == "post2023" & is_st_play == 1 ~ n_te_post2023_st,
      TRUE ~ 0
    ),
    
    n_wr = case_when(
      era == "pre2023" & is_st_play == 0 ~ n_wr_pre2023_reg,
      era == "pre2023" & is_st_play == 1 ~ n_wr_pre2023_st,
      era == "post2023" & is_st_play == 0 ~ n_wr_post2023_reg,
      era == "post2023" & is_st_play == 1 ~ n_wr_post2023_st,
      TRUE ~ 0
    ),
    
    n_ol = case_when(
      era == "pre2023" & is_st_play == 0 ~ n_ol_pre2023_reg,
      era == "pre2023" & is_st_play == 1 ~ n_ol_pre2023_st,
      era == "post2023" & is_st_play == 0 ~ n_ol_post2023_reg,
      era == "post2023" & is_st_play == 1 ~ n_ol_post2023_st,
      TRUE ~ 0
    ),
    
    n_st = case_when(
      is_st_play == 1 & era == "pre2023" ~ n_st_pre2023,
      is_st_play == 1 & era == "post2023" ~ n_st_post2023,
      TRUE ~ 0
    ),
    
    # Final verification calc
    n_offense_calc = n_ol + n_te + n_wr + n_rb + n_hb + 
      ifelse(is_st_play == 1, 0, 1) + n_st
  ) %>%
  # Clean up intermediate columns
  select(-starts_with("n_rb_pre"), -starts_with("n_rb_post"),
         -starts_with("n_wr_pre"), -starts_with("n_wr_post"),
         -starts_with("n_te_pre"), -starts_with("n_te_post"),
         -starts_with("n_ol_pre"), -starts_with("n_ol_post"),
         -starts_with("n_hb_post"), -starts_with("n_st_pre"), 
         -starts_with("n_st_post"),
         -n_c, -n_g, -n_t, -n_ol_explicit, 
         -n_rb_explicit, -n_hb_explicit, -n_fb_explicit,
         -n_te_explicit, -n_wr_explicit, 
         -n_cb, -n_db, -n_dt, -n_de, -n_ed,
         -n_fs, -n_ss, -n_s_plain, -n_lb_total, 
         -n_ls, -n_k, -n_p, -n_qb_listed)

# Sanity check
off_position_check <- sqldf("
  SELECT era, is_st_play, offense_personnel, 
         n_ol, n_hb, n_rb, n_te, n_wr, n_st, n_offense_calc, 
         COUNT(*) as n
  FROM part_nfl
  GROUP BY era, is_st_play, offense_personnel, 
           n_ol, n_hb, n_rb, n_te, n_wr, n_st, n_offense_calc
  ORDER BY COUNT(*) DESC
")
# ALL GOOD, THERE ARE SOME NEGATIVE ONES IN N_OL BUT HONESTLY FUCK IT. 



# DEFENSE PERSONNEL PROCESSING

# Step 1: Extract ALL defensive base positions
part_nfl <- part_nfl %>%
  mutate(
    # DL positions
    n_dl_explicit = extract_players(defense_personnel, "DL"),
    n_de = extract_players(defense_personnel, "DE"),
    n_dt = extract_players(defense_personnel, "DT"),
    n_nt = extract_players(defense_personnel, "NT"),
    
    # LB positions - extract each separately, THEN sum
    n_ilb = extract_players(defense_personnel, "ILB"),
    n_olb = extract_players(defense_personnel, "OLB"),
    n_mlb = extract_players(defense_personnel, "MLB"),
    # Plain LB - ALWAYS extract it
    n_lb_plain = extract_players(defense_personnel, "LB"),
    
    # DB positions
    n_cb_def = extract_players(defense_personnel, "CB"),
    n_fs_def = extract_players(defense_personnel, "FS"),
    n_ss_def = extract_players(defense_personnel, "SS"),
    n_db_explicit = extract_players(defense_personnel, "DB"),
    
    # Offensive players on defense
    n_te_on_def = extract_players(defense_personnel, "TE"),
    n_wr_on_def = extract_players(defense_personnel, "WR"),
    n_rb_on_def = extract_players(defense_personnel, "RB"),
    n_hb_on_def = extract_players(defense_personnel, "HB"),
    n_fb_on_def = extract_players(defense_personnel, "FB"),
    n_qb_on_def = extract_players(defense_personnel, "QB"),
    
    # OL on defense (for post-2023)
    n_c_def = extract_players(defense_personnel, "C"),
    n_g_def = extract_players(defense_personnel, "G"),
    n_t_def = extract_players(defense_personnel, "T"),
    n_ol_on_def = extract_players(defense_personnel, "OL"),
    
    # ST
    n_k_def = extract_players(defense_personnel, "K"),
    n_p_def = extract_players(defense_personnel, "P")
  )

# ===== PRE-2023, REGULAR DEFENSE =====
part_nfl <- part_nfl %>%
  mutate(
    n_dl_pre2023_reg = n_dl_explicit + n_ol_on_def,
    
    n_lb_pre2023_reg = n_lb_plain + n_ilb + n_olb + n_mlb,
    
    n_db_pre2023_reg = n_db_explicit + n_cb_def + n_fs_def + n_ss_def +
      n_te_on_def + n_rb_on_def + n_hb_on_def + 
      n_wr_on_def + n_fb_on_def + n_qb_on_def
  )

# ===== PRE-2023, ST DEFENSE =====
part_nfl <- part_nfl %>%
  mutate(
    n_dl_pre2023_st = n_dl_explicit + n_ol_on_def,
    
    n_lb_pre2023_st = n_lb_plain + n_ilb + n_olb + n_mlb +
      n_te_on_def + n_fb_on_def,
    
    n_db_pre2023_st = n_db_explicit + n_cb_def + n_fs_def + n_ss_def +
      n_wr_on_def + n_rb_on_def + n_hb_on_def + n_qb_on_def
  )

# ===== POST-2023, REGULAR DEFENSE =====
part_nfl <- part_nfl %>%
  mutate(
    n_dl_post2023_reg = n_de + n_dt + n_nt + 
      n_c_def + n_g_def + n_t_def + n_ol_on_def,
    
    n_lb_post2023_reg = n_lb_plain + n_ilb + n_olb + n_mlb,
    
    n_db_post2023_reg = n_cb_def + n_ss_def + n_fs_def + n_db_explicit +
      n_wr_on_def + n_rb_on_def + n_qb_on_def + 
      n_te_on_def + n_fb_on_def + n_hb_on_def
  )

# ===== POST-2023, ST DEFENSE =====
part_nfl <- part_nfl %>%
  mutate(
    n_dl_post2023_st = n_de + n_dt + n_nt + 
      n_c_def + n_g_def + n_t_def + n_ol_on_def,
    
    n_lb_post2023_st = n_lb_plain + n_ilb + n_olb + n_mlb +
      n_te_on_def + n_fb_on_def,
    
    n_db_post2023_st = n_cb_def + n_ss_def + n_fs_def + n_db_explicit +
      n_wr_on_def + n_rb_on_def + n_qb_on_def + n_hb_on_def,
    
    n_st_def_post2023 = n_k_def + n_p_def
  )

# ===== FINAL DEFENSE ASSIGNMENT =====
part_nfl <- part_nfl %>%
  mutate(
    n_dl = case_when(
      era == "pre2023" & is_st_play == 0 ~ n_dl_pre2023_reg,
      era == "pre2023" & is_st_play == 1 ~ n_dl_pre2023_st,
      era == "post2023" & is_st_play == 0 ~ n_dl_post2023_reg,
      era == "post2023" & is_st_play == 1 ~ n_dl_post2023_st,
      TRUE ~ 0
    ),
    
    n_lb = case_when(
      era == "pre2023" & is_st_play == 0 ~ n_lb_pre2023_reg,
      era == "pre2023" & is_st_play == 1 ~ n_lb_pre2023_st,
      era == "post2023" & is_st_play == 0 ~ n_lb_post2023_reg,
      era == "post2023" & is_st_play == 1 ~ n_lb_post2023_st,
      TRUE ~ 0
    ),
    
    n_db = case_when(
      era == "pre2023" & is_st_play == 0 ~ n_db_pre2023_reg,
      era == "pre2023" & is_st_play == 1 ~ n_db_pre2023_st,
      era == "post2023" & is_st_play == 0 ~ n_db_post2023_reg,
      era == "post2023" & is_st_play == 1 ~ n_db_post2023_st,
      TRUE ~ 0
    ),
    
    n_st_def = case_when(
      era == "post2023" & is_st_play == 1 ~ n_st_def_post2023,
      TRUE ~ 0
    ),
    
    n_defense_calc = n_dl + n_lb + n_db + n_st_def
  ) %>%
  select(-starts_with("n_dl_pre"), -starts_with("n_dl_post"),
         -starts_with("n_lb_pre"), -starts_with("n_lb_post"),
         -starts_with("n_db_pre"), -starts_with("n_db_post"),
         -starts_with("n_st_def_post"),
         -n_dl_explicit, -n_de, -n_dt, -n_nt,
         -n_lb_plain, -n_ilb, -n_olb, -n_mlb,
         -n_cb_def, -n_fs_def, -n_ss_def, -n_db_explicit,
         -n_te_on_def, -n_wr_on_def, -n_rb_on_def, -n_hb_on_def, 
         -n_fb_on_def, -n_qb_on_def,
         -n_c_def, -n_g_def, -n_t_def, -n_ol_on_def,
         -n_k_def, -n_p_def)

# Sanity check DEFENSE
def_position_fuckups <- sqldf("
  SELECT era, is_st_play, defense_personnel, 
         n_dl, n_lb, n_db, n_st_def, n_defense_calc, 
         COUNT(*) as n
  FROM part_nfl
  WHERE n_defense_calc != 11
    AND defense_personnel IS NOT NULL
    AND defense_personnel != ''
  GROUP BY era, is_st_play, defense_personnel, 
           n_dl, n_lb, n_db, n_st_def, n_defense_calc
  ORDER BY COUNT(*) DESC
")


part_nfl$n_st_def[which(part_nfl$n_st_def < 0)] = 0



####
####

# Step 1: Split data for validation
set.seed(42)
train_data <- part_nfl %>% filter(!is.na(defenders_in_box), play_type %in% c("run", "pass"))
train_idx <- createDataPartition(train_data$defenders_in_box, p = 0.8, list = FALSE)

train_set <- train_data[train_idx, ]
test_set <- train_data[-train_idx, ]

# Step 2: Variable selection on TRAINING set only
defenders_in_box_full <- lm(
  defenders_in_box ~ season_type + yardline_100 + half_seconds_remaining + 
    down + down_one_ind + down_two_ind + mod_ydstogo + 
    shotgun + no_huddle + last_timeout_ind + score_differential + 
    roof + surface + posteam_ind + n_ol + n_te + n_wr + 
    n_rb + n_hb + n_dl + n_lb + n_db + 0, 
  data = train_set
)

# Forward stepwise selection with p < 0.1
ols_def_in_box <- ols_step_forward_p(defenders_in_box_full, penter = 0.1, details = FALSE)

# Extract selected variables
selected_vars <- names(coef(ols_def_in_box$model))
cat("Selected variables:", paste(selected_vars, collapse = ", "), "\n")

# Step 3: Refit with only selected variables
formula_str <- paste("defenders_in_box ~", paste(selected_vars[2:length(selected_vars)], collapse = " + "), "+ 0")
defenders_in_box_selected <- lm(as.formula(formula_str), data = train_set)

# Step 4: Validate on test set
test_pred <- predict(defenders_in_box_selected, newdata = test_set)
mae <- mean(abs(test_pred - test_set$defenders_in_box))
rmse <- sqrt(mean((test_pred - test_set$defenders_in_box)^2))

cat(sprintf("Validation MAE: %.3f defenders\nValidation RMSE: %.3f defenders\n", mae, rmse))
summary(defenders_in_box_selected)

# Step 5: Refit on ALL non-NA data with selected variables
defenders_in_box_final <- lm(as.formula(formula_str), data = train_data)

# Step 6: Impute NAs
na_idx <- which(is.na(part_nfl$defenders_in_box))

# Create flag BEFORE imputing
part_nfl$dib_imputed <- as.integer(is.na(part_nfl$defenders_in_box))

# Now impute
part_nfl$defenders_in_box[na_idx] <- predict(defenders_in_box_final, 
                                             newdata = part_nfl[na_idx, ])
part_nfl$defenders_in_box[which(part_nfl$defenders_in_box < 0)] <- 0


part_nfl <- part_nfl %>%
  mutate(
    i_form_ind = as.integer(offense_formation == "I_FORM"),
    shotgun_ind = as.integer(offense_formation == "SHOTGUN"),
    singleback_ind = as.integer(offense_formation == "SINGLEBACK"),
    pistol_ind = as.integer(offense_formation == "PISTOL"),
    jumbo_ind = as.integer(offense_formation == "JUMBO"),
    empty_ind = as.integer(offense_formation == "EMPTY"),
    wildcat_ind = as.integer(offense_formation == "WILDCAT"),
    st_ind = as.integer(offense_formation %in% c("PUNT", "FIELD_GOAL"))
  )

part_nfl <- part_nfl %>% 
  mutate(
    flat_route_ind = as.integer(route == "FLAT"),
    angle_route_ind = as.integer(route == "ANGLE"),
    screen_route_ind = as.integer(route == "SCREEN"),
    hitch_route_ind = as.integer(route == "HITCH"),
    go_route_ind = as.integer(route == "GO"),
    out_route_ind = as.integer(route == "OUT"),
    slant_route_ind = as.integer(route == "SLANT"),
    cross_route_ind = as.integer(route == "CROSS"),
    post_route_ind = as.integer(route == "POST"),
    corner_route_ind = as.integer(route == "CORNER"),
    in_route_ind = as.integer(route == "IN"),
    wheel_route_ind = as.integer(route == "WHEEL"),
    cover3_ind = as.integer(defense_coverage_type == "COVER_3"),
    cover2_ind = as.integer(defense_coverage_type == "COVER_2"),
    cover0_ind = as.integer(defense_coverage_type == "COVER_0"),   
    cover1_ind = as.integer(defense_coverage_type == "COVER_1"),
    cover4_ind = as.integer(defense_coverage_type == "COVER_4"),
    twoman_ind = as.integer(defense_coverage_type == "2_MAN"),
    cover6_ind = as.integer(defense_coverage_type == "COVER_6"),  
    prevent_cov_ind = as.integer(defense_coverage_type == "PREVENT"),
    man_ind = as.integer(defense_man_zone_type == "MAN_COVERAGE"),
    zone_ind = as.integer(defense_man_zone_type == "ZONE_COVERAGE")
  )


part_nfl <- part_nfl %>% mutate(defense_formation = case_when(
  n_db >= 7 ~ "Prevent",
  (n_db == 6 | defense_personnel == "1 DL, 4 LB, 5 DB") ~ "Quarter",
  (n_dl >= 6 | defense_personnel %in% c("5 DL, 4 LB, 2 DB", "4 DL, 5 LB, 2 DB", "5 DL, 5 LB, 1 DB", "3 DL, 5 LB, 2 DB", "4 DL, 5 LB, 1 DB", "4 DL, 5 LB, 1 DB, 1 OL", "3 DL, 5 LB, 2 DB, 1 OL")) ~ "Goal_Line",
  (defense_personnel %in% c("1 DL, 5 LB, 5 DB", "2 DL, 4 LB, 5 DB", "0 DL, 6 LB, 5 DB", "2 DL, 3 LB, 5 DB", "1 DL, 6 LB, 4 DB", "1 DL, 5 LB, 4 DB", "0 DL, 5 LB, 5 DB, 1 RB") | (n_dl == 2 & n_db == 5)) ~ "Nickel_Lite",
  (defense_personnel %in% c("4 DL, 2 LB, 5 DB", "4 DL, 1 LB, 5 DB", "4 DL, 2 LB, 3 DB") | (n_dl == 4 & n_lb == 2)) ~ "Nickel",
  defense_personnel %in% c("3 DL, 3 LB, 5 DB", "3 DL, 3 LB, 4 DB", "5 DL, 1 LB, 5 DB", "3 DL, 2 LB, 5 DB", "4 DL, 3 LB, 5 DB", "5 DL, 1 LB, 4 DB", "2 DL, 3 LB, 4 DB", "1 DL, 3 LB, 5 DB, 2 RB", "3 DL, 3 LB, 4 DB, 1 WR", "2 DL, 3 LB, 5 DB, 1 RB", "4 DL, 1 LB, 5 DB, 1 TE", "2 DL, 3 LB, 5 DB, 1 TE") ~ "335",
  (defense_personnel %in% c("4 DL, 3 LB, 4 DB", "2 DL, 4 LB, 4 DB", "4 DL, 4 LB, 4 DB", "5 DL, 3 LB, 4 DB") | (n_dl == 4 & n_lb == 3)) ~ "43",
  (defense_personnel %in% c("3 DL, 5 LB, 3 DB", "5 DL, 2 LB, 4 DB", "3 DL, 3 LB, 4 DB", "3 DL, 4 LB, 3 DB", "5 DL, 2 LB, 3 DB", "3 DL, 6 LB, 2 DB", "3 DL, 4 LB, 5 DB", "3 DL, 5 LB, 2 DB", "4 DL, 2 LB, 3 DB, 1 OL", "4 DL, 2 LB, 4 DB, 1 OL") | (n_dl == 3 & n_lb == 4)) ~ "34",
  defense_personnel %in% c("4 DL, 4 LB, 3 DB", "5 DL, 3 LB, 3 DB", "4 DL, 6 LB, 1 DB", "4 DL, 4 LB, 2 DB", "5 DL, 3 LB, 2 DB", "3 DL, 4 LB, 3 DB, 1 OL", "3 DL, 4 LB, 3 DB, 1 RB", "4 DL, 3 LB, 3 DB, 1 OL") ~ "46",
  defense_personnel %in% c("2 DL, 5 LB, 4 DB", "2 DL, 6 LB, 3 DB") ~ "Heavy_2Front",
  offense_formation == "PUNT" ~ "Punt"))

part_nfl <- part_nfl %>%
  mutate(
    d43_ind = as.integer(defense_formation == "43"),
    d34_ind = as.integer(defense_formation == "34"),
    nickel_lite_ind = as.integer(defense_formation == "Nickel_Lite"),
    d335_ind = as.integer(defense_formation == "335"),
    quarter_ind = as.integer(defense_formation == "Quarter"),
    d46_ind = as.integer(defense_formation == "46"),
    nickel_ind = as.integer(defense_formation == "Nickel"),
    goal_line_ind = as.integer(defense_formation == "Goal_Line"),
    heavy_2front_ind = as.integer(defense_formation == "Heavy_2Front"),
    prevent_ind = as.integer(defense_formation == "Prevent"),
    punt_ind_def = as.integer(defense_formation == "Punt")
  )


part_nfl$n_st_def[which(part_nfl$n_st_def < 0)] = 0

part_nfl <- part_nfl %>% 
  mutate(
    outside_run_ind = as.integer(run_location %in% c("left", "right")),
    middle_run_ind = as.integer(run_location == "middle"),
    end_gap_ind = as.integer(run_gap == "end"),
    guard_gap_ind = as.integer(run_gap == "guard"),
    tackle_gap_ind = as.integer(run_gap == "tackle"),
    middle_gap_ind = as.integer(is.na(run_gap) & play_type == "run"),
    cover3_ind = as.integer(defense_coverage_type == "COVER_3"),
    cover2_ind = as.integer(defense_coverage_type == "COVER_2"),
    cover0_ind = as.integer(defense_coverage_type == "COVER_0"),   
    cover1_ind = as.integer(defense_coverage_type == "COVER_1"),
    cover4_ind = as.integer(defense_coverage_type == "COVER_4"),
    twoman_ind = as.integer(defense_coverage_type == "2_MAN"),
    cover6_ind = as.integer(defense_coverage_type == "COVER_6"),  
    prevent_cov_ind = as.integer(defense_coverage_type == "PREVENT"),
    man_ind = as.integer(defense_man_zone_type == "MAN_COVERAGE"),
    zone_ind = as.integer(defense_man_zone_type == "ZONE_COVERAGE")
  )


rm(test_set)
rm(train_data)
rm(train_idx)
rm(train_set)
rm(def_position_fuckups)
rm(defenders_in_box_final)
rm(defenders_in_box_full)
rm(defenders_in_box_selected)
rm(off_position_check)
rm(ols_def_in_box)


part_nfl$guard_gap_ind[which(is.na(part_nfl$guard_gap_ind))] <- 0
part_nfl$tackle_gap_ind[which(is.na(part_nfl$tackle_gap_ind))] <- 0
part_nfl$end_gap_ind[which(is.na(part_nfl$end_gap_ind))] <- 0
part_nfl$middle_gap_ind[which(is.na(part_nfl$middle_gap_ind))] <- 0
part_nfl$outside_run_ind[which(is.na(part_nfl$outside_run_ind))] <- 0
part_nfl$middle_run_ind[which(is.na(part_nfl$middle_run_ind))] <- 0

part_nfl <- part_nfl %>%
  mutate(
    rain_ind = ifelse(grepl("Rain|Showers|Rainy|Raining", weather, ignore.case = TRUE), 1, 0),
    snow_ind = ifelse(grepl("Snow", weather, ignore.case = TRUE), 1, 0),
    precip_ind = ifelse(rain_ind == 1 | snow_ind == 1, 1, 0)
  )

part_nfl$wind[which(is.na(part_nfl$wind))] <- 0
part_nfl$temp[which(is.na(part_nfl$temp))] <- 70

# Quick check
table(part_nfl$rain_ind, useNA = "ifany")
table(part_nfl$snow_ind, useNA = "ifany")
table(part_nfl$precip_ind, useNA = "ifany")