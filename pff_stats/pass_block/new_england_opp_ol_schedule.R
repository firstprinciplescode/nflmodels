# ============================================================
# NE 2026 — OPPONENT OL SLATE + FIGURES 1-2 — COMPLETE, SELF-CONTAINED
# run top to bottom; raw data in, plots out
# ONE FILE as of 2026-08-08: the fig2v2 panel script is merged in as
# section 11 — delete exploration/opp_ol_slate_2026_fig2v2.R locally.
#
# NEEDS IN SESSION (checked below, fails loudly with names):
#   all_pass_block_summary, all_pass_block_player_season_summary,
#   tps_pass_block_player_season_summary, gap_player_season_summary,
#   zone_player_season_summary, id_xwalk, pff_team_lookup
#   (last two: source data_build/pff_ids_validate_cross.R)
#
# Games: ALL — regular season (wk 1-18) + playoffs (PFF wk 28-32)
# Metrics plotted: tps / gap / zone grades + TPS pressure pctl
#   (pb dropped - TPS carries pass pro)
#
# tps_press ORIENTATION — verified empirically 2026-08-08, do not re-litigate:
#   cor(grade_season_pctl, pressure_season_pctl) = 0.748 on 2025 OL (hurries 0.577)
#   Creed Humphrey: grade .842 / pressure .947
#   -> higher pctl = FEWER pressures allowed = better blocker = HARDER for NE. No flip.
# Ruling 2026-08-08: pressure rides Fig 1 + the 3-season table ONLY.
#   Player panels (Fig 2 v1 + v2) stay grade-only — too many plots otherwise.
#   hurries stays out: subset of pressure, collinear clutter.
#
# Dot encoding: OPEN GREY CIRCLE = past (ghost) | SOLID NAVY = current/projected
#   red diamond = rookie prior | open navy = camp battle | navy triangle = swing
# Direction: higher pctl = better opposing OL = HARDER for NE's front
# ============================================================

library(dplyr); library(tidyr); library(ggplot2); library(stringr)

needed <- c("all_pass_block_summary", "all_pass_block_player_season_summary",
            "tps_pass_block_player_season_summary", "gap_player_season_summary",
            "zone_player_season_summary", "id_xwalk", "pff_team_lookup")
missing <- needed[!vapply(needed, exists, logical(1))]
if (length(missing)) stop("missing session objects: ", paste(missing, collapse = ", "),
                          " — load Athena tables + source pff_ids_validate_cross.R first")

ol_pos_levels  <- c("LT","LG","C","RG","RT")
opp_2026_teams <- c("SEA","PIT","JAX","BUF","LV","NYJ","CHI","MIA",
                    "GB","DET","LAC","MIN","KC","DEN")
sched_2026 <- c("SEA","PIT","JAX","BUF","LV","NYJ","CHI","MIA","GB","DET",
                "LAC","BUF","MIN","KC","NYJ","DEN","MIA")

in_season <- function(w) w <= 18 | w >= 28   # reg + playoffs, all games

# ------------------------------------------------------------
# 1. snap profiles (league-wide, all games)
# ------------------------------------------------------------

ol_2025_by_pos <- all_pass_block_summary %>%
  filter(season == 2025, in_season(week), det_position %in% ol_pos_levels) %>%
  group_by(player_id, det_position) %>%
  summarise(snaps_2025 = sum(snap_counts_pass_block, na.rm = TRUE),
            g_pos = n(), .groups = "drop")

ol_2025_snaps <- ol_2025_by_pos %>%
  group_by(player_id) %>%
  mutate(g_2025_total = sum(g_pos)) %>%
  slice_max(snaps_2025, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(g_2025 = g_pos)

# ------------------------------------------------------------
# 2. percentile currency (snap-weighted stint collapse where schema allows)
# ------------------------------------------------------------

pctl_year <- function(df, cols, yr) {
  w_col <- head(intersect(c("snaps", "snap_counts_pass_block", "n"), names(df)), 1)
  d <- df %>% filter(season == yr) %>%
    select(player_id, det_position, all_of(w_col), all_of(cols))
  out_cols <- names(cols)
  if (length(w_col) == 0) {
    message("pctl_year: no weight column found - unweighted mean across stints")
    d %>% group_by(player_id, det_position) %>%
      summarise(across(all_of(out_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  } else {
    message("pctl_year: weighting stints by '", w_col, "'")
    d %>% group_by(player_id, det_position) %>%
      summarise(across(all_of(out_cols),
                       ~ if (all(is.na(.x))) NA_real_
                       else weighted.mean(.x, w = .data[[w_col]], na.rm = TRUE)),
                .groups = "drop")
  }
}

build_pctl <- function(yr) {
  pctl_year(all_pass_block_player_season_summary,
            c(pb_grade = "grade_season_pctl", pb_press = "pressure_season_pctl",
              pb_hurr = "hurries_season_pctl"), yr) %>%
    full_join(pctl_year(tps_pass_block_player_season_summary,
                        c(tps_grade = "grade_season_pctl", tps_press = "pressure_season_pctl",
                          tps_hurr = "hurries_season_pctl"), yr),
              by = c("player_id","det_position")) %>%
    full_join(pctl_year(gap_player_season_summary,  c(gap  = "gap_season_pctl"), yr),
              by = c("player_id","det_position")) %>%
    full_join(pctl_year(zone_player_season_summary, c(zone = "zone_season_pctl"), yr),
              by = c("player_id","det_position"))
}

ol_season_pctl    <- build_pctl(2025)
ol_season_pctl_24 <- build_pctl(2024)

ol_2024_profile <- all_pass_block_summary %>%
  filter(season == 2024, in_season(week), det_position %in% ol_pos_levels) %>%
  group_by(player_id, det_position) %>%
  summarise(snaps_2024 = sum(snap_counts_pass_block, na.rm = TRUE),
            g_2024 = n(), .groups = "drop") %>%
  group_by(player_id) %>%
  slice_max(snaps_2024, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(det_position_24 = det_position) %>%
  left_join(ol_season_pctl_24, by = c("player_id", "det_position_24" = "det_position")) %>%
  rename_with(~ paste0(.x, "_24"),
              c(pb_grade, pb_press, pb_hurr, tps_grade, tps_press, tps_hurr, gap, zone))

# ------------------------------------------------------------
# 3. 2026 rosters -> PFF ids -> 24/25 blend
# ------------------------------------------------------------

blend2 <- function(x25, x24, w) case_when(
  is.na(x25) & is.na(x24) ~ NA_real_,
  is.na(x24)              ~ x25,
  is.na(x25)              ~ x24,
  TRUE                    ~ w * x25 + (1 - w) * x24
)

stopifnot(anyDuplicated(id_xwalk$gsis_id) == 0)

ol_2026 <- nflreadr::load_rosters(2026) %>%
  filter(position %in% c("T","G","C","OL","OT","OG")) %>%
  transmute(gsis_id,
            roster_name = full_name,
            team_name = dplyr::coalesce(pff_team_lookup[team], team),
            entry_year, years_exp, roster_pos = position) %>%
  filter(team_name %in% opp_2026_teams) %>%
  left_join(id_xwalk %>% filter(!is.na(gsis_id)) %>% select(player_id, gsis_id),
            by = "gsis_id") %>%
  left_join(ol_2025_snaps, by = "player_id") %>%
  left_join(ol_season_pctl, by = c("player_id","det_position")) %>%
  left_join(ol_2024_profile, by = "player_id") %>%
  filter(!(roster_name == "Wanya Morris" & team_name == "KC")) %>%
  mutate(w25 = pmin(dplyr::coalesce(g_2025_total, 0L) / 10, 1),
         pb_grade_bl   = blend2(pb_grade,  pb_grade_24,  w25),
         tps_grade_bl  = blend2(tps_grade, tps_grade_24, w25),
         tps_press_bl  = blend2(tps_press, tps_press_24, w25),
         gap_bl        = blend2(gap,       gap_24,       w25),
         zone_bl       = blend2(zone,      zone_24,      w25),
         status = case_when(
           entry_year == 2026                     ~ "rookie",
           is.na(player_id)                       ~ "no_pff_id",
           is.na(snaps_2025) & !is.na(snaps_2024) ~ "data_2024_only",
           is.na(snaps_2025)                      ~ "no_recent_ol_snaps",
           TRUE                                   ~ "has_data")) %>%
  arrange(team_name, desc(snaps_2025))

stopifnot(anyDuplicated(ol_2026[, c("team_name", "roster_name")]) == 0)

# ------------------------------------------------------------
# 4. projected starters tribble + join + audits + gates
# ------------------------------------------------------------

opp_ol_2026_starters <- tribble(
  ~team_name, ~det_position, ~roster_name,           ~conf,
  "BUF", "LT", "Dion Dawkins",    "high",
  "BUF", "LG", "Austin Corbett",  "battle",   # vs Alec Anderson; beat writers split
  "BUF", "C",  "Connor McGovern", "high",
  "BUF", "RG", "O'Cyrus Torrence","high",
  "BUF", "RT", "Spencer Brown",   "high",
  "CHI", "LT", "Ozzy Trapilo",           "battle",
  "CHI", "LG", "Joe Thuney",             "high",
  "CHI", "C",  "Garrett Bradbury",       "high",
  "CHI", "RG", "Jonah Jackson",          "high",
  "CHI", "RT", "Darnell Wright",         "high",
  "DEN", "LT", "Garett Bolles",          "high",
  "DEN", "LG", "Ben Powers",             "battle",
  "DEN", "C",  "Luke Wattenberg",        "high",
  "DEN", "RG", "Quinn Meinerz",          "high",
  "DEN", "RT", "Mike McGlinchey",        "high",
  "DET", "LT", "Penei Sewell",           "high",
  "DET", "LG", "Christian Mahogany",     "high",
  "DET", "C",  "Cade Mays",              "high",
  "DET", "RG", "Tate Ratledge",          "high",
  "DET", "RT", "Blake Miller",           "battle",
  "GB",  "LT", "Jordan Morgan",          "high",
  "GB",  "LG", "Aaron Banks",            "high",
  "GB",  "C",  "Sean Rhyan",             "high",
  "GB",  "RG", "Anthony Belton",         "battle",
  "GB",  "RT", "Zach Bako-Bewele",       "high",     # was "Zach Tom"; nflreadr 2026 roster spelling (same player, #50, gsis 00-0037817) -- 2026-09-12
  "JAX", "LT", "Cole Van Lanen",         "battle",
  "JAX", "LG", "Ezra Cleveland",         "high",
  "JAX", "C",  "Robert Hainsey",         "high",
  "JAX", "RG", "Patrick Mekari",         "high",
  "JAX", "RT", "Anton Harrison",         "high",
  "KC",  "LT", "Josh Simmons",           "high",
  "KC",  "LG", "Kingsley Suamataia",     "high",
  "KC",  "C",  "Creed Humphrey",         "high",
  "KC",  "RG", "Trey Smith",             "high",
  "KC",  "RT", "Kahlil Benson",          "battle",
  "LAC", "LT", "Rashawn Slater",         "high",
  "LAC", "LG", "Jake Slaughter",         "battle",
  "LAC", "C",  "Tyler Biadasz",          "high",
  "LAC", "RG", "Cole Strange",           "battle",
  "LAC", "RT", "Joe Alt",                "high",
  "LV",  "LT", "Kolton Miller",          "high",
  "LV",  "LG", "Spencer Burford",        "battle",
  "LV",  "C",  "Tyler Linderbaum",       "high",
  "LV",  "RG", "Jackson Powers-Johnson", "battle",
  "LV",  "RT", "DJ Glaze",               "high",
  "MIA", "LT", "Patrick Paul",           "high",
  "MIA", "LG", "Kadyn Proctor",          "high",
  "MIA", "C",  "Aaron Brewer",           "high",
  "MIA", "RG", "Jonah Savaiinaea",       "high",
  "MIA", "RT", "Austin Jackson",         "high",
  "MIN", "LT", "Christian Darrisaw",     "high",
  "MIN", "LG", "Donovan Jackson",        "high",
  "MIN", "C",  "Blake Brandel",          "high",
  "MIN", "RG", "Will Fries",             "high",
  "MIN", "RT", "Brian O'Neill",          "high",
  "NYJ", "LT", "Olumuyiwa Fashanu",      "high",
  "NYJ", "LG", "Dylan Parham",           "high",
  "NYJ", "C",  "Josh Myers",             "high",
  "NYJ", "RG", "Joe Tippmann",           "high",
  "NYJ", "RT", "Armand Membou",          "high",
  "PIT", "LT", "Troy Fautanu",           "high",
  "PIT", "LG", "Spencer Anderson",       "battle",
  "PIT", "C",  "Zach Frazier",           "high",
  "PIT", "RG", "Mason McCormick",        "high",
  "PIT", "RT", "Max Iheanachor",         "high",
  "SEA", "LT", "Charles Cross",          "high",
  "SEA", "LG", "Grey Zabel",             "high",
  "SEA", "C",  "Jalen Sundell",          "high",
  "SEA", "RG", "Anthony Bradford",       "high",
  "SEA", "RT", "Abraham Lucas",          "high"
)

opp_ol_2026 <- opp_ol_2026_starters %>%
  left_join(ol_2026 %>% select(team_name, roster_name, player_id, status,
                               data_pos = det_position,
                               snaps_2025, g_2025, g_2025_total,
                               pb_grade, tps_grade, tps_press, gap, zone,
                               pb_grade_bl, tps_grade_bl, tps_press_bl,
                               gap_bl, zone_bl),
            by = c("team_name", "roster_name"))

known_rookies_2026 <- c("Blake Miller", "Kahlil Benson", "Jake Slaughter",
                        "Kadyn Proctor", "Max Iheanachor")

opp_ol_2026 <- opp_ol_2026 %>%
  mutate(status = dplyr::coalesce(
    status, if_else(roster_name %in% known_rookies_2026, "rookie", NA_character_)))

name_misses <- opp_ol_2026 %>% filter(is.na(status))
if (nrow(name_misses) > 0) {
  print(name_misses %>% select(team_name, det_position, roster_name))
  stop("tribble names above matched no 2026 roster row - fix spellings vs nflreadr")
}

opp_ol_2026 <- opp_ol_2026 %>%
  mutate(slot_pool_mismatch = !is.na(data_pos) & data_pos != det_position)

cat("\n--- slot/pool mismatches ---\n")
print(opp_ol_2026 %>% filter(slot_pool_mismatch) %>%
        select(team_name, det_position, roster_name, data_pos,
               pb_grade, tps_grade, gap, zone), n = Inf)
cat("\n--- starter status counts ---\n")
print(opp_ol_2026 %>% count(status))

stopifnot(
  !any(opp_ol_2026$status == "no_pff_id"),
  nrow(opp_ol_2026) == 70,
  anyDuplicated(opp_ol_2026[, c("team_name", "det_position")]) == 0
)

# ------------------------------------------------------------
# 4b. BACKUPS — Andy's list, 2026-08-08. THE list; no auto-selection.
# ------------------------------------------------------------

opp_ol_2026_backups <- tribble(
  ~team_name, ~roster_name,            ~backs_up, ~note,
  "BUF", "Lloyd Cushenberry III",  "C",   "premier interior reserve in the division",
  "BUF", "Alec Anderson",          "LG",  "Corbett battle loser",
  "CHI", "Braxton Jones",          "LT",  "battle w/ Trapilo - injury return",
  "DEN", "Alex Palczewski",        "LG",  "Powers battle alt",
  "JAX", "Walker Little",          "LT",  "still quite useful - best swing T on slate",
  "KC",  "Jaylon Moore",           "RT",  "$15M/yr losing reps to a UDFA - battle live",
  "LAC", "Trevor Penning",         "LG",  "Slaughter battle alt",
  "LV",  "Jordan Meredith",        "LG",  "interior utility - LG candidate",
  "MIA", "Jamaree Salyer",         "LG",  "G/T flex, 2025 snaps at LT",
  "MIA", "Charlie Heck",           "RT",  "Jackson has missed games two straight years",
  "MIN", "Michael Jurgens",        "C",   "behind thin-data Brandel",
  "PIT", "Brock Hoffman",          "LG",  "C by trade, in the LG scramble",
  "SEA", "Bobby Hart",             "RT",  "veteran swing"
)

bk_miss <- opp_ol_2026_backups %>% anti_join(ol_2026, by = c("team_name", "roster_name"))
if (nrow(bk_miss) > 0) { print(bk_miss); stop("backup names not on 2026 rosters - fix vs nflreadr") }

backup_rows <- function(teams_vec) {
  opp_ol_2026_backups %>%
    filter(team_name %in% teams_vec) %>%
    left_join(ol_2026 %>% select(team_name, roster_name, player_id, status,
                                 data_pos = det_position, snaps_2025,
                                 pb25 = pb_grade, tps25 = tps_grade, gap25 = gap, zone25 = zone,
                                 pb26 = pb_grade_bl, tps26 = tps_grade_bl,
                                 gap26 = gap_bl, zone26 = zone_bl),
              by = c("team_name", "roster_name")) %>%
    left_join(ol_2024_profile %>%
                select(player_id, pb24 = pb_grade_24, tps24 = tps_grade_24,
                       gap24 = gap_24, zone24 = zone_24),
              by = "player_id") %>%
    mutate(pool_flag = !is.na(data_pos) & data_pos != backs_up,
           has_plot_cell = !is.na(tps25) | !is.na(gap25) | !is.na(zone25) |
             !is.na(tps24) | !is.na(gap24) | !is.na(zone24),
           name_lbl  = paste0("SW  ", roster_name, " (", backs_up, ")",
                              if_else(pool_flag, "*", "")))
}

# swing rows must have >=1 cell in a PLOTTED metric ('24/'25 x tps/gap/zone)
# or they are dropped from every panel - table-only, rendered as dashes.
# (pb grades exist for some but pb is not plotted, so they don't count.)
cat("\n-- swing rows dropped from panels (no plottable cell; table-only) --\n")
print(backup_rows(opp_2026_teams) %>%
        filter(!has_plot_cell) %>%
        select(team_name, roster_name, backs_up, snaps_2025, status), n = Inf)

# ------------------------------------------------------------
# 5. rookie prior (true entry-year seasons via id_xwalk) -> final starter table
# ------------------------------------------------------------

entry_years <- nflreadr::load_rosters(2017:2025) %>%
  filter(!is.na(gsis_id), !is.na(entry_year)) %>%
  group_by(gsis_id) %>%
  summarise(entry_year = min(entry_year), .groups = "drop") %>%
  inner_join(id_xwalk %>% filter(!is.na(gsis_id)) %>% select(player_id, gsis_id),
             by = "gsis_id") %>%
  select(player_id, entry_year)

first_season_prior <- function(df, col, out) {
  df %>% filter(det_position %in% ol_pos_levels) %>%
    inner_join(entry_years, by = "player_id") %>%
    filter(season == entry_year, entry_year >= 2017) %>%
    group_by(det_position) %>%
    summarise({{ out }} := median(.data[[col]], na.rm = TRUE), .groups = "drop")
}

rookie_prior <- first_season_prior(all_pass_block_player_season_summary, "grade_season_pctl", pr_pb) %>%
  left_join(first_season_prior(tps_pass_block_player_season_summary, "grade_season_pctl", pr_tps),
            by = "det_position") %>%
  left_join(first_season_prior(tps_pass_block_player_season_summary, "pressure_season_pctl", pr_tpsp),
            by = "det_position") %>%
  left_join(first_season_prior(gap_player_season_summary,  "gap_season_pctl",  pr_gap),
            by = "det_position") %>%
  left_join(first_season_prior(zone_player_season_summary, "zone_season_pctl", pr_zone),
            by = "det_position")

cat("\n--- rookie prior by position ---\n")
print(rookie_prior)

opp_ol_2026_final <- opp_ol_2026 %>%
  left_join(rookie_prior, by = "det_position") %>%
  mutate(pb_f   = if_else(status == "rookie", dplyr::coalesce(pb_grade_bl,  pr_pb),
                          dplyr::coalesce(pb_grade_bl,  pb_grade)),
         tps_f  = if_else(status == "rookie", dplyr::coalesce(tps_grade_bl, pr_tps),
                          dplyr::coalesce(tps_grade_bl, tps_grade)),
         tpsp_f = if_else(status == "rookie", dplyr::coalesce(tps_press_bl, pr_tpsp),
                          dplyr::coalesce(tps_press_bl, tps_press)),
         gap_f  = if_else(status == "rookie", dplyr::coalesce(gap_bl,       pr_gap),
                          dplyr::coalesce(gap_bl,       gap)),
         zone_f = if_else(status == "rookie", dplyr::coalesce(zone_bl,      pr_zone),
                          dplyr::coalesce(zone_bl,      zone))) %>%
  select(-starts_with("pr_"))

# ------------------------------------------------------------
# 6. slot ledger: 2025 primary starter vs 2026 projection
# ------------------------------------------------------------

team_ol_2025_starters <- all_pass_block_summary %>%
  filter(season == 2025, in_season(week),
         det_position %in% ol_pos_levels, team_name %in% opp_2026_teams) %>%
  group_by(team_name, det_position, player, player_id) %>%
  summarise(snaps = sum(snap_counts_pass_block, na.rm = TRUE), .groups = "drop") %>%
  group_by(team_name, det_position) %>%
  slice_max(snaps, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(ol_season_pctl, by = c("player_id", "det_position"))

ol_slot_delta <- team_ol_2025_starters %>%
  select(team_name, det_position, starter_2025 = player, pid_2025 = player_id,
         pb_2025 = pb_grade, tps_2025 = tps_grade, tpsp_2025 = tps_press,
         gap_2025 = gap, zone_2025 = zone) %>%
  full_join(opp_ol_2026_final %>%
              select(team_name, det_position, starter_2026 = roster_name,
                     pid_2026 = player_id, conf, slot_pool_mismatch,
                     pb_2026 = pb_f, tps_2026 = tps_f, tpsp_2026 = tpsp_f,
                     gap_2026 = gap_f, zone_2026 = zone_f),
            by = c("team_name", "det_position")) %>%
  mutate(same_player = !is.na(pid_2025) & !is.na(pid_2026) & pid_2025 == pid_2026,
         d_pb   = pb_2026 - pb_2025,
         d_tps  = tps_2026 - tps_2025,
         d_tpsp = tpsp_2026 - tpsp_2025) %>%
  arrange(team_name, factor(det_position, levels = ol_pos_levels))

# ------------------------------------------------------------
# 7. slates + comparison tables
# ------------------------------------------------------------

ne_2025_opp_ol_games <- all_pass_block_summary %>%
  filter(def_ssn == "NE2025", in_season(week), det_position %in% ol_pos_levels) %>%
  group_by(week, team_name, det_position) %>%
  slice_max(snap_counts_pass_block, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(week, team_name, det_position, player, player_id) %>%
  left_join(ol_season_pctl, by = c("player_id", "det_position")) %>%
  transmute(team_name, det_position,
            pb_f = pb_grade, tps_f = tps_grade, tpsp_f = tps_press,
            gap_f = gap, zone_f = zone)

cat("\n--- 2025 slate coverage (expect 21 games x 5 = 105 cells if all playoff weeks are loaded) ---\n")
print(ne_2025_opp_ol_games %>% count(det_position))
cat("\n--- NE playoff games present in table ---\n")
print(all_pass_block_summary %>%
        filter(def_ssn == "NE2025", week >= 28) %>%
        distinct(week, team_name) %>% arrange(week))

slate_2026_rows <- tibble(team_name = sched_2026) %>%
  left_join(opp_ol_2026_final %>%
              select(team_name, det_position, pb_f, tps_f, tpsp_f, gap_f, zone_f),
            by = "team_name", relationship = "many-to-many")

slate_profile <- function(rows, lbl) {
  rows %>%
    group_by(det_position) %>%
    summarise(n_rows = dplyr::n(),
              n_pb   = sum(!is.na(pb_f)),
              across(c(pb_f, tps_f, tpsp_f, gap_f, zone_f), ~ mean(.x, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(slate = lbl)
}

cmp_ol_slate <- bind_rows(
  slate_profile(ne_2025_opp_ol_games, "slate_2025"),
  slate_profile(slate_2026_rows,      "slate_2026")
) %>%
  select(-n_rows, -n_pb) %>%
  pivot_longer(c(pb_f, tps_f, tpsp_f, gap_f, zone_f), names_to = "metric", values_to = "pctl") %>%
  pivot_wider(names_from = slate, values_from = pctl) %>%
  mutate(delta = slate_2026 - slate_2025)

print(cmp_ol_slate, n = Inf)

cmp_ol_slate_sens <- bind_rows(
  slate_profile(ne_2025_opp_ol_games, "slate_2025"),
  slate_profile(slate_2026_rows,      "slate_2026_all"),
  slate_profile(tibble(team_name = sched_2026) %>%
                  left_join(opp_ol_2026_final %>% filter(conf == "high") %>%
                              select(team_name, det_position, pb_f, tps_f, tpsp_f,
                                     gap_f, zone_f),
                            by = "team_name", relationship = "many-to-many"),
                "slate_2026_highconf")
) %>%
  select(-n_rows, -n_pb) %>%
  pivot_longer(c(pb_f, tps_f, tpsp_f, gap_f, zone_f), names_to = "metric", values_to = "pctl") %>%
  pivot_wider(names_from = slate, values_from = pctl) %>%
  mutate(delta_all  = slate_2026_all - slate_2025,
         delta_high = slate_2026_highconf - slate_2025,
         sign_flip  = sign(delta_all) != sign(delta_high))

cat("\n--- battle sensitivity ---\n")
print(cmp_ol_slate_sens, n = Inf)

# ------------------------------------------------------------
# 8. FIG 1
# ------------------------------------------------------------

ol_metric_lbl <- c(pb_f = "Pass block", tps_f = "True pass set",
                   tpsp_f = "TPS pressure",
                   gap_f = "Gap run",   zone_f = "Zone run")

plot_ol_slate_delta <- function(cmp, title) {
  pd <- cmp %>%
    mutate(position = factor(det_position, levels = rev(ol_pos_levels)),
           metric   = factor(ol_metric_lbl[metric], levels = unname(ol_metric_lbl)))
  ggplot(pd, aes(y = position)) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey45") +
    geom_segment(aes(x = slate_2025, xend = slate_2026, yend = position),
                 arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
                 linewidth = 1, color = "grey60") +
    geom_point(aes(x = slate_2025), shape = 1, size = 3.2, stroke = 1.2, color = "grey55") +
    geom_point(aes(x = slate_2026), shape = 16, size = 2.6, color = "#002244") +
    facet_wrap(~ metric, nrow = 1) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(title = title,
         subtitle = "open circle = 2025 slate (actual OL faced) | solid navy = 2026 slate (projected) | higher = HARDER for NE's front | dashed = league median",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(color = "grey40", size = 8.5),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(face = "bold", size = 9),
          panel.spacing.x = unit(1.1, "lines"))
}

plot_ol_slate_delta(cmp_ol_slate %>% filter(metric != "pb_f"),
                    "NE defense - 2026 vs 2025 opposing-OL slate, by position")

# THE ordering law - settled by running ggplot_build, not theory:
#   (1) level[1] renders at the BOTTOM of a discrete y axis, including
#       inside facet_grid(scales = "free_y", space = "free_y")
#   (2) scale limits= must NEVER be set: limits force every level into
#       every facet (the all-teams-stacked soup)
#   (3) under free_y each panel trains its scale in LAYER order, so the
#       geom_blank anchor is the FIRST layer of every panel - it fixes
#       row order and keeps data-less rows visible as labeled empty rows
# Levels are built bottom -> top (SW band, RT, RG, C, LG, LT), so the
# display reads LT, LG, C, RG, RT, then swing depth.
# Mechanics live in lvl_order_v2 (fig2v2 file) — the v1 lvl_order was
# retired with the v1 panels 2026-08-08 to keep exactly one live version.

# compact level audit: one line per panel, capped detail, only on mismatch
audit_panel <- function(lvls, lblmap, frames, lbl) {
  ids <- unique(unlist(lapply(frames, function(f)
    if ("y_id" %in% names(f)) as.character(f$y_id) else character(0))))
  a <- setdiff(ids, lvls); b <- setdiff(lvls, ids)
  cat(sprintf("%s: %d levels / %d data ids - %s\n", lbl, length(lvls), length(ids),
              if (!length(a)) "clean" else "MISMATCH - restart R + source whole file"))
  if (length(a)) { cat("  in data, not levels (drops):\n"); print(utils::head(a, 8))
    if (length(a) > 8) cat("  ... +", length(a) - 8, "more\n") }
  if (length(b)) cat("  levels with no data (render as empty rows):",
                     paste(utils::head(lblmap[b], 4), collapse = " | "), "\n")
  ord <- tibble(y_id = lvls, team = sub("\\|\\|.*$", "", lvls), lbl = unname(lblmap[lvls])) %>%
    group_by(team) %>% summarise(bottom = dplyr::first(lbl), top = dplyr::last(lbl), .groups = "drop")
  for (i in seq_len(nrow(ord)))
    cat(sprintf("  %-4s top: %-28s bottom: %s\n", ord$team[i], ord$top[i], ord$bottom[i]))
}


# ------------------------------------------------------------
# 9. SHARED PANEL INFRASTRUCTURE — used by the Fig-2 v2 player panels
#    in section 11 below. v1 Panels A/B/C RETIRED 2026-08-08: a changed
#    slot stacked two players' marks on one row ("whose '24 is that") —
#    superseded by v2's one-row-one-player grammar.
# ------------------------------------------------------------

METRIC_LBL   <- c(tps = "True pass set", gap = "Gap run", zone = "Zone run")
PLOT_METRICS <- names(METRIC_LBL)

C_GHOST <- "grey55"; C_NEW <- "#002244"; C_ROOK <- "#C60C30"

div_teams  <- c("BUF", "NYJ", "MIA")
teams_2025 <- ne_2025_opp_ol_games %>% distinct(team_name) %>% pull(team_name)
rep_teams  <- setdiff(intersect(opp_2026_teams, teams_2025), div_teams)
new_teams  <- setdiff(opp_2026_teams, teams_2025)

cat("divisional:    ", paste(div_teams, collapse = ", "),
    "\nrepeat non-div:", paste(rep_teams, collapse = ", "),
    "\nnew opponents: ", paste(new_teams, collapse = ", "), "\n")

fig_theme2 <- function(ylab_size = 8) {
  theme_minimal(base_size = 11) +
    theme(plot.title       = element_text(face = "bold", size = 13),
          plot.subtitle    = element_text(color = "grey40", size = 8.5),
          panel.grid.major.y = element_blank(),
          strip.text.x     = element_text(face = "bold", size = 9),
          strip.text.y     = element_text(face = "bold"),
          axis.text.y      = element_text(size = ylab_size),
          legend.position  = "bottom",
          legend.text      = element_text(size = 8),
          legend.margin    = margin(t = -4),
          plot.caption     = element_text(size = 8, hjust = 0, color = "grey35",
                                          margin = margin(t = 4)),
          panel.spacing    = unit(0.9, "lines"))
}

x_pct <- scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1),
                            labels = scales::percent_format(accuracy = 1))


# ------------------------------------------------------------
# 10. THREE-SEASON HISTORY TABLE - 2023 / 2024 / 2025, each year at the
#     player's primary position that season. History lives HERE; the
#     panels show current-season only. TPS pressure spanner added
#     2026-08-08 (grade cols unchanged).
# ------------------------------------------------------------

library(gt)

ol_season_pctl_23 <- build_pctl(2023)

primary_pos_year <- function(yr) {
  all_pass_block_summary %>%
    filter(season == yr, in_season(week), det_position %in% ol_pos_levels) %>%
    count(player_id, det_position, wt = snap_counts_pass_block, name = "sn") %>%
    group_by(player_id) %>% slice_max(sn, n = 1, with_ties = FALSE) %>% ungroup() %>%
    select(player_id, det_position)
}

year_cols <- function(yr, pctl_df, sfx) {
  primary_pos_year(yr) %>%
    left_join(pctl_df, by = c("player_id", "det_position")) %>%
    transmute(player_id,
              !!paste0("tps_",  sfx) := tps_grade,
              !!paste0("tpsp_", sfx) := tps_press,
              !!paste0("gap_",  sfx) := gap,
              !!paste0("zone_", sfx) := zone)
}

hist3 <- year_cols(2023, ol_season_pctl_23, "23") %>%
  full_join(year_cols(2024, ol_season_pctl_24, "24"), by = "player_id") %>%
  full_join(year_cols(2025, ol_season_pctl,    "25"), by = "player_id")

tbl3_rows <- bind_rows(
  opp_ol_2026_final %>%
    transmute(team_name, slot = det_position, is_swing = FALSE, player_id,
              player = paste0(roster_name,
                              if_else(dplyr::coalesce(slot_pool_mismatch, FALSE), "*", ""),
                              if_else(conf == "battle", "\u00B0", ""),
                              if_else(status == "rookie", " (R)", ""),
                              if_else(status == "data_2024_only", " ('24)", ""))),
  backup_rows(opp_2026_teams) %>%
    transmute(team_name, slot = backs_up, is_swing = TRUE, player_id,
              player = paste0("SW ", roster_name, if_else(pool_flag, "*", "")))
) %>%
  left_join(hist3, by = "player_id") %>%
  mutate(t_ord = match(team_name, opp_2026_teams),
         s_ord = match(slot, ol_pos_levels)) %>%
  arrange(t_ord, is_swing, s_ord) %>%
  select(team_name, slot, player,
         tps_23, tps_24, tps_25, tpsp_23, tpsp_24, tpsp_25,
         gap_23, gap_24, gap_25, zone_23, zone_24, zone_25)

# spanner columns are EXPLICIT on purpose: starts_with("tps") would also
# grab the tpsp_ columns - do not "simplify" back to prefix matching
ol_hist_gt <- tbl3_rows %>%
  gt(groupname_col = "team_name") %>%
  tab_spanner(label = "True pass set", columns = c(tps_23, tps_24, tps_25)) %>%
  tab_spanner(label = "TPS pressure",  columns = c(tpsp_23, tpsp_24, tpsp_25)) %>%
  tab_spanner(label = "Gap run",       columns = c(gap_23, gap_24, gap_25)) %>%
  tab_spanner(label = "Zone run",      columns = c(zone_23, zone_24, zone_25)) %>%
  cols_label(slot = "", player = "Player",
             tps_23 = "'23", tps_24 = "'24", tps_25 = "'25",
             tpsp_23 = "'23", tpsp_24 = "'24", tpsp_25 = "'25",
             gap_23 = "'23", gap_24 = "'24", gap_25 = "'25",
             zone_23 = "'23", zone_24 = "'24", zone_25 = "'25") %>%
  fmt_percent(columns = -c(team_name, slot, player), decimals = 0) %>%
  data_color(columns = -c(team_name, slot, player),
             fn = scales::col_numeric(c("#deebf7", "#08306b"), domain = c(0, 1),
                                      na.color = "#f5f5f5"),
             autocolor_text = TRUE) %>%   # older gt: swap fn = for colors =
  sub_missing(missing_text = "\u2014") %>%
  tab_header(title = "2026 opponent OL \u2014 three-season history",
             subtitle = "each year at that season's primary position | darker = harder for NE's front | TPS pressure: higher = fewer pressures allowed | * pool caveat | \u00B0 battle | (R) rookie | ('24) 2024-only | SW swing depth") %>%
  tab_options(row_group.font.weight = "bold", table.font.size = px(11),
              data_row.padding = px(2))

ol_hist_gt
# gtsave(ol_hist_gt, "ol_hist_table.png", vwidth = 1450)   # was 1100; +3 cols

# ------------------------------------------------------------
# 10b. FIG-1 COMPANION TABLE — the slate numbers behind the arrows.
#     One row per position; '25 slate / '26 slate / delta per metric.
#     Bold delta = sign flips when battle slots resolve to high-conf
#     starters only (from cmp_ol_slate_sens). Division counts twice,
#     same weighting as Fig 1 (sched_2026).
# ------------------------------------------------------------

slate_tbl_wide <- cmp_ol_slate_sens %>%
  filter(metric != "pb_f") %>%
  transmute(det_position, metric,
            s25  = slate_2025,
            s26  = slate_2026_all,
            d    = delta_all,
            flip = dplyr::coalesce(sign_flip, FALSE)) %>%
  pivot_wider(names_from = metric, values_from = c(s25, s26, d, flip),
              names_glue = "{metric}_{.value}") %>%
  arrange(match(det_position, ol_pos_levels)) %>%
  select(det_position,
         tps_f_s25,  tps_f_s26,  tps_f_d,  tps_f_flip,
         tpsp_f_s25, tpsp_f_s26, tpsp_f_d, tpsp_f_flip,
         gap_f_s25,  gap_f_s26,  gap_f_d,  gap_f_flip,
         zone_f_s25, zone_f_s26, zone_f_d, zone_f_flip)

# symmetric diverging domain from the data — no hardcoded scale edges
d_dom <- max(abs(as.matrix(slate_tbl_wide %>%
                             select(tps_f_d, tpsp_f_d, gap_f_d, zone_f_d))),
             na.rm = TRUE)

ol_slate_cmp_gt <- slate_tbl_wide %>%
  gt() %>%
  tab_spanner(label = "True pass set", columns = c(tps_f_s25,  tps_f_s26,  tps_f_d)) %>%
  tab_spanner(label = "TPS pressure",  columns = c(tpsp_f_s25, tpsp_f_s26, tpsp_f_d)) %>%
  tab_spanner(label = "Gap run",       columns = c(gap_f_s25,  gap_f_s26,  gap_f_d)) %>%
  tab_spanner(label = "Zone run",      columns = c(zone_f_s25, zone_f_s26, zone_f_d)) %>%
  cols_hide(c(tps_f_flip, tpsp_f_flip, gap_f_flip, zone_f_flip)) %>%
  cols_label(det_position = "",
             tps_f_s25 = "'25",  tps_f_s26 = "'26",  tps_f_d = "\u0394",
             tpsp_f_s25 = "'25", tpsp_f_s26 = "'26", tpsp_f_d = "\u0394",
             gap_f_s25 = "'25",  gap_f_s26 = "'26",  gap_f_d = "\u0394",
             zone_f_s25 = "'25", zone_f_s26 = "'26", zone_f_d = "\u0394") %>%
  fmt_percent(columns = c(tps_f_s25, tps_f_s26, tpsp_f_s25, tpsp_f_s26,
                          gap_f_s25, gap_f_s26, zone_f_s25, zone_f_s26),
              decimals = 0) %>%
  fmt_percent(columns = c(tps_f_d, tpsp_f_d, gap_f_d, zone_f_d),
              decimals = 0, force_sign = TRUE) %>%   # older gt: drop force_sign
  data_color(columns = c(tps_f_d, tpsp_f_d, gap_f_d, zone_f_d),
             fn = scales::col_numeric(c("#6baed6", "#f7f7f7", "#C60C30"),
                                      domain = c(-d_dom, d_dom)),
             autocolor_text = TRUE) %>%   # older gt: swap fn = for colors =
  tab_style(style = cell_text(weight = "bold"),
            locations = list(
              cells_body(columns = tps_f_d,  rows = tps_f_flip),
              cells_body(columns = tpsp_f_d, rows = tpsp_f_flip),
              cells_body(columns = gap_f_d,  rows = gap_f_flip),
              cells_body(columns = zone_f_d, rows = zone_f_flip))) %>%
  tab_header(title = "2026 vs 2025 opposing-OL slate \u2014 the numbers behind Fig 1",
             subtitle = "schedule-weighted mean pctl, division counted twice | higher = harder for NE's front | \u0394 red = slate got harder, blue = easier | bold \u0394 = sign flips under high-conf starters only") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold")

ol_slate_cmp_gt
# gtsave(ol_slate_cmp_gt, "ol_slate_cmp_table.png", vwidth = 950)


# ------------------------------------------------------------
# 11. FIG 2 v2 — PLAYER PANELS (one row = one player)
# ------------------------------------------------------------
# After ANY label edit: re-source THIS WHOLE FILE (law 5 — never re-run a plot object).
#
# THE ONE RULE: one row = one player; max two marks = his observed '24 (open)
#   + '25 (solid). Rookie rows = labeled prior diamond only. No projections
#   anywhere on player panels — blends live in Fig 1 only.
#
# ROW GRAMMAR per slot:
#   ret    2026 starter == 2025 team-season primary at the slot -> one row "(ret)"
#   faced  slot changed hands on a team NE played in '25 -> outgoing row
#          "(faced '25)" ABOVE the incoming row (reads then -> now)
#   in     incoming starter on a changed slot -> "(in)"
#   rookie one row "(R)", prior diamond only
#   swing  Andy's 13-row tribble, has_plot_cell gate, below the slots
# New opponents: no outgoing rows AND no (ret)/(in) tags — NE never played
#   their '25 lines, so turnover there is untracked (ruling 2026-08-08).
#   (R) stays everywhere: it decodes the diamond, not turnover.
#
# VERIFIED LAWS (carry over unchanged): level[1] renders at the BOTTOM;
#   never set scale limits=; geom_blank anchor is the FIRST layer of every
#   panel; levels built bottom -> top.

faced_teams <- c(div_teams, rep_teams)

# ------------------------------------------------------------
# ordering: team -> swing band last -> slot LT..RT -> within a changed
# slot, outgoing ABOVE incoming. Levels are built bottom -> top, so the
# arrange is the reverse of display order: desc(r_ord) puts incoming (2)
# into the vector before faced (1), which renders faced on top.
#   r_ord: 2 = incoming (ret/in/rookie), 1 = faced, 0 = swing (inert)
# ------------------------------------------------------------

lvl_order_v2 <- function(df, team_vec) {
  df %>%
    distinct(team_name, slot, is_swing, r_ord, y_id) %>%
    mutate(t_ord = match(as.character(team_name), team_vec),
           s_ord = match(slot, ol_pos_levels)) %>%
    arrange(t_ord, desc(is_swing), desc(s_ord), desc(r_ord)) %>%
    pull(y_id)
}

# ------------------------------------------------------------
# incoming rows — all 70 slots, raw observed '24/'25 only
# ------------------------------------------------------------

v2_incoming <- opp_ol_2026_final %>%
  select(team_name, slot = det_position, roster_name, player_id, status, conf,
         data_pos, tps25 = tps_grade, gap25 = gap, zone25 = zone) %>%
  left_join(ol_slot_delta %>% select(team_name, det_position, same_player),
            by = c("team_name", "slot" = "det_position")) %>%
  left_join(ol_2024_profile %>%
              select(player_id, pos24 = det_position_24, tps24 = tps_grade_24,
                     gap24 = gap_24, zone24 = zone_24),
            by = "player_id") %>%
  # (ret)/(in) is a faced-team story only — new opponents' non-rookie rows
  # carry no role tag (ruling 2026-08-08)
  mutate(role  = case_when(status == "rookie"            ~ "rookie",
                           !(team_name %in% faced_teams) ~ "starter",
                           same_player                   ~ "ret",
                           TRUE                          ~ "in"),
         has24 = !is.na(tps24) | !is.na(gap24) | !is.na(zone24),
         has25 = !is.na(tps25) | !is.na(gap25) | !is.na(zone25),
         pool_star = (has25 & !is.na(data_pos) & data_pos != slot) |
           (has24 & !is.na(pos24)    & pos24    != slot),
         no25  = !has25 & role != "rookie",
         name_lbl = paste0(slot, "  ", roster_name,
                           if_else(pool_star, "*", ""),
                           if_else(conf == "battle", "\u00B0", ""),
                           case_when(role == "rookie" ~ " (R)",
                                     role == "ret"    ~ " (ret)",
                                     role == "in"     ~ " (in)",
                                     TRUE             ~ ""),
                           if_else(no25, " (no '25)", "")),
         y_id = paste(team_name, name_lbl, sep = "||"))

stopifnot(nrow(v2_incoming) == 70,
          !any(is.na(v2_incoming$same_player)))

# rookie rows are diamond-only by law; observed cells on a rookie row mean
# a status/known_rookies error — fix the label, don't filter the data
v2_rk <- v2_incoming %>% filter(role == "rookie")
stopifnot(all(is.na(v2_rk$tps24)), all(is.na(v2_rk$tps25)),
          all(is.na(v2_rk$gap24)), all(is.na(v2_rk$gap25)),
          all(is.na(v2_rk$zone24)), all(is.na(v2_rk$zone25)))

# ------------------------------------------------------------
# faced rows — 2025 team-season primary at the slot, only where the slot
# changed hands AND NE played the team in '25. His '25 grades are
# slot-keyed by the team_ol_2025_starters join, so the pool star can only
# fire off the '24 ghost. Departed / relocated players stay (they're
# history — that's the point); no team annotation.
# ------------------------------------------------------------

v2_faced <- ol_slot_delta %>%
  filter(team_name %in% faced_teams, !same_player, !is.na(pid_2025)) %>%
  select(team_name, slot = det_position, roster_name = starter_2025,
         player_id = pid_2025,
         tps25 = tps_2025, gap25 = gap_2025, zone25 = zone_2025) %>%
  left_join(ol_2024_profile %>%
              select(player_id, pos24 = det_position_24, tps24 = tps_grade_24,
                     gap24 = gap_24, zone24 = zone_24),
            by = "player_id") %>%
  mutate(has24 = !is.na(tps24) | !is.na(gap24) | !is.na(zone24),
         has25 = !is.na(tps25) | !is.na(gap25) | !is.na(zone25),
         pool_star = has24 & !is.na(pos24) & pos24 != slot,
         no25  = !has25,
         name_lbl = paste0(slot, "  ", roster_name,
                           if_else(pool_star, "*", ""),
                           " (faced '25)",
                           if_else(no25, " (no '25)", "")),
         y_id = paste(team_name, name_lbl, sep = "||"))

# ------------------------------------------------------------
# swing rows — the tribble, unchanged rules
# ------------------------------------------------------------

v2_swing <- backup_rows(opp_2026_teams) %>%
  filter(has_plot_cell) %>%
  transmute(team_name, slot = backs_up, roster_name, name_lbl,
            y_id = paste(team_name, name_lbl, sep = "||"),
            tps24, tps25, gap24, gap25, zone24, zone25)

# ------------------------------------------------------------
# row registry + gates
# ------------------------------------------------------------

v2_rows <- bind_rows(
  v2_incoming %>% transmute(team_name, slot, is_swing = FALSE, r_ord = 2L, y_id, name_lbl),
  v2_faced    %>% transmute(team_name, slot, is_swing = FALSE, r_ord = 1L, y_id, name_lbl),
  v2_swing    %>% transmute(team_name, slot, is_swing = TRUE,  r_ord = 0L, y_id, name_lbl)
)
stopifnot(anyDuplicated(v2_rows$y_id) == 0)

cat("\n--- v2 slot-turnover ledger (faced teams only; new-opp turnover untracked) ---\n")
print(ol_slot_delta %>%
        filter(team_name %in% faced_teams, !same_player) %>%
        transmute(team_name, slot = det_position,
                  out_2025 = starter_2025, in_2026 = starter_2026) %>%
        arrange(match(team_name, opp_2026_teams), match(slot, ol_pos_levels)),
      n = Inf)

cat("\n--- v2 starter roles ('starter' = new-opp row, no tag) ---\n")
print(v2_incoming %>% count(role))

# ------------------------------------------------------------
# long frames — plotted metrics only (tps/gap/zone; pb never plots)
# ------------------------------------------------------------

v2_obs_long <- bind_rows(
  v2_incoming %>% filter(role != "rookie") %>%
    select(team_name, y_id, tps24, tps25, gap24, gap25, zone24, zone25),
  v2_faced %>%
    select(team_name, y_id, tps24, tps25, gap24, gap25, zone24, zone25)
) %>%
  pivot_longer(-c(team_name, y_id),
               names_to = c("metric", "year"),
               names_pattern = "(tps|gap|zone)(24|25)",
               values_to = "pctl", values_drop_na = TRUE) %>%
  mutate(year = paste0("20", year))

v2_swing_long <- v2_swing %>%
  select(team_name, y_id, tps24, tps25, gap24, gap25, zone24, zone25) %>%
  pivot_longer(-c(team_name, y_id),
               names_to = c("metric", "year"),
               names_pattern = "(tps|gap|zone)(24|25)",
               values_to = "pctl", values_drop_na = TRUE) %>%
  mutate(year = paste0("20", year))

v2_prior_long <- v2_incoming %>%
  filter(role == "rookie") %>%
  select(team_name, slot, y_id) %>%
  left_join(rookie_prior %>% select(det_position, pr_tps, pr_gap, pr_zone),
            by = c("slot" = "det_position")) %>%
  pivot_longer(starts_with("pr_"), names_to = "metric", values_to = "pctl",
               values_drop_na = TRUE) %>%
  mutate(metric = str_remove(metric, "pr_")) %>%
  select(team_name, y_id, metric, pctl)

# ------------------------------------------------------------
# legend / decoder — five shapes only; role lives in the row label
# ------------------------------------------------------------

V2_LEGEND <- c("'24", "'25", "Swing '24", "Swing '25", "Rookie prior")
V2_SHAPES <- c("'24" = 1, "'25" = 16, "Swing '24" = 2, "Swing '25" = 17,
               "Rookie prior" = 5)
V2_COLORS <- c("'24" = C_GHOST, "'25" = C_NEW, "Swing '24" = C_GHOST,
               "Swing '25" = C_NEW, "Rookie prior" = C_ROOK)

V2_SUB <- "higher = HARDER for NE's front | observed seasons only, no projections | 3-season detail: table"
V2_CAP_ROLES <- paste0(
  "(ret)  returning starter      (faced '25)  2025 slot starter, since replaced      ",
  "(in)  incoming starter      (R)  rookie \u2014 diamond = entry-year positional prior      SW  swing depth")
V2_CAP_NEW   <- "(R)  rookie \u2014 diamond = entry-year positional prior      SW  swing depth"
V2_CAP_FLAGS <- paste0(
  "*  grades earned at a different position (pool caveat)      \u00B0  camp battle      ",
  "(no '25)  no 2025 grade in plotted views")

# ------------------------------------------------------------
# one plotting function, three team-vector calls
# ------------------------------------------------------------

build_fig2v2 <- function(team_vec, title, ylab_size, panel_lbl) {
  lv <- v2_rows %>% filter(team_name %in% team_vec) %>% lvl_order_v2(team_vec)
  lm <- setNames(v2_rows$name_lbl, v2_rows$y_id)[lv]
  anch <- tibble(y_f = factor(lv, levels = lv),
                 team_name = factor(sub("\\|\\|.*$", "", lv), levels = team_vec))
  
  fx <- function(df) df %>%
    filter(team_name %in% team_vec) %>%
    mutate(y_f       = factor(y_id, levels = lv),
           metric    = factor(METRIC_LBL[metric], levels = unname(METRIC_LBL)),
           team_name = factor(as.character(team_name), levels = team_vec))
  
  ob <- fx(v2_obs_long)
  sw <- fx(v2_swing_long)
  pr <- fx(v2_prior_long)
  
  audit_panel(lv, lm, list(ob, sw, pr), panel_lbl)
  
  # role decoder line only where role tags exist; the new-opponents panel
  # carries none, so its caption drops that line
  cap <- paste0(if (any(team_vec %in% faced_teams)) V2_CAP_ROLES else V2_CAP_NEW,
                "\n", V2_CAP_FLAGS)
  
  ggplot() +
    geom_blank(data = anch, aes(x = 0.5, y = y_f)) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey45") +
    geom_point(data = ob %>% filter(year == "2024"),
               aes(x = pctl, y = y_f, shape = "'24", color = "'24"),
               size = 3.2, stroke = 1.2) +
    geom_point(data = ob %>% filter(year == "2025"),
               aes(x = pctl, y = y_f, shape = "'25", color = "'25"),
               size = 2.6) +
    geom_point(data = sw %>% filter(year == "2024"),
               aes(x = pctl, y = y_f, shape = "Swing '24", color = "Swing '24"),
               size = 2.8, stroke = 1.1) +
    geom_point(data = sw %>% filter(year == "2025"),
               aes(x = pctl, y = y_f, shape = "Swing '25", color = "Swing '25"),
               size = 2.4) +
    geom_point(data = pr,
               aes(x = pctl, y = y_f, shape = "Rookie prior", color = "Rookie prior"),
               size = 3, stroke = 1.1) +
    scale_shape_manual(NULL, breaks = V2_LEGEND, values = V2_SHAPES) +
    scale_color_manual(NULL, breaks = V2_LEGEND, values = V2_COLORS) +
    scale_y_discrete(labels = lm) +
    facet_grid(team_name ~ metric, scales = "free_y", space = "free_y",
               switch = "y") +
    x_pct +
    labs(title = title, subtitle = V2_SUB, caption = cap,
         x = NULL, y = NULL) +
    fig_theme2(ylab_size) +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, face = "bold", size = 10))
}

plot_v2_div <- build_fig2v2(div_teams,
                            "Divisional opponents \u2014 slot history & 2026 starters",
                            ylab_size = 8, panel_lbl = "V2 DIV")
plot_v2_div

plot_v2_rep <- build_fig2v2(rep_teams,
                            "Repeat opponents \u2014 slot history & 2026 starters",
                            ylab_size = 7, panel_lbl = "V2 REP")
plot_v2_rep

plot_v2_new <- build_fig2v2(new_teams,
                            "New opponents \u2014 projected 2026 starters, last two seasons",
                            ylab_size = 7, panel_lbl = "V2 NEW")
plot_v2_new

# ------------------------------------------------------------
# checkpoint
# ------------------------------------------------------------
# ggsave("fig1_slate.png",    plot_ol_slate_delta(cmp_ol_slate %>% filter(metric != "pb_f"),
#        "NE defense - 2026 vs 2025 opposing-OL slate, by position"), width = 13, height = 6, dpi = 200)  # 4 facets now
# ggsave("fig2v2_div.png", plot_v2_div, width = 12, height = 10, dpi = 200)
# ggsave("fig2v2_rep.png", plot_v2_rep, width = 12,
#        height = 2.5 + 2.2 * length(rep_teams), dpi = 200)
# ggsave("fig2v2_new.png", plot_v2_new, width = 12,
#        height = 2.5 + 2.2 * length(new_teams), dpi = 200)
# save.image("~/ne_ol_slate_workspace.RData")
# system('aws s3 cp ~/ne_ol_slate_workspace.RData s3://nfl-pff-data-lucas/workspaces/')