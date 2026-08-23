# ============================================================
# PHASE 5 LEAGUE MODE -- THE MAN LENS (systemic test, both splits)
# Written 2026-08-16 on Andy's ask: "repeat this for man ... add
# the man to this - the man differential league wide."
#
# Mirror law: the ZONE engine (ne_receiving_dump_two receipts:
# NE d_zone +.091, rank 1/32, league mean +.002, sd .034) is
# reproduced VERBATIM and the man lens is the same machine with
# man_grade_pctl / pr_man_grade swapped in. ONE corps per team,
# TWO values per member -- membership (top N_CORPS by pmax
# routes) and weights (blend2 routes ladder) are split-neutral
# by the Phase 5 header ("split lives in the METRICS; weights
# are overall routes on BOTH sides"). Man pctls exist only
# above SPLIT_RTE_MIN = 50 man routes (the split floor);
# under-floor members ride the band entry-year prior, flagged
# in prior_share per split -- the man side will carry more
# prior weight. That is the honesty column, not a bug.
#
# Engine note (carried, not re-ruled): the league engine uses
# the 2-rung weight ladder (blend -> pmax), no corps-median
# fill; zero-evidence members carry w = 0 and drop out of the
# weighted mean. Same on both lenses -> commensurable.
#
# SOURCE ORDER: the Phase 5 build file sourced this session
# (receiving_func_base, rec_season_pctl_sos, rec_id_bridge,
# usage_rec, band_26, p25, p24, rec_rookie_prior, pos_keep,
# N_CORPS, X_QUAL) + the OL canon (in_season, blend2,
# pff_team_lookup). Re-source WHOLE file after any edit.
# ASCII only.
# ============================================================

needed_ls <- c("receiving_func_base", "rec_season_pctl_sos",
               "rec_id_bridge", "usage_rec", "band_26", "p25", "p24",
               "rec_rookie_prior", "pos_keep", "N_CORPS", "X_QUAL",
               "in_season", "blend2", "pff_team_lookup")
missing_ls <- needed_ls[!vapply(needed_ls, exists, logical(1))]
if (length(missing_ls)) stop("missing session objects: ",
                             paste(missing_ls, collapse = ", "),
                             " -- source the Phase 5 build file first")

library(dplyr)

# --- the 2026 schedule frame (division x2 automatic: 17 rows per focal)
sch26 <- nflreadr::load_schedules(2026) %>% filter(game_type == "REG")
opp_map <- bind_rows(
  sch26 %>% transmute(focal = home_team, opp = away_team),
  sch26 %>% transmute(focal = away_team, opp = home_team)) %>%
  mutate(focal = dplyr::coalesce(pff_team_lookup[focal], focal),
         opp   = dplyr::coalesce(pff_team_lookup[opp], opp))

# --- one lens run: sp in {"man", "zone"}; the machine is identical,
#     only the value/prior columns swap.
league_one_split <- function(sp) {
  v25 <- if (sp == "man") "mg25" else "zg25"
  v24 <- if (sp == "man") "mg24" else "zg24"
  vpr <- if (sp == "man") "pr_man_grade" else "pr_zone_grade"
  vfc <- if (sp == "man") "man_grade_pctl" else "zone_grade_pctl"
  
  corps <- nflreadr::load_rosters(2026) %>%
    filter(position %in% pos_keep) %>%
    transmute(gsis_id, roster_name = full_name,
              team_name = dplyr::coalesce(pff_team_lookup[team], team),
              entry_year) %>%
    left_join(rec_id_bridge, by = "gsis_id") %>%
    left_join(usage_rec, by = "player_id") %>%
    left_join(band_26, by = "player_id") %>%
    left_join(p25 %>% select(player_id, qual_g25, V25 = all_of(v25)),
              by = "player_id") %>%
    left_join(p24 %>% select(player_id, V24 = all_of(v24)),
              by = "player_id") %>%
    filter(!is.na(band)) %>%
    mutate(usage_ord = pmax(dplyr::coalesce(routes_2025, 0),
                            dplyr::coalesce(routes_2024, 0))) %>%
    group_by(team_name) %>%
    arrange(desc(usage_ord), .by_group = TRUE) %>%
    mutate(corps_rank = dplyr::row_number()) %>%
    filter(corps_rank <= N_CORPS) %>% ungroup() %>%
    left_join(rec_rookie_prior %>% select(band, VPR = all_of(vpr)),
              by = "band") %>%
    mutate(w25  = pmin(dplyr::coalesce(qual_g25, 0L) / 10, 1),
           w25u = pmin(dplyr::coalesce(qual_g25, 0L) / 18, 1),
           g_bl = blend2(V25, V24, w25),
           g_f  = dplyr::coalesce(g_bl, VPR),
           prior_used = is.na(g_bl),
           u_bl = blend2(dplyr::coalesce(routes_2025, 0),
                         dplyr::coalesce(routes_2024, 0), w25u),
           usage_w = dplyr::if_else(u_bl > 0, u_bl, usage_ord))
  
  team26 <- corps %>%
    group_by(team_name) %>%
    summarise(v26 = weighted.mean(g_f, w = usage_w, na.rm = TRUE),
              prior_share = sum(usage_w[prior_used]) / sum(usage_w),
              .groups = "drop")
  
  faced <- receiving_func_base %>%
    filter(season == 2025, in_season(week), !is.na(player_id),
           routes >= X_QUAL) %>%
    mutate(def_t = stringr::str_remove(def_ssn, "2025$")) %>%
    left_join(rec_season_pctl_sos %>% filter(season == 2025) %>%
                select(player_id, F25 = all_of(vfc)), by = "player_id") %>%
    group_by(def_t) %>%
    summarise(faced25 = weighted.mean(F25, w = routes, na.rm = TRUE),
              unscored_share = sum(routes[is.na(F25)]) / sum(routes),
              .groups = "drop")
  
  opp_map %>%
    left_join(team26, by = c("opp" = "team_name")) %>%
    group_by(focal) %>%
    summarise(v26 = mean(v26, na.rm = TRUE),
              prior_share = mean(prior_share, na.rm = TRUE),
              .groups = "drop") %>%
    left_join(faced, by = c("focal" = "def_t")) %>%
    mutate(split = sp, d = v26 - faced25) %>%
    select(focal, split, v26, faced25, d, prior_share, unscored_share)
}

league_sys_both <- bind_rows(league_one_split("zone"),
                             league_one_split("man"))

# ------------------------------------------------------------
# RECEIPT 1 -- the zone lens reproduces the docx, exactly:
#   NE d_zone +.091, rank 1; league mean +.002, sd .034.
# ------------------------------------------------------------
z <- league_sys_both %>% filter(split == "zone")
cat("--- zone-lens reproduction check (docx receipts in parens) ---\n")
cat("NE d_zone:", round(z$d[z$focal == "NE"], 3), "(+.091) | NE rank:",
    rank(-z$d)[z$focal == "NE"], "(1) | league mean:",
    round(mean(z$d), 3), "(+.002) | league sd:",
    round(sd(z$d), 3), "(.034)\n")

# ------------------------------------------------------------
# RECEIPT 2 -- the two lenses, one frame. prior_share = share of
#   corps weight riding band priors; unscored_share = share of
#   faced-2025 routes with no pctl. Man runs thinner by design.
# ------------------------------------------------------------
league_wide <- league_sys_both %>%
  tidyr::pivot_wider(names_from = split,
                     values_from = c(v26, faced25, d,
                                     prior_share, unscored_share)) %>%
  mutate(rank_zone = rank(-d_zone),
         rank_man  = rank(-d_man)) %>%
  arrange(desc(d_zone))

cat("\n--- THE LEAGUE DIFFERENTIAL, BOTH LENSES (sorted by d_zone) ---\n")
print(league_wide %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))), n = 32)

cat("\n--- same frame, sorted by d_man (the man-lens ladder) ---\n")
print(league_wide %>% arrange(desc(d_man)) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))), n = 32)

# ------------------------------------------------------------
# THE SUMMARY -- league mean/sd + NE rank per lens, and whether
#   the two lenses tell the same story (cor across the 32).
# ------------------------------------------------------------
summ_ls <- league_sys_both %>%
  group_by(split) %>%
  summarise(league_mean_d = round(mean(d), 3),
            league_sd_d   = round(sd(d), 3),
            ne_d    = round(d[focal == "NE"], 3),
            ne_rank = rank(-d)[focal == "NE"],
            mean_prior_share    = round(mean(prior_share), 3),
            mean_unscored_share = round(mean(unscored_share), 3),
            .groups = "drop")

cat("\n--- league summary per lens ---\n")
print(summ_ls)

cat("\ncor(d_man, d_zone) across 32:",
    round(cor(league_wide$d_man, league_wide$d_zone), 3), "\n")
cat("AFC East on the man ladder (BUF/MIA/NE/NYJ ranks):",
    paste(league_wide %>% filter(focal %in% c("BUF","MIA","NE","NYJ")) %>%
            arrange(rank_man) %>%
            transmute(paste0(focal, " #", rank_man)) %>% pull(),
          collapse = ", "), "\n")
