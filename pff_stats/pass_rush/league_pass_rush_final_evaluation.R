# ============================================================
# NE 2026 -- OPPONENT PASS-RUSH, ADJUSTED EDITION (currency three)
# Andy directive 2026-08-17: one place to look things up -- player
# lookup, team boards, and the slate framework valued in the
# schedule-adjusted currency, AS AN OPTION beside the standard slate.
#
# FIREWALL AMENDMENT (Andy directive, 08-17): currency three still
# never enters any CURRENCY-ONE artifact. Adjusted editions are their
# own, always-labeled family. Membership, deltas, phantoms, and
# usage_w are taken UNCHANGED from rot_2026 -- same rotations, same
# weights -- so standard-vs-adjusted differences are valuation only.
#
# VOCABULARY (Andy 08-17): printed labels are raw (= currency one)
# and adj (= currency three). Session OBJECT names keep c1/c3
# provenance so cross-file contracts stay intact.
#
# GRADE LENS ONLY: currency three exists for TPS grade (the
# within-opponent rank). PRP / win-rate adjusted variants = round two.
#
# SOURCE ORDER: new_england_opp_pass_rush (or league_opp_pass_rush)
# run through section 9 -- needs rot_2026, faced_games_2025,
# cmp_prush_slate -- then league_pass_rush_evaluating_currency_three
# (needs prush_c3_pctl, prush_game_c3), then this file.
# ============================================================

needed_adj <- c("rot_2026", "ledger", "faced_games_2025",
                "cmp_prush_slate", "prush_c3_pctl", "prush_game_c3",
                "entry_years_def", "blend2", "sched_2026")
missing_adj <- needed_adj[!vapply(needed_adj, exists, logical(1))]
if (length(missing_adj)) stop("missing session objects: ",
                              paste(missing_adj, collapse = ", "),
                              " -- see SOURCE ORDER in header")

# ------------------------------------------------------------
# 1. c3 entry-year prior by band (fills rookies / phantoms /
#    no-c3 members, mirroring the currency-one prior law)
# ------------------------------------------------------------

c3_rookie_prior <- prush_c3_pctl %>%
  inner_join(entry_years_def, by = "player_id") %>%
  filter(season == entry_year, entry_year >= 2017) %>%
  group_by(band) %>%
  summarise(pr_c3 = median(c3_pctl, na.rm = TRUE),
            n_entry = dplyr::n(), .groups = "drop")

cat("\n--- c3 entry-year prior by band ---\n")
print(c3_rookie_prior)

# ------------------------------------------------------------
# 2. LOOKUPS -- the "where is TJ Watt" layer
# ------------------------------------------------------------

player_c3 <- function(who) {
  if (is.numeric(who)) {
    d <- prush_c3_pctl %>% filter(player_id %in% who)
  } else {
    d <- prush_c3_pctl %>%
      filter(tolower(player) == tolower(who))        # full name first
    if (nrow(d) == 0) {
      d <- prush_c3_pctl %>%
        filter(grepl(who, player, ignore.case = TRUE))
    }
  }
  if (nrow(d) == 0) { cat("no match\n"); return(invisible(NULL)) }
  ids <- d %>% group_by(player_id, player) %>%
    summarise(seasons = paste0(min(season), "-", max(season)),
              career_tps = sum(tps), .groups = "drop")
  if (nrow(ids) > 1) {
    if (dplyr::n_distinct(ids$player) > 1) {
      cat("\n--", nrow(ids), "players match. Re-run with the full name: --\n")
      print(ids %>% select(player, seasons, career_tps), n = Inf)
    } else {
      cat("\n-- same name, different players (rare). Re-run",
          "player_c3(<player_id>): --\n")
      print(ids, n = Inf)
    }
    return(invisible(ids))
  }
  d <- d %>% arrange(season) %>% rename(adj = c3_pctl)
  if (exists("prush_tps_season_pctl_sos")) {
    d <- d %>%
      left_join(prush_tps_season_pctl_sos %>%
                  select(player_id, season,
                         raw = tps_grade_sos_pctl),
                by = c("player_id", "season")) %>%
      mutate(d = round(adj - raw, 3))
  }
  print(d %>% select(player, season, band, qual_g, tps, med_vsopp,
                     dplyr::any_of(c("raw")), adj,
                     dplyr::any_of(c("d"))),
        n = Inf)
  invisible(d)
}

team_c3 <- function(team, ssn = 2025) {
  yrs <- (ssn - 2):ssn
  ids <- prush_game_c3 %>%
    filter(season == ssn) %>%
    count(player_id, def_ssn, wt = tps_snaps, name = "sn") %>%
    group_by(player_id) %>%
    slice_max(sn, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    filter(def_ssn == paste0(team, ssn)) %>%
    select(player_id)
  hist <- prush_c3_pctl %>%
    filter(season %in% yrs) %>%
    semi_join(ids, by = "player_id")
  wide_adj <- hist %>%
    select(player_id, season, c3_pctl) %>%
    tidyr::pivot_wider(names_from = season, values_from = c3_pctl,
                       names_prefix = "adj_", names_sort = TRUE)
  b <- hist %>%
    filter(season == ssn) %>%
    select(player_id, player, band, qual_g, tps) %>%
    left_join(wide_adj, by = "player_id")
  hist_raw <- NULL
  if (exists("prush_tps_season_pctl_sos")) {
    hist_raw <- prush_tps_season_pctl_sos %>%
      filter(season %in% yrs) %>%
      semi_join(ids, by = "player_id") %>%
      select(player_id, season, raw = tps_grade_sos_pctl)
    b <- b %>%
      left_join(hist_raw %>%
                  tidyr::pivot_wider(names_from = season,
                                     values_from = raw,
                                     names_prefix = "raw_",
                                     names_sort = TRUE),
                by = "player_id") %>%
      mutate(d = round(.data[[paste0("adj_", ssn)]] -
                         .data[[paste0("raw_", ssn)]], 3))
  }
  b <- b %>% arrange(band, desc(.data[[paste0("adj_", ssn)]]))
  cat("\n==", team, ssn, "-- rushers, raw + adj,", min(yrs), "-", ssn,
      "(values travel with the player) ==\n")
  print(b %>% select(player, band, qual_g, tps,
                     dplyr::starts_with("raw_"),
                     dplyr::starts_with("adj_"),
                     dplyr::any_of(c("d"))), n = Inf)
  cat("\n-- band means per season, tps-weighted, this", ssn,
      "group (band = that season's band) --\n")
  mm <- hist %>%
    group_by(band, season) %>%
    summarise(adj = round(weighted.mean(c3_pctl, w = tps), 3),
              .groups = "drop")
  if (!is.null(hist_raw)) {
    mm <- mm %>%
      left_join(hist %>%
                  inner_join(hist_raw, by = c("player_id", "season")) %>%
                  group_by(band, season) %>%
                  summarise(raw = round(weighted.mean(raw, w = tps), 3),
                            .groups = "drop"),
                by = c("band", "season")) %>%
      select(band, season, raw, adj)
  }
  print(mm, n = Inf)
  invisible(b)
}

team_c3_26 <- function(tm) {
  # 2026 roster view, any of the 32 teams (rot_2026/ledger cover NE's
  # opponents only -- this goes the roster route). History travels
  # with the player; adj_proj = the same blend2 + prior the slate uses.
  if (!exists("pff_team_lookup")) stop("needs pff_team_lookup in session")
  xw <- if (exists("def_xwalk")) {
    def_xwalk
  } else if (exists("combined_ids_defense")) {
    combined_ids_defense %>% filter(!is.na(gsis_id)) %>%
      distinct(player_id, gsis_id)
  } else stop("needs def_xwalk or combined_ids_defense in session")
  ros <- nflreadr::load_rosters(2026) %>%
    filter(position %in% c("DL","DE","DT","NT","EDGE",
                           "OLB","LB","ILB","MLB")) %>%
    transmute(gsis_id, player = full_name, entry_year,
              team_name = dplyr::coalesce(pff_team_lookup[team], team)) %>%
    filter(team_name == tm) %>%
    left_join(xw, by = "gsis_id")
  yrs <- 2023:2025
  band26 <- prush_c3_pctl %>%
    filter(season %in% c(2024, 2025)) %>%
    semi_join(ros %>% filter(!is.na(player_id)), by = "player_id") %>%
    select(player_id, season, band) %>%
    tidyr::pivot_wider(names_from = season, values_from = band,
                       names_prefix = "b") %>%
    mutate(band = dplyr::coalesce(b2025, b2024)) %>%
    select(player_id, band)
  wide_adj <- prush_c3_pctl %>%
    filter(season %in% yrs) %>%
    semi_join(ros %>% filter(!is.na(player_id)), by = "player_id") %>%
    select(player_id, season, c3_pctl) %>%
    tidyr::pivot_wider(names_from = season, values_from = c3_pctl,
                       names_prefix = "adj_", names_sort = TRUE)
  wide_raw <- NULL
  if (exists("prush_tps_season_pctl_sos")) {
    wide_raw <- prush_tps_season_pctl_sos %>%
      filter(season %in% yrs) %>%
      semi_join(ros %>% filter(!is.na(player_id)), by = "player_id") %>%
      select(player_id, season, raw = tps_grade_sos_pctl) %>%
      tidyr::pivot_wider(names_from = season, values_from = raw,
                         names_prefix = "raw_", names_sort = TRUE)
  }
  proj <- prush_c3_pctl %>%
    filter(season == 2025) %>%
    select(player_id, a25 = c3_pctl, qg25 = qual_g) %>%
    full_join(prush_c3_pctl %>% filter(season == 2024) %>%
                select(player_id, a24 = c3_pctl),
              by = "player_id")
  b <- ros %>%
    select(player, player_id, entry_year) %>%
    left_join(band26, by = "player_id") %>%
    left_join(proj,   by = "player_id") %>%
    left_join(c3_rookie_prior %>% select(band, pr_c3), by = "band") %>%
    mutate(w25 = pmin(dplyr::coalesce(qg25, 0L) / 10, 1),
           adj_proj = round(dplyr::coalesce(blend2(a25, a24, w25),
                                            pr_c3), 3),
           note = dplyr::case_when(
             entry_year == 2026 ~ "rookie",
             is.na(player_id)   ~ "no_pff_id",
             is.na(band)        ~ "no_24_25_data",
             TRUE ~ "")) %>%
    left_join(wide_adj, by = "player_id")
  if (!is.null(wide_raw)) b <- b %>% left_join(wide_raw, by = "player_id")
  b <- b %>% arrange(band, desc(adj_proj))
  cat("\n==", tm, "2026 roster -- raw + adj history 2023-2025,",
      "adj_proj = slate valuation ==\n")
  print(b %>% select(player, band, entry_year,
                     dplyr::starts_with("raw_"),
                     dplyr::starts_with("adj_"),
                     adj_proj, note), n = Inf)
  invisible(b)
}

# ------------------------------------------------------------
# 3. FACED 2025 -- adjusted valuation. Same defender-games, same
#    snaps, same fill law; only the currency swaps.
# ------------------------------------------------------------

faced_c3_2025 <- faced_games_2025 %>%
  select(player_id, player, week, band, g_snaps) %>%
  left_join(prush_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, c3_25 = c3_pctl),
            by = "player_id") %>%
  left_join(c3_rookie_prior %>% select(band, pr_c3), by = "band") %>%
  mutate(fill_c3 = is.na(c3_25),
         c3_25   = dplyr::coalesce(c3_25, pr_c3))

faced_band_c3 <- faced_c3_2025 %>%
  group_by(band) %>%
  summarise(tot_snaps = sum(g_snaps),
            fill_share = round(sum(g_snaps[fill_c3]) / tot_snaps, 3),
            adj_25 = weighted.mean(c3_25, w = g_snaps),
            .groups = "drop")

cat("\n--- faced 2025, adj valuation by band ---\n")
print(faced_band_c3)

# ------------------------------------------------------------
# 4. 2026 PROJECTION -- adjusted valuation. rot_2026 UNCHANGED:
#    same members, same phantoms, same usage_w.
# ------------------------------------------------------------

rot_c3 <- rot_2026 %>%
  # rot_2026's final select keeps display columns only -- player_id is
  # dropped before bind_rows (canon pattern). Recover it via ledger;
  # phantoms don't match, stay NA, and fall to the prior as designed.
  left_join(ledger %>% distinct(team_name, roster_name, player_id),
            by = c("team_name", "roster_name")) %>%
  left_join(prush_c3_pctl %>% filter(season == 2025) %>%
              select(player_id, c3_25p = c3_pctl, qg25_c3 = qual_g),
            by = "player_id") %>%
  left_join(prush_c3_pctl %>% filter(season == 2024) %>%
              select(player_id, c3_24p = c3_pctl),
            by = "player_id") %>%
  left_join(c3_rookie_prior %>% select(band, pr_c3), by = "band") %>%
  mutate(w25_c3 = pmin(dplyr::coalesce(qg25_c3, 0L) / 10, 1),
         c3_bl  = blend2(c3_25p, c3_24p, w25_c3),
         prior_used_c3 = is.na(c3_bl),
         c3_f   = dplyr::coalesce(c3_bl, pr_c3))

slate_rows_c3 <- tibble::tibble(team_name = sched_2026) %>%
  left_join(rot_c3, by = "team_name", relationship = "many-to-many")

slate_band_c3 <- slate_rows_c3 %>%
  group_by(band) %>%
  summarise(interp_share = round(sum(usage_w[prior_used_c3]) /
                                   sum(usage_w), 3),
            adj_26 = weighted.mean(c3_f, w = usage_w),
            .groups = "drop")

# ------------------------------------------------------------
# 5. THE ANSWER, BOTH EDITIONS SIDE BY SIDE -- does the slate verdict
#    survive skill-cleaning? c1 columns pulled from the live
#    cmp_prush_slate; c3 columns from sections 3-4.
# ------------------------------------------------------------

cmp_adj <- cmp_prush_slate %>%
  select(band, raw_25 = grade_25, raw_26 = grade_26, d_raw = d_grade) %>%
  left_join(faced_band_c3 %>% select(band, adj_25, fill_share),
            by = "band") %>%
  left_join(slate_band_c3, by = "band") %>%
  mutate(d_adj = adj_26 - adj_25) %>%
  arrange(band)

cat("\n--- SLATE, raw vs adj (grade lens) ---\n")
print(cmp_adj, n = Inf)

prush_adj_gt <- cmp_adj %>%
  select(band, raw_25, raw_26, d_raw, adj_25, adj_26, d_adj,
         fill_share, interp_share) %>%
  gt() %>%
  tab_spanner(label = "Raw",
              columns = c(raw_25, raw_26, d_raw)) %>%
  tab_spanner(label = "Adjusted (same-slate)",
              columns = c(adj_25, adj_26, d_adj)) %>%
  cols_label(band = "", raw_25 = "'25", raw_26 = "'26", d_raw = "\u0394",
             adj_25 = "'25", adj_26 = "'26", d_adj = "\u0394",
             fill_share = "fill %", interp_share = "interp %") %>%
  fmt_percent(columns = c(raw_25, raw_26, adj_25, adj_26,
                          fill_share, interp_share), decimals = 0) %>%
  fmt_percent(columns = c(d_raw, d_adj), decimals = 0, force_sign = TRUE) %>%
  data_color(columns = c(d_raw, d_adj),
             fn = scales::col_numeric(c("#6baed6", "#f7f7f7", "#C60C30"),
                                      domain = c(-0.08, 0.08)),
             autocolor_text = TRUE) %>%
  tab_header(title = "Opposing pass-rush slate \u2014 standard vs adjusted",
             subtitle = "same rotations, phantoms, and snap weights in both editions | adjusted = currency three (same-slate skill), grade lens only | higher = harder for Maye's pocket") %>%
  tab_options(table.font.size = px(12), data_row.padding = px(3),
              column_labels.font.weight = "bold")
prush_adj_gt

rec_fig_pd_adj <- cmp_adj %>%
  select(band, adj_25, adj_26) %>%
  mutate(band = factor(band, levels = c("DI", "ED")))

plot_prush_slate_adj <- ggplot(rec_fig_pd_adj, aes(y = band)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey45") +
  geom_segment(aes(x = adj_25, xend = adj_26, yend = band),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               linewidth = 1, color = "grey60") +
  geom_point(aes(x = adj_25), shape = 1, size = 3.2, stroke = 1.2,
             color = "grey55") +
  geom_point(aes(x = adj_26), shape = 16, size = 2.6, color = "#002244") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(title = "NE offense - opposing pass-rush slate, ADJUSTED edition",
       subtitle = "adj = same-slate skill | open = 2025 faced, solid = 2026 projected | same rotations + weights as the raw slate | grade lens only",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey40", size = 8.5),
        panel.grid.major.y = element_blank())
plot_prush_slate_adj

# ------------------------------------------------------------
# 6. lookup usage printed at source (visible on dark themes);
#    nothing executes a lookup.
# ------------------------------------------------------------
cat("\nlookups ready (console tools, full names always):\n",
    '  player_c3("T.J. Watt")\n',
    '  team_c3("PIT")         # 2025 room, 3yr history\n',
    '  team_c3_26("NE")       # 2026 roster, history + slate valuation\n',
    '  slate_view("PIT")      # any team\'s slate, raw + adj, machine-only\n',
    sep = "")

# ------------------------------------------------------------
# 7. slate_view(focal) -- the slate question pointed at ANY team.
#    Identity via the schedule join in PFF space (never startsWith --
#    the league-mode law). MACHINE-ONLY: no deltas/tribbles for any
#    focal, so slate_view("NE") differs slightly from the signed NE
#    artifacts (cmp_prush_slate / cmp_adj) -- those remain canon.
#    Requires the league tail's _lg objects in session
#    (league_opp_pass_rush_schedule.R run end to end).
# ------------------------------------------------------------

lg_needed <- c("qual_lg", "cur_lg", "modal_lg", "opp25_lg", "opp26_lg",
               "ros_lg", "use_lg", "band_lg", "prior_lg", "slot_lg",
               "slot_med_lg", "fb_lg", "ph_lg", "NB_LG")
if (all(vapply(lg_needed, exists, logical(1)))) {
  rot26_full <- ros_lg %>%
    left_join(use_lg,  by = "player_id") %>%
    left_join(band_lg, by = "player_id") %>%
    filter(!is.na(band)) %>%
    mutate(uo = pmax(dplyr::coalesce(qs_2025, 0),
                     dplyr::coalesce(qs_2024, 0))) %>%
    group_by(team, band) %>%
    arrange(desc(uo), .by_group = TRUE) %>%
    mutate(rk = dplyr::row_number()) %>%
    filter(rk <= NB_LG) %>%
    ungroup() %>%
    left_join(slot_lg,     by = c("team", "band", "rk")) %>%
    left_join(slot_med_lg, by = c("band", "rk")) %>%
    left_join(prior_lg,    by = "band") %>%
    left_join(prush_c3_pctl %>% filter(season == 2025) %>%
                select(player_id, a25 = c3_pctl, qg25a = qual_g),
              by = "player_id") %>%
    left_join(prush_c3_pctl %>% filter(season == 2024) %>%
                select(player_id, a24 = c3_pctl),
              by = "player_id") %>%
    left_join(c3_rookie_prior %>% select(band, pr_c3), by = "band") %>%
    mutate(w25  = pmin(dplyr::coalesce(qg_2025, 0L) / 10, 1),
           gf_raw = dplyr::coalesce(blend2(g_pctl_2025, g_pctl_2024, w25),
                                    prg),
           w25a = pmin(dplyr::coalesce(qg25a, 0L) / 10, 1),
           gf_adj = dplyr::coalesce(blend2(a25, a24, w25a), pr_c3),
           uw = dplyr::if_else(uo > 0, uo,
                               dplyr::coalesce(sn_slot, sn_med, fb_lg))) %>%
    select(team, band, gf_raw, gf_adj, uw) %>%
    bind_rows(ph_lg %>%
                rename(gf_raw = gf) %>%
                left_join(c3_rookie_prior %>% select(band, pr_c3),
                          by = "band") %>%
                mutate(gf_adj = pr_c3) %>%
                select(team, band, gf_raw, gf_adj, uw))
  cat("\nrot26_full built:", nrow(rot26_full),
      "member rows across 32 teams (machine-only)\n")
} else {
  cat("\n[slate_view disabled] league tail objects missing:",
      paste(lg_needed[!vapply(lg_needed, exists, logical(1))],
            collapse = ", "), "\n")
}

slate_view <- function(focal) {
  if (!exists("rot26_full")) {
    stop("slate_view needs the league tail in session -- run ",
         "league_opp_pass_rush_schedule.R end to end first")
  }
  f <- qual_lg %>%
    filter(season == 2025) %>%
    transmute(player_id, week,
              team = stringr::str_remove(def_ssn, "2025$"),
              sn = true_pass_set_snap_counts_pass_rush) %>%
    inner_join(opp25_lg %>% filter(focal == !!focal) %>%
                 select(opp, week_pff),
               by = c("team" = "opp", "week" = "week_pff")) %>%
    left_join(modal_lg %>% filter(season == 2025) %>%
                select(player_id, band), by = "player_id") %>%
    filter(!is.na(band)) %>%
    left_join(cur_lg %>% filter(season == 2025) %>%
                select(player_id, raw = g_pctl), by = "player_id") %>%
    left_join(prush_c3_pctl %>% filter(season == 2025) %>%
                select(player_id, adj = c3_pctl), by = "player_id") %>%
    left_join(prior_lg, by = "band") %>%
    left_join(c3_rookie_prior %>% select(band, pr_c3), by = "band") %>%
    mutate(raw = dplyr::coalesce(raw, prg),
           adj = dplyr::coalesce(adj, pr_c3)) %>%
    group_by(band) %>%
    summarise(raw_25 = round(weighted.mean(raw, w = sn), 3),
              adj_25 = round(weighted.mean(adj, w = sn), 3),
              .groups = "drop")
  p <- opp26_lg %>%
    filter(focal == !!focal) %>%
    left_join(rot26_full, by = c("opp" = "team"),
              relationship = "many-to-many") %>%
    group_by(band) %>%
    summarise(raw_26 = round(weighted.mean(gf_raw, w = uw), 3),
              adj_26 = round(weighted.mean(gf_adj, w = uw), 3),
              .groups = "drop")
  out <- f %>% left_join(p, by = "band") %>%
    mutate(d_raw = raw_26 - raw_25, d_adj = adj_26 - adj_25) %>%
    select(band, raw_25, raw_26, d_raw, adj_25, adj_26, d_adj)
  cat("\n==", focal,
      "-- opposing pass-rush slate, raw + adj (MACHINE-ONLY:",
      "no deltas; playoffs in faced; fill at priors) ==\n")
  print(out, n = Inf)
  invisible(out)
}

# ------------------------------------------------------------
# Checkpoint (after eyeball):
# ------------------------------------------------------------
# ggsave("prush_slate_adjusted.png", plot_prush_slate_adj,
#        width = 8, height = 4.5, dpi = 200)
# gtsave(prush_adj_gt, "prush_slate_standard_vs_adjusted.png", vwidth = 900)