# dict filter: named list keyed by rank_grp, e.g. list(A = c(1,2), B = c(3,4))
apply_cluster_dict <- function(d, dict, cluster_col, grp_col = "rank_grp") {
  if (is.null(dict)) return(d)
  allow_key <- paste(rep(names(dict), lengths(dict)),
                     as.character(unlist(dict, use.names = FALSE)), sep = "|")
  row_key   <- paste(d[[grp_col]], as.character(d[[cluster_col]]), sep = "|")
  d[row_key %in% allow_key, , drop = FALSE]
}


situation_feats <- c("q20_xpass_diff","q35_xpass_diff","q50_xpass_diff","q65_xpass_diff","q80_xpass_diff")
gap_feats       <- c("center_perc","guard_perc","tackle_perc","end_perc")

# recover centers (mean of scaled cols by cluster) + scaling (mean/sd of raw cols) from a *_full df
build_cluster_ref <- function(full_df, raw_cols) {
  scaled_cols <- paste0(raw_cols, "_scaled")
  center_vec  <- vapply(raw_cols, function(cl) mean(full_df[[cl]], na.rm = TRUE), numeric(1))
  scale_vec   <- vapply(raw_cols, function(cl) sd(full_df[[cl]],   na.rm = TRUE), numeric(1))
  centers <- full_df %>%
    group_by(cluster) %>%
    summarise(across(all_of(scaled_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  list(raw_cols = raw_cols, center_vec = center_vec, scale_vec = scale_vec, centers = centers)
}

situation_ref <- list(
  A = build_cluster_ref(rusher_xpass_diff_a_full, situation_feats),
  B = build_cluster_ref(rusher_xpass_diff_b_full, situation_feats),
  C = build_cluster_ref(rusher_xpass_diff_c_full, situation_feats)
)
gap_ref <- list(
  A = build_cluster_ref(run_gap_a_full, gap_feats),
  B = build_cluster_ref(run_gap_b_full, gap_feats),
  C = build_cluster_ref(run_gap_c_full, gap_feats)
)

# VERIFY THESE: kmeans labels are seed-dependent, and your gap profiles printed via arrange(desc(center)) -
# so cluster integer -> name may be off. cross-check against the *_full cluster ids before trusting.
situation_labels <- list(
  A = c(`1`="BELLCOW", `2`="MID", `3`="LONG YARDAGE", `4`="LOW AF"),
  B = c(`1`="LATER/LONG", `2`="MID", `3`="SHORT YARDAGE", `4`="BELLCOW MID", `5`="EARLY DOWN"),
  C = c(`1`="SHORT YARDAGE", `2`="EARLY DOWN", `3`="LONG YARDAGE", `4`="MID")
)
gap_labels <- list(
  A = c(`1`="GUARD/TACKLE", `2`="GUARD", `3`="OUTSIDE", `4`="CENTER/TACKLE", `5`="TACKLE/OUTSIDE", `6`="CENTER/OUTSIDE", `7`="CENTER", `8`="GUARD/TACKLE"),
  B = c(`1`="OUTSIDE", `2`="TACKLE", `3`="CENTER", `4`="GUARD"),
  C = c(`1`="OUTSIDE", `2`="GUARD", `3`="TACKLE", `4`="CENTER")
)

lbl <- function(map, rg, cl) {
  v <- map[[rg]][as.character(cl)]
  if (is.null(v) || is.na(v)) NA_character_ else unname(v)
}

# distance from one raw input vector to every center in its rank_grp (scaled in that grp's space)
cluster_closest <- function(raw_vec, rank_grp, ref) {
  r <- ref[[rank_grp]]
  if (is.null(r)) { message("no ref for rank_grp ", rank_grp); return(tibble()) }
  scaled_in   <- (raw_vec - r$center_vec) / r$scale_vec
  scaled_cols <- paste0(r$raw_cols, "_scaled")
  cmat <- as.matrix(r$centers[scaled_cols])
  d    <- sqrt(rowSums(sweep(cmat, 2, scaled_in, "-")^2))
  tibble(rank_grp = rank_grp, cluster = r$centers$cluster, distance = d) %>% arrange(distance)
}

# player input rows (gsis id); may span multiple rank_grps in a season
get_rusher_cluster_inputs <- function(rusher_id, season_in,
                                      df_situation = rusher_xpass_diff_df,
                                      df_gap       = base_run_gap_cluster) {
  sit <- df_situation %>% ungroup() %>% filter(rusher_player_id == rusher_id, season %in% season_in)
  gap <- df_gap       %>% ungroup() %>% filter(rusher_player_id == rusher_id, season %in% season_in)
  if (nrow(sit) == 0 && nrow(gap) == 0) {
    message("No rusher rows for ", rusher_id, " season(s) ", paste(season_in, collapse = ","))
    return(invisible(NULL))
  }
  list(situation = sit, gap = gap)
}

# closest clusters per rank_grp appeared in (single season), trimmed by distance_mult - border-aware
get_rusher_cluster_neighbors <- function(rusher_id, season_in,
                                         distance_mult    = 1.75,
                                         situation_ref_in = situation_ref,
                                         gap_ref_in       = gap_ref,
                                         df_situation = rusher_xpass_diff_df,
                                         df_gap       = base_run_gap_cluster) {
  
  if (length(season_in) != 1) stop("get_rusher_cluster_neighbors takes ONE season. Use get_rusher_identity_history for multi-season.")
  
  inp <- get_rusher_cluster_inputs(rusher_id, season_in, df_situation, df_gap)
  if (is.null(inp)) return(invisible(NULL))
  
  score_rows <- function(rows_df, feats, ref) {
    if (nrow(rows_df) == 0) return(tibble())
    bind_rows(lapply(seq_len(nrow(rows_df)), function(i) {
      cluster_closest(as.numeric(rows_df[i, feats]), rows_df$rank_grp[i], ref) %>%
        filter(distance <= min(distance) * distance_mult) %>%
        mutate(season = rows_df$season[i], .before = 1)
    }))
  }
  
  list(
    situation = score_rows(inp$situation, situation_feats, situation_ref_in),
    gap       = score_rows(inp$gap,       gap_feats,       gap_ref_in)
  )
}

# multi-season assigned clusters - shows the A<->B wandering directly
get_rusher_identity_history <- function(rusher_id, season_in,
                                        df_situation_assign = situation_cluster_df,
                                        df_gap_assign       = gap_cluster_df,
                                        df_stats            = rush_stats_final) {
  
  sit <- df_situation_assign %>% ungroup() %>%
    filter(rusher_player_id == rusher_id, season %in% season_in) %>%
    group_by(season, rank_grp) %>%
    summarise(situation_cluster = first(cluster), .groups = "drop")
  
  gap <- df_gap_assign %>% ungroup() %>%
    filter(rusher_player_id == rusher_id, season %in% season_in) %>%
    group_by(season, rank_grp) %>%
    summarise(gap_cluster = first(cluster), .groups = "drop")
  
  stats <- df_stats %>% ungroup() %>%
    filter(rusher_player_id == rusher_id, season %in% season_in) %>%
    group_by(season, rank_grp) %>%
    summarise(
      xtd_percentile = mean(xtd_percentile, na.rm = TRUE),
      gap_z          = mean(gap_z, na.rm = TRUE),
      .groups = "drop"
    )
  
  sit %>%
    full_join(gap,   by = c("season", "rank_grp")) %>%
    full_join(stats, by = c("season", "rank_grp")) %>%
    arrange(season, rank_grp)
}

get_offense_rusher_identity <- function(qbgrp_ssn_in,
                                        rank_grp_input = NULL,
                                        rush_prop_min  = 0,
                                        rush_prop_max  = 1,
                                        df             = rush_stats_final) {
  
  d <- df %>% ungroup() %>%
    filter(qbgrp_ssn %in% qbgrp_ssn_in,
           rush_proportion >= rush_prop_min,
           rush_proportion <= rush_prop_max)
  
  if (!is.null(rank_grp_input)) d <- d %>% filter(rank_grp %in% rank_grp_input)
  
  d %>%
    group_by(qbgrp_ssn, rank_grp, rusher_player_id, player) %>%
    summarise(
      n_games        = n(),
      rush_prop_avg  = mean(rush_proportion, na.rm = TRUE),
      xtd_percentile = mean(xtd_percentile, na.rm = TRUE),
      gap_z          = mean(gap_z, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(qbgrp_ssn, rank_grp, desc(rush_prop_avg))
}


View(rush_stats_final %>% filter(qbgrp_ssn == "SEADarnold-2025") %>% arrange(player, week))
View(combined_ids %>% filter(team == "SEA", season == 2025) %>% select(player, player_id, team, season, gsis_id) %>% distinct())

get_rusher_cluster_neighbors("00-0038134", 2025, distance_mult = 2.25)   # situation + gap, per rank_grp
get_rusher_cluster_neighbors("00-0038134", 2024, distance_mult = 2.25)
get_rusher_cluster_neighbors("00-0039165", 2025, distance_mult = 2.25)   # situation + gap, per rank_grp
get_rusher_cluster_neighbors("00-0039165", 2024, distance_mult = 2.25)
get_rusher_identity_history("00-0039165", c(2025))         # drift across seasons + rank_grps
get_offense_rusher_identity(c("SEADarnold-2025"), rush_prop_min = .38, rush_prop_max = .7)



compare_rusher_cohort <- function(focal_player_id        = NULL,
                                  focal_season           = NULL,
                                  rush_prop_vec          = c(0, 1),
                                  position_group_input   = NULL,
                                  rank_grp_input         = NULL,
                                  gap_cluster_dict       = NULL,
                                  situation_cluster_dict = NULL,
                                  pos_rank_vec           = c(1, 99),
                                  team_rank_vec          = c(1, 99),
                                  gap_z_vec              = c(-Inf, Inf),
                                  gap_z_na               = FALSE,
                                  xtd_vec_input          = c(-Inf, Inf),
                                  xtd_na                 = FALSE,
                                  season_filter          = NULL,
                                  grain                  = c("season", "game"),
                                  drop_part_cols         = FALSE,
                                  df                     = rush_stats_final) {
  
  grain <- match.arg(grain)
  
  d <- df %>% ungroup() %>%
    filter(rush_proportion >= rush_prop_vec[1], rush_proportion <= rush_prop_vec[2],
           pos_rank        >= pos_rank_vec[1],  pos_rank        <= pos_rank_vec[2],
           team_rank       >= team_rank_vec[1], team_rank       <= team_rank_vec[2])
  
  if (!is.null(season_filter))        d <- d %>% filter(season         %in% season_filter)
  if (!is.null(position_group_input)) d <- d %>% filter(position_group %in% position_group_input)
  if (!is.null(rank_grp_input))       d <- d %>% filter(rank_grp       %in% rank_grp_input)
  
  d <- d %>% apply_cluster_dict(gap_cluster_dict,       "gap_cluster")
  d <- d %>% apply_cluster_dict(situation_cluster_dict, "situation_cluster")
  
  d <- d %>% filter(
    (gap_z          >= gap_z_vec[1]     & gap_z          <= gap_z_vec[2])     | (gap_z_na & is.na(gap_z)),
    (xtd_percentile >= xtd_vec_input[1] & xtd_percentile <= xtd_vec_input[2]) | (xtd_na   & is.na(xtd_percentile))
  )
  
  d <- d %>%
    mutate(
      pbp_ypc_oe  = ypc - pbp_xypc,
      part_ypc_oe = ypc - part_xypc
    )
  
  if (grain == "season") {
    out <- d %>%
      group_by(player_id, player, season) %>%
      summarise(
        n_weeks        = n(),
        total_attempts = sum(attempts, na.rm = TRUE),
        rush_prop_avg  = weighted.mean(rush_proportion, attempts, na.rm = TRUE),
        ypc            = weighted.mean(ypc,       attempts, na.rm = TRUE),
        ybc            = weighted.mean(ybc,       attempts, na.rm = TRUE),
        yac            = weighted.mean(yac,       attempts, na.rm = TRUE),
        pbp_xypc       = weighted.mean(pbp_xypc,  attempts, na.rm = TRUE),
        part_xypc      = weighted.mean(part_xypc, attempts, na.rm = TRUE),
        pbp_xtd        = sum(pbp_xtd,  na.rm = TRUE),
        part_xtd       = sum(part_xtd, na.rm = TRUE),
        pbp_xtd_share  = weighted.mean(pbp_xtd_share,  attempts, na.rm = TRUE),
        part_xtd_share = weighted.mean(part_xtd_share, attempts, na.rm = TRUE),
        xtd_percentile = mean(xtd_percentile, na.rm = TRUE),
        gap_z          = mean(gap_z, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        pbp_ypc_oe  = ypc - pbp_xypc,
        part_ypc_oe = ypc - part_xypc
      )
  } else {
    out <- d %>%
      select(player_id, player, season, week, team, qbgrp_ssn, def_ssn,
             rank_grp, situation_cluster, gap_cluster,
             attempts, rush_proportion,
             ypc, ybc, yac, pbp_xypc, part_xypc,
             pbp_ypc_oe, part_ypc_oe,
             pbp_xtd, part_xtd, pbp_xtd_share, part_xtd_share,
             xtd_percentile, gap_z)
  }
  
  if (!is.null(focal_player_id)) {
    out <- out %>%
      mutate(is_focal = if (!is.null(focal_season)) {
        player_id == focal_player_id & season == focal_season
      } else {
        player_id == focal_player_id
      })
    out <- if (grain == "season") out %>% arrange(desc(is_focal), desc(rush_prop_avg))
    else                   out %>% arrange(desc(is_focal), season, week)
  } else {
    out <- out %>% mutate(is_focal = FALSE)
    out <- if (grain == "season") out %>% arrange(desc(rush_prop_avg))
    else                   out %>% arrange(season, week, player)
  }
  
  if (drop_part_cols) out <- out %>% select(-starts_with("part_"))
  
  out
}

compare_rusher_cohort(focal_player_id        = 97630,
                      focal_season           = 2025,
                      rush_prop_vec          = c(.35, .65),   # PRIMARY usage filter (rush_proportion)
                      position_group_input   = c("HB"),      # HB / QB / REC
                      rank_grp_input         = c("A", "B"),      # A / B / C (standalone)
                      gap_cluster_dict       = list(A = c(1,2,6), B = c(1,2,3,4)),      # named list keyed by rank_grp
                      situation_cluster_dict = list(A = c(1,2), B = c(2,5)),      # named list keyed by rank_grp
                      pos_rank_vec           = c(1, 2),
                      team_rank_vec          = c(1, 2),
                      gap_z_vec              = c(-99, 0),
                      gap_z_na               = FALSE,
                      xtd_vec_input          = c(0, 70),# xtd_percentile
                      xtd_na                 = FALSE,
                      season_filter          = NULL,
                      grain                  = c("season", "game"),
                      drop_part_cols         = FALSE,
                      df                     = rush_stats_final)


charbonnet_season_view <- compare_rusher_cohort(focal_player_id        = 97630,
                                               focal_season           = 2025,
                                               rush_prop_vec          = c(.25, .65),   # PRIMARY usage filter (rush_proportion)
                                               position_group_input   = c("HB"),      # HB / QB / REC
                                               rank_grp_input         = c("A", "B"),      # A / B / C (standalone)
                                               gap_cluster_dict       = list(A = c(1,2,6), B = c(1,2,3,4)),      # named list keyed by rank_grp
                                               situation_cluster_dict = list(A = c(1,2), B = c(2,5)),      # named list keyed by rank_grp
                                               pos_rank_vec           = c(1, 2),
                                               team_rank_vec          = c(1, 2),
                                               gap_z_vec              = c(-99, 0),
                                               gap_z_na               = FALSE,
                                               xtd_vec_input          = c(0, 70),# xtd_percentile
                                               xtd_na                 = FALSE,
                                               season_filter          = NULL,
                                               grain                  = c("season"),
                                               drop_part_cols         = FALSE,
                                               df                     = rush_stats_final)

charbonnet_game_view <- compare_rusher_cohort(focal_player_id        = 97630,
                                             focal_season           = 2025,
                                             rush_prop_vec          = c(.25, .65),   # PRIMARY usage filter (rush_proportion)
                                             position_group_input   = c("HB"),      # HB / QB / REC
                                             rank_grp_input         = c("A", "B"),      # A / B / C (standalone)
                                             gap_cluster_dict       = list(A = c(1,2,6), B = c(1,2,3,4)),      # named list keyed by rank_grp
                                             situation_cluster_dict = list(A = c(1,2), B = c(2,5)),      # named list keyed by rank_grp
                                             pos_rank_vec           = c(1, 2),
                                             team_rank_vec          = c(1, 2),
                                             gap_z_vec              = c(-99, 0),
                                             gap_z_na               = FALSE,
                                             xtd_vec_input          = c(0, 70),# xtd_percentile
                                             xtd_na                 = FALSE,
                                             season_filter          = NULL,
                                             grain                  = c("game"),
                                             drop_part_cols         = FALSE,
                                             df                     = rush_stats_final)


# rushing metric -> display label (single source of truth; add as new metrics appear)
RUSH_METRIC_LABELS <- c(
  rush_prop_avg   = "Rush Prop",
  rush_proportion = "Rush Prop",
  pbp_ypc_oe      = "YPC OE (pbp)",
  part_ypc_oe     = "YPC OE (part)",
  ypc             = "YPC",
  ybc             = "YBC",
  yac             = "YAC",
  pbp_xypc        = "xYPC (pbp)",
  part_xypc       = "xYPC (part)",
  pbp_xtd         = "xTD (pbp)",
  part_xtd        = "xTD (part)",
  pbp_xtd_share   = "xTD Share (pbp)",
  part_xtd_share  = "xTD Share (part)",
  xtd_percentile  = "xTD %ile",
  gap_z           = "Gap Z",
  attempts        = "Attempts",
  total_attempts  = "Attempts"
)

# default order — function filters down to what exists + non-NA
DEFAULT_RUSH_PLOT_METRICS <- c("rush_prop_avg", "rush_proportion",
                               "pbp_ypc_oe", "part_ypc_oe",
                               "ypc", "ybc", "yac",
                               "pbp_xtd_share", "part_xtd_share",
                               "xtd_percentile", "gap_z")

plot_rusher_cohort_dots <- function(cohort_df,
                                    metrics      = DEFAULT_RUSH_PLOT_METRICS,
                                    label_map    = RUSH_METRIC_LABELS,
                                    title_suffix = NULL) {
  
  focal_rows <- cohort_df %>% filter(is_focal)
  if (nrow(focal_rows) == 0) { message("No focal flagged."); return(invisible(NULL)) }
  
  # keep only metrics that exist in the data AND have at least one non-NA value
  metrics <- metrics[metrics %in% names(cohort_df)]
  metrics <- metrics[vapply(metrics, function(c) any(!is.na(cohort_df[[c]])), logical(1))]
  
  if (length(metrics) == 0) { message("No plottable metrics."); return(invisible(NULL)) }
  
  metric_labels <- unname(ifelse(metrics %in% names(label_map),
                                 label_map[metrics],
                                 metrics))
  
  long <- cohort_df %>%
    select(player, season, is_focal, all_of(metrics)) %>%
    pivot_longer(cols = all_of(metrics), names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(metric, levels = metrics, labels = metric_labels))
  
  focal_name <- focal_rows$player[1]
  focal_yr   <- focal_rows$season[1]
  n_focal    <- sum(cohort_df$is_focal)
  n_cohort   <- nrow(cohort_df) - n_focal
  
  ggplot(long, aes(x = value, y = 0)) +
    geom_point(data = long %>% filter(!is_focal),
               position = position_jitter(width = 0, height = 0.3, seed = 42),
               color = "grey55", alpha = 0.45, size = 2.4) +
    geom_point(data = long %>% filter(is_focal),
               position = position_jitter(width = 0, height = 0.15, seed = 1),
               color = "firebrick2", size = 4, stroke = 1, alpha = 0.9) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.3) +
    facet_wrap(~ metric, scales = "free_x", ncol = 1, strip.position = "left") +
    scale_y_continuous(limits = c(-0.5, 0.5), breaks = NULL) +
    labs(
      title    = paste0(focal_name, " — ", focal_yr,
                        if (!is.null(title_suffix)) paste0(" — ", title_suffix) else "",
                        " vs Cohort"),
      subtitle = paste0("grey = cohort (n = ", n_cohort, "), red = focal (n = ", n_focal, ")  |  dashed line at 0"),
      x = "Value (raw)",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title         = element_text(face = "bold", size = 14),
          plot.subtitle      = element_text(size = 9, color = "grey30"),
          strip.placement    = "outside",
          strip.text.y.left  = element_text(face = "bold", size = 10, angle = 0),
          axis.text.y        = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank())
}

plot_rusher_cohort_dots(charbonnet_season_view, title_suffix = "Season")
plot_rusher_cohort_dots(charbonnet_game_view,   title_suffix = "Game")



plot_rusher_ybc_yac <- function(cohort_df,
                                title_suffix  = NULL,
                                show_isolines = TRUE,
                                show_fit      = TRUE) {
  
  focal_rows <- cohort_df %>% filter(is_focal)
  if (nrow(focal_rows) == 0) { message("No focal flagged."); return(invisible(NULL)) }
  
  d <- cohort_df %>% filter(!is.na(ybc), !is.na(yac))
  
  focal_name <- focal_rows$player[1]
  focal_yr   <- focal_rows$season[1]
  n_focal    <- sum(d$is_focal)
  n_cohort   <- nrow(d) - n_focal
  
  med_ybc <- median(d$ybc[!d$is_focal], na.rm = TRUE)
  med_yac <- median(d$yac[!d$is_focal], na.rm = TRUE)
  
  p <- ggplot(d, aes(x = ybc, y = yac))
  
  if (show_isolines) {
    iso_k  <- pretty(range(d$ybc + d$yac, na.rm = TRUE), n = 6)
    iso_k  <- iso_k[iso_k > 0]
    x_lab  <- min(d$ybc, na.rm = TRUE)
    iso_df <- tibble(k = iso_k, x = x_lab, y = iso_k - x_lab) %>%
      filter(y >= min(d$yac, na.rm = TRUE), y <= max(d$yac, na.rm = TRUE))
    p <- p +
      geom_abline(slope = -1, intercept = iso_k, color = "grey88", linewidth = 0.3) +
      geom_text(data = iso_df, aes(x = x, y = y, label = paste0(k, " YPC")),
                hjust = 0, vjust = -0.3, size = 3.4, color = "grey70")
  }
  
  if (show_fit) {
    p <- p + geom_smooth(data = d %>% filter(!is_focal), method = "lm", se = FALSE,
                         color = "steelblue", linewidth = 0.6)
  }
  
  p <- p +
    geom_vline(xintercept = med_ybc, linetype = "dashed", color = "grey60", linewidth = 0.3) +
    geom_hline(yintercept = med_yac, linetype = "dashed", color = "grey60", linewidth = 0.3) +
    geom_point(data = d %>% filter(!is_focal), color = "grey55", alpha = 0.45, size = 2.6) +
    geom_point(data = d %>% filter(is_focal),  color = "firebrick2", size = 5, alpha = 0.9)
  
  if ("week" %in% names(d)) {
    p <- p + geom_text(data = d %>% filter(is_focal),
                       aes(label = week), color = "firebrick4", size = 3.6, vjust = -1.1)
  }
  
  p +
    labs(
      title    = paste0(focal_name, " — ", focal_yr,
                        if (!is.null(title_suffix)) paste0(" — ", title_suffix) else "",
                        " — YBC vs YAC"),
      subtitle = paste0("grey = cohort (n = ", n_cohort, "), red = focal (n = ", n_focal,
                        ")  |  dashed = cohort median  |  blue = cohort fit  |  diagonals = constant YPC"),
      x = "Yards Before Contact (YBC)",
      y = "Yards After Contact (YAC)"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title       = element_text(face = "bold", size = 18),
          plot.subtitle    = element_text(size = 11, color = "grey30"),
          axis.title       = element_text(size = 13),
          axis.text        = element_text(size = 11),
          panel.grid.minor = element_blank())
}

plot_rusher_ybc_yac(charbonnet_season_view, title_suffix = "Season")
plot_rusher_ybc_yac(charbonnet_game_view,   title_suffix = "Game")


rusher_ybc_yac_scorecard <- function(cohort_df) {
  d <- cohort_df %>% filter(!is.na(ybc), !is.na(yac)) %>%
    mutate(ypc_tot = ybc + yac, yac_share = yac / (ybc + yac))
  coh <- d %>% filter(!is_focal)
  fit <- lm(yac ~ ybc, data = coh)                 # expected YAC given YBC, cohort-defined
  d <- d %>% mutate(yac_resid = yac - predict(fit, newdata = d))
  pr <- function(x) (rank(x, ties.method = "average") - 1) / (length(x) - 1)
  d %>%
    mutate(ybc_pct       = pr(ybc),
           yac_pct       = pr(yac),
           ypc_pct       = pr(ypc_tot),
           yac_share_pct = pr(yac_share),
           yac_resid_pct = pr(yac_resid)) %>%
    filter(is_focal) %>%
    select(player, season, any_of("week"),
           ybc, ybc_pct, yac, yac_pct, ypc_tot, ypc_pct,
           yac_share, yac_share_pct, yac_resid, yac_resid_pct)
}

charbonnet_season_scorecard <- rusher_ybc_yac_scorecard(charbonnet_season_view)
charbonnet_game_scorecard <- rusher_ybc_yac_scorecard(charbonnet_game_view)


SCORECARD_SPEC <- tibble::tribble(
  ~label,       ~raw_col,    ~pct_col,
  "YBC",        "ybc",       "ybc_pct",
  "YAC",        "yac",       "yac_pct",
  "YPC",        "ypc_tot",   "ypc_pct",
  "YAC Share",  "yac_share", "yac_share_pct",
  "YAC o/Exp",  "yac_resid", "yac_resid_pct"
)

plot_rusher_scorecard <- function(scorecard_df, spec = SCORECARD_SPEC, title_suffix = NULL) {
  
  has_week <- "week" %in% names(scorecard_df)
  
  d <- scorecard_df %>%
    mutate(row_id = if (has_week) paste0("Wk ", week) else "Season")
  
  card <- bind_rows(lapply(seq_len(nrow(spec)), function(i) {
    tibble(row_id = d$row_id,
           label  = spec$label[i],
           value  = d[[spec$raw_col[i]]],
           pct    = d[[spec$pct_col[i]]])
  })) %>%
    mutate(label  = factor(label, levels = rev(spec$label)),
           pct100 = pct * 100,
           lab    = sprintf("%.2f  (%d%%)", value, round(pct100)))
  
  if (has_week) {
    card <- card %>% mutate(row_id = factor(row_id, levels = paste0("Wk ", sort(unique(d$week)))))
  }
  
  focal_name <- d$player[1]
  focal_yr   <- d$season[1]
  
  ggplot(card, aes(x = pct100, y = label)) +
    geom_col(aes(fill = pct100), width = 0.65) +
    geom_vline(xintercept = 50, linetype = "dashed", color = "grey50", linewidth = 0.3) +
    geom_text(aes(label = lab), hjust = -0.1, size = 3.4, color = "grey20") +
    scale_fill_gradientn(colors = c("firebrick2", "gold", "forestgreen"),
                         limits = c(0, 100), guide = "none") +
    scale_x_continuous(limits = c(0, 145), breaks = c(0, 25, 50, 75, 100)) +
    facet_wrap(~ row_id) +
    labs(
      title    = paste0(focal_name, " — ", focal_yr,
                        if (!is.null(title_suffix)) paste0(" — ", title_suffix) else "",
                        " — Scorecard"),
      subtitle = "bar = percentile within cohort (0–100), 50 = median  |  label = raw value (percentile)",
      x = "Percentile", y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title         = element_text(face = "bold", size = 18),
          plot.subtitle      = element_text(size = 11, color = "grey30"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          strip.text         = element_text(face = "bold", size = 12),
          axis.text.y        = element_text(size = 12, face = "bold"))
}

plot_rusher_scorecard(charbonnet_season_scorecard,  title_suffix = "Season")
plot_rusher_scorecard(charbonnet_game_scorecard, title_suffix = "Game")
