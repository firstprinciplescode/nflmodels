# Week-by-week onfield_perc + role context for a player
show_player_onfield <- function(player_id_in,
                                season_filter = NULL,
                                qbgrp_filter  = NULL,
                                df = receiving_func_base) {
  d <- df %>% filter(player_id == player_id_in)
  if (!is.null(season_filter)) d <- d %>% filter(season %in% season_filter)
  if (!is.null(qbgrp_filter))  d <- d %>% filter(qbgrp_ssn %in% qbgrp_filter)
  
  d %>%
    arrange(season, week) %>%
    select(season, week, posteam, qbgrp_ssn, def_ssn,
           snap_counts_pass_route, total_pass_snaps, onfield_perc,
           routes, targets, tgt_share,
           final_position_group, pos_rank, team_rank,
           align_cluster_name, rte_cluster_name, tgt_cluster_name, man_zone_grp_cluster)
}

show_player_onfield(11824, season_filter = c(2023, 2024, 2025))    # Berrios NYJ 2019 only
show_player_onfield(11824, qbgrp_filter = "SEADarnold-2025")  # specific QB era


receiving_clustering_base <- run_athena_query("
    SELECT  *
    FROM    nfl_data.vw_receiving_clustering_base
")

get_player_cluster_inputs <- function(player_id_in, season_in,
                                      posteam = NULL,
                                      df = receiving_clustering_base) {
  
  d <- df %>% filter(player_id == player_id_in, season %in% season_in)
  if (!is.null(posteam)) d <- d %>% filter(team == posteam)
  
  if (nrow(d) == 0) {
    message("No rows for player ", player_id_in, " in season(s) ",
            paste(season_in, collapse = ","),
            if (!is.null(posteam)) paste0(" / ", posteam) else "", ".")
    return(invisible(NULL))
  }
  
  totals <- d %>%
    summarise(
      behind_los_routes  = sum(behind_los_routes,  na.rm = TRUE),
      short_routes       = sum(short_routes,       na.rm = TRUE),
      medium_routes      = sum(medium_routes,      na.rm = TRUE),
      deep_routes        = sum(deep_routes,        na.rm = TRUE),
      behind_los_targets = sum(behind_los_targets, na.rm = TRUE),
      short_targets      = sum(short_targets,      na.rm = TRUE),
      medium_targets     = sum(medium_targets,     na.rm = TRUE),
      deep_targets       = sum(deep_targets,       na.rm = TRUE),
      wide_snaps         = sum(wide_snaps,         na.rm = TRUE),
      slot_snaps         = sum(slot_snaps,         na.rm = TRUE),
      inline_snaps       = sum(inline_snaps,       na.rm = TRUE),
      behind_snaps       = sum(behind_snaps,       na.rm = TRUE)
    )
  
  list(
    rte_counts   = c(totals$behind_los_routes,  totals$short_routes,   totals$medium_routes,  totals$deep_routes),
    tgt_counts   = c(totals$behind_los_targets, totals$short_targets,  totals$medium_targets, totals$deep_targets),
    align_counts = c(totals$wide_snaps,         totals$slot_snaps,     totals$inline_snaps,   totals$behind_snaps)
  )
}

get_player_cluster_neighbors <- function(player_id_in, season_in,
                                         distance_mult   = 1.75,
                                         posteam         = NULL,
                                         df_cluster_base = receiving_clustering_base) {
  
  if (length(season_in) != 1) stop("get_player_cluster_neighbors takes ONE season. Use get_player_identity_history for multi-season.")
  
  inp <- get_player_cluster_inputs(player_id_in, season_in, posteam, df_cluster_base)
  if (is.null(inp)) return(invisible(NULL))
  
  trim <- function(closest_df) closest_df %>% filter(distance <= min(distance) * distance_mult)
  
  list(
    rte   = rte_closest_func(inp$rte_counts)     %>% trim(),
    tgt   = tgt_closest_func(inp$tgt_counts)     %>% trim(),
    align = align_closest_func(inp$align_counts) %>% trim()
  )
}


get_player_identity_history <- function(player_id_in, season_in,
                                        df_identity = receiving_func_base) {
  
  get_mode <- function(x) {
    tab <- table(x, useNA = "no")
    if (length(tab) == 0) NA_character_ else names(sort(tab, decreasing = TRUE))[1]
  }
  
  df_identity %>%
    filter(player_id == player_id_in, season %in% season_in) %>%
    group_by(season) %>%
    summarise(
      man_zone_grp_cluster = get_mode(man_zone_grp_cluster),
      td_grp_cluster       = get_mode(td_grp_cluster),
      z_score_percentile   = mean(z_score_percentile, na.rm = TRUE),
      xpass_percentile     = mean(xpass_percentile,   na.rm = TRUE),
      xtd_percentile       = mean(xtd_percentile,     na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(season)
}

# usage:
get_player_cluster_neighbors(48327, 2025, distance_mult = 2)    # single season, rte/tgt/align
get_player_cluster_neighbors(48327, 2024, distance_mult = 2)    # single season, rte/tgt/align
get_player_identity_history(48327, c(2023, 2024, 2025))          # multi-season identity drift


compare_receiver_cohort <- function(focal_player_id      = NULL,
                                    focal_season         = NULL,
                                    rte_cluster_input    = NULL,
                                    tgt_cluster_input    = NULL,
                                    align_cluster_input  = NULL,
                                    position_group_input = NULL,
                                    pos_rank_vec         = c(1, 99),
                                    team_rank_vec        = c(1, 99),
                                    man_zone_grp_input   = NULL,
                                    man_z_vec_input      = c(0, 100),
                                    man_z_na             = FALSE,
                                    xpass_vec_input      = c(0, 100),
                                    xpass_na             = TRUE,
                                    xtd_grp_input        = NULL,
                                    xtd_vec_input        = c(0, 100),
                                    xtd_grp_na           = TRUE,
                                    onfield_min          = 0.50,
                                    onfield_max          = 1.00,
                                    season_filter        = NULL,
                                    grain                = c("season", "game"),
                                    drop_part_cols       = FALSE,
                                    df                   = receiving_func_base) {
  
  grain <- match.arg(grain)
  
  d <- df %>% ungroup() %>%
    filter(onfield_perc >= onfield_min, onfield_perc <= onfield_max,
           pos_rank  >= pos_rank_vec[1],  pos_rank  <= pos_rank_vec[2],
           team_rank >= team_rank_vec[1], team_rank <= team_rank_vec[2])
  
  if (!is.null(season_filter))        d <- d %>% filter(season               %in% season_filter)
  if (!is.null(rte_cluster_input))    d <- d %>% filter(rte_cluster_name     %in% rte_cluster_input)
  if (!is.null(tgt_cluster_input))    d <- d %>% filter(tgt_cluster_name     %in% tgt_cluster_input)
  if (!is.null(align_cluster_input))  d <- d %>% filter(align_cluster_name   %in% align_cluster_input)
  if (!is.null(position_group_input)) d <- d %>% filter(final_position_group %in% position_group_input)
  if (!is.null(man_zone_grp_input))   d <- d %>% filter(man_zone_grp_cluster %in% man_zone_grp_input)
  if (!is.null(xtd_grp_input))        d <- d %>% filter(td_grp_cluster       %in% xtd_grp_input)
  
  d <- d %>% filter(
    (z_score_percentile >= man_z_vec_input[1] & z_score_percentile <= man_z_vec_input[2]) |
      (man_z_na & is.na(z_score_percentile)),
    (xpass_percentile   >= xpass_vec_input[1] & xpass_percentile   <= xpass_vec_input[2]) |
      (xpass_na & is.na(xpass_percentile)),
    (xtd_percentile     >= xtd_vec_input[1]   & xtd_percentile     <= xtd_vec_input[2]) |
      (xtd_grp_na & is.na(xtd_percentile))
  )
  
  d <- d %>%
    mutate(
      tgt_per_route = targets  / pmax(snap_counts_pass_route, 1),
      pbp_cp_oe     = acc_rate - pbp_cp,
      part_cp_oe    = acc_rate - part_cp,
      pbp_ypa_oe    = ypa      - pbp_xypa,
      part_ypa_oe   = ypa      - part_xypa,
      pbp_yac_oe    = yac      - pbp_yac,
      part_yac_oe   = yac      - part_yac
    )
  
  if (grain == "season") {
    out <- d %>%
      group_by(player_id, player, season) %>%
      summarise(
        n_weeks          = n(),
        total_routes     = sum(snap_counts_pass_route, na.rm = TRUE),
        total_targets    = sum(targets,                na.rm = TRUE),
        tgt_per_route    = total_targets / pmax(total_routes, 1),
        tgt_share_avg    = weighted.mean(tgt_share,  snap_counts_pass_route, na.rm = TRUE),
        acc_rate         = weighted.mean(acc_rate,   targets, na.rm = TRUE),
        pbp_cp           = weighted.mean(pbp_cp,     targets, na.rm = TRUE),
        part_cp          = weighted.mean(part_cp,    targets, na.rm = TRUE),
        ypa              = weighted.mean(ypa,        targets, na.rm = TRUE),
        pbp_xypa         = weighted.mean(pbp_xypa,   targets, na.rm = TRUE),
        part_xypa        = weighted.mean(part_xypa,  targets, na.rm = TRUE),
        yac              = weighted.mean(yac,        targets, na.rm = TRUE),
        pbp_yac          = weighted.mean(pbp_yac,    targets, na.rm = TRUE),
        part_yac         = weighted.mean(part_yac,   targets, na.rm = TRUE),
        adot             = weighted.mean(adot,       targets, na.rm = TRUE),
        onfield_perc_avg = mean(onfield_perc, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        pbp_cp_oe   = acc_rate - pbp_cp,
        part_cp_oe  = acc_rate - part_cp,
        pbp_ypa_oe  = ypa      - pbp_xypa,
        part_ypa_oe = ypa      - part_xypa,
        pbp_yac_oe  = yac      - pbp_yac,
        part_yac_oe = yac      - part_yac
      )
  } else {
    out <- d %>%
      select(player_id, player, season, week, posteam, qbgrp_ssn, def_ssn,
             snap_counts_pass_route, routes, targets, tgt_per_route,
             tgt_share, onfield_perc,
             acc_rate, pbp_cp, part_cp,
             ypa, pbp_xypa, part_xypa,
             yac, pbp_yac, part_yac,
             adot,
             pbp_cp_oe, part_cp_oe,
             pbp_ypa_oe, part_ypa_oe,
             pbp_yac_oe, part_yac_oe)
  }
  
  if (!is.null(focal_player_id)) {
    out <- out %>%
      mutate(is_focal = if (!is.null(focal_season)) {
        player_id == focal_player_id & season == focal_season
      } else {
        player_id == focal_player_id
      })
    
    out <- if (grain == "season") {
      out %>% arrange(desc(is_focal), desc(tgt_per_route))
    } else {
      out %>% arrange(desc(is_focal), season, week)
    }
  } else {
    out <- out %>% mutate(is_focal = FALSE)
    out <- if (grain == "season") {
      out %>% arrange(desc(tgt_per_route))
    } else {
      out %>% arrange(season, week, player)
    }
  }
  
  if (drop_part_cols) {
    out <- out %>% select(-starts_with("part_"))
  }
  
  out
}

# Season aggregate (default)
aj_brown_season_view <- compare_receiver_cohort(focal_player_id      = 48327,   # optional, just flags focal in output
                                                focal_season         = 2025,
                                                rte_cluster_input    = c("SMT","ST","RB","DT"),
                                                tgt_cluster_input    = c("ML"),
                                                align_cluster_input  = c("WWR"),
                                                position_group_input = c("WR"),
                                                pos_rank_vec         = c(1, 99),
                                                team_rank_vec        = c(1, 99),
                                                man_zone_grp_input   = c("WR_DEEP"),
                                                man_z_vec_input      = c(60, 100),
                                                man_z_na             = FALSE,
                                                xpass_vec_input      = c(20, 80),
                                                xpass_na             = FALSE,
                                                xtd_grp_input        = c("TD_LOW"),
                                                xtd_vec_input        = c(50, 100),
                                                xtd_grp_na           = FALSE,
                                                onfield_min          = 0.90,
                                                onfield_max          = 1.00,
                                                season_filter        = NULL,
                                                df                   = receiving_func_base,
                                                grain                = "season",
                                                drop_part_cols       = TRUE)

# Per-game view
aj_brown_game_view   <- compare_receiver_cohort(focal_player_id      = 48327,   # optional, just flags focal in output
                                                focal_season         = 2025,
                                                rte_cluster_input    = c("SMT","ST","RB","DT"),
                                                tgt_cluster_input    = c("ML"),
                                                align_cluster_input  = c("WWR"),
                                                position_group_input = c("WR"),
                                                pos_rank_vec         = c(1, 99),
                                                team_rank_vec        = c(1, 99),
                                                man_zone_grp_input   = c("WR_DEEP"),
                                                man_z_vec_input      = c(60, 100),
                                                man_z_na             = FALSE,
                                                xpass_vec_input      = c(20, 80),
                                                xpass_na             = FALSE,
                                                xtd_grp_input        = c("TD_LOW"),
                                                xtd_vec_input        = c(50, 100),
                                                xtd_grp_na           = FALSE,
                                                onfield_min          = 0.90,
                                                onfield_max          = 1.00,
                                                season_filter        = NULL,
                                                df                   = receiving_func_base,
                                                grain                = "game",
                                                drop_part_cols       = TRUE)


# Single source of truth for metric → display label.
# Add to it as new metrics get computed; the function will pick them up automatically.
METRIC_LABELS <- c(
  tgt_per_route    = "Tgt/Route",
  tgt_share_avg    = "Tgt Share",
  tgt_share        = "Tgt Share",
  pbp_cp_oe        = "CP OE (pbp)",
  part_cp_oe       = "CP OE (part)",
  pbp_ypa_oe       = "YPA OE (pbp)",
  part_ypa_oe      = "YPA OE (part)",
  pbp_yac_oe       = "YAC OE (pbp)",
  part_yac_oe      = "YAC OE (part)",
  adot             = "aDOT",
  acc_rate         = "Catch %",
  ypa              = "YPA",
  yac              = "YAC",
  onfield_perc_avg = "On-Field %",
  onfield_perc     = "On-Field %"
)

# Default plotting order — function filters this down to what's available + non-NA
DEFAULT_PLOT_METRICS <- c("tgt_per_route",
                          "pbp_cp_oe",  "part_cp_oe",
                          "pbp_ypa_oe", "part_ypa_oe",
                          "pbp_yac_oe", "part_yac_oe",
                          "adot")

plot_cohort_dots <- function(cohort_df,
                             metrics      = DEFAULT_PLOT_METRICS,
                             label_map    = METRIC_LABELS,
                             title_suffix = NULL) {
  
  focal_rows <- cohort_df %>% filter(is_focal)
  if (nrow(focal_rows) == 0) { message("No focal flagged."); return(invisible(NULL)) }
  
  # keep only metrics that exist in the data AND have at least one non-NA value
  metrics <- metrics[metrics %in% names(cohort_df)]
  metrics <- metrics[vapply(metrics, function(c) any(!is.na(cohort_df[[c]])), logical(1))]
  
  if (length(metrics) == 0) { message("No plottable metrics."); return(invisible(NULL)) }
  
  # display labels: lookup from dict, fall back to column name
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

plot_cohort_dots(aj_brown_season_view, title_suffix = "Season")
plot_cohort_dots(aj_brown_game_view,   title_suffix = "Game")