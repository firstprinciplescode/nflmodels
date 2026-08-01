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

show_player_onfield(145059, qbgrp_filter = "NEMaye-2025")$onfield_perc %>% sort()
show_player_onfield(84329, season_filter = 2025)$onfield_perc %>% sort()

#receiving_clustering_base <- run_athena_query("
#    SELECT  *
#    FROM    nfl_data.vw_receiving_clustering_base
#")

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


get_offense_receiver_identity <- function(qbgrp_ssn_in,
                                          rte_cluster_input    = NULL,
                                          tgt_cluster_input    = NULL,
                                          align_cluster_input  = NULL,
                                          position_group_input = NULL,
                                          man_zone_grp_input   = NULL,
                                          xtd_grp_input        = NULL,
                                          pos_rank_vec         = c(1, 99),
                                          team_rank_vec        = c(1, 99),
                                          onfield_min          = 0,
                                          onfield_max          = 1,
                                          tgt_share_min        = 0,
                                          tgt_share_max        = 1,
                                          df                   = receiving_func_base) {

  get_mode <- function(x) {
    tab <- table(x, useNA = "no")
    if (length(tab) == 0) NA_character_ else names(sort(tab, decreasing = TRUE))[1]
  }

  d <- df %>% ungroup() %>%
    filter(qbgrp_ssn %in% qbgrp_ssn_in,
           tgt_share    >= tgt_share_min, tgt_share    <= tgt_share_max,
           onfield_perc >= onfield_min,   onfield_perc <= onfield_max,
           pos_rank  >= pos_rank_vec[1],  pos_rank  <= pos_rank_vec[2],
           team_rank >= team_rank_vec[1], team_rank <= team_rank_vec[2])

  if (!is.null(rte_cluster_input))    d <- d %>% filter(rte_cluster_name     %in% rte_cluster_input)
  if (!is.null(tgt_cluster_input))    d <- d %>% filter(tgt_cluster_name     %in% tgt_cluster_input)
  if (!is.null(align_cluster_input))  d <- d %>% filter(align_cluster_name   %in% align_cluster_input)
  if (!is.null(position_group_input)) d <- d %>% filter(final_position_group %in% position_group_input)
  if (!is.null(man_zone_grp_input))   d <- d %>% filter(man_zone_grp_cluster %in% man_zone_grp_input)
  if (!is.null(xtd_grp_input))        d <- d %>% filter(td_grp_cluster       %in% xtd_grp_input)

  d %>%
    group_by(qbgrp_ssn, player_id, player) %>%
    summarise(
      n_games              = n(),
      final_position_group = get_mode(final_position_group),
      pos_rank             = get_mode(pos_rank),
      team_rank            = get_mode(team_rank),
      onfield_perc_avg     = mean(onfield_perc, na.rm = TRUE),
      tgt_share_avg        = mean(tgt_share, na.rm = TRUE),
      align_cluster_name   = get_mode(align_cluster_name),
      rte_cluster_name     = get_mode(rte_cluster_name),
      tgt_cluster_name     = get_mode(tgt_cluster_name),
      man_zone_grp_cluster = get_mode(man_zone_grp_cluster),
      td_grp_cluster       = get_mode(td_grp_cluster),
      z_score_percentile   = mean(z_score_percentile, na.rm = TRUE),
      xpass_percentile     = mean(xpass_percentile,   na.rm = TRUE),
      xtd_percentile       = mean(xtd_percentile,     na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(qbgrp_ssn, desc(tgt_share_avg))
}

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
                                    min_games            = 6,
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
      filter(n_weeks >= min_games) %>%
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


####
#### LOGGING INFRASTRUCTURE — wrapper + registry (lives right after compare_receiver_cohort)
####

`%||%` <- function(a, b) if (is.null(a)) b else a

# logged wrapper: runs compare_receiver_cohort AND stamps the call params onto the result
compare_receiver_cohort_logged <- function(...) {
  args <- list(...)
  out  <- compare_receiver_cohort(...)
  foc  <- out %>% filter(is_focal) %>% slice(1)
  attr(out, "call_params")  <- args
  attr(out, "focal_name")   <- if (nrow(foc)) foc$player[1] else NA_character_
  attr(out, "focal_season") <- if (nrow(foc)) foc$season[1] else args$focal_season %||% NA
  attr(out, "logged_at")    <- Sys.time()
  out
}

fmt_val <- function(v) {
  if (is.null(v)) return("NULL")
  if (is.logical(v) || is.numeric(v)) return(paste(v, collapse = ","))
  paste0("c(", paste0('"', v, '"', collapse = ","), ")")
}

# registry: one row per saved view, non-default params as a string
log_view <- function(view, view_name, registry = NULL) {
  p <- attr(view, "call_params")
  if (is.null(p)) stop("view has no call_params - build it with compare_receiver_cohort_logged()")
  defaults <- formals(compare_receiver_cohort)
  is_default <- function(name, val) {
    d <- defaults[[name]]
    if (is.symbol(d) || is.null(d)) return(FALSE)
    d <- tryCatch(eval(d), error = function(e) return(FALSE))
    isTRUE(all.equal(val, d))
  }
  nondef <- p[!vapply(names(p), function(n) is_default(n, p[[n]]), logical(1))]
  nondef <- nondef[!names(nondef) %in% "df"]
  
  row <- tibble(
    view_name    = view_name,
    focal_name   = attr(view, "focal_name"),
    focal_season = attr(view, "focal_season"),
    logged_at    = attr(view, "logged_at"),
    params       = paste(names(nondef), vapply(nondef, fmt_val, character(1)),
                         sep = " = ", collapse = " | ")
  )
  if (is.null(registry)) row else bind_rows(registry, row)
}

# show only the NON-default params of a logged view, tidy 2-col
show_params <- function(view) {
  p <- attr(view, "call_params")
  if (is.null(p)) { message("No logged params on this object."); return(invisible(NULL)) }
  defaults <- formals(compare_receiver_cohort)
  is_default <- function(name, val) {
    d <- defaults[[name]]
    if (is.symbol(d) || is.null(d)) return(FALSE)
    d <- tryCatch(eval(d), error = function(e) return(FALSE))
    isTRUE(all.equal(val, d))
  }
  nondef <- p[!vapply(names(p), function(n) is_default(n, p[[n]]), logical(1))]
  nondef <- nondef[!names(nondef) %in% "df"]
  df <- tibble(param = names(nondef),
               value = vapply(nondef, fmt_val, character(1)))
  cat(attr(view, "focal_name"), "-", attr(view, "focal_season"), "\n")
  print(df, n = Inf)
  invisible(df)
}

# reprint the exact rebuild call for a registry row
recover_call <- function(registry, view_name_in) {
  r <- registry %>% filter(view_name == view_name_in) %>% slice(1)
  cat(view_name_in, " <- compare_receiver_cohort_logged(\n  ",
      gsub(" \\| ", ",\n  ", r$params), ")\n", sep = "")
}


####
#### identity / offense functions (unchanged)
####

get_offense_receiver_identity <- function(qbgrp_ssn_in,
                                          rte_cluster_input    = NULL,
                                          tgt_cluster_input    = NULL,
                                          align_cluster_input  = NULL,
                                          position_group_input = NULL,
                                          tgt_share_min        = 0,
                                          tgt_share_max        = 1,
                                          df                   = receiving_func_base) {
  
  get_mode <- function(x) {
    tab <- table(x, useNA = "no")
    if (length(tab) == 0) NA_character_ else names(sort(tab, decreasing = TRUE))[1]
  }
  
  d <- df %>% ungroup() %>%
    filter(qbgrp_ssn %in% qbgrp_ssn_in,
           tgt_share >= tgt_share_min,
           tgt_share <= tgt_share_max)
  
  if (!is.null(rte_cluster_input))    d <- d %>% filter(rte_cluster_name     %in% rte_cluster_input)
  if (!is.null(tgt_cluster_input))    d <- d %>% filter(tgt_cluster_name     %in% tgt_cluster_input)
  if (!is.null(align_cluster_input))  d <- d %>% filter(align_cluster_name   %in% align_cluster_input)
  if (!is.null(position_group_input)) d <- d %>% filter(final_position_group %in% position_group_input)
  
  d %>%
    group_by(qbgrp_ssn, player_id, player) %>%
    summarise(
      n_games              = n(),
      tgt_share_avg        = mean(tgt_share, na.rm = TRUE),
      man_zone_grp_cluster = get_mode(man_zone_grp_cluster),
      td_grp_cluster       = get_mode(td_grp_cluster),
      z_score_percentile   = mean(z_score_percentile, na.rm = TRUE),
      xpass_percentile     = mean(xpass_percentile,   na.rm = TRUE),
      xtd_percentile       = mean(xtd_percentile,     na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(qbgrp_ssn, desc(tgt_share_avg))
}

# usage:
get_player_cluster_neighbors(84329, 2025, distance_mult = 7)
get_player_cluster_neighbors(84329, 2024, distance_mult = 3.5)
get_player_identity_history(84329, c(2023, 2024, 2025))

get_offense_receiver_identity(c("NEMaye-2025", "NEMaye-2024", "NEBrissett-2024"),
                              position_group_input = c("WR"),
                              tgt_cluster_input    = c("ML")) %>% View(.)


####
#### KAYSHON BOUTTE — logged views + registry appends
####

# Season aggregate (default)
doubs_season_view <- compare_receiver_cohort_logged(
  focal_player_id      = 84329 ,
  focal_season         = 2025,
  rte_cluster_input    = c("BT","DT","RB"),
  tgt_cluster_input    = c("ML"),
  align_cluster_input  = c("WWR"),
  position_group_input = c("WR"),
  man_zone_grp_input   = c("WR_DEEP"),
  man_z_vec_input      = c(0, 100),
  man_z_na             = FALSE,
  xpass_vec_input      = c(30, 80),
  xpass_na             = FALSE,
  xtd_grp_input        = c("TD_LOW"),
  xtd_vec_input        = c(0, 100),
  xtd_grp_na           = FALSE,
  onfield_min          = 0.55,
  onfield_max          = 1.00,
  grain                = "season")

doubs_season_view_lowtd <- compare_receiver_cohort_logged(
  focal_player_id      = 48327,
  focal_season         = 2025,
  rte_cluster_input    = c("ST","DT","SMT","RB"),
  tgt_cluster_input    = c("ML"),
  align_cluster_input  = c("WWR"),
  position_group_input = c("WR"),
  man_zone_grp_input   = c("WR_DEEP"),
  man_z_vec_input      = c(50, 100),
  man_z_na             = FALSE,
  xpass_vec_input      = c(0, 65),
  xpass_na             = FALSE,
  xtd_grp_input        = c("TD_LOW"),
  xtd_vec_input        = c(70, 100),
  xtd_grp_na           = FALSE,
  onfield_min          = 0.65,
  onfield_max          = 1.00,
  grain                = "season")

receiver_registry <- log_view(doubs_season_view, "doubs_season_view",
                              if (exists("receiver_registry")) receiver_registry else NULL)

doubs_season_view <- rbind(doubs_season_view_hightd, doubs_season_view_lowtd)

# Per-game view
doubs_game_view <- compare_receiver_cohort_logged(
  focal_player_id      = 84329 ,
  focal_season         = 2025,
  rte_cluster_input    = c("BT","DT","RB"),
  tgt_cluster_input    = c("ML"),
  align_cluster_input  = c("WWR"),
  position_group_input = c("WR"),
  man_zone_grp_input   = c("WR_DEEP"),
  man_z_vec_input      = c(0, 100),
  man_z_na             = FALSE,
  xpass_vec_input      = c(30, 80),
  xpass_na             = FALSE,
  xtd_grp_input        = c("TD_LOW"),
  xtd_vec_input        = c(0, 100),
  xtd_grp_na           = FALSE,
  onfield_min          = 0.55,
  onfield_max          = 1.00,
  grain                = "game")

doubs_game_view_lowtd <- compare_receiver_cohort_logged(
  focal_player_id      = 48327,
  focal_season         = 2025,
  rte_cluster_input    = c("ST","DT","SMT","RB"),
  tgt_cluster_input    = c("ML"),
  align_cluster_input  = c("WWR"),
  position_group_input = c("WR"),
  man_zone_grp_input   = c("WR_DEEP"),
  man_z_vec_input      = c(50, 100),
  man_z_na             = FALSE,
  xpass_vec_input      = c(0, 65),
  xpass_na             = FALSE,
  xtd_grp_input        = c("TD_LOW"),
  xtd_vec_input        = c(70, 100),
  xtd_grp_na           = FALSE,
  onfield_min          = 0.65,
  onfield_max          = 1.00,
  grain                = "game")

receiver_registry <- log_view(doubs_game_view, "doubs_game_view", receiver_registry)

doubs_game_view <- rbind(doubs_game_view_hightd, doubs_game_view_lowtd)

# Wide pool for common-opponent work (looser filters)
doubs_wide_game <- compare_receiver_cohort_logged(
  focal_player_id      = 84329 ,
  focal_season         = 2025,
  rte_cluster_input    = c("BT","DT","RB","SMT"),
  tgt_cluster_input    = c("ML"),
  align_cluster_input  = c("WWR"),
  position_group_input = c("WR"),
  man_zone_grp_input   = c("WR_DEEP"),
  man_z_vec_input      = c(0, 100),
  man_z_na             = FALSE,
  xpass_vec_input      = c(30, 80),
  xpass_na             = FALSE,
  xtd_grp_input        = c("TD_LOW"),
  xtd_vec_input        = c(0, 100),
  xtd_grp_na           = FALSE,
  onfield_min          = 0.55,
  onfield_max          = 1.00,
  grain                = "game")

doubs_wide_game_lowtd <- compare_receiver_cohort_logged(
  focal_player_id      = 48327,
  focal_season         = 2025,
  rte_cluster_input    = c("ST","DT","SMT","RB"),
  tgt_cluster_input    = c("ML", "SMT", "DT", "G"),
  align_cluster_input  = c("WWR"),
  position_group_input = c("WR"),
  man_zone_grp_input   = c("WR_DEEP"),
  man_z_vec_input      = c(45, 100),
  man_z_na             = FALSE,
  xpass_vec_input      = c(0, 65),
  xpass_na             = FALSE,
  xtd_grp_input        = c("TD_LOW"),
  xtd_vec_input        = c(60, 100),
  xtd_grp_na           = FALSE,
  onfield_min          = 0.65,
  onfield_max          = 1.00,
  grain                = "game")

receiver_registry <- log_view(doubs_wide_game, "doubs_wide_game", receiver_registry)

doubs_wide_game <- rbind(doubs_wide_game_hightd, doubs_wide_game_lowtd)

# check the registry / recover a recipe after restart:
# View(receiver_registry)
# show_params(kayshon_boutte_wide_game)
# recover_call(receiver_registry, "kayshon_boutte_wide_game")


####
#### plotting dictionaries + functions (unchanged from your script)
####

# Single source of truth for metric -> display label.
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

DEFAULT_PLOT_METRICS <- c("tgt_per_route", "tgt_share",
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

plot_cohort_dots(doubs_season_view, title_suffix = "Season")
plot_cohort_dots(doubs_game_view,   title_suffix = "Game")


# metric -> label; both season (*_avg) and game names listed, existence-filtered
RECV_PCTL_SPEC <- tibble::tribble(
  ~label,          ~col,
  "Tgt/Route",     "tgt_per_route",
  "Tgt Share",     "tgt_share_avg",
  "Tgt Share",     "tgt_share",
  "CP OE (pbp)",   "pbp_cp_oe",
  "CP OE (part)",  "part_cp_oe",
  "YPA OE (pbp)",  "pbp_ypa_oe",
  "YPA OE (part)", "part_ypa_oe",
  "YAC OE (pbp)",  "pbp_yac_oe",
  "YAC OE (part)", "part_yac_oe",
  "aDOT",          "adot",
  "On-Field %",    "onfield_perc_avg",
  "On-Field %",    "onfield_perc"
)

plot_receiver_pctl_heatmap <- function(cohort_df, spec = RECV_PCTL_SPEC, title = NULL) {
  
  focal_rows <- cohort_df %>% filter(is_focal)
  if (nrow(focal_rows) == 0) { message("No focal flagged."); return(invisible(NULL)) }
  
  has_week <- "week" %in% names(cohort_df)
  spec <- spec %>% filter(col %in% names(cohort_df))
  
  pctl_in <- function(col, v) {
    x <- cohort_df[[col]]
    mean(x <= v, na.rm = TRUE)
  }
  
  focal_keyed <- if (has_week) {
    focal_rows %>% mutate(col_id = paste0("Wk ", week)) %>%
      arrange(week)
  } else {
    focal_rows %>% mutate(col_id = "Season")
  }
  
  pd <- bind_rows(lapply(seq_len(nrow(spec)), function(i) {
    cc <- spec$col[i]
    tibble(
      metric = spec$label[i],
      col_id = focal_keyed$col_id,
      raw    = focal_keyed[[cc]],
      pctl   = vapply(focal_keyed[[cc]], function(v) if (is.na(v)) NA_real_ else pctl_in(cc, v), numeric(1))
    )
  })) %>%
    filter(!is.na(pctl)) %>%
    mutate(
      metric = factor(metric, levels = rev(unique(spec$label))),
      col_id = factor(col_id, levels = unique(focal_keyed$col_id))
    )
  
  focal_name <- focal_rows$player[1]
  focal_yr   <- focal_rows$season[1]
  
  ggplot(pd, aes(col_id, metric, fill = pctl)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = scales::percent(pctl, accuracy = 1),
                  color = abs(pctl - 0.5) > 0.25),
              size = if (has_week) 3 else 4.5, fontface = "bold", show.legend = FALSE) +
    scale_fill_gradient2(low = "#08519c", mid = "#f7f7f7", high = "#a63603",
                         midpoint = 0.5, limits = c(0, 1),
                         labels = scales::percent, name = "Pctl") +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    scale_x_discrete(position = "top") +
    labs(
      title    = title %||% paste0(focal_name, " — ", focal_yr, " — Percentile vs cohort"),
      subtitle = paste0("red→blue = high→low percentile within cohort (n = ", nrow(cohort_df) - sum(cohort_df$is_focal),
                        " grey)  |  OE = over expected"),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 16),
      plot.subtitle   = element_text(color = "grey40", size = 9),
      panel.grid      = element_blank(),
      axis.text.x.top = element_text(face = "bold", size = if (has_week) 9 else 12),
      axis.text.y     = element_text(size = 11),
      legend.position = "right",
      legend.key.height = unit(1.4, "cm")
    )
}

plot_receiver_pctl_heatmap(doubs_season_view)


####
#### common-opponent percentile machinery
####

RECV_CO_METRICS <- c(tgt_per_route = "Tgt/Route",  tgt_share    = "Tgt Share",
                     pbp_cp_oe     = "CP OE (pbp)", part_cp_oe   = "CP OE (part)",
                     pbp_ypa_oe    = "YPA OE (pbp)",part_ypa_oe  = "YPA OE (part)",
                     pbp_yac_oe    = "YAC OE (pbp)",part_yac_oe  = "YAC OE (part)",
                     adot          = "aDOT",        onfield_perc = "On-Field %")

common_opp_pctl <- function(game_cohort, metrics = RECV_CO_METRICS, min_comp = 3) {
  focal <- game_cohort %>% filter(is_focal)
  comp  <- game_cohort %>% filter(!is_focal)
  metric_cols <- names(metrics)
  
  # per focal game x metric: percentile within comp games vs the SAME def_ssn
  per_game <- bind_rows(lapply(seq_len(nrow(focal)), function(i) {
    d    <- focal$def_ssn[i]
    pool <- comp %>% filter(def_ssn == d)
    tibble(
      def_ssn = d,
      week    = focal$week[i],
      metric  = metric_cols,
      value   = vapply(metric_cols, function(m) focal[[m]][i], numeric(1)),
      pctl    = vapply(metric_cols, function(m) {
        v <- focal[[m]][i]; x <- pool[[m]]
        if (is.na(v) || sum(!is.na(x)) < min_comp) NA_real_ else mean(x <= v, na.rm = TRUE)
      }, numeric(1)),
      n_comp  = nrow(pool)
    )
  }))
  
  summary <- per_game %>%
    group_by(metric) %>%
    summarise(pctl_co = mean(pctl, na.rm = TRUE),
              n_defs  = sum(!is.na(pctl)), .groups = "drop") %>%
    mutate(label = factor(metrics[metric], levels = rev(unname(metrics))))
  
  # stamp focal identity for self-labeling plots
  focal_id <- focal %>% slice(1)
  list(per_game = per_game, summary = summary,
       focal_name   = if (nrow(focal_id)) focal_id$player[1] else NA_character_,
       focal_season = if (nrow(focal_id)) focal_id$season[1] else NA)
}

doubs_co <- common_opp_pctl(doubs_wide_game, min_comp = 3)

# DIAGNOSTIC - is there enough per defense?
def_n <- doubs_co$per_game %>% distinct(def_ssn, n_comp) %>% arrange(n_comp)
cat("comp games per focal-defense:\n"); print(def_n)
cat("\nmedian comp games/def:", median(def_n$n_comp),
    "| defenses with >=3:", sum(def_n$n_comp >= 3), "of", nrow(def_n), "\n")
cat("\nmetrics x how many defenses cleared min_comp:\n"); print(henry_co$summary %>% select(metric, n_defs))


threshold_sweep <- function(game_cohort, metrics = RECV_CO_METRICS, thresholds = 1:10) {
  focal <- game_cohort %>% filter(is_focal)
  comp  <- game_cohort %>% filter(!is_focal)
  
  pool_n <- sapply(focal$def_ssn, function(d) sum(comp$def_ssn == d))
  
  bind_rows(lapply(thresholds, function(mc) {
    co <- common_opp_pctl(game_cohort, metrics = metrics, min_comp = mc)
    ov <- co$summary %>% summarise(mean_pctl = mean(pctl_co, na.rm = TRUE),
                                   sd_across_metrics = sd(pctl_co, na.rm = TRUE))
    tibble(min_comp        = mc,
           defs_qualifying = sum(pool_n >= mc),
           defs_total      = length(pool_n),
           avg_n_defs_used = mean(co$summary$n_defs),
           mean_pctl       = ov$mean_pctl)
  }))
}

sweep <- threshold_sweep(doubs_wide_game)
print(sweep)

focal <- doubs_wide_game %>% filter(is_focal)
comp  <- doubs_wide_game %>% filter(!is_focal)
pool_n <- tibble(def_ssn = focal$def_ssn,
                 n_comp  = sapply(focal$def_ssn, function(d) sum(comp$def_ssn == d))) %>%
  arrange(n_comp)
print(pool_n)
cat("median:", median(pool_n$n_comp), " min:", min(pool_n$n_comp), " max:", max(pool_n$n_comp), "\n")


####
#### common-opponent plots (self-labeling off co_obj)
####

plot_co_pctl_heatmap <- function(co_obj, title = NULL) {
  pd <- co_obj$summary %>% filter(!is.na(pctl_co))
  nm <- co_obj$focal_name   %||% "Focal"
  yr <- co_obj$focal_season %||% ""
  ggplot(pd, aes(x = "Common-Opp", y = label, fill = pctl_co)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = scales::percent(pctl_co, accuracy = 1),
                  color = abs(pctl_co - 0.5) > 0.25),
              size = 4.5, fontface = "bold", show.legend = FALSE) +
    scale_fill_gradient2(low = "#08519c", mid = "#f7f7f7", high = "#a63603",
                         midpoint = 0.5, limits = c(0, 1),
                         labels = scales::percent, name = "Pctl") +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    scale_x_discrete(position = "top") +
    labs(title = title %||% paste0(nm, " — ", yr, " — vs common-opponent cohort"),
         subtitle = "percentile among same-archetype receivers facing the SAME defense, averaged across games",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 16),
          plot.subtitle = element_text(color = "grey40", size = 9),
          panel.grid = element_blank(),
          axis.text.x.top = element_text(face = "bold", size = 12),
          axis.text.y = element_text(size = 11),
          legend.key.height = unit(1.4, "cm"))
}

plot_co_pctl_heatmap(doubs_co)


RECV_CO_ORDER <- c("On-Field %", "Tgt Share", "Tgt/Route",
                   "CP OE (pbp)", "CP OE (part)",
                   "YPA OE (pbp)", "YPA OE (part)",
                   "YAC OE (pbp)", "YAC OE (part)",
                   "aDOT")

plot_co_pctl_bars <- function(co_obj, order = RECV_CO_ORDER, title = NULL,
                              focal_name = NULL, focal_season = NULL, n_games = NULL) {
  
  pd <- co_obj$summary %>%
    filter(!is.na(pctl_co)) %>%
    mutate(label = factor(as.character(label), levels = rev(order))) %>%
    filter(!is.na(label))
  
  nm <- focal_name   %||% co_obj$focal_name   %||% "Focal"
  yr <- focal_season %||% co_obj$focal_season %||% ""
  ng <- n_games %||% dplyr::n_distinct(co_obj$per_game$week)
  auto_title <- paste0(nm, " — ", yr, " — vs common-opponent cohort")
  
  ggplot(pd, aes(x = pctl_co, y = label, fill = pctl_co)) +
    geom_col(width = 0.72) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey45", linewidth = 0.4) +
    geom_text(aes(label = scales::percent(pctl_co, accuracy = 1)),
              hjust = -0.15, size = 4, fontface = "bold", color = "grey20") +
    scale_fill_gradient2(low = "#08519c", mid = "#f7f7f7", high = "#a63603",
                         midpoint = 0.5, limits = c(0, 1),
                         labels = scales::percent, name = "Pctl") +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1.08),
                       breaks = c(0, .25, .5, .75, 1)) +
    labs(title    = title %||% auto_title,
         subtitle = paste0("percentile among same-archetype receivers facing the SAME defense, averaged across ",
                           ng, " games  |  dashed = 50th"),
         x = "Percentile", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title       = element_text(face = "bold", size = 16),
          plot.subtitle    = element_text(color = "grey40", size = 9),
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text.y      = element_text(size = 11, face = "bold"),
          legend.position  = "none")
}

# plot_co_pctl_bars(hollins_co)
