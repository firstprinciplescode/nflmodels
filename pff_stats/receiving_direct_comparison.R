#### ############################################################
#### MULTI-FOCAL COMPARISON LAYER — complete, self-contained
#### Prereqs already in session: compare_receiver_cohort,
#### receiving_func_base, receiver_registry, METRIC_LABELS,
#### common_opp_pctl, tidyverse
#### ############################################################

library(stringr)

`%||%` <- function(a, b) if (is.null(a)) b else a

#### 1) resolve a logged view's FULL params (defaults + what was passed) ####
resolve_view_params <- function(view) {
  cp <- attr(view, "call_params")
  if (is.null(cp)) stop("view has no call_params - build it with compare_receiver_cohort_logged()")
  defs <- formals(compare_receiver_cohort)
  defs <- defs[!names(defs) %in% c("df", "focal_player_id", "focal_season")]
  defs <- lapply(defs, function(x) tryCatch(eval(x), error = function(e) NULL))
  out  <- modifyList(defs, cp[!names(cp) %in% c("df", "focal_player_id", "focal_season")])
  if (length(out$grain) > 1) out$grain <- "season"
  out
}

#### 2) merge N views into one UNION filter set ####
# NULL categorical = unrestricted and dominates; bands take outer range; NA-flags OR
merge_view_params <- function(views) {
  ps  <- lapply(views, resolve_view_params)
  nms <- names(ps[[1]])
  merged <- lapply(nms, function(nm) {
    vals <- lapply(ps, `[[`, nm)
    if (nm %in% c("rte_cluster_input","tgt_cluster_input","align_cluster_input",
                  "position_group_input","man_zone_grp_input","xtd_grp_input","season_filter")) {
      if (any(vapply(vals, is.null, logical(1)))) return(NULL)
      return(unique(unlist(vals)))
    }
    if (nm %in% c("man_z_vec_input","xpass_vec_input","xtd_vec_input",
                  "pos_rank_vec","team_rank_vec")) {
      rng <- do.call(rbind, vals); return(c(min(rng[,1]), max(rng[,2])))
    }
    if (nm == "onfield_min") return(min(unlist(vals)))
    if (nm == "onfield_max") return(max(unlist(vals)))
    if (nm %in% c("man_z_na","xpass_na","xtd_grp_na","drop_part_cols")) return(any(unlist(vals)))
    if (nm == "min_games") return(min(unlist(vals)))
    vals[[1]]
  })
  names(merged) <- nms
  merged
}

#### 3) build union cohort with NO focal, then flag your guys ####
build_comparison_cohort <- function(views, focals, grain = c("season","game")) {
  grain <- match.arg(grain)
  mp <- merge_view_params(views)
  mp$grain <- grain
  mp$df    <- receiving_func_base
  coh <- do.call(compare_receiver_cohort, mp)
  lbl <- setNames(focals$label, paste(focals$player_id, focals$season))
  coh %>% mutate(focal_label = unname(lbl[paste(player_id, season)]),
                 is_focal    = !is.na(focal_label))
}

# reflag one guy at a time for common_opp_pctl (others stay in comp pool - part of the stick)
reflag <- function(cohort_df, label_in) cohort_df %>% mutate(is_focal = focal_label == label_in)

#### 4) registry -> role resolver (TD-split aware, latest-params aware) ####
registry_roles <- function(registry) {
  registry %>%
    mutate(
      focal_id  = as.integer(str_extract(params, "(?<=focal_player_id = )\\d+")),
      td_half   = case_when(str_detect(view_name, "_hightd$") ~ "TD_HIGH",
                            str_detect(view_name, "_lowtd$")  ~ "TD_LOW",
                            TRUE ~ "solo"),
      view_stem = str_remove(view_name, "_(hightd|lowtd)$")
    ) %>%
    arrange(desc(logged_at)) %>%
    distinct(view_name, .keep_all = TRUE)   # most recent params per view_name
}

player_views <- function(registry, player_ids, stem = c("season_view","game_view","wide_game")) {
  stem <- match.arg(stem)
  rr <- registry_roles(registry) %>%
    filter(focal_id %in% player_ids, str_ends(view_stem, stem)) %>%   # suffix match
    group_by(focal_id) %>%
    filter(!(td_half == "solo" & any(td_half != "solo"))) %>%         # halves supersede stale solo
    ungroup()
  if (!nrow(rr)) stop("no views at stem '", stem, "' for those player_ids")
  missing <- rr$view_name[!vapply(rr$view_name, exists, logical(1), envir = .GlobalEnv)]
  if (length(missing)) warning("in registry but not workspace, skipped: ",
                               paste(missing, collapse = ", "))
  rr <- rr %>% filter(!view_name %in% missing)
  message("using: ", paste(rr$view_name, collapse = ", "))
  setNames(lapply(rr$view_name, function(v) get(v, envir = .GlobalEnv)), rr$view_name)
}

#### 5) membership matrix: who clears whose native filters ####
role_membership <- function(role_params, focals) {   # named list of full param sets
  out <- lapply(names(role_params), function(nm) {
    p <- role_params[[nm]]; p$df <- receiving_func_base; p$grain <- "season"
    coh <- do.call(compare_receiver_cohort, p)
    tibble(player = focals$label,
           "{nm}" := vapply(seq_len(nrow(focals)), function(i)
             any(coh$player_id == focals$player_id[i] & coh$season == focals$season[i]), logical(1)))
  })
  Reduce(function(x, y) full_join(x, y, by = "player"), out)
}

#### 6) multi-focal heatmap: rows = metrics, cols = players, tile = pctl + raw ####
plot_focal_pctl_heatmap <- function(cohort_df,
                                    metrics = c("tgt_per_route","tgt_share_avg",
                                                "pbp_cp_oe","part_cp_oe","pbp_ypa_oe","part_ypa_oe",
                                                "pbp_yac_oe","part_yac_oe","adot","onfield_perc_avg"),
                                    label_map = METRIC_LABELS, player_order = NULL, title = NULL) {
  foc <- cohort_df %>% filter(is_focal)
  if (!nrow(foc)) { message("No focals flagged."); return(invisible(NULL)) }
  metrics <- metrics[metrics %in% names(foc)]
  if (is.null(player_order)) player_order <- unique(foc$focal_label)
  pd <- bind_rows(lapply(metrics, function(m) {
    x <- cohort_df[[m]]
    tibble(metric = m, player = foc$focal_label, raw = foc[[m]],
           pctl = vapply(foc[[m]], function(v) if (is.na(v)) NA_real_ else mean(x <= v, na.rm = TRUE), numeric(1)))
  })) %>%
    mutate(metric = factor(metric, levels = metrics,
                           labels = ifelse(metrics %in% names(label_map), unname(label_map[metrics]), metrics)),
           player = factor(player, levels = player_order),
           lbl = ifelse(is.na(pctl), "NA",
                        paste0(scales::percent(pctl, accuracy = 1), "\n", signif(raw, 2))))
  ggplot(pd, aes(player, metric, fill = pctl)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = lbl, color = abs(pctl - 0.5) > 0.25),
              size = 3, fontface = "bold", show.legend = FALSE, lineheight = 0.9) +
    scale_fill_gradient2(low = "#08519c", mid = "#f7f7f7", high = "#a63603",
                         midpoint = 0.5, limits = c(0,1), labels = scales::percent, name = "Pctl") +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    scale_x_discrete(position = "top") +
    labs(title = title %||% "Head-to-head — percentile within SHARED cohort",
         subtitle = paste0("one pool for everyone (n = ", nrow(cohort_df) - nrow(foc), " grey)  |  tile = pctl, below = raw"),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(color = "grey40", size = 9),
          panel.grid = element_blank(),
          axis.text.x.top = element_text(face = "bold", size = 10),
          axis.text.y = element_text(size = 11),
          legend.key.height = unit(1.2, "cm"))
}

#### ############################################################
#### USAGE
#### ############################################################

focals_2026 <- tibble(
  player_id = c(48327,        124087,           84329,         9579),
  season    = 2025,
  label     = c("A.J. Brown", "Kayshon Boutte", "Romeo Doubs", "Stefon Diggs")
)

# sanity check: see how the registry decomposes for your four guys
registry_roles(receiver_registry) %>%
  filter(focal_id %in% focals_2026$player_id) %>%
  select(view_name, focal_id, td_half, view_stem, logged_at)

# ALL FOUR, one shared pool
v_all <- player_views(receiver_registry, focals_2026$player_id, "season_view")
four_way <- build_comparison_cohort(unname(v_all), focals_2026, "season")
plot_focal_pctl_heatmap(four_way, player_order = focals_2026$label,
                        title = "2026 WR room — shared-pool percentiles (2025)")

# MATCHUP 1: the X job — Brown vs Boutte
v_bb <- player_views(receiver_registry, c(48327, 124087), "season_view")
brown_boutte <- build_comparison_cohort(unname(v_bb), focals_2026[1:2,], "season")
plot_focal_pctl_heatmap(brown_boutte, title = "The X job — Brown vs Boutte")

# MATCHUP 2: the possession job — Doubs vs Diggs
v_dd <- player_views(receiver_registry, c(84329, 9579), "season_view")
doubs_diggs <- build_comparison_cohort(unname(v_dd), focals_2026[3:4,], "season")
plot_focal_pctl_heatmap(doubs_diggs, title = "The possession job — Doubs vs Diggs")

# CAN HE BE THAT GUY — TD-split players merged into ONE role before checking
roles <- list(
  Brown  = merge_view_params(unname(player_views(receiver_registry, 48327, "season_view"))),
  Boutte = merge_view_params(unname(player_views(receiver_registry, 124087, "season_view"))),
  Doubs  = merge_view_params(unname(player_views(receiver_registry, 84329, "season_view"))),
  Diggs  = merge_view_params(unname(player_views(receiver_registry, 9579, "season_view")))
)
role_membership(roles, focals_2026)

# SOS layer: game grain off the wide stems, one focal at a time
v_bb_wide <- player_views(receiver_registry, c(48327, 124087), "wide_game")
bb_game <- build_comparison_cohort(unname(v_bb_wide), focals_2026[1:2,], "game")
brown_co  <- common_opp_pctl(reflag(bb_game, "A.J. Brown"),     min_comp = 3)
boutte_co <- common_opp_pctl(reflag(bb_game, "Kayshon Boutte"), min_comp = 3)
plot_co_pctl_bars(brown_co)
plot_co_pctl_bars(boutte_co)