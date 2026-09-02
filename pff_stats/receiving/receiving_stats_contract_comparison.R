# contracts <- nflreadr::load_contracts()

# id_bridge <- combined_ids %>%
#  distinct(player_id, player, gsis_id)

cohort_gsis <- barner_season_view %>%
 left_join(id_bridge %>% distinct(player_id, gsis_id), by = "player_id")

cohort_contracts <- cohort_gsis %>%
 distinct(player_id, gsis_id) %>%
 inner_join(contracts, by = "gsis_id")

# cohort_gsis %>% filter(is.na(gsis_id)) %>% distinct(player_id, player)          # no gsis bridge
# setdiff(cohort_gsis$gsis_id, contracts$gsis_id)                                  # gsis with no OTC contract

cohort_seasons <- barner_season_view %>% distinct(player_id, season)

# contract active DURING each row's season (most recent signing if deals overlap)
active_contracts <- cohort_seasons %>%
  inner_join(cohort_contracts, by = "player_id", relationship = "many-to-many") %>%
  filter(year_signed <= season, year_signed + years > season) %>%
  group_by(player_id, season) %>%
  slice_max(year_signed, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, season, active_apy = apy, active_apy_pct = apy_cap_pct, active_value = value)

next_contracts <- cohort_seasons %>%
  inner_join(cohort_contracts, by = "player_id", relationship = "many-to-many") %>%
  filter(year_signed > season) %>%
  group_by(player_id, season) %>%
  slice_min(year_signed, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, season, next_apy = apy, next_apy_pct = apy_cap_pct, next_year = year_signed)

cohort_pay <- barner_season_view %>%
  left_join(active_contracts, by = c("player_id","season")) %>%
  left_join(next_contracts,   by = c("player_id","season")) %>%
  mutate(apy_pct_rank = (rank(active_apy_pct, ties.method = "average", na.last = "keep") - 1) /
           (sum(!is.na(active_apy_pct)) - 1))


`%||%` <- function(a, b) if (is.null(a)) b else a

# metrics that define "production" for this archetype - efficiency OE + involvement
prod_metrics <- c("part_ypa_oe","part_yac_oe","pbp_cp_oe","tgt_per_route","tgt_share_avg","adot")

pr <- function(x) (rank(x, ties.method = "average", na.last = "keep") - 1) / (sum(!is.na(x)) - 1)

pay_prod <- cohort_pay %>%
  mutate(across(all_of(prod_metrics), pr, .names = "{.col}_pctl")) %>%
  rowwise() %>%
  mutate(prod_pctl = mean(c_across(ends_with("_pctl")), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(lbl = paste0(player, " '", substr(season, 3, 4)))

plot_pay_vs_prod <- function(d, title = NULL) {
  ggplot(d, aes(prod_pctl, apy_pct_rank)) +
    annotate("rect", xmin = .5, xmax = 1, ymin = 0, ymax = .5, fill = "#2ca02c", alpha = .06) +  # underpaid
    annotate("rect", xmin = 0, xmax = .5, ymin = .5, ymax = 1, fill = "#d62728", alpha = .06) +  # overpaid
    geom_hline(yintercept = .5, linetype = "dashed", color = "grey55", linewidth = .3) +
    geom_vline(xintercept = .5, linetype = "dashed", color = "grey55", linewidth = .3) +
    geom_point(data = ~ filter(.x, !is_focal), color = "grey55", size = 3, alpha = .8) +
    geom_point(data = ~ filter(.x, is_focal),  color = "firebrick2", size = 5) +
    geom_text(aes(label = lbl), size = 3, vjust = -1, color = "grey25", check_overlap = TRUE) +
    annotate("text", x = .98, y = .02, label = "UNDERPAID", hjust = 1, fontface = "bold",
             color = "#2ca02c", size = 3.5) +
    annotate("text", x = .02, y = .98, label = "OVERPAID", hjust = 0, fontface = "bold",
             color = "#d62728", size = 3.5) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1.02)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1.02)) +
    labs(title = title %||% "Cohort — production vs pay (percentile within cohort)",
         subtitle = "x = composite production pctl  |  y = APY cap-% pctl  |  red = Hunter Henry",
         x = "Production percentile", y = "Pay percentile (APY % of cap)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(color = "grey40", size = 9),
          panel.grid.minor = element_blank())
}

plot_pay_vs_prod(pay_prod)
