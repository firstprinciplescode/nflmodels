#### CONTRACT LADDER — hypothetical cap hits vs required production ####

# pay_prod_doubs <- pay_prod

# 2026 cap derived from the signed deal itself ($17M APY = 5.6% of cap)
cap_2026 <- pay_prod_doubs %>% filter(player_id == 84329) %>% slice(1) %>%
  mutate(cap = next_apy / next_apy_pct) %>% pull(cap)

pay_ladder <- function(pay_prod_df, player_id_in, seasons_in,
                       ladder_m = c(3, 8, 14, 17, 19), cap = cap_2026) {
  apys <- pay_prod_df$active_apy_pct
  foc  <- pay_prod_df %>% filter(player_id == player_id_in, season %in% seasons_in)
  tidyr::expand_grid(season = foc$season, cap_hit_m = ladder_m) %>%
    mutate(
      prod_pctl = foc$prod_pctl[match(season, foc$season)],
      cap_pct   = cap_hit_m / cap,
      pay_pctl  = vapply(cap_pct, function(v) mean(apys <= v, na.rm = TRUE), numeric(1)),
      req_prod  = pay_pctl,                 # break-even: production pctl >= pay pctl
      gap       = prod_pctl - pay_pctl,     # >0 justified, <0 short
      cap_hit_m = factor(paste0("$", cap_hit_m, "M"), levels = paste0("$", ladder_m, "M"))
    )
}

ladder_doubs <- pay_ladder(pay_prod_doubs, 84329, c(2024, 2025))
ladder_doubs %>% select(season, cap_hit_m, cap_pct, pay_pctl, prod_pctl, gap)

plot_pay_ladder <- function(pay_prod_df, ladder_df, focal_lbl = "Romeo Doubs",
                            player_id_in = 84329) {
  
  # player captions ("Name 'YY"), built if the column isn't there
  if (!"lbl" %in% names(pay_prod_df)) {
    pay_prod_df <- pay_prod_df %>%
      mutate(lbl = paste0(player, " '", substr(season, 3, 4)))
  }
  
  # rungs sharing a pay percentile -> one point, one collapsed label
  rungs <- ladder_df %>%
    group_by(season, prod_pctl, pay_pctl) %>%
    summarise(rung_lbl = paste(cap_hit_m, collapse = " / "), .groups = "drop")
  
  # focal's ACTUAL contract position (rookie deal), for contrast vs the ladder
  actual <- pay_prod_df %>% filter(player_id == player_id_in, !is.na(apy_pct_rank))
  
  # ggrepel if you've got it, plain overlapping-ok labels if not
  use_repel <- requireNamespace("ggrepel", quietly = TRUE)
  
  p <- ggplot(pay_prod_df, aes(prod_pctl, apy_pct_rank)) +
    annotate("rect", xmin = .5, xmax = 1, ymin = 0, ymax = .5, fill = "#2ca02c", alpha = .06) +
    annotate("rect", xmin = 0, xmax = .5, ymin = .5, ymax = 1, fill = "#d62728", alpha = .06) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = .4) +
    geom_point(color = "grey65", size = 2.5, alpha = .8) +
    # ladder: line + points + collapsed rung labels
    geom_line(data = rungs, aes(y = pay_pctl, group = season, color = factor(season)),
              linewidth = .7) +
    geom_point(data = rungs, aes(y = pay_pctl, color = factor(season)), size = 4) +
    geom_text(data = rungs,
              aes(y = pay_pctl, label = rung_lbl, color = factor(season)),
              hjust = -0.15, size = 3.3, fontface = "bold", show.legend = FALSE) +
    # actual contract position: open diamonds
    geom_point(data = actual, shape = 5, size = 4, stroke = 1.4, color = "grey15") +
    geom_text(data = actual, aes(label = paste0("actual ", season)),
              vjust = 1.8, size = 3, color = "grey15", fontface = "italic") +
    annotate("text", x = .98, y = .03, label = "TEAM-FRIENDLY", hjust = 1,
             fontface = "bold", color = "#2ca02c", size = 3.5) +
    annotate("text", x = .03, y = .98, label = "OVERPAYING", hjust = 0,
             fontface = "bold", color = "#d62728", size = 3.5) +
    scale_color_manual(values = c("2024" = "#08519c", "2025" = "firebrick2"),
                       name = "Doubs season") +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1.02)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1.02)) +
    labs(title = paste0(focal_lbl, " — contract ladder vs production"),
         subtitle = "grey = cohort | dashed diagonal = break-even | diamonds = actual rookie-deal pay | rung above line = overpaying",
         x = "Production percentile (within cohort)",
         y = "Pay percentile (cap charge as % of 2026 cap)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(color = "grey40", size = 9),
          panel.grid.minor = element_blank(),
          legend.position = "top")
  
  # cohort player captions
  if (use_repel) {
    p <- p + ggrepel::geom_text_repel(aes(label = lbl), size = 2.7, color = "grey35",
                                      max.overlaps = 20, seed = 42)
  } else {
    p <- p + geom_text(aes(label = lbl), size = 2.7, vjust = -1, color = "grey35",
                       check_overlap = TRUE)
  }
  p
}

plot_pay_ladder(pay_prod_doubs, ladder_doubs)
