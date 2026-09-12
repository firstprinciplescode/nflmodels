xtds_rank_cols <- list(
  QB  = c(pbp = "pbp_xtds_rank_def", part = "part_xtds_rank_def"),   # offense: ranks WITH def
  DEF = c(pbp = "pbp_xtds_rank",     part = "part_xtds_rank")        # defense: ranks WITHOUT def
)
xtds_adj <- -0.085   # everyone

xtds_medians <- dplyr::bind_rows(
  lapply(XTDS_QBS, function(e)
    qb_stats_df_final %>% dplyr::filter(qbgrp_ssn == e) %>%
      dplyr::summarise(entity = e, side = "QB", games = dplyr::n(),
                       med_pbp  = median(.data[[xtds_rank_cols$QB[["pbp"]]]],  na.rm = TRUE),
                       med_part = median(.data[[xtds_rank_cols$QB[["part"]]]], na.rm = TRUE))),
  lapply(XTDS_DEFS, function(e)
    qb_stats_df_final %>% dplyr::filter(def_ssn == e) %>%
      dplyr::summarise(entity = e, side = "DEF", games = dplyr::n(),
                       med_pbp  = median(.data[[xtds_rank_cols$DEF[["pbp"]]]],  na.rm = TRUE),
                       med_part = median(.data[[xtds_rank_cols$DEF[["part"]]]], na.rm = TRUE)))
) %>%
  dplyr::mutate(adj          = xtds_adj,
                med_pbp_adj  = med_pbp  + adj,
                med_part_adj = med_part + adj)

print(as.data.frame(xtds_medians), digits = 3)



xtds_q <- function(x, p) unname(stats::quantile(x, probs = p, type = 4, na.rm = TRUE))
# type 4 = position n*p, linear between the two bracketing sorted values
# (20 games x 0.477 = 9.54 -> between the 9th and 10th)

xtds_pct <- function(entity) {
  r <- xtds_medians[xtds_medians$entity == entity, ]
  c(pbp = r$med_pbp_adj, part = r$med_part_adj)
}
xtds_games <- function(entity, side) {
  if (side == "QB") qb_stats_df_final[qb_stats_df_final$qbgrp_ssn == entity, ]
  else              qb_stats_df_final[qb_stats_df_final$def_ssn   == entity, ]
}

# offense percentile carried into each defense's game set
xtds_off_on_def <- dplyr::bind_rows(lapply(XTDS_QBS, function(q) lapply(XTDS_DEFS, function(d) {
  g <- xtds_games(d, "DEF"); p <- xtds_pct(q)
  data.frame(pct_from = q, games_of = d, n = nrow(g), pct_pbp = p[["pbp"]], pct_part = p[["part"]],
             pbp_xtds = xtds_q(g$pbp_xtds, p[["pbp"]]), part_xtds = xtds_q(g$part_xtds, p[["part"]]))
})))

# defense percentile carried into each QB's game set
xtds_def_on_off <- dplyr::bind_rows(lapply(XTDS_DEFS, function(d) lapply(XTDS_QBS, function(q) {
  g <- xtds_games(q, "QB"); p <- xtds_pct(d)
  data.frame(pct_from = d, games_of = q, n = nrow(g), pct_pbp = p[["pbp"]], pct_part = p[["part"]],
             pbp_xtds = xtds_q(g$pbp_xtds, p[["pbp"]]), part_xtds = xtds_q(g$part_xtds, p[["part"]]))
})))

cat("\n-- offense percentile -> defense's games --\n"); print(xtds_off_on_def, digits = 3, row.names = FALSE)
cat("\n-- defense percentile -> QB's games --\n");      print(xtds_def_on_off, digits = 3, row.names = FALSE)


xtds_off_on_def$pbp_xtds  <- round(xtds_off_on_def$pbp_xtds,  3)
xtds_off_on_def$part_xtds <- round(xtds_off_on_def$part_xtds, 3)
xtds_def_on_off$pbp_xtds  <- round(xtds_def_on_off$pbp_xtds,  3)
xtds_def_on_off$part_xtds <- round(xtds_def_on_off$part_xtds, 3)

print(xtds_off_on_def, digits = 5, row.names = FALSE)
print(xtds_def_on_off, digits = 5, row.names = FALSE)
