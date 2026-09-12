comparison_blitz_func("DENNix-2025", .89) # 138
comparison_depth_func("DENNix-2025", .995) # 69
comparison_less_func("DENNix-2025", .915) # 123
comparison_pa_func("DENNix-2025", .975) # 88
comparison_pressure_func("DENNix-2025", .93) # 114

all_qbs <- rbind(as.data.frame(comparison_blitz_func("DENNix-2025", .84)), 
             as.data.frame(comparison_depth_func("DENNix-2025", .945)), 
             as.data.frame(comparison_less_func("DENNix-2025", .865)), 
             as.data.frame(comparison_pa_func("DENNix-2025", .925)), 
             as.data.frame(comparison_pressure_func("DENNix-2025", .88)))

sim_qb <- sqldf("SELECT QB, COUNT(*) AS CNT
        FROM  all_qbs 
        GROUP BY  QB
        HAVING  CNT >= 5") %>% select(QB) %>% distinct()

sim_qb

### UPDATE THIS FIRST ####
det_blitz <- df_blitz_scaled_z %>%
  filter(qbgrp_ssn %in% c(sim_qb$QB, "DENNix-2025")) %>%
  select(-contains("snaps"), -contains("int_rate"))
  # %>%
  # select(-contains("adot"))


setup_team_logos <- function(dest_dir = "team_logos") {
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  
  teams <- nflreadr::load_teams() %>%
    dplyr::filter(!is.na(team_logo_espn) & nzchar(team_logo_espn)) %>%
    
    dplyr::select(team_abbr, team_logo_espn)
  
  for (i in seq_len(nrow(teams))) {
    abbr <- teams$team_abbr[i]
    url  <- teams$team_logo_espn[i]
    dest <- file.path(dest_dir, paste0(abbr, ".png"))
    if (!file.exists(dest)) {
      ok <- tryCatch({
        suppressWarnings(download.file(url, dest, mode = "wb", quiet = TRUE))
        TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (!ok) message("Failed: ", abbr)
    }
  }
  message("Done. ", length(list.files(dest_dir)), " logos in ", normalizePath(dest_dir))
}

setup_team_logos("C:/Users/vflre/Downloads/nflmodels_UPDATE/team_logos")   # or wherever


# ABSOLUTE PATH — no more working-directory guessing
logo_dir <- "C:/Users/vflre/Downloads/nflmodels_UPDATE/team_logos"

pff_to_nflverse <- c(ARZ = "ARI", BLT = "BAL", CLV = "CLE", HST = "HOU",
                     SD = "LAC", OAK = "LV")


# Preserve original column order from the CSV as the y-axis order
ordered_vars <- det_blitz %>%
  select(-qbgrp_ssn) %>%
  names() %>%
  sub("_(Good|Bad|diff)$", "", .) %>%
  unique()                # first-appearance order, no dupes

det_long <- det_blitz %>%
  pivot_longer(-qbgrp_ssn, names_to = "var", values_to = "z") %>%
  mutate(
    bucket = case_when(
      grepl("_Good$", var) ~ "Good",
      grepl("_Bad$",  var) ~ "Bad",
      grepl("_diff$", var) ~ "Diff (G-B)"
    ),
    var_label     = sub("_(Good|Bad|diff)$", "", var),
    team_pff      = sub("^([A-Z]{2,3})[A-Z][a-z]+-\\d{4}$", "\\1", qbgrp_ssn),
    team_nflverse = dplyr::coalesce(pff_to_nflverse[team_pff], team_pff)
  ) %>%
  filter(!is.na(z), !is.na(bucket)) %>%
  mutate(bucket = factor(bucket, levels = c("Good","Bad","Diff (G-B)")),
         var_label = factor(var_label, levels = rev(ordered_vars)))

det_summary <- det_long %>%
  group_by(var_label, bucket) %>%
  summarise(
    vs      = z[qbgrp_ssn == "DENNix-2025"],
    cc      = median(z[qbgrp_ssn != "DENNix-2025"], na.rm = TRUE),
    cc_mean = mean(z[qbgrp_ssn != "DENNix-2025"], na.rm = TRUE),
    .groups = "drop"
  )


# If FALSE, run this to see which are missing:
# det_long %>% distinct(team_nflverse, logo_path) %>% mutate(exists = file.exists(logo_path)) %>% filter(!exists)

# Build the x-axis gradient ONCE — same for every row, every facet
blend_white <- function(col, alpha) {
  rgb_vals <- col2rgb(col) / 255
  blended  <- rgb_vals * alpha + 1 * (1 - alpha)
  rgb(blended[1,], blended[2,], blended[3,])
}

n_steps <- 200
z_seq   <- seq(-2.5, 2.5, length.out = n_steps)
col_seq <- scales::col_numeric(
  palette = c("#2c6daa", "white", "#b03a3a"),
  domain  = c(-2.5, 2.5)
)(z_seq)
gradient_raster <- matrix(blend_white(col_seq, 0.4), nrow = 1)


plot_strip <- function(df = det_long, bkt = "Good", focal = "DENNix-2025") {
  ggplot(df %>% filter(bucket == bkt),
         aes(x = z, y = var_label)) +
    annotation_raster(gradient_raster,
                      xmin = -2.5, xmax = 2.5,
                      ymin = -Inf, ymax = Inf,
                      interpolate = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey25", linewidth = 0.5) +
    geom_vline(xintercept = seq(-2.4, 2.4, by = 0.4),
               linetype = "dotted", color = "grey60", linewidth = 0.4) +
    geom_point(data = . %>% filter(qbgrp_ssn != focal),
               color = "grey40", alpha = 0.75, size = 2.2) +
    geom_point(data = . %>% filter(qbgrp_ssn == focal),
               shape = 21, size = 4.5, color = "black",
               fill = "#0076b6", stroke = 1.1) +
    scale_x_continuous(breaks = seq(-2.4, 2.4, by = 0.4),
                       labels = function(x) sprintf("%.1f", x)) +
    coord_cartesian(xlim = c(-2.5, 2.5), clip = "off") +
    labs(title    = paste0(focal, " (blue) vs cohort — ", bkt),
         subtitle = "Background: white=0, blue=neg, red=pos",
         x = "z-score (within cohort)", y = NULL) +
    theme_minimal(base_size = 9) +
    theme(plot.title        = element_text(face = "bold"),
          panel.grid        = element_blank(),
          axis.ticks.x      = element_line(color = "grey30", linewidth = 0.5),
          axis.ticks.length = unit(0.18, "cm"),
          axis.text.y       = element_text(size = 7))
}

plot_dumb <- function(df = det_summary, bkt = "Good", focal = "DENNix-2025") {
  ggplot(df %>% filter(bucket == bkt), aes(y = var_label)) +
    annotation_raster(gradient_raster,
                      xmin = -2.5, xmax = 2.5,
                      ymin = -Inf, ymax = Inf,
                      interpolate = TRUE) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey25", linewidth = 0.5) +
    geom_vline(xintercept = seq(-2.4, 2.4, by = 0.4),
               linetype = "dotted", color = "grey60", linewidth = 0.4) +
    geom_segment(aes(x = vs, xend = cc, y = var_label, yend = var_label),
                 color = "grey30", linewidth = 0.5) +
    geom_point(aes(x = cc), shape = 23, size = 3.5, color = "black",
               fill = "white", stroke = 1) +
    geom_point(aes(x = cc_mean), shape = 21, size = 2.5, color = "black",
               fill = "white", stroke = 0.7) +
    geom_point(aes(x = vs), shape = 21, size = 4.5, color = "black",
               fill = "#0076b6", stroke = 1.1) +
    scale_x_continuous(breaks = seq(-2.4, 2.4, by = 0.4),
                       labels = function(x) sprintf("%.1f", x)) +
    coord_cartesian(xlim = c(-2.5, 2.5), clip = "off") +
    labs(title    = paste0(focal, " (blue) vs Cohort — ", bkt),
         subtitle = "Diamond = median  |  Small circle = mean  |  Background: white=0, blue=neg, red=pos",
         x = "z-score", y = NULL) +
    theme_minimal(base_size = 9) +
    theme(plot.title        = element_text(face = "bold"),
          plot.subtitle     = element_text(size = 8),
          panel.grid        = element_blank(),
          axis.ticks.x      = element_line(color = "grey30", linewidth = 0.5),
          axis.ticks.length = unit(0.18, "cm"),
          axis.text.y       = element_text(size = 7))
}

# Call however you want
plot_strip(df = det_long,    bkt = "Good", focal = "DENNix-2025")
plot_dumb(df = det_summary,  bkt = "Good", focal = "DENNix-2025")

plot_strip(df = det_long,    bkt = "Bad",  focal = "DENNix-2025")
plot_dumb(df = det_summary,  bkt = "Bad",  focal = "DENNix-2025")

plot_strip(df = det_long,    bkt = "Diff (G-B)", focal = "DENNix-2025")
plot_dumb(df = det_summary,  bkt = "Diff (G-B)", focal = "DENNix-2025")



sim_qb2 <- sqldf("SELECT QB, COUNT(*) AS CNT
        FROM  all_qbs 
        GROUP BY  QB
        HAVING  CNT >= 4") %>% select(QB) %>% distinct()

sim_qb2


df_less_scaled_z %>%
  filter(qbgrp_ssn %in% c("NEMaye-2025", "NEMaye-2024", "HSTWatson-2019", "TENMariota-2018")) %>%
  select(-contains("snaps"), -contains("int_rate"))


### UPDATE THIS FIRST ####
det_blitz2 <- df_pressure_scaled_z %>%
  filter(qbgrp_ssn %in% c(sim_qb2$QB, "DENNix-2025")) %>%
  select(-contains("snaps"), -contains("int_rate"))

# BLTJackson-2024, DENNix-2025, HSTWatson-2020

df_pressure_scaled_z %>%
  filter(qbgrp_ssn %in% c("BLTJackson-2024", "DENNix-2025", "HSTWatson-2020")) %>%
  select(pressure_ypa_Good, pressure_ypa_Bad, pressure_ypa_diff)



df_depth_scaled_z %>%
  filter(qbgrp_ssn %in% c(sim_qb$QB, "TBMayfield-2025")) %>%
  select(qbgrp_ssn, ends_with("_Good"))

df_pa_scaled_z %>%
  filter(qbgrp_ssn %in% c("TBMayfield-2024", "TBMayfield-2025")) %>%
  select(qbgrp_ssn, pressure_rate_difference_Bad)

df_pa_scaled_z %>%
  filter(qbgrp_ssn %in% c(sim_qb$QB, "TBMayfield-2025")) %>%
  aws.s3::s3write_using(write.csv, row.names = FALSE,
                        object = "outputs/mayfield_pa_comps.csv",
                        bucket = "nfl-pff-data-lucas")


df_less_scaled_z %>%
  filter(less_rate_Bad >= -1.1, less_rate_Bad <= .7, adot_difference_Bad >= -1, adot_difference_Bad <= 1, less_sack_pct_Bad <= .9, less_sack_pct_Bad >= -1.1, less_pressure_rate_Good <= .6, less_pressure_rate_Good >= -1.4, qbr_difference_Bad >= -1.1, qbr_difference_Bad <= .9, less_qbr_Bad >= .4) %>%
  pull(qbgrp_ssn)

df_pressure_scaled_z %>%
  filter(pressure_rate_Good <= .65, pressure_rate_Good >= -1.15, no_pressure_qbr_Good >= .05, no_pressure_qbr_Good <= 1.95, pressure_time_to_throw_Bad <= 1.05, pressure_time_to_throw_Bad >= -.85, pressure_grade_Bad >= -1.45, pressure_grade_Bad <= .45, acc_pct_difference_Bad <= 1.65, acc_pct_difference_Bad >= -.35) %>%
  pull(qbgrp_ssn)


sqldf("SELECT qbgrp_ssn,
              AVG(pass_rate_rank_def)              AS mn_pass_rate_rank_def,
              --XPASS_DEF METRICS
              AVG(fastr_xpass_rate_rank_def)       AS mn_fastr_xpass_rate_rank_def,
              AVG(pbp_xpass_rate_rank_def)         AS mn_pbp_xpass_rate_rank_def,
              AVG(part_xpass_rate_rank_def)        AS mn_part_xpass_rate_rank_def,
              --SCR_RATE_DEF METRICS
              AVG(scr_rate_rank_def)               AS mn_scr_rate_rank_def,
              --XTDS_DEF METRICS
              AVG(pbp_xtds_rank_def)               AS mn_pbp_xtds_rank_def,
              AVG(part_xtds_rank_def)              AS mn_part_xtds_rank_def,
              --PRESSURE_RATE_DEF METRICS (all of them)
              AVG(pressure_rate_rank_def)          AS mn_pressure_rate_rank_def,
              AVG(less_pressure_rate_rank_def)     AS mn_less_pressure_rate_rank_def,
              AVG(more_pressure_rate_rank_def)     AS mn_more_pressure_rate_rank_def,
              AVG(blitz_pressure_rate_rank_def)    AS mn_blitz_pressure_rate_rank_def,
              AVG(no_blitz_pressure_rate_rank_def) AS mn_no_blitz_pressure_rate_rank_def,
              AVG(npa_pressure_rate_rank_def)      AS mn_npa_pressure_rate_rank_def,
              AVG(pa_pressure_rate_rank_def)       AS mn_pa_pressure_rate_rank_def,
              AVG(pbp_pressure_rank_def)           AS mn_pbp_pressure_rank_def,
              AVG(part_pressure_before_rank_def)   AS mn_part_pressure_before_rank_def,
              AVG(part_pressure_after_rank_def)    AS mn_part_pressure_after_rank_def,
              --QBR_DEF METRICS
              AVG(pressure_qbr_rank_def)           AS mn_pressure_qbr_rank_def,
              AVG(no_pressure_qbr_rank_def)        AS mn_no_pressure_qbr_rank_def,
              AVG(blitz_qbr_rank_def)              AS mn_blitz_qbr_rank_def,
              AVG(no_blitz_qbr_rank_def)           AS mn_no_blitz_qbr_rank_def,
              AVG(less_qbr_rank_def)               AS mn_less_qbr_rank_def,
              AVG(more_qbr_rank_def)               AS mn_more_qbr_rank_def,
              AVG(pa_qbr_rank_def)                 AS mn_pa_qbr_rank_def,
              AVG(npa_qbr_rank_def)                AS mn_npa_qbr_rank_def
       FROM   qb_stats_df_final
       WHERE  qbgrp_ssn IN ('NEMaye-2025', 'NEMaye-2024', 'CARNewton-2017', 'CARNewton-2018',
                            'PHIWentz-2018', 'PHIWentz-2019', 'SFGaroppolo-2019', 
                            'DALPrescott-2022', 'DALPrescott-2025', 'PHIHurts-2024', 'PHIHurts-2025',
                            'SFGaroppolo-2020', 'SFGaroppolo-2021',
                            'TENMariota-2018', 'TENTannehill-2019')
       GROUP BY qbgrp_ssn")



sqldf("SELECT qbgrp_ssn,
              AVG(pass_rate_rank_def)              AS mn_pass_rate_rank_def,
              --XPASS_DEF METRICS
              AVG(fastr_xpass_rate_rank_def)       AS mn_fastr_xpass_rate_rank_def,
              AVG(pbp_xpass_rate_rank_def)         AS mn_pbp_xpass_rate_rank_def,
              AVG(part_xpass_rate_rank_def)        AS mn_part_xpass_rate_rank_def,
              --SCR_RATE_DEF METRICS
              AVG(scr_rate_rank_def)               AS mn_scr_rate_rank_def,
              --XTDS_DEF METRICS
              AVG(pbp_xtds_rank_def)               AS mn_pbp_xtds_rank_def,
              AVG(part_xtds_rank_def)              AS mn_part_xtds_rank_def,
              --PRESSURE_RATE_DEF METRICS (all of them)
              AVG(pressure_rate_rank_def)          AS mn_pressure_rate_rank_def,
              AVG(less_pressure_rate_rank_def)     AS mn_less_pressure_rate_rank_def,
              AVG(more_pressure_rate_rank_def)     AS mn_more_pressure_rate_rank_def,
              AVG(blitz_pressure_rate_rank_def)    AS mn_blitz_pressure_rate_rank_def,
              AVG(no_blitz_pressure_rate_rank_def) AS mn_no_blitz_pressure_rate_rank_def,
              AVG(npa_pressure_rate_rank_def)      AS mn_npa_pressure_rate_rank_def,
              AVG(pa_pressure_rate_rank_def)       AS mn_pa_pressure_rate_rank_def,
              AVG(pbp_pressure_rank_def)           AS mn_pbp_pressure_rank_def,
              AVG(part_pressure_before_rank_def)   AS mn_part_pressure_before_rank_def,
              AVG(part_pressure_after_rank_def)    AS mn_part_pressure_after_rank_def,
              --QBR_DEF METRICS
              AVG(pressure_qbr_rank_def)           AS mn_pressure_qbr_rank_def,
              AVG(no_pressure_qbr_rank_def)        AS mn_no_pressure_qbr_rank_def,
              AVG(blitz_qbr_rank_def)              AS mn_blitz_qbr_rank_def,
              AVG(no_blitz_qbr_rank_def)           AS mn_no_blitz_qbr_rank_def,
              AVG(less_qbr_rank_def)               AS mn_less_qbr_rank_def,
              AVG(more_qbr_rank_def)               AS mn_more_qbr_rank_def,
              AVG(pa_qbr_rank_def)                 AS mn_pa_qbr_rank_def,
              AVG(npa_qbr_rank_def)                AS mn_npa_qbr_rank_def
       FROM   qb_stats_df_final
       WHERE  qbgrp_ssn IN ('NEMaye-2025', 'DALPrescott-2025', 'TENTannehill-2019')
       GROUP BY qbgrp_ssn")