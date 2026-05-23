comparison_blitz_def_func("DET2025", 1.05) # 21
comparison_depth_def_func("DET2025", 1.02) # 35
comparison_less_def_func("DET2025", .97) # 77
comparison_pa_def_func("DET2025", 1) # 59
comparison_pressure_def_func("DET2025", 1.05) # 25

all_def <- rbind(as.data.frame(comparison_blitz_def_func("DET2025", 1.045)), 
                 as.data.frame(comparison_depth_def_func("DET2025", 1.015)), 
                 as.data.frame(comparison_less_def_func("DET2025", .965)), 
                 as.data.frame(comparison_pa_def_func("DET2025", .995)), 
                 as.data.frame(comparison_pressure_def_func("DET2025", 1.015)))

sim_def <- sqldf("SELECT QB, COUNT(*) AS CNT
        FROM  all_def 
        GROUP BY  QB
        HAVING  CNT >= 3") %>% select(QB) %>% distinct()

sim_def


det_blitz_def <- df_pressure_def_scaled_z %>%
  filter(def_ssn %in% c(sim_def$QB, "DET2025")) %>%
  select(-contains("snaps"), -contains("int_rate")) 

# Preserve original column order from the CSV as the y-axis order
ordered_vars_def <- det_blitz_def %>%
  select(-def_ssn) %>%
  names() %>%
  sub("_(Good|Bad|diff)$", "", .) %>%
  unique()                # first-appearance order, no dupes

det_long_def <- det_blitz_def %>%
  pivot_longer(-def_ssn, names_to = "var", values_to = "z") %>%
  mutate(
    bucket = case_when(
      grepl("_Good$", var) ~ "Good",
      grepl("_Bad$",  var) ~ "Bad",
      grepl("_diff$", var) ~ "Diff (G-B)"
    ),
    var_label     = sub("_(Good|Bad|diff)$", "", var),
    team_pff      = sub("^([A-Z]{2,3})[A-Z][a-z]+-\\d{4}$", "\\1", def_ssn),
    team_nflverse = dplyr::coalesce(pff_to_nflverse[team_pff], team_pff)
  ) %>%
  filter(!is.na(z), !is.na(bucket)) %>%
  mutate(bucket    = factor(bucket, levels = c("Good","Bad","Diff (G-B)")),
         var_label = factor(var_label, levels = rev(ordered_vars_def)))   # FIX: ordered_vars_def

det_summary_def <- det_long_def %>%
  group_by(var_label, bucket) %>%
  summarise(
    vs      = z[def_ssn == "DET2025"],
    cc      = median(z[def_ssn != "DET2025"], na.rm = TRUE),   # FIX: !=
    cc_mean = mean(z[def_ssn != "DET2025"], na.rm = TRUE),     # FIX: !=
    .groups = "drop"
  )

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


plot_strip <- function(df, bkt = "Good", focal = "DETGoff-2025", id_col = "qbgrp_ssn") {
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
    geom_point(data = . %>% filter(.data[[id_col]] != focal),
               color = "grey40", alpha = 0.75, size = 2.2) +
    geom_point(data = . %>% filter(.data[[id_col]] == focal),
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

plot_dumb <- function(df, bkt = "Good", focal = "DETGoff-2025") {
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

# Calls
plot_strip(df = det_long_def,    bkt = "Good", focal = "DET2025", id_col = "def_ssn")
plot_dumb(df = det_summary_def,  bkt = "Good", focal = "DET2025")

plot_strip(df = det_long_def,    bkt = "Bad",  focal = "DET2025", id_col = "def_ssn")
plot_dumb(df = det_summary_def,  bkt = "Bad",  focal = "DET2025")

plot_strip(df = det_long_def,    bkt = "Diff (G-B)", focal = "DET2025", id_col = "def_ssn")
plot_dumb(df = det_summary_def,  bkt = "Diff (G-B)", focal = "DET2025")




df_blitz_def_scaled_z %>%
  filter(def_ssn %in% c(sim_def$QB, "DET2025")) %>%
  aws.s3::s3write_using(write.csv, row.names = FALSE,
                        object = "outputs/tb2025_blitz_comps.csv",
                        bucket = "nfl-pff-data-lucas")

df_blitz_def_scaled_z %>%
  filter(def_ssn %in% c("WAS2024", "TB2024", "TB2025")) %>%
  select(def_ssn, blitz_rate_Good)


df_blitz_def_scaled_z %>%
  filter(blitz_rate_diff <= .25, no_blitz_twp_rate_Bad <= -.25, no_blitz_adot_Good >= -.25, blitz_grade_Bad <= .5) %>% 
  pull(def_ssn)
 
df_pa_def_scaled_z %>%
  filter(pa_time_to_throw_Good >= -.8, pa_scr_rate_Bad >= .5, ttt_difference_Bad >= .35, qbr_difference_diff >= -.45) %>% 
  pull(def_ssn)  
