comparison_blitz_func("DETGoff-2025", .97) # 147 # .93 - 120
comparison_depth_func("DETGoff-2025", 1.14) # 42 # 1.1 - 20
comparison_less_func("DETGoff-2025", 1.02) # 106 # .98 - 74
comparison_pa_func("DETGoff-2025", 1) # 131 #.96 - 96
comparison_pressure_func("DETGoff-2025", 1.01) # 122 # .96 - 99

all_qbs <- rbind(as.data.frame(comparison_blitz_func("DETGoff-2025", .957)), 
             as.data.frame(comparison_depth_func("DETGoff-2025", 1.127)), 
             as.data.frame(comparison_less_func("DETGoff-2025", 1.007)), 
             as.data.frame(comparison_pa_func("DETGoff-2025", .987)), 
             as.data.frame(comparison_pressure_func("DETGoff-2025", .997)))

sim_qb <- sqldf("SELECT QB, COUNT(*) AS CNT
        FROM  all_qbs 
        GROUP BY  QB
        HAVING  CNT >= 5") %>% select(QB) %>% distinct()

sim_qb


det_blitz <- df_pressure_scaled_z %>%
  filter(qbgrp_ssn %in% c(sim_qb$QB, "DETGoff-2025")) %>%
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
    vs      = z[qbgrp_ssn == "DETGoff-2025"],
    cc      = median(z[qbgrp_ssn != "DETGoff-2025"], na.rm = TRUE),
    cc_mean = mean(z[qbgrp_ssn != "DETGoff-2025"], na.rm = TRUE),
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


plot_strip <- function(df = det_long, bkt = "Good", focal = "DETGoff-2025") {
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

plot_dumb <- function(df = det_summary, bkt = "Good", focal = "DETGoff-2025") {
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
plot_strip(df = det_long,    bkt = "Good", focal = "DETGoff-2025", id_col = "qbgrp_ssn")
plot_dumb(df = det_summary,  bkt = "Good", focal = "DETGoff-2025")

plot_strip(df = det_long,    bkt = "Bad",  focal = "DETGoff-2025", id_col = "qbgrp_ssn")
plot_dumb(df = det_summary,  bkt = "Bad",  focal = "DETGoff-2025")

plot_strip(df = det_long,    bkt = "Diff (G-B)", focal = "DETGoff-2025", id_col = "qbgrp_ssn")
plot_dumb(df = det_summary,  bkt = "Diff (G-B)", focal = "DETGoff-2025")




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


df_depth_scaled_z %>%
  filter(short_qbr_Good >= -.15, ypa_difference_diff >= -.25, ms_acc_pct_difference_diff >= 0, medium_twp_rate_Bad >= 0) %>%
  pull(qbgrp_ssn)

df_pa_scaled_z %>%
  filter(pa_grade_Good >= -.75, pa_grade_Good <= .65, npa_grade_diff >= -.2, npa_twp_rate_diff <= 0, pressure_rate_difference_Bad <= -.1) %>%
  pull(qbgrp_ssn)
