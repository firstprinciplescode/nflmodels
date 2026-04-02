comparison_blitz_def_func("TB2025", .99) # 73
comparison_depth_def_func("TB2025", 1.02) # 47
comparison_less_def_func("TB2025", 1) # 68
comparison_pa_def_func("TB2025", 1.02) # 41
comparison_pressure_def_func("TB2025", .96) # 82

all_def <- rbind(as.data.frame(comparison_blitz_def_func("TB2025", 1.002)), 
                 as.data.frame(comparison_depth_def_func("TB2025", 1.032)), 
                 as.data.frame(comparison_less_def_func("TB2025", 1.012)), 
                 as.data.frame(comparison_pa_def_func("TB2025", 1.032)), 
                 as.data.frame(comparison_pressure_def_func("TB2025", .972)))

sim_def <- sqldf("SELECT QB, COUNT(*) AS CNT
        FROM  all_def 
        GROUP BY  QB
        HAVING  CNT >= 4") %>% select(QB) %>% distinct()

sim_def

df_blitz_def_scaled_z %>%
  filter(def_ssn %in% c(sim_def$QB, "TB2025")) %>%
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
