comparison_blitz_func("TBMayfield-2025", .89) # 50
comparison_depth_func("TBMayfield-2025", .99) # 14
comparison_less_func("TBMayfield-2025", .86) # 66
comparison_pa_func("TBMayfield-2025", .89) # 52
comparison_pressure_func("TBMayfield-2025", .89) # 47

all_qbs <- rbind(as.data.frame(comparison_blitz_func("TBMayfield-2025", .895)), 
             as.data.frame(comparison_depth_func("TBMayfield-2025", .995)), 
             as.data.frame(comparison_less_func("TBMayfield-2025", .865)), 
             as.data.frame(comparison_pa_func("TBMayfield-2025", .895)), 
             as.data.frame(comparison_pressure_func("TBMayfield-2025", .895)))

sim_qb <- sqldf("SELECT QB, COUNT(*) AS CNT
        FROM  all_qbs 
        GROUP BY  QB
        HAVING  CNT >= 4") %>% select(QB) %>% distinct()

sim_qb

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
