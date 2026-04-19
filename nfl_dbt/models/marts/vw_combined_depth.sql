SELECT
  team_name
, week
, season
, SUM(behind_los_passing_snaps) behind_los_snaps
, SUM(short_passing_snaps) short_snaps
, SUM(medium_passing_snaps) medium_snaps
, SUM(deep_passing_snaps) deep_snaps
, (CAST(SUM(behind_los_passing_snaps) AS DOUBLE) / NULLIF((((SUM(behind_los_passing_snaps) + SUM(short_passing_snaps)) + SUM(medium_passing_snaps)) + SUM(deep_passing_snaps)), 0)) behind_los_rate
, (CAST(SUM(short_passing_snaps) AS DOUBLE) / NULLIF((((SUM(behind_los_passing_snaps) + SUM(short_passing_snaps)) + SUM(medium_passing_snaps)) + SUM(deep_passing_snaps)), 0)) short_rate
, (CAST(SUM(medium_passing_snaps) AS DOUBLE) / NULLIF((((SUM(behind_los_passing_snaps) + SUM(short_passing_snaps)) + SUM(medium_passing_snaps)) + SUM(deep_passing_snaps)), 0)) medium_rate
, (CAST(SUM(deep_passing_snaps) AS DOUBLE) / NULLIF((((SUM(behind_los_passing_snaps) + SUM(short_passing_snaps)) + SUM(medium_passing_snaps)) + SUM(deep_passing_snaps)), 0)) deep_rate
, (CAST(SUM(behind_los_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(behind_los_passing_snaps), 0)) behind_los_pressure_rate
, (CAST(SUM(short_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(short_passing_snaps), 0)) short_pressure_rate
, (CAST(SUM(medium_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(medium_passing_snaps), 0)) medium_pressure_rate
, (CAST(SUM(deep_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(deep_passing_snaps), 0)) deep_pressure_rate
, (SUM((behind_los_passing_snaps * behind_los_grades_pass)) / NULLIF(SUM(behind_los_passing_snaps), 0)) behind_los_grade
, (SUM((short_passing_snaps * short_grades_pass)) / NULLIF(SUM(short_passing_snaps), 0)) short_grade
, (SUM((medium_passing_snaps * medium_grades_pass)) / NULLIF(SUM(medium_passing_snaps), 0)) medium_grade
, (SUM((deep_passing_snaps * deep_grades_pass)) / NULLIF(SUM(deep_passing_snaps), 0)) deep_grade
, (SUM((behind_los_passing_snaps * behind_los_avg_time_to_throw)) / NULLIF(SUM(behind_los_passing_snaps), 0)) behind_los_time_to_throw
, (SUM((short_passing_snaps * short_avg_time_to_throw)) / NULLIF(SUM(short_passing_snaps), 0)) short_time_to_throw
, (SUM((medium_passing_snaps * medium_avg_time_to_throw)) / NULLIF(SUM(medium_passing_snaps), 0)) medium_time_to_throw
, (SUM((deep_passing_snaps * deep_avg_time_to_throw)) / NULLIF(SUM(deep_passing_snaps), 0)) deep_time_to_throw
, (SUM((behind_los_passing_snaps * behind_los_ypa)) / NULLIF(SUM(behind_los_passing_snaps), 0)) behind_los_ypa
, (SUM((short_passing_snaps * short_ypa)) / NULLIF(SUM(short_passing_snaps), 0)) short_ypa
, (SUM((medium_passing_snaps * medium_ypa)) / NULLIF(SUM(medium_passing_snaps), 0)) medium_ypa
, (SUM((deep_passing_snaps * deep_ypa)) / NULLIF(SUM(deep_passing_snaps), 0)) deep_ypa
, (SUM((behind_los_passing_snaps * behind_los_accuracy_percent)) / NULLIF(SUM(behind_los_passing_snaps), 0)) behind_los_acc_pct
, (SUM((short_passing_snaps * short_accuracy_percent)) / NULLIF(SUM(short_passing_snaps), 0)) short_acc_pct
, (SUM((medium_passing_snaps * medium_accuracy_percent)) / NULLIF(SUM(medium_passing_snaps), 0)) medium_acc_pct
, (SUM((deep_passing_snaps * deep_accuracy_percent)) / NULLIF(SUM(deep_passing_snaps), 0)) deep_acc_pct
, (SUM((behind_los_passing_snaps * behind_los_qb_rating)) / NULLIF(SUM(behind_los_passing_snaps), 0)) behind_los_qbr
, (SUM((short_passing_snaps * short_qb_rating)) / NULLIF(SUM(short_passing_snaps), 0)) short_qbr
, (SUM((medium_passing_snaps * medium_qb_rating)) / NULLIF(SUM(medium_passing_snaps), 0)) medium_qbr
, (SUM((deep_passing_snaps * deep_qb_rating)) / NULLIF(SUM(deep_passing_snaps), 0)) deep_qbr
, (SUM((behind_los_passing_snaps * behind_los_btt_rate)) / NULLIF(SUM(behind_los_passing_snaps), 0)) behind_los_btt_rate
, (SUM((short_passing_snaps * short_btt_rate)) / NULLIF(SUM(short_passing_snaps), 0)) short_btt_rate
, (SUM((medium_passing_snaps * medium_btt_rate)) / NULLIF(SUM(medium_passing_snaps), 0)) medium_btt_rate
, (SUM((deep_passing_snaps * deep_btt_rate)) / NULLIF(SUM(deep_passing_snaps), 0)) deep_btt_rate
, (SUM((behind_los_passing_snaps * behind_los_twp_rate)) / NULLIF(SUM(behind_los_passing_snaps), 0)) behind_los_twp_rate
, (SUM((short_passing_snaps * short_twp_rate)) / NULLIF(SUM(short_passing_snaps), 0)) short_twp_rate
, (SUM((medium_passing_snaps * medium_twp_rate)) / NULLIF(SUM(medium_passing_snaps), 0)) medium_twp_rate
, (SUM((deep_passing_snaps * deep_twp_rate)) / NULLIF(SUM(deep_passing_snaps), 0)) deep_twp_rate
, (SUM(behind_los_interceptions) / NULLIF(SUM(behind_los_passing_snaps), 0)) behind_los_int_rate
, (SUM(short_interceptions) / NULLIF(SUM(short_passing_snaps), 0)) short_int_rate
, (SUM(medium_interceptions) / NULLIF(SUM(medium_passing_snaps), 0)) medium_int_rate
, (SUM(deep_interceptions) / NULLIF(SUM(deep_passing_snaps), 0)) deep_int_rate
, (SUM((behind_los_passing_snaps * behind_los_avg_depth_of_target)) / NULLIF(SUM(behind_los_passing_snaps), 0)) behind_los_adot
, (SUM((short_passing_snaps * short_avg_depth_of_target)) / NULLIF(SUM(short_passing_snaps), 0)) short_adot
, (SUM((medium_passing_snaps * medium_avg_depth_of_target)) / NULLIF(SUM(medium_passing_snaps), 0)) medium_adot
, (SUM((deep_passing_snaps * deep_avg_depth_of_target)) / NULLIF(SUM(deep_passing_snaps), 0)) deep_adot
, ((SUM((deep_passing_snaps * deep_grades_pass)) / NULLIF(SUM(deep_passing_snaps), 0)) - (SUM((short_passing_snaps * short_grades_pass)) / NULLIF(SUM(short_passing_snaps), 0))) ds_grade_difference
, ((SUM((medium_passing_snaps * medium_grades_pass)) / NULLIF(SUM(medium_passing_snaps), 0)) - (SUM((short_passing_snaps * short_grades_pass)) / NULLIF(SUM(short_passing_snaps), 0))) ms_grade_difference
, ((SUM((medium_passing_snaps * medium_avg_time_to_throw)) / NULLIF(SUM(medium_passing_snaps), 0)) - (SUM((short_passing_snaps * short_avg_time_to_throw)) / NULLIF(SUM(short_passing_snaps), 0))) ttt_difference
, ((SUM((medium_passing_snaps * medium_ypa)) / NULLIF(SUM(medium_passing_snaps), 0)) - (SUM((short_passing_snaps * short_ypa)) / NULLIF(SUM(short_passing_snaps), 0))) ypa_difference
, ((SUM((deep_passing_snaps * deep_accuracy_percent)) / NULLIF(SUM(deep_passing_snaps), 0)) - (SUM((short_passing_snaps * short_accuracy_percent)) / NULLIF(SUM(short_passing_snaps), 0))) ds_acc_pct_difference
, ((SUM((medium_passing_snaps * medium_accuracy_percent)) / NULLIF(SUM(medium_passing_snaps), 0)) - (SUM((short_passing_snaps * short_accuracy_percent)) / NULLIF(SUM(short_passing_snaps), 0))) ms_acc_pct_difference
, ((SUM((deep_passing_snaps * deep_qb_rating)) / NULLIF(SUM(deep_passing_snaps), 0)) - (SUM((short_passing_snaps * short_qb_rating)) / NULLIF(SUM(short_passing_snaps), 0))) ds_qbr_difference
, ((SUM((medium_passing_snaps * medium_qb_rating)) / NULLIF(SUM(medium_passing_snaps), 0)) - (SUM((short_passing_snaps * short_qb_rating)) / NULLIF(SUM(short_passing_snaps), 0))) ms_qbr_difference
, ((SUM((deep_passing_snaps * deep_twp_rate)) / NULLIF(SUM(deep_passing_snaps), 0)) - (SUM((short_passing_snaps * short_twp_rate)) / NULLIF(SUM(short_passing_snaps), 0))) ds_twp_difference
, ((SUM((medium_passing_snaps * medium_twp_rate)) / NULLIF(SUM(medium_passing_snaps), 0)) - (SUM((short_passing_snaps * short_twp_rate)) / NULLIF(SUM(short_passing_snaps), 0))) ms_twp_difference
, ((CAST(SUM(deep_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(deep_passing_snaps), 0)) - (CAST(SUM(medium_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(medium_passing_snaps), 0))) pressure_rate_difference
FROM
  {{ source('pff_raw', 'passing_depth') }}
GROUP BY team_name, week, season