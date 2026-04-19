SELECT
  team_name
, week
, season
, SUM(blitz_passing_snaps) blitz_snaps
, SUM(no_blitz_passing_snaps) no_blitz_snaps
, (CAST(SUM(blitz_passing_snaps) AS DOUBLE) / NULLIF((SUM(blitz_passing_snaps) + SUM(no_blitz_passing_snaps)), 0)) blitz_rate
, (CAST(SUM(blitz_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(blitz_passing_snaps), 0)) blitz_pressure_rate
, (CAST(SUM(no_blitz_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(no_blitz_passing_snaps), 0)) no_blitz_pressure_rate
, (SUM((blitz_passing_snaps * blitz_grades_pass)) / NULLIF(SUM(blitz_passing_snaps), 0)) blitz_grade
, (SUM((no_blitz_passing_snaps * no_blitz_grades_pass)) / NULLIF(SUM(no_blitz_passing_snaps), 0)) no_blitz_grade
, (SUM((blitz_passing_snaps * blitz_avg_time_to_throw)) / NULLIF(SUM(blitz_passing_snaps), 0)) blitz_time_to_throw
, (SUM((no_blitz_passing_snaps * no_blitz_avg_time_to_throw)) / NULLIF(SUM(no_blitz_passing_snaps), 0)) no_blitz_time_to_throw
, (SUM((blitz_passing_snaps * blitz_ypa)) / NULLIF(SUM(blitz_passing_snaps), 0)) blitz_ypa
, (SUM((no_blitz_passing_snaps * no_blitz_ypa)) / NULLIF(SUM(no_blitz_passing_snaps), 0)) no_blitz_ypa
, (SUM((blitz_passing_snaps * blitz_accuracy_percent)) / NULLIF(SUM(blitz_passing_snaps), 0)) blitz_acc_pct
, (SUM((no_blitz_passing_snaps * no_blitz_accuracy_percent)) / NULLIF(SUM(no_blitz_passing_snaps), 0)) no_blitz_acc_pct
, (SUM((blitz_passing_snaps * blitz_qb_rating)) / NULLIF(SUM(blitz_passing_snaps), 0)) blitz_qbr
, (SUM((no_blitz_passing_snaps * no_blitz_qb_rating)) / NULLIF(SUM(no_blitz_passing_snaps), 0)) no_blitz_qbr
, (SUM((blitz_passing_snaps * blitz_btt_rate)) / NULLIF(SUM(blitz_passing_snaps), 0)) blitz_btt_rate
, (SUM((no_blitz_passing_snaps * no_blitz_btt_rate)) / NULLIF(SUM(no_blitz_passing_snaps), 0)) no_blitz_btt_rate
, (SUM((blitz_passing_snaps * blitz_twp_rate)) / NULLIF(SUM(blitz_passing_snaps), 0)) blitz_twp_rate
, (SUM((no_blitz_passing_snaps * no_blitz_twp_rate)) / NULLIF(SUM(no_blitz_passing_snaps), 0)) no_blitz_twp_rate
, (SUM(blitz_interceptions) / NULLIF(SUM(blitz_passing_snaps), 0)) blitz_int_rate
, (SUM(no_blitz_interceptions) / NULLIF(SUM(no_blitz_passing_snaps), 0)) no_blitz_int_rate
, (SUM((blitz_passing_snaps * blitz_avg_depth_of_target)) / NULLIF(SUM(blitz_passing_snaps), 0)) blitz_adot
, (SUM((no_blitz_passing_snaps * no_blitz_avg_depth_of_target)) / NULLIF(SUM(no_blitz_passing_snaps), 0)) no_blitz_adot
, (CAST(SUM(blitz_scrambles) AS DOUBLE) / NULLIF(SUM(blitz_passing_snaps), 0)) blitz_scr_rate
, (CAST(SUM(no_blitz_scrambles) AS DOUBLE) / NULLIF(SUM(no_blitz_passing_snaps), 0)) no_blitz_scr_rate
, (CAST(SUM(blitz_sacks) AS DOUBLE) / NULLIF(SUM(blitz_passing_snaps), 0)) blitz_sack_pct
, (CAST(SUM(no_blitz_sacks) AS DOUBLE) / NULLIF(SUM(no_blitz_passing_snaps), 0)) no_blitz_sack_pct
, ((SUM((blitz_passing_snaps * blitz_grades_pass)) / NULLIF(SUM(blitz_passing_snaps), 0)) - (SUM((no_blitz_passing_snaps * no_blitz_grades_pass)) / NULLIF(SUM(no_blitz_passing_snaps), 0))) grade_difference
, ((SUM((blitz_passing_snaps * blitz_avg_time_to_throw)) / NULLIF(SUM(blitz_passing_snaps), 0)) - (SUM((no_blitz_passing_snaps * no_blitz_avg_time_to_throw)) / NULLIF(SUM(no_blitz_passing_snaps), 0))) ttt_difference
, ((SUM((blitz_passing_snaps * blitz_ypa)) / NULLIF(SUM(blitz_passing_snaps), 0)) - (SUM((no_blitz_passing_snaps * no_blitz_ypa)) / NULLIF(SUM(no_blitz_passing_snaps), 0))) ypa_difference
, ((SUM((blitz_passing_snaps * blitz_accuracy_percent)) / NULLIF(SUM(blitz_passing_snaps), 0)) - (SUM((no_blitz_passing_snaps * no_blitz_accuracy_percent)) / NULLIF(SUM(no_blitz_passing_snaps), 0))) acc_pct_difference
, ((SUM((blitz_passing_snaps * blitz_qb_rating)) / NULLIF(SUM(blitz_passing_snaps), 0)) - (SUM((no_blitz_passing_snaps * no_blitz_qb_rating)) / NULLIF(SUM(no_blitz_passing_snaps), 0))) qbr_difference
, ((SUM((blitz_passing_snaps * blitz_btt_rate)) / NULLIF(SUM(blitz_passing_snaps), 0)) - (SUM((no_blitz_passing_snaps * no_blitz_btt_rate)) / NULLIF(SUM(no_blitz_passing_snaps), 0))) btt_difference
, ((SUM((blitz_passing_snaps * blitz_twp_rate)) / NULLIF(SUM(blitz_passing_snaps), 0)) - (SUM((no_blitz_passing_snaps * no_blitz_twp_rate)) / NULLIF(SUM(no_blitz_passing_snaps), 0))) twp_difference
, ((SUM((blitz_passing_snaps * blitz_avg_depth_of_target)) / NULLIF(SUM(blitz_passing_snaps), 0)) - (SUM((no_blitz_passing_snaps * no_blitz_avg_depth_of_target)) / NULLIF(SUM(no_blitz_passing_snaps), 0))) adot_difference
, ((CAST(SUM(blitz_scrambles) AS DOUBLE) / NULLIF(SUM(blitz_passing_snaps), 0)) - (CAST(SUM(no_blitz_scrambles) AS DOUBLE) / NULLIF(SUM(no_blitz_passing_snaps), 0))) scr_rate_difference
, ((CAST(SUM(blitz_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(blitz_passing_snaps), 0)) - (CAST(SUM(no_blitz_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(no_blitz_passing_snaps), 0))) pressure_rate_difference
, ((CAST(SUM(blitz_sacks) AS DOUBLE) / NULLIF(SUM(blitz_passing_snaps), 0)) - (CAST(SUM(no_blitz_sacks) AS DOUBLE) / NULLIF(SUM(no_blitz_passing_snaps), 0))) sack_rate_difference
FROM
  {{ source('pff_raw', 'passing_pressure') }}
GROUP BY team_name, week, season