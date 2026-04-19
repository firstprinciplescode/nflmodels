SELECT
  team_name
, week
, season
, SUM(less_passing_snaps) less_snaps
, SUM(more_passing_snaps) more_snaps
, (CAST(SUM(less_passing_snaps) AS DOUBLE) / (SUM(less_passing_snaps) + SUM(more_passing_snaps))) less_rate
, (CAST(SUM(less_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(less_passing_snaps), 0)) less_pressure_rate
, (CAST(SUM(more_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(more_passing_snaps), 0)) more_pressure_rate
, (SUM((less_passing_snaps * less_grades_pass)) / NULLIF(SUM(less_passing_snaps), 0)) less_grade
, (SUM((more_passing_snaps * more_grades_pass)) / NULLIF(SUM(more_passing_snaps), 0)) more_grade
, (SUM((less_passing_snaps * less_avg_time_to_throw)) / NULLIF(SUM(less_passing_snaps), 0)) less_time_to_throw
, (SUM((more_passing_snaps * more_avg_time_to_throw)) / NULLIF(SUM(more_passing_snaps), 0)) more_time_to_throw
, (SUM((less_passing_snaps * less_ypa)) / NULLIF(SUM(less_passing_snaps), 0)) less_ypa
, (SUM((more_passing_snaps * more_ypa)) / NULLIF(SUM(more_passing_snaps), 0)) more_ypa
, (SUM((less_passing_snaps * less_accuracy_percent)) / NULLIF(SUM(less_passing_snaps), 0)) less_acc_pct
, (SUM((more_passing_snaps * more_accuracy_percent)) / NULLIF(SUM(more_passing_snaps), 0)) more_acc_pct
, (SUM((less_passing_snaps * less_qb_rating)) / NULLIF(SUM(less_passing_snaps), 0)) less_qbr
, (SUM((more_passing_snaps * more_qb_rating)) / NULLIF(SUM(more_passing_snaps), 0)) more_qbr
, (SUM((less_passing_snaps * less_btt_rate)) / NULLIF(SUM(less_passing_snaps), 0)) less_btt_rate
, (SUM((more_passing_snaps * more_btt_rate)) / NULLIF(SUM(more_passing_snaps), 0)) more_btt_rate
, (SUM((less_passing_snaps * less_twp_rate)) / NULLIF(SUM(less_passing_snaps), 0)) less_twp_rate
, (SUM((more_passing_snaps * more_twp_rate)) / NULLIF(SUM(more_passing_snaps), 0)) more_twp_rate
, ROUND((CAST(SUM(less_interceptions) AS DOUBLE) / NULLIF(SUM(less_passing_snaps), 0)), 10) less_int_rate
, ROUND((CAST(SUM(more_interceptions) AS DOUBLE) / NULLIF(SUM(more_passing_snaps), 0)), 10) more_int_rate
, (SUM((less_passing_snaps * less_avg_depth_of_target)) / NULLIF(SUM(less_passing_snaps), 0)) less_adot
, (SUM((more_passing_snaps * more_avg_depth_of_target)) / NULLIF(SUM(more_passing_snaps), 0)) more_adot
, (CAST(SUM(less_scrambles) AS DOUBLE) / NULLIF(SUM(less_passing_snaps), 0)) less_scr_rate
, (CAST(SUM(more_scrambles) AS DOUBLE) / NULLIF(SUM(more_passing_snaps), 0)) more_scr_rate
, (CAST(SUM(less_sacks) AS DOUBLE) / NULLIF(SUM(less_passing_snaps), 0)) less_sack_pct
, (CAST(SUM(more_sacks) AS DOUBLE) / NULLIF(SUM(more_passing_snaps), 0)) more_sack_pct
, ((SUM((less_passing_snaps * less_grades_pass)) / NULLIF(SUM(less_passing_snaps), 0)) - (SUM((more_passing_snaps * more_grades_pass)) / NULLIF(SUM(more_passing_snaps), 0))) grade_difference
, ((SUM((less_passing_snaps * less_avg_time_to_throw)) / NULLIF(SUM(less_passing_snaps), 0)) - (SUM((more_passing_snaps * more_avg_time_to_throw)) / NULLIF(SUM(more_passing_snaps), 0))) ttt_difference
, ((SUM((less_passing_snaps * less_ypa)) / NULLIF(SUM(less_passing_snaps), 0)) - (SUM((more_passing_snaps * more_ypa)) / NULLIF(SUM(more_passing_snaps), 0))) ypa_difference
, ((SUM((less_passing_snaps * less_accuracy_percent)) / NULLIF(SUM(less_passing_snaps), 0)) - (SUM((more_passing_snaps * more_accuracy_percent)) / NULLIF(SUM(more_passing_snaps), 0))) acc_pct_difference
, ((SUM((less_passing_snaps * less_qb_rating)) / NULLIF(SUM(less_passing_snaps), 0)) - (SUM((more_passing_snaps * more_qb_rating)) / NULLIF(SUM(more_passing_snaps), 0))) qbr_difference
, ((SUM((less_passing_snaps * less_btt_rate)) / NULLIF(SUM(less_passing_snaps), 0)) - (SUM((more_passing_snaps * more_btt_rate)) / NULLIF(SUM(more_passing_snaps), 0))) btt_difference
, ((SUM((less_passing_snaps * less_twp_rate)) / NULLIF(SUM(less_passing_snaps), 0)) - (SUM((more_passing_snaps * more_twp_rate)) / NULLIF(SUM(more_passing_snaps), 0))) twp_difference
, ((SUM((less_passing_snaps * less_avg_depth_of_target)) / NULLIF(SUM(less_passing_snaps), 0)) - (SUM((more_passing_snaps * more_avg_depth_of_target)) / NULLIF(SUM(more_passing_snaps), 0))) adot_difference
, ((CAST(SUM(less_scrambles) AS DOUBLE) / NULLIF(SUM(less_passing_snaps), 0)) - (CAST(SUM(more_scrambles) AS DOUBLE) / NULLIF(SUM(more_passing_snaps), 0))) scr_rate_difference
, ((CAST(SUM(less_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(less_passing_snaps), 0)) - (CAST(SUM(more_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(more_passing_snaps), 0))) pressure_rate_difference
, ((CAST(SUM(less_sacks) AS DOUBLE) / NULLIF(SUM(less_passing_snaps), 0)) - (CAST(SUM(more_sacks) AS DOUBLE) / NULLIF(SUM(more_passing_snaps), 0))) sack_rate_difference
FROM
  passing_tip
GROUP BY team_name, week, season