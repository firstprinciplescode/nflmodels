SELECT
  team_name
, week
, season
, SUM(pa_passing_snaps) pa_snaps
, SUM(npa_passing_snaps) npa_snaps
, (CAST(SUM(pa_passing_snaps) AS DOUBLE) / NULLIF((SUM(pa_passing_snaps) + SUM(npa_passing_snaps)), 0)) pa_rate
, (CAST(SUM(pa_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(pa_passing_snaps), 0)) pa_pressure_rate
, (CAST(SUM(npa_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(npa_passing_snaps), 0)) npa_pressure_rate
, (SUM((pa_passing_snaps * pa_grades_pass)) / NULLIF(SUM(pa_passing_snaps), 0)) pa_grade
, (SUM((npa_passing_snaps * npa_grades_pass)) / NULLIF(SUM(npa_passing_snaps), 0)) npa_grade
, (SUM((pa_passing_snaps * pa_avg_time_to_throw)) / NULLIF(SUM(pa_passing_snaps), 0)) pa_time_to_throw
, (SUM((npa_passing_snaps * npa_avg_time_to_throw)) / NULLIF(SUM(npa_passing_snaps), 0)) npa_time_to_throw
, (SUM((pa_passing_snaps * pa_ypa)) / NULLIF(SUM(pa_passing_snaps), 0)) pa_ypa
, (SUM((npa_passing_snaps * npa_ypa)) / NULLIF(SUM(npa_passing_snaps), 0)) npa_ypa
, (SUM((pa_passing_snaps * pa_accuracy_percent)) / NULLIF(SUM(pa_passing_snaps), 0)) pa_acc_pct
, (SUM((npa_passing_snaps * npa_accuracy_percent)) / NULLIF(SUM(npa_passing_snaps), 0)) npa_acc_pct
, (SUM((pa_passing_snaps * pa_qb_rating)) / NULLIF(SUM(pa_passing_snaps), 0)) pa_qbr
, (SUM((npa_passing_snaps * npa_qb_rating)) / NULLIF(SUM(npa_passing_snaps), 0)) npa_qbr
, (SUM((pa_passing_snaps * pa_btt_rate)) / NULLIF(SUM(pa_passing_snaps), 0)) pa_btt_rate
, (SUM((npa_passing_snaps * npa_btt_rate)) / NULLIF(SUM(npa_passing_snaps), 0)) npa_btt_rate
, (SUM((pa_passing_snaps * pa_twp_rate)) / NULLIF(SUM(pa_passing_snaps), 0)) pa_twp_rate
, (SUM((npa_passing_snaps * npa_twp_rate)) / NULLIF(SUM(npa_passing_snaps), 0)) npa_twp_rate
, (SUM((pa_passing_snaps * pa_avg_depth_of_target)) / NULLIF(SUM(pa_passing_snaps), 0)) pa_adot
, (SUM((npa_passing_snaps * npa_avg_depth_of_target)) / NULLIF(SUM(npa_passing_snaps), 0)) npa_adot
, (CAST(SUM(pa_scrambles) AS DOUBLE) / NULLIF(SUM(pa_passing_snaps), 0)) pa_scr_rate
, (CAST(SUM(npa_scrambles) AS DOUBLE) / NULLIF(SUM(npa_passing_snaps), 0)) npa_scr_rate
, (CAST(SUM(pa_sacks) AS DOUBLE) / NULLIF(SUM(pa_passing_snaps), 0)) pa_sack_pct
, (CAST(SUM(npa_sacks) AS DOUBLE) / NULLIF(SUM(npa_passing_snaps), 0)) npa_sack_pct
, ((SUM((pa_passing_snaps * pa_grades_pass)) / NULLIF(SUM(pa_passing_snaps), 0)) - (SUM((npa_passing_snaps * npa_grades_pass)) / NULLIF(SUM(npa_passing_snaps), 0))) grade_difference
, ((SUM((pa_passing_snaps * pa_avg_time_to_throw)) / NULLIF(SUM(pa_passing_snaps), 0)) - (SUM((npa_passing_snaps * npa_avg_time_to_throw)) / NULLIF(SUM(npa_passing_snaps), 0))) ttt_difference
, ((SUM((pa_passing_snaps * pa_ypa)) / NULLIF(SUM(pa_passing_snaps), 0)) - (SUM((npa_passing_snaps * npa_ypa)) / NULLIF(SUM(npa_passing_snaps), 0))) ypa_difference
, ((SUM((pa_passing_snaps * pa_accuracy_percent)) / NULLIF(SUM(pa_passing_snaps), 0)) - (SUM((npa_passing_snaps * npa_accuracy_percent)) / NULLIF(SUM(npa_passing_snaps), 0))) acc_pct_difference
, ((SUM((pa_passing_snaps * pa_qb_rating)) / NULLIF(SUM(pa_passing_snaps), 0)) - (SUM((npa_passing_snaps * npa_qb_rating)) / NULLIF(SUM(npa_passing_snaps), 0))) qbr_difference
, ((SUM((pa_passing_snaps * pa_btt_rate)) / NULLIF(SUM(pa_passing_snaps), 0)) - (SUM((npa_passing_snaps * npa_btt_rate)) / NULLIF(SUM(npa_passing_snaps), 0))) btt_difference
, ((SUM((pa_passing_snaps * pa_twp_rate)) / NULLIF(SUM(pa_passing_snaps), 0)) - (SUM((npa_passing_snaps * npa_twp_rate)) / NULLIF(SUM(npa_passing_snaps), 0))) twp_difference
, ((SUM((pa_passing_snaps * pa_avg_depth_of_target)) / NULLIF(SUM(pa_passing_snaps), 0)) - (SUM((npa_passing_snaps * npa_avg_depth_of_target)) / NULLIF(SUM(npa_passing_snaps), 0))) adot_difference
, ((CAST(SUM(pa_scrambles) AS DOUBLE) / NULLIF(SUM(pa_passing_snaps), 0)) - (CAST(SUM(npa_scrambles) AS DOUBLE) / NULLIF(SUM(npa_passing_snaps), 0))) scr_rate_difference
, ((CAST(SUM(pa_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(pa_passing_snaps), 0)) - (CAST(SUM(npa_def_gen_pressures) AS DOUBLE) / NULLIF(SUM(npa_passing_snaps), 0))) pressure_rate_difference
, ((CAST(SUM(pa_sacks) AS DOUBLE) / NULLIF(SUM(pa_passing_snaps), 0)) - (CAST(SUM(npa_sacks) AS DOUBLE) / NULLIF(SUM(npa_passing_snaps), 0))) sack_rate_difference
FROM
  {{ source('pff_raw', 'passing_concept') }}
GROUP BY team_name, week, season