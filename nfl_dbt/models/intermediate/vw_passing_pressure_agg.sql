SELECT
  team_name,
week,
season,
SUM(pressure_passing_snaps) AS pressure_snaps,
SUM(no_pressure_passing_snaps) AS no_pressure_snaps,
(CAST(SUM(pressure_passing_snaps) AS DOUBLE) / NULLIF((SUM(pressure_passing_snaps) + SUM(no_pressure_passing_snaps)), 0)) AS pressure_rate,
(SUM((pressure_passing_snaps * pressure_grades_pass)) / NULLIF(SUM(pressure_passing_snaps), 0)) AS pressure_grade,
(SUM((no_pressure_passing_snaps * no_pressure_grades_pass)) / NULLIF(SUM(no_pressure_passing_snaps), 0)) AS no_pressure_grade,
(SUM((pressure_passing_snaps * pressure_avg_time_to_throw)) / NULLIF(SUM(pressure_passing_snaps), 0)) AS pressure_time_to_throw,
(SUM((no_pressure_passing_snaps * no_pressure_avg_time_to_throw)) / NULLIF(SUM(no_pressure_passing_snaps), 0)) AS no_pressure_time_to_throw,
(SUM((pressure_passing_snaps * pressure_ypa)) / NULLIF(SUM(pressure_passing_snaps), 0)) AS pressure_ypa,
(SUM((no_pressure_passing_snaps * no_pressure_ypa)) / NULLIF(SUM(no_pressure_passing_snaps), 0)) AS no_pressure_ypa,
(SUM((pressure_passing_snaps * pressure_accuracy_percent)) / NULLIF(SUM(pressure_passing_snaps), 0)) AS pressure_acc_pct,
(SUM((no_pressure_passing_snaps * no_pressure_accuracy_percent)) / NULLIF(SUM(no_pressure_passing_snaps), 0)) AS no_pressure_acc_pct,
(SUM((pressure_passing_snaps * pressure_qb_rating)) / NULLIF(SUM(pressure_passing_snaps), 0)) AS pressure_qbr,
(SUM((no_pressure_passing_snaps * no_pressure_qb_rating)) / NULLIF(SUM(no_pressure_passing_snaps), 0)) AS no_pressure_qbr,
(SUM((pressure_passing_snaps * pressure_btt_rate)) / NULLIF(SUM(pressure_passing_snaps), 0)) AS pressure_btt_rate,
(SUM((no_pressure_passing_snaps * no_pressure_btt_rate)) / NULLIF(SUM(no_pressure_passing_snaps), 0)) AS no_pressure_btt_rate,
(SUM((pressure_passing_snaps * pressure_twp_rate)) / NULLIF(SUM(pressure_passing_snaps), 0)) AS pressure_twp_rate,
(SUM((no_pressure_passing_snaps * no_pressure_twp_rate)) / NULLIF(SUM(no_pressure_passing_snaps), 0)) AS no_pressure_twp_rate,
(SUM((pressure_passing_snaps * pressure_avg_depth_of_target)) / NULLIF(SUM(pressure_passing_snaps), 0)) AS pressure_adot,
(SUM((no_pressure_passing_snaps * no_pressure_avg_depth_of_target)) / NULLIF(SUM(no_pressure_passing_snaps), 0)) AS no_pressure_adot,
(CAST(SUM(pressure_scrambles) AS DOUBLE) / NULLIF(SUM(pressure_passing_snaps), 0)) AS pressure_scr_rate,
(CAST(SUM(no_pressure_scrambles) AS DOUBLE) / NULLIF(SUM(no_pressure_passing_snaps), 0)) AS no_pressure_scr_rate,
(CAST(SUM(pressure_sacks) AS DOUBLE) / NULLIF((SUM(pressure_passing_snaps) + SUM(no_pressure_passing_snaps)), 0)) AS sack_pct
FROM
  {{ ref('stg_pff__passing_pressure') }}
GROUP BY team_name, week, season
