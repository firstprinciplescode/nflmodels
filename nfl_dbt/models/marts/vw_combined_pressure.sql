SELECT
  p.*
, a.pressures_qb
, a.pressures_oth
, (p.pressure_grade - p.no_pressure_grade) grade_difference
, (p.pressure_time_to_throw - p.no_pressure_time_to_throw) ttt_difference
, (p.pressure_ypa - p.no_pressure_ypa) ypa_difference
, (p.pressure_acc_pct - p.no_pressure_acc_pct) acc_pct_difference
, (p.pressure_qbr - p.no_pressure_qbr) qbr_difference
, (p.pressure_btt_rate - p.no_pressure_btt_rate) btt_difference
, (p.pressure_twp_rate - p.no_pressure_twp_rate) twp_difference
, (p.pressure_adot - p.no_pressure_adot) adot_difference
, (p.pressure_scr_rate - p.no_pressure_scr_rate) scr_rate_difference
FROM
  ({{ ref('vw_passing_pressure_agg') }} p
LEFT JOIN {{ ref('vw_allowed_pressure') }} a ON ((p.team_name = a.team_name) AND (p.week = a.week) AND (p.season = a.season)))