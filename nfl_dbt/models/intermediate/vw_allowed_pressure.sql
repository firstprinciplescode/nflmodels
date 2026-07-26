SELECT
  team_name,
week,
season,
SUM(pressures_off) AS pressures_total,
SUM(pressures_self) AS pressures_qb,
(SUM(pressures_off) - SUM(pressures_self)) AS pressures_oth
FROM
  {{ ref('stg_pff__allowed_pressure') }}
GROUP BY team_name, week, season
