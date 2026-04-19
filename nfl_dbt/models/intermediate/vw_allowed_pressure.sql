SELECT
  team_name
, week
, season
, SUM(pressures_off) pressures_total
, SUM(pressures_self) pressures_qb
, (SUM(pressures_off) - SUM(pressures_self)) pressures_oth
FROM
  {{ source('pff_raw', 'allowed_pressure') }}
GROUP BY team_name, week, season