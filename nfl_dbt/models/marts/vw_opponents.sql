SELECT
  id,
season,
week,
away_abbreviation AS team,
away_franchise_id AS team_id,
home_abbreviation AS opp,
home_franchise_id AS opp_id
FROM
  {{ ref('stg_pff__games') }}
UNION ALL
SELECT
  id,
season,
week,
home_abbreviation AS team,
home_franchise_id AS team_id,
away_abbreviation AS opp,
away_franchise_id AS opp_id
FROM
  {{ ref('stg_pff__games') }}
