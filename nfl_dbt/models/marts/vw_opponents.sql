SELECT
  id
, season
, week
, away_abbreviation team
, away_franchise_id team_id
, home_abbreviation opp
, home_franchise_id opp_id
FROM
  {{ ref('stg_pff__games') }}
UNION ALL SELECT
  id
, season
, week
, home_abbreviation team
, home_franchise_id team_id
, away_abbreviation opp
, away_franchise_id opp_id
FROM
  {{ ref('stg_pff__games') }}