SELECT
  play_counts.*
, games.week
, games.season
, team_ids_tbl.abbreviation
FROM
  (({{ ref('stg_pff__play_counts') }} play_counts
LEFT JOIN {{ ref('stg_pff__games') }} games ON (games.id = CAST(play_counts.game_id AS BIGINT)))
LEFT JOIN {{ ref('stg_pff__team_ids_tbl') }} team_ids_tbl ON ((team_ids_tbl.season = games.season) AND (play_counts.franchise_id = team_ids_tbl.teamid)))