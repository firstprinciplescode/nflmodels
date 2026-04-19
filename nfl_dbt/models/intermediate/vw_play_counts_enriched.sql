SELECT
  play_counts.*
, games.week
, games.season
, team_ids_tbl.abbreviation
FROM
  (({{ source('pff_raw', 'play_counts') }}
LEFT JOIN {{ source('pff_raw', 'games') }} ON (games.id = CAST(play_counts.game_id AS BIGINT)))
LEFT JOIN {{ source('pff_raw', 'team_ids_tbl') }} ON ((team_ids_tbl.season = games.season) AND (play_counts.franchise_id = team_ids_tbl.teamid)))