SELECT
  a.player
, a.player_id
, a.week
, a.season
FROM
  ((
   SELECT
     player
   , player_id
   , week
   , season
   FROM
     {{ ref('vw_play_counts_enriched') }}
   WHERE (snap_counts_pass_route > 0)
   GROUP BY player, player_id, week, season
)  a
LEFT JOIN (
   SELECT
     player_id
   , week
   , season
   FROM
     {{ ref('vw_receiving_enriched') }}
   WHERE (targets > 0)
   GROUP BY player_id, week, season
)  b ON ((a.player_id = b.player_id) AND (a.week = b.week) AND (a.season = b.season)))
WHERE (b.player_id IS NULL)