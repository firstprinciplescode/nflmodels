SELECT
  player
, player_id
, season
FROM
  vw_play_counts_enriched
WHERE (snap_counts_pass_route > 0)
GROUP BY player, player_id, season