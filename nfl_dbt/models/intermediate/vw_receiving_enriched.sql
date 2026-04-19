SELECT
  vw_receiving.*
, vw_play_counts_enriched.player
, vw_play_counts_enriched.abbreviation team_abbreviation
, vw_play_counts_enriched.game_id
FROM
  ({{ ref('vw_receiving') }}
LEFT JOIN {{ ref('vw_play_counts_enriched') }} ON ((vw_receiving.player_id = vw_play_counts_enriched.player_id) AND (vw_receiving.week = vw_play_counts_enriched.week) AND (vw_receiving.season = vw_play_counts_enriched.season)))