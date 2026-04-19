SELECT
  wk_agg.*
, scheme.player
, scheme.position_group
, scheme.pass_plays
, scheme.behind_snaps
, scheme.inline_snaps
, scheme.slot_snaps
, scheme.wide_snaps
FROM
  (vw_receiving_depth_weekly_agg wk_agg
LEFT JOIN vw_receiving_enriched_scheme_agg scheme ON ((wk_agg.player_id = scheme.player_id) AND (wk_agg.team = scheme.team_abbreviation) AND (CAST(wk_agg.season AS BIGINT) = scheme.season)))