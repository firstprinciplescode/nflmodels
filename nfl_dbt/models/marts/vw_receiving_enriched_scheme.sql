SELECT
  A.*
, B.position scheme_position
, B.draft_season
, B.eligible_season
, (CASE WHEN (B.man_routes = 0) THEN GREATEST((A.routes - B.zone_routes), 0) ELSE B.man_routes END) man_routes
, B.man_targets
, B.man_receptions
, B.man_yards
, B.man_touchdowns
, B.man_first_downs
, B.man_drops
, B.man_fumbles
, B.man_interceptions
, B.man_contested_targets
, B.man_contested_receptions
, B.man_yards_after_catch
, B.man_avoided_tackles
, B.man_longest
, (CASE WHEN (B.man_pass_plays = 0) THEN GREATEST((A.pass_plays - B.zone_pass_plays), 0) ELSE B.man_pass_plays END) man_pass_plays
, (CASE WHEN (B.man_pass_blocks = 0) THEN GREATEST((A.pass_blocks - B.zone_pass_blocks), 0) ELSE B.man_pass_blocks END) man_pass_blocks
, B.man_route_rate
, B.man_yprr
, B.man_yards_per_reception
, B.man_yards_after_catch_per_reception
, B.man_avg_depth_of_target
, (CASE WHEN (B.man_targets = 0) THEN null ELSE B.man_targeted_qb_rating END) man_targeted_qb_rating
, B.man_caught_percent
, B.man_drop_rate
, B.man_contested_catch_rate
, B.man_targets_percent
, B.man_pass_block_rate
, B.man_grades_pass_route
, B.man_grades_hands_drop
, (CASE WHEN (B.zone_routes = 0) THEN GREATEST((A.routes - B.man_routes), 0) ELSE B.zone_routes END) zone_routes
, B.zone_targets
, B.zone_receptions
, B.zone_yards
, B.zone_touchdowns
, B.zone_first_downs
, B.zone_drops
, B.zone_fumbles
, B.zone_interceptions
, B.zone_contested_targets
, B.zone_contested_receptions
, B.zone_yards_after_catch
, B.zone_avoided_tackles
, B.zone_longest
, (CASE WHEN (B.zone_pass_plays = 0) THEN GREATEST((A.pass_plays - B.man_pass_plays), 0) ELSE B.zone_pass_plays END) zone_pass_plays
, (CASE WHEN (B.zone_pass_blocks = 0) THEN GREATEST((A.pass_blocks - B.man_pass_blocks), 0) ELSE B.zone_pass_blocks END) zone_pass_blocks
, B.zone_route_rate
, B.zone_yprr
, B.zone_yards_per_reception
, B.zone_yards_after_catch_per_reception
, B.zone_avg_depth_of_target
, (CASE WHEN (B.zone_targets = 0) THEN null ELSE B.zone_targeted_qb_rating END) zone_targeted_qb_rating
, B.zone_caught_percent
, B.zone_drop_rate
, B.zone_contested_catch_rate
, B.zone_targets_percent
, B.zone_pass_block_rate
, B.zone_grades_pass_route
, B.zone_grades_hands_drop
, (CASE WHEN ((A.position IN ('C', 'LT', 'LG', 'None', 'RG', 'RT')) OR (A.position IS NULL)) THEN 'Other' WHEN (A.position IN ('LWR', 'RWR', 'SLWR', 'SRWR')) THEN 'WR' WHEN (A.position IN ('TE-L', 'TE-R')) THEN 'TE' ELSE A.position END) position_group
FROM
  ({{ ref('vw_receiving_enriched') }} A
LEFT JOIN {{ source('pff_raw', 'receiver_scheme') }} B ON ((A.player_id = B.player_id) AND (A.season = B.season) AND (A.week = B.week) AND (A.jersey_number = B.jersey_number) AND (A.player = B.player) AND (A.team_abbreviation = B.team_name)))