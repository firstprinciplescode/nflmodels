SELECT
  avg_depth_of_target
, avoided_tackles
, (((pass_plays - inline_snaps) - wide_snaps) - slot_snaps) behind_snaps
, caught_percent
, contested_catch_rate
, contested_receptions
, contested_targets
, declined_penalties
, drop_rate
, drops
, first_downs
, franchise_id
, fumbles
, grades_hands_drop
, grades_hands_fumble
, grades_offense
, grades_pass_block
, grades_pass_route
, inline_rate
, inline_snaps
, interceptions
, jersey_number
, longest
, pass_block_rate
, pass_blocks
, pass_plays
, penalties
, player_id
, position
, receptions
, route_rate
, routes
, scraped_at
, season
, slot_rate
, slot_snaps
, targeted_qb_rating
, targets
, touchdowns
, week
, wide_rate
, wide_snaps
, yards
, yards_after_catch
, yards_after_catch_per_reception
, yards_per_reception
, yprr
FROM
  {{ source('pff_raw', 'receiving_with_targets') }}
UNION ALL SELECT
  avg_depth_of_target
, avoided_tackles
, (((pass_plays - inline_snaps) - wide_snaps) - slot_snaps) behind_snaps
, caught_percent
, contested_catch_rate
, contested_receptions
, contested_targets
, declined_penalties
, drop_rate
, drops
, first_downs
, player_franchise_id franchise_id
, fumbles
, grades_hands_drop
, grades_hands_fumble
, grades_offense
, grades_pass_block
, grades_pass_route
, inline_rate
, inline_snaps
, interceptions
, jersey_number
, longest
, pass_block_rate
, pass_blocks
, pass_plays
, penalties
, player_id
, position
, receptions
, route_rate
, routes
, scraped_at
, season
, slot_rate
, slot_snaps
, targeted_qb_rating
, targets
, touchdowns
, week
, wide_rate
, wide_snaps
, yards
, yards_after_catch
, yards_after_catch_per_reception
, yards_per_reception
, yprr
FROM
  {{ source('pff_raw', 'receiving_no_targets') }}