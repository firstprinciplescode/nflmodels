WITH
  no_targets AS (
   SELECT
     player_id
   , team_name
   , season
   , (((((SUM(center_behind_los_routes) + SUM(left_behind_los_routes)) + SUM(right_behind_los_routes)) + ((SUM(center_short_routes) + SUM(left_short_routes)) + SUM(right_short_routes))) + ((SUM(center_medium_routes) + SUM(left_medium_routes)) + SUM(right_medium_routes))) + ((SUM(center_deep_routes) + SUM(left_deep_routes)) + SUM(right_deep_routes))) routes
   , ((SUM(center_behind_los_routes) + SUM(left_behind_los_routes)) + SUM(right_behind_los_routes)) behind_los_routes
   , ((SUM(center_short_routes) + SUM(left_short_routes)) + SUM(right_short_routes)) short_routes
   , ((SUM(center_medium_routes) + SUM(left_medium_routes)) + SUM(right_medium_routes)) medium_routes
   , ((SUM(center_deep_routes) + SUM(left_deep_routes)) + SUM(right_deep_routes)) deep_routes
   , 0 targets
   , 0 behind_los_targets
   , 0 short_targets
   , 0 medium_targets
   , 0 deep_targets
   FROM
    {{ ref('stg_pff__receiving_depth_weekly_no_targets') }}
   GROUP BY player_id, team_name, season
) 
, targets AS (
   SELECT
     player_id
   , team_name
   , season
   , (((((((((((SUM(center_behind_los_routes) + SUM(left_behind_los_routes)) + SUM(right_behind_los_routes)) + SUM(center_short_routes)) + SUM(left_short_routes)) + SUM(right_short_routes)) + SUM(center_medium_routes)) + SUM(left_medium_routes)) + SUM(right_medium_routes)) + SUM(center_deep_routes)) + SUM(left_deep_routes)) + SUM(right_deep_routes)) routes
   , ((SUM(center_behind_los_routes) + SUM(left_behind_los_routes)) + SUM(right_behind_los_routes)) behind_los_routes
   , ((SUM(center_short_routes) + SUM(left_short_routes)) + SUM(right_short_routes)) short_routes
   , ((SUM(center_medium_routes) + SUM(left_medium_routes)) + SUM(right_medium_routes)) medium_routes
   , ((SUM(center_deep_routes) + SUM(left_deep_routes)) + SUM(right_deep_routes)) deep_routes
   , (((((((((((SUM(center_behind_los_targets) + SUM(left_behind_los_targets)) + SUM(right_behind_los_targets)) + SUM(center_short_targets)) + SUM(left_short_targets)) + SUM(right_short_targets)) + SUM(center_medium_targets)) + SUM(left_medium_targets)) + SUM(right_medium_targets)) + SUM(center_deep_targets)) + SUM(left_deep_targets)) + SUM(right_deep_targets)) targets
   , ((SUM(center_behind_los_targets) + SUM(left_behind_los_targets)) + SUM(right_behind_los_targets)) behind_los_targets
   , ((SUM(center_short_targets) + SUM(left_short_targets)) + SUM(right_short_targets)) short_targets
   , ((SUM(center_medium_targets) + SUM(left_medium_targets)) + SUM(right_medium_targets)) medium_targets
   , ((SUM(center_deep_targets) + SUM(left_deep_targets)) + SUM(right_deep_targets)) deep_targets
   FROM
     receiving_depth_weekly_with_targets
   GROUP BY player_id, team_name, season
) 
SELECT
  COALESCE(no_targets.player_id, targets.player_id) player_id
, COALESCE(no_targets.team_name, targets.team_name) team
, COALESCE(no_targets.season, targets.season) season
, (COALESCE(no_targets.routes, 0) + COALESCE(targets.routes, 0)) routes
, (COALESCE(no_targets.behind_los_routes, 0) + COALESCE(targets.behind_los_routes, 0)) behind_los_routes
, (COALESCE(no_targets.short_routes, 0) + COALESCE(targets.short_routes, 0)) short_routes
, (COALESCE(no_targets.medium_routes, 0) + COALESCE(targets.medium_routes, 0)) medium_routes
, (COALESCE(no_targets.deep_routes, 0) + COALESCE(targets.deep_routes, 0)) deep_routes
, (COALESCE(no_targets.targets, 0) + COALESCE(targets.targets, 0)) targets
, (COALESCE(no_targets.behind_los_targets, 0) + COALESCE(targets.behind_los_targets, 0)) behind_los_targets
, (COALESCE(no_targets.short_targets, 0) + COALESCE(targets.short_targets, 0)) short_targets
, (COALESCE(no_targets.medium_targets, 0) + COALESCE(targets.medium_targets, 0)) medium_targets
, (COALESCE(no_targets.deep_targets, 0) + COALESCE(targets.deep_targets, 0)) deep_targets
FROM
  (no_targets
FULL JOIN targets ON ((no_targets.player_id = targets.player_id) AND (no_targets.team_name = targets.team_name) AND (no_targets.season = targets.season)))