SELECT
  vw_receiving_enriched_scheme.player
, vw_receiving_enriched_scheme.player_id
, vw_receiving_enriched_scheme.team_abbreviation
, vw_receiving_enriched_scheme.season
, MIN(position_group.position_group) position_group
, SUM(pass_plays) pass_plays
, SUM(wide_snaps) wide_snaps
, SUM(slot_snaps) slot_snaps
, SUM(inline_snaps) inline_snaps
, SUM(behind_snaps) behind_snaps
, SUM(routes) routes
, SUM(targets) targets
, SUM(contested_targets) contested_targets
, (SUM(contested_targets) / NULLIF(SUM(targets), 0)) contested_target_rate
, SUM(receptions) receptions
, SUM(contested_receptions) contested_receptions
, (SUM(contested_receptions) / NULLIF(SUM(receptions), 0)) contested_reception_rate
, SUM(yards) yards
, SUM(yards_after_catch) yards_after_catch
, SUM(touchdowns) touchdowns
, SUM(interceptions) interceptions
, (SUM((routes * grades_pass_route)) / NULLIF(SUM(routes), 0)) weighted_avg_grade
, (SUM((routes * targeted_qb_rating)) / NULLIF(SUM(routes), 0)) weighted_avg_qbr
, (SUM((routes * avg_depth_of_target)) / NULLIF(SUM(routes), 0)) weighted_avg_adot
, AVG(grades_pass_route) avg_grade
, AVG(targeted_qb_rating) avg_qbr
, AVG(avg_depth_of_target) avg_adot
, SUM(man_pass_plays) man_pass_plays
, SUM(zone_pass_plays) zone_pass_plays
, SUM(man_pass_blocks) man_pass_blocks
, SUM(zone_pass_blocks) zone_pass_blocks
, SUM(man_routes) man_routes
, SUM(zone_routes) zone_routes
, SUM(man_targets) man_targets
, AVG(man_targets_percent) avg_man_target_share
, SUM(zone_targets) zone_targets
, AVG(zone_targets_percent) avg_zone_target_share
, SUM(man_receptions) man_receptions
, SUM(zone_receptions) zone_receptions
, SUM(man_yards) man_yards
, SUM(zone_yards) zone_yards
, SUM(man_yards_after_catch) man_yards_after_catch
, SUM(zone_yards_after_catch) zone_yards_after_catch
, SUM(man_touchdowns) man_touchdowns
, SUM(zone_touchdowns) zone_touchdowns
, (SUM((man_routes * man_targeted_qb_rating)) / NULLIF(SUM(man_routes), 0)) weighted_avg_qbr_man
, AVG(man_targeted_qb_rating) avg_qbr_man
, (SUM((zone_routes * zone_targeted_qb_rating)) / NULLIF(SUM(zone_routes), 0)) weighted_avg_qbr_zone
, AVG(zone_targeted_qb_rating) avg_qbr_zone
, (SUM((man_routes * man_grades_pass_route)) / NULLIF(SUM(man_routes), 0)) weighted_avg_man_grade
, AVG(man_grades_pass_route) avg_man_grade
, (SUM((zone_routes * zone_grades_pass_route)) / NULLIF(SUM(zone_routes), 0)) weighted_avg_zone_grade
, AVG(zone_grades_pass_route) avg_zone_grade
, AVG(man_avg_depth_of_target) avg_man_adot
, AVG(zone_avg_depth_of_target) avg_zone_adot
FROM
  (vw_receiving_enriched_scheme
LEFT JOIN (
   WITH
     position_counts AS (
      SELECT
        player_id
      , team_abbreviation
      , position_group
      , season
      , COUNT(*) CNT
      , ROW_NUMBER() OVER (PARTITION BY player_id, team_abbreviation, season ORDER BY COUNT(*) DESC, (CASE WHEN (position_group = 'HB') THEN 1 WHEN (position_group = 'TE') THEN 2 WHEN (position_group = 'WR') THEN 3 ELSE 4 END) ASC, position_group ASC) rn
      FROM
        vw_receiving_enriched_scheme
      GROUP BY player_id, team_abbreviation, position_group, season
   ) 
   SELECT
     player_id
   , team_abbreviation
   , season
   , position_group
   FROM
     position_counts
   WHERE (rn = 1)
)  position_group ON ((vw_receiving_enriched_scheme.player_id = position_group.player_id) AND (vw_receiving_enriched_scheme.team_abbreviation = position_group.team_abbreviation) AND (vw_receiving_enriched_scheme.season = position_group.season)))
GROUP BY vw_receiving_enriched_scheme.player, vw_receiving_enriched_scheme.player_id, vw_receiving_enriched_scheme.team_abbreviation, vw_receiving_enriched_scheme.season