SELECT
  vw_receiving_enriched_scheme.player,
vw_receiving_enriched_scheme.player_id,
vw_receiving_enriched_scheme.team_abbreviation,
vw_receiving_enriched_scheme.season,
MIN(position_group.position_group) AS position_group,
SUM(pass_plays) AS pass_plays,
SUM(wide_snaps) AS wide_snaps,
SUM(slot_snaps) AS slot_snaps,
SUM(inline_snaps) AS inline_snaps,
SUM(behind_snaps) AS behind_snaps,
SUM(routes) AS routes,
SUM(targets) AS targets,
SUM(contested_targets) AS contested_targets,
(SUM(contested_targets) / NULLIF(SUM(targets), 0)) AS contested_target_rate,
SUM(receptions) AS receptions,
SUM(contested_receptions) AS contested_receptions,
(SUM(contested_receptions) / NULLIF(SUM(receptions), 0)) AS contested_reception_rate,
SUM(yards) AS yards,
SUM(yards_after_catch) AS yards_after_catch,
SUM(touchdowns) AS touchdowns,
SUM(interceptions) AS interceptions,
(SUM((routes * grades_pass_route)) / NULLIF(SUM(routes), 0)) AS weighted_avg_grade,
(SUM((routes * targeted_qb_rating)) / NULLIF(SUM(routes), 0)) AS weighted_avg_qbr,
(SUM((routes * avg_depth_of_target)) / NULLIF(SUM(routes), 0)) AS weighted_avg_adot,
AVG(grades_pass_route) AS avg_grade,
AVG(targeted_qb_rating) AS avg_qbr,
AVG(avg_depth_of_target) AS avg_adot,
SUM(man_pass_plays) AS man_pass_plays,
SUM(zone_pass_plays) AS zone_pass_plays,
SUM(man_pass_blocks) AS man_pass_blocks,
SUM(zone_pass_blocks) AS zone_pass_blocks,
SUM(man_routes) AS man_routes,
SUM(zone_routes) AS zone_routes,
SUM(man_targets) AS man_targets,
AVG(man_targets_percent) AS avg_man_target_share,
SUM(zone_targets) AS zone_targets,
AVG(zone_targets_percent) AS avg_zone_target_share,
SUM(man_receptions) AS man_receptions,
SUM(zone_receptions) AS zone_receptions,
SUM(man_yards) AS man_yards,
SUM(zone_yards) AS zone_yards,
SUM(man_yards_after_catch) AS man_yards_after_catch,
SUM(zone_yards_after_catch) AS zone_yards_after_catch,
SUM(man_touchdowns) AS man_touchdowns,
SUM(zone_touchdowns) AS zone_touchdowns,
(SUM((man_routes * man_targeted_qb_rating)) / NULLIF(SUM(man_routes), 0)) AS weighted_avg_qbr_man,
AVG(man_targeted_qb_rating) AS avg_qbr_man,
(SUM((zone_routes * zone_targeted_qb_rating)) / NULLIF(SUM(zone_routes), 0)) AS weighted_avg_qbr_zone,
AVG(zone_targeted_qb_rating) AS avg_qbr_zone,
(SUM((man_routes * man_grades_pass_route)) / NULLIF(SUM(man_routes), 0)) AS weighted_avg_man_grade,
AVG(man_grades_pass_route) AS avg_man_grade,
(SUM((zone_routes * zone_grades_pass_route)) / NULLIF(SUM(zone_routes), 0)) AS weighted_avg_zone_grade,
AVG(zone_grades_pass_route) AS avg_zone_grade,
AVG(man_avg_depth_of_target) AS avg_man_adot,
AVG(zone_avg_depth_of_target) AS avg_zone_adot
FROM
  ({{ ref('vw_receiving_enriched_scheme') }} AS vw_receiving_enriched_scheme
LEFT JOIN (
   WITH
     position_counts AS (
      SELECT
        player_id,
      team_abbreviation,
      position_group,
      season,
      COUNT(*) AS cnt,
      ROW_NUMBER() OVER (PARTITION BY player_id, team_abbreviation, season ORDER BY COUNT(*) DESC, (CASE WHEN (position_group = 'HB') THEN 1 WHEN (position_group = 'TE') THEN 2 WHEN (position_group = 'WR') THEN 3 ELSE 4 END) ASC, position_group ASC) AS rn
      FROM
        {{ ref('vw_receiving_enriched_scheme') }}
      GROUP BY player_id, team_abbreviation, position_group, season
   ) 

SELECT
     player_id,
   team_abbreviation,
   season,
   position_group
   FROM
     position_counts
   WHERE (rn = 1)
)  AS position_group ON ((vw_receiving_enriched_scheme.player_id = position_group.player_id) AND (vw_receiving_enriched_scheme.team_abbreviation = position_group.team_abbreviation) AND (vw_receiving_enriched_scheme.season = position_group.season)))
GROUP BY vw_receiving_enriched_scheme.player, vw_receiving_enriched_scheme.player_id, vw_receiving_enriched_scheme.team_abbreviation, vw_receiving_enriched_scheme.season
