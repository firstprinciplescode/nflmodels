-- If this returns rows, dedup in stg_pff__receiving_depth_weekly_no_targets failed
SELECT
    player_id,
    week,
    season,
    COUNT(*) as dup_count
FROM {{ ref('stg_pff__receiving_depth_weekly_no_targets') }}
GROUP BY player_id, week, season
HAVING COUNT(*) > 1