-- Rate checks: pass_block_rate + route_rate = 100
-- Targets percent by depth should sum to 100

SELECT  player_id, season, week, test_name, expected, actual
FROM (
    -- pass_block_rate + route_rate = 100
    SELECT  player_id, season, week,
            'pass_block_plus_route_rate' AS test_name,
            100.0 AS expected,
            pass_block_rate + route_rate AS actual
    FROM    {{ source('pff_raw', 'receiving_depth_weekly_with_targets') }}
    WHERE   pass_block_rate IS NOT NULL
    AND     route_rate IS NOT NULL
    AND     ABS(pass_block_rate + route_rate - 100) > 0.1

    UNION ALL

    -- targets_percent by depth = 100
    SELECT  player_id, season, week,
            'targets_percent_by_depth' AS test_name,
            100.0 AS expected,
            behind_los_targets_percent + short_targets_percent + medium_targets_percent + deep_targets_percent AS actual
    FROM    {{ source('pff_raw', 'receiving_depth_weekly_with_targets') }}
    WHERE   targets_percent IS NOT NULL
    AND     ABS(behind_los_targets_percent + short_targets_percent + medium_targets_percent + deep_targets_percent - 100) > 0.1
)