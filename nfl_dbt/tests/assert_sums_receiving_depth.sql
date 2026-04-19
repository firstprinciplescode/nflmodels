SELECT  player_id, season, week, test_name, expected, actual
FROM (
    -- Routes by depth
    SELECT  player_id, season, week,
            'routes_by_depth' AS test_name,
            routes AS expected,
            behind_los_routes + short_routes + medium_routes + deep_routes AS actual
    FROM    {{ source('pff_raw', 'receiving_depth_weekly_with_targets') }}
    WHERE   routes IS NOT NULL
    AND     behind_los_routes + short_routes + medium_routes + deep_routes != routes

    UNION ALL

    -- Targets by depth
    SELECT  player_id, season, week,
            'targets_by_depth' AS test_name,
            targets AS expected,
            behind_los_targets + short_targets + medium_targets + deep_targets AS actual
    FROM    {{ source('pff_raw', 'receiving_depth_weekly_with_targets') }}
    WHERE   targets IS NOT NULL
    AND     behind_los_targets + short_targets + medium_targets + deep_targets != targets

    UNION ALL

    -- Receptions by depth
    SELECT  player_id, season, week,
            'receptions_by_depth' AS test_name,
            receptions AS expected,
            behind_los_receptions + short_receptions + medium_receptions + deep_receptions AS actual
    FROM    {{ source('pff_raw', 'receiving_depth_weekly_with_targets') }}
    WHERE   receptions IS NOT NULL
    AND     behind_los_receptions + short_receptions + medium_receptions + deep_receptions != receptions

    UNION ALL

    -- Yards by depth
    SELECT  player_id, season, week,
            'yards_by_depth' AS test_name,
            yards AS expected,
            behind_los_yards + short_yards + medium_yards + deep_yards AS actual
    FROM    {{ source('pff_raw', 'receiving_depth_weekly_with_targets') }}
    WHERE   yards IS NOT NULL
    AND     behind_los_yards + short_yards + medium_yards + deep_yards != yards

    UNION ALL

    -- Touchdowns by depth
    SELECT  player_id, season, week,
            'touchdowns_by_depth' AS test_name,
            touchdowns AS expected,
            behind_los_touchdowns + short_touchdowns + medium_touchdowns + deep_touchdowns AS actual
    FROM    {{ source('pff_raw', 'receiving_depth_weekly_with_targets') }}
    WHERE   touchdowns IS NOT NULL
    AND     behind_los_touchdowns + short_touchdowns + medium_touchdowns + deep_touchdowns != touchdowns
)