SELECT  player_id, season, week, test_name, expected, actual
FROM (
    -- Dropbacks by depth
    SELECT  player_id, season, week,
            'dropbacks_by_depth' AS test_name,
            dropbacks AS expected,
            behind_los_dropbacks + short_dropbacks + medium_dropbacks + deep_dropbacks AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   dropbacks IS NOT NULL
    AND     behind_los_dropbacks + short_dropbacks + medium_dropbacks + deep_dropbacks != dropbacks

    UNION ALL

    -- Attempts by depth
    SELECT  player_id, season, week,
            'attempts_by_depth' AS test_name,
            behind_los_attempts + short_attempts + medium_attempts + deep_attempts AS expected,
            behind_los_attempts + short_attempts + medium_attempts + deep_attempts AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   1=0  -- placeholder, no top-level attempts column

    UNION ALL

    -- Behind LOS: left + center + right = behind_los
    SELECT  player_id, season, week,
            'behind_los_attempts_by_direction' AS test_name,
            behind_los_attempts AS expected,
            left_behind_los_attempts + center_behind_los_attempts + right_behind_los_attempts AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   behind_los_attempts IS NOT NULL
    AND     left_behind_los_attempts + center_behind_los_attempts + right_behind_los_attempts != behind_los_attempts

    UNION ALL

    -- Short: left + center + right = short
    SELECT  player_id, season, week,
            'short_attempts_by_direction' AS test_name,
            short_attempts AS expected,
            left_short_attempts + center_short_attempts + right_short_attempts AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   short_attempts IS NOT NULL
    AND     left_short_attempts + center_short_attempts + right_short_attempts != short_attempts

    UNION ALL

    -- Medium: left + center + right = medium
    SELECT  player_id, season, week,
            'medium_attempts_by_direction' AS test_name,
            medium_attempts AS expected,
            left_medium_attempts + center_medium_attempts + right_medium_attempts AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   medium_attempts IS NOT NULL
    AND     left_medium_attempts + center_medium_attempts + right_medium_attempts != medium_attempts

    UNION ALL

    -- Deep: left + center + right = deep
    SELECT  player_id, season, week,
            'deep_attempts_by_direction' AS test_name,
            deep_attempts AS expected,
            left_deep_attempts + center_deep_attempts + right_deep_attempts AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   deep_attempts IS NOT NULL
    AND     left_deep_attempts + center_deep_attempts + right_deep_attempts != deep_attempts

    UNION ALL

    -- Completions by direction - behind_los
    SELECT  player_id, season, week,
            'behind_los_completions_by_direction' AS test_name,
            behind_los_completions AS expected,
            left_behind_los_completions + center_behind_los_completions + right_behind_los_completions AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   behind_los_completions IS NOT NULL
    AND     left_behind_los_completions + center_behind_los_completions + right_behind_los_completions != behind_los_completions

    UNION ALL

    -- Completions by direction - short
    SELECT  player_id, season, week,
            'short_completions_by_direction' AS test_name,
            short_completions AS expected,
            left_short_completions + center_short_completions + right_short_completions AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   short_completions IS NOT NULL
    AND     left_short_completions + center_short_completions + right_short_completions != short_completions

    UNION ALL

    -- Completions by direction - medium
    SELECT  player_id, season, week,
            'medium_completions_by_direction' AS test_name,
            medium_completions AS expected,
            left_medium_completions + center_medium_completions + right_medium_completions AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   medium_completions IS NOT NULL
    AND     left_medium_completions + center_medium_completions + right_medium_completions != medium_completions

    UNION ALL

    -- Completions by direction - deep
    SELECT  player_id, season, week,
            'deep_completions_by_direction' AS test_name,
            deep_completions AS expected,
            left_deep_completions + center_deep_completions + right_deep_completions AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   deep_completions IS NOT NULL
    AND     left_deep_completions + center_deep_completions + right_deep_completions != deep_completions

    UNION ALL

    -- Yards by direction - behind_los
    SELECT  player_id, season, week,
            'behind_los_yards_by_direction' AS test_name,
            behind_los_yards AS expected,
            left_behind_los_yards + center_behind_los_yards + right_behind_los_yards AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   behind_los_yards IS NOT NULL
    AND     left_behind_los_yards + center_behind_los_yards + right_behind_los_yards != behind_los_yards

    UNION ALL

    -- Yards by direction - short
    SELECT  player_id, season, week,
            'short_yards_by_direction' AS test_name,
            short_yards AS expected,
            left_short_yards + center_short_yards + right_short_yards AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   short_yards IS NOT NULL
    AND     left_short_yards + center_short_yards + right_short_yards != short_yards

    UNION ALL

    -- Yards by direction - medium
    SELECT  player_id, season, week,
            'medium_yards_by_direction' AS test_name,
            medium_yards AS expected,
            left_medium_yards + center_medium_yards + right_medium_yards AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   medium_yards IS NOT NULL
    AND     left_medium_yards + center_medium_yards + right_medium_yards != medium_yards

    UNION ALL

    -- Yards by direction - deep
    SELECT  player_id, season, week,
            'deep_yards_by_direction' AS test_name,
            deep_yards AS expected,
            left_deep_yards + center_deep_yards + right_deep_yards AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   deep_yards IS NOT NULL
    AND     left_deep_yards + center_deep_yards + right_deep_yards != deep_yards

    UNION ALL

    -- Touchdowns by direction - behind_los
    SELECT  player_id, season, week,
            'behind_los_touchdowns_by_direction' AS test_name,
            behind_los_touchdowns AS expected,
            left_behind_los_touchdowns + center_behind_los_touchdowns + right_behind_los_touchdowns AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   behind_los_touchdowns IS NOT NULL
    AND     left_behind_los_touchdowns + center_behind_los_touchdowns + right_behind_los_touchdowns != behind_los_touchdowns

    UNION ALL

    -- Touchdowns by direction - short
    SELECT  player_id, season, week,
            'short_touchdowns_by_direction' AS test_name,
            short_touchdowns AS expected,
            left_short_touchdowns + center_short_touchdowns + right_short_touchdowns AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   short_touchdowns IS NOT NULL
    AND     left_short_touchdowns + center_short_touchdowns + right_short_touchdowns != short_touchdowns

    UNION ALL

    -- Touchdowns by direction - medium
    SELECT  player_id, season, week,
            'medium_touchdowns_by_direction' AS test_name,
            medium_touchdowns AS expected,
            left_medium_touchdowns + center_medium_touchdowns + right_medium_touchdowns AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   medium_touchdowns IS NOT NULL
    AND     left_medium_touchdowns + center_medium_touchdowns + right_medium_touchdowns != medium_touchdowns

    UNION ALL

    -- Touchdowns by direction - deep
    SELECT  player_id, season, week,
            'deep_touchdowns_by_direction' AS test_name,
            deep_touchdowns AS expected,
            left_deep_touchdowns + center_deep_touchdowns + right_deep_touchdowns AS actual
    FROM    {{ source('pff_raw', 'passing_depth') }}
    WHERE   deep_touchdowns IS NOT NULL
    AND     left_deep_touchdowns + center_deep_touchdowns + right_deep_touchdowns != deep_touchdowns
)