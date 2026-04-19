SELECT  player_id, season, week, test_name, expected, actual
FROM (
    -- man_caught_percent = man_receptions / man_targets * 100
    SELECT  player_id, season, week,
            'man_caught_percent_calc' AS test_name,
            man_caught_percent AS expected,
            ROUND(man_receptions * 100.0 / man_targets, 1) AS actual
    FROM    {{ source('pff_raw', 'receiver_scheme') }}
    WHERE   man_targets > 0
    AND     man_caught_percent IS NOT NULL
    AND     ABS(man_caught_percent - (man_receptions * 100.0 / man_targets)) > 0.5

    UNION ALL

    -- zone_caught_percent = zone_receptions / zone_targets * 100
    SELECT  player_id, season, week,
            'zone_caught_percent_calc' AS test_name,
            zone_caught_percent AS expected,
            ROUND(zone_receptions * 100.0 / zone_targets, 1) AS actual
    FROM    {{ source('pff_raw', 'receiver_scheme') }}
    WHERE   zone_targets > 0
    AND     zone_caught_percent IS NOT NULL
    AND     ABS(zone_caught_percent - (zone_receptions * 100.0 / zone_targets)) > 0.5

    UNION ALL

    -- man_yprr = man_yards / man_routes
    SELECT  player_id, season, week,
            'man_yprr_calc' AS test_name,
            man_yprr AS expected,
            ROUND(man_yards * 1.0 / man_routes, 2) AS actual
    FROM    {{ source('pff_raw', 'receiver_scheme') }}
    WHERE   man_routes > 0
    AND     man_yprr IS NOT NULL
    AND     ABS(man_yprr - (man_yards * 1.0 / man_routes)) > 0.05

    UNION ALL

    -- zone_yprr = zone_yards / zone_routes
    SELECT  player_id, season, week,
            'zone_yprr_calc' AS test_name,
            zone_yprr AS expected,
            ROUND(zone_yards * 1.0 / zone_routes, 2) AS actual
    FROM    {{ source('pff_raw', 'receiver_scheme') }}
    WHERE   zone_routes > 0
    AND     zone_yprr IS NOT NULL
    AND     ABS(zone_yprr - (zone_yards * 1.0 / zone_routes)) > 0.05
)