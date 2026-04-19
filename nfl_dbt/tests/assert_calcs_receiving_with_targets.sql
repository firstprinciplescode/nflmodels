SELECT  player_id, season, week, test_name, expected, actual
FROM (
    -- caught_percent = receptions / targets * 100
    SELECT  player_id, season, week,
            'caught_percent_calc' AS test_name,
            caught_percent AS expected,
            ROUND(receptions * 100.0 / targets, 1) AS actual
    FROM    {{ source('pff_raw', 'receiving_with_targets') }}
    WHERE   targets > 0
    AND     caught_percent IS NOT NULL
    AND     ABS(caught_percent - (receptions * 100.0 / targets)) > 0.5

    UNION ALL

    -- yprr = yards / routes
    SELECT  player_id, season, week,
            'yprr_calc' AS test_name,
            yprr AS expected,
            ROUND(yards * 1.0 / routes, 2) AS actual
    FROM    {{ source('pff_raw', 'receiving_with_targets') }}
    WHERE   routes > 0
    AND     yprr IS NOT NULL
    AND     ABS(yprr - (yards * 1.0 / routes)) > 0.05

    UNION ALL

    -- inline_rate = inline_snaps / snaps * 100
    SELECT  player_id, season, week,
            'inline_rate_calc' AS test_name,
            inline_rate AS expected,
            ROUND(inline_snaps * 100.0 / pass_plays, 1) AS actual
    FROM    {{ source('pff_raw', 'receiving_with_targets') }}
    WHERE   pass_plays > 0
    AND     inline_rate IS NOT NULL
    AND     ABS(inline_rate - (inline_snaps * 100.0 / pass_plays)) > 0.5

    UNION ALL

    -- wide_rate = wide_snaps / snaps * 100
    SELECT  player_id, season, week,
            'wide_rate_calc' AS test_name,
            wide_rate AS expected,
            ROUND(wide_snaps * 100.0 / pass_plays, 1) AS actual
    FROM    {{ source('pff_raw', 'receiving_with_targets') }}
    WHERE   pass_plays > 0
    AND     wide_rate IS NOT NULL
    AND     ABS(wide_rate - (wide_snaps * 100.0 / pass_plays)) > 0.5

    UNION ALL

    -- slot_rate = slot_snaps / snaps * 100
    SELECT  player_id, season, week,
            'slot_rate_calc' AS test_name,
            slot_rate AS expected,
            ROUND(slot_snaps * 100.0 / pass_plays, 1) AS actual
    FROM    {{ source('pff_raw', 'receiving_with_targets') }}
    WHERE   pass_plays > 0
    AND     slot_rate IS NOT NULL
    AND     ABS(slot_rate - (slot_snaps * 100.0 / pass_plays)) > 0.5
)