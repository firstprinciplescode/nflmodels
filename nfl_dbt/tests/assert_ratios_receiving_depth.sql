-- Ratio checks: receptions / targets = caught_percent

SELECT  player_id, season, week, test_name, expected, actual
FROM (
    SELECT  player_id, season, week,
            'caught_percent_calculation' AS test_name,
            caught_percent AS expected,
            ROUND(receptions * 100.0 / targets, 1) AS actual
    FROM    {{ source('pff_raw', 'receiving_depth_weekly_with_targets') }}
    WHERE   targets > 0
    AND     caught_percent IS NOT NULL
    AND     ABS(caught_percent - (receptions * 100.0 / targets)) > 0.5
)