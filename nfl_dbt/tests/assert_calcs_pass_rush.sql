-- Within pass_rush_kpis: left + right splits must never EXCEED the total.
-- (total > lhs + rhs is legitimate: interior/center alignments are in neither split.)
SELECT  player_id, season, week, test_name, expected, actual
FROM (
    SELECT  player_id, CAST(season AS VARCHAR) AS season, week,
            'hits_lhs_rhs_leq_total' AS test_name,
            hits AS expected, lhs_hits + rhs_hits AS actual
    FROM    {{ source('pff_raw', 'pass_rush_kpis') }}
    WHERE   hits IS NOT NULL AND lhs_hits + rhs_hits > hits
    UNION ALL
    SELECT  player_id, CAST(season AS VARCHAR) AS season, week,
            'hurries_lhs_rhs_leq_total' AS test_name,
            hurries AS expected, lhs_hurries + rhs_hurries AS actual
    FROM    {{ source('pff_raw', 'pass_rush_kpis') }}
    WHERE   hurries IS NOT NULL AND lhs_hurries + rhs_hurries > hurries
    UNION ALL
    SELECT  player_id, CAST(season AS VARCHAR) AS season, week,
            'sacks_lhs_rhs_leq_total' AS test_name,
            sacks AS expected, lhs_sacks + rhs_sacks AS actual
    FROM    {{ source('pff_raw', 'pass_rush_kpis') }}
    WHERE   sacks IS NOT NULL AND lhs_sacks + rhs_sacks > sacks
)
