-- tests/assert_calcs_pass_rush.sql

SELECT  player_id, season, week, test_name, expected, actual
FROM (
    -- Within pass_rush_kpis: lhs + rhs = total
    -- hits
    SELECT  player_id, season, week,
            'hits_lhs_rhs_sum' AS test_name,
            hits AS expected,
            lhs_hits + rhs_hits AS actual
    FROM    {{ source('pff_raw', 'pass_rush_kpis') }}
    WHERE   hits IS NOT NULL
    AND     lhs_hits + rhs_hits != hits

    UNION ALL

    -- hurries
    SELECT  player_id, season, week,
            'hurries_lhs_rhs_sum' AS test_name,
            hurries AS expected,
            lhs_hurries + rhs_hurries AS actual
    FROM    {{ source('pff_raw', 'pass_rush_kpis') }}
    WHERE   hurries IS NOT NULL
    AND     lhs_hurries + rhs_hurries != hurries

    UNION ALL

    -- sacks
    SELECT  player_id, season, week,
            'sacks_lhs_rhs_sum' AS test_name,
            sacks AS expected,
            lhs_sacks + rhs_sacks AS actual
    FROM    {{ source('pff_raw', 'pass_rush_kpis') }}
    WHERE   sacks IS NOT NULL
    AND     lhs_sacks + rhs_sacks != sacks

    UNION ALL

    -- Cross-table: pass_rush_summary vs pass_rush_kpis
    -- hits
    SELECT  s.player_id, s.season, s.week,
            'hits_summary_vs_kpis' AS test_name,
            s.hits AS expected,
            k.hits AS actual
    FROM    {{ source('pff_raw', 'pass_rush_summary') }} s
    JOIN    {{ source('pff_raw', 'pass_rush_kpis') }} k
        ON  s.player_id = k.player_id
        AND CAST(s.season AS VARCHAR) = CAST(k.season AS VARCHAR)
        AND s.week = k.week
    WHERE   s.hits IS NOT NULL
    AND     k.hits IS NOT NULL
    AND     s.hits != k.hits

    UNION ALL

    -- hurries
    SELECT  s.player_id, s.season, s.week,
            'hurries_summary_vs_kpis' AS test_name,
            s.hurries AS expected,
            k.hurries AS actual
    FROM    {{ source('pff_raw', 'pass_rush_summary') }} s
    JOIN    {{ source('pff_raw', 'pass_rush_kpis') }} k
        ON  s.player_id = k.player_id
        AND CAST(s.season AS VARCHAR) = CAST(k.season AS VARCHAR)
        AND s.week = k.week
    WHERE   s.hurries IS NOT NULL
    AND     k.hurries IS NOT NULL
    AND     s.hurries != k.hurries

    UNION ALL

    -- prp
    SELECT  s.player_id, s.season, s.week,
            'prp_summary_vs_kpis' AS test_name,
            s.prp AS expected,
            k.prp AS actual
    FROM    {{ source('pff_raw', 'pass_rush_summary') }} s
    JOIN    {{ source('pff_raw', 'pass_rush_kpis') }} k
        ON  s.player_id = k.player_id
        AND CAST(s.season AS VARCHAR) = CAST(k.season AS VARCHAR)
        AND s.week = k.week
    WHERE   s.prp IS NOT NULL
    AND     k.prp IS NOT NULL
    AND     ABS(s.prp - k.prp) > 0.01

    UNION ALL

    -- sacks
    SELECT  s.player_id, s.season, s.week,
            'sacks_summary_vs_kpis' AS test_name,
            s.sacks AS expected,
            k.sacks AS actual
    FROM    {{ source('pff_raw', 'pass_rush_summary') }} s
    JOIN    {{ source('pff_raw', 'pass_rush_kpis') }} k
        ON  s.player_id = k.player_id
        AND CAST(s.season AS VARCHAR) = CAST(k.season AS VARCHAR)
        AND s.week = k.week
    WHERE   s.sacks IS NOT NULL
    AND     k.sacks IS NOT NULL
    AND     s.sacks != k.sacks
)