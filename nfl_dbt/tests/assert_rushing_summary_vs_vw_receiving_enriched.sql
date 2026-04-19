-- tests/assert_rushing_summary_vs_vw_receiving_enriched.sql

{{ config(
    severity = 'warn',
    warn_if = '>0',
    error_if = '>10'
) }}


SELECT  rs.player_id, rs.season, rs.week, test_name, expected, actual
FROM (
    -- targets
    SELECT  rs.player_id, rs.season, rs.week,
            'targets' AS test_name,
            rs.targets AS expected,
            vr.targets AS actual
    FROM    {{ source('pff_raw', 'rushing_summary') }} rs
    JOIN    {{ ref('vw_receiving_enriched') }} vr
        ON  rs.player_id = vr.player_id
        AND rs.season = vr.season
        AND rs.week = vr.week
    WHERE   rs.targets IS NOT NULL
    AND     vr.targets IS NOT NULL
    AND     rs.targets != vr.targets

    UNION ALL

    -- routes
    SELECT  rs.player_id, rs.season, rs.week,
            'routes' AS test_name,
            rs.routes AS expected,
            vr.routes AS actual
    FROM    {{ source('pff_raw', 'rushing_summary') }} rs
    JOIN    {{ ref('vw_receiving_enriched') }} vr
        ON  rs.player_id = vr.player_id
        AND rs.season = vr.season
        AND rs.week = vr.week
    WHERE   rs.routes IS NOT NULL
    AND     vr.routes IS NOT NULL
    AND     rs.routes != vr.routes

    UNION ALL

    -- receptions
    SELECT  rs.player_id, rs.season, rs.week,
            'receptions' AS test_name,
            rs.receptions AS expected,
            vr.receptions AS actual
    FROM    {{ source('pff_raw', 'rushing_summary') }} rs
    JOIN    {{ ref('vw_receiving_enriched') }} vr
        ON  rs.player_id = vr.player_id
        AND rs.season = vr.season
        AND rs.week = vr.week
    WHERE   rs.receptions IS NOT NULL
    AND     vr.receptions IS NOT NULL
    AND     rs.receptions != vr.receptions
) rs