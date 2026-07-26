{{ config(severity = 'warn') }}
-- Cross-endpoint drift monitor: pass_rush_summary vs pass_rush_kpis.
-- PFF re-grades stats after the fact, so endpoints scraped at different times
-- legitimately disagree. Warn-only: a sudden spike means a scrape problem;
-- steady low-level drift is expected noise.
SELECT  player_id, season, week, test_name, expected, actual
FROM (
    SELECT  s.player_id, CAST(s.season AS VARCHAR) AS season, s.week,
            'hits' AS test_name, s.hits AS expected, k.hits AS actual
    FROM    {{ source('pff_raw', 'pass_rush_summary') }} s
    JOIN    {{ source('pff_raw', 'pass_rush_kpis') }} k
        ON  s.player_id = k.player_id
        AND CAST(s.season AS VARCHAR) = CAST(k.season AS VARCHAR)
        AND s.week = k.week
    WHERE   s.hits IS NOT NULL AND k.hits IS NOT NULL AND s.hits != k.hits
    UNION ALL
    SELECT  s.player_id, CAST(s.season AS VARCHAR) AS season, s.week,
            'hurries', s.hurries, k.hurries
    FROM    {{ source('pff_raw', 'pass_rush_summary') }} s
    JOIN    {{ source('pff_raw', 'pass_rush_kpis') }} k
        ON  s.player_id = k.player_id
        AND CAST(s.season AS VARCHAR) = CAST(k.season AS VARCHAR)
        AND s.week = k.week
    WHERE   s.hurries IS NOT NULL AND k.hurries IS NOT NULL AND s.hurries != k.hurries
    UNION ALL
    SELECT  s.player_id, CAST(s.season AS VARCHAR) AS season, s.week,
            'sacks', s.sacks, k.sacks
    FROM    {{ source('pff_raw', 'pass_rush_summary') }} s
    JOIN    {{ source('pff_raw', 'pass_rush_kpis') }} k
        ON  s.player_id = k.player_id
        AND CAST(s.season AS VARCHAR) = CAST(k.season AS VARCHAR)
        AND s.week = k.week
    WHERE   s.sacks IS NOT NULL AND k.sacks IS NOT NULL AND s.sacks != k.sacks
    UNION ALL
    SELECT  s.player_id, CAST(s.season AS VARCHAR) AS season, s.week,
            'prp', s.prp, k.prp
    FROM    {{ source('pff_raw', 'pass_rush_summary') }} s
    JOIN    {{ source('pff_raw', 'pass_rush_kpis') }} k
        ON  s.player_id = k.player_id
        AND CAST(s.season AS VARCHAR) = CAST(k.season AS VARCHAR)
        AND s.week = k.week
    WHERE   s.prp IS NOT NULL AND k.prp IS NOT NULL AND ABS(s.prp - k.prp) > 0.01
)
