WITH max_game AS (
    SELECT  MAX(season) AS max_season,
            MAX(week) AS max_week
    FROM    {{ source('pff_raw', 'games') }}
    WHERE   season = (SELECT MAX(season) FROM {{ source('pff_raw', 'games') }})
)

SELECT  g.id,
        g.season,
        g.week,
        g.away_abbreviation,
        g.home_abbreviation,
        g.away_score,
        g.home_score
FROM    {{ source('pff_raw', 'games') }} g
CROSS JOIN max_game m
WHERE   (g.season < m.max_season OR (g.season = m.max_season AND g.week < m.max_week))
AND     (g.away_score IS NULL OR g.home_score IS NULL)
AND     g.id != 23348  -- Damar Hamlin cardiac arrest game, cancelled