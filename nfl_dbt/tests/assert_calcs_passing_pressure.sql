SELECT  player_id, season, week, test_name, expected, actual
FROM (
    -- dropbacks: blitz + no_blitz = base_dropbacks
    SELECT  player_id, season, week,
            'dropbacks_blitz_sum' AS test_name,
            base_dropbacks AS expected,
            blitz_dropbacks + no_blitz_dropbacks AS actual
    FROM    {{ source('pff_raw', 'passing_pressure') }}
    WHERE   base_dropbacks IS NOT NULL
    AND     blitz_dropbacks + no_blitz_dropbacks != base_dropbacks

    UNION ALL

    -- dropbacks: pressure + no_pressure = base_dropbacks
    SELECT  player_id, season, week,
            'dropbacks_pressure_sum' AS test_name,
            base_dropbacks AS expected,
            pressure_dropbacks + no_pressure_dropbacks AS actual
    FROM    {{ source('pff_raw', 'passing_pressure') }}
    WHERE   base_dropbacks IS NOT NULL
    AND     pressure_dropbacks + no_pressure_dropbacks != base_dropbacks

    UNION ALL

    -- attempts: blitz + no_blitz = pressure + no_pressure
    SELECT  player_id, season, week,
            'attempts_blitz_vs_pressure' AS test_name,
            blitz_attempts + no_blitz_attempts AS expected,
            pressure_attempts + no_pressure_attempts AS actual
    FROM    {{ source('pff_raw', 'passing_pressure') }}
    WHERE   blitz_attempts + no_blitz_attempts != pressure_attempts + no_pressure_attempts

    UNION ALL

    -- completions: blitz + no_blitz = pressure + no_pressure
    SELECT  player_id, season, week,
            'completions_blitz_vs_pressure' AS test_name,
            blitz_completions + no_blitz_completions AS expected,
            pressure_completions + no_pressure_completions AS actual
    FROM    {{ source('pff_raw', 'passing_pressure') }}
    WHERE   blitz_completions + no_blitz_completions != pressure_completions + no_pressure_completions

    UNION ALL

    -- yards: blitz + no_blitz = pressure + no_pressure
    SELECT  player_id, season, week,
            'yards_blitz_vs_pressure' AS test_name,
            blitz_yards + no_blitz_yards AS expected,
            pressure_yards + no_pressure_yards AS actual
    FROM    {{ source('pff_raw', 'passing_pressure') }}
    WHERE   blitz_yards + no_blitz_yards != pressure_yards + no_pressure_yards

    UNION ALL

    -- touchdowns: blitz + no_blitz = pressure + no_pressure
    SELECT  player_id, season, week,
            'touchdowns_blitz_vs_pressure' AS test_name,
            blitz_touchdowns + no_blitz_touchdowns AS expected,
            pressure_touchdowns + no_pressure_touchdowns AS actual
    FROM    {{ source('pff_raw', 'passing_pressure') }}
    WHERE   blitz_touchdowns + no_blitz_touchdowns != pressure_touchdowns + no_pressure_touchdowns

    UNION ALL

    -- interceptions: blitz + no_blitz = pressure + no_pressure
    SELECT  player_id, season, week,
            'interceptions_blitz_vs_pressure' AS test_name,
            blitz_interceptions + no_blitz_interceptions AS expected,
            pressure_interceptions + no_pressure_interceptions AS actual
    FROM    {{ source('pff_raw', 'passing_pressure') }}
    WHERE   blitz_interceptions + no_blitz_interceptions != pressure_interceptions + no_pressure_interceptions

    UNION ALL

    -- sacks: blitz + no_blitz = pressure + no_pressure
    SELECT  player_id, season, week,
            'sacks_blitz_vs_pressure' AS test_name,
            blitz_sacks + no_blitz_sacks AS expected,
            pressure_sacks + no_pressure_sacks AS actual
    FROM    {{ source('pff_raw', 'passing_pressure') }}
    WHERE   blitz_sacks + no_blitz_sacks != pressure_sacks + no_pressure_sacks

    UNION ALL

    -- scrambles: blitz + no_blitz = pressure + no_pressure
    SELECT  player_id, season, week,
            'scrambles_blitz_vs_pressure' AS test_name,
            blitz_scrambles + no_blitz_scrambles AS expected,
            pressure_scrambles + no_pressure_scrambles AS actual
    FROM    {{ source('pff_raw', 'passing_pressure') }}
    WHERE   blitz_scrambles + no_blitz_scrambles != pressure_scrambles + no_pressure_scrambles

    UNION ALL

    -- big_time_throws: blitz + no_blitz = pressure + no_pressure
    SELECT  player_id, season, week,
            'big_time_throws_blitz_vs_pressure' AS test_name,
            blitz_big_time_throws + no_blitz_big_time_throws AS expected,
            pressure_big_time_throws + no_pressure_big_time_throws AS actual
    FROM    {{ source('pff_raw', 'passing_pressure') }}
    WHERE   blitz_big_time_throws + no_blitz_big_time_throws != pressure_big_time_throws + no_pressure_big_time_throws

    UNION ALL

    -- turnover_worthy_plays: blitz + no_blitz = pressure + no_pressure
    SELECT  player_id, season, week,
            'turnover_worthy_plays_blitz_vs_pressure' AS test_name,
            blitz_turnover_worthy_plays + no_blitz_turnover_worthy_plays AS expected,
            pressure_turnover_worthy_plays + no_pressure_turnover_worthy_plays AS actual
    FROM    {{ source('pff_raw', 'passing_pressure') }}
    WHERE   blitz_turnover_worthy_plays + no_blitz_turnover_worthy_plays != pressure_turnover_worthy_plays + no_pressure_turnover_worthy_plays
)