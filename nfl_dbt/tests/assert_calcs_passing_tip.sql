SELECT  player_id, season, week, test_name, expected, actual
FROM (
    -- avg_ttt_attempts = less_avg_time_to_throw * less_attempts + more_avg_time_to_throw * more_attempts
    SELECT  player_id, season, week,
            'avg_ttt_attempts_weighted' AS test_name,
            avg_ttt_attempts AS expected,
            (less_avg_time_to_throw * less_attempts) + (more_avg_time_to_throw * more_attempts) AS actual
    FROM    {{ source('pff_raw', 'passing_tip') }}
    WHERE   avg_ttt_attempts IS NOT NULL
    AND     less_attempts IS NOT NULL
    AND     more_attempts IS NOT NULL
    AND     ABS(avg_ttt_attempts - ((less_avg_time_to_throw * less_attempts) + (more_avg_time_to_throw * more_attempts))) > 0.01
)