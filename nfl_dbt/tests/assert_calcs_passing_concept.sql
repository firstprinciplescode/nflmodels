-- tests/assert_calcs_passing_concept.sql

SELECT  player_id, season, week, test_name, expected, actual
FROM (
    -- dropbacks: no_screen + screen = pa + npa = dropbacks
    SELECT  player_id, season, week,
            'dropbacks_screen_sum' AS test_name,
            dropbacks AS expected,
            no_screen_dropbacks + screen_dropbacks AS actual
    FROM    {{ source('pff_raw', 'passing_concept') }}
    WHERE   dropbacks IS NOT NULL
    AND     no_screen_dropbacks + screen_dropbacks != dropbacks

    UNION ALL

    SELECT  player_id, season, week,
            'dropbacks_pa_npa_sum' AS test_name,
            dropbacks AS expected,
            pa_dropbacks + npa_dropbacks AS actual
    FROM    {{ source('pff_raw', 'passing_concept') }}
    WHERE   dropbacks IS NOT NULL
    AND     pa_dropbacks + npa_dropbacks != dropbacks

    UNION ALL

    -- attempts: no_screen + screen = pa + npa
    SELECT  player_id, season, week,
            'attempts_screen_vs_pa' AS test_name,
            no_screen_attempts + screen_attempts AS expected,
            pa_attempts + npa_attempts AS actual
    FROM    {{ source('pff_raw', 'passing_concept') }}
    WHERE   no_screen_attempts + screen_attempts != pa_attempts + npa_attempts

    UNION ALL

    -- completions: no_screen + screen = pa + npa
    SELECT  player_id, season, week,
            'completions_screen_vs_pa' AS test_name,
            no_screen_completions + screen_completions AS expected,
            pa_completions + npa_completions AS actual
    FROM    {{ source('pff_raw', 'passing_concept') }}
    WHERE   no_screen_completions + screen_completions != pa_completions + npa_completions

    UNION ALL

    -- big_time_throws: no_screen + screen = pa + npa
    SELECT  player_id, season, week,
            'big_time_throws_screen_vs_pa' AS test_name,
            no_screen_big_time_throws + screen_big_time_throws AS expected,
            pa_big_time_throws + npa_big_time_throws AS actual
    FROM    {{ source('pff_raw', 'passing_concept') }}
    WHERE   no_screen_big_time_throws + screen_big_time_throws != pa_big_time_throws + npa_big_time_throws

    UNION ALL

    -- passing_snaps: no_screen + screen = pa + npa
    SELECT  player_id, season, week,
            'passing_snaps_screen_vs_pa' AS test_name,
            no_screen_passing_snaps + screen_passing_snaps AS expected,
            pa_passing_snaps + npa_passing_snaps AS actual
    FROM    {{ source('pff_raw', 'passing_concept') }}
    WHERE   no_screen_passing_snaps + screen_passing_snaps != pa_passing_snaps + npa_passing_snaps

    UNION ALL

    -- sacks: no_screen + screen = pa + npa
    SELECT  player_id, season, week,
            'sacks_screen_vs_pa' AS test_name,
            no_screen_sacks + screen_sacks AS expected,
            pa_sacks + npa_sacks AS actual
    FROM    {{ source('pff_raw', 'passing_concept') }}
    WHERE   no_screen_sacks + screen_sacks != pa_sacks + npa_sacks

    UNION ALL

    -- turnover_worthy_plays: no_screen + screen = pa + npa
    SELECT  player_id, season, week,
            'turnover_worthy_plays_screen_vs_pa' AS test_name,
            no_screen_turnover_worthy_plays + screen_turnover_worthy_plays AS expected,
            pa_turnover_worthy_plays + npa_turnover_worthy_plays AS actual
    FROM    {{ source('pff_raw', 'passing_concept') }}
    WHERE   no_screen_turnover_worthy_plays + screen_turnover_worthy_plays != pa_turnover_worthy_plays + npa_turnover_worthy_plays
)