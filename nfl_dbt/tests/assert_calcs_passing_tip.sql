{{ config(severity = 'warn') }}
-- Consistency monitor: does the published avg_ttt_attempts reconcile with the
-- attempt-weighted mean of the less/more time-in-pocket splits?
-- Warn-only with 0.05 tolerance: PFF computes the aggregate and the splits on
-- different denominators (attempts vs. passing snaps incl. sacks/scrambles),
-- so exact reconciliation is impossible. A spike in warns = scrape problem;
-- steady low counts = expected definitional drift.
SELECT  player_id, season, week, test_name, expected, actual
FROM (
    SELECT  player_id, season, week,
            'avg_ttt_attempts_weighted' AS test_name,
            avg_ttt_attempts AS expected,
            ((less_avg_time_to_throw * less_attempts) + (more_avg_time_to_throw * more_attempts))
              / NULLIF(less_attempts + more_attempts, 0) AS actual
    FROM    {{ source('pff_raw', 'passing_tip') }}
    WHERE   avg_ttt_attempts IS NOT NULL
    AND     less_attempts IS NOT NULL
    AND     more_attempts IS NOT NULL
    AND     ABS(avg_ttt_attempts -
              ((less_avg_time_to_throw * less_attempts) + (more_avg_time_to_throw * more_attempts))
                / NULLIF(less_attempts + more_attempts, 0)) > 0.05
)
