-- Pins historical row counts. All seasons validated against old table.
-- If this fails, something changed in raw that shouldn't have.
WITH expected AS (
    SELECT 2016 AS season, 1296 AS expected_rows UNION ALL
    SELECT 2017, 1380 UNION ALL
    SELECT 2018, 1383 UNION ALL
    SELECT 2019, 1346 UNION ALL
    SELECT 2020, 1390 UNION ALL
    SELECT 2021, 1544 UNION ALL
    SELECT 2022, 1552 UNION ALL
    SELECT 2023, 1563 UNION ALL
    SELECT 2024, 1618 UNION ALL
    SELECT 2025, 1500
),
actual AS (
    SELECT season, COUNT(*) AS actual_rows
    FROM {{ ref('stg_pff__receiving_depth_weekly_no_targets') }}
    GROUP BY season
)
SELECT
    e.season,
    e.expected_rows,
    a.actual_rows
FROM expected e
LEFT JOIN actual a ON e.season = a.season
WHERE e.expected_rows <> a.actual_rows
   OR a.actual_rows IS NULL