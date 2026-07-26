SELECT
  season,
week,
abbreviation,
MAX(snap_counts_total) AS total_snaps,
MAX(snap_counts_total_pass) AS total_pass_snaps
FROM
  {{ ref('vw_play_counts_enriched') }}
GROUP BY season, week, abbreviation
