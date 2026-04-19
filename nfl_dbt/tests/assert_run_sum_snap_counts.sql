SELECT  player_id,
        game_id,
        snap_counts_run_block,
        snap_counts_run,
        snap_counts_total_run
FROM    {{ source('pff_raw', 'play_counts') }}
WHERE   snap_counts_run_block + snap_counts_run != snap_counts_total_run