SELECT  player_id,
        game_id,
        snap_counts_pass,
        snap_counts_pass_block,
        snap_counts_pass_route,
        snap_counts_run,
        snap_counts_run_block,
        snap_counts_total
FROM    {{ source('pff_raw', 'play_counts') }}
WHERE   snap_counts_pass + snap_counts_pass_block + snap_counts_pass_route + snap_counts_run + snap_counts_run_block != snap_counts_total