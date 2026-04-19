SELECT  player_id,
        game_id,
        snap_counts_pass_route,
        grades_pass_route
FROM    {{ source('pff_raw', 'play_counts') }}
WHERE   snap_counts_pass_route > 0
AND     grades_pass_route IS NULL