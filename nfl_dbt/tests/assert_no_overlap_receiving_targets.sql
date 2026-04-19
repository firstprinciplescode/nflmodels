SELECT  wt.player_id, wt.season, wt.week
FROM    {{ source('pff_raw', 'receiving_with_targets') }} wt
JOIN    {{ source('pff_raw', 'receiving_no_targets') }} nt
    ON  wt.player_id = nt.player_id
    AND wt.season = nt.season
    AND wt.week = nt.week