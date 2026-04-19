SELECT  id,
        season,
        week,
        away_abbreviation,
        home_abbreviation
FROM    {{ source('pff_raw', 'games') }}
WHERE   away_abbreviation IS NULL
OR      home_abbreviation IS NULL