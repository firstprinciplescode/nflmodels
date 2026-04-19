WITH
  base_data AS (
   SELECT
     t.team_name
   , t.week
   , t.season
   , ((SUM((t.less_passing_snaps * t.less_grades_pass)) + SUM((t.more_passing_snaps * t.more_grades_pass))) / NULLIF((SUM(t.less_passing_snaps) + SUM(t.more_passing_snaps)), 0)) passing_grade
   , (SELECT player
FROM
  passing_tip p
WHERE ((p.team_name = t.team_name) AND (p.week = t.week) AND (p.season = t.season))
ORDER BY (p.less_passing_snaps + p.more_passing_snaps) DESC
LIMIT 1
) starting_qb
   , (SELECT player_id
FROM
  passing_tip p
WHERE ((p.team_name = t.team_name) AND (p.week = t.week) AND (p.season = t.season))
ORDER BY (p.less_passing_snaps + p.more_passing_snaps) DESC
LIMIT 1
) starting_qb_id
   FROM
     passing_tip t
   GROUP BY t.team_name, t.week, t.season
) 
, with_off_ind AS (
   SELECT
     team_name
   , week
   , season
   , passing_grade
   , starting_qb
   , starting_qb_id
   , (CASE WHEN (ROW_NUMBER() OVER (PARTITION BY team_name, starting_qb_id, season ORDER BY passing_grade DESC) <= FLOOR((COUNT(*) OVER (PARTITION BY team_name, starting_qb_id, season) / 2E0))) THEN 'Good' WHEN (ROW_NUMBER() OVER (PARTITION BY team_name, starting_qb_id, season ORDER BY passing_grade DESC) > CEILING((COUNT(*) OVER (PARTITION BY team_name, starting_qb_id, season) / 2E0))) THEN 'Bad' WHEN (passing_grade >= 6.55E1) THEN 'Good' ELSE 'Bad' END) good_off_ind
   FROM
     base_data
) 
SELECT
  w.season
, w.week
, w.team_name team
, o.opp
, w.passing_grade
, w.starting_qb
, w.starting_qb_id
, w.good_off_ind
, (CASE WHEN (ROW_NUMBER() OVER (PARTITION BY o.opp, w.season ORDER BY w.passing_grade ASC) <= FLOOR((COUNT(*) OVER (PARTITION BY o.opp, w.season) / 2E0))) THEN 'Bad' WHEN (ROW_NUMBER() OVER (PARTITION BY o.opp, w.season ORDER BY w.passing_grade ASC) > CEILING((COUNT(*) OVER (PARTITION BY o.opp, w.season) / 2E0))) THEN 'Good' WHEN (w.passing_grade < 6.55E1) THEN 'Bad' ELSE 'Good' END) good_def_ind
FROM
  (with_off_ind w
LEFT JOIN vw_opponents o ON ((w.season = o.season) AND (w.week = o.week) AND (w.team_name = o.team)))
ORDER BY w.season ASC, w.week ASC, w.team_name ASC