-- Thin pass-through staging view for raw source `passing_pressure`.
-- Behavior-preserving (`select *`) so downstream results are identical to
-- reading the source directly. Tighten to an explicit column list and/or add
-- dedup logic (see stg_pff__receiving_depth_weekly_no_targets) once validated
-- against Athena.

select *
from {{ source('pff_raw', 'passing_pressure') }}
