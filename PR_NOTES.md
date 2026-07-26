# PR: dbt hardening + pytest + CI scaffolding

**Prime directive honored: zero data changes.** Everything here is code. No AWS access
occurred (environment had no credentials). All staging models are `select *`
pass-throughs, so every downstream model produces IDENTICAL results to before.
Fully revertible with `git revert`.

## What changed

### 1. Complete the DAG (bug fixes, behavior-identical)
Seven models referenced tables by bare name — invisible to dbt's dependency graph
(build-order luck, broken lineage, slim CI would skip them):
- `vw_passing_tip`, `vw_team_passing_summary` → bare `passing_tip` (raw source!)
- `vw_receiving_depth_weekly_agg` → bare `receiving_depth_weekly_with_targets`
- `vw_team_passing_summary` → bare `vw_opponents`
- `vw_players_with_routes` → bare `vw_play_counts_enriched`
- `vw_receiving_enriched_scheme_agg` → bare `vw_receiving_enriched_scheme` (x2)
- `vw_receiving_clustering_base` → bare `vw_receiving_depth_weekly_agg` + `vw_receiving_enriched_scheme_agg`
All now proper `ref()`. Where a table was referenced unaliased but with qualified
columns, an explicit alias preserves semantics exactly.

### 2. Staging layer (12 new models)
`stg_pff__<source>` pass-through views for every source in use. No model outside
staging reads `source()` anymore. Follow-up (your call, needs Athena): add dedup
to the raw append-only feeds, mirroring `stg_pff__receiving_depth_weekly_no_targets`.

### 3. Tests: 9 → 45
- `packages.yml` added (dbt_utils via hub; dbt_expectations commented for later)
- Grain-uniqueness tests (`dbt_utils.unique_combination_of_columns`) on every
  intermediate + mart model, with real inferred grains documented per model
- Severity policy: `error` where GROUP BY guarantees uniqueness; `warn` where
  grain is inferred / join fan-out possible. **Warnings on first build = information
  about real duplicate rows, not breakage.** Promote warn → error (or add staging
  dedup) once green.
- Modern syntax throughout (`data_tests:`, `arguments:`) — no deprecation warnings on dbt 1.12

### 4. Real documentation
Every model description rewritten from the actual SQL (grain, logic, purpose).
Boilerplate gone.

### 5. pytest (14 tests, all passing)
`tests/` + `pytest.ini` at repo root. In-memory fake S3 — handlers exercised
exactly as written, zero AWS calls possible. Covers: int64→float64 coercion with
value preservation, `seasons='all'` expansion, NoSuchKey → no_data (and nothing
written), corrupt object → captured error, default season, plus import/handler
smoke test parametrized over all 9 cleaners.

### 6. CI scaffolding (inert until you wire AWS)
- `.github/workflows/ci.yml`: PR-time pytest + SQLFluff + dbt build. First two
  jobs work immediately on push. `dbt-build` needs one-time setup (in comments):
  OIDC provider + role → secret `AWS_OIDC_ROLE_ARN`, plus `ATHENA_STAGING_S3`.
- `.github/workflows/docs.yml`: dbt docs → GitHub Pages on merge (needs Pages
  enabled: Settings → Pages → Source: GitHub Actions).
- `.sqlfluff` config (athena dialect, jinja templater, pragmatic rule excludes).

## Your runbook (in order)
1. Unzip over the repo (paths preserved), review `git diff`
2. `cd nfl_dbt && dbt deps && dbt parse` — should be clean
3. `dbt build` — expect possible WARNs from grain tests; each one is a finding
4. `python -m pytest` at repo root — expect 14 passed
5. Commit as one PR; push; watch pytest + sqlfluff jobs go green
6. When ready: OIDC setup per ci.yml comments to light up dbt-build + docs

## Known deferred decisions (yours)
- Dedup for raw with-targets/summary feeds (changes results = your territory)
- `vw_` prefix on table-materialized marts (rename ripples into R scripts — punt)
- Incremental materialization (needs your partition semantics call)
- SQLFluff will likely flag style issues on first run — lint debt, fix or exclude at leisure
