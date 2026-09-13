"""Smoke test for the PFF Developer API key -- run BEFORE editing the Lambda secret.

  set PFF_API_KEY=ak_live_...        (Windows)   /   export PFF_API_KEY=ak_live_...
  python scripts/pff_api_smoke.py

Reads the key from the environment only; prints statuses, row counts and the
rate-limit headers. The key is never printed. Compare row counts to Athena, e.g.
  SELECT COUNT(*) FROM nfl_data.rushing_summary WHERE season=2025 AND week=1
"""
import os, sys, requests

key = os.environ.get("PFF_API_KEY")
if not key:
    sys.exit("PFF_API_KEY is not set in this shell")
H = {"Authorization": f"Bearer {key}", "Accept": "application/json"}
B = "https://api.pff.com/v1"

r = requests.get(f"{B}/auth/whoami", headers=H, timeout=15)
print("whoami:", r.status_code, {k: v for k, v in r.json().items() if k in ("tier", "entitled", "entitlement_reason", "credential")} if r.ok else r.text[:200])
if not r.ok:
    sys.exit(1)

checks = [
    ("rushing/summary 2025 wk1",   f"{B}/facet/rushing/summary?league=nfl&season=2025&week=1",            "rushing_summary"),
    ("defense/run 2025 wk1",       f"{B}/facet/defense/run?league=nfl&season=2025&week=1",                "run_defense_summary"),
    ("defense/coverage 2025 wk1",  f"{B}/facet/defense/coverage?league=nfl&season=2025&week=1",           "coverage_summary"),
    ("defense/coverage 2021 wk3 (a missing week)", f"{B}/facet/defense/coverage?league=nfl&season=2021&week=3", "coverage_summary"),
    ("games 2025 wk1",             f"{B}/games?league=nfl&season=2025&week=1",                            None),
    ("player/receiving/depth 78092 2024 wk1", f"{B}/player/receiving/depth?league=nfl&season=2024&week=1&player_id=78092", None),
]
for label, url, key_name in checks:
    r = requests.get(url, headers=H, timeout=30)
    rows = "?"
    if r.ok:
        body = r.json()
        k = key_name or next((k for k, v in body.items() if isinstance(v, list)), None)
        rows = len(body.get(k, [])) if k else f"keys={list(body)[:4]}"
    rl = {h: r.headers.get(h) for h in ("x-ratelimit-limit", "x-ratelimit-remaining") if r.headers.get(h)}
    print(f"{label:44s} HTTP {r.status_code}  rows={rows}  {rl}" + ("" if r.ok else f"  {r.text[:160]}"))
