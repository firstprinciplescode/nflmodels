"""Which PFF cookies does the premium API accept, and will it refresh an expired __session?

Usage (your machine, your cookies -- never paste values into chat):
  1. Create pff_cookies.json next to this repo root (gitignored: *cookie*):
       {"__session": "...", "__refresh_aRfKpDDr": "...", "__client_uat": "..."}
     Values from DevTools > Application > Cookies > premium.pff.com.
  2. python scripts/pff_auth_probe.py
  3. Wait 2+ minutes (the __session JWT lives 60 s), run it again.

Prints only status codes and whether the server handed back a NEW __session.
"""
import json, sys, time, base64
from pathlib import Path

import requests

URL = "https://premium.pff.com/api/v1/facet/rushing/summary?league=nfl&season=2025&week=1"
if not Path("pff_cookies.json").exists():
    sys.exit("pff_cookies.json not found in the repo root -- create it first (see the top of this file):
"
             '  {"__session": "...", "__refresh_aRfKpDDr": "...", "__client_uat": "..."}')
cj = json.loads(Path("pff_cookies.json").read_text())

def jwt_exp(tok):
    try:
        p = tok.split(".")[1]; p += "=" * (-len(p) % 4)
        return json.loads(base64.urlsafe_b64decode(p))["exp"]
    except Exception:
        return None

exp = jwt_exp(cj.get("__session", ""))
if exp:
    print(f"__session JWT expires in {exp - int(time.time())} s")

def probe(label, cookies):
    s = requests.Session()
    r = s.get(URL, cookies=cookies, timeout=15,
              headers={"User-Agent": "Mozilla/5.0", "Accept": "application/json"})
    body = r.text[:200].replace("\n", " ")
    restricted = '"restricted"' in r.text and "true" in r.text[:400]
    new_sess = s.cookies.get("__session")
    refreshed = bool(new_sess) and new_sess != cookies.get("__session")
    print(f"{label:38s} HTTP {r.status_code} | restricted={restricted} | server set NEW __session={refreshed}")
    if r.status_code != 200:
        print("   body:", body)

probe("1) __session only", {k: v for k, v in cj.items() if k == "__session"})
probe("2) __session + refresh + client_uat", {k: v for k, v in cj.items()
                                             if k in ("__session", "__refresh_aRfKpDDr", "__client_uat")})
probe("3) refresh + client_uat, NO __session", {k: v for k, v in cj.items()
                                             if k in ("__refresh_aRfKpDDr", "__client_uat")})
