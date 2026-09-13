"""PFF premium auth probe: is the cookie jar alive, when does it die, and can we mint
a fresh __session ourselves (the way the browser does every 60 s) without buying PFF Pro?

Usage (your machine, your own account's cookies -- never paste values into chat):
  1. Create pff_cookies.json in the repo root (gitignored: *cookie*):
       {"__session": "...", "__client": "..."}
     __session : DevTools > Application > Cookies > https://premium.pff.com
     __client  : DevTools > Application > Cookies > https://clerk.pff.com   (HttpOnly, still shown)
  2. python scripts/pff_auth_probe.py

Prints status codes, field counts, session expiry times and whether a NEW __session
was minted. Never prints cookie or token values.
"""
import json, sys, time, base64
from datetime import datetime, timezone
from pathlib import Path

import requests

API = "https://premium.pff.com/api/v1/facet/rushing/summary?league=nfl&season=2025&week=1"
CLERK = "https://clerk.pff.com/v1"
H = {"User-Agent": "Mozilla/5.0", "Accept": "application/json", "Origin": "https://premium.pff.com",
     "Referer": "https://premium.pff.com/"}

if not Path("pff_cookies.json").exists():
    sys.exit('pff_cookies.json not found in the repo root -- create it first: {"__session": "...", "__client": "..."}')
cj = json.loads(Path("pff_cookies.json").read_text())


def jwt(tok):
    try:
        p = tok.split(".")[1]; p += "=" * (-len(p) % 4)
        return json.loads(base64.urlsafe_b64decode(p))
    except Exception:
        return {}


def when(ms_or_s):
    if not ms_or_s:
        return "n/a"
    t = ms_or_s / 1000 if ms_or_s > 1e11 else ms_or_s
    d = t - time.time()
    return f"{datetime.fromtimestamp(t, timezone.utc):%Y-%m-%d %H:%M}Z ({d/86400:+.1f} days)"


def api_probe(label, session_tok):
    r = requests.get(API, cookies={"__session": session_tok} if session_tok else {}, timeout=15, headers=H)
    try:
        b = r.json(); rows = b.get("rushing_summary", []); restricted = b.get("restricted") or []
        nf = len(rows[0]) if rows else 0
    except Exception:
        rows, nf, restricted = [], 0, ["?"]
    ok = r.status_code == 200 and not restricted
    print(f"{label:44s} HTTP {r.status_code} | rows={len(rows)} | fields/row={nf} | restricted fields={len(restricted)}  -> {'FULL DATA' if ok else 'NO GOOD'}")
    return ok


print("=== 1. the __session cookie you pasted ===")
s = jwt(cj.get("__session", ""))
if s:
    print(f"issuer {s.get('iss')} | sid {str(s.get('sid'))[:9]}... | expires {when(s.get('exp'))}")
api_probe("premium API with pasted __session", cj.get("__session"))
api_probe("premium API with no cookies (control)", None)

client = cj.get("__client")
if not client:
    print("\n=== 2. no __client in pff_cookies.json -- add it (DevTools > Cookies > https://clerk.pff.com) to test refresh ===")
    sys.exit(0)

print("\n=== 2. Clerk client state (this is the 'how long until revoked' answer) ===")
c = jwt(client)
if c:
    print(f"__client JWT: issued {when(c.get('iat'))} | expires {when(c.get('exp'))}")
r = requests.get(f"{CLERK}/client?_clerk_js_version=5", cookies={"__client": client}, timeout=15, headers=H)
print(f"GET /v1/client -> HTTP {r.status_code}")
if r.status_code != 200:
    print(r.text[:300]); sys.exit(1)
body = r.json().get("response") or r.json()
sessions = body.get("sessions") or []
sid = body.get("last_active_session_id")
for ss in sessions:
    print(f"session {ss.get('id','')[:9]}... status={ss.get('status')} | last_active {when(ss.get('last_active_at'))}"
          f" | expire_at {when(ss.get('expire_at'))} | abandon_at {when(ss.get('abandon_at'))}")
if not sid and sessions:
    sid = sessions[0]["id"]
if not sid:
    sys.exit("no session on this client -- you are logged out in that browser")

print("\n=== 3. mint a fresh __session from __client (what the browser does every 60 s) ===")
r = requests.post(f"{CLERK}/client/sessions/{sid}/tokens?_clerk_js_version=5",
                  cookies={"__client": client}, timeout=15, headers=H)
print(f"POST /v1/client/sessions/<sid>/tokens -> HTTP {r.status_code}")
if r.status_code != 200:
    print(r.text[:300]); sys.exit(1)
new_tok = r.json().get("jwt")
nj = jwt(new_tok or "")
print(f"new token minted: {bool(new_tok)} | expires {when(nj.get('exp'))} | differs from pasted: {new_tok != cj.get('__session')}")
ok = api_probe("premium API with the MINTED __session", new_tok)
print("\nVERDICT:", "the Lambdas can refresh themselves from __client -- no PFF Pro needed" if ok else "minted token did not unlock full data")
