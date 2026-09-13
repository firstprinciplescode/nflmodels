"""Run a scraper's real auth + fetch code on your machine, against pff_cookies.json,
before deploying. Stands in for Secrets Manager with the local file.

  python scripts/pff_lambda_local_test.py                  # rushing-summary, 2025 week 1
  python scripts/pff_lambda_local_test.py coverage-summary-scraper 2021 3

Prints statuses and row/field counts only. Never prints cookie values.
"""
import importlib.util, json, sys, types
from pathlib import Path

name = sys.argv[1] if len(sys.argv) > 1 else "rushing-summary"
season = int(sys.argv[2]) if len(sys.argv) > 2 else 2025
week = int(sys.argv[3]) if len(sys.argv) > 3 else 1
cj = json.loads(Path("pff_cookies.json").read_text())
if "__client" not in cj:
    sys.exit('pff_cookies.json needs {"__client": "..."}')

# fake boto3 so importing the scraper does not need AWS
class _Secrets:
    def get_secret_value(self, SecretId):
        return {"SecretString": json.dumps({"__client": cj["__client"]})}
class _S3:
    def __getattr__(self, _):
        raise RuntimeError("S3 is not touched in the local test")
fake_boto3 = types.ModuleType("boto3")
fake_boto3.client = lambda svc, **kw: _Secrets() if svc == "secretsmanager" else _S3()
sys.modules["boto3"] = fake_boto3

src = Path("lambdas/scrapers") / f"{name}.py"
spec = importlib.util.spec_from_file_location("scraper", src)
mod = importlib.util.module_from_spec(spec); spec.loader.exec_module(mod)

auth = mod.get_auth()
print("get_auth OK: session id", auth["sid"][:9] + "...")
fns = [f for f in dir(mod) if f.startswith(("scrape_", "fetch_")) and callable(getattr(mod, f))]
print("scrape functions in this file:", fns)
fn = getattr(mod, fns[0])
try:
    out = fn(season, week, auth)
except TypeError:
    out = fn(auth, [season], [week])
n = len(out) if out is not None else 0
cols = list(out.columns)[:6] if hasattr(out, "columns") else (list(out[0])[:6] if out else [])
print(f"{fns[0]}({season}, {week}) -> {n} rows; first cols {cols}")
print("PASS" if n else "FAIL: zero rows")
