"""LINEAGE.md and nfl_dbt/models/exposures.yml are generated from the R code by
scripts/build_lineage.py. This test fails when either committed file is older
than the code -- regenerate with `python scripts/build_lineage.py`.
"""
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]


def test_lineage_files_are_current():
    r = subprocess.run([sys.executable, str(REPO / "scripts" / "build_lineage.py"), "--check"],
                       capture_output=True, text=True, cwd=REPO)
    assert r.returncode == 0, r.stdout + r.stderr
