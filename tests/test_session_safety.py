"""Session-safety guards for the R pipeline (added 2026-09-12).

Two things cost a day's work on 2026-09-12:

1. A canon file began with ``rm(list = setdiff(ls(), keep_objects))`` and wiped
   the whole session when sourced. Every such line is now commented out; this
   test fails CI if one comes back.

2. Chain files walled on objects (``rushing_qbgrp``, ``run_defense_qbgrp``)
   that were built by "step-0" code living only in an old session -- no file in
   the repo produced them, so a fresh session could never rebuild them. This
   test parses every ``*needed* <- c("...")`` wall in ``pff_stats/`` and fails
   if any listed object is assigned nowhere in the repo.

The producer / wall parsing lives in scripts/build_lineage.py (the same code
that generates LINEAGE.md), so the guard and the map can never disagree.
Both are pure text scans over ``*.R`` -- no R, no AWS, no data.
"""
import re
from pathlib import Path

from build_lineage import needs, producers  # scripts/ is on pythonpath via pytest.ini

REPO = Path(__file__).resolve().parents[1]
SKIP_DIRS = {".git", "archive"}


def r_files():
    return [
        p for p in REPO.rglob("*.R")
        if not (set(p.parts) & SKIP_DIRS)
    ]


def read(p: Path) -> str:
    return p.read_text(encoding="utf-8", errors="replace")


# --- 1. no file may wipe the session -------------------------------------

# an ACTIVE (uncommented) rm(list = ls()) or rm(list = setdiff(ls(), ...))
SESSION_WIPE = re.compile(r"^\s*rm\(\s*list\s*=\s*(?:setdiff\(\s*)?ls\(", re.M)


def test_no_file_wipes_the_session():
    offenders = sorted(
        str(p.relative_to(REPO)).replace("\\", "/")
        for p in r_files()
        if SESSION_WIPE.search(read(p))
    )
    assert offenders == [], (
        "these files wipe the R session when sourced; comment the rm() out:\n  "
        + "\n  ".join(offenders)
    )


# --- 2. every walled-on object must have a producer somewhere ------------

# Objects a wall lists that no file in the repo produces, with the reason.
# Adding a NEW name here needs a comment saying where the object comes from.
KNOWN_HOLES = {
    # league_pass_rush_final_evaluation.R:374 lists opp25_lg in a SOFT gate
    # (`if (all(vapply(lg_needed, exists, ...)))`). Nothing in the repo ever
    # creates opp25_lg, so that gate can never pass and the rot26_full /
    # slate_view block behind it is dead code. Found 2026-09-13 by the lineage
    # verification; Andy to rule (build opp25_lg, or retire the block).
    "opp25_lg": "never created; soft gate in league_pass_rush_final_evaluation.R is dead",
}


def test_every_walled_object_has_a_producer():
    produced = set()
    needed = {}  # object -> set of files that wall on it
    for p in r_files():
        txt = read(p)
        produced.update(producers(txt))
        if "pff_stats" in p.parts:
            rel = str(p.relative_to(REPO)).replace("\\", "/")
            for name in needs(txt):
                needed.setdefault(name, set()).add(rel)

    assert needed, "no *needed* walls found -- the scan regex is broken"

    orphans = {
        name: sorted(files)
        for name, files in needed.items()
        if name not in produced and name not in KNOWN_HOLES
    }
    assert orphans == {}, (
        "objects that chain files wall on but NO file in the repo produces "
        "(a step-0 that only ever lived in a session):\n  "
        + "\n  ".join(f"{k}  <- needed by {', '.join(v)}" for k, v in sorted(orphans.items()))
    )
