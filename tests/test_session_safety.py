"""Session-safety guards for the R pipeline (added 2026-09-12).

Two things cost a day's work on 2026-09-12:

1. A canon file began with ``rm(list = setdiff(ls(), keep_objects))`` and wiped
   the whole session when sourced. Every such line is now commented out; this
   test fails CI if one comes back.

2. Chain files walled on objects (``rushing_qbgrp``, ``run_defense_qbgrp``)
   that were built by "step-0" code living only in an old session -- no file in
   the repo produced them, so a fresh session could never rebuild them. This
   test parses every ``needed_* <- c("...")`` wall in ``pff_stats/`` and fails
   if any listed object is assigned nowhere in the repo.

Both are pure text scans over ``*.R`` -- no R, no AWS, no data.
"""
import re
from pathlib import Path

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

NEEDED_VEC = re.compile(r"^\s*needed\w*\s*<-\s*c\(([^)]*)\)", re.M)
STRING = re.compile(r'"([^"]+)"')
# `name <- ...` / `name <<- ...` at line start or after a `;` (canon writes
# constants as `X_RB <- 4; G_RB <- 6; N_RB <- 2L`), or assign("name", ...).
# `name = ...` is deliberately NOT counted: at line start it is almost always a
# named argument inside list(...) / a call (the availability files' column
# contracts are `rushing_qbgrp = c("player", ...)`), and canon assigns with `<-`.
PRODUCER = re.compile(r"(?:^|;)\s*([A-Za-z_.][A-Za-z0-9_.]*)\s*<<?-", re.M)
ASSIGN_CALL = re.compile(r'assign\(\s*"([^"]+)"')

# Objects a wall lists that no file in the repo produces, with the reason.
# Adding a NEW name here needs a comment saying where the object comes from.
KNOWN_HOLES = {
    # (none as of 2026-09-12 -- rushing_qbgrp and run_defense_qbgrp got their
    #  step-0 files; opp_2026_teams & co. got pff_stats/shared_ne_2026_constants.R)
}


def producers(txt: str):
    """Names genuinely CREATED in this text.

    `x <- tibble::as_tibble(x)` and `x <- x %>% ...` are re-assignments of an
    object that must already exist -- every chain file opens with a page of
    them -- so an assignment whose right-hand side mentions the same name is
    NOT a producer. The RHS is the rest of the line, or the next line when the
    `<-` ends the line (canon style: `frame <-` newline `left_join(...)`).
    """
    lines = txt.splitlines()
    out = set()
    for i, line in enumerate(lines):
        for m in PRODUCER.finditer(line):
            name = m.group(1)
            rhs = line[m.end():]
            if not rhs.strip() and i + 1 < len(lines):
                rhs = lines[i + 1]
            if re.search(rf"\b{re.escape(name)}\b", rhs):
                continue          # self-referential: re-assignment, not creation
            out.add(name)
    out.update(ASSIGN_CALL.findall(txt))
    return out


def test_every_walled_object_has_a_producer():
    produced = set()
    needed = {}  # object -> set of files that wall on it
    for p in r_files():
        txt = read(p)
        produced.update(producers(txt))
        if "pff_stats" in p.parts:
            rel = str(p.relative_to(REPO)).replace("\\", "/")
            for vec in NEEDED_VEC.finditer(txt):
                for name in STRING.findall(vec.group(1)):
                    needed.setdefault(name, set()).add(rel)

    assert needed, "no needed_* walls found -- the scan regex is broken"

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
