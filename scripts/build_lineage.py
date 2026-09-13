"""Build the R-side lineage from the code itself (added 2026-09-13).

Reads every .R file under pff_stats/ and data_build/ and derives, per file:
  - makes:  objects the file CREATES at top level (see producers())
  - needs:  objects the file depends on through a `*needed* <- c("...")`
            vector -- a WALL when a stop() follows it, a GATE when the vector
            only guards an `if (all(exists(...)))` block (the block is skipped,
            the file continues)
  - athena: tables it pulls inside live run_athena_query("... FROM/JOIN t ...")
            calls (commented-out calls and calls quoted inside cat('...') do
            not count)
  - nflreadr: the nflreadr::load_*() feeds it reads
  - stage:  step0 / schedule / c3 / league / final / availability / viewer / ids / base
  - unit:   the pff_stats sub-folder (run_defense, rushing, ...) or data_build

and writes:
  LINEAGE.md                       Mermaid DAG (GitHub renders it) + tables
  nfl_dbt/models/exposures.yml     one dbt exposure per R file that pulls from
                                   Athena, so the dbt docs graph shows where
                                   each R chain starts from the warehouse

Usage:
  python scripts/build_lineage.py            # (re)write both files
  python scripts/build_lineage.py --check    # exit 1 if either file is stale

tests/test_lineage_current.py runs --check, and tests/test_session_safety.py
imports producers() / needs() from here, so the CI guard and the map can never
disagree. Edges are FILE -> FILE: file B needs object o, file A makes o. When
several files make the same name (each unit's schedule makes its own
`team_band_2026`), the producer in the consumer's own unit wins, then
data_build, then the shared constants file; the cross-unit collisions are
listed as a landmine table.
"""
from __future__ import annotations

import re
import sys
from collections import defaultdict
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]
SCAN_DIRS = ["pff_stats", "data_build"]
LINEAGE_MD = REPO / "LINEAGE.md"
EXPOSURES_YML = REPO / "nfl_dbt" / "models" / "exposures.yml"
SOURCES_YML = REPO / "nfl_dbt" / "models" / "staging" / "pff" / "_pff__sources.yml"
MODELS_DIR = REPO / "nfl_dbt" / "models"
GITHUB_BLOB = "https://github.com/firstprinciplescode/nflmodels/blob/main/"

STAGE_ORDER = ["base", "ids", "constants", "step0", "schedule", "c3", "league", "final", "availability", "viewer", "other"]
UNIT_ORDER = ["data_build", "shared", "run_defense", "rushing", "pass_rush", "pass_block", "run_block",
              "receiving", "secondary", "evaluation"]

# --- regexes ------------------------------------------------------------------
# any dependency vector whose name contains "needed": needed_p4, needed_mz, lg_needed, lg_needed_ol, needed_adj ...
NEEDED_VEC = re.compile(r"^\s*\w*needed\w*\s*<-\s*c\(([^)]*)\)", re.M)
STRING = re.compile(r'"([^"]+)"')
# an assignment at line start, after `;`, or after `{` (`if (x) { a <- 1; b <- 2 }`)
PRODUCER = re.compile(r"(?:^|[;{])\s*([A-Za-z_.][A-Za-z0-9_.]*)\s*<<?-")
ASSIGN_CALL = re.compile(r'assign\(\s*"([^"]+)"')
ATHENA_CALL = re.compile(r'run_athena_query\(\s*"((?:[^"\\]|\\.)*)"', re.S)
SQL_FROM = re.compile(r"\b(?:FROM|JOIN)\s+(?:nfl_data\.)?([A-Za-z_][A-Za-z0-9_]*)", re.I)
NFLREADR = re.compile(r"nflreadr::(load_[a-z_]+)\(")
COMMENT_LINE = re.compile(r"^\s*#")
STR_LIT = re.compile(r'"(?:[^"\\]|\\.)*"|\'(?:[^\'\\]|\\.)*\'')
GUARD = re.compile(r'if\s*\(\s*!\s*exists\(\s*"([^"]+)"\s*\)\s*\)')
DOLLAR = re.compile(r"\$[A-Za-z_.][A-Za-z0-9_.]*")     # pool$n_pool -> a column, not the object
FN_START = re.compile(r"\b(function|local)\s*\(")


def read(p: Path) -> str:
    return p.read_text(encoding="utf-8", errors="replace")


def strip_comments(txt: str) -> str:
    return "\n".join("" if COMMENT_LINE.match(l) else l for l in txt.splitlines())


def _code(raw: str) -> str:
    """The line with string literals blanked and any trailing # comment removed."""
    return STR_LIT.sub('""', raw).split("#", 1)[0]


def producers(txt: str) -> set[str]:
    """Names a file genuinely CREATES in the global environment.

    Not a producer:
      - `x <- f(x)` / `x <- x %>% ...`  -- re-assignment of something that must
        already exist (string literals and `$col` references are blanked first,
        so a table name inside run_athena_query("... FROM x") or `pool$x` does
        not count as a mention of x)
      - an assignment inside a `function(...) { ... }` body or a `local({ ... })`
        -- a local, even when the signature spans several lines
      - an assignment guarded by `if (!exists("x"))` -- a fallback, by its own
        admission: the real producer is whatever defined x first
    """
    lines = txt.splitlines()
    out: set[str] = set()
    stack: list[str] = []            # one entry per open brace: "fn" or "blk"
    guards: list[str | None] = []    # per open brace: the name an enclosing if(!exists("name")) guards
    pending_guard: str | None = None # `if (!exists("x"))` with no brace guards the next statement
    fn_kind: str | None = None       # "function" / "local" whose body brace has not appeared yet
    fn_depth = 0                     # paren depth inside that signature
    for i, raw in enumerate(lines):
        if COMMENT_LINE.match(raw) or not raw.strip():
            continue
        code = _code(raw)
        g = GUARD.search(raw)        # on the RAW line: the guarded name lives inside a string literal
        gname = g.group(1) if g else None
        in_fn = "fn" in stack
        for m in PRODUCER.finditer(code):
            name = m.group(1)
            rhs = code[m.end():]
            if not rhs.strip() and i + 1 < len(lines):
                rhs = _code(lines[i + 1])
            if re.search(rf"\b{re.escape(name)}\b", DOLLAR.sub("$", rhs)):
                continue
            if in_fn:
                continue
            if name == gname or name == pending_guard or name in guards:
                continue
            out.add(name)
        pending_guard = gname if (gname and "{" not in code and not PRODUCER.search(code)) else None
        # brace bookkeeping, char by char, so a function signature that spans lines
        # (`f <- function(a,` ... `b) {`) still marks its body as a function body
        starts = {m.start(): m.group(1) for m in FN_START.finditer(code)}
        closed_here = False
        j = 0
        while j < len(code):
            if j in starts and fn_kind is None:
                fn_kind, fn_depth = starts[j], 0
            ch = code[j]
            if ch == "(":
                if fn_kind:
                    fn_depth += 1
            elif ch == ")":
                if fn_kind:
                    fn_depth -= 1
                    if fn_depth <= 0 and fn_kind == "function":
                        closed_here = True
            elif ch == "{":
                is_body = fn_kind == "function" and fn_depth <= 0 or fn_kind == "local" and fn_depth >= 1
                stack.append("fn" if is_body else "blk")
                guards.append(gname)
                if is_body:
                    fn_kind = None
            elif ch == "}":
                if stack:
                    stack.pop()
                    guards.pop()
            j += 1
        if fn_kind == "function" and closed_here and fn_depth <= 0:
            fn_kind = None            # `function(x) x + 1` -- no brace body on this line
    for raw in lines:
        if not COMMENT_LINE.match(raw):
            out.update(ASSIGN_CALL.findall(raw))
    return out


def needs_detail(txt: str) -> list[tuple[str, str]]:
    """(object, kind) for every *needed* vector; kind = 'wall' when a stop()
    follows the vector within 8 lines, else 'gate' (a soft if/exists guard)."""
    seen: dict[str, str] = {}
    lines = txt.splitlines()
    for vec in NEEDED_VEC.finditer(txt):
        end_line = txt.count("\n", 0, vec.end())
        window = "\n".join(lines[end_line + 1: end_line + 9])
        kind = "wall" if "stop(" in window else "gate"
        for name in STRING.findall(vec.group(1)):
            if name not in seen or kind == "wall":
                seen[name] = kind
    return list(seen.items())


def needs(txt: str) -> list[str]:
    return [n for n, _ in needs_detail(txt)]


def athena_tables(txt: str) -> list[str]:
    """FROM / JOIN tables inside LIVE run_athena_query("...") calls: comment
    lines are dropped first, and a call that sits inside a single-quoted
    literal (a cat('...') printing instructions) is not a call."""
    code = strip_comments(txt)
    quoted = [(m.start(), m.end()) for m in STR_LIT.finditer(code) if m.group(0).startswith("'")]
    out: list[str] = []
    for m in ATHENA_CALL.finditer(code):
        if any(s <= m.start() < e for s, e in quoted):
            continue
        for t in SQL_FROM.findall(m.group(1)):
            t = t.lower()
            if t not in out:
                out.append(t)
    return out


def nflreadr_feeds(txt: str) -> list[str]:
    return sorted(set(NFLREADR.findall(strip_comments(txt))))


def stage_of(rel: str) -> str:
    n = Path(rel).name
    if rel.startswith("data_build/"):
        return "ids" if "pff_ids" in n else "base"
    if "shared_ne_2026_constants" in n:
        return "constants"
    if "any_team_evaluation" in n or "ne_players_evaluation" in n:
        return "viewer"
    if "_step0_" in n or n.endswith("_AWS.R"):
        return "step0"
    if n.startswith("new_england_opp_"):
        return "schedule"
    if "evaluating_currency_three" in n:
        return "c3"
    if n.startswith("league_opp_"):
        return "league"
    if "final_evaluation" in n:
        return "final"
    if n.endswith("_availability.R"):
        return "availability"
    return "other"


def unit_of(rel: str) -> str:
    parts = Path(rel).parts
    if parts[0] == "data_build":
        return "data_build"
    if parts[0] == "pff_stats" and len(parts) == 3:
        return parts[1]
    return "shared"


def scan() -> dict[str, dict]:
    files = {}
    for d in SCAN_DIRS:
        for p in sorted((REPO / d).rglob("*.R")):
            rel = p.relative_to(REPO).as_posix()
            txt = read(p)
            nd = needs_detail(txt)
            files[rel] = {
                "stage": stage_of(rel), "unit": unit_of(rel),
                "makes": producers(txt),
                "needs": [n for n, _ in nd], "need_kind": dict(nd),
                "athena": athena_tables(txt), "nflreadr": nflreadr_feeds(txt),
            }
    return files


def resolve_edges(files: dict[str, dict]):
    by_obj: dict[str, list[str]] = defaultdict(list)
    for rel, f in files.items():
        for o in f["makes"]:
            by_obj[o].append(rel)
    edges: dict[tuple[str, str], list[str]] = defaultdict(list)   # (from, to) -> objects
    hard: set[tuple[str, str]] = set()                             # edges carrying at least one WALL
    holes: list[tuple[str, str, str]] = []                         # (object, kind, needing file)
    self_walls: dict[str, list[str]] = defaultdict(list)           # file -> objects its own earlier section makes
    for rel, f in files.items():
        for o in f["needs"]:
            kind = f["need_kind"][o]
            if o in f["makes"]:
                self_walls[rel].append(o)
                continue
            prods = [p for p in by_obj.get(o, []) if p != rel]
            if not prods:
                holes.append((o, kind, rel))
                continue
            same = [p for p in prods if files[p]["unit"] == f["unit"]]
            base = [p for p in prods if files[p]["unit"] == "data_build"]
            shared = [p for p in prods if files[p]["unit"] == "shared"]
            for p in (same or base or shared or prods):
                edges[(p, rel)].append(o)
                if kind == "wall":
                    hard.add((p, rel))
    needed_all = {o for f in files.values() for o in f["needs"]}
    collisions = {o: sorted(ps) for o, ps in by_obj.items()
                  if o in needed_all and len({files[p]["unit"] for p in ps}) > 1}
    return edges, hard, holes, collisions, dict(self_walls)


def in_graph(rel: str, f: dict, edges) -> bool:
    """Chain files only: anything with a stage, or anything on an edge. Exploration
    scripts that sit inside a unit folder but touch no wall stay in the tables."""
    if f["stage"] != "other":
        return True
    return any(rel in e for e in edges)


# --- Mermaid ------------------------------------------------------------------
def node_id(rel: str) -> str:
    return "f_" + re.sub(r"[^A-Za-z0-9]", "_", rel)


def label(rel: str, f: dict) -> str:
    return f"{Path(rel).name}<br/><i>{f['stage']}</i>"


def mermaid(files, edges, hard) -> str:
    files = {rel: f for rel, f in files.items() if in_graph(rel, f, edges)}
    out = ["flowchart LR"]
    athena = sorted({t for f in files.values() for t in f["athena"]})
    if athena:
        out.append('  subgraph ath["Athena nfl_data"]')
        for t in athena:
            out.append(f'    t_{t}[("{t}")]')
        out.append("  end")
    feeds = sorted({x for f in files.values() for x in f["nflreadr"]})
    if feeds:
        out.append('  subgraph nfr["nflreadr"]')
        for x in feeds:
            out.append(f'    n_{x}[("{x}")]')
        out.append("  end")
    units = sorted({f["unit"] for f in files.values()},
                   key=lambda u: (UNIT_ORDER.index(u) if u in UNIT_ORDER else 99, u))
    for u in units:
        out.append(f'  subgraph {u}["{u}"]')
        members = sorted((rel for rel, f in files.items() if f["unit"] == u),
                         key=lambda r: (STAGE_ORDER.index(files[r]["stage"]), r))
        for rel in members:
            out.append(f'    {node_id(rel)}["{label(rel, files[rel])}"]')
        out.append("  end")
    for rel, f in files.items():
        for t in f["athena"]:
            out.append(f"  t_{t} --> {node_id(rel)}")
        for x in f["nflreadr"]:
            out.append(f"  n_{x} -.-> {node_id(rel)}")
    for (a, b), objs in sorted(edges.items()):
        objs = sorted(objs)
        lab = ", ".join(objs[:3]) + (f" +{len(objs) - 3}" if len(objs) > 3 else "")
        arrow = "-->" if (a, b) in hard else "-.->"      # dashed = soft gate only
        out.append(f'  {node_id(a)} {arrow}|"{lab}"| {node_id(b)}')
    return "\n".join(out)


def md_table(rows, header):
    lines = ["| " + " | ".join(header) + " |", "|" + "|".join("---" for _ in header) + "|"]
    for r in rows:
        lines.append("| " + " | ".join(str(c).replace("|", "\\|") for c in r) + " |")
    return "\n".join(lines)


def build_lineage_md(files, edges, hard, holes, collisions, self_walls) -> str:
    parts = [
        "# LINEAGE — the R pipeline, derived from the code",
        "",
        "*Generated by `python scripts/build_lineage.py` — do not edit by hand. "
        "CI (`tests/test_lineage_current.py`) fails if this file is older than the code.*",
        "",
        "Every node is a file. A solid arrow `A -> B` means B **walls** on an object "
        "(`needed_* <- c(...)` followed by `stop()`) that A creates; a dashed arrow means "
        "B only **gates** a block on it (`if (all(exists(...)))` — skipped, not stopped). "
        "Cylinders are Athena tables pulled inside live `run_athena_query()` calls; "
        "dotted arrows from the nflreadr box are roster/schedule feeds. "
        "Scope: `pff_stats/` and `data_build/`.",
        "",
        "```mermaid",
        mermaid(files, edges, hard),
        "```",
        "",
        "## Holes — needed objects that NO scanned file creates",
        "",
    ]
    if holes:
        parts.append(md_table(sorted(holes), ["object", "wall or gate", "needed by"]))
        parts.append("")
        parts.append("A *wall* hole stops the file. A *gate* hole means the guarded block can never "
                     "run — dead code until the object gets a producer.")
    else:
        parts.append("None. Every needed object has a producer in the scanned files.")
    parts += ["", "## Landmines — one needed name created by files in DIFFERENT units", "",
              "Each unit's schedule overwrites the same session name; the last file sourced wins. "
              "Edges above prefer the producer inside the consumer's own unit, then `data_build`, "
              "then `shared_ne_2026_constants.R`.", ""]
    if collisions:
        parts.append(md_table([(o, "<br/>".join(ps)) for o, ps in sorted(collisions.items())],
                              ["object", "created by"]))
    else:
        parts.append("None.")
    parts += ["", "## Two-part files — a later section needs what an earlier section built", "",
              "`source()` runs top to bottom and stops at the first wall. If section 1 dies, "
              "section 2's wall fires with the names below — and everything after it never runs "
              "(the rushing schedules' composite pack is why rushing needs run defense first).", ""]
    if self_walls:
        parts.append(md_table([(f"`{Path(r).name}`", ", ".join(objs)) for r, objs in sorted(self_walls.items())],
                              ["file", "section-2 needs (made in section 1)"]))
    else:
        parts.append("None.")
    units = sorted({f["unit"] for f in files.values()},
                   key=lambda u: (UNIT_ORDER.index(u) if u in UNIT_ORDER else 99, u))
    for u in units:
        parts += ["", f"## {u}", ""]
        rows = []
        for rel in sorted((r for r, f in files.items() if f["unit"] == u),
                          key=lambda r: (STAGE_ORDER.index(files[r]["stage"]), r)):
            f = files[rel]
            mk = sorted(f["makes"])
            nd = [n if f["need_kind"][n] == "wall" else f"{n} (gate)" for n in f["needs"]]
            rows.append((
                f"`{Path(rel).name}`", f["stage"],
                ", ".join(f["athena"]) or "—",
                ", ".join(nd) or "—",
                f"{len(mk)}: " + ", ".join(mk[:8]) + (" …" if len(mk) > 8 else ""),
            ))
        parts.append(md_table(rows, ["file", "stage", "athena tables", "needs (walls; gates marked)", "makes"]))
    return "\n".join(parts) + "\n"


# --- dbt exposures -------------------------------------------------------------
def source_tables() -> set[str]:
    """Table names under the pff_raw source (6-space `- name:` lines; columns are deeper)."""
    names = set()
    for line in read(SOURCES_YML).splitlines():
        m = re.match(r"^      - name: ([A-Za-z_0-9]+)\s*$", line)
        if m:
            names.add(m.group(1).lower())
    return names


def model_names() -> set[str]:
    return {p.stem.lower() for p in MODELS_DIR.rglob("*.sql")}


def build_exposures_yml(files) -> str:
    srcs, models = source_tables(), model_names()
    out = ["# Generated by scripts/build_lineage.py -- do not edit by hand.",
           "# One exposure per R file that pulls from Athena (step-0s, the id builds,",
           "# the base joins): the tables it pulls, so the dbt docs lineage graph shows",
           "# where each R chain starts from the warehouse.",
           "version: 2", "", "exposures:"]
    for rel, f in sorted(files.items()):
        if not f["athena"]:
            continue
        deps, unknown = [], []
        for t in f["athena"]:
            if t in srcs:
                deps.append(f"source('pff_raw', '{t}')")
            elif t in models:
                deps.append(f"ref('{t}')")
            else:
                unknown.append(t)
        if not deps:
            continue
        name = re.sub(r"[^a-z0-9_]", "_", Path(rel).stem.lower())
        desc = f"R {f['stage']} file {rel} (unit {f['unit']})."
        if unknown:
            desc += " Pulls not in dbt: " + ", ".join(unknown) + "."
        out += [f"  - name: {name}",
                "    type: analysis",
                "    maturity: medium",
                f"    url: {GITHUB_BLOB}{rel}",
                f'    description: "{desc}"',
                "    owner:",
                "      name: Andy",
                "    depends_on:"]
        out += [f"      - {d}" for d in deps]
    return "\n".join(out) + "\n"


def main(argv):
    files = scan()
    edges, hard, holes, collisions, self_walls = resolve_edges(files)
    lineage = build_lineage_md(files, edges, hard, holes, collisions, self_walls)
    exposures = build_exposures_yml(files)
    if "--check" in argv:
        stale = []
        for path, content in ((LINEAGE_MD, lineage), (EXPOSURES_YML, exposures)):
            cur = read(path).replace("\r\n", "\n") if path.exists() else None
            if cur != content:
                stale.append(path.relative_to(REPO).as_posix())
        if stale:
            print("STALE -- regenerate with: python scripts/build_lineage.py\n  " + "\n  ".join(stale))
            return 1
        print("lineage current")
        return 0
    LINEAGE_MD.write_text(lineage, encoding="utf-8", newline="\n")
    EXPOSURES_YML.parent.mkdir(parents=True, exist_ok=True)
    EXPOSURES_YML.write_text(exposures, encoding="utf-8", newline="\n")
    mm = mermaid(files, edges, hard)
    print(f"files {len(files)} (in graph {sum(in_graph(r, f, edges) for r, f in files.items())}) | "
          f"edges {len(edges)} (soft {len(edges) - len(hard)}) | holes {len(holes)} | "
          f"needed cross-unit collisions {len(collisions)} | mermaid {len(mm):,} chars")
    if len(mm) > 45_000:
        print("WARNING: mermaid source over ~45k chars -- GitHub may refuse to render it")
    print(f"wrote {LINEAGE_MD.relative_to(REPO)} and {EXPOSURES_YML.relative_to(REPO)}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
