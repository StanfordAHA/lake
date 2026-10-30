"""The appendix's Halide application code (``app:halide_code``), from the
Halide-to-Hardware generators the thesis apps compile with.

Sources: the app dirs and generator args the garnet app bundles record
(``THESIS/apps/registry.py``), the Halide-to-Hardware commit that aha pins
(``git ls-tree HEAD Halide-to-Hardware`` in the aha checkout, read with ``git
show`` from a local Halide-to-Hardware clone, so the clone's checkout doesn't
matter) and aha's ``aha/util/application_parameters.json`` ("default" profile)
for apps whose registry entry sets no ``HALIDE_GEN_ARGS``. Both checkouts
default to siblings of this lake repo (``../aha``, ``../Halide-to-Hardware``);
override with ``AHA_DIR`` / ``H2H_DIR``.

Each listing keeps the generator verbatim from the file-level declarations
through the algorithm, plus class members defined after ``generate()``, and of
the schedule only the Clockwork branch those args select: ``schedule=N`` (or
the ``schedule`` GeneratorParam's default) picks ``if (schedule == N)`` or a
``Clockwork && schedule == N`` branch, and likewise for a param named ``sched``;
a generator without a schedule param keeps its whole Clockwork branch. Commented-out code is dropped (see
``_code_comment``), and comments mark the omitted branches. The output is the
tex pasted into the appendix of main.tex (Overleaf).
"""

from __future__ import annotations

import json
import os
import re
import shlex
import subprocess
from pathlib import Path

from ..apps.registry import by_id
from .errors import MissingDataError

_LAKE_REPO = Path(__file__).resolve().parents[2]

# (registry app id, listing name, label, generator name when it isn't the app
# dir's). Matrix multiply lists the two-level (pond) entry: the single-level one
# doesn't map yet, and both run the same generator.
APPS = [
    ("gaussian_agg", "Gaussian", "lst:gaussian_halide", ""),
    ("unsharp_agg", "Unsharp", "lst:unsharp_halide", ""),
    ("harris_agg", "Harris color", "lst:harris_color_halide", ""),
    ("camera_pipeline_agg", "Camera Pipeline", "lst:camera_pipe_halide", ""),
    ("matmul_pond_agg", "Matrix Multiplication", "lst:matrix_multiply_halide", "matmul"),
    ("resnet_agg", "ResNet", "lst:resnet_halide", ""),
]

LST_OPTS = "basicstyle=\\scriptsize\\ttfamily, columns=fullflexible, breaklines=true, keepspaces=true"

# matrix_multiplication's comment listing previously working generator args
_NOTE = re.compile(r"^Note, these are the halide gen args")
_ARGS = re.compile(r"^(\w+=\S+\s+){3,}")


def _code_comment(line: str) -> bool:
    """A whole-line // comment that is commented-out code, not prose."""
    s = line.strip()
    if not s.startswith("//"):
        return False
    rest = s[2:].strip()
    if _NOTE.match(rest) or _ARGS.match(rest):
        return True
    assign = re.match(r"^[\w\.]+\s*=[^=]", rest) and re.search(r"[()\[\]<>*/+\-]", rest)
    return bool(re.search(r";\s*(//.*)?$", rest) or rest.endswith(("{", "}"))
                or re.match(r"^[\w\.]+(\(|\.\w)", rest) or assign)


def _block_end(lines: list[str], start: int) -> int:
    """Line holding the brace that closes the one ending lines[start] (a
    header like ``} else if (...) {``)."""
    depth = 1
    for i in range(start + 1, len(lines)):
        for ch in re.sub(r"//.*", "", lines[i]):
            depth += (ch == "{") - (ch == "}")
            if depth == 0:
                return i
    raise MissingDataError(f"unbalanced braces after line {start + 1}")


def _clean(lines: list[str]) -> list[str]:
    """Drop commented-out code and /* */ blocks (not the THE ALGORITHM/SCHEDULE
    markers); collapse blank runs; trim trailing blanks."""
    out, in_block = [], False
    for ln in lines:
        s = ln.strip()
        if in_block:
            in_block = "*/" not in s
            continue
        if s.startswith("/*") and "*/" not in s and "THE " not in s:
            in_block = True
            continue
        if not _code_comment(ln):
            out.append(ln.rstrip())
    res: list[str] = []
    for ln in out:
        if not (ln == "" and res and res[-1] == ""):
            res.append(ln)
    while res and res[-1] == "":
        res.pop()
    return res


def _strip_else(line: str) -> str:
    return re.sub(r"^(\s*)\}\s*else\s+", r"\1", line).rstrip()


def _indent(line: str) -> str:
    return re.match(r"\s*", line).group(0)


def _variant_chain(lines: list[str], cw: int, end: int, p: str, n: str) -> list[str]:
    """A Clockwork branch whose shared schedule holds an ``if ((int)p == K)``
    chain of variants (matmul_rvtry3's ``sched``): the shared code, with the
    chain cut to variant n, or to a comment when n (the default) takes none."""
    head = re.compile(rf"^\s*(\}}\s*else\s+)?if \((\(int\))?{p} == (\d+)\)\s*\{{")
    ch = next((i for i in range(cw + 1, end) if head.match(lines[i])), None)
    if ch is None:
        raise MissingDataError(f"no Clockwork '{p} == {n}' branch")
    branches, j = [], ch
    while True:
        e = _block_end(lines, j)
        branches.append((head.match(lines[j]).group(3), j, e))
        if not head.match(lines[e]):
            break
        j = e
    if re.match(r"\s*\}\s*else\s*\{", lines[e]):
        raise MissingDataError(f"'{p}' chain with an else branch")
    hit = [b for b in branches if b[0] == n]
    out = _clean(lines[cw + 1:ch])
    if hit:
        _, s, e2 = hit[0]
        out.append(_strip_else(lines[s]))
        out += _clean(lines[s + 1:e2])
        out.append(f"{_indent(_strip_else(lines[s]))}}}  // the other {p} variants are omitted")
    else:
        ks = ", ".join(k for k, _, _ in branches)
        out.append(f"{_indent(lines[ch])}// the {p} == {ks} variants are omitted ({p} = {n})")
    return out + _clean(lines[e + 1:end])


def listing(src: str, args: str) -> list[str]:
    """The listing body for one generator source and its generator args."""
    lines = src.splitlines()
    find = lambda pat, lo=0: next((i for i in range(lo, len(lines)) if re.search(pat, lines[i])), None)
    cls = find(r"class \w+ : public Halide::Generator")
    sched = find(r"THE SCHEDULE")
    gen = find(r"void generate\(\)")
    if None in (cls, sched, gen):
        raise MissingDataError("generator lacks a Generator class, generate() or THE SCHEDULE marker")
    pre = max(i for i in range(cls) if re.match(r"\s*(namespace \{|#include)", lines[i]))
    head = _clean(lines[pre + 1:sched])
    while head and head[0] == "":
        head.pop(0)

    d = re.search(r'GeneratorParam<[^>]*>\s*(sched(?:ule)?)\{"\1",\s*(\d+)\}', src)
    p = d.group(1) if d else "schedule"
    m = re.search(rf"\b{p}=(\d+)", args)
    n = m.group(1) if m else d.group(2) if d else None
    body = ["", lines[sched].rstrip()]
    cw = find(rf"has_feature\(Target::Clockwork\)\s*&&\s*{p} == {n}\)\s*\{{", sched) if n else None
    if cw is not None:  # the Clockwork branch is itself per schedule
        body.append(_strip_else(lines[cw]))
        body += _clean(lines[cw + 1:_block_end(lines, cw)])
    else:
        cw = find(r"has_feature\(Target::Clockwork\)\)\s*\{", sched)
        if cw is None:
            raise MissingDataError("no Clockwork schedule")
        body.append(_strip_else(lines[cw]))
        end = _block_end(lines, cw)
        sb = find(rf"if \({p} == {n}\)\s*\{{", cw) if n else None
        sb = sb if sb is not None and sb < end else None
        if sb is not None:
            body.append(_strip_else(lines[sb]))
            body += _clean(lines[sb + 1:_block_end(lines, sb)])
            body.append(f"{_indent(_strip_else(lines[sb]))}}}  // the other Clockwork schedules are omitted")
        elif n:
            body += _variant_chain(lines, cw, end, p, n)
        else:
            body += _clean(lines[cw + 1:end])
    body.append(f"{_indent(_strip_else(lines[cw]))}}}  // the other targets' schedules (incl. CPU) are omitted")
    body.append(f"{_indent(lines[gen])}}}")
    tail = _clean(lines[_block_end(lines, gen) + 1:_block_end(lines, cls)])
    while tail and tail[0] == "":
        tail.pop(0)
    if tail:
        body += [""] + tail
    body.append("};")
    return head + body


def _git(repo: Path, *args: str) -> str:
    r = subprocess.run(["git", "-C", str(repo), *args], capture_output=True, text=True)
    if r.returncode:
        raise MissingDataError(f"git {' '.join(args)} in {repo}: {r.stderr.strip()}")
    return r.stdout


def gen_args(app_id: str, params: dict) -> tuple[str, str]:
    """(HALIDE_GEN_ARGS, HL_TARGET) an app's bundles record: the registry's
    bundle_env, else application_parameters.json's "default" profile (args)."""
    app = by_id(app_id)
    env = dict(kv.partition("=")[::2] for kv in shlex.split(app.bundle_env))
    args = env.get("HALIDE_GEN_ARGS")
    if args is None:
        args = params.get(app.halide_app_dir, {}).get("default", {}).get("HALIDE_GEN_ARGS", "")
    return args, env.get("HL_TARGET", "")


def halide_appendix(ctx, outpath: Path) -> None:
    aha = Path(os.environ.get("AHA_DIR", _LAKE_REPO.parent / "aha"))
    h2h = Path(os.environ.get("H2H_DIR", _LAKE_REPO.parent / "Halide-to-Hardware"))
    params = aha / "aha" / "util" / "application_parameters.json"
    if not params.is_file() or not (h2h / ".git").exists():
        raise MissingDataError(f"needs the aha checkout ({aha}) and a Halide-to-Hardware clone ({h2h})")
    commit = _git(aha, "ls-tree", "HEAD", "Halide-to-Hardware").split()[2]
    branch = _git(aha, "config", "-f", ".gitmodules", "submodule.Halide-to-Hardware.branch").strip()
    app_params = json.loads(params.read_text())
    tex = []
    for app_id, name, label, gen in APPS:
        app_dir = by_id(app_id).halide_app_dir
        path = f"apps/hardware_benchmarks/{app_dir}/{gen or Path(app_dir).name}_generator.cpp"
        args, target = gen_args(app_id, app_params)
        note = f"; target {target}" if target else ""
        code = [f"// Halide-to-Hardware {path}",
                f"// (branch {branch}, {commit[:8]}); generator args: {args or 'none'}{note}", ""]
        code += listing(_git(h2h, "show", f"{commit}:{path}"), args)
        tex.append(f"\\begin{{lstlisting}}[caption=Halide code for the {name} application, "
                   f"language=C++, label={{{label}}}, {LST_OPTS}]")
        tex += code
        tex += ["\\end{lstlisting}", ""]
    outpath.parent.mkdir(parents=True, exist_ok=True)
    outpath.write_text("\n".join(tex))
