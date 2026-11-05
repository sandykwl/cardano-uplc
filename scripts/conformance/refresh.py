#!/usr/bin/env python3
"""Copy the conformance cases this package can run into test/conformance.

Usage: refresh.py <plutus checkout> <builtin name>...

A case is copied when its .uplc uses only the builtins named on the command
line and no constr or case, and it has a .flat. Only the .flat and the
.flat.expected come across, at the same path below test-cases/uplc.
"""
import datetime
import os
import re
import shutil
import subprocess
import sys

BUILTIN = re.compile(r"\(\s*builtin\s+([A-Za-z0-9_]+)\s*\)")
CONSTR_CASE = re.compile(r"\(\s*(constr|case)\b")

HERE = os.path.dirname(os.path.abspath(__file__))
DEST = os.path.normpath(os.path.join(HERE, "..", "..", "test", "conformance"))


def strip_comments(s):
    s = re.sub(r"\{-.*?-\}", " ", s, flags=re.S)
    return re.sub(r"--[^\n]*", " ", s)


def runnable(uplc, allowed):
    with open(uplc, encoding="utf-8") as f:
        src = strip_comments(f.read())
    return not CONSTR_CASE.search(src) and set(BUILTIN.findall(src)) <= allowed


def vcs(checkout, *args):
    return subprocess.check_output(["git", "-C", checkout, *args], text=True).strip()


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    checkout, allowed = sys.argv[1], set(sys.argv[2:])
    conf = os.path.join(checkout, "plutus-conformance")
    root = os.path.join(conf, "test-cases", "uplc")
    # Checked before anything is deleted, so a wrong path leaves the cases alone.
    if not os.path.isdir(root):
        sys.exit(f"{root} is not a directory; is {checkout} a plutus checkout?")

    shutil.rmtree(DEST, ignore_errors=True)
    copied = 0
    no_flat = 0
    for d, subdirs, files in os.walk(root):
        subdirs.sort()
        for f in sorted(files):
            if not f.endswith(".uplc"):
                continue
            stem = f[: -len(".uplc")]
            flat = os.path.join(d, stem + ".flat")
            expected = flat + ".expected"
            if not runnable(os.path.join(d, f), allowed):
                continue
            if not (os.path.exists(flat) and os.path.exists(expected)):
                no_flat += 1
                continue
            out = os.path.join(DEST, os.path.relpath(d, root))
            os.makedirs(out, exist_ok=True)
            shutil.copyfile(flat, os.path.join(out, stem + ".flat"))
            shutil.copyfile(expected, os.path.join(out, stem + ".flat.expected"))
            copied += 1

    for name in ("LICENSE", "NOTICE"):
        shutil.copyfile(os.path.join(conf, name), os.path.join(DEST, name))

    commit = vcs(checkout, "rev-parse", "HEAD")
    commit_date = vcs(checkout, "log", "-1", "--format=%cs")
    with open(os.path.join(DEST, "NOTICE"), encoding="utf-8") as f:
        notice = f.read().strip()
    builtins = ", ".join(f"`{b}`" for b in sorted(allowed)) or "none"
    with open(os.path.join(DEST, "SOURCE.md"), "w", encoding="utf-8", newline="\n") as f:
        f.write(f"""# Conformance cases

These files come from `plutus-conformance/test-cases/uplc` in
https://github.com/IntersectMBO/plutus, at commit `{commit}`
(committed {commit_date}, copied {datetime.date.today().isoformat()}).
Each case keeps its path below `test-cases/uplc`, and only its `.flat` input
and `.flat.expected` result are here. `test/Conformance.hs` runs them.

{copied} cases are here: those whose `.uplc` uses no builtin outside
{builtins}, and no `constr` or `case`, and that have a `.flat`. The text-parser
cases have none; {no_flat} otherwise qualifying cases were left out for that.

## Licence

The files are under the Apache License, Version 2.0, in `LICENSE` beside this
one. The upstream `NOTICE` reads:

```
{notice}
```

## Refreshing

```
scripts/conformance/refresh.py <plutus checkout> <builtin name>...
```

Name every builtin the machine implements. The script empties this
directory and fills it again, so cases that no longer qualify disappear.
""")
    print(f"copied {copied} cases from {commit}, skipped {no_flat} qualifying cases without a .flat")


if __name__ == "__main__":
    main()
