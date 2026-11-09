#!/usr/bin/env python3
"""Fail if SARIF contains new Semgrep findings at error/high severity.

Used by CI (#89). Full Semgrep still uploads once; this gate only fails the
build on *new* high/error results vs a baseline SARIF (or empty baseline).
"""
from __future__ import annotations

import argparse
import hashlib
import json
import sys
from pathlib import Path


def level_of(result: dict) -> str:
    lvl = (result.get("level") or "").lower()
    if lvl:
        return lvl
    props = result.get("properties") or {}
    sev = (props.get("security-severity") or props.get("priority") or "").lower()
    return sev


def is_blocking(result: dict) -> bool:
    lvl = level_of(result)
    return lvl in {"error", "high", "critical"}


def key(result: dict) -> tuple:
    """Identify a finding by rule, path and content, never by line number."""
    locs = result.get("locations") or []
    path = None
    region = {}
    if locs:
        phys = (locs[0].get("physicalLocation") or {})
        art = phys.get("artifactLocation") or {}
        region = phys.get("region") or {}
        path = art.get("uri")

    snippet = (region.get("snippet") or {}).get("text")
    if snippet is not None:
        content = {"kind": "snippet", "value": snippet}
    else:
        fingerprints = result.get("partialFingerprints") or result.get("fingerprints") or {}
        match_based = next(
            (fingerprints[name] for name in sorted(fingerprints) if name.startswith("matchBasedId")),
            None,
        )
        if not match_based:
            raise ValueError(
                f"Blocking result {result.get('ruleId')} at {path} has no content fingerprint"
            )
        content = {"kind": "matchBasedId", "value": match_based}

    canonical = json.dumps(content, ensure_ascii=False, sort_keys=True, separators=(",", ":"))
    digest = hashlib.sha256(canonical.encode("utf-8")).hexdigest()
    return (result.get("ruleId"), path, digest)


def load_results(path: Path) -> list:
    if not path.is_file():
        raise ValueError(f"SARIF file is missing: {path}")
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as error:
        raise ValueError(f"SARIF is not valid JSON: {path}: {error}") from error
    if not isinstance(data, dict) or data.get("version") != "2.1.0":
        raise ValueError(f"SARIF must be an object with version 2.1.0: {path}")
    runs = data.get("runs")
    if not isinstance(runs, list) or not runs:
        raise ValueError(f"SARIF must contain at least one tool run: {path}")
    out = []
    for run in runs:
        if not isinstance(run, dict) or not isinstance(run.get("tool"), dict):
            raise ValueError(f"SARIF contains an invalid tool run: {path}")
        results = run.get("results", [])
        if not isinstance(results, list) or not all(isinstance(result, dict) for result in results):
            raise ValueError(f"SARIF results must be a list of objects: {path}")
        out.extend(results)
    return out


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--sarif", required=True, type=Path, help="Current SARIF")
    ap.add_argument(
        "--baseline",
        type=Path,
        default=None,
        help="Baseline SARIF (findings already known). Required when supplied.",
    )
    args = ap.parse_args()

    try:
        current = load_results(args.sarif)
        baseline_keys = set()
        if args.baseline:
            baseline_keys = {key(r) for r in load_results(args.baseline) if is_blocking(r)}

        blockers = []
        for r in current:
            if not is_blocking(r):
                continue
            k = key(r)
            if k in baseline_keys:
                continue
            blockers.append(k)
    except (OSError, ValueError) as error:
        print(f"semgrep-new-high-gate: ERROR: {error}", file=sys.stderr)
        return 2

    if blockers:
        print("New high/error Semgrep findings (blocking):", file=sys.stderr)
        for b in blockers:
            print(f"  {b[0]} @ {b[1]} (content {b[2][:12]})", file=sys.stderr)
        return 1
    print("semgrep-new-high-gate: OK (no new high/error findings)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
