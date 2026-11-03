#!/usr/bin/env bash
# Local fixture checks for scripts/ci/semgrep-new-high-gate.py (#89).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
GATE="$ROOT/scripts/ci/semgrep-new-high-gate.py"
FIX="$ROOT/scripts/ci/fixtures/semgrep-gate"

python3 "$GATE" --sarif "$FIX/warning-only.sarif" --baseline "$FIX/baseline.sarif"
echo "OK: warning-only passes"

if python3 "$GATE" --sarif "$FIX/new-high.sarif" --baseline "$FIX/baseline.sarif"; then
  echo "FAIL: new-high should fail" >&2
  exit 1
fi
echo "OK: new-high fails"

python3 "$GATE" --sarif "$FIX/baseline.sarif" --baseline "$FIX/baseline.sarif"
echo "OK: same as baseline passes"

python3 "$GATE" --sarif "$FIX/moved-lines.sarif" --baseline "$FIX/baseline.sarif"
echo "OK: moving lines preserves the finding fingerprint"

if python3 "$GATE" --sarif "$FIX/changed-content.sarif" --baseline "$FIX/baseline.sarif"; then
  echo "FAIL: changed content at the same location should fail" >&2
  exit 1
fi
echo "OK: changed content is a new finding"

if python3 "$GATE" --sarif "$FIX/does-not-exist.sarif" --baseline "$FIX/baseline.sarif"; then
  echo "FAIL: missing SARIF should fail" >&2
  exit 1
fi
echo "OK: missing SARIF fails"

if python3 "$GATE" --sarif "$FIX/invalid.sarif" --baseline "$FIX/baseline.sarif"; then
  echo "FAIL: invalid SARIF should fail" >&2
  exit 1
fi
echo "OK: invalid SARIF fails"

echo "test-semgrep-new-high-gate: all OK"
