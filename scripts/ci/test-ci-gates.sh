#!/usr/bin/env bash
# Invariantes estáticas de los gates bloqueantes (T3).
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

fail() { echo "FAIL ci-gates: $*" >&2; exit 1; }

for workflow in .github/workflows/ci.yml .github/workflows/codeql-semgrep.yml; do
  ! grep -q 'continue-on-error:' "$workflow" || fail "$workflow tolera errores"
  ! grep -F -q '|| true' "$workflow" || fail "$workflow oculta un fallo con || true"
  grep -F "branches: [master, main, 'plan/*']" "$workflow" >/dev/null \
    || fail "$workflow no escucha las ramas plan/*"
done

! grep -q 'success|skipped' .github/workflows/ci.yml \
  || fail "ci-gate acepta skipped genéricamente"
grep -F 'require_success sbom "$SBOM_RESULT"' .github/workflows/ci.yml >/dev/null \
  || fail "ci-gate no exige SBOM success"
grep -F 'if [[ "$EVENT_NAME" == pull_request ]]' .github/workflows/ci.yml >/dev/null \
  || fail "dependency-review no está condicionado al evento PR"
grep -F 'elif [[ "$DEPENDENCY_REVIEW_RESULT" != skipped ]]' .github/workflows/ci.yml >/dev/null \
  || fail "skipped no está limitado a dependency-review fuera de PR"
grep -F 'if-no-files-found: error' .github/workflows/codeql-semgrep.yml >/dev/null \
  || fail "los artefactos SARIF no son obligatorios"
! grep -F 'if-no-files-found: ignore' .github/workflows/codeql-semgrep.yml >/dev/null \
  || fail "SARIF ausente sigue ignorado"
grep -F 'output: codeql-results' .github/workflows/codeql-semgrep.yml >/dev/null \
  || fail "CodeQL no fija un directorio SARIF dentro del workspace"
! grep -F -- '--error --max-target-bytes' .github/workflows/codeql-semgrep.yml >/dev/null \
  || fail "el escaneo completo convierte hallazgos ya triados en fallo de herramienta"

echo "test-ci-gates: OK"
