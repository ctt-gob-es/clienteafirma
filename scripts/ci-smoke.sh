#!/usr/bin/env bash
# Smoke mínimo del meta-repo. No firma, no clona, no arranca UI.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

fail() { echo "FAIL: $*" >&2; exit 1; }
ok() { echo "OK: $*"; }

help_out="$(bash scripts/mvp.sh --help)"
printf '%s\n' "$help_out" | grep -q 'Uso: bash scripts/mvp.sh' \
  || fail "mvp.sh --help no muestra el uso"
ok "mvp.sh --help"

dnie_out="$(bash scripts/dnie-hardware-check.sh)"
printf '%s\n' "$dnie_out" | grep -q 'DNIe hardware check' \
  || fail "dnie-hardware-check.sh no emitió la cabecera"
ok "dnie-hardware-check.sh"

presence=(
  docs/MVP.md
  docs/ESTADO-FASES.md
  docs/CODE-REVIEW-360.md
  docs/CODE-REVIEW-360-R2.md
  docs/REMEDIATION-360.md
  docs/VALIDATION-TESTS.md
  docs/SETUP.md
  docs/CIUDADANO.md
  docs/historico/propuesta-autofirma-2026.md
  packaging/README.md
  packaging/portal-prueba/index.html
  packaging/flatpak/org.autofirma.Autofirma2026.yml
  packaging/triphase-compose/docker-compose.yml
  .github/workflows/build-baseline.yml
  .github/workflows/workflow-lint.yml
  scripts/verify-remediation-360.sh
)

for f in "${presence[@]}"; do
  [[ -f "$f" ]] || fail "falta $f"
done
ok "presencia docs/packaging/CI"

echo "ci-smoke OK"
