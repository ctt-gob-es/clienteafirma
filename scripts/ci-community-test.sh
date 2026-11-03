#!/usr/bin/env bash
# Invariantes de la capa comunitaria (monorepo). No construye el JAR.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

fail() { echo "FAIL: $*" >&2; exit 1; }
ok() { echo "OK: $*"; }

p0_files=(
  README.md
  LICENSE
  SECURITY.md
  CONTRIBUTING.md
  CHANGELOG.md
  Makefile
  pom.xml
  .github/CODEOWNERS
  .github/PULL_REQUEST_TEMPLATE.md
  .github/ISSUE_TEMPLATE/bug.md
  .github/ISSUE_TEMPLATE/feature.md
  .github/ISSUE_TEMPLATE/config.yml
  .github/workflows/ci.yml
  docs/BASELINE.txt
  docs/MVP.md
  docs/ESTADO-FASES.md
  docs/historico/propuesta-autofirma-2026.md
)

for f in "${p0_files[@]}"; do
  [[ -f "$f" ]] || fail "falta $f"
  ok "$f"
done

grep -q '@Alexendros' .github/CODEOWNERS || fail "CODEOWNERS sin @Alexendros"
ok "CODEOWNERS @Alexendros"

grep -q 'SPDX-License-Identifier: GPL-2.0-or-later OR EUPL-1.1' LICENSE \
  || fail "LICENSE sin SPDX dual"
ok "LICENSE SPDX"

grep -qE '^BASELINE_COMMIT=[0-9a-f]{40}$' docs/BASELINE.txt \
  || fail "BASELINE_COMMIT ausente o no es SHA-40"
grep -q '^PRODUCT_VERSION=1.9.1' docs/BASELINE.txt || fail "PRODUCT_VERSION"
grep -q '^UPSTREAM=' docs/BASELINE.txt || fail "UPSTREAM"
ok "docs/BASELINE.txt"

vector_inputs=(
  vectors/input/plain.txt
  vectors/input/sample.xml
  vectors/input/facturae.xml
  vectors/input/sample.pdf
)
for f in "${vector_inputs[@]}"; do
  [[ -s "$f" ]] || fail "vector vacío o ausente: $f"
done
ok "vectors/input"

shopt -s nullglob
scripts=(scripts/*.sh)
[[ ${#scripts[@]} -gt 0 ]] || fail "no hay scripts/*.sh"
for f in "${scripts[@]}"; do
  head -n 8 "$f" | grep -q '#!/usr/bin/env bash' || fail "shebang: $f"
  grep -q 'set -euo pipefail' "$f" || fail "set -euo pipefail: $f"
done
ok "cabeceras scripts/"

# scripts/ci/*.sh also need shebang + pipefail
for f in scripts/ci/*.sh; do
  head -n 8 "$f" | grep -q '#!/usr/bin/env bash' || fail "shebang: $f"
  grep -q 'set -euo pipefail' "$f" || fail "set -euo pipefail: $f"
done
ok "cabeceras scripts/ci/"

bash scripts/ci/check-docs-truth.sh
ok "documentación contrastada con fuentes"
python3 scripts/ci/test-release-notes.py
ok "notas de release derivadas de assets"

for job in quality test smoke community; do
  grep -Eq "^[[:space:]]*${job}:" .github/workflows/ci.yml \
    || fail "ci.yml sin job $job"
done
ok "jobs CI quality/test/smoke/community"

for f in README.md SECURITY.md CONTRIBUTING.md; do
  grep -q 'Propósito de este documento' "$f" || fail "Propósito ausente: $f"
done
ok "meta-sección Propósito"

for d in 'integra/' 'fire/' 'tools/' 'dist/'; do
  grep -q "$d" .gitignore || fail ".gitignore sin $d"
done
ok ".gitignore toolchains"

[[ -f tests/validation-harness/pom.xml ]] || fail "falta harness pom"
ok "tests/validation-harness/pom.xml"

[[ -f afirma-simple/pom.xml ]] || fail "falta afirma-simple (árbol Java)"
ok "afirma-simple presente"

echo "ci-community-test OK"
