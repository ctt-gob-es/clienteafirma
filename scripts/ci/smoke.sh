#!/usr/bin/env bash
# smoke: inventario mínimo del fork. No lanza el instalador ni firma documentos.
set -euo pipefail

root="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$root"

fail=0

need() {
  if [[ ! -e "$1" ]]; then
    echo "FALTA: $1"
    fail=1
  fi
}

need README.md
need LICENSE
need SECURITY.md
need CONTRIBUTING.md
need CHANGELOG.md
need pom.xml
need .github/CODEOWNERS
need .github/PULL_REQUEST_TEMPLATE.md
need .github/renovate.json
need .github/workflows/ci.yml
need license/LICENSE.txt
need license/gpl-2.0.txt
need logo_autofirma.png

# Módulos vigentes que no deben desaparecer en un PR de higiene.
for m in \
  afirma-core \
  afirma-core-keystores \
  afirma-crypto-cades \
  afirma-crypto-xades \
  afirma-crypto-pdf \
  afirma-crypto-validation \
  afirma-simple \
  afirma-server-triphase-signer
do
  need "$m/pom.xml"
done

if ! grep -q 'Cliente @firma\|Autofirma' README.md; then
  echo "README.md no describe el producto"
  fail=1
fi

if [[ "$fail" -ne 0 ]]; then
  echo "smoke: FALLÓ"
  exit 1
fi

echo "smoke: OK (inventario del fork)"
