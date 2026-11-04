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

# T5: ninguna via interna desactiva la validacion TLS con strictSslChecks=true (defecto).
grep -F 'isStrictSslChecks()' afirma-core/src/main/java/es/gob/afirma/core/misc/http/DataDownloader.java >/dev/null \
  || fail "DataDownloader no respeta strictSslChecks"
grep -F 'isStrictSslChecks()' afirma-core/src/main/java/es/gob/afirma/core/misc/http/UrlHttpManagerImpl.java >/dev/null \
  || fail "UrlHttpManagerImpl no consulta strictSslChecks"
grep -F 'isStrictSslChecks()' afirma-core/src/main/java/es/gob/afirma/core/misc/http/SslSecurityManager.java >/dev/null \
  || fail "SslSecurityManager no consulta strictSslChecks"

# Los trust managers «dummy» solo pueden definirse en SslSecurityManager y, en el canal de
# datos, usarse tras el gate estricto de DataDownloader. Los usos aceptables (importar
# certificados) pasan por retrieveServerCertificateChain, que aborta el saludo TLS.
dummy_hits="$(grep -rn 'DUMMY_TRUST_MANAGER\|DUMMY_HOSTNAME_VERIFIER' --include='*.java' \
    afirma-*/src/main 2>/dev/null \
    | grep -v 'SslSecurityManager.java' | grep -v 'DataDownloader.java' || true)"
[ -z "$dummy_hits" ] || fail "trust manager dummy fuera de SslSecurityManager/DataDownloader: $dummy_hits"
for f in UrlHttpManagerImpl.java SSLErrorProcessor.java; do
  ! grep -F 'DUMMY_TRUST_MANAGER' "afirma-core/src/main/java/es/gob/afirma/core/misc/http/$f" >/dev/null \
    || fail "$f usa DUMMY_TRUST_MANAGER"
  ! grep -F 'DUMMY_HOSTNAME_VERIFIER' "afirma-core/src/main/java/es/gob/afirma/core/misc/http/$f" >/dev/null \
    || fail "$f usa DUMMY_HOSTNAME_VERIFIER"
done
! grep -F 'disableSslChecks(' afirma-core/src/main/java/es/gob/afirma/core/misc/http/SSLErrorProcessor.java >/dev/null \
  || fail "SSLErrorProcessor desactiva la validacion TLS"
! grep -F 'disableSslChecks(' afirma-simple/src/main/java/es/gob/afirma/standalone/ui/preferences/ImportCertificatesDialog.java >/dev/null \
  || fail "ImportCertificatesDialog desactiva la validacion TLS"
grep -F 'retrieveServerCertificateChain(' afirma-core/src/main/java/es/gob/afirma/core/misc/http/SSLErrorProcessor.java >/dev/null \
  || fail "SSLErrorProcessor no captura el certificado sin validarlo"
grep -F 'retrieveServerCertificateChain(' afirma-simple/src/main/java/es/gob/afirma/standalone/ui/preferences/ImportCertificatesDialog.java >/dev/null \
  || fail "ImportCertificatesDialog no captura el certificado sin validarlo"
! grep -F 'downloadData(filePath, false, true)' afirma-ui-simple-configurator-common/src/main/java/es/gob/afirma/standalone/configurator/common/ConfigUpdaterManager.java >/dev/null \
  || fail "ConfigUpdaterManager sigue pidiendo saltarse la validacion TLS"
grep -F 'downloadData(filePath, false)' afirma-ui-simple-configurator-common/src/main/java/es/gob/afirma/standalone/configurator/common/ConfigUpdaterManager.java >/dev/null \
  || fail "ConfigUpdaterManager no descarga la configuracion con validacion TLS"

echo "test-ci-gates: T5 OK"
