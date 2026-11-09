#!/usr/bin/env bash
# Comprueba afirmaciones documentales contra fuentes versionadas del repo.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

fail() { echo "FAIL docs-truth: $*" >&2; exit 1; }
ok() { echo "OK docs-truth: $*"; }

bc_version="$(python3 - <<'PY'
import xml.etree.ElementTree as ET
root = ET.parse('pom.xml').getroot()
ns = {'m': 'http://maven.apache.org/POM/4.0.0'}
print(root.find('m:properties/m:bouncycastle.version', ns).text)
PY
)"
grep -Fq 'org.bouncycastle` **' README.md || fail "README no identifica la coordenada BouncyCastle"
grep -Fq "**${bc_version}**" README.md || fail "README y pom discrepan en BouncyCastle ${bc_version}"
ok "BouncyCastle ${bc_version} coincide en README.md y pom.xml"

grep -Fq 'SslSecurityManager.isStrictSslChecks()' \
  afirma-core/src/main/java/es/gob/afirma/core/misc/http/UrlHttpManagerImpl.java \
  || fail "UrlHttpManagerImpl no consulta el modo SSL estricto"
grep -R -F -q 'System.getProperty(UrlHttpManagerImpl.JAVA_PARAM_STRICT_SSL_CHECKS, Boolean.TRUE.toString())' \
  afirma-core/src/main/java/es/gob/afirma/core/misc/http/SslSecurityManager.java \
  || fail "SslSecurityManager no activa SSL estricto por defecto"
grep -Fq 'strictSslChecks` por defecto' README.md || fail "README no documenta strictSslChecks por defecto"
ok "SSL estricto por defecto coincide con el código"

for row in 'RPM' 'AppImage' 'Flatpak'; do
  grep -E "^\| ${row} \| Pendiente \|" docs/integration/ROADMAP.md >/dev/null \
    || fail "ROADMAP no mantiene ${row} como pendiente"
done
grep -E '^\| Firmas GPG \(`\.asc`\) en la release \| Pendiente \|' docs/integration/ROADMAP.md >/dev/null \
  || fail "ROADMAP no mantiene las firmas de la release como pendientes"
ok "ROADMAP no anuncia como publicados los assets ausentes"

if [[ -f release-notes.md && -d release-artifacts ]]; then
  python3 scripts/ci/release_notes.py --assets-dir release-artifacts --check release-notes.md
  ok "release-notes.md coincide con release-artifacts/"
else
  ok "sin notas/assets locales que contrastar (se valida en el job de release)"
fi

baseline="$(sed -n 's/^BASELINE_COMMIT=//p' docs/BASELINE.txt)"
[[ "$baseline" =~ ^[0-9a-f]{40}$ ]] || fail "BASELINE_COMMIT inválido"
if [[ "$(git cat-file -t "$baseline" 2>/dev/null || true)" != commit ]]; then
  echo "NOTA docs-truth: baseline ${baseline} tiene formato válido pero no está en este clone shallow"
fi
upstream="$(sed -n 's/^- Último commit integrado del oficial: `//p' docs/upstream-sync.md | sed 's/`$//')"
[[ "$upstream" =~ ^[0-9a-f]{40}$ ]] || fail "SHA de último upstream inválido"
if [[ "$(git cat-file -t "$baseline" 2>/dev/null || true)" == commit \
  && "$(git cat-file -t "$upstream" 2>/dev/null || true)" == commit ]]; then
  ok "baseline y último commit upstream existen localmente"
elif [[ "$(git cat-file -t "$upstream" 2>/dev/null || true)" != commit ]]; then
  echo "NOTA docs-truth: ${upstream} tiene formato válido pero no está en este clone; se contrastará en la próxima sync"
fi

echo "check-docs-truth: OK"
