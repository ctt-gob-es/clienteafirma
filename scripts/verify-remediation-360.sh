#!/usr/bin/env bash
# Gates de mitigación post-auditoría 360º (asertos positivos).
# Criterio de cierre: make validate && make remediation360
# Uso: bash scripts/verify-remediation-360.sh
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

REPORT="docs/REMEDIATION-360.md"
EVIDENCE_DIR="$ROOT/dist"
EVIDENCE="$EVIDENCE_DIR/REMEDIATION-360-EVIDENCE.txt"
CA="."

pass=0
fail=0
assert_lines=()

ok() {
  pass=$((pass + 1))
  assert_lines+=("PASS|$1|$2")
  echo "PASS: $1 — $2"
}

ko() {
  fail=$((fail + 1))
  assert_lines+=("FAIL|$1|$2")
  echo "FAIL: $1 — $2" >&2
}

need() {
  local id="$1" desc="$2" n="$3"
  if [[ "$n" =~ ^[0-9]+$ ]] && (( n > 0 )); then
    ok "$id" "$desc (n=$n)"
  else
    ko "$id" "$desc (n=$n)"
  fi
}

eq() {
  local id="$1" desc="$2" got="$3" want="$4"
  if [[ "$got" == "$want" ]]; then
    ok "$id" "$desc (got=$got)"
  else
    ko "$id" "$desc (got=$got want=$want)"
  fi
}

has() {
  local id="$1" desc="$2" file="$3" pat="$4"
  if [[ -f "$file" ]] && grep -qE "$pat" "$file"; then
    ok "$id" "$desc"
  else
    ko "$id" "$desc (no match in $file)"
  fi
}

absent() {
  local id="$1" desc="$2" file="$3" pat="$4"
  if [[ -f "$file" ]] && grep -qE "$pat" "$file"; then
    ko "$id" "$desc (patrón indebido en $file)"
  else
    ok "$id" "$desc"
  fi
}

echo "==> verify-remediation-360"
[[ -f "$REPORT" ]] || { echo "Falta $REPORT" >&2; exit 1; }
ok "DOC-000" "playbook REMEDIATION-360 presente"

# --- Meta: SEC-002 portable wrapper ---
has "FIX-002a" "f6 genera /usr/bin/autofirma" \
  "scripts/f6-package-linux.sh" 'usr/bin/autofirma'
need "FIX-002b" "wrapper declara java portable" \
  "$(grep -cE 'JAVA_BIN=\"java\"|/usr/bin/java' scripts/f6-package-linux.sh || true)"
eq "FIX-002c" "sin JAVA_BIN=\$ROOT/tools" \
  "$(grep -cE 'JAVA_BIN=\"\$ROOT/tools' scripts/f6-package-linux.sh || true)" "0"

# --- Meta: SEC-004 Renovate ---
need "FIX-004a" "CHANGELOG menciona Renovate" \
  "$(grep -ci 'Renovate' CHANGELOG.md || true)"
[[ -f .github/renovate.json ]] && ok "FIX-004b" "existe .github/renovate.json" \
  || ko "FIX-004b" "falta .github/renovate.json"

# --- Meta: SEC-010 require-package ---
has "FIX-010a" "mvp.sh documenta --require-package" \
  "scripts/mvp.sh" 'require-package'
has "FIX-010b" "mvp.sh implementa REQUIRE_PACKAGE / fail-closed" \
  "scripts/mvp.sh" 'REQUIRE_PACKAGE'

# --- Docs: SEC-001 TLS opt-in sin default ---
fix001=$(grep -cE 'strictSslChecks|sin cambio de default|Default intacto' docs/F4-INVENTARIO-CRIPTO.md || true)
fix001=$((fix001 + $(grep -cE 'strictSslChecks|sin cambiar el default|No se cambia el default' docs/REMEDIATION-360.md || true)))
need "FIX-001-DOC" "F4/playbook documentan TLS estricto sin cambio de default" "$fix001"

# --- Ops checklist SEC-003 referenced ---
need "FIX-003-DOC" "playbook incluye checklist branch protection" \
  "$(grep -ci 'branch protection' docs/REMEDIATION-360.md || true)"

# --- Index ---
need "INDEX-ESTADO" "ESTADO-FASES enlaza remediación" \
  "$(grep -c 'REMEDIATION-360' docs/ESTADO-FASES.md || true)"
need "INDEX-VALIDATION" "VALIDATION-TESTS documenta gates 360" \
  "$(grep -c 'remediation360\|Gates de seguridad' docs/VALIDATION-TESTS.md || true)"

# --- Cliente / fork ---
if [[ ! -d "$CA" ]]; then
  ko "CLIENT-000" "falta árbol del cliente (pom.xml / afirma-*) (gates fork/SC)"
else
  # SEC-011 SpongyCastle
  sc=$( { rg -l 'org\.spongycastle' -g '*.java' "$CA" || true; } | wc -l | tr -d ' ')
  eq "FIX-011" "clone local sin SpongyCastle" "$sc" "0"

  # SEC-007 XmlHashDocument
  has "FIX-007a" "XmlHashDocument usa SecureXmlBuilder" \
    "$CA/afirma-simple-plugin-hash/src/main/java/es/gob/afirma/plugin/hash/XmlHashDocument.java" \
    'SecureXmlBuilder'
  absent "FIX-007b" "XmlHashDocument.load no usa DocumentBuilderFactory.newInstance" \
    "$CA/afirma-simple-plugin-hash/src/main/java/es/gob/afirma/plugin/hash/XmlHashDocument.java" \
    'DocumentBuilderFactory\.newInstance'
  # Note: generate() may still have been converted; absent checks whole file — after full convert, FIX-007b passes.

  has "FIX-007c" "ContentTypeManager usa SecureXmlBuilder" \
    "$CA/afirma-crypto-ooxml/src/main/java/es/gob/afirma/signers/ooxml/ContentTypeManager.java" \
    'SecureXmlBuilder'
  absent "FIX-007d" "ContentTypeManager sin DocumentBuilderFactory.newInstance" \
    "$CA/afirma-crypto-ooxml/src/main/java/es/gob/afirma/signers/ooxml/ContentTypeManager.java" \
    'DocumentBuilderFactory\.newInstance'

  # SEC-008 triphase
  has "FIX-008a" "XAdESTriPhaseSignerUtil SecureXmlBuilder" \
    "$CA/afirma-server-triphase-signer-core/src/main/java/es/gob/afirma/triphase/signer/processors/XAdESTriPhaseSignerUtil.java" \
    'SecureXmlBuilder'
  absent "FIX-008b" "XAdESTriPhaseSignerUtil sin factory insegura" \
    "$CA/afirma-server-triphase-signer-core/src/main/java/es/gob/afirma/triphase/signer/processors/XAdESTriPhaseSignerUtil.java" \
    'DocumentBuilderFactory\.newInstance'
  has "FIX-008c" "XAdESTriPhaseSignerServerSide SecureXmlBuilder" \
    "$CA/afirma-server-triphase-signer-core/src/main/java/es/gob/afirma/triphase/signer/xades/XAdESTriPhaseSignerServerSide.java" \
    'SecureXmlBuilder'
  absent "FIX-008d" "XAdESTriPhaseSignerServerSide sin factory insegura" \
    "$CA/afirma-server-triphase-signer-core/src/main/java/es/gob/afirma/triphase/signer/xades/XAdESTriPhaseSignerServerSide.java" \
    'DocumentBuilderFactory\.newInstance'

  # Keep controls
  has "KEEP-PROTO-LOCAL" "UrlParameters bloquea localhost" \
    "$CA/afirma-core/src/main/java/es/gob/afirma/core/misc/protocol/UrlParameters.java" \
    'LocalAccessRequestException'
  has "KEEP-PDF-SECURE" "PdfSignResult SecureXmlBuilder" \
    "$CA/afirma-crypto-pdf/src/main/java/es/gob/afirma/signers/pades/PdfSignResult.java" \
    'SecureXmlBuilder\.getSecureDocumentBuilder'
  has "KEEP-XMP-SECURE" "XmpHelper SecureXmlBuilder" \
    "$CA/afirma-crypto-pdf/src/main/java/es/gob/afirma/signers/pades/XmpHelper.java" \
    'SecureXmlBuilder\.getSecureDocumentBuilder'
fi

mkdir -p "$EVIDENCE_DIR"
{
  echo "Autofirma-2026 REMEDIATION-360 verification evidence"
  echo "date=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "meta_rev=$(git rev-parse HEAD)"
  echo "report=$REPORT"
  echo "pass=$pass"
  echo "fail=$fail"
  echo "total=$((pass + fail))"
  if (( pass + fail > 0 )); then
    LC_ALL=C awk -v p="$pass" -v t="$((pass + fail))" 'BEGIN{printf "pass_rate=%.4f\n", p/t}'
  fi
  echo "---"
  printf '%s\n' "${assert_lines[@]}"
} > "$EVIDENCE"

echo
if (( fail > 0 )); then
  echo "VERIFICACIÓN REMEDIATION FALLÓ pass=$pass fail=$fail" >&2
  echo "Evidencia: $EVIDENCE" >&2
  exit 1
fi
echo "VERIFICACIÓN REMEDIATION OK pass=$pass fail=0"
echo "Evidencia: $EVIDENCE"
