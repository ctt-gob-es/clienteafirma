#!/usr/bin/env bash
# Verificación matemática R2: cierra VERIFY y ancla SEC-007…011.
# Uso: bash scripts/verify-code-review-360-r2.sh
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

REPORT="docs/CODE-REVIEW-360-R2.md"
EVIDENCE_DIR="$ROOT/dist"
EVIDENCE="$EVIDENCE_DIR/CODE-REVIEW-360-R2-EVIDENCE.txt"
CA="."

pass=0
fail=0
assert_lines=()

ok() { pass=$((pass + 1)); assert_lines+=("PASS|$1|$2"); echo "PASS: $1 — $2"; }
ko() { fail=$((fail + 1)); assert_lines+=("FAIL|$1|$2"); echo "FAIL: $1 — $2" >&2; }

need() {
  local id="$1" desc="$2" n="$3"
  if [[ "$n" =~ ^[0-9]+$ ]] && (( n > 0 )); then ok "$id" "$desc (n=$n)"; else ko "$id" "$desc (n=$n)"; fi
}
eq() {
  local id="$1" desc="$2" got="$3" want="$4"
  if [[ "$got" == "$want" ]]; then ok "$id" "$desc (got=$got)"; else ko "$id" "$desc (got=$got want=$want)"; fi
}
has() {
  local id="$1" desc="$2" file="$3" pat="$4"
  if [[ -f "$file" ]] && grep -qE "$pat" "$file"; then ok "$id" "$desc"; else ko "$id" "$desc"; fi
}

echo "==> verify-code-review-360-r2"
[[ -f "$REPORT" ]] || { echo "Falta $REPORT" >&2; exit 1; }
ok "DOC-R2" "informe R2 presente"
[[ -f docs/CODE-REVIEW-360.md ]] && ok "DOC-R1" "informe R1 presente" || ko "DOC-R1" "falta R1"

# Cierres VERIFY documentados
for id in VERIFY-001 VERIFY-002 VERIFY-003; do
  need "REP-$id" "R2 menciona $id" "$(grep -c "$id" "$REPORT" || true)"
done
need "REP-PROMOTED" "R2 documenta promovido" "$(grep -ci 'Promovido' "$REPORT" || true)"
need "REP-DISCARD" "R2 documenta descartado" "$(grep -ci 'Descartado' "$REPORT" || true)"

for id in SEC-007 SEC-008 SEC-009 SEC-010 SEC-011; do
  need "REP-$id" "R2 menciona $id" "$(grep -c "$id" "$REPORT" || true)"
done

if [[ ! -d "$CA" ]]; then
  ko "CLIENT-000" "falta árbol del cliente (pom.xml / afirma-*)"
else
  # SEC-007/008: remediados — asertos de mitigación (ver REMEDIATION-360)
  has "SEC-007a" "XmlHashDocument SecureXmlBuilder" \
    "$CA/afirma-simple-plugin-hash/src/main/java/es/gob/afirma/plugin/hash/XmlHashDocument.java" \
    'SecureXmlBuilder'
  if grep -q 'DocumentBuilderFactory\.newInstance' \
      "$CA/afirma-simple-plugin-hash/src/main/java/es/gob/afirma/plugin/hash/XmlHashDocument.java"; then
    ko "SEC-007b" "XmlHashDocument aún usa DocumentBuilderFactory.newInstance"
  else
    ok "SEC-007b" "XmlHashDocument sin factory insegura"
  fi

  has "SEC-007c" "ContentTypeManager SecureXmlBuilder" \
    "$CA/afirma-crypto-ooxml/src/main/java/es/gob/afirma/signers/ooxml/ContentTypeManager.java" \
    'SecureXmlBuilder'
  if grep -q 'DocumentBuilderFactory\.newInstance' \
      "$CA/afirma-crypto-ooxml/src/main/java/es/gob/afirma/signers/ooxml/ContentTypeManager.java"; then
    ko "SEC-007d" "ContentTypeManager aún usa factory insegura"
  else
    ok "SEC-007d" "ContentTypeManager sin factory insegura"
  fi

  has "SEC-008a" "XAdESTriPhaseSignerUtil SecureXmlBuilder" \
    "$CA/afirma-server-triphase-signer-core/src/main/java/es/gob/afirma/triphase/signer/processors/XAdESTriPhaseSignerUtil.java" \
    'SecureXmlBuilder'
  has "SEC-008b" "XAdESTriPhaseSignerServerSide SecureXmlBuilder" \
    "$CA/afirma-server-triphase-signer-core/src/main/java/es/gob/afirma/triphase/signer/xades/XAdESTriPhaseSignerServerSide.java" \
    'SecureXmlBuilder'
  if grep -q 'DocumentBuilderFactory\.newInstance' \
      "$CA/afirma-server-triphase-signer-core/src/main/java/es/gob/afirma/triphase/signer/processors/XAdESTriPhaseSignerUtil.java" \
      "$CA/afirma-server-triphase-signer-core/src/main/java/es/gob/afirma/triphase/signer/xades/XAdESTriPhaseSignerServerSide.java"; then
    ko "SEC-008c" "triphase aún tiene DocumentBuilderFactory.newInstance"
  else
    ok "SEC-008c" "triphase sin factory insegura"
  fi

  # VERIFY-001 dismiss XmpHelper uses SecureXmlBuilder
  has "V1-DISMISS-XMP" "XmpHelper parse seguro" \
    "$CA/afirma-crypto-pdf/src/main/java/es/gob/afirma/signers/pades/XmpHelper.java" \
    'SecureXmlBuilder\.getSecureDocumentBuilder'

  # VERIFY-003 dismiss PdfSignResult
  has "V3-DISMISS" "PdfSignResult SecureXmlBuilder" \
    "$CA/afirma-crypto-pdf/src/main/java/es/gob/afirma/signers/pades/PdfSignResult.java" \
    'SecureXmlBuilder\.getSecureDocumentBuilder\(\)\.parse'

  # VERIFY-002: LookAndFeelManager exec with literal queries present
  has "V2-CONST-EXEC" "LookAndFeelManager Runtime.exec" \
    "$CA/afirma-simple/src/main/java/es/gob/afirma/standalone/LookAndFeelManager.java" \
    'Runtime\.getRuntime\(\)\.exec\(cmd\)'
  need "V2-LITERALS" "queries literales gnome/gsettings" \
    "$(grep -cE 'gsettings get|XDG_CURRENT_DESKTOP|ps -e' "$CA/afirma-simple/src/main/java/es/gob/afirma/standalone/LookAndFeelManager.java" || true)"

  # SEC-009 plugins (trust boundary — aún documentado, no remediado en producto)
  has "SEC-009a" "PluginLoader URLClassLoader" \
    "$CA/afirma-simple-plugins-manager/src/main/java/es/gob/afirma/standalone/plugins/manager/PluginLoader.java" \
    'new URLClassLoader'

  # Protocol localhost block still present
  has "PROTO-LOCAL" "UrlParameters bloquea localhost" \
    "$CA/afirma-core/src/main/java/es/gob/afirma/core/misc/protocol/UrlParameters.java" \
    'LocalAccessRequestException'
fi

# SEC-010: remediado — flag --require-package
has "SEC-010" "mvp.sh ofrece --require-package" \
  "scripts/mvp.sh" 'require-package'

# SEC-011: SpongyCastle still zero locally (gate)
if [[ -d "$CA" ]]; then
  sc=$( { rg -l 'org\.spongycastle' -g '*.java' "$CA" || true; } | wc -l | tr -d ' ')
  eq "SEC-011-STATE" "clone local sin SpongyCastle (estado actual)" "$sc" "0"
fi

# Index / suite pointer optional
need "INDEX-ESTADO" "ESTADO-FASES enlaza R2" "$(grep -c 'CODE-REVIEW-360-R2' docs/ESTADO-FASES.md || true)"
need "INDEX-REMED" "ESTADO-FASES enlaza remediación" "$(grep -c 'REMEDIATION-360' docs/ESTADO-FASES.md || true)"

mkdir -p "$EVIDENCE_DIR"
{
  echo "Autofirma-2026 CODE-REVIEW-360-R2 verification evidence"
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
} | tee "$EVIDENCE"

echo
echo "Resultado R2: pass=$pass fail=$fail total=$((pass + fail))"
if (( fail > 0 )); then
  echo "VERIFICACIÓN R2 FALLIDA" >&2
  exit 1
fi
echo "VERIFICACIÓN R2 OK"
exit 0
