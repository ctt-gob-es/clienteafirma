#!/usr/bin/env bash
# Verificación matemática de la revisión 360º: cada hallazgo del informe debe
# reproducirse con contadores > 0 (o igualdad documental). Fallo = informe no anclado.
# Uso: bash scripts/verify-code-review-360.sh
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

REPORT="docs/CODE-REVIEW-360.md"
SUITE=".archify/20260930-2117-code-review-360"
EVIDENCE_DIR="$ROOT/dist"
EVIDENCE="$EVIDENCE_DIR/CODE-REVIEW-360-EVIDENCE.txt"

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

echo "==> verify-code-review-360"
[[ -f "$REPORT" ]] || { echo "Falta $REPORT" >&2; exit 1; }
ok "DOC-001" "informe presente"

# --- Artefactos Archify (5/5 gates pass) ---
html_n=$(find "$SUITE" -type f -name '*.html' ! -path '*/visual-check/*' ! -path '*/review-2/*' | wc -l | tr -d ' ')
need "ARCH-001" "HTML Archify de la suite" "$html_n"
eq "ARCH-002" "suite declara 5 tipos" \
  "$(node -e "const j=require('./$SUITE/SUITE-SUMMARY.json'); console.log(Object.keys(j.diagrams).length)")" \
  "5"

for stem in architecture/revision-capas workflow/flujo-auditoria sequence/secuencia-afirma dataflow/flujo-confianza lifecycle/ciclo-fases; do
  sum="$SUITE/${stem}.finalize-summary.json"
  [[ -f "$sum" ]] || { ko "ARCH-GATE" "falta $sum"; continue; }
  status=$(node -e "const j=require('./$sum'); process.stdout.write(String(j.status||j.ok||''))")
  if [[ "$status" == "pass" || "$status" == "true" ]]; then
    ok "ARCH-GATE-${stem%%/*}" "finalize status=$status"
  else
    ko "ARCH-GATE-${stem%%/*}" "finalize status=$status"
  fi
done

# --- Meta: clean code / supply chain ---
# Contadores: siempre sumar líneas (grep -c multiarchivo imprime por fichero).
count_matches() {
  # usage: count_matches PATTERN PATH...
  local pat="$1"; shift
  { grep -E "$pat" "$@" 2>/dev/null || true; } | wc -l | tr -d ' '
}
count_files_with() {
  local pat="$1"; shift
  { grep -lE "$pat" "$@" 2>/dev/null || true; } | wc -l | tr -d ' '
}

scripts_n=$(find scripts -name '*.sh' | wc -l | tr -d ' ')
need "META-001" "scripts/*.sh existen" "$scripts_n"
pipefail_n=$(count_files_with 'set -euo pipefail' scripts/*.sh)
eq "META-002" "todos los scripts tienen set -euo pipefail" "$pipefail_n" "$scripts_n"

sha_pins=$(count_matches 'uses: actions/[a-z-]+@[0-9a-f]{40}' .github/workflows/*.yml)
need "META-003" "Actions pinneadas por SHA-40" "$sha_pins"

perm_n=$(count_matches '^permissions:' .github/workflows/*.yml)
need "META-004" "workflows declaran permissions" "$perm_n"

# SEC-004: Renovate operativo; CHANGELOG histórico menciona Dependabot; sin dependabot.yml
dep_changelog=$(grep -ci 'Dependabot' CHANGELOG.md || true)
need "SEC-004a" "CHANGELOG menciona Dependabot (histórico)" "$dep_changelog"
ren_changelog=$(grep -ci 'Renovate' CHANGELOG.md || true)
need "SEC-004a2" "CHANGELOG menciona Renovate (operativo)" "$ren_changelog"
[[ -f .github/renovate.json ]] && ok "SEC-004b" "existe .github/renovate.json" || ko "SEC-004b" "falta renovate.json"
if [[ -f .github/dependabot.yml ]]; then
  ko "SEC-004c" "dependabot.yml no debería existir (Renovate es el gestor)"
else
  ok "SEC-004c" "dependabot.yml ausente (Renovate activo)"
fi
automerge=$(grep -c 'automerge' .github/renovate.json || true)
need "SEC-004d" "Renovate declara automerge" "$automerge"

# SEC-003: branch protection pendiente documentada
bp=$(grep -ci 'Branch protection' docs/REPO-ENDING-AUDIT.md || true)
need "SEC-003" "branch protection documentada como pendiente" "$bp"

# SEC-002: wrapper .deb portable (remediado — gate de mitigación)
has_f6=$(grep -c 'usr/bin/autofirma' scripts/f6-package-linux.sh || true)
need "SEC-002a" "f6-package genera wrapper autofirma" "$has_f6"
portable=$(grep -cE 'JAVA_BIN="java"|/usr/bin/java' scripts/f6-package-linux.sh || true)
need "SEC-002b" "wrapper portable (java PATH /usr/bin/java)" "$portable"
abs_legacy=$(grep -cE 'JAVA_BIN="\$ROOT/tools' scripts/f6-package-linux.sh || true)
eq "SEC-002c" "sin embeber \$ROOT/tools en JAVA_BIN" "$abs_legacy" "0"

# --- Cliente (clone local; si falta, fallan asertos de producto) ---
CA="."
if [[ ! -d "$CA" ]]; then
  ko "CLIENT-000" "falta árbol del cliente (pom.xml / afirma-*) — no se pueden verificar SEC-001/005"
else
  dummy=$( { rg -c 'DUMMY_TRUST_MANAGER' -g '*.java' "$CA" || true; } | awk -F: '{s+=$2} END{print s+0}' )
  need "SEC-001a" "DUMMY_TRUST_MANAGER en código Java" "$dummy"
  disable=$( { rg -c 'disableSslChecks' -g '*.java' "$CA" || true; } | awk -F: '{s+=$2} END{print s+0}' )
  need "SEC-001b" "disableSslChecks referenciado" "$disable"
  hv=$( { rg -c 'DUMMY_HOSTNAME_VERIFIER' -g '*.java' "$CA" || true; } | awk -F: '{s+=$2} END{print s+0}' )
  need "SEC-001c" "DUMMY_HOSTNAME_VERIFIER presente" "$hv"

  # SEC-005 iText (propiedad o artifactId en POMs)
  itext=$( { rg -c 'afirma-lib-itext|afirma\.lib\.itext' -g 'pom.xml' "$CA" || true; } | awk -F: '{s+=$2} END{print s+0}' )
  need "SEC-005" "POMs declaran afirma-lib-itext / propiedad" "$itext"

  # Contraste BC vs SC en clone local
  bc_files=$( { rg -l 'org\.bouncycastle' -g '*.java' "$CA" || true; } | wc -l | tr -d ' ' )
  sc_files=$( { rg -l 'org\.spongycastle' -g '*.java' "$CA" || true; } | wc -l | tr -d ' ' )
  need "CRYPTO-BC" "ficheros Java con org.bouncycastle" "$bc_files"
  eq "CRYPTO-SC" "ficheros Java con org.spongycastle (esperado 0 en fork BC)" "$sc_files" "0"

  # Controles positivos
  secure_xml=$( { rg -l 'SecureXmlBuilder' -g '*.java' "$CA" || true; } | wc -l | tr -d ' ' )
  need "CTRL-001" "SecureXmlBuilder existe" "$secure_xml"
  proto=$( { rg -l 'ProtocolInvocationLauncher' -g '*.java' "$CA/afirma-simple" || true; } | wc -l | tr -d ' ' )
  need "CTRL-002" "ProtocolInvocationLauncher presente" "$proto"
  local_block=$( { rg -c 'localhost|127\.0\.0\.1' -g '*UrlParameters.java' "$CA" || true; } | awk -F: '{s+=$2} END{print s+0}' )
  need "CTRL-003" "UrlParameters menciona localhost/127.0.0.1" "$local_block"
fi

# SEC-006 frontera Integr@
f9=$(grep -ci 'iText' docs/F9-INTEGRA-FIRE.md || true)
need "SEC-006" "F9 documenta bloqueo iText" "$f9"

# Informe lista IDs de hallazgos
for id in SEC-001 SEC-002 SEC-003 SEC-004 SEC-005 SEC-006; do
  n=$(grep -c "$id" "$REPORT" || true)
  need "REP-$id" "informe menciona $id" "$n"
done

# Baseline pin SHA-40
base_sha=$(grep -E '^BASELINE_COMMIT=[0-9a-f]{40}$' docs/BASELINE.txt | wc -l | tr -d ' ')
eq "BASE-001" "BASELINE_COMMIT es SHA-40" "$base_sha" "1"

mkdir -p "$EVIDENCE_DIR"
{
  echo "Autofirma-2026 CODE-REVIEW-360 verification evidence"
  echo "date=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "meta_rev=$(git rev-parse HEAD)"
  echo "report=$REPORT"
  echo "suite=$SUITE"
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
echo "Resultado: pass=$pass fail=$fail total=$((pass + fail))"
if (( fail > 0 )); then
  echo "VERIFICACIÓN FALLIDA: el informe no está anclado a evidencia reproducible." >&2
  exit 1
fi
echo "VERIFICACIÓN OK: hallazgos y controles confirmados matemáticamente."
exit 0
