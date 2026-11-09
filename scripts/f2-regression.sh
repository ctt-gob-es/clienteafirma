#!/usr/bin/env bash
# F2 — No-regresión de formatos (CAdES / XAdES / PAdES / FacturaE / cofirma / contrafirma)
# Genera firmas con autofirma.jar de la línea 1.9.1 y comprueba que las operaciones terminan OK.
# VALIDe queda como ensayo manual periódico (docs/VALIDE_MANUAL.md).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
export JAVA_HOME="${JAVA_HOME:-$ROOT/tools/jdk8}"
export PATH="$JAVA_HOME/bin:${PATH:-}"
unset _JAVA_OPTIONS || true

JAR="${AUTOFIRMA_JAR:-$ROOT/afirma-simple/target/autofirma.jar}"
RES="$ROOT/afirma-simple/src/test/resources"
VECT="$ROOT/vectors"
P12="$RES/ANF_PF_Activo.pfx"
PWD_STORE=12341234
ALIAS="anf usuario activo"

if [[ ! -f "$JAR" ]]; then
  echo "ERROR: falta $JAR — ejecuta F1 (mvn -Denv=install) antes." >&2
  exit 1
fi

mkdir -p "$VECT/input" "$VECT/out" "$VECT/manifest"
echo "Hello Autofirma-2026 regression vector" > "$VECT/input/plain.txt"
cp -f "$RES/samples/2.xml" "$VECT/input/sample.xml"
cp -f "$RES/facturae_32v1.xml" "$VECT/input/facturae.xml"
PDF_SRC=$(find "$ROOT/afirma-simple" -path "*/samples/2.pdf" 2>/dev/null | head -1)
[[ -n "$PDF_SRC" ]] && cp -f "$PDF_SRC" "$VECT/input/sample.pdf"

sign_ok() {
  local format="$1" in="$2" out="$3"
  echo "=== SIGN $format ==="
  local log
  log=$(java -jar "$JAR" sign -i "$in" -o "$out" \
    -store "pkcs12:$P12" -password "$PWD_STORE" -alias "$ALIAS" \
    -format "$format" 2>&1) || { echo "$log"; return 1; }
  echo "$log" | grep -q "terminado correctamente" || { echo "$log"; return 1; }
  test -s "$out"
}

sign_ok cades "$VECT/input/plain.txt" "$VECT/out/plain-cades.csig"
sign_ok xades "$VECT/input/sample.xml" "$VECT/out/sample-xades.xsig"
sign_ok pades "$VECT/input/sample.pdf" "$VECT/out/sample-pades.pdf"
sign_ok facturae "$VECT/input/facturae.xml" "$VECT/out/facturae-signed.xml"

echo "=== COSIGN cades ==="
log=$(java -jar "$JAR" cosign -i "$VECT/out/plain-cades.csig" -o "$VECT/out/plain-cades-cosign.csig" \
  -store "pkcs12:$P12" -password "$PWD_STORE" -alias "$ALIAS" -format cades 2>&1)
echo "$log" | grep -q "terminado correctamente"
test -s "$VECT/out/plain-cades-cosign.csig"

echo "=== COUNTERSIGN cades ==="
log=$(java -jar "$JAR" countersign -i "$VECT/out/plain-cades.csig" -o "$VECT/out/plain-cades-countersign.csig" \
  -store "pkcs12:$P12" -password "$PWD_STORE" -alias "$ALIAS" -format cades 2>&1)
echo "$log" | grep -q "terminado correctamente"
test -s "$VECT/out/plain-cades-countersign.csig"

# Manifest: tamaños y SHA-256 (las firmas CMS/XAdES no son bit-idénticas entre runs por timestamp)
{
  echo "# F2 vector gate — $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "# baseline: $(cat "$ROOT/docs/BASELINE.txt" | tr '\n' ' ')"
  echo "# jar: $JAR"
  for f in plain-cades.csig sample-xades.xsig sample-pades.pdf facturae-signed.xml \
           plain-cades-cosign.csig plain-cades-countersign.csig; do
    path="$VECT/out/$f"
    sz=$(wc -c < "$path")
    sha=$(sha256sum "$path" | awk '{print $1}')
    echo "$f size=$sz sha256=$sha"
  done
} | tee "$VECT/manifest/latest.txt"

echo "F2 OK — formatos firmados y revalidados (operación correcta + fichero no vacío)."
