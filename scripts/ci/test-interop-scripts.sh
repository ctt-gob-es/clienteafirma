#!/usr/bin/env bash
# Test de los scripts de interoperabilidad (T3b): no depende de red ni de Docker.
# Comprueba generacion de certificados sinteticos, saneado de datos personales y que
# el repositorio no admite PKCS#12 versionados.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

fail() { echo "FAIL: $*" >&2; exit 1; }
ok() { echo "OK: $*"; }

# 1. Certificados sinteticos: CA + hoja RSA 3072 + hoja EC P-256 + dos PKCS#12.
bash "$ROOT/scripts/interop/gen-test-certs.sh" "$WORK/certs" >/dev/null
for f in ca.crt rsa.crt ec.crt rsa.p12 ec.p12; do
	[ -s "$WORK/certs/$f" ] || fail "falta $f en la generacion de certificados"
done
openssl verify -CAfile "$WORK/certs/ca.crt" "$WORK/certs/rsa.crt" "$WORK/certs/ec.crt" >/dev/null \
	|| fail "los certificados de prueba no validan contra su CA"
openssl x509 -in "$WORK/certs/rsa.crt" -noout -text | grep -q "3072 bit" \
	|| fail "la hoja RSA no es de 3072 bits"
openssl x509 -in "$WORK/certs/ec.crt" -noout -text | grep -q "prime256v1" \
	|| fail "la hoja EC no es P-256"
ok "certificados sinteticos (CA, RSA 3072, EC P-256, PKCS#12)"

# 2. Saneado: NIF, NIE, CIF, serie, correo y titular desaparecen; la evidencia tecnica queda.
mkdir -p "$WORK/out"
cat > "$WORK/out/report.txt" <<'TXT'
Titular: C=ES, CN=JUAN PEREZ GARCIA, O=Interop
NIF 12345678Z NIE X1234567L CIF B12345678 serie 00:11:22:33:44:55:66:77 correo juan@example.com
protocolo TLSv1.2 suite ECDHE-RSA-AES256-GCM-SHA384 resultado TOTAL_PASSED
TXT
INTEROP_SUBJECT_NAME="C=ES, CN=JUAN PEREZ GARCIA, O=Interop" \
	bash "$ROOT/scripts/interop/scrub-pii.sh" "$WORK/out" >/dev/null
if grep -Eq '[0-9]{8}[A-Za-z]|X1234567L|JUAN PEREZ|<vacio>' "$WORK/out/report.txt"; then
	fail "el saneado dejo datos personales"
fi
for keep in "TLSv1.2" "ECDHE-RSA-AES256-GCM-SHA384" "TOTAL_PASSED"; do
	grep -q "$keep" "$WORK/out/report.txt" || fail "el saneado elimino evidencia tecnica: $keep"
done
ok "saneado de datos personales sin perder evidencia"

# 3. Higiene: el repositorio no contiene PKCS#12 versionados ni interop-local/.
bash "$ROOT/scripts/interop/check-no-pkcs12.sh" >/dev/null \
	|| fail "hay PKCS#12 nuevos fuera de los almacenes de prueba heredados"
grep -q '^\*\.p12$' "$ROOT/.gitignore" || fail ".gitignore no ignora *.p12"
grep -q '^\*\.pfx$' "$ROOT/.gitignore" || fail ".gitignore no ignora *.pfx"
grep -q '^interop-local/$' "$ROOT/.gitignore" || fail ".gitignore no ignora interop-local/"
ok "higiene: sin PKCS#12 versionados y .gitignore correcto"

# 4. El canal local endurecido no admite protocolos obsoletos (estatico, sin arrancar nada).
SRC="$ROOT/afirma-simple/src/main/java/es/gob/afirma/standalone/protocol"
grep -q 'SSLCONTEXT = "TLSv1.2"' "$SRC/SecureSocketUtils.java" || fail "el contexto SSL no es TLSv1.2"
for legacy in TLSv1.0 TLSv1.1 SSLv3; do
	if grep -q "\"$legacy\"" "$SRC/SecureSocketUtils.java"; then
		fail "SecureSocketUtils habilita $legacy"
	fi
done
# Solo se miran las suites declaradas (literales TLS_...), no los comentarios.
suites="$(grep -oE '"TLS_[A-Z0-9_]+"' "$SRC/ServiceInvocationManager.java" | tr -d '"' || true)"
[ -n "$suites" ] || fail "no se han encontrado suites TLS declaradas"
if printf '%s\n' "$suites" | grep -qE '^TLS_RSA_WITH_|_CBC_SHA$'; then
	fail "quedan suites de RSA estatico o CBC-SHA1 en el canal local"
fi
count="$(printf '%s\n' "$suites" | wc -l | tr -d ' ')"
echo "[test] $count suites TLS declaradas en el canal local"
ok "configuracion TLS del canal local sin protocolos ni suites obsoletas"

# 5. Workflows de interoperabilidad: DSS por tag fijado y A4 en aviso mientras no haya imagen.
WF_IMAGE="$ROOT/.github/workflows/dss-image.yml"
WF_INTEROP="$ROOT/.github/workflows/interop.yml"
[ -f "$WF_IMAGE" ] || fail "falta .github/workflows/dss-image.yml"
grep -q "interop-dss-validator" "$WF_IMAGE" || fail "dss-image.yml no construye el validador propio"
grep -q "tests/interop-dss" "$WF_IMAGE" || fail "dss-image.yml no usa tests/interop-dss"
grep -q "trivy image" "$WF_IMAGE" || fail "dss-image.yml no escanea con Trivy"
grep -q -- "--ignore-unfixed" "$WF_IMAGE" || fail "el escaneo Trivy no ignora lo no corregible"
grep -q -- "-format xades" "$ROOT/.github/workflows/interop.yml" || fail "A3 no firma el vector XAdES"
grep -q -- "-format cades" "$ROOT/.github/workflows/interop.yml" || fail "A3 no firma el vector CAdES"
grep -q "firma.csig" "$ROOT/.github/workflows/interop.yml" || fail "A4 no valida la CAdES detached"
grep -q "attest-build-provenance" "$WF_IMAGE" || fail "dss-image.yml no attesta la procedencia"
grep -q "continue-on-error: \${{ vars.DSS_IMAGE == '' }}" "$WF_INTEROP" \
	|| fail "A4 no esta en modo aviso condicionado a DSS_IMAGE"
ok "workflows: DSS fijado por version, atestado y A4 en aviso hasta la primera imagen"

echo "test-interop-scripts: OK"
