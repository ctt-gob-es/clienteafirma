#!/usr/bin/env bash
# Genera una CA y certificados de prueba sinteticos para la interoperabilidad (T3b).
#
# Nunca se versionan: el directorio de salida queda fuera del repositorio y .gitignore
# cubre *.p12 y *.pfx. No se usa ningun dato personal: los sujetos son genericos.
#
# Uso: scripts/interop/gen-test-certs.sh [directorio-salida]
# Entorno: INTEROP_P12_PASS (si falta, se genera una aleatoria en <salida>/p12pass)
set -euo pipefail

OUT="${1:-${TMPDIR:-/tmp}/afirma-interop-certs}"
mkdir -p "$OUT"
chmod 700 "$OUT"

if [ -n "${INTEROP_P12_PASS:-}" ]; then
	PASS="$INTEROP_P12_PASS"
else
	PASS="$(openssl rand -base64 24 | tr -d '\n/=+')"
	printf '%s' "$PASS" > "$OUT/p12pass"
	chmod 600 "$OUT/p12pass"
fi

DAYS=2
SUBJ_BASE="/C=ES/O=Interop Test/OU=Autofirma CI"

echo "[certs] CA RSA 3072 en $OUT"
openssl req -x509 -newkey rsa:3072 -sha256 -days "$DAYS" -nodes \
	-keyout "$OUT/ca.key" -out "$OUT/ca.crt" \
	-subj "$SUBJ_BASE/CN=Interop Test CA" \
	-addext "basicConstraints=critical,CA:TRUE" \
	-addext "keyUsage=critical,keyCertSign,cRLSign" >/dev/null 2>&1

gen_leaf() {
	local name="$1"
	shift
	openssl req -new -nodes -keyout "$OUT/$name.key" -out "$OUT/$name.csr" \
		-subj "$SUBJ_BASE/CN=Interop Test Signer $name" "$@" >/dev/null 2>&1

	# noRevAvail (2.5.29.56, ETSI EN 319 412-5): permite que EU DSS valide sin CRL ni OCSP
	# manteniendo su politica por defecto (el certificado declara que no hay datos de revocacion).
	printf '%s\n' \
		"basicConstraints=critical,CA:FALSE" \
		"keyUsage=critical,digitalSignature,nonRepudiation" \
		"extendedKeyUsage=emailProtection,clientAuth,serverAuth" \
		"2.5.29.56=DER:05:00" > "$OUT/$name.ext"

	openssl x509 -req -in "$OUT/$name.csr" -CA "$OUT/ca.crt" -CAkey "$OUT/ca.key" \
		-CAcreateserial -days "$DAYS" -sha256 \
		-extfile "$OUT/$name.ext" -out "$OUT/$name.crt" >/dev/null 2>&1

	openssl pkcs12 -export -in "$OUT/$name.crt" -inkey "$OUT/$name.key" \
		-certfile "$OUT/ca.crt" -name "interop-$name" \
		-passout "pass:$PASS" -out "$OUT/$name.p12"

	chmod 600 "$OUT/$name.key" "$OUT/$name.p12"
	rm -f "$OUT/$name.csr" "$OUT/$name.ext"
	echo "[certs] $name: p12 generado"
}

# Hoja RSA 3072 y hoja EC P-256, como pide la OT.
gen_leaf "rsa" -newkey rsa:3072
gen_leaf "ec" -newkey ec -pkeyopt ec_paramgen_curve:P-256

cat "$OUT/ca.crt" > "$OUT/chain.pem"

echo "[certs] listo en $OUT"
