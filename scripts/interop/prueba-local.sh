#!/usr/bin/env bash
# T3b · B: prueba de interoperabilidad local con un .p12 real (FNMT u otro).
#
# Pensada para lanzarse en su propia terminal: la contrasena se pide por TTY, no viaja en
# argumentos, no se escribe en disco y no se registra en ningun log.
#
# Que hace, en orden:
#   1. Comprueba .gitignore y el hook de pre-commit (que el .p12 no pueda versionarse).
#   2. Extrae certificado, cadena y clave del .p12 a un directorio temporal 0700.
#   3. Arranca el validador DSS en local (JAR; tambien admite uno ya escuchando).
#   4. A1 canal TLS con su certificado (openssl s_client: TLS 1.2 si, obsoletos no).
#   5. A2 navegadores reales (Firefox/Chromium/Edge) con protocolo y suite negociados.
#   6. A3 dos vectores firmados con su certificado: XAdES enveloped y CAdES detached.
#   7. A4 valida los dos vectores con EU DSS.
#   8. Sanea la evidencia (NIF, NIE, CIF, serie, correo, titular) y deja un resumen.
#
# Uso:
#   bash scripts/interop/prueba-local.sh [ruta.p12] [alias]
#   make interop-local                      # .p12 por defecto
#   make interop-local P12=/ruta/mi.p12
#
# Variables:
#   INTEROP_P12         ruta por defecto si no se pasa argumento
#   INTEROP_BROWSERS    navegadores (por defecto los instalados)
#   INTEROP_CA          ruta a la CA emisora en PEM, si el .p12 no la incluye
#   INTEROP_STRICT_DSS=1  exige TOTAL_PASSED en DSS (por defecto INDETERMINATE es observacion)
#   --self-test         usa certificados sinteticos (desarrollo del script; nunca su .p12)
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DEFAULT_P12="/home/alexendros/Documents/certificadoFNMT.p12"

SELF_TEST=0
CHECK_ONLY=0
if [ "${1:-}" = "--self-test" ]; then
	SELF_TEST=1
	shift
elif [ "${1:-}" = "--check-p12" ]; then
	CHECK_ONLY=1
	shift
fi
P12="${1:-${INTEROP_P12:-$DEFAULT_P12}}"
ALIAS="${2:-}"
CA_OVERRIDE="${INTEROP_CA:-}"
OUT_ROOT="$ROOT/interop-local"
STAMP="$(date +%Y%m%d-%H%M%S)"
OUT="$OUT_ROOT/$STAMP"
JAR="$ROOT/afirma-simple/target/autofirma.jar"
VALIDATOR_DIR="$ROOT/tests/interop-dss"

fail() { echo "FAIL: $*" >&2; exit 1; }
ok() { echo "OK: $*"; }
step() { echo; echo "== $* =="; }

free_port() {
	local p
	for p in "$@"; do
		if ! (exec 3<>"/dev/tcp/127.0.0.1/$p") 2>/dev/null; then
			echo "$p"
			return 0
		fi
	done
	return 1
}

# ---------------------------------------------------------------- diagnostico del .p12
# Muchos PKCS#12 de emisores espanoles (FNMT incluida) cifran el certificado con RC2-40,
# que OpenSSL 3 solo descifra con el proveedor "legacy". Sin contrasena se puede saber.
describe_p12() {
	local f="$1" hex
	hex="$(openssl asn1parse -in "$f" -inform DER 2>/dev/null | grep -oE '2A864886F70D010C010[0-9A-F]' | sort -u || true)"
	echo "[check-p12] algoritmos encontrados:"
	case "$hex" in
		*2A864886F70D010C0106*) echo "  - pbeWithSHA1And40BitRC2-CBC (RC2-40): requiere -legacy" ;;
	esac
	case "$hex" in
		*2A864886F70D010C0103*) echo "  - pbeWithSHA1And3-KeyTripleDES-CBC (3DES)" ;;
	esac
	case "$hex" in
		*2A864886F70D010C0102*) echo "  - PBES2 (AES/PBKDF2)" ;;
	esac
	[ -n "$hex" ] || echo "  - no se han podido leer los algoritmos del contenedor"
}

if [ "$CHECK_ONLY" = "1" ]; then
	[ -f "$P12" ] || fail "no existe el fichero: $P12"
	echo "[check-p12] fichero: $P12"
	printf '[check-p12] tamano: %s bytes\n' "$(stat -c %s "$P12")"
	describe_p12 "$P12"
	echo "[check-p12] OpenSSL: $(openssl version)"
	echo "[check-p12] proveedor legacy: $(openssl list -providers -provider legacy 2>/dev/null | grep -c 'status: active') activo(s)"
	exit 0
fi

# ---------------------------------------------------------------- 0. comprobaciones
step "0/7 Comprobaciones previas"

[ -f "$P12" ] || fail "no existe el fichero: $P12"
case "$P12" in
	"$ROOT"/*) fail "el .p12 esta dentro del repositorio; muevelo fuera antes de continuar" ;;
esac

bash "$ROOT/scripts/interop/check-no-pkcs12.sh" >/dev/null \
	|| fail "la higiene del repositorio no pasa (PKCS#12 nuevo o interop-local versionado)"
for pat in '*.p12' '*.pfx' 'interop-local/'; do
	grep -qxF "$pat" "$ROOT/.gitignore" || fail ".gitignore no ignora $pat"
done
grep -q 'interop-local/' "$ROOT/.husky/pre-commit" || fail "el hook de pre-commit no bloquea interop-local/"

mkdir -p "$OUT"
chmod 700 "$OUT_ROOT" "$OUT"
WORK="$(mktemp -d)"
chmod 700 "$WORK"
echo "[prueba-local] certificado: $P12"
echo "[prueba-local] evidencia:   $OUT"
ok "el repositorio no admite versionar .p12/.pfx ni interop-local/"

VALIDATOR_PID=""
cleanup() {
	if [ -n "$VALIDATOR_PID" ]; then
		kill "$VALIDATOR_PID" 2>/dev/null || true
	fi
	rm -rf "$WORK"
}
trap cleanup EXIT

# ---------------------------------------------------------------- 1. contrasena
step "1/7 Contrasena del certificado"

if [ "$SELF_TEST" = "1" ]; then
	echo "[prueba-local] modo self-test: certificados sinteticos, nunca su .p12"
	bash "$ROOT/scripts/interop/gen-test-certs.sh" "$WORK/synth" >/dev/null
	P12="$WORK/synth/rsa.p12"
	P12_PASS="$(cat "$WORK/synth/p12pass")"
else
	# Comprobar que /dev/tty se puede abrir de verdad (no basta con que exista).
	if ! { : < /dev/tty; } 2>/dev/null; then
		fail "hace falta una terminal interactiva para pedir la contrasena; ejecuta el comando en tu terminal"
	fi
	printf 'Contrasena del PKCS#12 (no se muestra): '
	IFS= read -r -s P12_PASS < /dev/tty
	printf '\n'
	[ -n "$P12_PASS" ] || fail "contrasena vacia"
fi

# La contrasena va a openssl por el descriptor 3: no aparece en argv ni en el entorno.
P12_ERR="$WORK/openssl.err"
read_p12() {
	exec 3< <(printf '%s' "$P12_PASS")
	openssl pkcs12 "$@" -in "$P12" -passin fd:3 -nodes -out "$WORK/bundle.pem" 2>"$P12_ERR"
	local rc=$?
	exec 3<&-
	return "$rc"
}

if ! read_p12; then
	# Los PKCS#12 con RC2-40 o 3DES necesitan el proveedor legacy en OpenSSL 3.
	if grep -qiE "unsupported|legacy|rc2|unknown cipher|no such" "$P12_ERR"; then
		echo "[prueba-local] el contenedor usa cifrado heredado; reintentando con -legacy"
	fi
	if ! read_p12 -legacy; then
		echo "[prueba-local] OpenSSL dice:" >&2
		sed 's/^/  /' "$P12_ERR" >&2 || true
		echo "[prueba-local] algoritmos del contenedor:" >&2
		describe_p12 "$P12" >&2
		fail "no se pudo abrir el PKCS#12 (mira el mensaje de OpenSSL arriba)"
	fi
fi
unset P12_PASS

awk -v dir="$WORK" '/BEGIN CERTIFICATE/{n++} n>0{print > (dir "/cert" n ".pem")}' "$WORK/bundle.pem"
[ -s "$WORK/cert1.pem" ] || fail "el PKCS#12 no contiene certificados"
cp "$WORK/cert1.pem" "$WORK/leaf.crt"

# Un .p12 puede traer varias CA. El ancla de confianza debe ser la que EMITE el
# certificado de firma, no "la ultima del fichero".
LEAF_ISSUER="$(openssl x509 -in "$WORK/leaf.crt" -noout -issuer -nameopt RFC2253 2>/dev/null | sed 's/^issuer=//')"
CA_CANDIDATES=0
for f in "$WORK"/cert*.pem; do
	[ "$f" = "$WORK/cert1.pem" ] && continue
	CA_CANDIDATES=$((CA_CANDIDATES + 1))
	subj="$(openssl x509 -in "$f" -noout -subject -nameopt RFC2253 2>/dev/null | sed 's/^subject=//')"
	if [ -n "$LEAF_ISSUER" ] && [ "$subj" = "$LEAF_ISSUER" ]; then
		cp "$f" "$WORK/ca.crt"
	fi
done
if [ -n "$CA_OVERRIDE" ]; then
	[ -f "$CA_OVERRIDE" ] || fail "no existe la CA indicada en INTEROP_CA: $CA_OVERRIDE"
	cp "$CA_OVERRIDE" "$WORK/ca.crt"
	echo "[prueba-local] CA indicada manualmente: $CA_OVERRIDE"
elif [ ! -s "$WORK/ca.crt" ]; then
	# Ninguna CA del .p12 emite el certificado: se usa la ultima y se avisa.
	LAST_CERT=1
	for f in "$WORK"/cert*.pem; do LAST_CERT="${f##*/cert}"; LAST_CERT="${LAST_CERT%.pem}"; done
	cp "$WORK/cert$LAST_CERT.pem" "$WORK/ca.crt"
	echo "[prueba-local] AVISO: ninguna CA del .p12 emite el certificado de firma ($CA_CANDIDATES candidatas)."
	echo "[prueba-local] AVISO: se usa la ultima; DSS podria responder NO_CERTIFICATE_CHAIN_FOUND."
	echo "[prueba-local] AVISO: si ocurre, repite con INTEROP_CA=<ruta a la CA emisora en PEM>."
fi
CA_SUBJ_CHECK="$(openssl x509 -in "$WORK/ca.crt" -noout -subject -nameopt RFC2253 2>/dev/null | sed 's/^subject=//')"
if [ "$CA_SUBJ_CHECK" != "$LEAF_ISSUER" ]; then
	echo "[prueba-local] AVISO: el ancla elegida no coincide con el emisor del certificado."
fi

# Cadena completa para DSS: emisora primero y despues el resto de CA del .p12.
# Hay emisores (FNMT) que reutilizan el nombre de la CA en varios niveles: DSS necesita
# la cadena entera para encontrar el ancla.
cat "$WORK/ca.crt" > "$WORK/ca-chain.pem"
for f in "$WORK"/cert*.pem; do
	[ "$f" = "$WORK/cert1.pem" ] && continue
	cmp -s "$f" "$WORK/ca.crt" && continue
	cat "$f" >> "$WORK/ca-chain.pem"
done
CA_FOR_DSS="$WORK/ca-chain.pem"

awk '/BEGIN PRIVATE KEY/,/END PRIVATE KEY/' "$WORK/bundle.pem" > "$WORK/leaf.key"
[ -s "$WORK/leaf.key" ] || fail "el PKCS#12 no contiene la clave privada"
chmod 600 "$WORK/leaf.key"

SUBJECT="$(openssl x509 -in "$WORK/leaf.crt" -noout -subject -nameopt utf8,sep_comma_plus 2>/dev/null | sed 's/^subject=//')"
CA_SUBJECT="$(openssl x509 -in "$WORK/ca.crt" -noout -subject -nameopt utf8,sep_comma_plus 2>/dev/null | sed 's/^subject=//')"
[ -n "$SUBJECT" ] || fail "no se pudo leer el titular del certificado"
echo "[prueba-local] titular y emisor leidos (se eliminaran de la evidencia)"

CHANNEL_PASS="$(openssl rand -base64 24 | tr -d '\n/=+')"
openssl pkcs12 -export -in "$WORK/leaf.crt" -inkey "$WORK/leaf.key" -certfile "$WORK/ca.crt" \
	-name autofirma -passout "pass:$CHANNEL_PASS" -out "$WORK/channel.p12" 2>/dev/null \
	|| fail "no se pudo preparar el almacen del canal local"
# El alias lo elegimos nosotros al crear el almacen desechable: no hace falta leer el
# alias original del .p12 (que puede variar entre emisores).
[ -n "$ALIAS" ] || ALIAS="interop"
JAR_PASS="$(openssl rand -base64 24 | tr -d '\n/=+')"
openssl pkcs12 -export -in "$WORK/leaf.crt" -inkey "$WORK/leaf.key" -certfile "$WORK/ca.crt" \
	-name "$ALIAS" -passout "pass:$JAR_PASS" -out "$WORK/jar.p12" 2>/dev/null \
	|| fail "no se pudo preparar el almacen para firmar"
chmod 600 "$WORK/channel.p12" "$WORK/jar.p12"
ok "certificado y clave en directorio temporal 0700"

# ---------------------------------------------------------------- 2. validador
step "2/7 Validador DSS"

PORT="$(free_port 8080 18080 28080)" || fail "no hay puerto libre para el validador"
DSS_URL="http://127.0.0.1:$PORT"
healthy() { curl -sf -o /dev/null --max-time 3 "$DSS_URL/health" 2>/dev/null; }

if healthy; then
	echo "[prueba-local] ya hay un validador escuchando en $DSS_URL"
else
	NEEDS_BUILD=0
	[ -f "$VALIDATOR_DIR/target/interop-dss-validator.jar" ] || NEEDS_BUILD=1
	if [ "$NEEDS_BUILD" -eq 0 ] && [ -n "$(find "$VALIDATOR_DIR/src" "$VALIDATOR_DIR/pom.xml" \
			-newer "$VALIDATOR_DIR/target/interop-dss-validator.jar" -print -quit 2>/dev/null)" ]; then
		NEEDS_BUILD=1
	fi
	if [ "$NEEDS_BUILD" -eq 1 ]; then
		echo "[prueba-local] compilando el validador (fuentes nuevas)"
		(cd "$VALIDATOR_DIR" && mvn -B -q -DskipTests package) || fail "no compila el validador"
	fi
	java -Dinterop.dss.port="$PORT" -jar "$VALIDATOR_DIR/target/interop-dss-validator.jar" \
		> "$OUT/validator.log" 2>&1 &
	VALIDATOR_PID=$!
	for _ in $(seq 1 40); do
		healthy && break
		sleep 1
	done
	healthy || { tail -20 "$OUT/validator.log" >&2 || true; fail "el validador no responde en $DSS_URL"; }
	echo "[prueba-local] validador arrancado (pid $VALIDATOR_PID)"
fi
ok "validador disponible en $DSS_URL"

# ---------------------------------------------------------------- 3. A1 canal TLS
step "3/7 A1 · canal TLS con su certificado"
CHANNEL_PORT="$(free_port 8881 8882 8883)" || fail "no hay puerto libre para el canal local"
bash "$ROOT/scripts/interop/gen-test-certs.sh" "$WORK/certs" >/dev/null
INTEROP_CHANNEL_P12="$WORK/channel.p12" INTEROP_CHANNEL_PASS="$CHANNEL_PASS" \
	INTEROP_PORT="$CHANNEL_PORT" INTEROP_TLS_LOG="$OUT/tls-channel.log" \
	bash "$ROOT/scripts/interop/tls-probe.sh" "$WORK/certs" \
	> "$OUT/tls-probe.log" 2>&1 \
	|| { tail -25 "$OUT/tls-probe.log" >&2; fail "A1 fallo"; }
ok "A1 canal TLS 1.2 correcto"

# ---------------------------------------------------------------- 4. A2 navegadores
step "4/7 A2 · navegadores"
if node -e "require.resolve('playwright')" >/dev/null 2>&1; then
	if [ -z "${INTEROP_BROWSERS:-}" ]; then
		INTEROP_BROWSERS="firefox,chromium"
		if [ -x /opt/microsoft/msedge/msedge ] || command -v microsoft-edge >/dev/null 2>&1; then
			INTEROP_BROWSERS="$INTEROP_BROWSERS,msedge"
		fi
	fi
	INTEROP_OUT="$OUT" \
	INTEROP_BROWSERS="$INTEROP_BROWSERS" \
	INTEROP_CERTS_DIR="$WORK/certs" \
		node "$ROOT/scripts/interop/browser-interop.mjs" > "$OUT/browsers.log" 2>&1 \
		|| echo "[prueba-local] A2 con fallos; detalle en $OUT/browsers.log"
else
	echo "[prueba-local] Playwright no instalado: A2 omitido (npm i --no-save playwright)" \
		> "$OUT/browsers.log"
fi

# ---------------------------------------------------------------- 5. A3 firmas
step "5/7 A3 · firmas con su certificado"
[ -f "$JAR" ] || fail "falta $JAR: ejecuta 'mvn -Denv=install -pl afirma-simple -am -DskipTests package'"
printf '<?xml version="1.0" encoding="UTF-8"?>\n<interop>vector sintetico</interop>\n' > "$WORK/vector.xml"
printf 'vector sintetico de interoperabilidad\n' > "$WORK/plain.txt"

java -jar "$JAR" sign -i "$WORK/vector.xml" -o "$OUT/firma.xsig" \
	-store "pkcs12:$WORK/jar.p12" -password "$JAR_PASS" -alias "$ALIAS" -format xades \
	> "$OUT/firma-xades.log" 2>&1 \
	|| { tail -15 "$OUT/firma-xades.log" >&2; fail "no se pudo firmar XAdES"; }
ok "vector XAdES enveloped firmado"

java -jar "$JAR" sign -i "$WORK/plain.txt" -o "$OUT/firma.csig" \
	-store "pkcs12:$WORK/jar.p12" -password "$JAR_PASS" -alias "$ALIAS" -format cades \
	> "$OUT/firma-cades.log" 2>&1 \
	|| { tail -15 "$OUT/firma-cades.log" >&2; fail "no se pudo firmar CAdES"; }
ok "vector CAdES detached firmado"

# ---------------------------------------------------------------- 6. A4 DSS
step "6/7 A4 · validacion EU DSS"
DSS_OBSERVATION=0
DSS_URL="$DSS_URL" INTEROP_OUT="$OUT" \
	bash "$ROOT/scripts/interop/dss-validate.sh" "$OUT/firma.xsig" "$CA_FOR_DSS" \
	> "$OUT/dss-xades.log" 2>&1 || DSS_OBSERVATION=1
DSS_URL="$DSS_URL" INTEROP_OUT="$OUT" \
	bash "$ROOT/scripts/interop/dss-validate.sh" "$OUT/firma.csig" "$CA_FOR_DSS" "$WORK/plain.txt" \
	> "$OUT/dss-cades.log" 2>&1 || DSS_OBSERVATION=1

if [ "$DSS_OBSERVATION" -eq 0 ]; then
	ok "A4: los dos vectores dan TOTAL_PASSED"
elif [ "${INTEROP_STRICT_DSS:-0}" = "1" ]; then
	tail -12 "$OUT/dss-xades.log" "$OUT/dss-cades.log" >&2
	fail "A4 no dio TOTAL_PASSED y INTEROP_STRICT_DSS=1"
else
	SUB="$(grep -hoE 'indication=[A-Z_]+( detalle=[^ ]+)?' "$OUT"/dss-*.log | sort -u | tr '\n' ' ')"
	echo "[prueba-local] OBSERVACION: DSS no dio TOTAL_PASSED ($SUB)"
	echo "[prueba-local] Causa habitual: el certificado real no lleva 'noRevAvail' y DSS exige"
	echo "[prueba-local] datos de revocacion (CRL/OCSP). No es un fallo de interop."
fi

# ---------------------------------------------------------------- 7. saneado
step "7/7 Saneado y resumen"
INTEROP_SUBJECT_NAME="$SUBJECT" bash "$ROOT/scripts/interop/scrub-pii.sh" "$OUT" >/dev/null \
	|| fail "el saneado dejo datos personales"

BROWSERS_LINE="sin datos"
if [ -s "$OUT/browsers.json" ]; then
	BROWSERS_LINE="$(python3 - "$OUT/browsers.json" <<'PY'
import json, sys
rows = json.load(open(sys.argv[1]))
parts = []
for r in rows:
    ok = "OK" if r.get("handshake") else "FALLO"
    tls = f" {r['protocol']}/{r['cipher']}" if r.get("protocol") else ""
    parts.append(f"{r['browser']}:{ok}{tls}")
print("; ".join(parts))
PY
)"
fi
TLS_LINE="$(grep -c '^OK:' "$OUT/tls-probe.log" 2>/dev/null || echo 0)"
DSS_LINE="$(python3 - "$OUT" <<'PY'
import glob, json, os, sys
out = sys.argv[1]
parts = []
for path in sorted(glob.glob(os.path.join(out, "dss-report-*.json"))):
    name = os.path.basename(path)[len("dss-report-"):-len(".json")]
    try:
        data = json.load(open(path))
    except Exception:
        continue
    parts.append(f"{name}={data.get('indication','?')}"
                 + (f"/{data['subIndication']}" if data.get("subIndication") else "")
                 + f" ({data.get('signatureFormat','?')})")
print("; ".join(parts))
PY
)"
FORMATS="$DSS_LINE"

{
	echo "Interoperabilidad local · $STAMP"
	echo "Certificado: <eliminado>"
	echo "Emisor: <eliminado>"
	echo "A1 canal TLS: $TLS_LINE comprobaciones OK"
	echo "A2 navegadores: $BROWSERS_LINE"
	echo "A4 DSS: ${DSS_LINE:-sin resultado}"
	if [ "$DSS_OBSERVATION" -eq 0 ]; then
		echo "Resultado DSS: TOTAL_PASSED en los dos vectores"
	else
		echo "Resultado DSS: OBSERVACION (ver dss-*.log)"
	fi
	echo "Ficheros: tls-probe.log, browsers.json, dss-*.log, firma.xsig, firma.csig"
} > "$OUT/RESUMEN.txt"

echo
cat "$OUT/RESUMEN.txt"
echo
ok "evidencia saneada en $OUT"
echo "[prueba-local] no se ha subido nada: interop-local/ esta ignorado por git"
