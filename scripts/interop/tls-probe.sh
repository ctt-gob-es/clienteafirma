#!/usr/bin/env bash
# T3b · A1: comprueba el endurecimiento TLS del canal local de Autofirma.
#
# Arranca el canal local (misma ruta de codigo que el producto: ServiceInvocationManager
# + SecureSocketUtils) y sondea con openssl s_client:
#   - TLS 1.2 con ECDHE debe conectar;
#   - TLS 1.0, TLS 1.1, RSA estatico y CBC-SHA1 deben fallar.
# Ademas comprueba T4: el producto genera el secreto local (0600) y migra el almacen
# heredado, que deja de abrir con la contrasena antigua.
#
# Requiere: openssl, java, maven y el PFX sintetico generado en ${CERTS_DIR}.
# Uso: scripts/interop/tls-probe.sh [directorio-certs]
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CERTS_DIR="${1:-${INTEROP_CERTS_DIR:-${TMPDIR:-/tmp}/afirma-interop-certs}}"
PORT="${INTEROP_PORT:-8881}"
MODULE="$ROOT/afirma-simple"
APP_DIR="$MODULE/target"
LOG="${INTEROP_TLS_LOG:-${TMPDIR:-/tmp}/interop-tls-probe.log}"

fail() { echo "FAIL: $*" >&2; exit 1; }
ok() { echo "OK: $*"; }

[ -f "$CERTS_DIR/rsa.p12" ] || fail "faltan los certificados de prueba en $CERTS_DIR (ejecuta gen-test-certs.sh)"
command -v openssl >/dev/null || fail "openssl no disponible"

# La contrasena del PFX del canal local ya no es una constante del codigo (T4): el producto
# genera un secreto local en el primer arranque (autofirma.pfx.secret, 0600) y migra el
# almacen heredado. Aqui se prepara un PFX con la contrasena heredada, sin fichero de
# secreto, para comprobar esa migracion en la ruta real del producto.
LEGACY_KS_PASS="${INTEROP_LEGACY_KS_PASS:-654321}"
SECRET_FILE="$APP_DIR/autofirma.pfx.secret"
rm -f "$SECRET_FILE"

echo "[tls-probe] compilando e instalando afirma-simple (sin tests)"
# Se instala en el repositorio local para que el classpath de ejecucion use los JAR
# recien construidos (el canal depende ahora de afirma-core-keystores).
(cd "$ROOT" && mvn -B -q -Denv=install -pl afirma-simple -am -DskipTests install) || fail "no compila afirma-simple"

# SecureSocketUtils busca autofirma.pfx en el directorio de la aplicacion
# (padre de target/classes) con la contrasena heredada que debe migrar el producto.
SOURCE_P12="${INTEROP_CHANNEL_P12:-$CERTS_DIR/rsa.p12}"
SOURCE_PASS="${INTEROP_CHANNEL_PASS:-$(cat "$CERTS_DIR/p12pass")}"
[ -f "$SOURCE_P12" ] || fail "no existe el PKCS#12 del canal: $SOURCE_P12"
openssl pkcs12 -in "$SOURCE_P12" -passin "pass:$SOURCE_PASS" -nodes \
	-out "$APP_DIR/interop-channel.pem" >/dev/null 2>&1 \
	|| fail "no se pudo leer el PKCS#12 del canal"
openssl pkcs12 -export -in "$APP_DIR/interop-channel.pem" \
	-passout "pass:$LEGACY_KS_PASS" -out "$APP_DIR/autofirma.pfx" 2>/dev/null \
	|| fail "no se pudo preparar autofirma.pfx"
rm -f "$APP_DIR/interop-channel.pem"
chmod 600 "$APP_DIR/autofirma.pfx"

CP_FILE="$APP_DIR/interop-classpath.txt"
(cd "$ROOT" && mvn -B -q -Denv=install -pl afirma-simple dependency:build-classpath \
	-Dmdep.outputFile="$CP_FILE" -Dmdep.includeScope=test) || fail "no se pudo resolver el classpath"

CP="$MODULE/target/test-classes:$MODULE/target/classes:$(cat "$CP_FILE")"

echo "[tls-probe] arrancando el canal local en el puerto $PORT"
java -cp "$CP" \
	-Dinterop.port="$PORT" -Dinterop.session=interop \
	-Djava.util.logging.config.file=/dev/null \
	es.gob.afirma.standalone.protocol.TlsProbeServer >"$LOG" 2>&1 &
PROBE_PID=$!
trap 'kill "$PROBE_PID" 2>/dev/null || true' EXIT

# Espera a que el puerto acepte conexiones (maximo 60 s).
READY=0
for _ in $(seq 1 60); do
	if openssl s_client -connect "127.0.0.1:$PORT" -tls1_2 -brief </dev/null >/dev/null 2>&1; then
		READY=1
		break
	fi
	sleep 1
done
[ "$READY" -eq 1 ] || { tail -20 "$LOG" >&2 || true; fail "el canal local no abrio el puerto $PORT"; }

probe_ok() {
	local desc="$1"; shift
	local out
	if out="$(timeout 20 openssl s_client -connect "127.0.0.1:$PORT" "$@" </dev/null 2>&1)"; then
		echo "$out" >&2
		ok "$desc conecta"
		return 0
	fi
	echo "$out" >&2
	fail "$desc deberia conectar y no lo hace"
}

probe_fail() {
	local desc="$1"; shift
	if timeout 20 openssl s_client -connect "127.0.0.1:$PORT" "$@" </dev/null >/dev/null 2>&1; then
		fail "$desc NO deberia conectar"
	fi
	ok "$desc rechazado"
}

echo "[tls-probe] ---- debe conectar ----"
probe_ok "TLS 1.2 ECDHE-RSA-AES256-GCM-SHA384" -tls1_2 -cipher 'ECDHE-RSA-AES256-GCM-SHA384'

echo "[tls-probe] ---- debe fallar ----"
probe_fail "TLS 1.0"                 -tls1   -cipher 'AES256-SHA'
probe_fail "TLS 1.1"                 -tls1_1 -cipher 'AES256-SHA'
probe_fail "RSA estatico (TLS 1.2)"  -tls1_2 -cipher 'AES256-GCM-SHA384'
probe_fail "CBC-SHA1 (TLS 1.2)"      -tls1_2 -cipher 'ECDHE-RSA-AES256-SHA'

echo "[tls-probe] OK: el canal local solo admite TLS 1.2 con suites modernas (log: $LOG)"

# T4 · el almacen heredado se migra a un secreto local con permisos 0600
echo "[tls-probe] ---- T4: secreto local del canal ----"
[ -f "$SECRET_FILE" ] || fail "el producto no genero el fichero de secreto local"
if perms="$(stat -c '%a' "$SECRET_FILE" 2>/dev/null)"; then
	:
else
	perms="$(stat -f '%Lp' "$SECRET_FILE" 2>/dev/null || true)"
fi
[ "$perms" = "600" ] || fail "el fichero de secreto no tiene permisos 0600 (tiene '${perms:-desconocidos}')"
LOCAL_SECRET="$(cat "$SECRET_FILE")"
[ -n "$LOCAL_SECRET" ] || fail "el fichero de secreto esta vacio"
if openssl pkcs12 -in "$APP_DIR/autofirma.pfx" -passin "pass:$LEGACY_KS_PASS" -noout 2>/dev/null; then
	fail "el almacen sigue abriendo con la contrasena heredada: no se migro"
fi
openssl pkcs12 -in "$APP_DIR/autofirma.pfx" -passin "pass:$LOCAL_SECRET" -noout 2>/dev/null \
	|| fail "el almacen migrado no abre con el secreto local"
unset LOCAL_SECRET
ok "secreto local generado (0600) y almacen heredado migrado"
