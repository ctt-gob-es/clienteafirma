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

# T6: la TSA respeta strictSslChecks y valida la respuesta (firma, EKU, cadena y hora).
TSA_DIR="afirma-crypto-core-pkcs7-tsp/src/main/java/es/gob/afirma/signers/tsp/pkcs7"
grep -F 'isStrictSslChecks()' "$TSA_DIR/CMSTimestamper.java" >/dev/null \
  || fail "CMSTimestamper no consulta strictSslChecks"
grep -F 'else if (strictSsl) {' "$TSA_DIR/CMSTimestamper.java" >/dev/null \
  || fail "CMSTimestamper no usa la confianza del sistema en modo estricto"
# La TSA no define sus propios trust managers: usa los permisivos centralizados de T5
! grep -F 'checkServerTrusted' "$TSA_DIR/CMSTimestamper.java" >/dev/null \
  || fail "CMSTimestamper define trust managers propios"
grep -F 'SslSecurityManager.getPermissiveTrustManagers()' "$TSA_DIR/CMSTimestamper.java" >/dev/null \
  || fail "CMSTimestamper no usa los trust managers permisivos centralizados"
grep -F 'TsaResponseValidator.validate(' "$TSA_DIR/CMSTimestamper.java" >/dev/null \
  || fail "CMSTimestamper no valida la respuesta de la TSA"
grep -F 'isSignatureValid(' "$TSA_DIR/TsaResponseValidator.java" >/dev/null \
  || fail "el validador de TSA no verifica la firma del sello"
grep -F 'id_kp_timeStamping' "$TSA_DIR/TsaResponseValidator.java" >/dev/null \
  || fail "el validador de TSA no comprueba el EKU de sellado de tiempo"
grep -F 'MAX_CLOCK_SKEW_MILLIS' "$TSA_DIR/TsaResponseValidator.java" >/dev/null \
  || fail "el validador de TSA no comprueba la hora razonable"
grep -F 'id_sha256.getId()' "$TSA_DIR/CMSTimestamper.java" >/dev/null \
  || fail "CMSTimestamper no usa SHA-256 como algoritmo por defecto"
! grep -F 'new String(rawResponse)' "$TSA_DIR/CMSTimestamper.java" >/dev/null \
  || fail "CMSTimestamper registra o propaga el contenido de la respuesta de la TSA"

echo "test-ci-gates: T6 OK"

# T7: token firmado de la sesion trifasica validado en todos los caminos del cliente.
TOKEN_DIR="afirma-crypto-core-pkcs7/src/main/java/es/gob/afirma/signers/pkcs7"
grep -F 'getToken()' afirma-core/src/main/java/es/gob/afirma/core/signers/TriphaseData.java >/dev/null \
  || fail "TriphaseData no expone el token firmado de la sesion"
for f in \
    afirma-crypto-cadestri-client/src/main/java/es/gob/afirma/signers/cadestri/client/AOCAdESTriPhaseSigner.java \
    afirma-crypto-xadestri-client/src/main/java/es/gob/afirma/signers/xadestri/client/AOXAdESTriPhaseSigner.java \
    afirma-crypto-padestri-client/src/main/java/es/gob/afirma/signers/padestri/client/PDFTriPhaseSignerUtil.java \
    afirma-crypto-batch-client/src/main/java/es/gob/afirma/signers/batch/client/BatchSigner.java \
    afirma-core/src/main/java/es/gob/afirma/core/signers/AOPkcs1TriPhaseSigner.java ; do
  grep -q 'TriphaseTokenValidator' "$f" || fail "$f no valida el token trifasico"
done
grep -F 'signer.verify(' "$TOKEN_DIR/TriphaseTokenValidator.java" >/dev/null \
  || fail "el validador del token trifasico no verifica la firma"
grep -F 'id_kp_emailProtection' "$TOKEN_DIR/TriphaseTokenValidator.java" >/dev/null \
  || fail "el validador del token trifasico no comprueba el EKU de sellado"
grep -F 'CertPathBuilder' "$TOKEN_DIR/TriphaseTokenValidator.java" >/dev/null \
  || fail "el validador del token trifasico no valida la cadena PKIX"
grep -F 'MAX_TOKEN_LIFETIME_MILLIS' "$TOKEN_DIR/TriphaseTokenValidator.java" >/dev/null \
  || fail "el validador del token trifasico no comprueba su vigencia"
grep -F 'strictTriphaseToken' "$TOKEN_DIR/TriphaseTokenValidator.java" >/dev/null \
  || fail "el token trifasico no es activable por preferencia"

echo "test-ci-gates: T7 OK"

# T8: idsession con caducidad y uso unico, y Origin validado en el canal local.
HTTP_DIR="afirma-simple/src/main/java/es/gob/afirma/standalone/protocol"
CORE_DIR="afirma-core/src/main/java/es/gob/afirma/core/misc/protocol"

# La cabecera CORS no se fija de forma incondicional: la decide LocalChannelSessions.
! grep -F 'Access-Control-Allow-Origin: *' "$HTTP_DIR/CommandProcessorThread.java" >/dev/null \
  || fail "CommandProcessorThread fija Access-Control-Allow-Origin: * de forma incondicional"
grep -F 'LocalChannelSessions.corsHeaders(' "$HTTP_DIR/CommandProcessorThread.java" >/dev/null \
  || fail "CommandProcessorThread no usa las cabeceras CORS del canal local"

# Caducidad y uso unico dentro del validador, no en quien llama.
grep -F 'SESSION_TTL_PROPERTY' "$CORE_DIR/LocalChannelSessions.java" >/dev/null \
  || fail "el idsession no tiene caducidad configurable"
grep -F 'this.clock.millis() > expiry.longValue()' "$CORE_DIR/WebSocketSessionValidator.java" >/dev/null \
  || fail "el validador de sesiones no aplica la caducidad"
grep -F 'MAX_TRACKED_SESSIONS' "$CORE_DIR/WebSocketSessionValidator.java" >/dev/null \
  || fail "el registro de sesiones no acota la memoria"
grep -F 'public synchronized void purge()' "$CORE_DIR/WebSocketSessionValidator.java" >/dev/null \
  || fail "el registro de sesiones no purga las caducadas"
grep -F 'public synchronized void consume(' "$CORE_DIR/WebSocketSessionValidator.java" >/dev/null \
  || fail "el registro de sesiones no aplica el uso unico"

# Sin comodin por defecto en la lista de origenes.
grep -F 'return value == null ? "" : value.trim();' "$CORE_DIR/LocalChannelSessions.java" >/dev/null \
  || fail "la lista de origenes permitidos tiene un valor por defecto distinto de vacio"
grep -F 'Vary: Origin' "$CORE_DIR/WebSocketOriginValidator.java" >/dev/null \
  || fail "la respuesta CORS estricta no declara Vary: Origin"

# Los dos caminos, HTTP y websocket, pasan por la validacion.
grep -F 'LocalChannelSessions.isStrict()' "$HTTP_DIR/CommandProcessorThread.java" >/dev/null \
  || fail "el camino HTTP no respeta strictWebSocketSession"
grep -F 'guardSession(' "$HTTP_DIR/AfirmaWebSocketServer.java" >/dev/null \
  || fail "el websocket no valida la sesion"
grep -F 'guardSession(' "$HTTP_DIR/AfirmaWebSocketServerV4Sup.java" >/dev/null \
  || fail "el websocket v4 no valida la sesion"
grep -F 'releaseSession(' "$HTTP_DIR/AfirmaWebSocketServer.java" >/dev/null \
  || fail "el websocket no consume la sesion al cerrar"
grep -F 'LocalChannelSessions.closeChannel(' "$HTTP_DIR/ServiceInvocationManager.java" >/dev/null \
  || fail "el canal HTTP no consume la sesion al cerrar"

# Las pruebas de integracion del canal real forman parte de make test.
grep -F 'HttpChannelIntegrationTest' Makefile >/dev/null \
  || fail "make test no ejecuta las pruebas del canal HTTP real"
grep -F 'WebSocketChannelIntegrationTest' Makefile >/dev/null \
  || fail "make test no ejecuta las pruebas del websocket real"

echo "test-ci-gates: T8 OK"

# T9: modo por defecto distinto de ECB y algoritmos obsoletos detras de una opcion.
CIPHER_DIR="afirma-core/src/main/java/es/gob/afirma/core/ciphers"
grep -F 'ALLOW_LEGACY_CIPHERS_PROPERTY = "allowLegacyCiphers"' "$CIPHER_DIR/CipherConstants.java" >/dev/null \
  || fail "no existe la opcion explicita allowLegacyCiphers"
grep -F 'public static boolean isLegacyAllowed()' "$CIPHER_DIR/CipherConstants.java" >/dev/null \
  || fail "CipherConstants no expone la comprobacion de la opcion de legado"
grep -F 'private static final AOCipherBlockMode DEFAULT_BLOCK_MODE = CBC;' "$CIPHER_DIR/CipherConstants.java" >/dev/null \
  || fail "el modo de bloque por defecto no es CBC"
! grep -F 'DEFAULT_BLOCK_MODE = ECB' "$CIPHER_DIR/CipherConstants.java" >/dev/null \
  || fail "el modo de bloque por defecto sigue siendo ECB"
grep -F 'ciphAlgo.isLegacy() && !deciphering && !CipherConstants.isLegacyAllowed()' "$CIPHER_DIR/AOCipherConfig.java" >/dev/null \
  || fail "la creacion de cifrados no aplica la puerta de legado"
grep -F 'public static AOCipherConfig forDeciphering(' "$CIPHER_DIR/AOCipherConfig.java" >/dev/null \
  || fail "no existe el camino de descifrado heredado"
grep -F 'warnLegacyForDecipher' "$CIPHER_DIR/AOCipherConfig.java" >/dev/null \
  || fail "el descifrado heredado no deja aviso"
# Los seis algoritmos obsoletos deben estar marcados con la bandera de legado.
legacy_count="$(grep -cE '^[[:space:]]+(DES|PBEWITHMD5ANDDES|PBEWITHSHA1ANDDESEDE|PBEWITHSHA1ANDRC2_40|HMACMD5|HMACSHA1)\(".*, true\), //' "$CIPHER_DIR/CipherConstants.java" || true)"
[ "$legacy_count" -eq 6 ] || fail "los seis algoritmos obsoletos no estan marcados como legacy ($legacy_count)"

echo "test-ci-gates: T9 OK"
