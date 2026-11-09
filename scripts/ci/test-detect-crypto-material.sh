#!/usr/bin/env bash
# Mutaciones del detector de material. No escribe en el repositorio real.
set -euo pipefail

src="$(cd "$(dirname "$0")" && pwd)/detect-crypto-material.sh"
pass=0
fail=0

assert_status() {
  local name="$1" expected="$2"
  shift 2
  set +e
  "$@" >"$tmp/out" 2>"$tmp/err"
  local got=$?
  set -e
  if [[ "$got" -eq "$expected" ]]; then
    echo "OK $name"
    pass=$((pass + 1))
  else
    echo "FALLO $name: esperado $expected, obtuvo $got" >&2
    cat "$tmp/err" >&2
    fail=1
  fi
}

new_repo() {
  tmp="$(mktemp -d)"
  git init -q "$tmp"
  git -C "$tmp" config user.email "sec-2026@example.invalid"
  git -C "$tmp" config user.name "sec-2026"
  mkdir -p "$tmp/scripts/ci" "$tmp/src/test/resources" "$tmp/afirma-simple-installer/certificados"
  cp "$src" "$tmp/scripts/ci/detect-crypto-material.sh"
  : > "$tmp/scripts/ci/crypto-material-allowlist.txt"
}

track() {
  git -C "$tmp" add -A
}

allow() {
  local file="$1"
  local hash
  hash="$(sha256sum "$tmp/$file" | awk '{print $1}')"
  printf '%s  %s\n' "$hash" "$file" >> "$tmp/scripts/ci/crypto-material-allowlist.txt"
  git -C "$tmp" add "$tmp/scripts/ci/crypto-material-allowlist.txt"
}

run_detector() {
  bash "$tmp/scripts/ci/detect-crypto-material.sh"
}

# 1. Árbol vacío.
new_repo
track
assert_status "vacio" 0 run_detector

# 2. PKCS#12 fuera de test.
new_repo
mkdir -p "$tmp/ops"
printf 'not-a-real-key\n' > "$tmp/ops/demo.p12"
track
assert_status "pfx-fuera-de-test" 1 run_detector

# 3. Fixture de test sin allowlist.
new_repo
printf 'fixture\n' > "$tmp/src/test/resources/demo.p12"
track
assert_status "test-sin-allowlist" 1 run_detector

# 4. Fixture permitido.
new_repo
printf 'fixture\n' > "$tmp/src/test/resources/demo.p12"
track
allow "src/test/resources/demo.p12"
assert_status "test-allowlist" 0 run_detector

# 5. Hash cambiado.
new_repo
printf 'fixture\n' > "$tmp/src/test/resources/demo.p12"
track
allow "src/test/resources/demo.p12"
printf 'fixture-mutado\n' > "$tmp/src/test/resources/demo.p12"
git -C "$tmp" add "src/test/resources/demo.p12"
assert_status "hash-distinto" 1 run_detector

# 6. signtool con literal. El fuente de este test no contiene el patrón contiguo.
new_repo
printf 'fixture\n' > "$tmp/src/test/resources/demo.p12"
track
allow "src/test/resources/demo.p12"
printf '%s sign /f demo.pfx /p %s\n' 'signtool' 'not-from-env' > "$tmp/sign.bat"
track
assert_status "signtool-literal" 1 run_detector

# 7. signtool con variable de entorno.
new_repo
printf 'fixture\n' > "$tmp/src/test/resources/demo.p12"
track
allow "src/test/resources/demo.p12"
printf '%s sign /f "%%AFIRMA_SIGN_PFX%%" /p "%%AFIRMA_SIGN_PASS%%"\n' 'signtool' > "$tmp/sign.bat"
track
assert_status "signtool-env" 0 run_detector

# 8. Propiedad Maven literal.
new_repo
printf '<afirma.keytool.password>%s</afirma.keytool.password>\n' 'not-from-env' > "$tmp/pom-snippet.xml"
track
assert_status "password-literal" 1 run_detector

# 9. Propiedad Maven desde el entorno.
new_repo
printf '<afirma.keytool.password>${env.AFIRMA_KEYSTORE_PASS}</afirma.keytool.password>\n' > "$tmp/pom-snippet.xml"
track
assert_status "password-env" 0 run_detector

# 10. changeit solo en los scripts de cacerts del JRE.
new_repo
printf 'keytool -importcert -store%s changeit -file a\n' 'pass' > "$tmp/afirma-simple-installer/certificados/insert_cacerts.sh"
printf 'keytool -importcert -store%s changeit -file a\n' 'pass' > "$tmp/otro.sh"
track
assert_status "changeit-fuera-de-cacerts" 1 run_detector

new_repo
printf 'keytool -importcert -store%s changeit -file a\n' 'pass' > "$tmp/afirma-simple-installer/certificados/insert_cacerts.sh"
track
assert_status "changeit-cacerts" 0 run_detector

# 11. Allowlist que cita un fichero borrado del índice.
new_repo
printf 'fixture\n' > "$tmp/src/test/resources/demo.p12"
track
allow "src/test/resources/demo.p12"
git -C "$tmp" rm -q --cached "src/test/resources/demo.p12"
assert_status "allowlist-huerfana" 1 run_detector

echo "pasaron=$pass"
if [[ "$fail" -ne 0 ]]; then
  echo "test-detect-crypto-material: FALLÓ" >&2
  exit 1
fi
