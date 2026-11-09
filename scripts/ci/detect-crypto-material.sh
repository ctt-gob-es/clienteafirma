#!/usr/bin/env bash
# Falla si HEAD versiona material de firma operativo o una contraseña literal.
# Los almacenes de test solo pasan si su ruta y su SHA-256 están en la allowlist.
set -euo pipefail

root="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$root"

allowlist="${CRYPTO_MATERIAL_ALLOWLIST:-$root/scripts/ci/crypto-material-allowlist.txt}"
fail=0

if [[ ! -f "$allowlist" ]]; then
  echo "ERROR: falta la allowlist: $allowlist" >&2
  exit 1
fi

declare -A allowed_hash=()
while IFS= read -r line || [[ -n "$line" ]]; do
  [[ -z "$line" || "$line" == \#* ]] && continue
  hash="${line%% *}"
  path="${line#*  }"
  if [[ "$hash" == "$line" || -z "$path" ]]; then
    echo "ERROR: línea de allowlist inválida: $line" >&2
    fail=1
    continue
  fi
  if [[ ! "$path" =~ (^|/)src/test/resources/ ]]; then
    echo "ERROR: la allowlist solo admite src/test/resources: $path" >&2
    fail=1
    continue
  fi
  allowed_hash["$path"]="$hash"
done < "$allowlist"

while IFS= read -r -d '' file; do
  hash="$(sha256sum "$file" | awk '{print $1}')"
  if [[ ! "$file" =~ (^|/)src/test/resources/ ]]; then
    echo "ERROR: material criptográfico fuera de test: $file" >&2
    fail=1
    continue
  fi
  if [[ -z "${allowed_hash[$file]:-}" ]]; then
    echo "ERROR: almacén de test ausente de la allowlist: $file" >&2
    fail=1
    continue
  fi
  if [[ "${allowed_hash[$file]}" != "$hash" ]]; then
    echo "ERROR: hash distinto del registrado: $file" >&2
    fail=1
  fi
  unset "allowed_hash[$file]"
done < <(git ls-files -z | grep -zEi '\.(p12|pfx|jks|keystore|key|pem|der)$' || true)

for path in "${!allowed_hash[@]}"; do
  echo "ERROR: la allowlist cita un fichero que no está en el índice: $path" >&2
  fail=1
done

credential_re='signtool.*\/p[[:space:]]+["'\'']?[^$%[:space:]"'\'']|storepass[[:space:]=:>]+["'\'']?[^$%[:space:]"'\'']|keypass[[:space:]=:>]+["'\'']?[^$%[:space:]"'\'']|<afirma\.keytool\.password>[^<$%]|BEGIN (RSA |EC |OPENSSH )?PRIVATE KEY'

hits="$(git grep -nI -E "$credential_re" -- . ':!scripts/ci/crypto-material-allowlist.txt' || true)"
if [[ -n "$hits" ]]; then
  while IFS= read -r hit; do
    [[ -z "$hit" ]] && continue
    file="${hit%%:*}"
    if [[ "$file" == "afirma-simple-installer/certificados/insert_cacerts.bat" || "$file" == "afirma-simple-installer/certificados/insert_cacerts.sh" ]] \
      && [[ "$hit" == *changeit* ]]; then
      continue
    fi
    echo "ERROR: credencial o clave privada hardcodeada: $hit" >&2
    fail=1
  done <<< "$hits"
fi

exit "$fail"
