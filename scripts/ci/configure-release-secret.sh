#!/usr/bin/env bash
# Registra GPG_PRIVATE_KEY en el entorno release sin mostrar su contenido.
# Uso: bash scripts/ci/configure-release-secret.sh /ruta/privada/subclave-firma.asc
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

REPO=Soluciones-Alexendros/clienteafirma-alexendros
ENVIRONMENT=release
EXPECTED_FINGERPRINT="${RELEASE_EXPECTED_FINGERPRINT:-EFECA8DCB405D5F33F3FE35C050B8C2C622F92BD}"
PUBLIC_KEYS="${RELEASE_PUBLIC_KEYS:-$ROOT/KEYS}"
PRIVATE_KEY_FILE="${1:-}"

if [[ -z "$PRIVATE_KEY_FILE" || ! -r "$PRIVATE_KEY_FILE" ]]; then
  echo "Uso: bash scripts/ci/configure-release-secret.sh /ruta/privada/subclave-firma.asc" >&2
  exit 2
fi
[[ -s "$PUBLIC_KEYS" ]] || { echo "No existe la clave pública: $PUBLIC_KEYS" >&2; exit 1; }
command -v gpg >/dev/null
command -v gh >/dev/null
command -v python3 >/dev/null

# Exige que el entorno protegido ya exista; no cambia ajustes ni lee secretos.
gh api "repos/$REPO/environments/$ENVIRONMENT" >/dev/null

GNUPGHOME="$(mktemp -d)"
SIGN_TEST_DIR="$(mktemp -d)"
cleanup() {
  rm -rf "$GNUPGHOME" "$SIGN_TEST_DIR"
}
trap cleanup EXIT
chmod 700 "$GNUPGHOME" "$SIGN_TEST_DIR"
export GNUPGHOME

# Comprueba la huella sin volcar la clave ni registrar su contenido en logs.
if ! gpg --batch --show-keys --with-colons "$PUBLIC_KEYS" 2>/dev/null \
  | awk -F: -v expected="$EXPECTED_FINGERPRINT" '$1=="fpr" && $10==expected {found=1} END {exit !found}'; then
  echo 'La clave pública KEYS no contiene la huella esperada.' >&2
  exit 1
fi

gpg --batch --yes --import "$PRIVATE_KEY_FILE" >/dev/null 2>&1
if ! gpg --batch --list-secret-keys --with-colons "$EXPECTED_FINGERPRINT" 2>/dev/null \
  | awk -F: -v expected="$EXPECTED_FINGERPRINT" '$1=="fpr" && $10==expected {found=1} END {exit !found}'; then
  echo 'El fichero no contiene una clave secreta con la huella autorizada.' >&2
  exit 1
fi

# Prueba funcional local con un mensaje sintético, antes de modificar GitHub.
printf '%s\n' 'release-signing-self-test' > "$SIGN_TEST_DIR/message.txt"
gpg --batch --yes --local-user "$EXPECTED_FINGERPRINT" --armor \
  --detach-sign --output "$SIGN_TEST_DIR/message.txt.asc" "$SIGN_TEST_DIR/message.txt"
gpg --batch --verify "$SIGN_TEST_DIR/message.txt.asc" "$SIGN_TEST_DIR/message.txt" >/dev/null 2>&1

# El contenido solo viaja por stdin hacia gh secret set; nunca se imprime.
gh secret set GPG_PRIVATE_KEY --env "$ENVIRONMENT" -R "$REPO" < "$PRIVATE_KEY_FILE"

# Verificación exclusivamente por nombre, nunca por valor.
gh secret list --env "$ENVIRONMENT" -R "$REPO" --json name \
  --jq '.[] | select(.name == "GPG_PRIVATE_KEY") | .name' >/dev/null
printf '%s\n' 'GPG_PRIVATE_KEY: configurado en entorno release; firma local verificada; valor no mostrado.'
