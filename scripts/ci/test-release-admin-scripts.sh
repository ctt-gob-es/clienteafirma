#!/usr/bin/env bash
# Test local de los scripts de administración: gh simulado y clave sintética.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

command -v gpg >/dev/null
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$WORK/bin"
MOCK_GH_LOG="$WORK/gh.log"
export MOCK_GH_LOG
: > "$MOCK_GH_LOG"

cat > "$WORK/bin/gh" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >> "$MOCK_GH_LOG"
if [[ "${1:-}" == api && "${2:-}" == user ]]; then
  echo 424242
elif [[ "${1:-}" == api && "${2:-}" == -X && "${3:-}" == PUT ]]; then
  python3 - "$6" <<'PY'
import json, sys
with open(sys.argv[1], encoding='utf-8') as source:
    payload = json.load(source)
assert payload['wait_timer'] == 0
assert payload['prevent_self_review'] is False
assert payload['reviewers'] == [{'type': 'User', 'id': 424242}]
PY
elif [[ "${1:-}" == api && "${2:-}" == repos/Soluciones-Alexendros/clienteafirma-alexendros/environments/release ]]; then
  cat <<'JSON'
{"protection_rules":[{"type":"required_reviewers","prevent_self_review":false,"reviewers":[{"type":"User","reviewer":{"id":424242}}]},{"type":"wait_timer","wait_timer":0}]}
JSON
elif [[ "${1:-}" == secret && "${2:-}" == set ]]; then
  cat >/dev/null
elif [[ "${1:-}" == secret && "${2:-}" == list ]]; then
  echo GPG_PRIVATE_KEY
else
  echo "gh mock: comando inesperado: $*" >&2
  exit 1
fi
MOCK
chmod +x "$WORK/bin/gh"
export PATH="$WORK/bin:$PATH"

bash scripts/ci/setup-release-environment.sh
bash scripts/ci/setup-release-environment.sh
[[ "$(grep -c '^api -X PUT repos/Soluciones-Alexendros/clienteafirma-alexendros/environments/release' "$MOCK_GH_LOG")" -eq 2 ]]
echo 'OK: entorno idempotente y revisor validado con gh simulado'

# Clave exclusivamente sintética; nunca se imprime ni se usa fuera del directorio temporal.
export GNUPGHOME="$WORK/gnupg"
mkdir -m 700 "$GNUPGHOME"
gpg --batch --pinentry-mode loopback --passphrase '' --quick-generate-key \
  'Synthetic Release Test <synthetic@example.invalid>' ed25519 sign never >/dev/null 2>&1
FINGERPRINT="$(gpg --list-secret-keys --with-colons | awk -F: '$1=="fpr" {print $10; exit}')"
[[ "$FINGERPRINT" =~ ^[0-9A-F]{40}$ ]]
gpg --batch --armor --export "$FINGERPRINT" > "$WORK/KEYS"
gpg --batch --armor --export-secret-keys "$FINGERPRINT" > "$WORK/synthetic-private.asc"
unset GNUPGHOME

RELEASE_EXPECTED_FINGERPRINT="$FINGERPRINT" RELEASE_PUBLIC_KEYS="$WORK/KEYS" \
  bash scripts/ci/configure-release-secret.sh "$WORK/synthetic-private.asc"
grep -F 'secret set GPG_PRIVATE_KEY --env release -R Soluciones-Alexendros/clienteafirma-alexendros' "$MOCK_GH_LOG" >/dev/null
echo 'OK: secreto validado con clave sintética y enviado solo por stdin al mock'

calls_before="$(grep -c '^secret set GPG_PRIVATE_KEY' "$MOCK_GH_LOG")"
if RELEASE_EXPECTED_FINGERPRINT=0000000000000000000000000000000000000000 \
  RELEASE_PUBLIC_KEYS="$WORK/KEYS" \
  bash scripts/ci/configure-release-secret.sh "$WORK/synthetic-private.asc" >/dev/null 2>&1; then
  echo 'FAIL: se aceptó una huella incorrecta' >&2
  exit 1
fi
calls_after="$(grep -c '^secret set GPG_PRIVATE_KEY' "$MOCK_GH_LOG")"
[[ "$calls_before" = "$calls_after" ]]
echo 'OK: huella incorrecta rechazada antes de cualquier escritura'

echo 'test-release-admin-scripts: OK'
