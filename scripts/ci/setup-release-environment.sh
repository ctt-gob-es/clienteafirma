#!/usr/bin/env bash
# Crea/actualiza el entorno protegido release, sin secretos ni bypass de aprobación.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

REPO=Soluciones-Alexendros/clienteafirma-alexendros
ENVIRONMENT=release
REVIEWER_ID="${RELEASE_REVIEWER_ID:-$(gh api user --jq '.id')}"
[[ "$REVIEWER_ID" =~ ^[0-9]+$ ]] || { echo 'RELEASE_REVIEWER_ID debe ser numérico' >&2; exit 1; }

payload="$(mktemp)"
response="$(mktemp)"
trap 'rm -f "$payload" "$response"' EXIT
python3 - "$REVIEWER_ID" "$payload" <<'PY'
import json
import sys
reviewer_id = int(sys.argv[1])
with open(sys.argv[2], 'w', encoding='utf-8') as output:
    json.dump({
        'wait_timer': 0,
        'prevent_self_review': False,
        'reviewers': [{'type': 'User', 'id': reviewer_id}],
    }, output)
PY

gh api -X PUT "repos/$REPO/environments/$ENVIRONMENT" --input "$payload" >/dev/null

# Validación leída del servidor: un revisor obligatorio y autoaprobación permitida
# para el operador único. No se activa bypass ni se escriben secretos aquí.
gh api "repos/$REPO/environments/$ENVIRONMENT" > "$response"
python3 - "$REVIEWER_ID" "$response" <<'PY'
import json
import sys
with open(sys.argv[2], encoding='utf-8') as source:
    environment = json.load(source)
reviewer_id = int(sys.argv[1])
rules = environment.get('protection_rules', [])
reviewers = []
for rule in rules:
    if rule.get('type') == 'required_reviewers':
        reviewers = rule.get('reviewers', [])
        assert rule.get('prevent_self_review') is False, 'prevent_self_review bloquearía al operador único'
    if rule.get('type') == 'wait_timer':
        assert rule.get('wait_timer') == 0, 'wait_timer inesperado'
assert any(r.get('type') == 'User' and (r.get('reviewer') or {}).get('id') == reviewer_id for r in reviewers), \
    'el entorno no tiene el revisor obligatorio esperado'
print('release environment: OK (revisor obligatorio, wait_timer=0, prevent_self_review=false)')
PY
