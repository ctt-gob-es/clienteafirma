#!/usr/bin/env bash
# Invariantes de separación, firma y publicación de release (T5).
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

fail() { echo "FAIL release-workflow: $*" >&2; exit 1; }

workflow=.github/workflows/release.yml
! grep -F -q -- '-DskipTests' "$workflow" || fail "release omite tests"
grep -F 'sudo apt-get install -y xvfb' "$workflow" >/dev/null || fail "falta display virtual para tests UI"
[[ "$(grep -F -c 'xvfb-run -a mvn -B clean' "$workflow")" -eq 3 ]] \
  || fail "los tres builds con tests deben usar Xvfb"
! grep -F -q 'continue-on-error:' "$workflow" || fail "release tolera errores"
! grep -F -q '|| true' "$workflow" || fail "release oculta errores"
! grep -F -q 'if: env.' "$workflow" || fail "jobs.*.if no admite el contexto env"
! grep -F -q '/continuous/' "$workflow" || fail "appimagetool no está fijado"
grep -F "APPIMAGETOOL_VERSION: '13'" "$workflow" >/dev/null || fail "versión appimagetool ausente"
grep -F 'env -u SOURCE_DATE_EPOCH ARCH=x86_64 bash scripts/f6-appimage.sh' "$workflow" >/dev/null \
  || fail "appimagetool 13 es incompatible con SOURCE_DATE_EPOCH global"
grep -F "APPIMAGETOOL_SHA256: 'df3baf5ca5facbecfc2f3fa6713c29ab9cefa8fd8c1eac5d283b79cab33e4acb'" "$workflow" >/dev/null \
  || fail "SHA-256 appimagetool ausente"
test -s KEYS || fail "KEYS ausente o vacío"
gpg --show-keys KEYS >/dev/null 2>&1 || fail "KEYS no contiene una clave pública válida"

python3 - <<'PY'
from pathlib import Path
import re

text = Path('.github/workflows/release.yml').read_text(encoding='utf-8')
jobs_text = text.split('\njobs:\n', 1)[1]
matches = list(re.finditer(r'^  ([a-z][a-z0-9-]*):\n', jobs_text, re.MULTILINE))
jobs = {}
for index, match in enumerate(matches):
    end = matches[index + 1].start() if index + 1 < len(matches) else len(jobs_text)
    jobs[match.group(1)] = jobs_text[match.start():end]
required = {'build', 'sign', 'validate', 'publish', 'dry-run'}
assert required <= jobs.keys(), f'jobs ausentes: {required - jobs.keys()}'
assert 'secrets.GPG_PRIVATE_KEY' not in jobs['build'], 'build accede a la clave privada'
assert 'secrets.GPG_PRIVATE_KEY' in jobs['sign'], 'sign no recibe la clave privada'
assert 'environment: release' in jobs['sign'], 'sign no usa el entorno protegido'
assert 'allow-loopback-pinentry' in jobs['sign'], 'sign no habilita loopback en gpg-agent'
assert 'GPG_PASSPHRASE: ${{ secrets.GPG_PASSPHRASE }}' in jobs['sign'], 'sign no recibe la passphrase'
assert '--pinentry-mode loopback --passphrase-fd 0' in jobs['sign'], 'sign no usa passphrase-fd 0'
assert '--local-user A8EF66BB6B3A3FC0365E18226B4AE42D2F924E93!' in jobs['sign'], 'sign no fija la subclave'
assert 'GPG_PASSPHRASE' not in jobs['build'], 'build accede a la passphrase'
assert 'secrets.GPG_PRIVATE_KEY' not in jobs['validate'], 'validate debe usar solo la clave pública'
assert 'GPG_PASSPHRASE' not in jobs['validate'], 'validate accede a la passphrase'
assert 'gpg --batch --yes --import KEYS' in jobs['validate'], 'validate no importa KEYS'
assert 'git tag -v "$REF_NAME"' in jobs['validate'], 'validate no verifica el tag firmado'
assert 'if ! git rev-parse --verify --quiet "refs/tags/${REF_NAME}"' in jobs['validate'], 'validate no evita clobber del tag ya descargado'
assert 'secrets.GPG_PRIVATE_KEY' not in jobs['publish'], 'publish no necesita la clave privada'
assert '--draft' in jobs['publish'], 'la release no queda en borrador'
for job in ('validate', 'publish'):
    assert 'release_notes.py' in jobs[job], f'{job} no genera notas desde el inventario'
    assert '--check release-notes.md' in jobs[job], f'{job} no valida las notas'
assert 'needs: validate' in jobs['publish'], 'publish no depende de validate'
PY

echo "test-release-workflow: OK"
