#!/usr/bin/env bash
# T3b · criterio C: el repositorio no incorpora PKCS#12 nuevos.
#
# El arbol heredado del CTT ya versiona 84 almacenes de PRUEBA (ANF_PF_Activo.pfx y
# compania) en src/test/resources. Son vectores publicos del proyecto original, sin datos
# reales, y los usan los tests de criptografia y scripts/f2-regression.sh: se quedan.
#
# Decisión de Arquitecto (2026-11-04): lista fija de los 84 en
# scripts/interop/pkcs12-inherited.txt. Si aparece un PKCS#12 que no esta en la lista, el
# check falla. Si desaparece uno de la lista, tambien: obliga a actualizarla a proposito.
#
# Uso: scripts/interop/check-no-pkcs12.sh
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"
ALLOWLIST="scripts/interop/pkcs12-inherited.txt"

[ -f "$ALLOWLIST" ] || { echo "FAIL: falta la lista fija $ALLOWLIST" >&2; exit 1; }

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

git ls-files | grep -Ei '\.(p12|pfx)$' | sort > "$work/actual"
sort "$ALLOWLIST" > "$work/expected"

if ! diff -u "$work/expected" "$work/actual" > "$work/diff"; then
	echo "FAIL: los PKCS#12 del repositorio no coinciden con la lista fija heredada." >&2
	echo "      Revisa si es intencionado y actualiza $ALLOWLIST a proposito." >&2
	sed 's/^/      /' "$work/diff" >&2
	exit 1
fi

if git ls-files | grep -q '^interop-local/'; then
	echo "FAIL: interop-local/ esta versionado" >&2
	exit 1
fi

total="$(wc -l < "$work/actual" | tr -d ' ')"
echo "OK: los $total PKCS#12 coinciden exactamente con la lista heredada; ninguno nuevo"
