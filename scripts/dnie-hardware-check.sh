#!/usr/bin/env bash
# Guion DNIe en hardware real (F6) — NO se ejecuta en CI
set -euo pipefail
echo "=== DNIe hardware check (OpenSC / PKCS#11 / jmulticard) ==="
command -v opensc-tool >/dev/null && opensc-tool -l || echo "opensc-tool no instalado"
command -v pkcs11-tool >/dev/null && pkcs11-tool -L || echo "pkcs11-tool no instalado"
echo
echo "Pasos manuales:"
echo "1. Insertar DNIe 3/4/5 y lector CCID."
echo "2. opensc-tool -l  → debe listar el DNIe."
echo "3. autofirma listaliases -store dni"
echo "4. Firmar un PDF de prueba con -store dni (PIN en diálogo)."
echo "5. Anotar resultado en docs/dnie-log.md"
echo
echo "Dependencia upstream: https://github.com/ctt-gob-es/jmulticard"
exit 0
