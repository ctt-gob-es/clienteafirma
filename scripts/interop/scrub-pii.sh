#!/usr/bin/env bash
# T3b · B: sanea los datos personales de la salida de la prueba local con un .p12 real.
#
# Elimina del texto: DNI/NIE/NIF (8 digitos + letra), NIE (X/Y/Z + 7 digitos + letra),
# CIF (letra + 7 digitos + digito/letra), numeros de serie largos y correos.
# Deja el resto de la evidencia (protocolo, suite, resultado DSS) intacta.
#
# Uso: scripts/interop/scrub-pii.sh <directorio|fichero> [mas rutas...]
set -euo pipefail

[ "$#" -ge 1 ] || { echo "uso: scrub-pii.sh <ruta> [ruta...]" >&2; exit 2; }

scrub_file() {
	local f="$1"
	local tmp
	tmp="$(mktemp)"
	LC_ALL=C sed -E \
		-e 's/\b[0-9]{8}[A-Za-z]\b/<NIF>/g' \
		-e 's/\b[XYZxyz][0-9]{7}[A-Za-z]\b/<NIE>/g' \
		-e 's/\b[ABCDEFGHJNPQRSUVWabcdefghjnpqrsuvw][0-9]{7}[0-9A-Ja-j]\b/<CIF>/g' \
		-e 's/\b[0-9]{9,}\b/<NUM>/g' \
		-e 's/\b[0-9A-Fa-f]{2}(:[0-9A-Fa-f]{2}){5,}\b/<SERIAL>/g' \
		-e 's/[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}/<EMAIL>/g' \
		"$f" > "$tmp"
	mv "$tmp" "$f"
}

for target in "$@"; do
	[ -e "$target" ] || { echo "[scrub] no existe: $target" >&2; continue; }
	if [ -d "$target" ]; then
		while IFS= read -r -d '' f; do
			case "$f" in
				*.p12|*.pfx|*.key) echo "[scrub] omitido binario sensible: $f" >&2 ;;
				*) scrub_file "$f" ;;
			esac
		done < <(find "$target" -type f -print0)
	else
		scrub_file "$target"
	fi
done

# Titular del certificado: se elimina el nombre completo y el CN literal.
if [ -n "${INTEROP_SUBJECT_NAME:-}" ]; then
	python3 - "$INTEROP_SUBJECT_NAME" "$@" <<'PYEOF'
import os, re, sys

subject = sys.argv[1]
targets = sys.argv[2:]

tokens = set()
exact_tokens = set()
tokens.add(subject.strip())
for chunk in re.split(r"[,/]", subject):
    chunk = chunk.strip()
    if chunk.upper().startswith("CN="):
        cn = chunk[3:].strip()
        tokens.add(cn)
        for part in cn.split():
            # Solo palabras con grafia exacta: evita destrozar terminos tecnicos
            # (una "rsa" minuscula no debe tocar ECDHE-RSA) ni rutas del sistema.
            if len(part) > 3:
                exact_tokens.add(part)

def scrub(path):
    try:
        with open(path, "rb") as fh:
            data = fh.read()
        text = data.decode("utf-8", errors="ignore")
    except (OSError, UnicodeDecodeError):
        return
    original = text
    for token in sorted(tokens, key=len, reverse=True):
        if token:
            text = re.sub(re.escape(token), "<TITULAR>", text, flags=re.IGNORECASE)
    for token in sorted(exact_tokens, key=len, reverse=True):
        if token:
            text = re.sub(r"\b" + re.escape(token) + r"\b", "<TITULAR>", text)
    if text != original:
        with open(path, "w", encoding="utf-8") as fh:
            fh.write(text)

for target in targets:
    if os.path.isdir(target):
        for root, _dirs, files in os.walk(target):
            for name in files:
                if name.lower().endswith((".p12", ".pfx", ".key")):
                    continue
                scrub(os.path.join(root, name))
    elif os.path.isfile(target):
        scrub(target)
PYEOF
fi

# Verificacion: no debe quedar ningun patron de NIF en la salida.
if grep -rEI '\b[0-9]{8}[A-Za-z]\b' "$@" >/dev/null 2>&1; then
	echo "FAIL: quedan patrones de NIF tras el saneado" >&2
	exit 1
fi

echo "[scrub] OK: sin patrones de NIF/NIE/CIF en la salida"
