#!/usr/bin/env bash
# AppImage skeleton for Autofirma (F6 / oleada 4).
# Requires: appimagetool OR creates a runnable AppDir tree without packing.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
JAR="${AUTOFIRMA_JAR:-$ROOT/afirma-simple/target/autofirma.jar}"
OUT="${1:-$ROOT/dist/appimage}"
APPDIR="$OUT/Autofirma.AppDir"

if [[ ! -f "$JAR" ]]; then
  echo "Falta $JAR — construye el cliente antes (scripts/mvp.sh o mvn -Denv=install)." >&2
  exit 1
fi

rm -rf "$APPDIR"
mkdir -p "$APPDIR/usr/bin" "$APPDIR/usr/share/autofirma" "$APPDIR/usr/share/applications" "$APPDIR/usr/share/icons/hicolor/256x256/apps"

cp -f "$JAR" "$APPDIR/usr/share/autofirma/autofirma.jar"
cat > "$APPDIR/usr/bin/autofirma" <<'EOF'
#!/bin/sh
HERE="$(dirname "$(readlink -f "$0")")"
exec java -jar "$HERE/../share/autofirma/autofirma.jar" "$@"
EOF
chmod 755 "$APPDIR/usr/bin/autofirma"

cat > "$APPDIR/autofirma.desktop" <<'EOF'
[Desktop Entry]
Name=Autofirma (comunitario)
Comment=Cliente de firma electronica Autofirma-2026
Exec=autofirma %u
Icon=autofirma
Type=Application
Categories=Office;Utility;
MimeType=x-scheme-handler/afirma;
Terminal=false
EOF
cp -f "$APPDIR/autofirma.desktop" "$APPDIR/usr/share/applications/"

# Prefer packaging icon if present
ICON_SRC=$(find "$ROOT/packaging" -name 'autofirma*.png' 2>/dev/null | head -1 || true)
if [[ -z "${ICON_SRC:-}" && -f "$ROOT/logo_autofirma.png" ]]; then
  ICON_SRC="$ROOT/logo_autofirma.png"
fi
if [[ -n "${ICON_SRC:-}" ]]; then
  cp -f "$ICON_SRC" "$APPDIR/usr/share/icons/hicolor/256x256/apps/autofirma.png"
  cp -f "$ICON_SRC" "$APPDIR/autofirma.png"
else
  # Minimal placeholder PNG not required for AppDir usability via CLI
  touch "$APPDIR/autofirma.png"
fi

cat > "$APPDIR/AppRun" <<'EOF'
#!/bin/sh
HERE="$(dirname "$(readlink -f "$0")")"
export PATH="$HERE/usr/bin:$PATH"
exec "$HERE/usr/bin/autofirma" "$@"
EOF
chmod 755 "$APPDIR/AppRun"

echo "AppDir listo: $APPDIR"
if command -v appimagetool >/dev/null 2>&1; then
  appimagetool "$APPDIR" "$OUT/Autofirma-x86_64.AppImage"
  echo "AppImage: $OUT/Autofirma-x86_64.AppImage"
else
  echo "appimagetool no instalado — usa el AppDir o instálalo para empaquetar."
fi
