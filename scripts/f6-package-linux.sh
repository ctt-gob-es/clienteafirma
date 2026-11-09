#!/usr/bin/env bash
# F6 — Empaqueta .deb / staging RPM con registro afirma://
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
VER="${1:-1.9.1-autofirma2026.0}"
JAR="$ROOT/afirma-simple/target/autofirma.jar"
test -f "$JAR"

STAGE="$ROOT/packaging/stage-deb"
rm -rf "$STAGE"
mkdir -p "$STAGE/DEBIAN" \
  "$STAGE/usr/bin" \
  "$STAGE/usr/lib/Autofirma" \
  "$STAGE/usr/share/applications" \
  "$STAGE/usr/share/doc/autofirma"

cp -f "$JAR" "$STAGE/usr/lib/Autofirma/autofirma.jar"
# Portable wrapper: never embed build-machine paths (tools/jdk*).
# Runtime uses java from PATH, falling back to /usr/bin/java.
cat > "$STAGE/usr/bin/autofirma" <<'EOF'
#!/bin/bash
JAVA_BIN="java"
if ! command -v java >/dev/null 2>&1 && [[ -x /usr/bin/java ]]; then
  JAVA_BIN="/usr/bin/java"
fi
exec "$JAVA_BIN" -Djdk.tls.maxHandshakeMessageSize=65536 -jar /usr/lib/Autofirma/autofirma.jar "$@"
EOF
chmod 755 "$STAGE/usr/bin/autofirma"

cat > "$STAGE/usr/share/applications/afirma.desktop" <<'EOF'
[Desktop Entry]
Encoding=UTF-8
Version=1.9
Name=Autofirma (2026 fork)
Type=Application
Terminal=false
Categories=Office;Utilities;Signature;Java
Exec=/usr/bin/autofirma %u
Icon=application-x-executable
GenericName=Herramienta de firma
Comment=Cliente de firma — fork comunitario Autofirma-2026
MimeType=x-scheme-handler/afirma;
StartupNotify=true
StartupWMClass=autofirma
EOF

cat > "$STAGE/DEBIAN/control" <<EOF
Package: autofirma-2026
Architecture: all
Section: utils
Priority: optional
Depends: libnss3-tools
Version: $VER
Maintainer: Autofirma-2026 community <noreply@localhost>
Homepage: https://github.com/ctt-gob-es/clienteafirma
Description: Autofirma 1.9.1 community build (Autofirma-2026)
 Fork comunitario drop-in: mismo protocolo afirma:// y formatos.
 Atribución CTT / AEAD. Licencia GPL-2+ / EUPL-1.1.
EOF

cat > "$STAGE/DEBIAN/postinst" <<'EOF'
#!/bin/bash
set -e
if command -v update-desktop-database >/dev/null; then
  update-desktop-database -q /usr/share/applications || true
fi
if command -v xdg-mime >/dev/null; then
  xdg-mime default afirma.desktop x-scheme-handler/afirma || true
fi
EOF
chmod 755 "$STAGE/DEBIAN/postinst"

INSTALLED_SIZE=$(du -sk "$STAGE/usr" | awk '{print $1}')
echo "Installed-Size: $INSTALLED_SIZE" >> "$STAGE/DEBIAN/control"

# dpkg-deb exige permisos de directorio 755 (no 770)
find "$STAGE" -type d -exec chmod 755 {} +
find "$STAGE" -type f -exec chmod 644 {} +
chmod 755 "$STAGE/usr/bin/autofirma" "$STAGE/DEBIAN/postinst"

OUT_DEB="$ROOT/packaging/autofirma-2026_${VER}_all.deb"
if command -v dpkg-deb >/dev/null; then
  dpkg-deb --build "$STAGE" "$OUT_DEB"
  echo "DEB: $OUT_DEB"
else
  echo "dpkg-deb no disponible — staging en $STAGE"
  echo "Instalación local de usuario (sin root):"
  echo "  mkdir -p ~/.local/share/applications ~/.local/bin"
  echo "  cp $STAGE/usr/share/applications/afirma.desktop ~/.local/share/applications/"
  echo "  # Ajustar Exec a ruta del jar local y: xdg-mime default afirma.desktop x-scheme-handler/afirma"
fi

# Staging RPM-like tree (spec reuse note)
RPM_STAGE="$ROOT/packaging/stage-rpm"
rm -rf "$RPM_STAGE"
mkdir -p "$RPM_STAGE/usr/bin" "$RPM_STAGE/usr/lib/Autofirma" "$RPM_STAGE/usr/share/applications"
cp -a "$STAGE/usr/bin/autofirma" "$RPM_STAGE/usr/bin/"
cp -a "$STAGE/usr/lib/Autofirma/autofirma.jar" "$RPM_STAGE/usr/lib/Autofirma/"
cp -a "$STAGE/usr/share/applications/afirma.desktop" "$RPM_STAGE/usr/share/applications/"
cat > "$ROOT/packaging/README.md" <<EOF
# Empaquetado Linux (F6)

- Staging DEB: \`stage-deb/\`
- Artefacto: \`$(basename "$OUT_DEB")\` (si hay dpkg-deb)
- Staging RPM: \`stage-rpm/\` — embeber en los SPEC oficiales de \`afirma-simple-installer/linux/\`
- Protocolo: \`MimeType=x-scheme-handler/afirma\`
- Portal de prueba: \`packaging/portal-prueba/index.html\`
- DNIe: \`scripts/dnie-hardware-check.sh\` (requiere tarjeta + OpenSC / jmulticard)
EOF

echo "F6 packaging OK"
