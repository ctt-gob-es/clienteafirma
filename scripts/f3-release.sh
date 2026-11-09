#!/usr/bin/env bash
# F3 — Empaqueta release verificable: JAR + SBOM + checksums (+ GPG si hay clave)
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
export JAVA_HOME="${JAVA_HOME:-$ROOT/tools/jdk8}"
export PATH="$ROOT/tools/apache-maven-3.9.9/bin:$JAVA_HOME/bin:${PATH:-}"
unset _JAVA_OPTIONS || true

VER="${1:-1.9.1-autofirma2026.0}"
OUT="$ROOT/dist/$VER"
mkdir -p "$OUT"

JAR="$ROOT/afirma-simple/target/autofirma.jar"
test -f "$JAR"

cp -f "$JAR" "$OUT/autofirma-$VER.jar"
cp -f "$ROOT/docs/BASELINE.txt" "$OUT/"
cp -f "$ROOT/propuesta-autofirma-2026.md" "$OUT/"
cp -f "$ROOT/vectors/manifest/latest.txt" "$OUT/f2-manifest.txt" 2>/dev/null || true

cd "$ROOT"
mvn -B -DskipTests org.cyclonedx:cyclonedx-maven-plugin:2.8.1:makeAggregateBom \
  -DoutputFormat=json -DoutputName=bom 2>&1 | tee "$ROOT/docs/sbom-build.log" | tail -30 || true
if [[ -f target/bom.json ]]; then
  cp -f target/bom.json "$OUT/bom.json"
else
  mvn -B -DskipTests dependency:tree > "$OUT/dependency-tree.txt"
fi

(
  cd "$OUT"
  sha256sum ./* > SHA256SUMS
  if command -v gpg >/dev/null && gpg --list-secret-keys >/dev/null 2>&1; then
    gpg --detach-sign --armor SHA256SUMS
    echo "GPG signature written: SHA256SUMS.asc"
  else
    echo "# No GPG secret key — checksums only. Attach cosign/GPG in CI when keys exist." >> SHA256SUMS.NOTE
  fi
)

cat > "$OUT/CHANGELOG.md" <<EOF
# Autofirma-2026 $VER

- Línea base clienteafirma **1.9.1** ($(head -1 "$ROOT/docs/BASELINE.txt"))
- Build JDK 8, perfil \`-Denv=install\`
- Vectores F2 regenerados (ver f2-manifest.txt)
- SBOM CycloneDX o dependency-tree adjunto
- Licencia: GPL 2+ / EUPL 1.1 — atribución CTT / AEAD

EOF

echo "Release staged at $OUT"
ls -la "$OUT"
