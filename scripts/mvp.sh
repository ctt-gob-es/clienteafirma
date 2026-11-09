#!/usr/bin/env bash
# MVP operativo (monorepo): construir in-tree, validar vectores F2 y empaquetar evidencia.
# Uso: bash scripts/mvp.sh [--skip-build] [--skip-package] [--require-package]
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

SKIP_BUILD=0
SKIP_PACKAGE=0
REQUIRE_PACKAGE="${REQUIRE_PACKAGE:-0}"
for arg in "$@"; do
  case "$arg" in
    --skip-build|--skip-clone) SKIP_BUILD=1 ;;
    --skip-package) SKIP_PACKAGE=1 ;;
    --require-package) REQUIRE_PACKAGE=1 ;;
    -h|--help)
      echo "Uso: bash scripts/mvp.sh [--skip-build] [--skip-package] [--require-package]"
      echo "  --skip-build       omite mvn install (usa JAR ya construido)"
      echo "  --skip-clone       alias de --skip-build (compatibilidad)"
      echo "  --require-package  falla si el empaquetado Linux no completa (también REQUIRE_PACKAGE=1)"
      exit 0
      ;;
    *)
      echo "Argumento desconocido: $arg" >&2
      exit 2
      ;;
  esac
done

if [[ "$SKIP_PACKAGE" -eq 1 && "$REQUIRE_PACKAGE" -eq 1 ]]; then
  echo "Incompatible: --skip-package y --require-package" >&2
  exit 2
fi

BASELINE_FILE="$ROOT/docs/BASELINE.txt"
if [[ ! -f "$BASELINE_FILE" ]]; then
  echo "Falta docs/BASELINE.txt" >&2
  exit 1
fi

REF="$(grep -E '^BASELINE_COMMIT=' "$BASELINE_FILE" | cut -d= -f2- | tr -d '[:space:]' || true)"
REF="${REF:-0d7f3cf01fb65d2be5b245622d2c8f490f36e718}"

pick_java() {
  if [[ -n "${JAVA_HOME:-}" && -x "${JAVA_HOME}/bin/java" ]]; then
    return 0
  fi
  for candidate in \
    "$ROOT/tools/jdk8" \
    "$ROOT/tools/jdk-8" \
    "$ROOT/tools/temurin-8" \
    "$ROOT/tools/amazon-corretto-8" \
    /usr/lib/jvm/java-8-openjdk-amd64 \
    /usr/lib/jvm/temurin-8-jdk-amd64; do
    if [[ -x "$candidate/bin/java" ]]; then
      export JAVA_HOME="$candidate"
      return 0
    fi
  done
  echo "No se encontró JDK 8. Instala Temurin 8 o define JAVA_HOME." >&2
  exit 1
}

pick_mvn() {
  if command -v mvn >/dev/null 2>&1; then
    return 0
  fi
  for candidate in \
    "$ROOT/tools/apache-maven-3.9.9/bin/mvn" \
    "$ROOT/tools/apache-maven-3.9.6/bin/mvn" \
    "$ROOT/tools/maven/bin/mvn"; do
    if [[ -x "$candidate" ]]; then
      export PATH="$(dirname "$candidate"):$PATH"
      return 0
    fi
  done
  echo "No se encontró Maven (mvn)." >&2
  exit 1
}

echo "==> Autofirma-2026 MVP (Soluciones-Alexendros/clienteafirma-alexendros)"
echo "    upstream sync pin: $REF"

pick_java
pick_mvn
export PATH="$JAVA_HOME/bin:$PATH"
echo "    JAVA_HOME=$JAVA_HOME"
java -version 2>&1 | head -n1
mvn -version | head -n1

[[ -f "$ROOT/pom.xml" ]] || { echo "Falta pom.xml — este script espera el monorepo del cliente." >&2; exit 1; }

if [[ "$SKIP_BUILD" -eq 0 ]]; then
  echo "==> Build in-tree (JDK 8, skipTests, env=install)"
  mvn -B clean install -DskipTests -Denv=install
fi

JAR="$ROOT/afirma-simple/target/autofirma.jar"
if [[ ! -f "$JAR" ]]; then
  echo "No se generó autofirma.jar en $JAR" >&2
  exit 1
fi
echo "==> JAR OK: $JAR"
mkdir -p "$ROOT/dist"
sha256sum "$JAR" | tee "$ROOT/dist/MVP-SHA256.txt"

echo "==> Puerta F2 (regression + harness JUnit)"
bash "$ROOT/scripts/f2-regression.sh"
(
  cd "$ROOT/tests/validation-harness"
  mvn -B test -Dvectors.dir="$ROOT/vectors"
)

if [[ "$SKIP_PACKAGE" -eq 0 ]]; then
  if [[ -x "$ROOT/scripts/f6-package-linux.sh" ]]; then
    if [[ "$REQUIRE_PACKAGE" -eq 1 ]]; then
      echo "==> Empaquetado Linux (obligatorio)"
      bash "$ROOT/scripts/f6-package-linux.sh"
    else
      echo "==> Empaquetado Linux (best-effort; use --require-package para fallar si falla)"
      bash "$ROOT/scripts/f6-package-linux.sh" || echo "Aviso: empaquetado falló (no bloquea MVP)."
    fi
  elif [[ "$REQUIRE_PACKAGE" -eq 1 ]]; then
    echo "Falta scripts/f6-package-linux.sh y se pidió --require-package" >&2
    exit 1
  fi
fi

mkdir -p "$ROOT/dist"
{
  echo "Autofirma-2026 MVP evidence (monorepo)"
  echo "date=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "upstream_sync_pin=$REF"
  echo "jar=$JAR"
  sha256sum "$JAR"
  echo "java=$(java -version 2>&1 | head -n1)"
} > "$ROOT/dist/MVP-EVIDENCE.txt"

echo
echo "MVP operativo."
echo "  Evidencia: dist/MVP-EVIDENCE.txt"
echo "  JAR:       $JAR"
echo "  Siguiente: java -jar $JAR   # o instalar el .deb de packaging/"
