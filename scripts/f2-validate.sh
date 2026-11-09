#!/usr/bin/env bash
# Validación de integridad de vectores F2 + casos negativos + tests upstream de validation
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
export JAVA_HOME="${JAVA_HOME:-$ROOT/tools/jdk8}"
export PATH="$ROOT/tools/apache-maven-3.9.9/bin:$JAVA_HOME/bin:${PATH:-}"
unset _JAVA_OPTIONS || true

echo "=== 1) Regenerar vectores (sign) ==="
bash "$ROOT/scripts/f2-regression.sh"

echo "=== 2) Harness JUnit (integridad + negativos) ==="
cd "$ROOT/tests/validation-harness"
mvn -B test -Dvectors.dir="$ROOT/vectors"

echo "=== 3) Main CLI del harness ==="
mvn -B -q package -DskipTests
mvn -B -q exec:java \
  -Dexec.mainClass=es.gob.afirma.autofirma2026.validation.VectorIntegrityValidator \
  -Dexec.classpathScope=compile \
  -Dvectors.dir="$ROOT/vectors" \
  || java -cp "target/classes:$HOME/.m2/repository/es/gob/afirma/afirma-crypto-validation/1.9.1/afirma-crypto-validation-1.9.1.jar:$HOME/.m2/repository/es/gob/afirma/afirma-crypto-cades/1.9.1/afirma-crypto-cades-1.9.1.jar:$HOME/.m2/repository/es/gob/afirma/afirma-crypto-xades/1.9.1/afirma-crypto-xades-1.9.1.jar:$HOME/.m2/repository/es/gob/afirma/afirma-crypto-pdf/1.9.1/afirma-crypto-pdf-1.9.1.jar" \
     -Dvectors.dir="$ROOT/vectors" \
     es.gob.afirma.autofirma2026.validation.VectorIntegrityValidator

echo "=== 4) Unit tests upstream afirma-crypto-validation ==="
cd "$ROOT"
mvn -B -pl afirma-crypto-validation -am install -DskipTests -q
mvn -B -pl afirma-crypto-validation test \
  2>&1 | tee "$ROOT/docs/build-validation-upstream.log" | tail -40

echo "=== 5) Unit tests CAdES + PDF (sin DNIe ni Baseline anidados rotos) ==="
mvn -B -pl afirma-crypto-cades test \
  2>&1 | tee "$ROOT/docs/build-validation-cades.log" | grep -E 'Tests run:|BUILD' | tail -15
mvn -B -pl afirma-crypto-pdf test \
  -Dtest='TestPAdES*,TestPasswordPDF,TestPdf*,TestSign*,TestGet*,!MiniTestDNI,!TestPadesBaseline' \
  -Dsurefire.failIfNoSpecifiedTests=false \
  2>&1 | tee "$ROOT/docs/build-validation-pdf.log" | grep -E 'Tests run:|BUILD|ERROR' | tail -20

echo "F2-VALIDATE OK"
