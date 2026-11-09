# Setup — Autofirma-2026 (monorepo)

## Requirements

| Tool | Version | Notes |
|------|---------|--------|
| Git | 2.x | Este repo |
| JDK | **8** (build), **21** optional (runtime F5) | Temurin recommended under `tools/` |
| Maven | 3.9+ | Portable under `tools/apache-maven-*` |
| Linux | glibc x86_64 | Primary packaging target |

## Clone

```bash
git clone https://github.com/Soluciones-Alexendros/clienteafirma-alexendros.git
cd clienteafirma
```

Opcional (F9, gitignored):

```bash
git clone --depth 1 https://github.com/ctt-gob-es/integra.git integra
git clone --depth 1 https://github.com/ctt-gob-es/fire.git fire
```

Sync pin upstream: `docs/BASELINE.txt`.

## Build Autofirma

```bash
export JAVA_HOME=${JAVA_HOME:-/usr/lib/jvm/temurin-8-jdk-amd64}
export PATH=$JAVA_HOME/bin:$PATH
mvn -B clean install -DskipTests -Denv=install
# Artifact: afirma-simple/target/autofirma.jar
```

Or: `bash scripts/mvp.sh`

## Validate

```bash
make validate
bash scripts/f2-regression.sh
cd tests/validation-harness && mvn -B test -Dvectors.dir=$PWD/../../vectors
```

## Package Linux (optional)

```bash
bash scripts/f6-package-linux.sh
# Open packaging/portal-prueba/index.html to exercise afirma:// after install
```
