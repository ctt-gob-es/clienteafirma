# F5 — JDK 21 como runtime

**Resultado:** el `autofirma.jar` de la línea 1.9.1 **firma correctamente** bajo Temurin JDK **21.0.12.1** (vectores F2 OK el 2026-09-23).

| Aspecto | Estado |
|---------|--------|
| Runtime JDK 21 | OK — `scripts/f2-regression.sh` con `JAVA_HOME=tools/jdk21` |
| Compilación con JDK 21 | OK — `mvn -Denv=install -pl afirma-simple -am package` (bytecode destino sigue en `jdk.version=1.8` del POM) |
| Protocolo `afirma://` | Sin cambio de código; registro en SO = F6 |
| `--release 8` | El POM padre fija `jdk.version=1.8`; no se ha elevando el bytecode de producto en esta fase |

## Cómo reproducir

```bash
export JAVA_HOME=$PWD/tools/jdk21
export PATH=$JAVA_HOME/bin:$PATH
bash scripts/f2-regression.sh
```

Instalador de prueba Linux: ver F6 (`packaging/`).
