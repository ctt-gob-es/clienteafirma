# Comprobación de detección de proxy sin ini4j

Fecha: 2026-11-03.

`afirma-simple` excluye `org.ini4j:ini4j` de `proxy-vole` para eliminar la
dependencia afectada por CVE-2022-41404. No queda una excepción de riesgo
aceptado para esta dependencia.

## Prueba manual reproducible

Ejecutar desde la raíz del repositorio:

```bash
mvn -B -Denv=install -pl afirma-simple -am \
  -Dtest=ProxyUtilTest -Dsurefire.failIfNoSpecifiedTests=false test
```

La prueba invoca `ProxyUtil.setProxySettings()`, que recorre la detección real
del sistema operativo y del navegador mediante `proxy-vole`. No abre conexiones
de red ni utiliza credenciales. Restaura el `ProxySelector` original al
terminar. Es válida tanto en equipos con proxy como con conexión directa: debe
terminar sin `NoClassDefFoundError` ni otros errores.

Resultado local: **1 test, 0 fallos, 0 errores**. En el equipo de comprobación
no había proxy y el registro fue:

```text
INFO: No se usara proxy para las conexiones HTTP
INFO: No se usara proxy para las conexiones HTTPS
Tests run: 1, Failures: 0, Errors: 0, Skipped: 0
BUILD SUCCESS
```

## Comprobaciones adicionales

```bash
mvn -B -Pautofirma -pl afirma-simple dependency:tree -Dincludes=org.ini4j
mvn -B -Denv=install -pl afirma-simple -am verify
make test
```

Resultados locales: árbol sin `ini4j` (0 coincidencias), `verify` y `make test`
con `BUILD SUCCESS`. El perfil `-Pautofirma` es necesario para el comando de
`dependency:tree`, porque `afirma-simple` no pertenece al perfil `env-dev`
activo por defecto.
