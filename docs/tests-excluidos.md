# Tests excluidos o saltados en CI

Listado de pruebas que no se ejecutan o se saltan en CI, con motivo.
Generado en T3 de la OT Fase 1.

## Módulos incluidos en `make test`

`afirma-core`, `afirma-core-keystores`, `afirma-core-massive` y todos los `afirma-crypto-*` del reactor (ver `Makefile`, variable `TEST_MODULES`).

## Tests que se saltan en CI (marcados con `@Ignore` o condicionales de entorno)

| Módulo | Test / clase | Motivo |
|--------|--------------|--------|
| `afirma-core` | `TestDataDownloader`, `TestHttpConnection`, `TestZipForData` | Requieren red/endpoint HTTP; no disponible en CI sin red. |
| `afirma-core` | `TestConstants` | Comprobaciones de constantes de build/entorno. |
| `afirma-core` | `TestHttpConnection` | Red. |
| `afirma-core` | `XXESecurityTest` (parcial) | Algunos casos dependen del parser JDK. |
| `afirma-core-keystores` | `TestP` y almacenes DNIe/PKCS#11 | Requieren hardware (DNIe, PKCS#11) o certificados del sistema. |
| `afirma-crypto-cadestri-client` | `TestCAdES`, `TestCadesTriphase`, `TestTriphaseCountersign` | Requieren servicio trifásico de red. |
| `afirma-crypto-padestri-client` | `TestPdfTriphase` | Requiere servicio trifásico de red. |
| `afirma-crypto-xades` | `TestJavaBug8182580` | Depende de bug específico de JDK. |
| `afirma-keystores-filters` | `TestRFC2254CertificateFilter.TestRFC2254CertificateRecursiveFilter` | Requiere almacén de claves del sistema (CAPI/Apple/NSS); se omite con `Assume` cuando no hay almacén real (CI). |

## Módulos no incluidos en `make test`

| Módulo | Motivo |
|--------|--------|
| `afirma-crypto-cipher` (v1) | Sustituido por `afirma-crypto-cipher2`; no es módulo del reactor. |
| `afirma-crypto-cms-enveloper` | No es módulo del reactor raíz. |
| `afirma-crypto-jarverifier` | No es módulo del reactor raíz. |
| `afirma-keystores-*`, `afirma-ui-*`, `afirma-server-*`, `afirma-simple*` | Requieren entorno de escritorio/servidor; se validan en `smoke`/`community`. |

## Certificados en tests

Los tests que necesitan certificado lo generan en el propio test (BouncyCastle/keytool en tiempo de test). No se versionan certificados ni claves reales (regla 6 de la OT).

## Fallos corregidos (no excluidos)

| Módulo | Test | Causa | Arreglo |
|--------|------|-------|---------|
| `afirma-crypto-ooxml` | `TestOOXML.testSignature`, `TestOOXML.testCoSignature`, `TestOOXMLVersions.testSignature` | La firma OOXML consultaba la pantalla (número de monitores, resolución y profundidad de color) y fallaba con `HeadlessException` en equipos sin pantalla (CI, servidores). Es un fallo real: también rompería la firma en un servidor. | `OOXMLOfficeObjectHelper` usa valores neutros (1 monitor, 1920x1080, 32 bits) cuando `GraphicsEnvironment.isHeadless()`. |

Verificado el 2026-11-02: los tres módulos (`afirma-crypto-validation`, `afirma-crypto-xadestri-client`, `afirma-core-massive`) pasan en CI (PR 123) y en local (`mvn -B -ntp -Djava.awt.headless=true test`).
