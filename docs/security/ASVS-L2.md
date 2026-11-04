# Matriz ASVS L2 y modelo de amenazas — cliente @firma comunitario

**Fecha:** 2026-11-03
**Base:** `master` en `e01bbf7d3578a4edba278e1f7629e44667458bc2`
**Norma:** OWASP ASVS 4.0.3, Nivel 2
**OT:** OT Fase 2 · Seguridad ASVS L2 (tarea T2)
**Estado:** matriz y modelo de amenazas iniciales; las brechas se cierran en las tareas T3–T19.

Este documento es la fuente de verdad del alcance ASVS L2 de la fase 2. Cada fila indica el
estado real en el árbol, con fichero y línea. Una brecha **no** se marca como cerrada hasta que
existe una prueba que lo demuestre.

## 1. Alcance

El producto es una aplicación de escritorio Java (Swing) con:

- protocolo propio `afirma://` invocado desde el navegador;
- canal local por socket TLS y por websocket;
- carga de plugins desde JAR elegidos por el usuario;
- firma CAdES/PAdES/XAdES/ODF y servicios trifásicos de cliente;
- un WAR trifásico **de laboratorio**, no desplegado.

**Fuera de alcance:** el WAR trifásico (`afirma-server-triphase-*`) se trata como entorno de
pruebas F8, no expuesto a red no confiable. Si se desplegara, habría que reabrir V4 y V13 sobre él.
Tampoco entran E6 (empaquetado Linux), E7 (accesibilidad) ni E8 (gobernanza y upstream), que viven
en sus propias fases.

## 2. Matriz de cumplimiento por capítulo

| Capítulo ASVS 4.0.3 | Aplica | Estado | Brecha / tarea |
| --- | --- | --- | --- |
| V1 Arquitectura y modelado de amenazas | Sí | En curso | Sin modelo de amenazas previo; este documento lo abre (T2) |
| V2 Autenticación | Sí | Pendiente de reproducción | Token trifásico sin verificar firma/EKU/cadena (H5, T7) |
| V2.5 Credenciales y secretos | Sí | Cumple con límite | El PFX local ya no usa contraseña fija: secreto aleatorio por usuario en fichero 0600 y migración de almacenes heredados (T4). Límite: instalador y almacenes de solo lectura |
| V3 Gestión de sesión | Sí | Brecha parcial | `idsession` opcional; origen del websocket sin validar (T8) |
| V4 Control de acceso | No aplica | Fuera de alcance | Cliente monousuario; WAR trifásico no expuesto |
| V5 Validación de entrada | Sí | Mitigado sin gate | XXE cerrado en sinks de usuario; falta anti-regresión (T14) |
| V6 Criptografía | Sí | Brecha confirmada | DES/ECB por defecto, PBE MD5/SHA1, SHA-1 en ODF, SpongyCastle (T9, T10, T15, T16) |
| V7 Errores y logging | Sí | Brecha confirmada | `printStackTrace` y trazas de PIN (T12) |
| V8 Protección de datos | Sí | Brecha parcial | Permisos del PFX y del secreto corregidos a solo el usuario (T4); queda `readObject` de `PdfSignResult` (T14) |
| V9 Comunicaciones | Sí | Brecha parcial | Canal local ya en TLS 1.2 con suites filtradas (T3); quedan `DUMMY_TRUST` y descarga de configuración (T5) y TSA (T6) |
| V10 Código malicioso | Sí | Brecha confirmada | Plugins por `URLClassLoader` sin firma ni huella (T13) |
| V11 Lógica de negocio | No aplica | N/A | Sin lógica multiusuario ni antifraude |
| V12 Archivos y recursos | Parcial | Brecha confirmada | Nombres temporales con `Random`; permisos (T11) |
| V13 API y servicios web | Parcial | Brecha confirmada | Respuesta con `Access-Control-Allow-Origin: *` (T8) |
| V14 Configuración | Sí | Brecha parcial | Preferencia que debilita TLS y modo permisivo (T5) |

Criterio de cierre: cada fila pasa a «Cumple» solo con una prueba ejecutable en el repositorio
(`make validate`, `make test` o un gate dedicado) y evidencia citada en esta tabla.

### 2.1 Secreto del almacén del canal local (T4)

Brecha cerrada: `autofirma.pfx` se protegía con la contraseña fija `654321`, publicada en el
código y en los instaladores. Estado actual:

| Elemento | Comportamiento |
| --- | --- |
| Secreto | 32 bytes de `SecureRandom` codificados en Base64, generados en el primer arranque que necesita el canal |
| Ubicación | `autofirma.pfx.secret` en el mismo directorio que el almacén |
| Creación | Atómica: el temporal nace en 0600 (POSIX) o con ACL de solo el propietario (Windows) antes de escribir el secreto, y se mueve con `ATOMIC_MOVE` |
| Permisos | 0600 en Linux y macOS; ACL con una sola entrada `ALLOW` para el propietario en Windows. Si el secreto aparece con permisos abiertos se corrige y se registra un aviso (sin el valor) |
| Uso | `SecureSocketUtils` carga la contraseña desde el fichero y la borra de memoria (`Arrays.fill`) al terminar; no se registra ni aparece en excepciones |
| Compatibilidad | Si el almacén solo abre con la contraseña heredada se vuelve a cifrar con el secreto: copia `.bak`, escritura atómica, verificación (carga y recuperación de clave) y borrado del `.bak`. La contraseña heredada solo vive como constante de migración |
| Reinstalación | Si el instalador regenera el almacén con la contraseña heredada y el secreto ya existe, se reutiliza el secreto y se vuelve a migrar |
| Sin permiso de escritura | En una instalación por máquina cuyo directorio no es escribible por el usuario (caso típico: `C:\\Program Files\\Autofirma` en Windows) no se puede reescribir el almacén: se mantiene la contraseña heredada y se registra un aviso de riesgo. Condición de salida: mover el almacén a un directorio por usuario o conceder escritura en la instalación |

La generación del almacén en el instalador y en el configurador (`KS_PASSWORD`) queda como deuda
de esa condición de salida: el instalador se ejecuta elevado y una ACL de solo el usuario
instalador dejaría el canal inoperante para el resto de usuarios de la máquina.

Evidencia: `afirma-core-keystores/src/main/java/es/gob/afirma/keystores/LocalKeyStoreSecrets.java`,
pruebas `LocalKeyStoreSecretsTest` (7 casos, incluido el saludo TLS real) que corren en `make test`,
y comprobación de migración en la ruta del producto en `scripts/interop/tls-probe.sh`.

## 3. Activos y actores

**Activos.** Clave privada del ciudadano en el almacén del sistema (DNIe, PKCS#11, NSS, CAPI,
fichero PKCS#12); PIN; documento que se firma; firma generada; configuración remota de la
aplicación; PFX del canal local (`autofirma.pfx`) y su contraseña; suite de plugins instalados.

**Actores.** Ciudadano que opera el cliente; sede electrónica que invoca `afirma://`; atacante
local sin privilegios; sitio web malicioso abierto en el mismo navegador; atacante en la red
(MitM); JAR de plugin de origen no confiable; servidor remoto comprometido (configuración, TSA,
`stservlet`).

## 4. Límites de confianza

| Límite | Qué cruza | Confianza |
| --- | --- | --- |
| Navegador → `afirma://` | URI con parámetros (URLs, documento, algoritmo, sesión) | No confiable: cualquier web puede invocarlo |
| Navegador → websocket/socket local | Mensajes de operación y `idsession` | Semiconfiable: se filtra por IP, no por origen web |
| Red → cliente | Configuración remota, TSA, `stservlet`, CRL/OCSP | No confiable sin validación TLS |
| Disco → cliente | JAR de plugins, almacenes, documentos | No confiable sin verificación de integridad |
| Cliente → red | Peticiones HTTP/HTTPS con datos del documento | Saliente; riesgo de exfiltración |

## 5. Trazas entrada→sink

Cada flujo se traza desde el dato que controla un tercero hasta el punto donde produce efecto.
Columna `→ Sink` = punto exacto de efecto; `Control` = lo que hoy lo frena; `Brecha` = tarea.

| # | Entrada | Traza | → Sink | Control actual | Brecha |
| --- | --- | --- | --- | --- | --- |
| F1 | URI `afirma://` abierta por una web | `ProtocolInvocationUriParser` → `UrlParameters.validateURL` (`afirma-core/.../protocol/UrlParameters.java` L427–456) → `ProtocolInvocationLauncher.launch` | → Sink: Operación de firma sobre el almacén de claves | Esquema http/https obligatorio, sin parámetros en la URL del servlet, host local bloqueado (`LocalAccessRequestException`) | T2, T8 |
| F2 | URL de `stservlet`/recurso en los parámetros | `UrlHttpManagerImpl.readUrl` (`afirma-core/.../misc/http/UrlHttpManagerImpl.java` L214–226) → `SslSecurityManager` | → Sink: Conexión TLS saliente y envío del documento | `strictSslChecks=true` por defecto; modo permisivo si se pide explícitamente | T5 |
| F3 | Configuración remota | `ConfigUpdaterManager.readFromUrl` (`afirma-ui-simple-configurator-common/.../ConfigUpdaterManager.java` L118) → `DataDownloader.downloadData(filePath, false, true)` (`afirma-core/.../misc/http/DataDownloader.java` L57, L77–82) | → Sink: Descarga HTTPS sin validar el certificado | **Ninguno**: `ignoreSSLSecurity=true` aunque `strictSslChecks=true` | T5 |
| F4 | Mensaje al websocket local | `AfirmaWebSocketServerV4Sup.onMessage` (`afirma-simple/.../protocol/AfirmaWebSocketServerV4Sup.java` L64–103) → `ProtocolInvocationLauncher.launch` | → Sink: Operación de firma | `isLocalAddress` solo acepta `127.0.0.1` (L121–123) e `idsession` (L78–83); sin validar `Origin` | T8 |
| F5 | Petición HTTP al socket TLS local | `ServiceInvocationManager.startService` → `CommandProcessorThread.run` (`afirma-simple/.../protocol/CommandProcessorThread.java` L92–134) | → Sink: Respuesta con el resultado de la operación | TLS 1.2 con suites ECDHE/DHE sin SHA-1 (T3), `setEnabledProtocols`, suites filtradas por soporte; comprobación de host en L164–165 (`127.0.0.1`, `::1`) e `idsession`; respuesta con `Access-Control-Allow-Origin: *` (L510) | T8 |
| F6 | Documento XML de hashes del usuario | `XmlHashDocument.load` (`afirma-simple-plugin-hash/.../XmlHashDocument.java` L151) | → Sink: Parser XML (DOM) | `SecureXmlBuilder.getSecureDocumentBuilder()` | T14 |
| F7 | OOXML del usuario | `ContentTypeManager.getDocumentBuilder` (`afirma-crypto-ooxml/.../ContentTypeManager.java` L93) | → Sink: Parser XML del `[Content_Types].xml` | `SecureXmlBuilder` | T14 |
| F8 | Bytes de cliente al trifásico de laboratorio | `XAdESTriPhaseSignerUtil.getDocumentFromBytes` (`afirma-server-triphase-signer-core/.../XAdESTriPhaseSignerUtil.java` L406–409) | → Sink: Parser XML del servidor | `SecureXmlBuilder`; WAR no expuesto | T14 |
| F9 | JAR de plugin elegido por el usuario | `PluginLoader.loadPlugin` (`afirma-simple/.../plugins/manager/PluginLoader.java` L49–60) | → Sink: Ejecución de código en el proceso del cliente | Consentimiento del usuario en la UI; sin firma ni huella | T13 |
| F10 | PDF del usuario | `AOPDFSigner` (`afirma-crypto-pdf/.../pades/AOPDFSigner.java`) sobre `afirma-lib-itext:1.7` | → Sink: Análisis y escritura del PDF firmado | Librería heredada; sin vectores negativos de PDF incremental | T15, T18 |

Inventario de parsers XML en `master` (código principal, sin tests): `DocumentBuilderFactory.newInstance` 5,
`SAXParserFactory` 6, `XMLInputFactory` 0, `TransformerFactory` 6; `SecureXmlBuilder` se usa en 28 ficheros.
Es el punto de partida del gate de T14.

## 6. Usos aceptables de la desactivación SSL

Hay dos llamadas que desactivan la validación TLS con un propósito legítimo y **no** deben entrar
en el criterio de bloqueo de T5:

| Fichero | Línea | Para qué |
| --- | --- | --- |
| `afirma-core/src/main/java/es/gob/afirma/core/misc/http/SSLErrorProcessor.java` | L393 | Descargar los certificados del servidor para que el usuario los importe y confíe en ellos |
| `afirma-ui-simple-configurator-common/.../ImportCertificatesDialog.java` | L253 | Lo mismo desde el configurador, con acción explícita del usuario |

El criterio de T5 sigue siendo: **ninguna vía desactiva la validación cuando `strictSslChecks=true`**,
salvo estas dos, que solo obtienen el certificado del servidor y no transportan el documento.

## 7. Excepciones documentadas

| Excepción | Motivo | Condición de salida |
| --- | --- | --- |
| SHA-1 permitido en ODF | Compatibilidad con firmas ODF heredadas (`AOSignConstants.isSHA1AllowedForFormat`, L439–446) | Se mantiene mientras existan documentos ODF firmados con SHA-1 en circulación; revisar en cada fase |
| SHA-1 prohibido en XAdES, CAdES y PAdES | `AOSignConstants.validateSHA1ForFormat` (L454) lanza `IllegalArgumentException`; cubierto por `SHA1RejectionTest` | Cierre verificado por `make test` (T10) |
| Canal local sin TLS 1.3 | El objetivo de compilación es Java 8, que no soporta TLS 1.3, y sus suites forman un conjunto distinto del de TLS 1.2 | Reabrir si se eleva el objetivo mínimo a Java 11 (T3) |
| WAR trifásico fuera de alcance | Laboratorio F8, no desplegado | Reabrir V4/V13 si se expone |

## 8. Riesgo residual registrado fuera de esta matriz

| ID | Riesgo | Estado | Referencia |
| --- | --- | --- | --- |
| SEC-009 / H15 | Plugins sin firma; gobernanza en una sola persona | En tratamiento (T13) y aceptado (H15) | `docs/RISK-REGISTER.md` |
| H4 | TSA y TLS posiblemente insuficientes | Pendiente de reproducción (T6) | `docs/RISK-REGISTER.md` |
| H5 | Token trifásico sin validar | Pendiente de reproducción (T7) | `docs/RISK-REGISTER.md` |
| H6 | Canal local: loopback, TLS y sesión | Pendiente de reproducción (T8) | `docs/RISK-REGISTER.md` |
| SEC-010 | Almacén del canal local en instalación por máquina sin escritura: se mantiene la contraseña heredada con aviso | Abierto, con condiciones de salida en 2.1 (T4) | `docs/security/ASVS-L2.md` |

Ninguna de las hipótesis H4–H6 se presenta aquí como vulnerabilidad verificada.

## 9. Cómo se comprueba este documento

```bash
test -f docs/security/ASVS-L2.md && grep -c '→ Sink' docs/security/ASVS-L2.md   # >= 4 flujos
make validate
make interop-selftest   # T3b: scripts de interoperabilidad (sin red ni docker)

grep -rn 'KSPASS\|CTPASS' afirma-simple/src/main/java/es/gob/afirma/standalone/protocol/SecureSocketUtils.java  # sin resultados (T4)
mvn -B -pl afirma-core-keystores -am test -Dtest=LocalKeyStoreSecretsTest    # secreto, 0600 y socket TLS (T4)
bash scripts/interop/tls-probe.sh                                          # A1 + migración y 0600 en la ruta real (T4)
```

El job `interop` (`.github/workflows/interop.yml`, tarea T3b) comprueba en cada PR el
endurecimiento TLS del canal local, la compatibilidad de Firefox, Chromium y Edge con las
suites habilitadas, y la validación con EU DSS de una firma hecha con certificado sintético.

La firma completa **desde el navegador** (diálogo de confirmación de la GUI de Autofirma) no se
automatiza: sigue siendo prueba manual antes de publicar cada versión (`make interop-local`).

La matriz debe actualizarse en la misma tarea que cierre cada brecha: un cambio de estado sin
prueba asociada es un error de este documento.
