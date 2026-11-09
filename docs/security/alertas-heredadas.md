# Alertas heredadas del Cliente @firma oficial

**Fecha:** 2026-10-31  
**Repo:** `Soluciones-Alexendros/clienteafirma-alexendros`  
**Upstream:** `ctt-gob-es/clienteafirma`  
**Inventario de partida:** [`linea-base-2026-10-31.md`](linea-base-2026-10-31.md)  
**Edición normativa de referencia:** OWASP Top 10:2021

Este fork conserva el código de Autofirma oficial. Las alertas de code scanning listadas aquí están en líneas **iguales al oficial** (o en demos/servlets/legado no usados por el instalador Java de escritorio). Se documentan para justificar el cierre `won't fix` en GitHub **después** de que este fichero esté en `master`.

## Criterio de origen

| Origen | Criterio |
|--------|----------|
| **Oficial** | La ubicación (fichero + línea / patrón) coincide con `upstream` en el pin de [`docs/BASELINE.txt`](../BASELINE.txt). |
| **Nuestro** | Introducido o alterado en el fork; se corrige en el programa de alertas (#40–#43, #55, #61, #80, #89, #92, #93). |

No se descartan a mano las copias duplicadas de Semgrep (categoría `merged`): dejan de llegar tras #93.

## Corregidas en el programa (no heredadas)

| Issue | Acción |
|-------|--------|
| #40 | Actions por hash SHA + `ci-gate` |
| #41 | Datos externos de `release.yml` vía `env:` |
| #42 | `ProcessBuilder` con literales en detección de tema |
| #43 | Contención de rutas en `verify.mjs` / Archify |
| #55 / #80 | XXE: entidades y DTD externos desactivados; DOCTYPE interno admitido |
| #61 | Zip Slip: comprobación canónica antes de extraer |
| #89 | Diff Semgrep bloqueante solo para hallazgos **nuevos** graves/altos |
| #92 | `strictSslChecks=true` por defecto; preferencia para desactivar |
| #93 | Una sola subida SARIF de Semgrep |

## Registro de las 42 heredadas (#44–#88 salvo #55, #61, #80)

Recuentos: API abierta en la línea base (sin deduplicar filas internas salvo donde se indique). Mitigación = por qué se queda como el oficial.

### Semgrep OSS

| Issue | Regla | n | Por qué igual que el oficial | Mitigación / alcance |
|-------|-------|--:|------------------------------|----------------------|
| #44 | `command-injection-process-builder` (resto oficial) | ~20 | Sinks con argv fijo o shell ya tratado en PRs previos del fork; el resto coincide con CTT | Argv en lista sin shell; ver [`SEMGREP-TRIAGE.md`](SEMGREP-TRIAGE.md) |
| #45 | `plaintext-http-link` | 23 | Enlaces HTTP en ayuda, demos y docs heredados | Contenido estático; no es canal de firma |
| #47 | `unsafe-reflection` | 21 | Carga dinámica de proveedores/plugins del diseño CTT | Superficie del cliente de escritorio; no entrada web anónima |
| #48 | `use-of-sha1` | 12 | Digests/legacy de formatos de firma | Compatibilidad con documentos y CAs existentes (SEC-2026-008) |
| #49 | `overly-permissive-file-permission` | 9 | Permisos de ficheros de configuración local | Escritorio del usuario; no servicio multi-tenant |
| #50 | `weak-ssl-context` | 6 | Código de contexto SSL legacy del oficial | Preferencia `secureConnections` / #92 endurece el defecto del cliente |
| #53 | `weak-random` | 4 | Series de CA local del instalador | No es material de firma ciudadana |
| #54 | `eval-detected` | 4 | JS de demos/ayuda embebida | Fuera del camino de firma del JAR |
| #56 | `insecure-trust-manager` | 4 | Trust manager permisivo del oficial para modo relajado | Solo con comprobación TLS desactivada (#92); defecto estricto |
| #57 | `csharp … ssrf` | 3 | Cliente Windows Store legado | Fuera del instalador Java de esta release |
| #58 | `crlf-injection-logs` | 3 | Logs de escritorio | Riesgo residual bajo; sin exposición remota típica |
| #59 | `ecb-cipher` | 3 | Cifrado legacy en módulos cipher | Compatibilidad; no algoritmo de firma ciudadana por defecto |
| #63 | `react-unsanitized-method` | 2 | UI web de demos | No empaquetado en el cliente de firma |
| #64 | `permissive-cors` | 2 | Servlets trifásicos / demos | Despliegue de servidor de terceros; no el JAR de escritorio |
| #65 | `no-direct-response-writer` | 2 | Servlets storage/retrieve | Mismo ámbito servidor |
| #66 | `httpservlet-path-traversal` | 2 | Servlets heredados | Backlog servidor; no el protocolo `afirma://` local |
| #67 | `insecure-hostname-verifier` | 2 | Verificador permisivo del modo relajado | Solo con TLS relajado; defecto #92 |
| #68 | `bypass-tls-verification` (Spring) | 2 | Código de ejemplo/servidor | No camino del cliente de escritorio |
| #69 | `des-is-deprecated` | 2 | Algoritmos legacy | Compatibilidad de formatos antiguos |
| #70 | `cbc-padding-oracle` | 2 | CBC en módulos cipher legacy | Mismo alcance legacy |
| #75 | `django-no-csrf-token` | 1 | Ruido Semgrep `auto` en árbol Java | Falso positivo de clasificador |
| #76 | `missing-integrity` | 1 | Script sin SRI en demos | Solo demos HTML |
| #77 | `jdbc-sqli` | 1 | `afirma-miniapplet-store-testdata` | Testdata; no producción |
| #78 | `unencrypted-socket` | 1 | Socket local del protocolo `afirma://` | Loopback; diseño del protocolo |
| #79 | `bypass-tls-verification` (stdlib) | 1 | Misma familia TLS relajado del oficial | Preferencia explícita; defecto estricto (#92) |

### CodeQL

| Issue | Regla | n | Por qué igual que el oficial | Mitigación / alcance |
|-------|-------|--:|------------------------------|----------------------|
| #46 | `js/incomplete-sanitization` | 43 | JS de demos/ayuda | Fuera del JAR de firma |
| #51 | `java/path-injection` | 10 | Rutas en utilidades/servlets CTT | Entrada controlada por flujos de UI o servidor de terceros |
| #52 | `js/xss` | 10 | Demos web | No cliente de escritorio |
| #60 | `java/weak-cryptographic-algorithm` | 6 | Algoritmos legacy de formatos | Compatibilidad documental |
| #62 | `java/static-initialization-vector` | 5 | IV estático en cifrado auxiliar del oficial | No cambia el defecto TLS; ver nota #62/#73/#85 abajo |
| #71 | `java/implicit-cast-in-compound-assignment` | 4 | Estilo numérico heredado | Calidad; sin impacto de autenticación |
| #72 | `js/insecure-randomness` | 4 | JS demos | Fuera del cliente |
| #73 | `java/insecure-trustmanager` | 3 | TrustManager permisivo del oficial | Usado al desactivar comprobación; defecto #92 estricto |
| #74 | `js/redos` | 3 | JS demos | Fuera del cliente |
| #81 | `java/world-writable-file-read` | 1 | Lectura de fichero con permisos amplios en utilidad CTT | Escritorio local |
| #82 | `java/sql-injection` | 1 | Testdata / servlet legado | No producción del instalador |
| #83 | `java/unsafe-hostname-verification` | 1 | Hostname verifier relajado | Solo modo TLS desactivado |
| #84 | `java/xss` | 1 | Respuesta HTML en servlet legado | Ámbito servidor |
| #85 | `java/rsa-without-oaep` | 1 | Padding RSA legacy en utilidades CTT | Compatibilidad; no sustituye firmas XAdES/CAdES/PAdES del usuario |
| #86 | `js/functionality-from-untrusted-source` | 1 | JS demos | Fuera del cliente |
| #87 | `js/xss-through-exception` | 1 | JS demos | Fuera del cliente |
| #88 | `js/xss-through-dom` | 1 | JS demos | Fuera del cliente |

## Nota #62, #73 y #85 (después de #92)

El defecto del cliente **ya comprueba el certificado del servidor** (`strictSslChecks=true`, preferencia `secureConnections`). El código del oficial que desactiva la comprobación, usa TrustManager permisivo o IV/RSA legacy **sigue existiendo** para:

1. La opción de Preferencias que vuelve al comportamiento relajado (aviso claro de riesgo).
2. Utilidades de cifrado/compatibilidad idénticas a CTT.

No se elimina ese código para no diverger del protocolo y de las opciones documentadas. Estas tres issues se documentan aquí y se cierran/`won't fix` **solo** con este documento ya en `master` y con #92 fusionada.

## Cómo descartar en GitHub

1. Confirmar que esta página está en `master` (enlace estable).
2. En cada alerta de code scanning de las reglas de esta tabla: dismiss **Won't fix** con enlace a  
   `https://github.com/Soluciones-Alexendros/clienteafirma-alexendros/blob/master/docs/security/alertas-heredadas.md`
3. Cerrar el issue de seguimiento correspondiente.
4. **No** descartar a mano duplicados Semgrep de categoría `merged`.

## Revisión

Revisar esta tabla en cada sync con upstream (`docs/BASELINE.txt`). Si una línea deja de coincidir con el oficial, pasa a «nuestro» y se abre corrección.
