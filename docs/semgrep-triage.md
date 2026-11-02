# Semgrep — triaje de hallazgos

Fecha: 2026-11-02 · Herramienta: `semgrep scan --config=auto --config=p/ci`
(semgrep 1.183.0, fijado por hash en `requirements/ci-semgrep.txt`)

## Contexto: el escaneo llevaba tiempo sin ejecutarse

Hasta el 2026-11-02 el job `Semgrep` terminaba en verde **sin analizar nada**:
`pip3 install semgrep` (sin versión) instalaba una versión incompatible con el
Python del runner (3.15) y el paso terminaba en error, pero el workflow lo ocultaba
con `|| true`. Resultado: 0 hallazgos, 0 alertas y una falsa sensación de cobertura.

Con el pin por hash (`requirements/ci-semgrep.txt`) y `python-version: "3.14"` el
escaneo funciona: **143 hallazgos** sobre 2.607 ficheros. Este documento los clasifica.
Ver también [`scorecard.md`](scorecard.md).

## Qué queda fuera del análisis (`.semgrepignore`)

Código heredado del proyecto oficial que **no se compila ni se publica** desde este
repositorio, o recursos de terceros empaquetados:

| Ruta excluida | Motivo |
|---|---|
| `afirma-ui-applet-deploy/`, `afirma-ui-miniapplet-deploy/`, `afirma-standalone/` | Applets y despliegue JNLP del cliente antiguo; no están en el reactor Maven. |
| `afirma-miniapplet-store-testdata/` | Servlets de datos de prueba de los applets. |
| `afirma-windows-store/` | Aplicación Windows Store; no es el producto Linux/macOS publicado. |
| `afirma-simple-installer/` | Recursos de instalador (scripts y ayuda), no código de aplicación. |
| `**/src/main/resources/help/` | Ayuda HTML empaquetada (enlaces y licencias). |

## Clasificación por regla

Leyenda: **FP** = falso positivo técnico · **RA** = riesgo aceptado por diseño ·
**HP** = hallazgo real, *hardening pendiente* · **EX** = excluido por `.semgrepignore`.

| Regla | Nº | Clase | Justificación |
|---|---|---|---|
| `command-injection-process-builder` | 14 | FP | `ProcessBuilder(List<String>)` + `start()`: no hay intérprete de shell. Los comandos son literales o se componen con constantes (`osascript -e`, `tasklist.exe`, `wmic`, `certutil`). Semgrep marca cualquier `ProcessBuilder` con argumentos dinámicos. |
| `unsafe-reflection` | 12 | RA | Carga de plugins y de gestores de almacén por reflexión a partir de la configuración **local** del usuario (diseño del producto). No hay entrada remota. Endurecimiento propuesto: lista blanca de clases. |
| `crypto.use-of-sha1` | 8 | RA | SHA-1 exigido por formatos heredados: `CertID` de OCSP (RFC 6960), huellas de certificados y nombres de fichero temporales. El único uso sobre datos a firmar es el modo «XAdES explícita», ya marcado como obsoleto con aviso en log y pendiente de retirada. |
| `overly-permissive-file-permission` | 6 | HP | `addAllPermissionsToFile` aplica `0777` a scripts auxiliares de macOS. Real: reducir a `0700`/`0755` según si es ejecutable y revisar si el script se ejecuta como otro usuario. |
| `weak-random` | 4 | HP | Seriales de certificados autofirmados locales y nombres de fichero generados con `java.util.Random`. Real, corrección barata: `SecureRandom`. Pendiente de test que lo cubra. |
| `crlf-injection-logs` | 3 | FP | Se registra en log un identificador ya saneado (`getTrim`) o un literal; no hay escritura de log con salto de línea controlado por el atacante. |
| `permissive-cors` | 2 | RA | Servidores **locales** de almacenamiento/recuperación de firmas (`localhost`) que deben aceptar cualquier origen para que el navegador pueda invocarlos. Restringir el origen rompería el protocolo `afirma://`. |
| `no-direct-response-writer` | 2 | FP | Escritura directa en la respuesta de un servlet que devuelve texto plano o XML ya generado por el propio servicio, no HTML con datos de usuario. |
| `httpservlet-path-traversal` | 2 | FP | `RetrieveService` compone la ruta con `composeTargetFile(...)`, que valida y lanza `SecurityException` si el destino sale del directorio configurado (control explícito justo debajo). |
| `weak-ssl-context` | 1 | RA | `SecureSocketUtils` construye el contexto TLS del socket local del protocolo; la política estricta se controla con `strictSslChecks` (activada por defecto). |
| `eval-detected`, `react-unsanitized-method`, `django-no-csrf-token`, `missing-integrity`, `plaintext-http-link` | 4+2+1+1+19 | EX | Todos en recursos web heredados (applets, demo, ayuda HTML). Ver tabla de exclusiones. |
| `ssrf` | 3 | EX | `afirma-windows-store` (no publicado). |
| `jdbc-sqli` | 1 | EX | Servlet de datos de prueba de los applets. |
| `renovate-missing-minimum-release-age` | 4 | Arreglado | `minimumReleaseAge` añadido a las reglas de `packageRules` con `automerge: true`. |

## Hardening pendiente (seguimiento)

1. `weak-random`: usar `SecureRandom` en los dos `CertUtil` (seriales de certificado).
2. `overly-permissive-file-permission`: permisos mínimos en los scripts auxiliares.
3. `unsafe-reflection`: lista blanca de clases para plugins y almacenes.

Ninguno bloquea la release: son mejoras acotadas con test propio. Cuando se
arreglen, la alerta se cierra sola en el siguiente escaneo.

## Cómo revisar

```bash
gh api "repos/Soluciones-Alexendros/clienteafirma-alexendros/code-scanning/alerts?state=open&per_page=100" \
  -q '.[] | select(.tool.name=="Semgrep OSS") | "\(.rule.id)\t\(.most_recent_instance.location.path)"'
```

Toda alerta abierta debe estar en esta tabla con su clase y su justificación.
