# Riesgo aceptado: dependencias Python de Semgrep en CI

Fecha de aceptación: 2026-11-03.  
Responsable: Alejandro.  
Ámbito: únicamente `requirements/ci-semgrep.in` y `requirements/ci-semgrep.txt`.

## Decisión

Se aceptan temporalmente cinco avisos medios (diez alertas de Dependabot al
estar duplicados entre el fichero de entrada y el lock). No se puede aplicar la
versión corregida sin romper la resolución de dependencias:

- Semgrep `1.183.0` exige exactamente `mcp==1.29.0`; la corrección es `1.30.0`.
- Semgrep `1.183.0` exige `opentelemetry-api~=1.37.0`; la corrección es `1.45.1`.
- El índice de PyPI consultado el 2026-11-03 no ofrece una versión de Semgrep
  posterior a `1.183.0` que permita actualizar esas dependencias.

`pip-compile --generate-hashes` confirmó ambos conflictos. Por ello se conserva
el lock con hashes, sin forzar versiones incompatibles ni omitir la verificación.

## Justificación y límites

Las dependencias solo se instalan en un runner efímero para ejecutar
`semgrep scan` sobre el código del repositorio. CI no expone un servidor MCP ni
utiliza los transports cliente/servidor afectados, y no recibe trazas
OpenTelemetry de terceros. No hay credenciales de producción ni datos reales en
estos tests. Esto reduce la exposición, pero no elimina la vulnerabilidad de los
paquetes; por eso las alertas se descartan como **riesgo tolerable**, no como
corregidas.

| Alertas `.in` / `.txt` | Paquete | Aviso | Versión corregida | Motivo de aceptación |
| --- | --- | --- | --- | --- |
| 7 / 12 | `mcp 1.29.0` | GHSA-84m7-p3x7-pcfv · CVE-2026-59951 | 1.30.0 | Acumulación de sesiones en StreamableHTTP; no se expone ese servidor en CI. |
| 9 / 14 | `mcp 1.29.0` | GHSA-rwrf-2pqf-9j8j · CVE-2026-116686 | 1.30.0 | Resolución de `$ref` en herramientas MCP; no se consume ese flujo en `semgrep scan`. |
| 10 / 15 | `mcp 1.29.0` | GHSA-5h93-6whr-6q8j · CVE-2026-116685 | 1.30.0 | Redirecciones de transports MCP; no se usan esos clientes en CI. |
| 11 / 16 | `mcp 1.29.0` | GHSA-w4fh-qvv9-3v23 · CVE-2026-116684 | 1.30.0 | Autenticación bearer de servidor MCP; no hay servidor MCP expuesto. |
| 8 / 13 | `opentelemetry-api 1.37.0` | GHSA-xcq7-p3pf-p88h · CVE-2026-114603 | 1.45.1 | Parseo de `tracestate`; CI no recibe trazas de terceros y el runner es efímero. |

## Revisión y salida

Revisar en cuanto Semgrep publique una versión compatible y, como máximo, el
2027-02-03. La excepción se retira al regenerar el lock con `mcp>=1.30.0` y
`opentelemetry-api>=1.45.1`, instalar con `--require-hashes` y ejecutar Semgrep
correctamente. Este registro se incorpora al inventario general de
`docs/RISK-REGISTER.md`.

## Verificación del lock conservado

```bash
pip-compile --generate-hashes \
  --output-file requirements/ci-semgrep.txt requirements/ci-semgrep.in
python3 -m venv /tmp/clienteafirma-semgrep-venv
/tmp/clienteafirma-semgrep-venv/bin/pip install \
  --require-hashes -r requirements/ci-semgrep.txt
/tmp/clienteafirma-semgrep-venv/bin/semgrep --version
```
