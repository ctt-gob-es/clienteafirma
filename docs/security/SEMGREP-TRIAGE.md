# Triage Semgrep / Code scanning — clienteafirma-alexendros

**Fecha:** 2026-10-25 (hora de Madrid) · **Ámbito:** alertas abiertas en `master` (Semgrep OSS + CodeQL).  
**Propósito:** Clasificar cada regla abierta como `corregir`, `falso positivo (motivo)` o `aceptado (riesgo)`.  
**Conteo contrastado (API GitHub, 25/10/2026):** Semgrep OSS ~200+; CodeQL ~100+ (1 critical `java/xxe`). Dependabot abiertas: 0. Renovate es el único bot (no hay `dependabot.yml`; SEC-004 / `.github/renovate.json`).

No se cierran alertas en la pestaña Security con dismiss manual: las de `corregir` se cierran por código en PRs posteriores.

## Leyenda

| Etiqueta | Significado |
|----------|-------------|
| `corregir` | Se parchea en esta ronda (PR `fix/xxe-*` / `fix/process-builder-*`) |
| `falso positivo (motivo)` | Semgrep marca código sin shell/inyección real |
| `aceptado (riesgo)` | Hallazgo real o deuda; justificado; fuera de ciclo o compatibilidad |

## Tabla por regla Semgrep (abiertas)

| Regla | n (orden) | Clasificación | Motivo / acción |
|-------|-----------|---------------|-----------------|
| `command-injection-process-builder` | ~25 | **mixto** (ver sección abajo) | Sinks con shell → `corregir`; argv fijo → `falso positivo` |
| `documentbuilderfactory-disallow-doctype-decl-missing` | 1–4 | `corregir` | Cubierto por `fix/xxe-xmldsig` (`Utils` → `SecureXmlBuilder`) |
| `transformerfactory-dtds-not-disabled` | 1–2 | `corregir` | Cubierto por `fix/xxe-xmldsig` (`SecureXmlTransformer` en plugin hash) |
| `csharp.lang.security.ssrf.web-request.ssrf` | 3 | `aceptado (riesgo)` | Windows Store legado; URL del protocolo de firma; fuera del instalador Java de esta release |
| `use-of-sha1` | ~9–12 | `aceptado (riesgo)` | Compatibilidad formatos / digests legacy; política SEC-2026-008 |
| `weak-random` | 4 | `aceptado (riesgo)` | Serie de CA local de instalador, no material de firma ciudadana |
| `github-actions-mutable-action-tag` | ~50 | `aceptado (riesgo)` | Deuda CI; pin parcial SEC-2026-005 |
| `plaintext-http-link` | ~19–23 | `aceptado (riesgo)` | Enlaces HTTP en docs/HTML de ayuda y demos |
| `unsafe-reflection` | ~12–21 | `aceptado (riesgo)` | Carga dinámica de proveedores/plugins; diseño del cliente |
| `overly-permissive-file-permission` | ~6–9 | `aceptado (riesgo)` | Permisos de ficheros de configuración local |
| `eval-detected` (JS) | 4 | `aceptado (riesgo)` | Demos / ayuda embebida |
| `crlf-injection-logs` | 3 | `aceptado (riesgo)` | Logs de escritorio; riesgo residual bajo |
| `permissive-cors` | 2 | `aceptado (riesgo)` | Servlets trifásicos / demos |
| `no-direct-response-writer` | 2 | `aceptado (riesgo)` | Servlets de almacenamiento/retriever |
| `httpservlet-path-traversal` | 2 | `aceptado (riesgo)` | Mismo ámbito servlets; backlog CodeQL path-injection |
| `react-unsanitized-method` | 2 | `aceptado (riesgo)` | UI web de demos |
| `missing-integrity` | 1 | `aceptado (riesgo)` | Script sin SRI en demos |
| `jdbc-sqli` | 1 | `aceptado (riesgo)` | `afirma-miniapplet-store-testdata` (testdata, no producción) |
| `weak-ssl-context` / `insecure-trust-manager` / `bypass-tls-verification` | varios | `aceptado (riesgo)` | TLS opt-in / `disableSslChecks`; no endurecer por defecto sin preferencia |
| `run-shell-injection` (Actions) | 3 | `aceptado (riesgo)` | Deuda CI |
| `ecb-cipher` / `des-is-deprecated` / `cbc-padding-oracle` | varios | `aceptado (riesgo)` | Cifrado legacy / módulos cipher; backlog CodeQL high |
| `django-no-csrf-token` | 1 | `falso positivo (motivo)` | Falso positivo en árbol Java/desktop (ruido Semgrep `auto`) |
| `path-join-resolve-traversal` (JS) | 1 | `aceptado (riesgo)` | Demo / portal heredado |
| `unencrypted-socket` | 1 | `aceptado (riesgo)` | Socket local del protocolo `afirma://` |

## Command-injection ProcessBuilder (detalle)

**Regla:** `java.lang.security.audit.command-injection-process-builder.command-injection-process-builder`

### `corregir` (PRs `fix/process-builder-*`)

| Módulo | Sink | Motivo |
|--------|------|--------|
| `afirma-simple` | `LookAndFeelManager.query` → `Runtime.exec(String)` | Shell implícito con cadena |
| `afirma-simple` | `RestoreConfigLinux` → `/bin/bash -c` | Shell explícito |
| `afirma-simple` | `ShellScript` / `AppleScript` → `do shell script "..."` | Interpolación en osascript |
| `afirma-ui-simple-configurator` | `WindowsCmdExecutor` → `cmd.exe /c` | Shell Windows |
| `afirma-ui-simple-configurator` | `ConfiguratorMacOSX` (osascript / security con CN) | Shell / args interpolados |
| `afirma-core-keystores` | `KeyStoreUtilities` → `cmd.exe /c for %f...` | Concatenación de ruta |
| `afirma-ui-core-jse-keystores` | `CertificateUtils` → `cmd /C start` y `do shell script` | Shell + path |
| `afirma-keystores-mozilla` | `ShellScript` (apple) | Mismo patrón osascript |

### `falso positivo (motivo)`

| Módulo | Sink | Motivo |
|--------|------|--------|
| `afirma-simple` | `DesktopUtil` (`wmic`, `reg` en lista) | Argv fijo, sin shell |
| `afirma-simple` | `MainMenu` / `PluginsManagementHandler` (reinicio app vía lista) | Argv de rutas de despliegue, sin shell |
| `afirma-simple` | `RestoreConfigWindows` (`tasklist.exe` path fijo) | Sin shell |
| `afirma-simple` | `RestoreConfigFirefox` (`certutil` argv lista) | Sin shell; perfiles locales |
| `afirma-simple` | `RestoreConfigMacOSX` líneas con `ps` / argv lista fija | Sin shell |
| `afirma-ui-simple-configurator` | `ConfiguratorFirefox*` certutil lista | Sin shell |
| `afirma-ui-core-jse` | `AWTUIManager` osascript con script fijo de foco | Script constante |
| `afirma-simple-plugin-hash` | `HashPlugin` BAT empaquetado | Path del propio plugin |

## CodeQL (familias high / critical)

| Regla | Clasificación | Motivo |
|-------|---------------|--------|
| `java/xxe` (CustomUriDereferencer) | `corregir` | PR `fix/xxe-xmldsig` |
| `java/zipslip`, `java/path-injection` | `aceptado (riesgo)` | Backlog; fuera de esta ronda |
| `java/insecure-trustmanager` | `aceptado (riesgo)` | TLS opt-in; no cambiar default |
| `java/weak-cryptographic-algorithm`, `java/static-initialization-vector`, `java/rsa-without-oaep` | `aceptado (riesgo)` | Módulos cipher / enveloper legacy |
| `java/sql-injection` (testdata) | `aceptado (riesgo)` | Solo `afirma-miniapplet-store-testdata` |
| `java/xss`, `js/*` en help/demos | `aceptado (riesgo)` | Ayuda embebida y demos de applet |
| `java/world-writable-file-read`, `java/implicit-cast-in-compound-assignment` | `aceptado (riesgo)` | Calidad / local; fuera de ciclo |

## Criterio de salida de este documento

- Cada regla Semgrep abierta tiene etiqueta.
- Las filas `corregir` tienen PR de código en la ronda (XXE + ProcessBuilder).
- No se usa dismiss manual en GitHub para cerrar alertas.
- Cierre de Dependabot critical ya hecho (#23, BouncyCastle 1.86).
