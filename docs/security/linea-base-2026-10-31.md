# Línea base de alertas — 2026-10-31

**Repo:** `Soluciones-Alexendros/clienteafirma-alexendros`  
**Commit de master:** `7f1c1ad34` (tras fusionar #38)  
**Java local:** OpenJDK 17.0.21 (POM `jdk.version` = 1.8; CI usa Temurin 8)  
**Edición normativa:** OWASP Top 10:2021 (referencia del programa; este documento es inventario, no auditoría completa)

## Build y tests (comandos de `.github/workflows/ci.yml`)

| Comando | Resultado |
|---------|-----------|
| `make quality` + `mvn -N -B validate` | OK |
| `make test` (`mvn -B -pl afirma-core test`) | OK — 47 tests, 0 fallos, 10 skipped |
| `make test-community` | OK |
| `make build` | OK |
| `cd tests/validation-harness && mvn -B test` | OK — 9 tests |
| `make smoke` | OK |

No había fallos previos en esta batería antes de los cambios del programa de alertas.

## Code scanning abierto (API GitHub, 2026-10-31)

Recuento con `state=open` paginado. Semgrep se deduplica por **regla + fichero + línea** (misma ubicación).

| Herramienta | Abiertas (API) | Deduplicadas (regla+fichero+línea) |
|-------------|----------------|-------------------------------------|
| Semgrep OSS | 195 | 189 |
| CodeQL | 102 | 102 (sin duplicado de categoría) |
| **Total** | **297** | **291** |

Historial #93: en commits anteriores el mismo SARIF de Semgrep se subía dos veces (`category: semgrep` y `category: merged`), lo que llegó a duplicar el total (~390). En esta captura el API ya muestra 195 de Semgrep; siguen 6 pares duplicados por regla+fichero+línea (195→189). Tras #93 debe quedar **una sola categoría** de Semgrep por commit.

### Por regla (recuento API, sin deduplicar filas internas)

Origen según issues #40–#88 y #92–#93: **nuestro** = línea del fork; **oficial** = idéntica a `ctt-gob-es/clienteafirma`.

#### Semgrep OSS

| n | Regla | Origen (issue) |
|---|-------|----------------|
| 50 | `github-actions-mutable-action-tag` | nuestro (#40) |
| 24 | `command-injection-process-builder` | mixto (#42 nuestro; #44 oficial) |
| 23 | `plaintext-http-link` | oficial (#45) |
| 21 | `unsafe-reflection` | oficial (#47) |
| 12 | `use-of-sha1` | oficial (#48) |
| 9 | `overly-permissive-file-permission` | oficial (#49) |
| 6 | `weak-ssl-context` | oficial (#50) |
| 4 | `weak-random` | oficial (#53) |
| 4 | `eval-detected` | oficial (#54) |
| 4 | `documentbuilderfactory-disallow-doctype-decl-missing` | oficial (#55) |
| 4 | `insecure-trust-manager` | oficial (#56) |
| 3 | `csharp ... ssrf` | oficial (#57) |
| 3 | `crlf-injection-logs` | oficial (#58) |
| 3 | `ecb-cipher` | oficial (#59) |
| 2 | `run-shell-injection` | nuestro (#41) |
| 2 | `react-unsanitized-method` | oficial (#63) |
| 2 | `permissive-cors` | oficial (#64) |
| 2 | `no-direct-response-writer` | oficial (#65) |
| 2 | `httpservlet-path-traversal` | oficial (#66) |
| 2 | `insecure-hostname-verifier` | oficial (#67) |
| 2 | `bypass-tls-verification` (spring) | oficial (#68) |
| 2 | `des-is-deprecated` | oficial (#69) |
| 2 | `cbc-padding-oracle` | oficial (#70) |
| 1 | `django-no-csrf-token` | oficial (#75) |
| 1 | `missing-integrity` | oficial (#76) |
| 1 | `jdbc-sqli` | oficial (#77) |
| 1 | `unencrypted-socket` | oficial (#78) |
| 1 | `bypass-tls-verification` (stdlib) | oficial (#79) |
| 1 | `transformerfactory-dtds-not-disabled` | oficial (#80) |
| 1 | `path-join-resolve-traversal` | nuestro (#43) |

#### CodeQL

| n | Regla | Origen (issue) |
|---|-------|----------------|
| 43 | `js/incomplete-sanitization` | oficial (#46) |
| 10 | `java/path-injection` | oficial (#51) |
| 10 | `js/xss` | oficial (#52) |
| 6 | `java/weak-cryptographic-algorithm` | oficial (#60) |
| 6 | `java/zipslip` | oficial (#61) |
| 5 | `java/static-initialization-vector` | oficial (#62) |
| 4 | `java/implicit-cast-in-compound-assignment` | oficial (#71) |
| 4 | `js/insecure-randomness` | oficial (#72) |
| 3 | `java/insecure-trustmanager` | oficial (#73) |
| 3 | `js/redos` | oficial (#74) |
| 1 | `java/world-writable-file-read` | oficial (#81) |
| 1 | `java/sql-injection` | oficial (#82) |
| 1 | `java/unsafe-hostname-verification` | oficial (#83) |
| 1 | `java/xss` | oficial (#84) |
| 1 | `java/rsa-without-oaep` | oficial (#85) |
| 1 | `js/functionality-from-untrusted-source` | oficial (#86) |
| 1 | `js/xss-through-exception` | oficial (#87) |
| 1 | `js/xss-through-dom` | oficial (#88) |

### Código nuestro (graves/medias a corregir en M2–M3)

| Issue | Regla | n API | Acción |
|-------|-------|------:|--------|
| #93 | SARIF Semgrep duplicado | — | Una sola subida |
| #40 | mutable-action-tag | 50 | Pin SHA + ci-gate |
| #41 | run-shell-injection | 2 | `env:` en release.yml |
| #42 | command-injection-process-builder | (subset) | LookAndFeelManager |
| #43 | path-join-resolve-traversal | 1 | verify.mjs |
| #92 | strictSslChecks por defecto | — | Preferencia + tests |

Los títulos de issues no se reescriben hasta que el análisis único de #93 esté en master.

## Notas

- No se descartó ninguna alerta en esta fase.
- Las cifras de referencia post-#93 se repetirán tras el primer análisis con una sola categoría `semgrep`.
