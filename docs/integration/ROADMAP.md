# Hoja de ruta de integración — Autofirma comunitaria

**Fecha de refresco:** 2026-11-03 · **Versión:** 3.1
**Repositorio canónico:** [Soluciones-Alexendros/clienteafirma-alexendros](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros), rama `master`.

Este documento recoge solo lo que se puede comprobar hoy en `master` o en GitHub. Cada punto marcado como hecho tiene al final una comprobación concreta. Si algo no se puede comprobar, no se da por hecho. El tablero diario está en [TASKS.md](../TASKS.md).

---

## Actualización OT Fase 1 · Cierre (2026-11-03)

La fuente detallada de esta ejecución es [`docs/informe-fase2.md`](../informe-fase2.md). Las secciones posteriores conservan el inventario histórico del 2026-11-01; esta actualización prevalece cuando difieran.

| Etapa | Estado comprobado | Referencia |
| --- | --- | --- |
| E0.1 · Contención de release histórica | Notas y comando preparados; Alejandro aún no los ejecuta | PR #141 |
| E0.3 · Registro de riesgos | H1–H15, ini4j, Dependabot, Scorecard y BinaryArtifacts registrados | PR #140, `docs/RISK-REGISTER.md` |
| E1.1–E1.3 · Gates bloqueantes | Implementados; PR rojo #145 demostró SBOM y `ci-gate` en fallo | PR #144, run 41653635499 |
| E1.4 · Huella Semgrep | Huella de contenido con tests de desplazamiento y cambio | PR #142 |
| E1.5 · Comprobación documental | `check-docs-truth.sh` y notas derivadas del inventario | PR #143 |
| E2.1/E2.2/E2.4/E2.6 · Release fiable | Implementada; firma real pendiente de secreto y entorno protegido | PR #146 |
| E2.7 · Notas = assets | Generador/validador implementado y conectado al workflow de T5 | PR #143 y #146 |
| Retirada ini4j | Exclusión implementada y prueba de proxy verde | PR #138 |
| Dependencias Python CI | Actualización incompatible con el pin de Semgrep; riesgo aceptado y 0 alertas abiertas | PR #139, `docs/dependabot-ci.md` |
| Cierre de la OT | Pendiente de fusiones, release firmada de prueba y PR final | No completado |

E3, E4.2 y E5 continúan fuera de alcance. No se declara ninguna release nueva como validada ni libre de SpongyCastle.

---

## Resumen del estado

| ID | Qué es | Estado real | Dónde se ve |
|----|--------|-------------|-------------|
| P0 | Alineación del repositorio y CI mínima | Hecho | PR #1, `ci.yml` |
| P1-BC | Migración de SpongyCastle a BouncyCastle 1.86 | Hecho en el fork | PR #3 y subida a 1.86. PR al oficial [#573](https://github.com/ctt-gob-es/clienteafirma/pull/573) cerrada sin fusionar; seguimiento en [#572](https://github.com/ctt-gob-es/clienteafirma/issues/572) |
| P1-TEST | Batería de pruebas de validación en módulos cripto | Hecho | [TEST-PORTING-MAP.md](TEST-PORTING-MAP.md) |
| P1-TLS | Comprobaciones SSL estrictas | Hecho a medias | Ver apartado P1-TLS |
| P2-A11Y | Nombres accesibles (certificado, PIN, confirmar) | Hecho en `master`; prueba real con lector de pantalla pendiente | Ver apartado P2-A11Y |
| P2-PACK | Empaquetado Linux | Solo DEB funcional | Ver apartado P2-PACK |
| P3-ORG | Repositorio dentro de la organización `Soluciones-Alexendros` | Hecho | La URL canónica de arriba |
| P3-ARCHIVE | Retirar el repositorio meta `Alexendros/Autofirma-2026` | Hecho (ya no existe con ese nombre) | Ver criterios |
| P3-TRIPHASE | Servidor trifásico (firma en tres pasos con servidor) | Experimental | Informe F8; no bloquea el núcleo |
| R1 | Ronda 1 de seguridad (zip slip, sincronización con el oficial) | Hecho | PR 105, 106, 107 |
| R2 | Ronda 2 (releases como borrador, Renovate, Scorecard, plantillas) | Abierto, pendiente de fusionar | PR 110, 111, 112, 113, 117 |
| REL-1.9.2 | Versión 1.9.2 | Pendiente | Espera a que se fusione el PR 110 |

---

## P1-BC — BouncyCastle (hecho en el fork)

SpongyCastle era una copia antigua de la biblioteca criptográfica BouncyCastle. El fork usa BouncyCastle 1.86 (`jdk18on`) con los mismos formatos de firma. Detalle en [BC-DIFF-ANALYSIS.md](BC-DIFF-ANALYSIS.md), [BC-NOISE-REPORT.md](BC-NOISE-REPORT.md) y [BC-STRATEGY.md](BC-STRATEGY.md).

El oficial no fusionó la PR #573. El trabajo vive en este repositorio.

---

## P1-TLS — Comprobaciones SSL estrictas (hecho a medias)

Lo que hay en `master`:

- La opción de arranque `strictSslChecks` existe (`UrlHttpManagerImpl` y `SslSecurityManager`) y vale `true` por defecto. Con ella activa, el programa nunca desactiva la validación SSL.
- Se puede desactivar con `-DstrictSslChecks=false` si una sede lo necesita.
- Hay un test: `StrictSslChecksTest`.

Lo que **no** hay en `master`:

- Una casilla en Preferencias para cambiarla. Ese trabajo está en la rama `prefs/strict-ssl`, que es anterior a la migración a BouncyCastle. Fusionarla tocaría unos 97 archivos y desharía esa migración. **No se fusiona.** Si se quiere la casilla, se rehace desde cero sobre `master`.

Análisis: [TLS-OVERLAP-ANALYSIS.md](TLS-OVERLAP-ANALYSIS.md).

---

## P2-A11Y — Accesibilidad

- Hecho en `master`: nombres accesibles para la lista de certificados, el campo PIN y el diálogo de confirmar.
- La CI tiene el flujo `accessibility.yml` con dos trabajos: una comprobación de código y otro llamado "Accessibility (Orca GUI - Experimental)", que arranca Orca en una pantalla virtual. Ese segundo trabajo es experimental y no bloquea.
- Pendiente: una sesión real con Orca hecha por una persona y documentada. Hasta entonces no se da por validada la accesibilidad.

---

## P2-PACK — Empaquetado Linux (solo DEB funcional)

| Formato | Estado real | Prueba |
|---------|-------------|--------|
| DEB | Funcional | Publicado en la release `v1.9.1-autofirma-alexendros` |
| JAR | Funcional | Publicado en la misma release, con `SHA256SUMS` |
| RPM | Pendiente | Hay ficheros `.spec` en `afirma-simple-installer/linux/`, pero ninguna release trae un `.rpm` |
| AppImage | Pendiente | Hay un script (`scripts/f6-appimage.sh`) y un paso en `release.yml`, pero ninguna release trae un `.AppImage` |
| Flatpak | Pendiente | Solo hay un manifiesto en `packaging/flatpak/` |
| Firmas GPG (`.asc`) en la release | Pendiente | La release actual no trae ficheros `.asc` |

Nota: el texto automático de notas de `release.yml` anuncia RPM, AppImage y firmas GPG. Ese texto no refleja lo que se publica de verdad y hay que corregirlo.

---

## Ronda 1 de seguridad (hecho)

- **Zip slip corregido (PR 105, fusionado).** Las 6 alertas de extracción insegura de ZIP se cerraron. Hay una única utilidad común: `afirma-core/.../misc/ZipSecurePaths.java`.
- **Sincronización con el oficial (PR 106 y 107, fusionados).** Se trajeron 3 cambios del oficial (NSS y paquete DEB). El registro está en [upstream-sync.md](../upstream-sync.md), incluida la diferencia deliberada en `KeyStoreUtilities`.

## Ronda 2 (abierta, pendiente de fusionar)

| PR | Qué hace | Estado |
|----|----------|--------|
| 110 | Las releases se crean como borrador | Abierto. Hoy `release.yml` publica directamente (`draft: false`) |
| 111 | Configuración de Renovate afinada | Abierto. Ojo: la app de Renovate no está instalada, así que la configuración no hace nada aún |
| 112 | OpenSSF Scorecard en CI | Abierto |
| 113 | Plantilla de PR con lista de seguridad | Abierto |
| 117 | Plantilla de issue para alertas de seguridad (sustituye al PR 91) | Abierto. El PR 91 se cierra sin fusionar después |

**Cobertura JaCoCo:** el plugin está en el `pom.xml` raíz (prepara el agente y genera el informe en la fase `verify`). Ningún flujo de CI publica ni exige el informe. Pendiente: publicarlo y fijar un mínimo.

**Versión 1.9.2:** pendiente. El `pom.xml` sigue en 1.9.1. Se sacará cuando el PR 110 esté fusionado, para que salga como borrador.

---

## Próximos pasos

| Orden | Qué hay que hacer | Cierre comprobable |
|---:|---|---|
| 1 | Fusionar PR 110, 111, 112, 113 y 117 (los fusiona Alejandro porque tocan `.github`) | `gh pr view <n> --json state` devuelve `MERGED` |
| 2 | Cerrar PR 91 sin fusionar | `gh pr view 91 --json state` devuelve `CLOSED` |
| 3 | Sacar 1.9.2 como borrador | `gh release view v1.9.2... --json isDraft` devuelve `true` |
| 4 | Decidir si se instala la app de Renovate | Aparecen PRs de `renovate[bot]` |
| 5 | Corregir las notas de `release.yml` para que solo anuncien lo publicado | Las notas coinciden con la lista de ficheros de la release |
| 6 | RPM, AppImage y firmas GPG en la release | La release trae `.rpm`, `.AppImage` y `.asc` |
| 7 | Publicar el informe JaCoCo en CI | Hay un artefacto de cobertura en cada ejecución de CI |
| 8 | Sesión real con Orca documentada | Documento F7 con fecha y resultado |
| 9 | Casilla de SSL estricto en Preferencias, rehecha sobre `master` | PR nuevo; `prefs/strict-ssl` no se fusiona |

No se reescribe el historial y no se publica ninguna release sin borrador previo.

---

## Criterios de éxito (comprobables)

Cada punto lleva el comando y el resultado esperado. Se ejecutan en un clon de `master`.

- [x] **BouncyCastle en uso:** `rg '^import org\.spongycastle' --type java` → sin resultados (el nombre solo queda en comentarios).
- [x] **SSL estricto por defecto:** `rg -n 'JAVA_PARAM_STRICT_SSL_CHECKS, Boolean.TRUE' afirma-core/src/main/java` → aparece en `UrlHttpManagerImpl` y `SslSecurityManager`.
- [ ] **Casilla de SSL estricto en Preferencias:** `rg -l strictSsl afirma-core-prefs afirma-simple` → hoy sin resultados.
- [x] **Zip slip unificado:** `rg -l ZipSecurePaths --type java` → incluye `afirma-core/src/main/java/es/gob/afirma/core/misc/ZipSecurePaths.java`; `gh api repos/Soluciones-Alexendros/clienteafirma-alexendros/code-scanning/alerts?state=open` → ninguna alerta de zip slip.
- [x] **Sincronización con el oficial registrada:** existe `docs/upstream-sync.md`.
- [x] **Repositorio en la organización:** `gh repo view Soluciones-Alexendros/clienteafirma-alexendros` → responde.
- [x] **Meta antiguo retirado:** `gh repo view Alexendros/Autofirma-2026` → no existe.
- [x] **CI de `master` en verde:** `gh run list -b master -L 5` → todo `success`.
- [ ] **Releases como borrador:** `rg -n 'draft:' .github/workflows/release.yml` → `draft: true` (hoy `false`, pendiente del PR 110).
- [ ] **Scorecard:** existe un flujo de Scorecard en `.github/workflows/` (pendiente del PR 112).
- [ ] **Renovate activo:** hay PRs de `renovate[bot]` (hoy la app no está instalada).
- [ ] **Paquetes:** `gh release view <tag> --json assets` → incluye `.deb` (sí), `.rpm`, `.AppImage` y `.asc` (no).
- [ ] **Cobertura en CI:** `rg -n jacoco .github/workflows` → hoy sin resultados.
- [ ] **Versión 1.9.2:** `rg -m1 '<version>' pom.xml` → `1.9.2` (hoy `1.9.1`).

---

## Riesgos

| Riesgo | Probabilidad | Impacto | Qué lo reduce |
|--------|--------------|---------|---------------|
| Incompatibilidad con la API de BouncyCastle | Media | Alta | Vectores F2 y pruebas de validación |
| Fusionar por error `prefs/strict-ssl` y deshacer la migración | Media | Alta | No se fusiona; se rehace sobre `master` |
| Conflictos al traer cambios del oficial | Alta | Media | Registro en `upstream-sync.md` |
| Notas de release que prometen paquetes que no se publican | Alta | Media | Paso 5 de próximos pasos |
| Retraso del empaquetado RPM, AppImage y Flatpak | Alta | Baja | No bloquea el núcleo |

---

## Fase 1 — Base sólida (avance 2026-11-01)

- Tests cripto en CI: PR #123 (pendiente de fusión; `make test` incluye core + módulos `afirma-crypto-*`).
- Umbral JaCoCo afirma-core: 14 % (PR #122, fusionado).
- SECURITY.md con plazos de respuesta (PR #120, fusionado).
- Sync con el oficial documentado (PR #121, fusionado).
- Limpieza de `.archify` (PR #119, fusionado).
- Releases firmadas: PR #124 (pendiente de fusión; depende de #110).
- Renovate reordenado (PR #111, pendiente de fusión).
- Scorecard: PR #112 (pendiente de fusión).
- Plantillas PR/issue: PR #113 y #117 (pendientes de fusión).
- Protección de ramas verificada (rulesets 26286141 y 26288299).
