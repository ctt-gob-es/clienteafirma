# Tareas vivas — Autofirma comunitaria

### Propósito de este documento

- **Objetivos:** Tablero único de trabajo (Hecho / En curso / Pendiente) con criterio comprobable.
- **Estructura:** Bloques por prioridad y por fase F0–F10.
- **Contenido a integrar según contexto:** Actualizar al cerrar PRs o fases. Complementa [ROADMAP](integration/ROADMAP.md) y [ESTADO-FASES](ESTADO-FASES.md).

**Canónico:** [Soluciones-Alexendros/clienteafirma-alexendros](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros).

---

## En curso

| ID | Tarea | Criterio de salida | Evidencia |
|----|-------|--------------------|-----------|
| T-ORCA | Sesión real con Orca (lector de pantalla) en los 3 flujos | Informe F7 actualizado con evidencia | No bloquea merge de nombres accesibles |
| T-CI-TESTS | Ejecutar tests de validación cripto también en CI | Job CI documentado en verde | ROADMAP P2-CI |

---

## Hecho (reciente)

| ID | Tarea | Criterio | Evidencia |
|----|-------|----------|-----------|
| T-P0 | Alineación de contrato de repositorio | README/SECURITY/CI P0 | PR [#1](https://github.com/Alexendros/clienteafirma/pull/1) |
| T-P1-DOCS | Informes P1 (BC, tests, TLS, roadmap) | Docs bajo `docs/integration/` | PR [#2](https://github.com/Alexendros/clienteafirma/pull/2) |
| T-BC | SpongyCastle → BouncyCastle 1.78.1 + harness | Build + F2 verdes en monorepo | PR [#3](https://github.com/Alexendros/clienteafirma/pull/3) |
| T-XXE | `SecureXmlBuilder` en sinks XML de usuario/servidor | Compila; remediación SEC-007/008 | PR [#4](https://github.com/Alexendros/clienteafirma/pull/4) |
| T-MONO | Absorber Autofirma-2026 como monorepo | `make validate` + MVP + gates 360 | PR [#5](https://github.com/Alexendros/clienteafirma/pull/5) |
| T-F0…T-F6, T-F8, T-F9 | Fases de programa (salvo a11y Orca) | Ver ESTADO-FASES | Docs + scripts en árbol |
| T-A11Y | Nombres accesibles (cert list, PIN, confirm) | Mergeado + CI verde | Rama `a11y/signing-flows` |
| T-TLS | `strictSslChecks` opt-in (default off) | Mergeado + CI verde | Rama `prefs/strict-ssl` |
| T-ORG | Repo transferido a `Soluciones-Alexendros/clienteafirma-alexendros` | Remoto canónico = nueva URL | Transfer completada |
| T-RELEASE | Release v1.9.1-autofirma-alexendros publicado | Tag `v*` → Release con artefactos | GitHub Release v1.9.1-autofirma-alexendros |
| T-PACK | Empaquetado DEB/RPM/Flatpak/AppImage + firmas/checksums | Artefactos en release | GitHub Release assets |
| T-ARCHIFY | Suite Archify post-canon | `.archify/20261001-canon-org-release/` | 5 diagramas + SUITE-SUMMARY |
| T-ARCHIVE | Meta `Alexendros/Autofirma-2026` archivado | Repo read-only | `gh repo archive` |

---

## Pendiente

| ID | Tarea | Criterio | Notas |
|----|-------|----------|-------|
| T-ORCA | Sesión real con Orca (lector de pantalla) en los 3 flujos | Informe F7 actualizado con evidencia | No bloquea merge de nombres accesibles |
| T-CI-TESTS | Ejecutar tests de validación cripto también en CI (hoy parte del build usa skipTests) | Job CI documentado en verde | ROADMAP P2-CI |
| T-CTT-573 | Reintegración BC en upstream CTT | PR/issue CTT avanza | [#572](https://github.com/ctt-gob-es/clienteafirma/issues/572); #573 cerrada sin merge |
| T-FLATPAK | AppImage/Flatpak usable | Paquete instalable documentado | Experimental / P3 |
| T-OPENPDF | Coordinar PAdES con `openpdf-afirma` | Sin duplicar fork | F10 AF2026-4 |

---

## Fases F0–F10 (resumen)

| Fase | Estado tablero | Cómo comprobarlo |
|------|----------------|------------------|
| F0 Manifiesto | Hecho | `propuesta-autofirma-2026.md` |
| F1 Línea base 1.9.1 | Hecho | `docs/BASELINE.txt` |
| F2 Vectores | Hecho | `bash scripts/f2-regression.sh` |
| F3 Cadena de suministro | Hecho | Workflows con SHA pins + `make quality` |
| F4 Cripto | Hecho en fork (BC en `master`) | Inventario + PR #3; upstream CTT pendiente |
| F5 JDK 21 runtime | Hecho (informe) | `docs/F5-JDK21.md` |
| F6 Paquetes Linux | Hecho (dev) | `packaging/` + MVP |
| F7 Accesibilidad | En curso | T-A11Y + T-ORCA |
| F8 Trifásico | Hecho (informe/WAR) | `docs/F8-TRIFASICA.md` |
| F9 Integr@/FIRe | Hecho (con bloqueo Integr@) | `docs/F9-INTEGRA-FIRE.md` |
| F10 Upstream | Tabla viva | [F10-UPSTREAM.md](F10-UPSTREAM.md) |

---

## Comandos rápidos de verificación

```bash
make validate          # contrato + quality + tests + smoke
make review360-r2      # gates revisión 360 R2
make remediation360    # gates remediación
bash scripts/mvp.sh    # build + F2 + empaquetado best-effort
```
