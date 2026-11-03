# Informe de OT Fase 1 · Cierre

Fecha: 2026-11-03  
Repositorio: `Soluciones-Alexendros/clienteafirma-alexendros`  
Rama de integración: `plan/clienteafirma-fase1`

## Resultado

Se han implementado las tareas T1–T8 en sus ramas y PRs, en el orden fijado por la Nota de Arquitecto. T3 y T5 tocan `.github/` y esperan fusión de Alejandro. Los demás PR tienen auto-merge squash activado. T9 queda parcial: T9.3 está hecha; secreto, Renovate y tag son pasos de Alejandro.

La OT **no está cerrada**. La rama de plan todavía no contiene los cambios y no se abre un PR final vacío `plan/clienteafirma-fase1 → master`. Ninguna release nueva se ha creado ni publicado.

## Tabla de tareas

| Tarea | Estado | PR | Prueba |
| --- | --- | --- | --- |
| T0 · Arranque y comprobación | **Completada** | — | Rama de plan remota/local sincronizada; 0 PR iniciales, CI de master verde, 0 alertas code/secret scanning y 10 Dependabot iniciales. Alejandro retiró `required_linear_history` de `plan/*`; el resto de la regla se verificó intacto. |
| T1 · Quitar ini4j | **Implementada; auto-merge pendiente del check de base** | [#138](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros/pull/138) | Árbol inicial `proxy-vole → ini4j 0.5.4`; después 0 coincidencias. `ProxyUtilTest`: 1/1. `mvn -Denv=install -pl afirma-simple -am verify` y `make test`: BUILD SUCCESS. Commit GPG `2fd8e1ed455ed80a289efd71061426fdf619bdce` verificado. |
| T2 · Dependencias Python de CI | **Riesgo aceptado y documentado; auto-merge pendiente** | [#139](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros/pull/139) | Semgrep 1.183.0 fija `mcp==1.29.0` y `opentelemetry-api~=1.37.0`; no hay Semgrep posterior en PyPI. Lock regenerado e instalado con hashes; `semgrep --version=1.183.0`. Diez alertas descartadas como `tolerable_risk` con motivo en `docs/dependabot-ci.md`; 0 abiertas al comprobarlo. |
| T8 · Registro de riesgos | **Implementada; auto-merge pendiente** | [#140](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros/pull/140) | `docs/RISK-REGISTER.md`: 15 filas H1–H15, responsable, fecha, evidencia y salida. Incluye ini4j, Dependabot, Scorecard y 20 BinaryArtifacts; no presenta hipótesis como hechos. |
| T7 · Contener la release antigua | **Notas y comando preparados; ejecución reservada a Alejandro** | [#141](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros/pull/141) | Release previa contrastada: final, tres assets y sin `.asc`; notas originales anunciaban RPM/AppImage/Flatpak ausentes. `docs/releases/` contiene advertencia y comando. **No se ejecutó `gh release edit`**. |
| T4 · Huella estable de Semgrep | **Implementada; auto-merge pendiente** | [#142](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros/pull/142) | RED: mover línea 10→90 creaba hallazgo. GREEN: snippet SHA-256/`matchBasedId`; desplazamiento pasa y contenido cambiado bloquea. `test-semgrep-new-high-gate.sh`, `py_compile` y `make quality` correctos. |
| T6 · Notas = assets y verificación documental | **Implementada; auto-merge pendiente** | [#143](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros/pull/143) | 4 tests de `release_notes.py`; notas con RPM/AppImage ausentes rechazadas. `check-docs-truth.sh`, `make test-community` y `make quality` correctos. Se corrigió además el caso de clones CI sin objeto upstream local, sin consultar CTT. |
| T3 · Gates CI bloqueantes | **Implementada y verde; espera a Alejandro** | [#144](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros/pull/144) | SARIF ausente/inválido falla; sin `|| true` ni `continue-on-error`; SBOM bloqueante; skip solo en Dependency Review fuera de PR. PR rojo #145 cerrado sin fusionar y rama borrada: run `41653635499`, `Generate SBOM=failure` y `ci-gate=failure`. Runs `41655090712` (CI/ci-gate) y `41655090689` (CodeQL/Semgrep) en SUCCESS. |
| T5 · Release fiable | **Implementada; espera a Alejandro y a firma real** | [#146](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros/pull/146) | Jobs `build/sign/validate/publish`; solo `sign` usa secreto y entorno `release`; `KEYS` público versionado; tests sin `-DskipTests`; AppImageKit 13 + SHA-256 fijado; notas desde assets; `draft:true`. `make validate` verde; AppImage generada localmente. Dry-run `41655494188` lanzado tras corregir el clone upstream; sigue en build al cerrar este informe. Falta la ejecución firmada del tag por ausencia del secreto y del entorno protegido. |
| T9 · Pasos de Alejandro | **Parcial: T9.3 completada** | — | Ramas `docs/tests-excluidos-limpieza` y `fix/tests-excluidos-keystores` borradas tras confirmar PR #127/#130 cerrados. `GPG_PRIVATE_KEY` ausente por nombre; Dependency Dashboard inexistente; tag final no creado. |
| T10 · Informe y cierre | **Informe y actualización documental; cierre pendiente** | [#147](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros/pull/147) | Informe, ROADMAP y Notion actualizados. PR final `plan → master` no abierto porque la rama de plan aún no contiene cambios. CI verde final en master y borrado de plan quedan para después de las fusiones de Alejandro. |

## PRs que esperan a Alejandro

### Fusión directa obligatoria por tocar `.github/`

1. **#144 · T3 · `ci/gates-bloqueantes`**.
2. **#146 · T5 · `ci/release-fiable`**.

### Dependencia operativa de #144

Los workflows heredados por la rama base solo escuchaban `plan/clienteafirma-alertas`, no `plan/clienteafirma-fase1`. Por eso los PR #138–#143 no reciben el `ci-gate` requerido aunque las ejecuciones manuales de CI se lanzaron correctamente. GitHub no asocia esos `workflow_dispatch` al check obligatorio del PR.

Tras fusionar #144, actualizar estas ramas **con merge, nunca rebase**, para que hereden `plan/*` y ejecuten su CI:

- #138 · T1 · `fix/quitar-ini4j` — auto-merge activado.
- #139 · T2 · `fix/dependencias-python-ci` — auto-merge activado.
- #140 · T8 · `docs/registro-riesgos` — auto-merge activado.
- #141 · T7 · `docs/contener-release-antigua` — auto-merge activado.
- #142 · T4 · `fix/huella-estable-semgrep` — auto-merge activado.
- #143 · T6 · `fix/notas-assets-documentacion` — auto-merge activado.
- El PR de este informe, cuando se publique, estará en la misma situación.

Secuencia necesaria: fusionar #144 → `gh pr update-branch` en los PR anteriores → auto-merge squash en verde → fusionar #146 tras su revisión → abrir el PR final `plan → master`. No se usará `--admin` ni se forzará ninguna fusión.

## Pasos locales pendientes de Alejandro

1. Alta de `GPG_PRIVATE_KEY` sin compartirlo con el agente; solo se comprobó que el nombre no existía.
2. Crear/proteger el entorno `release` con la aprobación deseada. Actualmente no existe; el agente no cambia ajustes.
3. Ejecutar `docs/releases/contencion-v1.9.1-autofirma-alexendros.md` para marcar la release histórica como prerelease y sustituir sus notas.
4. Dar acceso a Renovate y comprobar el Dependency Dashboard: no existe al hacer T9.
5. Tras T5 y la configuración anterior, crear el tag firmado de prueba y comprobar release **borrador**, `.asc`, checksums y attestations. Después, el tag `v1.9.2-autofirma-alexendros` previsto en T9.4.
6. Fusionar el PR final y comprobar CI verde en master; borrar la rama de plan manualmente al terminar.

## Alertas y límites finales comprobados durante la ejecución

- Code scanning: 0 alertas abiertas.
- Dependabot: 0 alertas abiertas tras diez descartes documentados como riesgo tolerable, no como correcciones.
- Secret scanning: 0 alertas abiertas al inicio.
- E3, E4.2 y E5 permanecen fuera de alcance. No se anuncia ninguna release como «validada» ni «sin SpongyCastle».
- Todos los commits creados por el agente usan `commit -S` con la clave local `EFECA8DCB405D5F33F3FE35C050B8C2C622F92BD`, autorizada expresamente por Alejandro; cada commit se verificó con `git log --show-signature -1` y firma buena.
- No se usaron `--admin`, force push, rebase, reescritura de historia, `pull_request_target` ni `dependabot.yml`. No se leyeron ni mostraron secretos.
