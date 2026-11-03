# Registro de riesgos

Fecha de apertura: 2026-11-03.  
Responsable del registro y de las decisiones de aceptación: Alejandro.  
Base contrastada: `7ade423e5a6669d130cdd417eca54b981a102259` y evidencia del 2026-11-02 recogida en la página del proyecto.

Los estados distinguen **confirmado**, **parcial**, **pendiente de reproducción**, **en tratamiento**, **aceptado** y **fuera de alcance**. «Aceptado» no significa corregido. Las hipótesis H4–H6, H12, H14 y H15 no se presentan como vulnerabilidades verificadas.

## Hallazgos H1–H15

| ID | Riesgo y evidencia disponible | Estado y tratamiento | Responsable / revisión |
| --- | --- | --- | --- |
| H1 | La release `v1.9.1-autofirma-alexendros` está publicada como final, no como borrador ni prerelease, y solo contiene JAR, DEB y `SHA256SUMS`; no hay `.asc`. Evidencia: `gh release view ... --json isDraft,isPrerelease,assets` (2026-11-03). | **Confirmado. En tratamiento:** advertencia y prerelease en T7; release fiable en T5. | Alejandro · al ejecutar T7. |
| H2 | Las notas históricas anuncian RPM/AppImage ausentes; el tag no está firmado y describe un estado anterior al master contrastado. Evidencia: metadatos de release/tag y contraste documental del 2026-11-02. | **Confirmado. En tratamiento:** notas desde inventario en T6; tag firmado reservado a Alejandro en T9. | Alejandro · antes de otra release. |
| H3 | El JAR sombreado conserva SpongyCastle arrastrado por iText según `README.md`; no se inspeccionó el binario publicado en esta revisión. | **Parcial. Fuera de alcance:** E4.2 se pospone a Fase 3; no anunciar «sin SpongyCastle». | Alejandro · Fase 3. |
| H4 | Posible validación insuficiente de TSA/TLS: truststore, HTTP, hostname y protocolos. Evidencia: hipótesis E3.1–E3.3 del roadmap, no reproducción independiente. | **Pendiente de reproducción. Fuera de alcance:** E3 pasa a Fase 3. No atribuir exploitabilidad sin prueba. | Alejandro · antes de E3. |
| H5 | Posible validación insuficiente del token: firma, EKU de sellado y cadena PKIX. Evidencia: hipótesis E3.3, sin prueba de token alterado. | **Pendiente de reproducción. Fuera de alcance:** Fase 3. | Alejandro · antes de E3.3. |
| H6 | Riesgo del canal local: loopback IPv4/IPv6, TLS, secreto de keystore e `idsession`. Evidencia: hipótesis E3.4–E3.6; código y comportamiento no reproducidos en esta revisión. | **Pendiente de reproducción. Fuera de alcance:** Fase 3. | Alejandro · antes de E3.4. |
| H7 | El gate Semgrep podía devolver vacío ante SARIF ausente y usa `(regla,ruta,línea)`, huella inestable al desplazar código. Evidencia: `scripts/ci/semgrep-new-high-gate.py` en la base. | **Confirmado. En tratamiento:** T3 hace obligatorio el SARIF; T4 introduce huella de contenido con tests. | Alejandro · T3/T4. |
| H8 | CI toleraba errores de Semgrep/SBOM y `ci-gate` podía aceptar `skipped` sin una autorización por job. Evidencia: `.github/workflows/ci.yml` en la base. | **Confirmado. En tratamiento:** T3 y PR rojo de prueba; fusión reservada a Alejandro. | Alejandro · T3. |
| H9 | Release compilaba con `-DskipTests`, combinaba build y firma, toleraba empaquetado y usaba appimagetool `continuous`. Evidencia: `.github/workflows/release.yml` en la base. | **Confirmado parcialmente. En tratamiento:** T5 separa build/sign, ejecuta tests sobre el SHA del tag y fija appimagetool. | Alejandro · T5. |
| H10 | Verificación de release incompleta: `KEYS`, checksums, firmas, attestations y orden de validación/publicación; reproducibilidad limitada a worktrees de un runner. | **Confirmado parcialmente. En tratamiento:** T5 cubre E2.1/E2.2/E2.4/E2.6; E2.5 queda pendiente y no se afirma reproducibilidad entre runners. | Alejandro · T5 y E2.5 posterior. |
| H11 | `scripts/f2-regression.sh` comprueba ejecución/salida/hashes, no una validación criptográfica DSS independiente. | **Confirmado. Fuera de alcance:** E5 pasa a Fase 3; no anunciar release «validada». | Alejandro · Fase 3. |
| H12 | Posibles defectos de metadatos/dependencias Java e idempotencia de `postinst`/`prerm` en DEB. Evidencia: propuesta E6.1, sin `lintian` ni instalación en distribuciones en esta revisión. | **Pendiente de reproducción:** E6 posterior; no afirmar paquete validado. | Alejandro · antes de E6.1. |
| H13 | Divergencia upstream/documental: `docs/upstream-sync.md` cita `bd9254be` y `docs/BASELINE.txt` fija `0d7f3cf01`; su semántica y el recuento de 227 ficheros requieren contraste. | **Parcial:** E0.4/E1.5 y E8; no modificar referencias sin verificar diff y significado. | Alejandro · revisión mensual de sync. |
| H14 | Inventario de componentes y vulnerabilidades incompleto, incluidos binarios embebidos. ini4j sí está confirmado por árbol Maven; no se atribuyen otras CVE sin evidencia. | **En tratamiento:** T1 retira ini4j; T2 registra límites de Semgrep; E4.1/E4.4 continúan el inventario. | Alejandro · 2027-02-03 o antes si hay release de dependencias. |
| H15 | Gobernanza concentrada en una persona, sin segundo revisor ni CODEOWNERS verificado. Scorecard `CodeReviewID` (#553) está descartada por esta decisión, con PR + gate + firma como mitigación. | **Aceptado temporalmente:** evaluar segundo revisor/CODEOWNERS en E8.3 antes de exigir aprobaciones. | Alejandro · revisión de gobernanza. |

## Dependencias y excepciones activas

| Elemento | Decisión | Evidencia y salida | Responsable / fecha |
| --- | --- | --- | --- |
| `org.ini4j:ini4j 0.5.4` · CVE-2022-41404 | **Riesgo temporal retirado por T1:** exclusión de `proxy-vole`; no queda excepción activa cuando se integra PR #138. | Árbol inicial `proxy-vole 1.0.18 → ini4j 0.5.4`; después 0 coincidencias. `docs/proxy-detection.md`, `ProxyUtilTest`, `make test` y `verify` verdes. | Alejandro · 2026-11-03. |
| `mcp 1.29.0` y `opentelemetry-api 1.37.0` en CI | **Aceptado temporalmente:** Semgrep 1.183.0 impide las versiones corregidas; cinco avisos medios, diez alertas `.in`/`.txt` descartadas como `tolerable_risk`. | `docs/dependabot-ci.md` (PR #139): constraints, GHSA/CVE, ausencia de servidor MCP/trazas externas y lock con hashes verificado. Actualizar cuando Semgrep lo permita. | Alejandro · revisar al nuevo Semgrep y antes de 2027-02-03. |

## Scorecard y BinaryArtifacts aceptados

El 2026-11-03 hay **0 alertas abiertas de code scanning** y **0 de Dependabot**. Las alertas descartadas no se contabilizan como remediaciones.

| Grupo | Evidencia | Decisión y revisión | Responsable / fecha |
| --- | --- | --- | --- |
| `BinaryArtifactsID`: alertas #527–#546 (20) | `docs/binarios-heredados.md` inventaria 20 binarios heredados sin cambios respecto al oficial y no publicados por este fork. | Aceptadas como `won't fix`, con comentario individual. Revisar si upstream elimina o cambia alguno. | Alejandro · cada sync mensual. |
| `CIIBestPracticesID` #555 | `docs/scorecard.md`; requiere solicitud humana a OpenSSF Best Practices. | Aceptada; Alejandro decide si solicita la insignia. | Alejandro · gobernanza. |
| `MaintainedID` #554 | `docs/scorecard.md`; métrica temporal de un fork reciente. | Aceptada temporalmente; reevaluar actividad. | Alejandro · cierre de la siguiente fase. |
| `FuzzingID` #551 | `docs/scorecard.md`; sin OSS-Fuzz/Jazzer. Propuesta E5.3. | Aceptada hasta disponer de infraestructura; no afirmar cobertura de fuzzing. | Alejandro · Fase 3. |
| `CodeReviewID` #553, `BranchProtectionID` #526, `VulnerabilitiesID` #552 | Comentarios individuales en code scanning y `docs/scorecard.md`; rulesets exigen PR, `ci-gate` y firma. | Riesgo de proceso aceptado bajo operador único; revisar con H15 y al incorporar otro mantenedor. | Alejandro · revisión de gobernanza. |

## Operativa

1. Todo hallazgo nuevo se reproduce y se registra antes de anunciar corrección.
2. Toda aceptación incluye motivo, alcance, responsable, fecha y condición de salida.
3. Las fases fuera de alcance no se marcan como resueltas por esta OT.
4. Las alertas se comprueban con `gh api` contra `Soluciones-Alexendros/clienteafirma-alexendros`; nunca se consultan secretos ni se usan datos reales en pruebas.
