## Learned User Preferences

- Responder en español; labels, workflows, commits y nombres de checks en inglés técnico cuando aplique a GitHub/CI.
- Al implementar un plan de Cursor: no editar el fichero del plan; reutilizar los to-dos ya creados y completarlos hasta el final.
- Antes de mutaciones de plataforma (crear repo remoto, push inicial, publicación, archivar repo), pedir confirmación explícita y proceder solo tras un sí.
- Documentación orientada a contraste con Autofirma oficial (tablas comparativas) y a objetivos comprobables por fase; en revisiones amplias, informe dual (suite Archify HTML + markdown claro con tablas).
- Los planes deben recuperar el programa original con revisión crítica (correcciones, ampliaciones y adiciones) antes de ejecutar oleadas nuevas.
- Versionar artefactos Archify bajo `.archify/` (no añadirlos a `.gitignore`).
- Cuando pida auditorías/revisiones «desde aquí sin subagentes», ejecutarlas en el agente padre y cerrar con gates/tests medibles y evidencia, no solo narrativa.
- En remediaciones post-auditoría: documentar primero playbook, método y tests de validación; implementar después.

## Learned Workspace Facts

- Repo canónico comunitario: `https://github.com/Soluciones-Alexendros/clienteafirma-alexendros` (monorepo). El traslado desde `Alexendros/clienteafirma` ya está hecho. Rama por defecto `master`.
- Clone local preferido del mantenedor: directorio hermano `clienteafirma-alexendros` bajo `Aplicaciones/Fuentes/` (no usar el clone legacy `clienteafirma` ni anidar bajo el meta archivado).
- El meta histórico `Alexendros/Autofirma-2026` se archiva tras la migración; no es el working tree.
- Cliente construible/auditable/sustituible respecto a Autofirma 1.9.x (mismos formatos y protocolo `afirma://`); no sustituye `@firma`, VALIDe, TS@, Port@firmas ni Cl@ve.
- Comparativa fork↔CTT: `docs/COMPARATIVA-FORK-CTT.md`; tareas: `docs/TASKS.md`; hoja de ruta: `docs/integration/ROADMAP.md`.
- Clones locales opcionales `integra/`, `fire/`, toolchains en `tools/` y `dist/` están en `.gitignore`.
- Pin de sync upstream: `docs/BASELINE.txt` (`UPSTREAM=ctt-gob-es/clienteafirma`, `BASELINE_COMMIT`).
- Fachada P0: `make lint` / `make quality` / `make test` / `make test-community` / `make smoke` / `make validate`. Jobs CI: `quality`, `test`, `community`, `build`, `smoke` + `build-baseline` (JAR+F2).
- MVP operativo: `docs/MVP.md` y `bash scripts/mvp.sh` (evidencia en `dist/MVP-EVIDENCE.txt`).
- Programa por fases F0–F10; estado en `docs/ESTADO-FASES.md`; manifiesto en `docs/historico/propuesta-autofirma-2026.md`.
- SpongyCastle→BouncyCastle: F2 verde en `master` del monorepo; rama histórica `crypto/bouncycastle-jdk18on`; PR upstream `ctt-gob-es/clienteafirma#573` cerrada sin merge; seguimiento en `#572`. `strictSslChecks` va **activado por defecto** (#92); se puede desactivar en Preferencias o con `-DstrictSslChecks=false`.
- Licencia conservada GPL-2.0+ / EUPL-1.1.
- Archify: skill en `~/.cursor/skills/archify`; artefactos bajo `.archify/` (suite canónica 5 tipos) con texto visible en español sencillo.
- Revisión/remediación 360º: `docs/CODE-REVIEW-360.md` (+ R2), `docs/REMEDIATION-360.md`, gates `make review360` / `make review360-r2` / `make remediation360`; batería en `docs/VALIDATION-TESTS.md`.
