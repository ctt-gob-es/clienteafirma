# Autofirma comunitaria — estado de fases

**Canónico:** [Soluciones-Alexendros/clienteafirma-alexendros](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros). El traslado desde `Alexendros/clienteafirma` ya está hecho.  
**MVP operativo:** [docs/MVP.md](MVP.md) — `bash scripts/mvp.sh`  
**Tareas vivas:** [TASKS.md](TASKS.md) · **Hoja de ruta:** [integration/ROADMAP.md](integration/ROADMAP.md) · **Comparativa CTT:** [COMPARATIVA-FORK-CTT.md](COMPARATIVA-FORK-CTT.md)  
**BouncyCastle en el fork:** integrado en `master` (PR #3); rama histórica `crypto/bouncycastle-jdk18on` ya no es el tip canónico. Ver [W2-BOUNCYCASTLE-PREFLIGHT.md](W2-BOUNCYCASTLE-PREFLIGHT.md).  
**Revisión 360º:** [CODE-REVIEW-360.md](CODE-REVIEW-360.md) — `bash scripts/verify-code-review-360.sh`  
**Revisión 360º R2:** [CODE-REVIEW-360-R2.md](CODE-REVIEW-360-R2.md) — `make review360-r2`  
**Remediación 360º:** [REMEDIATION-360.md](REMEDIATION-360.md) — `make remediation360`  
**XXE (XML malicioso) en el fork:** PR [#4](https://github.com/Alexendros/clienteafirma/pull/4) @ `fee3debe1`  
**Archify monorepo:** [`.archify/20261001-0010-monorepo-canonical/`](../.archify/20261001-0010-monorepo-canonical/SUITE-SUMMARY.json)  
**Archify e2e remediación:** [`.archify/20260930-2331-remediation-360-e2e/`](../.archify/20260930-2331-remediation-360-e2e/SUITE-SUMMARY.json)  
**Meta histórico:** [Alexendros/Autofirma-2026](https://github.com/Alexendros/Autofirma-2026) (a archivar tras el traslado de org)

| Fase | Estado | Evidencia |
|------|--------|-----------|
| F0 Manifiesto | Hecho | `propuesta-autofirma-2026.md` |
| F1 Línea base 1.9.1 | Hecho | `docs/BASELINE.txt`, `docs/build-f1.log` |
| F2 Vectores | Hecho | `scripts/f2-regression.sh`, `vectors/` |
| F3 Cadena suministro | Hecho | `.github/workflows/` (SHA pins), Renovate, actionlint |
| F4 Inventario cripto | Hecho en el fork | `docs/F4-INVENTARIO-CRIPTO.md` + BC 1.86 en `master` (migración PR #3 a 1.78.1; bump CVE a 1.86); reintegración CTT pendiente (#572) |
| F5 JDK 21 runtime | Hecho | `docs/F5-JDK21.md` |
| F6 Paquetes Linux | Hecho | `packaging/*.deb`, portal de prueba |
| F7 Accesibilidad | En curso | `docs/F7-ACCESIBILIDAD.md` (nombres accesibles en integración; sesión Orca pendiente) |
| F8 Trifásico | Hecho | WAR 2.9.1 + `docs/F8-TRIFASICA.md` |
| F9 Integr@/FIRe | Hecho | FIRe OK; Integr@ bloqueado iText HTTP — `docs/F9-INTEGRA-FIRE.md` |
| F10 Upstream | Tabla | `docs/F10-UPSTREAM.md` |
