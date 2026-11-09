# Remediación 360º — playbook operativo

**Fecha:** 2026-09-30  
**Entrada:** [CODE-REVIEW-360.md](CODE-REVIEW-360.md) (R1) + [CODE-REVIEW-360-R2.md](CODE-REVIEW-360-R2.md) (R2)  
**Criterio de cierre vivo:** `make validate && make remediation360`  
**Auditorías congeladas:** `make review360` / `make review360-r2` anclan el historial; los asertos de ítems ya cerrados comprueban la **mitigación** (no la presencia del bug).

### Propósito

Convertir SEC-001…011 en oleadas ejecutables con dueño (meta | fork | ops-manual), ficheros concretos y prueba de aceptación. Solo se remedia deuda de **alta confianza** (entrada→sink). No se inventan CVE. **No** se cambia el default TLS (`disableSslChecks` / `DUMMY_TRUST`).

### Método

1. Trazar hallazgo R1/R2 hasta fichero y sink.
2. Aplicar el cambio mínimo que cierra el sink (o documentar checklist si es ops-manual).
3. Añadir/actualizar aserto positivo en `scripts/verify-remediation-360.sh`.
4. Evidencia en `dist/REMEDIATION-360-EVIDENCE.txt`.
5. Push al fork o mutaciones de GitHub Settings: solo tras confirmación explícita (cumplido para XXE en `Alexendros/clienteafirma` @ `fee3debe1`, 2026-09-30).

### Extremos

| Extremo | Árbol | Qué se toca |
|---------|-------|-------------|
| **Meta** | este repo | scripts, packaging, CI Make, CHANGELOG, Renovate docs, gates |
| **Cliente** | este monorepo (`Alexendros/clienteafirma`) | parsers XML (SEC-007/008); aterrizado en `master` |
| **Ops-manual** | GitHub Settings | branch protection (SEC-003) |

### Herramientas

| Herramienta | Rol |
|-------------|-----|
| `make validate` | quality + test + smoke del meta |
| `make remediation360` | gates de mitigación post-auditoría |
| `make review360` / `review360-r2` | anclaje histórico R1/R2 (mitigaciones donde el ítem ya cerró) |
| `bash scripts/mvp.sh` | MVP; `--require-package` para release fail-closed |
| `scripts/f6-package-linux.sh` | wrapper `.deb` portable |
| Archify `.archify/20260930-2150-code-review-360-r2/` | mapas de superficie R2 (auditoría) |
| Archify `.archify/20260930-2331-remediation-360-e2e/` | mapas post-remedio + hilos e2e (5/5 finalize=pass) |

---

## Matriz SEC → oleada → aceptación

| ID | Oleada | Owner | Ficheros / acción | Prueba de aceptación | Estado |
|----|--------|-------|-------------------|----------------------|--------|
| SEC-002 | W1 | meta | `scripts/f6-package-linux.sh` — wrapper con `java` de PATH | Gate `FIX-002`: wrapper generado sin `tools/jdk` | **Hecho** |
| SEC-004 | W1 | meta | `CHANGELOG.md` alinea Renovate (+ vigilancia automerge) | Gate `FIX-004`: CHANGELOG menciona Renovate; existe `.github/renovate.json` | **Hecho** |
| SEC-010 | W1 | meta | `scripts/mvp.sh --require-package` | Gate `FIX-010`: flag documentado; modo fail-closed disponible | **Hecho** |
| SEC-011 | W1 | meta+clone | aserto SpongyCastle=0 | Gate `FIX-011`: `org.spongycastle` = 0 en clone | **Hecho** (exige clone) |
| SEC-007 | W2 | fork | `XmlHashDocument`, `ContentTypeManager` → `SecureXmlBuilder` | Gate `FIX-007*` | **Hecho** — [Alexendros/clienteafirma#4](https://github.com/Alexendros/clienteafirma/pull/4) @ `fee3debe1` |
| SEC-008 | W2 | fork | `XAdESTriPhaseSignerUtil`, `XAdESTriPhaseSignerServerSide` → `SecureXmlBuilder` | Gate `FIX-008*` | **Hecho** — mismo PR/SHA |
| SEC-001 | W3 | meta docs | documentar `strictSslChecks` opt-in; **sin** cambiar default | Gate `FIX-001-DOC` en F4 + esta sección | **Hecho** (docs) |
| SEC-003 | W3 | ops-manual | checklist branch protection | Checks + **require PR** activos (0 approvals; dismiss stale) | **Hecho** (2026-09-30) |
| SEC-009 | W4 | fork/docs | política plugins (aviso trust; firma = futuro) | Sección W4 abajo | **Docs** (código diferido) |
| SEC-005 | W4 | fork (diferido) | OpenPDF / iText | F4 / F10 | Diferido |
| SEC-006 | W4 | frontera | Integr@ HTTP iText | F9 | Diferido |

---

## Oleadas

### W0 — Docs y gates

- Este playbook.
- Sección de gates en [VALIDATION-TESTS.md](VALIDATION-TESTS.md).
- `scripts/verify-remediation-360.sh` + `make remediation360`.

### W1 — Meta higiene

1. **SEC-002:** el script instalado `/usr/bin/autofirma` ejecuta `java` del PATH (o `/usr/bin/java`). El JDK bajo `tools/` solo sirve para *construir*, no se embebe en el wrapper.
2. **SEC-010:** `mvp.sh` acepta `--require-package` / `REQUIRE_PACKAGE=1`; si el empaquetado falla, exit ≠ 0. Sin el flag, best-effort (comportamiento histórico documentado).
3. **SEC-004:** CHANGELOG describe Renovate como gestor activo; Dependabot histórico en releases antiguas no se reescribe.
4. **SEC-011:** el gate de remediación exige cero `org.spongycastle` en el clone local (fork BC).

### W2 — Fork XXE (local)

Sustituir `DocumentBuilderFactory.newInstance()` en rutas de **parseo de entrada** por `SecureXmlBuilder.getSecureDocumentBuilder()` (mismo patrón que `PdfSignResult` / `XmpHelper`):

- `afirma-simple-plugin-hash/.../XmlHashDocument.java`
- `afirma-crypto-ooxml/.../ContentTypeManager.java`
- `afirma-server-triphase-signer-core/.../XAdESTriPhaseSignerUtil.java`
- `afirma-server-triphase-signer-core/.../XAdESTriPhaseSignerServerSide.java` (ambos parseos)

XXE: **hecho** — [PR #4](https://github.com/Alexendros/clienteafirma/pull/4) (`fee3debe1`). Monorepo: capa Autofirma-2026 absorbida en este árbol. No PR a `ctt-gob-es/clienteafirma` en esta tanda.

### W3 — Docs / ops sin default TLS

#### SEC-001 — TLS estricto (opt-in)

- Default del producto: checks SSL pueden desactivarse en escenarios de sede (compatibilidad). **No cambiar.**
- Preferencia avanzada en el fork: rama / trabajo `prefs/strict-ssl` con `strictSslChecks` (default **false**).
- Uso consciente: activar solo en entornos controlados; validar con vectores F2 + portal F6.
- Detalle técnico: [F4-INVENTARIO-CRIPTO.md](F4-INVENTARIO-CRIPTO.md) §3 y §6.

#### SEC-003 — Branch protection (estado 2026-09-30)

Rama por defecto: **`master`** (no existe `main`).

**Activo** (verificado por API):

- Required status checks: `quality`, `test`, `smoke` (strict)
- Require pull request before merging: sí (`required_approving_review_count: 0`)
- Dismiss stale reviews: sí
- `enforce_admins`: true
- Linear history: true
- Force push / deletions: false
- Conversation resolution: true

Ya no hace falta reaplicar el PUT de protección salvo regresión.

### W4 — Diferido / parcial

#### SEC-009 — Política de plugins (docs; sin cambiar `PluginLoader`)

- Los plugins se cargan con `URLClassLoader` desde un JAR elegido por el usuario (consentimiento UI).
- **Regla operativa:** no instalar plugins de fuentes no confiables; tratar un JAR malicioso como RCE con el mismo privilegio que Autofirma.
- **Futuro (código):** firma/hash de plugins, aviso UI más fuerte y/o lista blanca — fuera de esta tanda.
- **No hacer ahora:** modificar `PluginLoader` sin diseño de UX + vectores.

#### SEC-005 / SEC-006 (externos)

| ID | Criterio de cierre futuro |
|----|---------------------------|
| SEC-005 | Coordinar `openpdf-afirma` sin duplicar fork; F2 PAdES verde |
| SEC-006 | Integr@ sin Maven HTTP / iText 2.2 bloqueante (AF2026-6) |

---

## Controles que no se tocan (regresión prohibida)

| Control | Ubicación | Gate |
|---------|-----------|------|
| Bloqueo localhost en `stservlet` | `UrlParameters` → `LocalAccessRequestException` | `KEEP-PROTO-LOCAL` |
| Parseo PDF result seguro | `PdfSignResult` + `SecureXmlBuilder` | `KEEP-PDF-SECURE` |
| XMP seguro | `XmpHelper` + `SecureXmlBuilder` | `KEEP-XMP-SECURE` |

---

## Referencias

| Doc / artefacto | Rol |
|-----------------|-----|
| [CODE-REVIEW-360.md](CODE-REVIEW-360.md) | Auditoría R1 |
| [CODE-REVIEW-360-R2.md](CODE-REVIEW-360-R2.md) | Auditoría R2 estricta |
| [VALIDATION-TESTS.md](VALIDATION-TESTS.md) | Firma + gates de seguridad |
| [F4-INVENTARIO-CRIPTO.md](F4-INVENTARIO-CRIPTO.md) | TLS / SC / iText |
| [F8-TRIFASICA.md](F8-TRIFASICA.md) | Contexto WAR lab (SEC-008) |
| [F9-INTEGRA-FIRE.md](F9-INTEGRA-FIRE.md) | SEC-006 |
| [REPO-ENDING-AUDIT.md](REPO-ENDING-AUDIT.md) | Branch protection (require PR activo) |
| [`.archify/20260930-2150-code-review-360-r2/`](../.archify/20260930-2150-code-review-360-r2/) | Mapas ataque R2 |
| [`.archify/20260930-2331-remediation-360-e2e/`](../.archify/20260930-2331-remediation-360-e2e/) | Mapas post-remedio + e2e (5/5) |

---

## Cómo verificar

```bash
make validate
make remediation360
# Esperado: VERIFICACIÓN REMEDIATION OK … fail=0
# E2E completo (JAR local):
#   mvn -B clean install -Dmaven.test.skip=true && mvn -B install -Dmaven.test.skip=true -Denv=install
#   bash scripts/f2-regression.sh
#   bash scripts/f6-package-linux.sh
```

Evidencia: `dist/REMEDIATION-360-EVIDENCE.txt`; e2e local `dist/E2E-F2-PKG.log` / `dist/E2E-BUILD.log`.

### Mapas Archify e2e

| Tipo | HTML |
|------|------|
| Architecture | [superficies-remediadas.html](../.archify/20260930-2331-remediation-360-e2e/architecture/superficies-remediadas.html) |
| Workflow | [hilos-e2e.html](../.archify/20260930-2331-remediation-360-e2e/workflow/hilos-e2e.html) |
| Sequence | [protocolo-mitigado.html](../.archify/20260930-2331-remediation-360-e2e/sequence/protocolo-mitigado.html) |
| Dataflow | [xml-seguro.html](../.archify/20260930-2331-remediation-360-e2e/dataflow/xml-seguro.html) |
| Lifecycle | [deuda-cerrada.html](../.archify/20260930-2331-remediation-360-e2e/lifecycle/deuda-cerrada.html) |


## Archify (monorepo)

Suite post-migración: [`.archify/20261001-0010-monorepo-canonical/SUITE-SUMMARY.json`](../.archify/20261001-0010-monorepo-canonical/SUITE-SUMMARY.json).
