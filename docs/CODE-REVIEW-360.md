# Revisión 360º de código — Autofirma-2026

**Fecha:** 2026-09-30  
**Meta-repo:** `316bc56fa771dec19e57e0348822f85005272741`  
**Baseline CTT (CI):** `0d7f3cf01fb65d2be5b245622d2c8f490f36e718` (producto 1.9.1)  
**Clone local `clienteafirma`:** `91b8a077e` (fork BC 1.78.1 en `master`)  
**Ámbitos:** clean code · arquitectura · estructura de ficheros · seguridad OWASP  
**Mapas interactivos:** [suite Archify](../.archify/20260930-2117-code-review-360/SUITE-SUMMARY.json)  
**Ronda R2 (estricta):** [CODE-REVIEW-360-R2.md](CODE-REVIEW-360-R2.md) — `make review360-r2`  
**Remediación:** [REMEDIATION-360.md](REMEDIATION-360.md) — `make remediation360`

### Propósito de este documento

- **Objetivos:** Dejar por escrito, en tablas claras, qué ha avanzado el programa frente a Autofirma oficial, qué riesgos de seguridad son reales (alta confianza) y qué conviene hacer después — sin remediar código en esta oleada.
- **Estructura:** Resumen → contraste oficial vs 2026 → scorecard → hallazgos → fases → Archify → backlog.
- **Contenido a integrar según contexto:** Solo hallazgos explotables o deuda ya demostrada en el árbol; no listar teoría OWASP. No pegar secretos ni P12.

---

## 1. Resumen ejecutivo

| Pregunta | Respuesta corta |
|----------|-----------------|
| ¿El meta-repo está sano? | Sí, con matices operativos (branch protection, docs/CHANGELOG vs Renovate). |
| ¿El producto local es el baseline CTT? | No: el clone local ya migró a BouncyCastle; CI de línea base sigue fijada al SHA CTT. |
| ¿Hay vulnerabilidades nuevas críticas en scripts/CI? | No halladas con alta confianza. |
| ¿Qué deuda de seguridad sigue abierta? | TLS permisivo (`DUMMY_TRUST`), iText legacy, parsers XML sueltos, wrapper `.deb` con `JAVA_BIN` absoluto. |
| ¿FIRe / Integr@ entran en el sustituto? | No. Frontera documentada; Integr@ bloqueado por iText HTTP. |

**Veredicto:** el programa cumple el objetivo rector (cliente construible/auditable/sustituible) con evidencia F0–F6/F8 y fork BC; la revisión 360º **no** recomienda cambiar defaults TLS ni fusionar a ciegas BC en el baseline CTT de CI.

---

## 2. Contraste: Autofirma oficial vs Autofirma-2026

| Aspecto | Autofirma oficial (CTT 1.9.x) | Autofirma-2026 (este meta-repo + fork) | Avance |
|---------|--------------------------------|----------------------------------------|--------|
| Código fuente público auditable del ciclo completo | Parcial (Maven Central rezagado) | Meta-repo + baseline pin + fork BC | Logrado |
| CI abierta con SHA pins | No (en el programa oficial) | Actions pinneadas + actionlint | Logrado |
| SpongyCastle 1.58 | Presente en baseline | Migrado en fork `Alexendros/clienteafirma` | Logrado en fork |
| Vectores F2 + harness | No como producto comunitario | `scripts/f2-*.sh` + `tests/validation-harness` | Logrado |
| Paquetes Linux modernos | `.deb`/`.rpm` oficiales | Staging `.deb`, AppImage, Flatpak skeleton | Parcial |
| Accesibilidad EN 301 549 | Deuda conocida | Informe F7; Orca pendiente | Parcial |
| Protocolo `afirma://` | Contrato oficial | Conservado (drop-in) | Conservado |
| VALIDe / @firma / Cl@ve | Servicios Estado | Fuera de alcance (correcto) | N/A |

---

## 3. Scorecard por ámbito (1–5)

| Ámbito | Meta-repo | `clienteafirma` local | Comentario |
|--------|-----------|------------------------|------------|
| Clean code | **4** | **3** | Scripts uniformes (`set -euo pipefail`); Java legacy denso (protocolo/UI). |
| Arquitectura / seams | **4** | **3** | Capas Maven claras; acoplamiento Swing↔protocolo histórico. |
| Estructura de ficheros | **4** | **3** | Clones en `.gitignore`; logs `docs/*.log` ignorados; CHANGELOG desalineado con Renovate. |
| Seguridad OWASP / supply-chain | **4** | **3** | CI endurecida; cripto/TLS con deuda heredada documentada. |

---

## 4. Recuperación crítica del programa (F0–F10)

| Fase | Estado declarado | Evidencia revisada | Corrección / matiz |
|------|------------------|--------------------|--------------------|
| F0 Manifiesto | Hecho | `propuesta-autofirma-2026.md` | Sigue vigente: no sustituir @firma/VALIDe. |
| F1 Baseline | Hecho | `docs/BASELINE.txt` | CI debe usar este SHA, no el HEAD local. |
| F2 Vectores | Hecho | `scripts/f2-regression.sh`, `vectors/` | OK. |
| F3 Supply chain | Hecho | workflows SHA + Renovate | Audit 0.1.1 citaba Dependabot; hoy hay `renovate.json`, no `dependabot.yml`. |
| F4 Inventario cripto | Hecho (BC aplazado en baseline) | `docs/F4-…`, fork local BC | **Ampliación:** fork ya verde BC; baseline CTT aún SC. |
| F5 JDK 21 | Hecho | `docs/F5-JDK21.md` | Runtime documentado. |
| F6 Paquetes | Hecho | `scripts/f6-*.sh`, portal | Ver hallazgo JAVA_BIN. |
| F7 A11y | Informe | `docs/F7-ACCESIBILIDAD.md` | Sesión Orca pendiente. |
| F8 Trifásico | Hecho | compose + smoke docs | OK como laboratorio. |
| F9 FIRe/Integr@ | Hecho (parcial Integr@) | `docs/F9-…` | FIRe OK; Integr@ bloqueado HTTP iText. |
| F10 Upstream | Tabla | `#572`, PR `#573` cerrada sin merge | Seguimiento abierto. |

---

## 5. Hallazgos de seguridad (alta confianza)

Solo se listan patrones con entrada/efecto comprobados en el árbol. Severidad según impacto real en el contexto del cliente de escritorio / meta-repo.

| ID | Severidad | Capa | Issue | Evidencia | Impacto | Remediación sugerida (oleada futura) |
|----|-----------|------|-------|-----------|---------|--------------------------------------|
| SEC-001 | Medium | Cliente | `DUMMY_TRUST_MANAGER` / `DUMMY_HOSTNAME_VERIFIER` aceptan cualquier certificado cuando se desactivan checks SSL | `clienteafirma/.../SslSecurityManager.java` (~L41–100); `UrlHttpManagerImpl` usa `disableSslChecks` | MitM si un trámite fuerza o hereda checks desactivados | Preferencia explícita (`strictSslChecks` ya en rama `prefs/`); **no** cambiar default |
| SEC-002 | Medium | Packaging | Wrapper `.deb` embebe `JAVA_BIN` absoluto de la máquina de build (`tools/jdk21/...`) | `scripts/f6-package-linux.sh` L19–27 | En otro host el binario apunta a ruta inexistente | Usar siempre `java` del PATH o `/usr/bin/java` en el paquete |
| SEC-003 | Low–Med | Meta docs/ops | Branch protection en `master` sigue pendiente | `docs/REPO-ENDING-AUDIT.md` | Push directo puede saltarse checks | Activar protección + checks requeridos en GitHub |
| SEC-004 | Low | Meta docs | CHANGELOG habla de Dependabot; el repo opera Renovate con automerge de patch/Actions | `CHANGELOG.md` vs `.github/renovate.json` | Confusión operativa; automerge de digests de Actions exige vigilancia | Alinear docs; revisar reglas de automerge |
| SEC-005 | Info (deuda) | Cliente | `afirma-lib-itext:1.7` (iText 2.x modificado) retenido para PAdES | `docs/F4-INVENTARIO-CRIPTO.md`; POM local | CVE heredados conocidos del ecosistema iText 2 | Coordinar `openpdf-afirma` (no duplicar fork) |
| SEC-006 | Info | Frontera | Integr@ no compila reactor completo: repo Maven HTTP + iText 2.2 | `docs/F9-INTEGRA-FIRE.md` | No afecta al cliente sustituible | Issue AF2026-6 / no bloquea Autofirma |

### Necesita verificación (no reportado como explotable)

| ID | Tema | Pregunta abierta |
|----|------|------------------|
| VERIFY-001 | `DocumentBuilderFactory` sin `SecureXmlBuilder` en algunos módulos (XMP, hash plugin, trifásico) | ¿Entrada de atacante o solo documentos ya firmados/local? |
| VERIFY-002 | `ProcessBuilder` abundante en configuradores OS | ¿Argumentos controlados por URI `afirma://` o solo rutas fijas del instalador? |
| VERIFY-003 | `ObjectInputStream` en `PdfSignResult` | ¿Deserializa bytes de red o solo estado interno? |

### Controles positivos observados

| Control | Dónde |
|---------|--------|
| `set -euo pipefail` + shebang en todos los `scripts/*.sh` | Invariante en `ci-meta-test.sh` |
| Actions pinneadas por SHA + `permissions: contents: read` | `.github/workflows/*` |
| actionlint con checksum de release | `workflow-lint.yml` |
| Clones y `docs/*.log` en `.gitignore` | `.gitignore` |
| Bloqueo de servlets en `localhost`/`127.0.0.1` | `UrlParameters` / `ProtocolInvocationLauncher` |
| `SecureXmlBuilder` con FEATURE_SECURE_PROCESSING | `afirma-core` |
| Socket de protocolo con suites TLS acotadas | `ServiceInvocationManager` |

---

## 6. Clean code y arquitectura

| Hallazgo | Ámbito | Fuerza | Notas |
|----------|--------|--------|-------|
| Fachada Make ↔ jobs CI alineada | Meta | Strong (mantener) | `quality` / `test` / `smoke` |
| Duplicación ligera de `pick_java` / rutas tools | Meta | Worth exploring | Extraer helper común si crecen scripts |
| Protocolo partido en muchos `ProtocolInvocationLauncher*` | Cliente | Worth exploring | Seam profundo pero superficie ancha |
| UI Swing acoplada a keystores/OS scripts | Cliente | Speculative | Refactor caro; no priorizar frente a cripto/TLS |
| Dos verdades de código (baseline CI vs HEAD local BC) | Programa | Strong | Documentar en cada release qué SHA se empaqueta |

### Capas del cliente (mapa mental)

| Capa | Módulos típicos | Rol |
|------|-----------------|-----|
| Entrada | `afirma-simple` (`SimpleAfirma`, `protocol/`) | CLI, UI, `afirma://`, socket local |
| Core | `afirma-core`, `afirma-core-keystores`, prefs | HTTP, XML seguro, almacenes |
| Cripto formatos | `afirma-crypto-*` | CAdES, XAdES, PAdES, batch, trifásico cliente |
| Servidor lab | `afirma-server-triphase-*` | Solo empaquetado trifásico local |
| UI util | `afirma-ui-*`, configurator | Instalación navegadores / restauración |

---

## 7. Estructura de ficheros y contenidos

| Elemento | Estado | Observación |
|----------|--------|-------------|
| `scripts/` (9 guiones) | Sano | Cabeceras homogéneas |
| `.github/workflows/` | Sano | SHA pins |
| `docs/*.md` de fases | Sano | Tablas de evidencia |
| `docs/*.log` locales | Ignorados | Bien: no van al remoto |
| `packaging/` | Parcial | Portal + compose + scripts; `.deb` ignorado |
| `clienteafirma/`, `fire/`, `integra/` | Ignorados | Correcto para meta-repo |
| `.archify/` | Versionable | Suite de revisión 360º añadida |

---

## 8. Frontera FIRe / Integr@

| Pregunta | Respuesta |
|----------|-----------|
| ¿Autofirma-2026 sustituye FIRe? | No — FIRe orquesta; puede delegar firma local. |
| ¿Sustituye Integr@ / @firma? | No — requiere Red SARA / federado. |
| ¿Qué se verificó? | FIRe compila; Integr@ parcial (bloqueo iText HTTP). |
| ¿Acción en el cliente? | Ninguna obligatoria; documentar y no hinchar alcance. |

---

## 9. Mapas Archify (revisión 360º)

Suite: `.archify/20260930-2117-code-review-360/` (gates validate/deliver/check/browser-check OK).

| Tipo | HTML | Qué muestra |
|------|------|-------------|
| Architecture | [revision-capas.html](../.archify/20260930-2117-code-review-360/architecture/revision-capas.html) | Capas meta / cliente / frontera |
| Workflow | [flujo-auditoria.html](../.archify/20260930-2117-code-review-360/workflow/flujo-auditoria.html) | Make → CI → MVP → evidencia |
| Sequence | [secuencia-afirma.html](../.archify/20260930-2117-code-review-360/sequence/secuencia-afirma.html) | `afirma://` → socket SSL → keystore |
| Dataflow | [flujo-confianza.html](../.archify/20260930-2117-code-review-360/dataflow/flujo-confianza.html) | Documento → BC/iText → firma → límite |
| Lifecycle | [ciclo-fases.html](../.archify/20260930-2117-code-review-360/lifecycle/ciclo-fases.html) | F0–F10 y deudas abiertas |

*(La arquitectura pasó gates automáticos; el aviso de cruces visuales es perceptivo, no bloqueante.)*

---

## 10. Backlog priorizado

Seguimiento vivo: [REMEDIATION-360.md](REMEDIATION-360.md) — `make remediation360`.

| Prioridad | Ítem | Origen | Estado |
|-----------|------|--------|--------|
| P0 | Branch protection + status checks en `master` | SEC-003 | Checklist humano (W3) |
| P0 | Alinear CHANGELOG (Renovate) | SEC-004 | Hecho |
| P1 | Wrapper `.deb` sin `JAVA_BIN` absoluto | SEC-002 | Hecho |
| P1 | Política release baseline CTT vs fork BC | Programa | Abierto |
| P2 | Preferencia `strictSslChecks` documentada | SEC-001 / F4 | Docs; default intacto |
| P2 | Sesión Orca F7 | ESTADO-FASES | Pendiente |
| P0/P1 | XXE SecureXmlBuilder (R2 SEC-007/008) | R2 | Hecho en clone local |
| P3 | OpenPDF / Integr@ iText | F4 / F9 / F10 | Diferido W4 |

---

## 11. Verificación matemática (obligatoria)

Los hallazgos no son solo narrativos: `scripts/verify-code-review-360.sh` **reproduce contadores** sobre el árbol y exige `fail=0`.

| Métrica | Valor medido (2026-09-30T19:24:29Z) |
|---------|--------------------------------------|
| Asertos PASS | **36** |
| Asertos FAIL | **0** |
| pass_rate | **1.0000** |
| HTML Archify con `finalize status=pass` | **5 / 5** |
| `set -euo pipefail` en scripts | **10 / 10** |
| Actions pinneadas SHA-40 | **31** coincidencias |
| `DUMMY_TRUST_MANAGER` (Java) | **4** |
| `disableSslChecks` (Java) | **8** |
| Ficheros `org.bouncycastle` | **72** |
| Ficheros `org.spongycastle` | **0** (fork local) |
| POMs `afirma-lib-itext` | **5** |
| `make validate` | EXIT **0** |
| `shellcheck -S error` | EXIT **0** |

Evidencia máquina: `dist/CODE-REVIEW-360-EVIDENCE.txt` (local, gitignored vía `dist/`).

```bash
make validate
bash scripts/verify-code-review-360.sh
# Esperado: VERIFICACIÓN OK … pass=36 fail=0
```

Interpretación: un aserto PASS sobre SEC-* **confirma que el riesgo/deuda existe en el código o docs** (hay que resolverlo en oleadas futuras). No significa “seguro”. Los CTRL-* confirman mitigaciones presentes.

## 12. Cómo reproducir el contexto de esta revisión

```bash
# Meta-repo (sin clonar producto)
make validate

# Anclaje de hallazgos 360º (requiere clone local clienteafirma para SEC-001/005)
bash scripts/verify-code-review-360.sh

# Evidencia de mapas
ls .archify/20260930-2117-code-review-360/*/*.html

# Recuerda: el clone local puede estar en BC; CI usa docs/BASELINE.txt
```

---

## 13. Referencias

| Documento | Uso |
|-----------|-----|
| [ESTADO-FASES.md](ESTADO-FASES.md) | Estado F0–F10 |
| [REPO-ENDING-AUDIT.md](REPO-ENDING-AUDIT.md) | Readiness MVP previo |
| [F4-INVENTARIO-CRIPTO.md](F4-INVENTARIO-CRIPTO.md) | SC/BC, iText, TLS |
| [F9-INTEGRA-FIRE.md](F9-INTEGRA-FIRE.md) | Frontera servicios |
| [SECURITY.md](../SECURITY.md) | Canal de aviso |
| [propuesta-autofirma-2026.md](historico/propuesta-autofirma-2026.md) | Manifiesto |
