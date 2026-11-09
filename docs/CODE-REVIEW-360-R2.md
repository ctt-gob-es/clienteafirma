# Revisión 360º R2 (estricta) — Autofirma-2026

**Fecha:** 2026-09-30  
**Base:** [CODE-REVIEW-360.md](CODE-REVIEW-360.md) (R1, 36/36 asertos)  
**Método:** trazado entrada→sink; solo HIGH confidence si hay entrada controlable  
**Alcance:** este monorepo (código + capa comunitaria) (auditoría; remediación en [REMEDIATION-360.md](REMEDIATION-360.md))

### Propósito de este documento

- **Objetivos:** Cerrar VERIFY-001…003, forzar protocolo/plugins/XML/packaging y anclar hallazgos nuevos con contadores reproducibles.
- **Estructura:** Delta R1→R2 → cierres VERIFY → SEC nuevos → descartes → backlog → verificación.
- **Contenido a integrar según contexto:** No inventar CVE; no cambiar defaults TLS.

---

## 1. Delta crítico R1 → R2

| Tema | R1 | R2 |
|------|----|----|
| VERIFY-001 XXE | Abierto | **Promovido → SEC-007 / SEC-008** |
| VERIFY-002 ProcessBuilder | Abierto | **Descartado** (sin entrada de atacante en sinks revisados) |
| VERIFY-003 ObjectInputStream | Abierto | **Descartado** como deserialización Java RCE (`SecureXmlBuilder`) |
| Hallazgo > SEC-001/002 | No | **SEC-007** (XXE en XML de usuario / OOXML / hash plugin) — severidad Medium–High en JDK 8 |
| Protocolo `afirma://` | Controles localhost | Confirmado: SSRF remoto a `stservlet` es **diseño**; agravante = SEC-001 |
| Plugins JAR | No cubierto | **SEC-009** trust boundary (URLClassLoader = RCE con JAR malicioso instalado por el usuario) |
| Meta fail-open | Parcial | **SEC-010** `mvp.sh` no bloquea si falla empaquetado; **SEC-011** sin gate anti-SpongyCastle en release |

**Veredicto R2:** hay superficie XXE real con documento de usuario; no hay RCE vía `ProcessBuilder`/`ObjectInputStream` clásico desde URI. El vector más grave nuevo es XML inseguro + fichero controlado por el usuario (o servidor trifásico que parsea bytes del cliente).

---

## 2. Cierre VERIFY (trazas)

| ID | Estado | Evidencia de traza | Conclusión |
|----|--------|--------------------|------------|
| VERIFY-001 | Promovido | `XmlHashDocument.load` parsea `byte[] document` de fichero de usuario **sin** `FEATURE_SECURE` / `SecureXmlBuilder`; `ContentTypeManager` parsea `[Content_Types].xml` de OOXML de usuario; trifásico `getDocumentFromBytes` igual | SEC-007 (cliente), SEC-008 (servidor lab) |
| VERIFY-001 parcial | Descartado | `XmpHelper` crea `DocumentBuilderFactory` pero parsea con `SecureXmlBuilder.getSecureDocumentBuilder()` | No explotable por esa vía |
| VERIFY-001 parcial | Descartado | `CheckHashDirDialog.generateXMLReport` solo **genera** DOM, no parsea entrada | Sin sink XXE |
| VERIFY-002 | Descartado | `DesktopUtil` / registro Windows: args constantes; `LookAndFeelManager.query` usa literales (`gsettings…`, `ps -e…`); reset = path del JAR instalado vía `getCodeSource` | Sin cadena desde `afirma://` |
| VERIFY-003 | Descartado (RCE Java) | `PdfSignResult.readObject` hace `SecureXmlBuilder.getSecureDocumentBuilder().parse(in)` — formato XML propio, no gadgets `ObjectInputStream` | Residual: XML malicioso en canal de serialización custom (mitigado por SecureXmlBuilder) |

---

## 3. Hallazgos nuevos (alta confianza)

| ID | Severidad | Issue | Evidencia | Impacto | Remediación sugerida |
|----|-----------|-------|-----------|---------|----------------------|
| SEC-007 | Medium–High | Parsers XML sin endurecer sobre entrada de usuario | `XmlHashDocument.java` ~L150–156; `ContentTypeManager.java` ~L37–52 y `loadDocument` | XXE / DoS al abrir XML de hashes o firmar OOXML malicioso (JDK 8 suele permitir entities por defecto) | Usar solo `SecureXmlBuilder`; desactivar DTDs/external entities |
| SEC-008 | Medium | Servidor trifásico parsea XML del cliente sin secure features | `XAdESTriPhaseSignerUtil.getDocumentFromBytes`; `XAdESTriPhaseSignerServerSide` | XXE/DoS en lab Tomcat si el WAR se expone | Mismo endurecimiento; no exponer WAR sin red confiable |
| SEC-009 | Info / trust | `URLClassLoader` carga JAR elegido por el usuario | `PluginLoader.loadPlugin` ~L49–60 | RCE **si** el usuario instala un plugin malicioso (consentimiento UI) | Firma/hash de plugins; avisos; lista blanca |
| SEC-010 | Low | `mvp.sh` continúa si falla `f6-package-linux.sh` | `scripts/mvp.sh` ~L129 `\|\| echo` | Evidencia MVP “OK” sin paquete usable | Flag `--require-package` o fallar en modo release |
| SEC-011 | Low | No hay aserto CI/meta que falle si el árbol de release reintroduce SpongyCastle | R1 midió SC=0 solo en clone local actual | Regresión cripto silenciosa | Añadir check en `verify` / job fork |

### R1 que se mantienen (sin re-litigar)

SEC-001 (TLS dummy), SEC-002 (JAVA_BIN absoluto), SEC-003 (branch protection), SEC-004 (Dependabot vs Renovate), SEC-005 (iText), SEC-006 (Integr@ HTTP).

### Intento de hallazgo más severo (traza negativa)

| Vector probado | Resultado |
|----------------|-----------|
| RCE vía `ProcessBuilder` desde URI `afirma://` | **No** — args no vienen de la URI |
| RCE vía deserialización Java en `PdfSignResult` | **No** — XML + `SecureXmlBuilder` |
| SSRF a `localhost` vía `stservlet` | **Bloqueado** (`LocalAccessRequestException`) |
| SSRF a host remoto + MitM TLS | Posible solo con checks SSL desactivados (**SEC-001**), no es bug nuevo de protocolo |

---

## 4. Scorecard R2 (forzado)

| Ámbito | Nota R2 | Cambio vs R1 |
|--------|---------|--------------|
| OWASP XML / XXE | 2/5 en sinks de usuario | Bajó tras SEC-007/008 |
| Protocolo local | 4/5 | Sin cambio material |
| Exec / OS | 4/5 | VERIFY-002 cerrado limpio |
| Plugins | 3/5 | Trust boundary explícito |
| Meta hygiene | 3/5 | SEC-010/011 |

---

## 5. Backlog depurado → remediación

Seguimiento vivo: [REMEDIATION-360.md](REMEDIATION-360.md) (`make remediation360`).

| Prioridad | Ítem | ID | Estado remediación |
|-----------|------|-----|--------------------|
| P0 | Endurecer parsers XML con `SecureXmlBuilder` | SEC-007, SEC-008 | **Hecho** en fork [Alexendros/clienteafirma#4](https://github.com/Alexendros/clienteafirma/pull/4) @ `fee3debe1` |
| P0 | Branch protection + alinear CHANGELOG/Renovate | SEC-003, SEC-004 | **Hecho** |
| P1 | Wrapper `.deb` sin `JAVA_BIN` absoluto | SEC-002 | Hecho (W1) |
| P1 | Preferencia TLS estricta documentada (sin default) | SEC-001 | Docs (W3); default intacto |
| P2 | Política de plugins firmados / aviso fuerte | SEC-009 | Docs en REMEDIATION W4; código diferido |
| P2 | `mvp.sh --require-package` + aserto SpongyCastle=0 | SEC-010, SEC-011 | Hecho (W1) |
| P3 | OpenPDF / Integr@ iText | SEC-005, SEC-006 | Diferido W4 (externo) |

---

## 6. Verificación matemática R2

```bash
make validate
make review360-r2
```

| Métrica | Valor (2026-09-30T19:49:06Z) |
|---------|------------------------------|
| Asertos PASS | **28** |
| Asertos FAIL | **0** |
| pass_rate | **1.0000** |
| `make validate` | EXIT **0** |

Evidencia: `dist/CODE-REVIEW-360-R2-EVIDENCE.txt`.

## 7. Mapas Archify R2

Suite: [`.archify/20260930-2150-code-review-360-r2/`](../.archify/20260930-2150-code-review-360-r2/SUITE-SUMMARY.json) (5/5 `finalize=pass`).

| Tipo | HTML |
|------|------|
| Architecture | [ataque-superficies.html](../.archify/20260930-2150-code-review-360-r2/architecture/ataque-superficies.html) |
| Workflow | [trazado-verify.html](../.archify/20260930-2150-code-review-360-r2/workflow/trazado-verify.html) |
| Sequence | [protocolo-hostil.html](../.archify/20260930-2150-code-review-360-r2/sequence/protocolo-hostil.html) |
| Dataflow | [xml-exec.html](../.archify/20260930-2150-code-review-360-r2/dataflow/xml-exec.html) |
| Lifecycle | [deuda-r2.html](../.archify/20260930-2150-code-review-360-r2/lifecycle/deuda-r2.html) |

## 8. Referencias

| Doc | Rol |
|-----|-----|
| [CODE-REVIEW-360.md](CODE-REVIEW-360.md) | R1 |
| [F4-INVENTARIO-CRIPTO.md](F4-INVENTARIO-CRIPTO.md) | TLS / iText |
| [F8-TRIFASICA.md](F8-TRIFASICA.md) | Contexto WAR lab |
