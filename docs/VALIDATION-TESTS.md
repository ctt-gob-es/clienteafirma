# Tests de validación (Autofirma-2026)

## Qué se valida

| Capa | Qué comprueba | Cómo |
|------|---------------|------|
| Generación F2 | Firma CLI CAdES/XAdES/PAdES/FacturaE + cofirma/contrafirma | `scripts/f2-regression.sh` |
| Integridad local | `SignValider` sobre vectores; datos correctos vs adulterados | `tests/validation-harness` |
| Upstream validation | Tests CTT de `afirma-crypto-validation` | Maven `-pl afirma-crypto-validation` |
| Upstream CAdES/PDF | Suite unitaria (sin DNIe / sin Red SARA) | Maven `-pl afirma-crypto-cades,afirma-crypto-pdf` |

## Qué no se valida aquí

- Revocación OCSP / confianza de CA de producción (certificado ANF de prueba).
- VALIDe / WS `@firma` (Red SARA) — ver `docs/VALIDE_MANUAL.md`.
- OOXML/ODF: el factory no tiene validador (`SignValiderFactory` avisa).
- DNIe hardware — `scripts/dnie-hardware-check.sh`.

## Ejecutar

```bash
export JAVA_HOME=$PWD/tools/jdk8
export PATH=$PWD/tools/apache-maven-3.9.9/bin:$JAVA_HOME/bin:$PATH
bash scripts/f2-validate.sh
# o solo harness:
bash scripts/f2-regression.sh
cd tests/validation-harness && mvn -B test -Dvectors.dir=$PWD/../../vectors
```

Criterio de aceptación del harness: la firma no está corrupta ni desalineada con los datos; un KO solo por certificado de prueba **sí** se acepta.

## Resultados locales (2026-09-23)

| Suite | Resultado |
|-------|-----------|
| `validation-harness` | 9 tests, 0 fallos |
| `afirma-crypto-validation` | 11 tests, 0 fallos |
| `afirma-crypto-cades` | 24 tests (1 skipped), 0 fallos |
| `afirma-crypto-pdf` (filtro sin Baseline/DNIe) | 31 tests (4 skipped), 0 fallos |

---

## Gates de seguridad e integridad (post-auditoría 360)

Playbook: [REMEDIATION-360.md](REMEDIATION-360.md). Criterio vivo: `make validate && make remediation360`.

| Extremo | Gate | Comando / aserto |
|---------|------|------------------|
| Meta | Empaquetado portable (SEC-002) | Wrapper de `f6-package-linux.sh` no embebe path `tools/jdk` |
| Meta | MVP release (SEC-010) | `mvp.sh --require-package` falla si falla el empaquetado |
| Meta | Docs supply-chain (SEC-004) | CHANGELOG menciona Renovate; existe `.github/renovate.json` |
| Monorepo | Anti-regresión SC (SEC-011) | `rg org.spongycastle` en el árbol Java = 0 |
| Fork | XXE cerrado (SEC-007/008) | Sinks de usuario/servidor usan `SecureXmlBuilder` |
| Fork | Controles intactos | `UrlParameters` bloquea localhost; `PdfSignResult` / `XmpHelper` siguen seguros |
| Ops | Branch protection (SEC-003) | Checklist manual en el playbook (no Make) |

```bash
make remediation360
# Evidencia: dist/REMEDIATION-360-EVIDENCE.txt
```

E2E local post-remedio (2026-09-30): build Maven → `f2-regression` → harness → `f6-package` (wrapper portable) → gates. Suite Archify: `.archify/20260930-2331-remediation-360-e2e/`.

Las auditorías R1/R2 (`make review360`, `make review360-r2`) quedan como ancla histórica; tras remediación, sus asertos de ítems cerrados comprueban la mitigación.
