# Vectores de no-regresión (F2)

Generados con `autofirma.jar` construido desde **clienteafirma 1.9.1** (commit en `docs/BASELINE.txt`), mismo código que la línea de producto Autofirma 1.9.x publicada.

| Artefacto | Formato |
|-----------|--------|
| `out/plain-cades.csig` | CAdES |
| `out/sample-xades.xsig` | XAdES |
| `out/sample-pades.pdf` | PAdES |
| `out/facturae-signed.xml` | FacturaE |
| `out/plain-cades-cosign.csig` | Cofirma CAdES |
| `out/plain-cades-countersign.csig` | Contrafirma CAdES |

Certificado de prueba: `ANF_PF_Activo.pfx` (incluido en tests upstream).

Regenerar / puerta CI: `scripts/f2-regression.sh`
