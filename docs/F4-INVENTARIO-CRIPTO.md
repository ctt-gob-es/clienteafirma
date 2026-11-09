# F4 — Inventario criptográfico y de confianza TLS (línea base 1.9.1)

**Fecha:** 2026-09-23 · **Commit:** `0d7f3cf01fb65d2be5b245622d2c8f490f36e718`

## 1. SpongyCastle 1.58.0.0

| Hecho | Detalle |
|-------|---------|
| Coordenadas Maven | `com.madgag.spongycastle:core|prov|pkix:1.58.0.0` en `pom.xml` |
| Archivos `.java` con `org.spongycastle` | **72** |
| Estado upstream | Sin migrar a `org.bouncycastle:*-jdk18on` en este commit |
| Riesgo | SpongyCastle sin mantenimiento desde ~2018; fork Android de BC |

**Decisión F4:** la migración mecánica (reemplazo de imports + provider) es viable pero toca ≥72 ficheros y debe pasar `scripts/f2-regression.sh`. **No se aplica en este ciclo** hasta disponer de una rama dedicada con vectores verdes; salida de fase = este inventario + criterio de bloqueo.

## 2. iText / OpenPDF (PAdES)

| Hecho | Detalle |
|-------|---------|
| Dependencia | `es.gob.afirma.lib:afirma-lib-itext:1.7` (fork `com.aowagie.*` basado en iText 2.1.7) |
| Usos `com.aowagie` | **22** ficheros Java |
| Documentación en código | `AOPDFSigner` exige la versión modificada para PAdES |
| Trabajo paralelo CTT | Existe [ctt-gob-es/openpdf-afirma](https://github.com/ctt-gob-es/openpdf-afirma) (feb 2026); **no integrado** en el cliente |
| Bloqueo conocido | Parches PAdES (`PdfPKCS7`, `createSignature`/`preClose`, etc.) ausentes en OpenPDF stock |

**Decisión F4:** **mantener `afirma-lib-itext:1.7`** y documentar CVE heredados en avisos de release. Coordinar con `openpdf-afirma` antes de forkear otra vez. Migración = fase futura con vectores PAdES de F2 como puerta.

## 3. Confianza TLS / “revocación”

Hallazgo en [`SslSecurityManager.java`](afirma-core/src/main/java/es/gob/afirma/core/misc/http/SslSecurityManager.java):

- Existe `DUMMY_TRUST_MANAGER` / `DUMMY_HOSTNAME_VERIFIER` que **no comprueban** la cadena del servidor.
- Se usan al configurar conexiones cuando no hay truststore Afirma / en modos permisivos (`disableSslChecks` / configuradores asociados).
- La UI informa explícitamente: *“No se ha realizado la comprobación de revocación del certificado”* (`ValidationInfoDialog.40`).
- La validación de **integridad** de firmas (`afirma-crypto-validation`) separa `checkCertificates`; OCSP/CRL viven sobre todo en el plugin `validatecerts`, no en el camino crítico de firma.

**Decisión F4 (programa corregido):** **verificar, no activar a ciegas**. Activar trust estricto / OCSP por defecto rompería trámites sin red o con CAs no en el almacén. Cualquier endurecimiento exige:

1. Preferencia explícita de usuario/admin.
2. Vectores F2 + prueba de portal F6.
3. Documentación ciudadana del cambio.

## 6. Preferencia TLS estricta (opt-in; remediación SEC-001)

- **Default intacto:** no se cambia `disableSslChecks` / uso de `DUMMY_TRUST_MANAGER` por defecto (rompe sedes).
- **Opt-in en el fork:** trabajo en rama `prefs/strict-ssl` con preferencia `strictSslChecks` (**default false**). Activar solo en entornos controlados.
- **Playbook:** [REMEDIATION-360.md](REMEDIATION-360.md) W3.
- **No hacer:** activar trust estricto en builds de distribución ciudadana sin campaña de compatibilidad.

## 7. Criterio de salida de F4 (cumplido)

- [x] Inventario SpongyCastle / iText / TLS
- [x] OpenPDF: bloqueado → informe; no se duplica `openpdf-afirma`
- [x] Migración BC aplazada con condición: rama + F2 verde
- [x] Revocación TLS: documentada; sin cambio de default

## 8. Próximo intento de migración BC (checklist)

1. Rama `crypto/bouncycastle-jdk18on` desde baseline.
2. Sustituir coordenadas Maven y `org.spongycastle` → `org.bouncycastle`.
3. `Security.addProvider(new BouncyCastleProvider())` donde aplique.
4. `scripts/f2-regression.sh` + tests `-pl afirma-crypto-cades,afirma-crypto-pdf,afirma-crypto-xades` (excluir tests que llamen a `estaticos.redsara.es`).
5. PR upstream (F10) si es genérico.
