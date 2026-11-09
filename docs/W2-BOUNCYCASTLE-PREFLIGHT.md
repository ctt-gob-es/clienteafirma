# W2 preflight — SpongyCastle → BouncyCastle

**Fecha:** 2026-09-24 · **Baseline:** `0d7f3cf` (Autofirma 1.9.1)

## Decisión de versión

| Campo | Valor |
|-------|--------|
| Artefactos | `org.bouncycastle:bcprov-jdk18on`, `bcpkix-jdk18on`, `bcutil-jdk18on` |
| Versión fija | **1.78.1** (sin rangos Maven) |
| Runtime mínimo | JDK 8 (alineado con producto) |
| Provider JCA | `BC` (deja de usarse el literal `SC` de SpongyCastle) |

## Inventario (baseline)

| Ítem | Cantidad / nota |
|------|-----------------|
| Ficheros `.java` con `org.spongycastle` | 72 |
| Coordenadas SC en POMs | `core`, `prov`, `bcpkix-jdk15on` @ 1.58.0.0 |
| Call sites `PROVIDER = "SC"` | `CertUtil` (configurator + restoreconfig) |
| Array providers PAdES | `SunEC`, `BC`, `SC` → queda `SunEC`, `BC` |
| `SCChecker` | Ya comprobaba `org.bouncycastle.asn1.ASN1Primitive`; mensajes actualizados |
| Shade `afirma-simple` | Exclusiones antiguas `bcprov-jdk15` de PDFBox conservadas |
| jmulticard 2.1 | Sin dependencia directa SC en POM del cliente |

## Contraste seifreed

Solo referencia; no se importan commits a ciegas. La migración de este programa es mecánica + F2.

## Puertas de aceptación

1. `mvn -B clean install -DskipTests -Denv=install` (JDK 8)
2. `bash scripts/f2-regression.sh` + harness JUnit
3. Tests `-pl afirma-crypto-cades,afirma-crypto-pdf,afirma-crypto-validation` (sin Red SARA)
4. CI dual: job baseline CTT + job fork BC

## Rama

`crypto/bouncycastle-jdk18on` en fork `Alexendros/clienteafirma` (cuando exista remoto).
