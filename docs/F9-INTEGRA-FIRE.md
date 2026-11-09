# F9 — Integr@ y FIRe (pista @firma)

## Compilación (2026-09-23)

| Proyecto | Resultado | Notas |
|----------|-----------|-------|
| [fire](https://github.com/ctt-gob-es/fire) | **BUILD SUCCESS** (2.4) | `mvn -DskipTests package` con JDK 8 |
| [integra](https://github.com/ctt-gob-es/integra) | **Parcial OK** | `Integra-commons` + `Integra-commons-xml` **BUILD SUCCESS**. Fallo del reactor completo en `Integra-commons-pdf-bc` (`com.lowagie:itext:2.2` vía repo HTTP bloqueado). |

Logs: `docs/build-fire.log`, `docs/build-integra.log`, `docs/build-integra-partial.log`.

## Contrato con servicios (qué exige Red SARA)

Integr@ facilita clientes Java hacia:

- WS de **@firma** (validación de certificados y firmas)
- WS / RFC 3161 de **TS@** (sellado de tiempo)
- **OCSP** de @firma y de otras CAs

URLs típicas (documentación sede; solo alcanzables desde red interadministrativa SARA o modelo federado):

- Producción WS: `https://afirma.redsara.es/afirmaws/services/`
- Desarrollo: `https://des-afirma.redsara.es/...`
- OCSP: `http://afirma.redsara.es/servidorOcsp/servidorOCSP`

**Sin Red SARA / sin instancia federada:** se puede **compilar** el cliente; no se puede **ejecutar** validación ni sellado reales.

## Relación con Autofirma / FIRe

- Autofirma **genera** firmas locales; no sustituye @firma/VALIDe.
- FIRe concentra firma local + nube; puede delegar la firma local en Autofirma (protocolo / conector). Con el cliente F5/F6 operativo, FIRe sigue siendo el orquestador en el servidor del organismo.
- Este programa **no** reimplementa el servicio de validación.

## Criterio de salida

- [x] FIRe compila
- [x] Integr@: estado documentado (bloqueo iText HTTP) + intento de módulos no-PDF
- [x] Nota “qué no podemos ejecutar sin Red SARA”
