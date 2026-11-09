# Qué firma Autofirma-2026 (y qué no)

## Qué hace este programa

Autofirma (y este fork comunitario) es una aplicación en tu ordenador que **crea firmas electrónicas** con tu certificado digital (archivo `.p12` o DNIe). Sirve para trámites de las Administraciones Públicas que abren Autofirma desde el navegador (`afirma://`).

Puedes comprobar una release: ficheros `SHA256SUMS` y, si existe, `SHA256SUMS.asc` (firma GPG) en `dist/`.

## Qué no hace

- **No valida** si tu certificado está revocado frente a la plataforma estatal `@firma` (eso es trabajo de @firma, VALIDe o Integr@ en los servidores de la Administración).
- **No sustituye** Cl@ve, Port@firmas ni la autoridad de sellado de tiempo TS@.
- Una firma “correcta” aquí significa que el formato (CAdES, XAdES, PAdES…) se generó bien; la validez jurídica completa depende del certificado y de los servicios de validación oficiales.

## Confianza

- Código basado en el cliente oficial del CTT, licencia **GPL 2+ / EUPL 1.1**.
- Puedes compilarlo tú mismo (`docs/BASELINE.txt`) y repetir las pruebas de formatos (`scripts/f2-regression.sh`).
