# Autofirma v1.9.1-autofirma-alexendros — release histórica

> **Advertencia:** esta release histórica no debe considerarse una release
> validada. Los artefactos no incluyen firmas OpenPGP `.asc`, el tag no está
> firmado y la versión precede al endurecimiento posterior de CI, dependencias y
> release. Queda marcada como prerelease para evitar presentarla como la versión
> final recomendada.

## Assets publicados realmente

- `autofirma-v1.9.1-autofirma-alexendros.jar`
- `autofirma-2026_1.9.1-autofirma-alexendros_all.deb`
- `SHA256SUMS`

No se publicaron RPM, AppImage, Flatpak ni firmas `.asc` en esta release. No se
afirma que el JAR esté libre de SpongyCastle ni que las firmas generadas hayan
sido validadas mediante DSS.

## Comprobación disponible

Descargar los tres assets en un mismo directorio y comprobar los checksums:

```bash
sha256sum -c SHA256SUMS
```

Esta comprobación verifica integridad frente al fichero `SHA256SUMS` publicado,
pero no sustituye una firma OpenPGP ni una attestation. Al no existir
`SHA256SUMS.asc` en esta release histórica, no puede ofrecerse aquí un comando
de verificación de firma.

## Cambios y licencia

Consultar `CHANGELOG.md` en el contenido correspondiente al tag. Licencia
conservada: GPL-2.0+ / EUPL-1.1.
