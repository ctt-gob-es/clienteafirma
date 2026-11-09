# Empaquetado Linux (F6)

- Staging DEB: `stage-deb/`
- Artefacto: `autofirma-2026_1.9.1-autofirma2026.0_all.deb` (si hay dpkg-deb)
- Staging RPM: `stage-rpm/` — embeber en los SPEC oficiales de `afirma-simple-installer/linux/`
- Protocolo: `MimeType=x-scheme-handler/afirma`
- Portal de prueba: `packaging/portal-prueba/index.html`
- DNIe: `scripts/dnie-hardware-check.sh` (requiere tarjeta + OpenSC / jmulticard)
