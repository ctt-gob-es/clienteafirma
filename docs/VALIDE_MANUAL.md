# Ensayo manual periódico VALIDe

VALIDe ([valide.redsara.es](https://valide.redsara.es) / servicio web de demostración de `@firma`) **no** es puerta de CI:

- Es un servicio externo; los WS de producción viven en Red SARA.
- Autofirma solo genera firmas; no valida la cadena de certificados como hace la plataforma.

## Procedimiento (mensual o tras cambios F4)

1. Tomar `vectors/out/sample-pades.pdf`, `plain-cades.csig` y `sample-xades.xsig` generados por `scripts/f2-regression.sh`.
2. Subirlos a VALIDe (validación de firma) con el certificado de prueba ANF del kit.
3. Anotar resultado en `docs/valide-log.md` (fecha, OK/KO, captura).

Si falla: no mergear cambios criptográficos hasta aclarar si el vector o VALIDe/red es la causa.
