# F7 — Accesibilidad (EN 301 549 / WCAG)

**Referencia escritorio:** EN 301 549 (no WCAG como estándar primario de Swing).
**Referencia HTML de invocación:** WCAG 2.2 AA.

## Localización de mensajes (flujos de firma)

Ya presentes en upstream 1.9.1 (`simpleafirmamessages_*`):

| Locale | Fichero |
|--------|---------|
| ES | `simpleafirmamessages_es_ES.properties` |
| CA | `simpleafirmamessages_ca_ES.properties` |
| EU | `simpleafirmamessages_eu_ES.properties` |
| GL | `simpleafirmamessages_gl_ES.properties` |
| VA | `simpleafirmamessages_va_ES.properties` |
| EN | `simpleafirmamessages_en_US.properties` |

Preparados para Weblate: copiar este directorio como componente; no se ha montado instancia Weblate en este ciclo.

## Auditoría de los tres flujos (checklist)

| Flujo | Teclado | Lector (Orca/NVDA) | Estado 2026-09-24 |
|-------|---------|--------------------|-------------------|
| 1. Elegir certificado | Pendiente sesión GUI | Pendiente | Código: `AccessibleName/Description` en `JList` + nombre en botón abrir almacén |
| 2. Introducir PIN | Pendiente | Pendiente | Código: `AccessibleName` en `JPasswordField` + descripción del panel (`JSEUIManager`) |
| 3. Confirmar firma | Pendiente | Pendiente | Código: nombre/descripción en diálogo y checkbox «no volver a mostrar» |

### Checklist Orca imprimible (sesión física)

1. Arrancar Orca + Autofirma con el JAR del fork (`a11y/signing-flows` o release).
2. Flujo certificado: Tab entra en la lista; Orca anuncia «Lista de certificados»; flechas cambian ítem; Enter selecciona.
3. Flujo PIN: foco en campo; anuncio del texto de prompt; Tab a Aceptar/Cancelar.
4. Flujo confirmar: anuncio del título; checkbox anunciado; Aceptar/Cancelar alcanzables.
5. Registrar fallos en issue; no bloquear build.

## Criterio de salida F7 (este ciclo)

- [x] Informe con fallos/pendientes fechados
- [x] Locales ES/CA/EU/GL/VA/EN documentados
- [x] Página de invocación mínima con base WCAG
- [ ] Correcciones de teclado/lector en los 3 flujos — **aplazadas a sesión GUI** (no bloquean build)

Cuando se complete la sesión Orca, actualizar esta tabla y abrir issues/PRs (F10).
