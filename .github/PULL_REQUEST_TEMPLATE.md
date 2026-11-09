<!-- canon-managed: true -->

### Propósito de este documento

- **Objetivos:** Plantilla de PR para describir el cambio y exigir las comprobaciones `lint` / `test` / `smoke` y los jobs `quality` / `test` / `build` / `smoke`.
- **Estructura:** Qué cambia → checklist (Make, docs, artefactos, CI).
- **Contenido a integrar según contexto:** Adapta el checklist a este fork Maven. No copies plantillas de un SaaS. El `mvn -Denv=install` completo no es required.

## Qué cambia

<!-- feat/fix/docs + alcance en una o dos frases -->

## Checklist

- [ ] `make lint && make test && make smoke`
- [ ] Docs actualizadas (`README.md`, `CHANGELOG.md` si el cambio es visible)
- [ ] Sin keystores reales, PINs, certificados de producción ni secretos
- [ ] No se pisan textos de `license/`
- [ ] CI `quality` / `test` / `build` / `smoke` en verde

## Seguridad y estabilidad

- [ ] Commits firmados (GPG)
- [ ] Rama actualizada con merge, no con rebase
- [ ] Tests añadidos o actualizados
- [ ] Sin secretos ni certificados reales
- [ ] Toca `.github/` (la fusiona Alejandro)
- [ ] Toca criptografía o almacenes de claves (revisión manual)
- [ ] Actualiza el `CHANGELOG` si cambia el comportamiento
