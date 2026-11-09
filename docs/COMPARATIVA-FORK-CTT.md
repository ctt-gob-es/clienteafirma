# Comparativa: fork comunitario vs Autofirma oficial (CTT)

### Propósito de este documento

- **Objetivos:** Dejar claro, en una sola tabla, qué comparte este fork con el cliente oficial y qué aporta de más (o de distinto).
- **Estructura:** Igualdad de producto → diferencias técnicas → fuera de alcance.
- **Contenido a integrar según contexto:** Actualizar esta tabla cuando cambie cripto, CI, TLS o empaquetado. Enlazada desde el [README](../README.md).

**Canónico comunitario:** [Soluciones-Alexendros/clienteafirma-alexendros](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros)  
**Upstream oficial:** [ctt-gob-es/clienteafirma](https://github.com/ctt-gob-es/clienteafirma) (Centro de Transferencia de Tecnología)  
**Línea de producto:** Autofirma **1.9.1** · Licencia dual **GPL 2+ / EUPL 1.1**

---

## Lo que debe ser igual (sustituible)

| Tema | Significado en palabras sencillas | Fork | Oficial CTT |
|------|-----------------------------------|------|-------------|
| Protocolo `afirma://` | El navegador abre Autofirma al pulsar «Firmar» en una sede | Igual | Igual |
| Formatos de firma | CAdES, XAdES, PAdES, FacturaE, cofirma y contrafirma | Mismos formatos | Mismos formatos |
| Certificado del usuario | Firma con el certificado que eliges (DNIe, FNMT, etc.) | Sí | Sí |
| Licencia | Software libre; se conservan las licencias originales (sin cambiar de licencia) | GPL 2+ / EUPL 1.1 | GPL 2+ / EUPL 1.1 |
| JDK de compilación del cliente | Java 8 para el JAR de escritorio | JDK 8 | JDK 8 |

Si una sede acepta firmas del cliente oficial, el objetivo de este fork es que **acepten las mismas firmas** generadas aquí (puerta F2: vectores de no-regresión).

---

## Diferencias técnicas del fork

| Área | Oficial CTT (`master`) | Fork comunitario | Notas |
|------|------------------------|------------------|-------|
| Biblioteca criptográfica declarada | **SpongyCastle** | **BouncyCastle 1.86** (`org.bouncycastle`, `jdk18on`) en `dependencyManagement` | PR upstream [#573](https://github.com/ctt-gob-es/clienteafirma/pull/573) cerrada sin merge; seguimiento [#572](https://github.com/ctt-gob-es/clienteafirma/issues/572). Pin elevado desde 1.78.1 para CVE Dependabot |
| SpongyCastle en el JAR de escritorio | Sí | **También**: `mvn -Denv=install` sombrea `com.madgag.spongycastle` 1.56.0.0 junto a BouncyCastle 1.86 | No presentar el JAR como libre de SpongyCastle |
| Material de firma de instalador | PFX y contraseña versionados | Fuera de `HEAD`. `make security-material` falla si vuelven | Certificado de laboratorio UJI, serie `4AFA8450`, caducado en 2019. Nota: [SEC-2026-001-material.md](security/SEC-2026-001-material.md) |
| Pruebas de firma (F2) | No hay batería pública equivalente en el repo | Scripts + vectores en `vectors/` y `scripts/f2-regression.sh` | Comprueba la generación e integridad local de los formatos F2; no demuestra por sí sola paridad bit a bit con el cliente oficial 1.9.1 |
| CI abierta | Limitada / no equivale al programa comunitario | Jobs `quality`, `test`, `community`, `build`, `smoke`, `build-baseline` | Fachada local: `make validate` |
| Empaquetado Linux de prueba | `.deb`/`.rpm` oficiales en descarga del Estado | Empaquetado de desarrollo + `scripts/mvp.sh` | No sustituye el instalador publicado en firmaelectronica.gob.es |
| Parsers XML endurecidos | `SecureXmlBuilder` en parte del árbol; sinks XXE ampliados en el fork | OOXML / XAdES trifásico vía builder seguro ([#4](https://github.com/Alexendros/clienteafirma/pull/4)); hash: validación XSD previa y parseo posterior seguro | Mitiga XXE en varios sinks; la validación XSD previa del flujo hash aún no está endurecida del todo |
| TLS estricto | Comportamiento histórico permisivo | `strictSslChecks` **activado por defecto** (#92); se puede desactivar en Preferencias | Compatibilidad con sedes vía preferencia |
| Accesibilidad (a11y) | Deuda frente a EN 301 549 | Nombres accesibles en certificado, PIN y confirmar, en `master` (`8420011`) | La sesión con Orca sigue pendiente |
| Documentación de programa | README de producto CTT | Fases F0–F10, ROADMAP, TASKS, Archify | Orientada a ciudadanos y mantenedores |

---

## Qué **no** es este fork

| Producto / servicio | ¿Lo incluye el fork? | Quién lo ofrece |
|---------------------|----------------------|-----------------|
| Plataforma `@firma` (validación federada) | No | Estado / CTT |
| VALIDe | No | Estado |
| TS@ (sellado de tiempo) | No | Estado |
| Port@firmas | No | Estado |
| Cl@ve | No | Estado |
| Apps móviles oficiales | No | Canales oficiales |

El cliente solo **genera** firmas con tu certificado. La validez jurídica completa (caducidad, revocación, política) la dan los servicios de validación del Estado.

---

## Cómo comprobar la paridad

1. `bash scripts/mvp.sh` — construye el JAR, ejecuta vectores F2 y empaqueta si puede.
2. `make validate` — contrato del repo + tests + humo comunitario.
3. Detalle ciudadano: [CIUDADANO.md](CIUDADANO.md) · Manifiesto: [propuesta-autofirma-2026.md](historico/propuesta-autofirma-2026.md).
