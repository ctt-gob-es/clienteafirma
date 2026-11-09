# Autofirma-2026 — Programa corregido de rescate comunitario

> **Objetivo rector:** un cliente de escritorio **construible, auditable y sustituible** respecto a Autofirma 1.9.x: mismas firmas aceptadas por el cliente oficial, mismo protocolo `afirma://`, mismos portales. Las mejoras genéricas se ofrecen al upstream. No se relicencia (GPL 2+ / EUPL 1.1) y no se sustituye el servicio de validación del Estado.

**Autor:** alexendros · **Fecha:** 23 de septiembre de 2026 (revisión del manifiesto del 14-sep-2026)
**Upstream:** [ctt-gob-es/clienteafirma](https://github.com/ctt-gob-es/clienteafirma) (Centro de Transferencia de Tecnología) · Licencia dual **GPL 2+ / EUPL 1.1**.

---

## 1. Diagnóstico (corregido frente a hechos de 2026)

Autofirma es la herramienta de escritorio de la suite @firma para firma electrónica avanzada (CAdES, XAdES, PAdES, ASiC, FacturaE). **No es la plataforma @firma:** solo genera firmas con el certificado del usuario. Validar certificados y promover firmas longevas corresponde a `@firma`, VALIDe e Integr@.

Situación real en septiembre 2026:

- **Producto publicado:** Autofirma **1.9** (Windows/Linux) y **1.9.1 / 1.9.2** (macOS) en [firmaelectronica.gob.es](https://firmaelectronica.gob.es/descargas). Maven Central se quedó en `afirma-client` **1.8.2** (2023-06-15): los binarios oficiales van por delante de los artefactos públicos.
- **Base tecnológica:** el cliente se compila para **Java 1.8**; interfaz Swing; `master` de `clienteafirma` sigue usando **SpongyCastle** (`org.spongycastle`) e **iText 2.1.7 modificado** para PAdES. Existe [ctt-gob-es/openpdf-afirma](https://github.com/ctt-gob-es/openpdf-afirma) (feb 2026), pero **no está integrado** en el cliente. Tratar SpongyCastle→BouncyCastle o iText→OpenPDF como hechos adelanta una fase abierta.
- **Linux de segunda clase:** hay `.deb`/`.rpm` oficiales; falta integración moderna (AppImage/Flatpak) y pruebas sistemáticas de Wayland, PKCS#11 y DNIe.
- **Accesibilidad:** deuda acumulada frente a **EN 301 549** (escritorio) y RD 1112/2018. WCAG 2.2 AA aplica a las **páginas de invocación** (autoscript), no al cliente Swing como estándar primario.
- **Cadena de suministro:** sin releases públicas versionadas del código, sin CI abierta ni SBOM verificable en el upstream.
- **Código público relacionado (inventario previo):** [jmulticard](https://github.com/ctt-gob-es/jmulticard) (DNIe), [integra](https://github.com/ctt-gob-es/integra), [fire](https://github.com/ctt-gob-es/fire). Forks comunitarios (p. ej. seifreed) pueden haber avanzado JDK 21 / BC; sus hitos **no se dan por ciertos** hasta compilarlos. No se duplica `openpdf-afirma`.

No es código malo: es **obra pública de alto valor social en mantenimiento contractivo**. El fork es continuador técnico, no competencia del Estado.

---

## 2. Fuera de alcance (primer ciclo)

- Plataforma `@firma` (modelo federado), VALIDe, TSA TS@ y Port@firmas (no hay producto compilable equivalente en la forja).
- Cl@ve, apps móviles Android/iOS y el cliente `@firma` obsoleto.
- Cambiar formato de firma, protocolo `afirma://` o política de revocación TLS por defecto **sin** prueba de no-regresión. La revocación se **verifica** en la línea base; no se activa a ciegas.
- VALIDe como test bloqueante de CI (es servicio web; WS de producción en Red SARA). Sirve como ensayo manual periódico.
- DNIe 3/4/5 dentro de GitHub Actions (hace falta hardware).

---

## 3. Fases y objetivos a cumplir

Cada fase termina solo si se cumple su objetivo. No se adelanta la siguiente para “modernizar”.

| Fase | Objetivo | Criterio de salida |
|------|----------|--------------------|
| **F0** | Manifiesto alineado con hechos | Este documento y el programa de trabajo dicen lo mismo |
| **F1** | Línea base Autofirma 1.9.x | Fork con upstream remoto; `mvn` en verde con **JDK 8**; módulos obsoletos aparte, sin borrar |
| **F2** | No-regresión de formatos | Vectores CAdES/XAdES/PAdES/FacturaE/cofirma/contrafirma frente al binario 1.9; puerta de cambios cripto |
| **F3** | Cadena de suministro mínima | CI Linux (+ Windows si hay runner), SBOM CycloneDX, release firmada con checksums |
| **F4** | Dependencias cripto sin cambiar la firma | BC `jdk18on` con vectores verdes **o** informe del bloqueo; OpenPDF solo con parches PAdES o aviso CVE |
| **F5** | JDK 21 como runtime | Cliente firma en JDK 21 LTS; mismos vectores y `afirma://` |
| **F6** | Paquete Linux y portales | `.deb`/`.rpm` con `afirma://`; trámite de prueba local; DNIe en hardware (guion), no en CI |
| **F7** | Accesibilidad de flujos de firma | EN 301 549 en elegir certificado / PIN / confirmar; WCAG 2.2 AA solo en HTML de invocación |
| **F8** | Servidor trifásico compatible | WAR compatible con Autofirma 1.8/1.9 y Autoscript 1.8/1.9; Jakarta en línea versionada aparte |
| **F9** | Pista @firma: Integr@ y FIRe | Build de ambos; nota de qué exige Red SARA; FIRe delega firma local al cliente F5/F6 |
| **F10** | Reintegración | PR upstream de cambios genéricos + tabla pública; el merge del Estado no es criterio |

### Orden

F1 y F2 bloquean el resto. F4 antes de F5. F6 antes de declarar el fork usable. F8 no se mezcla con Jakarta. F9 no empieza hasta que el cliente de F6 firma.

```mermaid
flowchart LR
  ciudadano[Ciudadano] --> autofirma[Autofirma_cliente]
  portal[Portal_AAPP] -->|"afirma://"| autofirma
  autofirma --> formatos[CAdES_XAdES_PAdES_FacturaE]
  portal --> trifasica[Servidor_trifasico]
  trifasica --> autofirma
  formatos --> validacion["@firma_VALIDe_Integr@"]
  integraLib[Integr@] --> validacion
  fire[FIRe] --> autofirma
  jmc[jmulticard] --> autofirma
```

---

## 4. Criterios profesionales (ajustados)

### Funcionalidad (matriz de aceptación)

| Dominio | Criterio |
|---|---|
| Formatos | CAdES/XAdES/PAdES, ASiC-S/C, FacturaE, cofirma/contrafirma, firma visible en PDF |
| Certificados | Software (.p12), DNIe (PKCS#11 / jmulticard) en hardware; filtros AGE/FNMT/Camerfirma/ACCV/ACA |
| Operaciones | Firma simple, por lotes (según 1.9), trifásica, validación de **integridad** de firmas (`afirma-crypto-validation`) |
| No-regresión | Vectores F2 en CI; VALIDe como ensayo manual periódico |

### Herramientas

- **Build:** Apache Maven, JDK 8 en F1; JDK 21 como runtime desde F5.
- **Cripto:** SpongyCastle en línea base; BouncyCastle `jdk18on` solo tras F4; OpenPDF vía `openpdf-afirma` o parches portados.
- **Entrega:** GitHub Actions, SBOM CycloneDX, checksums, firma GPG/cosign cuando haya clave.
- **Accesibilidad:** EN 301 549 (escritorio); WCAG 2.2 AA (HTML de invocación).

### Gobernanza

- Licencia upstream intacta; atribución al CTT en cada artefacto.
- Cada mejora genérica se ofrece como PR al upstream (F10).
- Nada de telemetría ni servidores propios obligatorios.

---

## 5. Objetivos a 12 meses

1. Release firmada, con SBOM, que en Linux firma CAdES, XAdES y PAdES y un portal de prueba la invoca por `afirma://`.
2. Vectores de F2 en CI: ningún cambio de dependencia entra si rompen.
3. JDK 21 ejecuta el cliente sin cambiar formatos ni el protocolo.
4. Informe EN 301 549 de los tres flujos de firma, con fallos de teclado y lector corregidos o fechados.
5. PR upstream abierto y nota pública de Integr@/FIRe: qué compila y qué exige red administrativa.
6. Documento ciudadano corto: qué firma el programa, qué no valida, y cómo comprobar la release.

---

## 6. Riesgos

| Riesgo | Mitigación |
|---|---|
| Duplicar `openpdf-afirma` o forks comunitarios | Inventario previo; coordinar antes de rehacer |
| Romper `afirma://` o la trifásica | Vectores F2 + prueba de portal F6 + F8 |
| Activar revocación TLS y dejar trámites sin red | Verificar en línea base; no activar a ciegas |
| Prometer validación VALIDe dentro del cliente | Documentar límites; Integr@/FIRe solo como pista (F9) |
| Upstream acelera y el fork queda redundante | Mejor resultado posible: reintegrar y archivar |

---

*Este documento es el manifiesto técnico del repositorio. El código habla después.*
