# Política de seguridad

### Propósito de este documento

- **Objetivos:** Declarar versiones soportadas, el canal privado de avisos y la superficie criptográfica (almacenes, claves, protocolos de firma) de este fork.
- **Estructura:** Versiones soportadas → cómo reportar → superficie relevante → alcance.
- **Contenido a integrar según contexto:** Adapta canales a este repo. No copies la política de un SaaS. Nunca pegues claves privadas, PINs, certificados de producción ni trazas con datos de ciudadanía.

## Versiones soportadas

| Componente | Soportada |
| ---------- | --------- |
| `master` de este fork (baseline 1.9.1) | Sí, como copia de trabajo |
| Cliente oficial CTT / instaladores AEAD | Sigue la guía de CTT; no es este repo |
| Módulos marcados «obsoletos» en el README | No |

## Cómo reportar una vulnerabilidad

**No abras un issue público** si el hallazgo puede comprometer firmas de ciudadanía, claves privadas, PINs de DNIe/PKCS#11 o almacenes de certificados.

1. Preferible: [GitHub Security Advisory](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros/security/advisories/new) en este repositorio.
2. Alternativa: correo a [operaciones@alexendros.dev](mailto:operaciones@alexendros.dev).
3. Si el defecto es del **producto oficial**, avisa también a AEAD (`soporte.afirma@correo.gob.es` según publica CTT) y/o abre un issue upstream **sin** detalles de explotación.

Incluye: commit, módulo Maven, JDK/OS, y un caso **mínimo sintético** (nunca claves reales). Responderemos en un plazo máximo de 7 días naturales.

### Plazos de respuesta

| Hito | Plazo |
| ---- | ----- |
| Acuse de recibo del aviso | ≤ 72 h |
| Evaluación inicial y clasificación de gravedad | ≤ 7 días |
| Corrección de vulnerabilidad **crítica** | ≤ 14 días |
| Corrección de vulnerabilidad **alta** | ≤ 30 días |
| Corrección de vulnerabilidad **media/baja** | ≤ 90 días |
| Divulgación coordinada (embargo) | 90 días desde el acuse, salvo acuerdo distinto |

Canal privado preferente: [GitHub Private Vulnerability Reporting](https://github.com/Soluciones-Alexendros/clienteafirma-alexendros/security/advisories/new) (activado en el repositorio). Versiones soportadas: ver tabla anterior (`master` del fork). Si no hay corrección en el plazo, se acuerda una extensión por escrito antes de divulgar.

## Superficie relevante

- El árbol actual no contiene el PFX ni los `afirma.keystore` de aplicación. La nota está en `docs/security/SEC-2026-001-material.md`. Firmar instaladores exige `AFIRMA_SIGN_PFX` y `AFIRMA_SIGN_PASS` fuera de Git.
- Almacenes de claves (CAPI, NSS/Firefox, PKCS#11, DNIe) y diálogos de PIN.
- Generación y validación CAdES / PAdES / XAdES / FacturaE y servicios trifásicos.
- Dependencias criptográficas (SpongyCastle/BouncyCastle, iText, xmlsec).
- No commitees keystores con claves reales, PINs ni certificados de producción.

## Alcance

Este repositorio es un fork de código del cliente de escritorio. No opera VALIDe, `@firma` de producción ni una TSA. Las vulnerabilidades de CAs, navegadores o del cliente instalado desde el paquete oficial de AEAD pertenecen a esos componentes, salvo que el defecto esté en este árbol.
