# Contribuir a clienteafirma

### Propósito de este documento

- **Objetivos:** Explicar setup, flujo de rama/PR y reglas para tocar este fork sin romper el árbol Maven ni las licencias duales de CTT.
- **Estructura:** Idioma → setup → flujo de trabajo → comprobaciones antes del PR → reglas.
- **Contenido a integrar según contexto:** Adapta Maven y Make de este repo. No copies un flujo npm/SaaS. El build completo `env=install` (Autofirma.jar / WARs) no es required en cada PR; vive también en [Autofirma-2026](https://github.com/Alexendros/Autofirma-2026).

Idioma: este fichero, `README.md` y las políticas de raíz en español. Identificadores de CI y nombres de jobs en inglés (`quality`, `test`, `build`, `smoke`).

Lee también [SECURITY.md](SECURITY.md).

## Setup

JDK **8** (el producto se compila a 1.8; las pruebas JUnit piden 8+). Maven 3.8+.

```bash
mvn -B -pl afirma-core test
# o
make lint test smoke
```

Perfiles Maven del producto (no los reescribas):

- `env-dev` (por defecto): módulos básicos.
- `-Denv=install`: Autofirma.jar, configurador y WARs trifásicos.
- `-Denv=deploy`: fuentes, javadoc y firma de artefactos.

## Flujo de trabajo

Rama `feat/*` / `fix/*` / `docs/*` / `chore/*` → PR contra `master`. Los agentes Cloud usan `cursor/…`.

## Antes de un PR

```bash
make lint
make test
make smoke
```

Si tocas módulos más allá de `afirma-core` y quieres un artefacto de escritorio:

```bash
mvn -B clean package -DskipTests -Denv=install
```

Ese comando es largo y no es required del job `build` de este fork.

## Reglas

- **No pises** `license/` ni cambies la dual GPL-2.0+ / EUPL-1.1 del cliente.
- Preferible proponer arreglos genéricos a [ctt-gob-es/clienteafirma](https://github.com/ctt-gob-es/clienteafirma). Este fork es copia de trabajo; el programa de rescate está en Autofirma-2026.
- Conventional Commits. Cuerpo y docs en español.
- Vulnerabilidades: [SECURITY.md](SECURITY.md), no un issue público.
- Sin secretos, keystores reales, PINs ni certificados de producción en el diff.
- Los módulos listados como obsoletos en el README no se «rehabilitan» en un PR de higiene.
