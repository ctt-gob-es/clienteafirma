# MVP operativo — Autofirma-2026

**Estado:** monorepo canónico (`Soluciones-Alexendros/clienteafirma-alexendros`) con CI auditable, vectores F2 y build de Autofirma **1.9.1**.

Este MVP no sustituye la descarga oficial ni la plataforma `@firma`. Ofrece lo que el canal gubernamental de código abierto no entrega de forma operativa: **compilar, verificar y empaquetar** el cliente con evidencia pública.

## Qué incluye (definición de “operativo”)

| Capacidad | Cómo |
|-----------|------|
| Línea base / sync pin | `docs/BASELINE.txt` + SHA en docs |
| Build JDK 8 | `scripts/mvp.sh` o CI `build-baseline` |
| JAR Autofirma | `afirma-simple/target/autofirma.jar` |
| No-regresión firma | `scripts/f2-validate.sh` (vectores + harness) |
| Empaquetado Linux | `scripts/f6-package-linux.sh` / `packaging/` |
| Cadena de suministro CI | Actions pinneadas, Renovate, actionlint |
| Evidencia local | `dist/MVP-EVIDENCE.txt` tras `scripts/mvp.sh` |

## Arranque en un comando

```bash
# Requisitos: JDK 8 + Maven (o bajo ./tools/)
bash scripts/mvp.sh
```

Opciones:

- `--skip-build` (alias `--skip-clone`) si ya tienes el JAR construido
- `--skip-package` si solo quieres build + F2
- `--require-package` (o `REQUIRE_PACKAGE=1`) para fallar si el empaquetado Linux no completa

## Qué NO es el MVP

- No es un sustituto legal de la descarga en [firmaelectronica.gob.es](https://firmaelectronica.gob.es/descargas) para trámites que exijan el instalador firmado por el Estado.
- No construye Integr@ completo (bloqueado por iText legacy HTTP).
- No valida certificados en la plataforma `@firma` / VALIDe (fuera de alcance del cliente).

## Verificación rápida

```bash
test -f afirma-simple/target/autofirma.jar
test -f dist/MVP-EVIDENCE.txt
make validate
bash scripts/f2-validate.sh   # si ya construiste
```
