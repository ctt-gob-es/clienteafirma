# Ramas heredadas — inventario y propuesta

**Fecha:** 2026-10-25 (hora de Madrid).  
**Nota:** Issues están deshabilitados en este repositorio; este documento sustituye el issue «Ramas heredadas» del plan. **No se borra ni archiva nada aquí.**

## Tabla

| Rama | SHA (corto) | Fecha | Último mensaje | Propuesta |
|------|-------------|-------|----------------|-----------|
| `master` | (rama por defecto) | 2026-10-25 | merges Ronda 2 | **Conservar** |
| `prefs/strict-ssl` | `695268c263` | 2026-09-24 | prefs: add strictSslChecks opt-in | **Conservar** (opt-in TLS; a propósito fuera de master) |
| `v1_9_2_macOS` | `b4fe147c32` | 2026-05-12 | Versión 1.9.2 | **Conservar** como referencia upstream; no mergear a ciegas |
| `Migracion_BouncyCastle` | `d606a3ea69` | 2026-03-05 | OpenPDF transparencias | **Archivar** (migración BC ya en #23 / master) |
| `develop` | `431eb8bdde` | 2026-09-10 | Corrección actualización Autofirma | **Archivar** (flujo ya es master) |
| `docs/canon-es-roadmap` | `6aeb12d2d6` | 2026-10-01 | docs ROADMAP/TASKS para release | **Archivar** tras cherry-pick selectivo si hace falta: commits de versionado `1.9.1-autofirma-alexendros` no están en master (producto sigue `1.9.1`); la rama también está detrás de master |
| `docs/semgrep-triage` | (PR #24) | 2026-10-25 | triage Semgrep Ronda 2 | No es herencia: PR activo |
| `Bloqueo_almacen_Autofirma` | `55815295b2` | 2026-02-17 | Corrección dominio SSL | **Archivar** (propuesta) |
| `Bug_ContrasenaFirefox` | `99693cfa6f` | 2026-08-24 | Restaurar Firefox | **Archivar** (propuesta) |
| `Comprobaciones_seguridad_URL_local` | `d6a6dc6cc0` | 2026-08-05 | Servicio local | **Archivar** (propuesta) |
| `Mejora_Dialogo_PIN_DNIe` | `a43f7ea7ed` | 2026-02-04 | Alto contraste | **Archivar** (propuesta) |
| `Mejoras_Carga_PKCS11_DNIe` | `e3a2c72bc1` | 2026-07-27 | Correcciones | **Archivar** (propuesta) |
| `Mejoras_configuracion_triserver` | `55664ab0f8` | 2026-01-29 | Trifásico | **Archivar** (propuesta) |
| `Mejoras_usabilidad_rendimiento` | `0cc16dc256` | 2025-12-22 | Diálogo carga | **Archivar** (propuesta) |
| `Proceso_firma_asincrono` | `612e087172` | 2025-11-18 | Corrección seguridad | **Archivar** (propuesta) |
| `soporte_servicios_jakarta` | `68b9ffa11c` | 2026-09-23 | Jakarta/Javax | **Archivar** (propuesta) |

## Acción pedida a Alejandro

1. Habilitar Issues en el repo si se prefiere el seguimiento en GitHub Issues.
2. Decidir qué ramas archivar (Archive / delete en GitHub) y cuáles conservar.
3. No se ejecuta borrado desde CI ni desde este documento.
