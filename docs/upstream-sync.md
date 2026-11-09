# Sincronización con el oficial (upstream)

## Último commit integrado

- Fecha de integración: 2026-10-31
- Repositorio oficial: `ctt-gob-es/clienteafirma` (rama `master`)
- Último commit integrado del oficial: `bd9254be70004be095b1251b5a4af15b9d392dd9`
- Enlace: https://github.com/ctt-gob-es/clienteafirma/commit/bd9254be70004be095b1251b5a4af15b9d392dd9
- Título: Corregir descripción paquete DEB
- Cambios integrados:
  1. Corregir el campo Description duplicado en el control del paquete DEB.
  2. Descartar entradas de clave simétrica (SecretKeyEntry) de los almacenes NSS.
  3. Corregir descripción paquete DEB.

## Procedimiento

Periodicidad: mensual (primer lunes de cada mes) o ante aviso de release del oficial.

1. `git fetch upstream` y `git fetch origin`.
2. Crear rama de sync: `git checkout -b sync/upstream-AAAA-MM origin/master`.
3. Comprobar commits nuevos del oficial: `git log --oneline origin/master..upstream/master`.
4. Comparar por contenido las rutas comunes: `git diff upstream/master master --stat -- <rutas comunes>` (excluir ficheros con diferencias deliberadas).
5. Integrar commit a commit, preferentemente con cherry-pick firmado: `git cherry-pick -S <sha>` (resolver conflictos manualmente, nunca con `-X theirs` ciego).
6. Para cada commit integrado, ejecutar `make test` (afirma-core) y, si toca cripto, los tests del módulo afectado.
7. Actualizar la tabla de "Diferencias deliberadas" si aparece una nueva divergencia intencional; documentar motivo y commit.
8. Actualizar este documento: "Último commit integrado" con el SHA nuevo, fecha y cambios.
9. Commit firmado: `git commit -S -m "sync: upstream <sha corto>"`.
10. Push y PR firmado hacia `master`: `git push -u origin sync/upstream-AAAA-MM` y `gh pr create`. El PR no toca `.github/` salvo que el upstream haya cambiado workflows (entonces lo fusiona Alejandro).
11. Tras fusionar, borrar la rama de sync (solo con OK de Alejandro).

Reglas: ramas se actualizan con `git merge origin/master`, nunca rebase. Fusión siempre squash. Commits firmados con GPG.

## Diferencias deliberadas

| # | Fichero | Motivo | Commit fork | Comprobación |
|---|---------|--------|-------------|--------------|
| 1 | `afirma-core-keystores/src/main/java/es/gob/afirma/keystores/KeyStoreUtilities.java` | Endurecimiento frente a inyección de comandos (evita concatenación de rutas en nombre corto de Windows; usa variable de entorno). | `d22ca8c01` | `git diff upstream/master origin/master -- afirma-core-keystores/src/main/java/es/gob/afirma/keystores/KeyStoreUtilities.java` debe mostrar solo ese bloque. |

Los demás ficheros son idénticos al oficial. Si una sincronización introduce una nueva diferencia deliberada, añadirla a esta tabla con motivo y commit.

## Verificación

- El commit citado en "Último commit integrado" debe existir: `git fetch upstream && git cat-file -t <sha>` → `commit`.
