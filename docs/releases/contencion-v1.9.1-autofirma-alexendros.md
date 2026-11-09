# Contención de la release v1.9.1-autofirma-alexendros

Estado comprobado el 2026-11-03: release final (`isDraft=false`,
`isPrerelease=false`), con JAR, DEB y `SHA256SUMS`; sin `.asc`. Las notas
originales anunciaban además RPM, AppImage y Flatpak que no están entre los
assets.

Las notas corregidas están en
[`notas-advertencia-v1.9.1-autofirma-alexendros.md`](notas-advertencia-v1.9.1-autofirma-alexendros.md).

## Acción reservada a Alejandro

Ejecutar localmente desde la raíz del repositorio, después de revisar el fichero
de notas:

```bash
gh release edit v1.9.1-autofirma-alexendros \
  -R Soluciones-Alexendros/clienteafirma-alexendros \
  --prerelease \
  --notes-file docs/releases/notas-advertencia-v1.9.1-autofirma-alexendros.md
```

El agente **no ha ejecutado** este comando. La operación cambia la release
histórica a prerelease y sustituye sus notas; no publica ninguna release nueva.

Comprobación posterior:

```bash
gh release view v1.9.1-autofirma-alexendros \
  -R Soluciones-Alexendros/clienteafirma-alexendros \
  --json isPrerelease,assets,body
```

Resultado esperado: `isPrerelease=true`, los mismos tres assets y la advertencia
al inicio de las notas.
