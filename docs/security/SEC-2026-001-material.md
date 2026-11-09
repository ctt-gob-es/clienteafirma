# SEC-2026-001 — material de firma retirado de HEAD

### Propósito de este documento

- **Objetivos:** Registrar qué material operativo salió del índice, cómo identificarlo y cómo queda el empaquetado.
- **Estructura:** Identidad del certificado → ficheros retirados → empaquetado → lo que este cambio no hace.
- **Contenido a integrar según contexto:** No reproduce contraseñas ni claves. El aviso al titular está en el borrador aparte y no se ha enviado.

## Identidad

El PFX del instalador y los cinco `afirma.keystore` de aplicación son el mismo certificado autofirmado.

| Campo | Valor |
| --- | --- |
| Sujeto | `CN=Universitat Jaume I, OU=TecLab & SI, O=UJI, L=Castellon, ST=Spain, C=SP` |
| Emisor | El mismo sujeto |
| Serie | `4AFA8450` |
| Huella SHA-256 | `46:7B:9A:E9:BC:00:3B:10:2D:25:49:30:23:16:00:0C:B6:D9:B7:C8:4A:AD:3D:9E:76:11:40:86:99:BF:C5:62` |
| NotBefore | 2009-11-11 |
| NotAfter | 2019-11-09 |

Caducó en 2019 y no hay una CA pública que publique su revocación. Cualquiera con un clon histórico puede seguir produciendo una firma que presente esta identidad. Por eso el fork deja de usarlo. Reescribir el historial no lo retira de `ctt-gob-es/clienteafirma` y no forma parte de este cambio.

`afirma-windows-store/App1/App1_TemporaryKey.pfx` es otro almacén, distinto de este certificado. No se abrió. Su SHA-256 de fichero era `422cd926813071aded01052cf24508840cf518f3d0b9a3b4544fb30b3652ee25`.

## Ficheros retirados del índice

- `afirma-simple-installer/Autofirma_sign.pfx`
- `afirma-simple/afirma.keystore`
- `afirma-standalone/afirma-ui-standalone/afirma.keystore`
- `afirma-ui-applet/afirma.keystore`
- `afirma-ui-miniapplet/afirma.keystore`
- `afirma-ui-simple-webstart/afirma.keystore`
- `afirma-windows-store/App1/App1_TemporaryKey.pfx`

Los `.bat` de `signtool` ya no llevan contraseña. Firman solo si existen `AFIRMA_SIGN_PFX` y `AFIRMA_SIGN_PASS`. Los POM dejan la contraseña en `${env.AFIRMA_KEYSTORE_PASS}` y la ruta en `${env.AFIRMA_KEYSTORE_PATH}`. `afirma.code.sign.skip` vale `true` salvo que el build pase `-Dafirma.code.sign.skip=false`.

Los almacenes bajo `src/test/resources/` siguen en el repositorio. Su ruta y su SHA-256 están en `scripts/ci/crypto-material-allowlist.txt`. Sustituirlos por una CA de prueba local es trabajo posterior, no de esta contención.

## Comprobación

```bash
bash scripts/ci/test-detect-crypto-material.sh
bash scripts/ci/detect-crypto-material.sh
```

La primera debe terminar con las mutaciones en su estado esperado. La segunda debe salir 0 en este árbol.
