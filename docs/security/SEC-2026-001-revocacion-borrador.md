# Borrador de aviso — no enviado

### Propósito de este documento

- **Objetivos:** Dejar escrito el aviso al titular del certificado retirado en SEC-2026-001, sin publicarlo.
- **Estructura:** Hechos → texto propuesto → condición para enviarlo.
- **Contenido a integrar según contexto:** No incluye la contraseña ni la clave. No se envía hasta un sí explícito.

## Hechos

El certificado es autofirmado, a nombre de Universitat Jaume I (TecLab & SI), serie `4AFA8450`, huella SHA-256 `467B9AE9BC003B102D2549302316000CB6D9B7C84AAD3D9E7611408699BFC562`. Estuvo versionado en el cliente `@firma` y en este fork. Caducó el 9 de noviembre de 2019. No hay una CRL de CA que este fork pueda actualizar.

## Texto propuesto

Asunto: certificado autofirmado de laboratorio versionado en clienteafirma

El repositorio público del cliente `@firma` y el fork `Soluciones-Alexendros/clienteafirma-alexendros` contenían un PKCS#12 y almacenes JKS con un certificado autofirmado emitido a nombre de Universitat Jaume I, OU TecLab & SI, serie 4AFA8450. El fork ha dejado de usarlo para firmar instaladores o JAR y lo ha retirado del árbol actual.

El objeto sigue en el historial público, incluido el de `ctt-gob-es/clienteafirma`. Pedimos confirmación de si ese certificado llegó a usarse fuera del laboratorio y de si debe darse por no confiable en cualquier almacén que aún lo tenga. No esperamos una revocación de CA: el certificado es autofirmado y ya está caducado.

## Condición

Este texto no se ha enviado. Publicarlo, o abrir una issue upstream, requiere confirmación del mantenedor.
