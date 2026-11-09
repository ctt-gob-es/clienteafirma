# F8 — Servidor trifásico (compatible 1.8 / 1.9)

## Artefacto

Construido en F1 con JDK 8 / `-Denv=install`:

```
afirma-server-triphase-signer/target/afirma-server-triphase-signer-2.9.1.war
```

Versión de servicio (POM): `triphase.service.version=2.9.1` · Cliente `1.9.1`.

## Compatibilidad (upstream)

Según [issue #460](https://github.com/ctt-gob-es/clienteafirma/issues/460) del CTT:

- Los servicios auxiliares y el servidor trifásico de Autofirma 1.9 son **compatibles** con clientes y Autoscript 1.8 y 1.9 (y a la inversa).
- Se recomienda actualizar el servidor para nuevas opciones de 1.9; no obliga a romper clientes antiguos.

## Jakarta (línea aparte)

- El WAR actual usa el stack **javax.servlet** (contenedores Tomcat 9 / equivalentes que ya despliegan las AAPP).
- Una línea **Jakarta** (Tomcat 10 / Jetty 12) **no sustituye** este WAR: debe versionarse aparte (p. ej. `3.x`) si se implementa.
- En este ciclo: **no se migra a Jakarta**.

## Prueba de ida y vuelta (guion)

```bash
# Desplegar afirma-server-triphase-signer-2.9.1.war en Tomcat 9
# Configurar SignatureService según manual integrador 1.9
# Firmar CAdES o PAdES trifásico desde Autofirma 1.9 / Autoscript 1.9
# Repetir con cliente 1.8 si se dispone del binario
```

Resultado local sin contenedor AAPP: WAR generado y listo; prueba E2E requiere Tomcat + red del organismo.

## Criterio de salida

- [x] WAR 2.9.1 construido
- [x] Política Jakarta documentada (línea aparte)
- [x] Compatibilidad 1.8/1.9 documentada desde upstream
