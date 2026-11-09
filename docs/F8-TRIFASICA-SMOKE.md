# Smoke trifásico local (W4)

## Prerrequisitos

1. JDK 8 + Maven; `mvn -Denv=install` en este repo (genera los tres WAR).
2. Docker con Compose v2.
3. Cliente Autofirma 1.9.x (baseline o fork BC) instalado o `java -jar autofirma.jar`.

## Levantar

```bash
cd packaging/triphase-compose
export WAR_DIR=../..
docker compose up -d
docker compose ps
curl -s -o /dev/null -w '%{http_code}\n' http://127.0.0.1:8080/
```

Contextos esperados (tras despliegue Tomcat):

- `/afirma-server-triphase-signer/`
- `/afirma-signature-storage/`
- `/afirma-signature-retriever/`

## Smoke mínimo (CAdES trifásico)

1. Configurar en el organismo / `SignatureService` las URLs locales de storage/retriever según el [manual integrador 1.9](https://administracionelectronica.gob.es/).
2. Firmar un PDF o texto en modo trifásico desde Autofirma apuntando al host `http://127.0.0.1:8080/afirma-server-triphase-signer/`.
3. Criterio OK: operación «terminado correctamente» y fichero de salida no vacío; F2 monofásico sigue siendo la puerta de no-regresión del meta-repo.

## Recorte consciente

Este compose **no** incluye Autoscript ni un front de sede. Sirve para validar despliegue de WARs 2.9.1 / 1.9.1 en Tomcat 9. Jakarta = línea aparte (ver `docs/F8-TRIFASICA.md`).

## Parar

```bash
docker compose down
```
