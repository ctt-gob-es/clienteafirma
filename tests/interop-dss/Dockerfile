# syntax=docker/dockerfile:1
# Validador propio de firmas sobre EU DSS (OT Fase 2 · T3b · A4).
# Bases fijadas por digest; usuario no root; el contenedor corre con --read-only + --tmpfs /tmp.

# ---- build ----
FROM maven:3.9.9-eclipse-temurin-17@sha256:f58d59b6273e785ac0a4477f6e9b5ba1d7731c75b906c0f7b34076f1851318cc AS build

WORKDIR /src
COPY pom.xml ./
# Capa de dependencias separada para aprovechar la cache del builder.
RUN mvn -B -q -DskipTests dependency:go-offline || true
COPY src ./src
RUN mvn -B -q -DskipTests package && \
    cp target/interop-dss-validator.jar /src/app.jar

# ---- runtime ----
FROM eclipse-temurin:17-jre-alpine@sha256:5c658c46ee58c35b00394b0ccd9c52dc76f34e66ad7bc426de7be8a466a20431

LABEL org.opencontainers.image.title="interop-dss-validator" \
      org.opencontainers.image.description="Validador de firmas con EU DSS para el gate de interoperabilidad (clienteafirma-alexendros)" \
      org.opencontainers.image.source="https://github.com/Soluciones-Alexendros/clienteafirma-alexendros" \
      org.opencontainers.image.licenses="GPL-2.0-or-later"

# Usuario sin privilegios.
RUN addgroup -S -g 10001 app && adduser -S -u 10001 -G app -H -s /sbin/nologin app

WORKDIR /app
COPY --from=build --chown=10001:10001 /src/app.jar /app/app.jar

USER 10001:10001
EXPOSE 8080

ENV JAVA_TOOL_OPTIONS="-XX:MaxRAMPercentage=75 -Djava.io.tmpdir=/tmp -Dfile.encoding=UTF-8"

# El contenedor se espera con sistema de ficheros de solo lectura: /tmp es tmpfs.
HEALTHCHECK --interval=10s --timeout=3s --start-period=10s --retries=6 \
    CMD wget -q -O- http://127.0.0.1:8080/health || exit 1

ENTRYPOINT ["java", "-jar", "/app/app.jar"]
