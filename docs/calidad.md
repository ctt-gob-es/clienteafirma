# Calidad — cobertura de tests (JaCoCo)

Línea base medida con `mvn verify` (JDK 8) tras añadir `jacoco-maven-plugin`.

## Línea base por módulo crítico

| Módulo | Cobertura de líneas (línea base) | Umbral check (línea base − 1 pt) |
|---|---|---|
| `afirma-core` | ~14,7 % (0,1474) | 14 % (0,14) |
| `afirma-core-keystores` | ~0 % (tests dependen de entorno/DNIe y se saltan en CI) | pendiente |
| `afirma-crypto-*` | pendiente de medición en entorno | pendiente |

La regla `check` de JaCoCo se aplica por ahora en `afirma-core` (umbral 14 % de líneas, medida 0,1474 redondeada hacia abajo). El resto de módulos críticos se añadirá cuando su entorno de test sea reproducible en CI.
