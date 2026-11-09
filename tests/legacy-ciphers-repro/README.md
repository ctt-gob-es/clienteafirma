# Reproduccion de V6.2 (T9)

`LegacyCiphersRepro.java` comprueba dos cosas sin tocar estado: el modo de bloque que se
usa por defecto al crear un cifrado y si se aceptan los algoritmos obsoletos (DES,
PBE-MD5-DES, PBE-SHA1-DESede, PBE-SHA1-RC2-40, HmacMD5 y HmacSHA1) sin ninguna opcion.

El mismo fichero se compila contra la base (`plan/clienteafirma-fase2` en `1c3a389bd`) y
contra la rama de T9; solo usa APIs presentes en ambas.

```bash
javac -cp afirma-core/target/classes -d /tmp/repro-legacy tests/legacy-ciphers-repro/LegacyCiphersRepro.java
java -cp "/tmp/repro-legacy:afirma-core/target/classes" es.gob.afirma.core.ciphers.LegacyCiphersRepro
# En la rama, para ver el efecto de la opcion explicita:
java -DallowLegacyCiphers=true -cp "/tmp/repro-legacy:afirma-core/target/classes" es.gob.afirma.core.ciphers.LegacyCiphersRepro
```
