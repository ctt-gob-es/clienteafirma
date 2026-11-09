# Reproduccion de H6 (T8)

`LocalChannelRepro.java` levanta el canal local real (HTTP y websocket) y comprueba tres
cosas: si se acepta una peticion sin `idsession`, que cabecera CORS se devuelve a un
origen desconocido y si el websocket acepta mensajes sin sesion.

El mismo fichero se compila contra la base (`plan/clienteafirma-fase2` en `e42f6b865`) y
contra la rama de T8. En la base solo interviene el comportamiento heredado; en la rama la
diferencia es `-DstrictWebSocketSession=true -Dwebsocket.allowedOrigins=https://app.example`.

```bash
# En el arbol que se quiera medir
mvn -B -Pautofirma -pl afirma-simple -am -DskipTests install -q
mvn -B -Pautofirma -pl afirma-simple dependency:build-classpath -Dmdep.outputFile=/tmp/cp.txt -q
mkdir -p /tmp/repro && javac -cp "afirma-simple/target/classes:$(cat /tmp/cp.txt)" \
  -d /tmp/repro tests/local-channel-repro/LocalChannelRepro.java
java -DstrictWebSocketSession=true -Dwebsocket.allowedOrigins=https://app.example \
  -cp "/tmp/repro:afirma-simple/target/classes:$(cat /tmp/cp.txt)" \
  es.gob.afirma.standalone.protocol.LocalChannelRepro
```
