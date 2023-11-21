ruta_jre=../macos/java_x64
for c in *.cer; do
   sudo $ruta_jre/bin/keytool -importcert -keystore $ruta_jre/lib/security/cacerts -storetype JKS -storepass changeit -alias $c -file $c
done
ruta_jre=../macos/java_aarch64
for c in *.crt; do
   $ruta_jre/bin/keytool -importcert -keystore $ruta_jre/lib/security/cacerts -storetype JKS -storepass changeit -alias $c -file $c
done
