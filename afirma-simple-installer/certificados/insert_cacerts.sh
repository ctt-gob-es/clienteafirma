ruta_jre=../macos/java_x64
for c in *.cer; do
   keytool -importcert -keystore $ruta_jre/lib/security/cacerts -storetype JKS -storepass changeit -alias $c -file $c
done
for c in *.crt; do
   keytool -importcert -keystore $ruta_jre/lib/security/cacerts -storetype JKS -storepass changeit -alias $c -file $c
done

ruta_jre=../macos/java_aarch64
for c in *.cer; do
   keytool -importcert -keystore $ruta_jre/lib/security/cacerts -storetype JKS -storepass changeit -alias $c -file $c
done
for c in *.crt; do
   keytool -importcert -keystore $ruta_jre/lib/security/cacerts -storetype JKS -storepass changeit -alias $c -file $c
done
ruta_jre=../macos/java_old
for c in *.cer; do
   keytool -importcert -keystore $ruta_jre/lib/security/cacerts -storetype JKS -storepass changeit -alias $c -file $c
done
for c in *.crt; do
   keytool -importcert -keystore $ruta_jre/lib/security/cacerts -storetype JKS -storepass changeit -alias $c -file $c
done
