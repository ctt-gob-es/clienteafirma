ruta_jre=../AutoFirma_Packages/AutoFirma.app/Contents/Resources/Home
for c in *.cer; do
   $ruta_jre/bin/keytool -importcert —keystore $ruta_jre/lib/security/cacerts -storetype JKS -storepass changeit -alias $c -file $c
done
