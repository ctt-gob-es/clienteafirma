Name:           autofirma
Version:        1.9
Release:        1
Summary:        Aplicación de firma electrónica en escritorio y en trámites web

License:        GPLv2+ or EUPL 1.1

# Fuentes obtenidos https://github.com/ctt-gob-es/clienteafirma/tags
Source0:        sources.tar.gz

Requires: bash
# Para Fedora se require nss-tools
# Para OpenSuse se require mozilla-nss-tools
Requires: nss-tools
AutoReqProv: no

BuildArch: noarch

%description 
Aplicación para la firma electrónica de documentos locales y operaciones de firma desde navegador web compatibles con el Cliente @firma.

%pre
if pgrep firefox; then
  pkill firefox;
fi

%build
cat > %{name}.desktop <<EOF
[Desktop Entry]
Encoding=UTF-8
Version=1.9
Type=Application
Terminal=false
Categories=Office;Utilities;Signature;Java
Exec=java -Djdk.tls.maxHandshakeMessageSize=65536 -jar %{_libdir}/%{name}/%{name}.jar %u
Name=AutoFirma
Icon=%{_libdir}/%{name}/%{name}.png
GenericName=Herramienta de firma
Comment=Herramienta de firma
MimeType=x-scheme-handler/afirma
EOF

cat > %{name}.js <<EOF
pref("network.protocol-handler.app.afirma","%{_bindir}/%{name}");
pref("network.protocol-handler.warn-external.afirma",false);
pref("network.protocol-handler.external.afirma",true);
EOF

cat > %{name} <<EOF
#!/bin/bash
java -Djdk.tls.maxHandshakeMessageSize=65536 -jar %{_libdir}/%{name}/%{name}.jar "\$@"
EOF

%install
install -d -m 0755 %{buildroot}%{_libdir}/%{name}
install -d -m 0755 %{buildroot}%{_bindir}
install -d -m 0755 %{buildroot}/usr/share/applications/
install -d -m 0755 %{buildroot}%{_libdir}/firefox/defaults/pref

install -m 0644 %{name}.png %{buildroot}%{_libdir}/%{name}/
install -m 0755 %{name}.jar %{buildroot}%{_libdir}/%{name}/
install -m 0755 %{name}Configurador.jar %{buildroot}%{_libdir}/%{name}/
install -m 0755 %{name} %{buildroot}%{_bindir}/
install -m 0644 %{name}.desktop %{buildroot}/usr/share/applications
install -m 0755 %{name}.js %{buildroot}%{_libdir}/firefox/defaults/pref

%post
java -Djava.awt.headless=true -jar %{_libdir}/%{name}/%{name}Configurador.jar -install

if [ -e /usr/share/applications/mimeapps.list ]; then
  echo x-scheme-handler/afirma=%{name}.desktop >> /usr/share/applications/mimeapps.list
else
  cat > /usr/share/applications/mimeapps.list <<EOF
[Default Applications]
x-scheme-handler/afirma=%{name}.desktop
EOF
fi
if [ -f "%{_libdir}/%{name}/script.sh" ]; then
  chmod +x %{_libdir}/%{name}/script.sh
  %{_libdir}/%{name}/script.sh
fi

%preun
# Ejecutamos solo cuando sea una desinstalacion ($1 = 0),
# no cuando sea una actualizacion ($1 > 0)
if [ $1 -eq 0 ] ; then
    java -Djava.awt.headless=true -jar %{_libdir}/%{name}/%{name}Configurador.jar -uninstall
    if [ -f "%{_libdir}/%{name}/uninstall.sh" ]; then
        if pgrep firefox; then
            pkill firefox;
        fi
        chmod +x %{_libdir}/%{name}/uninstall.sh
        %{_libdir}/%{name}/uninstall.sh
    fi
fi

%postun
# Ejecutamos solo cuando sea una desinstalacion ($1 = 0),
# no cuando sea una actualizacion ($1 > 0)
if [ $1 -eq 0 ] ; then
    if [ -e /usr/share/applications/mimeapps.list ]; then
        sed -i '/x-scheme-handler\/afirma=%{name}.desktop/d' /usr/share/applications/mimeapps.list
    fi
    rm -r %{_libdir}/%{name}
    echo "Desinstalación completada con exito"
fi

%files
%license LICENSE
%dir %{_libdir}/%{name}
%attr(0755, root, root) %{_bindir}/%{name}
%attr(0755, -, -) /usr/share/applications/%{name}.desktop
%attr(0755, -, -) %{_libdir}/firefox/defaults/pref/%{name}.js
%attr(-, root, root) %{_libdir}/%{name}/*

%changelog
* Mon Feb 26 2024 Gobierno de España AutoFirma 1.9
- Se permite la configuración del almacén de claves por defecto de la aplicación y si se quiere utilizar también desde las invocaciones web.
- Se permite dar de alta tarjetas inteligentes a partir de su PKCS#11 desde la interfaz de AutoFirma para tratarlas directamente como almacenes de claves.
- Se carga configuración establecida a nivel de sistema, lo que permite a los administradores establecer configuración por defecto para la aplicación.
- Se registra la compatibilidad con Java 21 para no emitir mensaje de advertencia al detectarlo.
- Actualización de la biblioteca JSON a la versión 20231013.
- Se corrige que el diálogo de selección de certificados permita la selección de más de un certificado.
- Se corrige la restauración de algunos campos del diálogo de preferencias.
- Se agrega un nuevo panel en las preferencias para la configuración de los almacenes de claves para aglutinar la configuración de los almacenes de claves.
- Se agrega una opción en el panel de configuación de almacenes para sólo mostrar los certificados de firma en el diálogo de selección de certificados. [INC 2076804]
- Se incluye una opción en la configuración de la interfaz gráfica para sólo mostrar los certificados de seudónimo.
- Se mantiene cargado el último almacén de claves utilizado durante la sesión.
- Se omite el certificado de autenticación del DNIe cuando se utiliza la interfaz de escritorio.
- Se permite ver el detalle de la validación de firmas, que muestra más información de la firma y los posibles errores encontrados.
- Cuando el usuario selecciona el filtro genérico del diálogo de guardado, se guardan las firmas con el nombre asignado en lugar de agregarle la extensión por defecto.
- Al cargar un fichero para firmar, se permite cambiar el formato de firma del fichero sin necesidad de cambiar la configuración por defecto global.
- Se agrega el atributo "id_aa_ets_longTermValidation" como  evita que se evite generen cofirmas enveloping con política de firma de la AGE. Se hace configurable y se consulta al usuario de ser necesario. [INC 102540]
- Se evita que se generen cofirmas enveloping con política de firma de la AGE. Se hace configurable y se consulta al usuario de ser necesario. [INC 2139437]
- Se realizan cambio para una mejor adaptación al estándar y la compatibilidad con @firma.
- Se corrige el uso de rúbricas e imágenes en formatos PNG y GIF en las firmas PAdES. [INC 1763380]
- Se corrige la detección de cambios en campos de formulario tras la firma cuando los campos no estaban bien declarados. [INC 1787999]
- Se corrige que no se detecten firmas PDF cuando internamente estas definen un elemento padre. [INC 2093582]
- Se evita la caducidad del WebSocket durante las operaciones para evitar que se cierre la aplicación cuando tarda más de dos minutos en completarse. [INC 2027540]
- Se mejora la identificación del error cuando se fuerza al uso de servidor intermedio en AutoFirma, pero no se proporciona la URL de los servicios. [No reproducible con AutoScript.js]
