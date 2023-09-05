Name:           autofirma
Version:        1.8.2
Release:        1
Summary:        Aplicación de firma electrónica en escritorio y en trámites web

License:        GPLv2+ or EUPL 1.1

# Fuentes obtenidos https://github.com/ctt-gob-es/clienteafirma/archive/vVERSION.tar.gz
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
Version=1.8.2
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
* Wed May 17 2023 Gobierno de España AutoFirma 1.8.2
- Correcciones de seguridad.
- Se evita que un problema al identificar la configuración de proxy del sistema impida la ejecución de AutoFirma.
- Actualización de la biblioteca Java WebSockets a la versión 1.5.3.
- Actualización de la biblioteca Apache Santuario a la versión 2.2.3.
- Actualización de la biblioteca JSON a la versión 20230227.
- Actualización de la biblioteca proxy-vole a la versión 1.0.18.
- Actualización de la biblioteca JNA a la versión 5.13.0.
- Se elimina el envío de datos estadísticos.
- Se actualiza la ayuda con la información del plugin de hashes.
- Se modifica el filtro de certificados de seudónimo para permitir que se configuren para sólo mostrar estos certificados.
- Se configura por defecto el proveedor de firma XML de Apache con la máxima prioridad y se agrega una propiedad para configurar este comportamiento en servidor.
- Se evita la aparición del mensaje de log "[Fatal Error]" en la operación de firma XAdES/FacturaE para evitar ensuciar la salida en las llamadas por consola.
- Se corrige y flexibiliza la configuración de página de PDF para que vuelva a aceptar números seguidos de espacios en blanco y se permite exceder el número de páginas para referirse a la última.
- Se vuelve a permitir definir áreas de firma visible que sobresalgan de la página, aunque estas se recortarán al borde de ésta.
- Se cuida que el nombre de los nuevos campos de firma no coincida con el de otros campos, aunque no sean de firma.
- Se evita que se puedan firmar documentos CAdES con un PDF contenido.
- Se evita que se generen firmas con el mismo nombre que otro campo del documento, aunque no sean firmas.
- Se cambia el algoritmo interno de compresión del instalador DEB para evitar problemas con DPKG en Debian.
- Se actualiza la ayuda integrada para señalar que la funcionalidad de hashes ahora se ha externalizado a un plugin.
- Se suprime la modificación de los perfiles de Chrome durante la instalación y, con ello, tampoco es necesario cerrar el navegador al instalar y desinstalar AutoFirma.

