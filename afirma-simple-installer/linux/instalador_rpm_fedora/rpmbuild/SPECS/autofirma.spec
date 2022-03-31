Name:           autofirma
Version:        1.8.0
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
if pgrep chromium; then
  pkill chromium;
fi
if pgrep chrome; then
  pkill chrome;
fi
if pgrep firefox; then
  pkill firefox;
fi

%build
cat > %{name}.desktop <<EOF
[Desktop Entry]
Encoding=UTF-8
Version=1.8.0
Type=Application
Terminal=false
Categories=Office;Utilities;Signature;Java
Exec=java -Djdk.tls.maxHandshakeMessageSize=50000 -jar %{_libdir}/%{name}/%{name}.jar %u
Name=AutoFirma
Icon=%{_libdir}/%{name}/%{name}.png
GenericName=Herramienta de firma
Comment=Herramienta de firma
MimeType=x-scheme-handler/afirma
EOF

cat > %{name} <<EOF
#!/bin/bash
java -Djdk.tls.maxHandshakeMessageSize=50000 -jar %{_libdir}/%{name}/%{name}.jar \$*
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
java -Djava.awt.headless=true -jar %{_libdir}/%{name}/%{name}Configurador.jar -uninstall
if [ -f "%{_libdir}/%{name}/uninstall.sh" ]; then
  if pgrep chromium; then
    pkill chromium;
  fi
  if pgrep chrome; then
    pkill chrome;
  fi
  if pgrep firefox; then
    pkill firefox;
  fi
  chmod +x %{_libdir}/%{name}/uninstall.sh
  %{_libdir}/%{name}/uninstall.sh
fi

%postun
#Clean files created in post
if [ -e /usr/share/applications/mimeapps.list ]; then
  sed -i '/x-scheme-handler\/afirma=%{name}.desktop/d' /usr/share/applications/mimeapps.list
fi
rm -r %{_libdir}/%{name}
echo "Desinstalación completada con exito"

%files
%license LICENSE
%dir %{_libdir}/%{name}
%attr(0755, root, root) %{_bindir}/%{name}
%attr(0755, -, -) /usr/share/applications/%{name}.desktop
%attr(0755, -, -) %{_libdir}/firefox/defaults/pref/%{name}.js
%attr(-, root, root) %{_libdir}/%{name}/*

%changelog
* Fri Mar 11 2022 Gobierno de España AutoFirma 1.8.0
- Se agrega mayor flexibilidad al mecanismo de plugins permitiendo nuevas opciones de seguridad e integración a nivel gráfico y en el proceso de firma por protocolo.
- Se filtra el log de la aplicación para evitar registrar datos referentes a la cuenta del usuario (ruta del directorio de usuario, alias de certificados, etc).
- Actualización de la biblioteca Java WebSockets a la versión 1.5.0.
- Actualización de la biblioteca PDFBox a la versión 2.0.25.
- Actualización de la biblioteca XMLSec a la 2.1.7.
- Se incorpora la librería 1.4 de la versión modificada de iText del cliente (basada en la v1.2.7). Esta versión corrige el problema por el que se podían recuperar firmas no declaradas en un PDF cuando también existen firmas declaradas.
- Mejoras y correcciones de accesibilidad.
- Se permite omitir el cierre de Chrome y Firefox durante la restauración si el usuario lo indica.
- Migración de la ayuda a HTML.
- Se incluye una página de ayuda con las configuraciones admitidas por los distintos formatos de firma para su uso por línea de comandos.
- Se permite la firma masiva visible de documentos PDF y que esta se agregue en más de una página.
- Se permitia seleccionar un área de firma visible para el PDF aún cuando se seleccionaba un campo de firma y se usaba el área del campo.
- No se permitía insertar una marca visible en el PDF cuando se seleccionaba un campo de firma.
- No se mostraba el mensaje de advertencia antes de firmar con las firmas visibles PDF.
- Se evitan bloqueos durante la restauración de la instalación cuando el almacén de Firefox tiene establecida una contraseña.
- Se abre el visor de firmas y se informa de que la firma no tiene certificados cuando se intenta cargar una firma con este defecto.
- Se identifica la arquitectura de las bibliotecas NSS antes de la carga para comprobar que son de la misma arquitecura que el la JRE utilizada.
- Se implementa un nuevo mecanismo de firma de lotes más óptimo y fácil de usar.
- No se reemplazaban en el texto de las firmas visibles todas las entradas en las que había que insertar la fecha.
- No se firmaba el documento cuando se seleccionaba un campo de firma preexistente para firma invisible. [INC 308732]
- Se permite la firma visible en todas o varias páginas de los documentos PDF.
- Se añaden nuevos atributos que permiten dar la opción o forzar al usuario a determinar el área y el aspecto de la firma visible.
- Se permite que el usuario seleccione el área y la apariencia de la firma PAdES en la invocación por protocolo con los parámetros "visibleSignature" y "visibleAppearance".
- Se muestran las imagenes del documento en la previsualización de la firma visible PDF incluso cuando estas están mal formadas.
- Se agrega el extraParam "includeContentHintAttribute" en las firmas CAdES para configurar que se genere una firma sin el atributo "content-hint", necesario para las firmas PAdES. [INC 1014561]
- Se bloquea la multifirma XAdES cuando se encuentran firmas con distintas versiones de XAdES en el mismo documento.
- En las firmas XAdES explícitas (configuración no recomendada) trifásicas, la aplicación que solicita la firma debe proporcionar el hash de los datos. No se calculan en servidor.
- Las cofirmas y contrafirmas manifest ya solicitan el fichero de firma cuando este no se indica.
- La firma trifásica manifest ya no solicita el fichero de datos.
- Se corrige la detección de versión de XAdES para permitir la multifirma de versiones distintas a la 1.3.2.
- Se vuelven a extraer como binarios los datos de firmas XAdES Enveloped de un nodo concreto en el que se encuentren como Base 64. [INC 1110905]
- Se externaliza la funcionalidad a un plugin.
- Se corrige la integración en Windows para poder usar el menú contextual para generar y comprobar hashes.
- Algunos errores no se trasladaban a la función callback de error.
- Existían problemas de codificación de mensajes de error devueltos.
- La cancelación de la operación de firma y guardado ya no invoca al método JavaScript de éxito.
- Se implementa la obtención del certificado de firma de los lotes cuando la comunicación es por sockets.
- En las firmas PDF por línea de comandos se permite indicar la ruta de los ficheros de la imagen de rúbrica, la imagen a estampar y los datos adjuntos, en lugar de proporcionarlos en Base 64.

