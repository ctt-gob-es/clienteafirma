Name:           autofirma
Version:        1.10
Release:        1
Summary:        Aplicación de firma electrónica
Summary(ca):    Aplicació de signatura electrònica
Summary(eu):    Sinadura elektronikoaren aplikazioa
Summary(ga):    Aplicación de firma electrónica
Summary(va):    Aplicació de signatura electrònica
Summary(en):    Application of electronic signature

License:        GPLv2+ or EUPL 1.1


# Fuentes obtenidos https://github.com/ctt-gob-es/clienteafirma/tags
Source0:        sources.tar.gz

Requires: bash
# Para Fedora se require nss-tools
# Para OpenSuse se require mozilla-nss-tools
Requires: mozilla-nss-tools
AutoReqProv: no

BuildArch: noarch

%description 
Aplicación para la firma electrónica de documentos locales y operaciones de firma desde navegador web compatibles con el Cliente @firma.

%description -l ca
Aplicació per a la signatura electrònica de documents locals i operacions de signatura des de navegador web compatibles amb el Client @firma.

%description -l eu
Dokumentu lokalen sinadura elektronikorako aplikazioa eta @firma Bezeroarekin bateragarriak diren sinadura eragiketak web nabigatzailetik.

%description -l gl
Aplicación para a firma electrónica de documentos locais e operacións de sinatura desde navegador web compatibles co Cliente @firma.

%description -l va
Aplicació per a la signatura electrònica de documents locals i operacions de signatura des de navegador web compatibles amb el Client @firma.

%description -l en
Application for electronic signature of local documents and signing operations from web browser compatible with the Client @firma.

%pre
if pgrep firefox; then
  pkill firefox;
fi

%build
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
install -d -m 0755 %{buildroot}/usr/share/metainfo/
install -d -m 0755 %{buildroot}/usr/share/applications/
install -d -m 0755 %{buildroot}/usr/local/share/applications/
install -d -m 0755 %{buildroot}%{_libdir}/firefox/defaults/pref

install -m 0644 %{name}.png %{buildroot}%{_libdir}/%{name}/
install -m 0644 %{name}.svg %{buildroot}%{_libdir}/%{name}/
install -m 0755 %{name}.jar %{buildroot}%{_libdir}/%{name}/
install -m 0755 %{name}Configurador.jar %{buildroot}%{_libdir}/%{name}/
install -m 0755 %{name} %{buildroot}%{_bindir}/
install -m 0644 es.gob.afirma.metainfo.xml %{buildroot}/usr/share/metainfo
install -m 0755 %{name}.js %{buildroot}%{_libdir}/firefox/defaults/pref

%post

# Establecemos la descripcion en el idioma del sistema
if [ -z ${LANGUAGE+x} ]; then
    CURRENT_LANG=${LANG:1:2}
else
    CURRENT_LANG=$(expr $LANGUAGE : '\([a-zA-Z@]*\)')
fi

echo "Procesamos la cadena de idioma: $CURRENT_LANG"

if  [ ${CURRENT_LANG:1:2} == "en" ]; then
    DESC="Signature Tool"
    AFIRMA_LOCALE="en_US"
elif [ $CURRENT_LANG == "ca" ]; then
    DESC="Eina de signatura"
    AFIRMA_LOCALE="ca_ES"
elif [ $CURRENT_LANG == "eu" ]; then
    DESC="Sinadura tresna"
    AFIRMA_LOCALE="eu_ES"
elif [ $CURRENT_LANG == "gl" ]; then
    DESC="Ferramenta de firma"
    AFIRMA_LOCALE="gl_ES"
elif [ $CURRENT_LANG == "ca@valencia" ]; then
    DESC="Eina de signatura"
    AFIRMA_LOCALE="va_ES"
else
    DESC="Herramienta de firma"
    AFIRMA_LOCALE="es_ES"
fi

# Creamos el archivo para la integracion como herramienta de escritorio
cat > /usr/share/applications/%{name}.desktop <<EOF
[Desktop Entry]
Encoding=UTF-8
Version=1.10
Type=Application
Terminal=false
Categories=Office;Utilities;Signature;Java
Exec=java -Djdk.tls.maxHandshakeMessageSize=65536 -jar %{_libdir}/%{name}/%{name}.jar %u
Name=AutoFirma
Icon=%{_libdir}/%{name}/%{name}.png
GenericName=$DESC
Comment=$DESC
MimeType=x-scheme-handler/afirma
EOF

# Damos permisos y copiamos el archivo
chmod 755 /usr/share/applications/%{name}.desktop
cp /usr/share/applications/%{name}.desktop /usr/local/share/applications/%{name}.desktop


java -Djava.awt.headless=true -jar %{_libdir}/%{name}/%{name}Configurador.jar -install -default_language "$AFIRMA_LOCALE"

if [ -e /usr/share/applications/mimeapps.list ]; then
  echo x-scheme-handler/afirma=%{name}.desktop >> /usr/share/applications/mimeapps.list
else
  cat > /usr/share/applications/mimeapps.list <<EOF
[Default Applications]
x-scheme-handler/afirma=%{name}.desktop
EOF
fi
if [ -e /usr/share/applications/gnome-mimeapps.list ]; then
  echo x-scheme-handler/afirma=%{name}.desktop >> /usr/share/applications/gnome-mimeapps.list
fi
if [ -e /usr/local/share/applications/mimeapps.list ]; then
  echo x-scheme-handler/afirma=%{name}.desktop >> /usr/local/share/applications/mimeapps.list
else
  cat > /usr/local/share/applications/mimeapps.list <<EOF
[Default Applications]
x-scheme-handler/afirma=%{name}.desktop
EOF
fi
if [ -e /usr/local/share/applications/gnome-mimeapps.list ]; then
  echo x-scheme-handler/afirma=%{name}.desktop >> /usr/local/share/applications/gnome-mimeapps.list
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
    if [ -e /usr/local/share/applications/mimeapps.list ]; then
        sed -i '/x-scheme-handler\/afirma=%{name}.desktop/d' /usr/local/share/applications/mimeapps.list
    fi
    if [ -e /usr/share/applications/gnome-mimeapps.list ]; then
        sed -i '/x-scheme-handler\/afirma=%{name}.desktop/d' /usr/share/applications/gnome-mimeapps.list
    fi
    if [ -e /usr/local/share/applications/gnome-mimeapps.list ]; then
        sed -i '/x-scheme-handler\/afirma=%{name}.desktop/d' /usr/local/share/applications/gnome-mimeapps.list
    fi
    rm -r %{_libdir}/%{name}
    echo "Desinstalación completada con exito"
fi

%files
%license LICENSE
%dir %{_libdir}/%{name}
%attr(0755, root, root) %{_bindir}/%{name}
%attr(0755, -, -) /usr/share/metainfo/es.gob.afirma.metainfo.xml
%attr(0755, -, -) %{_libdir}/firefox/defaults/pref/%{name}.js
%attr(-, root, root) %{_libdir}/%{name}/*

%changelog
* Mon Jan 27 2025 Gobierno de España AutoFirma 1.9
- Se actualiza el icono, logo y nombre de la aplicación.
- Se permite la configuración del almacén de claves por defecto de la aplicación y si se quiere utilizar también desde las invocaciones web.
- Se permite dar de alta tarjetas inteligentes a partir de su PKCS#11 desde la interfaz de AutoFirma para tratarlas directamente como almacenes de claves.
- Se carga configuración establecida a nivel de sistema, lo que permite a los administradores establecer configuración por defecto para la aplicación.
- Se registra la compatibilidad con Java 21 para no emitir mensaje de advertencia al detectarlo.
- Se permite la generación de firmas con certificado de curva elíptica en todos los formatos.
- Se permite la validación de firmas con certificado de curva elíptica.
- Se permite que se indique como algoritmo de firma únicamente el algoritmo de huella digital, ya que el cifrado dependerá del tipo de clave del certificado.
- Actualización de la biblioteca JSON a la versión 20231013.
- Actualización de la biblioteca Apache Santuario a la versión 3.0.5.
- Se corrige que el diálogo de selección de certificados permita la selección de más de un certificado.
- Se corrige el dimensionamiento de la previsualización de la firma visible PDF que en algunos sistemas (como con Windows 11) podía hacer que quedasen componentes inaccesibles. [INC 108063]
- Se corrige que no se pudiese generar una firma de factura electrónica con la política 3.0 de FacturaE.
- Se corrige el bloqueo de algunas preferencias de AutoFirma para ajustar el comportamiento al del manual del instalación y gestión.
- Se permite la generación de firmas PDF certificadas desde la interfaz gráfica.
- Se proporciona información sobre las firmas certificadas PDF en el panel del detalle de las firmas.
- Se permiten redimensionar las ventanas de configuración para evitar problemas e visualización en Linux.
- Se agrega un nuevo panel en las preferencias para la configuración de los almacenes de claves para aglutinar la configuración de los almacenes de claves.
- Se agrega una opción en el panel de configuración de almacenes para sólo mostrar los certificados de firma en el diálogo de selección de certificados. [INC 2076804]
- Se incluye una opción en la configuración de la interfaz gráfica para sólo mostrar los certificados de seudónimo.
- Se mantiene cargado el último almacén de claves utilizado durante la sesión.
- Se permite omitir el certificado de autenticación del DNIe cuando se utiliza la interfaz de escritorio.
- Se permite ver el detalle de la validación de firmas, que muestra más información de la firma y los posibles errores encontrados.
- Cuando el usuario selecciona el filtro genérico del diálogo de guardado, se guardan las firmas con el nombre asignado en lugar de agregarle la extensión por defecto.
- Al cargar un fichero para firmar, se permite cambiar el formato de firma del fichero sin necesidad de cambiar la configuración por defecto global.
- Se modifica el texto de solicitud y confirmación de firma.
- Se identifica el atributo "id_aa_ets_longTermValidation" de las firmas CAdES como atributo longevo para que no cause problemas al construir el árbol de firmantes. [INC 102540]
- Se evita que se generen cofirmas enveloping con política de firma de la AGE. Se hace configurable y se consulta al usuario de ser necesario. [INC 2139437]
- Se corrige que las firmas enveloped no fuesen válidas cuando se omitía la transformación XPATH mediante el extraParam "avoidXpathExtraTransformsOnEnveloped".
- Se realizan cambio para una mejor adaptación al estándar y la compatibilidad con @firma.
- Se corrige el uso de rúbricas e imágenes en formatos PNG y GIF en las firmas PAdES. [INC 1763380]
- Se corrige la detección de cambios en campos de formulario tras la firma cuando los campos no estaban bien declarados. [INC 1787999]
- Se corrige que no se detecten firmas PDF cuando internamente estas definen un elemento padre. [INC 2093582]
- Se agregan nuevos patrones para insertar textos del certificado de firma en las firmas PAdES ($$PSEUDONYM$$, $$OU$$, $$OUS$$ y $$TITLE$$).
- Se evita la caducidad del WebSocket durante las operaciones para evitar que se cierre la aplicación cuando tarda más de dos minutos en completarse. [INC 2027540]
- Se mejora la identificación del error cuando se fuerza al uso de servidor intermedio en AutoFirma, pero no se proporciona la URL de los servicios.
- Se permite declarar el algoritmo de hash como algoritmo de firma, de tal forma que AutoFirma elegirá el cifrado de la firma en base al tipo de clave del certificado.
- Se inhabilita la operación getCurrentLog de AutoScript y se elimina la función de Autofirma.
- Los enlaces y ejecutables de los distintos sistemas para su uso por consola ahora ejecutan directamente la lógica de consola para evitar configuraciones y logs innecesarios.
- Se muestra en los ejemplos de sintaxis el nombre del ejecutable utilizado.
- Se utiliza la operación, algoritmo, formato y configuración del formato indicados cuando se abre la interfaz gráfica (-gui) para ejecutar una operación por consola.

