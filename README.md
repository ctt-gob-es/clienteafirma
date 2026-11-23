# Autofirma

Autofirma es uno de los productos de la Suite @firma de soluciones de identificación y firma electrónica. Se proporciona de a las Administraciones Públicas para que dispongan de los instrumentos necesarios para implementar la autenticación y firma electrónica avanzada de una forma rápida y efectiva.

Autofirma es una herramienta de firma electrónica en entornos de escritorio y dispositivos móviles, que funciona en forma de Applet de Java integrado en una página Web mediante JavaScript, como aplicación de escritorio, o como aplicación móvil, dependiendo del entorno del usuario.

Es software libre con licencia GPL 2+ y EUPL 1.1. Puede consular más información y el código del producto en la forja del CTT.

## Construcción de Autofirma

Los módulos de Autofirma se encuentran preparados para su compilación y empaquetado mediante Apache Maven. Los módulos básicos del Cliente son compatibles con Java 1.7. Sin embargo, las bibliotecas JUnit que se importan ya requieren el uso de Java 1.8 o superior, por lo que deberemos tener configurado una JDK 1.8 o superior para compilar el proyecto. La aplicación Autofirma se compila directamente para Java 1.8.

A continuación se indican los distintos parámetros a utilizar para construir sus artefactos según el uso que se desee dar. A cualquiera de los comandos que se indican se le puede agregar el parámetro `-DskipTests` para omitir los tests JUnit.

### Módulos básicos

Los módulos de Autofirma incluidos en este repositorio se pueden construir mediante el siguiente comando de Maven.

`mvn clean install`

Este comando generará todos los módulos básicos del proyecto.

### Artefactos desplegables y aplicaciones

Para la construcción de Autofirma (JAR) será necesario usar el perfil `autofirma`. Se puede activar directamente con -P o mediante el comando:

`mvn clean install -Denv=install`

Con esto, se podrán construir, entre otros, los artefactos:

* `afirma-simple`: JAR autoejecutable de Autofirma (`Autofirma.jar`).
* `afirma-ui-simple-configurator`: JAR autoejecutable del configurador necesario para la instalación de Autofirma (`AutofirmaConfigurador.jar`).

Hay dos versiones de cada uno de los servicios: los compatibles hasta Java EE 8 y los compatibles a partir de Jakarta EE 9. 

* Para generar los servicios compatibles Java EE 8 y anteriores se puede usar el perfil `services-javax`. Los artefactos generados serán:
    * `afirma-server-triphase-signer-javax`
    * `afirma-server-retriever-javax`
    * `afirma-server-storage-javax`

* Para generar los servicios compatibles Jakarta EE 9 y posteriores se puede usar el perfil `services-jakarta`.
    * `afirma-server-triphase-signer-jakarta`
    * `afirma-server-retriever-jakarta`
    * `afirma-server-storage-jakarta`
	
Aunque el id de artefacto sea distinto, los WAR generados por ambos perfiles tienen el mismo nombre para que puedan sustituirse uno por otro directamente. Estos son:

* `afirma-server-triphase-signer`: WAR con el servicio para la generación de firmas trifásicas.
* `afirma-signature-retriever`: WAR con el servicio de recuperación de datos del servidor intermedio (generado a partir de los módulos afirma-server-retriever-javax y afirma-server-retriever-jakarta).
* `afirma-signature-storage`: WAR con el servicio de guardado de datos en el servidor intermedio.


### Despliegue en repositorio de artefactos

Para el despliegue de los distintos módulos en un repositorio de artefactos, además de la construcción de los los propios artefactos, es necesario aportar el código fuente de la aplicación, su JavaDoc y firmar los distintos artefactos. Para evitar generar estos recursos y realizar la firma de los artefactos para la operativa ordinaria de compilación y empaquetado se ha creado un perfil `env-deploy` para que se utilice sólo cuando se va a proceder al despliegue de los artefactos en un repositorio. Se puede hacer eso mediante el comando:

`mvn clean deploy -Denv=deploy`

## Módulos del proyecto

El proyecto está formado por múltiples módulos, algunos de los cuales se utilizan en varias de las aplicaciones de Autofirma. Otros son los módulos de las propias aplicaciones o con recursos necesarios para su construcción o su uso.

### Módulos vigentes

A continuación, se muestra un listado de los distintos módulos actualmente en uso en el proyecto:

* `afirma-core`: Módulo con los componentes principales del proyecto.
* `afirma-core-keystores`: Módulo con las clases de gestión de almacenes de claves de usuario.
* `afirma-core-massive`: Módulo con funcionalidades para la ejecución de operaciones masivas de firma.
* `afirma-core-prefs`: Módulo con las funciones de acceso a las preferencias de Autofirma.
* `afirma-crypto-batch-client`: Módulo con el componente cliente para la invocación de las operaciones de firma de lote en servidor.
* `afirma-crypto-cades`: Módulo con la lógica de generación de las firmas CAdES (excluidas cofirmas y contrafirmas) y ASiC-CAdES.
* `afirma-crypto-cades-multi`: Módulo con la lógica de generación de las cofirmas y contrafirmas CAdES.
* `afirma-crypto-cadestri-client`: Módulo con lógica de invocación para la generación de firmas trifásicas CAdES en servidor.
* `afirma-crypto-cipher2': Módulo con la lógica de cifrado para la subida de datos al servidor intermedio.
* `afirma-crypto-cms`: Módulo con la lógica de generación de las firmas CMS.
* `afirma-crypto-core-pkcs7`: Módulo con la lógica básica de estructuras PKCS#7, necesarias para la generación de firmas ASN.1 (CAdES, PAdES, etc.).
* `afirma-crypto-core-xml`: Módulo con la lógica básica de estructuras XML, necesarias para la generación de firmas XML (XAdES, ODF, OOXML, etc.).
* `afirma-crypto-odf`: Módulo con la lógica de generación de las firmas ODF.
* `afirma-crypto-ooxml`: Módulo con la lógica de generación de las firmas OOXML.
* `afirma-crypto-padestri-client`: Módulo con lógica de invocación para la generación de firmas trifásicas PAdES en servidor.
* `afirma-crypto-pdf`: Módulo con la lógica de generación de las firmas PAdES.
* `afirma-crypto-pdf-common`: Módulo con recursos comunes utilizados en los módulos que operan sobre firmas PDF.
* `afirma-crypto-validation`: Módulo con la lógica de verificación de la integridad de las firmas CAdES, PAdES y XAdES (no incluye la comprobación de la validez de los certificados).
* `afirma-crypto-xades`: Módulo con la lógica de generación de las firmas XAdES, ASiC-XAdES y FacturaE.
* `afirma-crypto-xadestri-client`: Módulo con lógica de invocación para la generación de firmas trifásicas XAdES y de FacturaE en servidor.
* `afirma-crypto-xmlsignature`: Módulo con la lógica de generación de las firmas XMLdSig.
* `afirma-keystores-filters`: Módulo con los filtros de certificados utilizados por Autofirma.
* `afirma-keystores-jmulticard-ui`: Módulo con las interfaces gráfica para el uso de JMulticard.
* `afirma-keystores-mozilla`: Módulo para la gestión del almacén de claves de Mozilla Firefox.
* `afirma-server-retriever`: Módulo con la lógica del servicio de recuperación del servidor intermedio.
* `afirma-server-retriever-jakarta`: Módulo del servicio de recuperación del servidor intermedio compatible con Jakarta EE 9 y posteriores.
* `afirma-server-retriever-javax`: Módulo del servicio de recuperación del servidor intermedio compatible con Java EE 8 y anteriores.
* `afirma-server-storage`: Módulo con la lógica del servicio de guardado del servidor intermedio.
* `afirma-server-storage-jakarta`: Módulo del servicio de recuperación del servidor intermedio compatible con Jakarta EE 9 y posteriores.
* `afirma-server-storage-javax`: Módulo del servicio de recuperación del servidor intermedio compatible con Java EE 8 y anteriores.
* `afirma-server-triphase-signer`: Módulo con la lógica del servicio de firma trifásica y de lotes.
* `afirma-server-triphase-signer-cache`: Módulo con la interfaz que define las operaciones de guardado y recuperación de datos de caché del servidor trifásico.
* `afirma-server-triphase-signer-core`: Módulo con la funcionalidad básica de firma trifásica CAdES, PAdES, XAdES y de FacturaE.
* `afirma-server-triphase-signer-document`: Módulo con la interfaz que define las operaciones de guardado y recuperación de documentos para firmar del servidor trifásico.
* `afirma-server-triphase-signer-jakarta`: Módulo del servicio de firma trifásica compatible con Jakarta EE 9 y posteriores.
* `afirma-server-triphase-signer-javax`: Módulo del servicio de firma trifásica compatible con Java EE 8 y anteriores.
* `afirma-simple`: Módulo principal de la aplicación Autofirma.
* `afirma-simple-installer`: Módulo con los componentes para la generación de los instaladores de Autofirma.
* `afirma-simple-plugin-hash`: Módulo con el plugin de Autofirma para generación y validación de hashes.
* `afirma-simple-plugin-hash-exe`: Módulo de la aplicación EXE para el registro de las entradas de generación y validación de hashes en el menú contextual de Windows.
* `afirma-simple-plugin-validatecerts`: Módulo con el plugin de Autofirma para validación de certificados.
* `afirma-simple-plugins`: Módulo con los recursos base para la implementación de plugins de Autofirma.
* `afirma-simple-plugins-manager`: Módulo con la lógica de gestión de los plugins de Autofirma.
* `afirma-ui-core-jse`: Módulo con las interfaces gráficas genéricas usadas por las distintas aplicaciones de Autofirma.
* `afirma-ui-core-jse-keystores`: Módulo con la interfaz gráfica del diálogo de selección de certificados.
* `afirma-ui-miniapplet-deploy`: Módulo principal para el desarrollo de AutoScript.
* `afirma-ui-simple-configurator`: Módulo principal de la aplicación de configuración ejecutada durante la instalación de Autofirma.
* `afirma-ui-simple-configurator-common`: Módulo con lógica de configuración de Autofirma usada en la instalación y restauración.

### Módulos sin mantenimiento

La lista de módulos obsoletos y/o sin soporte que se conservan en el repositorio son los siguientes:

* `afirma-crypto-cipher`: __Obsoleto.__ Módulo con las clases para el cifrado sincrono y asíncrono de datos usado en el antiguo Applet de @firma y StandAlone.
* `afirma-crypto-cms-enveloper`: __Obsoleto.__ Módulo con la lógica para la generación de sobre digitales CMS utilizada en los antiguos Applet de @firma y StandAlone.
* `afirma-crypto-core-pkcs7-tsp`: __Sin soporte.__ Módulo con la lógica para agregar sellos de siempre a firmas PKCS#7 
* `afirma-crypto-jarverifier`: __Obsoleto.__ Módulo para la comprobación de la integridad de un JAR utilizada en el antiguo Applet de @firma.
* `afirma-crypto-pdf-enhancer`: __Obsoleto.__ Módulo con un cliente SOAP para el envío de peticiones a @firma para la actualización de PDF a formatos longevos.
* `afirma-keystores-capiaddressbook`: __Obsoleto.__ Módulo con la lógica de acceso a la libreta de direcciones de Windows.
* `afirma-keystores-single`: __Obsoleto.__ Módulo con un proveedor criptográfico para la gestión de certificados sueltos como si fuesen almacenes.
* `afirma-miniapplet-report`: __Obsoleto.__ Módulo para la generación de informes de las pruebas del antiguo MiniApplet.
* `afirma-miniapplet-store-testdata`: __Obsoleto.__ Módulo para el guardado de los datos de los informes de las pruebas del antiguo MiniApplet.
* `afirma-report-fail-tests`: __Obsoleto.__ Módulo para la notificación de errores de las pruebas del antiguo MiniApplet.
* `afirma-server-simple-webstart`: __Obsoleto.__ Módulo principal del servicio para la generación del JNLP para la ejecución de Autofirma WebStart.
* `afirma-standalone`: __Obsoleto.__ Módulo principal de la antigua herramienta de escritorio StandAlone.
* `afirma-standalone-installer`: __Obsoleto.__ Módulo con los componentes para la generación del instalador de la antigua herramienta de escritorio StandAlone.
* `afirma-ui-applet`: __Obsoleto.__ Módulo principal del antiguo Applet de @firma.
* `afirma-ui-applet-deploy`: __Obsoleto.__ Módulo con el JavaScript de despliegue del antiguo Applet de @firma.
* `afirma-ui-miniapplet`: __Obsoleto.__ Módulo principal del antiguo MiniApplet.
* `afirma-ui-simple-webstart`: __Obsoleto.__ Módulo principal del antiguo empaquetado de Autofirma como aplicación WebStart,
* `afirma-windows-store`: __Obsoleto.__ Módulo principal del antiguo cliente de firma para Windows 8.

No se ofrece ningún tipo de mantenimiento ni soporte sobre estos módulos.
