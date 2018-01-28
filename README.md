# Cliente @firma

El Cliente @firma es uno de los productos de la Suite @firma de soluciones de identificación y firma electrónica. Se proporciona de a las Administraciones Públicas para que dispongan de los instrumentos necesarios para implementar la autenticación y firma electrónica avanzada de una forma rápida y efectiva.

El cliente de firma es una herramienta de firma electrónica en entornos de escritorio y dispositivos móviles, que funciona en forma de Applet de Java integrado en una página Web mediante JavaScript, como aplicación de escritorio, o como aplicación móvil, dependiendo del entorno del usuario.

Es software libre con licencia GPL 2+ y EUPL 1.1. Puede consular más información y el código del producto en la forja del CTT.

## Construcción del Cliente @firma

Los módulos del Cliente @firma se encuentran preparados para su compilación y empaquetado mediante Apache Maven. A continuación se indican los distintos parámetros a utilizar para construir sus artefactos según el uso que se desee dar.

A cualquiera de los comandos que se indican se le puede agregar el parámetro `-DskipTests` para omitir los tests JUnit.

### Módulos básicos y servicios

Los módulos del Cliente @firma incluidos en este repositorio se pueden construir mediante el siguiente comando de Maven.

`mvn clean install`

Este comando generará todos los módulos básicos del proyecto y los distintos servicios WAR:

* `afirma-server-simple-webstart`: WAR con el servicio para la ayuda al despliegue JNLP de AutoFirma.
* `afirma-server-triphase-signer`: WAR con el servicio para la generación de firmas trifásicas.
* `afirma-signature-retriever`: WAR con el servicio de recuperación de datos del servidor intermedio.
* `afirma-signature-storage`: WAR con el servicio de guardado de datos en el servidor intermedio.

### Artefactos desplegables y aplicaciones

Para la construcción de los productos MiniApplet y AutoFirma (JAR), ya preparados para su distribución será necesario utilizar el perfil `env=install` mediante el comando:

`mvn clean install -Denv=install`

Con esto, se podrán construir los artefactos:

* `afirma-simple`: JAR autoejecutable de AutoFirma (`AutoFirma.jar`).
* `afirma-ui-miniapplet`: JAR del MiniApplet firmado con certificado de pruebas (`miniapplet-full_X_Y.jar`).
* `afirma-ui-simple-configurator`: JAR autoejecutable del configurador necesario para la instalación de AutoFirma (`AutoFirmaConfigurador.jar`).
* `afirma-ui-simple-webstart`: JAR de AutoFirma WebStart firmado con certificado de prueba (`AutoFirmaWS__V1.6.jar`).

### Despliegue en repositorio de artefactos

Para el despliegue de los distintos módulos en un repositorio de artefactos, además de la construcción de los los propios artefactos, es necesario aportar el código fuente de la aplicación, su JavaDoc y firmar los distintos artefactos. Para evitar generar estos recursos y realizar la firma de los artefactos para la operativa ordinaria de compilación y empaquetado se ha creado un perfil `env=deploy` para que se utilice sólo cuando se va a proceder al despliegue de los artefactos en un repositorio. Se puede hacer eso mediante el comando:

`mvn clean deploy -Denv=deploy`
