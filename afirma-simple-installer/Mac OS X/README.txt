Se pueden consultar las instrucciones para generar el instalador de AutoFirma para macOS en el manual publicado en:

https://github.com/ctt-gob-es/clienteafirma-docs/blob/master/AF_Instalador%20Mac%20OS%20X.docx

El directorio "nss" contiene las bibliotecas NSS necesarias para acceder al almacén de Firefox. Estas bibliotecas se reverencian entre si con la variable "@executable_path" lo que hace que se busquen en el directorio del ejecutable que las carga. Por este motivo deben insertarse en el directorio de ejecutables de la JRE que se empaqueta junto a la aplicación.