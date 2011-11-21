
/*******************************************************************************
 * Ruta al directorio del miniapplet.                           	  	 		*
 * Si no se establece, supone que estan en el mismo directorio que el HTML	 	*
 * desde el que se carga el cliente.							 				*
 * Las rutas absolutas deben comenzar por "file:///", "http://" o "https://"	*
 * (por ejemplo, "file:///C:/ficheros", "http://www.mpr.es/ficheros",...)	 	*
 * y las rutas relativas no pueden empezar por "/" (por ejemplo,			 	*
 * "afirma/ficheros"). Se debe usar siempre el separador "/", nunca "\". 	 	*
 * El fichero "version.properties" se toma de esta ruta.				 		*
 ******************************************************************************/
var baseDownloadURL;

/*******************************************************************************
 * Ruta al directorio en donde se encuentra el fichero de despliegue JNLP.		*
 * Si no se establece, supone que esta en el mismo directorio que el HTML	 	*
 * desde el que se carga el cliente.							 				*
 * Si es una ruta absoluta debe comenzar por "file:///", "http://" o "https://"	*
 * (por ejemplo, "file:///C:/Instalador", "http://www.mpr.es/instalador",...)	*
 * y si es una ruta relativa no puede empezar por "/" (por ejemplo,		 		*
 * "afirma/Instalador"). Se debe usar siempre el separador "/", nunca "\".	 	*
 ******************************************************************************/
var base;
