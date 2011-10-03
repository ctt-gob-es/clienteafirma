/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

/*******************************************************************************
 * Ruta al directorio de los instalables.                           	  	 *
 * Si no se establece, supone que estan en el mismo directorio que el HTML	 *
 * desde el que se carga el cliente.							 *
 * Las rutas absolutas deben comenzar por "file:///", "http://" o "https://"	 *
 * (por ejemplo, "file:///C:/ficheros", "http://www.mpr.es/ficheros",...)	 *
 * y las rutas relativas no pueden empezar por "/" (por ejemplo,			 *
 * "afirma/ficheros"). Se debe usar siempre el separador "/", nunca "\". 	 *
 * El fichero "version.properties" se toma de esta ruta.				 *
 ******************************************************************************/
var baseDownloadURL = "http://localhost:8080/cliente_3.2_inst";

/*******************************************************************************
 * Ruta al directorio del instalador.							 *
 * Si no se establece, supone que esta en el mismo directorio que el HTML	 *
 * desde el que se carga el cliente.							 *
 * Si es una ruta absoluta debe comenzar por "file:///", "http://" o "https://"*
 * (por ejemplo, "file:///C:/Instalador", "http://www.mpr.es/instalador",...)	 *
 * y si es una ruta relativa no puede empezar por "/" (por ejemplo,		 *
 * "afirma/Instalador"). Se debe usar siempre el separador "/", nunca "\".	 *
 ******************************************************************************/
var base = "http://localhost:8080/cliente_3.2_inst";

/*******************************************************************************
 * Algoritmo de firma. Puede ser 'SHA1withRSA', 'MD5withRSA' o, salvo que sea  *
 * firma XML, MD2withRSA. Se estable al llamar a configuraFirma en firma.js    *
 ******************************************************************************/
var signatureAlgorithm = 'SHA1withRSA'; // Valor por defecto

/*******************************************************************************
 * Formato de firma. Puede ser 'CMS', 'XADES', 'XMLDSIGN' o 'NONE'.            *
 * Se estable al llamar a configuraFirma en firma.js      				 *
 * Por defecto: CMS.										 *
 ******************************************************************************/
var signatureFormat = 'CMS'; // Valor por defecto

/*******************************************************************************
 * Mostrar los errores al usuario. Puede ser 'true' o 'false'.                 *
 * Se estable al llamar a configuraFirma en firma.js                           *
 * Por defecto: false.										 *
 ******************************************************************************/
var showErrors = 'true'; // Valor por defecto

/*******************************************************************************
 * Filtro de certificados (expresión que determina que certificados se le      *
 * permite elegir al usuario). Ver la documentación.                           *
 * Se estable al llamar a configuraFirma en firma.js                           *
 *                                                                             *
 * Ejemplos:                                                                   *
 * - Solo mostrar certificados de DNIe de firma:                               *
 * var certFilter = '{ISSUER.DN#MATCHES#{"CN=AC DNIE 00(1|2|3),OU=DNIE,'+      *
 *      'O=DIRECCION GENERAL DE LA POLICIA,C=ES"}&&{SUBJECT.DN#MATCHES#'+      *
 *      '{".*(FIRMA).*"}}}';                                                   *
 *                                                                             *
 * - Sólo mostrar certificados de la FNMT:                                     *
 * var certFilter = '{ISSUER.DN={"OU = FNMT Clase 2 CA,O= FNMT,C = ES"}}';     *
 *                                                                             *
 * - Mostrar todos los certificados menos el de validacion:                    *
 * var certFilter = '{SUBJECT.DN#NOT_MATCHES#{".*(AUTENTICACIÓN).*"}}}'        *
 ******************************************************************************/
var certFilter; // Valor por defecto

/*******************************************************************************
 * Indica si se debe mostrar una advertencia a los usuarios de Mozilla Firefox *
 * en el momento de arrancar el cliente de firma. Ya que en los navegadores	 *
 * Mozilla los certificados de los tokens externos, como las tarjetas		 *
 * inteligentes, sólo se mostrarán si estaban insertados en el momento de abrir*
 * el almacén, será necesario que los usuarios los mantengan insertados en sus *
 * correspondientes lectores desde el inicio de la aplicación. Esta opción	 *
 * permite avisar a los usuarios para que actúen de esta forma. Las distintas	 *
 * opciones que se pueden indicar y los comportamientos asociados son los	 *
 * siguientes:											 *
 * 	- true: Mostrar advertencia.								 *
 *	- false: No mostrar advertencia.							 *
 * Por defecto: true (Mostrar advertencia).						 *
 ******************************************************************************/
var showMozillaSmartCardWarning = 'true';

/*******************************************************************************
 * Mostrar los certificados caducados en la listas de seleccion de		 *
 * certificados.						 					 *
 * Por defecto: 'true'.										 *
 ******************************************************************************/
var showExpiratedCertificates = 'true';

/*******************************************************************************
 * Construccion del cliente que se instalara cuando no se indique			 *
 * explicitamente.										 *
 * Los valores aceptados son:									 *
 *   - 'LITE':     Incluye los formatos de firma CMS y CADES.			 *
 *   - 'MEDIA':    Incluye los formatos de firma de la LITE + XMLDSIG y XADES. *
 *   - 'COMPLETA': Incluye los formatos de firma de la MEDIA + PDF.		 *
 * Por defecto: 'LITE'.										 *
 ******************************************************************************/
var defaultBuild;
