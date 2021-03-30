/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009-2013 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

/*******************************************************************************
 * Ruta al directorio de ficheros del Cliente.							 *
 * Si no se establece, supone que esta en el mismo directorio que el HTML	 *
 * desde el que se carga el cliente.							 *
 * Si es una ruta absoluta debe comenzar por "file:///", "http://" o "https://"*
 * (por ejemplo, "file:///C:/Instalador", "http://www.mpr.es/instalador",...)	 *
 * y si es una ruta relativa no puede empezar por "/" (por ejemplo,		 *
 * "afirma/Instalador"). Se debe usar siempre el separador "/", nunca "\".	 *
 ******************************************************************************/
var base;

/*******************************************************************************
 * Algoritmo de firma. Puede ser 'SHA1withRSA', 'SHA256withRSA', 'SHA384withRSA'*
 * o 'SHA512withRSA'. Se estable al llamar a				*
 * configuraFirma() en firma.js    												*
 * Valor por defecto: SHA512withRSA
 ******************************************************************************/
var signatureAlgorithm = 'SHA512withRSA'; // Valor por defecto

/*******************************************************************************
 * Formato de firma. Puede ser 'CMS', 'CAdES' 'XADES', 'XMLDSIGN' o 'Adobe PDF'.*
 * Se estable al llamar a configuraFirma en firma.js      				 *
 * Por defecto: CAdES.														 *
 ******************************************************************************/
var signatureFormat = 'CAdES'; // Valor por defecto

/*******************************************************************************
 * Formato de firma. Puede ser 'AES', 'ARCFOUR' 'Blowfish', 'DES', 'DESese', 'RC2',
 * 'PBEWITHSHA1ANDDESEDE', 'PBEWithSHA1AndRC2_40' o 'PBEWITHMD5ANDDES'. *
 * Se estable al llamar a configuraCifrador en crypto.js      				 *
 * Por defecto: AES.														 *
 ******************************************************************************/
var cipherAlgorithm = 'AES'; // Valor por defecto

/*******************************************************************************
 * Mostrar los errores al usuario. Puede ser 'true' o 'false'.                 *
 * Se estable al llamar a configuraFirma en firma.js                           *
 * Por defecto: false.										 *
 ******************************************************************************/
var showErrors = 'false'; // Valor por defecto

/*******************************************************************************
 * Indica si se debe mostrar una advertencia a los usuarios de Mozilla Firefox *
 * en el momento de arrancar el cliente de firma. Ya que en los navegadores	 *
 * Mozilla los certificados de los tokens externos, como las tarjetas		 *
 * inteligentes, solo se mostraron si estaban insertados en el momento de abrir*
 * el almacen, sera necesario que los usuarios los mantengan insertados en sus *
 * correspondientes lectores desde el inicio de la aplicacion. Esta opcion	 *
 * permite avisar a los usuarios para que actuen de esta forma. Las distintas	 *
 * opciones que se pueden indicar y los comportamientos asociados son los	 *
 * siguientes:											 *
 * 	- true: Mostrar advertencia.								 *
 *	- false: No mostrar advertencia.							 *
 * Por defecto: false (No mostrar advertencia).						 *
 ******************************************************************************/
var showMozillaSmartCardWarning = 'false';

/*******************************************************************************
 * Mostrar los certificados caducados en la listas de seleccion de		 *
 * certificados.						 					 *
 * Por defecto: 'true'.										 *
 ******************************************************************************/
var showExpiratedCertificates = 'true';

/********************************************************************************
 * Localizaci&oacute;n para la aplicaci&oacute;n. En base a esta				*
 * localizaci&oacute;n se selecciona el idioma de los textos de la				*
 * aplicaci&oacute;n.					 					 					*
 * Por ejemplo: 'en_UK'.										 				*
 ********************************************************************************/
var locale;

/********************************************************************************
 * Par&aacute;metros de configuraci&oacute;n de la m&aacute;quina virtual		*
 * que se requieran para el funcionamiento (no la carga) del applet.			*
 * Se estableceran como propiedades del sistema.								*
 * Por ejemplo: '-Dprueba=true'													*						
 ********************************************************************************/
var CUSTOM_JAVA_ARGUMENTS;