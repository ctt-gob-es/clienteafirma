/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

/** Nombre de las preferencias de configuraci&oacute;n del programa.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PreferencesManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Objecto general de preferencias donde se guarda la configuraci&oacute;n de la
	 * aplicaci&oacute;n. */
	private static final Preferences USER_PREFERENCES;
	private static final Preferences SYSTEM_PREFERENCES;
	private static final Properties DEFAULT_PREFERENCES;
	private static final String TRUE_VALUE = "true"; //$NON-NLS-1$
	private static final String FALSE_VALUE = "false"; //$NON-NLS-1$

	static {
		USER_PREFERENCES = Preferences.userNodeForPackage(PreferencesManager.class);
		SYSTEM_PREFERENCES = Preferences.systemNodeForPackage(PreferencesManager.class);

		DEFAULT_PREFERENCES = new Properties();
		try {
			DEFAULT_PREFERENCES.load(PreferencesManager.class.getResourceAsStream("/properties/preferences.properties")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.severe(
				"No han podido cargarse los valores por defecto del fichero de configuracion de preferencias, se usaran los valores por defecto: " //$NON-NLS-1$
					+ e
			);
		}
	}

	/** Origen del valor de las preferencias. */
	public static enum PreferencesSource {
		/** Preferencias establecidas por el usuario. */
		USER,
		/** Preferencias establecidas por el administrador a nivel de sistema. */
		SYSTEM,
		/** Valor por defecto de las preferencias. */
		DEFAULT
	}

	private PreferencesManager() {
		// No permitimos la instanciacion
	}

	//**************************************************************************************************************************
	//**************** VALORES GENERALES ***************************************************************************************

	/** Realizar cofirma en multifirmas. */
	public static final String VALUE_MULTISIGN_COSIGN = "cosign";  //$NON-NLS-1$

	/** Realizar contrafirma en hojas en multifirmas. */
	public static final String VALUE_MULTISIGN_COUNTERSIGN_LEAFS = "countersignLeafs";  //$NON-NLS-1$

	/** Realizar contrafirma en arbol en multifirmas. */
	public static final String VALUE_MULTISIGN_COUNTERSIGN_TREE = "countersignTree";  //$NON-NLS-1$

	//**************************************************************************************************************************

	//**************************************************************************************************************************
	//**************** PREFERENCIAS GENERALES **********************************************************************************

    /** Idioma por defecto. */
    public static final String PREFERENCES_LOCALE = "default.locale"; //$NON-NLS-1$

	/** Proteger cambios en preferencias.
	 * Un valor de <code>true</code> en esta preferencia indica que deben limitarse las opciones de configuraci&oacute;n
	 * mediante interfaz gr&aacute;fico, apareciendo de forma deshabilitada (solo para consulta).
	 * Un valor de <code>false</code> habilitar&aacute; que cualquier opci&oacute;n de configuraci&oacute;n pueda ser
	 * alterada por parte del usuario mediante el interfaz gr&aacute;fico. */
	public static final String PREFERENCE_GENERAL_BLOCKED = "preferencesBlocked"; //$NON-NLS-1$

	/** Comprobar que la versi&oacute;n actual de Java est&aacute; soportada.
	 * Un valor de <code>true</code> en esta preferencia hace que, al arrancar, la aplicaci&oacute;n compruebe autom&aacute;ticamente
	 * si la versi&oacute;n de Java con la que se ejecuta la aplicaci&oacute;n est&aacute; entre las versiones soportadas. Un valor de
	 * <code>false</code> har&aacute; que no se haga esta comprobaci&oacute;n. */
	public static final String PREFERENCE_GENERAL_CHECK_JAVA_VERSION = "checkJavaVersion"; //$NON-NLS-1$

	/** Evitar la confirmaci&oacute;n al cerrar la aplicaci&oacute;n o no.
	 * Un valor de <code>true</code> en esta preferencia permitir&aacute; cerrar la aplicaci&oacute;n sin ning&uacute;n di&aacute;logo
	 * de advertencia. Un valor de <code>false</code> har&aacute; que se muestre un di&aacute;logo para que el usuario confirme que
	 * realmente desea cerrar la aplicaci&oacute;n. */
	public static final String PREFERENCE_GENERAL_OMIT_ASKONCLOSE = "omitAskOnClose"; //$NON-NLS-1$

	/** Buscar actualizaciones al arrancar.
	 * Un valor de <code>true</code> en esta preferencia hace que, al arrancar, la aplicaci&oacute;n compruebe autom&aacute;ticamente
	 * si hay publicadas versiones m&aacute;s actuales del aplicativo. Un valor de <code>false</code> har&aacute; que no se haga
	 * esta comprobaci&oacute;n. */
	public static final String PREFERENCE_GENERAL_UPDATECHECK = "checkForUpdates"; //$NON-NLS-1$

	/** Mantiene habilitado el funcionamiento de JMultiCard.
	 * Un valor de <code>true</code> en esta preferencia hace que la aplicacion deje el comportamiento
	 * por defecto de JMulticard, que usaria las tarjetas DNIe y CERES. Un valor de <code>false</code>
	 * har&aacute; que no se desactive el uso de JMulticard para estas tarjetas. */
	public static final String PREFERENCE_GENERAL_ENABLED_JMULTICARD = "enabledJmulticard"; //$NON-NLS-1$

	/**
	 * Configura una propiedad que indica a la biblioteca WebSocket para la comunicaci&oacute;n con el
	 * navegador que aplique un peque&ntilde;o retardo en las comunicaciones para as&iacute; evitar que
	 * se bloquee el canal. Esto relantizar&aacute; las comunicaciones, lo cual es muy evidente conforme
	 * se trabaje con ficheros m&aacute;s grandes. S&oacute;lo se recomienda el su uso de esta propiedad
	 * cuando se use el cliente sobre VDI para evitar un mal mayor.
	 */
	public static final String PREFERENCE_GENERAL_VDI_OPTIMIZATION = "vdiOptimization"; //$NON-NLS-1$

	/** Algoritmo de huella para firma.
	 * Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>SHA1</li>
	 *  <li>SHA256</li>
	 *  <li>SHA384</li>
	 *  <li>SHA512</li>
	 * </ul> */
	public static final String PREFERENCE_GENERAL_SIGNATURE_ALGORITHM = "signatureHashAlgorithm"; //$NON-NLS-1$


	/** Solicitar confirmaci&oacute;n antes de firmar.
	 * Un valor de <code>true</code> en esta preferencia hace que se muestre un di&aacute;logo de
	 * confirmaci&oacute;n con las implicaciones de firma al iniciar una firma desde la interfaz
	 * de escritorio. */
	public static final String PREFERENCE_GENERAL_CONFIRMTOSIGN = "confirmToSign"; //$NON-NLS-1$

	/** Permitir la multifirma de firmas inv&aacute;lidas.
	 * Un valor de <code>true</code> en esta preferencia hace que se puedan multifirmar firmas a pesar
	 * de haberse detectado que no son v&aacute;lidas. */
	public static final String PREFERENCE_GENERAL_ALLOW_INVALID_SIGNATURES = "allowInvalidSignatures"; //$NON-NLS-1$

	/** Indica si en los procesos de firma masiva se deben sobreescribir o no los ficheros que
	 * se encuentren en el directorio de salida. */
	public static final String PREFERENCE_GENERAL_MASSIVE_OVERWRITE = "massiveOverride"; //$NON-NLS-1$

	/** Formato de firma por defecto para documentos PDF.
	 * Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>PAdes</li>
	 *  <li>CAdes</li>
	 *  <li>XAdes</li>
	 * </ul> */
	public static final String PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF = "defaultSignatureFormatPdf"; //$NON-NLS-1$

	/** Formato de firma por defecto para documentos OOXML de Microsoft Office.
	 * Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>OOXML (Office Open XML)</li>
	 *  <li>CAdES</li>
	 *  <li>XAdES</li>
	 * </ul> */
	public static final String PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML = "defaultSignatureFormatOoxml"; //$NON-NLS-1$

	/** Formato de firma por defecto para Facturas Electr&oacute;nicas.
	 * Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>FacturaE</li>
	 *  <li>CAdES</li>
	 *  <li>XAdES</li>
	 * </ul> */
	public static final String PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE = "defaultSignatureFormatFacturae"; //$NON-NLS-1$

	/** Formato de firma por defecto para documentos ODF de LibreOffice / OpenOffice.
	 * Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>ODF (Open Document Format)</li>
	 *  <li>CAdES</li>
	 *  <li>XAdES</li>
	 * </ul> */
	public static final String PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF = "defaultSignatureFormatOdf"; //$NON-NLS-1$

	/** Formato de firma por defecto para documentos XML.
	 * Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>CAdES</li>
	 *  <li>XAdES</li>
	 * </ul> */
	public static final String PREFERENCE_GENERAL_DEFAULT_FORMAT_XML = "defaultSignatureFormatXml"; //$NON-NLS-1$

	/** Formato de firma por defecto para ficheros binarios que no se adec&uacute;en a ninguna otra categor&iacute;a.
	 * Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>CAdES</li>
	 *  <li>XAdES</li>
	 * </ul> */
	public static final String PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN = "defaultSignatureFormatBin"; //$NON-NLS-1$

	/** Indica si debe validarse el certificado SSL en las conexiones de red. */
	public static final String PREFERENCE_GENERAL_SECURE_CONNECTIONS = "secureConnections"; //$NON-NLS-1$

	/** Lista de dominios seguros donde realizar conexiones SSL. */
	public static final String PREFERENCE_GENERAL_SECURE_DOMAINS_LIST = "secureDomainsList"; //$NON-NLS-1$

	/** Configuraci&oacute;n de <i>proxy</i> seleccionada.
	 * Un valor de <code>true</code> en esta preferencia indica que debe usarse el <i>proxy</i> configurado,
	 * y un valor de <code>false</code> que no usar&aacute; <i>proxy</i> en las conexiones de red. */
	public static final String PREFERENCE_GENERAL_PROXY_SELECTED = "proxySelected"; //$NON-NLS-1$

	/** Tipo de configuraci&oacute;n de proxy. */
	public static final String PREFERENCE_GENERAL_PROXY_TYPE = "proxyType"; //$NON-NLS-1$

	/** Host del servidor <i>proxy</i> configurado. */
	public static final String PREFERENCE_GENERAL_PROXY_HOST = "proxyHost"; //$NON-NLS-1$

	/** Puerto del servidor <i>proxy</i> configurado. */
	public static final String PREFERENCE_GENERAL_PROXY_PORT = "proxyPort"; //$NON-NLS-1$

	/** Nombre de usuario del servidor <i>proxy</i> configurado. */
	public static final String PREFERENCE_GENERAL_PROXY_USERNAME = "proxyUsername"; //$NON-NLS-1$

	/** Contrase&ntilde;a del servidor <i>proxy</i> configurado. */
	public static final String PREFERENCE_GENERAL_PROXY_PASSWORD = "proxyPassword"; //$NON-NLS-1$

	/** Listado de URLs excluidas del uso de proxy. */
	public static final String PREFERENCE_GENERAL_PROXY_EXCLUDED_URLS = "proxyExcludedUrls"; //$NON-NLS-1$

	//**************** FIN PREFERENCIAS GENERALES ******************************************************************************
	//**************************************************************************************************************************

	//**************************************************************************************************************************
	//************************* PREFERENCIAS DE FIRMAS XAdES *******************************************************************

	/** Identificador de la pol&iacute;tica de firma para XAdES. Debe ser un OID.*/
	public static final String PREFERENCE_XADES_POLICY_IDENTIFIER = "xadesPolicyIdentifier"; //$NON-NLS-1$

	/** Huella digital del identificador de la pol&iacute;tica de firma para XAdES. Debe estar en base64.*/
	public static final String PREFERENCE_XADES_POLICY_HASH = "xadesPolicyIdentifierHash"; //$NON-NLS-1$

	/** Algoritmo de la huella digital del identificador de la pol&iacute;tica de firma para XAdES.
	 * Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>SHA1</li>
	 *  <li>SHA-512</li>
	 *  <li>SHA-384</li>
	 *  <li>SHA-256</li>
	 * </ul> */
	public static final String PREFERENCE_XADES_POLICY_HASH_ALGORITHM = "xadesPolicyIdentifierHashAlgorithm"; //$NON-NLS-1$

	/** Calificador de la pol&iacute;tica de firma para XAdES. Debe ser una URL.*/
	public static final String PREFERENCE_XADES_POLICY_QUALIFIER = "xadesPolicyQualifier"; //$NON-NLS-1$

	/** Ciudad de firma para firmas XAdES. */
	public static final String PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY = "xadesSignatureProductionCity"; //$NON-NLS-1$

	/** Provincia de firma para firmas XAdES. */
	public static final String PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE = "xadesSignatureProductionProvince"; //$NON-NLS-1$

	/** C&oacute;digo de firma para firmas XAdES. */
	public static final String PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE = "xadesSignatureProductionPostalCode"; //$NON-NLS-1$

	/** Pa&iacute;s de firma para firmas XAdES. */
	public static final String PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY = "xadesSignatureProductionCountry"; //$NON-NLS-1$

	/** Cargo supuesto para el firmante en firmas XAdES. */
	public static final String PREFERENCE_XADES_SIGNER_CLAIMED_ROLE = "xadesSignerClaimedRole"; //$NON-NLS-1$

	/** Formato de las firmas XAdES.
	 * Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>XAdES Detached</li>
	 *  <li>XAdES Enveloping</li>
	 *  <li>XAdES Enveloped</li>
	 * </ul> */
	public static final String PREFERENCE_XADES_SIGN_FORMAT = "xadesSignFormat"; //$NON-NLS-1$

	/** Tipo de multifirma a realizar. */
	public static final String PREFERENCE_XADES_MULTISIGN = "xadesMultisign";  //$NON-NLS-1$

	//************************* FIN PREFERENCIAS DE FIRMAS XAdES ***************************************************************
	//**************************************************************************************************************************

	//**************************************************************************************************************************
	//************************* PREFERENCIAS DE FIRMAS PAdES *******************************************************************

	/** Motivo de la firma en firmas PAdES. */
	public static final String PREFERENCE_PADES_SIGN_REASON = "padesSignReason"; //$NON-NLS-1$

	/** Ciudad de firma para firmas PAdES. */
	public static final String PREFERENCE_PADES_SIGN_PRODUCTION_CITY = "padesSignProductionCity"; //$NON-NLS-1$

	/** Contacto del firmante en firmas PAdES. */
	public static final String PREFERENCE_PADES_SIGNER_CONTACT = "padesSignerContact"; //$NON-NLS-1$

	/** Formato de firma PAdES (PAdES B&aacute;sico o PAdES-BES).
	 * Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>PAdES-BES</li>
	 *  <li>PAdES B&aacute;sico</li>
	 * </ul>*/
	public static final String PREFERENCE_PADES_FORMAT = "padesBasicFormat"; //$NON-NLS-1$

	/** Identificador de la pol&iacute;tica de firma para PAdES. Debe ser un OID.*/
	public static final String PREFERENCE_PADES_POLICY_IDENTIFIER = "padesPolicyIdentifier"; //$NON-NLS-1$

	/** Huella digital del identificador de la pol&iacute;tica de firma para PAdES. Debe estar en base64.*/
	public static final String PREFERENCE_PADES_POLICY_HASH = "padesPolicyIdentifierHash"; //$NON-NLS-1$

	/** Algoritmo de la huella digital del identificador de la pol&iacute;tica de firma para PAdES.
	 *  Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>SHA1</li>
	 *  <li>SHA-512</li>
	 *  <li>SHA-384</li>
	 *  <li>SHA-256</li>
	 * </ul> */
	public static final String PREFERENCE_PADES_POLICY_HASH_ALGORITHM = "padesPolicyIdentifierHashAlgorithm"; //$NON-NLS-1$

	/** Calificador de la pol&iacute;tica de firma para PAdES. Debe ser una URL.*/
	public static final String PREFERENCE_PADES_POLICY_QUALIFIER = "padesPolicyQualifier"; //$NON-NLS-1$

	/** Si est&aacute; establecido a <code>true</code> se pide al usuario que determine mediante di&aacute;logos
	 * gr&aacute;ficos los par&aacute;metros de una firma visible PDF y se inserta como tal en el
	 * documento. */
	public static final String PREFERENCE_PADES_VISIBLE = "padesVisibleSignature"; //$NON-NLS-1$

	/** Si est&aacute; establecido a <code>true</code> se ofuscan los identificadores de usuario del CN
	 * y DN del certificado antes de mostrarlos en la firma visible del PDF. */
	public static final String PREFERENCE_PADES_OBFUSCATE_CERT_INFO = "padesObfuscateCertInfo"; //$NON-NLS-1$

	/** Si est&aacute; establecido a <code>true</code> se pide al usuario que determine mediante di&aacute;logos
	 * gr&aacute;ficos los par&aacute;metros de una marca visible en PDF y se inserta como tal en el
	 * documento. */
	public static final String PREFERENCE_PADES_STAMP = "padesVisibleStamp"; //$NON-NLS-1$

	/** Si est&aacute; establecido a <code>true</code> se comprobar&aacute;n posibles PDF Shadow Attacks
	 * si se establece a <code>false</code>, no se realizar&aacute; la comprobaci&oacute;n */
	public static final String PREFERENCE_PADES_CHECK_SHADOW_ATTACK = "allowShadowAttack"; //$NON-NLS-1$

	//************************* FIN PREFERENCIAS DE FIRMAS PAdES ***************************************************************
	//**************************************************************************************************************************

	//**************************************************************************************************************************
	//************************* PREFERENCIAS DE FIRMAS CAdES *******************************************************************

	/** Identificador de la pol&iacute;tica de firma para CAdES. Debe ser un OID.*/
	public static final String PREFERENCE_CADES_POLICY_IDENTIFIER = "cadesPolicyIdentifier"; //$NON-NLS-1$

	/** Huella digital del identificador de la pol&iacute;tica de firma para CAdES. */
	public static final String PREFERENCE_CADES_POLICY_HASH = "cadesPolicyIdentifierHash"; //$NON-NLS-1$

	/** Algoritmo de la huella digital del identificador de la pol&iacute;tica de firma para CAdES.
	 * Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>SHA1</li>
	 *  <li>SHA-512</li>
	 *  <li>SHA-384</li>
	 *  <li>SHA-256</li>
	 * </ul> */
	public static final String PREFERENCE_CADES_POLICY_HASH_ALGORITHM = "cadesPolicyIdentifierHashAlgorithm"; //$NON-NLS-1$

	/** Calificador de la pol&iacute;tica de firma para CAdES. Debe ser una URL.*/
	public static final String PREFERENCE_CADES_POLICY_QUALIFIER = "cadesPolicyQualifier"; //$NON-NLS-1$

	/** Si est&aacute; establecido a <code>true</code> la firma CAdES se realizar&aacute; en modo impl&iacute;cito (<i>attached</i>),
	 *  si est&aacute; establecido a <code>false</code> se realizar&aacute; en modo (<i>detached</i>). */
	public static final String PREFERENCE_CADES_IMPLICIT = "cadesImplicitMode"; //$NON-NLS-1$

	/** Tipo de multifirmas CAdES. */
	public static final String PREFERENCE_CADES_MULTISIGN = "cadesMultisign";  //$NON-NLS-1$


	//************************* FIN PREFERENCIAS DE FIRMAS CAdES ***************************************************************
	//**************************************************************************************************************************

	//**************************************************************************************************************************
	//**************** PREFERENCIAS DE FACTURAS ELECTRONICAS *******************************************************************

	/** Nombre de la pol&iacute;tica de FacturaE.
	 *  Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>Pol&iacute;tica de Factura Electr&oacute;nica 3.0</li>
	 *  <li>Pol&iacute;tica de Factura Electr&oacute;nica 3.1</li>
	 * </ul> */
	public static final String PREFERENCE_FACTURAE_POLICY = "facturaEPolicy"; //$NON-NLS-1$

	/** Ciudad de firma para firmas FacturaE. */
	public static final String PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_CITY = "facturaeSignatureProductionCity"; //$NON-NLS-1$

	/** Provincia de firma para firmas FacturaE. */
	public static final String PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_PROVINCE = "facturaeSignatureProductionProvince"; //$NON-NLS-1$

	/** C&oacute;digo de firma para firmas FacturaE. */
	public static final String PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_POSTAL_CODE = "facturaeSignatureProductionPostalCode"; //$NON-NLS-1$

	/** Pa&iacute;s de firma para firmas FacturaE. */
	public static final String PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_COUNTRY = "facturaeSignatureProductionCountry"; //$NON-NLS-1$

	/** Papel del firmante de las facturas.
	 * Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>Emisor</li>
	 *  <li>Receptor</li>
	 *  <li>Tercero</li>
	 * </ul> */
	public static final String PREFERENCE_FACTURAE_SIGNER_ROLE = "facturaeSignerRole"; //$NON-NLS-1$

	/** Identificador de la pol&iacute;tica de firma para FacturaE. Debe ser un OID.*/
	public static final String PREFERENCE_FACTURAE_POLICY_IDENTIFIER = "facturaePolicyIdentifier"; //$NON-NLS-1$

	/** Huella digital del identificador de la pol&iacute;tica de firma para FacturaE. Debe estar en base64.*/
	public static final String PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH = "facturaePolicyIdentifierHash"; //$NON-NLS-1$

	/** Algoritmo de la huella digital del identificador de la pol&iacute;tica de firma para FacturaE.
	 * Esta preferencia debe tener uno de estos valores:
	 * <ul>
	 *  <li>SHA1</li>
	 *  <li>SHA-512</li>
	 *  <li>SHA-384</li>
	 *  <li>SHA-256</li>
	 * </ul> */
	public static final String PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH_ALGORITHM = "facturaePolicyIdentifierHashAlgorithm"; //$NON-NLS-1$

	/** Calificador de la pol&iacute;tica de firma para FacturaE. Debe ser una URL. */
	public static final String PREFERENCE_FACTURAE_POLICY_QUALIFIER = "facturaePolicyQualifier"; //$NON-NLS-1$

	//**************** FIN PREFERENCIAS DE FACTURAS ELECTRONICAS ***************************************************************
	//**************************************************************************************************************************

	//**************************************************************************************************************************
	//**************** PREFERENCIAS DE FIRMA VISIBLE EN PDF ********************************************************************

	/** Texto contenido en la firma visible en pdf.*/
	public static final String PREFERENCE_PDF_SIGN_LAYER2TEXT = "pdfLayer2Text"; //$NON-NLS-1$

	/** Fuente utilizada en el texto de la firma visible en pdf.*/
	public static final String PREFERENCE_PDF_SIGN_LAYER2FONTFAMILY = "pdfLayer2FontFamily"; //$NON-NLS-1$

	/** Tamaño de la fuente utilizada en el texto de la firma visible en pdf.*/
	public static final String PREFERENCE_PDF_SIGN_LAYER2FONTSIZE = "pdfLayer2FontSize"; //$NON-NLS-1$

	/** Estilo de la fuente utilizada en el texto de la firma visible en pdf.*/
	public static final String PREFERENCE_PDF_SIGN_LAYER2FONTSTYLE = "pdfLayer2FontStyle"; //$NON-NLS-1$

	/** Color de la fuente utilizada en el texto de la firma visible en pdf.*/
	public static final String PREFERENCE_PDF_SIGN_LAYER2FONTCOLOR = "pdfLayer2FontColor"; //$NON-NLS-1$

	/** Indica el &aacute;ngulo que se debe rotar la firma.*/
	public static final String PREFERENCE_PDF_SIGN_SIGNATUREROTATION = "pdfSignatureRotation"; //$NON-NLS-1$

	/** Imagen de fondo de la firma visible en pdf.*/
	public static final String PREFERENCE_PDF_SIGN_IMAGE = "pdfSignatureImage"; //$NON-NLS-1$

	//**************** FIN PREFERENCIAS DE FIRMA VISIBLE EN PDF ****************************************************************
	//**************************************************************************************************************************

	//**************************************************************************************************************************
	//**************** PREFERENCIAS DE certificados ********************************************************************

	/** Texto contenido en la firma visible en pdf.*/
	public static final String PREFERENCE_DEFAULT_KEYSTORE = "defaultKeystore"; //$NON-NLS-1$

	//**************** FIN PREFERENCIAS DE FIRMA VISIBLE EN PDF ****************************************************************
	//**************************************************************************************************************************

	//**************************************************************************************************************************
	//**************** PREFERENCIAS DE ALMACENES DE CLAVES *********************************************************************

	/** Indica si se usa o no el certificado por defecto configurado en llamadas desde el navegador. */
	public static final String PREFERENCE_USE_DEFAULT_STORE_IN_BROWSER_CALLS = "useDefaultStoreInBrowserCalls"; //$NON-NLS-1$

	/** No mostrar la pantalla inicial de uso de DNIe.
	 * Un valor de <code>true</code> en esta preferencia hace que nunca se muestre la pantalla inicial que sugiere al usuario
	 * el uso directo del DNIe como almac&eacute;n de claves. Un valor de <code>false</code> har&aacute; que se muestre esta pantalla
	 * al inicio siempre que se detecte un lector de tarjetas en el sistema. */
	public static final String PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN = "hideDnieStartScreen"; //$NON-NLS-1$

	/** En firma, restringir que &uacute;nicamente se puedan usar certificados de firma.
	 * Un valor de <code>true</code> en esta preferencia permitir&aacute; usar solo certificados espec&iacute;ficos
	 * para firma en las firmas electr&oacute;nicas. */
	public static final String PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS = "useOnlySignatureCertificates"; //$NON-NLS-1$

	/** Mostrar certificados caducados.
	 * Un valor de <code>true</code> en esta preferencia hace que el di&aacute;logo de selecci&oacute;n
	 * de certificados muestre los certificados caducados. */
	public static final String PREFERENCE_KEYSTORE_SHOWEXPIREDCERTS = "showExpiredCerts"; //$NON-NLS-1$

	/** En firma, restringir que &uacute;nicamente se puedan usar certificados de seud&oacute;nimo cuando estos est&eacute;n
	 * disponibles. Un valor de <code>true</code> en esta preferencia permitir&aacute; usar solo  certificados de
	 * seud&oacute;nimo cuando estos est&eacute;n disponibles.*/
	public static final String PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS = "useOnlyAliasCertificates"; //$NON-NLS-1$

	/** Almac&eacute;n de claves por defecto. */
	public static final String PREFERENCE_KEYSTORE_DEFAULT_STORE = "defaultStore"; //$NON-NLS-1$

	/** Ruta del almac&eacute;n de claves local seleccionado por defecto. */
	public static final String PREFERENCE_LOCAL_KEYSTORE_PATH = "defaultLocalKeystorePath"; //$NON-NLS-1$

	/** Valor default para indicar que se desea seleccionar el almac&eacute;n de claves del sistema. */
	public static final String VALUE_DEFAULT = "default"; //$NON-NLS-1$

	//**************** FIN PREFERENCIAS DE ALMACENES DE CLAVES *****************************************************************
	//**************************************************************************************************************************


	/** Recupera la cadena con el valor de una propiedad de configuraci&oacute;n. La propiedad se
	 * buscar&aacute;, por orden, en las preferencia del usuario, del sistema o en la configuraci&oacute;n
	 * por defecto.
	 * @param key Clave del valor que queremos recuperar.
	 * @return El valor almacenado de la propiedad o su valor por defecto si no se encontr&oacute;. */
	public static String get(final String key) {
		return USER_PREFERENCES.get(
				key,
				SYSTEM_PREFERENCES.get(
						key,
						DEFAULT_PREFERENCES.getProperty(key)));
	}

	/** Recupera la cadena con el valor de una propiedad de configuraci&oacute;n. La propiedad se
	 * buscar&aacute;, por orden, en las preferencia del usuario, del sistema o en la configuraci&oacute;n
	 * por defecto.
	 * @param key Clave del valor que queremos recuperar.
	 * @return El valor almacenado de la propiedad o su valor por defecto si no se encontr&oacute;. */
	public static String get(final String key, final PreferencesSource src) {

		if (src == null) {
			return get(key);
		}

		switch(src) {
		case USER:
			return USER_PREFERENCES.get(key, null);
		case SYSTEM:
			return SYSTEM_PREFERENCES.get(key, null);
		default:
		case DEFAULT:
			return DEFAULT_PREFERENCES.getProperty(key);
		}
	}

	/** Recupera el valor {@code true} o {@code false} almacenado entre las preferencias de la
	 * aplicaci&oacute;n.
	 * @param key Clave del valor que queremos recuperar.
	 * @return La preferencia almacenada o la configurada en el sistema si no se encontr&oacute;. */
	public static boolean getBoolean(final String key) {
		return USER_PREFERENCES.getBoolean(
				key,
				SYSTEM_PREFERENCES.getBoolean(
						key,
						Boolean.parseBoolean(DEFAULT_PREFERENCES.getProperty(key))));
	}

	/** Recupera el valor {@code true} o {@code false} almacenado en el tipo de preferencia
	 * indicado de la aplicaci&oacute;n.
	 * Si se indica una fuente nula, se obtendr&aacute; por orden el valor indicado en las preferencias
	 * de usuario, del sistema o el por defecto.
	 * @param key Clave del valor que queremos recuperar.
	 * @return El valor almacenado de la propiedad o {@code false} si no estaba declarado. */
	public static boolean getBoolean(final String key, final PreferencesSource src) {

		if (src == null) {
			return getBoolean(key);
		}

		switch(src) {
		case USER:
			return USER_PREFERENCES.getBoolean(key, false);
		case SYSTEM:
			return SYSTEM_PREFERENCES.getBoolean(key, false);
		default:
		case DEFAULT:
			return Boolean.parseBoolean(DEFAULT_PREFERENCES.getProperty(key));
		}
	}

	/** Establece una cadena de texto en la configuraci&oacute;n de la aplicaci&oacute;n
	 * identific&aacute;ndola con una clave. Si el valor que se le va a asignar a la propiedad es
	 * el que ya indico un administrador (propiedad del sistema) o el valor por defecto, si no
	 * existiese aquel, no se guarda para no ofuscar esos valores. Para realizar el guardado
	 * completo, es necesario ejecutar el m&eacute;todo {@code flush()}.
	 * @param key Clave con la que identificaremos el valor.
	 * @param value Valor que se desea almacenar. */
	public static void put(final String key, final String value) {
		final String systemValue = SYSTEM_PREFERENCES.get(
				key,
				DEFAULT_PREFERENCES.getProperty(key));

		// Si el nuevo valor que se establece en el sistema o por defecto, se elimina la clave
		// para dar visibilidad al valor establecido en el sistema o por defecto
		if (value.equals(systemValue)) {
			USER_PREFERENCES.remove(key);
		} else if (!value.equals(get(key))) {
			USER_PREFERENCES.put(key, value);
		}
	}

	/** Establece un {@code true} o {@code false} en la configuraci&oacute;n de la aplicaci&oacute;n
	 * identific&aacute;ndolo con una clave. Si el valor que se le va a asignar a la propiedad es
	 * el que ya indico un administrador (propiedad del sistema) o el valor por defecto, si no
	 * existiese aquel, no se guarda para no ofuscar esos valores. Para realizar el guardado completo, es
	 * necesario ejecutar el m&eacute;todo {@code flush()}.
	 * @param key Clave con la que identificaremos el valor.
	 * @param value Valor que se desea almacenar. */
	public static void putBoolean(final String key, final boolean value) {
		final boolean systemValue = SYSTEM_PREFERENCES.getBoolean(
				key,
				Boolean.parseBoolean(DEFAULT_PREFERENCES.getProperty(key)));

		// Si el nuevo valor que se establece en el sistema o por defecto, se elimina la clave
		// para dar visibilidad al valor establecido en el sistema o por defecto
		if (value == systemValue) {
			USER_PREFERENCES.remove(key);
		} else if (value != getBoolean(key)) {
			// Si la propiedad ha cambiado con respecto a la configurada en el sistema o por defecto, se guardara
			USER_PREFERENCES.putBoolean(key, value);
		}
	}

	/** Establece una cadena con clave y valor en las preferencias del sistema. Para realizar el guardado completo, es
	 * necesario ejecutar el m&eacute;todo {@code flush()}.
	 * @param key Clave con la que identificaremos el valor.
	 * @param value Valor que se desea almacenar. */
	public static void putSystemPref(final String key, final String value) {
		SYSTEM_PREFERENCES.put(key, value);
	}

	/** Establece un {@code true} o {@code false} en las preferencias del sistema. Para realizar el guardado completo, es
	 * necesario ejecutar el m&eacute;todo {@code flush()}.
	 * @param key Clave con la que identificaremos el valor.
	 * @param value Valor que se desea almacenar. */
	public static void putBooleanSystemPref(final String key, final boolean value) {
		SYSTEM_PREFERENCES.putBoolean(key, value);
	}

	/** Elimina una clave de entre la configuraci&oacute;n de la aplicaci&oacute;n.
	 * @param key Clave que eliminar. */
	public static void remove(final String key) {
		USER_PREFERENCES.remove(key);
	}

	/**
	 * Elimina todas las preferencias del usuario de la aplicaci&oacute;n.
	 * @throws BackingStoreException Si ocurre un error eliminando las preferencias.
	 */
	public static void clearAll() throws BackingStoreException {
		USER_PREFERENCES.clear();
	}

	/**
	 * Elimina todas las preferencias del sistema de la aplicaci&oacute;n.
	 * @throws BackingStoreException Si ocurre un error eliminando las preferencias.
	 */
	public static void clearAllSystemPrefs() throws BackingStoreException {
		SYSTEM_PREFERENCES.clear();
	}

	/** Almacena en las preferencias del usuario de la aplicaci&oacute;n todos los valores
	 * establecidos hasta el momento.
	 * @throws BackingStoreException Cuando ocurre un error durante el guardado. */
	public static void flush() throws BackingStoreException {
		USER_PREFERENCES.flush();
	}

	/** Almacena en las preferencias del sistema de la aplicaci&oacute;n todos los valores
	 * establecidos hasta el momento.
	 * @throws BackingStoreException Cuando ocurre un error durante el guardado. */
	public static void flushSystemPrefs() throws BackingStoreException {
		SYSTEM_PREFERENCES.flush();
	}

	/**
	 * Comprueba si la preferencia se puede bloquear
	 * @param key Clave de la preferencia
	 * @return {@code true} Si es una preferencia bloqueable, {@code false} en caso contrario
	 */
	public static boolean isProtectedPreference(final String key) {
		return key.equals(PREFERENCE_GENERAL_UPDATECHECK)
				|| key.equals(PREFERENCE_GENERAL_ENABLED_JMULTICARD)
				|| key.equals(PREFERENCE_GENERAL_SIGNATURE_ALGORITHM)
				|| key.equals(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF)
				|| key.equals(PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML)
				|| key.equals(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE)
				|| key.equals(PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF)
				|| key.equals(PREFERENCE_GENERAL_DEFAULT_FORMAT_XML)
				|| key.equals(PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN)
				|| key.equals(PREFERENCE_GENERAL_ALLOW_INVALID_SIGNATURES)
				|| key.equals(PREFERENCE_GENERAL_SECURE_CONNECTIONS)
				|| key.equals(PREFERENCE_XADES_POLICY_IDENTIFIER)
				|| key.equals(PREFERENCE_XADES_POLICY_HASH)
				|| key.equals(PREFERENCE_XADES_POLICY_HASH_ALGORITHM)
				|| key.equals(PREFERENCE_XADES_POLICY_QUALIFIER)
				|| key.equals(PREFERENCE_XADES_SIGN_FORMAT)
				|| key.equals(PREFERENCE_PADES_FORMAT)
				|| key.equals(PREFERENCE_PADES_POLICY_IDENTIFIER)
				|| key.equals(PREFERENCE_PADES_POLICY_HASH)
				|| key.equals(PREFERENCE_PADES_POLICY_HASH_ALGORITHM)
				|| key.equals(PREFERENCE_PADES_POLICY_QUALIFIER)
				|| key.equals(PREFERENCE_PADES_CHECK_SHADOW_ATTACK)
				|| key.equals(PREFERENCE_CADES_POLICY_IDENTIFIER)
				|| key.equals(PREFERENCE_CADES_POLICY_HASH)
				|| key.equals(PREFERENCE_CADES_POLICY_HASH_ALGORITHM)
				|| key.equals(PREFERENCE_CADES_POLICY_QUALIFIER)
				|| key.equals(PREFERENCE_CADES_IMPLICIT)
				|| key.equals(PREFERENCE_FACTURAE_POLICY)
				|| key.equals(PREFERENCE_KEYSTORE_SHOWEXPIREDCERTS);
	}

	/**
	 * Se obtienen las preferencias a exportar que se hayan registrado en el sistema y en el usuario.
	 * Si la preferencia existe en usuario y sistema, tendr&aacute; prioridad la del usuario.
	 * @return Mapa con las claves y valores del sistema.
	 */
	public static Map<String, Object> getPrefsToExport() {

		final Map<String, Object> result = new HashMap<>();
		try {
			final String[] systemKeys = SYSTEM_PREFERENCES.keys();
			for (int i = 0 ; i < systemKeys.length ; i++) {
				final String value = SYSTEM_PREFERENCES.get(systemKeys[i], null);
				if (value != null && (value.equals(TRUE_VALUE) || value.equals(FALSE_VALUE))) {
					result.put(systemKeys[i], Boolean.valueOf(value));
				} else {
					result.put(systemKeys[i], value);
				}
			}
			final String[] userKeys = USER_PREFERENCES.keys();
			for (int i = 0 ; i < userKeys.length ; i++) {
				final String value = USER_PREFERENCES.get(userKeys[i], null);
				if (value != null && (value.equals(TRUE_VALUE) || value.equals(FALSE_VALUE))) {
					result.put(userKeys[i], Boolean.valueOf(value));
				} else {
					result.put(userKeys[i], value);
				}
			}
		} catch (final BackingStoreException e) {
			LOGGER.severe(
					"Error al obtener preferencias configuradas por el usuario" //$NON-NLS-1$
						+ e
				);
		}
		return result;
	}

}
