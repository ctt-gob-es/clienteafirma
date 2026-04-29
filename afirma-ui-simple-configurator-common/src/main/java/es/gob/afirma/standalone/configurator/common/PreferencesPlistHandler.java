/* Copyr0ight (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator.common;

import java.awt.Component;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidSignatureFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.SecureXmlBuilder;
import es.gob.afirma.core.prefs.KeyStorePreferencesManager;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.configurator.common.PreferencesManager.PreferencesSource;
import es.gob.afirma.standalone.configurator.common.xmlwise.Plist;
import es.gob.afirma.standalone.configurator.common.xmlwise.XmlElement;
import es.gob.afirma.standalone.configurator.common.xmlwise.XmlParseException;

/** Carga las preferencias de la aplicaci&oacute;n desde un fichero PList. */
public final class PreferencesPlistHandler {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String XML_HEAD = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"; //$NON-NLS-1$
	private static final String PLIST_OPEN_TAG = "<plist version=\"1.0\">"; //$NON-NLS-1$
	private static final String PLIST_CLOSE_TAG = "</plist>"; //$NON-NLS-1$
	private static final String SMARTCARDS_KEY = "smartcards"; //$NON-NLS-1$

	/**
	 * Algoritmo de hash usado para calcular la huella del certificado de firma del
	 * fichero de preferencias.
	 */
	private static final String PREF_CERT_HASH_ALGORITHM = "SHA-256"; //$NON-NLS-1$

	/**
	 * Importa las preferencias de la aplicaci&oacute;n desde un fichero PList.
	 * @param configFile Fichero de importaci&oacute;n con las preferencias de la aplicaci&oacute;n.
	 * @param parent Componente padre para la modalidad.
	 * @param unprotected
	 *        {@code true} Si las preferencias no est&aacute;n protegidas,
	 *        {@code false} en caso contrario
	 * @throws AOException Cuando se produce un error durante la importaci&oacute;n.
	 */
	public static void importPreferences(final String configFile, final Component parent, final boolean unprotected) throws AOException {

		if (configFile == null){
			throw new IllegalStateException("El fichero de configuracion no puede ser nulo"); //$NON-NLS-1$
		}

		final Document configDocument;
		try (
			final InputStream fis = new FileInputStream(configFile);
			final InputStream bis = new BufferedInputStream(fis);
		) {
			configDocument = SecureXmlBuilder.getSecureDocumentBuilder().parse(bis);
		}
		catch(final IOException e) {
			throw new AOException("No ha sido posible leer el fichero de configuracion de las preferencias", e, ConfiguratorErrorCode.Internal.CANT_READ_PREFERENCES_FILE); //$NON-NLS-1$
		}
		catch (final SAXException e) {
			throw new AOException("El fichero de configuracion no es XML", e, ConfiguratorErrorCode.Internal.INVALID_XML_PREFERENCES_FILE); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new AOException("Error interno al cargar el fichero de configuracion", e, ConfiguratorErrorCode.Internal.IMPORTING_PREFERENCES_ERROR); //$NON-NLS-1$
		}

		importPreferences(configDocument, parent, unprotected);
	}

	/**
	 * Importa las preferencias de la aplicaci&oacute;n desde un fichero PList.
	 * @param config Documento de importaci&oacute;n con las preferencias de la aplicaci&oacute;n.
	 * @param parent Componente padre para la modalidad.
	 * @param unprotected
	 *        {@code true} Si las preferencias no est&aacute;n protegidas,
	 *        {@code false} en caso contrario
	 * @throws AOException Cuando se produce un error durante la importaci&oacute;n.
	 */
	public static void importPreferences(final byte[] config, final Component parent, final boolean unprotected) throws AOException {

		if (config == null){
			throw new IllegalStateException("El objeto de configuracion no puede ser nodo"); //$NON-NLS-1$
		}

		final Document configDocument;
		try (
			final InputStream fis = new ByteArrayInputStream(config);
			final InputStream bis = new BufferedInputStream(fis);
		) {
			configDocument = SecureXmlBuilder.getSecureDocumentBuilder().parse(bis);
		}
		catch(final IOException e) {
			throw new AOException("No ha sido posible leer la configuacion de las preferencias", e, ConfiguratorErrorCode.Internal.CANT_READ_PREFERENCES_FILE); //$NON-NLS-1$
		}
		catch (final SAXException e) {
			throw new AOException("La configuracion no se tiene formato XML", e, ConfiguratorErrorCode.Internal.INVALID_XML_PREFERENCES_FILE); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new AOException("Error interno al cargar la configuracion", e, ConfiguratorErrorCode.Internal.IMPORTING_PREFERENCES_ERROR); //$NON-NLS-1$
		}

		importPreferences(configDocument, parent, unprotected);
	}

	/**
	 * Importa las preferencias de la aplicaci&oacute;n desde un fichero PList.
	 * @param configDocument Documento XML con las preferencias de la aplicaci&oacute;n.
	 * @param parent Componente padre para la modalidad.
	 * @param unprotected
	 *        {@code true} Si las preferencias no est&aacute;n protegidas,
	 *        {@code false} en caso contrario
	 * @throws AOException Cuando se produce un error durante la importaci&oacute;n.
	 */
	public static void importPreferences(final Document configDocument, final Component parent, final boolean unprotected) throws AOException {

		if (configDocument == null){
			throw new IllegalStateException("La configuracion no puede ser nula"); //$NON-NLS-1$
		}

		String signMessage;
		SignatureInfo signatureInfo = null;
		try {
			signatureInfo = XAdESValidator.validate(configDocument, true);

		}
		catch (final AOInvalidSignatureFormatException e) {
			LOGGER.info("El documento no esta firmado"); //$NON-NLS-1$
		}
		catch (final CertificateException e) {
			throw new AOException("El certificado del firmante del fichero de preferencias no es valido", e, ConfiguratorErrorCode.Functional.INVALID_CERTIFICATE_SIGNER); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new AOException("El fichero de configuracion contiene una firma invalida", e, ConfiguratorErrorCode.Functional.INVALID_PREFERENCES_FILE_SIGNATURE); //$NON-NLS-1$
		}

		// Comprobamos si es obligatoria la firma
		final boolean requireSignedPreference = PreferencesManager.getBoolean(
				PreferencesManager.ADMIN_PREFERENCE_REQUIRE_SIGNED_PREFERENCES,
				PreferencesSource.SYSTEM_INTERNAL);

		if (requireSignedPreference && signatureInfo != null) {
			throw new AOException("Solo se admite la importacion de preferencias firmadas", ConfiguratorErrorCode.Functional.UNSIGNED_PREFERENCES_FILE); //$NON-NLS-1$
		}

		// Si las preferencias estan firmadas, obtenemos la informacion del firmante
		if (signatureInfo != null) {
			final X509Certificate signingCert = signatureInfo.getSigningCertificateChain()[0];
			signMessage = ConfiguratorCommonMessages.getString("PreferencesPlistHandler.1", AOUtil.getCN(signingCert)); //$NON-NLS-1$
			final String allowedCertHashB64 = PreferencesManager.get(
					PreferencesManager.ADMIN_PREFERENCE_CERT_HASH_TO_SIGNED_PREFERENCES,
					PreferencesSource.SYSTEM_INTERNAL);
			if (allowedCertHashB64 != null) {
				String signingCertHashB64;
				try {
					signingCertHashB64 = Base64.encode(MessageDigest.getInstance(PREF_CERT_HASH_ALGORITHM).digest(signingCert.getEncoded()));
				}
				catch (final Exception e) {
					throw new AOException("El certificado del firmante del fichero de preferencias no se ha podido codificar",  //$NON-NLS-1$
							e, ConfiguratorErrorCode.Functional.INVALID_CERTIFICATE_SIGNER);
				}

				if (!allowedCertHashB64.equals(signingCertHashB64)) {
					throw new AOException("El certificado del firmante del fichero de preferencias no es el certificado autorizado", ConfiguratorErrorCode.Functional.INVALID_PREFERENCES_FILE_SIGNER); //$NON-NLS-1$
				}
			}
		}
		else {
			signMessage = ConfiguratorCommonMessages.getString("PreferencesPlistHandler.0"); //$NON-NLS-1$
		}


		final int resp = AOUIFactory.showConfirmDialog(
			parent,
			ConfiguratorCommonMessages.getString("PreferencesPlistHandler.2", signMessage), //$NON-NLS-1$
			ConfiguratorCommonMessages.getString("PreferencesPlistHandler.3"), //$NON-NLS-1$
			AOUIFactory.YES_NO_OPTION,
			AOUIFactory.WARNING_MESSAGE);

		if (AOUIFactory.YES_OPTION != resp) {
			return;
		}

		try {
			importUserPreferencesFromXml(configDocument, unprotected);
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "El fichero de configuracion es invalido", e); //$NON-NLS-1$
			throw new AOException("El fichero de configuracion es invalido", ConfiguratorErrorCode.Internal.INVALID_FORMAT_PREFERENCES_FILE); //$NON-NLS-1$
		}

	}

	/**
	 * Se importan las preferencias del usuario desde un XML
	 * @param xml XML con la informaci&oacute;n
	 * @param unprotected Indica si las preferencias est&aacute;n protegidas o no
	 * @throws InvalidPreferencesFileException error en caso de archivo no v&aacute;lido
	 */
	private static void importUserPreferencesFromXml(final Document xml, final boolean unprotected) throws InvalidPreferencesFileException {
		final Map<String, Object> properties = loadPreferencesFromXml(xml, unprotected);

		// Registramos las tarjetas inteligentes que se encuentren en el XML
		if (properties.containsKey(SMARTCARDS_KEY)) {
			KeyStorePreferencesManager.putUserSmartCardsMap((Map<String, Object>) properties.get(SMARTCARDS_KEY));
			properties.remove(SMARTCARDS_KEY);
		}

		checkPreferences(properties);
		storeUserPreferences(properties, unprotected);
	}

	/**
	 * Se cargan las preferencias del usuario desde un XML.
	 * @param xml XML con la informaci&oacute;n.
	 * @param unprotected Indica si las preferencias est&aacute;n protegidas o no.
	 * @return Mapa de preferencias cargadas.
	 * @throws InvalidPreferencesFileException error en caso de archivo no v&aacute;lido.
	 */
	public static Map<String, Object> loadPreferencesFromXml(final Document xml, final boolean unprotected) throws InvalidPreferencesFileException {
		final Map<String, Object> properties;
		try {
			properties = Plist.fromXml(xml);
		}
		catch (final XmlParseException e) {
			throw new InvalidPreferencesFileException("Error analizando el fichero XML: " + e, e); //$NON-NLS-1$
		}
		return properties;
	}

	/**
	 * Se importan las preferencias del sistema desde un XML
	 * @param xml XML con la informaci&oacute;n
	 * @throws InvalidPreferencesFileException error en caso de archivo no v&aacute;lido
	 */
	public static void importSystemPreferencesFromXml(final byte[] xml) throws InvalidPreferencesFileException {
		final Map<String, Object> properties = loadPreferencesFromXml(xml);

		// Registramos las tarjetas inteligentes que se encuentren en el XML
		if (properties.containsKey(SMARTCARDS_KEY)) {
			KeyStorePreferencesManager.putSystemSmartCardsMap((Map<String, Object>) properties.get(SMARTCARDS_KEY));
			properties.remove(SMARTCARDS_KEY);
		}

		checkPreferences(properties);
		storeSystemPreferences(properties);
	}

	/**
	 * Se cargan las preferencias del usuario desde un XML.
	 * @param xml XML con la informaci&oacute;n.
	 * @return Mapa de preferencias cargadas.
	 * @throws InvalidPreferencesFileException error en caso de archivo no v&aacute;lido.
	 */
	public static Map<String, Object> loadPreferencesFromXml(final byte[] xml) throws InvalidPreferencesFileException {
		final Map<String, Object> properties;
		try {
			properties = Plist.fromXml(xml);
		}
		catch (final XmlParseException e) {
			throw new InvalidPreferencesFileException("Error analizando el fichero XML: " + e, e); //$NON-NLS-1$
		}
		return properties;
	}

	/**
	 * Exporta las preferencias del sistema a una cadena XML.
	 * @return Cadena con las preferencias en formato XML
	 */
	public static String exportPreferencesToXml() {
		final Map<String, Object> userProperties = PreferencesManager.getPrefsToExport();

		final Map<String, String> smartCardsPreferences = KeyStorePreferencesManager.getAllSmartCardsMap();
		if (smartCardsPreferences.size() > 0) {
			userProperties.put(SMARTCARDS_KEY, smartCardsPreferences);
		}
		final XmlElement xml = Plist.objectToXml(userProperties);
		return XML_HEAD + PLIST_OPEN_TAG + xml.toXml() + PLIST_CLOSE_TAG;
	}

	/** Comprueba que las preferencias pasadas sean <code>String</code> o <code>Boolean</code>.
	 * @param prefs Preferencias a comprobar.
	 * @throws InvalidPreferencesFileException Si hay alguna preferencia de un tipo no soportado. */
	private static void checkPreferences(final Map<String, Object> prefs) throws InvalidPreferencesFileException {
		final Set<String> keys = prefs.keySet();
		for(final String key : keys) {
			final Object o = prefs.get(key);
			if (o == null) {
				throw new InvalidPreferencesFileException(
					"El valor de la clave de preferencia " + key + " es nulo" //$NON-NLS-1$ //$NON-NLS-2$
				);
			}
			if (!(o instanceof String) && !(o instanceof Boolean)) {
				throw new InvalidPreferencesFileException(
					"No se soporta el tipo " + o.getClass().getName() + " en un fichero de preferencias"//$NON-NLS-1$ //$NON-NLS-2$
				);
			}
		}
	}

	/**
	 * Almacena las preferencias del usuario en el registro del sistema de Windows.
	 * @param prefs
	 *            Preferencias que se guardar&aacute; en el registro de Windows.
	 * @param unprotected
	 *            {@code true} Si las preferencias no est&aacute;n protegidas,
	 *            {@code false} en caso contrario
	 */
	private static void storeUserPreferences(final Map<String, Object> prefs, final boolean unprotected){
		final Set<String> keys = prefs.keySet();
		for(final String key : keys) {
			final Object o = prefs.get(key);
			// Si unprotected es false, se pueden modificar todas las preferencias
			if (!PreferencesManager.isProtectedPreference(key) || !unprotected) {
				if (o instanceof Boolean) {
					PreferencesManager.putBoolean(key, ((Boolean) o).booleanValue());
				}
				else {
					PreferencesManager.put(key, o.toString());
				}
			}
		}
	}

	/**
	 * Almacena las preferencias del sistema en el registro del sistema de Windows.
	 * @param prefs Preferencias que se guardar&aacute; en el registro de Windows.
	 */
	private static void storeSystemPreferences(final Map<String, Object> prefs){
		final Set<String> keys = prefs.keySet();
		for (final String key : keys) {
			final Object o = prefs.get(key);
			if (o instanceof Boolean) {
				PreferencesManager.putBooleanSystemPref(key, ((Boolean) o).booleanValue());
			}
			else {
				PreferencesManager.putSystemPref(key, o.toString());
			}
		}

		try {
			PreferencesManager.flushSystemPrefs();
		} catch (final BackingStoreException e) {
			LOGGER.warning("No se pudo guardar la preferencia del sistema: " + e); //$NON-NLS-1$
		}
	}

	/** Error de an&aacute;lisis del fichero de preferencias. */
	public static class InvalidPreferencesFileException extends Exception {

		private static final long serialVersionUID = 1L;

		InvalidPreferencesFileException(final String desc, final Throwable cause) {
			super(desc, cause);
		}

		InvalidPreferencesFileException(final String desc) {
			super(desc);
		}
	}

}
