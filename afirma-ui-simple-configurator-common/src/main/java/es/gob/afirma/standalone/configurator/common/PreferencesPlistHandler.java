/* Copyright (C) 2011 [Gobierno de Espana]
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
import java.io.FileInputStream;
import java.io.InputStream;
import java.security.cert.X509Certificate;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;

import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.prefs.KeyStorePreferencesManager;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.standalone.configurator.common.xmlwise.Plist;
import es.gob.afirma.standalone.configurator.common.xmlwise.XmlElement;
import es.gob.afirma.standalone.configurator.common.xmlwise.XmlParseException;

/** Carga las preferencias de la aplicaci&oacute;n desde un fichero PList. */
public final class PreferencesPlistHandler {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

//	private static final String PREFERENCES_SIGNATURE_PUK_BASE64;
//	private static final boolean ALLOW_UNSIGNED_PREFERENCES;
	private static final String XML_HEAD = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"; //$NON-NLS-1$
	private static final String PLIST_OPEN_TAG = "<plist version=\"1.0\">"; //$NON-NLS-1$
	private static final String PLIST_CLOSE_TAG = "</plist>"; //$NON-NLS-1$
	private static final String SMARTCARDS_KEY = "smartcards"; //$NON-NLS-1$

//	static {
//		final Properties p = new Properties();
//		try {
//			p.load(
//				PreferencesPlistHandler.class.getResourceAsStream(
//					"/properties/configuration.properties" //$NON-NLS-1$
//				)
//			);
//		}
//		catch(final Exception e) {
//			LOGGER.severe(
//				"No han podido cargarse las preferencias de configuracion, se usaran los valores por defecto: " + e //$NON-NLS-1$
//			);
//		}
//		ALLOW_UNSIGNED_PREFERENCES = Boolean.parseBoolean(
//			p.getProperty("allowUnsignedPreferencesFiles", "true") //$NON-NLS-1$ //$NON-NLS-2$
//		);
//		PREFERENCES_SIGNATURE_PUK_BASE64 = 	p.getProperty("preferencesSignaturePublicKey"); //$NON-NLS-1$
//		if (!ALLOW_UNSIGNED_PREFERENCES && PREFERENCES_SIGNATURE_PUK_BASE64 == null) {
//			throw new IllegalStateException(
//				"Si no se permiten preferencias sin firmar es necesario configurar una clave publica de firmante" //$NON-NLS-1$
//			);
//		}
//	}

//	/**
//	 * Importa las preferencias de la aplicaci&oacute;n desde un fichero PList descargado desde una URL.
//	 * @param url URL para la descarga del fichero de importaci&oacute;n con las preferencias de la aplicaci&oacute;n.
//	 * @param unprotected
//	 *        {@code true} Si las preferencias no est&aacute;n protegidas,
//	 *        {@code false} en caso contrario
//	 * @throws IOException Si hay problemas con la descarga del fichero de preferencias.
//	 * @throws InvalidPreferencesFileException Si las preferencias no son v&aacute;lidas por cualquier motivo.
//	 * @throws AOInvalidFormatException Si no se puede obtener informac&oacute;n sobre la firma de las preferencias.
//	 */
//	public static void importPreferencesFromUrl(final String url, final boolean unprotected) throws IOException, InvalidPreferencesFileException, AOInvalidFormatException {
//		if (url == null){
//			throw new IllegalStateException("La URL de descarga del fichero de configuracion no puede ser nula"); //$NON-NLS-1$
//		}
//		final byte[] configData = DataDownloader.downloadData(url);
//		try {
//			XAdESValidator.validate(configData, true);
//		}
//		catch (final Exception e) {
//			throw new InvalidPreferencesFileException(
//					"La firma del fichero de preferencias no es valida" //$NON-NLS-1$
//					);
//		}
////		final AOTreeModel tm = new AOXAdESSigner().getSignersStructure(configData, true);
////		final AOSimpleSignInfo ssi = (AOSimpleSignInfo) ((AOTreeNode)AOTreeModel.getChild(tm.getRoot(), 0)).getUserObject();
////		final X509Certificate signerCert = ssi.getCerts()[0];
////		if (!Base64.encode(signerCert.getPublicKey().getEncoded()).equals(PREFERENCES_SIGNATURE_PUK_BASE64)) {
////			throw new InvalidPreferencesFileException(
////					"La firmante del fichero de preferencias no esta autorizado" //$NON-NLS-1$
////					);
////		}
//		importUserPreferencesFromXml(new String(configData), unprotected);
//	}

	/**
	 * Importa las preferencias de la aplicaci&oacute;n desde un fichero PList.
	 * @param configFile Fichero de importaci&oacute;n con las preferencias de la aplicaci&oacute;n.
	 * @param parent Componente padre para la modalidad.
	 * @param unprotected
	 *        {@code true} Si las preferencias no est&aacute;n protegidas,
	 *        {@code false} en caso contrario
	 */
	public static void importPreferences(final String configFile, final Component parent, final boolean unprotected) {

		if (configFile == null){
			throw new IllegalStateException("El fichero de configuracion no puede ser nulo"); //$NON-NLS-1$
		}

		final byte[] configData;
		try (
			final InputStream fis = new FileInputStream(configFile);
			final InputStream bis = new BufferedInputStream(fis);
		) {
			configData = AOUtil.getDataFromInputStream(bis);
		}
		catch(final Exception e) {
			LOGGER.log(Level.SEVERE, "No ha sido posible leer el fichero de configuracion", e); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
				ConfiguratorCommonMessages.getString("PreferencesPlistHandler.0"), //$NON-NLS-1$
				ConfiguratorCommonMessages.getString("PreferencesPlistHandler.1"), //$NON-NLS-1$
				AOUIFactory.ERROR_MESSAGE,
				e);
			return;
		}

		if (!AOFileUtils.isXML(configData)) {
			LOGGER.severe("El fichero de preferencias no es XML"); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
					ConfiguratorCommonMessages.getString("PreferencesPlistHandler.9"), //$NON-NLS-1$
					ConfiguratorCommonMessages.getString("PreferencesPlistHandler.1"), //$NON-NLS-1$
					AOUIFactory.ERROR_MESSAGE,
					null
				);
			return;
		}

		String signMessage;
		try {
			XAdESValidator.validate(configData, true);

			try {
				final AOTreeModel tm = new AOXAdESSigner().getSignersStructure(configData, true);
				final AOSimpleSignInfo ssi = (AOSimpleSignInfo) ((AOTreeNode)AOTreeModel.getChild(tm.getRoot(), 0)).getUserObject();
				final X509Certificate signerCert = ssi.getCerts()[0];
				signMessage = ConfiguratorCommonMessages.getString("PreferencesPlistHandler.5", AOUtil.getCN(signerCert)); //$NON-NLS-1$
				//			if (!ALLOW_UNSIGNED_PREFERENCES && !Base64.encode(signerCert.getPublicKey().getEncoded()).equals(PREFERENCES_SIGNATURE_PUK_BASE64)) {
				//				LOGGER.severe("El firmante de las preferencias con el siguiente numero de serie no esta autorizado: " + signerCert.getSerialNumber()); //$NON-NLS-1$
				//				AOUIFactory.showErrorMessage(
				//						ConfiguratorCommonMessages.getString("PreferencesPlistHandler.6", AOUtil.getCN(signerCert)), //$NON-NLS-1$
				//						ConfiguratorCommonMessages.getString("PreferencesPlistHandler.1"), //$NON-NLS-1$
				//						AOUIFactory.ERROR_MESSAGE,
				//						null);
				//			}
			}
			catch (final AOInvalidFormatException e) {
				LOGGER.log(Level.SEVERE, "No se ha podido extraer la informacion de la firma del fichero de configuracion", e); //$NON-NLS-1$
				AOUIFactory.showErrorMessage(
						ConfiguratorCommonMessages.getString("PreferencesPlistHandler.7"), //$NON-NLS-1$
						ConfiguratorCommonMessages.getString("PreferencesPlistHandler.1"), //$NON-NLS-1$
						AOUIFactory.ERROR_MESSAGE,
						e);
				return;
			}
		}
		catch (final AOInvalidFormatException e) {
			LOGGER.severe("El fichero de configuracion no esta firmado"); //$NON-NLS-1$
			signMessage = ConfiguratorCommonMessages.getString("PreferencesPlistHandler.3"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.severe("El fichero de configuracion contiene una firma invalida: " + e); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
					ConfiguratorCommonMessages.getString("PreferencesPlistHandler.4"), //$NON-NLS-1$
					ConfiguratorCommonMessages.getString("PreferencesPlistHandler.1"), //$NON-NLS-1$
					AOUIFactory.ERROR_MESSAGE,
					null);
			return;
		}


		final int resp = AOUIFactory.showConfirmDialog(
			parent,
			ConfiguratorCommonMessages.getString("PreferencesPlistHandler.10", signMessage), //$NON-NLS-1$
			ConfiguratorCommonMessages.getString("PreferencesPlistHandler.11"), //$NON-NLS-1$
			AOUIFactory.YES_NO_OPTION,
			AOUIFactory.WARNING_MESSAGE);

		if (AOUIFactory.NO_OPTION == resp) {
			return;
		}

		try {
			importUserPreferencesFromXml(new String(configData), unprotected);
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "El fichero de configuracion es invalido", e); //$NON-NLS-1$
			AOUIFactory.showErrorMessage(
				ConfiguratorCommonMessages.getString("PreferencesPlistHandler.8"), //$NON-NLS-1$
				ConfiguratorCommonMessages.getString("PreferencesPlistHandler.1"), //$NON-NLS-1$
				AOUIFactory.ERROR_MESSAGE,
				e);
		}

	}

	/**
	 * Se importan las preferencias del usuario desde un XML
	 * @param xml XML con la informaci&oacute;n
	 * @param unprotected Indica si las preferencias est&aacute;n protegidas o no
	 * @throws InvalidPreferencesFileException error en caso de archivo no v&aacute;lido
	 */
	public static void importUserPreferencesFromXml(final String xml, final boolean unprotected) throws InvalidPreferencesFileException {
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
	public static Map<String, Object> loadPreferencesFromXml(final String xml, final boolean unprotected) throws InvalidPreferencesFileException {
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
		final Map<String, Object> properties;
		try {
			properties = Plist.fromXml(xml);
		}
		catch (final XmlParseException e) {
			throw new InvalidPreferencesFileException("Error analizando el fichero XML: " + e, e); //$NON-NLS-1$
		}

		// Registramos las tarjetas inteligentes que se encuentren en el XML
		if (properties.containsKey(SMARTCARDS_KEY)) {
			KeyStorePreferencesManager.putSystemSmartCardsMap((Map<String, Object>) properties.get(SMARTCARDS_KEY));
			properties.remove(SMARTCARDS_KEY);
		}

		checkPreferences(properties);
		storeSystemPreferences(properties);
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

	/** Error de a&aacute;lisis del fichero de preferencias. */
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
