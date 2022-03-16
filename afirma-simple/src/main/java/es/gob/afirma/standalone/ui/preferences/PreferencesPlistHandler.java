/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import java.awt.Component;
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.cert.X509Certificate;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.http.DataDownloader;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signvalidation.SignValider;
import es.gob.afirma.signvalidation.SignValiderFactory;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.standalone.DataAnalizerUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import xmlwise.Plist;
import xmlwise.XmlParseException;

/** Carga las preferencias de la aplicaci&oacute;n desde un fichero PList. */
final class PreferencesPlistHandler {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String PREFERENCES_SIGNATURE_PUK_BASE64;
	private static final boolean ALLOW_UNSIGNED_PREFERENCES;
	static {
		final Properties p = new Properties();
		try {
			p.load(
				PreferencesPlistHandler.class.getResourceAsStream(
					"/properties/configuration.properties" //$NON-NLS-1$
				)
			);
		}
		catch(final Exception e) {
			LOGGER.severe(
				"No han podido cargarse las preferencias de configuracion, se usaran los valores por defecto: " + e //$NON-NLS-1$
			);
		}
		ALLOW_UNSIGNED_PREFERENCES = Boolean.parseBoolean(
			p.getProperty("allowUnsignedPreferencesFiles", "false") //$NON-NLS-1$ //$NON-NLS-2$
		);
		PREFERENCES_SIGNATURE_PUK_BASE64 = 	p.getProperty("preferencesSignaturePublicKey"); //$NON-NLS-1$
		if (!ALLOW_UNSIGNED_PREFERENCES && PREFERENCES_SIGNATURE_PUK_BASE64 == null) {
			throw new IllegalStateException(
				"Si no se permiten preferencias sin firmar es necesario configurar una clave publica de firmante" //$NON-NLS-1$
			);
		}
	}

	/** Importa las preferencias de la aplicaci&oacute;n desde un fichero PList descargado desde una URL.
	 * @param url URL para la descarga del fichero de importaci&oacute;n con las preferencias de la aplicaci&oacute;n.
	 * @param unprotected
	 *        {@code true} Si las preferencias no est&aacute;n protegidas,
	 *        {@code false} en caso contrario
	 * @throws IOException Si hay problemas con la descarga del fichero de preferencias.
	 * @throws InvalidPreferencesFileException Si las preferencias no son v&aacute;lidas por cualquier motivo.
	 * @throws AOInvalidFormatException Si no se puede obtener informac&oacute;n sobre la firma de las preferencias. */
	static void importPreferencesFromUrl(final String url, final boolean unprotected) throws IOException, InvalidPreferencesFileException, AOInvalidFormatException {
		if (url == null){
			throw new IllegalStateException("La URL de descarga del fichero de configuracion no puede ser nula"); //$NON-NLS-1$
		}
		final byte[] configData = DataDownloader.downloadData(url);
		if (!ALLOW_UNSIGNED_PREFERENCES) {
			final SignValider valider= SignValiderFactory.getSignValider(configData);
			if (valider == null) {
				throw new InvalidPreferencesFileException(
					"La firma del fichero de preferencias no es valida" //$NON-NLS-1$
				);
			}
			final SignValidity sv = valider.validate(configData);
			if (!sv.getValidity().equals(SIGN_DETAIL_TYPE.OK)) {
				throw new InvalidPreferencesFileException(
					"La firma del fichero de preferencias no es valida" //$NON-NLS-1$
				);
			}
			final AOTreeModel tm = new AOXAdESSigner().getSignersStructure(configData, true);
			final AOSimpleSignInfo ssi = (AOSimpleSignInfo) ((AOTreeNode)AOTreeModel.getChild(tm.getRoot(), 0)).getUserObject();
			final X509Certificate signerCert = ssi.getCerts()[0];
			if (!Base64.encode(signerCert.getPublicKey().getEncoded()).equals(PREFERENCES_SIGNATURE_PUK_BASE64)) {
				throw new InvalidPreferencesFileException(
					"La firmante del fichero de preferencias no esta autorizado" //$NON-NLS-1$
				);
			}
		}
		importPreferencesFromXml(new String(configData), unprotected);
	}

	/** Importa las preferencias de la aplicaci&oacute;n desde un fichero PList.
	 * @param configFile Fichero de importaci&oacute;n con las preferencias de la aplicaci&oacute;n.
	 * @param parent Componente padre para la modalidad.
	 * @param unprotected
	 *        {@code true} Si las preferencias no est&aacute;n protegidas,
	 *        {@code false} en caso contrario */
	static void importPreferences(final String configFile, final Component parent, final boolean unprotected) {

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
			LOGGER.log(
				Level.SEVERE,
				"No ha sido posible leer el fichero de configuracion: " + e, //$NON-NLS-1$
				e
			);
			AOUIFactory.showErrorMessage(
				SimpleAfirmaMessages.getString("PreferencesPlistHandler.0"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("PreferencesPlistHandler.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				e
			);
			return;
		}

		if (!DataAnalizerUtil.isXML(configData)) {
			AOUIFactory.showErrorMessage(
					SimpleAfirmaMessages.getString("PreferencesPlistHandler.9"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("PreferencesPlistHandler.1"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					null
				);
			return;
		}

		SignValidity sv = new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, null);
		try {
			sv = SignValiderFactory.getSignValider(configData).validate(configData);
		}
		catch (final Exception e) {
			LOGGER.severe(
				"Error comprobando la firma del fichero: " + e //$NON-NLS-1$
			);
		}
		final String signMessage;
		if (!sv.getValidity().equals(SIGN_DETAIL_TYPE.OK)) {
			switch(sv.getError()) {
				case NO_SIGN:
					if (ALLOW_UNSIGNED_PREFERENCES) {
						signMessage = SimpleAfirmaMessages.getString("PreferencesPlistHandler.2"); //$NON-NLS-1$
					}
					else {
						LOGGER.severe(
							"El fichero de configuracion no esta firmado" //$NON-NLS-1$
						);
						AOUIFactory.showErrorMessage(
							SimpleAfirmaMessages.getString("PreferencesPlistHandler.3"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("PreferencesPlistHandler.1"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE,
							null
						);
						return;
					}
					break;
				default:
					LOGGER.severe(
						"El fichero de configuracion contiene una firma invalida: " + sv.getError() //$NON-NLS-1$
					);
					AOUIFactory.showErrorMessage(
						SimpleAfirmaMessages.getString("PreferencesPlistHandler.4"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("PreferencesPlistHandler.1"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						null
					);
					return;
			}

		}
		else {
			try {
				final AOTreeModel tm = new AOXAdESSigner().getSignersStructure(configData, true);
				final AOSimpleSignInfo ssi = (AOSimpleSignInfo) ((AOTreeNode)AOTreeModel.getChild(tm.getRoot(), 0)).getUserObject();
				final X509Certificate signerCert = ssi.getCerts()[0];
				signMessage = SimpleAfirmaMessages.getString("PreferencesPlistHandler.5", AOUtil.getCN(signerCert)); //$NON-NLS-1$
				if (!ALLOW_UNSIGNED_PREFERENCES && !Base64.encode(signerCert.getPublicKey().getEncoded()).equals(PREFERENCES_SIGNATURE_PUK_BASE64)) {
					LOGGER.severe(
						"El firmante de las preferencias con el siguiente numero de serie no esta autorizado: " + signerCert.getSerialNumber() //$NON-NLS-1$
					);
					AOUIFactory.showErrorMessage(
						SimpleAfirmaMessages.getString("PreferencesPlistHandler.6", AOUtil.getCN(signerCert)), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("PreferencesPlistHandler.1"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE,
						null
					);
				}
			}
			catch (final AOInvalidFormatException e) {
				LOGGER.severe(
					"No se ha podido extraer la informacion de la firma del fichero de configuracion: " + e //$NON-NLS-1$
				);
				AOUIFactory.showErrorMessage(
					SimpleAfirmaMessages.getString("PreferencesPlistHandler.7"), //$NON-NLS-1$
					SimpleAfirmaMessages.getString("PreferencesPlistHandler.1"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					e
				);
				return;
			}
		}

		final int resp = AOUIFactory.showConfirmDialog(
			parent,
			SimpleAfirmaMessages.getString("PreferencesPlistHandler.109", signMessage),  //$NON-NLS-1$
			SimpleAfirmaMessages.getString("PreferencesPlistHandler.110"),  //$NON-NLS-1$
			AOUIFactory.YES_NO_OPTION,
			AOUIFactory.WARNING_MESSAGE
		);
		if (JOptionPane.NO_OPTION == resp) {
			return;
		}

		try {
			importPreferencesFromXml(new String(configData), unprotected);
		}
		catch (final Exception e) {
			LOGGER.log(
				Level.SEVERE,
				"El fichero de configuracion es invalido: " + e, //$NON-NLS-1$
				e
			);
			AOUIFactory.showErrorMessage(
				SimpleAfirmaMessages.getString("PreferencesPlistHandler.8"), //$NON-NLS-1$
				SimpleAfirmaMessages.getString("PreferencesPlistHandler.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				e
			);
		}

	}

	private static void importPreferencesFromXml(final String xml, final boolean unprotected) throws InvalidPreferencesFileException {
		final Map<String, Object> properties;
		try {
			properties = Plist.fromXml(xml);
		}
		catch (final XmlParseException e) {
			throw new InvalidPreferencesFileException("Error analizando el fichero XML: " + e, e); //$NON-NLS-1$
		}

		checkPreferences(properties);
		storePreferences(properties, unprotected);
	}

	/** Comprueba que las preferencias pasadas sean <code>String</code> o <code>Boolean</code>.
	 * @param prefs Preferencias a comprobar.
	 * @throws InvalidPreferencesFileException Si hay alguna preferencia de un tipo no soportado. */
	private static void checkPreferences(final Map<String, Object> prefs) throws InvalidPreferencesFileException{
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
	 * Almacena las preferencias en el registro del sistema de Windows.
	 * @param prefs
	 *            Preferencias que se guardar&aacute; en el registro de Windows.
	 * @param unprotected
	 *            {@code true} Si las preferencias no est&aacute;n protegidas,
	 *            {@code false} en caso contrario
	 */
	private static void storePreferences(final Map<String, Object> prefs, final boolean unprotected){
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
