/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server.document;

import java.io.File;
import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.http.DataDownloader;
import es.gob.afirma.signers.batch.SignSaver;
import es.gob.afirma.signers.batch.xml.SingleSign;
import es.gob.afirma.triphase.server.ConfigManager;

/**
 * Gestor de documentos que incorpora la funcionalidad de los SignSaver usados en el modelo antiguo de firma de lotes.
 */
public class LegacyBatchDocumentManager implements BatchDocumentManager {

	private static final String HTTP_SCHEME = "http://"; //$NON-NLS-1$
	private static final String HTTPS_SCHEME = "https://"; //$NON-NLS-1$
	private static final String FTP_SCHEME = "ftp://"; //$NON-NLS-1$
	private static final String FILE_SCHEME = "file:/"; //$NON-NLS-1$
	private static final String BASE64 = "base64"; //$NON-NLS-1$

	private static final String CONFIG_PARAM_DOCMANAGER_ALLOWSOURCES = "docmanager.legacybatch.allowedsources"; //$NON-NLS-1$
	private static final String CONFIG_PARAM_DOCMANAGER_CHECKSSLCERTS = "docmanager.legacybatch.checksslcerts"; //$NON-NLS-1$
	private static final String CONFIG_PARAM_DOCMANAGER_MAXDOCSIZE = "docmanager.legacybatch.maxDocSize"; //$NON-NLS-1$
	private static final String ALLOWSOURCES_SEPARATOR = ";"; //$NON-NLS-1$
	private static final String ALLOWSOURCES_WILD_CARD = "*"; //$NON-NLS-1$

	private static final String REPONSE_OK = Base64.encode("OK".getBytes()); //$NON-NLS-1$

	private static final String PROP_SIGNSAVER = "signSaver"; //$NON-NLS-1$

	private static final String DEFAULT_VALUE_CHECKSSLCERTS = "true"; //$NON-NLS-1$

	private static final String SYS_PROP_DISABLE_SSL = "disableSslChecks"; //$NON-NLS-1$

	@Override
	public void init(final Properties config) {

		// Establecemos si se deben comprobar los certificados SSL de las conexiones remotas
		final boolean checkSslCerts = Boolean.parseBoolean(
				ConfigManager.getConfig().getProperty(CONFIG_PARAM_DOCMANAGER_CHECKSSLCERTS, DEFAULT_VALUE_CHECKSSLCERTS));
		System.setProperty(SYS_PROP_DISABLE_SSL, Boolean.toString(!checkSslCerts));
	}

	@Override
	public byte[] getDocument(final String dataRef, final X509Certificate[] certChain, final Properties config) throws IOException, SecurityException {

		// Si no se indica el mecanismo de guardado, indicamos ya el error paraevitar que se deba iniciar
		// el procesado de la firma para fallar mas tarde
		if (config == null || !config.containsKey(PROP_SIGNSAVER)) {
			throw new IOException("No se ha indicado el mecanismo de guardado de la firma"); //$NON-NLS-1$
		}

		byte[] data;
		if (dataRef.startsWith(HTTP_SCHEME) || dataRef.startsWith(HTTPS_SCHEME)
				|| dataRef.startsWith(FTP_SCHEME) || dataRef.startsWith(FILE_SCHEME)) {
			checkDataSource(dataRef);
			data = getRemoteData(dataRef);
		}
		else {
			checkDataSource(dataRef);
			data = Base64.decode(dataRef.replace("-", "+").replace("_", "/")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		}

		if (ConfigManager.getConfig().containsKey(CONFIG_PARAM_DOCMANAGER_MAXDOCSIZE)) {
			final long maxDocSize = Long.parseLong(ConfigManager.getConfig().getProperty(CONFIG_PARAM_DOCMANAGER_MAXDOCSIZE));
			if (maxDocSize > 0 && data.length > maxDocSize) {
				throw new SecurityException("El tamano del documento es superior al permitido. Tamano del documento: " + data.length); //$NON-NLS-1$
			}
		}

		return data;
	}

	/**
	 * Carga el contenido de un fichero local o remoto.
	 * @param dataRef URI o referencia a los datos.
	 * @return Contenido del fichero.
	 * @throws IOException Cuando falla la carga de los datos.
	 */
	private static byte[] getRemoteData(final String dataRef) throws IOException {
		return DataDownloader.downloadData(dataRef);
	}

	/**
	 * Comprueba que la referencia a los datos se corresponda con un origen v&aacute;lido
	 * seg&uacute;n la configuraci&oacute;n establecida.
	 * @param dataRef Referencia a los datos.
	 * @throws IOException Cuando el origen de los datos no es v&aacute;lido.
	 */
	private static void checkDataSource(final String dataRef) throws IOException {
		if (dataRef == null) {
			throw new IllegalArgumentException(
				"El origen de los datos no puede ser nulo" //$NON-NLS-1$
			);
		}
		final String allowSources = ConfigManager.getConfig().getProperty(CONFIG_PARAM_DOCMANAGER_ALLOWSOURCES);
		if (allowSources != null) {
			for (final String allowed : allowSources.split(ALLOWSOURCES_SEPARATOR)) {
				if (BASE64.equalsIgnoreCase(allowed) && Base64.isBase64(dataRef)) {
					return;
				}
				if (allowed.endsWith(ALLOWSOURCES_WILD_CARD)) {
					if (dataRef.startsWith(allowed.replace(ALLOWSOURCES_WILD_CARD, ""))) { //$NON-NLS-1$
						// Si no es de tipo file:/, lo damos por bueno
						if (!allowed.startsWith(FILE_SCHEME)) {
							return;
						}
						// Si es de tipo file:/, nos aseguramos de anular vulnerabilidades de tipo Path Transversal
						final String path = dataRef.substring(FILE_SCHEME.length());
						final String dataRefCleaned = FILE_SCHEME + new File(path).getCanonicalPath().replace('\\', '/');
						if (dataRefCleaned.startsWith(allowed.replace(ALLOWSOURCES_WILD_CARD, ""))) { //$NON-NLS-1$
							return;
						}
					}
				}
				else if (dataRef.equals(allowed)) {
					return;
				}
			}
		}
		throw new IOException("Origen de datos no valido: " + cleanTextToPrint(dataRef)); //$NON-NLS-1$
	}

	/**
	 * Devuelve el texto o una porcion del mismo que no exceda un tama&ntilde;o considerado excesivo para imprimir.
	 * @param text Texto a limpiar.
	 * @return Texto apto para imprimir en el log.
	 */
	private static String cleanTextToPrint(final String text) {
		if (text == null) {
			return "null"; //$NON-NLS-1$
		}
		if (text.length() > 60) {
			return text.substring(0, 60) + "..."; //$NON-NLS-1$
		}
		return text;
	}

	@Override
	public String storeDocument(final String id, final X509Certificate[] certChain, final byte[] data, final Properties config) throws IOException {

		// Cargamos la clase de guardado
		final SignSaver signSaver = loadSignSaver(config, id);
		// Cargamos la configuracion
		final SingleSign ss = loadSingleSignConfig(config, id);
		// Guardamos
		signSaver.saveSign(ss, data);

		return REPONSE_OK;
	}

	@Override
	public void rollback(final String id,
            final X509Certificate[] certChain,
            final Properties config) throws IOException {

		// Cargamos la clase de guardado
		final SignSaver signSaver = loadSignSaver(config, id);
		// Cargamos la configuracion
		final SingleSign ss = loadSingleSignConfig(config, id);
		// Revertimos el guardado
		signSaver.rollback(ss);
	}

	/**
	 * Carga un objeto para el procesado de la firma.
	 * @param config Datos proporcionados de la configuraci&oacute;n.
	 * @param id Identificador de los datos a firmar.
	 * @return Objeto para el procesado de la firma.
	 * @throws IOException Cuando no se puede instanciar el objeto.
	 */
	private SignSaver loadSignSaver(final Properties config, final String id) throws IOException {

		// Comprobamos que la clase indicada implemente la interfaz SignSaver para
		// asegurarnos de que no se ejecuta su contexto estatico si no es necesario
		SignSaver signSaver;
		try {
			final String signSaverClassName = config.getProperty(PROP_SIGNSAVER);
			final Class<?> signSaverClass = Class.forName(signSaverClassName, false, getClass().getClassLoader());
			if (!SignSaver.class.isAssignableFrom(signSaverClass)) {
				throw new IllegalArgumentException("La clase indicada no implementa la interfaz " + SignSaver.class.getName()); //$NON-NLS-1$
			}
			signSaver = (SignSaver) signSaverClass.newInstance();
		} catch (final Exception e) {
			throw new IOException("No se pudo cargar la clase de guardado para la firma " + cleanTextToPrint(id), e); //$NON-NLS-1$
		}

		try {
			signSaver.init(config);
		} catch (final Exception e) {
			throw new IOException("No se pudo inicializar la clase de guardado para la firma " + cleanTextToPrint(id), e); //$NON-NLS-1$
		}

		return signSaver;
	}

	/**
	 * Carga la configuracion de firma.
	 * @param config Datos proporcionados de la configuraci&oacute;n.
	 * @param id Identificador de los datos a firmar.
	 * @return Configuraci&oacute;n de firma.
	 */
	private static SingleSign loadSingleSignConfig(final Properties config, final String id) {
		final SingleSign ss = new SingleSign(id);
		for (final String key : config.keySet().toArray(new String[0])) {
			ss.getExtraParams().setProperty(key, config.getProperty(key));
		}
		return ss;
	}
}
