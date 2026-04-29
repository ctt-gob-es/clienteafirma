/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc.protocol;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.ErrorCode;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.http.DataDownloader;

/** Par&aacute;metros habitualmente comunes para todas las operaciones. */
public abstract class UrlParameters {

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Par&aacute;metro de entrada con las opciones de configuraci&oacute;n de la firma. */
	protected static final String PROPERTIES_PARAM = "properties"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con los datos a firmar. */
	protected static final String DATA_PARAM = "dat"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que indica si los datos a firmar vienen comrpimidos en GZIP,
	 * pero hay que firmarlos descomprimidos.
	 * La compresi&oacute;n en GZIP permite reducir el uso de servidor intermedio haciendo que
	 * quepan m&aacute;s datos en la URL de invocaci&oacute;n. */
	protected static final String GZIPPED_DATA_PARAM = "gzip"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el servlet remoto de recuperaci&oacute;n de datos. */
	protected static final String RETRIEVE_SERVLET_PARAM = "rtservlet"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el servlet remoto de guardado de datos. */
	protected static final String STORAGE_SERVLET_PARAM = "stservlet"; //$NON-NLS-1$

	/** N&uacute;mero m&aacute;ximo de caracteres permitidos para el identificador
	 * de sesi&oacute;n de la operaci&oacute;n. */
	protected static final int MAX_ID_LENGTH = 20;

	/** Longitud permitida para la clave de cifrado. */
	private static final int CIPHER_KEY_LENGTH = 8;

	/** Par&aacute;metro de entrada con la clave para el cifrado del documento. */
	protected static final String KEY_PARAM = "key"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la clave AES para el cifrado del documento. */
	protected static final String CIPHER_PARAM = "cipher"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el identificador del fichero remoto de datos. */
	protected static final String FILE_ID_PARAM = "fileid"; //$NON-NLS-1$

	//TODO: Eliminar para terminar la compatibilidad con Autofirma 1.4.X
	/** Viejo par&aacute;metro de entrada con la configuraci&oacute;n del almac&eacute;n de claves. */
	protected static final String KEYSTORE_OLD_PARAM = "keystore"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la configuraci&oacute;n del almac&eacute;n de claves en base64. */
	protected static final String KEYSTORE_PARAM = "ksb64"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el valor indicativo de si se debe realizar una espera
	 * activa hasta la devoluci&uacute;on servlet remoto de guardado de datos. */
	protected static final String ACTIVE_WAITING_PARAM = "aw"; //$NON-NLS-1$

	/** Versi&oacute;n m&iacute;nima de cliente requetida. */
	protected static final String MINIMUM_CLIENT_VERSION_PARAM = "mcv"; //$NON-NLS-1$

	/** Nombre de aplicaci&oacute;n o dominio desde el que se realiza la llamada. */
	protected static final String APP_NAME_PARAM = "appname"; //$NON-NLS-1$

	/** Tiempo de espera para la lectura de peticiones. */
	protected static final String SERVICE_TIMEOUT_PARAM = "servicetimeout"; //$NON-NLS-1$

	/** Codificaci&oacute;n por defecto. */
	private static final String DEFAULT_ENCODING = StandardCharsets.UTF_8.name();

	protected byte[] data = null;
	private String fileId = null;
	private byte[] cipherConfig = null;
	private URL retrieveServletUrl = null;
	private URL storageServerUrl = null;
	private String id = null;
	private String minimumClientVersion = null;
	private boolean activeWaiting = false;
	private int serviceTimeout = -1;

	private String defaultKeyStore = null;
	private String defaultKeyStoreLib = null;
	private Properties extraParams = null;
	private String filename = null;

	/**
	 * Obtiene los par&aacute;metros adicionales de la firma.
	 * Se pueden modificar estos parametros a partir del
	 * objeto devuelto.
	 * @return Par&aacute;metros adicionales de la firma
	 */
	public Properties getExtraParams() {
		if (this.extraParams == null) {
			this.extraParams = new Properties();
		}
		return this.extraParams;
	}

	/** Establece los par&aacute;metros adicionales para la configuraci&oacute;n
	 * de la operaci&oacute;n de firma.
	 * @param properties Propiedades adicionales. */
	void setExtraParams(final Properties properties) {
		this.extraParams = properties != null ? properties : new Properties();
	}

	/** Obtiene el nombre de fichero propuesto para guardar los datos.
	 * @return Nombre de fichero propuesto para guardar los datos */
	public String getFileName() {
		return this.filename;
	}

	/** Establece el nombre de fichero propuesto para guardar los datos.
	 * @param filename Nombre de fichero propuesto para guardar los datos */
	void setFilename(final String filename) {
		this.filename = filename;
	}

	/** Obtiene el nombre del almac&eacute;n de claves a usar por defecto.
	 * @return Nombre del almac&eacute;n de claves a usar por defecto */
	public String getDefaultKeyStore() {
		return this.defaultKeyStore;
	}

	/** Establece el nombre del almac&eacute;n de claves a usar por defecto.
	 * @param storeName Nombre del almac&eacute;n de claves a usar por defecto */
	void setDefaultKeyStore(final String storeName) {
		this.defaultKeyStore = storeName;
	}

	/** Obtiene la ruta de la biblioteca del almac&eacute;n de claves a usar por defecto.
	 * @return Ruta de la biblioteca del almac&eacute;n de claves a usar por defecto */
	public String getDefaultKeyStoreLib() {
		return this.defaultKeyStoreLib;
	}

	/** Establece la biblioteca del almac&eacute;n de claves a usar por defecto.
	 * @param storeLib Ruta de la biblioteca del almac&eacute;n de claves a usar por defecto */
	void setDefaultKeyStoreLib(final String storeLib) {
		this.defaultKeyStoreLib = storeLib;
	}

	/** Establece los datos.
	 * @param dat Datos. */
	public void setData(final byte[] dat) {
		this.data = dat != null ? Arrays.copyOf(dat, dat.length) : null;
	}

	/** Establece el identificador de los datos en el servidor intermedio.
	 * @param fileId Identificador de los datos en el servidor intermedio */
	void setFileId(final String fileId) {
		this.fileId = fileId;
	}

	/** Establece la URL de subida al servidor intermedio.
	 * @param retrieveServletUrl URL de subida al servidor intermedio */
	void setRetrieveServletUrl(final URL retrieveServletUrl) {
		this.retrieveServletUrl = retrieveServletUrl;
	}

	/** Obtiene los datos.
	 * @return Datos. */
	public byte[] getData() {
		return  this.data != null ? Arrays.copyOf(this.data, this.data.length) : null;
	}

	/** Obtiene el identificador de los datos en el servidor intermedio.
	 * @return Identificador de los datos en el servidor intermedio */
	public String getFileId() {
		return this.fileId;
	}

	/** Obtiene la URL de subida al servidor intermedio.
	 * @return URL de subida al servidor intermedio */
	public URL getRetrieveServletUrl() {
		return this.retrieveServletUrl;
	}

	/** Obtiene la URL del servlet de almacenamiento temporal en servidor.
	 * @return URL del servlet de almacenamiento temporal en servidor */
	public URL getStorageServletUrl() {
		return this.storageServerUrl;
	}

	protected void setStorageServletUrl(final URL url) {
		this.storageServerUrl = url;
	}

	/** Obtiene el indicador de si se ha solicitado que se realice una espera activa
	 * a la espera del fin de la operaci&oacute;n solicitada.
	 * @return {@code true} si se pide que se emita la solicitud de espera activa,
	 * {@code false} en caso contrario. */
	public boolean isActiveWaiting() {
		return this.activeWaiting;
	}

	/**
	 * Establece si debe solicitarse a los clientes la espera activa hasta la obtenci&oacute;n
	 * del resultado de la operaci&oacute;n.
	 * @param activeWaiting Espera activa.
	 */
	protected void setActiveWaiting(final boolean activeWaiting) {
		this.activeWaiting = activeWaiting;
	}

	/** Obtiene la versi&oacute;n m&iacute;nima del cliente exigida por la
	 * aplicaci&oacute;n.
	 * @return Versi&oacute;n m&iacute;nima del cliente o {@code null} si no
	 * se estableci&oacute;. */
	public String getMinimumClientVersion() {
		return this.minimumClientVersion;
	}

	/**
	 * Establece qu&eacute; versi&oacute;n m&iacute;nima del cliente exige la
	 * aplicaci&oacute;n.
	 * @param minimumClientVersion Versi&oacute;n m&iacute;nima del cliente.
	 */
	protected void setMinimumClientVersion(final String minimumClientVersion) {
		this.minimumClientVersion = minimumClientVersion;
	}

	/** Obtiene el identificador de sesi&oacute;n.
	 * @return Identificador de sesi&oacute;n */
	public String getId() {
		return this.id;
	}

	void setSessionId(final String sessionId) {
		this.id = sessionId;
	}

	void setCipherConfig(final byte[] cipherConfig) {
		this.cipherConfig = cipherConfig;
	}

	public byte [] getCipherConfig() {
		return this.cipherConfig;
	}

	void setServiceTimeout(final int serviceTimeout) {
		this.serviceTimeout = serviceTimeout;
	}

	public int getServiceTimeout() {
		return this.serviceTimeout;
	}

	void setCommonParameters(final Map<String, String> params) throws ParameterException, ParameterLocalAccessRequestedException{

		if (params.containsKey(CIPHER_PARAM)) {
			try {
				byte[] decodedCipherConfig = Base64.decode(params.get(CIPHER_PARAM).replace("_", "/").replace("-", "+"));  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				if (params.containsKey(KEY_PARAM)) {
					final String cipherKeyDES = verifyDESCipherKey(params);
					if (cipherKeyDES != null) {
						decodedCipherConfig = addLegacyDesConfig(decodedCipherConfig, cipherKeyDES);
					}
				}
				setCipherConfig(decodedCipherConfig);
			} catch (final IOException e) {
				throw new ParameterException("El JSON para la clave de cifrado no esta formado correctamente",  //$NON-NLS-1$
												ErrorCode.Request.UNSUPPORTED_CIPHER_KEY);
			}
		} else if (params.containsKey(KEY_PARAM)) {
			final String cipherKeyDES = verifyDESCipherKey(params);
			if (cipherKeyDES != null) {
				setCipherConfig(createDesJSON(cipherKeyDES).getBytes());
			}
		}

		setActiveWaiting(params.containsKey(ACTIVE_WAITING_PARAM) &&
				Boolean.parseBoolean(params.get(ACTIVE_WAITING_PARAM)));

		if (params.containsKey(MINIMUM_CLIENT_VERSION_PARAM)) {
			setMinimumClientVersion(params.get(MINIMUM_CLIENT_VERSION_PARAM));
		}

		if (params.containsKey(SERVICE_TIMEOUT_PARAM)) {
			try {
				final int servTimeout = Integer.parseInt(params.get(SERVICE_TIMEOUT_PARAM));
				if (servTimeout >= 0) {
					setServiceTimeout(servTimeout);
				}
			} catch (final Exception e) {
				throw new ParameterException("El valor del parametro del tiempo de espera de lectura de los servicios no es valido",  //$NON-NLS-1$
						ErrorCode.Request.UNSUPPORTED_CIPHER_KEY);
			}
		}

		// Comprobamos que se nos hayan indicado los datos o, en su defecto, el
		// identificador de fichero remoto a descargar y la ruta del
		// servicio remoto para el fichero
		if (!params.containsKey(DATA_PARAM)) {

			if (params.containsKey(FILE_ID_PARAM)) {

				setFileId(params.get(FILE_ID_PARAM));

				if (!params.containsKey(RETRIEVE_SERVLET_PARAM)) {
					throw new ParameterException(
						"No se ha recibido la direccion del servlet para la recuperacion de los datos a firmar", //$NON-NLS-1$
						ErrorCode.Request.RETRIEVE_URL_TO_SIGN_NOT_FOUND
					);
				}

				try {
					setRetrieveServletUrl(
						validateURL(
							params.get(RETRIEVE_SERVLET_PARAM)
						)
					);
				}
				catch (final LocalAccessRequestException e) {
					throw new ParameterLocalAccessRequestedException(
						"La URL del servicio de recuperacion de datos no puede ser local", e, ErrorCode.Request.LOCAL_RETRIEVE_URL //$NON-NLS-1$
					);
				}
				catch (final IllegalArgumentException e) {
					throw new ParameterException(
						"Error al validar la URL del servlet de recuperacion: " + e, e, //$NON-NLS-1$
						ErrorCode.Request.INVALID_RETRIEVE_URL_TO_SIGN
					);
				}
			}
		}
		else {
			final String dataPrm = params.get(DATA_PARAM);
			if (dataPrm.startsWith("file:/")) { //$NON-NLS-1$
				throw new ParameterException(
					"No se permite la lectura de ficheros locales: " + dataPrm, //$NON-NLS-1$
					ErrorCode.Request.RETRIEVE_URL_TO_SIGN_CANT_BE_LOCAL
				);
			}
			try {
				setData(
					DataDownloader.downloadData(
						dataPrm,
						// Boolean.parseBoolean() da false con null y en general con cualquier cosa que
						// no sea la cadena "true"
						Boolean.parseBoolean(params.get(GZIPPED_DATA_PARAM))
					)
				);
			}
			catch (final Exception e) {
				throw new ParameterException(
					"No se han podido obtener los datos: " + e, e, //$NON-NLS-1$
					ErrorCode.Internal.LOADING_LOCAL_FILE_ERROR
				);
			}
		}
	}

	/**
	 * Agrega una propiedad adicional al JSON de configuraci&oacute;n en la que se
	 * indica la clave DES necesaria para la compatibilidad con el JavaScript antiguo.
	 * @param config JSON de configuraci&oacute;n de cifrado decodificado.
	 * @param cipherKeyDES Clave DES.
	 * @return Configuraci&oacute;n de cifrado de entrada junto con la clave DES.
	 */
	private static byte[] addLegacyDesConfig(final byte[] config, final String cipherKeyDES) {

		String configJson = new String(config);
		configJson = configJson.substring(0, configJson.length() - 1)
				+ ",legacydes:\"" + cipherKeyDES + "\"}"; //$NON-NLS-1$ //$NON-NLS-2$

		return configJson.getBytes();
	}

	/** Extrae y verifica la clave de cifrado de los par&aacute;metros de entrada.
	 *  @param params Par&aacute;metros extra&iacute;dos de la URI.
	 *  @return Clave de cifrado o <code>null</code> si no se declar&oacute; un valor en los par&aacute;metros.
	 *  @throws ParameterException Cuando la clave de cifrado es err&oacute;nea. */
	private static String verifyDESCipherKey(final Map<String, String> params) throws ParameterException {

		// Comprobamos que se ha especificado la clave de cifrado
		if (!params.containsKey(KEY_PARAM)) {
			return null;
		}

		// Si se ha indicado el parametro pero no un valor, se intepretara que no hay cifrado
		final String key = params.get(KEY_PARAM);
		if (key == null || key.length() == 0) {
			return null;
		}

		// Comprobamos que la clave de cifrado tenga la longitud correcta
		if (key.length() != CIPHER_KEY_LENGTH) {
			throw new ParameterException("La longitud de la clave de cifrado no es correcta", ErrorCode.Request.UNSUPPORTED_CIPHER_KEY); //$NON-NLS-1$
		}

		return key;
	}

	/** Forma un JSON con el algoritmo DES y su clave
	 *  @param desKey ClaveDES.
	 *  @return JSON con algoritmo y clave. */
	private static String createDesJSON(final String desKey) {

		return "{algo:\"DES\", key:\"" + desKey + "\"}";  //$NON-NLS-1$//$NON-NLS-2$
	}

	/**
	 * Valida una URL para asegurar que cumple con los requisitos m&iacute;nimos de seguridad.
	 * @param url URL que se desea validar.
	 * @return URL formada y validada.
	 * @throws IllegalArgumentException Cuando ocurre alg&uacute;n problema al validar la URL.
	 * @throws LocalAccessRequestException Cuando la URL usa los dominios localhost o 127.0.0.1.
	 */
	protected static URL validateURL(final String url) throws IllegalArgumentException, LocalAccessRequestException {

		// Comprobamos que la URL sea valida
		final URL servletUrl;
		try {
			servletUrl = new URL(URLDecoder.decode(url, DEFAULT_ENCODING));
		}
		catch (final Exception e) {
			throw new IllegalArgumentException(
				"La URL proporcionada para el servlet no es valida (" + url + "): " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		// Comprobamos que el protocolo este soportado
		if (!"http".equals(servletUrl.getProtocol()) && !"https".equals(servletUrl.getProtocol())) { //$NON-NLS-1$ //$NON-NLS-2$
			throw new IllegalArgumentException(
				"El protocolo de la URL proporcionada para el servlet no esta soportado: " + servletUrl.getProtocol() //$NON-NLS-1$
			);
		}
		// Comprobamos que la URL sea una llamada al servlet y que no sea local
		if ("localhost".equals(servletUrl.getHost()) || "127.0.0.1".equals(servletUrl.getHost())) { //$NON-NLS-1$ //$NON-NLS-2$
			throw new LocalAccessRequestException(
				"El host de la URL proporcionada para el Servlet es local" //$NON-NLS-1$
			);
		}
		// El servlet no puede recibir parametros
		if (servletUrl.toString().indexOf('?') != -1 || servletUrl.toString().indexOf('=') != -1) {
			throw new IllegalArgumentException("Se han encontrado parametros en la URL del servlet"); //$NON-NLS-1$
		}
		return servletUrl;
	}

	protected static String getKeyStoreName(final Map<String, String> params) {

		// Si se ha especificado un almacen, se usara ese
		String ksValue = null;
		if (params.get(KEYSTORE_OLD_PARAM) != null) {
			ksValue = params.get(KEYSTORE_OLD_PARAM);
		}
		else if (params.get(KEYSTORE_PARAM) != null) {
			try {
				ksValue = new String(Base64.decode(params.get(KEYSTORE_PARAM)));
			}
			catch (final Exception e) {
				// Interpretamos que no era Base64 y no se ha pasado un almacen valido
			}
		}

		if (ksValue != null) {
			final int separatorPos = ksValue.indexOf(':');
			if (separatorPos == -1) {
				return ksValue;
			}
			if (ksValue.length() > 1) {
				return ksValue.substring(0, separatorPos).trim();
			}
			LOGGER.info(
				"El almacen indicado no es valido ('" + ksValue + "'), se usara el por defecto del sistema operativo" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		return null;
	}


	protected static String getDefaultKeyStoreLib(final Map<String, String> params) {

		// Si se ha especificado un almacen, se usara ese
		String ksValue = null;
		if (params.get(KEYSTORE_OLD_PARAM) != null) {
			ksValue = params.get(KEYSTORE_OLD_PARAM);
		}
		else if (params.get(KEYSTORE_PARAM) != null) {
			try {
				ksValue = new String(Base64.decode(params.get(KEYSTORE_PARAM)));
			}
			catch (final Exception e) {
				// Interpretamos que no era Base64 y no se ha pasado un almacen valido
			}
		}

		if (ksValue == null) {
			return null;
		}

		final int separatorPos = ksValue.indexOf(':');
		if (separatorPos != -1 && separatorPos < ksValue.length() - 1) {
			return cleanupPath(ksValue.substring(separatorPos + 1));
		}
		return null;
	}

	/**
	 * Sanea una ruta de archivo.
	 * @param path Ruta de archivo.
	 * @return Ruta de archivo limpia.
	 */
	private static String cleanupPath(final String path) {
		String cleanedpath = path.trim().replace("\"", "").replace("'", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		try {
			cleanedpath = new File(cleanedpath).getCanonicalPath();
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "Ruta de fichero no valida", e); //$NON-NLS-1$
			cleanedpath = null;
		}
		return cleanedpath;
	}

	protected static class LocalAccessRequestException extends Exception {
		private static final long serialVersionUID = -2823729189870521694L;

		protected LocalAccessRequestException(final String msg) {
			super(msg);
		}
	}
}
