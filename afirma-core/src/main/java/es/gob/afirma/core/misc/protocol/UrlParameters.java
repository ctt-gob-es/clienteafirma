/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.misc.protocol;

import java.net.URL;
import java.net.URLDecoder;
import java.util.Arrays;
import java.util.Map;
import java.util.Properties;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.http.DataDownloader;

/** Par&aacute;metros habitualmente comunes para todas las operaciones. */
public abstract class UrlParameters {

	/** Par&aacute;metro de entrada con las opciones de configuraci&oacute;n de la firma. */
	protected static final String PROPERTIES_PARAM = "properties"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con los datos a firmar. */
	protected static final String DATA_PARAM = "dat"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el servlet remoto de recuperaci&oacute;n de datos. */
	private static final String RETRIEVE_SERVLET_PARAM = "rtservlet"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el servlet remoto de guardado de datos. */
	protected static final String STORAGE_SERVLET_PARAM = "stservlet"; //$NON-NLS-1$

	/** Longitud permitida para la clave de cifrado. */
	private static final int CIPHER_KEY_LENGTH = 8;

	/** Par&aacute;metro de entrada con la clave para el cifrado del documento. */
	private static final String KEY_PARAM = "key"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el identificador del fichero remoto de datos. */
	protected static final String FILE_ID_PARAM = "fileid"; //$NON-NLS-1$

	/** Par&aacute;metro que identifica el <i>User Agent</i> del navegador Web usado. */
	private static final String KEYSTORE ="keystore"; //$NON-NLS-1$

	/** Codificaci&oacute;n por defecto. */
	private static final String DEFAULT_ENCODING = "utf-8"; //$NON-NLS-1$

	private byte[] data = null;
	// UAL - para firma de multiples documentos
	private byte [][] datas = null;
	private String [] operaciones;
	private boolean multifirma = false; 
	// fin UAL
	private String fileId = null;
	private byte[] desKey = null;
	private URL retrieveServletUrl = null;
	private URL storageServer = null;
	private String id = null;
	private String defaultKeyStore = null;
	private Properties extraParams = null;
	private String filename = null;

	/** Obtiene los par&aacute;metros adicionales de la firma.
	 * @return Par&aacute;metros adicionales de la firma */
	public Properties getExtraParams() {
		return this.extraParams != null ? this.extraParams : new Properties();
	}

	/**
	 * Establece los par&aacute;metros adicionales para la configuraci&oacute;n
	 * de la operaci&oacute;n de firma.
	 * @param properties Propiedades adicionales.
	 */
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

	/** Establece la clave DES de cifrado de los datos a subir al servidor intermedio.
	 * @param key Clave DES de cifrado de los datos a subir al servidor intermedio */
	private void setDesKey(final byte[] key) {
		this.desKey = key != null ? Arrays.copyOf(key, key.length) : null;
	}

	/** Establece la URL de subida al servidor intermedio.
	 * @param retrieveServletUrl URL de subida al servidor intermedio */
	void setRetrieveServletUrl(final URL retrieveServletUrl) {
		this.retrieveServletUrl = retrieveServletUrl;
	}

	/** Obtiene los datos.
	 * @return Datos. */
	public byte[] getData() {
		return this.data;
	}

	/** Obtiene el identificador de los datos en el servidor intermedio.
	 * @return Identificador de los datos en el servidor intermedio */
	public String getFileId() {
		return this.fileId;
	}

	/** Obtiene la clave DES de cifrado de los datos a subir al servidor intermedio.
	 * @return Clave DES de cifrado de los datos a subir al servidor intermedio */
	public byte[] getDesKey() {
		return this.desKey;
	}

	/** Obtiene la URL de subida al servidor intermedio.
	 * @return URL de subida al servidor intermedio */
	public URL getRetrieveServletUrl() {
		return this.retrieveServletUrl;
	}

	/** Obtiene la URL del servlet de almacenamiento temporal en servidor.
	 * @return URL del servlet de almacenamiento temporal en servidor */
	public URL getStorageServletUrl() {
		return this.storageServer;
	}

	protected void setStorageServletUrl(final URL url) {
		this.storageServer = url;
	}

	/** Obtiene el identificador de sesi&oacute;n.
	 * @return Identificador de sesi&oacute;n */
	public String getId() {
		return this.id;
	}

	void setSessionId(final String sessionId) {
		this.id = sessionId;
	}
	
	public byte[][] getDatas() {
		return datas;
	}

	public void setDatas(byte[][] datas) {
		this.datas = datas;
	}

	public String[] getOperaciones() {
		return operaciones;
	}

	public void setOperaciones(String[] operaciones) {
		this.operaciones = operaciones;
	}

	void setCommonParameters(final Map<String, String> params) throws ParameterException {

		setDesKey(verifyCipherKey(params));

		// Comprobamos que se nos hayan indicado los datos o, en su defecto, el
		// identificador de fichero remoto
		// para descargar los datos y la ruta del servicio remoto para el
		// fichero
		if (!params.containsKey(DATA_PARAM)) {

			if (params.containsKey(FILE_ID_PARAM)) {

				setFileId(params.get(FILE_ID_PARAM));

				if (!params.containsKey(RETRIEVE_SERVLET_PARAM)) {
					throw new ParameterException(
							"No se ha recibido la direccion del servlet para la recuperacion de los datos a firmar"); //$NON-NLS-1$
				}

				try {
					setRetrieveServletUrl(
						validateURL(
							params.get(RETRIEVE_SERVLET_PARAM)
						)
					);
				}
				catch (final ParameterLocalAccessRequestedException e) {
					throw new ParameterLocalAccessRequestedException(
						"La URL del servicio de recuperacion de datos no puede ser local", e //$NON-NLS-1$
					);
				}
				catch (final ParameterException e) {
					throw new ParameterException(
						"Error al validar la URL del servlet de recuperacion: " + e, e //$NON-NLS-1$
					);
				}
			}
		}
		else {
			final String dataPrm = params.get(DATA_PARAM);
			if (dataPrm.startsWith("file:/")) { //$NON-NLS-1$
				throw new ParameterException(
					"No se permite la lectura de ficheros locales: " + dataPrm //$NON-NLS-1$
				);
			}
			try {
				setData(
					DataDownloader.downloadData(dataPrm)
				);
				
				// UAL - para firma de multiples documentos
				String [] sDatosAfirmar = URLDecoder.decode(params.get(DATA_PARAM), DEFAULT_ENCODING).split(":");
				byte [][] bDatosAfirmar = new byte [sDatosAfirmar.length][];
				String [] operaciones =  new String [sDatosAfirmar.length];
				for (int i = 0; i < sDatosAfirmar.length; i++)
				{
					
					if (sDatosAfirmar[i].contains(";"))
					{
						this.multifirma = true;
						String [] sDatoIndividual = sDatosAfirmar[i].split(";");
						byte [] bDatoIndividual = DataDownloader.downloadData(sDatoIndividual[1]);
						operaciones[i] = sDatoIndividual[0];
						bDatosAfirmar[i] = new byte[bDatoIndividual.length];
						bDatosAfirmar[i] = bDatoIndividual;
					}
					else
					{
						String sDatoIndividual = sDatosAfirmar[i];
						byte [] bDatoIndividual = DataDownloader.downloadData(sDatoIndividual);
						operaciones[i] = params.get(ProtocolConstants.OPERATION_PARAM);
						bDatosAfirmar[i] = new byte[bDatoIndividual.length];
						bDatosAfirmar[i] = bDatoIndividual;
					}
				}
				setDatas(bDatosAfirmar);
				setOperaciones(operaciones);
				// fin UAL
			}
			catch (final Exception e) {
				throw new ParameterException(
					"No se han podido obtener los datos: " + e, e //$NON-NLS-1$
				);
			}
		}
	}

	/** Extrae y verifica la clave de cifrado de los par&aacute;metros de entrada.
	 *  @param params Par&aacute;metros extra&iacute;dos de la URI.
	 *  @return Clave de cifrado o null si no se declar&oacute; un valor en los par&aacute;metros.
	 *  @throws ParameterException Cuando la clave de cifrado es err&oacute;nea. */
	private static byte[] verifyCipherKey(final Map<String, String> params) throws ParameterException {

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
			throw new ParameterException("La longitud de la clave de cifrado no es correcta"); //$NON-NLS-1$
		}
		return key.getBytes();
	}

	/** Valida una URL para asegurar que cumple con los requisitos m&iacute;nimos de seguridad.
	 * @param url URL que se desea validar.
	 * @return URL formada y validada.
	 * @throws ParameterException Cuando ocurre alg&uacute;n problema al validar la URL. */
	protected static URL validateURL(final String url) throws ParameterException {

		// Comprobamos que la URL sea valida
		final URL servletUrl;
		try {
			servletUrl = new URL(URLDecoder.decode(url, DEFAULT_ENCODING));
		}
		catch (final Exception e) {
			throw new ParameterException(
				"La URL proporcionada para el servlet no es valida (" + url + "): " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		// Comprobamos que el protocolo este soportado
		if (!"http".equals(servletUrl.getProtocol()) && !"https".equals(servletUrl.getProtocol())) { //$NON-NLS-1$ //$NON-NLS-2$
			throw new ParameterException(
				"El protocolo de la URL proporcionada para el servlet no esta soportado: " + servletUrl.getProtocol() //$NON-NLS-1$
			);
		}
		// Comprobamos que la URL sea una llamada al servlet y que no sea local
		if ("localhost".equals(servletUrl.getHost()) || "127.0.0.1".equals(servletUrl.getHost())) { //$NON-NLS-1$ //$NON-NLS-2$
			throw new ParameterLocalAccessRequestedException(
				"El host de la URL proporcionada para el Servlet es local" //$NON-NLS-1$
			);
		}
		// El servlet no puede recibir parametros
		if (servletUrl.toString().indexOf('?') != -1 || servletUrl.toString().indexOf('=') != -1) {
			throw new ParameterException("Se han encontrado parametros en la URL del servlet"); //$NON-NLS-1$
		}
		return servletUrl;
	}

	protected static String verifyDefaultKeyStoreName(final Map<String, String> params) {

		// Si se ha especificado un almacen, se usara ese
		if (params.containsKey(KEYSTORE)) {
			return params.get(KEYSTORE);
		}

		// Si no se ha especificado almacen, se usara el del sistema operativo

		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return "WINDOWS"; //$NON-NLS-1$
		}
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			return "APPLE"; //$NON-NLS-1$
		}
		if (Platform.OS.LINUX.equals(Platform.getOS())) {
			return "SHARED_NSS"; //$NON-NLS-1$
		}
		if (Platform.OS.SOLARIS.equals(Platform.getOS())) {
			return "MOZ_UNI"; //$NON-NLS-1$
		}
		return null;
	}

}
