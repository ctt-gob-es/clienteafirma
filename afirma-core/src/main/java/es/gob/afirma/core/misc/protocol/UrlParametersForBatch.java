/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc.protocol;

import java.net.URL;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import es.gob.afirma.core.misc.AOUtil;

/** Par&aacute;metros para el proceso de lotes de firmas predefinidos en XML.
 * En este caso, los datos son el XML de definici&oacute;n de lote.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class UrlParametersForBatch extends UrlParameters {

	/** Par&aacute;metro de entrada con el identificador de sesi&oacute;n de la operaci&oacute;n. */
	private static final String ID_PARAM = "id"; //$NON-NLS-1$

	private static final String PARAM_BATCH_POSTSIGNER = "batchpostsignerurl"; //$NON-NLS-1$
	private static final String PARAM_BATCH_PRESIGNER = "batchpresignerurl"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la m&iacute;nima versi&oacute;n requerida del aplicativo a usar en la invocaci&oacute;n por protocolo. */
	private static final String PARAM_VER = "ver"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que nos dice si tenemos que usar una clave prefijada o establecer una nueva. */
	private static final String PARAM_STICKY = "sticky"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que nos dice si tenemos que ignorar
	 * cla <code>PrivateKeyEntry</code> fijada. */
	private static final String RESET_STICKY_PARAM = "resetsticky"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que nos indica que se quiere tambien obtener el certificado utilizado. */
	private static final String PARAM_NEED_CERT = "needcert"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que nos indica si es una peticion de tipo JSON. */
	private static final String PARAM_JSON_BATCH = "jsonbatch"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que nos indica si es una firma por lotes monofasica. */
	private static final String PARAM_LOCAL_BATCH_PROCESS = "localBatchProcess"; //$NON-NLS-1$

	private String batchPreSignerUrl = null;
	private String batchPostSignerUrl = null;

	/** Indica si la operaci&oacute;n requiere de los servicios de comunicaci&oacute;n. */
	private final boolean servicesRequired;

	/**
	 * Versi&oacute;n m&iacute;nima del protocolo que define los requisitos
	 * de esta operaci&oacute;n.
	 */
	private String minimumProtocolVersion;

	/** Opci&oacute;n de configuraci&oacute;n que determina si se debe mantener
	 * el primer certificado seleccionado para todas las operaciones. */
	private boolean sticky;

	/** Opci&oacute;n de configuraci&oacute;n que determina si se debe ignorar
	 * cualquier certificado prefijado. */
	private boolean resetSticky;

	/** Opci&oacute;n de configuraci&oacute;n que determina si se debe devolver
	 * el certificado utilizado para firmar o no. */
	private boolean certNeeded;

	/** Indica si la peticion de firma por lotes es con JSON o XML */
	private boolean jsonBatch;

	/** Indica si la peticion de firma por lotes es monof&aacute;sica o trif&aacute;sica */
	private boolean localBatchProcess;
	
	/** Nombre de aplicaci&oacute;n o dominio desde que se realiza la llamada. */
	private String appName;

	/**
	 * Crea el conjunto de par&aacute;metros necesario para el uso de la operaci&acute;n
	 * de firma de lotes.
	 */
	public UrlParametersForBatch() {
		this(false);
	}

	/**
	 * Crea el conjunto de par&aacute;metros necesario para el uso de la operaci&acute;n
	 * de firma de lotes indicando si es necesario proporcionar la URL del servicio para
	 * el guardado del resultado.
	 * @param servicesRequired Indica si es necesario proporcionar la URL del servicio de
	 * guardado.
	 */
	public UrlParametersForBatch(final boolean servicesRequired) {
		this.servicesRequired = servicesRequired;
	}

	public boolean isJsonBatch() {
		return this.jsonBatch;
	}

	public void setJsonBatch(final boolean jsonBatch) {
		this.jsonBatch = jsonBatch;
	}

	public boolean isLocalBatchProcess() {
		return this.localBatchProcess;
	}

	public void setLocalBatchProcess(final boolean localBatchProcess) {
		this.localBatchProcess = localBatchProcess;
	}

	/** Obtiene la URL del servicio de preprocesado de lotes de firma.
	 * @return URL del servicio de preprocesado de lotes de firma. */
	public String getBatchPresignerUrl() {
		return this.batchPreSignerUrl;
	}

	void setBatchPresignerUrl(final String url) {
		this.batchPreSignerUrl = url;
	}

	/** Establece la opci&oacute;n de configuraci&oacute;n sticky
	 * @param sticky Opci&oacute;n de configuraci&oacute;n que determina si se debe
	 *         mantener el primer certificado seleccionado ({@code true}) o se
	 *         debe pedir siempre que el usuario elija uno ({@code false}). */
	public void setSticky(final boolean sticky) {
		this.sticky = sticky;
	}

	/** Obtiene la opci&oacute;n de configuraci&oacute;n sticky
	 * @return Opci&oacute;n de configuraci&oacute;n que determina si se debe
	 *         mantener el primer certificado seleccionado ({@code true}) o se
	 *         debe pedir siempre que el usuario elija uno ({@code false}). */
	public boolean getSticky() {
		return this.sticky;
	}

	/** Establece la opci&oacute;n de configuraci&oacute;n <i>resetsticky</i>.
	 * @param resetSticky Opci&oacute;n de configuraci&oacute;n que determina si se debe
	 *         ignorar el certificado mantener el primer certificado seleccionado ({@code true})
	 *         o si se puede utilizar en caso de que se solicite ({@code false}). */
	public void setResetSticky(final boolean resetSticky) {
		this.resetSticky = resetSticky;
	}

	/** Obtiene la opci&oacute;n de configuraci&oacute;n <i>resetsticky</i>.
	 * @return Opci&oacute;n de configuraci&oacute;n que determina si se debe
	 *         ignorar el cualquier certificado seleccionado ({@code true}) o si
	 *         deber&iacute;a usarse este si as&iacute; se indica ({@code false}). */
	public boolean getResetSticky() {
		return this.resetSticky;
	}

	/** Obtiene la opci&oacute;n de configuraci&oacute;n needcert
	 * @return Opci&oacute;n de configuraci&oacute;n que determina si se debe
	 *         devolver el certificado utilizado para firmar el lote ({@code true}) o
	 *         no ({@code false}). */
	public boolean isCertNeeded() {
		return this.certNeeded;
	}

	/** Establece la opci&oacute;n de configuraci&oacute;n needcert
	 * @param certNeeded Opci&oacute;n de configuraci&oacute;n que determina si se debe
	 *         devolver el certificado utilizado para firmar el lote ({@code true}) o
	 *         no ({@code false}). */
	public void setCertNeeded(final boolean certNeeded) {
		this.certNeeded = certNeeded;
	}

	/** Obtiene la URL del servicio de preprocesado de lotes de firma.
	 * @return URL del servicio de preprocesado de lotes de firma. */
	public String getBatchPostSignerUrl() {
		return this.batchPostSignerUrl;
	}

	void setBatchPostsignerUrl(final String url) {
		this.batchPostSignerUrl = url;
	}
	
	public String getAppName() {
		return this.appName;
	}

	void setAppName(final String appName) {
		this.appName = appName;
	}

	public void setBatchParameters(final Map<String, String> params) throws ParameterException {

		// idSession para el service Web. Con socket no se usa
		if (params.containsKey(ID_PARAM) || params.containsKey(FILE_ID_PARAM)) {
			// Comprobamos que el identificador de sesion de la firma no sea mayor de un cierto numero de caracteres
			final String sessionId = params.containsKey(ID_PARAM) ? params.get(ID_PARAM) : params.get(FILE_ID_PARAM);
			if (sessionId.length() > MAX_ID_LENGTH) {
				throw new ParameterException("La longitud del identificador de la operacion es mayor de " + MAX_ID_LENGTH + " caracteres."); //$NON-NLS-1$ //$NON-NLS-2$
			}

			// Comprobamos que el identificador de sesion de la firma sea alfanumerico (se usara como nombre de fichero)
			for (final char c : sessionId.toLowerCase(Locale.ENGLISH).toCharArray()) {
				if ((c < 'a' || c > 'z') && (c < '0' || c > '9')) {
					throw new ParameterException("El identificador de la firma debe ser alfanumerico."); //$NON-NLS-1$
				}
			}

			setSessionId(sessionId);
		}

		// Version minima requerida del protocolo que se debe soportar
		if (params.containsKey(PARAM_VER)) {
			setMinimumProtocolVersion(params.get(PARAM_VER));
		}
		else {
			setMinimumProtocolVersion(Integer.toString(ProtocolVersion.VERSION_0.getVersion()));
		}
		
		if (params.containsKey(APP_NAME_PARAM)) {
			this.appName = params.get(APP_NAME_PARAM);
		}

		// Si hemos recibido el identificador para la descarga de la configuracion,
		// no encontraremos el resto de parametros
		if (getFileId() != null) {
			return;
		}

		// Valor del parametro localBatchProcess
		if (params.containsKey(PARAM_LOCAL_BATCH_PROCESS)) {
			setLocalBatchProcess(Boolean.parseBoolean(params.get(PARAM_LOCAL_BATCH_PROCESS)));
		}

		if (!isLocalBatchProcess()) {

			if (!params.containsKey(PARAM_BATCH_POSTSIGNER)) {
				throw new ParameterException(
					"No se ha recibido la URL del postprocesador de lotes" //$NON-NLS-1$
				);
			}
			if (!params.containsKey(PARAM_BATCH_PRESIGNER)) {
				throw new ParameterException(
					"No se ha recibido la URL del preprocesador de lotes" //$NON-NLS-1$
				);
			}

			setBatchPostsignerUrl(
				validateURL(
					params.get(PARAM_BATCH_POSTSIGNER)
				).toString()
			);

			setBatchPresignerUrl(
				validateURL(
					params.get(PARAM_BATCH_PRESIGNER)
				).toString()
			);
		}

		// Validamos la URL del servlet de guardado en caso de ser necesaria
		if (this.servicesRequired) {
			if (params.containsKey(STORAGE_SERVLET_PARAM)) {

				// Comprobamos que la URL sea valida
				URL storageServletUrl;
				try {
					storageServletUrl = validateURL(params.get(STORAGE_SERVLET_PARAM));
				}
				catch (final ParameterLocalAccessRequestedException e) {
					throw new ParameterLocalAccessRequestedException("La URL del servicio de guardado no puede ser local", e); //$NON-NLS-1$
				}
				catch (final ParameterException e) {
					throw new ParameterException("Error al validar la URL del servicio de guardado: " + e, e); //$NON-NLS-1$
				}
				setStorageServletUrl(storageServletUrl);
			}
			// Si no se encuentra a pesar de tener todos los parametros, falla la operacion
			else if (params.containsKey(ID_PARAM)) {
				throw new ParameterException("No se ha recibido la direccion del servlet para el guardado del resultado de la operacion"); //$NON-NLS-1$
			}
		}

		String props = null;
		if (params.containsKey(PROPERTIES_PARAM)) {
			props = params.get(PROPERTIES_PARAM);
		}

		if (props != null && !props.isEmpty()) {
			try {
				setExtraParams(AOUtil.base642Properties(props));
			}
			catch (final Exception e) {
				LOGGER.severe(
					"Las propiedades adicionales indicadas en el parametro '" + PROPERTIES_PARAM + "' no se han podido cargar: " + e //$NON-NLS-1$ //$NON-NLS-2$
				);
				setExtraParams(new Properties());
			}
		}
		else {
			setExtraParams(new Properties());
		}

		// Valor de parametro sticky
		if (params.containsKey(PARAM_STICKY)) {
			setSticky(Boolean.parseBoolean(params.get(PARAM_STICKY)));
		}
		else {
			setSticky(false);
		}

		// Valor de parametro resetsticky
		if (params.containsKey(RESET_STICKY_PARAM)) {
			setResetSticky(Boolean.parseBoolean(params.get(RESET_STICKY_PARAM)));
		}
		else {
			setResetSticky(false);
		}

		// Valor del parametro needCert
		if (params.containsKey(PARAM_NEED_CERT)) {
			setCertNeeded(Boolean.parseBoolean(params.get(PARAM_NEED_CERT)));
		}
		else {
			setCertNeeded(false);
		}

		// Valor del parametro jsonBatch
		if (params.containsKey(PARAM_JSON_BATCH)) {
			setJsonBatch(Boolean.parseBoolean(params.get(PARAM_JSON_BATCH)));
		}

		setDefaultKeyStore(getKeyStoreName(params));
		setDefaultKeyStoreLib(getDefaultKeyStoreLib(params));
	}

	/** Obtiene la versi&oacute;n m&iacute;nima requerida del aplicativo.
	 * @return Versi&oacute;n m&iacute;nima requerida del aplicativo. */
	public String getMinimumProtocolVersion() {
		return this.minimumProtocolVersion;
	}

	void setMinimumProtocolVersion(final String minimumProtocolVersion) {
		this.minimumProtocolVersion = minimumProtocolVersion;
	}

}
