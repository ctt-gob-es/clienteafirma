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
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import es.gob.afirma.core.misc.AOUtil;

/** Par&aacute;metros de la URL de llamada a la aplicaci&oacute;n
 * para la operaci&oacute;n de selecci&oacute;n de certificados. */
public final class UrlParametersToSelectCert extends UrlParameters {

	/** N&uacute;mero m&aacute;ximo de caracteres permitidos para el identificador de sesi&oacute;n de la firma. */
	private static final int MAX_ID_LENGTH = 20;

	/** Par&aacute;metro de entrada con el identificador del documento. */
	private static final String ID_PARAM = "id"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la m&iacute;nima versi&oacute;n requerida del aplicativo a usar en la invocaci&oacute;n por protocolo. */
	private static final String VER_PARAM = "ver"; //$NON-NLS-1$
	
	/** Par&aacute;metro que identifica el almac&acute;n de claves por defecto. */
	private static final String KEYSTORE_PARAM ="keystore"; //$NON-NLS-1$
	
	/** Par&aacute;metro de entrada que nos dice si tenemos que usar un provatekeyentry fijado o fijar uno nuevo. */
	private static final String STICKY_PARAM = "sticky"; //$NON-NLS-1$

	private String minimumVerstion;
	
	/**
	 * Opci&oacute;n de configuraci&oacute;n que determina si se debe mantener
	 * el primer certificado seleccionado para todas las operaciones. 
	 */
	private Boolean sticky;

	/** Obtiene la versi&oacute;n m&iacute;nima requerida del aplicativo.
	 * @return Versi&oacute;n m&iacute;nima requerida del aplicativo. */
	public String getMinimumVersion() {
		return this.minimumVerstion;
	}

	UrlParametersToSelectCert() {
		setData(null);
		setFileId(null);
		setRetrieveServletUrl(null);
	}

	void setMinimumVersion(final String minVer) {
		this.minimumVerstion = minVer;
	}
	
	/**
	 * Obtiene la opci&oacute;n de configuraci&oacute;n sticky
	 * 
	 * @return Opci&oacute;n de configuraci&oacute;n que determina si se debe
	 *         mantener el primer certificado seleccionado ({@code true}) o se
	 *         debe pedir siempre que el usuario elija uno ({@code false})
	 */
	public Boolean getSticky() {
		return sticky;
	}

	/**
	 * Establece la opci&oacute;n de configuraci&oacute;n sticky
	 * @param sticky Opci&oacute;n de configuraci&oacute;n que determina si se debe
	 *         mantener el primer certificado seleccionado ({@code true}) o se
	 *         debe pedir siempre que el usuario elija uno ({@code false})
	 */
	public void setSticky(final Boolean sticky) {
		this.sticky = sticky;
	}

	void setSelectCertParameters(final Map<String, String> params) throws ParameterException {

		// Comprobamos que el identificador de sesion de la firma no sea mayor de un cierto numero de caracteres
		String sessionId = null;
		if (params.containsKey(ID_PARAM)) {
			sessionId = params.get(ID_PARAM);
		}
		else if (params.containsKey(FILE_ID_PARAM)) {
			 sessionId = params.get(FILE_ID_PARAM);
		}

		if (sessionId != null) {
			if (sessionId.length() > MAX_ID_LENGTH) {
				throw new ParameterException("La longitud del identificador para la firma es mayor de " + MAX_ID_LENGTH + " caracteres."); //$NON-NLS-1$ //$NON-NLS-2$
			}

			// Comprobamos que el identificador de sesion de la firma sea alfanumerico (se usara como nombre de fichero)
			for (final char c : sessionId.toLowerCase(Locale.ENGLISH).toCharArray()) {
				if ((c < 'a' || c > 'z') && (c < '0' || c > '9')) {
					throw new ParameterException("El identificador de la sesion debe ser alfanumerico."); //$NON-NLS-1$
				}
			}

			setSessionId(sessionId);
		}

		// Version minima requerida del protocolo que se debe soportar
		if (params.containsKey(VER_PARAM)) {
			setMinimumVersion(params.get(VER_PARAM));
		}
		else {
			setMinimumVersion(Integer.toString(ProtocolVersion.VERSION_0.getVersion()));
		}

		// Si hemos recibido el identificador para la descarga de la configuracion,
		// no encontraremos el resto de parametros
		if (getFileId() != null) {
			return;
		}

		// Comprobamos la validez de la URL del servlet de guardado en caso de indicarse
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

		String props = null;
		if (params.containsKey(PROPERTIES_PARAM)) {
			props = params.get(PROPERTIES_PARAM);
		}

		if (props != null) {
			try {
				setExtraParams(AOUtil.base642Properties(props));
			}
			catch (final Exception e) {
				setExtraParams(new Properties());
			}
		}
		else {
			setExtraParams(new Properties());
		}
		
		// Valor de parametro sticky
		if (params.containsKey(STICKY_PARAM)) {
			setSticky(new Boolean(params.get(STICKY_PARAM)));
		} else {
			setSticky(Boolean.FALSE);
		}

		setDefaultKeyStore(getDefaultKeyStoreName(params));
		setDefaultKeyStoreLib(getDefaultKeyStoreLib(params));
	}
}
