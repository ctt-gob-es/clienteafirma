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

/** Par&aacute;metros de la URL de llamada a la aplicaci&oacute;n
 * para la operaci&oacute;n de selecci&oacute;n de certificados. */
public final class UrlParametersToSelectCert extends UrlParameters {

	/** Par&aacute;metro de entrada con el identificador del documento. */
	private static final String ID_PARAM = "id"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la m&iacute;nima versi&oacute;n requerida del aplicativo a usar en la invocaci&oacute;n por protocolo. */
	private static final String VER_PARAM = "ver"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que nos dice si tenemos que usar un provatekeyentry fijado o fijar uno nuevo. */
	private static final String STICKY_PARAM = "sticky"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que nos dice si tenemos que ignorar
	 * cla <code>PrivateKeyEntry</code> fijada. */
	private static final String RESET_STICKY_PARAM = "resetsticky"; //$NON-NLS-1$

	/** Indica si la operaci&oacute;n requiere de los servicios de comunicaci&oacute;n. */
	private final boolean servicesRequired;

	/**
	 * Versi&oacute;n m&iacute;nima del protocolo que define los requisitos
	 * de esta operaci&oacute;n.
	 */
	private String minimumVerstion;

	/** Opci&oacute;n de configuraci&oacute;n que determina si se debe mantener
	 * el primer certificado seleccionado para todas las operaciones. */
	private boolean sticky;

	/** Opci&oacute;n de configuraci&oacute;n que determina si se debe ignorar
	 * cualquier certificado prefijado. */
	private boolean resetSticky;

	/**
	 * Construye el conjunto de par&aacute;metros vac&iacute;o.
	 */
	public UrlParametersToSelectCert() {
		this(false);
	}

	/**
	 * Construye el conjunto de par&aacute;metros necesario para el uso de la operaci&acute;n
	 * de selecci&oacute;n de certificado indicando si es necesario proporcionar la URL del
	 * servicio para transmitir el resultado.
	 * @param servicesRequired Indica si es necesario proporcionar la URL del servicio de
	 * guardado.
	 */
	public UrlParametersToSelectCert(final boolean servicesRequired) {
		this.servicesRequired = servicesRequired;
		setData(null);
		setFileId(null);
		setRetrieveServletUrl(null);
	}

	/** Obtiene la versi&oacute;n m&iacute;nima requerida del aplicativo.
	 * @return Versi&oacute;n m&iacute;nima requerida del aplicativo. */
	public String getMinimumProtocolVersion() {
		return this.minimumVerstion;
	}

	void setMinimumProtocolVersion(final String minVer) {
		this.minimumVerstion = minVer;
	}

	/**
	 * Establece la opci&oacute;n de configuraci&oacute;n sticky
	 * @param sticky Opci&oacute;n de configuraci&oacute;n que determina si se debe
	 *         mantener el primer certificado seleccionado ({@code true}) o se
	 *         debe pedir siempre que el usuario elija uno ({@code false})
	 */
	public void setSticky(final boolean sticky) {
		this.sticky = sticky;
	}

	/**
	 * Obtiene la opci&oacute;n de configuraci&oacute;n sticky
	 *
	 * @return Opci&oacute;n de configuraci&oacute;n que determina si se debe
	 *         mantener el primer certificado seleccionado ({@code true}) o se
	 *         debe pedir siempre que el usuario elija uno ({@code false})
	 */
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

	public void setSelectCertParameters(final Map<String, String> params) throws ParameterException {

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
				throw new ParameterException("La longitud del identificador de la operacion es mayor de " + MAX_ID_LENGTH + " caracteres."); //$NON-NLS-1$ //$NON-NLS-2$
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
			setMinimumProtocolVersion(params.get(VER_PARAM));
		}
		else {
			setMinimumProtocolVersion(Integer.toString(ProtocolVersion.VERSION_0.getVersion()));
		}

		// Si hemos recibido el identificador para la descarga de la configuracion,
		// no encontraremos el resto de parametros
		if (getFileId() != null) {
			return;
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
		if (params.containsKey(STICKY_PARAM)) {
			setSticky(Boolean.parseBoolean(params.get(STICKY_PARAM)));
		} else {
			setSticky(false);
		}

		// Valor de parametro resetsticky
		if (params.containsKey(RESET_STICKY_PARAM)) {
			setResetSticky(Boolean.parseBoolean(params.get(RESET_STICKY_PARAM)));
		}
		else {
			setResetSticky(false);
		}

		setDefaultKeyStore(getKeyStoreName(params));
		setDefaultKeyStoreLib(getDefaultKeyStoreLib(params));
	}
}
