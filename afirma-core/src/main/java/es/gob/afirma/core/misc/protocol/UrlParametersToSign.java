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
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.ExtraParamsProcessor;
import es.gob.afirma.core.signers.ExtraParamsProcessor.IncompatiblePolicyException;

/** Par&aacute;metros de la URL de llamada a la aplicaci&oacute;n. */
public final class UrlParametersToSign extends UrlParameters {

	/** N&uacute;mero m&aacute;ximo de caracteres permitidos para el identificador
	 * de sesi&oacute;n de la firma. */
	private static final int MAX_ID_LENGTH = 20;

	/** Par&aacute;metro de entrada con el formato de firma. */
	private static final String FORMAT_PARAM = "format"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el algoritmo de firma. */
	private static final String ALGORITHM_PARAM = "algorithm"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el identificador del documento. */
	private static final String ID_PARAM = "id"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la m&iacute;nima versi&oacute;n requerida del
	 * aplicativo a usar en la invocaci&oacute;n por protocolo. */
	private static final String VER_PARAM = "ver"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada que nos dice si tenemos que usar un
	 * <code>PrivateKeyEntry</code> fijado o fijar uno nuevo. */
	private static final String STICKY_PARAM = "sticky"; //$NON-NLS-1$

	/** Tipo de operaci&oacute;n de firma. */
	public enum Operation {

		/** Operaci&oacute;n de firma. */
		SIGN,

		/** Operaci&oacute;n de cofirma. */
		COSIGN,

		/** Operaci&oacute;n de contrafirma. */
		COUNTERSIGN;

		/** Obtiene el tipo de operaci&oacute;n de firma a partir de su nombre, o <code>null</code>
		 * si el nombre no corresponde a ninguna operaci&oacute;n conocida.
		 * @param opName Nombre de la operaci&oacute;n de firma.
		 * @return Operaci&oacute;n de firma. */
		public static Operation getOperation(final String opName) {
			if ("SIGN".equalsIgnoreCase(opName)) { //$NON-NLS-1$
				return SIGN;
			}
			if ("COSIGN".equalsIgnoreCase(opName)) { //$NON-NLS-1$
				return COSIGN;
			}
			if ("COUNTERSIGN".equalsIgnoreCase(opName)) { //$NON-NLS-1$
				return COUNTERSIGN;
			}
			return null;
		}
	}

	/** Algoritmos de firma soportados. */
	private static final Set<String> SUPPORTED_SIGNATURE_ALGORITHMS = new HashSet<>();
	static {
		SUPPORTED_SIGNATURE_ALGORITHMS.add("SHA1withRSA"); //$NON-NLS-1$
		SUPPORTED_SIGNATURE_ALGORITHMS.add("SHA256withRSA"); //$NON-NLS-1$
		SUPPORTED_SIGNATURE_ALGORITHMS.add("SHA384withRSA"); //$NON-NLS-1$
		SUPPORTED_SIGNATURE_ALGORITHMS.add("SHA512withRSA"); //$NON-NLS-1$
	}

	private Operation operation;
	private String signFormat;
	private String signAlgorithm;
	private String minimumVersion;

	/** Opci&oacute;n de configuraci&oacute;n que determina si se debe mantener
	 * el primer certificado seleccionado para todas las operaciones. */
	private boolean sticky;

	/** Obtiene la versi&oacute;n m&iacute;nima requerida del aplicativo.
	 * @return Versi&oacute;n m&iacute;nima requerida del aplicativo. */
	public String getMinimumVersion() {
		return this.minimumVersion;
	}

	/** Obtiene el tipo de operaci&oacute;n a realizar (firma, cofirma o contrafirma).
	 * @return Operaci&oacute;n. */
	public Operation getOperation() {
		return this.operation;
	}

	/** Obtiene el formato de firma.
	 * @return Formato de firma */
	public String getSignatureFormat() {
		return this.signFormat;
	}

	/** Obtiene el algoritmo de firma.
	 * @return Algoritmo de firma */
	public String getSignatureAlgorithm() {
		return this.signAlgorithm;
	}

	UrlParametersToSign() {
		setData(null);
		setFileId(null);
		setRetrieveServletUrl(null);
	}

	void setOperation(final Operation operation) {
		this.operation = operation;
	}

	/** Establece el nombre del formato de firma que se debe utilizar.
	 * @param format Formato de firma. */
	public void setSignFormat(final String format) {
		this.signFormat = format;
	}

	void setSignAlgorithm(final String algo) {
		this.signAlgorithm = algo;
	}

	void setMinimumVersion(final String minVer) {
		this.minimumVersion = minVer;
	}

	/** Obtiene la opci&oacute;n de configuraci&oacute;n <i>sticky</i>.
	 * @return Opci&oacute;n de configuraci&oacute;n que determina si se debe
	 *         mantener el primer certificado seleccionado ({@code true}) o se
	 *         debe pedir siempre que el usuario elija uno ({@code false}). */
	public boolean getSticky() {
		return this.sticky;
	}

	/** Establece la opci&oacute;n de configuraci&oacute;n <i>sticky</i>.
	 * @param sticky Opci&oacute;n de configuraci&oacute;n que determina si se debe
	 *         mantener el primer certificado seleccionado ({@code true}) o se
	 *         debe pedir siempre que el usuario elija uno ({@code false}). */
	public void setSticky(final boolean sticky) {
		this.sticky = sticky;
	}

	void setSignParameters(final Map<String, String> params) throws ParameterException {

		// Comprobamos que el identificador de sesion de la firma no sea mayor de un cierto numero de caracteres
		String signatureSessionId = null;
		if (params.containsKey(ID_PARAM)) {
			signatureSessionId = params.get(ID_PARAM);
		}
		else if (params.containsKey(FILE_ID_PARAM)) {
			 signatureSessionId = params.get(FILE_ID_PARAM);
		}

		if (signatureSessionId != null) {
			if (signatureSessionId.length() > MAX_ID_LENGTH) {
				throw new ParameterException(
					"La longitud del identificador para la firma es mayor de " + MAX_ID_LENGTH + " caracteres." //$NON-NLS-1$ //$NON-NLS-2$
				);
			}

			// Comprobamos que el identificador de sesion de la firma sea alfanumerico (se usara como nombre de fichero)
			for (final char c : signatureSessionId.toLowerCase(Locale.ENGLISH).toCharArray()) {
				if ((c < 'a' || c > 'z') && (c < '0' || c > '9')) {
					throw new ParameterException("El identificador de la firma debe ser alfanumerico."); //$NON-NLS-1$
				}
			}

			setSessionId(signatureSessionId);
		}

		// Version minima requerida del protocolo que se debe soportar
		if (params.containsKey(VER_PARAM)) {
			setMinimumVersion(params.get(VER_PARAM));
		}
		else {
			setMinimumVersion(Integer.toString(ProtocolVersion.VERSION_0.getVersion()));
		}

		// Tomamos el tipo de operacion
		final Operation op = Operation.getOperation(
			params.get(
				ProtocolConstants.OPERATION_PARAM
			)
		);
		if (op != null) {
			setOperation(op);
		}
		else {
			throw new ParameterException("Se ha indicado un codigo de operacion incorrecto"); //$NON-NLS-1$
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

		// Comprobamos que se ha especificado el formato
		if (!params.containsKey(FORMAT_PARAM)) {
			throw new ParameterException("No se ha recibido el formato de firma"); //$NON-NLS-1$
		}

		final String format = params.get(FORMAT_PARAM);
		setSignFormat(format);

		// Comprobamos que se ha especificado el algoritmo
		if (!params.containsKey(ALGORITHM_PARAM)) {
			throw new ParameterException("No se ha recibido el algoritmo de firma"); //$NON-NLS-1$
		}
		final String algo = params.get(ALGORITHM_PARAM);
		if (!SUPPORTED_SIGNATURE_ALGORITHMS.contains(algo)) {
			throw new ParameterException("Algoritmo de firma no soportado: " + algo); //$NON-NLS-1$
		}

		setSignAlgorithm(algo);

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
			setSticky(Boolean.parseBoolean(params.get(STICKY_PARAM)));
		}
		else {
			setSticky(false);
		}

		setDefaultKeyStore(UrlParameters.getDefaultKeyStoreName(params));
		setDefaultKeyStoreLib(UrlParameters.getDefaultKeyStoreLib(params));

	}

	/** Expande los extraParams configurados en la URL que lo permitran. Por ejemplo,
	 * la politica de firma establecida mediante "expPolicy" se expandir&aacute; a los
	 * valores correspondientes de la pol&iacute;tica.
	 * @throws IncompatiblePolicyException Cuando se hayan proporcionado par&aacute;metros
	 * incompatibles con la pol&iacute;tica de firma configurada. */
	public void expandExtraParams() throws IncompatiblePolicyException {
		setExtraParams(
				ExtraParamsProcessor.expandProperties(
				getExtraParams(),
				getData(),
				getSignatureFormat()
			)
		);
	}
}
