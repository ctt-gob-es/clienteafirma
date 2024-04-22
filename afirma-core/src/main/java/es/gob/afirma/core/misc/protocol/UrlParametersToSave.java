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


/** Par&aacute;metros para el guardado de datos. */
public final class UrlParametersToSave extends UrlParameters {

	/** Par&aacute;metro de entrada con el t&iacute;tulo de la actividad o del di&aacute;logo de guardado. */
	private static final String TITLE_PARAM = "title"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la descripci&oacute;n del tipo de fichero de salida. */
	private static final String FILETYPE_DESCRIPTION_PARAM = "desc"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el nombre propuesto para un fichero. */
	private static final String FILENAME_PARAM = "filename"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con las extensiones recomendadas para el fichero de salida. */
	private static final String FILENAME_EXTS_PARAM = "exts"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con el identificador del documento. */
	private static final String ID_PARAM = "id"; //$NON-NLS-1$

	/** Par&aacute;metro de entrada con la m&iacute;nima versi&oacute;n requerida del aplicativo a usar en la invocaci&oacute;n por protocolo. */
	private static final String VER_PARAM = "ver"; //$NON-NLS-1$

	private String title = null;
	private String filename = null;
	private String extensions = null;
	private String fileTypeDescription = null;

	/** Indica si la operaci&oacute;n requiere de los servicios de comunicaci&oacute;n. */
	private final boolean servicesRequired;

	/**
	 * Versi&oacute;n m&iacute;nima del protocolo que define los requisitos
	 * de esta operaci&oacute;n.
	 */
	private String minimumProtocolVersion = null;

	/**
	 * Crea el conjunto de par&aacute;metros necesario para el uso de la operaci&acute;n
	 * de guardado de datos.
	 */
	public UrlParametersToSave() {
		this(false);
	}

	/**
	 * Crea el conjunto de par&aacute;metros necesario para el uso de la operaci&acute;n
	 * de guardado de datos en disco indicando si es necesario proporcionar la URL del
	 * servicio para transmitir el resultado.
	 * @param servicesRequired Indica si es necesario proporcionar la URL del servicio de
	 * guardado.
	 */
	public UrlParametersToSave(final boolean servicesRequired) {
		this.servicesRequired = servicesRequired;
	}

	/** Establece la descripci&oacute;n del tipo de fichero a guardar.
	 * @param desc Descripci&oacute;n del tipo de fichero a guardar */
	void setFileTypeDescription(final String desc) {
		this.fileTypeDescription = desc;
	}

	/** Establece las extensiones recomendadas para el fichero a guardar.
	 * Deben indicarse como una lista separada por comas
	 * @param exts Extensiones recomendadas, indicadas como una lista separada por comas */
	void setExtensions(final String exts) {
		this.extensions = exts;
	}

	/** Establece el nombre de fichero propuesto para guardar los datos.
	 * @param filename Nombre de fichero propuesto para guardar los datos */
	@Override
	void setFilename(final String filename) {
		this.filename = filename;
	}

	/** Establece el t&iacute;tulo del di&aacute;logo de guardado de datos.
	 * @param title T&iacute;tulo del di&aacute;logo de guardado de datos */
	void setTitle(final String title) {
		this.title = title;
	}

	/** Establece la versi&oacute;n m&iacute;nima exigida del protocolo de comunicaci&oacute;n.
	 * @param minVer Versi&oacute;n m&iacute;nima del protocolo. */
	void setMinimumProtocolVersion(final String minVer) {
		this.minimumProtocolVersion = minVer;
	}

	/** Obtiene la descripci&oacute;n del tipo de fichero a guardar.
	 * @return Descripci&oacute;n del tipo de fichero a guardar */
	public String getFileTypeDescription() {
		return this.fileTypeDescription;
	}

	/** Obtiene, como una lista separada por comas, las extensiones recomendadas para el
	 * fichero de salida.
	 * @return Lista separada por comas con las extensiones para el fichero de salida */
	public String getExtensions() {
		return this.extensions;
	}

	/** Obtiene el nombre de fichero propuesto para guardar los datos.
	 * @return Nombre de fichero propuesto para guardar los datos */
	@Override
	public String getFileName() {
		return this.filename;
	}

	/** Obtiene el t&iacute;tulo del di&aacute;logo de guardado de datos.
	 * @return T&iacute;tulo del di&aacute;logo de guardado de datos */
	public String getTitle() {
		return this.title;
	}

	/** Obtiene la versi&oacute;n m&iacute;nima requerida del aplicativo.
	 * @return Versi&oacute;n m&iacute;nima requerida del aplicativo. */
	public String getMinimumProtocolVersion() {
		return this.minimumProtocolVersion;
	}

	public void setSaveParameters(final Map<String, String> params) throws ParameterException {

		// Comprobamos que se nos hayan indicado los datos o, en su defecto, el
		// identificador de fichero remoto
		// para descargar los datos y la ruta del servicio remoto para el
		// fichero
		if (!params.containsKey(FILE_ID_PARAM) && !params.containsKey(DATA_PARAM)) {
			throw new ParameterException("No se ha proporcionado el identificador de fichero ni los datos a guardar"); //$NON-NLS-1$
		}

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
					throw new ParameterException("El identificador de la firma debe ser alfanumerico."); //$NON-NLS-1$
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

		// Cargamos la URL del servlet si es necesaria
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

		setTitle(verifyTitle(params));
		setFilename(verifyFilename(params));
		setExtensions(verifyExtensions(params));
		setFileTypeDescription(verifyFileTypeDescription(params));
	}

	private static String verifyFilename(final Map<String, String> params) throws ParameterException {
		String filename = null;
		if (params.containsKey(FILENAME_PARAM)) {
			filename = params.get(FILENAME_PARAM);
			// Determinamos si el nombre tiene algun caracter que no consideremos valido para un nombre de fichero
			for (final char invalidChar : "\\/:*?\"<>|".toCharArray()) { //$NON-NLS-1$
				if (filename.indexOf(invalidChar) != -1) {
					throw new ParameterException("Se ha indicado un nombre de fichero con el caracter invalido: " + invalidChar); //$NON-NLS-1$
				}
			}
		}
		return filename;
	}

	private static String verifyExtensions(final Map<String, String> params) throws ParameterException {
		String extensions = null;
		if (params.containsKey(FILENAME_EXTS_PARAM)) {
			extensions = params.get(FILENAME_EXTS_PARAM);
			// Determinamos si el nombre tiene algun caracter que no consideremos valido para un nombre de fichero
			for (final char invalidChar : "\\/:*?\"<>|; ".toCharArray()) { //$NON-NLS-1$
				if (extensions.indexOf(invalidChar) != -1) {
					throw new ParameterException("Se ha indicado una lista de extensiones de nombre de fichero con caracteres invalidos: " + invalidChar); //$NON-NLS-1$
				}
			}
		}
		return extensions;
	}

	private static String verifyTitle(final Map<String, String> params) {
		if (params.containsKey(TITLE_PARAM)) {
			return params.get(TITLE_PARAM);
		}
		return ProtocoloMessages.getString("ProtocolInvocationUriParser.1"); //$NON-NLS-1$
	}

	private static String verifyFileTypeDescription(final Map<String, String> params) {
		String desc = null;
		if (params.containsKey(FILETYPE_DESCRIPTION_PARAM)) {
			desc = params.get(FILETYPE_DESCRIPTION_PARAM);
			// Anadimos las extensiones si fuese preciso
			if (params.containsKey(FILENAME_EXTS_PARAM) && !desc.endsWith(")")) { //$NON-NLS-1$
				final StringBuilder sb = new StringBuilder(desc).append(" ("); //$NON-NLS-1$
				for (final String ext : params.get(FILENAME_EXTS_PARAM).split(",")) { //$NON-NLS-1$
					sb.append("*."); //$NON-NLS-1$
					sb.append(ext);
				}
				sb.append(")"); //$NON-NLS-1$
				desc = sb.toString();
			}
		}
		return desc;
	}

}
