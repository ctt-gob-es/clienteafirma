/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc.http;

import java.io.IOException;

import es.gob.afirma.core.misc.LoggerUtil;

/** Error de conexi&oacute;n HTTP.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class HttpError extends IOException {

	private static final long serialVersionUID = 8997766820804553378L;

	/** C&oacute;digo del error. */
	private final int responseCode;

	/** Descripci&oacute;n del error. */
	private final String responseDescription;

	/** Flujo de octetos del error (del contenido del error). */
	private final byte[] errorStreamBytes;

	/** Crea una excepci&oacute;n de error de conexi&oacute;n HTTP.
	 * @param resCode C&oacute;digo HTTP de respuesta. */
	HttpError(final int resCode) {
		super("Error en conexion HTTP con codigo de respuesta " + resCode); //$NON-NLS-1$
		this.responseCode = resCode;
		this.responseDescription = null;
		this.errorStreamBytes = null;
	}

	/** Crea una excepci&oacute;n de error de conexi&oacute;n HTTP.
	 * @param resCode C&oacute;digo HTTP de respuesta.
	 * @param resDescription Descripci&oacute;n del error.
	 * @param url URL a la que se intent&oacute; conectar.  */
	public HttpError(final int resCode, final String resDescription, final String url) {
		this(resCode, resDescription, null, url);
	}

	/** Crea una excepci&oacute;n de error de conexi&oacute;n HTTP.
	 * @param resCode C&oacute;digo HTTP de respuesta.
	 * @param resDescription Descripci&oacute;n del error.
	 * @param errorStreamContent Contenido del flujo de error.
	 * @param url URL a la que se intent&oacute; conectar.  */
	public HttpError(final int resCode, final String resDescription, final byte[] errorStreamContent, final String url) {
		super(
			"Error HTTP con codigo  " + resCode + //$NON-NLS-1$
				(errorStreamContent != null && errorStreamContent.length > 0  ? ", cuerpo '" + LoggerUtil.getTrimBytes(errorStreamContent) + "'" : "") + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					" y descripcion '" + resDescription  + //$NON-NLS-1$
						"' para la direccion: " + LoggerUtil.getTrimStr(url) //$NON-NLS-1$
		);
		this.responseCode = resCode;
		this.responseDescription = resDescription;
		this.errorStreamBytes = errorStreamContent != null ? errorStreamContent.clone() : null;
	}

	/** Obtiene el contenido del flujo de error HTTP.
	 * @return Contenido del flujo de error HTTP. */
	public byte[] getErrorStreamBytes() {
		return this.errorStreamBytes != null ? this.errorStreamBytes.clone() : null;
	}

	/** Obtiene el c&oacute;digo HTTP de respuesta.
	 * @return C&oacute;digo HTTP de respuesta. */
	public int getResponseCode() {
		return this.responseCode;
	}

	/** Obtiene la descripci&oacute;n del error HTTP.
	 * @return Descripci&oacute;n del error HTTP. */
	public String getResponseDescription() {
		return this.responseDescription;
	}

}
