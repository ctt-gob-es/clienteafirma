/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.misc.http;

import java.io.IOException;

/** Error de conexi&oacute;n HTTP.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class HttpError extends IOException {

	private static final long serialVersionUID = 8997766820804553378L;

	private final int responseCode;
	private final String responseDescription;

	/** Crea una excepci&oacute;n de error de conexi&oacute;n HTTP.
	 * @param resCode C&oacute;digo HTTP de respuesta. */
	HttpError(final int resCode) {
		super("Error en conexion HTTP con codigo de respuesta " + resCode); //$NON-NLS-1$
		this.responseCode = resCode;
		this.responseDescription = null;
	}

	/** Crea una excepci&oacute;n de error de conexi&oacute;n HTTP.
	 * @param resCode C&oacute;digo HTTP de respuesta.
	 * @param resDescription Descripci&oacute;n del error.
	 * @param url URL a la que se intent&oacute; conectar.  */
	public HttpError(final int resCode, final String resDescription, final String url) {
		super(
			"Error en conexion HTTP con codigo de respuesta " + resCode + //$NON-NLS-1$
				" y descripcion '" + resDescription  + //$NON-NLS-1$
					"' para la direccion: " + url //$NON-NLS-1$
		);
		this.responseCode = resCode;
		this.responseDescription = resDescription;
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
