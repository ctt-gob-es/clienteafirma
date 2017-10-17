/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc.protocol;

/** Error en los par&aacute;metros de la URL recibida por la aplicaci&oacute;n. */
public class ParameterException extends Exception {

	private static final long serialVersionUID = 976364958815642808L;

	ParameterException(final String msg) {
		super(msg);
	}

	ParameterException(final String msg, final Throwable t) {
		super(msg, t);
	}
}