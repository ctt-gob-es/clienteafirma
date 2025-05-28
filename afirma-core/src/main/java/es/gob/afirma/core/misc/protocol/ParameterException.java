/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc.protocol;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;

/** Error en los par&aacute;metros de la URL recibida por la aplicaci&oacute;n. */
public class ParameterException extends AOException {

	private static final long serialVersionUID = 976364958815642808L;

	/**
	 * Construye una excepci&oacute;n de error en los par&aacute;metros de la URL
	 * recibida por la aplicaci&oacute;n.
	 * @param msg Mensaje de la excepci&oacute;n.
	 * @param errorCode C&oacute;digo del error concreto que caus&oacute; la excepci&oacute;n.
	 */
	public ParameterException(final String msg, final ErrorCode errorCode) {
		super(msg, errorCode);
	}

	/**
	 * Construye una excepci&oacute;n de error en los par&aacute;metros de la URL
	 * recibida por la aplicaci&oacute;n.
	 * @param msg Mensaje de la excepci&oacute;n.
	 * @param t Causa inicial de la excepci&oacute;n.
	 * @param errorCode C&oacute;digo del error concreto que caus&oacute; la excepci&oacute;n.
	 */
	public ParameterException(final String msg, final Throwable t, final ErrorCode errorCode) {
		super(msg, t, errorCode);
	}
}