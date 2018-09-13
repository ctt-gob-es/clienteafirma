/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc.protocol;

/** Error que indica que se ha solicitado en los par&aacute;metros un acceso local prohibido.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class ParameterLocalAccessRequestedException extends ParameterException {

	private static final long serialVersionUID = -6979789543878872249L;

	/** Crea una excepci&oacute;n que indica que se ha solicitado en los par&aacute;metros
	 * un acceso local prohibido.
	 * @param msg Mensaje de la excepci&oacute;n. */
	public ParameterLocalAccessRequestedException(final String msg) {
		super(msg);
	}

	/** Crea una excepci&oacute;n que indica que se ha solicitado en los par&aacute;metros
	 * un acceso local prohibido.
	 * @param msg Mensaje de la excepci&oacute;n.
	 * @param cause Causa inicial de la excepci&oacute;n. */
	public ParameterLocalAccessRequestedException(final String msg, final Throwable cause) {
		super(msg, cause);
	}
}
