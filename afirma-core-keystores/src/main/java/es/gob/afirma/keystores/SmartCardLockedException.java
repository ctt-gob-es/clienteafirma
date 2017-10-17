/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

/**
 * Excepci&oacute;n para se&ntilde;alar un error debido a que se encontr&oacute; una
 * tarjeta inteligente bloqueada.
 */
public class SmartCardLockedException extends SmartCardException {

	/** Serial Id. */
	private static final long serialVersionUID = 482605403233218951L;

	/**
	 * Se contruye la excepci&oacute;n con un mensaje descriptivo.
	 * @param msg Mensaje que describe el error que origin&oacute; la excepci&oacute;n.
	 */
	public SmartCardLockedException(final String msg) {
		super(msg);
	}

	/**
	 * Se contruye la excepci&oacute;n con un mensaje descriptivo y el error que la
	 * origin&oacute;.
	 * @param msg Mensaje que describe el error que origin&oacute; la excepci&oacute;n.
	 * @param t Error que origin&oacute; la excepci&oacute;n.
	 */
	public SmartCardLockedException(final String msg, final Throwable t) {
		super(msg, t);
	}
}
