/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

/** Excepci&oacute;n que denota un error derivado del uso de una tarjeta inteligente. */
public class SmartCardException extends RuntimeException {

	/** Serial Id. */
	private static final long serialVersionUID = -7200145202118291387L;

	/**
	 * Se contruye la excepci&oacute;n con un mensaje descriptivo.
	 * @param msg Mensaje que describe el error que origin&oacute; la excepci&oacute;n.
	 */
	public SmartCardException(final String msg) {
		super(msg);
	}

	/**
	 * Se contruye la excepci&oacute;n con un mensaje descriptivo y el error que la
	 * origin&oacute;.
	 * @param msg Mensaje que describe el error que origin&oacute; la excepci&oacute;n.
	 * @param t Error que origin&oacute; la excepci&oacute;n.
	 */
	public SmartCardException(final String msg, final Throwable t) {
		super(msg, t);
	}
}
