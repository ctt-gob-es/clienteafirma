/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xml.style;

/** Excepci&oacute;n relativa a los errores de firma de hojas de estilo XML. */
public abstract class StyleException extends Exception {

	private static final long serialVersionUID = 1922584585798106575L;

	StyleException(final String msg) {
		super(msg);
	}

	StyleException(final String msg, final Throwable e) {
		super(msg, e);
	}

	StyleException(final Throwable e) {
		super(e);
	}
}
