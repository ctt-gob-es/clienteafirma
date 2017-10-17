/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.ooxml;

/** Versi&oacute;n del JRE no soportada.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class UnsupportedJreVersionException extends RuntimeException {

	private static final long serialVersionUID = -5356570249002228729L;

	UnsupportedJreVersionException() {
		super(
			"Version de Java no soportada. Se necesita Java 7 o superior, y se ha encontrado Java " + System.getProperty("java.version") //$NON-NLS-1$ //$NON-NLS-2$
		);
	}


}
