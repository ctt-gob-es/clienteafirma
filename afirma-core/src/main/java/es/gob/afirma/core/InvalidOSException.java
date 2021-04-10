/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core;

/** Indica que se necesita un sistema operativo distinto al actual.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class InvalidOSException extends RuntimeException {
	private static final long serialVersionUID = -6174267665607129065L;

	/** Sistema operativo actual. */
	private final String os;

	/** Crea una nueva instancia de la excepci&oacute;n.
	 * @param expectedOS Sistema operativo esperado */
	public InvalidOSException(final String expectedOS) {
		super("Se esperaba el sistema operativo: " + expectedOS); //$NON-NLS-1$
		this.os = expectedOS;
	}

	/** Obtiene el sistema operativo esperado.
	 * @return nombre del sistema operativo esperado */
	public String getExpectedOS() {
		return this.os;
	}
}
