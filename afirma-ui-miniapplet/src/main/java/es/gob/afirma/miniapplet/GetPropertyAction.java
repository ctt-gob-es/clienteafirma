/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.miniapplet;

import java.security.PrivilegedAction;

/**
 * Acci&oacute;n privilegiada para la recuperaci&oacute;n de una propiedad del sistema.
 * @author Carlos Gamuci Mill&aacute;n
 */
final class GetPropertyAction implements PrivilegedAction<String> {

	private final String property;

	/**
	 * Crea la opci&oacute;n para recuperar la propiedad indicada.
	 * @param property Propiedad del sistema que se desea recuperar.
	 */
	public GetPropertyAction(final String property) {
		this.property = property;
	}

	/**
	 * Recupera la propiedad del sistema indicada.
	 */
	@Override
	public String run() {
		return System.getProperty(this.property);
	}
}
