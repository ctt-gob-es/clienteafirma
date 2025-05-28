/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import es.gob.afirma.core.AOException;

/**
 * Error que indica que se necesita una versi&oacute;n m&aacute;s actualizada del aplicativo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class NeedsUpdatedVersionException extends AOException {

	private static final long serialVersionUID = 7936191422727825394L;

	NeedsUpdatedVersionException() {
		super("Se necesita actualizar la aplicacion"); //$NON-NLS-1$
	}

}
