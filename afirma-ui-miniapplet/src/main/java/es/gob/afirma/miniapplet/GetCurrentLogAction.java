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
import java.util.logging.Logger;

import es.gob.afirma.core.LogManager;

/** Acci&oacute;n para recuperar el log generado por la aplicaci&oacute;n.
 * @author Carlos Gamuci Mill&aacute;n. */
final class GetCurrentLogAction implements PrivilegedAction<String> {

    /** Obtiene el registro generado por la aplicaci&oacute;n desde su inicio o desde
     * que se recuper&oacute; por &uacute;ltima vez.
     * @return Registro generado por la aplicaci&oacute;n. */
	@Override
	public String run() {
		try {
			return LogManager.getLogFile();
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No ha sido posible obtener el registro actual del MiniApplet, se devuelve una cadena vacia: " + e //$NON-NLS-1$
			);
		}
		return ""; //$NON-NLS-1$
	}
}
