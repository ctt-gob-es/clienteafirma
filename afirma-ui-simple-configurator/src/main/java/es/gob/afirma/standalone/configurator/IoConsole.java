/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

import java.awt.Component;

final class IoConsole implements Console {

	private final java.io.Console console;

	IoConsole(final java.io.Console con) {
		if (con == null) {
			throw new IllegalArgumentException(
				"La consola de Java no puede ser nula" //$NON-NLS-1$
			);
		}
		this.console = con;
	}

	@Override public void showConsole() { /* Vacio */ }
	@Override public void dispose() { /* Vacio */ }

	@Override
	public void print(final String s) {
		this.console.printf(s);
	}

	@Override public Component getParentComponent() {
		return null;
	}
}
