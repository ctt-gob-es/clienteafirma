/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

import java.util.ResourceBundle;

/** Manejador de las cadenas de texto externalizadas. */
final class Messages {
	private static final String BUNDLE_NAME = "es.gob.afirma.standalone.configurator.messages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle
			.getBundle(BUNDLE_NAME);

	private Messages() {
		// No instanciable
	}

	/** Recupera la cadena de texto en el idioma configurado.
	 * @param key Clave de la cadena.
	 * @return Cadena de texto. */
	public static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		}
		catch (final Exception e) {
			return '!' + key + '!';
		}
	}
}
