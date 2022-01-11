/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.certvalidation.validation;

import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.logging.Logger;

final class CertValidationMessages {

	private static final String BUNDLE_NAME = "es.gob.afirma.plugin.certvalidation.certvalidationmessages"; //$NON-NLS-1$

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

	private CertValidationMessages() {
		// No instanciable
	}

	static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		}
		catch (final MissingResourceException e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No se ha encontrado el texto para la clave '" + key + "': " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
			return '!' + key + '!';
		}
	}
}
