/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.pdf;

import java.util.ResourceBundle;
import java.util.logging.Logger;

final class SignPdfUiMessages {

	private static final String BUNDLE_NAME = "properties.signpdfuimessages"; //$NON-NLS-1$
	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

	private SignPdfUiMessages() {
		// No instanciable
	}

	static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"Texto '" + key + "' del UI de firma de PDF no encontrado: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
			return '!' + key + '!';
		}
	}

    /** Recupera el texto identificado con la clave proporcionada y sustituye las
     * subcadenas de tipo "%i" por el texto correspondiente segun posici&oacute;n de los
     * par&aacute;metros indicados despu&eacute;s de la clave. Las posiciones empiezan a contar
     * por 0. As&iacute;, el texto "%0" se sustituir&aacute; por el segundo par&aacute;ametro
     * de la funci&oacute;n, el texto "%1" por el siguiente,...<br>
     * Introducir m&aacute;s de 10 cadenas distintas para sustituir
     * conllevar&aacute; errores en el resultado.
     * @param key Clave del texto.
     * @param params Par&aacute;metros que se desean insertar.
     * @return Recurso textual con las subcadenas sustituidas. */
    static String getString(final String key, final String... params) {

        String text;
        try {
            text = RESOURCE_BUNDLE.getString(key);
        }
        catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"Texto '" + key + "' del UI de firma de PDF no encontrado: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
            return '!' + key + '!';
        }

        if (params != null && params.length > 0) {
            for (int i = 0; i < params.length; i++) {
            	if (params[i] != null) {
            		text = text.replace("%" + i, params[i]); //$NON-NLS-1$
            	}
            }
        }

        return text;
    }
}
