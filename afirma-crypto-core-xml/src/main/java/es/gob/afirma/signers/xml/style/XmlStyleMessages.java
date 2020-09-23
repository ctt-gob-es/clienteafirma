/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xml.style;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.logging.Logger;

/** Clase para la obtencion de los recursos textuales del n&uacute;cleo del
 * cliente de firma. */
final class XmlStyleMessages {

    private static final String BUNDLE_NAME = "xmlmessages"; //$NON-NLS-1$

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());

    private XmlStyleMessages() {
        // No permitimos la instanciacion
    }

    /** Recupera el texto identificado con la clave proporcionada.
     * @param key
     *        Clave del texto.
     * @return Recuerso textual. */
    static String getString(final String key) {
        try {
            return RESOURCE_BUNDLE.getString(key);
        }
        catch (final Exception e) {
        	Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
    			"No se ha encontrado el texto con la clave '" + key + "': " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
            return '!' + key + '!';
        }
    }

    /** Recupera el texto identificado con la clave proporcionada y sustituye la
     * subcadenas "%0" por el texto proporcionado.
     * @param key
     *        Clave del texto.
     * @param text
     *        Texto que se desea insertar.
     * @return Recuerso textual con la subcadena sustituida. */
    static String getString(final String key, final String text) {
        try {
            return RESOURCE_BUNDLE.getString(key).replace("%0", text); //$NON-NLS-1$
        }
        catch (final Exception e) {
        	Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
    			"No se ha encontrado el texto con la clave '" + key + "': " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
            return '!' + key + '!';
        }
    }

}
