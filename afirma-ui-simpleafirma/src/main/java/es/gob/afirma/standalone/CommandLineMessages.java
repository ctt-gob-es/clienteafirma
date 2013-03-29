/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/** Gesti&oacute;n de mensajes del aplicativo. */
final class CommandLineMessages {

    private static final String BUNDLE_NAME = "es.gob.afirma.standalone.commandlinemessages"; //$NON-NLS-1$

    private static ResourceBundle bundle = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());

    private CommandLineMessages() { /* No permitimos la instanciacion */ }

    /** Obtiene un mensaje.
     * @param key Clave del mensaje
     * @return Mensaje correspondiente a la clave */
    static String getString(final String key) {
        try {
            return bundle.getString(key);
        }
        catch (final MissingResourceException e) {
            return '!' + key + '!';
        }
    }

    /** Cambia la localizaci&oacute;n a la establecida por defecto. */
    static void changeLocale() {
        bundle = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());
    }
}
