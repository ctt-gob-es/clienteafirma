/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/** Gesti&oacute;n de mensajes del aplicativo. */
final class CommandLineMessages {

    private static final String BUNDLE_NAME = "properties/commandlinemessages"; //$NON-NLS-1$

    private static ResourceBundle bundle = ResourceBundle.getBundle(BUNDLE_NAME/*, Locale.getDefault()*/);

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

    /** Obtiene un mensaje.
     * @param key Clave del mensaje
     * @param params Particulas de texto que se agregan al mensaje.
     * @return Mensaje correspondiente a la clave */
    static String getString(final String key, String... params) {
    	
    	String text;
        try {
            text = bundle.getString(key);
        }
        catch (final Exception e) {
            return '!' + key + '!';
        }

        if (params != null) {
            for (int i = 0; i < params.length; i++) {
            	if (params[i] != null) {
            		text = text.replace("%" + i, params[i]); //$NON-NLS-1$
            	}
            }
        }

        return text;
    }
    
    /** Cambia la localizaci&oacute;n a la establecida por defecto. */
    static void changeLocale() {
        bundle = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());
    }
}
