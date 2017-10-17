/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */


package es.gob.afirma.applet;

import java.util.Locale;
import java.util.ResourceBundle;

final class AppletMessages {
    private static final String BUNDLE_NAME = "appletmessages"; //$NON-NLS-1$

    private static ResourceBundle resourceBundle;

    static {
        try {
            resourceBundle = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());
        }
        catch (final Exception e) {
            resourceBundle = ResourceBundle.getBundle(BUNDLE_NAME);
        }
    }

    private AppletMessages() {
        // No permitimos la instanciacion
    }

    static String getString(final String key) {
        try {
            return resourceBundle.getString(key);
        }
        catch (final Exception e) {
            return '!' + key + '!';
        }
    }

    /** Recupera el texto identificado con la clave proporcionada y sustituye las
     * subcadenas de tipo "%i" por el texto en la posici&oacute;n 'i' del array
     * proporcionado.
     * @param key Clave del texto.
     * @param params Par&aacute;metros que se desean insertar.
     * @return Recuerso textual con las subcadenas sustituidas. */
    static String getString(final String key, final String... params) {
        String text;
        try {
            text = resourceBundle.getString(key);
        }
        catch (final Exception e) {
            return '!' + key + '!';
        }
        if (params != null) {
            for (int i = 0; i < params.length; i++) {
            	if (params[i] == null) {
            		params[i] = "null"; //$NON-NLS-1$
            	}
                text = text.replace("%" + i, params[i]); //$NON-NLS-1$
            }
        }
        return text;
    }
}
