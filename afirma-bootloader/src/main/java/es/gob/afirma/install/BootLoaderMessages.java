/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.install;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;


/** Gestor de mensajes del BootLoader. */
final class BootLoaderMessages {

    private static final String BUNDLE_NAME = "messages"; //$NON-NLS-1$

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault(), AOBootUtil.getCleanClassLoader());

    private BootLoaderMessages() {
        // No permitimos la instanciacion
    }

    /** Obtiene un mensaje de usuario.
     * @param key Clave del mensaje a obtener
     * @return Mensaje obtenido */
    static String getString(final String key) {
        try {
            return RESOURCE_BUNDLE.getString(key);
        }
        catch (final MissingResourceException e) {
            return '!' + key + '!';
        }
    }
}
