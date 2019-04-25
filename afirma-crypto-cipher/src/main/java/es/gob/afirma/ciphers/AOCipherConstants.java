/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.ciphers;

/**
 * Constantes para el uso de las funcionalidades de cifrado.
 */
public final class AOCipherConstants {

    private AOCipherConstants() {
        // No permitimos la instancacion
    }

    /** Modo de generaci&oacute;n autom&aacute;tica de clave sim&eacute;trica
     * aleatoria. */
    public static final String KEY_MODE_GENERATEKEY = "GENERATEKEY"; //$NON-NLS-1$

    /** Modo de inserci&oacute;n directa de clave por parte del usuario. */
    public static final String KEY_MODE_USERINPUT = "USERINPUT"; //$NON-NLS-1$

    /** Modo de generaci&oacute;n de clave a partir de un texto. */
    public static final String KEY_MODE_STRING = "PASSWORD"; //$NON-NLS-1$

    /** Algoritmo de cifrado que se usa por defecto. */
    public static final String DEFAULT_KEY_MODE = KEY_MODE_GENERATEKEY;
}
