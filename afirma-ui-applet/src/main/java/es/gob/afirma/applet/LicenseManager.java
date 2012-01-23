/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;

import es.gob.afirma.core.misc.AOUtil;

/** Clase para la carga del acuerdo de licencia. */
final class LicenseManager {

    private static String LICENSE_FILE = "/resources/licenses"; //$NON-NLS-1$

    /** recupera el texto de la licencia del Cliente @firma. Si ocurre un error
     * lanza una excepci&oacute;n.
     * @return Texto de la licencia.
     * @throws IOException
     *         Cuando no se ha podido recuperar el texto de la licencia. */
    static String getLicenceText() throws IOException {

        InputStream is = Class.class.getResourceAsStream(LICENSE_FILE + "_" + Locale.getDefault()); //$NON-NLS-1$

        if (is == null) {
            is = Class.class.getResourceAsStream(LICENSE_FILE + "_" + Locale.getDefault().getLanguage()); //$NON-NLS-1$
        }

        if (is == null) {
            is = Class.class.getClass().getResourceAsStream(LICENSE_FILE);
        }

        try {
            return new String(AOUtil.getDataFromInputStream(is), "UTF-8"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            throw new IOException("No se ha podido recuperar el texto de la licencia:" + e); //$NON-NLS-1$
        }
        finally {
            try {
                is.close();
            }
            catch (final Exception e) {
                // Ignoramos los errores en el cierre
            }
        }
    }
}
