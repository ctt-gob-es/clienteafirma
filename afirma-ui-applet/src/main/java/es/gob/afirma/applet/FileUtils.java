/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.net.URI;
import java.security.AccessController;
import java.util.logging.Logger;


/** Funciones de utilidad orientadas al uso de ficheros, teniendo en cuenta que
 * estas deben tratarse como funciones privilegiadas. */
final class FileUtils {

	private FileUtils() {
		// No permitimos la instanciacion
	}

    /** Carga un fichero de datos. Si ocurre un error durante la carga, se
     * devuelve {@code null}.
     * @param path
     *        Ruta del fichero.
     * @return Contenido del fichero. */
    static byte[] loadFile(final String path) {
    	try {
    		return AccessController.doPrivileged(new LoadFileAction(path));
    	} catch (final Exception e) {
    		Logger.getLogger("es.gob.afirma").severe(e.toString()); //$NON-NLS-1$
    		return null;
    	}
    }

    /** Carga un fichero de datos. Si ocurre un error durante la carga, se
     * devuelve {@code null}.
     * @param uri
     *        Ruta del fichero.
     * @return Contenido del fichero. */
    static byte[] loadFile(final URI uri) {
    	try {
    		return AccessController.doPrivileged(new LoadFileAction(uri));
    	} catch (final Exception e) {
    		Logger.getLogger("es.gob.afirma").severe(e.toString()); //$NON-NLS-1$
    		return null;
    	}
    }
}
