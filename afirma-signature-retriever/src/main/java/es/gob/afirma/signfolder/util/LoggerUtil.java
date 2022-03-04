/* Copyright (C) 2022 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signfolder.util;

/** Clase con m&eacute;todos para el trabajo con logs. */
public final class LoggerUtil {

	private LoggerUtil() {
		// No permitimos la instanciacion
	}

    /**
     * Omite el directorio del usuario por USERHOME.
     * @param path Ruta del directorio a tratar.
     * @return Ruta ofuscada.
     */
    public static String getCleanUserHomePath(final String path) {
    	final String userHomeDir = System.getProperty("user.home"); //$NON-NLS-1$
    	if (userHomeDir != null) {
    		return userHomeDir;
    	}
    	return ""; //$NON-NLS-1$
    }
}
