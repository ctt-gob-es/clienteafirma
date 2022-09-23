/* Copyright (C) 2022 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc;

import java.util.Arrays;

/** Clase con m&eacute;todos para el trabajo con logs. */
public final class LoggerUtil {

	private static Boolean allowExtendedLogs = null;

	private LoggerUtil() {
		// No permitimos la instanciacion
	}

    /**
     * Omite el directorio del usuario por USERHOME.
     * @param path Ruta del directorio a tratar.
     * @return Ruta ofuscada.
     */
    public static String getCleanUserHomePath(final String path) {
    	return path.replace(Platform.getUserHome(), "USERHOME"); //$NON-NLS-1$
    }

    /**
     * Limita la cadena a 200 caracteres. En caso de que la propiedad "allow.extended.logs"
     * est&eacute; activa, se omite el recortarla.
     * @param str Cadena a recortar.
     * @return Cadena tratada.
     */
    public static String getTrimStr(final String str) {
    	if (allowExtendedLogs == null) {
    		allowExtendedLogs = Boolean.valueOf(Boolean.getBoolean("allow.extended.logs")); //$NON-NLS-1$
    	}

    	if (!allowExtendedLogs.booleanValue() && str != null && str.length() >= 200) {
    		return str.substring(0, 200) + "..."; //$NON-NLS-1$
    	}

    	return str;
    }

    /**
     * Limita un array de bytes a 200 caracteres. En caso de que la propiedad "allow.extended.logs"
     * est&eacute; activa, se omite el recortarlo.
     * @param content Contenido a recortar.
     * @return Cadena tratada.
     */
    public static String getTrimBytes(final byte[] content) {
    	if (allowExtendedLogs == null) {
    		allowExtendedLogs = Boolean.valueOf(Boolean.getBoolean("allow.extended.logs")); //$NON-NLS-1$
    	}

    	if (!allowExtendedLogs.booleanValue() && content != null && content.length > 200) {
    		return new String(Arrays.copyOfRange(content, 0, 200)) + "..."; //$NON-NLS-1$
    	}

    	return new String(content);
    }
}
