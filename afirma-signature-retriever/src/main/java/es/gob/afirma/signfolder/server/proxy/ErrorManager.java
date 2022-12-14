/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signfolder.server.proxy;

import java.util.AbstractMap;
import java.util.HashMap;

final class ErrorManager {

	private static final String ERROR_SEPARATOR = ":="; //$NON-NLS-1$

	private static final String GENERIC_ERROR = "Error gen\u00E9rico"; //$NON-NLS-1$

	static final String ERROR_MISSING_OPERATION_NAME     = "ERR-00"; //$NON-NLS-1$
	static final String ERROR_UNSUPPORTED_OPERATION_NAME = "ERR-01"; //$NON-NLS-1$
	static final String ERROR_MISSING_DATA_ID            = "ERR-05"; //$NON-NLS-1$
	static final String ERROR_INVALID_DATA_ID            = "ERR-06"; //$NON-NLS-1$
	static final String ERROR_INVALID_DATA               = "ERR-07"; //$NON-NLS-1$
	static final String ERROR_MISSING_SYNTAX_VERSION	 = "ERR-20"; //$NON-NLS-1$


	private static final AbstractMap<String, String> ERRORS = new HashMap<String, String>();
	static {
		ERRORS.put(ERROR_MISSING_OPERATION_NAME,     "No se ha indicado c\u00F3digo de operaci\u00F3n");                          //$NON-NLS-1$
		ERRORS.put(ERROR_UNSUPPORTED_OPERATION_NAME, "C\u00F3digo de operaci\u00F3n no soportado");                               //$NON-NLS-1$
		ERRORS.put(ERROR_MISSING_DATA_ID,            "No se ha proporcionado un identificador para los datos");                   //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_DATA_ID,            "El identificador para los datos es inv\u00E1lido");                         //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_DATA,               "Los datos solicitados o enviados son inv\u00E1lidos");                      //$NON-NLS-1$
		ERRORS.put(ERROR_MISSING_SYNTAX_VERSION,     "No se ha indicado la versi\u00F3n de la sintaxis de la operaci\u00F3n");    //$NON-NLS-1$
	}

	private ErrorManager() {
		// No instanciable
	}

	static String genError(final String number) {
		return genError(number, null);
	}

	static String genError(final String number, final String msg) {
		final String resultMsg = msg != null ? msg : ERRORS.get(number);
		return new StringBuilder(number)
			.append(ERROR_SEPARATOR)
			.append(resultMsg != null ? resultMsg : GENERIC_ERROR).toString();
	}
}
