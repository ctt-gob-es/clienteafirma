/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signfolder.server.proxy;

import java.util.Dictionary;
import java.util.Hashtable;

final class ErrorManager {

	private static final String ERROR_SEPARATOR = ":="; //$NON-NLS-1$

	private static final String GENERIC_ERROR = "Error generico"; //$NON-NLS-1$

	static final String ERROR_MISSING_OPERATION_NAME     = "ERR-00"; //$NON-NLS-1$
	static final String ERROR_UNSUPPORTED_OPERATION_NAME = "ERR-01"; //$NON-NLS-1$
	static final String ERROR_MISSING_DATA               = "ERR-02"; //$NON-NLS-1$
	static final String ERROR_BAD_XML                    = "ERR-03"; //$NON-NLS-1$
	static final String ERROR_BAD_CERTIFICATE            = "ERR-04"; //$NON-NLS-1$
	static final String ERROR_MISSING_DATA_ID            = "ERR-05"; //$NON-NLS-1$
	static final String ERROR_INVALID_DATA_ID            = "ERR-06"; //$NON-NLS-1$
	static final String ERROR_INVALID_DATA               = "ERR-07"; //$NON-NLS-1$
	static final String ERROR_MISSING_SERVLET      		 = "ERR-08"; //$NON-NLS-1$
	static final String ERROR_INVALID_SERVLET        	 = "ERR-09"; //$NON-NLS-1$
	static final String ERROR_NOT_SUPPORTED_FORMAT       = "ERR-10"; //$NON-NLS-1$
	static final String ERROR_CANCELLED_OPERATION        = "ERR-11"; //$NON-NLS-1$
	static final String ERROR_CODING_BASE64				 = "ERR-12"; //$NON-NLS-1$
	static final String ERROR_PKE       				 = "ERR-13"; //$NON-NLS-1$
	static final String ERROR_SIGNING       			 = "ERR-14"; //$NON-NLS-1$
	static final String ERROR_INVALID_CIPHER_KEY         = "ERR-15"; //$NON-NLS-1$
	static final String ERROR_CIPHERING			         = "ERR-16"; //$NON-NLS-1$
	static final String ERROR_NO_CERT_SELECTED			 = "ERR-17"; //$NON-NLS-1$
	static final String ERROR_COMMUNICATING_WITH_WEB	 = "ERR-18"; //$NON-NLS-1$
	static final String ERROR_CONFIGURATION_FILE_PROBLEM = "ERR-19"; //$NON-NLS-1$
	static final String ERROR_MISSING_SYNTAX_VERSION	 = "ERR-20"; //$NON-NLS-1$

	private static final Dictionary<String, String> ERRORS = new Hashtable<String, String>();
	static {
		ERRORS.put(ERROR_MISSING_OPERATION_NAME, "No se ha indicado codigo de operacion"); //$NON-NLS-1$
		ERRORS.put(ERROR_UNSUPPORTED_OPERATION_NAME, "Codigo de operacion no soportado"); //$NON-NLS-1$
		ERRORS.put(ERROR_MISSING_DATA, "No se han proporcionado los datos de la operacion"); //$NON-NLS-1$
		ERRORS.put(ERROR_BAD_XML, "Se ha recibido un XML mal formado"); //$NON-NLS-1$
		ERRORS.put(ERROR_BAD_CERTIFICATE, "Se ha recibido un certificado corrupto"); //$NON-NLS-1$
		ERRORS.put(ERROR_MISSING_DATA_ID, "No se ha proporcionado un identificador para los datos"); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_DATA_ID, "El identificador para los datos es invalido"); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_DATA, "Los datos solicitados o enviados son invalidos"); //$NON-NLS-1$
		ERRORS.put(ERROR_MISSING_SERVLET, "No se ha proporcionado el sevlet para la comunicacion de los datos"); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_SERVLET, "La ruta del servlet es invalida"); //$NON-NLS-1$
		ERRORS.put(ERROR_NOT_SUPPORTED_FORMAT, "Se ha configurado un formato de firma no soportado"); //$NON-NLS-1$
		ERRORS.put(ERROR_CANCELLED_OPERATION, "Operaci\u00F3n cancelada"); //$NON-NLS-1$
		ERRORS.put(ERROR_CODING_BASE64, "Error en la codificaci\u00F3n del base 64"); //$NON-NLS-1$
		ERRORS.put(ERROR_PKE, "No se seleccion\u00F3 certificado de firma"); //$NON-NLS-1$
		ERRORS.put(ERROR_SIGNING, "Ocurri\u00F3 un error en la operaci\u00F3n de firma"); //$NON-NLS-1$
		ERRORS.put(ERROR_INVALID_CIPHER_KEY, "La clave de cifrado proporcionada no es valida"); //$NON-NLS-1$
		ERRORS.put(ERROR_CIPHERING, "Error durante el proceso de cifrado de los datos"); //$NON-NLS-1$
		ERRORS.put(ERROR_NO_CERT_SELECTED, "El usuario no seleccion\u00F3 ning\u00FAn certificado "); //$NON-NLS-1$
		ERRORS.put(ERROR_COMMUNICATING_WITH_WEB, "No se ha podido enviar la firma generada a la web de origen"); //$NON-NLS-1$
		ERRORS.put(ERROR_CONFIGURATION_FILE_PROBLEM, "Error de configuraci\u00F3n de la aplicaci\u00F3n"); //$NON-NLS-1$
		ERRORS.put(ERROR_MISSING_SYNTAX_VERSION, "No se ha indicado la versi\u00F3n de la sintaxis de la operaci\u00F3n"); //$NON-NLS-1$
	}

	private ErrorManager() {
		// No instanciable
	}

	static String genError(final String number) {
		return genError(number, null);
	}

	static String genError(final String number, final String msg) {
		final String resultMsg = msg != null ? msg : ERRORS.get(number);
		return new StringBuilder(number).append(ERROR_SEPARATOR).
				append(resultMsg != null ? resultMsg : GENERIC_ERROR).toString();
	}
}
