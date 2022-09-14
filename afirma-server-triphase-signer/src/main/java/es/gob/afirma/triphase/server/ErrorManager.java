/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server;

import java.util.Dictionary;
import java.util.Hashtable;

final class ErrorManager {

	public static final int OPERATION_ARGUMENT_NOT_FOUND = 1;
	public static final int DOCUMENT_ID_ARGUMENT_NOT_FOUND = 2;
	public static final int ALGORITHM_ARGUMENT_NOT_FOUND = 3;
	public static final int FORMAT_ARGUMENT_NOT_FOUND = 4;
	public static final int CERTIFICATE_ARGUMENT_NOT_FOUND = 5;
	public static final int INVALID_EXTRAPARAMS_FORMAT = 6;
	public static final int INVALID_CERTIFICATE_FORMAT = 7;
	public static final int UNSUPPORTED_FORMAT = 8;
	public static final int PRESIGN_ERROR = 9;
	public static final int SAVING_DOCUMENT_ERROR = 10;
	public static final int UNSUPPORTED_OPERATION = 11;
	public static final int POSTSIGN_ERROR = 12;
	public static final int INVALID_SUBOPERATION = 13;
	public static final int RECOVERING_DOCUMENT_ERROR = 14;
	public static final int INVALID_SESSION_DATA = 15;
	public static final int GENERATING_CSV_ERROR = 16;
	public static final int CHECKING_CSV_ERROR = 17;
	public static final int SIGNATURE_INTEGRITY_ERROR = 18;
	public static final int INVALID_DATA_OPERATION_FORMAT = 19;
	public static final int UNSUPPORTED_ALGORITHM = 20;
	public static final int CONFIGURATION_NEEDED = 21;

	private static final Dictionary<Integer, String> errorMessages = new Hashtable<>();
	static {
		errorMessages.put(Integer.valueOf(OPERATION_ARGUMENT_NOT_FOUND), "No se ha indicado la operacion a realizar"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(DOCUMENT_ID_ARGUMENT_NOT_FOUND), "No se ha indicado el identificador del documento"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(ALGORITHM_ARGUMENT_NOT_FOUND), "No se ha indicado el algoritmo de firma"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(FORMAT_ARGUMENT_NOT_FOUND), "No se ha indicado el formato de firma"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(CERTIFICATE_ARGUMENT_NOT_FOUND), "No se ha indicado el certificado de usuario"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(INVALID_EXTRAPARAMS_FORMAT), "El formato de los parametros adicionales suministrados es erroneo"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(INVALID_CERTIFICATE_FORMAT), "El certificado de usuario no esta en formato X.509"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(UNSUPPORTED_FORMAT), "Formato de firma no soportado"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(PRESIGN_ERROR), "Error realizando la prefirma"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(SAVING_DOCUMENT_ERROR), "Error al almacenar el documento"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(UNSUPPORTED_OPERATION), "Operacion desconocida"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(POSTSIGN_ERROR), "Error realizando la postfirma"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(INVALID_SUBOPERATION), "No se indicado una sub-operacion valida a realizar (firma, cofirma,...)"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(RECOVERING_DOCUMENT_ERROR), "Error al recuperar el documento"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(INVALID_SESSION_DATA), "El formato de los datos de sesion suministrados es erroneo"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(GENERATING_CSV_ERROR), "Error al generar el codigo de verificacion de las firmas"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(CHECKING_CSV_ERROR), "Error al comprobar el codigo de verificacion de las firmas"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(SIGNATURE_INTEGRITY_ERROR), "Error de integridad en la firma"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(INVALID_DATA_OPERATION_FORMAT), "El formato de los datos de operacion suministrados es erroneo"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(UNSUPPORTED_ALGORITHM), "Algoritmo de firma no soportado"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(CONFIGURATION_NEEDED), "Se requiere intervencion del usuario"); //$NON-NLS-1$
	}

	static String getErrorMessage(final int errNo) {
		return getErrorPrefix(errNo) + ": " + errorMessages.get(Integer.valueOf(errNo)); //$NON-NLS-1$
	}

	static String getErrorMessage(final int errNo, final String code) {
		return getErrorPrefix(errNo) + ":" + code + ": " + errorMessages.get(Integer.valueOf(errNo)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	static String getErrorPrefix(final int errNo) {
		return "ERR-" + Integer.toString(errNo); //$NON-NLS-1$
	}

}



