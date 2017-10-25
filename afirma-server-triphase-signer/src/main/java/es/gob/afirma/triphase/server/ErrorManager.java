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

	private static final Dictionary<Integer, String> errorMessages = new Hashtable<>();
	static {
		errorMessages.put(Integer.valueOf(1), "No se ha indicado la operacion a realizar"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(2), "No se ha indicado el identificador del documento"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(3), "No se ha indicado el algoritmo de firma"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(4), "No se ha indicado el formato de firma"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(5), "No se ha indicado el certificado de usuario"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(6), "El formato de los parametros adicionales suministrados es erroneo"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(7), "El certificado de usuario no esta en formato X.509"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(8), "Formato de firma no soportado"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(9), "Error realizando la prefirma"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(10), "Error en el almacen final del documento"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(11), "Operacion desconocida"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(12), "Error realizando la postfirma"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(13), "No se indicado una sub-operacion valida a realizar (firma, cofirma,...)"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(14), "Error al recuperar el documento"); //$NON-NLS-1$
		errorMessages.put(Integer.valueOf(15), "El formato de los datos de sesion suministrados es erroneo"); //$NON-NLS-1$
	}

	static String getErrorMessage(final int errNo) {
		return "ERR-" + Integer.toString(errNo) + ": " + errorMessages.get(Integer.valueOf(errNo)); //$NON-NLS-1$ //$NON-NLS-2$
	}

}
