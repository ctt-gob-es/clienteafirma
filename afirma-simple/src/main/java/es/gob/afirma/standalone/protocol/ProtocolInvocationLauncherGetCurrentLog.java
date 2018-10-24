/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.util.logging.Logger;

import es.gob.afirma.core.LogManager;
import es.gob.afirma.core.misc.protocol.UrlParametersToGetCurrentLog;

final class ProtocolInvocationLauncherGetCurrentLog {

	private static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private ProtocolInvocationLauncherGetCurrentLog() {
		// No instanciable
	}

	static String processGetCurrentLog(final UrlParametersToGetCurrentLog options, final boolean bySocket) throws SocketOperationException {


		if (!ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED.support(options.getMinimumVersion())) {
			LOGGER.severe(String.format("Version de protocolo no soportada (%1s). Version actual: %s2. Hay que actualizar la aplicacion.", options.getMinimumVersion(), ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED)); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_PROCEDURE);
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_UNSUPPORTED_PROCEDURE);
		}

		// Preparamos el buffer para enviar el resultado
		final String dataToSend;
		try {
			dataToSend = getCurrentLog();
		} catch (final IOException e) {
			LOGGER.severe("Error al obtener el registro de log acumulado hasta la ejecucion actual: " + e); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_LOG);
			if (!bySocket) {
				throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_LOG);
			}
			return ProtocolInvocationLauncherErrorManager
					.getErrorMessage(ProtocolInvocationLauncherErrorManager.ERROR_RECOVERING_LOG);
		}

		return dataToSend;
	}

	/** Obtiene, en formato XML, el registro acumulado de la ejecuci&oacute;n actual.
	 * @return String que contiene el registro de log acumulado hasta la ejecuci&oacute;n actual.
	 * @throws IOException Si no hay registro o este no se puede leer. */
	private static String getCurrentLog() throws IOException {
		return LogManager.getLogFile();
	}

	public static String getResultCancel() {
		return RESULT_CANCEL;
	}

}
