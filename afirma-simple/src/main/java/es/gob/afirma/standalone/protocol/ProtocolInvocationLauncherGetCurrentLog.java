package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.util.logging.Logger;

import es.gob.afirma.core.LogManager;
import es.gob.afirma.core.misc.protocol.UrlParametersToLoad;

final class ProtocolInvocationLauncherGetCurrentLog {

	private static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private ProtocolInvocationLauncherGetCurrentLog() {
		// No instanciable
	}

	static String processGetCurrentLog(final UrlParametersToLoad options, final boolean bySocket) throws SocketOperationException {


		if (!ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED.support(options.getMinimumVersion())) {
			LOGGER.severe(String.format("Version de protocolo no soportada (%1s). Version actual: %s2. Hay que actualizar la aplicacion.", options.getMinimumVersion(), ProtocolInvocationLauncher.MAX_PROTOCOL_VERSION_SUPPORTED)); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_21);
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_21);
		}

		// Preparamos el buffer para enviar el resultado
		final String dataToSend;

		// Intentamos extraer la informacion de cada objecto File obtenidos en el
		// paso anterior
		try {

			dataToSend = getCurrentLog();

		} catch (final IOException e) {
			LOGGER.severe("Error al obtener el registro de log acumulado hasta la ejecucion actual: " + e); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_24);
			if (!bySocket) {
				throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.SAF_24);
			}
			return ProtocolInvocationLauncherErrorManager
					.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_24);
		}

		return dataToSend;
	}

	/** Obtiene, en formato XML, el registro acumulado de la ejecuci&oacute;n actual.
	 * @return String que contiene el registro de log acumulado hasta la ejecuci&oacute;n actual.
	 * @throws IOException Si no hay registro o este no se puede leer. */
	public static String getCurrentLog() throws IOException {
		return LogManager.getLogFile();
	}

	public static String getResultCancel() {
		return RESULT_CANCEL;
	}

}
