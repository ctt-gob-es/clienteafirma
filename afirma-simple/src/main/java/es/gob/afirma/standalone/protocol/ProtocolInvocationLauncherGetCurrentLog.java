package es.gob.afirma.standalone.protocol;

import java.security.AccessController;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.protocol.UrlParametersToLoad;
import es.gob.afirma.standalone.protocol.GetCurrentLog;

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
			
		} catch (final Exception e) {
			LOGGER.severe("Error en la lectura de los datos a cargar: " + e); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_00);
			if (!bySocket) {
				throw new SocketOperationException(ProtocolInvocationLauncherErrorManager.SAF_00);
			}
			return ProtocolInvocationLauncherErrorManager
					.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_00);
		}
		
		return dataToSend;
	}
	
	public static String getCurrentLog() {
		return AccessController.doPrivileged(new GetCurrentLog());
	}

	public static String getResultCancel() {
		return RESULT_CANCEL;
	}
	
}
