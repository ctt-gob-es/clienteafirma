package es.gob.afirma.standalone.protocol;

import java.util.logging.Logger;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.protocol.UrlParametersToSave;
import es.gob.afirma.core.ui.AOUIFactory;

final class ProtocolInvocationLauncherSave {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String RESULT_OK = "OK"; //$NON-NLS-1$
	private static final String RESULT_CANCEL = "CANCEL"; //$NON-NLS-1$

	private ProtocolInvocationLauncherSave() {
		// No instanciable
	}

	static String processSave(final UrlParametersToSave  options) {
		try {
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				ServiceInvocationManager.focusApplication();
			}
			AOUIFactory.getSaveDataToFile(
				options.getData(),
				options.getTitle(),
				null,
				options.getFileName(),
				options.getExtensions() != null ? new String[] { options.getExtensions() } : null,
				options.getFileTypeDescription(),
				null
			);
		}
		catch(final AOCancelledOperationException e) {
			LOGGER.severe("Operacion cancelada por el usuario" + e); //$NON-NLS-1$
			return RESULT_CANCEL;
		}
		catch (final Exception e) {
			LOGGER.severe("Error en el guardado de datos: " + e); //$NON-NLS-1$
			ProtocolInvocationLauncherErrorManager.showError(ProtocolInvocationLauncherErrorManager.SAF_05);
			return ProtocolInvocationLauncherErrorManager.getErrorMessage(ProtocolInvocationLauncherErrorManager.SAF_05);
		}
		return RESULT_OK;
	}

	public static String getResultOk() {
		return RESULT_OK;
	}

	public static String getResultCancel() {
		return RESULT_CANCEL;
	}

}
