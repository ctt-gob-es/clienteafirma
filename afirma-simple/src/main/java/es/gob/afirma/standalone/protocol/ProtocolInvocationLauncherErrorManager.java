package es.gob.afirma.standalone.protocol;

import java.util.Dictionary;
import java.util.Hashtable;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;

/** Gestiona los errores de la ejecuci&oacute;n del Cliente Afirma en una invocaci&oacute;n
 * por protocolo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class ProtocolInvocationLauncherErrorManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final boolean HEADLESS = Boolean.getBoolean(
		"es.gob.afirma.protocolinvocation.HeadLess" //$NON-NLS-1$
	);

	static final String SAF_00 = "SAF_00"; //$NON-NLS-1$
	static final String SAF_01 = "SAF_01"; //$NON-NLS-1$
	static final String SAF_02 = "SAF_02"; //$NON-NLS-1$
	static final String SAF_03 = "SAF_03"; //$NON-NLS-1$
	static final String SAF_04 = "SAF_04"; //$NON-NLS-1$
	static final String SAF_05 = "SAF_05"; //$NON-NLS-1$
	static final String SAF_06 = "SAF_06"; //$NON-NLS-1$
	static final String SAF_07 = "SAF_07"; //$NON-NLS-1$
	static final String SAF_08 = "SAF_08"; //$NON-NLS-1$
	static final String SAF_09 = "SAF_09"; //$NON-NLS-1$
	static final String SAF_10 = "SAF_10"; //$NON-NLS-1$
	static final String SAF_11 = "SAF_11"; //$NON-NLS-1$
	static final String SAF_12 = "SAF_12"; //$NON-NLS-1$
	static final String SAF_13 = "SAF_13"; //$NON-NLS-1$
	static final String SAF_14 = "SAF_14"; //$NON-NLS-1$
	static final String SAF_15 = "SAF_15"; //$NON-NLS-1$
	static final String SAF_16 = "SAF_16"; //$NON-NLS-1$
	static final String SAF_17 = "SAF_17"; //$NON-NLS-1$
	static final String SAF_18 = "SAF_18"; //$NON-NLS-1$
	static final String SAF_19 = "SAF_19"; //$NON-NLS-1$
	static final String SAF_20 = "SAF_20"; //$NON-NLS-1$
	static final String SAF_21 = "SAF_21"; //$NON-NLS-1$
	static final String SAF_22 = "SAF_22"; //$NON-NLS-1$

	private static final Dictionary<String, String> ERRORS = new Hashtable<>();
	static {
		ERRORS.put(SAF_00, ProtocolMessages.getString("ProtocolLauncher.0")); //$NON-NLS-1$
		ERRORS.put(SAF_01, ProtocolMessages.getString("ProtocolLauncher.1")); //$NON-NLS-1$
		ERRORS.put(SAF_02, ProtocolMessages.getString("ProtocolLauncher.2")); //$NON-NLS-1$
		ERRORS.put(SAF_03, ProtocolMessages.getString("ProtocolLauncher.3")); //$NON-NLS-1$
		ERRORS.put(SAF_04, ProtocolMessages.getString("ProtocolLauncher.4")); //$NON-NLS-1$
		ERRORS.put(SAF_05, ProtocolMessages.getString("ProtocolLauncher.5")); //$NON-NLS-1$
		ERRORS.put(SAF_06, ProtocolMessages.getString("ProtocolLauncher.6")); //$NON-NLS-1$
		ERRORS.put(SAF_07, ProtocolMessages.getString("ProtocolLauncher.7")); //$NON-NLS-1$
		ERRORS.put(SAF_08, ProtocolMessages.getString("ProtocolLauncher.8")); //$NON-NLS-1$
		ERRORS.put(SAF_09, ProtocolMessages.getString("ProtocolLauncher.9")); //$NON-NLS-1$
		ERRORS.put(SAF_10, ProtocolMessages.getString("ProtocolLauncher.10")); //$NON-NLS-1$
		ERRORS.put(SAF_11, ProtocolMessages.getString("ProtocolLauncher.11")); //$NON-NLS-1$
		ERRORS.put(SAF_12, ProtocolMessages.getString("ProtocolLauncher.12")); //$NON-NLS-1$
		ERRORS.put(SAF_13, ProtocolMessages.getString("ProtocolLauncher.13")); //$NON-NLS-1$
		ERRORS.put(SAF_14, ProtocolMessages.getString("ProtocolLauncher.14")); //$NON-NLS-1$
		ERRORS.put(SAF_15, ProtocolMessages.getString("ProtocolLauncher.15")); //$NON-NLS-1$
		ERRORS.put(SAF_16, ProtocolMessages.getString("ProtocolLauncher.16")); //$NON-NLS-1$
		ERRORS.put(SAF_17, ProtocolMessages.getString("ProtocolLauncher.17")); //$NON-NLS-1$
		ERRORS.put(SAF_18, ProtocolMessages.getString("ProtocolLauncher.18")); //$NON-NLS-1$
		ERRORS.put(SAF_19, ProtocolMessages.getString("ProtocolLauncher.19")); //$NON-NLS-1$
		ERRORS.put(SAF_20, ProtocolMessages.getString("ProtocolLauncher.20")); //$NON-NLS-1$
		ERRORS.put(SAF_21, ProtocolMessages.getString("ProtocolLauncher.21")); //$NON-NLS-1$
		ERRORS.put(SAF_22, ProtocolMessages.getString("ProtocolLauncher.22")); //$NON-NLS-1$
	}

	static void showError(final String code) {
		final String desc = ProtocolMessages.getString("ProtocolLauncher.28") + "\n(" + code + ": " + ERRORS.get(code) + ")";  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		if (!HEADLESS) {
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				ServiceInvocationManager.focusApplication();
			}
			AOUIFactory.showErrorMessage(
				null,
				desc,
				ProtocolMessages.getString("ProtocolLauncher.29"), //$NON-NLS-1$
				AOUIFactory.ERROR_MESSAGE
			);
		}
		LOGGER.severe(desc);
	}

	static String getErrorMessage(final String code) {
		return code + ": " + ERRORS.get(code); //$NON-NLS-1$
	}
}
