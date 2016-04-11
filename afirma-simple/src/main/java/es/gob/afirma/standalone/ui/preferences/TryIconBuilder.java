package es.gob.afirma.standalone.ui.preferences;

import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.standalone.AutoFirmaUtil;

public class TryIconBuilder {

	static final int TRY_ICON_INSTALL = 0;
	static final int TRY_ICON_UNINSTALL = 1;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String INSTALL_COMMAND[] = {
			"REG", //$NON-NLS-1$
			"ADD", //$NON-NLS-1$
			"\"HKCU\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Run\"", //$NON-NLS-1$
			"/V", //$NON-NLS-1$
			"\"AutoFirmaTrayIcon\"", //$NON-NLS-1$
			"/t", //$NON-NLS-1$
			"REG_SZ", //$NON-NLS-1$
			"/F", //$NON-NLS-1$
			"/D", //$NON-NLS-1$
			"\"" + AutoFirmaUtil.getApplicationDirectory().getAbsolutePath() + "\\AutoFirmaTrayIcon.exe\"" //$NON-NLS-1$ //$NON-NLS-2$
	};

	private static final String UNINSTALL_COMMAND[] = {
			"REG", //$NON-NLS-1$
			"DELETE", //$NON-NLS-1$
			"\"HKCU\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Run\"", //$NON-NLS-1$
			"/V", //$NON-NLS-1$
			"\"AutoFirmaTrayIcon\"", //$NON-NLS-1$
			"/f" //$NON-NLS-1$
	};

	private static final String QUERY_COMMAND[] = {
			"REG", //$NON-NLS-1$
			"QUERY", //$NON-NLS-1$
			"\"HKCU\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Run\"", //$NON-NLS-1$
			"/V", //$NON-NLS-1$
			"\"AutoFirmaTrayIcon\"", //$NON-NLS-1$
			"/s" //$NON-NLS-1$
	};

	static boolean tryIcon(final int install) {
		final ProcessBuilder builder;
		if (install == TRY_ICON_INSTALL && !isTryIconInstalled()) {
			LOGGER.log(Level.INFO, "TrayIcon no instalado, instalamos"); //$NON-NLS-1$
			builder = new ProcessBuilder(INSTALL_COMMAND);
		}
		else if (install == TRY_ICON_UNINSTALL && isTryIconInstalled()){
			LOGGER.log(Level.INFO, "TrayIcon instalado, desinstalamos"); //$NON-NLS-1$
			builder = new ProcessBuilder(UNINSTALL_COMMAND);
		}
		else {
			return true;
		}
		try {
			final Process process = builder.start();
			process.waitFor();
			return process.exitValue() == 0;
		}
		catch (final Exception e) {
			LOGGER.log(Level.SEVERE, "Error ejecutando comando TrayIcon: " + builder.command() + ", " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return false;
	}

	private static boolean isTryIconInstalled() {
		final ProcessBuilder builder = new ProcessBuilder(QUERY_COMMAND);

		try {
			final Process process = builder.start();
			process.waitFor();
			return process.exitValue() == 0;
		}
		catch (final Exception e) {
			LOGGER.log(	Level.SEVERE, "Error ejecutando query TrayIcon: " + e); //$NON-NLS-1$
		}
		return false;
	}
}
