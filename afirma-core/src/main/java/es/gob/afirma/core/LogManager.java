package es.gob.afirma.core;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;

/** Gestor de registro del Cliente @firma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class LogManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private LogManager() {
		// No permito la instanciacion
	}

	/** Aplicaci&oacute;n que va a registrar. */
	public enum App {
		/** MiniApplet. */
		MINIAPPLET,
		/** Applet. */
		APPLET,
		/** Firma F&aacute;cil. */
		SIMPLE,
		/** StandAlone. */
		STANDALONE,
		/** Cliente para Android. */
		ANDROID,
		/** Otra. */
		OTHER
	}

	private static final String LOG_FILE = "%h/.afirma/%a.afirma.log.xml"; //$NON-NLS-1$
	private static final int LOG_MAX_SIZE = 1024 * 1024 * 2;

	private static boolean installed = false;
	private static App application = App.OTHER;

	/** Instala los manejadores de registro adicionales.
	 * @param app Aplicaci&oacute;n que va a registrar
	 * @throws java.lang.SecurityException Si no hay permisos para instalar el gestor de registro
	 * @throws IOException En caso de errores de entrada / salida */
	public static void install(final App app) throws IOException {
		if (app == null) {
			application = App.OTHER;
		}
		else {
			application = app;
		}
		LOGGER.addHandler(createFileHandler());
		installed = true;
	}

	/**
	 * Crea un manejador para el guardado de log en fichero.
	 * @return Manejador de log en fichero.
	 * @throws IOException Cuando ocurren errores al crear o utilizar el fichero.
	 */
	private static FileHandler createFileHandler() throws IOException {
		return new FileHandler(
				LOG_FILE.replace("%a", application.toString()), //$NON-NLS-1$
				LOG_MAX_SIZE,
				1,
				false
			);
	}

	/** Obtiene, en formato XML, el registro acumulado de la ejecuci&oacute;n actual.
	 * @return Registro acumulado de la ejecuci&oacute;n actual
	 * @throws IOException Si no hay registro o este no se puede leer */
	public static String getLogFile() throws IOException {
		if (!installed) {
			throw new IOException("No esta instalado el manejador de fichero"); //$NON-NLS-1$
		}

		final Handler[] handlers = LOGGER.getHandlers();
		for (final Handler h : handlers) {
			if (h instanceof FileHandler) {
				h.close();
				LOGGER.info("Cerrado el manejador de fichero para permitir que sea procesado"); //$NON-NLS-1$
				LOGGER.removeHandler(h);
			}
		}

		final InputStream is = new FileInputStream(
			new File(
				LOG_FILE.replace("%h", Platform.getUserHome()).replace("%a", application.toString())  //$NON-NLS-1$//$NON-NLS-2$
			)
		);
		final String log = new String(AOUtil.getDataFromInputStream(is));
		is.close();

		LOGGER.addHandler(createFileHandler());

		return log;
	}

}
