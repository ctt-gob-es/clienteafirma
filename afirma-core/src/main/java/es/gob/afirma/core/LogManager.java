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
		/** Firma manuscrita digitalizada biom&eacute;trica. */
		HANDWRITTEN,
		/** Otra. */
		OTHER
	}

	private static final String LOG_FILE_NAME = "%a.afirma.log.xml"; //$NON-NLS-1$
	private static final String LOG_FILE_PATH = "%h/.afirma"; //$NON-NLS-1$

	private static final int LOG_MAX_SIZE = 1024 * 1024 * 2;

	private static boolean installed = false;
	private static String logFile = null;
	private static App application = App.OTHER;

	/** Instala los manejadores de registro adicionales creando el fichero de registro en el directorio
	 * predeterminado.
	 * @param app Aplicaci&oacute;n que va a registrar.
	 * @throws java.lang.SecurityException Si no hay permisos para instalar el gestor de registro.
	 * @throws IOException En caso de errores de entrada / salida. */
	public static void install(final App app) throws IOException {
		install(app, null);
	}

	/** Instala los manejadores de registro adicionales.
	 * @param app Aplicaci&oacute;n que va a registrar.
	 * @param logFilePath Ruta de directorios donde guardar el fichero de registro.
	 *                    Los directorios deben separarse con "/", no debe indicarse solo la ruta
	 *                    de directorios, nunca el nombre del fichero de registro (este lo asigna el
	 *                    gestor) y pueden usarse los comodines de <i>Java Logging API</i> para indicar
	 *                    directorios especiales.
	 * @throws java.lang.SecurityException Si no hay permisos para instalar el gestor de registro.
	 * @throws IOException En caso de errores de entrada / salida. */
	public static void install(final App app, final String logFilePath) throws IOException {

		// Aplicacion del log
		if (app == null) {
			application = App.OTHER;
		}
		else {
			application = app;
		}

		// Ruta del fichero
		if (logFilePath == null) {
			logFile = LOG_FILE_PATH.replace("%h", Platform.getUserHome()) + //$NON-NLS-1$
				"/" + //$NON-NLS-1$
					LOG_FILE_NAME.replace("%a", application.toString()); //$NON-NLS-1$
		}
		else {
			logFile = (logFilePath.replace("\\", "/") + //$NON-NLS-1$ //$NON-NLS-2$
				(logFilePath.endsWith("/") ? "" : "/") + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					application + ".afirma.log.xml") //$NON-NLS-1$
						.replace("%h", Platform.getUserHome()); //$NON-NLS-1$
		}
		final File path = new File(new File(logFile).getParent());
		if (!path.exists()) {
			LOGGER.info("La ruta para el fichero de registro ('" + path + "') no existe, se creara");  //$NON-NLS-1$//$NON-NLS-2$
			if (!path.mkdirs()) {
				LOGGER.severe("No se ha podido crear la ruta para el fichero de registro ('" + path + "')"); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		LOGGER.addHandler(createFileHandler(logFile));
		installed = true;
	}

	/** Crea un manejador para el guardado de log en fichero.
	 * @param logFileString Fichero de registro (con ruta) a usar, seg&uacute;n la norma de codificaci&oacute;n
	 *                      de <i>Java Logging API</i> para los c&oacute;digos de directorios.
	 * @return Manejador de log en fichero.
	 * @throws IOException Cuando ocurren errores al crear o utilizar el fichero. */
	private static FileHandler createFileHandler(final String logFileString) throws IOException {
		return new FileHandler(
			logFileString,
			LOG_MAX_SIZE,
			1,
			false
		);
	}

	/** Obtiene, en formato XML, el registro acumulado de la ejecuci&oacute;n actual.
	 * @return Registro acumulado de la ejecuci&oacute;n actual
	 * @throws IOException Si no hay registro o este no se puede leer */
	public static String getLogFile() throws IOException {
		if (!installed || logFile == null) {
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
				logFile.replace("%h", Platform.getUserHome())  //$NON-NLS-1$
			)
		);
		final String log = new String(AOUtil.getDataFromInputStream(is));
		is.close();

		LOGGER.addHandler(createFileHandler(logFile));

		return log;
	}

}
