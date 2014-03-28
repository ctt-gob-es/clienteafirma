package es.gob.afirma.core;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.FileHandler;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;

/** Gestor de registro del Cliente @firma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class LogManager {

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
	 * @throws SecurityException
	 * @throws IOException */
	public static void install(final App app) throws SecurityException, IOException {
		if (app == null) {
			application = App.OTHER;
		}
		else {
			application = app;
		}
		Logger.getLogger("es.gob.afirma").addHandler( //$NON-NLS-1$
			new FileHandler(
				LOG_FILE.replace("%a", application.toString()), //$NON-NLS-1$
				LOG_MAX_SIZE,
				1,
				false
			)
		);
		installed = true;
	}

	/** Obtiene, en formato XML, el registro acumulado de la ejecuci&oacute;n actual.
	 * @return Registro acumulado de la ejecuci&oacute;n actual
	 * @throws IOException Si no hay registro o este no se puede leer */
	public static String getLogFile() throws IOException {
		if (!installed) {
			throw new IOException("No esta instalado el manejador de fichero"); //$NON-NLS-1$
		}
		final InputStream is = new FileInputStream(
			new File(
				LOG_FILE.replace("%h", Platform.getUserHome()).replace("%a", application.toString())  //$NON-NLS-1$//$NON-NLS-2$
			)
		);
		final String log = new String(AOUtil.getDataFromInputStream(is));
		is.close();
		return log;
	}

}
