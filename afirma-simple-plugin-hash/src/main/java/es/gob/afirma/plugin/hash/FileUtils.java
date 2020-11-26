package es.gob.afirma.plugin.hash;

import java.io.File;
import java.io.IOException;
import java.util.logging.Logger;

/**
 * Clase con funciones auxiliares para el tratamiento de ficheros.
 */
public class FileUtils {

	private static final Logger LOGGER = Logger.getLogger(FileUtils.class.getName());

	/** Devuelve el fichero en su forma can&oacute;nica.
	 * @param file Fichero del cual obtener su forma can&oacute;nica.
	 * @return Fichero en su forma can&oacute;nica o el fichero de entrada si hay error.*/
	public static File getCanonicalFile(final File file) {
		try {
			return file.getCanonicalFile();
		}
		catch(final IOException e) {
			LOGGER.severe(
				"No se ha podido obtener el fichero canonico: " + e //$NON-NLS-1$
			);
			return file;
		}
	}
}
