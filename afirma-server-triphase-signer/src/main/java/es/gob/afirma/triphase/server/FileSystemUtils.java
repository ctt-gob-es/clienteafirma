package es.gob.afirma.triphase.server;

import java.io.File;
import java.io.IOException;

import es.gob.afirma.core.misc.LoggerUtil;

/**
 * M&eacute;todos de utilidad para el tratamiento de ficheros.
 */
public class FileSystemUtils {

	/**
	 * Compone un objeto fichero a partir de su ruta asegurando que esta dentro del directorio indicado.
	 * @param baseDir Directorio base del fichero ya canonicalizado.
	 * @param filename Nombre del fichero.
	 * @return Fichero de destino.
	 * @throws IOException Cuando no se pueda componer la ruta del fichero.
	 * @throws SecurityException Cuando se trate de cargar un fichero fuera
	 * del directorio base.
	 */
	public static File composeTargetFile(final File baseDir, final String filename)
			throws IOException, SecurityException {

		final File targetFile = new File(baseDir, filename).getCanonicalFile();
		if (!baseDir.getAbsolutePath().equals(targetFile.getParentFile().getAbsolutePath())) {
			throw new SecurityException("El fichero solicitado no esta en el raiz del directorio: " + LoggerUtil.getTrimStr(filename)); //$NON-NLS-1$
		}

		return targetFile;
	}
}
