package es.gob.afirma.keystores.mozilla;

import java.io.File;
import java.io.InputStream;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.Platform;

/** Prueba de obtenci&oacute;n de nombre corto en Windows.
 * @author Tom&aacute;s Garc&iacute;-Mer&aacute;s Capote. */
public final class TestGetShort {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Obtiene el nombre corto (8+3) de un fichero o directorio indicado (con ruta).
	 * @param originalPath Ruta completa hacia el fichero o directorio que queremos pasar a nombre corto.
	 * @return Nombre corto del fichero o directorio con su ruta completa, o la cadena originalmente indicada si no puede
	 *         obtenerse la versi&oacute;n corta */
	private static String getShort(final String originalPath) {
		if (originalPath == null || !Platform.OS.WINDOWS.equals(Platform.getOS())) {
			return originalPath;
		}
		final File dir = new File(originalPath);
		if (!dir.exists()) {
			return originalPath;
		}
		final String[] command = new String[] { "cmd.exe", "/c", "for %f in (\"" + originalPath + "\") do @echo %~sf" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		System.out.print("Se ejecutara: "); //$NON-NLS-1$
		for (final String s : command) {
			System.out.print(s + " "); //$NON-NLS-1$
		}
		System.out.println();
		try {
			final Process p = new ProcessBuilder(
				command
			).start();
			try (
				final InputStream is = p.getInputStream()
			) {
				return new String(AOUtil.getDataFromInputStream(is)).trim();
			}
		}
		catch(final Exception e) {
			LOGGER.warning("No se ha podido obtener el nombre corto de " + LoggerUtil.getCleanUserHomePath(originalPath) + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return originalPath;
	}

	/** Main para pruebas.
	 * @param args No se usa. */
	public static void main(final String[] args) {
		final String path = "c:\\Program Files (x86)\\Microsoft Silverlight\\5.1.41212.0\\agcore.debug.dll"; //$NON-NLS-1$
		System.out.println("Nombre largo: " + path); //$NON-NLS-1$
		System.out.println("Nombre corto: " + getShort(path)); //$NON-NLS-1$
	}

}
