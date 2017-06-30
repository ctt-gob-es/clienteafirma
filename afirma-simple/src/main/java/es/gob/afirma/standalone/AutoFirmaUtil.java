package es.gob.afirma.standalone;

import java.awt.Image;
import java.awt.Toolkit;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URISyntaxException;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.ui.hash.CheckHashDialog;

/** Utilidades generales y de control del autoarranque de AutoFirma en el inicio de Windows.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class AutoFirmaUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final Image ICON = Toolkit.getDefaultToolkit().getImage(
		CheckHashDialog.class.getResource("/resources/afirma_ico.png") //$NON-NLS-1$
	);

	/** Obtiene el icono por defecto para los di&aacute;logos gr&aacute;fcos.
	 * @return Icono por defecto para los di&aacute;logos gr&aacute;fcos. */
	public static Image getDefaultDialogsIcon() {
		return ICON;
	}

	/** Recupera el directorio en el que se encuentra la aplicaci&oacute;n.
	 * @return Directorio de ejecuci&oacute;n. */
	public static File getApplicationDirectory() {

		if (isJnlpDeployment()) {
			if (Platform.getOS() == Platform.OS.WINDOWS) {
				final File appDir = getWindowsAlternativeAppDir();
				if (appDir.isDirectory() || appDir.mkdirs()) {
					return appDir;
				}
			}
			else if (Platform.getOS() == Platform.OS.MACOSX) {
				final File appDir = getMacOsXAlternativeAppDir();
				if (appDir.isDirectory() || appDir.mkdirs()) {
					return appDir;
				}
			}
			return new File(System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
		}

		// Identificamos el directorio de instalacion
		try {
			return new File(
				AutoFirmaUtil.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()
			).getParentFile();
		}
		catch (final URISyntaxException e) {
			LOGGER.warning("No se pudo localizar el directorio del fichero en ejecucion: " + e); //$NON-NLS-1$
		}

		return null;
	}

	/** Recupera el directorio de instalaci&oacute;n alternativo en los sistemas Windows.
	 * @return Directorio de instalaci&oaucte;n. */
	public static File getWindowsAlternativeAppDir() {
		final String commonDir = System.getenv("ALLUSERSPROFILE"); //$NON-NLS-1$
		return new File (commonDir, "AutoFirma"); //$NON-NLS-1$
	}

	/**
	 * Recupera el directorio de instalaci&oacute;n alternativo en los sistemas Linux.
	 * @return Directorio de instalaci&oaucte;n.
	 */
	public static File getLinuxAlternativeAppDir() {
		final String userHome = System.getProperty("user.home"); //$NON-NLS-1$
		return new File(userHome, ".afirma/AutoFirma"); //$NON-NLS-1$
	}

	/**
	 * Recupera el directorio de instalaci&oacute;n alternativo en los sistemas macOS.
	 * @return Directorio de instalaci&oaucte;n.
	 */
	public static File getMacOsXAlternativeAppDir() {
		final String userDir = System.getenv("HOME"); //$NON-NLS-1$
		return new File (userDir, "Library/Application Support/AutoFirma"); //$NON-NLS-1$
	}

	/**
	 * Comprueba si estamos en un despliegue JNLP de la aplicaci&oacute;n.
	 * @return {@code true} si estamos en un despliegue JNLP, {@code false}
	 * en caso contrario.
	 */
	private static boolean isJnlpDeployment() {
		try {
			javax.jnlp.ServiceManager.lookup("javax.jnlp.ExtendedService"); //$NON-NLS-1$
		}
		catch (final Throwable e) {
			return false;
		}
		return true;
	}

	/** Recupera el DPI de la pantalla principal.
	 * @return DPI de la pantalla principal. */
	public static int getDPI() {
		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			final String[] cmd = {"wmic", "desktopmonitor", "get", "PixelsPerXLogicalInch"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			final ProcessBuilder builder = new ProcessBuilder(cmd);
			try {
				final Process process = builder.start();
				process.waitFor();
				try (
					final BufferedReader bufferedReader = new BoundedBufferedReader(
						new InputStreamReader(process.getInputStream())
					);
				) {
					String line;
					int dpi = 0;
					while ((line = bufferedReader.readLine()) != null) {
						try {
							dpi = Integer.parseInt(line.trim());
							break;
						}
						catch (final Exception e) {
							continue;
						}
		            }
		            return dpi;
				}
			}
			catch (final Exception e) {
				LOGGER.log(	Level.SEVERE, "Error obteniendo DPI: " + e); //$NON-NLS-1$
				return 0;
			}
		}
		return 0;
	}

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
