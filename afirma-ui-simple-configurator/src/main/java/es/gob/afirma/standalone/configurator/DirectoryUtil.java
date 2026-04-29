package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.Platform;

public class DirectoryUtil {
	
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	
	private static final String LANGUAGES_DIRNAME = "languages"; //$NON-NLS-1$
	
    /**
     * Obtiene el directorio en el que se encuentran guardados o se deben
     * guardar los idiomas.
     * @return Directorio de idiomas.
     */
    public static File getLanguagesDir() {
		File appDir = getAlternativeDirectory();
		if (appDir == null) {
			appDir = getApplicationDirectory();
		}
		return new File(appDir, LANGUAGES_DIRNAME);
	}
    
	/**
	 * Recupera el directorio alternativo de la aplicaci&oacute;n, en el que
	 * se pueden almacenar los logs y recursos externos.
	 * @return Directorio alternativo de la aplicaci&oacute;n o {@code null} si
	 * no ha podido determinarse.
	 */
    private static File getAlternativeDirectory() {

		File appDir = null;
		if (Platform.getOS() == Platform.OS.WINDOWS) {
			appDir = getWindowsAlternativeAppDir();
		}
		else if (Platform.getOS() == Platform.OS.LINUX) {
			appDir = getLinuxAlternativeAppDir();
		}
		else if (Platform.getOS() == Platform.OS.MACOSX) {
			appDir = getMacOsXAlternativeAppDir();
		}
		return appDir;
	}
	
	/**
	 * Recupera el directorio de instalaci&oacute;n alternativo en los sistemas Windows.
	 * @return Directorio de instalaci&oacute;n.
	 */
	private static File getWindowsAlternativeAppDir() {
		final String commonDir = System.getenv("ALLUSERSPROFILE"); //$NON-NLS-1$
		return new File (commonDir, "Autofirma"); //$NON-NLS-1$
	}

	/**
	 * Recupera el directorio de instalaci&oacute;n alternativo en los sistemas Linux.
	 * @return Directorio de instalaci&oacute;n.
	 */
	private static File getLinuxAlternativeAppDir() {
		final String userHome = System.getProperty("user.home"); //$NON-NLS-1$
		return new File(userHome, ".afirma/Autofirma"); //$NON-NLS-1$
	}

	/**
	 * Recupera el directorio de instalaci&oacute;n alternativo en los sistemas macOS.
	 * @return Directorio de instalaci&oacute;n.
	 */
	private static File getMacOsXAlternativeAppDir() {
		final String userDir = System.getenv("HOME"); //$NON-NLS-1$
		return new File (userDir, "Library/Application Support/Autofirma"); //$NON-NLS-1$
	}
	
	private static File getApplicationDirectory() {

		if (isJnlpDeployment()) {
			return getJNLPApplicationDirectory();
		}

		// Identificamos el directorio de instalacion
		try {
			return new File(
				DirectoryUtil.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()
			).getParentFile();
		}
		catch (final URISyntaxException e) {
			LOGGER.warning("No se pudo localizar el directorio del fichero en ejecucion: " + e); //$NON-NLS-1$
		}

		return null;
	}
	
	/**
	 * Obtiene el directorio de aplicaci&oacute;n que corresponde cuando se
	 * ejecuta la aplicaci&oacute;n mediante un despliegue es JNLP.
	 * @return Directorio de aplicaci&oacute;n.
	 */
	private static File getJNLPApplicationDirectory() {
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
	
	/**
	 * Comprueba si estamos en un despliegue JNLP de la aplicaci&oacute;n.
	 * @return {@code true} si estamos en un despliegue JNLP, {@code false}
	 * en caso contrario.
	 */
	private static boolean isJnlpDeployment() {

		// Para comprobar si estamos en un despliegue JNLP sin crear una dependencia
		// con javaws, hacemos una llamada equivalente a:
		//     javax.jnlp.ServiceManager.lookup("javax.jnlp.ExtendedService");
		// Si falla la llamda, no estamos en un despliegue JNLP
		try {
			final Class<?> serviceManagerClass = Class.forName("javax.jnlp.ServiceManager"); //$NON-NLS-1$
			final Method lookupMethod = serviceManagerClass.getMethod("lookup", String.class); //$NON-NLS-1$
			lookupMethod.invoke(null, "javax.jnlp.ExtendedService"); //$NON-NLS-1$
		}
		catch (final Throwable e) {
			return false;
		}
		return true;
	}

}
