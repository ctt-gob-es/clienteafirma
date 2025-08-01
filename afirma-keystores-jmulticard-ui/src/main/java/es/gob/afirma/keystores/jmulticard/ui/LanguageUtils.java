package es.gob.afirma.keystores.jmulticard.ui;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.util.Locale;
import java.util.logging.Logger;


public class LanguageUtils {
	
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	
	private static final String LANGUAGES_DIRNAME = "languages"; //$NON-NLS-1$	
    
	private static final String METADATA_FILENAME = "metadata.info"; //$NON-NLS-1$

	private static final String FALLBACK_LOCALE = "fallback.locale"; //$NON-NLS-1$
	
	public static final Locale [] AFIRMA_DEFAULT_LOCALES = {
	    	new Locale("es", "ES"), //$NON-NLS-1$ //$NON-NLS-2$     
	        new Locale("ca", "ES"), //$NON-NLS-1$ //$NON-NLS-2$
	        new Locale("gl", "ES"), //$NON-NLS-1$ //$NON-NLS-2$
	        new Locale("eu", "ES"), //$NON-NLS-1$ //$NON-NLS-2$
	        new Locale("va", "ES"),  //$NON-NLS-1$ //$NON-NLS-2$
	        new Locale("en", "US") //$NON-NLS-1$ //$NON-NLS-2$
	};
	
    /** Sistema operativo. */
    private static OS os = null;
	
    /** Sistema operativo. */
    public enum OS {
        /** Microsoft Windows. */
        WINDOWS,
        /** Linux. */
        LINUX,
        /** Sun Solaris / Open Solaris. */
        SOLARIS,
        /** Apple Mac OS X. */
        MACOSX,
        /** Google Android. */
        ANDROID,
        /** Sistema operativo no identificado. */
        OTHER
    }
	
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
		if (getOS() == OS.WINDOWS) {
			appDir = getWindowsAlternativeAppDir();
		}
		else if (getOS() == OS.LINUX) {
			appDir = getLinuxAlternativeAppDir();
		}
		else if (getOS() == OS.MACOSX) {
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
				LanguageUtils.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()
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
		if (getOS() == OS.WINDOWS) {
			final File appDir = getWindowsAlternativeAppDir();
			if (appDir.isDirectory() || appDir.mkdirs()) {
				return appDir;
			}
		}
		else if (getOS() == OS.MACOSX) {
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
	
	  /** Recupera el sistema operativo de ejecuci&oacute;n.
     * @return Sistema operativo actual. */
	private static OS getOS() {
        if (os == null) {
            os = recoverOsName();
        }
        return os;
    }
    
    private static OS recoverOsName() {

        final String osName = System.getProperty("os.name"); //$NON-NLS-1$

        if (osName.contains("indows")) { //$NON-NLS-1$
            return OS.WINDOWS;
        }
        else if (osName.contains("inux")) { //$NON-NLS-1$
            if ("Dalvik".equals(System.getProperty("java.vm.name"))) { //$NON-NLS-1$ //$NON-NLS-2$
                return OS.ANDROID;
            }
            return OS.LINUX;
        }
        else if (osName.contains("SunOS") || osName.contains("olaris")) { //$NON-NLS-1$ //$NON-NLS-2$
            return OS.SOLARIS;
        }
        else if (osName.startsWith("Mac OS X")) { //$NON-NLS-1$
            return OS.MACOSX;
        }
        else {
            LOGGER.warning("No se ha podido determinar el sistema operativo"); //$NON-NLS-1$
            return OS.OTHER;
        }
    }
    
    /**
     * Comprueba si existe una version importada por el usuario de los idiomas que ofrece Autofirma.
     * @return true en caso de que exista una version importada del idioma configurado.
     */
    public static boolean existDefaultLocaleNewVersion() {
		final File localeDir = new File(LanguageUtils.getLanguagesDir(), Locale.getDefault().getLanguage() + "_" + Locale.getDefault().getCountry()); //$NON-NLS-1$
        return localeDir.exists();
    }
    
	public static Locale readMetadataBaseLocale(final Locale locale) throws IOException {
        Locale result = null;
        final File localeDir = new File(LanguageUtils.getLanguagesDir(), locale.getLanguage() + "_" + locale.getCountry()); //$NON-NLS-1$
    	final File metadataFile = new File(localeDir, METADATA_FILENAME);
        try (FileInputStream fis = new FileInputStream(metadataFile);
        	BufferedReader reader = new BufferedReader(new InputStreamReader(fis))) {
            String line;
            while ((line = reader.readLine()) != null) {
            	final String[] parts = line.split("=", 2); //$NON-NLS-1$
            	if (FALLBACK_LOCALE.equals(parts[0])) {
            		final String baseLocale = parts[1];
            		final String[] localeParts = baseLocale.split("_", 2); //$NON-NLS-1$
            		result = new Locale(localeParts[0], localeParts[1]);
            	}
            }
        } catch (final IOException e) {
        	throw new IOException("Error al leer el archivo " + metadataFile, e); //$NON-NLS-1$
        }
        return result;
	}
	
    /**
     * Comprueba si el idioma pertenece a alguno de los que ofrece Autofirma por defecto.
     * @param locale Idioma a comprobar.
     * @return true en caso de que sea un idioma de los que proporciona Autofirma.
     */
    public static boolean isDefaultLocale(final Locale locale) {
    	boolean result = false;
    	for (final Locale l : LanguageUtils.AFIRMA_DEFAULT_LOCALES) {
    		if (l.equals(locale)) {
    			result = true;
    		}
    	}
    	return result;
    }

}
