/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.core.misc;

import java.io.File;
import java.util.logging.Logger;

/** Clase para la identificaci&oacute;n de la plataforma Cliente y
 * extracci&oacute;n de datos relativos a la misma. */
public final class Platform {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    
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

    /** Navegador Web. */
    public enum BROWSER {
        /** Microsoft internet Explorer. */
        INTERNET_EXPLORER,
        /** Mozilla Firefox. */
        FIREFOX,
        /** Google Chrome. */
        CHROME,
        /** Apple Safari. */
        SAFARI,
        /** Opera. */
        OPERA,
        /** Navegador Web no identificado. */
        OTHER
    }

    /** Version del entorno de ejecuci&oacute;n de Java. */
    public enum JREVER {
        /** Java 4 y anteriores. */
        J4,
        /** Java 5. */
        J5,
        /** Java 6. */
        J6,
        /** Java 7. */
        J7
    }

    /** Directorio endorsed de la distribuci&oacute;n Java. */
    private static String endorsedDir = null;

    /** Sistema operativo. */
    private static OS os;

    /** Versi&oacute;n del sistema operativo. */
    private static String osVersion;

    /** Versi&oacute;n de la m&eacute;quina virtual de java. */
    private static JREVER javaVersion;

    /** Arquitectura de la m&eacute;quina virtual java. */
    private static String javaArch;

    /** Arquitectura del sistema operativo. */
    private static String osArch;

    /** Directorio de Java. */
    private static String javaHome;

    /** Directorio de bibliotecas de Java. */
    private static String javaLibraryPath;

    /** Directorio del usuario. */
    private static String userHome;

    /** Navegador web. */
    private static BROWSER browser = BROWSER.OTHER;

    /** Indica si est&aacute; inicializada la clase. */
    private static boolean initialized = false;

    /** Constructor bloqueado. */
    private Platform() {
        // No permitimos la instanciacion
    }

    /*
     * Se realiza una inicializacion automatica con las propiedades de la
     * plataforma. Si falla la incializacion automatica el usuario debera
     * reintentarlo manualmente. Esto puede ocurrir, por ejemplo, en applets en
     * donde no se controlan los priilegios.
     */
    static {
        init();
    }

    /** Fuerza la identificaci&oacute;n de la plataforma. Inicializa los valores
     * que pueden reconocerse sin informacion adicional, como el sistema
     * operativo y la versi&oacute;n de Java. Se agrupan las inicializaciones
     * aqu&iacute; para poder llamarlo desde un <code>privilegedAction</code> */
    public static void init() {
        try {
            osVersion = System.getProperty("os.version"); //$NON-NLS-1$
            userHome = System.getProperty("user.home"); //$NON-NLS-1$
            javaLibraryPath = System.getProperty("java.library.path"); //$NON-NLS-1$
            final String osName = System.getProperty("os.name"); //$NON-NLS-1$

            if (osName.contains("indows")) { //$NON-NLS-1$
                os = OS.WINDOWS;
            }
            else if (osName.contains("inux")) { //$NON-NLS-1$
                if ("Dalvik".equals(System.getProperty("java.vm.name"))) { //$NON-NLS-1$ //$NON-NLS-2$
                    os = OS.ANDROID;
                }
                else {
                    os = OS.LINUX;
                }
            }
            else if (osName.contains("SunOS") || osName.contains("olaris")) { //$NON-NLS-1$ //$NON-NLS-2$
                os = OS.SOLARIS;
            }
            else if (osName.startsWith("Mac OS X")) { //$NON-NLS-1$
                os = OS.MACOSX;
            }
            else {
                os = OS.OTHER;
                LOGGER.warning("No se ha podido determinar el sistema operativo"); //$NON-NLS-1$
            }

            javaArch = System.getProperty("sun.arch.data.model"); //$NON-NLS-1$
            if (javaArch == null) {
                javaArch = System.getProperty("com.ibm.vm.bitmode"); //$NON-NLS-1$
            }
            javaHome = recoverJavaHome();
            initialized = true;
            final String jreVersion = System.getProperty("java.version"); //$NON-NLS-1$
            if (jreVersion.startsWith("1.0") || jreVersion.startsWith("1.1") //$NON-NLS-1$ //$NON-NLS-2$
                || jreVersion.startsWith("1.2") //$NON-NLS-1$
                || jreVersion.startsWith("1.3") //$NON-NLS-1$
                || jreVersion.startsWith("1.4")) { //$NON-NLS-1$
                javaVersion = JREVER.J4;
            }
            else if (jreVersion.startsWith("1.5")) { //$NON-NLS-1$
                javaVersion = JREVER.J5;
            }
            else if (jreVersion.startsWith("1.6")) { //$NON-NLS-1$
                javaVersion = JREVER.J6;
            }
            else {
                javaVersion = JREVER.J7;
            }
            osArch = System.getProperty("os.arch"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            initialized = false;
        }
    }

    /** Indica si la instancia est&aacute;tica est&aacute; inicializada.
     * @return <code>true</code> si si la instancia est&aacute;tica est&aacute;
     *         inicializada, <code>false</code> en caso contrario */
    public static boolean isInitialized() {
        return initialized;
    }

    /** Recupera el navegador web actual.
     * @return Navegador web desde el que se realiza la ejecuci&oacute;n o {@code null} si no se realiza desde un navegador. */
    public static BROWSER getBrowser() {
        return browser;
    }

    /** Establece el UserAgent del navegador desde el que se carga el applet.
     * @param userAgent
     *        UserAgent del aplicativo de acceso. */
    public static void setUserAgent(final String userAgent) {

        if (userAgent == null) {
            browser = BROWSER.OTHER;
        }
        else if (userAgent.toLowerCase().contains("msie")) { //$NON-NLS-1$
            browser = BROWSER.INTERNET_EXPLORER;
        }
        else if (userAgent.toLowerCase().contains("firefox")) { //$NON-NLS-1$
            browser = BROWSER.FIREFOX;
        }
        else if (userAgent.toLowerCase().contains("chrome")) { //$NON-NLS-1$
            browser = BROWSER.CHROME;
        }
        else if (userAgent.toLowerCase().contains("safari")) { //$NON-NLS-1$
            // CUIDADO: Chrome incluye la cadena "safari" como parte de su
            // UserAgent
            browser = BROWSER.SAFARI;
        }
        else if (userAgent.toLowerCase().contains("opera")) { //$NON-NLS-1$
            browser = BROWSER.OPERA;
        }
        else { // Cualquier otro navegador
            browser = BROWSER.OTHER;
        }
    }

    /** Recupera el sistema operativo de ejecuci&oacute;n.
     * @return Sistema operativo actual. */
    public static Platform.OS getOS() {
        return os;
    }

    /** Recupera la versi&oacute; del n&uacute;cleo del sistema operativo
     * seg&uacute;n las propiedades de Java.
     * @return Versi&oacute;n del sistema operativo actual. */
    public static String getOsVersion() {
        return osVersion;
    }

    /** Recupera la arquitectura del sistema operativo seg&uacute;n las
     * propiedades de Java.
     * @return Arquitectura del sistema operativo actual. */
    public static String getOsArch() {
        return osArch;
    }

    /** Recupera la versi&oacute;n de la JVM en ejecuci&oacute;n seg&uacute;n las
     * propiedades de Java.
     * @return Versi&oacute;n de la JVM. */
    public static JREVER getJavaVersion() {
        return javaVersion;
    }

    /** Recupera la arquitectura de la JVM en ejecuci&oacute;n seg&uacute;n las
     * propiedades de Java.
     * @return Arquitectura de la JVM. */
    public static String getJavaArch() {
        return javaArch;
    }

    /** Recupera la ruta del directorio de instalaci&oacute;n de Java.
     * @return Ruta del directorio de instalaci&oacute;n de Java. */
    public static String getJavaHome() {
        return javaHome;
    }

    /** Recupera la propiedad Path de Java.
     * @return Propiedad en el Path de Java. */
    public static String getJavaLibraryPath() {
        return javaLibraryPath;
    }

    /** Recupera la ruta del directorio personal del usuario del sistema
     * operativo.
     * @return Ruta del directorio del usuario. */
    public static String getUserHome() {
        return userHome;
    }

    /** Obtiene el directorio de instalaci&oacute;n del entorno de
     * ejecuci&oacute;n de Java actualmente en uso. Si no se puede obtener, se
     * devolver&aacute; {@code null}. Copiado de com.sun.deploy.config.Config.
     * @return Directorio del entorno de ejecuci&oacute;n de Java. */
    private final static String recoverJavaHome() {
        String ret = null;
        try {
            ret = System.getProperty("jnlpx.home"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            // Se ignora, puede que no haya permisos para leerla
        }
        if (ret != null) {
            return ret.substring(0, ret.lastIndexOf(File.separator));
        }

        try {
            return System.getProperty("java.home"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            LOGGER.warning("No se ha podido identificar el directorio de java"); //$NON-NLS-1$
        }

        return null;
    }

    /** Recupera el directorio "endorsed" de la JRE usada para la
     * instalaci&oacute;n o <code>null</code> si no se pudo determinar
     * ning&uacute;n directorio.
     * @return Directorio "endorsed". */
    public static String getEndorsedDir() {

        if (endorsedDir != null) {
            return endorsedDir;
        }

        final String endorsedDirsString = System.getProperty("java.endorsed.dirs"); //$NON-NLS-1$
        String[] endorsedDirs;
        if (endorsedDirsString != null && (endorsedDirs = endorsedDirsString.split(System.getProperty("path.separator"))).length > 0) { //$NON-NLS-1$
            endorsedDir = endorsedDirs[0];
        }

        // Si no se ha asignado, acudimos al directorio por defecto
        // ($JAVA_HOME/lib/endorsed)
        if (endorsedDir == null) {
            endorsedDir = getJavaHome() + File.separator + "lib" + File.separator + "endorsed"; //$NON-NLS-1$ //$NON-NLS-2$
        }

        return endorsedDir;
    }
}
