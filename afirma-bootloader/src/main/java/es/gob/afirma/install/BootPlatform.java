/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.install;

import java.io.File;
import java.util.logging.Logger;

/** Clase para la identificaci&oacute;n de la plataforma Cliente y
 * extracci&oacute;n de datos relativos a la misma. */
final class BootPlatform {

    /** Gestor de registro. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$;
    
    /** Sistema operativo. */
    enum OS {
        /** Microsoft Windows. */
        WINDOWS,
        /** Linux. */
        LINUX,
        /** Sun Solaris / Open Solaris. */
        SOLARIS,
        /** Apple Mac OS X. */
        MACOSX,
        /** Sistema operativo no identificado. */
        OTHER
    }

    /** Version del entorno de ejecuci&oacute;n de Java. */
    enum JREVER {
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
    private static OS os = null;

    /** Versi&oacute;n de la m&eacute;quina virtual de java. */
    private static JREVER javaVersion = null;

    /** Arquitectura de la m&eacute;quina virtual java. */
    private static String javaArch = null;

    /** Arquitectura del sistema operativo. */
    private static String osArch = null;

    /** Directorio de Java. */
    private static String javaHome = null;

    /** Directorio del usuario. */
    private static String userHome = null;
    
    /** Directorio de extensiones del JRE. */
    private static String javaExtDir = null;

    /** Constructor bloqueado. */
    private BootPlatform() {
        // No permitimos la instanciacion
    }

    /** Recupera el sistema operativo de ejecuci&oacute;n.
     * @return Sistema operativo actual. */
    static BootPlatform.OS getOS() {
        if (os == null) {
            os = recoverOsName();
        }
        return os;
    }

    /** Recupera la arquitectura del sistema operativo seg&uacute;n las
     * propiedades de Java.
     * @return Arquitectura del sistema operativo actual. */
    static String getOsArch() {
        if (osArch == null) {
            osArch = System.getProperty("os.arch"); //$NON-NLS-1$
        }
        return osArch;
    }

    /** Recupera la versi&oacute;n de la JVM en ejecuci&oacute;n seg&uacute;n las propiedades de Java.
     * @return Versi&oacute;n de la JVM. */
    static JREVER getJavaVersion() {
        if (javaVersion == null) {
            final String jreVersion = System.getProperty("java.version"); //$NON-NLS-1$
            if (jreVersion.startsWith("1.0") || jreVersion.startsWith("1.1")  //$NON-NLS-1$//$NON-NLS-2$
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
        }
        return javaVersion;
    }

    /** Recupera la arquitectura de la JVM en ejecuci&oacute;n seg&uacute;n las propiedades de Java.
     * @return Arquitectura de la JVM. */
    static String getJavaArch() {
        if (javaArch == null) {
            javaArch = System.getProperty("sun.arch.data.model"); //$NON-NLS-1$
            if (javaArch == null) {
                javaArch = System.getProperty("com.ibm.vm.bitmode"); //$NON-NLS-1$
            }
        }
        return javaArch;
    }

    /** Recupera la ruta del directorio de instalaci&oacute;n de Java.
     * @return Ruta del directorio de instalaci&oacute;n de Java. */
    static String getJavaHome() {
        if (javaHome == null) {
            javaHome = recoverJavaHome();
        }
        return javaHome;
    }

    /** Recupera la ruta del directorio personal del usuario del sistema operativo.
     * @return Ruta del directorio del usuario. */
    static String getUserHome() {
        if (userHome == null) {
            userHome = System.getProperty("user.home"); //$NON-NLS-1$
        }
        return userHome;
    }

    private static OS recoverOsName() {
        final String osName = System.getProperty("os.name"); //$NON-NLS-1$
        if (osName.contains("indows")) { //$NON-NLS-1$
            return OS.WINDOWS;
        }
        else if (osName.contains("inux")) { //$NON-NLS-1$
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
    
    /** Obtiene el directorio de instalaci&oacute;n del entorno de ejecuci&oacute;n de Java
     * actualmente en uso. Si no se puede obtener, se devolver&aacute; {@code null}.
     * Copiado de com.sun.deploy.config.Config.
     * @return Directorio del entorno de ejecuci&oacute;n de Java. */
    private static String recoverJavaHome() {
        String ret = null;
        try {
            ret = System.getProperty("jnlpx.home"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            // Ignoramos los errores
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

    /** Recupera el directorio "endorsed" de la JRE usada para la instalaci&oacute;n o <code>null</code> si no se pudo determinar ning&uacute;n
     * directorio.
     * @return Directorio "endorsed". */
    static String getEndorsedDir() {
        if (endorsedDir == null) {
            final String endorsedDirsString = System.getProperty("java.endorsed.dirs"); //$NON-NLS-1$
            final String[] endorsedDirs;
            if (endorsedDirsString != null && (endorsedDirs = endorsedDirsString.split(System.getProperty("path.separator"))).length > 0) { //$NON-NLS-1$
                endorsedDir = endorsedDirs[0];
            }
            // Si no se ha asignado, acudimos al directorio por defecto ($JAVA_HOME/lib/endorsed)
            if (endorsedDir == null) {
                endorsedDir = getJavaHome() + File.separator + "lib" + File.separator + "endorsed"; //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        return endorsedDir;
    }
    
    /** Obtiene el directorio de extensiones del entorno de ejecuci&oacute;n de Java en uso.
     * @return Directorio de extensiones del JRE o {@code null} si no se pudo identificar */
    static String getJavaExtDir() {
        final File extDir = new File(getJavaHome() + (getJavaHome().endsWith(File.separator) ? "" : File.separator) + "lib" + File.separator + "ext"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        if (extDir.exists() && extDir.isDirectory()) {
            javaExtDir = extDir.getAbsolutePath();
        }
        return javaExtDir;
    }
    
}
