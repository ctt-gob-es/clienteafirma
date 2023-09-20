/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc;

import java.io.File;
import java.util.Locale;
import java.util.logging.Logger;

/** Clase para la identificaci&oacute;n de la plataforma Cliente y
 * extracci&oacute;n de datos relativos a la misma. */
public final class Platform {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Arquitectura del procesador. */
    public enum MACHINE {
    	/** x86 32 bits. */
    	X86,
    	/** ARM 32 bits. */
    	ARM32,
    	/** MIPS 32 bits. */
    	MIPS32,
    	/** AMD64 / x86_64 (64 bits). */
    	AMD64,
    	/** ARM 64 bits. */
    	ARM64,
    	/** MIPS 64 bits. */
    	MIPS64,
    	/** Otro */
    	OTHER
    }

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

    /** Sistema operativo. */
    private static OS os = null;

    /** Arquitectura de la m&eacute;quina virtual java. */
    private static String javaArch = null;

    /** Directorio de Java. */
    private static String javaHome = null;

    /** Directorio de bibliotecas de Java. */
    private static String javaLibraryPath = null;

    /** Directorio del usuario. */
    private static String userHome = null;

    /** Constructor bloqueado. */
    private Platform() {
        // No permitimos la instanciacion
    }

    /** Obtiene la arquitectura de procesador de la m&aacute;quina actual.
     * @return Arquitectura de procesador de la m&aacute;quina actual. */
    public static MACHINE getMachineType() {
    	final String osArch = System.getProperty("os.arch"); //$NON-NLS-1$
    	if (osArch == null) {
    		LOGGER.warning("No se ha podido determinar el la arquitectura del procesador"); //$NON-NLS-1$
            return MACHINE.OTHER;
    	}
    	if (osArch.equals("aarch64") || osArch.startsWith("arm64")) { //$NON-NLS-1$ //$NON-NLS-2$
    		return MACHINE.ARM64;
    	}
    	if (osArch.equals("mips64")) { //$NON-NLS-1$
    		return MACHINE.MIPS64;
    	}
    	if (osArch.equals("mips")) { //$NON-NLS-1$
    		return MACHINE.MIPS32;
    	}
    	if (osArch.equals("x86_64") || osArch.startsWith("amd64")) { //$NON-NLS-1$ //$NON-NLS-2$
    		return MACHINE.AMD64;
    	}
    	if (osArch.equals("x86")) { //$NON-NLS-1$
    		return MACHINE.X86;
    	}
    	LOGGER.warning("No se ha podido determinar el la arquitectura del procesador: " + osArch); //$NON-NLS-1$
        return MACHINE.OTHER;
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

    /** Recupera el navegador web actual.
     * @param userAgent <i>UserAgent</i> del navegador Web
     * @return Navegador web correspondiente al UserAgent indicado. */
    public static BROWSER getBrowser(final String userAgent) {
        if (userAgent == null) {
            return BROWSER.OTHER;
        }
        else if (userAgent.toLowerCase(Locale.US).contains("msie")) { //$NON-NLS-1$
            return BROWSER.INTERNET_EXPLORER;
        }
        else if (userAgent.toLowerCase(Locale.US).contains("firefox")) { //$NON-NLS-1$
            return BROWSER.FIREFOX;
        }
        else if (userAgent.toLowerCase(Locale.US).contains("chrome")) { //$NON-NLS-1$
            return BROWSER.CHROME;
        }
        else if (userAgent.toLowerCase(Locale.US).contains("safari")) { //$NON-NLS-1$
            // CUIDADO: Chrome incluye la cadena "safari" como parte de su
            // UserAgent
            return BROWSER.SAFARI;
        }
        else if (userAgent.toLowerCase(Locale.US).contains("opera")) { //$NON-NLS-1$
            return BROWSER.OPERA;
        }
        else { // Cualquier otro navegador
            return BROWSER.OTHER;
        }
    }

    /** Recupera el sistema operativo de ejecuci&oacute;n.
     * @return Sistema operativo actual. */
    public static Platform.OS getOS() {
        if (os == null) {
            os = recoverOsName();
        }
        return os;
    }

    /** Recupera la arquitectura de la JVM en ejecuci&oacute;n seg&uacute;n las
     * propiedades de Java.
     * @return Arquitectura de la JVM. */
    public static String getJavaArch() {
        if (javaArch == null) {
        	try {
	            javaArch = System.getProperty("sun.arch.data.model"); //$NON-NLS-1$
	            if (javaArch == null) {
	                javaArch = System.getProperty("com.ibm.vm.bitmode"); //$NON-NLS-1$
	            }
        	}
        	catch(final Exception e) {
        		LOGGER.severe(
    				"No se ha podido determinar si la JVM es de 32 o 64 bits, se asume que es 64: " + e //$NON-NLS-1$
				);
        		return "64"; //$NON-NLS-1$
        	}
        }
        return javaArch;
    }

    /** Recupera la ruta del directorio de instalaci&oacute;n de Java.
     * @return Ruta del directorio de instalaci&oacute;n de Java. */
    public static String getJavaHome() {
        if (javaHome == null) {
            javaHome = recoverJavaHome();
        }
        return javaHome;
    }

    /** Recupera la propiedad Path de Java.
     * @return Propiedad en el Path de Java. */
    public static String getJavaLibraryPath() {
        if (javaLibraryPath == null) {
            javaLibraryPath = System.getProperty("java.library.path"); //$NON-NLS-1$
        }
        return javaLibraryPath;
    }

    /** Recupera la ruta del directorio personal del usuario del sistema
     * operativo (no incluye nunca una barra final).
     * @return Ruta del directorio del usuario. */
    public static String getUserHome() {
        if (userHome == null) {
            userHome = System.getProperty("user.home"); //$NON-NLS-1$
        }
        return userHome;
    }

	/**
	 * Indica si el sistema operativo es Linux, macOS o Solaris.
	 * @param system Sistem operativo.
	 * @return {@code true} si el sistema operativo es de la familia de Unix, {@code false}.
	 */
	public static boolean isUnixSystem(final Platform.OS system) {
		return system == Platform.OS.LINUX || system == Platform.OS.MACOSX || system == Platform.OS.SOLARIS;
	}

    /** Obtiene el directorio de instalaci&oacute;n del entorno de
     * ejecuci&oacute;n de Java actualmente en uso. Si no se puede obtener, se
     * devolver&aacute; {@code null}. Copiado de com.sun.deploy.config.Config.
     * @return Directorio del entorno de ejecuci&oacute;n de Java. */
    private static String recoverJavaHome() {
        String ret = null;
        try {
            ret = System.getProperty("jnlpx.home"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            LOGGER.warning("No se ha podido leer la variable 'jnlpx.home': " + e); //$NON-NLS-1$
        }
        if (ret != null) {
            return ret.substring(0, ret.lastIndexOf(File.separator));
        }

        try {
            return System.getProperty("java.home"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            LOGGER.warning("No se ha podido identificar el directorio de Java: " + e); //$NON-NLS-1$
        }

        return null;
    }

    /** Obtiene el directorio principal del sistema operativo del sistema.
     * @return Directorio principal del sistema operativo */
    public static String getSystemRoot() {
        if (!Platform.getOS().equals(Platform.OS.WINDOWS)) {
            return File.separator;
        }
        final String systemRoot = System.getProperty("SystemRoot"); //$NON-NLS-1$
        if (systemRoot == null) {
            final String defaultSystemRoot = "C:\\WINDOWS"; //$NON-NLS-1$
            final File winSys32 = new File(defaultSystemRoot + "\\system32"); //$NON-NLS-1$
            if (winSys32.isDirectory()) {
                return defaultSystemRoot;
            }
        }
        if (systemRoot == null) {
            LOGGER
            .warning("No se ha encontrado el directorio ra&iacute;z del sistema, se devolver&aacute;: " + File.separator); //$NON-NLS-1$
            return File.separator;
        }
        return systemRoot;
    }

    /** Devuelve el directorio principal de bibliotecas del sistema.
     * @return Directorio principal de bibliotecas
     */
    public static String getSystemLibDir() {
        if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
            String systemRoot = getSystemRoot();
            if (systemRoot == null) {
                LOGGER .warning("No se ha podido determinar el directorio de Windows accediendo al registro, se usara 'C:\\WINDOWS\\'"); //$NON-NLS-1$
                systemRoot = "c:\\windows\\"; //$NON-NLS-1$
            }
            if (!systemRoot.endsWith("\\")) { //$NON-NLS-1$
                systemRoot += "\\"; //$NON-NLS-1$
            }
            if ("32".equals(Platform.getJavaArch())) { //$NON-NLS-1$
            	final File tmpFile = new File(systemRoot + "SysWOW64");  //$NON-NLS-1$
            	if (tmpFile.isDirectory()) {
            		return tmpFile.getAbsolutePath();
            	}
            }

            return systemRoot + "System32"; //$NON-NLS-1$
        }

        // ALL UNIX VARIANTS FOLLOWING FHS
        // There are, generally, two kind of Unix flavours:
        // a) Those whose libdir has explicit bit set: /usr/lib64 or /usr/lib32
        // b) Those whose libdir does not include it: /usr/lib
        //
        // In the case of X bits and /usr/libX exists, it is clear that we
        // are safe.
        // In the case of X bits and /usr/libX does not exist, it is probably
        // due to either the case:
        //     Platform is 64, and folders /usr/lib and /usr/lib32 exist
        // or
        //     Platform is 32, and folders /usr/lib and /usr/lib64 exist
        //
        // In any case, the *safe* route is to first try the a) case before b).

        final String canonicalLibDir = "/usr/lib" + Platform.getJavaArch(); //$NON-NLS-1$
        if (new File(canonicalLibDir).exists()) {
            return canonicalLibDir;
        }
        return "/usr/lib"; //$NON-NLS-1$
    }

}
