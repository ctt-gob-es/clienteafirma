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

import static es.gob.afirma.install.BootPlatform.getEndorsedDir;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.logging.Logger;

import es.gob.afirma.install.BootPlatform.JREVER;
import es.gob.afirma.install.BootPlatform.OS;

/** Clase para la comprobaci&oacute;n e instalaci&oacute;n de las dependencias de
 * entorno del Cliente @firma. */
final class CheckAndInstallMissingParts {
    
    /** Gestor de registro. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$;

    private static final long serialVersionUID = -7508890516967067205L;

    /** JAR con las clases de compatibilidad del cliente con Java 5 (s&oacute;lo Java 5). */
    private static final String AFIRMA_JAVA5_JAR = "afirma_5_java_5.jar"; //$NON-NLS-1$

    /** Archivo zip con las librerias Xalan en formato PACK200 necesarias para la firma XML en JDK 5. */
    private static final String XALAN_LIBRARY_ZIP = "xalan.zip"; //$NON-NLS-1$

    /** Archivo zip con las librerias Xalan en formato JAR necesarias para la firma XML en JDK 5. */
    private static final String XALAN_JARLIBRARY_ZIP = "xalanjar.zip"; //$NON-NLS-1$

    private final String build;

    private final OS os;
    private final JREVER jreVersion;

    private final URL installFilesCodeBase;

    /** Crea el objeto para la comprobaci&oacute;n de los requisitos del entorno del usuario
     * necesarios para la ejecuci&oacute;n del Cliente y la instalaci&oacute;n de los mismos.
     * @param ost Tipo de sistema operativo.
     * @param jreVer Versi&oacute;n de la m&aacute;quina virtual.
     * @param instalDir Directorio de instalaci&oacute;n. */
    CheckAndInstallMissingParts(final OS ost, final JREVER jreVer, final String bld, final URL filesCodeBase) {
        if (filesCodeBase == null) {
            throw new IllegalArgumentException("Es necesario proporcionar una URL de descarga para los ficheros de instalacion"); //$NON-NLS-1$
        }
        this.installFilesCodeBase = filesCodeBase;
        this.os = ost;
        this.jreVersion = jreVer;
        this.build = bld;
    }

    /** Instala en el directorio endorsed del JRE en uso el pack de compatibilidad de firma
     * del cliente para la generacion de firmas XML con Java 5.
     * @param installFilesCodeBase Ruta en donde se encuentran los ficheros que se deben instalar.
     * @throws URISyntaxException Si las URI utilizadas no tienen una sintaxis v&aacute;lida
     * @throws IOException Si ocurre un error de entrada/salida */
    void installEndorsedJava5AFirmaDependencies() throws IOException, URISyntaxException {
        try {
            AOInstallUtils.copyFileFromURL(AOBootUtil.createURLFile(this.installFilesCodeBase, AFIRMA_JAVA5_JAR + AOInstallUtils.PACK200_SUFIX),
                                           new File(BootPlatform.getEndorsedDir(), AFIRMA_JAVA5_JAR + AOInstallUtils.PACK200_SUFIX));
            AOInstallUtils.unpack(BootPlatform.getEndorsedDir() + File.separator + AFIRMA_JAVA5_JAR + AOInstallUtils.PACK200_SUFIX);
        }
        catch (final Exception e) {
            LOGGER.warning(
               "No se ha podido instalar el paquete de compatibilidad con Java 5 en formato PACK200, se intentara en formato JAR: " + e //$NON-NLS-1$
            );
            // Borramos el Pack200 si se llego a copiar
            final File compPack200File = new File(BootPlatform.getEndorsedDir() + File.separator + AFIRMA_JAVA5_JAR + AOInstallUtils.PACK200_SUFIX);
            if (compPack200File.exists()) {
                try {
                    compPack200File.delete();
                }
                catch (final Exception ex) {
                    // Ignoramos los errores en el borrado, es responsabilidad del usuario limpiar periodicamente los temporales
                }
            }
            // Copiamos el JAR normal
            AOInstallUtils.copyFileFromURL(AOBootUtil.createURLFile(this.installFilesCodeBase, AFIRMA_JAVA5_JAR), new File(BootPlatform.getEndorsedDir()));
        }
    }

    /** Instala el proveedor de seguridad SunMSCAPI.
     * @param installFilesCodeBase Localizaci&oacute;n de los ficheros necesarios para la instalaci&oacute;n
     * @throws URISyntaxException Si las URI utilizadas no tienen una sintaxis v&aacute;lida
     * @throws IOException Si ocurre un error de entrada/salida */
    void installSunMSCAPI() throws IOException, URISyntaxException {
        // Copiamos el JAR de SunMSCAPI en el directorio de extensiones del JRE
        AOInstallUtils.installFile(
           AOBootUtil.createURLFile(this.installFilesCodeBase, "sunmscapi.jar"), //$NON-NLS-1$
           new File(BootPlatform.getJavaExtDir() + File.separator + "sunmscapi.jar"), //$NON-NLS-1$
           SigningCert.SUN
        );

        // Descomprimimos las DLL de SunMSCAPI en el directorio de binarios del JRE
        AOInstallUtils.installZip(
              AOBootUtil.createURLFile(
                   this.installFilesCodeBase,
                   "mscapi_" + BootPlatform.getOsArch() + "_JRE" + BootPlatform.getJavaArch() + ".zip" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
              ), 
              new File(BootPlatform.getJavaHome() + File.separator + "bin"), //$NON-NLS-1$
              SigningCert.INTEGRATOR
        );
    }

    /** Instala el proveedor de seguridad SunPKCS11.
     * @param installFilesCodeBase Localizaci&oacute;n de los ficheros necesarios para la instalaci&oacute;n
     * @throws URISyntaxException Si las URI utilizadas no tienen una sintaxis v&aacute;lida
     * @throws IOException Si ocurre un error de entrada/salida */
    void installSunPKCS11() throws IOException, URISyntaxException {
        // Copiamos el JAR de SunPKCS11 en el directorio de extensiones del JRE
        AOInstallUtils.installFile(
           AOBootUtil.createURLFile(this.installFilesCodeBase, "sunpkcs11.jar"), //$NON-NLS-1$
           new File(BootPlatform.getJavaExtDir() + File.separator + "sunpkcs11.jar"), //$NON-NLS-1$
           SigningCert.SUN
        );

        // Descomprimimos las DLL de SunPKCS11 en el directorio de binarios del JRE
        AOInstallUtils.installZip(AOBootUtil.createURLFile(this.installFilesCodeBase,
                                                           this.os.toString() + "_pkcs11lib_" + BootPlatform.getOsArch() + "_JRE" + BootPlatform.getJavaArch() + ".zip" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        ),
        new File(BootPlatform.getJavaHome() + File.separator + "bin"), //$NON-NLS-1$
        SigningCert.INTEGRATOR);
    }

    /** Instala las bibliotecas Xalan para la generaci&oacute;n de firmas XML desde Java 5.
     * @param installFilesCodeBase Ruta en donde se encuentra el fichero Zip con las bibliotecas que se desean instalar.
     * @throws URISyntaxException Si las URI utilizadas no tienen una sintaxis v&aacute;lida
     * @throws IOException Si ocurre un error de entrada/salida */
    void installEndorsedXalan() throws IOException, URISyntaxException {

        File tempDir = null;
        try {
            tempDir = AOInstallUtils.createTempFile(true);
            AOInstallUtils.installZip(AOBootUtil.createURLFile(this.installFilesCodeBase, XALAN_LIBRARY_ZIP), tempDir, SigningCert.INTEGRATOR);

            // Desempaquetamos los ficheros Pack200 del directorio temporal en el
            // directorio Endorsed de Java
            String filename;
            for (final File file : tempDir.listFiles()) {
                filename = file.getName();
                if (filename.endsWith(".pack.gz")) { //$NON-NLS-1$
                    AOInstallUtils.unpack(file.getAbsolutePath(),
                                          BootPlatform.getEndorsedDir() + File.separator + filename.substring(0, filename.lastIndexOf(".pack.gz"))); //$NON-NLS-1$
                }
            }
        }
        catch (final Exception e) {
            LOGGER.warning("No se ha podido instalar la version PACK200 de Xalan, se intentara la version JAR: " + e); //$NON-NLS-1$
            if (AfirmaBootLoader.DEBUG) {
                final java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                e.printStackTrace(new java.io.PrintStream(baos));
                LOGGER.warning(new String(baos.toByteArray()));
            }
            AOInstallUtils.installZip(AOBootUtil.createURLFile(this.installFilesCodeBase, XALAN_JARLIBRARY_ZIP),
                                      new File(getEndorsedDir()),
                                      SigningCert.INTEGRATOR);
        }
        finally {
            if (tempDir != null) {
                try {
                    for (final File file : tempDir.listFiles()) {
                        file.delete();
                    }
                    AOInstallUtils.deleteDir(tempDir);
                }
                catch (final Exception e) {
                 // Ignoramos los errores en el borrado, es responsabilidad del usuario limpiar periodicamente los temporales
                }
            }
        }
    }

    /** Indica si es necesario instalar el proveedor de seguridad SunMSCAPI.
     * @return <code>true</code> si es necesaria la instalaci&oacute;n, <code>false</code> en caso contrario */
    boolean isSunMSCAPINeeded() {
        if (!this.os.equals(OS.WINDOWS)) {
            return false;
        }
        try {
            AOBootUtil.getCleanClassLoader().loadClass("sun.security.mscapi.SunMSCAPI"); //$NON-NLS-1$
        }
        catch (final Throwable e) {
            return true;
        }
        return false;
    }

    /** Indica si es necesario instalar el proveedor de seguridad SunPKCS11.
     * @return <code>true</code> si es necesaria la instalaci&oacute;n, <code>false</code> en caso contrario */
    boolean isSunPKCS11Needed() {
        try {
            AOBootUtil.getCleanClassLoader().loadClass("sun.security.pkcs11.SunPKCS11"); //$NON-NLS-1$
        }
        catch (final Throwable e) {
            return true;
        }
        return false;
    }

    /** Comprueba si los paquetes XALAN son necesarios en el directorio endorsed del JRE
     * configurado.
     * @return Devuelve <code>true</code> si se necesita XALAN, <code>false</code> en caso contrario. */
    boolean isEndorsedXalanNeeded() {
        if (JREVER.J6.equals(this.jreVersion)) {
            return false;
        }
        if (Installer.LITE.equalsIgnoreCase(this.build)) {
            return false;
        }
        if (getEndorsedDir() == null) {
            LOGGER.severe("No se ha podido determinar el directorio ENDORSED del JRE, por lo que no se considera necesario instalar Apache XALAN"); //$NON-NLS-1$
            return false;
        }
        return !((new File(getEndorsedDir() + File.separator + "serializer.jar").exists() || new File(getEndorsedDir() + File.separator //$NON-NLS-1$
                                                                                                      + "serializer-2.7.1.jar").exists()) && (new File(getEndorsedDir() + File.separator //$NON-NLS-1$
                                                                                                                                                       + "xalan.jar").exists() || new File(getEndorsedDir() + File.separator //$NON-NLS-1$
                                                                                                                                                                                           + "xalan-2.7.1.jar").exists()) //$NON-NLS-1$
                                                                                                                                                                                           && (new File(getEndorsedDir() + File.separator + "xercesImpl.jar").exists() || new File(getEndorsedDir() + File.separator //$NON-NLS-1$
                                                                                                                                                                                                                                                                                   + "xercesImpl-2.9.1.jar").exists()) && (new File(getEndorsedDir() + File.separator //$NON-NLS-1$
                                                                                                                                                                                                                                                                                                                                    + "xml-apis.jar").exists() || new File(getEndorsedDir() + File.separator //$NON-NLS-1$
                                                                                                                                                                                                                                                                                                                                                                           + "xml-apis-1.3.03.jar").exists())); //$NON-NLS-1$
    }

    /** Comprueba si se necesitan las dependencias para la compatibilidad del Cliente AFirma con Java 5.
     * @return <code>true</code> si se necesita instalar las dependencias, <code>false</code> en caso contrario. */
    boolean isEndorsedJava5AFirmaDependenciesNeeded() {
        if (this.jreVersion.equals(JREVER.J6) || this.jreVersion.equals(JREVER.J7)) {
            return false;
        }
        if (new File(getEndorsedDir() + File.separator + AFIRMA_JAVA5_JAR).exists()) {
            return false;
        }
        return true;
    }

}
