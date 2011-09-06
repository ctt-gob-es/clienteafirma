/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.install;

import static es.gob.afirma.misc.Platform.getEndorsedDir;

import java.awt.Component;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.script.ScriptEngineManager;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOBootUtil;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.misc.Platform.JREVER;
import es.gob.afirma.misc.Platform.OS;
import es.gob.afirma.misc.WinRegistryWrapper;

/** Clase para la comprobaci&oacute;n e instalaci&oacute;n de las dependencias de
 * entorno del Cliente @firma. */
final class CheckAndInstallMissingParts {

    private static final long serialVersionUID = -7508890516967067205L;

    /** JAR con las clases de compatibilidad del cliente con Java 5 (s&oacute;lo Java 5). */
    private static final String AFIRMA_JAVA5_JAR = "afirma_5_java_5.jar";

    /** Archivo zip con las librerias Xalan en formato PACK200 necesarias para la firma XML en JDK 5. */
    private static final String XALAN_LIBRARY_ZIP = "xalan.zip";

    /** Archivo zip con las librerias Xalan en formato JAR necesarias para la firma XML en JDK 5. */
    private static final String XALAN_JARLIBRARY_ZIP = "xalanjar.zip";

    /** Archivo zip con las librerias Apache XMLSec en formato PACK200 necesarias para la firma XML en JDK 7. */
    private static final String XMLSEC_LIBRARY_ZIP = "xmlsec.zip";

    /** Archivo zip con las librerias Apache XMLSec en formato JAR necesarias para la firma XML en JDK 7. */
    private static final String XMLSEC_JARLIBRARY_ZIP = "xmlsecjar.zip";

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
            throw new IllegalArgumentException("Es necesario proporcionar una URL de descarga para los ficheros de instalacion");
        }
        this.installFilesCodeBase = filesCodeBase;
        this.os = ost;
        this.jreVersion = jreVer;
        this.build = bld;
    }

    /** Instala las bibliotecas de NSS para el acceso al almac&eacute;n de Mozilla Firefox.
     * @param installFilesCodeBase Ruta con las bibliotecas a instalar.
     * @throws AOException Cuando se produce un error durante la instalaci&oacute;n de las bibliotecas. */
    void installNSS() throws AOException {
        final File nssDir = new File(Platform.getUserHome() + File.separator + Installer.INSTALL_DIR + File.separator + "nss" + //$NON-NLS-1$
                                     Platform.getJavaArch());

        try {
            AOInstallUtils.installZip(AOBootUtil.createURLFile(this.installFilesCodeBase, this.os.toString() + "_nss_"
                                                               + Platform.getOsArch()
                                                               + "_JRE"
                                                               + Platform.getJavaArch()
                                                               + ".zip"),
                                                               nssDir,
                                                               SigningCert.INTEGRATOR);
        }
        catch (final Exception e) {
            throw new AOException("No se puede copiar NSS al directorio por defecto, compruebe que no existe previamente y que se cuenta con los permisos adecuados: " + e, e //$NON-NLS-1$
            );
        }
    }

    /** Instala en el directorio endorsed del JRE en uso el pack de compatibilidad de firma
     * del cliente para la generacion de firmas XML con Java 5.
     * @param installFilesCodeBase Ruta en donde se encuentran los ficheros que se deben instalar.
     * @throws URISyntaxException Si las URI utilizadas no tienen una sintaxis v&aacute;lida
     * @throws AOException Si ocurre cualquier otro error durante la copia de ficheros
     * @throws IOException Si ocurre un error de entrada/salida */
    void installEndorsedJava5AFirmaDependencies() throws IOException, AOException, URISyntaxException {
        try {
            AOInstallUtils.copyFileFromURL(AOBootUtil.createURLFile(this.installFilesCodeBase, AFIRMA_JAVA5_JAR + AOInstallUtils.PACK200_SUFIX),
                                           new File(Platform.getEndorsedDir(), AFIRMA_JAVA5_JAR + AOInstallUtils.PACK200_SUFIX));
            AOInstallUtils.unpack(Platform.getEndorsedDir() + File.separator + AFIRMA_JAVA5_JAR + AOInstallUtils.PACK200_SUFIX);
        }
        catch (final Exception e) {
            AfirmaBootLoader.LOGGER.warning( //$NON-NLS-1$
                                                       "No se ha podido instalar el paquete de compatibilidad con Java 5 en formato PACK200, se intentara en formato JAR: " + e //$NON-NLS-1$
            );
            // Borramos el Pack200 si se llego a copiar
            final File compPack200File = new File(Platform.getEndorsedDir() + File.separator + AFIRMA_JAVA5_JAR + AOInstallUtils.PACK200_SUFIX);
            if (compPack200File.exists()) {
                try {
                    compPack200File.delete();
                }
                catch (final Exception ex) {}
            }
            // Copiamos el JAR normal
            AOInstallUtils.copyFileFromURL(AOBootUtil.createURLFile(this.installFilesCodeBase, AFIRMA_JAVA5_JAR), new File(Platform.getEndorsedDir()));
        }
    }

/** Instala las bibliotecas Apache XMLSec para la generaci&oacute;n de firmas XML desde Java 7.
     * @param installFilesCodeBase Ruta en donde se encuentra el fichero Zip con las bibliotecas que se desean instalar. */
    void installEndorsedApacheXMLSec() throws AOException, IOException, URISyntaxException {
        File tempDir = null;
        try {
            tempDir = AOInstallUtils.createTempFile(true);
            AOInstallUtils.installZip(AOBootUtil.createURLFile(installFilesCodeBase, XMLSEC_LIBRARY_ZIP), tempDir, SigningCert.INTEGRATOR);

            String filename;
            for (final File file : tempDir.listFiles()) {
                filename = file.getName();
                if (filename.endsWith(".pack.gz")) { //$NON-NLS-1$
                    AOInstallUtils.unpack(file.getAbsolutePath(),
                                          Platform.getEndorsedDir() + File.separator + filename.substring(0, filename.lastIndexOf(".pack.gz")));
                }
            }

        }
        catch (final Exception e) {
            AfirmaBootLoader.LOGGER
                  .warning("No se ha podido instalar la version PACK200 de Apache XMLSec, se intentara la version JAR: " + e);
            if (AfirmaBootLoader.DEBUG) {
                final java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                e.printStackTrace(new java.io.PrintStream(baos));
                AfirmaBootLoader.LOGGER.warning(new String(baos.toByteArray()));
            }
            AOInstallUtils.installZip(AOBootUtil.createURLFile(installFilesCodeBase, XMLSEC_JARLIBRARY_ZIP),
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
                    // Ignoramos los errores en el borrado
                }
            }
        }

    }

    /** Instala el proveedor de seguridad SunMSCAPI.
     * @param installFilesCodeBase Localizaci&oacute;n de los ficheros necesarios para la instalaci&oacute;n
     * @throws URISyntaxException Si las URI utilizadas no tienen una sintaxis v&aacute;lida
     * @throws AOException Si ocurre cualquier otro error durante la copia de ficheros
     * @throws IOException Si ocurre un error de entrada/salida */
    void installSunMSCAPI() throws IOException, AOException, URISyntaxException {
        // Copiamos el JAR de SunMSCAPI en el directorio de extensiones del JRE
        AOInstallUtils.installFile(AOBootUtil.createURLFile(this.installFilesCodeBase, "sunmscapi.jar"), //$NON-NLS-1$
                                   new File(Platform.getJavaHome() + File.separator
                                            + "lib" + File.separator + "ext" + File.separator + "sunmscapi.jar"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                                            SigningCert.SUN);

        // Descomprimimos las DLL de SunMSCAPI en el directorio de binarios del JRE
        AOInstallUtils.installZip(AOBootUtil.createURLFile(this.installFilesCodeBase,
                                                           "mscapi_" + Platform.getOsArch() + "_JRE" + Platform.getJavaArch() + ".zip" //$NON-NLS-1$ //$NON-NLS-2$
        ), new File(Platform.getJavaHome() + File.separator + "bin"), //$NON-NLS-1$
        SigningCert.INTEGRATOR);
    }

    /** Instala el proveedor de seguridad SunPKCS11.
     * @param installFilesCodeBase Localizaci&oacute;n de los ficheros necesarios para la instalaci&oacute;n
     * @throws URISyntaxException Si las URI utilizadas no tienen una sintaxis v&aacute;lida
     * @throws AOException Si ocurre cualquier otro error durante la copia de ficheros
     * @throws IOException Si ocurre un error de entrada/salida */
    void installSunPKCS11() throws IOException, AOException, URISyntaxException {
        // Copiamos el JAR de SunPKCS11 en el directorio de extensiones del JRE
        AOInstallUtils.installFile(AOBootUtil.createURLFile(this.installFilesCodeBase, "sunpkcs11.jar"), //$NON-NLS-1$
                                   new File(Platform.getJavaHome() + File.separator
                                            + "lib" + File.separator + "ext" + File.separator + "sunpkcs11.jar"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                                            SigningCert.SUN);

        // Descomprimimos las DLL de SunPKCS11 en el directorio de binarios del JRE
        AOInstallUtils.installZip(AOBootUtil.createURLFile(this.installFilesCodeBase,
                                                           this.os.toString() + "_pkcs11lib_" + Platform.getOsArch() + "_JRE" + Platform.getJavaArch() + ".zip" //$NON-NLS-1$ //$NON-NLS-2$
        ),
        new File(Platform.getJavaHome() + File.separator + "bin"), //$NON-NLS-1$
        SigningCert.INTEGRATOR);
    }

    /** Instala las bibliotecas Xalan para la generaci&oacute;n de firmas XML desde Java 5.
     * @param installFilesCodeBase Ruta en donde se encuentra el fichero Zip con las bibliotecas que se desean instalar.
     * @throws URISyntaxException Si las URI utilizadas no tienen una sintaxis v&aacute;lida
     * @throws AOException Si ocurre cualquier otro error durante la copia de ficheros
     * @throws IOException Si ocurre un error de entrada/salida */
    void installEndorsedXalan() throws IOException, AOException, URISyntaxException {

        File tempDir = null;
        try {
            tempDir = AOInstallUtils.createTempFile(true);
            AOInstallUtils.installZip(AOBootUtil.createURLFile(this.installFilesCodeBase, XALAN_LIBRARY_ZIP), tempDir, SigningCert.INTEGRATOR);

            // Desempaquetamos los ficheros Pack200 del directorio temporal en el
            // directorio Endorsed de Java
            String filename;
            for (final File file : tempDir.listFiles()) {
                filename = file.getName();
                if (filename.endsWith(".pack.gz")) {
                    AOInstallUtils.unpack(file.getAbsolutePath(),
                                          Platform.getEndorsedDir() + File.separator + filename.substring(0, filename.lastIndexOf(".pack.gz")));
                }
            }
        }
        catch (final Exception e) {
            AfirmaBootLoader.LOGGER.warning("No se ha podido instalar la version PACK200 de Xalan, se intentara la version JAR: " + e);
            if (AfirmaBootLoader.DEBUG) {
                final java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                e.printStackTrace(new java.io.PrintStream(baos));
                AfirmaBootLoader.LOGGER.warning(new String(baos.toByteArray()));
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
                catch (final Exception e) {}
            }
        }
    }

    /** Configura NSS en Mac OS X para permitir la carga desde directorios fuera de
     * <i>LD_LIBRARY_PATH</i> y <i>PATH</i>.
     * @param parent Componente padre para los di&aacute;logos gr&aacute;ficos */
    void configureNSS(final Component parent) throws AOException {
        if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
            return;
        }
        String nssLibDir;
        try {
            nssLibDir = getSystemNSSLibDirMacOSX();
        }
        catch (final Exception e) {
            AfirmaBootLoader.LOGGER.severe("No se ha encontrado un NSS para configurar: " + e); //$NON-NLS-1$
            return;
        }

        if (!nssLibDir.endsWith("/")) {
            nssLibDir = nssLibDir + "/"; //$NON-NLS-1$
        }

        final String[] libs = new String[] {
            "libnspr4.dylib", //$NON-NLS-1$
            "libplds4.dylib", //$NON-NLS-1$
            "libplc4.dylib", //$NON-NLS-1$
            "libmozsqlite3.dylib", //$NON-NLS-1$
            "libnssutil3.dylib" //$NON-NLS-1$
        };

        // Creamos enlaces simbolicos via AppleScript
        final StringBuilder sb = new StringBuilder();
        for (final String lib : libs) {
            sb.append("ln -s ");
            sb.append(nssLibDir);
            sb.append(lib);
            sb.append(" /usr/lib/");
            sb.append(lib);
            sb.append("; ");
        }
        try {
            new ScriptEngineManager().getEngineByName("AppleScript").eval("do shell script \"" + sb.toString() + "\" with administrator privileges");    
        }
        catch(final Exception e) {
            AfirmaBootLoader.LOGGER.severe("No se ha podido crear los enlaces simbolicos para NSS: " + e);
        }

        // Y reintentamos la carga, para ver si ha surtido efecto
        try {
            System.load(nssLibDir + "libsoftokn3.dylib");
        }
        catch (final Throwable e) {
            if (AfirmaBootLoader.DEBUG) {
                final java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                e.printStackTrace(new java.io.PrintStream(baos));
                AfirmaBootLoader.LOGGER.warning(new String(baos.toByteArray()));
            }
            throw new AOException("La configuracion de NSS para Mac OS X ha fallado", e);
        }

    }

    /** Indica si es necesario instalar el proveedor de seguridad SunMSCAPI.
     * @return <code>true</code> si es necesaria la instalaci&oacute;n, <code>false</code> en caso contrario */
    boolean isSunMSCAPINeeded() {
        if (!this.os.equals(OS.WINDOWS)) {
            return false;
        }
        try {
            Class.forName("sun.security.mscapi.SunMSCAPI"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            return true;
        }
        return false;
    }

    /** Indica si es necesario instalar el proveedor de seguridad SunPKCS11.
     * @return <code>true</code> si es necesaria la instalaci&oacute;n, <code>false</code> en caso contrario */
    boolean isSunPKCS11Needed() {
        try {
            Class.forName("sun.security.pkcs11.SunPKCS11"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            return true;
        }
        return false;
    }

    /** Indica si es necesario instalar una copia local de las biliotecas <i>NSS</i>.
     * @return <code>true</code> si es necesaria la instalaci&oacute;n, <code>false</code> en caso contrario */
    boolean isNSSNeeded() {

        String nssDir = null;
        String nssLib = null;

        // En Windows solo en Firefox
        if (this.os.equals(OS.WINDOWS)) {
            // Buscamos en el registro el directorio normal de NSS en Windows
            String dir = WinRegistryWrapper.getString(WinRegistryWrapper.HKEY_CURRENT_USER, "Software\\Classes\\FirefoxURL\\shell\\open\\command", //$NON-NLS-1$
                                                      "" //$NON-NLS-1$
            );
            // Segundo intento, ahora el LOCAL_MACHINE
            if (dir == null) {
                dir =
                    WinRegistryWrapper.getString(WinRegistryWrapper.HKEY_LOCAL_MACHINE, "SOFTWARE\\Classes\\FirefoxURL\\shell\\open\\command", //$NON-NLS-1$
                                                 "" //$NON-NLS-1$
                    );
            }

            if (dir != null) {
                final String regKeyLowCase = dir.toLowerCase();
                final int pos = regKeyLowCase.indexOf("firefox.exe"); //$NON-NLS-1$
                if (pos != -1) {
                    dir = dir.substring(0, pos);
                    if (dir.startsWith("\"")) {
                        dir = dir.substring(1);
                    }
                    if (dir.endsWith(File.separator)) {
                        dir = dir.substring(0, dir.length() - 1);
                    }
                    File tmpFile = new File(dir);
                    if (tmpFile.exists() && tmpFile.isDirectory() && tmpFile.canRead()) {
                        tmpFile = new File(dir + File.separator + "softokn3.dll"); //$NON-NLS-1$
                        if (tmpFile.exists()) {
                            nssDir = tmpFile.getParentFile().getAbsolutePath();
                            nssLib = "mozcrt19.dll"; //$NON-NLS-1$
                        }
                    }
                }
            }
        }
        else if (this.os.equals(OS.MACOSX)) {
            final String[] paths = new String[] {
                                                 "/Applications/Firefox.app/Contents/MacOS", "/lib", "/usr/lib", "/usr/lib/nss", "/Applications/Minefield.app/Contents/MacOS"
            };
            for (final String path : paths) {
                if (new File(path + "/libsoftokn3.dylib").exists()) {
                    nssDir = path;
                    break;
                }
            }
            if (nssDir != null)
            {
                nssLib = "libnspr4.dylib"; //$NON-NLS-1$
            }
        }

        else if (this.os.equals(OS.LINUX) || this.os.equals(OS.SOLARIS)) {

            final String[] paths = new String[] {
                                                 "/usr/lib/firefox-" + searchLastFirefoxVersion("/usr/lib/"), //$NON-NLS-1$ //$NON-NLS-2$
                                                 "/usr/lib/firefox", //$NON-NLS-1$
                                                 "/opt/firefox", //$NON-NLS-1$
                                                 "/opt/firefox-" + searchLastFirefoxVersion("/opt/"), //$NON-NLS-1$ //$NON-NLS-2$
                                                 "/lib", //$NON-NLS-1$
                                                 "/usr/lib", //$NON-NLS-1$
                                                 "/usr/lib/nss", //$NON-NLS-1$
                                                 "/opt/fedora-ds/clients/lib" //$NON-NLS-1$
            };

            for (final String path : paths) {
                if (new File(path + "/libsoftokn3.so").exists() && new File(path + "/libnspr4.so").exists()) {
                    nssDir = path;
                }
                if (nssDir != null) {
                    break;
                }
            }

            if (nssDir != null)
            {
                nssLib = "libnspr4.so"; //$NON-NLS-1$
            }
        }

        // Si nssDir no es null es que hay un NSS previamente instalado en el sistema,
        // asi que intentamos cargarlo a ver si la arquitectura es la buena.
        // Probamos con nssLib, que debe ser una biblioteca sin dependencias
        if (nssDir != null && nssLib != null) {
            try {
                System.load(nssDir + File.separator + nssLib);
                return false;
            }
            catch (final Throwable e) {
                if (AfirmaBootLoader.DEBUG) {
                    AfirmaBootLoader.LOGGER.info("No se ha podido cargar NSS: " + e);
                }
            }
        }

        // Consideramos que si esta instalado el NSS de dentro funciona
        return !isNSSInstalled();
    }

    /** Comprueba si hay una copia local de NSS instalada por el Cliente Afirma.
     * @return Devuelve {@code true} si el NSS del Cliente est&aacute; instalado, {@code false} en caso contrario. */
    private boolean isNSSInstalled() {
        // Miramos si AFirma tiene
        // instalada una copia local propia de NSS
        final File localNSS = new File(Platform.getUserHome() + File.separator + Installer.INSTALL_DIR + File.separator + "nss" + //$NON-NLS-1$
                                       Platform.getJavaArch());

        // Si existe el directorio, doy por bueno que el NSS de dentro me vale
        return localNSS.exists() && localNSS.isDirectory() && localNSS.canRead() && isNSSUpdated();
    }

    /** Indica si la version de NSS instalada en local es la misma que se encuentra publicada
     * junto con el Cliente para ese entorno. Se presupone que es necesaria la instalacion
     * de NSS. Si no esta instalada, se instalar&aacute; y si no se puede comprobar la version,
     * se utilizara la actual.
     * @param installFilesCodeBase Ruta remota con los archivos de instalaci&oacute;n.
     * @return {@code true} si la version instalada de NSS esta actualizada, {@code false} en
     *         caso contrario. */
    private boolean isNSSUpdated() {
        final String localVersion;
        try {
            localVersion = getNssLocalVersion();
        }
        catch (final FileNotFoundException e) {
            AfirmaBootLoader.LOGGER.warning("No se ha localizado el NSS instalado");
            return false;
        }
        catch (final Exception e) {
            AfirmaBootLoader.LOGGER
            .warning("No se pudo recuperar la version instalada de NSS en el directorio del Cliente, se considerara actualizado: " + e);
            return true;
        }

        final String remoteVersion;
        try {
            remoteVersion = getNssRemoteVersion();
        }
        catch (final Exception e) {
            AfirmaBootLoader.LOGGER
            .warning("No se pudo recuperar la version instalada de NSS en el directorio del Cliente, se considerara actualizado: " + e);
            return true;
        }

        return localVersion.compareTo(remoteVersion) > -1;
    }

    /** Recupera la versi&oacute;n del NSS instalado para soportar el acceso al almac&eacute;n de Firefox
     * del Cliente Afirma.
     * @return Identificador completo de la versi&oacute;n (Por ejemplo: 'x.y.z').
     * @throws IOException Cuando no existe el directorio de NSS local o el fichero de versi&oacute;n
     *         (FileNotFoundException) o cuando se produce un error durante la lectura (IOException). */
    private String getNssLocalVersion() throws IOException {
        // Accedemos al directorio local
        final File localNSS = new File(Platform.getUserHome() + File.separator + Installer.INSTALL_DIR + File.separator + "nss" + //$NON-NLS-1$
                                       Platform.getJavaArch());

        if (!localNSS.exists() || !localNSS.isDirectory()) {
            throw new FileNotFoundException("El directorio local de NSS no existe: " + localNSS.getAbsolutePath());
        }

        final File versionFile = new File(localNSS, "version.txt");
        if (!versionFile.exists() || !versionFile.isFile()) {
            throw new FileNotFoundException("El fichero de version de NSS no existe: " + versionFile.getAbsolutePath());
        }

        return this.getVersion(versionFile);
    }

    /** Recupera la versi&oacute;n del NSS instalado para soportar el acceso al almac&eacute;n de Firefox
     * del Cliente Afirma.
     * @return Identificador completo de la versi&oacute;n (Por ejemplo: 'x.y.z').
     * @throws IOException Cuando no existe el directorio de NSS local o el fichero de versi&oacute;n
     *         (FileNotFoundException) o cuando se produce un error durante la lectura (IOException). */
    private String getNssRemoteVersion() throws IOException {
        final File versionTempFile = AOInstallUtils.createTempFile();
        try {
            AOInstallUtils.copyFileFromURL(
                                           AOBootUtil.createURLFile(
                                                                    this.installFilesCodeBase,
                                                                    this.os.toString() + "_nss_" + Platform.getOsArch() + "_JRE" + Platform.getJavaArch() + ".txt"),
                                                                    versionTempFile);
        }
        catch (final Exception e) {
            throw new IOException("No se pudo comprobar la version remota de NSS: " + e);
        }

        try {
            return this.getVersion(versionTempFile);
        }
        finally {
            try {
                versionTempFile.delete();
            }
            catch (final Exception e) {}
        }
    }

    /** Recupera un codigo corto de versi&oacute;n de un fichero.
     * @return Identificador completo de la versi&oacute;n (Por ejemplo: 'x.y.z').
     * @throws IOException Cuando se produce un error durante la lectura. */
    private String getVersion(final File versionFile) throws IOException {
        final String version;
        InputStream fis = null;
        try {
            fis = new FileInputStream(versionFile);
            final byte[] buffer = new byte[10];
            final int n = fis.read(buffer);
            version = new String(buffer, 0, n);
        }
        catch (final Exception e) {
            throw new IOException("Error al leer el fichero de version: " + e);
        }
        finally {
            if (fis != null) {
                try {
                    fis.close();
                }
                catch (final Exception e) {}
            }
        }
        return version;
    }

    /** Indica si las bibliotecas NSS necesitan un proceso adicional de configuraci&oacute;n.
     * @return <code>true</code> si es necesaria configuraci&oacute;n adicional, <code>false</code> en caso contrario */
    boolean isNSSConfigurationNeeded() {
        // IMPORTANTE: Comprobar que el tener NSS 64 bits en /usr/lib no
        // estropea un Firefox 32 bits
        if (!Platform.OS.MACOSX.equals(Platform.getOS())) {
            return false;
        }

        try {
            String nssLibDir = getSystemNSSLibDirMacOSX();
            if (!nssLibDir.endsWith("/")) { //$NON-NLS-1$
                nssLibDir = nssLibDir + "/"; //$NON-NLS-1$
            }
            System.load(nssLibDir + "libsoftokn3.dylib");
        }
        catch (final Exception e) {
            if (AfirmaBootLoader.DEBUG) {
                final java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                e.printStackTrace(new java.io.PrintStream(baos));
                AfirmaBootLoader.LOGGER.warning(new String(baos.toByteArray()));
            }
            return true;
        }
        catch (final Throwable e) {
            if (AfirmaBootLoader.DEBUG) {
                final java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                e.printStackTrace(new java.io.PrintStream(baos));
                AfirmaBootLoader.LOGGER.warning(new String(baos.toByteArray()));
            }
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
            AfirmaBootLoader.LOGGER
            .severe("No se ha podido determinar el directorio ENDORSED del JRE, por lo que no se considera necesario instalar Apache XALAN");
            return false;
        }
        return !((new File(getEndorsedDir() + File.separator + "serializer.jar").exists() || new File(getEndorsedDir() + File.separator
                                                                                                      + "serializer-2.7.1.jar").exists()) && (new File(getEndorsedDir() + File.separator
                                                                                                                                                       + "xalan.jar").exists() || new File(getEndorsedDir() + File.separator
                                                                                                                                                                                           + "xalan-2.7.1.jar").exists())
                                                                                                                                                                                           && (new File(getEndorsedDir() + File.separator + "xercesImpl.jar").exists() || new File(getEndorsedDir() + File.separator
                                                                                                                                                                                                                                                                                   + "xercesImpl-2.9.1.jar").exists()) && (new File(getEndorsedDir() + File.separator
                                                                                                                                                                                                                                                                                                                                    + "xml-apis.jar").exists() || new File(getEndorsedDir() + File.separator
                                                                                                                                                                                                                                                                                                                                                                           + "xml-apis-1.3.03.jar").exists()));
    }

	/** Comprueba si los paquetes Apache XMLSec son necesarios en el directorio endorsed del JRE
     * configurado.
     * @return Devuelve <code>true</code> si se necesita Apache XMLSec, <code>false</code> en caso contrario. */
    boolean isEndorsedApacheXMLSecNeeded() {
        if (!Platform.JREVER.J7.equals(jreVersion)) {
            return false;
        }
        return !(new File(getEndorsedDir() + File.separator + "xmlsec-1.4.4.jar").exists() && new File(getEndorsedDir() + File.separator
                                                                                                       + "commons-logging-api-1.1.jar").exists());
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

    /** Busca la &uacute;ltima versi&oacute;n de firefox instalada en un sistema Linux o Solaris
     * @return &Uacute;ltima versi&oacute;n instalada en el sistema */
    private String searchLastFirefoxVersion(final String path) {
        final File directoryLib = new File(path);
        if (directoryLib.isDirectory()) {
            final String filenames[] = directoryLib.list();
            final List<String> firefoxDirectories = new ArrayList<String>();
            for (final String filename : filenames) {
                if (filename.startsWith("firefox-")) {
                    firefoxDirectories.add(filename.replace("firefox-", ""));
                }
            }
            if (firefoxDirectories.isEmpty()) {
                return "";
            }
            for (int i = 0; i < firefoxDirectories.size(); i++) {
                try {
                    Integer.getInteger(firefoxDirectories.get(i));
                }
                catch (final Exception e) {
                    firefoxDirectories.remove(i);
                }
            }
            if (firefoxDirectories.size() == 1) {
                return firefoxDirectories.get(0);
            }
            Collections.sort(firefoxDirectories, new Comparator<String>() {
                /** {@inheritDoc} */
                public int compare(final String o1, final String o2) {
                    return o1.compareTo(o2);
                }
            });
            return firefoxDirectories.get(0);
        }
        return "";
    }

    private String getSystemNSSLibDirMacOSX() throws FileNotFoundException {

        String nssLibDir = null;

        if (new File("/Applications/Firefox.app/Contents/MacOS/libsoftokn3.dylib").exists() && new File("/Applications/Firefox.app/Contents/MacOS/libnspr4.dylib").exists()) {
            try {
                System.load("/Applications/Firefox.app/Contents/MacOS/libnspr4.dylib");
                nssLibDir = "/Applications/Firefox.app/Contents/MacOS";
            }
            catch (final Throwable e) {
                nssLibDir = null;
            }
        }
        if (nssLibDir == null && new File("/lib/libsoftokn3.dylib").exists() && new File("/lib/libnspr4.dylib").exists()) {
            try {
                System.load("/lib/libnspr4.dylib");
                nssLibDir = "/lib";
            }
            catch (final Throwable e) {
                nssLibDir = null;
            }
        }
        if (nssLibDir == null && new File("/usr/lib/libsoftokn3.dylib").exists() && new File("/usr/lib/libnspr4.dylib").exists()) {
            try {
                System.load("/usr/lib/libnspr4.dylib");
                nssLibDir = "/usr/lib";
            }
            catch (final Throwable e) {
                nssLibDir = null;
            }
        }
        if (nssLibDir == null && new File("/usr/lib/nss/libsoftokn3.dylib").exists() && new File("/usr/lib/nss/libnspr4.dylib").exists()) {
            try {
                System.load("/usr/lib/nss/libnspr4.dylib");
                nssLibDir = "/usr/lib/nss";
            }
            catch (final Throwable e) {
                nssLibDir = null;
            }
        }
        if (nssLibDir == null && new File("/Applications/Minefield.app/Contents/MacOS/libsoftokn3.dylib").exists()
                && new File("/Applications/Minefield.app/Contents/MacOS/libnspr4.dylib").exists()) {
            try {
                System.load("/Applications/Minefield.app/Contents/MacOS/libnspr4.dylib");
                nssLibDir = "/Applications/Minefield.app/Contents/MacOS";
            }
            catch (final Throwable e) {
                nssLibDir = null;
            }
        }
        if (nssLibDir == null && new File(Platform.getUserHome() + File.separator
                                          + Installer.INSTALL_DIR
                                          + File.separator
                                          + "nss"
                                          + Platform.getJavaArch()
                                          + File.separator
                                          + "libsoftokn3.dylib").exists()
                                          && new File(Platform.getUserHome() + File.separator
                                                      + Installer.INSTALL_DIR
                                                      + File.separator
                                                      + "nss"
                                                      + Platform.getJavaArch()
                                                      + File.separator
                                                      + "libnspr4.dylib").exists()) {
            try {
                System.load(Platform.getUserHome() + File.separator
                            + Installer.INSTALL_DIR
                            + File.separator
                            + "nss"
                            + Platform.getJavaArch()
                            + File.separator
                            + "libnspr4.dylib");
                nssLibDir = Platform.getUserHome() + File.separator + Installer.INSTALL_DIR + File.separator + "nss" + Platform.getJavaArch();
            }
            catch (final Throwable e) {
                nssLibDir = null;
            }
        }

        if (nssLibDir == null) {
            throw new FileNotFoundException("No se ha podido determinar la localizacion de NSS en Mac OS X");
        }

        return nssLibDir;
    }
    
    public static void main(String args[]) throws Exception {
        new CheckAndInstallMissingParts(Platform.OS.MACOSX, Platform.JREVER.J6, null, new URL("http://www.google.com")).configureNSS(null);
    }

}
