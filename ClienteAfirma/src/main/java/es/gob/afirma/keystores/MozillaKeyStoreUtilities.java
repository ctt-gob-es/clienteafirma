/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.keystores;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import es.atosorigin.exe.PEParser;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.keystores.AOSecMod.ModuleName;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.misc.WinRegistryWrapper;
import es.gob.afirma.misc.mozilla.utils.NSPreferences;

/** Clase con m&eacute;toos de utilidad para la gesti&oacute;n del almac&eacute;n
 * de certificados de Mozilla. */
public final class MozillaKeyStoreUtilities {

    /** Directorio con las bibliotecas de NSS necesarias para el acceso al
     * almac&eacute;n de Mozilla. */
    private static String nssLibDir = null;

    private final static String NSS_INSTALL_DIR = ".cafirma"; //$NON-NLS-1$

    /** Crea las l&iacute;neas de configuraci&oacute;n para el uso de las
     * bibliotecas NSS como m&oacute;dulo PKCS#11 por el proveedor de Sun.
     * @param userProfileDirectory
     *        Directorio donde se encuentra el perfil de usuario de Mozilla
     *        Firefox
     * @param libDir
     *        Directorio que contiene las bibliotecas NSS
     * @return Fichero con las propiedades de configuracion del proveedor
     *         PKCS#11 de Sun para acceder al KeyStore de Mozilla v&iacute;a
     *         NSS. */
    static String createPKCS11NSSConfigFile(final String userProfileDirectory, final String libDir) {

        String softoknLib = "libsoftokn3.so";
        if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
            softoknLib = "softokn3.dll";
        }
        else if (Platform.getOS().equals(Platform.OS.MACOSX)) {
            softoknLib = "libsoftokn3.dylib";
        }

        final StringBuilder buffer = new StringBuilder("name=NSSCrypto-AFirma\r\n");

        // Java 1.5 tenia un metodo indocumentado para acceder a NSS,
        // http://docs.sun.com/app/docs/doc/819-3671/gcsoc?a=view

        // if (System.getProperty("java.version").startsWith("1.5")) {
        buffer.append("library=")
              .append(libDir)
              .append(java.io.File.separator)
              .append(softoknLib)
              .append("\r\n")
              .append("attributes=compatibility\r\n")
              .append("slot=2\r\n")
              .append("showInfo=false\r\n")
              .append("nssArgs=\"")
              .append("configdir='")
              .append(userProfileDirectory)
              .append("' ")
              .append("certPrefix='' ")
              .append("keyPrefix='' ")
              .append("secmod='secmod.db' ")
              .append("flags=readOnly")
              .append("\"\n");
        // //.append("omitInitialize=true");
        // }
        // else {
        // Inicializacion segun Java 6
        // buffer
        // .append("nssLibraryDirectory=").append(libDir).append("\r\n")
        // .append("nssSecmodDirectory=\"").append(userProfileDirectory).append("\"\r\n")
        // //.append("nssUseSecmod=true\r\n") // No es necesario, con usar
        // nssLibraryDirectory o nssSecModDirectory ya se activa el modo NSS
        // .append("nssDbMode=readOnly\r\n")
        // .append("attributes=compatibility\r\n")
        // .append("nssModule=keystore\r\n")
        // //.append("allowSingleThreadedModules=true\r\n");
        // //.append("nssNetscapeDbWorkaround=true\r\n") // Solo si necesitamos
        // crear claves privadas
        // //.append("showInfo=true\r\n")
        //
        // ;
        // }

        // Logger.getLogger("es.gob.afirma").info("Configuracion SunPKCS11 para NSS:\n"
        // + buffer.toString());

        return buffer.toString();

    }

    private static String getSystemNSSLibDirWindows() throws FileNotFoundException, AOInvalidFormatException {

        // Intentamos extraer la ruta de instalacion de Firefox del registro
        String dir = WinRegistryWrapper.getString(WinRegistryWrapper.HKEY_CURRENT_USER, "Software\\Classes\\FirefoxURL\\shell\\open\\command", "");
        if (dir == null) {
            dir = WinRegistryWrapper.getString(WinRegistryWrapper.HKEY_LOCAL_MACHINE, "SOFTWARE\\Classes\\FirefoxURL\\shell\\open\\command", "");
            if (dir == null) {
                throw new FileNotFoundException("No se ha podido localizar el directorio de Firefox a traves del registro de Windows");
            }
        }

        final String regKeyLowCase = dir.toLowerCase();
        int pos = regKeyLowCase.indexOf("firefox.exe");
        if (pos != -1) {
            dir = dir.substring(0, pos);
            if (dir.startsWith("\"")) {
                dir = dir.substring(1);
            }
            if (dir.endsWith(File.separator)) {
                dir = dir.substring(0, dir.length() - 1);
            }

            File tmpFile = new File(dir);
            if (tmpFile.exists() && tmpFile.isDirectory()) {
                tmpFile = new File(dir + File.separator + "softokn3.dll");
                if (tmpFile.exists()) {
                    try {
                        Logger.getLogger("es.gob.afirma").info("Informacion sobre el modulo PKCS#11 de NSS");
                        InputStream is = new FileInputStream(tmpFile);
                        new PEParser().parse(AOUtil.getDataFromInputStream(is), Logger.getLogger("es.gob.afirma"));
                        try {
                            is.close();
                        }
                        catch (final Exception e) {}
                    }
                    catch (final Exception e) {
                        Logger.getLogger("es.gob.afirma").warning("No se ha podido obtener informacion sobre el modulo PKCS#11 de NSS" + e);
                    }
                    try {
                        dir = tmpFile.getParentFile().getCanonicalPath();
                    }
                    catch (final Exception e) {
                        if (dir.contains("\u007E")) {
                            throw new FileNotFoundException("No se ha podido obtener el nombre del directorio del modulo PKCS#11, parece estar establecido como un nombre corto (8+3): " + e);
                        }
                    }

                    // Tenemos NSS en el sistema, comprobamos arquitectura
                    // cargando una de las bibliotecas
                    try {
                        final String[] libs = getSoftkn3Dependencies(dir);
                        if (libs != null && libs.length > 0) {
                            System.load(libs[0]);
                        }
                    }
                    catch (final Exception e) {
                        final File localNSS = new File(Platform.getUserHome() + "\\" + NSS_INSTALL_DIR + "\\nss" + Platform.getJavaArch());
                        if (localNSS.exists() && localNSS.isDirectory() && localNSS.canRead()) {
                            Logger.getLogger("es.gob.afirma").info("Existe una copia local de NSS");
                            // Existe un NSS local
                            try {
                                dir = localNSS.getCanonicalPath();
                            }
                            catch (final Exception t) {
                                dir = localNSS.getAbsolutePath();
                                throw new FileNotFoundException("No se ha encontrado un NSS compatible en el sistema: " + t);
                            }
                        }
                        throw new AOInvalidFormatException("El NSS del sistema (" + dir
                                                           + ") es de una arquitectura incompatible "
                                                           + "y no existe una version alternativa en "
                                                           + localNSS.getAbsolutePath()
                                                           + ": "
                                                           + e);
                    }

                    // Tenemos la ruta del NSS, comprobamos adecuacion por bugs
                    // de Java

                    if (dir.contains(")") || dir.contains("(") || dir.contains("\u007E")) {
                        // Tenemos una ruta con caracteres ilegales para la
                        // configuracion de SunPKCS#11 por el bug 6581254:
                        // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6581254
                        try {

                            // Copiamos las DLL necesarias a un temporal y
                            // devolvemos el temporal
                            File tmp = File.createTempFile("nss", null);
                            tmp.delete();
                            if (!tmp.mkdir()) {
                                throw new AOException("No se ha creado el directorio temporal");
                            }
                            String dest = tmp.getCanonicalPath() + File.separator;

                            // Las cuatro primeras bibliotecas son comunes para
                            // Firefox 2, 3 y 4
                            AOUtil.copyFile(new File(dir + File.separator + "softokn3.dll"), new File(dest + "softokn3.dll"));

                            AOUtil.copyFile(new File(dir + File.separator + "plc4.dll"), new File(dest + "plc4.dll"));
                            AOUtil.copyFile(new File(dir + File.separator + "plds4.dll"), new File(dest + "plds4.dll"));
                            AOUtil.copyFile(new File(dir + File.separator + "nspr4.dll"), new File(dest + "nspr4.dll"));

                            // A partir de aqui comprobamos exsitencia antes,
                            // porque no estan en Mozilla 2

                            // Cuidado, en Firefox 4 sqlite3.dll pasa a llamarse
                            // mozsqlite3.dll
                            File tmpFile2 = new File(dir + File.separator + "mozsqlite3.dll");
                            if (tmpFile2.exists()) {
                                AOUtil.copyFile(tmpFile2, new File(dest + "mozsqlite3.dll"));
                            }
                            else {
                                tmpFile2 = new File(dir + File.separator + "sqlite3.dll");
                                if (tmpFile2.exists()) {
                                    AOUtil.copyFile(tmpFile2, new File(dest + "sqlite3.dll"));
                                }
                            }

                            tmpFile2 = new File(dir + File.separator + "mozcrt19.dll");
                            if (tmpFile2.exists()) {
                                AOUtil.copyFile(tmpFile2, new File(dest + "mozcrt19.dll"));
                            }

                            tmpFile2 = new File(dir + File.separator + "nssutil3.dll");
                            if (tmpFile2.exists()) {
                                AOUtil.copyFile(tmpFile2, new File(dest + "nssutil3.dll"));
                            }

                            tmpFile2 = new File(dir + File.separator + "freebl3.dll");
                            if (tmpFile2.exists()) {
                                AOUtil.copyFile(tmpFile2, new File(dest + "freebl3.dll"));
                            }

                            tmpFile2 = new File(dir + File.separator + "nssdbm3.dll");
                            if (tmpFile2.exists()) {
                                AOUtil.copyFile(tmpFile2, new File(dest + "nssdbm3.dll"));
                            }

                            return tmp.getCanonicalPath();

                        }
                        catch (final Exception e) {
                            Logger.getLogger("es.gob.afirma")
                                  .warning("No se ha podido duplicar NSS en un directorio temporal, si esta version de JRE esta afectada por el error 6581254 es posible que no pueda cargarse: " + e);
                        }
                    }
                    return dir;
                }
            }
        }
        else {
            // No hay NSS en el sistema, buscamos uno local
            File localNSS = new File(Platform.getUserHome() + "\\" + NSS_INSTALL_DIR + "\\nss" + Platform.getJavaArch());
            if (localNSS.exists() && localNSS.isDirectory() && localNSS.canRead()) {
                try {
                    return localNSS.getCanonicalPath();
                }
                catch (final Exception e) {
                    throw new FileNotFoundException("No se ha encontrado un NSS compatible en Windows: " + e);
                }
            }
        }

        throw new FileNotFoundException("No se ha encontrado un NSS compatible en Windows");

    }

    private static String getSystemNSSLibDirMacOSX() throws FileNotFoundException {

        final String[] paths =
                new String[] {
                        "/Applications/Firefox.app/Contents/MacOS",
                        "/lib",
                        "/usr/lib",
                        "/usr/lib/nss",
                        "/Applications/Minefield.app/Contents/MacOS",
                        Platform.getUserHome() + "/" + NSS_INSTALL_DIR + "/nss" + Platform.getJavaArch()
                };

        for (String path : paths) {
            if (new File(path + "/libsoftokn3.dylib").exists() && new File(path + "/libnspr4.dylib").exists()) {
                try {
                    System.load(path + "/libnspr4.dylib");
                    nssLibDir = path;
                }
                catch (final Exception e) {
                    nssLibDir = null;
                    Logger.getLogger("es.gob.afirma").warning("Descartamos el NSS situado en '" + path
                                                              + "' porque no puede cargarse adecuadamente: "
                                                              + e);
                }
            }
        }

        if (nssLibDir == null) {
            throw new FileNotFoundException("No se ha podido determinar la localizacion de NSS en Mac OS X");
        }

        return nssLibDir;
    }

    private static String getSystemNSSLibDirUNIX() throws FileNotFoundException {

        if (nssLibDir != null && (!"".equals(nssLibDir))) {
            return nssLibDir;
        }

        // *********************************************************************
        // *********************************************************************
        // Compobamos antes el caso especifico de NSS partido entre /usr/lib y
        // /lib, que se da en Fedora
        if (new File("/usr/lib/libsoftokn3.so").exists() && new File("/lib/libnspr4.so").exists()) {
            try {
                System.load("/lib/libnspr4.so");
                nssLibDir = "/usr/lib";
            }
            catch (final Exception e) {
                nssLibDir = null;
                Logger.getLogger("es.gob.afirma")
                      .warning("Descartamos el NSS situado entre /lib y /usr/lib porque no puede cargarse adecuadamente: " + e);
            }
            if (nssLibDir != null) {
                return nssLibDir;
            }
        }
        // *********************************************************************
        // *********************************************************************

        final String[] paths =
                new String[] {
                        "/usr/lib/firefox-" + searchLastFirefoxVersion("/usr/lib/"),
                        "/usr/lib/firefox",
                        "/opt/firefox",
                        "/opt/firefox-" + searchLastFirefoxVersion("/opt/"),
                        "/lib",
                        "/usr/lib",
                        "/usr/lib/nss",
                        "/opt/fedora-ds/clients/lib",
                        Platform.getUserHome() + "/" + NSS_INSTALL_DIR + "/nss" + Platform.getJavaArch()
                };

        for (String path : paths) {
            if (new File(path + "/libsoftokn3.so").exists() && new File(path + "/libnspr4.so").exists()) {
                try {
                    System.load(path + "/libnspr4.so");
                    nssLibDir = path;
                }
                catch (final Exception e) {
                    nssLibDir = null;
                    Logger.getLogger("es.gob.afirma").warning("Descartamos el NSS situado en '" + path
                                                              + "' porque no puede cargarse adecuadamente: "
                                                              + e);
                }
                if (nssLibDir != null) {
                    return nssLibDir;
                }
            }
        }

        if (nssLibDir == null) {
            throw new FileNotFoundException("No se ha podido determinar la localizacion de NSS en UNIX");
        }

        return nssLibDir;
    }

    /** Obtiene el directorio de las bibliotecas NSS (<i>Netscape Security
     * Services</i>) del sistema.
     * @return Directorio de las bibliotecas NSS del sistema
     * @throws FileNotFoundException
     *         Si no se puede encontrar NSS en el sistema
     * @throws AOInvalidFormatException
     *         Cuando se encuentra un NSS en una arquitectura distinta de la
     *         del entorno de ejecuci&oacute;n de Java (32 &oacute; 64 bits)
     * @throws AOException
     *         Cuando no se ha podido encontrar y cargar una versi&oacute;n
     *         v&aacute;lidad de NSS. */
    public static String getSystemNSSLibDir() throws FileNotFoundException, AOInvalidFormatException {

        if (nssLibDir != null) {
            return nssLibDir;
        }

        if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
            return getSystemNSSLibDirWindows();
        }
        if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.SOLARIS)) {
            return getSystemNSSLibDirUNIX();
        }
        if (Platform.getOS().equals(Platform.OS.MACOSX)) {
            return getSystemNSSLibDirMacOSX();
        }

        throw new FileNotFoundException("No se han encontrado bibliotecas NSS instaladas en su sistema operativo");
    }

    /** Busca la &uacute;ltima versi&oacute;n de firefox instalada en un sistema
     * Linux o Solaris
     * @return &Uacute;ltima versi&oacute;n instalada en el sistema */
    private static String searchLastFirefoxVersion(final String startDir) {
        final File directoryLib = new File(startDir);
        if (directoryLib.isDirectory()) {
            final String filenames[] = directoryLib.list();
            final List<String> firefoxDirectories = new ArrayList<String>();
            for (String filename : filenames) {
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
                public int compare(String o1, String o2) {
                    return o1.compareTo(o2);
                }
            });
            return firefoxDirectories.get(0);
        }
        return "";
    }

    /** Obtiene las rutas completas hacia las bibliotecas (.dll o .so) de los
     * m&oacute;dulos de seguridad externos (PKCS#11) instalados en Mozilla /
     * Firefox, indexados por su descripci&oacute;n dentro de una <code>Hashtable</code>.
     * @return Nombres de las bibliotecas de los m&oacute;dulos de seguridad de
     *         Mozilla / Firefox */
    static Hashtable<String, String> getMozillaPKCS11Modules() {
        try {
            final Hashtable<String, String> modsByDesc = new Hashtable<String, String>();
            for (ModuleName module : AOSecMod.getModules(getMozillaUserProfileDirectory())) {
                modsByDesc.put(module.getDescription(), module.getLib());
            }

            return purgeStoresTable(modsByDesc); // Eliminamos las entradas que
                                                 // usen la misma biblioteca
        }
        catch (final Exception t) {
            Logger.getLogger("es.gob.afirma")
                  .severe("No se han podido obtener los modulos externos de Mozilla, se devolvera una lista vacia o unicamente con el DNIe: " + t);
            return new Hashtable<String, String>(0);
        }
    }

    /** Obtiene el nombre (<i>commonName</i>) de un m&oacute;dulo externo de
     * Mozilla a partir de su representaci&oacute;n textual. Este m&eacute;todo
     * es dependiente de la implementaci&oacute;n de <code>toString()</code> de
     * la clase <code>sun.security.pkcs11.Secmod.Module</code>, ya que no
     * podemos acceder directamente al atributo <code>slot</code> por ser de
     * tipo <i>friend</i>: <code><pre> 
     * public String toString() {
     *   return 
     *   commonName + " (" + type + ", " + libraryName + ", slot " + slot + ")";
     *  }
     *  </pre></code>
     * @param description
     *        Resultado de una llamada a <code>sun.security.pkcs11.Secmod.Module.toString()</code>
     * @return Nombre correspondiente al m&oacute;dulo de seguridad */
    static String getMozModuleName(final String description) {
        final int ini = description.indexOf('(');
        if (ini > 0) {
            return description.substring(0, ini).trim();
        }
        return description;
    }

    /** Dada una tabla que indexa por descripci&oacute;n los m&oacute;dulos
     * pkcs11, eliminamos las entradas necesarias para que aparezca una
     * &uacute;nica vez cada m&oacute;dulo PKCS#11.
     * @param table
     *        Tabla con las descripciones de los m&oacute;dulos pkcs11 y las
     *        librer&iacute;as asociadas.
     * @return Tabla con los m&oacute;dulos eliminados. */
    private static Hashtable<String, String> purgeStoresTable(final Hashtable<String, String> table) {

        if (table == null) {
            return new Hashtable<String, String>();
        }

        final Hashtable<String, String> purgedTable = new Hashtable<String, String>();
        final Set<String> revisedLibs = new HashSet<String>();

        String tmpLib;
        for (final String key : table.keySet()) {
            tmpLib = table.get(key);
            if (tmpLib.toLowerCase().endsWith(".dll")) {
                tmpLib = tmpLib.toLowerCase();
            }

            if (!revisedLibs.contains(tmpLib) && (!tmpLib.toLowerCase().contains("nssckbi"))) {
                purgedTable.put(key, table.get(key));
                revisedLibs.add(tmpLib);
            }
            else {
                Logger.getLogger("es.gob.afirma").warning("Se eliminara el modulo '" + key
                                                          + "' porque ya existe uno con la misma biblioteca o es un modulo de certificados raiz: "
                                                          + table.get(key));
            }
        }

        return purgedTable;
    }

    /** Carga las dependencias de la biblioteca "softokn3" necesaria para acceder
     * al almac&eacute;n de certificados. Hacemos esto por precauci&oacute;n ya
     * que esto se har&iacute;a autom&aacute;ticamente si las dependencias
     * estuviesen en el PATH del sistema.
     * @param nssDirectory
     *        Directorio en donde se encuentran las bibliotecas de NSS. */
    static void loadNSSDependencies(final String nssDirectory) {
        // *********************************************************************
        // *********************************************************************
        // Compobamos antes el caso especifico de NSS partido entre /usr/lib y
        // /lib, que se da en Fedora
        if (Platform.OS.LINUX.equals(Platform.getOS()) && new File("/usr/lib/libsoftokn3.so").exists() && new File("/lib/libnspr4.so").exists()) {
            try {
                System.load("/lib/libnspr4.so");
                if (new File("/lib/libplds4.so").exists()) {
                    System.load("/lib/libplds4.so");
                }
                if (new File("/usr/lib/libplds4.so").exists()) {
                    System.load("/usr/lib/libplds4.so");
                }
                if (new File("/lib/libplc4.so").exists()) {
                    System.load("/lib/libplc4.so");
                }
                if (new File("/usr/lib/libplc4.so").exists()) {
                    System.load("/usr/lib/libplc4.so");
                }
                if (new File("/lib/libnssutil3.so").exists()) {
                    System.load("/lib/libnssutil3.so");
                }
                if (new File("/usr/lib/libnssutil3.so").exists()) {
                    System.load("/usr/lib/libnssutil3.so");
                }
                if (new File("/lib/libsqlite3.so").exists()) {
                    System.load("/lib/libsqlite3.so");
                }
                if (new File("/usr/lib/libsqlite3.so").exists()) {
                    System.load("/usr/lib/libsqlite3.so");
                }
                if (new File("/lib/libmozsqlite3.so").exists()) {
                    System.load("/lib/libmozsqlite3.so");
                }
                if (new File("/usr/lib/libmozsqlite3.so").exists()) {
                    System.load("/usr/lib/libmozsqlite3.so");
                }
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("Error cargando NSS en una instalacion partida entre /lib y /usr/lib: " + e);
            }
            return;
        }
        // *********************************************************************
        // *********************************************************************

        final String path = nssDirectory + (nssDirectory.endsWith(File.separator) ? "" : File.separator);
        for (final String libPath : getSoftkn3Dependencies(path)) {
            try {
                if (new File(libPath).exists()) {
                    System.load(libPath);
                }
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").warning("Error al cargar la biblioteca " + libPath
                                                          + " para el acceso al almacen de claves de Mozilla: "
                                                          + e);
            }
        }
    }

    /** Recupera el listado de dependencias de la biblioteca "softkn3" para el
     * sistema operativo en el que se est&aacute; ejecutando la
     * aplicaci&oacute;n. Los nombres apareceran ordenados de tal forma las
     * bibliotecas no tengan dependencias de otra que no haya aparecido
     * anterioremente en la lista.
     * @param nssPath
     *        Ruta al directorio de NSS (terminado en barra).
     * @return Listado con los nombres de las bibliotecas. */
    private static String[] getSoftkn3Dependencies(String nssPath) {

        if (nssPath == null) {
            return new String[0];
        }

        if (Platform.getOS().equals(Platform.OS.MACOSX)) {
            // En Mac OS X no funciona la precarga de bibliotecas
            return new String[0];
        }

        nssPath = (!nssPath.endsWith(File.separator) ? nssPath + File.separator : nssPath);

        if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
            // Mozilla Firefox 4.0
            if (new File(nssPath + "mozsqlite3.dll").exists()) {
                Logger.getLogger("es.gob.afirma").info("Detectado NSS de Firefox 4 en Windows");
                return new String[] {
                        nssPath + "mozcrt19.dll",
                        nssPath + "nspr4.dll",
                        nssPath + "plds4.dll",
                        nssPath + "plc4.dll",
                        nssPath + "nssutil3.dll",
                        nssPath + "mozsqlite3.dll",
                        nssPath + "nssdbm3.dll",
                        nssPath + "freebl3.dll"
                };
            }
            // Mozilla Firefox 3.0
            else if (new File(nssPath + "mozcrt19.dll").exists()) {
                // Logger.getLogger("es.gob.afirma").info("Detectado NSS de Firefox 3");
                return new String[] {
                        nssPath + "mozcrt19.dll",
                        nssPath + "nspr4.dll",
                        nssPath + "plds4.dll",
                        nssPath + "plc4.dll",
                        nssPath + "nssutil3.dll",
                        nssPath + "sqlite3.dll",
                        nssPath + "nssdbm3.dll",
                        nssPath + "freebl3.dll"
                };
            }
            // Mozilla Firefox 2.0
            else if (new File(nssPath + "nspr4.dll").exists()) {
                // Logger.getLogger("es.gob.afirma").info("Detectado NSS de Firefox 2");
                return new String[] {
                        nssPath + "nspr4.dll", nssPath + "plds4.dll", nssPath + "plc4.dll"
                };
            }
        }
        else if (Platform.getOS().equals(Platform.OS.LINUX) || Platform.getOS().equals(Platform.OS.SOLARIS)) {
            if (new File(nssPath + "libmozsqlite.so").exists()) {
                Logger.getLogger("es.gob.afirma").info("Detectado NSS de Firefox 4 en UNIX");
                return new String[] {
                        nssPath + "libnspr4.so",
                        nssPath + "libplds4.so",
                        nssPath + "libplc4.so",
                        nssPath + "libnssutil3.so",
                        nssPath + "libsqlite3.so"
                };
            }
            return new String[] {
                    nssPath + "libnspr4.so",
                    nssPath + "libplds4.so",
                    nssPath + "libplc4.so",
                    nssPath + "libnssutil3.so",
                    nssPath + "libmozsqlite3.so"
            };
        }

        Logger.getLogger("es.gob.afirma").warning("Plataforma no soportada para la precarga de las bibliotecas NSS: " + Platform.getOS()
                                                  + " + Java "
                                                  + Platform.getJavaArch()
                                                  + "-bits");
        return new String[0];
    }

    /** Obtiene el directorio del perfil de usuario de Mozilla / Firefox.
     * @return Ruta completa del directorio del perfil de usuario de Mozilla /
     *         Firefox */
    final static String getMozillaUserProfileDirectory() {

        File regFile;
        if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
            final String appDataDir =
                    WinRegistryWrapper.getString(WinRegistryWrapper.HKEY_CURRENT_USER,
                                                 "Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders",
                                                 "AppData");
            if (appDataDir != null) {
                String finalDir = null;
                // En Firefox usamos preferentemente el profiles.ini
                regFile = new File(appDataDir + "\\Mozilla\\Firefox\\profiles.ini");
                try {
                    if (regFile.exists()) {
                        finalDir = NSPreferences.getFireFoxUserProfileDirectory(regFile);
                    }
                }
                catch (Exception e) {
                    Logger.getLogger("es.gob.afirma")
                          .severe("Error obteniendo el directorio de perfil de usuario de Firefox, " + "se devolvera null: " + e);
                    return null;
                }
                if (finalDir != null) {
                    return finalDir.replace('\\', '/');
                }
                // Hemos probado el de Firefox, vamos ahora con el de Mozilla
                regFile = new File(appDataDir + "\\Mozilla\\registry.dat");
                try {
                    if (regFile.exists()) {
                        finalDir = NSPreferences.getNS6UserProfileDirectory(regFile);
                    }
                }
                catch (Exception e) {
                    Logger.getLogger("es.gob.afirma")
                          .severe("Error obteniendo el directorio de perfil de usuario de Mozilla, " + "se devolvera null: " + e);
                    return null;
                }
                if (finalDir != null) {
                    return finalDir.replace('\\', '/');
                }
            }
            Logger.getLogger("es.gob.afirma")
                  .severe("Error obteniendo el directorio de perfil de usuario de Mozilla/Firefox (Windows), " + "se devolvera null");
            return null;
        }

        else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            // Si es un Mac OS X, profiles.ini esta en una ruta distinta...
            regFile = new File(Platform.getUserHome() + "/Library/Application Support/Firefox/profiles.ini");
            try {
                if (regFile.exists()) {
                    return NSPreferences.getFireFoxUserProfileDirectory(regFile);
                }
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma")
                      .severe("Error obteniendo el directorio de perfil de usuario de Firefox (" + regFile.getAbsolutePath()
                              + "), se devolvera null: "
                              + e);
                return null;
            }
        }
        else {

            // No es Windows ni Mac OS X, entonces es UNIX (Linux / Solaris)

            // Probamos primero con "profiles.ini" de Firefox
            regFile = new File(Platform.getUserHome() + "/.mozilla/firefox/profiles.ini");
            try {
                if (regFile.exists()) {
                    return NSPreferences.getFireFoxUserProfileDirectory(regFile);
                }
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").severe("Error obteniendo el directorio de perfil de usuario de Firefox, " + "se devolvera null: "
                                                         + e);
                return null;
            }

            // Y luego con el registro clasico de Mozilla
            regFile = new File(Platform.getUserHome() + "/.mozilla/appreg");
            try {
                if (regFile.exists()) {
                    return NSPreferences.getNS6UserProfileDirectory(regFile);
                }
            }
            catch (final Exception e) {
                Logger.getLogger("es.gob.afirma").severe("Error obteniendo el directorio de perfil de usuario de Firefox, " + "se devolvera null: "
                                                         + e);
                return null;
            }
        }

        Logger.getLogger("es.gob.afirma")
              .severe("Error obteniendo el directorio de perfil de usuario de Mozilla/Firefox (UNIX), " + "se devolvera null");

        return null;
    }

}
