package es.gob.afirma.standalone.configurator;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.security.GeneralSecurityException;
import java.security.KeyStoreException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.script.ScriptEngine;
import javax.script.ScriptException;
import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilities;
import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilitiesOsX;

/** Configurador para instalar un certificado SSL de confianza en Mozilla NSS.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
final class ConfiguratorFirefox {

    static final class MozillaProfileNotFoundException extends Exception {
        private static final long serialVersionUID = -2329746920496661591L;
    }

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String FILE_AUTOFIRMA_CERTIFICATE = "autofirma.cer"; //$NON-NLS-1$
    static final String DIR_CERTUTIL = "certutil"; //$NON-NLS-1$
    private static final String LINUX_UNINSTALLSCRIPT_NAME = "uninstall.sh"; //$NON-NLS-1$
    private static final String LINUX_SCRIPT_NAME = "script.sh"; //$NON-NLS-1$
    private static final String LINUX_MOZILLA_PATH = "/.mozilla/firefox/profiles.ini";//$NON-NLS-1$
    private static final String MACOSX_MOZILLA_PATH = "/Library/Application Support/firefox/profiles.ini";//$NON-NLS-1$

    static final String CERTUTIL_EXE;
    private static final String FILE_CERTUTIL;
    private static final String RESOURCE_BASE;

    static {
        switch(Platform.getOS()) {
            case WINDOWS:
                CERTUTIL_EXE = "certutil.exe"; //$NON-NLS-1$
                FILE_CERTUTIL = "certutil.windows.zip"; //$NON-NLS-1$
                RESOURCE_BASE = "/windows/"; //$NON-NLS-1$
                break;
            case MACOSX:
                CERTUTIL_EXE = "certutil"; //$NON-NLS-1$
                FILE_CERTUTIL = "certutil.osx.zip"; //$NON-NLS-1$
                RESOURCE_BASE = "/osx/"; //$NON-NLS-1$
                break;
            case LINUX:
                CERTUTIL_EXE = "certutil"; //$NON-NLS-1$
                FILE_CERTUTIL = "certutil.linux.zip"; //$NON-NLS-1$
                RESOURCE_BASE = "/linux/"; //$NON-NLS-1$
                break;
            default:
                throw new IllegalStateException(
                    "Sistema operativo no soportado: " + Platform.getOS() //$NON-NLS-1$
                );
        }
    }

    private ConfiguratorFirefox() {
        // No instanciable
    }

    static void installRootCAMozillaKeyStore(final File targetDir,
                                             final X509Certificate cert) throws MozillaProfileNotFoundException,
                                                                                IOException {
        final File firefoxProfilesDir = getFirefoxProfilesDir();
        if (firefoxProfilesDir == null) {
            throw new MozillaProfileNotFoundException();
        }
        copyConfigurationFiles(targetDir);
        ConfiguratorFirefox.importCARootOnFirefoxKeyStore(
            cert,
            targetDir,
            firefoxProfilesDir
        );

    }

    /**
     * Genera el script de instalaci&oacute; del certificado en firefox para MacOSX y LINUX.
     * En linux genera el script que hay que ejecutar para realizar la instalaci&oacute;n pero no lo ejecuta, de eso se encarga el instalador Debian.
     * En MacOSX el script se ejecuta a la vuelta de este m&eacute;todo.
     * @param targetDir Directorio de instalaci&oacute;n del sistema
     * @param cert Certificado a instalar.
     * @param command Usado para sacar los directorios de usuario dentro del sistema operativo.
     *  <ul>
     * <li>En LINUX contiene el contenido del script a ejecutar.</li>
     * <li>En MacOSX contiene la ruta del script a ejecutar.</li>
     * </ul>
     * @throws MozillaProfileNotFoundException No se ha encontrado el directorio de perfiles de Mozilla.
     * @throws IOException Cuando ocurre un error en el tratamiento de datos.
     */
	static void installRootCAMozillaKeyStore(final File targetDir, final X509Certificate cert, final String []command )
			throws MozillaProfileNotFoundException, IOException {

		// sacamos el listado de usuarios de la aplicacion
		final List<String> usersDirs = getFirefoxProfilesUsersDir(command);
		// dados los usuarios sacamos el directorio de perfiles de mozilla en caso de que lo tengan
		final List <File> mozillaUsersProfilesPath = getMozillaUsersProfilesPath(usersDirs);
		// para cada usuario tenemos sus distintos directorios de perfiles
		final Set <File> profiles = getProfiles(mozillaUsersProfilesPath);
		if (profiles.isEmpty()){
			throw new MozillaProfileNotFoundException();
		}
		copyConfigurationFiles(targetDir);

		ConfiguratorFirefox.importCARootOnFirefoxKeyStore(cert, targetDir, profiles);

	}


    static void uninstallRootCAMozillaKeyStore(final File targetDir) {

        try {
            copyConfigurationFiles(targetDir);
        }
        catch (final Exception e) {
            LOGGER.warning(
                "No se pudo descomprimir certutil para la desinstalacion del certificado SSL raiz del almacen de Mozilla Firefox. Se aborta la operacion: " + e //$NON-NLS-1$
            );
        }

        try {
        	ConfiguratorFirefox.executeCertUtilToDelete(targetDir);
        }
        catch (final Exception e) {
            LOGGER.warning("No se pudo desinstalar el certificado SSL raiz del almacen de Mozilla Firefox: " + e); //$NON-NLS-1$
        }

        removeConfigurationFiles(targetDir);
    }


    static void generateUninstallScriptMac(final File targetDir) throws MozillaProfileNotFoundException, IOException {

    	final StringBuilder sb = new StringBuilder(ConfiguratorMacOSX.OSX_GET_USERS_COMMAND);
		final String path = targetDir + ConfiguratorMacOSX.GET_USER_SCRIPT;
		try {
			ConfiguratorFirefox.writeScriptFile(path, sb, true);
		} catch (final IOException e) {
			LOGGER.severe(" Ha ocurrido un error : " + e); //$NON-NLS-1$
		}
		addExexPermissionsToAllFilesOnDirectory(ConfiguratorUtil.getApplicationDirectory());
		// sacamos el listado de usuarios de la aplicacion
		final List<String> usersDirs = getFirefoxProfilesUsersDir(new String[]{path});
		// dados los usuarios sacamos el directorio de perfiles de mozilla en caso de que lo tengan
		final List <File> mozillaUsersProfilesPath = getMozillaUsersProfilesPath(usersDirs);
		// para cada usuario tenemos sus distintos directorios de perfiles
		final Set <File> profiles = getProfiles(mozillaUsersProfilesPath);
		if (profiles.isEmpty()){
			throw new MozillaProfileNotFoundException();
		}



		final File certutilFile = new File(targetDir, DIR_CERTUTIL + File.separator + CERTUTIL_EXE);

		if (!certutilFile.exists() || !certutilFile.isFile() || !certutilFile.canExecute()) {
            throw new IOException("No se encuentra o no se puede leer el ejecutable para la instalacion en Firefox"); //$NON-NLS-1$
        }

		for (final File profile : profiles) {
			if (!profile.isDirectory()) {
				continue;
			}

		final String[] certutilCommands = new String[] {
				escapePath(certutilFile.getAbsolutePath()),
                "-D", //$NON-NLS-1$
                "-d", //$NON-NLS-1$
                escapePath(profile.getAbsolutePath()),
                "-n", //$NON-NLS-1$
                "\"" + ConfiguratorUtil.CERT_ALIAS + "\"", //$NON-NLS-1$ //$NON-NLS-2$
        };

		execCommandLineCertUtil(certutilCommands);

		}

	}

	/** Elimina la carpeta certutil generada durante el proceso de instalaci&oacute;n.
     * @param targetDir Directorio en el que se copia certUtil. */
    static void removeConfigurationFiles(final File targetDir) {
        if (!targetDir.exists()) {
            return;
        }
        ConfiguratorFirefox.deleteConfigDir(targetDir);
    }

    static void addExexPermissionsToFile(final File f) {
        final Set<PosixFilePermission> perms = new HashSet<>();
        perms.add(PosixFilePermission.OWNER_EXECUTE);
        perms.add(PosixFilePermission.GROUP_EXECUTE);
        perms.add(PosixFilePermission.OTHERS_EXECUTE);
        perms.add(PosixFilePermission.OWNER_READ);
        perms.add(PosixFilePermission.GROUP_READ);
        perms.add(PosixFilePermission.OTHERS_READ);
        perms.add(PosixFilePermission.OWNER_WRITE);
        perms.add(PosixFilePermission.GROUP_WRITE);
        perms.add(PosixFilePermission.OTHERS_WRITE);
        try {
            Files.setPosixFilePermissions(
                Paths.get(f.getAbsolutePath()),
                perms
            );
        }
        catch (final Exception e) {
            LOGGER.warning(
                "No se ha podido dar permiso de ejecucion a " + f.getAbsolutePath() //$NON-NLS-1$
            );
        }
    }

    /**
     * Da permisos de ejecuci&oacute;n a todos los ficheros de un directorio dado.
     * @param dir Directorio al que dar permiso.
     */
    public static void addExexPermissionsToAllFilesOnDirectory(final File dir) {
        if (!Platform.OS.WINDOWS.equals(Platform.getOS())) {
            for (final File fileEntry : dir.listFiles()) {
                addExexPermissionsToFile(fileEntry);
            }
        }
    }

    private static String escapePath(final String path) {
        if (path == null) {
            throw new IllegalArgumentException(
                "La ruta a 'escapar' no puede ser nula" //$NON-NLS-1$
            );
        }
        if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
            if (path.contains(" ")) { //$NON-NLS-1$
                return "\"" + path + "\""; //$NON-NLS-1$ //$NON-NLS-2$
            }
            return path;
        }
        return path.replace(" ", "\\ "); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * Copiamos el certificado en un directorio para que certutil pueda usarlo
     * @param certUtilAbsolutePath Ruta del fichero certUtil.
     * @param cert Certificado que hay que instalar.
     * @param targetDir Directorio donde se copiar&aacute; el certificado.
     * @return certutilFile Fichero certutil.
     * @throws CertificateEncodingException Se lanza cuando hay un problema en la codificaci&oacute;n del certificado.
     * @throws IOException Se lanza cuando hay un problema con el fichero certuti.
     */
    public static File InstallCerFile(final String certUtilAbsolutePath, final X509Certificate cert,
			final File targetDir) throws CertificateEncodingException, IOException{

		ConfiguratorUtil.installFile(cert.getEncoded(), new File(targetDir, FILE_AUTOFIRMA_CERTIFICATE));

		final File certutilFile = new File(certUtilAbsolutePath);

		if (!certutilFile.exists() || !certutilFile.isFile()) {
			throw new IOException("No se encuentra el ejecutable CertUtil para la instalacion en Firefox" //$NON-NLS-1$
			);
		}

		if (!certutilFile.canExecute()) {
			addExexPermissionsToAllFilesOnDirectory(certutilFile.getParentFile());
		}

		if (!certutilFile.canExecute()) {
			throw new IOException("No hay permisos de ejecucion para Mozilla CertUtil" //$NON-NLS-1$
			);
		}

		return certutilFile;
    }

    /**
     * Ejecuta la utilidad Mozilla CertUtil para la instalaci&oacute;n del certificado ra&iacute;z de  confianza en Firefox.
     * @param certUtilAbsolutePath Directorio en el que se encuentra certutil.
     * @param cert Certificado que se debe instalar.
     * @param targetDir Directorio en el que se encuentra el certificado a importar.
     * @param profilesDir Listado de directorios de perfiles de usuario de Mozilla Firefox.
     * @throws IOException Cuando ocurre un error en el tratamiento de datos.
     * @throws GeneralSecurityException Cuando ocurre un error en la inserci&oacute;n del certificado en el KeyStore.
     */
	private static void executeCertUtilToImport(final String certUtilAbsolutePath, final X509Certificate cert,
			final File targetDir, final Set<File> profilesDir) throws IOException, GeneralSecurityException {

		final File certutilFile = InstallCerFile(certUtilAbsolutePath, cert, targetDir);

		// Obtenemos todos los directorios de perfil de Firefox del usuario
		boolean error = false;

		 // exportamos el PATH y LD_LIBRARY_PATH en LINUX Y MACOSX
		 if ( Platform.OS.LINUX.equals(Platform.getOS()) ) {
             writeScriptFile(targetDir + File.separator + LINUX_SCRIPT_NAME, new StringBuilder(ConfiguratorLinux.EXPORT_PATH + certutilFile.getAbsolutePath().substring(0,certutilFile.getAbsolutePath().lastIndexOf(File.separator) )), true);
             writeScriptFile(targetDir + File.separator + LINUX_SCRIPT_NAME, new StringBuilder(ConfiguratorLinux.EXPORT_LD_LIBRARY + certutilFile.getAbsolutePath().substring(0,certutilFile.getAbsolutePath().lastIndexOf(File.separator) )), true);
		 }
		 else if ( Platform.OS.MACOSX.equals(Platform.getOS()) ){
		     writeScriptFile(ConfiguratorMacOSX.MAC_PATH_SCRIPT, new StringBuilder(ConfiguratorMacOSX.EXPORT_PATH + certutilFile.getAbsolutePath().substring(0,certutilFile.getAbsolutePath().lastIndexOf(File.separator) )), true);
             writeScriptFile(ConfiguratorMacOSX.MAC_PATH_SCRIPT, new StringBuilder(ConfiguratorMacOSX.EXPORT_LIBRARY_LD + certutilFile.getAbsolutePath().substring(0,certutilFile.getAbsolutePath().lastIndexOf(File.separator) )), true);
        }
		for (final File profileDir : profilesDir) {
			if (!profileDir.isDirectory()) {
				continue;
			}

			final String[] certutilCommands = new String[] {
			        escapePath(certutilFile.getAbsolutePath()),
			        "-A", //$NON-NLS-1$
					"-d", //$NON-NLS-1$
					escapePath(profileDir.getAbsolutePath()), "-i", //$NON-NLS-1$
					escapePath(new File(targetDir, FILE_AUTOFIRMA_CERTIFICATE).getAbsolutePath()), "-n", //$NON-NLS-1$
					"\"" + ConfiguratorUtil.CERT_ALIAS + "\"", //$NON-NLS-1$ //$NON-NLS-2$
					"-t", //$NON-NLS-1$
					"\"C,,\"" //$NON-NLS-1$
			};

			error = execCommandLineCertUtil(certutilCommands);
		}


		if (error) {
			throw new KeyStoreException(
					"Error en la instalacion del certificado de CA en alguno de los perfiles de usuario " //$NON-NLS-1$
							+ "de Firefox. Es posible que la aplicacion funcione en su propio perfil. Si desea que la aplicacion se " //$NON-NLS-1$
							+ "ejecute correctamente en todos los perfiles, desinstalela y vuelvala a instalar." //$NON-NLS-1$
			);
		}

	}

    /** Ejecuta Mozilla CertUtil como comando del sistema.
     * @param command Comando a ejecutar, con el nombre de comando y sus par&aacute;metros separados en un array.
     * @return <code>true</code> si la ejecuci&oacute;n de CertUtil termin&oacute; con error, <code>false</code> si se
     *         ejecut&oacute; correctamente.
     * @throws IOException Si no se pudo realizar la propia ejecuci&oacute;n. */
    private static boolean execCommandLineCertUtil(final String[] command)
            throws IOException {

        final StringBuilder sb = new StringBuilder();
        for (final String s : command) {
            sb.append(s);
            sb.append(' ');
        }

        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            writeScriptFile(ConfiguratorMacOSX.MAC_PATH_SCRIPT, sb, true);
            return false;

        } else if (Platform.OS.LINUX.equals(Platform.getOS())) {
            // Generamos el script de instalacion y desistalacion
            try {
                // montamos el script de desinstalacion, reutilizamos datos que
                // vienen en el script de instalacion
                final StringBuilder uninstall = new StringBuilder();
                uninstall.append(command[0] + ' ');
                uninstall.append("-D" + ' '); //$NON-NLS-1$
                uninstall.append("-d" + ' ');//$NON-NLS-1$
                uninstall.append(command[3] + ' ');
                uninstall.append("-n" + ' ');//$NON-NLS-1$
                uninstall.append(command[7] + ' ');

                // tenemos en command[5] la ruta del fichero .cer, sacamos de
                // ahi la ruta del directorio de instalacion
                final String path = command[5].substring(0,
                        command[5].lastIndexOf("/") + 1) + LINUX_SCRIPT_NAME; //$NON-NLS-1$
                final String uninstallPath = command[5].substring(0,
                        command[5].lastIndexOf("/") + 1) + LINUX_UNINSTALLSCRIPT_NAME; //$NON-NLS-1$
                final File installScript = new File(path);
                final File uninstallScript = new File(uninstallPath);
                sb.append("\n"); //$NON-NLS-1$
                uninstall.append("\n"); //$NON-NLS-1$

                try (final FileOutputStream fout = new FileOutputStream(installScript, true);
                        final FileOutputStream foutUninstall = new FileOutputStream(
                                uninstallScript, true);) {
                    fout.write(sb.toString().getBytes());
                    foutUninstall.write(uninstall.toString().getBytes());
                }
                return false;
            } catch (final Exception e) {
                LOGGER.severe("Excepcion en la creacion del script linux para la instalacion del certificado en el almacen de Firefox: " + e); //$NON-NLS-1$
                return true;
            }

        } else {
            LOGGER.info("Se ejecutara el siguiente comando:\n" + sb.toString()); //$NON-NLS-1$
            final Process process = new ProcessBuilder(command).start();
            // Cuando se instala correctamente no hay salida de ningun tipo, asi
            // que se interpreta
            // cualquier salida como un error
            String line;
            try (final InputStream resIs = process.getInputStream();
                    final BufferedReader resReader = new BufferedReader(
                            new InputStreamReader(resIs));) {
                while ((line = resReader.readLine()) != null) {
                    LOGGER.severe(line);
                    return true;
                }
            }

            try (final InputStream errIs = process.getErrorStream();
                    final BufferedReader errReader = new BufferedReader(
                            new InputStreamReader(errIs));) {
                while ((line = errReader.readLine()) != null) {
                    LOGGER.severe(line);
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Ejecuta un script en MacOsx.
     * @param path El path donde se encuentra el script.
     * @param administratorMode <code>true</code> el script se ejecuta como permisos de adminsitrador, <code>false</code>  en caso contrario
     * @param delete <code>true</code> se borra el fichero despu&eacute;s de haberse ejecutado.
     * @return El objeto que da como resultado el script, o null en caso contrario
     * @throws IOException Excepci&oacute;n lanzada en caso de ocurrir alg&uacute;n error en la ejecuci&oacute;n del script
     */
    public static Object executeScriptMacOsx(final String path, final boolean administratorMode, final boolean delete) throws IOException{
    	final ScriptEngine se = MozillaKeyStoreUtilitiesOsX.getAppleScriptEngine();
        if (se != null) {
            LOGGER.info("Path del script: " + path); //$NON-NLS-1$
            try {
            	Object o;
            	if (administratorMode){
            		o = se.eval("do shell script \"" + path + "\" with administrator privileges"); //$NON-NLS-1$ //$NON-NLS-2$
            	}
            	else {
            		o = se.eval("do shell script \"" + path + "\" "); //$NON-NLS-1$ //$NON-NLS-2$
            	}
                System.out.println("Tipo: " + o.getClass().getName()); //$NON-NLS-1$
                System.out.println("Contenido: " + o.toString()); //$NON-NLS-1$
                if (delete){
    				final File scriptInstall = new File(path);
    				if (scriptInstall.exists()){
    				    scriptInstall.delete();
    				}

                }
                return o;
            } catch (final ScriptException e) {
                throw new IOException("Error en la ejecucion de CertUtil via AppleScript: " + e, e); //$NON-NLS-1$
            }
        }
        LOGGER.severe("No se ha podido instanciar el motor AppleScript para instalar la raiz SSL en NSS" //$NON-NLS-1$
        );
        return null;
    }

    private static void importCARootOnFirefoxKeyStore (final X509Certificate cert,
            final File appConfigDir,
            final Set<File> profilesDir){
    	boolean installed = false;
        boolean cancelled = false;
        do {
            try {
                // Usamos CertUtil para instalar el certificado en Firefox
                executeCertUtilToImport(
                    appConfigDir.getAbsolutePath() + File.separator + DIR_CERTUTIL + File.separator + CERTUTIL_EXE,
                    cert,
                    appConfigDir,
                    profilesDir
                );
                installed = true;
            }
            catch (final Exception e) {
                LOGGER.warning(
                    "No se pudo instalar la CA del certificado SSL para el socket en el almacen de Firefox: " + e //$NON-NLS-1$
                );
                final int result = JOptionPane.showConfirmDialog(
                    null,
                    Messages.getString("ConfiguratorWindows.10"), //$NON-NLS-1$
                    Messages.getString("ConfiguratorWindows.1"), //$NON-NLS-1$
                    JOptionPane.OK_CANCEL_OPTION,
                    JOptionPane.WARNING_MESSAGE
                );
                if (result == JOptionPane.CANCEL_OPTION) {
                    cancelled = true;
                    LOGGER.severe(
                        "El usuario cancelo la instalacion del certificado SSL para el socket en Firefox: " + e //$NON-NLS-1$
                    );
                }
            }
        } while (!installed && !cancelled);

    }

	private static void importCARootOnFirefoxKeyStore(final X509Certificate cert,
                                                      final File appConfigDir,
                                                      final File profilesDir) {
    	// en windows recibimos un unico directorio de perfil, lo convertimos a una estructura Set<File>
    	final Set<File> profile = new HashSet<>(Arrays.asList(profilesDir.listFiles()));
    	importCARootOnFirefoxKeyStore(cert, appConfigDir, profile);
    }

    /** Ejecuta la aplicacion Mozilla CertUtil para eliminar el certificado de confianza ra&iacute;z
     * SSL de Firefox.
     * @param targetDir Directorio padre en el que se encuentra el directorio de certUtil.
     * @throws IOException Cuando no se encuentra o puede leer alguno de los ficheros necesarios.
     * @throws GeneralSecurityException Cuando no se puede ejecutar. */
    private static void executeCertUtilToDelete(final File targetDir) throws IOException, GeneralSecurityException {

        final File certutilFile = new File(targetDir, DIR_CERTUTIL + File.separator + CERTUTIL_EXE);

        if (!certutilFile.exists() || !certutilFile.isFile() || !certutilFile.canExecute()) {
            throw new IOException("No se encuentra o no se puede leer el ejecutable para la instalacion en Firefox"); //$NON-NLS-1$
        }

        // Obtenemos todos los directorios de perfil de Firefox del usuario
        boolean error = false;
        final File profilesDir = new File(MozillaKeyStoreUtilities.getMozillaUserProfileDirectory()).getParentFile();
        for (final File profileDir : profilesDir.listFiles()) {
            if (!profileDir.isDirectory()) {
                continue;
            }

            final String[] certutilCommands = new String[] {
                    certutilFile.getAbsolutePath(),
                    "-D", //$NON-NLS-1$
                    "-d", //$NON-NLS-1$
                    profileDir.getAbsolutePath(),
                    "-n", //$NON-NLS-1$
                    "\"" + ConfiguratorUtil.CERT_ALIAS + "\"", //$NON-NLS-1$ //$NON-NLS-2$
            };

            final Process process = new ProcessBuilder(certutilCommands).start();

            // Cuando se instala correctamente no hay salida de ningun tipo, asi que se interpreta
            // cualquier salida como un error
            String line;
            try (
                final InputStream resIs = process.getInputStream();
                final BufferedReader resReader = new BufferedReader(new InputStreamReader(resIs));
            ) {
                while ((line = resReader.readLine()) != null) {
                    error = true;
                    LOGGER.severe(line);
                }
            }

            try (final InputStream errIs = process.getErrorStream();
                    final BufferedReader errReader = new BufferedReader(new InputStreamReader(errIs));) {
                while ((line = errReader.readLine()) != null) {
                    error = true;
                    LOGGER.severe(line);
                }
            }
        }

        if (error) {
            throw new KeyStoreException("Error en el borrado del certificado de CA en alguno de los perfiles de usuario de Firefox"); //$NON-NLS-1$
        }
    }

    private static void deleteConfigDir(final File appConfigDir) {
        ConfiguratorUtil.deleteDir(new File(appConfigDir, DIR_CERTUTIL));
    }

    private static void copyConfigurationFiles(final File appConfigDir) throws IOException {
        uncompressResource(RESOURCE_BASE + FILE_CERTUTIL, appConfigDir);
    }

    /** Descomprime un fichero ZIP de recurso al disco.
     * @param resource Ruta del recurso ZIP.
     * @param outDir Directorio local en el que descomprimir.
     * @throws IOException Cuando ocurre un error al descomprimir.
     **/
     private static void uncompressResource(final String resource, final File outDir) throws IOException {
        int n;
        ZipEntry entry;
        final byte[] buffer = new byte[1024];
        try (final ZipInputStream zipIs = new ZipInputStream(
                ConfiguratorFirefox.class.getResourceAsStream(resource));) {
            // en linux el funcionamiento es ligeramente diferente
            if (Platform.OS.LINUX == Platform.getOS()) {
                while ((entry = zipIs.getNextEntry()) != null) {
                    new File(outDir, "certutil").mkdirs(); //$NON-NLS-1$
                    try (final FileOutputStream outFis = new FileOutputStream(
                            new File(outDir, entry.getName()));) {
                        while ((n = zipIs.read(buffer)) > 0) {
                            outFis.write(buffer, 0, n);
                        }
                        outFis.flush();
                    }

                    zipIs.closeEntry();
                }
            } else {
                while ((entry = zipIs.getNextEntry()) != null) {
                    if (entry.isDirectory()) {
                        new File(outDir, entry.getName()).mkdirs();
                    } else {
                        try (final FileOutputStream outFis = new FileOutputStream(
                                new File(outDir, entry.getName()));) {
                            while ((n = zipIs.read(buffer)) > 0) {
                                outFis.write(buffer, 0, n);
                            }
                            outFis.flush();
                        }
                    }
                    zipIs.closeEntry();
                }
            }

        }
    }


    private static File getFirefoxProfilesDir() {
        File profilesDir;
        try {
            profilesDir = new File(MozillaKeyStoreUtilities.getMozillaUserProfileDirectory()).getParentFile();
        }
        catch (final Exception e) {
            LOGGER.warning("No se encontro el directorio de perfiles de Mozilla Firefox: " + e); //$NON-NLS-1$
            profilesDir = null;
        }
        return profilesDir;
    }

    /**
     * Devuelve un listado con todos directorios personales de los usuarios del sistema ejecutando un script.
     * Utilizado en Linux y MacOSX
     * @param command Script para sacar los directorios de usuario dentro del sistema operativo.
     *  <ul>
     * <li>En LINUX contiene el contenido del script a ejecutar.</li>
     * <li>En MacOSX contiene la ruta del script a ejecutar.</li>
     * </ul>
     * @return Listado con todos directorios personales de los usuarios del sistema.
     */
	private static List<String> getFirefoxProfilesUsersDir(final String[] command) {
		if (Platform.OS.LINUX.equals(Platform.getOS())) {
			try {
				final Process process = new ProcessBuilder(command).start();

				String line;
				// arraylist con todos los directorios de usuario
				final List<String> usersDir = new ArrayList<>();
				try (final InputStream resIs = process.getInputStream();
						final BufferedReader resReader = new BufferedReader(new InputStreamReader(resIs));) {
					while ((line = resReader.readLine()) != null) {
						usersDir.add(line);
					}
				}

				return usersDir;
			} catch (final IOException e) {
				LOGGER.info("Error al generar el listado de directorios de usuarios del sistema." + e); //$NON-NLS-1$
				return null;
			}
		}
		// MAC
		else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			try {
				final Object o = ConfiguratorFirefox.executeScriptMacOsx(command[0],false,false);
				final List<String> usersDir = new ArrayList<>();
				String line;
				final String initLine = "dir: "; //$NON-NLS-1$
				try (final InputStream resIs = new ByteArrayInputStream(o.toString().getBytes());
						final BufferedReader resReader = new BufferedReader(new InputStreamReader(resIs));) {
					while ((line = resReader.readLine()) != null) {
						if (line.startsWith(initLine)){
							usersDir.add(line.substring(line.indexOf(initLine)+initLine.length()));

						}
					}
				}

				return usersDir;

			} catch (final IOException e) {
				LOGGER.info("Error al generar el listado de directorios de usuarios del sistema." + e); //$NON-NLS-1$
				return null;
			}
		}

		else {
			throw new IllegalArgumentException("Sistema operativo no soportado."); //$NON-NLS-1$
		}

	}

	/**
	 * Devuelve un listado con los directorios donde se encuentra el fichero profiles.ini de firefox en linux y en MacOSX.
	 * @param users Listado de usuarios del sistema.
	 * @return Listado de directorios donde se encuentra el fichero profiles.ini.
	 */
	private static List<File> getMozillaUsersProfilesPath(final List<String> users){
		String pathProfile = null;
		final List<File> path = new ArrayList<>();
		if (Platform.OS.LINUX.equals(Platform.getOS())) {
			pathProfile = LINUX_MOZILLA_PATH;
		}
		else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			pathProfile = MACOSX_MOZILLA_PATH;
		}
		else {
			throw new IllegalArgumentException("Sistema operativo no soportado."); //$NON-NLS-1$
		}
		for (final String usr: users){
			final File mozillaPath = new File(usr + pathProfile);
			// comprobamos que el fichero exista
			if (mozillaPath.exists() && mozillaPath.isFile()){
				path.add(mozillaPath);
				LOGGER.info("Ruta: " + mozillaPath ); //$NON-NLS-1$
			}
		}

		return path;

	}

	/**
	 * Devuelve un listado de directorios donde se encuentran los perfiles de usuario de firefox en linux.
	 * @param profilesPath Listado de directorios que contienen un fichero profiles.ini.
	 * @return Listado de directorios donde se encuentran los perfiles de usuario de firefox.
	 */
	private static Set<File> getProfiles(final List<File> profilesPath){
		final String PATH = "Path="; //$NON-NLS-1$
	    final Set<File> profile = new HashSet<>();
		for (final File path: profilesPath){
			String line;
			try (final InputStream resIs = new FileInputStream(path);
					final BufferedReader resReader = new BufferedReader(new InputStreamReader(resIs));) {
				while ((line = resReader.readLine()) != null) {
					if (line.startsWith(PATH)){
						final File file = new File(path.getAbsolutePath().substring(0,path.getAbsolutePath().lastIndexOf("/")+1)+line.substring(PATH.length())); //$NON-NLS-1$
						if (file.exists() && file.isDirectory()){
							profile.add(file);
						}

					}
				}
			} catch (final FileNotFoundException e) {
				LOGGER.severe("Error al buscar el directorio profile de firefox, no se ha encontrado el directorio :" + e); //$NON-NLS-1$
			} catch (final IOException e) {
			    LOGGER.severe("Error al buscar el directorio profile de firefox :" + e); //$NON-NLS-1$
			}
		}

		return profile;
	}

	/**
	 * Escribe un script en un fichero dado.
	 * @param path Ruta donde se escribir&aacute; el fichero
	 * @param data Datos a escribir.
	 * @param append  <code> true </code> permite contatenar el contenido del fichero con lo que se va a escribir. <code> false </code> el fichero se sobrescribe.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero.
	 */
	public static void writeScriptFile(final String path, final StringBuilder data, final boolean append ) throws IOException{
		 LOGGER.info("Se escribira en fichero el siguiente comando:\n" + data.toString()); //$NON-NLS-1$
         final File macScript = new File(path);
         data.append("\n"); //$NON-NLS-1$
         try (final FileOutputStream fout = new FileOutputStream(macScript, append);) {
             fout.write(data.toString().getBytes());
         }

	}

}
