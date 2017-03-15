package es.gob.afirma.standalone.configurator;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.standalone.configurator.CertUtil.CertPack;

/** Configura la instalaci&oacute;n en Linux para la correcta ejecuci&oacute;n de AutoFirma. */
final class ConfiguratorLinux implements Configurator {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String LINUX_CHROMIUM_PREFS_PATH = "/.config/chromium/Local State";//$NON-NLS-1$
	private static final String LINUX_CHROME_PREFS_PATH = "/.config/google-chrome/Local State";//$NON-NLS-1$

	private static final String UNINSTALL_SCRIPT_NAME = "uninstall.sh"; //$NON-NLS-1$
	private static final String INSTALL_SCRIPT_NAME = "script.sh"; //$NON-NLS-1$

    private static final String KS_FILENAME = "autofirma.pfx"; //$NON-NLS-1$
    private static final String FILE_AUTOFIRMA_CERTIFICATE = "AutoFirma_ROOT.cer"; //$NON-NLS-1$
    private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$

    @Override
    public void configure(final Console window) throws IOException, GeneralSecurityException {

        LOGGER.info(Messages.getString("ConfiguratorLinux.2")); //$NON-NLS-1$

        final File appDir = ConfiguratorUtil.getApplicationDirectory();

        LOGGER.info(Messages.getString("ConfiguratorLinux.3") + appDir.getAbsolutePath()); //$NON-NLS-1$

        if (!checkSSLKeyStoreGenerated(appDir)) {
            LOGGER.info(Messages.getString("ConfiguratorLinux.5")); //$NON-NLS-1$

            final CertPack certPack = CertUtil.getCertPackForLocalhostSsl(
                ConfiguratorUtil.CERT_ALIAS,
                KS_PASSWORD
            );

            LOGGER.info(Messages.getString("ConfiguratorLinux.11")); //$NON-NLS-1$

           //Generacion del certificado pfx
            ConfiguratorUtil.installFile(certPack.getPkcs12(), new File(
            		ConfiguratorUtil.getApplicationDirectory(), KS_FILENAME));

          //Generacion del certificado raiz .cer
            ConfiguratorUtil.installFile(
            		certPack.getCaCertificate().getEncoded(),
            		new File(ConfiguratorUtil.getApplicationDirectory(), FILE_AUTOFIRMA_CERTIFICATE));

            // Obtenemos los directorios de los usuarios
    		final String[] usersDirs = getSystemUsersHomes();

            createScriptsRemoveChromeWarnings(appDir, usersDirs);

            try {
                LOGGER.info(Messages.getString("ConfiguratorLinux.13")); //$NON-NLS-1$
                ConfiguratorFirefoxLinux.createScriptsToSystemKeyStore(appDir, usersDirs,
                		new File(appDir, ConfiguratorLinux.INSTALL_SCRIPT_NAME),
                		new File(appDir, ConfiguratorLinux.UNINSTALL_SCRIPT_NAME));
                ConfiguratorFirefoxLinux.createScriptsToMozillaKeyStore(appDir, usersDirs,
                		new File(appDir, ConfiguratorLinux.INSTALL_SCRIPT_NAME),
                		new File(appDir, ConfiguratorLinux.UNINSTALL_SCRIPT_NAME));
              }
            catch(final MozillaProfileNotFoundException e) {
                LOGGER.warning(Messages.getString("ConfiguratorLinux.12")); //$NON-NLS-1$
            }
        }
        else {
            LOGGER.info(Messages.getString("ConfiguratorLinux.14")); //$NON-NLS-1$
        }

        LOGGER.info(Messages.getString("ConfiguratorLinux.8")); //$NON-NLS-1$
    }

    /** Comprueba si ya existe un almac&eacute;n de certificados generado.
     * @param appConfigDir Directorio de configuraci&oacute;n de la aplicaci&oacute;n.
     * @return {@code true} si ya existe un almacen de certificados SSL, {@code false} en caso contrario. */
    private static boolean checkSSLKeyStoreGenerated(final File appConfigDir) {
        return new File(appConfigDir, KS_FILENAME).exists();
    }

    /** Obtiene los directorios de usuarios del sistema.
	 * @return Listado con todos directorios de los usuarios del sistema.
     * @throws IOException Cuando no se puede obtener el listado de directorios. */
	private static String[] getSystemUsersHomes() throws IOException {

        // Comando para sacar los usuarios del sistema
        final String[] command = new String[] {
				"cut", //$NON-NLS-1$
				"-d:", //$NON-NLS-1$
				"-f6", //$NON-NLS-1$
				"/etc/passwd" //$NON-NLS-1$
				};

		try {
			final Process process = new ProcessBuilder(command).start();

			String line;
			// arraylist con todos los directorios de usuario
			final List<String> usersDir = new ArrayList<>();
			try (
					final InputStream resIs = process.getInputStream();
					final BufferedReader resReader = new BoundedBufferedReader(
							new InputStreamReader(resIs),
							2048, // Maximo 256 lineas de salida (256 perfiles)
							2048 // Maximo 2048 caracteres por linea
							);
					) {
				while ((line = resReader.readLine()) != null) {
					usersDir.add(line);
				}
			}

			return usersDir.toArray(new String[usersDir.size()]);
		}
		catch (final Exception e) {
			LOGGER.severe("Error al obtener el listado de directorios de usuarios del sistema: " + e); //$NON-NLS-1$
			throw new IOException("No se pudo obtener el listado de directorios de usuarios del sistema", e); //$NON-NLS-1$
		}
	}

    /** Genera el script que elimina el warning al ejecutar AutoFirma desde Chrome.
	 * En linux genera el script que hay que ejecutar para realizar la instalaci&oacute;n pero no lo ejecuta, de eso se encarga el instalador Debian.
	 * @param targetDir Directorio de instalaci&oacute;n del sistema
	 *  <ul>
	 * <li>En LINUX contiene el contenido del script a ejecutar.</li>
	 * </ul>
	 */
	private static void createScriptsRemoveChromeWarnings(final File targetDir, final String[] usersDirs) {

		for (final String userDir : usersDirs) {
			// Montamos el script de instalacion y desinstalacion que
			// incluya el protocolo "afirma" en el fichero Local State
			if( new File(userDir, LINUX_CHROME_PREFS_PATH).isFile() ) {
				try {
					createScriptsRemoveChromeWarnings(targetDir, userDir, LINUX_CHROME_PREFS_PATH);
				} catch (final IOException e) {
					LOGGER.warning("No se pudieron crear los scripts para registrar el esquema 'afirma' en Chrome: " + e); //$NON-NLS-1$
				}
			}
			if ( new File(userDir, LINUX_CHROMIUM_PREFS_PATH).isFile() ) {
				try {
					createScriptsRemoveChromeWarnings(targetDir, userDir, LINUX_CHROMIUM_PREFS_PATH);
				} catch (final IOException e) {
					LOGGER.warning("No se pudieron crear los scripts para registrar el esquema 'afirma' en Chromium: " + e); //$NON-NLS-1$
				}
			}
		}
	}

	/** Genera los scripts que registran/desregistran el esquema "afirma" como un
	 * protocolo de confiable en Chrome.
	 * @param appDir Directorio de instalaci&oacute;n del sistema
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @throws IOException */
	private static void createScriptsRemoveChromeWarnings(final File appDir, final String userDir, final String browserPath) throws IOException {

		// Comando para sobreescribir el fichero de configuracion
		final String[] commandCopy = new String[] {
				"\\cp", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
				escapePath(userDir + browserPath),
		};

		// Comando para agregar la confianza del esquema 'afirma'
		final String[] commandInstall = new String[] {
				"sed", //$NON-NLS-1$
				"s/\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{/\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false,/g", //$NON-NLS-1$
				escapePath(userDir + browserPath),
				">", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};

		// Comando para retirar la confianza del esquema 'afirma'
		final String[] commandUninstall = new String[] {
				"sed", //$NON-NLS-1$
				"s/\\\"afirma\\\":false,//g", //$NON-NLS-1$
				escapePath(userDir + browserPath),
				">", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};

		// Generamos el script de instalacion
		final StringBuilder installationScript = new StringBuilder();
		ConfiguratorUtil.printScript(commandInstall, installationScript);
		ConfiguratorUtil.printScript(commandCopy, installationScript);

		try {
			ConfiguratorUtil.writeScript(installationScript, new File(appDir, INSTALL_SCRIPT_NAME));
		}
		catch (final Exception e) {
			throw new IOException("Error al crear el script para agregar la confianza del esquema 'afirma' al fichero " + browserPath, e); //$NON-NLS-1$
		}

		// Generamos el script de desistalacion
		final StringBuilder uninstallationScript = new StringBuilder();
		ConfiguratorUtil.printScript(commandUninstall, uninstallationScript);
		ConfiguratorUtil.printScript(commandCopy, uninstallationScript);

		try {
			ConfiguratorUtil.writeScript(uninstallationScript, new File(appDir, UNINSTALL_SCRIPT_NAME));
		}
		catch (final Exception e) {
			LOGGER.severe(
					"Excepcion en la creacion del script linux para la modificacion del fichero de protocolos de Google Chrome: " + e //$NON-NLS-1$
					);
		}
	}

	/**
	 * Escapa rutas de fichero para poder usarlas como parte de un script.
	 * @param path Ruta de fichero.
	 * @return Ruta escapada.
	 */
	private static String escapePath(final String path) {
		if (path == null) {
			throw new IllegalArgumentException(
				"La ruta a 'escapar' no puede ser nula" //$NON-NLS-1$
			);
		}
		return path.replace(" ", "\\ "); //$NON-NLS-1$ //$NON-NLS-2$
	}

    @Override
    public void uninstall() {
        // No es necesario hacer nada porque el  proceso de desinstalacion de linux
        // eliminara el directorio de aplicacion con todo su contenido.
    }
}
