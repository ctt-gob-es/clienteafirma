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

    private static final String LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH = "/.config/chromium/Local State";//$NON-NLS-1$
	private static final String LINUX_CHROME_V56_OR_LOWER_PREFS_PATH = "/.config/google-chrome/Local State";//$NON-NLS-1$

	private static final String LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH = "/.config/chromium/Default/Preferences";//$NON-NLS-1$
	private static final String LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH = "/.config/google-chrome/Default/Preferences";//$NON-NLS-1$

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
			// incluya el protocolo "afirma" en el fichero Local State o Preferences (segun la version)
			// para Google Chrome o Chromium
			try {
				createScriptsRemoveChromeAndChromiumWarnings(targetDir, userDir);
			} catch (final IOException e) {
				LOGGER.warning("No se pudieron crear los scripts para registrar el esquema 'afirma' en Chrome: " + e); //$NON-NLS-1$
			}
		}
	}

	/** Genera los scripts que registran/desregistran el esquema "afirma" como un
	 * protocolo de confiable en Chrome.
	 * @param appDir Directorio de instalaci&oacute;n del sistema
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @throws IOException */
	private static void createScriptsRemoveChromeAndChromiumWarnings(final File appDir, final String userDir) throws IOException {

		// Generamos el script de instalacion
		final StringBuilder installationScript = new StringBuilder();
		// Generamos el script de desistalacion
		final StringBuilder uninstallationScript = new StringBuilder();

		// Final del if
		final String[] endIfStatement = new String[] {
				"fi", //$NON-NLS-1$
		};

		/////////////////////////////////////////////////////////////////////////////
		////// Chrome/Chromium v56 o inferior
		/////////////////////////////////////////////////////////////////////////////
		if( new File(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH).isFile() ) {
			//Se incluye afirma como protocolo de confianza en Chromium v56 o inferior
			final String[] commandInstallChromium56OrLower1 =
					addProtocolInPreferencesFile(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH);
			final String[] commandInstallChromium56OrLower2 =
					correctProtocolInPreferencesFile(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH);

			// Comandos para agregar la confianza del esquema 'afirma' en caso de tener Chromium v56 o inferior recien instalado
			final String[] commandInstallChromium56OrLower3 = getIfCommand(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH);
			final String[] commandInstallChromium56OrLower4 = new String[] {
					"sed -i", //$NON-NLS-1$ -i para reemplazar en el propio fichero
					"'s/last_active_profiles\\([^,]*\\),/" //$NON-NLS-1$
					+ "last_active_profiles\\1,\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false}},/'", //$NON-NLS-1$
					escapePath(userDir + LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH) + "1", //$NON-NLS-1$
			};

			// Generacion de script de instalacion
			ConfiguratorUtil.printScript(commandInstallChromium56OrLower1, installationScript);
			ConfiguratorUtil.printScript(commandInstallChromium56OrLower2, installationScript);
			ConfiguratorUtil.printScript(commandInstallChromium56OrLower3, installationScript);
			ConfiguratorUtil.printScript(commandInstallChromium56OrLower4, installationScript);
			ConfiguratorUtil.printScript(endIfStatement, installationScript);
			ConfiguratorUtil.printScript(
					copyConfigurationFile(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH), installationScript);

			// Generacion de script de desinstalacion
			removeProtocolInPreferencesFile1(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH, uninstallationScript);
			removeProtocolInPreferencesFile2(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH, uninstallationScript);
			removeProtocolInPreferencesFile3(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH, uninstallationScript);

			ConfiguratorUtil.printScript(
					copyConfigurationFile(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH), uninstallationScript);
		}

		if( new File(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH).isFile() ) {
			//Se incluye afirma como protocolo de confianza en Chrome v56 o inferior
			final String[] commandInstallChrome56OrLower1 =
					addProtocolInPreferencesFile(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH);

			final String[] commandInstallChrome56OrLower2 =
					correctProtocolInPreferencesFile(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH);

			final String[] commandInstallChrome56OrLower3 = getIfCommand(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH);
			// Comando para agregar la confianza del esquema 'afirma' en caso de tener Chrome v56 o inferior recien instalado
			final String[] commandInstallChrome56OrLower4 = new String[] {
					"sed -i", //$NON-NLS-1$ -i para reemplazar en el propio fichero
					"'s/last_active_profiles\\([^,]*\\),/" //$NON-NLS-1$
					+ "last_active_profiles\\1,\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false}},/'", //$NON-NLS-1$
					escapePath(userDir + LINUX_CHROME_V56_OR_LOWER_PREFS_PATH) + "1", //$NON-NLS-1$
			};

			// Generacion de script de instalacion
			ConfiguratorUtil.printScript(commandInstallChrome56OrLower1, installationScript);
			ConfiguratorUtil.printScript(commandInstallChrome56OrLower2, installationScript);
			ConfiguratorUtil.printScript(commandInstallChrome56OrLower3, installationScript);
			ConfiguratorUtil.printScript(commandInstallChrome56OrLower4, installationScript);
			ConfiguratorUtil.printScript(endIfStatement, installationScript);
			ConfiguratorUtil.printScript(
					copyConfigurationFile(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH), installationScript);

			// Generacion de script de desinstalacion
			removeProtocolInPreferencesFile1(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH, uninstallationScript);
			removeProtocolInPreferencesFile2(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH, uninstallationScript);
			removeProtocolInPreferencesFile3(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH, uninstallationScript);

			ConfiguratorUtil.printScript(
					copyConfigurationFile(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH), uninstallationScript);
		}

		/////////////////////////////////////////////////////////////////////////////
		////// Chrome/Chromium v57 o superior
		/////////////////////////////////////////////////////////////////////////////

		if( new File(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH).isFile() ) {
			//Se incluye afirma como protocolo de confianza en Chromium v57 o superior
			final String[] commandInstallChromium57OrHigher1 =
					addProtocolInPreferencesFile(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH);
			final String[] commandInstallChromium57OrHigher2 =
					correctProtocolInPreferencesFile(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH);

			// Generacion de script de instalacion
			ConfiguratorUtil.printScript(commandInstallChromium57OrHigher1, installationScript);
			ConfiguratorUtil.printScript(commandInstallChromium57OrHigher2, installationScript);
			ConfiguratorUtil.printScript(
					copyConfigurationFile(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH), installationScript);

			// Generacion de script de desinstalacion
			removeProtocolInPreferencesFile1(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH, uninstallationScript);
			removeProtocolInPreferencesFile2(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH, uninstallationScript);
			removeProtocolInPreferencesFile3(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH, uninstallationScript);

			ConfiguratorUtil.printScript(
					copyConfigurationFile(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH), uninstallationScript);
		}
		if( new File(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH).isFile() ) {
			//Se incluye afirma como protocolo de confianza en Chrome v57 o superior
			final String[] commandInstallChrome57OrHigher1 =
					addProtocolInPreferencesFile(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH);
			final String[] commandInstallChrome57OrHigher2 =
					correctProtocolInPreferencesFile(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH);

			// Generacion de script de instalacion
			ConfiguratorUtil.printScript(commandInstallChrome57OrHigher1, installationScript);
			ConfiguratorUtil.printScript(commandInstallChrome57OrHigher2, installationScript);
			ConfiguratorUtil.printScript(
					copyConfigurationFile(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH), installationScript);

			// Generacion de script de desinstalacion
			removeProtocolInPreferencesFile1(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH, uninstallationScript);
			removeProtocolInPreferencesFile2(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH, uninstallationScript);
			removeProtocolInPreferencesFile3(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH, uninstallationScript);

			ConfiguratorUtil.printScript(
					copyConfigurationFile(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH), uninstallationScript);
		}


		// Se almacenan los script de instalación y desinstalación
		try {
			ConfiguratorUtil.writeScript(installationScript, new File(appDir, INSTALL_SCRIPT_NAME));
		}
		catch (final Exception e) {
			throw new IOException("Error al crear el script para agregar la confianza del esquema 'afirma'", e); //$NON-NLS-1$
		}

		try {
			ConfiguratorUtil.writeScript(uninstallationScript, new File(appDir, UNINSTALL_SCRIPT_NAME));
		}
		catch (final Exception e) {
			LOGGER.severe(
					"Excepcion en la creacion del script linux para la modificacion del fichero de protocolos de Google Chrome: " + e //$NON-NLS-1$
					);
		}
	}

	/** Genera los scripts para confirmar si existen protocolos definidos en el fichero.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome. */
	private static String[] getIfCommand(final String userDir, final String browserPath) {
		// If para comprobar si es necesario incluir la sintaxis entera de definicion de protocolos o si,
		// por el contrario, ya estaba
		final String[] ifStatement = new String[] {
				"if ! ", //$NON-NLS-1$
				"grep -q \"excluded_schemes\" " +  //$NON-NLS-1$
				escapePath(userDir + browserPath),
				"; then", //$NON-NLS-1$
		};
		return ifStatement;
	}

	/** Genera los scripts para reemplazar el fichero original por el temporal con el que se estaba trabajando.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome. */
	private static String[] copyConfigurationFile(final String userDir, final String browserPath) {
		// Comando para sobreescribir el fichero de configuracion
		final String[] commandCopy = new String[] {
				"\\cp", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
				escapePath(userDir + browserPath),
		};

		return commandCopy;
	}

	/** Genera los scripts para eliminar el protocolo afirma.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome. */
	private static String[] addProtocolInPreferencesFile(final String userDir, final String browserPath) {

		// Comando para agregar la confianza del esquema 'afirma' en Chrome
		final String[] commandInstall1 = new String[] {
				"sed", //$NON-NLS-1$
				"'s/\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{/" //$NON-NLS-1$
				+ "\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false,/g'", //$NON-NLS-1$
				escapePath(userDir + browserPath),
				">", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};
		return commandInstall1;
	}

	/** Genera los scripts para eliminar la coma en caso de que sea el unico protocolo definido en el fichero.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome. */
	private static String[] correctProtocolInPreferencesFile(final String userDir, final String browserPath) {

		// Comando para eliminar la coma en caso de ser el unico protocolo de confianza
		final String[] commandInstall2 = new String[] {
				"sed -i", //$NON-NLS-1$ -i para reemplazar en el propio fichero
				"'s/\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false,}/" //$NON-NLS-1$
				+ "\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false}/g'", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};
		return commandInstall2;
	}


	/** Genera los scripts para eliminar el protocolo afirma del fichero.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @param sb Objeto para escribir en fichero. */
	private static void removeProtocolInPreferencesFile1(final String userDir, final String browserPath, final StringBuilder sb) {

		// Comando para retirar la confianza del esquema 'afirma'
		final String[] commandUninstall1 = new String[] {
				"sed", //$NON-NLS-1$
				"'s/\\\"afirma\\\":false,//g'", //$NON-NLS-1$
				escapePath(userDir + browserPath),
				">", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};

		ConfiguratorUtil.printScript(commandUninstall1, sb);

	}

	/** Genera los scripts para eliminar el protocolo afirma del fichero.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @param sb Objeto para escribir en fichero. */
	private static void removeProtocolInPreferencesFile2(final String userDir, final String browserPath, final StringBuilder sb) {

		// Comando para retirar la confianza del esquema 'afirma'
		final String[] commandUninstall1 = new String[] {
				"sed -i", //$NON-NLS-1$ -i para reemplazar en el propio fichero
				"'s/\\\"afirma\\\":false//g'", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};

		ConfiguratorUtil.printScript(commandUninstall1, sb);

	}

	/** Genera los scripts para eliminar la sintaxis que define los protocolos de confianza si no existe ninguno.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @param sb Objeto para escribir en fichero. */
	private static void removeProtocolInPreferencesFile3(final String userDir, final String browserPath, final StringBuilder sb) {

		// Comando para retirar la confianza del esquema 'afirma'
		final String[] commandUninstall1 = new String[] {
				"sed -i", //$NON-NLS-1$ -i para reemplazar en el propio fichero
				"'s/\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{}},//g'", //$NON-NLS-1$ Se elimina la lista vacia de protocolos
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};

		ConfiguratorUtil.printScript(commandUninstall1, sb);

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
