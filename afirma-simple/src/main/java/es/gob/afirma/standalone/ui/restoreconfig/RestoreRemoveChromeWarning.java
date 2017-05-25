package es.gob.afirma.standalone.ui.restoreconfig;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * Clase que contiene la l&oacute;gica para realizar las tareas de restauraci&oacute;n
 * de las preferencias de Google Chrome en Windows, Linux y Mac.
 *
 */
final public class RestoreRemoveChromeWarning {

	private static Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH = "/.config/chromium/Local State";//$NON-NLS-1$
	private static final String LINUX_CHROME_V56_OR_LOWER_PREFS_PATH = "/.config/google-chrome/Local State";//$NON-NLS-1$

	private static final String LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH = "/.config/chromium/Default/Preferences";//$NON-NLS-1$
	private static final String LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH = "/.config/google-chrome/Default/Preferences";//$NON-NLS-1$


	// A partir de la version 57 de Chrome cambia el fichero en el que se guardan los protocol handler
	private static final String CHROME_V56_OR_LOWER_CONFIG_FILE = "AppData/Local/Google/Chrome/User Data/Local State"; //$NON-NLS-1$
	private static final String CHROME_V57_OR_HIGHER_CONFIG_FILE = "AppData/Local/Google/Chrome/User Data/Default/Preferences"; //$NON-NLS-1$

	private static final String MAC_CHROME_V56_OR_LOWER_PREFS_PATH = "/Library/Application Support/Google/Chrome/Local State"; //$NON-NLS-1$
	private static final String MAC_CHROME_V57_OR_HIGHER_PREFS_PATH = "/Library/Application Support/Google/Chrome/Default/Preferences"; //$NON-NLS-1$

	/** Nombre del usuario por defecto en Windows. Este usuario es el que se usa como base para
	 * crear nuevos usuarios y no se deber&iacute;a tocar. */
	private static String DEFAULT_WINDOWS_USER_NAME = "Default"; //$NON-NLS-1$

	private static final String RECONFIG_SCRIPT_NAME = "reconfig.sh"; //$NON-NLS-1$


	/** Genera los comandos que elimina el warning al ejecutar AutoFirma desde Chrome.
	 * En MAC genera el script que hay que ejecutar para realizar la instalaci&oacute;n pero no lo ejecuta, de eso se encarga el instalador Debian.
	 *  <ul>
	 * <li>En MAC contiene el contenido del script a ejecutar.</li>
	 * </ul>
	 * @param workingDir Directorio donde se crea el script.
	 * @param usersDirs Directorio de los usuarios del sistema.
	 */
	public static void removeChromeWarningsMac(final File workingDir, final List<String> usersDirs) {
		try {
			for (final String userDir : usersDirs) {
				// Montamos los comandos de instalacion y desinstalacion que
				// incluya el protocolo "afirma" en el fichero Local State o Preferences (segun la version)
				// para Google Chrome o Chromium

				// Se escriben los comandos de reconfiguracion
				final StringBuilder reconfigScript = new StringBuilder();

				final ArrayList<String[]> installCommands = getCommandsToRemoveChromeAndChromiumWarningsOnInstallMac(workingDir, userDir);
				final Iterator<String[]> list = installCommands.iterator();
				while(list.hasNext()) {
					ConfiguratorUtil.printScript(list.next(), reconfigScript);
				}

				// Se almacenan los script de reconfiguracion en un fichero
				try {
					ConfiguratorUtil.writeScript(reconfigScript, new File(workingDir, RECONFIG_SCRIPT_NAME));
				}
				catch (final Exception e) {
					throw new IOException("Error al crear el script para agregar la confianza del esquema 'afirma'", e); //$NON-NLS-1$
				}
			}

			// Se ejecuta el script creado
			new ProcessBuilder("chmod", "u+x", workingDir + "/" + RECONFIG_SCRIPT_NAME).start(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

			// Se ejecuta el script creado
			new ProcessBuilder("sh", workingDir + "/" + RECONFIG_SCRIPT_NAME).start(); //$NON-NLS-1$ //$NON-NLS-2$

			// Se elimina el script tras ejecutarlo
			new ProcessBuilder("rm", workingDir + "/" + RECONFIG_SCRIPT_NAME).start(); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (final IOException e) {
			LOGGER.warning("No se pudieron crear los scripts para registrar el esquema 'afirma' en Chrome: " + e); //$NON-NLS-1$
		}
	}

	/** Genera los comandos que elimina el warning al ejecutar AutoFirma desde Chrome.
	 * En linux genera el script que hay que ejecutar para realizar la instalaci&oacute;n pero no lo ejecuta, de eso se encarga el instalador Debian.
	 *  <ul>
	 * <li>En LINUX contiene el contenido del script a ejecutar.</li>
	 * </ul>
	 * @param workingDir Directorio donde se crea el script.
	 * @param usersDirs Directorio de los usuarios del sistema.
	 */
	public static void removeChromeWarningsLinux(final File workingDir, final List<String> usersDirs) {


		try {
			for (final String userDir : usersDirs) {
				// Montamos los comandos de instalacion y desinstalacion que
				// incluya el protocolo "afirma" en el fichero Local State o Preferences (segun la version)
				// para Google Chrome o Chromium

				// Se escriben los comandos de reconfiguracion
				final StringBuilder reconfigScript = new StringBuilder();
				// Comandos de desinstalacion
				final ArrayList<String[]> uninstallCommands = getCommandsToRemoveChromeAndChromiumWarningsOnUninstall(userDir);
				final Iterator<String[]> list2 = uninstallCommands.iterator();

				while(list2.hasNext()) {
					ConfiguratorUtil.printScript(list2.next(), reconfigScript);
				}

				// Comandos de instalacion
				final ArrayList<String[]> installCommands = getCommandsToRemoveChromeAndChromiumWarningsOnInstallLinux(userDir);
				final Iterator<String[]> list = installCommands.iterator();
				while(list.hasNext()) {
					ConfiguratorUtil.printScript(list.next(), reconfigScript);
				}

				// Se almacenan los script de reconfiguracion en un fichero
				try {
					ConfiguratorUtil.writeScript(reconfigScript, new File(workingDir, RECONFIG_SCRIPT_NAME));
				}
				catch (final Exception e) {
					throw new IOException("Error al crear el script para agregar la confianza del esquema 'afirma'", e); //$NON-NLS-1$
				}
			}

			// Se ejecuta el script creado
			new ProcessBuilder("sh", workingDir + "/" + RECONFIG_SCRIPT_NAME).start(); //$NON-NLS-1$ //$NON-NLS-2$
			// Se elimina el script tras ejecutarlo
			new ProcessBuilder("rm", workingDir + "/" + RECONFIG_SCRIPT_NAME).start(); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (final IOException e) {
			LOGGER.warning("No se pudieron crear los scripts para registrar el esquema 'afirma' en Chrome: " + e); //$NON-NLS-1$
		}
	}

	/**
	 * Configura el protocolo "afirma" en Chrome para todos los usuarios de Windows.
	 * (Repetido en ConfiguradorWindows)
	 * @param window Consola de salida.
	 * @param installing Indica si se debe configurar ({@code true}) o desconfigurar
	 * ({@code false}) el protocolo "afirma" en Chrome.
	 */
	public static void removeChromeWarningsWindows(final Console window, final boolean installing) {

		if (window != null && installing) {
			window.print(SimpleAfirmaMessages.getString("ConfiguratorWindows.16")); //$NON-NLS-1$
		}

		final File usersDir = new File(System.getProperty("user.home")).getParentFile(); //$NON-NLS-1$
		for (final File userDir : usersDir.listFiles()) {
			if (userDir.isDirectory() && !DEFAULT_WINDOWS_USER_NAME.equalsIgnoreCase(userDir.getName())) {
				try {
					final File chromeConfigFileV56OrLower = new File(userDir, CHROME_V56_OR_LOWER_CONFIG_FILE);
					if (chromeConfigFileV56OrLower.isFile() && chromeConfigFileV56OrLower.canWrite()) {
						String config;
						try (final FileInputStream fis = new FileInputStream(chromeConfigFileV56OrLower)) {
							config = new String(AOUtil.getDataFromInputStream(fis), StandardCharsets.UTF_8);
						}
						config = config.replace("\"afirma\":false,", ""); //$NON-NLS-1$ //$NON-NLS-2$
						config = config.replace("\"afirma\":false", ""); //$NON-NLS-1$ //$NON-NLS-2$

						if (installing) {
							config = config.replace("\"protocol_handler\":{\"excluded_schemes\":{", //$NON-NLS-1$
									"\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false,"); //$NON-NLS-1$
							config = config.replace("\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false,}", //$NON-NLS-1$
									"\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false}"); //$NON-NLS-1$
							// En caso de que Google Chrome este recien instalado no encontrara cadena a reemplazar,
							// por lo que directamente insertaremos la cadena nosotros en el lugar correspondiente
							if(!config.contains("excluded_schemes")) { //$NON-NLS-1$
								config = config.replaceAll("last_active_profiles([^,]*),", //$NON-NLS-1$
										"last_active_profiles$1,\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false}},"); //$NON-NLS-1$
							}
						}
						else {
							// Elimina la sintaxis que define los protocolos de confianza si no existe ninguno.
							config = config.replace("\"protocol_handler\":{\"excluded_schemes\":{}},", ""); //$NON-NLS-1$ //$NON-NLS-2$
						}
						try (final FileOutputStream fos = new FileOutputStream(chromeConfigFileV56OrLower)) {
							fos.write(config.getBytes(StandardCharsets.UTF_8));
						}
					}
					final File chromeConfigFileV57OrHigher = new File(userDir, CHROME_V57_OR_HIGHER_CONFIG_FILE);
					if (chromeConfigFileV57OrHigher.isFile() && chromeConfigFileV57OrHigher.canWrite()) {
						String config;
						try (final FileInputStream fis = new FileInputStream(chromeConfigFileV57OrHigher)) {
							config = new String(AOUtil.getDataFromInputStream(fis), StandardCharsets.UTF_8);
						}
						config = config.replace("\"afirma\":false,", ""); //$NON-NLS-1$ //$NON-NLS-2$
						config = config.replace("\"afirma\":false", ""); //$NON-NLS-1$ //$NON-NLS-2$
						if (installing) {
							config = config.replace("\"protocol_handler\":{\"excluded_schemes\":{", //$NON-NLS-1$
									"\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false,"); //$NON-NLS-1$
							config = config.replace("\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false,}", //$NON-NLS-1$
									"\"protocol_handler\":{\"excluded_schemes\":{\"afirma\":false}"); //$NON-NLS-1$
							// En caso de que Google Chrome este recien instalado no encontrara cadena a reemplazar,
							// pero si se ha insertado en el fichero Local State del paso anterior se configurara automaticamente
							// en el Default/Preferences cuando se invoque el protocolo afirma
						}
						try (final FileOutputStream fos = new FileOutputStream(chromeConfigFileV57OrHigher)) {
							fos.write(config.getBytes(StandardCharsets.UTF_8));
						}
					}
				}
				catch (final Exception e) {
					if (window != null) {
						window.print(String.format(SimpleAfirmaMessages.getString("ConfiguratorWindows.15"), userDir.getName())); //$NON-NLS-1$
					}
					LOGGER.warning("No se pudo configurar Chrome para el usuario " + userDir + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}
	}

	/** Genera los comandos que desregistran el esquema "afirma" como un
	 * protocolo de confiable en Chrome. (Repetido en ConfiguratorLinux)
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @throws IOException */
	private static ArrayList<String[]> getCommandsToRemoveChromeAndChromiumWarningsOnUninstall(final String userDir) throws IOException {

		final ArrayList<String[]> commandList = new ArrayList<>();

		// Final del if
				final String[] endIfStatement = new String[] {
						"fi", //$NON-NLS-1$
				};

		final String[] ifFileFound1 =
				getIfFileFoundCommand(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH);


		// Generacion de comandos de desinstalacion
		/////////////////////////////////////////////////////////////////////////////
		////// Chromium v56 o inferior
		/////////////////////////////////////////////////////////////////////////////
		commandList.add(ifFileFound1);
		removeProtocolInPreferencesFile1(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH, commandList);
		removeProtocolInPreferencesFile2(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH, commandList);
		removeProtocolInPreferencesFile3(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH, commandList);
		commandList.add(
				copyConfigurationFile(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH));
		commandList.add(endIfStatement);

		/////////////////////////////////////////////////////////////////////////////
		////// Chrome v56 o inferior
		/////////////////////////////////////////////////////////////////////////////

		final String[] ifFileFound2 =
				getIfFileFoundCommand(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH);
		// Generacion de comandos de desinstalacion
		commandList.add(ifFileFound2);
		removeProtocolInPreferencesFile1(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH, commandList);
		removeProtocolInPreferencesFile2(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH, commandList);
		removeProtocolInPreferencesFile3(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH, commandList);
		commandList.add(
				copyConfigurationFile(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH));
		commandList.add(endIfStatement);

		/////////////////////////////////////////////////////////////////////////////
		////// Chromium v57 o superior
		/////////////////////////////////////////////////////////////////////////////

		final String[] ifFileFound3 =
				getIfFileFoundCommand(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH);

		// Generacion de comandos de desinstalacion
		commandList.add(ifFileFound3);
		removeProtocolInPreferencesFile1(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH, commandList);
		removeProtocolInPreferencesFile2(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH, commandList);
		removeProtocolInPreferencesFile3(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH, commandList);
		commandList.add(
				copyConfigurationFile(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH));
		commandList.add(endIfStatement);

		/////////////////////////////////////////////////////////////////////////////
		////// Chrome v57 o superior
		/////////////////////////////////////////////////////////////////////////////
		final String[] ifFileFound4 =
				getIfFileFoundCommand(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH);

		// Generacion de comandos de desinstalacion
		commandList.add(ifFileFound4);
		removeProtocolInPreferencesFile1(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH, commandList);
		removeProtocolInPreferencesFile2(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH, commandList);
		removeProtocolInPreferencesFile3(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH, commandList);
		commandList.add(
				copyConfigurationFile(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH));
		commandList.add(endIfStatement);

		return commandList;

	}

	/** Genera los scripts que registran el esquema "afirma" como un
	 * protocolo de confiable en Chrome. (Repetido en ConfiguratorLinux)
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @throws IOException */
	private static ArrayList<String[]> getCommandsToRemoveChromeAndChromiumWarningsOnInstallLinux(final String userDir) throws IOException {

		final ArrayList<String[]> commandList = new ArrayList<>();
		// Final del if
		final String[] endIfStatement = new String[] {
				"fi", //$NON-NLS-1$
		};

		/////////////////////////////////////////////////////////////////////////////
		////// Chromium v56 o inferior
		/////////////////////////////////////////////////////////////////////////////
		if( new File(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH).isFile() ) {
			//Se incluye afirma como protocolo de confianza en Chromium v56 o inferior
			final String[] commandInstallChromium56OrLower1 =
					addProtocolInPreferencesFile(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH);
			final String[] commandInstallChromium56OrLower2 =
					correctProtocolInPreferencesFile(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH);

			// Comandos para agregar la confianza del esquema 'afirma' en caso de tener Chromium v56 o inferior recien instalado
			final String[] ifContainsString1 = getIfNotContainsStringCommand(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH);
			final String[] commandInstallChromium56OrLower4 = new String[] {
					"sed -i", //$NON-NLS-1$ -i para reemplazar en el propio fichero
					"'s/last_active_profiles\\([^,]*\\),/" //$NON-NLS-1$
					+ "last_active_profiles\\1,\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false}},/'", //$NON-NLS-1$
					escapePath(userDir + LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH) + "1", //$NON-NLS-1$
			};

			// Generacion de comandos de instalacion
			commandList.add(commandInstallChromium56OrLower1);
			commandList.add(commandInstallChromium56OrLower2);
			commandList.add(ifContainsString1);
			commandList.add(commandInstallChromium56OrLower4);
			commandList.add(endIfStatement);
			commandList.add(
					copyConfigurationFile(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH));
		}


		/////////////////////////////////////////////////////////////////////////////
		////// Chrome v56 o inferior
		/////////////////////////////////////////////////////////////////////////////
		if( new File(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH).isFile() ) {
			//Se incluye afirma como protocolo de confianza en Chrome v56 o inferior
			final String[] commandInstallChrome56OrLower1 =
					addProtocolInPreferencesFile(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH);

			final String[] commandInstallChrome56OrLower2 =
					correctProtocolInPreferencesFile(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH);

			final String[] ifContainsString2 = getIfNotContainsStringCommand(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH);
			// Comando para agregar la confianza del esquema 'afirma' en caso de tener Chrome v56 o inferior recien instalado
			final String[] commandInstallChrome56OrLower4 = new String[] {
					"sed -i", //$NON-NLS-1$ -i para reemplazar en el propio fichero
					"'s/last_active_profiles\\([^,]*\\),/" //$NON-NLS-1$
					+ "last_active_profiles\\1,\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false}},/'", //$NON-NLS-1$
					escapePath(userDir + LINUX_CHROME_V56_OR_LOWER_PREFS_PATH) + "1", //$NON-NLS-1$
			};

			// Generacion de comandos de instalacion
			commandList.add(commandInstallChrome56OrLower1);
			commandList.add(commandInstallChrome56OrLower2);
			commandList.add(ifContainsString2);
			commandList.add(commandInstallChrome56OrLower4);
			commandList.add(endIfStatement);
			commandList.add(
					copyConfigurationFile(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH));
		}

		/////////////////////////////////////////////////////////////////////////////
		////// Chromium v57 o superior
		/////////////////////////////////////////////////////////////////////////////

		if( new File(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH).isFile() ) {
			//Se incluye afirma como protocolo de confianza en Chromium v57 o superior
			final String[] commandInstallChromium57OrHigher1 =
					addProtocolInPreferencesFile(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH);
			final String[] commandInstallChromium57OrHigher2 =
					correctProtocolInPreferencesFile(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH);

			// Generacion de comandos de instalacion
			commandList.add(commandInstallChromium57OrHigher1);
			commandList.add(commandInstallChromium57OrHigher2);
			commandList.add(
					copyConfigurationFile(userDir, LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH));
		}

		/////////////////////////////////////////////////////////////////////////////
		////// Chrome v57 o superior
		/////////////////////////////////////////////////////////////////////////////
		if( new File(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH).isFile() ) {
			//Se incluye afirma como protocolo de confianza en Chrome v57 o superior
			final String[] commandInstallChrome57OrHigher1 =
					addProtocolInPreferencesFile(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH);
			final String[] commandInstallChrome57OrHigher2 =
					correctProtocolInPreferencesFile(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH);

			// Generacion de comandos de instalacion
			commandList.add(commandInstallChrome57OrHigher1);
			commandList.add(commandInstallChrome57OrHigher2);
			commandList.add(
					copyConfigurationFile(userDir, LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH));

		}
		return commandList;
	}

	/** Genera los scripts que registran el esquema "afirma" como un
	 * protocolo de confiable en Chrome. (Repetido en ConfiguratorMacOSX)
	 * @param appDir Directorio de instalaci&oacute;n del sistema
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @throws IOException */
	private static ArrayList<String[]> getCommandsToRemoveChromeAndChromiumWarningsOnInstallMac(final File appDir, final String userDir) throws IOException {

		final ArrayList<String[]> commandList = new ArrayList<>();
		// Final del if
		final String[] endIfStatement = new String[] {
				"fi", //$NON-NLS-1$
		};

		/////////////////////////////////////////////////////////////////////////////
		////// Chrome v56 o inferior
		/////////////////////////////////////////////////////////////////////////////
		if( new File(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH).isFile() ) {
			//Se incluye afirma como protocolo de confianza en Chrome v56 o inferior

			final String[] commandInstallChrome56OrLower01 =
					deleteProtocolInPreferencesFile1(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH);
			final String[] commandInstallChrome56OrLower02 =
					deleteProtocolInPreferencesFile2(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH);

			final String[] commandInstallChrome56OrLower1 =
					addProtocolInPreferencesFileMac(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH);

			final String[] commandInstallChrome56OrLower2 =
					correctProtocolInPreferencesFileMac(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH);

			final String[] ifContainsString2 = getIfNotCointainsStringCommand(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH);
			// Comando para agregar la confianza del esquema 'afirma' en caso de tener Chrome v56 o inferior recien instalado
			final String[] commandInstallChrome56OrLower4 = new String[] {
					"sed -i ''", //$NON-NLS-1$ -i para reemplazar en el propio fichero
					"'s/last_active_profiles\\([^,]*\\),/" //$NON-NLS-1$
					+ "last_active_profiles\\1,\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false}},/'", //$NON-NLS-1$
					escapePath(userDir + MAC_CHROME_V56_OR_LOWER_PREFS_PATH) + "1", //$NON-NLS-1$
			};

			// Generacion de comandos de instalacion
			commandList.add(commandInstallChrome56OrLower01);
			commandList.add(commandInstallChrome56OrLower02);
			commandList.add(commandInstallChrome56OrLower1);
			commandList.add(commandInstallChrome56OrLower2);
			commandList.add(ifContainsString2);
			commandList.add(commandInstallChrome56OrLower4);
			commandList.add(endIfStatement);
			commandList.add(
					copyConfigurationFile(userDir, MAC_CHROME_V56_OR_LOWER_PREFS_PATH));
		}

		/////////////////////////////////////////////////////////////////////////////
		////// Chrome v57 o superior
		/////////////////////////////////////////////////////////////////////////////
		if( new File(userDir, MAC_CHROME_V57_OR_HIGHER_PREFS_PATH).isFile() ) {
			//Se incluye afirma como protocolo de confianza en Chrome v57 o superior
			final String[] commandInstallChrome57OrHigher01 =
					deleteProtocolInPreferencesFile1(userDir, MAC_CHROME_V57_OR_HIGHER_PREFS_PATH);
			final String[] commandInstallChrome57OrHigher02 =
					deleteProtocolInPreferencesFile2(userDir, MAC_CHROME_V57_OR_HIGHER_PREFS_PATH);
			final String[] commandInstallChrome57OrHigher1 =
					addProtocolInPreferencesFileMac(userDir, MAC_CHROME_V57_OR_HIGHER_PREFS_PATH);
			final String[] commandInstallChrome57OrHigher2 =
					correctProtocolInPreferencesFileMac(userDir, MAC_CHROME_V57_OR_HIGHER_PREFS_PATH);

			// Generacion de comandos de instalacion
			commandList.add(commandInstallChrome57OrHigher01);
			commandList.add(commandInstallChrome57OrHigher02);
			commandList.add(commandInstallChrome57OrHigher1);
			commandList.add(commandInstallChrome57OrHigher2);
			commandList.add(
					copyConfigurationFile(userDir, MAC_CHROME_V57_OR_HIGHER_PREFS_PATH));

		}
		return commandList;
	}

	/** Genera los scripts para confirmar si existen protocolos definidos en el fichero.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome. */
	private static String[] getIfNotCointainsStringCommand(final String userDir, final String browserPath) {
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

	/** Genera los scripts para confirmar si existen protocolos definidos en el fichero. (Repetido en
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome. */
	private static String[] getIfNotContainsStringCommand(final String userDir, final String browserPath) {
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

	/** Genera los scripts para confirmar si existe el fichero con el que se va a trabajar.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome. */
	private static String[] getIfFileFoundCommand(final String userDir, final String browserPath) {
		// If para comprobar si es necesario incluir la sintaxis entera de definicion de protocolos o si,
		// por el contrario, ya estaba
		final String[] ifStatement = new String[] {
				"if [ -f ", //$NON-NLS-1$
				escapePath(userDir + browserPath),
				" ]; then", //$NON-NLS-1$
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
	private static String[] deleteProtocolInPreferencesFile1(final String userDir, final String browserPath) {

		// Comando para agregar la confianza del esquema 'afirma' en Chrome
		final String[] commandInstall1 = new String[] {
				"sed", //$NON-NLS-1$
				"'s/\\\"afirma\\\":false,//g'", //$NON-NLS-1$
				escapePath(userDir + browserPath),
				">", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};
		return commandInstall1;
	}

	/** Genera los scripts para eliminar el protocolo afirma.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome. */
	private static String[] deleteProtocolInPreferencesFile2(final String userDir, final String browserPath) {

		// Comando para agregar la confianza del esquema 'afirma' en Chrome
		final String[] commandInstall1 = new String[] {
				"sed -i ''", //$NON-NLS-1$
				"'s/\\\"afirma\\\":false//g'", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};
		return commandInstall1;
	}

	/** Genera los scripts para incluir el protocolo afirma.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome. */
	private static String[] addProtocolInPreferencesFileMac(final String userDir, final String browserPath) {

		// Comando para agregar la confianza del esquema 'afirma' en Chrome
		final String[] commandInstall1 = new String[] {
				"sed -i ''", //$NON-NLS-1$
				"'s/\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{/" //$NON-NLS-1$
				+ "\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false,/g'", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};
		return commandInstall1;
	}

	/** Genera los scripts para eliminar la coma en caso de que sea el unico protocolo definido en el fichero.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome. */
	private static String[] correctProtocolInPreferencesFileMac(final String userDir, final String browserPath) {

		// Comando para eliminar la coma en caso de ser el unico protocolo de confianza
		final String[] commandInstall2 = new String[] {
				"sed -i ''", //$NON-NLS-1$ -i para reemplazar en el propio fichero
				"'s/\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false,}/" //$NON-NLS-1$
				+ "\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{\\\"afirma\\\":false}/g'", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};
		return commandInstall2;
	}

	/** Genera los scripts para incluir el protocolo afirma.
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
	private static void removeProtocolInPreferencesFile1(final String userDir, final String browserPath, final ArrayList<String[]> commandList) {

		// Comando para retirar la confianza del esquema 'afirma'
		final String[] commandUninstall1 = new String[] {
				"sed", //$NON-NLS-1$
				"'s/\\\"afirma\\\":false,//g'", //$NON-NLS-1$
				escapePath(userDir + browserPath),
				">", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};

		commandList.add(commandUninstall1);

	}

	/** Genera los scripts para eliminar el protocolo afirma del fichero.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @param sb Objeto para escribir en fichero. */
	private static void removeProtocolInPreferencesFile2(final String userDir, final String browserPath, final ArrayList<String[]> commandList) {

		// Comando para retirar la confianza del esquema 'afirma'
		final String[] commandUninstall1 = new String[] {
				"sed -i", //$NON-NLS-1$ -i para reemplazar en el propio fichero
				"'s/\\\"afirma\\\":false//g'", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};

		commandList.add(commandUninstall1);

	}

	/** Genera los scripts para eliminar la sintaxis que define los protocolos de confianza si no existe ninguno.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @param sb Objeto para escribir en fichero. */
	private static void removeProtocolInPreferencesFile3(final String userDir, final String browserPath, final ArrayList<String[]> commandList) {

		// Comando para retirar la confianza del esquema 'afirma'
		final String[] commandUninstall1 = new String[] {
				"sed -i", //$NON-NLS-1$ -i para reemplazar en el propio fichero
				"'s/\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{}},//g'", //$NON-NLS-1$ Se elimina la lista vacia de protocolos
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};

		commandList.add(commandUninstall1);

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

}
