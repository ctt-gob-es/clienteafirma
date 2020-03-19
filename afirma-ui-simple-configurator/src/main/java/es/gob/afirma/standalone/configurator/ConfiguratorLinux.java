/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.FileVisitOption;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.standalone.configurator.CertUtil.CertPack;

/** Configura la instalaci&oacute;n en Linux para la correcta ejecuci&oacute;n de AutoFirma. */
final class ConfiguratorLinux implements Configurator {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH = "/.config/chromium/Local State";//$NON-NLS-1$
	private static final String LINUX_CHROME_V56_OR_LOWER_PREFS_PATH = "/.config/google-chrome/Local State";//$NON-NLS-1$

	private static final String LINUX_CHROMIUM_V57_OR_HIGHER_PREFS_PATH = "/.config/chromium/Default/Preferences";//$NON-NLS-1$
	private static final String LINUX_CHROME_V57_OR_HIGHER_PREFS_PATH = "/.config/google-chrome/Default/Preferences";//$NON-NLS-1$

	private static final String ALTERNATIVE_APP_SUBDIR = ".afirma/AutoFirma"; //$NON-NLS-1$

	private static final String UNINSTALL_SCRIPT_NAME = "uninstall.sh"; //$NON-NLS-1$
	private static final String INSTALL_SCRIPT_NAME = "script.sh"; //$NON-NLS-1$

    private static final String KS_FILENAME = "autofirma.pfx"; //$NON-NLS-1$
    private static final String FILE_AUTOFIRMA_CERTIFICATE = "AutoFirma_ROOT.cer"; //$NON-NLS-1$
    private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$

	private final boolean jnlpInstance;

    public ConfiguratorLinux(final boolean jnlpInstance) {
		this.jnlpInstance = jnlpInstance;
	}

    @Override
    public void configure(final Console window) throws IOException, GeneralSecurityException {

        LOGGER.info(Messages.getString("ConfiguratorLinux.2")); //$NON-NLS-1$

        final File appDir = getApplicationDirectory(this.jnlpInstance);

        LOGGER.info(Messages.getString("ConfiguratorLinux.3") + appDir.getAbsolutePath()); //$NON-NLS-1$

        // Obtenemos los directorios de los usuarios
		final String[] usersDirs = getSystemUsersHomes();

        if (!checkSSLKeyStoreGenerated(appDir)) {
            LOGGER.info(Messages.getString("ConfiguratorLinux.5")); //$NON-NLS-1$

            final CertPack certPack = CertUtil.getCertPackForLocalhostSsl(
                ConfiguratorUtil.CERT_ALIAS,
                KS_PASSWORD
            );

            LOGGER.info(Messages.getString("ConfiguratorLinux.11")); //$NON-NLS-1$

           //Generacion del certificado pfx
            ConfiguratorUtil.installFile(
        		certPack.getPkcs12(),
        		new File(appDir, KS_FILENAME)
    		);

            //Generacion del certificado raiz .cer
            ConfiguratorUtil.installFile(
        		certPack.getCaCertificate().getEncoded(),
        		new File(appDir, FILE_AUTOFIRMA_CERTIFICATE)
    		);

            try {
                LOGGER.info(Messages.getString("ConfiguratorLinux.13")); //$NON-NLS-1$
                ConfiguratorFirefoxLinux.createScriptsToSystemKeyStore(
            		appDir,
            		usersDirs,
            		new File(appDir, ConfiguratorLinux.INSTALL_SCRIPT_NAME),
            		new File(appDir, ConfiguratorLinux.UNINSTALL_SCRIPT_NAME)
        		);

                ConfiguratorFirefoxLinux.createScriptsToMozillaKeyStore(
            		appDir,
            		usersDirs,
                	new File(appDir, ConfiguratorLinux.INSTALL_SCRIPT_NAME),
                	new File(appDir, ConfiguratorLinux.UNINSTALL_SCRIPT_NAME)
            	);
              }
            catch(final MozillaProfileNotFoundException e) {
                LOGGER.warning(Messages.getString("ConfiguratorLinux.12")); //$NON-NLS-1$
            }
        }
        else {
            LOGGER.info(Messages.getString("ConfiguratorLinux.14")); //$NON-NLS-1$
        }

        createScriptsRemoveChromeWarnings(appDir, usersDirs);

        LOGGER.info(Messages.getString("ConfiguratorLinux.8")); //$NON-NLS-1$
    }

    /** Comprueba si ya existe un almac&eacute;n de certificados generado.
     * @param appConfigDir Directorio de configuraci&oacute;n de la aplicaci&oacute;n.
     * @return {@code true} si ya existe un almacen de certificados SSL, {@code false} en caso contrario. */
    private static boolean checkSSLKeyStoreGenerated(final File appConfigDir) {
        return new File(appConfigDir, KS_FILENAME).exists();
    }

	private static File getApplicationDirectory(final boolean jnlpDeployment) {

		// Devolver un directorio que utilizar como directorio de instalacion cuando
		// se realice un despliegue JNLP
		if (jnlpDeployment) {
			try {
				return getIntApplicationDirectory();
			} catch (final IOException e) {
				LOGGER.severe("No se encuentra ni ha podido generarse el directorio de aplicacion: " + e); //$NON-NLS-1$
			}
		}

		return ConfiguratorUtil.getApplicationDirectory();
	}

	/** Obtiene un directorio en el que almacenar los ficheros de la aplicaci&oacute;n.
	 * @return Directorio de aplicaci&oacute;n.
	 * @throws IOException EN cualquier error. */
	private static File getIntApplicationDirectory() throws IOException {
		final String userHome;
		try {
			userHome = System.getProperty("user.home"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new IOException("No se ha podido identificar el directorio del usuario para almacenar los ficheros de instalacion", e); //$NON-NLS-1$
		}
		if (userHome == null) {
			throw new IOException("No se encuentra definido el directorio del usuario"); //$NON-NLS-1$
		}
		final File appDir = new File(userHome, ALTERNATIVE_APP_SUBDIR);
		if (!appDir.isDirectory() && !appDir.mkdirs()) {
			throw new IOException("No ha podido crearse el directorio para los ficheros de aplicacion"); //$NON-NLS-1$
		}
		return appDir;
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
					if(line.toLowerCase().contains("home/") && !usersDir.contains(line)) { //$NON-NLS-1$
						usersDir.add(line);
					}
				}
			}
			return usersDir.toArray(new String[usersDir.size()]);
		}
		catch (final Exception e) {
			LOGGER.severe("Error al obtener el listado de directorios de usuarios del sistema: " + e); //$NON-NLS-1$
			throw new IOException("No se pudo obtener el listado de directorios de usuarios del sistema", e); //$NON-NLS-1$
		}
	}

    /** Genera el <i>script</i> que elimina la advertencia al ejecutar AutoFirma desde Chrome.
	 * En linux genera el script que hay que ejecutar para realizar la instalaci&oacute;n pero no lo ejecuta, de eso se encarga el instalador Debian.
	 * @param targetDir Directorio de instalaci&oacute;n del sistema.
     *                  En LINUX contiene el contenido del script a ejecutar.
     * @param usersDirs Lista de directorios de los usuarios del sistema. */
	private static void createScriptsRemoveChromeWarnings(final File targetDir, final String[] usersDirs) {
		for (final String userDir : usersDirs) {

			// Generamos el script de instalacion
			final StringBuilder installationScript = new StringBuilder();
			// Generamos el script de desistalacion
			final StringBuilder uninstallationScript = new StringBuilder();

			// Montamos el script de instalacion y desinstalacion que
			// incluya el protocolo "afirma" en el fichero Local State o Preferences (segun la version)
			// para Google Chrome o Chromium
			try {
				// Se escriben los comandos en el script de instalacion
				final ArrayList<String[]> installCommands = getCommandsToRemoveChromeAndChromiumWarningsOnInstall(targetDir, userDir);
				final Iterator<String[]> list = installCommands.iterator();
				while(list.hasNext()) {
					ConfiguratorUtil.printScript(list.next(), installationScript);
				}

				// Se almacenan los script de instalación
				try {
					ConfiguratorUtil.writeScript(installationScript, new File(targetDir, INSTALL_SCRIPT_NAME));
				}
				catch (final Exception e) {
					throw new IOException("Error al crear el script para agregar la confianza del esquema 'afirma'", e); //$NON-NLS-1$
				}

				// Se escriben los comandos en el script de desinstalacion
				final ArrayList<String[]> uninstallCommands = getCommandsToRemoveChromeAndChromiumWarningsOnUninstall(targetDir, userDir);
				final Iterator<String[]> list2 = uninstallCommands.iterator();
				// Se escriben los comandos en el script de instalacion
				while(list2.hasNext()) {
					ConfiguratorUtil.printScript(list2.next(), uninstallationScript);
				}

				try {
					ConfiguratorUtil.writeScript(uninstallationScript, new File(targetDir, UNINSTALL_SCRIPT_NAME));
				}
				catch (final Exception e) {
					LOGGER.severe(
							"Excepcion en la creacion del script linux para la modificacion del fichero de protocolos de Google Chrome: " + e //$NON-NLS-1$
							);
				}
			} catch (final IOException e) {
				LOGGER.warning("No se pudieron crear los scripts para registrar el esquema 'afirma' en Chrome: " + e); //$NON-NLS-1$
			}
		}
	}

	/** Genera los comandos que desregistran el esquema "afirma" como un
	 * protocolo de confianza en Chrome.
	 * @param appDir Directorio de instalaci&oacute;n del sistema
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @return Lista de comandos que desregistran el esquema "afirma" en Chrome.
	 * @throws IOException En cualquier error. */
	private static ArrayList<String[]> getCommandsToRemoveChromeAndChromiumWarningsOnUninstall(final File appDir,
			                                                                                   final String userDir) throws IOException {
		final ArrayList<String[]> commandList = new ArrayList<>();

		// Final del if
		final String[] endIfStatement = new String[] {
				"fi", //$NON-NLS-1$
		};

		// Generacion de comandos de desinstalacion
		/////////////////////////////////////////////////////////////////////////////
		////// Chromium v56 o inferior
		/////////////////////////////////////////////////////////////////////////////

		final String[] ifFileFound1 =
				getIfFileFoundCommand(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH);
		// Generacion de comandos de desinstalacion
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
	 * protocolo de confiable en Chrome.
	 * @param appDir Directorio de instalaci&oacute;n del sistema
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @return Lista de comandos que desregistran el esquema "afirma" en Chrome.
	 * @throws IOException Cuando se produce un error de entrada/salida. */
	private static ArrayList<String[]> getCommandsToRemoveChromeAndChromiumWarningsOnInstall(final File appDir,
			                                                                                 final String userDir) throws IOException {
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
			final String[] ifContainsString1 = getIfNotCointainsStringCommand(userDir, LINUX_CHROMIUM_V56_OR_LOWER_PREFS_PATH);
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

			final String[] ifContainsString2 = getIfNotCointainsStringCommand(userDir, LINUX_CHROME_V56_OR_LOWER_PREFS_PATH);
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

	/** Genera los scripts para confirmar si existen protocolos definidos en el fichero.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @return Scripts para confirmar si existen protocolos definidos en el fichero. */
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

	/** Genera los <i>scripts</i> para confirmar si existe el fichero con el que se va a trabajar.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @return <i>Scripts</i> para confirmar si existe el fichero con el que se va a trabajar. */
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

	/** Genera los <i>scripts</i> para reemplazar el fichero original por el temporal
	 * con el que se estaba trabajando.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @return <i>Scripts</i> para reemplazar el fichero original por el temporal
	 *         con el que se estaba trabajando. */
	private static String[] copyConfigurationFile(final String userDir, final String browserPath) {
		// Comando para sobreescribir el fichero de configuracion
		return new String[] {
				"\\cp", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
				escapePath(userDir + browserPath),
		};
	}

	/** Genera los <i>scripts</i> para eliminar el protocolo <code>afirma</code>.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @return <i>scripts</i> para eliminar el protocolo <code>afirma</code> en Chrome. */
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

	/** Genera los <i>scripts</i> para eliminar la coma en caso de que sea el
	 * &uacute;nico protocolo definido en el fichero.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @return <i>Scripts</i> para eliminar la coma en caso de que sea el
	 * &uacute;nico protocolo definido en el fichero. */
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


	/** Genera los <i>scripts</i> para eliminar el protocolo afirma del fichero.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @param commandList lista donde a&ntilde;adir los comandos del <i>script</i>. */
	private static void removeProtocolInPreferencesFile1(final String userDir,
			                                             final String browserPath,
			                                             final ArrayList<String[]> commandList) {

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

	/** Genera los <i>scripts</i> para eliminar el protocolo
	 * <code>afirma</code> del fichero.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @param commandList lista donde a&ntilde;adir los comandos del <i>script</i>. */
	private static void removeProtocolInPreferencesFile2(final String userDir,
			                                             final String browserPath,
			                                             final ArrayList<String[]> commandList) {

		// Comando para retirar la confianza del esquema 'afirma'
		final String[] commandUninstall2 = new String[] {
				"sed -i", //$NON-NLS-1$ -i para reemplazar en el propio fichero
				"'s/\\\"afirma\\\":false//g'", //$NON-NLS-1$
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};
		commandList.add(commandUninstall2);
	}

	/** Genera los <i>scripts</i> para eliminar la sintaxis que define los protocolos
	 * de confianza si no existe ninguno.
	 * @param userDir Directorio de usuario dentro del sistema operativo.
	 * @param browserPath Directorio de configuraci&oacute;n de Chromium o Google Chrome.
	 * @param commandList lista donde a&ntilde;adir los comandos del <i>script</i>. */
	private static void removeProtocolInPreferencesFile3(final String userDir,
			                                             final String browserPath,
			                                             final ArrayList<String[]> commandList) {

		// Comando para retirar la confianza del esquema 'afirma'
		final String[] commandUninstall3 = new String[] {
				"sed -i", //$NON-NLS-1$ -i para reemplazar en el propio fichero
				"'s/\\\"protocol_handler\\\":{\\\"excluded_schemes\\\":{}},//g'", //$NON-NLS-1$ Se elimina la lista vacia de protocolos
				escapePath(userDir + browserPath) + "1", //$NON-NLS-1$
		};
		commandList.add(commandUninstall3);
	}

	/** <i>Escapa</i> rutas de fichero para poder usarlas como parte de un <i>script</i>.
	 * @param path Ruta de fichero.
	 * @return Ruta <i>escapada</i>. */
	private static String escapePath(final String path) {
		if (path == null) {
			throw new IllegalArgumentException(
				"La ruta a 'escapar' no puede ser nula" //$NON-NLS-1$
			);
		}
		return path.replace(" ", "\\ "); //$NON-NLS-1$ //$NON-NLS-2$
	}

    @Override
    public void uninstall(final Console console) {
        // No es necesario hacer nada mas  alla de eliminar los elementos creados por la aplicacion
    	// porque el  proceso de desinstalacion de linux eliminara el directorio de aplicacion con
    	// todo su contenido.

    	// Eliminamos si existe el directorio alternativo usado para el guardado de certificados
    	// SSL durante el proceso de restauracion de la instalacion
    	final File alternativeDir = getLinuxAlternativeAppDir();
    	if (alternativeDir.isDirectory()) {
    		try {
    			Files.walkFileTree(
    					alternativeDir.toPath(),
    					new HashSet<FileVisitOption>(),
    					Integer.MAX_VALUE,
    					new SimpleFileVisitor<Path>() {
    						@Override
    						public FileVisitResult visitFile(final Path file, final BasicFileAttributes attr) {
    							try {
    								Files.delete(file);
    							}
    							catch (final Exception e) {
    								LOGGER.warning("No se pudo eliminar el fichero: " + file); //$NON-NLS-1$
    							}
    							return FileVisitResult.CONTINUE;
    						}
    						@Override
    						public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) {
    							try {
    								Files.delete(dir);
    							} catch (final IOException e) {
    								LOGGER.warning("No se pudo eliminar el directorio: " + dir); //$NON-NLS-1$
    							}
    							return FileVisitResult.CONTINUE;
    						}
    					});
    		}
    		catch (final Exception e) {
    			LOGGER.log(Level.WARNING, "No se ha podido eliminar por completo el directorio alternativo para el certificado SSL", e); //$NON-NLS-1$
    		}
    	}
    }

	/**
	 * Recupera el directorio de instalaci&oacute;n alternativo en los sistemas Linux.
	 * @return Directorio de instalaci&oacute;n.
	 */
	private static File getLinuxAlternativeAppDir() {
		final String userHome = System.getProperty("user.home"); //$NON-NLS-1$
		return new File(userHome, ".afirma/AutoFirma"); //$NON-NLS-1$
	}
}
