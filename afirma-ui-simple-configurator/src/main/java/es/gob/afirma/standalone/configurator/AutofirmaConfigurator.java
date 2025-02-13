/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

import java.awt.Image;
import java.awt.Toolkit;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.GeneralSecurityException;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.LogManager;
import es.gob.afirma.core.LogManager.App;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.prefs.KeyStorePreferencesManager;
import es.gob.afirma.standalone.configurator.common.ConfigUpdaterManager;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.plugins.manager.PluginsManager;

/** Configurador de la instalaci&oacute;n de Autofirma. Identifica el entorno, genera un
 * certificado de firma y lo instala en los almacenes pertinentes. */
public class AutofirmaConfigurator implements ConsoleListener {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final File TMP = new File("/var/tmp"); //$NON-NLS-1$
	private static final File TEMP = new File("/var/temp"); //$NON-NLS-1$

	/** Indica que la operacion que se debe realizar es la de instalaci&oacute;n. */
	public static final String PARAMETER_INSTALL = "-install"; //$NON-NLS-1$

	/** Indica que la operacion que se debe realizar es la de desinstalaci&oacute;n. */
	public static final String PARAMETER_UNINSTALL = "-uninstall"; //$NON-NLS-1$

	/** Indica que se debe mantener abierta la aplicaci&oacute;n despu&eacute;s de
	 * finalizar la operaci&oacute;n. */
	public static final String PARAMETER_KEEP_OPEN = "-keep_open"; //$NON-NLS-1$

	/** Indica que no se debe mostrar el di&aacute;logo gr&aacute;fico con las trazas
	 * del proceso de instalaci&oacute;n. */
	public static final String PARAMETER_HEADLESS = "-headless"; //$NON-NLS-1$

	/** Indica que se realiza una carga mediante JNLP. */
	public static final String PARAMETER_JNLP_INSTANCE = "-jnlp"; //$NON-NLS-1$

	/** Indica que debe habilitarse el que Firefox utilice los certificados de confianza del sistema. */
	public static final String PARAMETER_FIREFOX_SECURITY_ROOTS = "-firefox_roots"; //$NON-NLS-1$

	/** Indica la ruta del certificado pasado por el administrador. */
	public static final String PARAMETER_CERTIFICATE_PATH = "-certificate_path"; //$NON-NLS-1$

	/** Indica la ruta del certificado pasado por el administrador. */
	public static final String PARAMETER_KEYSTORE_PATH = "-keystore_path"; //$NON-NLS-1$

	/** Indica la ruta del fichero de configuraci&oacute;nn PList con preferencias para el sistema. */
	public static final String CONFIG_PATH = "-config_path"; //$NON-NLS-1$

	/** Indica la ruta del fichero de actualizaci&oacute;n de preferencias del sistema. */
	public static final String UPDATE_CONFIG = "-update_config"; //$NON-NLS-1$

	private static final String PLUGINS_DIRNAME = "plugins"; //$NON-NLS-1$

	private Configurator configurator;

	private final ConfigArgs config;

	private Console mainScreen;

	static {
		// Instalamos el registro a disco
		try {
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				final File appDir = new File (System.getenv("HOME"), "Library/Application Support/Autofirma"); //$NON-NLS-1$ //$NON-NLS-2$

				if (appDir.mkdirs()) {
					ConfiguratorMacUtils.addExexPermissionsToFile(appDir);
				}

				if (appDir.isDirectory() && appDir.canWrite()) {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, appDir.getAbsolutePath());
				}
				else if (TMP.isDirectory() && TMP.canWrite()) {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, TMP.getAbsolutePath());
				}
				else if (TEMP.isDirectory() && TEMP.canWrite()) {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, TEMP.getAbsolutePath());
				}
				else {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
				}
			}
			else if (Platform.OS.LINUX.equals(Platform.getOS())) {
				if (TMP.isDirectory() && TMP.canWrite()) {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, TMP.getAbsolutePath());
				}
				else if (TEMP.isDirectory() && TEMP.canWrite()) {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, TEMP.getAbsolutePath());
				}
				else {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
				}
			}
			else if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
				// En Windows se ejecutara en modo administrador y el directorio de usuario apuntara al del administrador,
				// asi que componemos el directorio de usuario real
				final String drive = System.getenv("HOMEDRIVE"); //$NON-NLS-1$
				final String path = System.getenv("HOMEPATH"); //$NON-NLS-1$
				if (drive != null && path != null && new File(drive + path).isDirectory()) {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, new File(drive + path, LogManager.SUBDIR).getAbsolutePath());
				}
				else {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR);
				}
			}
			else {
				LogManager.install(App.AUTOFIRMA_CONFIGURATOR);
			}
		}
		catch(final Exception e) {
			LOGGER.severe("No ha sido posible instalar el gestor de registro: " + e); //$NON-NLS-1$
		}
	}

	/** Configurador de Autofirma.
	* @param args Argumentos para configurar la ejecuci&oacute;n del proceso. */
	public AutofirmaConfigurator(final String[] args) {
		this(new ConfigArgs(args));
	}

	/**
	 * Configurador de Autofirma.
	 * @param config Argumentos para configurar la ejecuci&oacute;n del proceso.
	 */
	public AutofirmaConfigurator(final ConfigArgs config) {

		this.config = config;

		final boolean jnlpDeployment = this.config.isJnlpInstance();
		if (jnlpDeployment) {
			LOGGER.info("Se configurara la aplicacion en modo JNLP"); //$NON-NLS-1$
		}
		else {
			LOGGER.info("Se configurara la aplicacion en modo nativo"); //$NON-NLS-1$
		}

		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			this.configurator = new ConfiguratorWindows(jnlpDeployment, this.config.isFirefoxSecurityRoots(),
					this.config.getCertificatePath(), this.config.getKeystorePath());
		}
		else if (Platform.OS.LINUX == Platform.getOS()){
		    this.configurator = new ConfiguratorLinux(jnlpDeployment);
		}
		else if (Platform.OS.MACOSX == Platform.getOS()){
            this.configurator = new ConfiguratorMacOSX(this.config.isHeadless(), this.config.isFirefoxSecurityRoots());
        }
		else {
			LOGGER.warning(
				"El sistema operativo '" + Platform.getOS() + "' no tiene definida una secuencia de configuracion/desinstalacion" //$NON-NLS-1$ //$NON-NLS-2$
			);
			this.configurator = null;
		}
	}

	/** Configura el entorno para permitir la correcta ejecuci&oacute;n de Autofirma.
	 * @throws GeneralSecurityException Cuando se produce un error al manipular los almacenes de certificados.
	 * @throws ConfigurationException Cuando falla la generacion del certificados SSL.
	 * @throws IOException Cuando no es posible cargar o manipular alg&uacute;n fichero de configuraci&oacute;n o recursos. */
	public void configure() throws GeneralSecurityException, ConfigurationException, IOException {

		if (this.configurator == null) {
			LOGGER.warning("No se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}

		// Preparamos la consola para las trazas
		this.mainScreen = ConsoleManager.getConsole(this.config.isHeadless() ? null : this);
		this.mainScreen.showConsole();

		// Creamos el almacen para la configuracion del SSL
		try {
			this.configurator.configure(this.mainScreen);
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Error al copiar o leer alguno de los ficheros de configuracion. El configurador se detendra", e); //$NON-NLS-1$
			this.mainScreen.print(Messages.getString("AutofirmaConfigurator.3")); //$NON-NLS-1$
			throw e;
		}
		catch (final ConfigurationException e) {
			LOGGER.log(Level.SEVERE, "Error al generar las claves de cifrado SSL. El configurador se detendra", e); //$NON-NLS-1$
			this.mainScreen.print(Messages.getString("AutofirmaConfigurator.4")); //$NON-NLS-1$
			throw e;
		}
		catch (final GeneralSecurityException e) {
			LOGGER.log(Level.SEVERE, "Error en la importacion de la CA de confianza o la limpieza del almacen", e); //$NON-NLS-1$
			this.mainScreen.print(Messages.getString("AutofirmaConfigurator.5")); //$NON-NLS-1$
			throw e;
		}
	}

	/** Inicia la desinstalaci&oacute;n del certificado ra&iacute;z de confianza
	 * del almac&eacute;n de claves. */
	private void uninstall() {

		if (this.configurator == null) {
			LOGGER.warning("No se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}

		// Creamos el almacen para la configuracion del SSL
		final File pluginsDir = getPluginsDir(this.configurator);
		final PluginsManager pluginsManager = new PluginsManager(pluginsDir);
		this.configurator.uninstall(this.mainScreen, pluginsManager);

		// Borramos las preferencias del sistema, que es algo comun a todos los sistemas operativos
		try {
			PreferencesManager.removeSystemPrefs();
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se han podido eliminar las preferencias del sistema de la aplicacion: " + e); //$NON-NLS-1$
		}
		try {
			KeyStorePreferencesManager.removeSystemPrefs();
		} catch (final Exception e) {
			LOGGER.log(Level.WARNING, "No se han podido eliminar las preferencias del sistema de los almacenes: " + e); //$NON-NLS-1$
		}
	}

    /**
     * Obtiene el directorio en el que se encuentran guardados o se deben
     * guardar los plugins.
     * @return Directorio de plugins.
     */
    private static File getPluginsDir(final Configurator configurator) {
		File appDir = configurator.getAlternativeApplicationDirectory();
		if (appDir == null) {
			appDir = configurator.getAplicationDirectory();
		}
		return new File(appDir, PLUGINS_DIRNAME);
	}

    /** Cierra la aplicaci&oacute;n.
     * @param exitCode C&oacute;digo de cierre de la aplicaci&oacute;n (negativo
     *                 indica error y cero indica salida normal. */
    public void closeApplication(final int exitCode) {
    	this.closeApplication(exitCode, false);
    }

    /** Cierra la aplicaci&oacute;n.
     * @param exitCode C&oacute;digo de cierre de la aplicaci&oacute;n (negativo
     *                 indica error y cero indica salida normal.
     * @param keepOpen Indica si hay que salir de la aplicaci&oacute;n. */
    public void closeApplication(final int exitCode, final boolean keepOpen) {
        if (this.mainScreen != null) {
            this.mainScreen.dispose();
        }

        if (!keepOpen) {
			System.exit(exitCode);
		}
    }

	@Override
	public void close() {
		closeApplication(0);
	}

    /** Inicia el proceso de configuraci&oacute;n.
	 * @param args No usa par&aacute;metros. */
	public static void main(final String[] args) {

		final ConfigArgs config = new ConfigArgs(args);
		final AutofirmaConfigurator configurator = new AutofirmaConfigurator(config);

       	// Propiedades especificas para Mac OS X
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			final Image icon = Toolkit.getDefaultToolkit()
					.getImage(AutofirmaConfigurator.class.getResource("/logo_cliente_256.png")); //$NON-NLS-1$
        	try {
        		settingDockMacIconWithJava8(icon);
			} catch (final Exception | Error e) {
        		try {
        			settingDockMacIconWithJava9(icon);
				} catch (Exception | Error e2) {
        			LOGGER.warning("No ha sido posible establecer el icono del Dock de macOS: " + e); //$NON-NLS-1$
				}
        	}
        }

		// Si se indico por parametro que se trata de una desinstalacion, desinstalamos
		if (config.isUninstallation()) {
			configurator.uninstall();
		}
		// Si no, instalamos
		else {
			try {
				configurator.configure();
			}
			catch (final Exception | Error e) {
				LOGGER.log(Level.SEVERE, "Error grave durante el proceso de configuracion", e); //$NON-NLS-1$
				configurator.closeApplication(-1, config.isNeedKeep());
			}
		}

		// Si se ha indicado un archivo de configuracion,
		// se estableceran las nuevas propiedades del sistema indicadas en el mismo
		if (!config.getConfigPath().isEmpty()) {
			ConfigUpdaterManager.savePrefsConfigFile(config.getConfigPath(), config.getUpdateConfig());
		}

		configurator.closeApplication(0, config.isNeedKeep());
	}

	private static void settingDockMacIconWithJava8(final Image icon)
			throws ClassNotFoundException, NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		final Class<?> applicationClass = Class.forName("com.apple.eawt.Application"); //$NON-NLS-1$
		final Method getApplicationMethod = applicationClass.getMethod("getApplication"); //$NON-NLS-1$
		final Object applicationObject = getApplicationMethod.invoke(null);
		final Method setDockIconImageMethod = applicationClass.getMethod("setDockIconImage", Image.class); //$NON-NLS-1$
		setDockIconImageMethod.invoke(applicationObject, icon);
    }

	private static void settingDockMacIconWithJava9(final Image icon)
			throws ClassNotFoundException, NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {

    	final Class<?> taskbarClass = Class.forName("java.awt.Taskbar"); //$NON-NLS-1$
    	final Method getTaskbarMethod = taskbarClass.getMethod("getTaskbar"); //$NON-NLS-1$
    	final Object taskbarObject = getTaskbarMethod.invoke(null);
    	final Method setIconImageMethod = taskbarClass.getMethod("setIconImage", Image.class); //$NON-NLS-1$
    	setIconImageMethod.invoke(taskbarObject, icon);
    }

	/** Operaciones admitidas. */
	private enum Operation {
		INSTALLATION,
		UNINSTALLATION
	}

	/** Configuraci&oacute;n establecida para la ejecuci&oacute;n del configurador. */
	private static class ConfigArgs {

		private Operation op = Operation.INSTALLATION;
		private boolean needKeep = false;
		private boolean headless = false;
		private boolean jnlpInstance = false;
		private boolean firefoxSecurityRoots = false;
		private String certificatePath = ""; //$NON-NLS-1$
		private String keystorePath = ""; //$NON-NLS-1$
		private String configPath = ""; //$NON-NLS-1$
		private boolean updateConfig = false;

		public ConfigArgs(final String[] args) {
			if (args != null) {
				for (int i = 0; i < args.length; i++) {
					final String arg = args[i];
					if (PARAMETER_INSTALL.equalsIgnoreCase(arg)) {
						this.op = Operation.INSTALLATION;
					} else if (PARAMETER_UNINSTALL.equalsIgnoreCase(arg)) {
						this.op = Operation.UNINSTALLATION;
					} else if (PARAMETER_KEEP_OPEN.equalsIgnoreCase(arg)) {
						this.needKeep = true;
					} else if (PARAMETER_HEADLESS.equalsIgnoreCase(arg)) {
						this.headless = true;
					} else if (PARAMETER_JNLP_INSTANCE.equalsIgnoreCase(arg)) {
						this.jnlpInstance = true;
					} else if (PARAMETER_FIREFOX_SECURITY_ROOTS.equalsIgnoreCase(arg)) {
						this.firefoxSecurityRoots = true;
					} else if (PARAMETER_CERTIFICATE_PATH.equalsIgnoreCase(arg)) {
						if (i < args.length - 1) {
							this.certificatePath = args[++i];
						}
					} else if (PARAMETER_KEYSTORE_PATH.equalsIgnoreCase(arg) && i < args.length - 1) {
						this.keystorePath = args[++i];
					} else if (CONFIG_PATH.equalsIgnoreCase(arg) && i < args.length - 1) {
						this.configPath = args[++i];
					} else if (UPDATE_CONFIG.equalsIgnoreCase(arg)) {
						this.updateConfig = true;
					}
				}
			}
		}

		public boolean isUninstallation() {
			return this.op == Operation.UNINSTALLATION;
		}

		public boolean isNeedKeep() {
			return this.needKeep;
		}

		public boolean isHeadless() {
			return this.headless;
		}

		public boolean isJnlpInstance() {
			return this.jnlpInstance;
		}

		public boolean isFirefoxSecurityRoots() {
			return this.firefoxSecurityRoots;
		}

		public String getCertificatePath() {
			return this.certificatePath;
		}

		public String getKeystorePath() {
			return this.keystorePath;
		}

		public String getConfigPath() {
			return this.configPath;
		}

		public boolean getUpdateConfig() {
			return this.updateConfig;
		}

	}
}
