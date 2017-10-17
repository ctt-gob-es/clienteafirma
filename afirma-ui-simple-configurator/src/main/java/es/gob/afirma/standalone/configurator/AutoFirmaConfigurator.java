/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.LogManager;
import es.gob.afirma.core.LogManager.App;
import es.gob.afirma.core.misc.Platform;

/** Clase para la configuraci&oacute;n de la instalaci&oacute;n de AutoFirma. Identifica el entorno, genera un
 * certificado de firma y lo instala en los almacenes pertinentes. */
public class AutoFirmaConfigurator implements ConsoleListener {

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

	private Configurator configurator;

	private final  ConfigArgs config;

	private Console mainScreen;

	static {
		// Instalamos el registro a disco
		try {
			if (Platform.getOS().equals(Platform.OS.MACOSX)) {
				final File appDir = new File (System.getenv("HOME"), "Library/Application Support/AutoFirma"); //$NON-NLS-1$ //$NON-NLS-2$
				if (appDir.isDirectory() && appDir.canWrite() || appDir.mkdirs()) {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, appDir.getAbsolutePath());
				}
				else if (TMP.exists() && TMP.isDirectory() && TMP.canWrite()) {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, TMP.getAbsolutePath());
				}
				else if (TEMP.exists() && TEMP.isDirectory() && TEMP.canWrite()) {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, TEMP.getAbsolutePath());
				}
				else {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
				}
			}
			else if (Platform.getOS().equals(Platform.OS.LINUX)) {
				if (TMP.exists() && TMP.isDirectory() && TMP.canWrite()) {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, TMP.getAbsolutePath());
				}
				else if (TEMP.exists() && TEMP.isDirectory() && TEMP.canWrite()) {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, TEMP.getAbsolutePath());
				}
				else {
					LogManager.install(App.AUTOFIRMA_CONFIGURATOR, System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
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

	/** Configurador de AutoFirma. */
	public AutoFirmaConfigurator(final String[] args) {
		this(new ConfigArgs(args));
	}

	/** Configurador de AutoFirma. */
	public AutoFirmaConfigurator(final ConfigArgs config) {

		this.config = config;

		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			this.configurator = new ConfiguratorWindows(this.config.isJnlpInstance());
		}
		else if (Platform.OS.LINUX == Platform.getOS()){
		    this.configurator = new ConfiguratorLinux();
		}
		else if (Platform.OS.MACOSX == Platform.getOS()){
            this.configurator = new ConfiguratorMacOSX();
        }
		else {
			LOGGER.warning(
				"El sistema operativo '" + Platform.getOS() + "' no tiene definida una secuencia de configuracion/desinstalacion" //$NON-NLS-1$ //$NON-NLS-2$
			);
			this.configurator = null;
		}
	}

	/** Configura el entorno para permitir la correcta ejecuci&oacute;n de AutoFirma.
	 * @throws GeneralSecurityException Cuando se produce un error al manipular los almacenes de certificados.
	 * @throws ConfigurationException Cuando falla la generacion del certificados SSL.
	 * @throws IOException Cuando no es posible cargar o manipular alg&uacute;n fichero de configuraci&oacute;n o recursos. */
	public void configure() throws IOException, ConfigurationException, GeneralSecurityException {

		if (this.configurator == null) {
			LOGGER.warning("No se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}

		// Preparamos la consola para las trazas
		this.mainScreen = ConsoleManager.getConsole(this.config.isHeadless() ? null : this);
		this.mainScreen.showConsole();

		// Si se indico por parametro que se trata de una desinstalacion, desinstalamos
		if (this.config.isUninstallation()) {
			uninstall();
			return;
		}

		// Creamos el almacen para la configuracion del SSL
		try {
			this.configurator.configure(this.mainScreen);
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Error al copiar o leer alguno de los ficheros de configuracion. El configurador se detendra", e); //$NON-NLS-1$
			this.mainScreen.print(Messages.getString("AutoFirmaConfigurator.3")); //$NON-NLS-1$
			throw e;
		}
		catch (final ConfigurationException e) {
			LOGGER.log(Level.SEVERE, "Error al generar las claves de cifrado SSL. El configurador se detendra", e); //$NON-NLS-1$
			this.mainScreen.print(Messages.getString("AutoFirmaConfigurator.4")); //$NON-NLS-1$
			throw e;
		}
		catch (final GeneralSecurityException e) {
			LOGGER.log(Level.SEVERE, "Error en la importacion de la CA de confianza o la limpieza del almacen", e); //$NON-NLS-1$
			this.mainScreen.print(Messages.getString("AutoFirmaConfigurator.5")); //$NON-NLS-1$
			throw e;
		}
	}

	/** Inicia la desinstalaci&oacute;n del certificado ra&iacute;z de confianza
	 * del almac&eacute;n de claves. */
	private void uninstall() {

		// Creamos el almacen para la configuracion del SSL
		this.configurator.uninstall();
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

	/** Operaciones admitidas. */
	private static enum Operation {
		INSTALLATION,
		UNINSTALLATION
	}

	/** Configuraci&oacute;n establecida para la ejecuci&oacute;n del configurador. */
	private static class ConfigArgs {

		private Operation op = Operation.INSTALLATION;
		private boolean needKeep = false;
		private boolean headless = false;
		private boolean jnlpInstance = false;

		public ConfigArgs(final String[] args) {
			if (args != null) {
				for (final String arg : args) {
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
	}


	/** Inicia el proceso de configuraci&oacute;n.
	 * @param args No usa par&aacute;metros. */
	public static void main(final String[] args) {

		final ConfigArgs config = new ConfigArgs(args);
		final AutoFirmaConfigurator configurator = new AutoFirmaConfigurator(config);

		// Iniciamos la configuracion
		try {
			configurator.configure();
		}
		catch (final Exception e) {
			configurator.closeApplication(-1, config.isNeedKeep());
		}

		configurator.closeApplication(0, config.isNeedKeep());
	}

}
