/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.LogManager;
import es.gob.afirma.core.LogManager.App;
import es.gob.afirma.core.misc.Platform;

/** Configurador de la instalaci&oacute;n de AutoFirma.
 * Identifica el entorno, genera un certificado de firma y lo instala en los almacenes pertinentes.
 * @author Sergio Mart&iacute;nez Rico. */
public final class AutoFirmaConfiguratorSilent implements ConsoleListener {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final File TMP = new File("/var/tmp"); //$NON-NLS-1$
	private static final File TEMP = new File("/var/temp"); //$NON-NLS-1$

	/** Indica que la operacion que se debe realizar es la de desinstalaci&oacute;n. */
	public static final String PARAMETER_UNINSTALL = "-uninstall"; //$NON-NLS-1$

	private Configurator configurator;

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
	public AutoFirmaConfiguratorSilent() {

		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			this.configurator = new ConfiguratorWindows(false);
		}
		else if (Platform.OS.LINUX == Platform.getOS()){
		    this.configurator = new ConfiguratorLinux(false);
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
	public void configure() throws GeneralSecurityException, ConfigurationException, IOException {

		if (this.configurator == null) {
			LOGGER.warning("No se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}

		this.mainScreen = ConsoleManager.getConsole(this);

		// Creamos el almacen para la configuracion del SSL
		try {
			this.configurator.configure(this.mainScreen);
		}
		catch (final IOException e) {
			LOGGER.log(Level.SEVERE, "Error al copiar o leer alguno de los ficheros de configuracion. El configurador se detendra", e); //$NON-NLS-1$
			throw e;
		}
		catch (final ConfigurationException e) {
			LOGGER.log(Level.SEVERE, "Error al generar las claves de cifrado SSL. El configurador se detendra", e); //$NON-NLS-1$
			throw e;
		}
		catch (final GeneralSecurityException e) {
			LOGGER.log(Level.SEVERE, "Error en la importacion de la CA de confianza o la limpieza del almacen", e); //$NON-NLS-1$
			throw e;
		}
	}

	/** Devuelve la ventana padre del configurador.
	 * @return Ventana padre del configurador. */
	private Component getParentComponent() {
		return this.mainScreen.getParentComponent();
	}

	/** Inicia la desinstalaci&oacute;n del certificado ra&iacute;z de confianza
	 * del almac&eacute;n de claves. */
	private void uninstall() {

		if (this.configurator == null) {
			LOGGER.warning("No se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}

		// Creamos el almacen para la configuracion del SSL
		this.configurator.uninstall();
	}

    /** Cierra la aplicaci&oacute;n.
     * @param exitCode C&oacute;digo de cierre de la aplicaci&oacute;n (negativo
     *                 indica error y cero indica salida normal. */
    public void closeApplication(final int exitCode) {
        if (this.mainScreen != null) {
            this.mainScreen.dispose();
        }
        System.exit(exitCode);
    }

	@Override
	public void close() {
		closeApplication(0);
	}

    /** Inicia el proceso de configuraci&oacute;n.
	 * @param args No usa par&aacute;metros. */
	public static void main(final String[] args) {

		final AutoFirmaConfiguratorSilent configurator = new AutoFirmaConfiguratorSilent();

		// Si se indico por parametro que se trata de una desinstalacion, desinstalamos
		if (args != null && args.length > 0 && PARAMETER_UNINSTALL.equalsIgnoreCase(args[0])) {
			configurator.uninstall();
			configurator.closeApplication(0);
			return;
		}

		// Iniciamos la configuracion
		try {
			configurator.configure();
		}
		catch (final Exception | Error e) {
			ConsoleManager.showErrorMessage(
				configurator.getParentComponent(),
				Messages.getString("AutoFirmaConfigurator.0") //$NON-NLS-1$
			);
			configurator.closeApplication(-1);
		}

		configurator.closeApplication(0);
	}
}
