/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone.configurator;

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.logging.Logger;

import es.gob.afirma.core.LogManager;
import es.gob.afirma.core.LogManager.App;
import es.gob.afirma.core.misc.Platform;

/** Clase para la configuraci&oacute;n de la instalaci&oacute;n de AutoFirma. Identifica el entorno, genera un
 * certificado de firma y lo instala en los almacenes pertinentes. */
public class AutoFirmaConfigurator implements ConsoleListener {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String PARAMETER_UNISTALL = "-uninstall"; //$NON-NLS-1$
	private static final File TMP = new File("/var/tmp"); //$NON-NLS-1$
	private static final File TEMP = new File("/var/temp"); //$NON-NLS-1$

	private Configurator configurator;

	private Console mainScreen;

	static {
		// Instalamos el registro a disco
		try {
			if (Platform.getOS().equals(Platform.OS.MACOSX) || Platform.getOS().equals(Platform.OS.LINUX)) {
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
	public AutoFirmaConfigurator() {

		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			this.configurator = new ConfiguratorWindows();
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
	public void configureAutoFirma() throws GeneralSecurityException, ConfigurationException, IOException {

		if (this.configurator == null) {
			LOGGER.warning("No se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}

		this.mainScreen = ConsoleManager.getConsole(this);
		this.mainScreen.showConsole();

		// Creamos el almacen para la configuracion del SSL
		try {
			this.configurator.configure(this.mainScreen);
		}
		catch (final IOException e) {
			LOGGER.severe("Error al copiar o leer alguno de los ficheros de configuracion. El configurador se detendra: " + e); //$NON-NLS-1$
			this.mainScreen.print(Messages.getString("AutoFirmaConfigurator.3")); //$NON-NLS-1$
			throw e;
		}
		catch (final ConfigurationException e) {
			LOGGER.severe("Error al generar las claves de cifrado SSL. El configurador se detendra: " + e); //$NON-NLS-1$
			this.mainScreen.print(Messages.getString("AutoFirmaConfigurator.4")); //$NON-NLS-1$
			throw e;
		}
		catch (final GeneralSecurityException e) {
			LOGGER.severe("Error en la importacion de la CA de confianza o la limpieza del almacen: " + e); //$NON-NLS-1$
			this.mainScreen.print(Messages.getString("AutoFirmaConfigurator.5")); //$NON-NLS-1$
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

    /** Inicia el proceso de configuraci&oacute;n.
	 * @param args No usa par&aacute;metros. */
	public static void main(final String[] args) {

		final AutoFirmaConfigurator configurator = new AutoFirmaConfigurator();

		// Si se indico por parametro que se trata de una desinstalacion, desinstalamos
		if (args != null && args.length > 0 && PARAMETER_UNISTALL.equalsIgnoreCase(args[0])) {
			configurator.uninstall();
			configurator.closeApplication(0);
			return;
		}

		// Iniciamos la configuracion
		try {
			configurator.configureAutoFirma();
		}
		catch (final Exception e) {
			LOGGER.warning("Error en la configuracion de AutoFirma: " + e); //$NON-NLS-1$
			ConsoleManager.showErrorMessage(
				configurator.getParentComponent(),
				Messages.getString("AutoFirmaConfigurator.0") //$NON-NLS-1$
			);
			configurator.closeApplication(-1);
		}
		catch (final Error e) {
			LOGGER.warning("Capturado error no controlado: " + e); //$NON-NLS-1$
			e.printStackTrace();
			ConsoleManager.showErrorMessage(
				configurator.getParentComponent(),
				Messages.getString("AutoFirmaConfigurator.1") //$NON-NLS-1$
			);
			configurator.closeApplication(-2);
		}

//		// Ejecutamos una operacion de red para que al usuario le aparezca
//		// la opcion de dar permisos a la maquina virtual de Java
//		try {
//			AutoFirmaConfigurator.checkNetworkPermissions();
//		} catch (final Exception e) {
//			LOGGER.warning("Capturado error no controlado: " + e); //$NON-NLS-1$
//			e.printStackTrace();
//			ConsoleManager.showErrorMessage(
//				configurator.getParentComponent(),
//				Messages.getString("AutoFirmaConfigurator.6") //$NON-NLS-1$
//			);
//			configurator.closeApplication(-3);
//		}

		configurator.closeApplication(0);
	}

//	/**
//	 * Comprueba la conectividad de red.
//	 * @throws IOException Cuando no puede descargar datos de la red.
//	 */
//	private static void checkNetworkPermissions() throws IOException {
//		UrlHttpManagerFactory.getInstalledManager()
//		.readUrl("http://estaticos.redsara.es/comunes/autofirma/autofirma.version", UrlHttpMethod.GET);
//	}

	@Override
	public void close() {
		closeApplication(0);
	}
}
