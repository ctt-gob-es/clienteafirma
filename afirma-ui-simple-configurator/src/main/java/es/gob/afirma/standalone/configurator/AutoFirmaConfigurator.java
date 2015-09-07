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
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.LogManager;
import es.gob.afirma.core.LogManager.App;
import es.gob.afirma.core.misc.Platform;

/**
 * Clase para la configuraci&oacute;n de la instalaci&oacute;n de AutoFirma. Identifica el entorno, genera un
 * certificado de firma y lo instala en los almacenes pertinentes.
 */
public class AutoFirmaConfigurator implements WindowListener {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int DEFAULT_WINDOW_WIDTH = 480;
	private static final int DEFAULT_WINDOW_HEIGHT = 350;

	private static final String PARAMETER_UNISTALL = "-uninstall"; //$NON-NLS-1$

	private Configurator configurator;

	private ConfiguratorConsole mainScreen;

	static {
		// Instalamos el registro a disco
		try {
			LogManager.install(App.SIMPLE_CONFIGURATOR);
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("No ha sido posible instalar el gestor de registro: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/**
	 * Configurador de AutoFirma.
	 */
	public AutoFirmaConfigurator() {

		if (Platform.OS.WINDOWS == Platform.getOS()) {
			this.configurator = new ConfiguratorWindows();
		}
		else {
			LOGGER.warning("El sistema operativo actual no tiene definida una secuencia de configuracion/desinstalacion"); //$NON-NLS-1$
			this.configurator = null;
		}
	}

	/**
	 * Configura el entorno para permitir la correcta ejecuci&oacute;n de AutoFirma.
	 * @throws GeneralSecurityException Cuando se produce un error al manipular los almacenes de certificados.
	 * @throws ConfigurationException Cuando falla la generacion del certificados SSL.
	 * @throws IOException Cuando no es posible cargar o manipular alg&uacute;n fichero de configuraci&oacute;n o recursos.
	 */
	public void configureAutoFirma() throws GeneralSecurityException, ConfigurationException, IOException {

		if (this.configurator == null) {
			LOGGER.warning("No se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}

		this.mainScreen = new ConfiguratorConsole();
		this.mainScreen.showConsole(this, DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT);

		// Creamos el almacen para la configuracion del SSL
		try {
			this.configurator.configure(this.mainScreen);
		} catch (final IOException e) {
			LOGGER.severe("Error al copiar o leer alguno de los ficheros de configuracion. El configurador se detendra: " + e); //$NON-NLS-1$
			this.mainScreen.print(Messages.getString("AutoFirmaConfigurator.3")); //$NON-NLS-1$
			throw e;
		} catch (final ConfigurationException e) {
			LOGGER.severe("Error al generar las claves de cifrado SSL. El configurador se detendra: " + e); //$NON-NLS-1$
			this.mainScreen.print(Messages.getString("AutoFirmaConfigurator.4")); //$NON-NLS-1$
			throw e;
		} catch (final GeneralSecurityException e) {
			LOGGER.severe("Error en la importacion de la CA de confianza o la limpieza del almacen: " + e); //$NON-NLS-1$
			this.mainScreen.print(Messages.getString("AutoFirmaConfigurator.5")); //$NON-NLS-1$
			throw e;
		}
	}

	/**
	 * Devuelve la ventana padre del configurador.
	 * @return
	 */
	private Component getParentComponent() {
		return this.mainScreen;
	}

	/**
	 * Inicia la desinstalaci&oacute;n del certificado ra&iacute;z de confianza del almac&eacute;n de claves.
	 */
	private void uninstall() {

		if (this.configurator == null) {
			LOGGER.warning("No se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}

		// Creamos el almacen para la configuracion del SSL
		try {
			this.configurator.uninstall();
		} catch (final IOException e) {
			LOGGER.warning("Error al acceder al almacen de certificados. No se eliminara la CA del certificados SSL: " + e); //$NON-NLS-1$
		} catch (final GeneralSecurityException e) {
			LOGGER.warning("Error al eliminar la CA del certificado SSL. No se eliminara la CA del certificados SSL: " + e); //$NON-NLS-1$
		}
	}

	private static void showErrorMessage(final Component parent, final String errorText) {
		JOptionPane.showMessageDialog(parent, errorText, Messages.getString("AutoFirmaConfigurator.2"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
	}

    @Override
    public void windowClosing(final WindowEvent we) {
    	closeApplication(0);
    }

    @Override public void windowOpened(final WindowEvent we) { /* No implementado */ }
    @Override public void windowClosed(final WindowEvent we) { /* No implementado */ }
    @Override public void windowActivated(final WindowEvent we) { /* No implementado */ }
    @Override public void windowIconified(final WindowEvent we) { /* No implementado */ }
    @Override public void windowDeiconified(final WindowEvent we) { /* No implementado */ }
    @Override public void windowDeactivated(final WindowEvent we) { /* No implementado */ }

    /** Cierra la aplicaci&oacute;n.
     * @param exitCode C&oacute;digo de cierre de la aplicaci&oacute;n (negativo
     *                 indica error y cero indica salida normal. */
    public void closeApplication(final int exitCode) {
        if (this.mainScreen != null) {
            this.mainScreen.dispose();
        }
        System.exit(exitCode);
    }

    /**
	 * Inicia el proceso de configuraci&oacute;n.
	 * @param args No hacerta par&aacute;metros.
	 */
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
			LOGGER.info("Excepcion capturada: " + e); //$NON-NLS-1$
			e.printStackTrace();
			showErrorMessage(configurator.getParentComponent(), Messages.getString("AutoFirmaConfigurator.0")); //$NON-NLS-1$
			configurator.closeApplication(-1);
		}
		catch (final Error e) {
			LOGGER.info("Capturado error no controlado: " + e); //$NON-NLS-1$
			e.printStackTrace();
			showErrorMessage(configurator.getParentComponent(), Messages.getString("AutoFirmaConfigurator.1")); //$NON-NLS-1$
			configurator.closeApplication(-2);
		}

		configurator.closeApplication(0);
	}
}
