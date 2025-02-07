/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

/** Clase para la configuraci&oacute;n de la instalaci&oacute;n de Autofirma desde un despliegue JNLP. */
public class AutoFirmaConfiguratorJNLP {

//    /** Inicia el proceso de configuraci&oacute;n.
//	 * @param args No usa par&aacute;metros. */
//	public static void main(final String[] args) {
//
//		// Iniciamos el servicio para la comunicacion con JNLP. Con el indicaremos
//		// al proceso de carga como termino el proceso de instalacion
//		final ExtensionInstallerService installerService;
//		try {
//			installerService =
//	            (ExtensionInstallerService) ServiceManager.lookup("javax.jnlp.ExtensionInstallerService"); //$NON-NLS-1$
//		}
//		catch (final Throwable e) {
//			Logger.getLogger("es.gob.afirma").severe("No se pudieron cargar los servicios JNLP de Java: " + e); //$NON-NLS-1$ //$NON-NLS-2$
//			return;
//		}
//
//		// Realizamos la instalacion del cliente
//
//		final AutoFirmaConfigurator configurator = new AutoFirmaConfigurator(new String[] { "-install" }); //$NON-NLS-1$
//		try {
//			configurator.configure();
//		}
//		catch (final Exception e) {
//			// Notificamos un error durante la instalacion
//			installerService.installFailed();
//			return;
//		}
//		// Notificamos que la instalacion finalizo correctamente
//		installerService.installSucceeded(false);
//	}

	/**
	 * Inicia el proceso de configuraci&oacute;n de la aplicaci&oacute;n.
	 * @param args Argumentos de configuraci&oacute;n.
	 * @throws ConfigurationException Cuando se produce alg&uacute;n error durante la
	 * configuraci&oacute;n.
	 */
	public static void configure(final String[] args) throws ConfigurationException {

		// Solo es necesario configurar algo cuando la aplicacion se va a comunicar a traves de sockets
		if (!isRequestBySocket(args)) {
			return;
		}

		// Realizamos la instalacion del cliente
		final AutoFirmaConfigurator configurator = new AutoFirmaConfigurator(new String[] {
				AutoFirmaConfigurator.PARAMETER_INSTALL,
				AutoFirmaConfigurator.PARAMETER_HEADLESS,
				AutoFirmaConfigurator.PARAMETER_KEEP_OPEN,
				AutoFirmaConfigurator.PARAMETER_JNLP_INSTANCE,
				});

		try {
			configurator.configure();
		}
		catch (final Exception e) {
			throw new ConfigurationException("No se pudo configurar la aplicacion: " + e, e); //$NON-NLS-1$
		}
	}

	/**
	 * Indica si la aplicaci&oacute;n se comunicar&aacute; con el navegador web
	 * a trav&eacute;s de sockets.
	 * @param args Opciones de configuraci&oacute;n recibidas.
	 * @return {@code true} si la aplicaci&oacute;n utilizar&aacute; la comunicaci&oacute;n
	 * por sockets, {@code false} en caso contrario.
	 */
	public static boolean isRequestBySocket(final String[] args) {
		return args != null && args.length > 0 && args[0].startsWith("afirma://") && //$NON-NLS-1$
				(args[0].indexOf("ports=") != -1 || args[0].indexOf("op=install") != -1); //$NON-NLS-1$ //$NON-NLS-2$
	}
}
