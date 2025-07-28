/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.restoreconfig;

import java.util.logging.Logger;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * Clase que contiene la l&oacute;gica para iniciar el proceso de restauraci&oacute;n decidiendo
 * el sistema operativo objetivo.
 *
 */
public class RestoreConfigManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private RestoreConfig configurator;

	/**
	 * Constructor del restaurador de configuración
	 * de Autofirma
	 */
	public RestoreConfigManager() {

		if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			this.configurator = new RestoreConfigWindows();
		}
		else if (Platform.OS.LINUX == Platform.getOS()){
		    this.configurator = new RestoreConfigLinux();
		}
		else if (Platform.OS.MACOSX == Platform.getOS()){
            this.configurator = new RestoreConfigMacOSX();
        }
		else {
			LOGGER.warning(
				"El sistema operativo '" + Platform.getOS() + "' no tiene definida una secuencia de configuracion/desinstalacion" //$NON-NLS-1$ //$NON-NLS-2$
			);
			this.configurator = null;
		}
	}

	/**
	 * Repara la configuraci&oacute;n de navegadores para permitir la comunicacion con la aplicaci&oacute;n.
	 * @param configPanel Panel de configuraci&oacute;n con las trazas de ejecuci&oacute;n.
	 */
	public void restoreConfig(final RestoreConfigPanel configPanel) {

		if (this.configurator == null) {
			LOGGER.warning("No se realizara ninguna accion"); //$NON-NLS-1$
			return;
		}

		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreApplication.13")); //$NON-NLS-1$
		LOGGER.info("Inicio de la restauracion de la instalacion" ); //$NON-NLS-1$

		this.configurator.restore(configPanel);

		configPanel.appendMessage(SimpleAfirmaMessages.getString("RestoreApplication.14")); //$NON-NLS-1$
		LOGGER.info("Fin de la restauracion de la instalacion" ); //$NON-NLS-1$
	}

}
