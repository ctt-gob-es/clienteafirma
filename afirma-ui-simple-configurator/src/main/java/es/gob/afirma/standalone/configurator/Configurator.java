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

import es.gob.afirma.standalone.plugins.manager.PluginsManager;

/** Interfaz que define los m&eacute;todos necesarios para configurar el sistema operativo tras la
 * instalaci&oacute;n de Autofirma. */
interface Configurator {

	/** Configura el entorno para la ejecuci&oacute;n de Autofirma.
	 * @param window Ventana padre con consola.
	 * @throws IOException Cuando no es posible cargar o manipular alg&uacute;n fichero de configuraci&oacute;n o recursos.
	 * @throws ConfigurationException Cuando falla la generaci&oacute;n del certificados SSL.
	 * @throws GeneralSecurityException Cuando se produce un error al manipular los almacenes de certificados. */
	void configure(Console window) throws IOException, ConfigurationException, GeneralSecurityException;

	/**
	 * Desinstala del sistema los recursos de la aplicaci&oacute;n y los plugins instalados.
	 * @param window Ventana padre con consola.
	 * @param pluginsManager Gestor de plugins.
	 */
	void uninstall(Console window, final PluginsManager pluginsManager);

	/**
	 * Obtiene el directorio de instalaci&oacute;n de la aplicaci&oacute;n.
	 * @return Directorio de instalaci&oacute;n de la aplicaci&oacute;n.
	 */
	File getAplicationDirectory();

	/**
	 * Obtiene el directorio alternativo en el que se almacenaran los recursos
	 * de la aplicaci&oacute;n que se hayan generado posteriormente a la
	 * instalaci&oacute;n.
	 * @return Directorio alternativo de la aplicaci&oacute;n.
	 */
	File getAlternativeApplicationDirectory();
}
