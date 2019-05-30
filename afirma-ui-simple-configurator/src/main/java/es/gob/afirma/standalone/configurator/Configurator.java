/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

import java.io.IOException;
import java.security.GeneralSecurityException;

/** Interfaz que define los m&eacute;todos necesarios para configurar el sistema operativo tras la
 * instalaci&oacute;n de AutoFirma. */
interface Configurator {

	/** Configura el entorno para la ejecuci&oacute;n de AutoFirma.
	 * @param window Ventana padre con consola.
	 * @throws IOException Cuando no es posible cargar o manipular alg&uacute;n fichero de configuraci&oacute;n o recursos.
	 * @throws ConfigurationException Cuando falla la generaci&oacute;n del certificados SSL.
	 * @throws GeneralSecurityException Cuando se produce un error al manipular los almacenes de certificados. */
	void configure(Console window) throws IOException, ConfigurationException, GeneralSecurityException;

	/** Desinstala los componentes necesarios del sistema.
	 * @param window Ventana padre con consola. */
	void uninstall(Console window);
}
