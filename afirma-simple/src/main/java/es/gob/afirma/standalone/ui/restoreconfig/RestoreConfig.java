/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.restoreconfig;

/** Interfaz que define los m&eacute;todos necesarios para realizar la restauraci&oacute;n
 *  de la configuraci&oacute;n de navegadores de Autofirma.
 **/
interface RestoreConfig {

	/** Repara la configuraci&oacute;n del entorno para la ejecuci&oacute;n de Autofirma.
	 * @param configPanel Panel de configuraci&oacute;n con las trazas de ejecuci&oacute;n. */
	void restore(RestoreConfigPanel configPanel);

}
