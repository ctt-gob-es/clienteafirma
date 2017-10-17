/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc.http;

/** Factor&iacute;a para la obtenci&oacute;n de un manejador para la lectura y env&iacute;o de datos a URL remotas.
 * @author Carlos Gamuci */
public abstract class UrlHttpManagerFactory {

	private static UrlHttpManager staticUrlManager = null;

	/** Instala el manejador que se encargar&aacute; de realizar las conexiones con las URL indicadas.
	 * @param urlManager Manejador de conexicion para la lectura y env&iacute;o de datos. */
	public static void install(final UrlHttpManager urlManager) {
		staticUrlManager = urlManager;
	}

	/** Recupera el manejador instalado o, en su defecto, el manejador por defecto.
	 * @return Manejador de conexi&oacute;nes. */
	public static UrlHttpManager getInstalledManager() {
		if (staticUrlManager == null) {
			staticUrlManager = new UrlHttpManagerImpl();
		}
		return staticUrlManager;
	}
}
