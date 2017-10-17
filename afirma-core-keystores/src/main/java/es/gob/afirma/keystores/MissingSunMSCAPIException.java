/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import es.gob.afirma.core.MissingLibraryException;

/** Indica que el JRE carece de la biblioteca SunMSCAPI para el acceso al almac&eacute;n de claves y certificados
 * de Windows
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class MissingSunMSCAPIException extends MissingLibraryException {

	private static final long serialVersionUID = -338521022809698613L;

	/** Crea una nueva instancia de la excepci&oacute;n.
	 * @param e Excepci&oacute;n previa en la cadena */
	public MissingSunMSCAPIException(final Exception e) {
		super("No esta instalada la biblioteca SunMSCAPI", e); //$NON-NLS-1$
	}

}
