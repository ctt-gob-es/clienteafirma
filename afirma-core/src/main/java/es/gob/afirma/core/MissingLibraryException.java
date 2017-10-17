/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core;

/** Indica que el JRE carece de una biblioteca necesaria para la ejecuci&oacute;n de Afirma.
 * A menudo es debido a las diferencias entre los JRE de 32 y 64 bits o entre los JRE de
 * Oracle y de IBM.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class MissingLibraryException extends RuntimeException {

	/** Crea una nueva instancia de la excepci&oacute;n.
	 * @param desc Descripci&oacute;n de la excepci&oacute;n
	 * @param e Excepci&oacute;n previa en la cadena */
	public MissingLibraryException(final String desc, final Exception e) {
		super(desc, e);
	}

	private static final long serialVersionUID = -2323154676951325543L;
}
