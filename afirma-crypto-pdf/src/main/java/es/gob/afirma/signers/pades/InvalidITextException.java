/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades;

import es.gob.afirma.core.InvalidLibraryException;

/** Indica que hay un iText inv&aacute;lido en el CLASSPATH o en el BOOTCLASSPATH, a menudo
 * porque se ha instalado el JAR inapropiadamente como extensi&oacute;n del JRE.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class InvalidITextException extends InvalidLibraryException {

	private static final long serialVersionUID = -322997692480101275L;

	/** Crea una instancia de la excepci&oacute;n.
	 * @param e Error por el cual se ha identificado qie el iText es inv&aacute;lido */
	public InvalidITextException(final Throwable e) {
		super("El Cliente afirma necesita una version especifica propia de iText", e); //$NON-NLS-1$
	}

}
