/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.pades;

import es.gob.afirma.core.AOFormatFileException;

/** Excepci&oacute;n para notificar que se ha proporcionado un fichero que no es un PDF o es un
 * PDF no soportado / inv&aacute;lido / corrupto.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class InvalidPdfException extends AOFormatFileException {

	private static final long serialVersionUID = 674827105543544636L;

	/** Crea una excepci&oacute;n para notificar que se ha proporcionado un fichero que no es un PDF
	 * @param e Excepci&oacute;n de orien */
	public InvalidPdfException(final Exception e) {
		super("El fichero no es un PDF o es un PDF no soportado", e); //$NON-NLS-1$
	}

}
