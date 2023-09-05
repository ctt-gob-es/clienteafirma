/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades;

import es.gob.afirma.core.AOFormatFileException;

/** Excepci&oacute;n para notificar que se ha proporcionado un fichero que no es un PDF o es un
 * PDF no soportado / inv&aacute;lido / corrupto.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class InvalidPdfException extends AOFormatFileException {

	private static final long serialVersionUID = 674827105543544636L;

	/**
	 * Crea una excepci&oacute;n para notificar que se ha proporcionado un fichero que no es un PDF.
	 * @param cause Origen del error.
	 */
	public InvalidPdfException(final Throwable cause) {
		super("El fichero no es un PDF o es un PDF no soportado", cause); //$NON-NLS-1$
	}

	/**
	 * Crea una excepci&oacute;n para notificar que se ha proporcionado un fichero que no es un PDF.
	 * @param msg Mensaje de error.
	 */
	public InvalidPdfException(final String msg) {
		super(msg);
	}

}
