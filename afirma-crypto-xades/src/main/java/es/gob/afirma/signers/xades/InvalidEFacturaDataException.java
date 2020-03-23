/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xades;

import es.gob.afirma.core.AOInvalidFormatException;

/** Excepci&oacute;n para notificar que se ha encontrado un objeto que no es una factura electr&oacute;nica v&aacute;lida.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class InvalidEFacturaDataException extends AOInvalidFormatException {

	private static final long serialVersionUID = -7465457150119381288L;

	public InvalidEFacturaDataException() {
		super("Los datos proporcionados no son una factura electronica"); //$NON-NLS-1$
	}

}
