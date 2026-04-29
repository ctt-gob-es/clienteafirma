/* Copyright (C) 2022 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades;

import es.gob.afirma.core.AORuntimeException;
import es.gob.afirma.signers.pades.common.PdfErrorCode;

/**
 * Indica que la pagina indicada donde firmar no es correcta
 * @author Jos&eacute; Montero Rivero
 */
public final class IncorrectPageException extends AORuntimeException {

	private static final long serialVersionUID = -2415913095143368605L;

	/**
	 * Crea la excepci&oacute;n indicando que la p&aacute;gina no es correcta.
	 * @param msg Mensaje de error.
	 */
	public IncorrectPageException(final String msg) {
		super(msg, PdfErrorCode.Request.INVALID_SIGNATURE_PAGE);
	}

}
