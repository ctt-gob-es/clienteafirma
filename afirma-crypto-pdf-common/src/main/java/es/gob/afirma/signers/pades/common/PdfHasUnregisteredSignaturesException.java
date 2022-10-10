/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades.common;

import es.gob.afirma.core.RuntimeConfigNeededException;

/**
 * Indica que el PDF no ha podido firmarse por contener firmas previas no registradas en campos (<i>AcroFields</i>).
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * @author Carlos Gamuci
 */
public final class PdfHasUnregisteredSignaturesException extends RuntimeConfigNeededException {

	/** Serial Id. */
	private static final long serialVersionUID = 8047929185538217109L;

	public static final String REQUESTOR_MSG_CODE = "signingPdfWithUnregisteredSigns"; //$NON-NLS-1$

	/**
	 * Crea una excepci&oacute;n que indica que el PDF no ha podido firmarse por contener firmas previas
	 * no registradas en campos (<i>AcroFields</i>).
	 * @param msg Mensaje de error.
	 */
	public PdfHasUnregisteredSignaturesException(final String msg) {
		super(msg, RequestType.CONFIRM, REQUESTOR_MSG_CODE, PdfExtraParams.ALLOW_COSIGNING_UNREGISTERED_SIGNATURES);
	}

}
