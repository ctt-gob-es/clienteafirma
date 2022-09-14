/* Copyright (C) 2022 [Gobierno de Espana]
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
 * Excepci&oacute;n que indica que el PDF esta certificado y que firmarlo
 * podr&iacute;a invalidar firmas anteriores, por lo que ser&iacute;a necesaria
 * la confirmaci&oacute;n del usuario.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * @author Carlos Gamuci */
public final class PdfFormModifiedException extends RuntimeConfigNeededException {

	/** Serial ID. */
	private static final long serialVersionUID = -4146182718127282210L;

	public static final String REQUESTOR_MSG_CODE = "signingModifiedPdfForm"; //$NON-NLS-1$

	public PdfFormModifiedException(final String msg) {
		super(msg, RequestType.CONFIRM, REQUESTOR_MSG_CODE, PdfExtraParams.ALLOW_SIGN_MODIFIED_FORM);
	}
}
