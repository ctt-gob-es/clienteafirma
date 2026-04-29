/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xml;

import es.gob.afirma.core.RuntimeConfigNeededException;

/** Indica que al realizar una cofirma, una de las firmas ya existente no incluye algoritmo XPath. */

public final class NotFoundXPathException extends RuntimeConfigNeededException {

	/** Serial Id. */
	private static final long serialVersionUID = 8047929185538217109L;

	public static final String REQUESTOR_MSG_CODE = "xPathNotFound"; //$NON-NLS-1$
	private static final String EXTRA_PARAM_NEEDED = "allowXadesEnvelopedWithoutXPath"; //$NON-NLS-1$

	/**
	 * Crea una excepci&oacute;n que indica que no se ha encontrado el algoritmo XPath
	 * @param msg Mensaje de error.
	 */
	public NotFoundXPathException(final String msg) {
		super(msg, RequestType.CONFIRM, REQUESTOR_MSG_CODE, EXTRA_PARAM_NEEDED, XMLErrorCode.Functional.XPATH_NOT_FOUND);
	}

}
