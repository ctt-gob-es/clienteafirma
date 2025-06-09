/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;

/** Error usado para indicar que es necesaria la comunicaci&oacute;n por
 * socket para realizar una operaci&oacute;n. */
class SocketOperationException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = -1031351741046263364L;

	SocketOperationException(final ErrorCode errorCode) {
		super(errorCode);
	}

	SocketOperationException(final AOException cause) {
		super(cause, cause.getErrorCode());
	}

	SocketOperationException(final String message, final ErrorCode errorCode) {
		super(message, errorCode);
	}

	SocketOperationException(final Throwable cause, final ErrorCode errorCode) {
		super(cause != null ? cause.getMessage() : null, cause, errorCode);
	}

	SocketOperationException(final String message, final Throwable cause, final ErrorCode errorCode) {
		super(message, cause, errorCode);
	}

	@Override
	public String getMessage() {
		return super.getMessage() != null ? super.getMessage() : getErrorCode().getCode();
	}
}