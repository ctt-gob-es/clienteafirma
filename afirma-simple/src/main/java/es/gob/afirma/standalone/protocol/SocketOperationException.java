/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

/** Error usado para indicar que es necesaria la comunicaci&oacute;n por
 * socket para realizar una operaci&oacute;n. */
class SocketOperationException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = -1031351741046263364L;

	private String errorCode ;

	SocketOperationException(final String code) {
		setErrorCode(code);
	}

	SocketOperationException(final String code, final String msg) {
		super(msg);
		setErrorCode(code);
	}

	public String getErrorCode() {
		return this.errorCode;
	}

	public void setErrorCode(final String errorCode) {
		this.errorCode = errorCode;
	}


}