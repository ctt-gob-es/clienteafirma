/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core;

/**
 * Excepci&oacute;n gen&eacute;rica.
 * @version 1.1
 */
public class AOException extends Exception implements AOControlledException {

    private static final long serialVersionUID = -662191654860389176L;

    private ErrorCode errorCode = null;

    /**
     * Contruye una excepci&oacute;n gen&eacute;rica con mensaje.
     * @param code C&oacute;dico que identifica al error.
     */
    public AOException(final ErrorCode code) {
		super(code != null ? code.getDescription() : null);
		this.errorCode = code;
	}

    /**
     * Contruye una excepci&oacute;n gen&eacute;rica con mensaje.
     * @param msg Descripci&oacute;n del error.
     * @param code C&oacute;dico que identifica al error.
     */
    public AOException(final String msg, final ErrorCode code) {
		super(msg);
		this.errorCode = code;
	}

    /**
     * Contruye una excepci&oacute;n gen&eacute;rica y define su causa.
     * @param cause Causa del error.
     * @param code C&oacute;dico que identifica al error.
     */
    public AOException(final Throwable cause, final ErrorCode code) {
		super(code != null ? code.getDescription() : null, cause);
		this.errorCode = code;
	}

    /**
     * Contruye una excepci&oacute;n gen&eacute;rica con mensaje y define su causa.
     * @param msg Descripci&oacute;n del error.
     * @param cause Causa del error.
     * @param code C&oacute;dico que identifica al error.
     */
    public AOException(final String msg, final Throwable cause, final ErrorCode code) {
    	super(msg, cause);
    	this.errorCode = code;
	}

    @Override
	public ErrorCode getErrorCode() {
		return this.errorCode;
	}

	@Override
	public String toString() {
		String appendix = null;
		if (this.errorCode != null) {
			appendix = " (" + this.errorCode + ")"; //$NON-NLS-1$ //$NON-NLS-2$
		}
		return appendix != null ? super.toString() + appendix : super.toString();
	}
}