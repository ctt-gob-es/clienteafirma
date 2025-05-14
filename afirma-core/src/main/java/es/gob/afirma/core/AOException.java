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
public class AOException extends Exception {

    private static final long serialVersionUID = -662191654860389176L;

    private ErrorCode errorCode = null;

//    /**
//     * Contruye una excepci&oacute;n gen&eacute;rica con mensaje.
//     * @param msg Mensaje de la excepci&oacute;n.
//     */
//    public AOException(final String msg) {
//        super(msg);
//    }
//
//    /**
//     * Contruye una excepci&oacute;n gen&eacute;rica definiendo su causa.
//     * @param cause Causa del error.
//     */
//    public AOException(final Throwable cause) {
//        super(cause);
//    }
//
//    /**
//     * Contruye una excepci&oacute;n gen&eacute;rica con mensaje y define su causa.
//     * @param msg Descripci&oacute;n del error.
//     * @param cause Causa del error.
//     */
//    public AOException(final String msg, final Throwable cause) {
//        super(msg, cause);
//    }

    /**
     * Contruye una excepci&oacute;n gen&eacute;rica con mensaje.
     * @param msg Descripci&oacute;n del error.
     */
    public AOException(final ErrorCode code) {
		super(code != null ? code.getDescription() : null);
		this.errorCode = code;
	}

    /**
     * Contruye una excepci&oacute;n gen&eacute;rica con mensaje.
     * @param msg Descripci&oacute;n del error.
     */
    public AOException(final String msg, final ErrorCode code) {
		super(msg);
		this.errorCode = code;
	}

    /**
     * Contruye una excepci&oacute;n gen&eacute;rica y define su causa.
     * @param cause Causa del error.
     */
    public AOException(final Throwable cause, final ErrorCode code) {
		super(code != null ? code.getDescription() : null, cause);
		this.errorCode = code;
	}

    /**
     * Contruye una excepci&oacute;n gen&eacute;rica con mensaje y define su causa.
     * @param msg Descripci&oacute;n del error.
     * @param cause Causa del error.
     */
    public AOException(final String msg, final Throwable cause, final ErrorCode code) {
    	super(msg, cause);
    	this.errorCode = code;
	}

    /**
     * Recupera el c&oacute;digo de error asociado a la excepci&oacute;n,
     * @return C&oacute;digo de error o {@code null} si no se defini&oacute;n.
     */
    public ErrorCode getErrorCode() {
		return this.errorCode;
	}

    @Override
    public String toString() {
    	String description = super.toString();
    	if (this.errorCode != null) {
    		description += " (" + this.errorCode + ")"; //$NON-NLS-1$ //$NON-NLS-2$
    	}
    	return description;
    }
}