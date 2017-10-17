/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.callbacks;

import javax.security.auth.callback.PasswordCallback;

/** PasswordCallbak que almacena internamente y devuelve la contrase&ntilde;a con la que se
 * construy&oacute; o la que se le establece posteriormente. */
public final class CachePasswordCallback extends PasswordCallback {

    private static final long serialVersionUID = 816457144215238935L;

    /** Contruye una Callback con una contrase&ntilde; preestablecida.
     * @param password Contrase&ntilde;a por defecto. */
    public CachePasswordCallback(final char[] password) {
        super(">", false); //$NON-NLS-1$
        this.setPassword(password);
    }

    @Override
	public String toString() {
    	return "PasswordCallback con contrasena '" + new String(getPassword()) + "'"; //$NON-NLS-1$ //$NON-NLS-2$
    }
}
