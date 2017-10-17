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

/** PasswordCallback que siempre devuelve <code>null</code> como
 * contrase&ntilde;a. */
public final class NullPasswordCallback extends PasswordCallback {

    private static final long serialVersionUID = -5926953046433722802L;

    private static final NullPasswordCallback INSTANCE = new NullPasswordCallback();

    /** Contruye la forma b&aacute;sica de la clase. */
    private NullPasswordCallback() {
        super(">", false); //$NON-NLS-1$
    }

    @Override
    public char[] getPassword() {
        return null;
    }

    /** Devuelve una instancia de la clase.
     * @return Instancia (&uacute;nica) de <code>NullPasswordCallback</code> */
    public static NullPasswordCallback getInstance() {
    	return INSTANCE;
    }
}
