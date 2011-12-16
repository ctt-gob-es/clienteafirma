/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores.main.callbacks;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.core.ui.AOUIFactory;

/** <i>PasswordCallbak</i> que muestra un di&aacute;logo para solicitar una
 * contrase&ntilde;a. */
public final class UIPasswordCallback extends PasswordCallback {

    private static final long serialVersionUID = 1719174318602363633L;

    /** Componente padre sobre el que se mostrar&aacute; el di&aacute;logo para
     * la inserci&oacute;n de la contrase&ntilde;a. */
    private Object parent = null;

    /** Crea una <i>CallBack</i> para solicitar al usuario una contrase&ntilde;a
     * mediante un di&aacute;logo gr&aacute;fico. La contrase&ntilde;a no se
     * retiene ni almacena internamente en ning&uacute;n momento
     * @param prompt
     *        Texto del di&aacute;logo para solicitar la contrase&ntilde;a
     * @param parent
     *        Componente padre para la modalidad del di&aacute;logo */
    public UIPasswordCallback(final String prompt, final Object parent) {
        super(prompt, false);
        this.parent = parent;
    }

    @Override
    public char[] getPassword() {
        return AOUIFactory.getPassword(this.getPrompt(), this.parent);
    }
}
