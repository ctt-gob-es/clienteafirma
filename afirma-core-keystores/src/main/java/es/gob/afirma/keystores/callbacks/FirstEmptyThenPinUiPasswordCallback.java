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

/** <i>PasswordCallbak</i> que, la primera vez que se le pide una contrase&ntilde;a siempre
 * devuelve <code>null</code> y las siguientes pide una contrase&ntilde;a al usuario mediante
 * un di&aacute;logo gr&aacute;fico.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class FirstEmptyThenPinUiPasswordCallback extends PasswordCallback {

	private static final long serialVersionUID = -6862690043430780707L;

    private static final boolean isFirstTime = true;

    /** Di&aacute;logo a utilizar cuando se usa interfaz gr&aacute;fico. */
    private final UIPasswordCallback uiPasswordCallback;

    /** Crea una <i>CallBack</i> para solicitar al usuario una contrase&ntilde;a
     * mediante un di&aacute;logo gr&aacute;fico. La contrase&ntilde;a no se
     * retiene ni almacena internamente en ning&uacute;n momento
     * @param prompt Texto del di&aacute;logo para solicitar la contrase&ntilde;a
     * @param parent Componente padre para la modalidad del di&aacute;logo */
    public FirstEmptyThenPinUiPasswordCallback(final String prompt, final Object parent) {
        super(prompt, false);
        this.uiPasswordCallback = new UIPasswordCallback(prompt, parent);
    }

    /** Crea una <i>CallBack</i> para solicitar al usuario una contrase&ntilde;a
     * mediante un di&aacute;logo gr&aacute;fico. La contrase&ntilde;a no se
     * retiene ni almacena internamente en ning&uacute;n momento.
     * Si no se establece un componente padre para la modalidad con <code>setParent()</code>
     * antes de llamar a <code>getPassword()</code> se usar&aacute; null
     * @param prompt Texto del di&aacute;logo para solicitar la contrase&ntilde;a */
    public FirstEmptyThenPinUiPasswordCallback(final String prompt) {
        super(prompt, false);
        this.uiPasswordCallback = new UIPasswordCallback(prompt);
    }

    /** Establece el componente padre para la modalidad del di&aacute;logo
     * @param p Componente padre para la modalidad del di&aacute;logo */
    public void setParent(final Object p) {
    	this.uiPasswordCallback.setParent(p);
    }

    @Override
    public char[] getPassword() {
    	if (isFirstTime) {
    		return new char[0];
    	}
        return this.uiPasswordCallback.getPassword();
    }

}
