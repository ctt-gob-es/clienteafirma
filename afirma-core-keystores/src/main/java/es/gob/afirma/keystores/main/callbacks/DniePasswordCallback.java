/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores.main.callbacks;

import java.net.URL;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.Icon;
import javax.swing.ImageIcon;

import es.gob.afirma.core.ui.AOUIFactory;

/** <i>PasswordCallbak</i> que muestra un di&aacute;logo para solicitar el
 * PIN del DNIe. */
public final class DniePasswordCallback extends PasswordCallback {

    private static final long serialVersionUID = 1719174318602363633L;

    /** Componente padre sobre el que se mostrar&aacute; el di&aacute;logo para
     * la inserci&oacute;n del PIN. */
    private Object parent = null;

    /** Crea una <i>CallBack</i> para solicitar al usuario el PIN del DNIe
     * mediante un di&aacute;logo gr&aacute;fico. El PIN no se retiene
     * ni almacena internamente en ning&uacute;n momento.
     * @param prompt
     *        Texto del di&aacute;logo para solicitar la contrase&ntilde;a
     * @param parent
     *        Componente padre para la modalidad del di&aacute;logo */
    public DniePasswordCallback(final String prompt, final Object parent) {
        super(prompt, false);
        this.parent = parent;
    }

    @Override
    public char[] getPassword() {
    	return AOUIFactory.getPassword(this.getPrompt(), loadDnieIcon(), null, false, this.parent);
    }
    
    /**
     * Carga un icono para identificar al DNIe.
     * @return Icono.
     */
    private static Icon loadDnieIcon() {
    	return new ImageIcon(loadResourceURL("resources/dnielogoicon.png")); //$NON-NLS-1$
    }
    
    /**
     * Carga la URL del icono de un recurso de la biblioteca.
     * @param resourcesPath Ruta del recurso.
     * @return URL del recurso.
     */
    private static URL loadResourceURL(String resourcesPath) {
    	return DniePasswordCallback.class.getClassLoader().getResource(resourcesPath);
    }
}
