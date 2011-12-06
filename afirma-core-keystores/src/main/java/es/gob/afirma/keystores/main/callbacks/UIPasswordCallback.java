/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

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
