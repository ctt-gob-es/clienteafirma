/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.callbacks;

import javax.security.auth.callback.PasswordCallback;

/** PasswordCallback que siempre devuelve <code>null</code> como
 * contrase&ntilde;a. */
public final class NullPasswordCallback extends PasswordCallback {

    private static final long serialVersionUID = -5926953046433722802L;

    /** Contruye el la forma b&aacute;sica de la clase. */
    public NullPasswordCallback() {
        super(">", false);
    }

    @Override
    public char[] getPassword() {
        return null;
    }
}
