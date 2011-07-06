/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.callbacks;

import javax.security.auth.callback.PasswordCallback;

/** PasswordCallback que siempre devuelve un array de caracteres vac&iacute;o
 * como contrase&ntilde;a. */
public final class EmptyPasswordCallback extends PasswordCallback {

    private static final long serialVersionUID = -8633754935170264262L;

    /** Contruye el la forma b&aacute;sica de la clase. */
    public EmptyPasswordCallback() {
        super(">", false);
    }

    @Override
    public char[] getPassword() {
        return new char[0];
    }
}
