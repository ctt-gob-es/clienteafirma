/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.exceptions;

/** Excepci&oacute;n gen&eacute;rica.
 * @version 1.0 */
public class AOException extends Exception {

    private static final long serialVersionUID = -662191654860389176L;

    /** Contruye una excepci&oacute;n gen&eacute;rica con mensaje.
     * @param msg
     *        Mensaje de la excepci&oacute;n */
    public AOException(String msg) {
        super(msg);
    }

    /** Contruye una excepci&oacute;n gen&eacute;rica con mensaje y define su
     * causa.
     * @param msg
     *        Descripcion del error.
     * @param e
     *        Causa del error. */
    public AOException(String msg, Exception e) {
        super(msg, e);
    }
}
