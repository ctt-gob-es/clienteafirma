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

/** Excepci&oacute;n utilizada para indicar que se ha sobrepasado el
 * n&uacute;mero m&aacute;ximo de intentos de acceso a un recurso protegido. */
public final class AOMaxAttemptsExceededException extends AOException {

    private static final long serialVersionUID = -1340950945486548407L;

    /** Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n. */
    public AOMaxAttemptsExceededException(String msg) {
        super(msg);
    }
}
