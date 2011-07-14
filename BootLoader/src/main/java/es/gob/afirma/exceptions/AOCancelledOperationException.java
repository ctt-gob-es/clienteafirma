/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.exceptions;

/** Excepci&oacute;n que indica una operaci&oacute;n cancelada voluntariamente por el usuario. */
public final class AOCancelledOperationException extends RuntimeException {

    private static final long serialVersionUID = 4447842480432712246L;

    /** Crea una excepci&oacute;n sin informaci&oacute;n adicional. */
    public AOCancelledOperationException() {
        super();
    }

    /** Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg Mensaje descriptivo de la excepci&oacute;n. */
    public AOCancelledOperationException(final String msg) {
        super(msg);
    }

}
