/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signature;

/**
 * Indica cuando los datos contenidos en una firma no coincide con los datos firmados.
 * @author Carlos Gamuci
 */
class NoMatchDataException extends Exception {

    /** Serial ID. */
    private static final long serialVersionUID = 1L;

    /**
     * Indica que los datos contenidos en la firma no coinciden con los datos firmados.
     */
    public NoMatchDataException() {
        super();
    }

    /**
     * Indica que los datos contenidos en la firma no coinciden con los datos firmados.
     * @param message Mensaje que detalle el error.
     */
    NoMatchDataException(final String message) {
        super(message);
    }
}
