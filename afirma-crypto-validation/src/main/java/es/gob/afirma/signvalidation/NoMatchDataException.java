/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signvalidation;

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
