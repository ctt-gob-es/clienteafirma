/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core;

/**
 * Excepci&oacute;n para notificar que los datos proporcionados no disponen del formato
 * de firma que se esperaba.
 */
public class AOInvalidSignatureFormatException extends AOException {

    private static final long serialVersionUID = 825249824660706387L;

    /**
     * Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n.
     */
    public AOInvalidSignatureFormatException(final String msg) {
        super(msg, ErrorCode.Internal.COMPATIBLE_SIGNATURE_NOT_FOUND);
    }

    /**
     * Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n.
     * @param e
     *        Excepci&oacute;n que ha causado el lanzamiento de esta.
     */
    public AOInvalidSignatureFormatException(final String msg, final Exception e) {
        super(msg, e, ErrorCode.Internal.COMPATIBLE_SIGNATURE_NOT_FOUND);
    }

    /**
     * Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n.
     * @param errorCode
     *        C&oacute;digo que detalla el error producido.
     */
    public AOInvalidSignatureFormatException(final String msg, final ErrorCode errorCode) {
        super(msg, errorCode);
    }

    /**
     * Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n.
     * @param e
     *        Excepci&oacute;n que ha causado el lanzamiento de esta.
     * @param errorCode
     *        C&oacute;digo que detalla el error producido.
     */
    public AOInvalidSignatureFormatException(final String msg, final Exception e, final ErrorCode errorCode) {
        super(msg, e, errorCode);
    }
}
