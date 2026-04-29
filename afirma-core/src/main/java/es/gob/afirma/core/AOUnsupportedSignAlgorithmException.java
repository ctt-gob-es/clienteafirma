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
 * Excepci&oacute;n lanzada cuando se detecta un algoritmo de firma no
 * reconocido o a partir del cual no se pueden generar toda la
 * configuraci&oacute;n necesaria para componer la firma en el formato deseado.
 */
public final class AOUnsupportedSignAlgorithmException extends AOException {

    /** Serial Id. */
	private static final long serialVersionUID = 5925504116235792982L;

	/** Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n. */
    public AOUnsupportedSignAlgorithmException(final String msg) {
        super(msg, ErrorCode.Request.UNSUPPORTED_SIGNATURE_ALGORITHM);
    }

    /** Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n.
     * @param e
     *        Excepci&oacute;n que ha causado el lanzamiento de esta. */
    public AOUnsupportedSignAlgorithmException(final String msg, final Exception e) {
        super(msg, e, ErrorCode.Request.UNSUPPORTED_SIGNATURE_ALGORITHM);
    }
}
