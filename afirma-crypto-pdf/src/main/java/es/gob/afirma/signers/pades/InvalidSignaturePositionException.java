/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades;

/** Indica que la firma visible no se encuentra en el rango en ninguna de las p&aacute;ginas del documento a firmar
 * @author Jos&eacute; Montero Rivero */
public final class InvalidSignaturePositionException extends IllegalArgumentException {

	private static final long serialVersionUID = -2415913095143368605L;

	/** Crea la excepci&oacute;n indicando que la firma visible no se encuentra en el
	 * rango en ninguna de las p&aacute;ginas del documento.
	 * @param msg Mensaje de error. */
	public InvalidSignaturePositionException(final String msg) {
		super(msg);
	}

}
