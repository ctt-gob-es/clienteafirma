/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xades;

import es.gob.afirma.core.AOInvalidFormatException;

/** Excepci&oacute;n para notificar que la factura electr&oacute;nica ya est&aacute; firmada y por lo tanto no se pueden
 * a&ntilde;adir m&aacute;s firmas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class EFacturaAlreadySignedException extends AOInvalidFormatException {

	private static final long serialVersionUID = -2721087059930025508L;

	public EFacturaAlreadySignedException() {
		super("La factura ya tiene una firma electronica y no admite firmas adicionales"); //$NON-NLS-1$
	}

}
