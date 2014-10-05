/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.pades;

import es.gob.afirma.core.AOException;

/** Indica que el PDF no ha podido firmarse por contener firmas previas no registradas en campos (<i>AcroFields</i>).
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PdfHasUnregisteredSignaturesException extends AOException {

	private static final long serialVersionUID = 7058068490466267934L;

	/** Crea una excepci&oacute;n que indica que el PDF no ha podido firmarse por contener firmas previas
	 * no registradas en campos (<i>AcroFields</i>). */
	PdfHasUnregisteredSignaturesException() {
		super("El PDF no se ha firmado por contener firmas no registradas"); //$NON-NLS-1$
	}

}
