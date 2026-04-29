/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import es.gob.afirma.core.AOException;
import es.gob.afirma.standalone.SimpleErrorCode;

/**
 * Indica un error durante la carga del certificado SSL para realizar la comunicaci&oacute;n con el navegador.
 */
public final class LoadTrustedCertException extends AOException {

	private static final long serialVersionUID = 7936191422727825394L;

	LoadTrustedCertException() {
		super(SimpleErrorCode.Internal.NEEDS_UPDATED_VERSION);
	}

}
