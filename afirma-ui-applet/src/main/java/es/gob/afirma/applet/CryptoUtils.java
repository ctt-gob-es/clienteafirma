/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/** Utilidades criptogr&aacute;ficas. */
final class CryptoUtils {

	private CryptoUtils() {
		// No permitimos la instanciacion
	}

    static byte[] getMessageDigest(final byte[] data, final String algorithm) throws NoSuchAlgorithmException {
    	return MessageDigest.getInstance(algorithm).digest(data);
    }
}
