/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pkcs7;

/** Comprobador de la versi&oacute;n de BouncyCastle del sistema.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SCChecker {

    /** Versi&oacute;n de BouncyCastle necesaria para el uso de esta clase (1.47 o superior). */
    private static final String SC_VERSION = "1.47"; //$NON-NLS-1$

	/** Comprueba que la versi&oacute;n de BouncyCastle existente sea v1.71 o superior.
	 * @throws InvalidBouncyCastleException Cuando no se puede detectar la versi&oacute;n de
	 * BouncyCastle disponible o no es compatible con afirma. */
	@SuppressWarnings("static-method")
	public void checkBouncyCastle() {
		try {
			Class.forName("org.bouncycastle.asn1.ASN1Primitive"); //$NON-NLS-1$
		}
		catch(final ClassNotFoundException e) {
			throw new InvalidBouncyCastleException(SC_VERSION, "1.71 o anterior", e); //$NON-NLS-1$
		}
	}

}
