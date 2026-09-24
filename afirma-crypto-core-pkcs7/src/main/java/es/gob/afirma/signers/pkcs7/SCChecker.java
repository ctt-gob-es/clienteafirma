/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pkcs7;

/** Comprobador de la presencia de BouncyCastle en el classpath.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SCChecker {

    /** Versi&oacute;n m&iacute;nima hist&oacute;rica documentada (API ASN.1 compatible). */
    private static final String SC_VERSION = "1.78"; //$NON-NLS-1$

	/** Comprueba que exista un proveedor BouncyCastle usable (jdk18on).
	 * @throws InvalidSpongyCastleException Cuando no se encuentra la API ASN.1 de BouncyCastle. */
	@SuppressWarnings("static-method")
	public void checkSpongyCastle() {
		try {
			Class.forName("org.bouncycastle.asn1.ASN1Primitive"); //$NON-NLS-1$
		}
		catch(final ClassNotFoundException e) {
			throw new InvalidSpongyCastleException(SC_VERSION, "ausente o incompatible", e); //$NON-NLS-1$
		}
	}

}
