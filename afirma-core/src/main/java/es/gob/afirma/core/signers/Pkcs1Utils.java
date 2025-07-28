/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.io.IOException;
import java.math.BigInteger;
import java.security.SignatureException;

import es.gob.afirma.core.signers.der.DerInputStream;
import es.gob.afirma.core.signers.der.DerOutputStream;
import es.gob.afirma.core.signers.der.DerValue;

/**
 * Utilidades para la codificaci&oacute;n del PKCS#1 de las firmas.
 */
public final class Pkcs1Utils {

	private Pkcs1Utils() {
		// No instanciable
	}


 	/**
 	 * Convierte la concatenaci&oacute;n de R y S a codificaci&oacute;n DER.
 	 * Esto permite validar el PKCS#1 sin usar formato P1363 cuando se ha generado la firma en este
 	 * formato.
 	 * @param signature Firma PKCS#1.
 	 * @return Firma codificada.
 	 * @throws SignatureException Cuando ocurre un error en la codificaci&oacute;n.
 	 */
    public static byte[] encodeSignature(final byte[] signature) throws SignatureException {

        try {

            final int n = signature.length >> 1;
            final byte[] bytes = new byte[n];
            System.arraycopy(signature, 0, bytes, 0, n);
            final BigInteger r = new BigInteger(1, bytes);
            System.arraycopy(signature, n, bytes, 0, n);
            final BigInteger s = new BigInteger(1, bytes);

            final DerOutputStream out = new DerOutputStream(signature.length + 10);
            out.putInteger(r);
            out.putInteger(s);
            final DerValue result =
                new DerValue(DerValue.tag_Sequence, out.toByteArray());

            return result.toByteArray();

        } catch (final Exception e) {
            throw new SignatureException("Could not encode signature", e); //$NON-NLS-1$
        }
    }

    // Decodifica Convert the DER encoding of R and S into a concatenation of R and S
 	/**
 	 * Convierte la codificacion DER de R y S en la concatenaci&oacute;n de R y S.
 	 * Esto permite codificar el PKCS#1 generado para su posterior validaci&oacute;n usando
 	 * P1363Format.
 	 * @param signature Firma PKCS#1.
 	 * @return Firma decodificada.
 	 * @throws SignatureException Cuando ocurre un error en la codificaci&oacute;n.
 	 */
    public static byte[] decodeSignature(final byte[] signature) throws SignatureException {

        try {
            // Enforce strict DER checking for signatures
            final DerInputStream in = new DerInputStream(signature, 0, signature.length, false);
            final DerValue[] values = in.getSequence(2);

            // check number of components in the read sequence
            // and trailing data
            if (values.length != 2 || in.available() != 0) {
                throw new IOException("Invalid encoding for signature");
            }

            final BigInteger r = values[0].getPositiveBigInteger();
            final BigInteger s = values[1].getPositiveBigInteger();

            // trim leading zeroes
            final byte[] rBytes = trimZeroes(r.toByteArray());
            final byte[] sBytes = trimZeroes(s.toByteArray());
            final int k = Math.max(rBytes.length, sBytes.length);
            // r and s each occupy half the array
            final byte[] result = new byte[k << 1];
            System.arraycopy(rBytes, 0, result, k - rBytes.length,
                rBytes.length);
            System.arraycopy(sBytes, 0, result, result.length - sBytes.length,
                sBytes.length);
            return result;

        } catch (final Exception e) {
            throw new SignatureException("Invalid encoding for signature", e);
        }
    }

    // trim leading (most significant) zeroes from the result
    private static byte[] trimZeroes(final byte[] b) {
    	int i = 0;
    	while (i < b.length - 1 && b[i] == 0) {
    		i++;
    	}
    	if (i == 0) {
    		return b;
    	}
    	final byte[] t = new byte[b.length - i];
    	System.arraycopy(b, i, t, 0, t.length);
    	return t;
    }
}
