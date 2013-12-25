/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.pkcs7;

import java.util.Dictionary;
import java.util.Hashtable;
import java.util.Locale;

/** Identificadores (OID) comunes de algoritmos usados en CMS/CAdES
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class AOAlgorithmID {

    private static final String OID_SHA1   = "1.3.14.3.2.26"; //$NON-NLS-1$
    private static final String OID_SHA512 = "2.16.840.1.101.3.4.2.3"; //$NON-NLS-1$
    private static final String OID_SHA256 = "2.16.840.1.101.3.4.2.1"; //$NON-NLS-1$
    private static final String OID_SHA384 = "2.16.840.1.101.3.4.2.2"; //$NON-NLS-1$
    private static final String OID_RSA    = "1.2.840.113549.1.1.1"; //$NON-NLS-1$

    private AOAlgorithmID() {
        // No permitimos la instanciacion
    }

    private static final Dictionary<String, String> OIDS = new Hashtable<String, String>();
    static {
        OIDS.put("SHA1", OID_SHA1); //$NON-NLS-1$
        OIDS.put("SHA-1", OID_SHA1); //$NON-NLS-1$
        OIDS.put("SHA", OID_SHA1); //$NON-NLS-1$
        OIDS.put(OID_SHA1, OID_SHA1);

        OIDS.put("SHA-512", OID_SHA512); //$NON-NLS-1$
        OIDS.put("SHA512", OID_SHA512); //$NON-NLS-1$
        OIDS.put(OID_SHA512, OID_SHA512);

        OIDS.put("SHA-256", OID_SHA256); //$NON-NLS-1$
        OIDS.put("SHA256", OID_SHA256); //$NON-NLS-1$
        OIDS.put(OID_SHA256, OID_SHA256);

        OIDS.put("SHA-384", OID_SHA384); //$NON-NLS-1$
        OIDS.put("SHA384", OID_SHA384); //$NON-NLS-1$
        OIDS.put(OID_SHA384, OID_SHA384);

        OIDS.put("RSA", OID_RSA); //$NON-NLS-1$
        OIDS.put(OID_RSA, OID_RSA);
    }

    /** Obtiene el OID del algoritmo indicado.
     * @param name Nombre del algoritmo
     * @return OID del algoritmo
     */
    public static String getOID(final String name) {
        if (name == null) {
            return null;
        }
        final String res = OIDS.get(name.toUpperCase(Locale.US));
        if (res == null) {
            throw new IllegalArgumentException("Se deconoce el algoritmo '" + name + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return res;
    }

}
