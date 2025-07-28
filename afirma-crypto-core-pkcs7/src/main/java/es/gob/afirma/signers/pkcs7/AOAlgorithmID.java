/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pkcs7;

import java.util.Dictionary;
import java.util.Hashtable;

/** Identificadores (OID) comunes de algoritmos usados en CMS/CAdES
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class AOAlgorithmID {

    /** OID del algoritmo de huella SHA-256. */
    public static final String OID_SHA256 = "2.16.840.1.101.3.4.2.1"; //$NON-NLS-1$

    private static final String OID_SHA1   = "1.3.14.3.2.26"; //$NON-NLS-1$
    private static final String OID_SHA224 = "2.16.840.1.101.3.4.2.4"; //$NON-NLS-1$
    private static final String OID_SHA512 = "2.16.840.1.101.3.4.2.3"; //$NON-NLS-1$
    private static final String OID_SHA384 = "2.16.840.1.101.3.4.2.2"; //$NON-NLS-1$

    private static final String OID_RSA        = "1.2.840.113549.1.1.1"; //$NON-NLS-1$
    private static final String OID_RSA_SHA1   = "1.2.840.113549.1.1.5"; //$NON-NLS-1$
    private static final String OID_RSA_SHA224 = "1.2.840.113549.1.1.14"; //$NON-NLS-1$
    private static final String OID_RSA_SHA256 = "1.2.840.113549.1.1.11"; //$NON-NLS-1$
    private static final String OID_RSA_SHA384 = "1.2.840.113549.1.1.12"; //$NON-NLS-1$
    private static final String OID_RSA_SHA512 = "1.2.840.113549.1.1.13"; //$NON-NLS-1$

    private static final String OID_ECDSA_SHA1   = "1.2.840.10045.4.1"; //$NON-NLS-1$
    private static final String OID_ECDSA_SHA224 = "1.2.840.10045.4.3.1"; //$NON-NLS-1$
    private static final String OID_ECDSA_SHA256 = "1.2.840.10045.4.3.2"; //$NON-NLS-1$
    private static final String OID_ECDSA_SHA384 = "1.2.840.10045.4.3.3"; //$NON-NLS-1$
    private static final String OID_ECDSA_SHA512 = "1.2.840.10045.4.3.4"; //$NON-NLS-1$

    private static final String OID_RSA_SHA3_224 = "2.16.840.1.101.3.4.3.13"; //$NON-NLS-1$
    private static final String OID_RSA_SHA3_256 = "2.16.840.1.101.3.4.3.14"; //$NON-NLS-1$
    private static final String OID_RSA_SHA3_384 = "2.16.840.1.101.3.4.3.15"; //$NON-NLS-1$
    private static final String OID_RSA_SHA3_512 = "2.16.840.1.101.3.4.3.16"; //$NON-NLS-1$

    private static final String OID_ECDSA_SHA3_224 = "2.16.840.1.101.3.4.3.9"; //$NON-NLS-1$
    private static final String OID_ECDSA_SHA3_256 = "2.16.840.1.101.3.4.3.10"; //$NON-NLS-1$
    private static final String OID_ECDSA_SHA3_384 = "2.16.840.1.101.3.4.3.11"; //$NON-NLS-1$
    private static final String OID_ECDSA_SHA3_512 = "2.16.840.1.101.3.4.3.12"; //$NON-NLS-1$
    
    private static final String OID_DSA = "1.2.840.10040.4.1"; //$NON-NLS-1$
    private static final String OID_DSA_SHA1 = "1.2.840.10040.4.3"; //$NON-NLS-1$
    private static final String OID_DSA_SHA224 = "2.16.840.1.101.3.4.3.1"; //$NON-NLS-1$
    private static final String OID_DSA_SHA256 = "2.16.840.1.101.3.4.3.2"; //$NON-NLS-1$
    private static final String OID_DSA_SHA384 = "2.16.840.1.101.3.4.3.3"; //$NON-NLS-1$
    private static final String OID_DSA_SHA512 = "2.16.840.1.101.3.4.3.4"; //$NON-NLS-1$
    
    private static final String OID_DSA_SHA3_224 = "2.16.840.1.101.3.4.3.5"; //$NON-NLS-1$
    private static final String OID_DSA_SHA3_256 = "2.16.840.1.101.3.4.3.6"; //$NON-NLS-1$
    private static final String OID_DSA_SHA3_384 = "2.16.840.1.101.3.4.3.7"; //$NON-NLS-1$
    private static final String OID_DSA_SHA3_512 = "2.16.840.1.101.3.4.3.8"; //$NON-NLS-1$


    private AOAlgorithmID() {
        // No permitimos la instanciacion
    }

    private static final Dictionary<String, String> OIDS = new Hashtable<>();
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

        OIDS.put("SHA-224", OID_SHA224); //$NON-NLS-1$
        OIDS.put("SHA224", OID_SHA224); //$NON-NLS-1$
        OIDS.put(OID_SHA224, OID_SHA224);

        OIDS.put("RSA", OID_RSA); //$NON-NLS-1$
        OIDS.put(OID_RSA, OID_RSA);

        OIDS.put("SHA1withRSA", OID_RSA_SHA1); //$NON-NLS-1$
        OIDS.put("SHA-1withRSA", OID_RSA_SHA1); //$NON-NLS-1$

        OIDS.put("SHA224withRSA", OID_RSA_SHA224); //$NON-NLS-1$
        OIDS.put("SHA-224withRSA", OID_RSA_SHA224); //$NON-NLS-1$

        OIDS.put("SHA256withRSA", OID_RSA_SHA256); //$NON-NLS-1$
        OIDS.put("SHA-256withRSA", OID_RSA_SHA256); //$NON-NLS-1$

        OIDS.put("SHA384withRSA", OID_RSA_SHA384); //$NON-NLS-1$
        OIDS.put("SHA-384withRSA", OID_RSA_SHA384); //$NON-NLS-1$

        OIDS.put("SHA512withRSA", OID_RSA_SHA512); //$NON-NLS-1$
        OIDS.put("SHA-512withRSA", OID_RSA_SHA512); //$NON-NLS-1$

        OIDS.put("SHA1withECDSA", OID_ECDSA_SHA1); //$NON-NLS-1$
        OIDS.put("SHA-1withECDSA", OID_ECDSA_SHA1); //$NON-NLS-1$

        OIDS.put("SHA224withECDSA", OID_ECDSA_SHA224); //$NON-NLS-1$
        OIDS.put("SHA-224withECDSA", OID_ECDSA_SHA224); //$NON-NLS-1$

        OIDS.put("SHA256withECDSA", OID_ECDSA_SHA256); //$NON-NLS-1$
        OIDS.put("SHA-256withECDSA", OID_ECDSA_SHA256); //$NON-NLS-1$

        OIDS.put("SHA384withECDSA", OID_ECDSA_SHA384); //$NON-NLS-1$
        OIDS.put("SHA-384withECDSA", OID_ECDSA_SHA384); //$NON-NLS-1$

        OIDS.put("SHA512withECDSA", OID_ECDSA_SHA512); //$NON-NLS-1$
        OIDS.put("SHA-512withECDSA", OID_ECDSA_SHA512); //$NON-NLS-1$
        
        OIDS.put("DSA", OID_DSA); //$NON-NLS-1$
        OIDS.put(OID_DSA, OID_DSA);
        
        OIDS.put("SHA1withDSA", OID_DSA_SHA1); //$NON-NLS-1$
        OIDS.put("SHA-1withDSA", OID_DSA_SHA1); //$NON-NLS-1$

        OIDS.put("SHA224withDSA", OID_DSA_SHA224); //$NON-NLS-1$
        OIDS.put("SHA-224withDSA", OID_DSA_SHA224); //$NON-NLS-1$

        OIDS.put("SHA256withDSA", OID_DSA_SHA256); //$NON-NLS-1$
        OIDS.put("SHA-256withDSA", OID_DSA_SHA256); //$NON-NLS-1$

        OIDS.put("SHA384withDSA", OID_DSA_SHA384); //$NON-NLS-1$
        OIDS.put("SHA-384withDSA", OID_DSA_SHA384); //$NON-NLS-1$

        OIDS.put("SHA512withDSA", OID_DSA_SHA512); //$NON-NLS-1$
        OIDS.put("SHA-512withDSA", OID_DSA_SHA512); //$NON-NLS-1$

        // https://docs.oracle.com/javase/9/docs/specs/security/standard-names.html#signature-algorithms

        OIDS.put("SHA3-224withRSA", OID_RSA_SHA3_224); //$NON-NLS-1$
        OIDS.put("SHA3-256withRSA", OID_RSA_SHA3_256); //$NON-NLS-1$
        OIDS.put("SHA3-384withRSA", OID_RSA_SHA3_384); //$NON-NLS-1$
        OIDS.put("SHA3-512withRSA", OID_RSA_SHA3_512); //$NON-NLS-1$

        OIDS.put("SHA3-224withECDSA", OID_ECDSA_SHA3_224); //$NON-NLS-1$
        OIDS.put("SHA3-256withECDSA", OID_ECDSA_SHA3_256); //$NON-NLS-1$
        OIDS.put("SHA3-384withECDSA", OID_ECDSA_SHA3_384); //$NON-NLS-1$
        OIDS.put("SHA3-512withECDSA", OID_ECDSA_SHA3_512); //$NON-NLS-1$
        
        OIDS.put("SHA3-224withDSA", OID_DSA_SHA3_224); //$NON-NLS-1$
        OIDS.put("SHA3-256withDSA", OID_DSA_SHA3_256); //$NON-NLS-1$
        OIDS.put("SHA3-384withDSA", OID_DSA_SHA3_384); //$NON-NLS-1$
        OIDS.put("SHA3-512withDSA", OID_DSA_SHA3_512); //$NON-NLS-1$

    }

    /** Obtiene el OID del algoritmo indicado.
     * @param name Nombre del algoritmo.
     * @return OID del algoritmo. */
    public static String getOID(final String name) {
        if (name == null) {
            return null;
        }
        final String res = OIDS.get(name);
        if (res == null) {
            throw new IllegalArgumentException("Se desconoce el algoritmo '" + name + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return res;
    }
    
    /** Indica si el OID pertenece a una encriptaci&oacute;n de tipo RSA o no.
     * @param oid OID del algoritmo.
     * @return true en caso de que sea RSA. */
    public static boolean isRSAOID(final String oid) {
    	boolean isRSA = false;
    	
        if (OID_RSA.equals(oid)
        || OID_RSA_SHA1.equals(oid)
        || OID_RSA_SHA224.equals(oid)
        || OID_RSA_SHA256.equals(oid)
        || OID_RSA_SHA384.equals(oid)
        || OID_RSA_SHA512.equals(oid)
        || OID_RSA_SHA3_224.equals(oid)
        || OID_RSA_SHA3_256.equals(oid)
        || OID_RSA_SHA3_384.equals(oid)
        || OID_RSA_SHA3_512.equals(oid)) {
        	
        	isRSA = true;
        }
        
        return isRSA;
    }
    
    /** Indica si el OID pertenece a una encriptaci&oacute;n de tipo ECDSA o no.
     * @param oid OID del algoritmo.
     * @return true en caso de que sea ECDSA. */
    public static boolean isECDSAOID(final String oid) {
    	boolean isECDSA = false;
    	
        if (OID_ECDSA_SHA1.equals(oid)
        || OID_ECDSA_SHA224.equals(oid)
        || OID_ECDSA_SHA256.equals(oid)
        || OID_ECDSA_SHA384.equals(oid)
        || OID_ECDSA_SHA512.equals(oid)
        || OID_ECDSA_SHA3_224.equals(oid)
        || OID_ECDSA_SHA3_256.equals(oid)
        || OID_ECDSA_SHA3_384.equals(oid)
        || OID_ECDSA_SHA3_512.equals(oid)) {
        	
        	isECDSA = true;
        }
        
        return isECDSA;
    }
    
    /** Indica si el OID pertenece a una encriptaci&oacute;n de tipo DSA o no.
     * @param oid OID del algoritmo.
     * @return true en caso de que sea DSA. */
    public static boolean isDSAOID(final String oid) {
    	boolean isDSA = false;
    	
        if (OID_DSA.equals(oid)
        || OID_DSA_SHA1.equals(oid)
        || OID_DSA_SHA224.equals(oid)
        || OID_DSA_SHA256.equals(oid)
        || OID_DSA_SHA384.equals(oid)
        || OID_DSA_SHA512.equals(oid)
        || OID_DSA_SHA3_224.equals(oid)
        || OID_DSA_SHA3_256.equals(oid)
        || OID_DSA_SHA3_384.equals(oid)
        || OID_DSA_SHA3_512.equals(oid)) {
        	
        	isDSA = true;
        }
        
        return isDSA;
    }

}
