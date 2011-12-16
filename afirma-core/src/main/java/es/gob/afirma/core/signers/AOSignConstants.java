/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.signers;

/** Constantes relativas a las firmas digitales. */
public final class AOSignConstants {

    // ************************************************************
    // ************* FORMATOS DE FIRMA*****************************
    // ************************************************************

    /** Identificador de la firma CMS. */
    public static final String SIGN_FORMAT_CMS = "CMS/PKCS#7"; //$NON-NLS-1$

    /** Identificador de la firma CAdES. */
    public static final String SIGN_FORMAT_CADES = "CAdES"; //$NON-NLS-1$

    /** Identificador de la firma PKCS1 (RAW). */
    public static final String SIGN_FORMAT_PKCS1 = "NONE"; //$NON-NLS-1$

    /** Identificador de la firma XAdES Internally Detached. */
    public static final String SIGN_FORMAT_XADES_DETACHED = "XAdES Detached"; //$NON-NLS-1$

    /** Identificador de la firma XAdES Externally Detached. */
    public static final String SIGN_FORMAT_XADES_EXTERNALLY_DETACHED = "XAdES Externally Detached"; //$NON-NLS-1$

    /** Identificador de la firma XAdES Enveloped. */
    public static final String SIGN_FORMAT_XADES_ENVELOPED = "XAdES Enveloped"; //$NON-NLS-1$

    /** Identificador de la firma XAdES Enveloping. */
    public static final String SIGN_FORMAT_XADES_ENVELOPING = "XAdES Enveloping"; //$NON-NLS-1$

    /** Identificador de la firma XAdES por defecto. */
    public static final String SIGN_FORMAT_XADES = "XAdES"; //$NON-NLS-1$

    /** Identificador de la firma XMLDsig Detached. */
    public static final String SIGN_FORMAT_XMLDSIG_DETACHED = "XMLDSig Detached"; //$NON-NLS-1$

    /** Identificador de la firma XMLdSig Externally Detached. */
    public static final String SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED = "XMLDSig Externally Detached"; //$NON-NLS-1$

    /** Identificador de la firma XMLDsig Enveloped. */
    public static final String SIGN_FORMAT_XMLDSIG_ENVELOPED = "XMLDSig Enveloped"; //$NON-NLS-1$

    /** Identificador de la firma XMLDsig Enveloping. */
    public static final String SIGN_FORMAT_XMLDSIG_ENVELOPING = "XMLDSig Enveloping"; //$NON-NLS-1$

    /** Identificador de la firma XMLDSig (<i>XML Digital Signature</i>). */
    public static final String SIGN_FORMAT_XMLDSIG = "XMLDSig"; //$NON-NLS-1$

    /** Identificador de la firma OOXML (<i>Office Open XML</i>). */
    public static final String SIGN_FORMAT_OOXML = "OOXML (Office Open XML)"; //$NON-NLS-1$

    /** Identificador de la firma ODF (<i>Open Document Format</i>). */
    public static final String SIGN_FORMAT_ODF = "ODF (Open Document Format)"; //$NON-NLS-1$

    /** Identificador de la firma Adobe PDF. */
    public static final String SIGN_FORMAT_PDF = "Adobe PDF"; //$NON-NLS-1$

    /** Identificador de la firma SOAP. */
    public static final String SIGN_FORMAT_SOAP = "SOAP"; //$NON-NLS-1$

    /** Formato de firma por defecto. */
    public static final String DEFAULT_SIGN_FORMAT = SIGN_FORMAT_CMS;

    /** Identificador de la operaci&oacute;n de firma masiva. */
    public static final String MASSIVE_OPERATION_SIGN = "FIRMAR"; //$NON-NLS-1$

    /** Identificador de la operaci&oacute;n de cofirma masiva. */
    public static final String MASSIVE_OPERATION_COSIGN = "COFIRMAR"; //$NON-NLS-1$

    /** Identificador de la operaci&oacute;n de contrafirma masiva de todo el
     * &aacute;rbol de firma. */
    public static final String MASSIVE_OPERATION_COUNTERSIGN_TREE = "CONTRAFIRMAR_ARBOL"; //$NON-NLS-1$

    /** Identificador de la operaci&oacute;n de contrafirma masiva de nodos hoja
     * de firma. */
    public static final String MASSIVE_OPERATION_COUNTERSIGN_LEAFS = "CONTRAFIRMAR_HOJAS"; //$NON-NLS-1$

    /** Operaci&oacute;n masiva por defecto. */
    public static final String DEFAULT_MASSIVE_OPERATION = MASSIVE_OPERATION_SIGN;

    /** Envoltorio binario de tipo Data (datos envueltos en un envoltorio
     * PKCS#7). */
    public static final String CMS_CONTENTTYPE_DATA = "Data"; //$NON-NLS-1$

    /** Firma binaria de tipo Signed Data */
    public static final String CMS_CONTENTTYPE_SIGNEDDATA = "SignedData"; //$NON-NLS-1$

    /** Envoltorio binario de tipo Digest. */
    public static final String CMS_CONTENTTYPE_DIGESTEDDATA = "DigestedData"; //$NON-NLS-1$

    /** Envoltario binario de tipo AuthenticatedEnvelopedData. */
    public static final String CMS_CONTENTTYPE_COMPRESSEDDATA = "CompressedData"; //$NON-NLS-1$

    /** Firma binaria de tipo Encrypted Data */
    public static final String CMS_CONTENTTYPE_ENCRYPTEDDATA = "EncryptedData"; //$NON-NLS-1$

    /** Envoltorio binario de tipo Enveloped (sobre digital). */
    public static final String CMS_CONTENTTYPE_ENVELOPEDDATA = "EnvelopedData"; //$NON-NLS-1$

    /** Envoltorio binario de tipo Signed and Enveloped. */
    public static final String CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA = "SignedAndEnvelopedData"; //$NON-NLS-1$

    /** Envoltario binario de tipo AuthenticatedData. */
    public static final String CMS_CONTENTTYPE_AUTHENTICATEDDATA = "AuthenticatedData"; //$NON-NLS-1$

    /** Envoltario binario de tipo AuthenticatedEnvelopedData. */
    public static final String CMS_CONTENTTYPE_AUTHENVELOPEDDATA = "AuthEnvelopedData"; //$NON-NLS-1$

    /** Envoltorio binario por defecto. */
    public static final String DEFAULT_CMS_CONTENTTYPE = CMS_CONTENTTYPE_ENVELOPEDDATA;

    /** OID por defecto para los datos firmados. */
    public static final String DEFAULT_OID_TO_SIGN = "1.3.6.1.4.1.1466.115.121.1.40"; // Octect //$NON-NLS-1$
                                                                                      // String    
    // ************************************************************
    // ************* ALGORITMOS DE FIRMA **************************
    // ************************************************************

    /** Algoritmo de firma SHA1withRSA. */
    public static final String SIGN_ALGORITHM_SHA1WITHRSA = "SHA1withRSA"; //$NON-NLS-1$

    /** Algoritmo de firma MD5withRSA. */
    public static final String SIGN_ALGORITHM_MD5WITHRSA = "MD5withRSA"; //$NON-NLS-1$

    /** Algoritmo de firma MD2withRSA. */
    public static final String SIGN_ALGORITHM_MD2WITHRSA = "MD2withRSA"; //$NON-NLS-1$

    /** Algoritmo de firma SHA256withRSA. */
    public static final String SIGN_ALGORITHM_SHA256WITHRSA = "SHA256withRSA"; //$NON-NLS-1$

    /** Algoritmo de firma SHA384withRSA. */
    public static final String SIGN_ALGORITHM_SHA384WITHRSA = "SHA384withRSA"; //$NON-NLS-1$

    /** Algoritmo de firma SHA512withRSA. */
    public static final String SIGN_ALGORITHM_SHA512WITHRSA = "SHA512withRSA"; //$NON-NLS-1$

    /** Algoritmo de firma RSA que no incluye la generaci&oacute;n de la huella
     * digital (NONEwithRSA). */
    public static final String SIGN_ALGORITHM_NONEWITHRSA = "NONEwithRSA"; //$NON-NLS-1$

    /** Algoritmo de firma SHA1withDSA. */
    public static final String SIGN_ALGORITHM_SHA1WITHDSA = "SHA1withDSA"; //$NON-NLS-1$

    /** Algoritmo de firma SHA1withECDSA. */
    public static final String SIGN_ALGORITHM_SHA1WITHECDSA = "SHA1withECDSA"; //$NON-NLS-1$

    /** Algoritmo de firma ECDSA que no incluye la generaci&oacute;n de la huella
     * digital (NONEwithEDSSA). */
    public static final String SIGN_ALGORITHM_NONEWITHECDSA = "NONEwithECDSA"; //$NON-NLS-1$

    /** Algoritmos de firma soportados. */
    public static final String[] SUPPORTED_SIGN_ALGOS = new String[] {
            SIGN_ALGORITHM_SHA1WITHRSA,
            SIGN_ALGORITHM_MD5WITHRSA,
            SIGN_ALGORITHM_MD2WITHRSA,
            SIGN_ALGORITHM_NONEWITHRSA,
            SIGN_ALGORITHM_SHA256WITHRSA,
            SIGN_ALGORITHM_SHA384WITHRSA,
            SIGN_ALGORITHM_SHA512WITHRSA,
            SIGN_ALGORITHM_SHA1WITHECDSA,
            SIGN_ALGORITHM_NONEWITHECDSA
    };
    
    /** Algoritmo de firma por defecto. */
    public static final String DEFAULT_SIGN_ALGO = SIGN_ALGORITHM_SHA1WITHRSA;
    
    // ************************************************************
    // ****************** MODOS DE FIRMA **************************
    // ************************************************************
    
    /** Identificador del modo de firma Explicita (Los datos NO se incluyen en la
     * firma). */
    public static final String SIGN_MODE_EXPLICIT = "explicit"; //$NON-NLS-1$

    /** Identificador del modo de firma Implicita (Los datos SI se incluyen en la
     * firma). */
    public static final String SIGN_MODE_IMPLICIT = "implicit"; //$NON-NLS-1$

    /** Modo de firma por defecto. */
    public static final String DEFAULT_SIGN_MODE = SIGN_MODE_EXPLICIT;

    private AOSignConstants() {
        // No permitimos la instanciacion
    }
    
    /** Obtiene el nombre de un algoritmo de huella digital a partir de una de
     * las variantes de este.
     * @param pseudoName
     *        Nombre o variante del nombre del algoritmo de huella digital
     * @return Nombre del algoritmo de huella digital */
    public static String getDigestAlgorithmName(final String pseudoName) {
        final String upperPseudoName = pseudoName.toUpperCase();
        if (upperPseudoName.equals("SHA")  //$NON-NLS-1$
            || upperPseudoName.equals("http://www.w3.org/2000/09/xmldsig#sha1".toUpperCase()) //$NON-NLS-1$
            || upperPseudoName.equals("1.3.14.3.2.26") //$NON-NLS-1$
            || upperPseudoName.startsWith("SHA1") //$NON-NLS-1$
            || upperPseudoName.startsWith("SHA-1")) //$NON-NLS-1$            
        {
            return "SHA1"; //$NON-NLS-1$
        }

        if (upperPseudoName.equals("http://www.w3.org/2001/04/xmlenc#sha256".toUpperCase())  //$NON-NLS-1$
            || upperPseudoName.equals("2.16.840.1.101.3.4.2.1") //$NON-NLS-1$
            || upperPseudoName.startsWith("SHA256") //$NON-NLS-1$
            || upperPseudoName.startsWith("SHA-256")) { //$NON-NLS-1$
            return "SHA-256"; //$NON-NLS-1$
        }

        if (upperPseudoName.startsWith("SHA384") //$NON-NLS-1$
            || upperPseudoName.equals("2.16.840.1.101.3.4.2.2") //$NON-NLS-1$
            || upperPseudoName.startsWith("SHA-384")) { //$NON-NLS-1$
            return "SHA-384"; //$NON-NLS-1$
        }

        if (upperPseudoName.equals("http://www.w3.org/2001/04/xmlenc#sha512".toUpperCase())  //$NON-NLS-1$
            || upperPseudoName.equals("2.16.840.1.101.3.4.2.3") //$NON-NLS-1$
            || upperPseudoName.startsWith("SHA512") //$NON-NLS-1$
            || upperPseudoName.startsWith("SHA-512")) { //$NON-NLS-1$
            return "SHA-512"; //$NON-NLS-1$
        }

        if (upperPseudoName.equals("http://www.w3.org/2001/04/xmlenc#ripemd160".toUpperCase())  //$NON-NLS-1$
            || upperPseudoName.startsWith("RIPEMD160") //$NON-NLS-1$
            || upperPseudoName.startsWith("RIPEMD-160")) { //$NON-NLS-1$
            return "RIPEMD160"; //$NON-NLS-1$
        }

        if (upperPseudoName.equals("MD5") //$NON-NLS-1$
            || upperPseudoName.equals("1.2.840.113549.2.5") //$NON-NLS-1$
            || upperPseudoName.startsWith("MD5")) { //$NON-NLS-1$
            return "MD5"; //$NON-NLS-1$
        }

        if (upperPseudoName.equals("MD2")  //$NON-NLS-1$
            || upperPseudoName.equals("1.2.840.113549.2.2") //$NON-NLS-1$
            || upperPseudoName.startsWith("MD2")) { //$NON-NLS-1$
            return "MD2"; //$NON-NLS-1$
        }

        throw new IllegalArgumentException("Algoritmo de huella digital no soportado: " + pseudoName); //$NON-NLS-1$
    }

}
