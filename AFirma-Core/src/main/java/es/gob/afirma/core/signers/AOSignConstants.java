/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.core.signers;

/** Constantes relativas a las firmas digitales. */
public final class AOSignConstants {

    // ************************************************************
    // ************* FORMATOS DE FIRMA*****************************
    // ************************************************************

    /** Identificador de la firma CMS. */
    public static final String SIGN_FORMAT_CMS = "CMS/PKCS#7";

    /** Identificador de la firma CAdES. */
    public static final String SIGN_FORMAT_CADES = "CAdES";

    /** Identificador de la firma PKCS1 (RAW). */
    public static final String SIGN_FORMAT_PKCS1 = "NONE";

    /** Identificador de la firma XAdES Internally Detached. */
    public static final String SIGN_FORMAT_XADES_DETACHED = "XAdES Detached";

    /** Identificador de la firma XAdES Externally Detached. */
    public static final String SIGN_FORMAT_XADES_EXTERNALLY_DETACHED = "XAdES Externally Detached";

    /** Identificador de la firma XAdES Enveloped. */
    public static final String SIGN_FORMAT_XADES_ENVELOPED = "XAdES Enveloped";

    /** Identificador de la firma XAdES Enveloping. */
    public static final String SIGN_FORMAT_XADES_ENVELOPING = "XAdES Enveloping";

    /** Identificador de la firma XAdES por defecto. */
    public static final String SIGN_FORMAT_XADES = "XAdES";

    /** Identificador de la firma XMLDsig Detached. */
    public static final String SIGN_FORMAT_XMLDSIG_DETACHED = "XMLDSig Detached";

    /** Identificador de la firma XMLdSig Externally Detached. */
    public static final String SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED = "XMLdSig Externally Detached";

    /** Identificador de la firma XMLDsig Enveloped. */
    public static final String SIGN_FORMAT_XMLDSIG_ENVELOPED = "XMLDSig Enveloped";

    /** Identificador de la firma XMLDsig Enveloping. */
    public static final String SIGN_FORMAT_XMLDSIG_ENVELOPING = "XMLDSig Enveloping";

    /** Identificador de la firma XMLDSig (<i>XML Digital Signature</i>). */
    public static final String SIGN_FORMAT_XMLDSIG = "XMLDSig";

    /** Identificador de la firma OOXML (<i>Office Open XML</i>). */
    public static final String SIGN_FORMAT_OOXML = "OOXML (Office Open XML)";

    /** Identificador de la firma ODF (<i>Open Document Format</i>). */
    public static final String SIGN_FORMAT_ODF = "ODF (Open Document Format)";

    /** Identificador de la firma Adobe PDF. */
    public static final String SIGN_FORMAT_PDF = "Adobe PDF";

    /** Identificador de la firma SOAP. */
    public static final String SIGN_FORMAT_SOAP = "SOAP";

    /** Formato de firma por defecto. */
    public static final String DEFAULT_SIGN_FORMAT = SIGN_FORMAT_CMS;

    /** Identificador de la operaci&oacute;n de firma masiva. */
    public static final String MASSIVE_OPERATION_SIGN = "FIRMAR";

    /** Identificador de la operaci&oacute;n de cofirma masiva. */
    public static final String MASSIVE_OPERATION_COSIGN = "COFIRMAR";

    /** Identificador de la operaci&oacute;n de contrafirma masiva de todo el
     * &aacute;rbol de firma. */
    public static final String MASSIVE_OPERATION_COUNTERSIGN_TREE = "CONTRAFIRMAR_ARBOL";

    /** Identificador de la operaci&oacute;n de contrafirma masiva de nodos hoja
     * de firma. */
    public static final String MASSIVE_OPERATION_COUNTERSIGN_LEAFS = "CONTRAFIRMAR_HOJAS";

    /** Operaci&oacute;n masiva por defecto. */
    public static final String DEFAULT_MASSIVE_OPERATION = MASSIVE_OPERATION_SIGN;

    /** Envoltorio binario de tipo Data (datos envueltos en un envoltorio
     * PKCS#7). */
    public static final String CMS_CONTENTTYPE_DATA = "Data";

    /** Firma binaria de tipo Signed Data */
    public static final String CMS_CONTENTTYPE_SIGNEDDATA = "SignedData";

    /** Envoltorio binario de tipo Digest. */
    public static final String CMS_CONTENTTYPE_DIGESTEDDATA = "DigestedData";

    /** Envoltario binario de tipo AuthenticatedEnvelopedData. */
    public static final String CMS_CONTENTTYPE_COMPRESSEDDATA = "CompressedData";

    /** Firma binaria de tipo Encrypted Data */
    public static final String CMS_CONTENTTYPE_ENCRYPTEDDATA = "EncryptedData";

    /** Envoltorio binario de tipo Enveloped (sobre digital). */
    public static final String CMS_CONTENTTYPE_ENVELOPEDDATA = "EnvelopedData";

    /** Envoltorio binario de tipo Signed and Enveloped. */
    public static final String CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA = "SignedAndEnvelopedData";

    /** Envoltario binario de tipo AuthenticatedData. */
    public static final String CMS_CONTENTTYPE_AUTHENTICATEDDATA = "AuthenticatedData";

    /** Envoltario binario de tipo AuthenticatedEnvelopedData. */
    public static final String CMS_CONTENTTYPE_AUTHENVELOPEDDATA = "AuthEnvelopedData";

    /** Envoltorio binario por defecto. */
    public static final String DEFAULT_CMS_CONTENTTYPE = CMS_CONTENTTYPE_ENVELOPEDDATA;

    /** OID por defecto para los datos firmados. */
    public static final String DEFAULT_OID_TO_SIGN = "1.3.6.1.4.1.1466.115.121.1.40"; // Octect
                                                                                      // String    
    // ************************************************************
    // ************* OPCIONES DE MULTIFIRMA ***********************
    // ************************************************************

    /** Permite definir los objetivos para la contrafirma:
     * <ul>
     * <li>Signers: Contrafirma de firmantes concretos.</li>
     * <li>Nodes: Contrafirma de nodos de firma concretos.</li>
     * <li>Tree: Contrafirma de todo el &aacute;rbol de firma.</li>
     * <li>Leafs: Contrafirma de todos los nodos de firma.</li>
     * </ul> */
    public static enum CounterSignTarget {
        /** Contrafirma de firmantes concretos. */
        Signers,
        /** Contrafirma de nodos de firma concretos. */
        Nodes,
        /** Contrafirma de todo el &aacute;rbol de firma. */
        Tree,
        /** Contrafirma de todas las hojas del &aacute;rbol de firma. */
        Leafs
    }
    
    // ************************************************************
    // ************* ALGORITMOS DE FIRMA **************************
    // ************************************************************

    /** Algoritmo de firma SHA1withRSA. */
    public static final String SIGN_ALGORITHM_SHA1WITHRSA = "SHA1withRSA";

    /** Algoritmo de firma MD5withRSA. */
    public static final String SIGN_ALGORITHM_MD5WITHRSA = "MD5withRSA";

    /** Algoritmo de firma MD2withRSA. */
    public static final String SIGN_ALGORITHM_MD2WITHRSA = "MD2withRSA";

    /** Algoritmo de firma SHA256withRSA. */
    public static final String SIGN_ALGORITHM_SHA256WITHRSA = "SHA256withRSA";

    /** Algoritmo de firma SHA384withRSA. */
    public static final String SIGN_ALGORITHM_SHA384WITHRSA = "SHA384withRSA";

    /** Algoritmo de firma SHA512withRSA. */
    public static final String SIGN_ALGORITHM_SHA512WITHRSA = "SHA512withRSA";

    /** Algoritmo de firma RSA que no incluye la generaci&oacute;n de la huella
     * digital (NONEwithRSA). */
    public static final String SIGN_ALGORITHM_NONEWITHRSA = "NONEwithRSA";

    /** Algoritmo de firma SHA1withDSA. */
    public static final String SIGN_ALGORITHM_SHA1WITHDSA = "SHA1withDSA";

    /** Algoritmo de firma SHA1withECDSA. */
    public static final String SIGN_ALGORITHM_SHA1WITHECDSA = "SHA1withECDSA";

    /** Algoritmo de firma ECDSA que no incluye la generaci&oacute;n de la huella
     * digital (NONEwithEDSSA). */
    public static final String SIGN_ALGORITHM_NONEWITHECDSA = "NONEwithECDSA";

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
    public static final String SIGN_MODE_EXPLICIT = "explicit";

    /** Identificador del modo de firma Implicita (Los datos SI se incluyen en la
     * firma). */
    public static final String SIGN_MODE_IMPLICIT = "implicit";

    /** Modo de firma por defecto. */
    public static final String DEFAULT_SIGN_MODE = SIGN_MODE_EXPLICIT;

    private AOSignConstants() {}
    
    /** Obtiene el nombre de un algoritmo de huella digital a partir de una de
     * las variantes de este.
     * @param pseudoName
     *        Nombre o variante del nombre del algoritmo de huella digital
     * @return Nombre del algoritmo de huella digital */
    public static String getDigestAlgorithmName(final String pseudoName) {
        final String upperPseudoName = pseudoName.toUpperCase();
        if (upperPseudoName.equals("SHA") || upperPseudoName.equals("http://www.w3.org/2000/09/xmldsig#sha1".toUpperCase())
            || upperPseudoName.startsWith("SHA1")
            || upperPseudoName.startsWith("SHA-1")) {
            return "SHA1";
        }

        if (upperPseudoName.equals("http://www.w3.org/2001/04/xmlenc#sha256".toUpperCase()) || upperPseudoName.startsWith("SHA256")
            || upperPseudoName.startsWith("SHA-256")) {
            return "SHA-256";
        }

        if (upperPseudoName.startsWith("SHA384") || upperPseudoName.startsWith("SHA-384")) {
            return "SHA-384";
        }

        if (upperPseudoName.equals("http://www.w3.org/2001/04/xmlenc#sha512".toUpperCase()) || upperPseudoName.startsWith("SHA512")
            || upperPseudoName.startsWith("SHA-512")) {
            return "SHA-512";
        }

        if (upperPseudoName.equals("http://www.w3.org/2001/04/xmlenc#ripemd160".toUpperCase()) || upperPseudoName.startsWith("RIPEMD160")
            || upperPseudoName.startsWith("RIPEMD-160")) {
            return "RIPEMD160";
        }

        if (upperPseudoName.equals("MD5") || upperPseudoName.startsWith("MD5")) {
            return "MD5";
        }

        if (upperPseudoName.equals("MD2") || upperPseudoName.startsWith("MD2")) {
            return "MD2";
        }

        throw new IllegalArgumentException("Algoritmo de huella digital no soportado: " + pseudoName);
    }

}
