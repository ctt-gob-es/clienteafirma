/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.misc;

import java.util.HashMap;
import java.util.Map;

/** Constantes de utilidad para toda la aplicaci&oacute;n.
 * @version 0.3 */
public final class AOConstants {

    private AOConstants() {}

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

    /** Identificador del modo de firma Explicita (Los datos NO se incluyen en la
     * firma). */
    public static final String SIGN_MODE_EXPLICIT = "explicit";

    /** Identificador del modo de firma Implicita (Los datos SI se incluyen en la
     * firma). */
    public static final String SIGN_MODE_IMPLICIT = "implicit";

    /** Modo de firma por defecto. */
    public static final String DEFAULT_SIGN_MODE = SIGN_MODE_EXPLICIT;

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

    /** MimeType por defecto para los datos firmados. */
    public static final String DEFAULT_MIMETYPE = "application/octet-stream";

    /** OID por defecto para los datos firmados. */
    public static final String DEFAULT_OID_TO_SIGN = "1.3.6.1.4.1.1466.115.121.1.40"; // Octect
                                                                                      // String

    // ************************************************************
    // ************* ALGORITMOS DE CIFRADO***** *******************
    // ************************************************************

    /** Caracteres ASCII validos para la contrase&ntilde;a de cifrado. */
    public final static String ACCEPTED_CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";

    /** Algoritmos de cifrado soportados. */
    public enum AOCipherAlgorithm {

        /** Advanced Encryption Standard (AES). */
        AES("AES", "Advanced Encryption Standard (AES)", false, true, "2.16.840.1.101.3.4.1"),
        /** Alleged RC4. */
        ARCFOUR("ARCFOUR", "Alleged RC4", false, true, "1.2.840.113549.3.4"),
        /** Blowfish. */
        BLOWFISH("Blowfish", "Blowfish", false, true, "1.3.6.1.4.1.3029.1.1.1"),
        /** Data Encryption Standard (DES). */
        DES("DES", "Data Encryption Standard (DES)", false, true, "1.2.840.113549.3.6"),
        /** Triple DES (3DES). */
        TRIPLEDES("DESede", "Triple DES (3DES)", false, true, "1.2.840.113549.3.7"),
        /** RC2. */
        RC2("RC2", "RC2", false, true, "1.2.840.113549.3.3"),
        /** Contrase&ntilde;a con MD5 y DES. */
        PBEWITHMD5ANDDES("PBEWithMD5AndDES", "Contrase\u00F1a con MD5 y DES", true, false, "1.2.840.113549.1.5.3"),
        /** Contrase&ntilde;a con SHA1 y 3DES. */
        PBEWITHSHA1ANDDESEDE("PBEWithSHA1AndDESede", "Contrase\u00F1a con SHA1 y 3DES", true, false, "1.2.840.113549.1.5.10"),
        /** Contrase&ntilde;a con SHA1 y RC2. */
        PBEWITHSHA1ANDRC2_40("PBEWithSHA1AndRC2_40", "Contrase\u00F1a con SHA1 y RC2", true, false, "1.2.840.113549.1.5.11"),

        /** MAC HMACMD5 **/
        HMACMD5("HmacMD5", "HmacMD5", false, true, "1.3.6.1.5.5.8.1.1"),
        /** MAC HMACSHA1 **/
        HMACSHA1("HmacSHA1", "HmacSHA1", false, true, "1.2.840.113549.2.7"),
        /** MAC HMACSHA256 **/
        HMACSHA256("HmacSHA256", "HmacSHA256", false, true, "1.2.840.113549.2.9"),
        /** MAC HMACSHA384 **/
        HMACSHA384("HmacSHA384", "HmacSHA384", false, true, "1.2.840.113549.2.10"),
        /** MAC HMACSHA512 **/
        HMACSHA512("HmacSHA512", "HmacSHA512", false, true, "1.2.840.113549.2.11");

        /** Algoritmo de cifrado por defecto. */
        private static final AOCipherAlgorithm DEFAULT_CIPHER_ALGO = AES;

        /** Obtiene al algoritmo de cifrado por defecto.
         * @return Algoritmo de cifrado por defecto */
        public static AOCipherAlgorithm getDefault() {
            return DEFAULT_CIPHER_ALGO;
        }

        private AOCipherAlgorithm(String n, String d, boolean p, boolean k, String oi) {
            name = n;
            description = d;
            password = p;
            key = k;
            oid = oi;
        }

        private String name;
        private String description;
        private boolean password;
        private boolean key;
        private String oid;

        /** Indica si el algoritmo de cifrado soporta contrase&ntilde;as.
         * @return <code>true</code> si el algoritmo soporta contrase&ntilde;as
         *         (en vez de claves), <code>false</code> en caso contrario */
        public boolean supportsPassword() {
            return password;
        }

        /** Indica si el algoritmo de cifrado soporta claves.
         * @return <code>true</code> si el algoritmo soporta claves (en vez de
         *         contrase&ntilde;as), <code>false</code> en caso contrario */
        public boolean supportsKey() {
            return key;
        }

        /** Obtiene el nombre del algoritmo.
         * @return Nombre del algoritmo */
        public String getName() {
            return name;
        }

        @Override
        public String toString() {
            return description;
        }

        /** Obtiene el OID (<i>Object IDentifier</i>) ASN.1 del algoritmo.
         * @return OID del algoritmo */
        public String getOid() {
            return oid;
        }

        /** Recupera el algoritmo soportado cuyo nombre se indique. Si el
         * algoritmo indicado es nulo o no esta soportado, se devolver&aacute; <code>null</code>.
         * @param algorithmName
         *        Nombre del algoritmo.
         * @return Algoritmo solicitado. */
        public static AOCipherAlgorithm getValueOf(String algorithmName) {
            for (AOCipherAlgorithm algorithm : AOCipherAlgorithm.values()) {
                if (algorithm.getName().equalsIgnoreCase(algorithmName)) {
                    return algorithm;
                }
            }
            return null;
        }
    }

    /** Relleno (padding) para algorimos de cifrado. */
    public enum AOCipherPadding {
        /** Relleno PKCS#5. */
        PKCS5PADDING("PKCS5PADDING", "Relleno PKCS#5"),
        /** Relleno ISO 10126. */
        ISO10126PADDING("ISO10126PADDING", "Relleno ISO-10126"),
        /** Sin relleno. */
        NOPADDING("NOPADDING", "Sin Relleno");

        private String name;

        private String description;

        private AOCipherPadding(String n, String d) {
            name = n;
            description = d;
        }

        /** Obtiene el nombre del tipo de relleno.
         * @return Nombre del tipo de relleno (padding) */
        public String getName() {
            return name;
        }

        @Override
        public String toString() {
            return description;
        }

        /** Recupera el relleno soportado cuyo nombre se indique. Si el relleno
         * indicado es nulo o no est&aacute; soportado, se devolver&aacute; <code>null</code>.
         * @param paddingName
         *        Nombre del algoritmo de relleno.
         * @return Agoritmo de relleno solicitado. */
        public static AOCipherPadding getValueOf(String paddingName) {
            for (AOCipherPadding padding : AOCipherPadding.values())
                if (padding.getName().equals(paddingName)) return padding;
            return null;
        }
    }

    /** Modo de bloque para algoritmos de cifrado. */
    public enum AOCipherBlockMode {
        /** Electronic CodeBook (ECB). */
        ECB("ECB", "Electronic CodeBook (ECB)"),
        /** Cipher-Block Chaining (CBC). */
        CBC("CBC", "Cipher-Block Chaining (CBC)"),
        /** Propagating Cipher-Block Chaining (PCBC). */
        PCBC("PCBC", "Propagating Cipher-Block Chaining (PCBC)"),
        /** Counter (CTR). */
        CTR("CTR", "Counter (CTR)"),
        /** Cipher FeedBack (CFB). */
        CFB("CFB", "Cipher FeedBack (CFB)"),
        /** Output FeedBack (OFB). */
        OFB("OFB", "Output FeedBack (OFB)");

        private String name;
        private String description;

        private AOCipherBlockMode(String n, String d) {
            name = n;
            description = d;
        }

        /** Obtiene el nombre del modo de bloque.
         * @return Nombre del modo de bloque */
        public String getName() {
            return name;
        }

        @Override
        public String toString() {
            return description;
        }

        /** Recupera el modo de bloque soportado cuyo nombre se indique. Si el
         * modo de bloque indicado es nulo o no est&aacute; soportado, se
         * devolver&aacute; <code>null</code>.
         * @param blockModeName
         *        Nombre del modo de bloque.
         * @return Modo de bloque solicitado. */
        public static AOCipherBlockMode getValueOf(String blockModeName) {
            for (AOCipherBlockMode blockMode : AOCipherBlockMode.values()) {
                if (blockMode.getName().equals(blockModeName)) {
                    return blockMode;
                }
            }
            return null;
        }
    }

    /** Configuraci&oacute;n de cifrado por defecto. */
    public static final String DEFAULT_CIPHER_CONFIG = AOCipherAlgorithm.AES.name() + "/"
                                                       + AOCipherBlockMode.CBC.name()
                                                       + "/"
                                                       + AOCipherPadding.PKCS5PADDING.name();

    // ************************************************************
    // ************* MODOS DE GENERACION DE CLAVES ****************
    // ************************************************************

    /** Modo de generaci&oacute;n autom&aacute;tica de clave sim&eacute;trica
     * aleatoria. */
    public static final String KEY_MODE_GENERATEKEY = "GENERATEKEY";

    /** Modo de inserci&oacute;n directa de clave por parte del usuario. */
    public static final String KEY_MODE_USERINPUT = "USERINPUT";

    /** Modo de generaci&oacute;n de clave a partir de una password. */
    public static final String KEY_MODE_PASSWORD = "PASSWORD";

    /** Algoritmo de cifrado que se usa por defecto. */
    public static final String DEFAULT_KEY_MODE = KEY_MODE_GENERATEKEY;

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

    /** URIs de los algoritmos de firma */
    public static final Map<String, String> SIGN_ALGOS_URI = new HashMap<String, String>() {
        private static final long serialVersionUID = 1897588397257599853L;
        {
            put(SIGN_ALGORITHM_SHA1WITHRSA, "http://www.w3.org/2000/09/xmldsig#rsa-sha1");
            // Introducimos variantes para hacerlo mas robusto
            put("SHA-1withRSA", "http://www.w3.org/2000/09/xmldsig#rsa-sha1");
            put("SHA1withRSAEncryption", "http://www.w3.org/2000/09/xmldsig#rsa-sha1");
            put("SHA-1withRSAEncryption", "http://www.w3.org/2000/09/xmldsig#rsa-sha1");
            put("SHAwithRSAEncryption", "http://www.w3.org/2000/09/xmldsig#rsa-sha1");
            put("SHAwithRSA", "http://www.w3.org/2000/09/xmldsig#rsa-sha1");

            put(SIGN_ALGORITHM_MD5WITHRSA, "http://www.w3.org/2001/04/xmldsig-more#rsa-md5");

            put(SIGN_ALGORITHM_SHA256WITHRSA, "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256");
            // Introducimos variantes para hacerlo mas robusto
            put("SHA-256withRSA", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256");
            put("SHA256withRSAEncryption", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256");
            put("SHA-256withRSAEncryption", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256");

            put(SIGN_ALGORITHM_SHA384WITHRSA, "http://www.w3.org/2001/04/xmldsig-more#rsa-sha384");
            // Introducimos variantes para hacerlo mas robusto
            put("SHA-384withRSA", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha384");
            put("SHA384withRSAEncryption", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha384");
            put("SHA-384withRSAEncryption", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha384");

            put(SIGN_ALGORITHM_SHA512WITHRSA, "http://www.w3.org/2001/04/xmldsig-more#rsa-sha512");
            // Introducimos variantes para hacerlo mas robusto
            put("SHA-512withRSA", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha512");
            put("SHA512withRSAEncryption", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha512");
            put("SHA-512withRSAEncryption", "http://www.w3.org/2001/04/xmldsig-more#rsa-sha512");

        }
    };

    /** URIs de los algoritmos de hash. Las claves se encuentran en
     * min&uacute;sculas. */
    public static final Map<String, String> MESSAGEDIGEST_ALGOS_URI = new HashMap<String, String>() {
        private static final long serialVersionUID = 7994196143222514908L;
        {
            // Introducimos variantes para hacerlo mas robusto

            // SHA1
            put("sha1", "http://www.w3.org/2000/09/xmldsig#sha1");
            put("sha-1", "http://www.w3.org/2000/09/xmldsig#sha1");

            // MD5
            put("md5", "http://www.w3.org/2001/04/xmldsig-more#md5");

            // SHA256
            put("sha256", "http://www.w3.org/2001/04/xmlenc#sha256");
            put("sha-256", "http://www.w3.org/2001/04/xmlenc#sha256");

            // SHA384
            put("sha384", "http://www.w3.org/2001/04/xmldsig-more#sha384");
            put("sha-384", "http://www.w3.org/2001/04/xmldsig-more#sha384");

            // SHA512
            put("sha512", "http://www.w3.org/2001/04/xmlenc#sha512");
            put("sha-512", "http://www.w3.org/2001/04/xmlenc#sha512");
        }
    };

    /** Codificaci&oacute;n Base64 para firmas XMLDSig y XAdES. */
    public static final String BASE64_ENCODING = "http://www.w3.org/2000/09/xmldsig#base64";

    /** Algoritmo de firma por defecto. */
    public static final String DEFAULT_SIGN_ALGO = SIGN_ALGORITHM_SHA1WITHRSA;

    /** Algoritmo Hash por defecto. */
    public static final String DEFAULT_DIGEST_ALGORITHM = "SHA-1";

    // ************************************************************
    // ************* ALMACENES DE CLAVES **************************
    // ************************************************************

    /** Almacenes de claves y certificados soportados. */
    public enum AOKeyStore {
        /** Windows / Internet Explorer (CAPI, certificados de usuario). */
        WINDOWS("Windows / Internet Explorer", 0, "Windows-MY"),
        /** Apple Mac OS X / Safari Keychain. */
        APPLE("Mac OS X / Safari", 1, "KeychainStore"),
        /** Mozilla / Firefox (NSS v&iacute;a PKCS#11). */
        MOZILLA("Mozilla / Firefox", 2, "PKCS11"), // Via PKCS11/NSS
        /** PKCS#12. */
        PKCS12("PKCS#12 / PFX", 3, "PKCS12"),
        /** Java KeyStore (JKS). */
        JAVA("Java KeyStore / JKS", 4, "JKS"),
        /** PKCS#11. */
        PKCS11("PKCS#11", 5, "PKCS11"),
        /** PKCS#7 / X.509 en Base64. */
        SINGLE("PKCS#7 / X.509", 6, "PKCS7"),
        /** Mozilla / Firefox (NSS / PKCS#11, con m&oacute;dulos de seguridad
         * internos y externos unificados). */
        MOZ_UNI("Mozilla / Firefox (unificado)", 7, "PKCS11"),
        /** Java Cryptography Extension KeyStore (JCEKS). */
        JCEKS("Java Cryptography Extension KeyStore (JCEKS)", 8, "JCEKS"),
        /** Java KeyStore (JKS, distinguiendo entre may&uacute;sculas y
         * min&uacute;sculas). */
        JAVACE("Java KeyStore / JKS (Case Exact)", 9, "CaseExactJKS"),
        /** Windows / Internet Explorer (CAPI, certificados ra&iacute;z). */
        WINROOT("Windows / Internet Explorer (raiz)", 10, "Windows-ROOT"),
        /** Windows / INternet Explorer (CAPI, certificados de otras personas /
         * libreta de direcciones). */
        WINADDRESSBOOK("Windows / Internet Explorer (otras personas / libreta de direcciones)", 11, "Windows-ADDRESSBOOK"),
        /** Windows / Internet Explorer (CAPI, certificados CA intermedias). */
        WINCA("Windows / Internet Explorer (CA intermedias)", 12, "Windows-CA")
        // /** Windows (MY) con proveedor alternativo (JRE Deploy). */
        // WINDEPLOY("Windows / Internet Explorer (despliegue)", 13,
        // "WIExplorerMy")
        ;

        private AOKeyStore(String d, int o, String n) {
            description = d;
            ordinal = o;
            name = n;
        }

        @Override
        public String toString() {
            return getDescription();
        }

        private String description;
        private final int ordinal;
        private final String name;

        /** Obtiene el nombre del almac&eacute;n de claves y certificados.
         * @return Nombre del almac&eacute;n de claves y certificados */
        public String getName() {
            return name;
        }

        /** Obtiene el n&uacute;mero interno del n del almac&eacute;n de claves.
         * @return N&uacute;mero interno del almac&eacute;n de claves */
        public int getOrdinal() {
            return ordinal;
        }

        /** Obtiene la descripci&oacute;n del almac&eacute;n de claves.
         * @return Descripci&oacute;n del almac&eacute;n de claves */
        public String getDescription() {
            return description;
        }

    }

    /** <i>KeyUsage</i> t&iacute;pico de un certificado v&aacute;lido para firmas
     * digitales. */
    public static final Boolean[] SIGN_CERT_USAGE = {
            null, // digitalSignature
            true, // nonRepudiation
            null, // keyEncipherment
            null, // dataEncipherment
            null, // keyAgreement
            null, // keyCertSign
            null, // cRLSign
            null, // encipherOnly
            null
    // decipherOnly
            };

}
