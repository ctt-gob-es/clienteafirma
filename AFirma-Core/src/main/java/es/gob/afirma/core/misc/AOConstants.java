/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.core.misc;

/** Constantes de utilidad para toda la aplicaci&oacute;n.
 * @version 0.3 */
public final class AOConstants {

    private AOConstants() {}

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

        private AOCipherAlgorithm(final String n, final String d, final boolean p, final boolean k, final String oi) {
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
        public static AOCipherAlgorithm getValueOf(final String algorithmName) {
            for (final AOCipherAlgorithm algorithm : AOCipherAlgorithm.values()) {
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

        private AOCipherPadding(final String n, final String d) {
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
        public static AOCipherPadding getValueOf(final String paddingName) {
            for (final AOCipherPadding padding : AOCipherPadding.values()) {
                if (padding.getName().equals(paddingName)) {
                    return padding;
                }
            }
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

        private AOCipherBlockMode(final String n, final String d) {
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
        public static AOCipherBlockMode getValueOf(final String blockModeName) {
            for (final AOCipherBlockMode blockMode : AOCipherBlockMode.values()) {
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
    
    /** Algoritmo Hash por defecto. */
    public static final String DEFAULT_DIGEST_ALGORITHM = "SHA-1";

    // ************************************************************
    // ************* ALMACENES DE CLAVES **************************
    // ************************************************************

    /** <i>KeyUsage</i> t&iacute;pico de un certificado v&aacute;lido para firmas
     * digitales. */
    public static final Boolean[] SIGN_CERT_USAGE = {
            null, // digitalSignature
            Boolean.TRUE, // nonRepudiation
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
