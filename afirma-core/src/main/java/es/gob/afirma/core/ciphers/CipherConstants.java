/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.ciphers;

/** Constantes de cifrado (usando JCE).
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CipherConstants {

    private CipherConstants() {
        // No permitimos la instanciacion
    }

    /** Algoritmos de cifrado soportados. */
    public enum AOCipherAlgorithm {

        /** Advanced Encryption Standard (AES). */
        AES("AES", "Advanced Encryption Standard (AES)", false, true, "2.16.840.1.101.3.4.1"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        /** Blowfish. */
        BLOWFISH("Blowfish", "Blowfish", false, true, "1.3.6.1.4.1.3029.1.1.1"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        /** Data Encryption Standard (DES). */
        DES("DES", "Data Encryption Standard (DES)", false, true, "1.2.840.113549.3.6"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        /** Triple DES (3DES). */
        TRIPLEDES("DESede", "Triple DES (3DES)", false, true, "1.2.840.113549.3.7"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        /** Contrase&ntilde;a con MD5 y DES. */
        PBEWITHMD5ANDDES("PBEWithMD5AndDES", "Contrase\u00F1a con MD5 y DES", true, false, "1.2.840.113549.1.5.3"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        /** Contrase&ntilde;a con SHA1 y 3DES. */
        PBEWITHSHA1ANDDESEDE("PBEWithSHA1AndDESede", "Contrase\u00F1a con SHA1 y 3DES", true, false, "1.2.840.113549.1.5.10"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        /** Contrase&ntilde;a con SHA1 y RC2. */
        PBEWITHSHA1ANDRC2_40("PBEWithSHA1AndRC2_40", "Contrase\u00F1a con SHA1 y RC2", true, false, "1.2.840.113549.1.5.11"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        /** MAC HMACMD5 **/
        HMACMD5("HmacMD5", "HmacMD5", false, true, "1.3.6.1.5.5.8.1.1"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        /** MAC HMACSHA1 **/
        HMACSHA1("HmacSHA1", "HmacSHA1", false, true, "1.2.840.113549.2.7"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        /** MAC HMACSHA256 **/
        HMACSHA256("HmacSHA256", "HmacSHA256", false, true, "1.2.840.113549.2.9"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        /** MAC HMACSHA384 **/
        HMACSHA384("HmacSHA384", "HmacSHA384", false, true, "1.2.840.113549.2.10"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        /** MAC HMACSHA512 **/
        HMACSHA512("HmacSHA512", "HmacSHA512", false, true, "1.2.840.113549.2.11"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        /** Algoritmo de cifrado por defecto. */
        private static final AOCipherAlgorithm DEFAULT_CIPHER_ALGO = AES;

        /** Obtiene al algoritmo de cifrado por defecto.
         * @return Algoritmo de cifrado por defecto */
        public static AOCipherAlgorithm getDefault() {
            return DEFAULT_CIPHER_ALGO;
        }

        private AOCipherAlgorithm(final String n, final String d, final boolean p, final boolean k, final String oi) {
            this.name = n;
            this.description = d;
            this.password = p;
            this.key = k;
            this.oid = oi;
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
            return this.password;
        }

        /** Indica si el algoritmo de cifrado soporta claves.
         * @return <code>true</code> si el algoritmo soporta claves (en vez de
         *         contrase&ntilde;as), <code>false</code> en caso contrario */
        public boolean supportsKey() {
            return this.key;
        }

        /** Obtiene el nombre del algoritmo.
         * @return Nombre del algoritmo */
        public String getName() {
            return this.name;
        }

        @Override
        public String toString() {
            return this.description;
        }

        /** Obtiene el OID (<i>Object IDentifier</i>) ASN.1 del algoritmo.
         * @return OID del algoritmo */
        public String getOid() {
            return this.oid;
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
        PKCS5PADDING("PKCS5PADDING", "Relleno PKCS#5"), //$NON-NLS-1$ //$NON-NLS-2$
        /** Relleno ISO 10126. */
        ISO10126PADDING("ISO10126PADDING", "Relleno ISO-10126"), //$NON-NLS-1$ //$NON-NLS-2$
        /** Sin relleno. */
        NOPADDING("NOPADDING", "Sin Relleno"); //$NON-NLS-1$ //$NON-NLS-2$

        private String name;

        private String description;

        private AOCipherPadding(final String n, final String d) {
            this.name = n;
            this.description = d;
        }

        /** Obtiene el nombre del tipo de relleno.
         * @return Nombre del tipo de relleno (padding) */
        public String getName() {
            return this.name;
        }

        @Override
        public String toString() {
            return this.description;
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
        ECB("ECB", "Electronic CodeBook (ECB)"), //$NON-NLS-1$ //$NON-NLS-2$
        /** Cipher-Block Chaining (CBC). */
        CBC("CBC", "Cipher-Block Chaining (CBC)"), //$NON-NLS-1$ //$NON-NLS-2$
        /** Propagating Cipher-Block Chaining (PCBC). */
        PCBC("PCBC", "Propagating Cipher-Block Chaining (PCBC)"), //$NON-NLS-1$ //$NON-NLS-2$
        /** Counter (CTR). */
        CTR("CTR", "Counter (CTR)"), //$NON-NLS-1$ //$NON-NLS-2$
        /** Cipher FeedBack (CFB). */
        CFB("CFB", "Cipher FeedBack (CFB)"), //$NON-NLS-1$ //$NON-NLS-2$
        /** Output FeedBack (OFB). */
        OFB("OFB", "Output FeedBack (OFB)"); //$NON-NLS-1$ //$NON-NLS-2$

        private String name;
        private String description;

        private AOCipherBlockMode(final String n, final String d) {
            this.name = n;
            this.description = d;
        }

        /** Obtiene el nombre del modo de bloque.
         * @return Nombre del modo de bloque */
        public String getName() {
            return this.name;
        }

        @Override
        public String toString() {
            return this.description;
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


}
