package es.gob.afirma.test.keystores;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/** Tipo ASN.1 PKCS#1 <i>DigestInfo</i>.
 * 
 * <pre>
 *  DigestInfo::=SEQUENCE {
 *    digestAlgorithm  AlgorithmIdentifier,
 *    digest OCTET STRING
 *  }
 * </pre>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class DigestInfo {

    private static final String SHA1_NORMALIZED_ALGO_NAME = "SHA-1"; //$NON-NLS-1$

    private static final String SHA256_NORMALIZED_ALGO_NAME = "SHA-256"; //$NON-NLS-1$

    private static final String SHA384_NORMALIZED_ALGO_NAME = "SHA-384"; //$NON-NLS-1$

    private static final String SHA512_NORMALIZED_ALGO_NAME = "SHA-512"; //$NON-NLS-1$

    private static final String SHA1WITHRSA_NORMALIZED_ALGO_NAME = "SHA1withRSA"; //$NON-NLS-1$

    private static final String SHA256WITHRSA_NORMALIZED_ALGO_NAME = "SHA256withRSA"; //$NON-NLS-1$

    private static final String SHA384WITHRSA_NORMALIZED_ALGO_NAME = "SHA384withRSA"; //$NON-NLS-1$

    private static final String SHA512WITHRSA_NORMALIZED_ALGO_NAME = "SHA512withRSA"; //$NON-NLS-1$

    private static final byte[] SHA1_DIGESTINFO_HEADER = new byte[] {
            (byte) 0x30, (byte) 0x21, (byte) 0x30, (byte) 0x09, (byte) 0x06, (byte) 0x05, (byte) 0x2B, (byte) 0x0E, (byte) 0x03, (byte) 0x02,
            (byte) 0x1A, (byte) 0x05, (byte) 0x00, (byte) 0x04, (byte) 0x14
    };

    private static final byte[] SHA256_DIGESTINFO_HEADER = new byte[] {
            (byte) 0x30, (byte) 0x31, (byte) 0x30, (byte) 0x0D, (byte) 0x06, (byte) 0x09, (byte) 0x60, (byte) 0x86, (byte) 0x48, (byte) 0x01,
            (byte) 0x65, (byte) 0x03, (byte) 0x04, (byte) 0x02, (byte) 0x01, (byte) 0x05, (byte) 0x00, (byte) 0x04, (byte) 0x20
    };

    private static final byte[] SHA384_DIGESTINFO_HEADER = new byte[] {
            (byte) 0x30, (byte) 0x41, (byte) 0x30, (byte) 0x0D, (byte) 0x06, (byte) 0x09, (byte) 0x60, (byte) 0x86, (byte) 0x48, (byte) 0x01,
            (byte) 0x65, (byte) 0x03, (byte) 0x04, (byte) 0x02, (byte) 0x02, (byte) 0x05, (byte) 0x00, (byte) 0x04, (byte) 0x30
    };

    private static final byte[] SHA512_DIGESTINFO_HEADER = new byte[] {
            (byte) 0x30, (byte) 0x51, (byte) 0x30, (byte) 0x0D, (byte) 0x06, (byte) 0x09, (byte) 0x60, (byte) 0x86, (byte) 0x48, (byte) 0x01,
            (byte) 0x65, (byte) 0x03, (byte) 0x04, (byte) 0x02, (byte) 0x03, (byte) 0x05, (byte) 0x00, (byte) 0x04, (byte) 0x40
    };

    /** @return the sha1DigestinfoHeader */
    protected static byte[] getSha1DigestinfoHeader() {
        final byte[] out = new byte[SHA1_DIGESTINFO_HEADER.length];
        System.arraycopy(SHA1_DIGESTINFO_HEADER, 0, out, 0, SHA1_DIGESTINFO_HEADER.length);
        return out;
    }

    /** @return the sha256DigestinfoHeader */
    protected static byte[] getSha256DigestinfoHeader() {
        final byte[] out = new byte[SHA256_DIGESTINFO_HEADER.length];
        System.arraycopy(SHA256_DIGESTINFO_HEADER, 0, out, 0, SHA256_DIGESTINFO_HEADER.length);
        return out;
    }

    /** @return the sha384DigestinfoHeader */
    protected static byte[] getSha384DigestinfoHeader() {
        final byte[] out = new byte[SHA384_DIGESTINFO_HEADER.length];
        System.arraycopy(SHA384_DIGESTINFO_HEADER, 0, out, 0, SHA384_DIGESTINFO_HEADER.length);
        return out;
    }

    /** @return the sha512DigestinfoHeader */
    protected static byte[] getSha512DigestinfoHeader() {
        final byte[] out = new byte[SHA512_DIGESTINFO_HEADER.length];
        System.arraycopy(SHA512_DIGESTINFO_HEADER, 0, out, 0, SHA512_DIGESTINFO_HEADER.length);
        return out;
    }


    // TODO: Modificar este metodo y las llamadas al mismo para que reciba el algoritmo
    // de digest directamente

    /** Codifica una estructura DigestInfo.
     * @param signingAlgorithm Algoritmo de huella digital.
     * @param data Datos de los que obtener la estructura.
     * @param cryptoHelper Manejador de operaciones criptogr&aacute;ficas.
     * @return Estructura DigestInfo.
     * @throws IOException Cuando se produce algun error en la estrucura de la estructura. 
     * @throws NoSuchAlgorithmException */
    public static byte[] encode(final String signingAlgorithm, final byte[] data) throws IOException, NoSuchAlgorithmException {

        final String normalizedSignningAlgorithm = getNormalizedSigningAlgorithm(signingAlgorithm);

        final String digestAlgorithm = getDigestAlgorithm(normalizedSignningAlgorithm);

        final byte[] header = selectHeaderTemplate(digestAlgorithm);

        final byte[] md = MessageDigest.getInstance(digestAlgorithm).digest(data);

        final byte[] digestInfo = new byte[header.length + md.length];
        System.arraycopy(header, 0, digestInfo, 0, header.length);
        System.arraycopy(md, 0, digestInfo, header.length, md.length);

        return digestInfo;
    }

    /** Normaliza los nombres de algorimo de firma.
     * @param algorithm Nombre de algoritmo.
     * @return Nombre de algoritmo normalizado. */
    private static String getNormalizedSigningAlgorithm(final String algorithm) {
        if ("SHA1withRSA".equalsIgnoreCase(algorithm) || //$NON-NLS-1$
            "SHAwithRSA".equalsIgnoreCase(algorithm) ||  //$NON-NLS-1$
            "SHA-1withRSA".equalsIgnoreCase(algorithm) ||  //$NON-NLS-1$
            "SHA1withRSAEncryption".equalsIgnoreCase(algorithm) ||  //$NON-NLS-1$
            "SHA-1withRSAEncryption".equalsIgnoreCase(algorithm)) { //$NON-NLS-1$
            return SHA1WITHRSA_NORMALIZED_ALGO_NAME;
        }
        else if ("SHA256withRSA".equalsIgnoreCase(algorithm) || //$NON-NLS-1$
                 "SHA-256withRSA".equalsIgnoreCase(algorithm) ||  //$NON-NLS-1$
                 "SHA-256withRSAEncryption".equalsIgnoreCase(algorithm) ||  //$NON-NLS-1$
                 "SHA256withRSAEncryption".equalsIgnoreCase(algorithm)) { //$NON-NLS-1$
            return SHA256WITHRSA_NORMALIZED_ALGO_NAME;
        }
        else if ("SHA384withRSA".equalsIgnoreCase(algorithm) || //$NON-NLS-1$
                 "SHA-384withRSA".equalsIgnoreCase(algorithm) ||  //$NON-NLS-1$
                 "SHA-384withRSAEncryption".equalsIgnoreCase(algorithm) ||  //$NON-NLS-1$
                 "SHA384withRSAEncryption".equalsIgnoreCase(algorithm)) { //$NON-NLS-1$
            return SHA384WITHRSA_NORMALIZED_ALGO_NAME;
        }
        else if ("SHA512withRSA".equalsIgnoreCase(algorithm) || //$NON-NLS-1$
                 "SHA-512withRSA".equalsIgnoreCase(algorithm) ||  //$NON-NLS-1$
                 "SHA-512withRSAEncryption".equalsIgnoreCase(algorithm) ||  //$NON-NLS-1$
                 "SHA512withRSAEncryption".equalsIgnoreCase(algorithm)) { //$NON-NLS-1$
            return SHA512WITHRSA_NORMALIZED_ALGO_NAME;
        }
        return algorithm;
    }

    /** Selecciona una plantilla con la cabecera del DigestInfo para
     * un algoritmo concreto.
     * @param algorithm Algoritmo del que obtener la plantilla de cabecera.
     * @return Cabecera. */
    private static byte[] selectHeaderTemplate(final String algorithm) {
        if (SHA1_NORMALIZED_ALGO_NAME.equals(algorithm)) {
            return getSha1DigestinfoHeader();
        }
        else if (SHA256_NORMALIZED_ALGO_NAME.equals(algorithm)) {
            return getSha256DigestinfoHeader();
        }
        else if (SHA384_NORMALIZED_ALGO_NAME.equals(algorithm)) {
            return getSha384DigestinfoHeader();
        }
        else if (SHA512_NORMALIZED_ALGO_NAME.equals(algorithm)) {
            return getSha512DigestinfoHeader();
        }
        return new byte[0];
    }

    /** Obtiene el algoritmo de huella digital correspondiente a un algoritmo de firma
     * concreto.
     * @param signatureAlgorithm Algoritmo de firma.
     * @return Algoritmo de huella digital o la propia entrada si no se identific&oacute;. */
    private static String getDigestAlgorithm(final String signatureAlgorithm) {
        if (SHA1WITHRSA_NORMALIZED_ALGO_NAME.equals(signatureAlgorithm)) {
            return SHA1_NORMALIZED_ALGO_NAME;
        }
        else if (SHA256WITHRSA_NORMALIZED_ALGO_NAME.equals(signatureAlgorithm)) {
            return SHA256_NORMALIZED_ALGO_NAME;
        }
        else if (SHA384WITHRSA_NORMALIZED_ALGO_NAME.equals(signatureAlgorithm)) {
            return SHA384_NORMALIZED_ALGO_NAME;
        }
        else if (SHA512WITHRSA_NORMALIZED_ALGO_NAME.equals(signatureAlgorithm)) {
            return SHA512_NORMALIZED_ALGO_NAME;
        }
        return signatureAlgorithm;
    }
}