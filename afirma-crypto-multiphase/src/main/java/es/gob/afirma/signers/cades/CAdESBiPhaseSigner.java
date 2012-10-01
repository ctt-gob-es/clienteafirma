package es.gob.afirma.signers.cades;

import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.util.Date;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;

/** Firmador CAdES en tres fases independientes, adecuado para su uso en un entorno mixto cliente-servidor.
 * <p>&Uacute;nicamente se soportan firmas RSA.</p>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CAdESBiPhaseSigner {

    private static RSAPrivateKey generateRSAPrivateKey(final int keyLength) throws NoSuchAlgorithmException {
        final KeyPairGenerator keyGen = KeyPairGenerator.getInstance("RSA"); //$NON-NLS-1$
        keyGen.initialize(keyLength);
        final KeyPair keypair = keyGen.genKeyPair();
        return (RSAPrivateKey) keypair.getPrivate();
    }

    /** Genera una firma CAdES completa realizada con la cadena de certificados proporcionada (tomando como certificado
     * y clave privada del firmante la primera posici&oacute;n de esta) pero usando una clave privada ficticia.
     * La firma devuelta no ser&aacute; v&aacute;lida hasta que se reemplace los datos PKCS#1 internos de esta por
     * una firma PKCS#1 v1.5 realizada con la verdadera clave privada del firmante.
     * <p>Este m&eacute;todo realiza exclusivamente firmas impl&iacute;citas, en las que siempre se incluyen los datos
     * firmados dentro de la propia firma.</p>
     * @param data Datos que se desean firmar
     * @param algorithm Nombre del algoritmo de firma a usar
     * <p>Se aceptan los siguientes algoritmos en el par&aacute;metro <code>algorithm</code>:</p>
     * <ul>
     *  <li><i>SHA1withRSA</i></li>
     *  <li><i>MD5withRSA</i> (no recomendado por vulnerable)</li>
     *  <li><i>MD2withRSA</i> (no recomendado por vulnerable)</li>
     *  <li><i>SHA256withRSA</i></li>
     *  <li><i>SHA384withRSA</i></li>
     *  <li><i>SHA512withRSA</i></li>
     * </ul>
     * @param certChain Cadena de certificados del firmante
     * @param xParams Par&aacute;metros adicionales para la firma
     * <p>Se aceptan los siguientes valores en el par&aacute;metro <code>xParams</code>:</p>
     * <dl>
     *  <dt><b><i>mode</i></b></dt>
     *   <dd>
     *    Modo de firma a usar. El valor <code>explicit</code> indica que no se incluyen los datos firmados, sino una
     *    referencia a estos, mientras que el valor <code>implicit</code> indica que s&iacute; se incluir&aacute;n dentro de
     *    la propia firma los datos firmados
     *   </dd>
     *  <dt><b><i>policyIdentifier</i></b></dt>
     *   <dd>
     *    Identificadora de la pol&iacute;tica de firma. Debe ser un OID (o una URN de tipo OID) que identifique
     *    &uacute;nivocamente la pol&iacute;tica en formato ASN.1 procesable.
     *   </dd>
     *  <dt><b><i>policyIdentifierHash</i></b></dt>
     *   <dd>
     *    Huella digital del documento de pol&iacute;tica de firma (normalmente del mismo fichero en formato ASN.1 procesable).
     *    Si no se indica una huella digital y el par&aacute;metro <code>policyIdentifier</code> no es una URL accesible
     *    universalmente se usar&aacute; <code>0</code>, mientras que si no se indica una huella digital pero el par&aacute;metro
     *    <code>policyIdentifier</code> es una URL accesible universalmente, se descargara el fichero apuntado por la URL para calcular la huella
     *    digital <i>al vuelo</i>.
     *   </dd>
     *  <dt><b><i>policyIdentifierHashAlgorithm</i></b></dt>
     *   <dd>
     *    Algoritmo usado para el c&aacute;lculo de la huella digital indicada en el par&aacute;metro <code>policyIdentifierHash</code>.
     *    Es obligario indicarlo cuando se proporciona una huella digital distinta de <code>0</code>.
     *   </dd>
     *  <dt><b><i>policyQualifier</i></b></dt>
     *   <dd>
     *    URL que apunta al documento descriptivo de la pol&iacute;tica de firma (normalmente un documento PDF con una descripci&oacute;n textual).
     *   </dd>
     *  <dt><b><i>signingCertificateV2</i></b></dt>
     *   <dd>
     *    Debe establecerse a <code>true</code> si se desea usar la versi&oacute;n 2 del atributo
     *    <i>Signing Certificate</i> de CAdES. Si no se establece un valor para este par&aacute;metro
     *    o se hace a <code>false</code> se utilizara la versi&oacute;n 2
     *   </dd>
     * </dl>
     * @return Resultado de la primera fase de una firma en dos fases
     * @throws NoSuchAlgorithmException Si el algoritmo de firma proporcionado no est&aacute; soportado
     * @throws AOException EN caso de cualquier otro tipo de error */
    public static CAdESBiPhasePreSignResult preSign(final String algorithm,
                                             final byte[] data,
                                             final X509Certificate[] certChain,
                                             final Properties xParams) throws NoSuchAlgorithmException, AOException {

        if (certChain == null || certChain.length < 1) {
            throw new IllegalArgumentException("Es necesario proporcionar una cadena de certificados con al menos un elemento"); //$NON-NLS-1$
        }

        final PublicKey publicKey = certChain[0].getPublicKey();
        if (!(publicKey instanceof RSAPublicKey)) {
            throw new IllegalArgumentException("La clave privada del certificado es de un tipo no soportado: " + publicKey.getFormat()); //$NON-NLS-1$
        }

        final int keySize = ((RSAPublicKey)publicKey).getModulus().bitLength();
        final PrivateKey prk =  generateRSAPrivateKey(keySize);
        final Date signDate = new Date();
        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        String contentTypeOid = MimeHelper.DEFAULT_CONTENT_OID_DATA;
        String contentDescription = MimeHelper.DEFAULT_CONTENT_DESCRIPTION;
		if (data != null) {
			try {
				final MimeHelper mimeHelper = new MimeHelper(data);
				contentDescription = mimeHelper.getDescription();
				contentTypeOid = MimeHelper.transformMimeTypeToOid(mimeHelper.getMimeType());
			}
			catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"No se han podido cargar las librerias para identificar el tipo de dato firmado: " + e //$NON-NLS-1$
				);
			}
		}

        final byte[] preSign = CAdESTriPhaseSigner.preSign(
           AOSignConstants.getDigestAlgorithmName(algorithm), // Algoritmo de huella digital
           "explicit".equalsIgnoreCase(extraParams.getProperty("mode")) ? null : data, // Datos (o null si es explicita) //$NON-NLS-1$ //$NON-NLS-2$
           certChain, // Cadena de certificados del firmante
           new AdESPolicy(extraParams), // Pol&iacute;tica de firma
           !"false".equalsIgnoreCase(extraParams.getProperty("signingCertificateV2")),  //$NON-NLS-1$ //$NON-NLS-2$
           "explicit".equalsIgnoreCase(extraParams.getProperty("mode")) ? MessageDigest.getInstance(AOSignConstants.getDigestAlgorithmName(algorithm)).digest(data) : null, // Huella digital de los datos si es explicita o null si es implicita //$NON-NLS-1$ //$NON-NLS-2$
           signDate,
		   Boolean.parseBoolean(extraParams.getProperty("padesMode", "false")), //$NON-NLS-1$ //$NON-NLS-2$
		   contentTypeOid,
		   contentDescription
        );

        final byte[] pkcs1Sign = new AOPkcs1Signer().sign(
    	   preSign,
    	   algorithm,
    	   new PrivateKeyEntry(prk, new Certificate[0]),
           null
        );

        final byte[] postSign = CAdESTriPhaseSigner.postSign(
             AOSignConstants.getDigestAlgorithmName(algorithm),
             "explicit".equalsIgnoreCase(extraParams.getProperty("mode")) ? null : data, // Datos (o null si es explicita) //$NON-NLS-1$ //$NON-NLS-2$
             certChain,
             pkcs1Sign,
             preSign
        );

        return new CAdESBiPhasePreSignResult(
             postSign,
             indexOf(postSign, pkcs1Sign),
             pkcs1Sign.length,
             preSign
        );
    }

    /** Resultado de la primera fase (prefirma) de una firma bif&aacute;sica CAdES. */
    public static class CAdESBiPhasePreSignResult {
        private final byte[] signature;
        private final int indexOfPKCS1ToReplace;
        private final int lengthOfPKCS1ToReplace;
        private final byte[] pkcs1Data;

        CAdESBiPhasePreSignResult(final byte[] sign, final int p1SignIndex, final int p1SignLength, final byte[] p1Data) {
            this.signature = sign.clone();
            this.pkcs1Data = p1Data.clone();
            this.lengthOfPKCS1ToReplace = p1SignLength;
            this.indexOfPKCS1ToReplace = p1SignIndex;
        }

        /** Devuelve la firma CAdES completa realizada con la cadena de certificaci&oacute;n y la clave p&uacute;blica suministrada pero
         * con una clave privada impostada.
         * @return Firma CAdES (inv&aacute;lida, es necesario sustituir el valor PKCS#1) */
        public byte[] getSignature() {
            return this.signature.clone();
        }

        /** Devuelve la posici&oacute;n (utilizando un array de octetos) en la firma completa del valor PKCS#1 que es necesario sustituir en la firma obtenida mediante <code>getSignature()</code> para
         * convertir la firma CAdES en una firma v&aacute;lida.
         * @return Indice de la primera posici&oacute;n a sustituir por el PKCS#1 correcto */
        public int getIndexOfPKCS1ToReplace() {
            return this.indexOfPKCS1ToReplace;
        }

        /** Devuelve la cantidad de octetos que hay que reemplazar desde la posici&oacute;n obtebida mediante
         * <code>getLengthOfPKCS1ToReplace()</code> con una firma PKCS#1 v&aacute;lida.
         * Aunque la nueva firma PKCS#1 generada debe tener exactamente la misma longitud que la retirada (y por lo tanto podr&iacute;a
         * parecer que este m&eacute;todo es redundate), es &uacute;til conocer el tama–o para reobtener el &iacute;dice en caso de
         * modificaciones en la firma completa que muevan su popsici&oacute;n
         * @return N&uacute;mero de octetos que hay que reemplazar con una firma PKCS#1 v&aacute;lida */
        public int getLengthOfPKCS1ToReplace() {
            return this.lengthOfPKCS1ToReplace;
        }

        /** Devuelve los datos (atributos CAdES a firmar) que deben firmarse con PKCS#1 v1.5 y la verdadera clave privada del
         * firmante para reemplazar en el valor obtenido mediante <code>getSignature()</code>.
         * @return Atributos CAdES a firmar mediante PKCS#1 */
        public byte[] getPreSign() {
            return this.pkcs1Data;
        }
    }

    /** Obtiene la primera posici&oacute;n en la que se encuentra una determinada secuencia de octetos dentro de otra.
     * @param data Secuencia de octetos contenedora
     * @param pattern Secuencia de octetos a localizar
     * @return Primera posici&oacute;n en la que se encuentra una determinada secuencia de octetos dentro de otra o
     *         <code>-1</code> si no se encuentra */
    private static int indexOf(final byte[] data, final byte[] pattern) {
        final int[] failure = computeFailure(pattern);
        int j = 0;
        for (int i = 0; i < data.length; i++) {
            while (j > 0 && pattern[j] != data[i]) {
                j = failure[j - 1];
            }
            if (pattern[j] == data[i]) {
                j++;
            }
            if (j == pattern.length) {
                return i - pattern.length + 1;
            }
        }
        return -1;
    }

    private static int[] computeFailure(final byte[] pattern) {
        final int[] failure = new int[pattern.length];
        int j = 0;
        for (int i = 1; i < pattern.length; i++) {
            while (j>0 && pattern[j] != pattern[i]) {
                j = failure[j - 1];
            }
            if (pattern[j] == pattern[i]) {
                j++;
            }
            failure[i] = j;
        }
        return failure;
    }


}
