package es.gob.afirma.crypto.handwritten;

import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.envelopers.cms.AOCMSEnveloper;

/** Funciones criptogr&aacute;ficas b&aacute;sicas de utilidad.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SimpleCryptoHelper {

	private SimpleCryptoHelper() {
		// No instanciable
	}

	/** Calcula la huella digital de unos datos con el algoritmo especificado.
	 * @param data Datos de los que calcular la huella digital.
	 * @param algorithm Algoritmo de huella digital.
	 * @return Huella digital de los datos.
	 * @throws NoSuchAlgorithmException Si el algoritmo de huella no es v&aacute;lido. */
	public static byte[] messageDigest(final byte[] data, final String algorithm) throws NoSuchAlgorithmException {
		return MessageDigest.getInstance(algorithm).digest(data);
	}

	/** Cifra (sobre PKCS#7) los datos con una clave p&uacute;blica.
	 * @param data Datos a cifrar.
	 * @param cert Certificado del destinatario del cifrado.
	 * @return Datos cifrados.
	 * @throws AOException Si hay problemas generales en la creaci&oacute;n del sobre.
	 * @throws IOException Si hay problemas en el tratamiento de datos.
	 * @throws BadPaddingException Si hay problemas en el relleno criptogr&aacute;fico.
	 * @throws IllegalBlockSizeException Si no se soporta del modo de bloques del cifrado.
	 * @throws InvalidAlgorithmParameterException Si los par&aacute;metros del cifrado son incorectos.
	 * @throws NoSuchPaddingException Si no se soporta un modo de relleno criptogr&aacute;fico necesario.
	 * @throws NoSuchAlgorithmException Si no se soporta un algoritmo necesario.
	 * @throws InvalidKeyException Si la clave privada del certificado es incorrecta.
	 * @throws CertificateEncodingException Si el certificado proporcionado no es v&aacute;lido. */
	public static byte[] cipherData(final byte[] data, final X509Certificate cert) throws CertificateEncodingException,
	                                                                               InvalidKeyException,
	                                                                               NoSuchAlgorithmException,
	                                                                               NoSuchPaddingException,
	                                                                               InvalidAlgorithmParameterException,
	                                                                               IllegalBlockSizeException,
	                                                                               BadPaddingException,
	                                                                               IOException,
	                                                                               AOException {
        return new AOCMSEnveloper().createCMSEnvelopedData(
    		data,
    		null,
    		new AOCipherConfig(
	    		AOCipherAlgorithm.AES,
	    		null, // BlockMode (sin uso)
	    		null  // Padding (sin uso)
			),
    		new X509Certificate[] { cert },
    		null
		);
	}

}
