package es.gob.afirma.signers.batch;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.logging.Logger;

import javax.crypto.Cipher;
import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;
import es.gob.afirma.triphase.server.ConfigManager;

public class TriPhaseHelper {

	private static final Logger LOGGER = Logger.getLogger(TriPhaseHelper.class.getName());


	/** Algoritmo para el c&aacute;lculo de los valores de integridad. */
	private static final String HMAC_ALGORITHM = "HmacSHA256"; //$NON-NLS-1$

	/** Propiedad de la informacion trifasica en la que se almacenan las prefirmas. */
	private static final String TRIPHASE_PROP_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Propiedad de la informacion trifasica en la que se almacenan las prefirmas. */
	private static final String TRIPHASE_PROP_PKCS1 = "PK1"; //$NON-NLS-1$

	/** Propiedad de la informacion trifasica en la que se almacenan los c&oacute;digos
	 * de verificaci&oacute;n de integridad. */
	private static final String TRIPHASE_PROP_HMAC = "HMAC"; //$NON-NLS-1$

	/** Juego de caracteres usado internamente para la codificaci&oacute;n de textos. */
	private static final Charset DEFAULT_CHARSET = StandardCharsets.UTF_8;

	/**
	 * Agrega a la informaci&oacute;n de firma trif&aacute;sica un c&oacute;digo de verificaci&oacute;n
	 * con el que se podr&aacute; comprobar que la prefirma y el certificado no se han modificado entre
	 * las operaciones de prefirma y postfirma.
	 * @param triphaseData Informaci&oacute;n trif&aacute;sica de la operaci&oacute;n.
	 * @param cert Certificado utilizado para crear la prefirma.
	 * @throws NoSuchAlgorithmException Nunca se deber&iacute;a dar.
	 * @throws InvalidKeyException Cuando la clave para la generaci&oacute;n del c&oacute;digo de
	 * verificaci&oacute;n no sea v&aacute;lida.
	 * @throws CertificateEncodingException Cuando no se puede codificar el certificado.
	 * @throws IllegalStateException Nunca se deber&iacute;a dar.
	 */
	public static void addVerificationCodes(final TriphaseData triphaseData, final X509Certificate cert)
			throws NoSuchAlgorithmException, InvalidKeyException, CertificateEncodingException,
			IllegalStateException {

		final String hmacSeed = ConfigManager.getHMacKey();
		if (hmacSeed == null) {
			return;
		}

		final SecretKeySpec key = new SecretKeySpec(hmacSeed.getBytes(DEFAULT_CHARSET), HMAC_ALGORITHM);
		for (final TriSign triSign : triphaseData.getTriSigns()) {

			final String preSign = triSign.getProperty(TRIPHASE_PROP_PRESIGN);

			final Mac mac = Mac.getInstance(HMAC_ALGORITHM);
			mac.init(key);
			mac.update(preSign.getBytes(DEFAULT_CHARSET));
			mac.update(hmacSeed.getBytes(DEFAULT_CHARSET));
			mac.update(cert.getEncoded());

			final byte[] hmac = mac.doFinal();
			triSign.addProperty(TRIPHASE_PROP_HMAC, Base64.encode(hmac));
		}
	}

	/**
	 * Comprueba que la prefirma y el certificado estuviesen asociados a un mismo proceso de
	 * prefirma anterior validando el c&oacute;digo MAC de verificaci&oacute;n que acompa&ntilde;a
	 * al PKCS#1. Despu&eacute;s, comprueba que con la clave privada de ese certificado se generase
	 * ese PKCS#1.
	 * @param triphaseData Informaci&oacute;n de la firma.
	 * @param cert Certificado que se declara haber usado en la prefirma.
	 * @throws SecurityException Cuando el PKCS#1 de la firma no se generase con el
	 * certificado indicado o cuando no se pudiese comprobar.
	 * @throws IOException Cuando falla la decodificaci&oacute;n Base 64 de los datos.
	 */
	// TODO: Esto podr&iacute;a ser mas robusto en las firmas XAdES, en la que no se utiliza la
	// prefirma (par&aacute;metro PRE) para completar la firma, sino el parametro BASE. Habr&iacute;a
	// que extraer la prefirma del BASE en lugar de coger la que se pasa como par&aacute;metro (que
	// ya podr&iacute;a dejar de pasarse).
	public static void checkSignaturesIntegrity(final TriphaseData triphaseData, final X509Certificate cert)
			throws SecurityException, IOException {

		final String hmacSeed = ConfigManager.getHMacKey();
		if (hmacSeed == null) {
			return;
		}

		final SecretKeySpec key = new SecretKeySpec(hmacSeed.getBytes(DEFAULT_CHARSET), HMAC_ALGORITHM);
		for (final TriSign triSign : triphaseData.getTriSigns()) {

			final String verificationHMac = triSign.getProperty(TRIPHASE_PROP_HMAC);
			if (verificationHMac == null) {
				throw new SecurityException("Alguna de las firmas no contenida el codigo de verificacion"); //$NON-NLS-1$
			}

			final String preSign = triSign.getProperty(TRIPHASE_PROP_PRESIGN);

			byte[] hmac;
			try {
				final Mac mac = Mac.getInstance(HMAC_ALGORITHM);
				mac.init(key);
				mac.update(preSign.getBytes(DEFAULT_CHARSET));
				mac.update(hmacSeed.getBytes(DEFAULT_CHARSET));
				mac.update(cert.getEncoded());
				hmac = mac.doFinal();
			}
			catch (final Exception e) {
				throw new SecurityException("No se pudo completar la verificacion de integridad de la firma", e); //$NON-NLS-1$
			}

			if (!Arrays.equals(hmac, Base64.decode(verificationHMac))) {
				throw new SecurityException("Se ha detectado un error de integridad en los datos de firma"); //$NON-NLS-1$
			}

			final String signatureValue = triSign.getProperty(TRIPHASE_PROP_PKCS1);
			if (signatureValue == null) {
				throw new SecurityException("No se ha proporcionado el PKCS#1 de la firma"); //$NON-NLS-1$
			}

			verifyPkcs1(Base64.decode(signatureValue), cert.getPublicKey());
		}
	}

	/**
     * Verifica que un PKCS#1 se pueda descifrar con la clave p&uacute;blica del certificado
     * asociado a la clave privada con la cual se gener&oacute;.
     * privada con la que en .
     * @param signatureValue PKCS#1 de la firma.
     * @param publicKey Clave p&uacute;blica con la que validar la firma.
     * @throws SecurityException Cuando no se proporciona un par&aacute;metro v&aacute;lido o
     * el PKCS#1 se gener&oacute; con una clave privada distinta a la esperada.
     */
    public static void verifyPkcs1(final byte[] signatureValue, final PublicKey publicKey) throws SecurityException {
    	try {

    		//TODO: Probar y soportar algoritmos de cifrado de curva eliptica
    		if (!"RSA".equalsIgnoreCase(publicKey.getAlgorithm())) { //$NON-NLS-1$
    			LOGGER.warning("No se soporta la validacion del PKCS#1 con el algoritmo de cifrado asociado a la clave de firma utilizada"); //$NON-NLS-1$
    			return;
    		}

    		final Cipher cipher = Cipher.getInstance(publicKey.getAlgorithm());
    		cipher.init(Cipher.DECRYPT_MODE, publicKey);
    		cipher.doFinal(signatureValue);
    	}
    	catch (final Exception e) {
    		throw new SecurityException("El PKCS#1 de la firma no se ha generado con el certificado indicado", e); //$NON-NLS-1$
    	}
    }
}
