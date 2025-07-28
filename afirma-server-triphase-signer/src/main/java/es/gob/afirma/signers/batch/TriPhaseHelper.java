package es.gob.afirma.signers.batch;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.Signature;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Properties;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;
import es.gob.afirma.signers.batch.SingleSignConstants.DigestAlgorithm;
import es.gob.afirma.signers.batch.xml.SingleSign;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.triphase.server.ConfigManager;
import es.gob.afirma.triphase.signer.processors.CAdESASiCSTriPhasePreProcessor;
import es.gob.afirma.triphase.signer.processors.CAdESTriPhasePreProcessor;
import es.gob.afirma.triphase.signer.processors.FacturaETriPhasePreProcessor;
import es.gob.afirma.triphase.signer.processors.PAdESTriPhasePreProcessor;
import es.gob.afirma.triphase.signer.processors.Pkcs1TriPhasePreProcessor;
import es.gob.afirma.triphase.signer.processors.TriPhasePreProcessor;
import es.gob.afirma.triphase.signer.processors.XAdESASiCSTriPhasePreProcessor;
import es.gob.afirma.triphase.signer.processors.XAdESTriPhasePreProcessor;

public class TriPhaseHelper {

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
	 * N&uacute;mero de p&aacute;ginas por defecto de un PDF sobre las que
	 * comprobar si se ha producido un PDF Shadow Attack.
	 */
	private static final int DEFAULT_PAGES_TO_CHECK_PSA = 10;

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
	 * @param docBytes Documento firmado.
	 * @param cert Certificado que se declara haber usado en la prefirma.
	 * @param algorithm Algoritmo de hash de la firma.
	 * @param needVerifyPkcs1 Indica si debe verificarse que el PKCS#1 recibido
	 * se corresponde con el de los datos.
	 * @throws SecurityException Cuando el PKCS#1 de la firma no se generase con el
	 * certificado indicado o cuando no se pudiese comprobar.
	 * @throws IOException Cuando falla la decodificaci&oacute;n Base 64 de los datos.
	 */
	// TODO: Esto podr&iacute;a ser mas robusto en las firmas XAdES, en la que no se utiliza la
	// prefirma (par&aacute;metro PRE) para completar la firma, sino el parametro BASE. Habr&iacute;a
	// que extraer la prefirma del BASE en lugar de coger la que se pasa como par&aacute;metro (que
	// ya podr&iacute;a dejar de pasarse).
	public static void checkSignaturesIntegrity(final TriphaseData triphaseData, final byte[] docBytes, final X509Certificate cert,
			final DigestAlgorithm algorithm, final boolean needVerifyPkcs1)
			throws SecurityException, IOException {

		final String hmacSeed = ConfigManager.getHMacKey();
		if (hmacSeed == null) {
			return;
		}

		final SecretKeySpec key = new SecretKeySpec(hmacSeed.getBytes(DEFAULT_CHARSET), HMAC_ALGORITHM);
		for (final TriSign triSign : triphaseData.getTriSigns()) {

			verifyIntegrity(triSign, key, hmacSeed, cert);

			if (needVerifyPkcs1) {
				verifyPkcs1(triSign, cert.getPublicKey(), docBytes, algorithm);
			}
		}
	}

	/**
	 * Verificamos que la prefirma y el certificado proporcionados son los mismos que generamos
	 * en la prefirma.
	 */
	private static void verifyIntegrity(final TriSign triSign, final SecretKeySpec key, final String hmacSeed, final X509Certificate cert) {

		final String verificationHMacB64 = triSign.getProperty(TRIPHASE_PROP_HMAC);
		if (verificationHMacB64 == null) {
			throw new SecurityException("Alguna de las firmas no contenida el codigo de verificacion"); //$NON-NLS-1$
		}

		byte[] verificationHMac;
		try {
			verificationHMac = Base64.decode(verificationHMacB64);
		} catch (final IOException e) {
			throw new SecurityException("Alguna de las firmas contenida un codigo de verificacion no valido", e); //$NON-NLS-1$
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

		if (!Arrays.equals(hmac, verificationHMac)) {
			throw new SecurityException("Se ha detectado un error de integridad en los datos de firma"); //$NON-NLS-1$
		}
	}

	/**
     * Verifica que un PKCS#1 se pueda descifrar con la clave p&uacute;blica del certificado
     * asociado a la clave privada con la cual se gener&oacute;.
     * privada con la que en .
     * @param triSign Informaci&oacute;n de la firma realizada, incluyendo el PKCS#1.
     * @param publicKey Clave p&uacute;blica con la que validar la firma.
     * @param data Datos que se firmaron.
     * @param digestAlgorithm Algoritmo de huella usada en la firma.
     * @throws SecurityException Cuando no se proporciona un par&aacute;metro v&aacute;lido o
     * el PKCS#1 se gener&oacute; con una clave privada distinta a la esperada.
     */
	public static void verifyPkcs1(final TriSign triSign, final PublicKey publicKey, final byte[] data, final DigestAlgorithm digestAlgorithm) throws SecurityException {

		final String signatureValueB64 = triSign.getProperty(TRIPHASE_PROP_PKCS1);
		if (signatureValueB64 == null) {
			throw new SecurityException("No se ha proporcionado el PKCS#1 de la firma"); //$NON-NLS-1$
		}

		byte[] signatureValue;
		try {
			signatureValue = Base64.decode(signatureValueB64);
		}
		catch (final Exception e) {
			throw new SecurityException("El PKCS#1 de la firma no esta correctamente codificado", e); //$NON-NLS-1$
		}

		final String signAlgorithm = AOSignConstants.composeSignatureAlgorithmName(digestAlgorithm.getName(), publicKey.getAlgorithm());

		boolean valid = false;
		try {
			final Signature sigVerifier = Signature.getInstance(signAlgorithm);
			sigVerifier.initVerify(publicKey);
			sigVerifier.update(data);
			valid = sigVerifier.verify(signatureValue);
		}
		catch (final Exception e) {
			throw new SecurityException("Error al verificar el PKCS#1 de la firma", e); //$NON-NLS-1$
		}

		if (!valid) {
			throw new SecurityException("El PKCS#1 de firma obtenido no se genero con el certificado indicado"); //$NON-NLS-1$
		}

	}


	public static TriPhasePreProcessor getTriPhasePreProcessor(final SingleSign sSign) throws AOInvalidFormatException {
		if (sSign == null) {
			throw new IllegalArgumentException("La firma no puede ser nula"); //$NON-NLS-1$
		}
		switch(sSign.getSignFormat()) {
			case PADES:
				configurePdfShadowAttackParameters(sSign.getExtraParams());
				return new PAdESTriPhasePreProcessor();
			case CADES:
				return new CAdESTriPhasePreProcessor();
			case CADES_ASIC:
				return new CAdESASiCSTriPhasePreProcessor();
			case XADES:
				return new XAdESTriPhasePreProcessor();
			case XADES_ASIC:
				return new XAdESASiCSTriPhasePreProcessor();
			case FACTURAE:
				return new FacturaETriPhasePreProcessor();
			case PKCS1:
				return new Pkcs1TriPhasePreProcessor();
			default:
				throw new AOInvalidFormatException("Formato de firma no soportado: " + sSign.getSignFormat()); //$NON-NLS-1$
		}
	}

	private static void configurePdfShadowAttackParameters(final Properties extraParams) {
		if (!Boolean.parseBoolean(extraParams.getProperty(PdfExtraParams.ALLOW_SHADOW_ATTACK))) {
			// Evitamos explicitamente que se firmen documentos susceptibles de haber sufrido PDF
			// Shadow Attack en caso de que la aplicacion no indicase que hacer con ellos
			extraParams.setProperty(PdfExtraParams.ALLOW_SHADOW_ATTACK, Boolean.FALSE.toString());

			final int maxPagestoCheck = ConfigManager.getMaxPagesToCheckPSA();
			int pagesToCheck = DEFAULT_PAGES_TO_CHECK_PSA;
			if (extraParams.containsKey(PdfExtraParams.PAGES_TO_CHECK_PSA)) {
				final String pagesToCheckProp = extraParams.getProperty(PdfExtraParams.PAGES_TO_CHECK_PSA);
				if (PdfExtraParams.PAGES_TO_CHECK_PSA_VALUE_ALL.equalsIgnoreCase(pagesToCheckProp)) {
					pagesToCheck = Integer.MAX_VALUE;
				}
				else {
					try {
						pagesToCheck = Integer.parseInt(pagesToCheckProp);
					}
					catch (final Exception e) {
						pagesToCheck = DEFAULT_PAGES_TO_CHECK_PSA;
					}
				}
			}
			// Comprobaremos el menor numero de paginas posible, que sera el indicado por la aplicacion
			// (el por defecto si no se paso un valor) o el maximo establecido por el servicio. Si el
			// menor numero de paginas es 0, entonces se evita la comprobacion
			pagesToCheck = Math.min(pagesToCheck, maxPagestoCheck);
			if (pagesToCheck <= 0) {
				extraParams.setProperty(PdfExtraParams.ALLOW_SHADOW_ATTACK, Boolean.TRUE.toString());
			}
			else {
				extraParams.setProperty(PdfExtraParams.PAGES_TO_CHECK_PSA, Integer.toString(pagesToCheck));
			}
		}
	}
}
