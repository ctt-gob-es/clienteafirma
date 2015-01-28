package es.gob.afirma.triphase.server;

import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.SignatureException;
import java.security.cert.X509Certificate;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.triphase.server.xades.XAdESTriPhaseSignerServerSide;
import es.gob.afirma.triphase.server.xades.XAdESTriPhaseSignerServerSide.Op;
import es.gob.afirma.triphase.server.xades.XmlPreSignException;
import es.gob.afirma.triphase.server.xades.XmlPreSignResult;

final class XAdESTriPhasePreProcessor implements TriPhasePreProcessor {

	/** Prefijo para cada prefirma. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Firma PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	/** Nombre de la propiedad que guarda la estructura b&aacute;sica con la composici&oacute;n
	 * de la firma trif&aacute;sica. */
	private static final String PROPERTY_NAME_SCHEMA_BASE = "BASE"; //$NON-NLS-1$

	/** Manejador de log. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	@Override
	public byte[] preProcessPreSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws IOException, AOException {

		LOGGER.info("Prefirma XAdES - Firma - INICIO"); //$NON-NLS-1$

		final byte[] presign = preProcessPre(data, algorithm, cert, extraParams, Op.SIGN);

		LOGGER.info("Prefirma XAdES - Firma - FIN"); //$NON-NLS-1$

		return presign;
	}

	@Override
	public byte[] preProcessPreCoSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams) throws IOException, AOException {

		LOGGER.info("Prefirma XAdES - Cofirma - INICIO"); //$NON-NLS-1$

		final byte[] presign = preProcessPre(data, algorithm, cert, extraParams, Op.COSIGN);

		LOGGER.info("Prefirma XAdES - Cofirma - FIN"); //$NON-NLS-1$

		return presign;
	}


	@Override
	public byte[] preProcessPreCounterSign(final byte[] sign, final String algorithm,
			final X509Certificate cert, final Properties extraParams,
			final CounterSignTarget targets) throws IOException, AOException {

		LOGGER.info("Prefirma XAdES - Contrafirma - INICIO"); //$NON-NLS-1$

		final byte[] presign = preProcessPre(sign, algorithm, cert, extraParams, Op.COUNTERSIGN);

		LOGGER.info("Prefirma XAdES - Contrafirma - FIN"); //$NON-NLS-1$

		return presign;
	}

	private static byte[] preProcessPre(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams,
			final Op op) throws IOException, AOException {

		final String algoUri = SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new AOException(
					"El formato de firma XAdES no soporta el algoritmo de firma '" + algorithm + "'"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		final XmlPreSignResult preSignature;
		try {
			preSignature = XAdESTriPhaseSignerServerSide.preSign(
					data,
					algorithm,
					new X509Certificate[] { cert },
					extraParams,
					op);
		}
		catch (final InvalidKeyException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final NoSuchAlgorithmException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final SignatureException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final SAXException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final ParserConfigurationException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final MarshalException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final XMLSignatureException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}
		catch (final XmlPreSignException e) {
			throw new AOException("Error en la prefirma XAdES", e); //$NON-NLS-1$
		}

		// Ahora pasamos al cliente los datos de la prefirma
		final TriphaseData triphaseData = new TriphaseData(
				AOSignConstants.SIGN_FORMAT_XADES, AOSignConstants.MASSIVE_OPERATION_SIGN);

		for (int i = 0; i < preSignature.getSignedInfos().size(); i++) {
			final Map<String, String> signConfig = new HashMap<String, String>();
			signConfig.put(PROPERTY_NAME_PRESIGN, Base64.encode(preSignature.getSignedInfos().get(i)));

			// Pasamos como datos de sesion el documento base en donde se realizan las sustituciones, pero solo lo
			// haremos en la primera prefirma ya que todos serian iguales
			if (i == 0) {
				signConfig.put(PROPERTY_NAME_SCHEMA_BASE, Base64.encode(preSignature.getXmlSign()));
			}
			triphaseData.addSignOperation(signConfig);
		}

		return triphaseData.toString().getBytes();
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams,
			final byte[] session) throws NoSuchAlgorithmException,
			AOException, IOException {

		LOGGER.info("Postfirma XAdES - Firma - INICIO"); //$NON-NLS-1$

		final byte[] signature = preProcessPost(session);

		LOGGER.info("Postfirma XAdES - Firma - FIN"); //$NON-NLS-1$

		return signature;
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams,
			final byte[] session) throws NoSuchAlgorithmException,
			AOException, IOException {

		LOGGER.info("Postfirma XAdES - Cofirma - INICIO"); //$NON-NLS-1$

		final byte[] signature = preProcessPost(session);

		LOGGER.info("Postfirma XAdES - Cofirma - FIN"); //$NON-NLS-1$

		return signature;
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams,
			final byte[] session,
			final CounterSignTarget targets) throws NoSuchAlgorithmException,
			AOException, IOException {

		LOGGER.info("Postfirma XAdES - Contrafirma - INICIO"); //$NON-NLS-1$

		final byte[] signature = preProcessPost(session);

		LOGGER.info("Postfirma XAdES - Contrafirma - FIN"); //$NON-NLS-1$

		return signature;
	}

	private static byte[] preProcessPost(final byte[] session) throws IOException, AOException {


		final TriphaseData triphaseData = TriphaseData.parser(session);

		if (triphaseData.getSignsCount() < 1) {
			LOGGER.severe("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
			throw new AOException("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
		}

		// El XML base se incluye como datos de sesion de la primera firma y solo de la primera
		String xmlBase = new String(Base64.decode(triphaseData.getSign(0).get(PROPERTY_NAME_SCHEMA_BASE)));

		// Sustituimos los valores dummy de la firmapor los reales
		for (int i = 0; i < triphaseData.getSignsCount(); i++) {
			final String pkcs1Base64 = triphaseData.getSign(i).get(PROPERTY_NAME_PKCS1_SIGN);
			if (pkcs1Base64 == null) {
				throw new IllegalArgumentException("La propiedades adicionales no contienen la firma PKCS#1"); //$NON-NLS-1$
			}

			xmlBase = xmlBase.replace(
					XAdESTriPhaseSignerServerSide.REPLACEMENT_STRING.replace(XAdESTriPhaseSignerServerSide.REPLACEMENT_CODE, Integer.toString(i)),
					pkcs1Base64.trim());
		}

		return xmlBase.getBytes();
	}

	private static final String URL_SHA1_RSA    = "http://www.w3.org/2000/09/xmldsig#rsa-sha1"; //$NON-NLS-1$
	private static final String URL_SHA256_RSA  = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256"; //$NON-NLS-1$
	private static final String URL_SHA384_RSA  = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha384"; //$NON-NLS-1$
	private static final String URL_SHA512_RSA  = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha512"; //$NON-NLS-1$

	/** URIs de los algoritmos de firma */
	public static final Map<String, String> SIGN_ALGOS_URI = new HashMap<String, String>() {
		private static final long serialVersionUID = 1897588397257599853L;
		{
			put(AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA, URL_SHA1_RSA);
			// Introducimos variantes para hacerlo mas robusto
			put("RSA", URL_SHA1_RSA); //$NON-NLS-1$
			put("SHA-1withRSA", URL_SHA1_RSA); //$NON-NLS-1$
			put("SHA1withRSAEncryption", URL_SHA1_RSA); //$NON-NLS-1$
			put("SHA-1withRSAEncryption", URL_SHA1_RSA); //$NON-NLS-1$
			put("SHAwithRSAEncryption", URL_SHA1_RSA); //$NON-NLS-1$
			put("SHAwithRSA", URL_SHA1_RSA); //$NON-NLS-1$

			put(AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, URL_SHA256_RSA);
			// Introducimos variantes para hacerlo mas robusto
			put("SHA-256withRSA", URL_SHA256_RSA); //$NON-NLS-1$
			put("SHA256withRSAEncryption", URL_SHA256_RSA); //$NON-NLS-1$
			put("SHA-256withRSAEncryption", URL_SHA256_RSA); //$NON-NLS-1$

			put(AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA, URL_SHA384_RSA);
			// Introducimos variantes para hacerlo mas robusto
			put("SHA-384withRSA", URL_SHA384_RSA); //$NON-NLS-1$
			put("SHA384withRSAEncryption", URL_SHA384_RSA); //$NON-NLS-1$
			put("SHA-384withRSAEncryption", URL_SHA384_RSA); //$NON-NLS-1$

			put(AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA, URL_SHA512_RSA);
			// Introducimos variantes para hacerlo mas robusto
			put("SHA-512withRSA", URL_SHA512_RSA); //$NON-NLS-1$
			put("SHA512withRSAEncryption", URL_SHA512_RSA); //$NON-NLS-1$
			put("SHA-512withRSAEncryption", URL_SHA512_RSA); //$NON-NLS-1$

		}
	};
}
