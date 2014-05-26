package es.gob.afirma.triphase.server;

import java.io.ByteArrayInputStream;
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
import es.gob.afirma.signers.xadestri.server.XAdESTriPhaseSignerServerSide;
import es.gob.afirma.signers.xadestri.server.XAdESTriPhaseSignerServerSide.Op;
import es.gob.afirma.signers.xadestri.server.XmlPreSignException;
import es.gob.afirma.signers.xadestri.server.XmlPreSignResult;

final class XAdESTriPhasePreProcessor implements TriPhasePreProcessor {

	/** Nombre de la propiedad que contiene el n&uacute;mero de firmas proporcionadas. */
	private static final String PROPERTY_NAME_SIGN_COUNT = "SIGN_COUNT"; //$NON-NLS-1$
	
	/** Prefijo para cada prefirma. */
	private static final String PROPERTY_NAME_PRESIGN_PREFIX = "PRE."; //$NON-NLS-1$
	
	/** Nombre de la propiedad de los sesi&oacute;n necesarios para completar la firma. */
	private static final String PROPERTY_NAME_SESSION_DATA_PREFIX = "SESSION."; //$NON-NLS-1$

	/** Car&aacute;cter por el que se sustituye a {@code #EQUAL} en los Properties anidados. */
	private static final String EQUAL_REPLACEMENT = "%%%"; //$NON-NLS-1$
	
	/** Car&aacute;cter igual. */
	private static final String EQUAL = "="; //$NON-NLS-1$
	
	/** Car&aacute;cter por el que se sustituye a {@code #CR} en los Properties anidados. */
	private static final String CR_REPLACEMENT = "&&&"; //$NON-NLS-1$
	
	/** Car&aacute;cter de salto de l&iacute;nea usado para los Properties. */
	private static final String CR = "\n"; //$NON-NLS-1$

	/** Firma PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN_PREFIX = "PK1."; //$NON-NLS-1$
	
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
		final StringBuilder urlParamBuilder = new StringBuilder();
		urlParamBuilder.append(PROPERTY_NAME_SIGN_COUNT).append(EQUAL).append(preSignature.getSignedInfos().size()).append(CR);
		
		for (int i = 0; i < preSignature.getSignedInfos().size(); i++) {
			urlParamBuilder.append(PROPERTY_NAME_PRESIGN_PREFIX).append(i).append(EQUAL).append(Base64.encode(preSignature.getSignedInfos().get(i))).append(CR);

			// Pasamos como datos de sesion el documento base en donde se realizan las sustituciones, pero solo lo
			// haremos en la primera prefirma ya que todos serian iguales 
			if (i == 0) {
				final StringBuilder sessionBuilder = new StringBuilder();
				sessionBuilder.append(PROPERTY_NAME_SCHEMA_BASE).append(EQUAL).append(Base64.encode(preSignature.getXmlSign()));
				
				urlParamBuilder.append(PROPERTY_NAME_SESSION_DATA_PREFIX).append(0).append(EQUAL).
				append(sessionBuilder.toString().replace(EQUAL, EQUAL_REPLACEMENT).replace(CR, CR_REPLACEMENT)).append(CR);
			}
		}

		return urlParamBuilder.toString().getBytes();
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams,
			final Properties sessionData) throws NoSuchAlgorithmException,
			AOException, IOException {

		LOGGER.info("Postfirma XAdES - Firma - INICIO"); //$NON-NLS-1$

		final byte[] signature = preProcessPost(sessionData);

		LOGGER.info("Postfirma XAdES - Firma - FIN"); //$NON-NLS-1$

		return signature;
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			final String algorithm,
			final X509Certificate cert,
			final Properties extraParams,
			final Properties sessionData) throws NoSuchAlgorithmException,
			AOException, IOException {

		LOGGER.info("Postfirma XAdES - Cofirma - INICIO"); //$NON-NLS-1$

		final byte[] signature = preProcessPost(sessionData);

		LOGGER.info("Postfirma XAdES - Cofirma - FIN"); //$NON-NLS-1$

		return signature;
	}


	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign, final String algorithm,
			final X509Certificate cert, final Properties extraParams, final Properties sessionData,
			final CounterSignTarget targets) throws NoSuchAlgorithmException,
			AOException, IOException {

		LOGGER.info("Postfirma XAdES - Contrafirma - INICIO"); //$NON-NLS-1$

		final byte[] signature = preProcessPost(sessionData);

		LOGGER.info("Postfirma XAdES - Contrafirma - FIN"); //$NON-NLS-1$

		return signature;
	}
	
	private static byte[] preProcessPost(final Properties sessionData) throws IOException, AOException {

		checkSessionProperties(sessionData);
		
		final int signCount = sessionData.containsKey(PROPERTY_NAME_SIGN_COUNT) ? Integer.parseInt(sessionData.getProperty(PROPERTY_NAME_SIGN_COUNT)) : 1;
		
		// El XML base se incluye como datos de sesion de la primera firma y solo de la primera
		final Properties configParams = new Properties();
		configParams.load(new ByteArrayInputStream(
				sessionData.getProperty(PROPERTY_NAME_SESSION_DATA_PREFIX + 0)
				.replace(CR_REPLACEMENT, CR).replace(EQUAL_REPLACEMENT, EQUAL).getBytes()
				));
		
		String xmlBase = new String(Base64.decode(configParams.getProperty(PROPERTY_NAME_SCHEMA_BASE)));
		
		for (int i = 0; i < signCount; i++) {
			final String pkcs1Base64Sign = sessionData.getProperty(PROPERTY_NAME_PKCS1_SIGN_PREFIX + i);
			if (pkcs1Base64Sign == null) {
				throw new IllegalArgumentException("La propiedades adicionales no contienen la firma PKCS#1"); //$NON-NLS-1$
			}
			
			xmlBase = xmlBase.replace(
					XAdESTriPhaseSignerServerSide.REPLACEMENT_STRING.replace(XAdESTriPhaseSignerServerSide.REPLACEMENT_CODE, Integer.toString(i)),
					pkcs1Base64Sign.trim());
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
	
	/**
	 * Comprueba que hayan proporcionado todos los datos de sesi&oacute;n necesarios.
	 * @param sessionData Properties con los datos de sesi&oacute;n necesarios para una postfirma. 
	 * @throws AOException Cuando se encuentra un error en los dados de sesi&oacute;n.
	 */
	private static void checkSessionProperties(final Properties sessionData) throws AOException {

		if (sessionData == null) {
			throw new AOException("No se han recibido los datos de sesion de firma necesarios"); //$NON-NLS-1$
		}
		try {
			if (!sessionData.containsKey(PROPERTY_NAME_SESSION_DATA_PREFIX + 0)) {
				throw new AOException("Los datos de sesion no contienen la configuracion global de la firma"); //$NON-NLS-1$
			}	
			if (sessionData.containsKey(PROPERTY_NAME_SIGN_COUNT)) {
				int count;
				try {
					count = Integer.parseInt(sessionData.getProperty(PROPERTY_NAME_SIGN_COUNT));
				}
				catch (Exception e) {
					throw new AOException("Se ha introducido como un numero de firmas el valor invalido " + sessionData.getProperty(PROPERTY_NAME_SIGN_COUNT), e); //$NON-NLS-1$
				}
				for (int i = 0; i < count; i++) {
					if (!sessionData.containsKey(PROPERTY_NAME_PKCS1_SIGN_PREFIX + i)) {
						throw new AOException("Los datos de sesion no contienen la contrafirma numero " + i); //$NON-NLS-1$
					}	
				}
			}
		} finally {
			LOGGER.severe("Datos de sesion contenidos:"); //$NON-NLS-1$
			for (String key : sessionData.keySet().toArray(new String[sessionData.size()])) {
				LOGGER.severe(key);
			}
			LOGGER.severe("---------------------------"); //$NON-NLS-1$
		}
	}
}
