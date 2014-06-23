package es.gob.afirma.signfolder.server.proxy;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.UrlHttpManagerFactory;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signfolder.server.proxy.TriphaseSignDocumentRequest.TriphaseConfigDataBean;

public class TriSigner {

	/** Identificador de la operaci&oacute;n de prefirma en servidor. */
	private static final String OPERATION_PRESIGN = "pre"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de postfirma en servidor. */
	private static final String OPERATION_POSTSIGN = "post"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n trif&aacute;sica en la URL del servidor de firma. */
	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n criptogr&aacute;fica en la URL del servidor de firma. */
	private static final String PARAMETER_NAME_CRYPTO_OPERATION = "cop"; //$NON-NLS-1$

	private static final String HTTP_CGI = "?"; //$NON-NLS-1$
	private static final String HTTP_EQUALS = "="; //$NON-NLS-1$
	private static final String HTTP_AND = "&"; //$NON-NLS-1$

	// Parametros que necesitamos para la URL de las llamadas al servidor de firma
	private static final String PARAMETER_NAME_DOCID = "doc"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_ALGORITHM = "algo"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_FORMAT = "format"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_CERT = "cert"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_EXTRA_PARAM = "params"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_SESSION_DATA = "session"; //$NON-NLS-1$
	
	/** Indicador de finalizaci&oacute;n correcta de proceso. */
	private static final String SUCCESS = "OK"; //$NON-NLS-1$


	/**
	 * Prefirma el documento de una petici&oacute;n y muta la propia peticion para almacenar en ella
	 * el resultado.
	 * @param docReq Petici&oacute;n de firma de un documento.
	 * @param docManager Manejador que permite la descarga de un documento si es necesario. 
	 * @param signerCert Certificado de firma.
	 * @throws IOException Cuando no se puede obtener el documento para prefirmar.
	 * @throws AOException Cuando ocurre un error al generar la prefirma.
	 */
	public static void doPreSign(final TriphaseSignDocumentRequest docReq,
			final X509Certificate signerCert,
			final String signServiceUrl) throws IOException, AOException {

		// Empezamos la prefirma
		try {
			// Llamamos a una URL pasando como parametros los datos necesarios para
			// configurar la operacion:
			//  - Operacion trifasica (prefirma o postfirma)
			//  - Formato de firma
			//  - Algoritmo de firma a utilizar
			//  - Certificado de firma
			//  - Parametros extra de configuracion
			//  - Datos o identificador del documento a firmar
			final StringBuffer urlBuffer = new StringBuffer();
			urlBuffer.append(signServiceUrl).append(HTTP_CGI).
			append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_PRESIGN).append(HTTP_AND).
			append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(docReq.getCryptoOperation()).append(HTTP_AND).
			append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(normalizeSignatureFormat(docReq.getSignatureFormat())).append(HTTP_AND).
			append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(digestToSignatureAlgorithmName(docReq.getMessageDigestAlgorithm())).append(HTTP_AND).
			append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(Base64.encodeBytes(signerCert.getEncoded(), Base64.URL_SAFE)).append(HTTP_AND).
			append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(docReq.getContent());
			if (docReq.getParams() != null) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS).append(docReq.getParams());
			}
			
			docReq.setPartialResult(parseTriphaseResult(UrlHttpManagerFactory.getInstalledManager().readUrlByPost(urlBuffer.toString())));
			urlBuffer.setLength(0);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando el certificado del firmante: " + e, e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de prefirma al servidor: " + e, e); //$NON-NLS-1$
		}
	}

	/**
	 * Postfirma el documento de una petici&oacute;n.
	 * @param docReq Petici&oacute;n de firma de un documento.
	 * @param signerCert Certificado de firma.
	 * @return Properties en base64 con el resultado de la operacion de postfirma.
	 * @throws IOException Cuando no se puede obtener el documento para postfirmar.
	 * @throws AOException Cuando ocurre un error al generar la postfirma.
	 */
	public static void doPostSign(final TriphaseSignDocumentRequest docReq,
			final X509Certificate signerCert,
			final String signServiceUrl) throws IOException, AOException {
		
		final byte[] triSignFinalResult;
		try {
			final StringBuffer urlBuffer = new StringBuffer();
			urlBuffer.append(signServiceUrl).append(HTTP_CGI).
			append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_POSTSIGN).append(HTTP_AND).
			append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(docReq.getCryptoOperation()).append(HTTP_AND).
			append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(normalizeSignatureFormat(docReq.getSignatureFormat())).append(HTTP_AND).
			append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(digestToSignatureAlgorithmName(docReq.getMessageDigestAlgorithm())).append(HTTP_AND).
			append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(Base64.encodeBytes(signerCert.getEncoded(), Base64.URL_SAFE));

			if (docReq.getParams() != null && docReq.getParams().length() > 0) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS).append(docReq.getParams());
			}
			
			// Datos de sesion en forma de properies codificado en Base64 URL SAFE  
			final String sessionData = docReq.getPartialResult().toPropertiesBase64();
			if (sessionData != null) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_SESSION_DATA).append(HTTP_EQUALS)
				.append(sessionData);
			}
			
			if (docReq.getPartialResult().isNeedData() != null && docReq.getPartialResult().isNeedData().booleanValue()) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(docReq.getContent());
			}
			
			triSignFinalResult = UrlHttpManagerFactory.getInstalledManager().readUrlByPost(urlBuffer.toString());
			urlBuffer.setLength(0);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando el certificado del firmante: " + e, e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de postfirma al servidor: " + e, e); //$NON-NLS-1$
		}

		// Analizamos la respuesta del servidor
		final String stringTrimmedResult = new String(triSignFinalResult).trim();
		if (!stringTrimmedResult.startsWith(SUCCESS)) {
			throw new AOException("La firma trifasica no ha finalizado correctamente: " + new String(triSignFinalResult)); //$NON-NLS-1$
		}

		// Los datos no se devuelven, se quedan en el servidor
		try {
			docReq.setResult(Base64.decode(stringTrimmedResult.replace(SUCCESS + " NEWID=", ""), Base64.URL_SAFE)); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final IOException e) {
			Logger.getLogger("es.gob.afirma").warning("El resultado de NEWID del servidor no estaba en Base64: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			throw new AOException("El resultado devuelto por el servidor no es correcto", e); //$NON-NLS-1$
		}
	}

	/**
	 * Transforma el nombre de un algoritmo de huella digital en uno de firma que
	 * utilice ese mismo algoritmo de huella digital y un cifrado RSA.
	 * @param digestAlgorithm Algoritmo de huella digital.
	 * @return Nombre del algoritmo de firma.
	 */
	private static String digestToSignatureAlgorithmName(final String digestAlgorithm) {
		return digestAlgorithm.replace("-", "").toUpperCase() + "withRSA";  //$NON-NLS-1$ //$NON-NLS-2$//$NON-NLS-3$
	}
	
	/**
	 * Normalizamos el nombre del formato de firma. 
	 * @param format Formato de firma.
	 * @return Nombre de formato normalizado o el mismo formato de entrada si no se ha encontrado correspondencia.
	 */
	private static String normalizeSignatureFormat(final String format) {
		String normalizeFormat = format;
		if (format.equalsIgnoreCase("pdf") || format.equalsIgnoreCase("pades")) { //$NON-NLS-1$ //$NON-NLS-2$
			normalizeFormat = AOSignConstants.SIGN_FORMAT_PADES;
		} else if (format.equalsIgnoreCase("cades")) { //$NON-NLS-1$
			normalizeFormat = AOSignConstants.SIGN_FORMAT_CADES;
		} else if (format.toLowerCase().contains("xades")) { //$NON-NLS-1$
			normalizeFormat = AOSignConstants.SIGN_FORMAT_XADES;
		}
		return normalizeFormat;
	}
	
	private static TriphaseConfigDataBean parseTriphaseResult(byte[] triphaseResult) throws IOException {
		
		Properties resultProperties = new Properties();
		resultProperties.load(new ByteArrayInputStream(Base64.decode(triphaseResult, 0, triphaseResult.length, Base64.URL_SAFE)));

		TriphaseConfigDataBean triphaseConfig = new TriphaseConfigDataBean();
		
		int sc = 1;
		if (resultProperties.containsKey("SIGN_COUNT")) { //$NON-NLS-1$
			sc = Integer.parseInt(resultProperties.getProperty("SIGN_COUNT")); //$NON-NLS-1$
			triphaseConfig.setSignCount(new Integer(sc));
		}
		if (resultProperties.containsKey("NEED_PRE")) { //$NON-NLS-1$
			triphaseConfig.setNeedPreSign(Boolean.valueOf(resultProperties.getProperty("NEED_PRE"))); //$NON-NLS-1$
		}
		if (resultProperties.containsKey("NEED_DATA")) { //$NON-NLS-1$
			triphaseConfig.setNeedData(Boolean.valueOf(resultProperties.getProperty("NEED_DATA"))); //$NON-NLS-1$
		}
		for (int i = 0; i < sc; i++) {
			triphaseConfig.addSession(resultProperties.containsKey("SESSION." + i) ?  //$NON-NLS-1$
					Base64.encode(resultProperties.getProperty("SESSION." + i).getBytes()) :  //$NON-NLS-1$
						null);
			
			if (resultProperties.containsKey("PRE." + i)) { //$NON-NLS-1$
				triphaseConfig.addPreSign(resultProperties.getProperty("PRE." + i)); //$NON-NLS-1$
			}
			
			if (resultProperties.containsKey("PK1." + i)) { //$NON-NLS-1$
				triphaseConfig.addPk1(resultProperties.getProperty("PK1." + i)); //$NON-NLS-1$
			}
		}
		
		return triphaseConfig;
	}
}
