package es.gob.afirma.signers.cadestri.client;

import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.CADES_FORMAT;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.HTTP_AND;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.HTTP_CGI;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.HTTP_EQUALS;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_ALGORITHM;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_CERT;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_CRYPTO_OPERATION;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_DOCID;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_EXTRA_PARAM;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_FORMAT;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_OPERATION;
import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PARAMETER_NAME_SESSION_DATA;

import java.io.IOException;
import java.net.URL;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.util.Properties;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.http.UrlHttpManager;

final class PostSigner {

	/** Identificador de la operaci&oacute;n de postfirma en servidor. */
	private static final String OPERATION_POSTSIGN = "post"; //$NON-NLS-1$

	private PostSigner() {
		// No instanciable
	}

	static byte[] postSign(final String algorithm,
			             final Certificate[] certChain,
			             final String cryptoOperation,
			             final String documentId,
			             final Properties extraParams,
			             final boolean needData,
			             final UrlHttpManager urlManager,
			             final URL signServerUrl,
			             final String preResultAsBase64) throws IOException, CertificateEncodingException {

		final StringBuffer urlBuffer = new StringBuffer();
		urlBuffer.append(signServerUrl).append(HTTP_CGI).
		append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_POSTSIGN).append(HTTP_AND).
		append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(cryptoOperation).append(HTTP_AND).
		append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(CADES_FORMAT).append(HTTP_AND).
		append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(algorithm).append(HTTP_AND).
		append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(Base64.encode(certChain[0].getEncoded(), true));

		if (extraParams.size() > 0) {
			urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS).
			append(ProtocolConstants.properties2Base64(extraParams));
		}

		urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_SESSION_DATA).append(HTTP_EQUALS).
		append(preResultAsBase64);

		if (needData) {
			urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(documentId);
		}

		return urlManager.readUrlByPost(urlBuffer.toString());
	}

}
