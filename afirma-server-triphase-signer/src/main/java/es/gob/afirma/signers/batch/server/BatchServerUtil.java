package es.gob.afirma.signers.batch.server;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.TriphaseData;

final class BatchServerUtil {

	private BatchServerUtil() {
		// No instanciable
	}

	static TriphaseData getTriphaseData(final String triphaseDataAsUrlSafeBase64) throws IOException {
		return TriphaseData.parser(
			Base64.decode(unDoUrlSafe(triphaseDataAsUrlSafeBase64))
		);
	}

	static byte[] getSignBatchConfig(final String xmlAsUrlSafeBase64) throws IOException {
		if (xmlAsUrlSafeBase64 == null) {
			throw new IllegalArgumentException(
				"La definicion de lote no puede ser nula" //$NON-NLS-1$
			);
		}
		final byte[] xml = Base64.isBase64(xmlAsUrlSafeBase64.getBytes()) ?
			Base64.decode(unDoUrlSafe(xmlAsUrlSafeBase64)) :
				xmlAsUrlSafeBase64.getBytes();

		return xml;
	}

	static X509Certificate[] getCertificates(final String certListUrlSafeBase64) throws CertificateException,
	                                                                                    IOException {
		if (certListUrlSafeBase64 == null) {
			throw new IllegalArgumentException(
				"La lista de certificados no puede ser nula" //$NON-NLS-1$
			);
		}

		final String[] certs = unDoUrlSafe(certListUrlSafeBase64).split(";"); //$NON-NLS-1$
		final CertificateFactory cf = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$
		final List<X509Certificate> ret = new ArrayList<X509Certificate>(certs.length);
		for (final String cert : certs) {
			ret.add(
				(X509Certificate) cf.generateCertificate(
					new ByteArrayInputStream(
						Base64.decode(
							unDoUrlSafe(cert)
						)
					)
				)
			);
		}
		return ret.toArray(new X509Certificate[0]);
	}

	private static String unDoUrlSafe(final String b64) {
		return b64.replace("-", "+").replace("_", "/"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	}
}
