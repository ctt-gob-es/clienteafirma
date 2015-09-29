package es.gob.afirma.signers.batch.server;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.SignBatch;
import es.gob.afirma.signers.batch.SignBatchConcurrent;
import es.gob.afirma.signers.batch.SignBatchSerial;

final class BatchServerUtil {

	private static final boolean CONCURRENT;
	static {
		final Properties p = new Properties();
		try {
			p.load(BatchServerUtil.class.getResourceAsStream("/config.properties")); //$NON-NLS-1$
		}
		catch(final Exception e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No se ha podido cargar la configuracion del proceso por lotes, se usara el modo no concurrente: " + e //$NON-NLS-1$
			);
		}
		CONCURRENT = Boolean.parseBoolean(p.getProperty("concurrentmode", "false")); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private BatchServerUtil() {
		// No instanciable
	}

	static TriphaseData getTriphaseData(final String triphaseDataAsUrlSafeBase64) throws IOException {
		return TriphaseData.parser(
			Base64.decode(unDoUrlSafe(triphaseDataAsUrlSafeBase64))
		);
	}

	static SignBatch getSignBatch(final String xmlAsUrlSafeBase64) throws IOException {
		if (xmlAsUrlSafeBase64 == null) {
			throw new IllegalArgumentException(
				"La definicion de lote no puede ser nula" //$NON-NLS-1$
			);
		}
		final byte[] xml = AOUtil.isBase64(xmlAsUrlSafeBase64.getBytes()) ?
			Base64.decode(unDoUrlSafe(xmlAsUrlSafeBase64)) :
				xmlAsUrlSafeBase64.getBytes();
		if (CONCURRENT) {
			return new SignBatchConcurrent(xml);
		}
		return new SignBatchSerial(xml);
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
