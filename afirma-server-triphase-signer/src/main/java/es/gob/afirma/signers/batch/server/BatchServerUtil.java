/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

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
import es.gob.afirma.signers.batch.json.TriphaseDataParser;

final class BatchServerUtil {

	private BatchServerUtil() {
		// No instanciable
	}

	static TriphaseData getTriphaseData(final byte[] triphaseDataAsUrlSafeBase64) throws IOException {
		return TriphaseData.parser(
			Base64.decode(triphaseDataAsUrlSafeBase64, 0, triphaseDataAsUrlSafeBase64.length, true)
		);
	}

	static byte[] getSignBatchConfig(final byte[] batchAsUrlSafeBase64) throws IOException {
		if (batchAsUrlSafeBase64 == null) {
			throw new IllegalArgumentException(
				"La definicion de lote no puede ser nula" //$NON-NLS-1$
			);
		}
		final byte[] batch = Base64.isBase64(batchAsUrlSafeBase64) ?
			Base64.decode(batchAsUrlSafeBase64, 0, batchAsUrlSafeBase64.length, true) :
				batchAsUrlSafeBase64;

		return batch;
	}

	static TriphaseData getTriphaseDataFromJSON(final byte[] triphaseDataAsUrlSafeBase64) throws IOException {
		return TriphaseDataParser.parseFromJSON(
			Base64.decode(triphaseDataAsUrlSafeBase64, 0, triphaseDataAsUrlSafeBase64.length, true)
		);
	}

	static X509Certificate[] getCertificates(final String certListUrlSafeBase64) throws CertificateException,
	                                                                                    IOException {
		if (certListUrlSafeBase64 == null) {
			throw new IllegalArgumentException(
				"La lista de certificados no puede ser nula" //$NON-NLS-1$
			);
		}

		final String[] certs = certListUrlSafeBase64.split(";"); //$NON-NLS-1$
		final CertificateFactory cf = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$
		final List<X509Certificate> ret = new ArrayList<>(certs.length);
		for (final String cert : certs) {
			ret.add(
					(X509Certificate) cf.generateCertificate(
							new ByteArrayInputStream(Base64.decode(cert, true))
							)
					);
		}
		return ret.toArray(new X509Certificate[0]);
	}
}
