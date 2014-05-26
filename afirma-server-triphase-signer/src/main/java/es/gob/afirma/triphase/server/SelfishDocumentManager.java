package es.gob.afirma.triphase.server;

import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.misc.Base64;

final class SelfishDocumentManager implements DocumentManager {

	@Override
	public byte[] getDocument(final String id, final X509Certificate cert, final Properties config) throws IOException {
		return Base64.decode(
				// Por si acaso deshacemos un posible URL Safe
				id.replace("-", "+").replace("_", "/") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				);
	}

	@Override
	public String storeDocument(final String id, final X509Certificate cert, final byte[] data, final Properties config) throws IOException {
		return Base64.encodeBytes(data, Base64.URL_SAFE);
	}

}
