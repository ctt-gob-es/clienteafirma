package es.gob.afirma.triphase.server;

import java.io.IOException;
import java.util.Properties;

import es.gob.afirma.core.misc.Base64;

final class SelfishDocumentManager implements DocumentManager {

	@Override
	public byte[] getDocument(final String id, final Properties config) throws IOException {
		return Base64.decode(
				// Por si acaso deshacemos un posible URL Safe
				id.replace("-", "+").replace("_", "/") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				);
	}

	@Override
	public String storeDocument(final String id, final byte[] data, final Properties config) throws IOException {
		return Base64.encode(data);
	}

}
