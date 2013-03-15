package es.gob.afirma.triphase.server;

import java.io.IOException;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.padestri.server.DocumentManager;

final class SelfishDocumentManager implements DocumentManager {

	@Override
	public byte[] getDocument(final String id) throws IOException {
		return Base64.decode(
			// Por si acaso deshacemos un posible URL Safe
			id.replace("-", "+").replace("_", "/") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		);
	}

	@Override
	public String storeDocument(final String id, final byte[] data) throws IOException {
		return Base64.encode(data);
	}

}
