/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batchV2;

import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.triphase.server.document.BatchDocumentManager;

/** Guarda firmas envi&aacute;ndolas a un servicio HTTP POST. */
public abstract class HttpPostStoreDocManager implements BatchDocumentManager {

	@Override
	public void init(final Properties config) {
		// No es necesario inicializar nada en esta clase
	}

	@Override
	public abstract byte[] getDocument(final String id, final X509Certificate[] certChain, final Properties config) throws IOException;

	@Override
	public String storeDocument(final String id, final X509Certificate[] certChain, final byte[] data, final Properties config) throws IOException {
		return Base64.encode(data, true);
	}

	@Override
	public void rollback(final String singleSignId) {
		Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Se ha pedido deshacer la firma " + singleSignId + ", pero no se puede hacer porque ya se ha finalizado el POST" //$NON-NLS-1$ //$NON-NLS-2$
			);
	}

}
