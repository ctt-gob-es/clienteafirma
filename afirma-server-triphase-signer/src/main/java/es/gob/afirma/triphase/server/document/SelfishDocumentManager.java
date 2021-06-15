/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server.document;

import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.misc.Base64;

/** Simulador de gestor documental, la referencia al documento es el propio documento en Base 64.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SelfishDocumentManager implements DocumentManager {

	/** Constructor vac&iacute;o.
	 * @param config No se usa. */
	public SelfishDocumentManager(final Properties config) {
		// No hacemos nada
	}

	@Override
	public byte[] getDocument(final String dataRef, final X509Certificate[] certChain, final Properties config) throws IOException {
		return Base64.decode(
			// Por si acaso deshacemos un posible URL Safe
			dataRef.replace("-", "+").replace("_", "/") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		);
	}

	@Override
	public String storeDocument(final String dataRef, final X509Certificate[] certChain, final byte[] data, final Properties config) throws IOException {
		return Base64.encode(data, true);
	}

}
