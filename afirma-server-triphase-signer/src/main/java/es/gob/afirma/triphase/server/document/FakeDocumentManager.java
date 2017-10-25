/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server.document;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;

/** Fachada simulada de gestor documental.
 * @author Tom&aacute;s Garc&iacute;a-;er&aacute;s */
public final class FakeDocumentManager implements DocumentManager {

	private static final String PDF_DOC = "TEST_PDF.pdf"; //$NON-NLS-1$
	private static Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** {@inheritDoc} */
	@Override
	public byte[] getDocument(final String id, final X509Certificate[] certChain, final Properties config) throws IOException {
		return AOUtil.getDataFromInputStream(this.getClass().getResourceAsStream(PDF_DOC));
	}

	/** {@inheritDoc} */
	@Override
	public String storeDocument(final String id, final X509Certificate[] certChain, final byte[] data, final Properties config) throws IOException {
		final File tempFile = File.createTempFile("fakeDocumentRetriever-" + id, ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
		try (
			final OutputStream fos = new FileOutputStream(tempFile);
		) {
			fos.write(data);
			fos.close();
		}
		catch (final IOException e) {
			LOGGER.severe("Error al almacenar los datos en el fichero '" + tempFile.getAbsolutePath() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			throw e;
		}
		LOGGER.info("Guardamos la firma generada en: " + tempFile.getAbsolutePath()); //$NON-NLS-1$
		return "id-fake-" + id; //$NON-NLS-1$
	}
}
