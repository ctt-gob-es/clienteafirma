/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.pades;

import java.io.IOException;
import java.util.Properties;
import java.util.logging.Logger;

import com.lowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.misc.Base64;

final class PdfPreProcessor {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	private PdfPreProcessor() {
		// No permitimos la instancacion
	}

	static void attachFile(final Properties extraParams, final PdfStamper stp) throws IOException {
		if (extraParams == null) {
			return;
		}
		if (stp == null) {
			throw new IllegalArgumentException("No se puede adjuntar un fichero a un PdfStamper nulo"); //$NON-NLS-1$
		}
		// Contenido a adjuntar (en Base64)
		final String b64Attachment = extraParams.getProperty("attach"); //$NON-NLS-1$

		// Nombre que se pondra al fichero adjunto en el PDF
		final String attachmentFileName = extraParams.getProperty("attachFileName"); //$NON-NLS-1$

		// Descripcion del adjunto
		final String attachmentDescription = extraParams.getProperty("attachDescription"); //$NON-NLS-1$

		if (b64Attachment != null && attachmentFileName != null) {
			byte[] attachment = null;
			try {
				attachment = Base64.decode(b64Attachment);
			}
			catch(final IOException e) {
				LOGGER.warning("Se ha indicado un adjunto, pero no estaba en formato Base64, se ignorara : " + e); //$NON-NLS-1$
			}
			if (attachment != null) {
				stp.getWriter().addFileAttachment(attachmentDescription, attachment, null, attachmentFileName);
			}
		}

	}

}
