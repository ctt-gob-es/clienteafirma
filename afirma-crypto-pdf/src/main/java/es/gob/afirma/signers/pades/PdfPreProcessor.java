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

import com.lowagie.text.DocumentException;
import com.lowagie.text.Jpeg;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfContentByte;
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

	static void addImage(final PdfStamper stp, final Properties extraParams) throws IOException {
		if (extraParams == null || stp == null) {
			return;
		}

		final com.lowagie.text.Image image = getImage(extraParams.getProperty("image")); //$NON-NLS-1$

		if (image == null) {
			return;
		}

		final Rectangle rect = getPositionOnPage(extraParams, "image"); //$NON-NLS-1$

		if (rect == null) {
			return;
		}

		final int pageNum = 1;
		final PdfContentByte content = stp.getOverContent(pageNum);
		try {
			content.addImage(
				image,            // Image
				rect.getWidth(),  // Image width
				0,
				0,
				rect.getHeight(), // Image height
				rect.getLeft(),   // Lower left X position of the image
				rect.getBottom(), // Lower left Y position of the image
				false             // Inline
			);
		}
		catch (final DocumentException e) {
			throw new IOException("Error durante la insercion de la imagen en el PDF: " + e, e); //$NON-NLS-1$
		}
	}

    /** Devuelve la posici&oacute;n de la p&aacute;gina en donde debe agregarse el elemento
     * gr&aacute;fico indicado como prefijo. La medida de posicionamiento es el p&iacute;xel y se cuenta en
     * el eje horizontal de izquierda a derecha y en el vertical de abajo a arriba. */
    static Rectangle getPositionOnPage(final Properties extraParams, final String prefix) {
    	if (extraParams == null || prefix == null) {
    		LOGGER.severe("Se ha pedido una posicion para un elemento grafico nulo"); //$NON-NLS-1$
    		return null;
    	}
    	if (extraParams.getProperty(prefix + "PositionOnPageLowerLeftX") != null && //$NON-NLS-1$
    		extraParams.getProperty(prefix + "PositionOnPageLowerLeftY") != null && //$NON-NLS-1$
			extraParams.getProperty(prefix + "PositionOnPageUpperRightX") != null && //$NON-NLS-1$
			extraParams.getProperty(prefix + "PositionOnPageUpperRightY") != null //$NON-NLS-1$
		) {
	        try {
	            return new Rectangle(Integer.parseInt(extraParams.getProperty(prefix + "PositionOnPageLowerLeftX")), //$NON-NLS-1$
	                                 Integer.parseInt(extraParams.getProperty(prefix + "PositionOnPageLowerLeftY")), //$NON-NLS-1$
	                                 Integer.parseInt(extraParams.getProperty(prefix + "PositionOnPageUpperRightX")), //$NON-NLS-1$
	                                 Integer.parseInt(extraParams.getProperty(prefix + "PositionOnPageUpperRightY")) //$NON-NLS-1$
	            );
	        }
	        catch (final Exception e) {
	        	LOGGER.severe("Se ha indicado una posicion invalida para el elemento grafico '" + prefix + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
	        }
    	}
    	return null;
    }

    static com.lowagie.text.Image getImage(final String imagebase64Encoded) {
    	if (imagebase64Encoded == null || "".equals(imagebase64Encoded)) { //$NON-NLS-1$
    		return null;
    	}
    	final byte[] image;
    	try {
			image = Base64.decode(imagebase64Encoded);
		}
    	catch (final Exception e) {
    		LOGGER.severe("Se ha proporcionado una imagen de rubrica que no esta codificada en Base64: " + e); //$NON-NLS-1$
			return null;
		}
    	try {
			return new Jpeg(image);
		}
    	catch (final Exception e) {
    		LOGGER.info("Se ha proporcionado una imagen de rubrica que no esta codificada en JPEG: " + e); //$NON-NLS-1$
		}
    	return null;
    }

}
