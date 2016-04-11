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
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import com.aowagie.text.Annotation;
import com.aowagie.text.DocumentException;
import com.aowagie.text.Image;
import com.aowagie.text.Jpeg;
import com.aowagie.text.Rectangle;
import com.aowagie.text.pdf.PdfContentByte;
import com.aowagie.text.pdf.PdfReader;
import com.aowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.misc.Base64;

/** Utilidades para el manejo y modificaci&oacute;n de PDF antes de firmarlo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PdfPreProcessor {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

    private static final int LAST_PAGE = -1;
    private static final int ALL_PAGES = 0;
    private static final int FIRST_PAGE = 1;

	private PdfPreProcessor() {
		// No permitimos la instancacion
	}

	/** A&ntilde;ade campos adicionales al diccionario PDF.
	 * @param moreInfo Campos a a&ntilde;adir al diccionario PDF
	 * @param stp Estampador de PDF, debe abrirse y cerrarse fuera de este m&eacute;todo */
	public static void addMoreInfo(final Map<String, String> moreInfo, final PdfStamper stp) {
		if (moreInfo == null || moreInfo.isEmpty()) {
			return;
		}
		stp.setMoreInfo(moreInfo);
	}

	static void attachFile(final Properties extraParams, final PdfStamper stp) throws IOException {
		if (extraParams == null) {
			return;
		}
		if (stp == null) {
			throw new IllegalArgumentException("No se puede adjuntar un fichero a un PdfStamper nulo"); //$NON-NLS-1$
		}
		// Contenido a adjuntar (en Base64)
		final String b64Attachment = extraParams.getProperty(PdfExtraParams.ATTACH);

		// Nombre que se pondra al fichero adjunto en el PDF
		final String attachmentFileName = extraParams.getProperty(PdfExtraParams.ATTACH_FILENAME);

		// Descripcion del adjunto
		final String attachmentDescription = extraParams.getProperty(PdfExtraParams.ATTACH_DESCRIPTION);

		if (b64Attachment != null && attachmentFileName != null) {
			final byte[] attachment;
			try {
				attachment = Base64.decode(b64Attachment);
			}
			catch(final IOException e) {
				LOGGER.warning("Se ha indicado un adjunto, pero no estaba en formato Base64, se ignorara : " + e); //$NON-NLS-1$
				return;
			}
			stp.getWriter().addFileAttachment(attachmentDescription, attachment, null, attachmentFileName);
		}

	}

	/** Sobreimpone una imagen JPEG en un documento PDF.
	 * @param jpegImage Imagen JPEG
	 * @param width Ancho de la imagen
	 * @param height Alto de la imagen
	 * @param left Distancia de la imagen al borde izquiero de la p&aacute;gina del PDF
	 * @param bottom Distancia de la imagen al borde inferior de la p&aacute;gina del PDF
	 * @param pageNum N&uacute;mero de p&aacute;gina del PDF donde insertar la imagen
	 *                (la numeraci&oacute;n comienza en 1)
	 * @param url URL a la que enlazar&aacute; la imagen si queremos que esta sea un hiperv&iacute;nculo
	 *            (puede ser <code>null</code>)
	 * @param stp Estampador PDF de iText
	 * @throws IOException En caso de errores de entrada / salida */
	public static void addImage(final byte[] jpegImage,
			                     final int width,
			                     final int height,
			                     final int left,
			                     final int bottom,
			                     final int pageNum,
			                     final String url,
			                     final PdfStamper stp) throws IOException {
		final PdfContentByte content = stp.getOverContent(pageNum);
		try {
			final Image image = new Jpeg(jpegImage);
			if (url != null) {
				image.setAnnotation(new Annotation(0, 0, 0, 0, url));
			}
			content.addImage(
				image,  // Image
				width,  // Image width
				0,
				0,
				height, // Image height
				left,   // Lower left X position of the image
				bottom, // Lower left Y position of the image
				false   // Inline
			);
		}
		catch (final DocumentException e) {
			throw new IOException("Error durante la insercion de la imagen en el PDF: " + e, e); //$NON-NLS-1$
		}
	}

	/** Sobreimpone una imagen en un documento PDF.
	 * @param extraParams Datos de la imagen a a&ntilde;adir como <a href="doc-files/extraparams.html">par&aacute;metros adicionales</a>.
	 * @param stp Estampador de PDF, debe abrirse y cerrarse fuera de este m&eacute;todo.
	 * @param pdfReader Lector PDF, para obtener el n&uacute;mero de p&aacute;ginas del documento.
	 * @throws IOException Cuando ocurren errores de entrada / salida. */
	static void addImage(final Properties extraParams, final PdfStamper stp, final PdfReader pdfReader) throws IOException {

		if (extraParams == null || stp == null) {
			return;
		}

		final String imageDataBase64 = extraParams.getProperty(PdfExtraParams.IMAGE);
		if (imageDataBase64 == null || imageDataBase64.length() < 1) {
			return;
		}
		final byte[] image = Base64.decode(imageDataBase64);

		final Rectangle rect = getPositionOnPage(extraParams, PdfExtraParams.IMAGE);

		if (rect == null) {
			return;
		}

		final String imagePage = extraParams.getProperty(PdfExtraParams.IMAGE_PAGE);
		if (imagePage == null) {
			return;
		}

		int pageNum;
		try {
			pageNum = Integer.parseInt(imagePage);
		}
		catch(final NumberFormatException e) {
			throw new IOException("Se ha indicado un numero de pagina con formato invalido para insertar la imagen (" + imagePage + "): " + e, e); //$NON-NLS-1$ //$NON-NLS-2$
		}

		if (pageNum == LAST_PAGE) {
			pageNum = pdfReader.getNumberOfPages();
		}
		final int pageLimit;
		if (pageNum == ALL_PAGES) {
			pageNum = FIRST_PAGE;
			pageLimit = pdfReader.getNumberOfPages();
		}
		else {
			pageLimit = pageNum;
		}

		for (int i= pageNum; i<=pageLimit; i++) {
			addImage(
				image,
				(int) rect.getWidth(),
				(int) rect.getHeight(),
				(int) rect.getLeft(),
				(int) rect.getBottom(),
				i,
				null,
				stp
			);
		}

		LOGGER.info("Anadida imagen al PDF antes de la firma"); //$NON-NLS-1$
	}

    /** Devuelve la posici&oacute;n de la p&aacute;gina en donde debe agregarse el elemento
     * gr&aacute;fico indicado como prefijo. La medida de posicionamiento es el p&iacute;xel y se cuenta en
     * el eje horizontal de izquierda a derecha y en el vertical de abajo a arriba.
     * @param extraParams Definici&oacute;n de las coordenadas como conjunto de propiedades
     * @param prefix Prefijo de las propiedades de coordenada en el conjunto
     * @return Rect&aacute;ngulo que define una posici&oacute;n de un elemento en una p&aacute;gina del PDF */
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

    static com.aowagie.text.Image getImage(final String imagebase64Encoded) {
    	if (imagebase64Encoded == null || imagebase64Encoded.isEmpty()) {
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
