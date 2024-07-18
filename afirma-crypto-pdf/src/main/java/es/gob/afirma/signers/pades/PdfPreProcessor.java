/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.aowagie.text.Annotation;
import com.aowagie.text.DocumentException;
import com.aowagie.text.Image;
import com.aowagie.text.Jpeg;
import com.aowagie.text.Rectangle;
import com.aowagie.text.pdf.PdfContentByte;
import com.aowagie.text.pdf.PdfReader;
import com.aowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.pades.common.PdfExtraParams;

/** Utilidades para el manejo y modificaci&oacute;n de PDF antes de firmarlo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PdfPreProcessor {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

    /** Tama&ntilde;o m&aacute;ximo de ruta de un recurso. */
	private static final int MAX_PATH_SIZE = 500;

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

	static void attachFile(final Properties extraParams, final PdfStamper stp, final boolean secureMode) throws IOException {
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

			byte[] attachment = null;

			// Si no tenemos habilitado el modo seguro, permitiriamos que el recurso se cargue desde una
			// ruta externa
			if (!secureMode) {
				// Permitimos un tamano maximo de ruta para no interpretar como ruta
				// un base 64 grande
				if (b64Attachment.length() < MAX_PATH_SIZE) {
					try {
						final URI uri = AOUtil.createURI(b64Attachment);
						try (InputStream is = AOUtil.loadFile(uri)) {
							attachment = AOUtil.getDataFromInputStream(is);
						}
					} catch (final Exception e) {
						LOGGER.info("El parametro de adjunto no contiene una ruta valida a un recurso: " + e); //$NON-NLS-1$
						attachment = null;
					}
				}
			}

			// Si estaba configurado el modo seguro o el parametro de adjunto no era una ruta,
			// interpretamos que es un Base 64 con el propio adjunto
			if (attachment == null) {
				try {
					attachment = Base64.decode(b64Attachment);
				}
				catch(final IOException e) {
					LOGGER.warning("Se ha indicado un adjunto, pero no estaba en formato Base64, se ignorara : " + e); //$NON-NLS-1$
					return;
				}
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
		Image image;
		try {
			image = new Jpeg(jpegImage);
		}
		catch (final DocumentException e) {
			throw new IOException("Error durante la carga de la imagen JPG", e); //$NON-NLS-1$
		}
		addImage(image, width, height, left, bottom, pageNum, url, stp);
	}

	/** Sobreimpone una imagen JPEG en un documento PDF.
	 * @param image Imagen JPEG
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
	private static void addImage(final Image image,
			                     final int width,
			                     final int height,
			                     final int left,
			                     final int bottom,
			                     final int pageNum,
			                     final String url,
			                     final PdfStamper stp) throws IOException {
		final PdfContentByte content = stp.getOverContent(pageNum);
		try {
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

	/**
	 * Sobreimpone una imagen en un documento PDF.
	 * @param extraParams Datos de la imagen a a&ntilde;adir como <a href="doc-files/extraparams.html">par&aacute;metros
	 *                    adicionales</a>.
	 * @param stp Estampador de PDF, debe abrirse y cerrarse fuera de este m&eacute;todo.
	 * @param pdfReader Lector PDF, para obtener el n&uacute;mero de p&aacute;ginas del documento.
	 * @param secureMode Con {@code true} se habilita el modo seguro, con {@code false} se deshabilita. Al deshabilitar el
	 * modo seguro se permite la carga de imagenes a partir de una ruta local indicada a trav&eacute;s del par&aacute;metro
	 * {@code extraParams}.
	 * @throws IOException Cuando ocurren errores de entrada / salida.
	 */
	static void addImage(final Properties extraParams, final PdfStamper stp, final PdfReader pdfReader, final boolean secureMode) throws IOException {

		if (extraParams == null || stp == null) {
			return;
		}

		final String imageDataBase64 = extraParams.getProperty(PdfExtraParams.IMAGE);
		if (imageDataBase64 == null || imageDataBase64.length() < 1) {
			return;
		}

		final Image image = getImage(imageDataBase64, secureMode);
		if (image == null) {
			throw new IOException("No se pudo cargar la imagen para agregarla al PDF"); //$NON-NLS-1$
		}


		final Rectangle rect = PdfUtil.getPositionOnPage(extraParams, PdfExtraParams.IMAGE);

		if (rect == null) {
			return;
		}

		final String imagePage = extraParams.getProperty(PdfExtraParams.IMAGE_PAGE);
		if (imagePage == null) {
			return;
		}

		int pageNum;
		try {
			pageNum = Integer.parseInt(imagePage.trim());
		}
		catch(final NumberFormatException e) {
			throw new IOException(
				"Se ha indicado un numero de pagina con formato invalido para insertar la imagen (" + imagePage + "): " + e, e //$NON-NLS-1$ //$NON-NLS-2$
			);
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

    static com.aowagie.text.Image getImage(final String imageReference, final boolean secureMode)
    		throws IOException {
    	if (imageReference == null || imageReference.isEmpty()) {
    		return null;
    	}

    	byte[] image = null;

    	// Si no tenemos habilitado el modo seguro, permitiriamos que la imagen se cargue desde una
    	// ruta externa
    	if (!secureMode) {
    		// Permitimos un tamano maximo de ruta para no interpretar como ruta
    		// un base 64 grande
    		if (imageReference.length() < MAX_PATH_SIZE) {
    			try {
    				final URI uri = AOUtil.createURI(imageReference);
    				try (InputStream is = AOUtil.loadFile(uri)) {
    					image = AOUtil.getDataFromInputStream(is);
    				}
    			} catch (final Exception e) {
    				throw new IOException("El parametro de imagen no contiene una ruta valida a un recurso", e); //$NON-NLS-1$
    			}
    		}
    	}

    	// Si estaba configurado el modo seguro o el parametro de imagen no era una ruta,
    	// interpretamos que es un Base 64 con la propia imagen
    	if (image == null) {

    		try {
    			image = Base64.decode(imageReference);
    		}
    		catch (final Exception e) {
    			throw new IOException("Se ha proporcionado una imagen de rubrica que no esta codificada en Base64", e); //$NON-NLS-1$
    		}
    	}

    	// Si es posible en este entorno la imagen para hacerla asegurar su compatibilidad con PDF
    	byte[] normalizedImage;
    	try {
    		final Class<?> ImageUtilsClass = Class.forName("es.gob.afirma.ui.utils.ImageUtils"); //$NON-NLS-1$
    		final Method normalizeImageToPdfMethod = ImageUtilsClass.getMethod("normalizeImageToPdf", byte[].class); //$NON-NLS-1$
    		final Object normalizedImageObject = normalizeImageToPdfMethod.invoke(null, image);
    		normalizedImage = (byte[]) normalizedImageObject;
    	}
    	catch (final Throwable e) {
    		LOGGER.log(Level.WARNING, "No se pudo normalizar la imagen de rubrica. Se agregara tal cual: " + e); //$NON-NLS-1$
    		normalizedImage = image;
    	}

    	final Image jpgImage;
    	try {
			jpgImage = new Jpeg(normalizedImage);
		}
    	catch (final Exception e) {
    		throw new IOException("Se ha proporcionado una imagen de rubrica que no esta codificada en JPEG", e); //$NON-NLS-1$
		}
    	return jpgImage;
    }
}
