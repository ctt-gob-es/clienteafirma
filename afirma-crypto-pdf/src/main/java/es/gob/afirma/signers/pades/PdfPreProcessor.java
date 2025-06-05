/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pades;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.List;
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

	private static Boolean imageIOExist = null;

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

		final int totalPages = pdfReader.getNumberOfPages();
		final List<Integer> pages = PdfUtil.getImagePages(extraParams, totalPages);

		for (int i : pages) {
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
    	if (canNormalize()) {
    		normalizedImage = normalizeImage(image);
    	}
    	else {
    		normalizedImage = image;
    	}

    	Image jpgImage;
    	try {
			jpgImage = new Jpeg(normalizedImage);
		}
    	catch (final Exception e) {
    		throw new IOException("Se ha proporcionado una imagen de rubrica que no esta codificada en JPEG", e); //$NON-NLS-1$
		}
    	return jpgImage;
    }

	/**
	 * Normaliza la imagen para hacerla compatible con el PDF.
	 * @param bi Imagen que se desea codificar.
	 * @return Base 64 de la imagen JPEG.
	 * @throws IOException Cuando ocurre un error en la codificaci&oacute;n o
	 * transformaci&oacute;n de la imagen.
	 */
	static byte[] normalizeImage(final byte[] image) throws IOException {

		BufferedImage loadedImage;
		try (final InputStream is = new ByteArrayInputStream(image)) {
			loadedImage = javax.imageio.ImageIO.read(is);
		}

		byte[] imageEncoded;
		try (final ByteArrayOutputStream osImage = new ByteArrayOutputStream()) {
			// Eliminamos las transparencias de la imagen
			final BufferedImage opaqueImage = removeAlphaChannel(loadedImage);
			// Convertimos la imagen a JPEG
			if (!javax.imageio.ImageIO.write(opaqueImage, "jpg", osImage)) { //$NON-NLS-1$
				throw new IOException("No se ha podido convertir la imagen a JPEG"); //$NON-NLS-1$
			}
			// Codificamos la imagen
			imageEncoded = osImage.toByteArray();
		}
        catch (final Exception e) {
        	throw new IOException("No ha podido decodificar la imagen", e); //$NON-NLS-1$
        }
		return imageEncoded;
	}

	/**
	 * Elimina las transparencias de una imagen.
	 * @param img Imagen de la que eliminar las transparencias.
	 * @return Imagen sin transparencias.
	 */
	private static BufferedImage removeAlphaChannel(final BufferedImage img) {
	    if (!img.getColorModel().hasAlpha()) {
	        return img;
	    }

	    final BufferedImage target = createImage(img.getWidth(), img.getHeight(), false);
	    final Graphics2D g = target.createGraphics();
	    // g.setColor(new Color(color, false));
	    g.fillRect(0, 0, img.getWidth(), img.getHeight());
	    g.drawImage(img, 0, 0, null);
	    g.dispose();

	    return target;
	}

	/**
	 * Crea una imagen vac&iacute;a con un tama&ntilde;o determinado y el canal alfa o no
	 * seg&uacute;n se indique.
	 * @param width Anchura de a imagen.
	 * @param height Altura de la imagen.
	 * @param hasAlpha Indica si la imagen debe tener canal alfa o no.
	 * @return Imagen vac&iacute;a.
	 */
	private static BufferedImage createImage(final int width, final int height, final boolean hasAlpha) {
	    return new BufferedImage(width, height, hasAlpha ? BufferedImage.TYPE_INT_ARGB : BufferedImage.TYPE_INT_RGB);
	}

	/**
	 * Comprueba si se puede normalizar la imagen en el entorno actual.
	 * @return {@code true} s el entorno permite que se normalice la imagen, {@code false}
	 * en caso contrario.
	 */
	private static boolean canNormalize() {
		if (imageIOExist == null) {
			try {
				Class.forName("javax.imageio.ImageIO", false, PdfPreProcessor.class.getClassLoader()); //$NON-NLS-1$
				imageIOExist = Boolean.TRUE;
			}
			catch (final Exception e) {
				imageIOExist = Boolean.FALSE;
			}
		}
		return imageIOExist.booleanValue();
	}
}
