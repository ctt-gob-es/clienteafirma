/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signvalidation;

import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.pdmodel.interactive.annotation.PDAnnotation;
import org.apache.pdfbox.pdmodel.interactive.annotation.PDAnnotationWidget;
import org.apache.pdfbox.rendering.ImageType;
import org.apache.pdfbox.rendering.PDFRenderer;

import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cms.AOCMSSigner;
import es.gob.afirma.signers.odf.AOODFSigner;
import es.gob.afirma.signers.ooxml.AOOOXMLSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pades.PdfExtraParams;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xmldsig.AOXMLDSigSigner;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.SignValidity.VALIDITY_ERROR;

/** Utilidad para el an&aacute;lisis de ficheros de datos.
 * @author Carlos Gamuci. */
public final class DataAnalizerUtil {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

    private DataAnalizerUtil() {
        // No permitimos la instanciacion
    }

    /** Identifica si los datos proporcionados son un certificado X509, devolvi&eacute;ndolo en ese caso.
     * @param data Datos a analizar.
     * @return Certificado X509 si los datos lo eran.
     * @throws CertificateException Cuando los datos proporcionados no son un certificado X509. */
    public static X509Certificate isCertificate(final byte[] data) throws CertificateException {

    	if (data == null || data.length < 1) {
    		throw new CertificateException(
				"Los datos eran nulos o vacios" //$NON-NLS-1$
			);
    	}

    	final CertificateFactory cf = CertificateFactory.getInstance("X.509"); //$NON-NLS-1$

    	// Antes de nada un intento directo
    	try {
	        return (X509Certificate) cf.generateCertificate(
	            new ByteArrayInputStream(
	                 data
	             )
	        );
    	}
        catch(final Exception e) {
            // Ignoramos los errores
        }

    	// Despues, intento en Base64 directo sin cabeceras y con posibilidad de URLEncoding
    	try {
	        return (X509Certificate) cf.generateCertificate(
	            new ByteArrayInputStream(
	                 Base64.decode(
	                   new String(data)
	                   		.replace("%0A", "") //$NON-NLS-1$ //$NON-NLS-2$
	                   		.replace("%2F", "/") //$NON-NLS-1$ //$NON-NLS-2$
	                   		.replace("%2B", "+") //$NON-NLS-1$ //$NON-NLS-2$
	                   		.replace("%3D", "=") //$NON-NLS-1$ //$NON-NLS-2$
	                 )
	             )
	        );
    	}
        catch(final Exception e) {
            // Ignoramos los errores
        }

    	throw new CertificateException(
    		"Los datos proporcionados no son un certificado X.509" //$NON-NLS-1$
		);
    }

    /** Comprueba si los datos introducidos se corresponden a un fichero XML.
     * @param data Datos a analizar.
     * @return {@code true} si los datos son XML. */
    public static boolean isXML(final byte[] data) {
        return AOFileUtils.isXML(data);
    }

    /** Comprueba si los datos introducidos se corresponden a una firma XML soportada.
     * @param data Datos a analizar.
     * @return {@code true} si los datos son una firma XML soportada. */
    public static boolean isSignedXML(final byte[] data) {
        try {
            return new AOXAdESSigner().isSign(data) || new AOXMLDSigSigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Comprueba si los datos introducidos se corresponden a un fichero PDF.
     * @param data Datos a analizar.
     * @return {@code true} si los datos son un PDF. */
    public static boolean isPDF(final byte[] data) {
        try {
            return new AOPDFSigner().isValidDataFile(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Comprueba si los datos introducidos se corresponden a un fichero PDF firmado.
     * @param data Datos a analizar.
     * @return {@code true} si los datos son un PDF firmado. */
    public static boolean isSignedPDF(final byte[] data) {
        try {
            return new AOPDFSigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Comprueba si los datos introducidos se corresponden a un fichero binario.
     * @param data Datos a analizar.
     * @return {@code true} si los datos son un fichero binario. */
    public static boolean isBinary(final byte[] data) {
    	try {
    		return new AOCMSSigner().isValidDataFile(data);
    	}
    	catch (final Exception e) {
    		return false;
    	}
    }

    /** Comprueba si los datos introducidos se corresponden a una firma binaria soportada.
     * @param data Datos a analizar.
     * @return {@code true} si los datos son una firma binaria soportada. */
    public static boolean isSignedBinary(final byte[] data) {

        try {
            return new AOCMSSigner().isSign(data) || new AOCAdESSigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Indica si los datos son una factura electr&oacute;nica.
     * @param file Datos a comprobar
     * @return <code>true</code> si los datos son una <a href="http://www.facturae.es/">factura electr&oacute;nica</a>,
     *         <code>false</code> en caso contrario */
    public static boolean isFacturae(final byte[] file) {
        try {
            return new AOFacturaESigner().isValidDataFile(file);
        }
        catch(final Exception e) {
            return false;
        }
    }

	/** Comprueba si los datos introducidos se corresponden con una factura
     * electr&oacute;nica firmada.
     * @param data Datos a analizar.
     * @return {@code true} si los datos son una firma XML soportada. */
    public static boolean isSignedFacturae(final byte[] data) {

        try {
            return new AOFacturaESigner().isSign(data);
        }
        catch(final Exception e) {
            return false;
        }
    }

    /** Comprueba si los datos introducidos se corresponden a un documento ODF.
     * @param data Datos a analizar.
     * @return {@code true} si los datos son ODF. */
    public static boolean isODF(final byte[] data) {
        try {
       		return new AOODFSigner().isValidDataFile(data);
        }
        catch(final Exception e) {
        	LOGGER.log(Level.WARNING, "Ocurrio un error al analizar los datos: " + e); //$NON-NLS-1$
        }
        return false;
    }

    /** Comprueba si los datos introducidos se corresponden a una firma ODF soportada.
     * @param data Datos a analizar.
     * @return {@code true} si los datos son una firma ODF soportada. */
    public static boolean isSignedODF(final byte[] data) {
        try {
            return new AOODFSigner().isSign(data);
        }
        catch(final Exception e) {
        	LOGGER.log(Level.WARNING, "Ocurrio un error al analizar los datos: " + e); //$NON-NLS-1$
        }
        return false;
    }

    /** Comprueba si los datos introducidos se corresponden a un documento OOXML.
     * @param data Datos a analizar.
     * @return {@code true} si los datos son OOXML. */
    public static boolean isOOXML(final byte[] data) {
        try {
        	return new AOOOXMLSigner().isValidDataFile(data);
        }
        catch(final Exception e) {
        	LOGGER.log(Level.WARNING, "Ocurrio un error al analizar los datos: " + e); //$NON-NLS-1$
        }
        return false;
    }

    /** Comprueba si los datos introducidos se corresponden a una firma OOXML soportada.
     * @param data Datos a analizar.
     * @return {@code true} si los datos son una firma OOXML soportada. */
    public static boolean isSignedOOXML(final byte[] data) {
        try {
            return new AOOOXMLSigner().isSign(data);
        }
        catch(final Exception e) {
        	LOGGER.log(Level.WARNING, "Ocurrio un error al analizar los datos: " + e); //$NON-NLS-1$
        }
        return false;
    }

	/**
	 * Indica si el documento ha recibido un posible PDF Shadow Attack.
	 * @param actualdata Datos del documento actual.
	 * @param lastReviewData Datos de la &uacute;ltima revisi&oacute;n firmada.
	 * @param pagesToCheck P&aacuteginas a comprobar.
	 * @return Validez o no de los datos en el documento.
	 * @throws IOException Cuando falla la carga del documento como PDF o
	 * la generaci&oacute;n de las im&aacute;genes.
	 */
	public static SignValidity checkPdfShadowAttack(final byte[] actualdata, final byte[] lastReviewData, final String pagesToCheck) throws IOException {

		try (final PDDocument actualDoc = PDDocument.load(new ByteArrayInputStream(actualdata));
				final PDDocument lastReviewDoc = PDDocument.load(new ByteArrayInputStream(lastReviewData))) {

			final PDFRenderer actualPdfRenderer = new PDFRenderer(actualDoc);
			final PDFRenderer lastReviewPdfRenderer = new PDFRenderer(lastReviewDoc);
			BufferedImage actualReviewImage;
			BufferedImage lastReviewImage;

			int totalPagesToCheck;

			if (PdfExtraParams.PAGES_TO_CHECK_PSA_VALUE_ALL.equals(pagesToCheck)) {
				totalPagesToCheck = actualDoc.getNumberOfPages();
			} else {
				totalPagesToCheck = Integer.parseInt(pagesToCheck);
			}

			for (int i = 0; i < totalPagesToCheck; i++) {
				// Se comprueba si en la misma pagina se esta solapando alguna firma visible con otra
				final boolean isOverlappingSignatures = checkSignatureOverlaping(actualDoc.getPage(i).getAnnotations());
				if (isOverlappingSignatures) {
					return new SignValidity(SIGN_DETAIL_TYPE.PENDING_CONFIRM_BY_USER, VALIDITY_ERROR.OVERLAPPING_SIGNATURE);
				}
				actualReviewImage = actualPdfRenderer.renderImageWithDPI(i, 40, ImageType.GRAY);
				lastReviewImage = lastReviewPdfRenderer.renderImageWithDPI(i, 40, ImageType.GRAY);
				final boolean equalImages = checkImagesChanges(actualReviewImage, lastReviewImage);

				if (!equalImages) {
					return new SignValidity(SIGN_DETAIL_TYPE.PENDING_CONFIRM_BY_USER, VALIDITY_ERROR.MODIFIED_DOCUMENT);
				}
			}
        }
		catch (final Exception e) {
        	LOGGER.log(Level.WARNING, "Error al cargar el fichero, se salta al siguiente"); //$NON-NLS-1$
 		}

		return null;
	}

	/**
	 * Comprueba si las im&aacute;genes son iguales
	 * @param img1 {@link BufferedImage}
	 * @param img2 {@link BufferedImage}
	 * @return TRUE si son iguales, FALSE si no lo son.
	 */
	public static boolean checkImagesChanges(final BufferedImage img1, final BufferedImage img2) {
		if (imageDimensionsEqual(img1, img2)) {
			final int diffAmount = drawSubtractionImage(img1, img2, null);
			return diffAmount == 0;
		}
		return false;
	}

	/**
	 * Comprueba si las im&aacute;genes tienen el mismo tama&ntilde;o.
	 * @param img1 {@link BufferedImage}
	 * @param img2 {@link BufferedImage}
	 * @return TRUE si las dimensiones de las im&aacute;genes son iguales, FALSE en caso contrario.
	 */
	private static boolean imageDimensionsEqual(final BufferedImage img1, final BufferedImage img2) {
		if (img1.getWidth() != img2.getWidth() || img1.getHeight() != img2.getHeight()) {
			LOGGER.log(Level.WARNING, "Las imagenes no tienen el mismo tamano"); //$NON-NLS-1$
			return false;
		}
		return true;
	}

	/**
	 * Analiza las im&aacute;genes p&iacute;xel a p&iacute;xel y devuelve la diferencia de p&iacute;xeles entre im&aacute;genes.
	 * @param img1   {@link BufferedImage} a comparar.
	 * @param img2   {@link BufferedImage} a comparar.
	 * @param outImg {@link BufferedImage} la imagen resultante.
	 * @return Cantidad de p&iacute;xeles distintos entre las im&aacute;genes.
	 */
	private static int drawSubtractionImage(final BufferedImage img1, final BufferedImage img2, final BufferedImage outImg) {
		int diffAmount = 0;
		int diff;
		int result;
		for (int i = 0; i < img1.getHeight() && i < img2.getHeight(); i++) {
			for (int j = 0; j < img1.getWidth() && j < img2.getWidth(); j++) {
				final int rgb1 = img1.getRGB(j, i);
				final int rgb2 = img2.getRGB(j, i);
				final int r1 = rgb1 >> 16 & 0xff;
				final int g1 = rgb1 >> 8 & 0xff;
				final int b1 = rgb1 & 0xff;
				final int r2 = rgb2 >> 16 & 0xff;
				final int g2 = rgb2 >> 8 & 0xff;
				final int b2 = rgb2 & 0xff;

				diff = Math.abs(r1 - r2);
				diff += Math.abs(g1 - g2);
				diff += Math.abs(b1 - b2);

				if (diff > 0) {
					diffAmount++;
				}

				if (outImg != null) {
					diff /= 3;
					result = diff << 16 | diff << 8 | diff;
					outImg.setRGB(j, i, result);
				}
			}
		}
		return diffAmount;
	}

	private static boolean checkSignatureOverlaping(final List<PDAnnotation> signAnnotations) {
		for (int i = 0 ; i < signAnnotations.size() - 1 ; i++) {
			if (signAnnotations.get(i) instanceof PDAnnotationWidget) {
				final Rectangle2D rect = toJavaRectangle(signAnnotations.get(i).getRectangle());
				if (rect.getWidth() == 0 || rect.getHeight() == 0) {
					// Es una firma invisible
					continue;
				}
				final boolean isOverlap = checkIsOverlapping(rect, signAnnotations, ++i);
				if (isOverlap) {
					return true;
				}
			}
		}
		return false;
	}

	private static boolean checkIsOverlapping(final Rectangle2D rect1, final List<PDAnnotation> signAnnotations, final int index) {
		// Se comprueba a partir de la siguiente posicion para no comprobarse a si mismo
		for (int i = index; i < signAnnotations.size() ; i++) {
			final Rectangle2D rect2 = toJavaRectangle(signAnnotations.get(i).getRectangle());
			if (rect1.getMinX() > rect2.getMaxX() || rect2.getMinX() > rect1.getMaxX()) {
				return false;
			}
			if (rect1.getMinY() > rect2.getMaxY() || rect2.getMinY() > rect1.getMaxY()) {
				return false;
			}
		}
		return true;
	}

	private static Rectangle2D toJavaRectangle(final PDRectangle pdRect) {
		final float x = pdRect.getLowerLeftX();
		final float y = pdRect.getLowerLeftY();
		final float width = pdRect.getUpperRightX() - pdRect.getLowerLeftX();
		final float height = pdRect.getUpperRightY() - pdRect.getLowerLeftY();
		return new Rectangle2D.Float(x, y, width, height);
	}
}
