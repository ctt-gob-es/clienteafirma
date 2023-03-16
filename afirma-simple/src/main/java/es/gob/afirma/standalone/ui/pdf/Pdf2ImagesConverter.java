/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.pdf;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.rendering.ImageType;
import org.apache.pdfbox.rendering.PDFRenderer;

/** Conversor de PDF a conjunto de im&aacute;genes.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
final class Pdf2ImagesConverter {

	/** Devuelve un listado con las miniaturas de todas las p&aacute;ginas del PDF.
	 * @param inPdf Documento PDF.
	 * @return Listado de im&aacute;genes.
	 * @throws IOException Cuando falla la carga del documento como PDF o
	 * la generaci&oacute;n de las im&aacute;genes. */
	static List<BufferedImage> pdf2Images(final byte[] inPdf) throws IOException {
		List<BufferedImage> pagesAsImages;
		try (final PDDocument document = PDDocument.load(new ByteArrayInputStream(inPdf))) {
			final PDFRenderer pdfRenderer = new PDFRenderer(document);
			pagesAsImages = new ArrayList<>(document.getNumberOfPages());
			for (int i = 0; i < document.getNumberOfPages(); i++) {
				pagesAsImages.add(pdfRenderer.renderImageWithDPI(i, 75, ImageType.RGB));
			}
        }
        return pagesAsImages;
	}

	/**
	 * Devuelve un listado con las miniaturas de las p&aacute;ginas que podemos
	 * necesitar del PDF. Estas son las primeras, las &uacute;ltimas y las del entorno
	 * de la p&aacute;gina actual.
	 * @param inPdf Documento PDF.
	 * @param password Contrase&ntilde;a de apertura del PDF.
	 * @param currentPage P&aacute;gina mostrada actualmente.
	 * @return Listado de im&aacute;genes.
	 * @throws IOException Cuando falla la carga del documento como PDF o
	 * la generaci&oacute;n de las im&aacute;genes.
	 */
	static List<BufferedImage> pdf2ImagesUsefulSections(final byte[] inPdf, final char[] passwordChars, final int currentPage) throws IOException {
		List<BufferedImage> pagesAsImages;
		final String password = passwordChars != null ? new String(passwordChars) : null;
		try (final PDDocument document = PDDocument.load(new ByteArrayInputStream(inPdf), password)) {
			final PDFRenderer pdfRenderer = new PDFRenderer(document);
			pagesAsImages = new ArrayList<>(document.getNumberOfPages());
			for (int i = 0; i < document.getNumberOfPages(); i++) {
				if (isOfTheFirstPages(i) || isOfTheLastPages(i, document.getNumberOfPages()) || isNearTheCurrentPage(i, currentPage)) {
					pagesAsImages.add(pdfRenderer.renderImageWithDPI(i, 75, ImageType.RGB));
				}
				else {
					pagesAsImages.add(null);
				}
			}
        }
        return pagesAsImages;
	}

	/** Actualiza el array de im&aacute;genes con la nueva selecci&oacute;n de miniaturas necesarias
	 * en base a la posici&oacute;n se&ntilde;alada, elimina las que ya no son necesarias.
	 * @param inPdf Documento PDF.
	 * @param currentPage P&aacute;gina entorno a la cual son necesarias las miniaturas.
	 * @param pagesAsImages Listado actual de miniaturas.
	 * @throws IOException Cuando falla la carga del documento como PDF o
	 *                     la generaci&oacute;n de las im&aacute;genes. */
	static void updateUsefulSections(final byte[] inPdf, final int currentPage, final List<BufferedImage> pagesAsImages) throws IOException {
		try (final PDDocument document = PDDocument.load(new ByteArrayInputStream(inPdf))) {
			final PDFRenderer pdfRenderer = new PDFRenderer(document);
			final int numPages = document.getNumberOfPages();
			for (int i = 0; i < numPages; i++) {
				if (isOfTheFirstPages(i) || isOfTheLastPages(i, numPages) || isNearTheCurrentPage(i, currentPage)) {
					if (pagesAsImages.get(i) == null) {
						pagesAsImages.set(i, pdfRenderer.renderImageWithDPI(i, 75, ImageType.RGB));
					}
				}
				else {
					pagesAsImages.set(i, null);
				}
			}
        }
	}

	/** Comprueba si un n&uacute;mero de p&aacute;gina est&acute; al inicio del documento.
	 * @param page N&uacute;mero de p&aacute;gina.
	 * @return {@code true} si la p&aacute;gina est&aacute; entre las primeras del documento,
	 *         {@code false} en caso contrario. */
	private static boolean isOfTheFirstPages(final int page) {
		return page < 4;
	}

	/** Comprueba si un n&uacute;mero de p&aacute;gina est&acute; al final del documento.
	 * @param page N&uacute;mero de p&aacute;gina.
	 * @param totalPages N&uacute;mero total de p&aacute;ginas del documento.
	 * @return {@code true} si la p&aacute;gina est&aacute; entre las &uacute;ltimas del documento,
	 *         {@code false} en caso contrario. */
	private static boolean isOfTheLastPages(final int page, final int totalPages) {
		return page >= totalPages - 4 && page < totalPages;
	}

	/** Comprueba si un n&uacute;mero de p&aacute;gina es cercana a la p&aacute;gina actual del documento.
	 * @param page N&uacute;mero de p&aacute;gina.
	 * @param currentPage N&uacute;mero de la p&aacute;gina actual.
	 * @return {@code true} si la p&aacute;gina es cercana a la actual, {@code false} en caso contrario. */
	private static boolean isNearTheCurrentPage(final int page, final int currentPage) {
		return Math.abs(page - currentPage) <= 4;
	}
}
