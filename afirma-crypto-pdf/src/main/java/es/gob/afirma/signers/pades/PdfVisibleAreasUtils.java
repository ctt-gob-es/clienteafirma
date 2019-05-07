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
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import com.aowagie.text.DocumentException;
import com.aowagie.text.Image;
import com.aowagie.text.Rectangle;
import com.aowagie.text.pdf.ByteBuffer;
import com.aowagie.text.pdf.PdfSignatureAppearance;
import com.aowagie.text.pdf.PdfStamper;
import com.aowagie.text.pdf.PdfTemplate;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.Platform.OS;

final class PdfVisibleAreasUtils {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final int DEFAULT_LAYER_2_FONT_SIZE = 12;
    private static final int COURIER = 0;
    private static final int UNDEFINED = -1;
    private static final String BLACK = "black"; //$NON-NLS-1$

	private static final String LAYERTEXT_TAG_DELIMITER = "$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_DATE_PREFIX = LAYERTEXT_TAG_DELIMITER + "SIGNDATE"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_DATE_DELIMITER = "="; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_SUBJECTCN = "$$SUBJECTCN$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_SUBJECTDN = "$$SUBJECTDN$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_ISSUERCN = "$$ISSUERCN$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_CERTSERIAL = "$$CERTSERIAL$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_GIVENNAME = "$$GIVENNAME$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_SURNAME = "$$SURNAME$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_ORGANIZATION = "$$ORGANIZATION$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_REASON = "$$REASON$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_LOCATION = "$$LOCATION$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_CONTACT = "$$CONTACT$$"; //$NON-NLS-1$

	private static final Map<String, ColorValues> COLORS = new HashMap<>(7);
	static {
		COLORS.put(BLACK      , new ColorValues(0,     0,   0));
		COLORS.put("white"    , new ColorValues(255, 255, 255)); //$NON-NLS-1$
		COLORS.put("lightGray", new ColorValues(192, 192, 192)); //$NON-NLS-1$
		COLORS.put("gray"     , new ColorValues(128, 128, 128)); //$NON-NLS-1$
		COLORS.put("darkGray" , new ColorValues(64,   64,  64)); //$NON-NLS-1$
		COLORS.put("red"      , new ColorValues(255,   0,   0)); //$NON-NLS-1$
		COLORS.put("pink"     , new ColorValues(255, 175, 175)); //$NON-NLS-1$
	}

	private PdfVisibleAreasUtils() {
		// No instanciable
	}

	static com.aowagie.text.Font getFont(final int fontFamily,
			                             final int fontSize,
			                             final int fontStyle,
			                             final String fontColor) {

		final String colorName = fontColor != null ? fontColor.toLowerCase() : BLACK;

		final ColorValues cv = COLORS.get(colorName) != null ? COLORS.get(colorName) : COLORS.get(BLACK);

		try {
			Class<?> colorClass;
			if (Platform.getOS() == OS.ANDROID) {
				colorClass = Class.forName("harmony.java.awt.Color"); //$NON-NLS-1$
			}
			else {
				colorClass = Class.forName("java.awt.Color"); //$NON-NLS-1$
			}
			final Object color = colorClass.getConstructor(
				Integer.TYPE,
				Integer.TYPE,
				Integer.TYPE
			).newInstance(
				Integer.valueOf(cv.getR()),
				Integer.valueOf(cv.getG()),
				Integer.valueOf(cv.getB())
			);

			return com.aowagie.text.Font.class.getConstructor(
				Integer.TYPE,
				Float.TYPE,
				Integer.TYPE,
				colorClass
			).newInstance(
				// Family (COURIER = 0, HELVETICA = 1, TIMES_ROMAN = 2,
				// SYMBOL = 3, ZAPFDINGBATS = 4)
				Integer.valueOf(fontFamily == UNDEFINED ? COURIER : fontFamily),
				// Size (DEFAULTSIZE = 12)
				Float.valueOf(fontSize == UNDEFINED ? DEFAULT_LAYER_2_FONT_SIZE : fontSize),
				// Style (NORMAL = 0, BOLD = 1, ITALIC = 2,
				// BOLDITALIC = 3, UNDERLINE = 4, STRIKETHRU = 8)
				Integer.valueOf(fontStyle == UNDEFINED ? com.aowagie.text.Font.NORMAL : fontStyle),
				// Color
				color
			);
		}
		catch (final Exception e) {
			LOGGER.warning(
				"Error estableciendo el color del tipo de letra para la firma visible PDF, se usara el por defecto: " + e //$NON-NLS-1$
			);
			return new com.aowagie.text.Font(
				// Family (COURIER = 0, HELVETICA = 1, TIMES_ROMAN = 2, SYMBOL = 3,
				// ZAPFDINGBATS = 4)
				fontFamily == UNDEFINED ? COURIER : fontFamily,
				// Size (DEFAULTSIZE = 12)
				fontSize == UNDEFINED ? DEFAULT_LAYER_2_FONT_SIZE : fontSize,
				// Style (NORMAL = 0, BOLD = 1, ITALIC = 2, BOLDITALIC = 3,
				// UNDERLINE = 4, STRIKETHRU = 8)
				fontStyle == UNDEFINED ? com.aowagie.text.Font.NORMAL : fontStyle,
				// Color
				null
			);
		}
	}

	/**
	 * Obtiene el texto que debe mostrarse en la firmna PDF visible.
	 * @param txt Texto base con los patrones de los datos a integrar.
	 * @param cert Certificado de firma.
	 * @param signDate Fecha de firma.
	 * @param reason Raz&oacute;n de firma del PDF.
	 * @param signatureProductionCity Ciudad en la que se firma el PDF.
	 * @param signerContact Informaci&oacute;n de contacto del firmante.
	 * @return Texto que mostrar en el campo de firma PDF.
	 */
	static String getLayerText(final String txt,
							   final X509Certificate cert,
							   final Calendar signDate,
							   final String reason,
							   final String signatureProductionCity,
							   final String signerContact) {
		if (txt == null) {
			return null;
		}

		String ret = txt;

		// Se mapean los datos relativos al certificado de firma
		if (cert != null) {
			ret = ret.replace(LAYERTEXT_TAG_SUBJECTCN, AOUtil.getCN(cert))
					.replace(LAYERTEXT_TAG_ISSUERCN, AOUtil.getCN(cert.getIssuerX500Principal().getName()))
					.replace(LAYERTEXT_TAG_CERTSERIAL, cert.getSerialNumber().toString());

			// Se mapea el principal del subject del certificado
			final String subjectPrincipal = cert.getSubjectX500Principal().toString();
			ret = ret.replace(LAYERTEXT_TAG_SUBJECTDN, cert.getSubjectX500Principal().toString());

			// Se mapea el nombre declarado en el subject del certificado
			final String givenName = AOUtil.getRDNvalueFromLdapName("GIVENNAME", subjectPrincipal);  //$NON-NLS-1$
			ret = ret.replace(LAYERTEXT_TAG_GIVENNAME, givenName != null ? givenName : ""); //$NON-NLS-1$

			// Se mapea el apellido declarado en el subject del certificado
			final String surname = AOUtil.getRDNvalueFromLdapName("SURNAME", subjectPrincipal); //$NON-NLS-1$
			ret = ret.replace(LAYERTEXT_TAG_SURNAME, surname != null ? surname : ""); //$NON-NLS-1$

			// Se mapea la organizacion declarada en el subject del certificado
			final String organization = AOUtil.getRDNvalueFromLdapName("o", subjectPrincipal); //$NON-NLS-1$
			ret = ret.replace(LAYERTEXT_TAG_ORGANIZATION, organization != null ? organization : ""); //$NON-NLS-1$
		}

		// Se mapean los datos del PDF
		ret = ret.replace(LAYERTEXT_TAG_REASON, reason != null ? reason : ""); //$NON-NLS-1$
		ret = ret.replace(LAYERTEXT_TAG_LOCATION, signatureProductionCity != null ? signatureProductionCity : ""); //$NON-NLS-1$
		ret = ret.replace(LAYERTEXT_TAG_CONTACT, signerContact != null ? signerContact : ""); //$NON-NLS-1$

		// Se mapea la fecha con el formato proporcionado
		if (txt.contains(LAYERTEXT_TAG_DATE_PREFIX)) {
			final int strIdx = txt.indexOf(LAYERTEXT_TAG_DATE_PREFIX);
			final String sdTag = txt.substring(
				strIdx,
				txt.indexOf(LAYERTEXT_TAG_DELIMITER, strIdx + LAYERTEXT_TAG_DATE_PREFIX.length()) + LAYERTEXT_TAG_DELIMITER.length()
			);
			String date;
			final Date tbpDate = signDate != null ? signDate.getTime() : new Date();
			if (sdTag.contains(LAYERTEXT_TAG_DATE_DELIMITER)) {
				final String dateFormat = sdTag.replace(LAYERTEXT_TAG_DELIMITER, "").split(LAYERTEXT_TAG_DATE_DELIMITER)[1]; //$NON-NLS-1$
				try {
					date = new SimpleDateFormat(dateFormat).format(
							tbpDate
					);
				}
				catch(final Exception e) {
					LOGGER.warning(
						"Patron incorrecto para la fecha de firma en la firma visible (" + dateFormat + "), se usara el por defecto: " + e //$NON-NLS-1$ //$NON-NLS-2$
					);
					date = new SimpleDateFormat().format(
						tbpDate
					);
				}
			}
			else {
				date = new SimpleDateFormat().format(
					tbpDate
				);
			}
			ret = ret.replace(sdTag, date);
		}
		return ret;
	}

	private static final class ColorValues {

		private final int r;
		private final int g;
		private final int b;

		ColorValues(final int red, final int green, final int blue) {
			this.r = red;
			this.g = green;
			this.b = blue;
		}

		int getR() {
			return this.r;
		}

		int getG() {
			return this.g;
		}

		int getB() {
			return this.b;
		}
	}

	/** A&ntilde;ade al PDF un campo de firma rotado.
	 * Para ello, se obtiene la representaci&oacute;n gr&aacute;fica de la capa 2 de la apariencia de
	 * la firma, se convierte a imagen, se rota y se inserta como imagen. Esto hace que se pierda la
	 * imagen original de la capa 2.
	 * @param stamper Estampador PDF.
	 * @param appearance Apariencia de la firma PDF.
	 * @param pageRect Rect&aacute;ngulo de firma.
	 * @param page P&aacute;gina donde insertar la firma.
	 * @param fieldName Nombre del campo de firma a usar (si se especifica <code>null</code> se
	 *                  crea uno nuevo.
	 * @param degrees Grados de ro5taci&oacute;n del campo de firma.
	 * @throws DocumentException Si hay problemas tratando el PDF.
	 * @throws IOException En cualquier otro error. */
    static void setVisibleSignatureRotated(final PdfStamper stamper,
    		                               final PdfSignatureAppearance appearance,
    		                               final Rectangle pageRect,
    		                               final int page,
    		                               final String fieldName,
    		                               final int degrees) throws DocumentException,
                                                                     IOException {
        final float height = pageRect.getHeight();
        final float width = pageRect.getWidth();
        final float llx = pageRect.getLeft();
        final float lly = pageRect.getBottom();

        // La firma visible se configura inicialmente de forma horizontal.
        appearance.setVisibleSignature(
    		new Rectangle(0, 0, height, width),
    		page,
    		null
		);

        // Iniciamos la creacion de la apariencia, de forma que la podamos modificar posteriormente.
        appearance.getAppearance();

        appearance.getTopLayer().setWidth(height);
        appearance.getTopLayer().setHeight(width);
        final PdfTemplate n2Layer = appearance.getLayer(2);
        n2Layer.setWidth(height);
        n2Layer.setHeight(width);
        // Rotamos entonces la capa 2: http://developers.itextpdf.com/question/how-rotate-paragraph.
        final PdfTemplate t = PdfTemplate.createTemplate(stamper.getWriter(), height, width);
        try (
    		final ByteBuffer internalBuffer = t.getInternalBuffer();
		) {
	        internalBuffer.write(n2Layer.toString().getBytes());
	        n2Layer.reset();
	        final Image textImg = Image.getInstance(t);
	        textImg.setInterpolation(true);
	        textImg.setRotationDegrees(degrees);
	        textImg.setAbsolutePosition(0, 0);
	        n2Layer.addImage(textImg);
	        n2Layer.setWidth(width);
	        n2Layer.setHeight(height);
	        appearance.getTopLayer().setWidth(width);
	        appearance.getTopLayer().setHeight(height);
        }

        // Usamos las dimensiones indicadas.
        appearance.setVisibleSignature(
    		new Rectangle(llx, lly, llx + width, lly + height),
    		page,
    		fieldName
		);
    }

    /** Devuelve la posici&oacute;n de la p&aacute;gina en donde debe agregarse
     * la firma. La medida de posicionamiento es el p&iacute;xel y se cuenta en
     * el eje horizontal de izquierda a derecha y en el vertical de abajo a
     * arriba.
     * @param extraParams Conjunto de propiedades con las coordenadas del rect&aacute;ngulo
     * @return  Rect&aacute;ngulo que define la posici&oacute;n de la p&aacute;gina en donde
     *          debe agregarse la firma*/
    static Rectangle getSignaturePositionOnPage(final Properties extraParams) {
    	return PdfUtil.getPositionOnPage(extraParams, "signature"); //$NON-NLS-1$
    }

}
