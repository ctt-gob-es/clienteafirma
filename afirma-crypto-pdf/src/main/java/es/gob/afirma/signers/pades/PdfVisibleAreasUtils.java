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
import java.util.logging.Level;
import java.util.logging.Logger;

import com.aowagie.text.DocumentException;
import com.aowagie.text.Element;
import com.aowagie.text.ExceptionConverter;
import com.aowagie.text.Font;
import com.aowagie.text.Image;
import com.aowagie.text.Phrase;
import com.aowagie.text.Rectangle;
import com.aowagie.text.pdf.BaseFont;
import com.aowagie.text.pdf.ByteBuffer;
import com.aowagie.text.pdf.ColumnText;
import com.aowagie.text.pdf.PdfSignatureAppearance;
import com.aowagie.text.pdf.PdfStamper;
import com.aowagie.text.pdf.PdfTemplate;
import com.aowagie.text.pdf.PdfWriter;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.misc.Platform.OS;
import es.gob.afirma.signers.pades.common.PdfExtraParams;

final class PdfVisibleAreasUtils {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final int DEFAULT_LAYER_2_FONT_SIZE = 12;
    private static final int UNDEFINED = -1;
    private static final String BLACK = "black"; //$NON-NLS-1$

	private static final String LAYERTEXT_TAG_DELIMITER = "$$"; //$NON-NLS-1$
	static final String LAYERTEXT_TAG_DATE_PREFIX = LAYERTEXT_TAG_DELIMITER + "SIGNDATE"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_DATE_DELIMITER = "="; //$NON-NLS-1$
	static final String LAYERTEXT_TAG_SUBJECTCN = "$$SUBJECTCN$$"; //$NON-NLS-1$
	static final String LAYERTEXT_TAG_PSEUDONYM = "$$PSEUDONYM$$"; //$NON-NLS-1$
	static final String LAYERTEXT_TAG_OU = "$$OU$$"; //$NON-NLS-1$
	static final String LAYERTEXT_TAG_OUS = "$$OUS$$"; //$NON-NLS-1$
	static final String LAYERTEXT_TAG_TITLE = "$$TITLE$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_SUBJECTDN = "$$SUBJECTDN$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_ISSUERCN = "$$ISSUERCN$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_CERTSERIAL = "$$CERTSERIAL$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_GIVENNAME = "$$GIVENNAME$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_SURNAME = "$$SURNAME$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_ORGANIZATION = "$$ORGANIZATION$$"; //$NON-NLS-1$
	static final String LAYERTEXT_TAG_REASON = "$$REASON$$"; //$NON-NLS-1$
	static final String LAYERTEXT_TAG_LOCATION = "$$LOCATION$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_CONTACT = "$$CONTACT$$"; //$NON-NLS-1$

	/** Margen de la firmas visibles PDF rotadas. */
    private static final float MARGIN = 2;

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

	/**
	 * Obtiene la fuente para generar el texto de la firma visible.
	 * @param fontFamily Identificador de familia de la fuente (0: COURIER, 1: HELVETICA,
	 * 2: TIMES_ROMAN, 3: SYMBOL, 4: ZAPFDINGBATS). Con -1 se usa el valor por defecto: COURIER.
	 * @param fontSize Tama&ntilde;o de fuente. Con -1 se usa el valor por defecto: 12.
	 * @param fontStyle Estilo a aplicar al texto (0: NORMAL, 1: BOLD, 2: ITALIC, 3: BOLDITALIC,
	 * 4: UNDERLINE, 8: STRIKETHRU). Con -1 se usa el valor por defecto: NORMAL.
	 * @param fontColor Nombre del color (black, white, lightGray, gray, darkGray, red o pink).
	 * @param pdfa {@code true} si se trata de un documento PDF/A, {@code false} en caso contrario.
	 * @return Fuente de letra.
	 */
	static Font getFont(final int fontFamily,
			                             final int fontSize,
			                             final int fontStyle,
			                             final String fontColor,
			                             final boolean pdfa) {

		final int family = fontFamily == UNDEFINED ? Font.COURIER : fontFamily;
		final int size = fontSize == UNDEFINED ? DEFAULT_LAYER_2_FONT_SIZE : fontSize;
		final int style = fontStyle == UNDEFINED ? Font.NORMAL : fontStyle;

		BaseFont baseFont;
		try {
			baseFont = getBaseFont(fontFamily, pdfa);
		}
		catch (final Exception e) {
			LOGGER.warning(
					"Error construyendo la fuente de letra para la firma visible PDF, se usara la por defecto y el PDF no sera compatible PDF/A: " + e //$NON-NLS-1$
					);

			return new Font(family, size, style, null);
		}

		final String colorName = fontColor != null ? fontColor.toLowerCase() : BLACK;
		final ColorValues cv = COLORS.get(colorName) != null ? COLORS.get(colorName) : COLORS.get(BLACK);

		try {
			Class<?> colorClass;
			if (Platform.getOS() == OS.ANDROID || Platform.getOS() == OS.OTHER) {
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

			// Utilizamos el constructor por reflexion para poder
			// utilizar indistintamente la case de color de Java
			// y de Android
			return Font.class.getConstructor(
				BaseFont.class,
				Float.TYPE,
				Integer.TYPE,
				colorClass
			).newInstance(baseFont, Float.valueOf(size), Integer.valueOf(style), color);
		}
		catch (final Exception e) {
			LOGGER.warning(
				"Error estableciendo el color del tipo de letra para la firma visible PDF, se usara el por defecto: " + e); //$NON-NLS-1$
			return new Font(baseFont, size, style, null);
		}
	}

	private static BaseFont getBaseFont(final int fontFamily, final boolean pdfa) throws DocumentException, IOException {

		BaseFont font;

		// Si la firma es PDF/A, incrustamos toda la fuente para seguir
		// respetando el estandar
		if (pdfa) {
			try {
				font = loadFontToEmbed(fontFamily);
			}
			// En Android puede fallar con un error la carga de una fuente, asi que se
			// protege con un Throwable
			catch (final Throwable e) {
				LOGGER.log(Level.WARNING,
						"No se ha podido cargar la fuente de letra para incrustar. Puede que el resultado no sea un PDF/A", //$NON-NLS-1$
						e);
				font = loadInternalFont(fontFamily);
			}
		}
		else {
			font = loadInternalFont(fontFamily);
		}
		return font;
	}

	/**
	 * Carga una de las fuentes incluidas en la biblioteca y la marca para que se incruste en el PDF.
	 * @param fontFamily Familia de fuentes.
	 * @return Fuente de letra.
	 * @throws DocumentException Cuando falla la composici&oacute;n de la fuente.
	 * @throws IOException Cuando falla la carga de la fuente.
	 */
	private static BaseFont loadFontToEmbed(final int fontFamily) throws DocumentException, IOException {
		BaseFont font;
		switch (fontFamily) {
		case Font.HELVETICA:
			font = BaseFont.createFont("/fonts/helvetica.ttf", BaseFont.IDENTITY_H, BaseFont.EMBEDDED); //$NON-NLS-1$
			break;
		case Font.TIMES_ROMAN:
			font = BaseFont.createFont("/fonts/times.ttf", BaseFont.IDENTITY_H, BaseFont.EMBEDDED); //$NON-NLS-1$
			break;
		case Font.COURIER:
		default:
			font = BaseFont.createFont("/fonts/courier.ttf", BaseFont.IDENTITY_H, BaseFont.EMBEDDED); //$NON-NLS-1$
		}
		font.setSubset(false);
		return font;
	}

	/**
	 * Compone una de las fuentes por defecto de PDF.
	 * @param fontFamily Familia de fuentes.
	 * @return Fuente de letra.
	 * @throws DocumentException Cuando falla la composici&oacute;n de la fuente.
	 * @throws IOException Cuando falla la carga de la fuente.
	 */
	private static BaseFont loadInternalFont(final int fontFamily) throws DocumentException, IOException {
		BaseFont font;
		switch (fontFamily) {
		case Font.HELVETICA:
			font = BaseFont.createFont(BaseFont.HELVETICA, "", BaseFont.NOT_EMBEDDED); //$NON-NLS-1$
			break;
		case Font.TIMES_ROMAN:
			font = BaseFont.createFont(BaseFont.TIMES_ROMAN, "", BaseFont.NOT_EMBEDDED); //$NON-NLS-1$
			break;
		case Font.COURIER:
		default:
			font = BaseFont.createFont(BaseFont.COURIER, "", BaseFont.NOT_EMBEDDED); //$NON-NLS-1$
		}
		return font;
	}

	/**
	 * Obtiene el texto que debe mostrarse en la firmna PDF visible.
	 * @param txt Texto base con los patrones de los datos a integrar.
	 * @param cert Certificado de firma.
	 * @param signDate Fecha de firma.
	 * @param reason Raz&oacute;n de firma del PDF.
	 * @param signatureProductionCity Ciudad en la que se firma el PDF.
	 * @param signerContact Informaci&oacute;n de contacto del firmante.
	 * @param obfuscate Indica si se deben ofuscar los identificadores
	 * encontrados en los campos que se incorporan al texto.
	 * @param maskConfig M&aacute;scara con los par&aacute;metros para la
	 * identificaci&oacute;n de los identificados de los campos.
	 * @return Texto que mostrar en el campo de firma PDF.
	 */
	static String getLayerText(final String txt,
							   final X509Certificate cert,
							   final Calendar signDate,
							   final String reason,
							   final String signatureProductionCity,
							   final String signerContact,
							   final boolean obfuscate,
							   final String maskConfig) {

		if (txt == null) {
			return null;
		}

		String ret = txt;

		// Se mapean los datos relativos al certificado de firma
		if (cert != null) {

			final PdfTextMask mask = prepareMask(obfuscate, maskConfig);
			String cn = AOUtil.getCN(cert);

			if (cn != null && mask != null) {
				cn = obfuscateIds(cn, mask);
			}
			ret = ret.replace(LAYERTEXT_TAG_SUBJECTCN, cn)
					.replace(LAYERTEXT_TAG_ISSUERCN, AOUtil.getCN(cert.getIssuerX500Principal().getName()))
					.replace(LAYERTEXT_TAG_CERTSERIAL, cert.getSerialNumber().toString());

			// Se mapea el principal del subject del certificado
			String subjectPrincipal = cert.getSubjectX500Principal().toString();

			final String pseudonym = AOUtil.getRDNvalueFromLdapName("OID.2.5.4.65", subjectPrincipal); //$NON-NLS-1$
			if (pseudonym != null) {
				ret = ret.replace(LAYERTEXT_TAG_PSEUDONYM, pseudonym);
			}

			final String [] ous = AOUtil.getOUS(subjectPrincipal);
			if (ous.length > 0) {
				ret = ret.replace(LAYERTEXT_TAG_OU, ous[0]);
				String ousResult = ous[0];
				for (int i = 1 ; i < ous.length ; i++) {
					ousResult += ", " + ous[i]; //$NON-NLS-1$
				}
				ret = ret.replace(LAYERTEXT_TAG_OUS, ousResult);
			}
			else {
				ret = ret.replace(LAYERTEXT_TAG_OU, ""); //$NON-NLS-1$
				ret = ret.replace(LAYERTEXT_TAG_OUS, ""); //$NON-NLS-1$
			}

			final String title = AOUtil.getRDNvalueFromLdapName("t", subjectPrincipal); //$NON-NLS-1$
			ret = ret.replace(LAYERTEXT_TAG_TITLE, title != null ? title : ""); //$NON-NLS-1$

			if (subjectPrincipal != null && mask != null) {
				subjectPrincipal = obfuscateIds(subjectPrincipal, mask);
			}

			ret = ret.replace(LAYERTEXT_TAG_SUBJECTDN, subjectPrincipal);

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
		while (ret.contains(LAYERTEXT_TAG_DATE_PREFIX)) {
			final int strIdx = ret.indexOf(LAYERTEXT_TAG_DATE_PREFIX);
			final String sdTag = ret.substring(
				strIdx,
				ret.indexOf(LAYERTEXT_TAG_DELIMITER, strIdx + LAYERTEXT_TAG_DATE_PREFIX.length()) + LAYERTEXT_TAG_DELIMITER.length()
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

	/**
	 * Genera una mascara de ofuscacion si se solicita ofuscar.
	 * @param obfuscate Indica si se debe ofuscar texto o no.
	 * @param maskConfig Configuraci&oacute;n de la mascara a aplicar o {@code null}
	 * si se quiere usar la por defecto.
	 * @return M&aacute;scara de ofuscaci&oacute;n o {@code null} si no se desea ofuscar.
	 */
	private static PdfTextMask prepareMask(final boolean obfuscate, final String maskConfig) {

		PdfTextMask mask = null;
		if (obfuscate) {
			if (maskConfig != null) {
				try {
					mask = PdfTextMask.parseParam(maskConfig);
				}
				catch (final Exception e) {
					LOGGER.log(Level.WARNING, "La mascara de ofuscacion no esta bien definida. Se usara la por defecto", e); //$NON-NLS-1$
				}
			}
			else {
				mask = new PdfTextMask();
			}
		}
		return mask;
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

	/**
	 * Crea una imagen de firma ya con el texto y/o r&uacute;brica ya rotados.
	 * @param stamper Estampador PDF.
	 * @param appearance Apariencia de la firma PDF. De esta se obtiene el texto de la firma y su
	 * fuente de letra.
	 * @param rubricRect Rect&aacute;ngulo de firma.
	 * @param pageRotation Rotaci&oacute;n de la propia p&aacute;gina.
	 * @param degrees Grados de rotaci&oacute;n del campo de firma.
	 * @param rubricImg imagen a estampar
	 * @throws DocumentException Si hay problemas tratando el PDF.
	 * @throws IOException En cualquier otro error. */
    static Image buildRotatedSignatureImage(final PdfStamper stamper,
    		                               final PdfSignatureAppearance appearance,
    		                               final Rectangle rubricRect,
    		                               final int degrees,
    		                               final Image rubricImg) throws DocumentException,
                                                                     IOException {

    	// Anchura y altura del espacio en el que se mostrara la firma
    	final float rubricWidth = rubricRect.getWidth();
        final float rubricHeight = rubricRect.getHeight();

        // Normalizamos los grados de rotacion
        int rotation = degrees % 360;
        if (rotation < 0) {
        	rotation += 360;
        }

    	// Crear la imagen rotada vamos a generar la imagen sin rotar y despues rotarla. Para ello:
        // 1. Creamos una plantilla para imprimir la rubrica sin rotar.
        // 2. Imprimimos en la plantilla la imagen de rubrica si procede.
        // 3. Imprimimos en la plantilla el texto de firma si procede.
        // 4. Rotamos la imagen de la plantilla (el rotado solo es a nivel logico).
        // 5. Creamos una plantilla y le agregamos la nueva imagen para hacer efectivo el rotado.
        // 6. Devolvemos la imagen de la nueva plantilla.


        // 1 -- Creamos una plantilla con las dimensiones apropiadas para imprimir la rubrica sin
        // rotar
        final float canvasWidth = rotation == 0 || rotation == 180 ? rubricWidth : rubricHeight;
        final float canvasHeight = rotation == 0 || rotation == 180 ? rubricHeight : rubricWidth;
        final PdfTemplate canvas = PdfTemplate.createTemplate(stamper.getWriter(), canvasWidth, canvasHeight);

        // 2 -- Si hay imagen de rubrica, la imprimimos en la plantilla escalandola para que encaje
        // en ella sin deformarse
        if (rubricImg != null) {

        	rubricImg.setInterpolation(true);
        	rubricImg.setAbsolutePosition(0, 0);

        	// Reescalamos la imagen para que se adapte al nuevo rectangulo
        	if (rotation == 90 || rotation == 270) {
        		final float scale = Math.min(rubricHeight / rubricImg.getWidth(), rubricWidth / rubricImg.getHeight());
        		final float w = rubricImg.getWidth() * scale;
        		final float h = rubricImg.getHeight() * scale;
        		final float x = (rubricWidth - h) / 2;
        		final float y = (rubricHeight - w) / 2;
        		canvas.addImage(rubricImg, w, 0, 0, h, y, x);
        	} else {
        		final float scale = Math.min(rubricWidth / rubricImg.getWidth(), rubricHeight / rubricImg.getHeight());
        		final float w = rubricImg.getWidth() * scale;
        		final float h = rubricImg.getHeight() * scale;
        		final float x = (rubricWidth - w) / 2;
        		final float y = (rubricHeight - h) / 2;
        		canvas.addImage(rubricImg, w, 0, 0, h, x, y);
        	}
        }

    	// 3 -- Si hay texto, configuramos la fuente apropiada y lo agregamos
        if (appearance.getLayer2Text() != null) {

        	Font f = appearance.getLayer2Font();
        	if (f == null) {
        		f = new Font();
        	}
        	final BaseFont bf = f.getCalculatedBaseFont(false);
    		canvas.setFontAndSize(bf, f.getSize());

        	final Rectangle rect = new Rectangle(canvas.getBoundingBox());
        	printText(canvas, appearance.getLayer2Text(), f, rect);
        }

        // 4 -- Tomamos la imagen generada en el lienzo y la rotamos
        Image rotatedRubric = Image.getInstance(canvas);
        rotatedRubric.setInterpolation(true);
        rotatedRubric.setRotationDegrees(rotation);
        rotatedRubric.setAbsolutePosition(0, 0);

        // Ahora, la imagen esta rotada a nivel logico, pero no lo estara realmente hasta que se
        // imprima, asi que creamos un segundo lienzo con el tamano correspondiente a la firma e
        // imprimimos en el la imagen rotada

        // 5 -- Creamos un segundo lienzo, con el tamano real de la firma, e imprimimos en el la
        // imagen rotada

        // Creamos el lienzo y copiamos en la version rotada del anterior. Esto es necesario para
        // conseguir la imagen rotada, pero, por algun motivo, tiene efectos inexperados:
        //  - Copiara el texto sin rotar, por lo que tendremos que eliminarlo de la version rotada.
        //  - Habra rotado la imagen (que incluye tanto la rubrica como el texto), pero no la habra
        //    agregado al nuevo lienzo, por lo que  habra que agregarla.
        final PdfTemplate rotatedCanvas = PdfTemplate.createTemplate(stamper.getWriter(), rubricWidth, rubricHeight);
        try (final ByteBuffer actualBuffer = canvas.getInternalBuffer();
        		final ByteBuffer rotatedBuffer = rotatedCanvas.getInternalBuffer();) {
        	rotatedBuffer.write(actualBuffer.toByteArray());
        }

        // Reseteamos para eliminar el texto sin rotar
        rotatedCanvas.reset();

        // Agregamos la imagen rotada con la rubrica y el texto
        rotatedCanvas.addImage(rotatedRubric);

        // 6 -- Tomamos la imagen del lienzo y la devolvemos
        rotatedRubric = Image.getInstance(rotatedCanvas);
        rotatedRubric.setInterpolation(true);
        rotatedRubric.setAbsolutePosition(0, 0);

        return rotatedRubric;
    }

    /**
     * Imprime un texto en una plantilla PDF.
     * @param template Plantilla en la que imprimir el texto.
     * @param text Texto.
     * @param font Fuente que aplicar.
     * @param dataRect Espacio que debe ocupar la firma.
     * @throws DocumentException Cuando ocurre un error al imprimir el texto.
     */
    private static void printText(final PdfTemplate template, final String text, final Font font, final Rectangle dataRect) throws DocumentException {

    	final Rectangle sr = new Rectangle(MARGIN, MARGIN, dataRect.getWidth() - MARGIN, dataRect.getHeight() - MARGIN);

    	final float adjustedFontSize = fitText(font, text, sr, font.getSize(), PdfWriter.RUN_DIRECTION_DEFAULT);

    	final ColumnText ct = new ColumnText(template);
    	ct.setRunDirection(PdfWriter.RUN_DIRECTION_DEFAULT);
    	ct.setSimpleColumn(new Phrase(text, font), sr.getLeft(), sr.getBottom(), sr.getRight(), sr.getTop(), adjustedFontSize, Element.ALIGN_LEFT);
    	ct.go();
    }

    /**
     * Fits the text to some rectangle adjusting the font size as needed. M&eacute;todo copiado de iText.
     * @param font the font to use
     * @param text the text
     * @param rect the rectangle where the text must fit
     * @param maxFontSize the maximum font size
     * @param runDirection the run direction
     * @return the calculated font size that makes the text fit.
     * @author Paulo Soares
     */
    private static float fitText(final Font font, final String text, final Rectangle rect, final float maxFontSize, final int runDirection) {
        float maxSize = maxFontSize;
    	try {
            ColumnText ct = null;
            int status = 0;
            if (maxSize <= 0) {
                int cr = 0;
                int lf = 0;
                final char t[] = text.toCharArray();
                for (int k = 0; k < t.length; ++k) {
                    if (t[k] == '\n') {
						++lf;
					} else if (t[k] == '\r') {
						++cr;
					}
                }
                final int minLines = Math.max(cr, lf) + 1;
                maxSize = Math.abs(rect.getHeight()) / minLines - 0.001f;
            }
            font.setSize(maxSize);
            final Phrase ph = new Phrase(text, font);
            ct = new ColumnText(null);
            ct.setSimpleColumn(ph, rect.getLeft(), rect.getBottom(), rect.getRight(), rect.getTop(), maxFontSize, Element.ALIGN_LEFT);
            ct.setRunDirection(runDirection);
            status = ct.go(true);
            if ((status & ColumnText.NO_MORE_TEXT) != 0) {
				return maxSize;
			}
            final float precision = 0.1f;
            float min = 0;
            float max = maxSize;
            float size = maxSize;
            for (int k = 0; k < 50; ++k) { //just in case it doesn't converge
                size = (min + max) / 2;
                ct = new ColumnText(null);
                font.setSize(size);
                ct.setSimpleColumn(new Phrase(text, font), rect.getLeft(), rect.getBottom(), rect.getRight(), rect.getTop(), size, Element.ALIGN_LEFT);
                ct.setRunDirection(runDirection);
                status = ct.go(true);
                if ((status & ColumnText.NO_MORE_TEXT) != 0) {
                    if (max - min < size * precision) {
						return size;
					}
                    min = size;
                } else {
					max = size;
				}
            }
            return size;
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
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

    /**
     * Indica si se ha establecido una configuraci&oacute;n que d&eacute; pie
     * a realizar una firma visible PDF. Se considerar&aacute; que ser&aacute;
     * una firma visible aquella que defina un &aacute;rea de firma o un campo
     * de firma.
     * @param extraParams Conjunto de propiedades con la cofiguraci&oacute;n de
     * la firma.
     * @return  {@code true} si la firma es visible, {@code false} si no lo es.
     */
    static boolean isVisibleSignature(final Properties extraParams) {

    	if (extraParams == null) {
    		return true;
    	}

    	final String signatureField = extraParams.getProperty(PdfExtraParams.SIGNATURE_FIELD);
    	if (signatureField != null) {
    		return true;
    	}

    	final Rectangle signatureRect = PdfUtil.getPositionOnPage(extraParams, "signature"); //$NON-NLS-1$
    	if (signatureRect != null
    			&& Math.signum(signatureRect.getWidth()) != 0
    			&& Math.signum(signatureRect.getHeight()) != 0) {
    		return true;
    	}

    	return false;
    }

    /**
     * Ofusca de un texto las part&iacute;culas que pueden ser interpretables
     * como un identificador de usuario.
     * @param text Texto del que ofuscar.
     * @param mask Configuraci&oacute;n con la m&aacute;scara a aplicar.
     * @return Texto ofuscado.
     */
    static String obfuscateIds(final String text, final PdfTextMask mask) {

    	final char[] chars = text.toCharArray();

    	int digitCount = 0;
    	int pos = 0;
    	boolean found = false;
    	for (int i = 0; i < chars.length; i++) {
    		if (Character.isLetterOrDigit(chars[i])) {
    			if (Character.isDigit(chars[i])) {
    				digitCount++;
    				if (digitCount == mask.getMinLength()) {
    					found = true;
    				}
    			}
    			else {
    				digitCount = 0;
    			}
    		}
    		else {
    			if (found) {
    				obfuscate(chars, pos, i - pos, mask);
    				found = false;
    			}
   				pos = i + 1;
    		}
    	}
    	if (found) {
			obfuscate(chars, pos, chars.length - pos, mask);
			found = false;
		}

    	return new String(chars);
    }

    /**
     * Aplica el algoritmo de ofuscaci&oacute;n sobre un fragmento determinado
     * de un texto.
     * @param text Array de caracteres del texto.
     * @param pos Posici&oacute;n del texto a partir de la cual aplicar el
     * algoritmo de ofuscaci&oacute;n.
     * @param length Longitud de la subcadena sobre la que hay que aplicar la
     * ofuscaci&oacute;n.
     * @param mask Configuraci&oacute;n con la m&aacute;scara a aplicar.
     */
    private static void obfuscate (final char[] text, final int pos, final int length, final PdfTextMask mask) {

    	final int numDigits = countDigits(text);
    	final int plainDigits = countPlainPositions(mask.getPositions());
    	final boolean applyOnlyDigits = numDigits >= plainDigits;

		// Si hay suficientes digitos para aplicar la mascara,
		// esta se aplicara solo sobre los digitos
    	if (applyOnlyDigits) {

    		boolean[] posMasked = mask.getPositions();

    		// Si no hay suficientes digitos en el texto para aplicar la mascara
    		// completa, se adaptara la mascara para omitir posiciones ofuscadas
    		// del inicio del texto
    		if (mask.isShiftSupported() && numDigits < posMasked.length) {

    			posMasked = new boolean[numDigits];
    			final boolean[] maskPositions = mask.getPositions();

    			int currentCount = 0;
    			final int omitCount = maskPositions.length - numDigits;
    			for (int i = 0; i < maskPositions.length; i++) {
    				if (maskPositions[i] || currentCount >= omitCount) {
    					posMasked[i - currentCount] = maskPositions[i];
    				}
    				else {
    					currentCount++;
    				}
    			}
    		}

    		// Omitiremos todas las letras y los digitos que se encuentren en
    		// una posicion a ofuscar segun la mascara
    		for (int i = 0, j = 0; i < length; i++) {
    			if (Character.isDigit(text[pos + i])) {
    				if (j >= posMasked.length || !posMasked[j]) {
    					text[pos + i] = mask.getObfuscatedChar();
    				}
    				j++;
    			}
    			else {
    				text[pos + i] = mask.getObfuscatedChar();
    			}
    		}
    	}
    	// Si no hay suficientes digitos para aplicar la mascara
    	// aplicaremos la mascara desde atras
    	else {
    		final boolean[] posMasked = mask.getPositions();
    		int posMaskedPos = posMasked.length - 1;
    		for (int i = pos + length - 1; i >= pos; i--) {
    			if (posMaskedPos < 0 || !posMasked[posMaskedPos]) {
    				text[i] = mask.getObfuscatedChar();
    			}
				posMaskedPos--;
    		}
    	}
    }

    /**
     * Cuenta el n&uacute;mero de caracteres en claro que admite la
     * m&aacute;scara.
     * @param positions Posiciones contempladas por la m&aacute;scara.
     * @return N&uacute;mero de caracteres que la m&aacute;scara no
     * ofuscar&aacute;.
     */
    private static int countPlainPositions(final boolean[] positions) {
    	int count = 0;
    	for (final boolean position : positions) {
    		if (position) {
    			count++;
    		}
    	}
		return count;
	}

    /**
     * Cuenta el n&uacute;mero de d&iacute;gitos de una cadena de texto.
     * @param text Cadena de texto.
     * @return N&uacute;mero de d&iacute;gitos en la cadena.
     */
    private static int countDigits(final char[] text) {
    	int digitsCount = 0;
    	for (final char c : text) {
    		if (Character.isDigit(c)) {
    			digitsCount++;
    		}
    	}
    	return digitsCount;
    }
}
