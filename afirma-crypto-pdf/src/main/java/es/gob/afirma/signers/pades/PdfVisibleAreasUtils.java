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

import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

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
	private static final String LAYERTEXT_TAG_ISSUERCN = "$$ISSUERCN$$"; //$NON-NLS-1$
	private static final String LAYERTEXT_TAG_CERTSERIAL = "$$CERTSERIAL$$"; //$NON-NLS-1$

	private static final Map<String, ColorValues> COLORS = new HashMap<String, ColorValues>(7);
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

	static String getLayerText(final String txt, final X509Certificate cert, final Calendar signDate) {
		if (txt == null) {
			return null;
		}
		String ret = cert == null ?
			txt :
				txt.replace(LAYERTEXT_TAG_SUBJECTCN, AOUtil.getCN(cert))
				   .replace(LAYERTEXT_TAG_ISSUERCN, AOUtil.getCN(cert.getIssuerX500Principal().getName()))
				   .replace(LAYERTEXT_TAG_CERTSERIAL, cert.getSerialNumber().toString());
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


}
