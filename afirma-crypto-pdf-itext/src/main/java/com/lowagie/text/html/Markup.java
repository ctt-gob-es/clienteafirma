/*
 * $Id: Markup.java 3654 2009-01-21 16:11:00Z blowagie $
 *
 * Copyright 2001, 2002 by Bruno Lowagie.
 *
 * The contents of this file are subject to the Mozilla Public License Version 1.1
 * (the "License"); you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the License.
 *
 * The Original Code is 'iText, a free JAVA-PDF library'.
 *
 * The Initial Developer of the Original Code is Bruno Lowagie. Portions created by
 * the Initial Developer are Copyright (C) 1999, 2000, 2001, 2002 by Bruno Lowagie.
 * All Rights Reserved.
 * Co-Developer of the code is Paulo Soares. Portions created by the Co-Developer
 * are Copyright (C) 2000, 2001, 2002 by Paulo Soares. All Rights Reserved.
 *
 * Contributor(s): all the names of the contributors are added in the source code
 * where applicable.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * LGPL license (the "GNU LIBRARY GENERAL PUBLIC LICENSE"), in which case the
 * provisions of LGPL are applicable instead of those above.  If you wish to
 * allow use of your version of this file only under the terms of the LGPL
 * License and not to allow others to use your version of this file under
 * the MPL, indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by the LGPL.
 * If you do not delete the provisions above, a recipient may use your version
 * of this file under either the MPL or the GNU LIBRARY GENERAL PUBLIC LICENSE.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the MPL as stated above or under the terms of the GNU
 * Library General Public License as published by the Free Software Foundation;
 * either version 2 of the License, or any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library general Public License for more
 * details.
 *
 * Contributions by:
 * Lubos Strapko
 *
 * If you didn't download this code from the following link, you should check if
 * you aren't using an obsolete version:
 * http://www.lowagie.com/iText/
 */

package com.lowagie.text.html;

import java.awt.Color;
import java.util.Properties;
import java.util.StringTokenizer;

/**
 * A class that contains all the possible tagnames and their attributes.
 */

public class Markup {

	// HTML attributes

	/** This is used for inline css style information */
	public static final String HTML_ATTR_STYLE = "style";



	// CSS keys

	/** the CSS tag for text color */
	public static final String CSS_KEY_COLOR = "color";

	/** the CSS tag for the font family */
	public static final String CSS_KEY_FONTFAMILY = "font-family";

	/** the CSS tag for the font size */
	public static final String CSS_KEY_FONTSIZE = "font-size";

	/** the CSS tag for the font style */
	public static final String CSS_KEY_FONTSTYLE = "font-style";

	/** the CSS tag for the font weight */
	public static final String CSS_KEY_FONTWEIGHT = "font-weight";

	/** the CSS tag for text decorations */
	public static final String CSS_KEY_LINEHEIGHT = "line-height";

	/** the CSS tag for text decorations */
	public static final String CSS_KEY_VERTICALALIGN = "vertical-align";

	// CSS values

	/** a CSS value for text font weight */
	public static final String CSS_VALUE_BOLD = "bold";

	/** a CSS value for text font style */
	public static final String CSS_VALUE_ITALIC = "italic";

	/** a CSS value for text decoration */
	public static final String CSS_VALUE_LINETHROUGH = "line-through";

	/** a CSS value */
	public static final String CSS_VALUE_NORMAL = "normal";

	/** a CSS value for text font style */
	public static final String CSS_VALUE_OBLIQUE = "oblique";

	/** a CSS value for text decoration */
	public static final String CSS_VALUE_UNDERLINE = "underline";

	/** a default value for font-size
     * @since 2.1.3
     */
	public static final float DEFAULT_FONT_SIZE = 12f;

	/**
	 * Parses a length.
	 *
	 * @param string
	 *            a length in the form of an optional + or -, followed by a
	 *            number and a unit.
	 * @return a float
	 */

	public static float parseLength(String string) {
		// TODO: Evaluate the effect of this.
		// It may change the default behavour of the methd if this is changed.
		// return parseLength(string, Markup.DEFAULT_FONT_SIZE);
		int pos = 0;
		final int length = string.length();
		boolean ok = true;
		while (ok && pos < length) {
			switch (string.charAt(pos)) {
			case '+':
			case '-':
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			case '.':
				pos++;
				break;
			default:
				ok = false;
			}
		}
		if (pos == 0) {
			return 0f;
		}
		if (pos == length) {
			return Float.parseFloat(string + "f");
		}
		final float f = Float.parseFloat(string.substring(0, pos) + "f");
		string = string.substring(pos);
		// inches
		if (string.startsWith("in")) {
			return f * 72f;
		}
		// centimeters
		if (string.startsWith("cm")) {
			return f / 2.54f * 72f;
		}
		// millimeters
		if (string.startsWith("mm")) {
			return f / 25.4f * 72f;
		}
		// picas
		if (string.startsWith("pc")) {
			return f * 12f;
		}
		// default: we assume the length was measured in points
		return f;
	}

	/**
	 * New method contributed by: Lubos Strapko
	 *
	 * @since 2.1.3
	 */
	public static float parseLength(String string, final float actualFontSize) {
		if (string == null) {
			return 0f;
		}
		int pos = 0;
		final int length = string.length();
		boolean ok = true;
		while (ok && pos < length) {
			switch (string.charAt(pos)) {
			case '+':
			case '-':
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			case '.':
				pos++;
				break;
			default:
				ok = false;
			}
		}
		if (pos == 0) {
			return 0f;
		}
		if (pos == length) {
			return Float.parseFloat(string + "f");
		}
		final float f = Float.parseFloat(string.substring(0, pos) + "f");
		string = string.substring(pos);
		// inches
		if (string.startsWith("in")) {
			return f * 72f;
		}
		// centimeters
		if (string.startsWith("cm")) {
			return f / 2.54f * 72f;
		}
		// millimeters
		if (string.startsWith("mm")) {
			return f / 25.4f * 72f;
		}
		// picas
		if (string.startsWith("pc")) {
			return f * 12f;
		}
		// 1em is equal to the current font size
		if (string.startsWith("em")) {
			return f * actualFontSize;
		}
		// one ex is the x-height of a font (x-height is usually about half the
		// font-size)
		if (string.startsWith("ex")) {
			return f * actualFontSize / 2;
		}
		// default: we assume the length was measured in points
		return f;
	}

	/**
	 * Converts a <CODE>Color</CODE> into a HTML representation of this <CODE>
	 * Color</CODE>.
	 *
	 * @param s
	 *            the <CODE>Color</CODE> that has to be converted.
	 * @return the HTML representation of this <COLOR>Color </COLOR>
	 */

	public static Color decodeColor(String s) {
		if (s == null) {
			return null;
		}
		s = s.toLowerCase().trim();
		try {
			return WebColors.getRGBColor(s);
		}
		catch(final IllegalArgumentException iae) {
			return null;
		}
	}

	/**
	 * This method parses a String with attributes and returns a Properties
	 * object.
	 *
	 * @param string
	 *            a String of this form: 'key1="value1"; key2="value2";...
	 *            keyN="valueN" '
	 * @return a Properties object
	 */
	public static Properties parseAttributes(final String string) {
		final Properties result = new Properties();
		if (string == null) {
			return result;
		}
		final StringTokenizer keyValuePairs = new StringTokenizer(string, ";");
		StringTokenizer keyValuePair;
		String key;
		String value;
		while (keyValuePairs.hasMoreTokens()) {
			keyValuePair = new StringTokenizer(keyValuePairs.nextToken(), ":");
			if (keyValuePair.hasMoreTokens()) {
				key = keyValuePair.nextToken().trim();
			} else {
				continue;
			}
			if (keyValuePair.hasMoreTokens()) {
				value = keyValuePair.nextToken().trim();
			} else {
				continue;
			}
			if (value.startsWith("\"")) {
				value = value.substring(1);
			}
			if (value.endsWith("\"")) {
				value = value.substring(0, value.length() - 1);
			}
			result.setProperty(key.toLowerCase(), value);
		}
		return result;
	}

}