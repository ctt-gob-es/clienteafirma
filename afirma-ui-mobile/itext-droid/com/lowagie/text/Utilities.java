/*
 * $Id: Utilities.java 3514 2008-06-27 09:26:36Z blowagie $
 *
 * Copyright 2007 by Bruno Lowagie.
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
 * If you didn't download this code from the following link, you should check if
 * you aren't using an obsolete version:
 * http://www.lowagie.com/iText/
 */
package com.lowagie.text;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Properties;
import java.util.Set;

import com.lowagie.text.pdf.PRTokeniser;

/**
 * A collection of convenience methods that were present in many different iText
 * classes.
 */

public class Utilities {

	/**
	 * Gets the keys of a Hashtable
	 *
	 * @param table
	 *            a Hashtable
	 * @return the keyset of a Hashtable (or an empty set if table is null)
	 */
	static Set getKeySet(final Hashtable table) {
		return table == null ? Collections.EMPTY_SET : table.keySet();
	}

	/**
	 * Utility method to extend an array.
	 *
	 * @param original
	 *            the original array or <CODE>null</CODE>
	 * @param item
	 *            the item to be added to the array
	 * @return a new array with the item appended
	 */
	public static Object[][] addToArray(Object original[][], final Object item[]) {
		if (original == null) {
			original = new Object[1][];
			original[0] = item;
			return original;
		} else {
			final Object original2[][] = new Object[original.length + 1][];
			System.arraycopy(original, 0, original2, 0, original.length);
			original2[original.length] = item;
			return original2;
		}
	}

	/**
	 * Checks for a true/false value of a key in a Properties object.
	 * @param attributes
	 * @param key
	 * @return a true/false value of a key in a Properties object
	 */
	public static boolean checkTrueOrFalse(final Properties attributes, final String key) {
		return "true".equalsIgnoreCase(attributes.getProperty(key));
	}

	/**
	 * Unescapes an URL. All the "%xx" are replaced by the 'xx' hex char value.
	 * @param src the url to unescape
	 * @return the unescaped value
	 */
	static String unEscapeURL(final String src) {
	    final StringBuffer bf = new StringBuffer();
	    final char[] s = src.toCharArray();
	    for (int k = 0; k < s.length; ++k) {
	        final char c = s[k];
	        if (c == '%') {
	            if (k + 2 >= s.length) {
	                bf.append(c);
	                continue;
	            }
	            final int a0 = PRTokeniser.getHex(s[k + 1]);
	            final int a1 = PRTokeniser.getHex(s[k + 2]);
	            if (a0 < 0 || a1 < 0) {
	                bf.append(c);
	                continue;
	            }
	            bf.append((char)(a0 * 16 + a1));
	            k += 2;
	        } else {
				bf.append(c);
			}
	    }
	    return bf.toString();
	}

	/**
	 * This method makes a valid URL from a given filename.
	 * <P>
	 * This method makes the conversion of this library from the JAVA 2 platform
	 * to a JDK1.1.x-version easier.
	 *
	 * @param filename
	 *            a given filename
	 * @return a valid URL
	 * @throws MalformedURLException
	 */
	static URL toURL(final String filename) throws MalformedURLException {
        try {
            return new URL(filename);
        }
        catch (final Exception e) {
            return new File(filename).toURI().toURL();
        }
	}

	/**
	 * This method is an alternative for the <CODE>InputStream.skip()</CODE>
	 * -method that doesn't seem to work properly for big values of <CODE>size
	 * </CODE>.
	 *
	 * @param is
	 *            the <CODE>InputStream</CODE>
	 * @param size
	 *            the number of bytes to skip
	 * @throws IOException
	 */
	static public void skip(final InputStream is, int size) throws IOException {
	    long n;
		while (size > 0) {
	        n = is.skip(size);
	        if (n <= 0) {
				break;
			}
			size -= n;
		}
	}

	/**
	 * Measurement conversion from millimeters to inches.
	 * @param	value	a value in millimeters
	 * @return	a value in inches
	 * @since	2.1.2
	 */
	private static final float millimetersToInches(final float value) {
	    return value / 25.4f;
	}

	/**
	 * Measurement conversion from points to inches.
	 * @param	value	a value in points
	 * @return	a value in inches
	 * @since	2.1.2
	 */
	private static final float pointsToInches(final float value) {
	    return value / 72f;
	}

	/**
	 * Measurement conversion from inches to millimeters.
	 * @param	value	a value in inches
	 * @return	a value in millimeters
	 * @since	2.1.2
	 */
	private static final float inchesToMillimeters(final float value) {
	    return value * 25.4f;
	}

	/**
	 * Measurement conversion from inches to points.
	 * @param	value	a value in inches
	 * @return	a value in points
	 * @since	2.1.2
	 */
	private static final float inchesToPoints(final float value) {
	    return value * 72f;
	}

    /**
     * Check if the value of a character belongs to a certain interval
     * that indicates it's the higher part of a surrogate pair.
     * @param c	the character
     * @return	true if the character belongs to the interval
     * @since	2.1.2
     */
    public static boolean isSurrogateHigh(final char c) {
        return c >= '\ud800' && c <= '\udbff';
    }

    /**
     * Check if the value of a character belongs to a certain interval
     * that indicates it's the lower part of a surrogate pair.
     * @param c	the character
     * @return	true if the character belongs to the interval
     * @since	2.1.2
     */
    private static boolean isSurrogateLow(final char c) {
        return c >= '\udc00' && c <= '\udfff';
    }

    /**
     * Checks if two subsequent characters in a String are
     * are the higher and the lower character in a surrogate
     * pair (and therefore eligible for conversion to a UTF 32 character).
     * @param text	the String with the high and low surrogate characters
     * @param idx	the index of the 'high' character in the pair
     * @return	true if the characters are surrogate pairs
     * @since	2.1.2
     */
    public static boolean isSurrogatePair(final String text, final int idx) {
        if (idx < 0 || idx > text.length() - 2) {
			return false;
		}
        return isSurrogateHigh(text.charAt(idx)) && isSurrogateLow(text.charAt(idx + 1));
    }

    /**
     * Checks if two subsequent characters in a character array are
     * are the higher and the lower character in a surrogate
     * pair (and therefore eligible for conversion to a UTF 32 character).
     * @param text	the character array with the high and low surrogate characters
     * @param idx	the index of the 'high' character in the pair
     * @return	true if the characters are surrogate pairs
     * @since	2.1.2
     */
    public static boolean isSurrogatePair(final char[] text, final int idx) {
        if (idx < 0 || idx > text.length - 2) {
			return false;
		}
        return isSurrogateHigh(text[idx]) && isSurrogateLow(text[idx + 1]);
    }

    /**
     * Returns the code point of a UTF32 character corresponding with
     * a high and a low surrogate value.
     * @param highSurrogate	the high surrogate value
     * @param lowSurrogate	the low surrogate value
     * @return	a code point value
     * @since	2.1.2
     */
    public static int convertToUtf32(final char highSurrogate, final char lowSurrogate) {
         return (highSurrogate - 0xd800) * 0x400 + lowSurrogate - 0xdc00 + 0x10000;
    }

    /**
     * Converts a unicode character in a character array to a UTF 32 code point value.
     * @param text	a character array that has the unicode character(s)
     * @param idx	the index of the 'high' character
     * @return	the code point value
     * @since	2.1.2
     */
    public static int convertToUtf32(final char[] text, final int idx) {
         return (text[idx] - 0xd800) * 0x400 + text[idx + 1] - 0xdc00 + 0x10000;
    }

    /**
     * Converts a unicode character in a String to a UTF32 code point value
     * @param text	a String that has the unicode character(s)
     * @param idx	the index of the 'high' character
     * @return	the codepoint value
     * @since	2.1.2
     */
    public static int convertToUtf32(final String text, final int idx) {
         return (text.charAt(idx) - 0xd800) * 0x400 + text.charAt(idx + 1) - 0xdc00 + 0x10000;
    }

}
