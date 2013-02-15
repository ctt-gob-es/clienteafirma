/*
 * Copyright 2005 by Paulo Soares.
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

package com.lowagie.text.pdf.hyphenation;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.StringTokenizer;

import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.html.HtmlTags;
import com.lowagie.text.xml.simpleparser.SimpleXMLDocHandler;
import com.lowagie.text.xml.simpleparser.SimpleXMLParser;

/** Parses the xml hyphenation pattern.
 *
 * @author Paulo Soares (psoares@consiste.pt)
 */
class SimplePatternParser implements SimpleXMLDocHandler,
		PatternConsumer {
	private int currElement;

	private PatternConsumer consumer;

	private final StringBuffer token;

	private ArrayList exception;

	private char hyphenChar;



	private static final int ELEM_CLASSES = 1;

	private static final int ELEM_EXCEPTIONS = 2;

	private static final int ELEM_PATTERNS = 3;

	private static final int ELEM_HYPHEN = 4;

	/** Creates a new instance of PatternParser2 */
	public SimplePatternParser() {
		this.token = new StringBuffer();
		this.hyphenChar = '-'; // default
	}

	public void parse(final InputStream stream, final PatternConsumer consumer) {
		this.consumer = consumer;
		try {
			SimpleXMLParser.parse(this, stream);
		} catch (final IOException e) {
			throw new ExceptionConverter(e);
		} finally {
			try {
				stream.close();
			} catch (final Exception e) {
			}
		}
	}

	private static String getPattern(final String word) {
		final StringBuffer pat = new StringBuffer();
		final int len = word.length();
		for (int i = 0; i < len; i++) {
			if (!Character.isDigit(word.charAt(i))) {
				pat.append(word.charAt(i));
			}
		}
		return pat.toString();
	}

	private ArrayList normalizeException(final ArrayList ex) {
		final ArrayList res = new ArrayList();
		for (int i = 0; i < ex.size(); i++) {
			final Object item = ex.get(i);
			if (item instanceof String) {
				final String str = (String) item;
				final StringBuffer buf = new StringBuffer();
				for (int j = 0; j < str.length(); j++) {
					final char c = str.charAt(j);
					if (c != this.hyphenChar) {
						buf.append(c);
					} else {
						res.add(buf.toString());
						buf.setLength(0);
						final char[] h = new char[1];
						h[0] = this.hyphenChar;
						// we use here hyphenChar which is not necessarily
						// the one to be printed
						res.add(new Hyphen(new String(h), null, null));
					}
				}
				if (buf.length() > 0) {
					res.add(buf.toString());
				}
			} else {
				res.add(item);
			}
		}
		return res;
	}

	private String getExceptionWord(final ArrayList ex) {
		final StringBuffer res = new StringBuffer();
		for (int i = 0; i < ex.size(); i++) {
			final Object item = ex.get(i);
			if (item instanceof String) {
				res.append((String) item);
			} else {
				if (((Hyphen) item).noBreak != null) {
					res.append(((Hyphen) item).noBreak);
				}
			}
		}
		return res.toString();
	}

	private static String getInterletterValues(final String pat) {
		final StringBuffer il = new StringBuffer();
		final String word = pat + "a"; // add dummy letter to serve as sentinel
		final int len = word.length();
		for (int i = 0; i < len; i++) {
			final char c = word.charAt(i);
			if (Character.isDigit(c)) {
				il.append(c);
				i++;
			} else {
				il.append('0');
			}
		}
		return il.toString();
	}

	@Override
	public void endDocument() {
	}

	@Override
	public void endElement(final String tag) {
		if (this.token.length() > 0) {
			final String word = this.token.toString();
			switch (this.currElement) {
			case ELEM_CLASSES:
				this.consumer.addClass(word);
				break;
			case ELEM_EXCEPTIONS:
				this.exception.add(word);
				this.exception = normalizeException(this.exception);
				this.consumer.addException(getExceptionWord(this.exception),
						(ArrayList) this.exception.clone());
				break;
			case ELEM_PATTERNS:
				this.consumer.addPattern(getPattern(word),
						getInterletterValues(word));
				break;
			case ELEM_HYPHEN:
				// nothing to do
				break;
			}
			if (this.currElement != ELEM_HYPHEN) {
				this.token.setLength(0);
			}
		}
		if (this.currElement == ELEM_HYPHEN) {
			this.currElement = ELEM_EXCEPTIONS;
		} else {
			this.currElement = 0;
		}
	}

	@Override
	public void startDocument() {
	}

	@Override
	public void startElement(final String tag, final java.util.HashMap h) {
		if (tag.equals("hyphen-char")) {
			final String hh = (String) h.get("value");
			if (hh != null && hh.length() == 1) {
				this.hyphenChar = hh.charAt(0);
			}
		} else if (tag.equals("classes")) {
			this.currElement = ELEM_CLASSES;
		} else if (tag.equals("patterns")) {
			this.currElement = ELEM_PATTERNS;
		} else if (tag.equals("exceptions")) {
			this.currElement = ELEM_EXCEPTIONS;
			this.exception = new ArrayList();
		} else if (tag.equals("hyphen")) {
			if (this.token.length() > 0) {
				this.exception.add(this.token.toString());
			}
			this.exception.add(new Hyphen((String) h.get(HtmlTags.PRE), (String) h
					.get("no"), (String) h.get("post")));
			this.currElement = ELEM_HYPHEN;
		}
		this.token.setLength(0);
	}

	@Override
	public void text(final String str) {
		final StringTokenizer tk = new StringTokenizer(str);
		while (tk.hasMoreTokens()) {
			final String word = tk.nextToken();
			// System.out.println("\"" + word + "\"");
			switch (this.currElement) {
			case ELEM_CLASSES:
				this.consumer.addClass(word);
				break;
			case ELEM_EXCEPTIONS:
				this.exception.add(word);
				this.exception = normalizeException(this.exception);
				this.consumer.addException(getExceptionWord(this.exception),
						(ArrayList) this.exception.clone());
				this.exception.clear();
				break;
			case ELEM_PATTERNS:
				this.consumer.addPattern(getPattern(word),
						getInterletterValues(word));
				break;
			}
		}
	}

	// PatternConsumer implementation for testing purposes
	@Override
	public void addClass(final String c) {
		System.out.println("class: " + c);
	}

	@Override
	public void addException(final String w, final ArrayList e) {
		System.out.println("exception: " + w + " : " + e.toString());
	}

	@Override
	public void addPattern(final String p, final String v) {
		System.out.println("pattern: " + p + " : " + v);
	}

	public static void main(final String[] args) throws Exception {
		try {
			if (args.length > 0) {
				final SimplePatternParser pp = new SimplePatternParser();
				pp.parse(new FileInputStream(args[0]), pp);
			}
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}
}
