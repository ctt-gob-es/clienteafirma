/*
 * $Id: Annotation.java 3373 2008-05-12 16:21:24Z xlv $
 *
 * Copyright 1999, 2000, 2001, 2002 by Bruno Lowagie.
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

import java.util.ArrayList;
import java.util.HashMap;

/**
 * An <CODE>Annotation</CODE> is a little note that can be added to a page on
 * a document.
 *
 * @see Element
 * @see Anchor
 */

public class Annotation implements Element {

	// membervariables

	/** This is a possible annotation type. */
	private static final int TEXT = 0;

	/** This is a possible annotation type. */
	public static final int URL_NET = 1;

	/** This is a possible annotation type. */
	public static final int URL_AS_STRING = 2;

	/** This is a possible annotation type. */
	public static final int FILE_DEST = 3;

	/** This is a possible annotation type. */
	public static final int FILE_PAGE = 4;

	/** This is a possible annotation type. */
	public static final int NAMED_DEST = 5;

	/** This is a possible annotation type. */
	public static final int LAUNCH = 6;

	/** This is a possible annotation type. */
	public static final int SCREEN = 7;

	/** This is a possible attribute. */
	private static final String TITLE = "title";

	/** This is a possible attribute. */
	private static final String CONTENT = "content";

	/** This is a possible attribute. */
	public static final String URL = "url";

	/** This is a possible attribute. */
	public static final String FILE = "file";

	/** This is a possible attribute. */
	public static final String DESTINATION = "destination";

	/** This is a possible attribute. */
	public static final String PAGE = "page";

	/** This is a possible attribute. */
	public static final String NAMED = "named";

	/** This is a possible attribute. */
	public static final String APPLICATION = "application";

	/** This is a possible attribute. */
	public static final String PARAMETERS = "parameters";

	/** This is a possible attribute. */
	public static final String OPERATION = "operation";

	/** This is a possible attribute. */
	public static final String DEFAULTDIR = "defaultdir";

	/** This is a possible attribute. */
	public static final String MIMETYPE = "mime";

	/** This is the type of annotation. */
	private int annotationtype;

	/** This is the title of the <CODE>Annotation</CODE>. */
	private HashMap annotationAttributes = new HashMap();

	/** This is the lower left x-value */
	private float llx = Float.NaN;

	/** This is the lower left y-value */
	private float lly = Float.NaN;

	/** This is the upper right x-value */
	private float urx = Float.NaN;

	/** This is the upper right y-value */
	private float ury = Float.NaN;

	// constructors

	/**
	 * Constructs an <CODE>Annotation</CODE> with a certain title and some
	 * text.
	 *
	 * @param llx
	 *            lower left x coordinate
	 * @param lly
	 *            lower left y coordinate
	 * @param urx
	 *            upper right x coordinate
	 * @param ury
	 *            upper right y coordinate
	 */
	private Annotation(final float llx, final float lly, final float urx, final float ury) {
		this.llx = llx;
		this.lly = lly;
		this.urx = urx;
		this.ury = ury;
	}

	/**
	 * Copy constructor.
	 */
    public Annotation(final Annotation an) {
        this.annotationtype = an.annotationtype;
        this.annotationAttributes = an.annotationAttributes;
        this.llx = an.llx;
        this.lly = an.lly;
        this.urx = an.urx;
        this.ury = an.ury;
    }

	/**
	 * Constructs an <CODE>Annotation</CODE> with a certain title and some
	 * text.
	 *
	 * @param title
	 *            the title of the annotation
	 * @param text
	 *            the content of the annotation
	 * @param llx
	 *            the lower left x-value
	 * @param lly
	 *            the lower left y-value
	 * @param urx
	 *            the upper right x-value
	 * @param ury
	 *            the upper right y-value
	 */
	public Annotation(final String title, final String text, final float llx, final float lly,
			final float urx, final float ury) {
		this(llx, lly, urx, ury);
		this.annotationtype = TEXT;
		this.annotationAttributes.put(TITLE, title);
		this.annotationAttributes.put(CONTENT, text);
	}

	/**
	 * Constructs an <CODE>Annotation</CODE>.
	 *
	 * @param llx
	 *            the lower left x-value
	 * @param lly
	 *            the lower left y-value
	 * @param urx
	 *            the upper right x-value
	 * @param ury
	 *            the upper right y-value
	 * @param url
	 *            the external reference
	 */
	public Annotation(final float llx, final float lly, final float urx, final float ury, final String url) {
		this(llx, lly, urx, ury);
		this.annotationtype = URL_AS_STRING;
		this.annotationAttributes.put(FILE, url);
	}

	/**
	 * Constructs an <CODE>Annotation</CODE>.
	 *
	 * @param llx
	 *            the lower left x-value
	 * @param lly
	 *            the lower left y-value
	 * @param urx
	 *            the upper right x-value
	 * @param ury
	 *            the upper right y-value
	 * @param file
	 *            an external PDF file
	 * @param dest
	 *            the destination in this file
	 */
	public Annotation(final float llx, final float lly, final float urx, final float ury, final String file,
			final String dest) {
		this(llx, lly, urx, ury);
		this.annotationtype = FILE_DEST;
		this.annotationAttributes.put(FILE, file);
		this.annotationAttributes.put(DESTINATION, dest);
	}

	/**
	 * Constructs an <CODE>Annotation</CODE>.
	 *
	 * @param llx
	 *            the lower left x-value
	 * @param lly
	 *            the lower left y-value
	 * @param urx
	 *            the upper right x-value
	 * @param ury
	 *            the upper right y-value
	 * @param file
	 *            an external PDF file
	 * @param page
	 *            a page number in this file
	 */
	public Annotation(final float llx, final float lly, final float urx, final float ury, final String file,
			final int page) {
		this(llx, lly, urx, ury);
		this.annotationtype = FILE_PAGE;
		this.annotationAttributes.put(FILE, file);
		this.annotationAttributes.put(PAGE, new Integer(page));
	}

	/**
	 * Constructs an <CODE>Annotation</CODE>.
	 *
	 * @param llx
	 *            the lower left x-value
	 * @param lly
	 *            the lower left y-value
	 * @param urx
	 *            the upper right x-value
	 * @param ury
	 *            the upper right y-value
	 * @param named
	 *            a named destination in this file
	 */
	public Annotation(final float llx, final float lly, final float urx, final float ury, final int named) {
		this(llx, lly, urx, ury);
		this.annotationtype = NAMED_DEST;
		this.annotationAttributes.put(NAMED, new Integer(named));
	}

	// implementation of the Element-methods

	/**
	 * Gets the type of the text element.
	 *
	 * @return a type
	 */
	@Override
	public int type() {
		return Element.ANNOTATION;
	}



	/**
	 * Gets all the chunks in this element.
	 *
	 * @return an <CODE>ArrayList</CODE>
	 */

	@Override
	public ArrayList getChunks() {
		return new ArrayList();
	}

	// methods

	/**
	 * Sets the dimensions of this annotation.
	 *
	 * @param llx
	 *            the lower left x-value
	 * @param lly
	 *            the lower left y-value
	 * @param urx
	 *            the upper right x-value
	 * @param ury
	 *            the upper right y-value
	 */
	public void setDimensions(final float llx, final float lly, final float urx, final float ury) {
		this.llx = llx;
		this.lly = lly;
		this.urx = urx;
		this.ury = ury;
	}

	// methods to retrieve information

	/**
	 * Returns the lower left x-value.
	 *
	 * @return a value
	 */
	public float llx() {
		return this.llx;
	}

	/**
	 * Returns the lower left y-value.
	 *
	 * @return a value
	 */
	public float lly() {
		return this.lly;
	}

	/**
	 * Returns the upper right x-value.
	 *
	 * @return a value
	 */
	public float urx() {
		return this.urx;
	}

	/**
	 * Returns the upper right y-value.
	 *
	 * @return a value
	 */
	public float ury() {
		return this.ury;
	}









	/**
	 * Returns the type of this <CODE>Annotation</CODE>.
	 *
	 * @return a type
	 */
	public int annotationType() {
		return this.annotationtype;
	}

	/**
	 * Returns the title of this <CODE>Annotation</CODE>.
	 *
	 * @return a name
	 */
	public String title() {
		String s = (String) this.annotationAttributes.get(TITLE);
		if (s == null) {
			s = "";
		}
		return s;
	}

	/**
	 * Gets the content of this <CODE>Annotation</CODE>.
	 *
	 * @return a reference
	 */
	public String content() {
		String s = (String) this.annotationAttributes.get(CONTENT);
		if (s == null) {
			s = "";
		}
		return s;
	}

	/**
	 * Gets the content of this <CODE>Annotation</CODE>.
	 *
	 * @return a reference
	 */
	public HashMap attributes() {
		return this.annotationAttributes;
	}

	/**
	 * @see com.lowagie.text.Element#isContent()
	 * @since	iText 2.0.8
	 */
	@Override
	public boolean isContent() {
		return true;
	}

	/**
	 * @see com.lowagie.text.Element#isNestable()
	 * @since	iText 2.0.8
	 */
	@Override
	public boolean isNestable() {
		return true;
	}

}