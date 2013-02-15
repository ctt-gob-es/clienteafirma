/*
 * $Id: Chunk.java 3427 2008-05-24 18:32:31Z xlv $
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

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;

import com.lowagie.text.pdf.HyphenationEvent;
import com.lowagie.text.pdf.PdfAction;
import com.lowagie.text.pdf.draw.DrawInterface;

/**
 * This is the smallest significant part of text that can be added to a
 * document.
 * <P>
 * Most elements can be divided in one or more <CODE>Chunk</CODE>s. A chunk
 * is a <CODE>String</CODE> with a certain <CODE>Font</CODE>. All other
 * layout parameters should be defined in the object to which this chunk of text
 * is added.
 * <P>
 * Example: <BLOCKQUOTE>
 *
 * <PRE>
 *
 * <STRONG>Chunk chunk = new Chunk("Hello world",
 * FontFactory.getFont(FontFactory.COURIER, 20, Font.ITALIC, new Color(255, 0,
 * 0))); </STRONG> document.add(chunk);
 *
 * </PRE>
 *
 * </BLOCKQUOTE>
 */

public class Chunk implements Element {

	// public static membervariables

	/** The character stand in for an image or a separator. */
	public static final String OBJECT_REPLACEMENT_CHARACTER = "\ufffc";

	/** This is a Chunk containing a newline. */
	public static final Chunk NEWLINE = new Chunk("\n");

	/** This is a Chunk containing a newpage. */
	private static final Chunk NEXTPAGE = new Chunk("");
	static {
		NEXTPAGE.setNewPage();
	}

	// member variables

	/** This is the content of this chunk of text. */
	private StringBuffer content = null;

	/** This is the <CODE>Font</CODE> of this chunk of text. */
	private Font font = null;

	/** Contains some of the attributes for this Chunk. */
	private HashMap attributes = null;

	// constructors

	/**
	 * Empty constructor.
	 */
	public Chunk() {
		this.content = new StringBuffer();
		this.font = new Font();
	}

	/**
	 * Constructs a chunk of text with a certain content and a certain <CODE>
	 * Font</CODE>.
	 *
	 * @param content
	 *            the content
	 * @param font
	 *            the font
	 */
	public Chunk(final String content, final Font font) {
		this.content = new StringBuffer(content);
		this.font = font;
	}

	/**
	 * Constructs a chunk of text with a certain content, without specifying a
	 * <CODE>Font</CODE>.
	 *
	 * @param content
	 *            the content
	 */
	public Chunk(final String content) {
		this(content, new Font());
	}

	/**
	 * Constructs a chunk of text with a char and a certain <CODE>Font</CODE>.
	 *
	 * @param c
	 *            the content
	 * @param font
	 *            the font
	 */
	private Chunk(final char c, final Font font) {
		this.content = new StringBuffer();
		this.content.append(c);
		this.font = font;
	}

	/**
	 * Constructs a chunk containing an <CODE>Image</CODE>.
	 *
	 * @param image
	 *            the image
	 * @param offsetX
	 *            the image offset in the x direction
	 * @param offsetY
	 *            the image offset in the y direction
	 */
	public Chunk(final Image image, final float offsetX, final float offsetY) {
		this(OBJECT_REPLACEMENT_CHARACTER, new Font());
		final Image copyImage = Image.getInstance(image);
		copyImage.setAbsolutePosition(Float.NaN, Float.NaN);
		setAttribute(IMAGE, new Object[] { copyImage, new Float(offsetX),
				new Float(offsetY), Boolean.FALSE });
	}

	/**
	 * Key for drawInterface of the Separator.
	 * @since	2.1.2
	 */
	public static final String SEPARATOR = "SEPARATOR";

	/**
	 * Creates a separator Chunk.
     * Note that separator chunks can't be used in combination with tab chunks!
	 * @param	separator	the drawInterface to use to draw the separator.
	 * @param	vertical	true if this is a vertical separator
	 * @since	2.1.2
	 */
	public Chunk(final DrawInterface separator, final boolean vertical) {
		this(OBJECT_REPLACEMENT_CHARACTER, new Font());
		setAttribute(SEPARATOR, new Object[] {separator, Boolean.valueOf(vertical)});
	}

	/**
	 * Key for drawInterface of the tab.
	 * @since	2.1.2
	 */
	public static final String TAB = "TAB";

	/**
	 * Creates a tab Chunk.
     * Note that separator chunks can't be used in combination with tab chunks!
	 * @param	separator	the drawInterface to use to draw the tab.
	 * @param	tabPosition	an X coordinate that will be used as start position for the next Chunk.
	 * @param	newline		if true, a newline will be added if the tabPosition has already been reached.
	 * @since	2.1.2
	 */
	private Chunk(final DrawInterface separator, final float tabPosition, final boolean newline) {
		this(OBJECT_REPLACEMENT_CHARACTER, new Font());
		if (tabPosition < 0) {
			throw new IllegalArgumentException("A tab position may not be lower than 0; yours is " + tabPosition);
		}
		setAttribute(TAB, new Object[] {separator, new Float(tabPosition), Boolean.valueOf(newline), new Float(0)});
	}

	/**
	 * Constructs a chunk containing an <CODE>Image</CODE>.
	 *
	 * @param image
	 *            the image
	 * @param offsetX
	 *            the image offset in the x direction
	 * @param offsetY
	 *            the image offset in the y direction
	 * @param changeLeading
	 *            true if the leading has to be adapted to the image
	 */
	public Chunk(final Image image, final float offsetX, final float offsetY,
			final boolean changeLeading) {
		this(OBJECT_REPLACEMENT_CHARACTER, new Font());
		setAttribute(IMAGE, new Object[] { image, new Float(offsetX),
				new Float(offsetY), Boolean.valueOf(changeLeading) });
	}

	// implementation of the Element-methods



	/**
	 * Gets the type of the text element.
	 *
	 * @return a type
	 */
	@Override
	public int type() {
		return Element.CHUNK;
	}

	/**
	 * Gets all the chunks in this element.
	 *
	 * @return an <CODE>ArrayList</CODE>
	 */
	@Override
	public ArrayList getChunks() {
		final ArrayList tmp = new ArrayList();
		tmp.add(this);
		return tmp;
	}

	// methods that change the member variables

	/**
	 * appends some text to this <CODE>Chunk</CODE>.
	 *
	 * @param string
	 *            <CODE>String</CODE>
	 * @return a <CODE>StringBuffer</CODE>
	 */
	public StringBuffer append(final String string) {
		return this.content.append(string);
	}

	/**
	 * Sets the font of this <CODE>Chunk</CODE>.
	 *
	 * @param font
	 *            a <CODE>Font</CODE>
	 */
	public void setFont(final Font font) {
		this.font = font;
	}

	// methods to retrieve information

	/**
	 * Gets the font of this <CODE>Chunk</CODE>.
	 *
	 * @return a <CODE>Font</CODE>
	 */
	public Font getFont() {
		return this.font;
	}

	/**
	 * Returns the content of this <CODE>Chunk</CODE>.
	 *
	 * @return a <CODE>String</CODE>
	 */
	public String getContent() {
		return this.content.toString();
	}

	/**
	 * Returns the content of this <CODE>Chunk</CODE>.
	 *
	 * @return a <CODE>String</CODE>
	 */
	@Override
	public String toString() {
		return getContent();
	}

	/**
	 * Checks is this <CODE>Chunk</CODE> is empty.
	 *
	 * @return <CODE>false</CODE> if the Chunk contains other characters than
	 *         space.
	 */
	public boolean isEmpty() {
		return this.content.toString().trim().length() == 0
				&& this.content.toString().indexOf("\n") == -1
				&& this.attributes == null;
	}

	/**
	 * Gets the width of the Chunk in points.
	 *
	 * @return a width in points
	 */
	public float getWidthPoint() {
		if (getImage() != null) {
			return getImage().getScaledWidth();
		}
		return this.font.getCalculatedBaseFont(true).getWidthPoint(getContent(),
				this.font.getCalculatedSize())
				* getHorizontalScaling();
	}

	// attributes

	/**
	 * Checks the attributes of this <CODE>Chunk</CODE>.
	 *
	 * @return false if there aren't any.
	 */

	boolean hasAttributes() {
		return this.attributes != null;
	}

	/**
	 * Gets the attributes for this <CODE>Chunk</CODE>.
	 * <P>
	 * It may be null.
	 *
	 * @return the attributes for this <CODE>Chunk</CODE>
	 */

	public HashMap getAttributes() {
		return this.attributes;
	}

	/**
	 * Sets the attributes all at once.
	 * @param	attributes	the attributes of a Chunk
	 */
	public void setAttributes(final HashMap attributes) {
		this.attributes = attributes;
	}

	/**
	 * Sets an arbitrary attribute.
	 *
	 * @param name
	 *            the key for the attribute
	 * @param obj
	 *            the value of the attribute
	 * @return this <CODE>Chunk</CODE>
	 */

	private Chunk setAttribute(final String name, final Object obj) {
		if (this.attributes == null) {
			this.attributes = new HashMap();
		}
		this.attributes.put(name, obj);
		return this;
	}

	// the attributes are ordered as they appear in the book 'iText in Action'

	/** Key for text horizontal scaling. */
	public static final String HSCALE = "HSCALE";

	/**
	 * Gets the horizontal scaling.
	 *
	 * @return a percentage in float
	 */
	public float getHorizontalScaling() {
		if (this.attributes == null) {
			return 1f;
		}
		final Float f = (Float) this.attributes.get(HSCALE);
		if (f == null) {
			return 1f;
		}
		return f.floatValue();
	}

	/** Key for underline. */
	public static final String UNDERLINE = "UNDERLINE";

	/**
	 * Sets an horizontal line that can be an underline or a strikethrough.
	 * Actually, the line can be anywhere vertically and has always the <CODE>
	 * Chunk</CODE> width. Multiple call to this method will produce multiple
	 * lines.
	 *
	 * @param color
	 *            the color of the line or <CODE>null</CODE> to follow the
	 *            text color
	 * @param thickness
	 *            the absolute thickness of the line
	 * @param thicknessMul
	 *            the thickness multiplication factor with the font size
	 * @param yPosition
	 *            the absolute y position relative to the baseline
	 * @param yPositionMul
	 *            the position multiplication factor with the font size
	 * @param cap
	 *            the end line cap. Allowed values are
	 *            PdfContentByte.LINE_CAP_BUTT, PdfContentByte.LINE_CAP_ROUND
	 *            and PdfContentByte.LINE_CAP_PROJECTING_SQUARE
	 * @return this <CODE>Chunk</CODE>
	 */
	private Chunk setUnderline(final Color color, final float thickness, final float thicknessMul,
			final float yPosition, final float yPositionMul, final int cap) {
		if (this.attributes == null) {
			this.attributes = new HashMap();
		}
		final Object obj[] = {
				color,
				new float[] { thickness, thicknessMul, yPosition, yPositionMul, cap } };
		final Object unders[][] = Utilities.addToArray((Object[][]) this.attributes.get(UNDERLINE),
				obj);
		return setAttribute(UNDERLINE, unders);
	}

	/** Key for sub/superscript. */
	public static final String SUBSUPSCRIPT = "SUBSUPSCRIPT";

	/**
	 * Sets the text displacement relative to the baseline. Positive values rise
	 * the text, negative values lower the text.
	 * <P>
	 * It can be used to implement sub/superscript.
	 *
	 * @param rise
	 *            the displacement in points
	 * @return this <CODE>Chunk</CODE>
	 */

	public Chunk setTextRise(final float rise) {
		return setAttribute(SUBSUPSCRIPT, new Float(rise));
	}

	/**
	 * Gets the text displacement relative to the baseline.
	 *
	 * @return a displacement in points
	 */
	public float getTextRise() {
		if (this.attributes != null && this.attributes.containsKey(SUBSUPSCRIPT)) {
			final Float f = (Float) this.attributes.get(SUBSUPSCRIPT);
			return f.floatValue();
		}
		return 0.0f;
	}

	/** Key for text skewing. */
	public static final String SKEW = "SKEW";

	/** Key for background. */
	public static final String BACKGROUND = "BACKGROUND";

	/**
	 * Sets the color of the background <CODE>Chunk</CODE>.
	 *
	 * @param color
	 *            the color of the background
	 * @return this <CODE>Chunk</CODE>
	 */
	public Chunk setBackground(final Color color) {
		return setBackground(color, 0, 0, 0, 0);
	}

	/**
	 * Sets the color and the size of the background <CODE>Chunk</CODE>.
	 *
	 * @param color
	 *            the color of the background
	 * @param extraLeft
	 *            increase the size of the rectangle in the left
	 * @param extraBottom
	 *            increase the size of the rectangle in the bottom
	 * @param extraRight
	 *            increase the size of the rectangle in the right
	 * @param extraTop
	 *            increase the size of the rectangle in the top
	 * @return this <CODE>Chunk</CODE>
	 */
	private Chunk setBackground(final Color color, final float extraLeft, final float extraBottom,
			final float extraRight, final float extraTop) {
		return setAttribute(BACKGROUND, new Object[] { color,
				new float[] { extraLeft, extraBottom, extraRight, extraTop } });
	}

	/** Key for text rendering mode. */
	public static final String TEXTRENDERMODE = "TEXTRENDERMODE";

	/** Key for split character. */
	public static final String SPLITCHARACTER = "SPLITCHARACTER";

	/** Key for hyphenation. */
	public static final String HYPHENATION = "HYPHENATION";

	/**
	 * sets the hyphenation engine to this <CODE>Chunk</CODE>.
	 *
	 * @param hyphenation
	 *            the hyphenation engine
	 * @return this <CODE>Chunk</CODE>
	 */
	Chunk setHyphenation(final HyphenationEvent hyphenation) {
		return setAttribute(HYPHENATION, hyphenation);
	}

	/** Key for remote goto. */
	public static final String REMOTEGOTO = "REMOTEGOTO";

	/**
	 * Sets a goto for a remote destination for this <CODE>Chunk</CODE>.
	 *
	 * @param filename
	 *            the file name of the destination document
	 * @param name
	 *            the name of the destination to go to
	 * @return this <CODE>Chunk</CODE>
	 */

	public Chunk setRemoteGoto(final String filename, final String name) {
		return setAttribute(REMOTEGOTO, new Object[] { filename, name });
	}

	/**
	 * Sets a goto for a remote destination for this <CODE>Chunk</CODE>.
	 *
	 * @param filename
	 *            the file name of the destination document
	 * @param page
	 *            the page of the destination to go to. First page is 1
	 * @return this <CODE>Chunk</CODE>
	 */

	public Chunk setRemoteGoto(final String filename, final int page) {
		return setAttribute(REMOTEGOTO, new Object[] { filename,
				new Integer(page) });
	}

	/** Key for local goto. */
	public static final String LOCALGOTO = "LOCALGOTO";

	/**
	 * Sets a local goto for this <CODE>Chunk</CODE>.
	 * <P>
	 * There must be a local destination matching the name.
	 *
	 * @param name
	 *            the name of the destination to go to
	 * @return this <CODE>Chunk</CODE>
	 */

	public Chunk setLocalGoto(final String name) {
		return setAttribute(LOCALGOTO, name);
	}

	/** Key for local destination. */
	public static final String LOCALDESTINATION = "LOCALDESTINATION";

	/**
	 * Sets a local destination for this <CODE>Chunk</CODE>.
	 *
	 * @param name
	 *            the name for this destination
	 * @return this <CODE>Chunk</CODE>
	 */
	public Chunk setLocalDestination(final String name) {
		return setAttribute(LOCALDESTINATION, name);
	}

	/** Key for generic tag. */
	public static final String GENERICTAG = "GENERICTAG";

	/**
	 * Sets the generic tag <CODE>Chunk</CODE>.
	 * <P>
	 * The text for this tag can be retrieved with <CODE>PdfPageEvent</CODE>.
	 *
	 * @param text
	 *            the text for the tag
	 * @return this <CODE>Chunk</CODE>
	 */

	public Chunk setGenericTag(final String text) {
		return setAttribute(GENERICTAG, text);
	}

	/** Key for image. */
	public static final String IMAGE = "IMAGE";

	/**
	 * Returns the image.
	 *
	 * @return the image
	 */

	public Image getImage() {
		if (this.attributes == null) {
			return null;
		}
		final Object obj[] = (Object[]) this.attributes.get(Chunk.IMAGE);
		if (obj == null) {
			return null;
		} else {
			return (Image) obj[0];
		}
	}

	/** Key for Action. */
	public static final String ACTION = "ACTION";

	/**
	 * Sets an anchor for this <CODE>Chunk</CODE>.
	 *
	 * @param url
	 *            the url to link to
	 * @return this <CODE>Chunk</CODE>
	 */

	Chunk setAnchor(final String url) {
		return setAttribute(ACTION, new PdfAction(url));
	}

	/** Key for newpage. */
	public static final String NEWPAGE = "NEWPAGE";

	/**
	 * Sets a new page tag..
	 *
	 * @return this <CODE>Chunk</CODE>
	 */

	public Chunk setNewPage() {
		return setAttribute(NEWPAGE, null);
	}

	/** Key for annotation. */
	public static final String PDFANNOTATION = "PDFANNOTATION";

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

	/**
     * Returns the hyphenation (if present).
     * @since	2.1.2
	 */
    public HyphenationEvent getHyphenation() {
        if (this.attributes == null) {
			return null;
		}
        return (HyphenationEvent) this.attributes.get(Chunk.HYPHENATION);
	}

	// keys used in PdfChunk

	/** Key for color. */
	public static final String COLOR = "COLOR";

	/** Key for encoding. */
	public static final String ENCODING = "ENCODING";

}