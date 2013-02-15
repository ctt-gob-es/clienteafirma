/*
 * $Id: HtmlTags.java 3533 2008-07-07 21:27:13Z Howard_s $
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

/**
 * A class that contains all the possible tagnames and their attributes.
 */

public class HtmlTags {

	/** the root tag. */
	static final String HTML = "html";

	// Phrases, Anchors, Lists and Paragraphs

	/** the chunk tag */
	static final String CHUNK = "font";

	/** the phrase tag */
	static final String CODE = "code";

	/** the phrase tag */
	static final String VAR = "var";

	/** the anchor tag */
	static final String ANCHOR = "a";

	/** the list tag */
	static final String ORDEREDLIST = "ol";

	/** the list tag */
	static final String UNORDEREDLIST = "ul";

	/** the listitem tag */
	static final String LISTITEM = "li";

	/** the paragraph tag */
	static final String PARAGRAPH = "p";

	/** attribute of anchor tag */
	static final String NAME = "name";

	/** attribute of anchor tag */
	static final String REFERENCE = "href";

	/** attribute of anchor tag */
	static final String[] H = new String[6];
	static {
		H[0] = "h1";
		H[1] = "h2";
		H[2] = "h3";
		H[3] = "h4";
		H[4] = "h5";
		H[5] = "h6";
	}

	// Chunks

	/** attribute of the chunk tag */
	static final String FONT = "face";

	/** attribute of the chunk tag */
	static final String SIZE = "point-size";

	/** attribute of the chunk/table/cell tag */
	static final String COLOR = "color";

	/** some phrase tag */
	static final String EM = "em";

	/** some phrase tag */
	static final String I = "i";

	/** some phrase tag */
	static final String STRONG = "strong";

	/** some phrase tag */
	static final String B = "b";

	/** some phrase tag */
	static final String S = "s";

	/** some phrase tag */
	static final String U = "u";

	/** some phrase tag */
	static final String SUB = "sub";

	/** some phrase tag */
	static final String SUP = "sup";

	/** the possible value of a tag */
	static final String HORIZONTALRULE = "hr";

	// tables/cells

	/** the table tag */
	static final String TABLE = "table";

	/** the cell tag */
	static final String ROW = "tr";

	/** the cell tag */
	static final String CELL = "td";

	/** attribute of the cell tag */
	static final String HEADERCELL = "th";

	/** attribute of the table tag */
	static final String COLUMNS = "cols";

	/** attribute of the table tag */
	static final String CELLPADDING = "cellpadding";

	/** attribute of the table tag */
	static final String CELLSPACING = "cellspacing";

	/** attribute of the cell tag */
	static final String COLSPAN = "colspan";

	/** attribute of the cell tag */
	static final String ROWSPAN = "rowspan";

	/** attribute of the cell tag */
	static final String NOWRAP = "nowrap";

	/** attribute of the table/cell tag */
	static final String BORDERWIDTH = "border";

	/** attribute of the table/cell tag */
	static final String WIDTH = "width";

	/** attribute of the table/cell tag */
	static final String BACKGROUNDCOLOR = "bgcolor";

	/** attribute of the table/cell tag */
	static final String BORDERCOLOR = "bordercolor";

	/** attribute of paragraph/image/table tag */
	static final String ALIGN = "align";

	/** attribute of the cell tag */
	static final String HORIZONTALALIGN = "align";

	/** attribute of the cell tag */
	static final String VERTICALALIGN = "valign";

	// Misc

	/** the image tag */
	static final String IMAGE = "img";



	/** attribute of the image tag */
	static final String ALT = "alt";

	/** attribute of the image tag */
	static final String PLAINWIDTH = "width";

	/** attribute of the image tag */
	static final String PLAINHEIGHT = "height";

	/** the newpage tag */
	static final String NEWLINE = "br";

	/** The DIV tag. */
	static final String DIV = "div";

	/** The SPAN tag. */
	static final String SPAN = "span";

	/** This is a possible HTML attribute for auto-formated
     * @since 2.1.3
     */
	public static final String PRE = "pre";
}