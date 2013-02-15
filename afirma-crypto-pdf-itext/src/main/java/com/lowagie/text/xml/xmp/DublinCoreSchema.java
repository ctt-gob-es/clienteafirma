/*
 * $Id: DublinCoreSchema.java 3596 2008-10-10 11:37:58Z psoares33 $
 *
 * Copyright 2005 by Bruno Lowagie.
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
 * the Initial Developer are Copyright (C) 1999-2005 by Bruno Lowagie.
 * All Rights Reserved.
 * Co-Developer of the code is Paulo Soares. Portions created by the Co-Developer
 * are Copyright (C) 2000-2005 by Paulo Soares. All Rights Reserved.
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
 * of this file under either the MPL or the GNU LIBRARY GENERAL PUBLIC LICENSE
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the MPL as stated above or under the terms of the GNU
 * Library General Public License as published by the Free Software Foundation;
 * either version 2 of the License, or any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU LIBRARY GENERAL PUBLIC LICENSE for more
 * details.
 *
 * If you didn't download this code from the following link, you should check if
 * you aren't using an obsolete version:
 * http://www.lowagie.com/iText/
 */

package com.lowagie.text.xml.xmp;


/**
 * An implementation of an XmpSchema.
 */
class DublinCoreSchema extends XmpSchema {

	private static final long serialVersionUID = -4551741356374797330L;
	/** default namespace identifier*/
	private static final String DEFAULT_XPATH_ID = "dc";
	/** default namespace uri*/
	private static final String DEFAULT_XPATH_URI = "http://purl.org/dc/elements/1.1/";



	/** The authors of the resource (listed in order of precedence, if significant). */
	private static final String CREATOR = "dc:creator";

	/** A textual description of the content of the resource. Multiple values may be present for different languages. */
	private static final String DESCRIPTION = "dc:description";
	/** The file format used when saving the resource. Tools and applications should set this property to the save format of the data. It may include appropriate qualifiers. */
	private static final String FORMAT = "dc:format";






	/** An unordered array of descriptive phrases or keywords that specify the topic of the content of the resource. */
	private static final String SUBJECT = "dc:subject";
	/** The title of the document, or the name given to the resource. Typically, it will be a name by which the resource is formally known. */
	private static final String TITLE = "dc:title";



	public DublinCoreSchema() {
		super("xmlns:" + DEFAULT_XPATH_ID + "=\"" + DEFAULT_XPATH_URI + "\"");
		setProperty(FORMAT, "application/pdf");
	}

	/**
	 * Adds a title.
	 * @param title
	 */
	public void addTitle(final String title) {
		final XmpArray array = new XmpArray(XmpArray.ALTERNATIVE);
		array.add(title);
		setProperty(TITLE, array);
	}

	/**
	 * Adds a description.
	 * @param desc
	 */
	public void addDescription(final String desc) {
		final XmpArray array = new XmpArray(XmpArray.ALTERNATIVE);
		array.add(desc);
		setProperty(DESCRIPTION, array);
	}

	/**
	 * Adds a subject.
	 * @param subject
	 */
	void addSubject(final String subject) {
		final XmpArray array = new XmpArray(XmpArray.UNORDERED);
		array.add(subject);
		setProperty(SUBJECT, array);
	}




	/**
	 * Adds a single author.
	 * @param author
	 */
	void addAuthor(final String author) {
		final XmpArray array = new XmpArray(XmpArray.ORDERED);
		array.add(author);
		setProperty(CREATOR, array);
	}






}
