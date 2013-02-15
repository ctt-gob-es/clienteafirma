/*
 * $Id: XmpBasicSchema.java 3373 2008-05-12 16:21:24Z xlv $
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
class XmpBasicSchema extends XmpSchema {

	private static final long serialVersionUID = -2416613941622479298L;
	/** default namespace identifier*/
	private static final String DEFAULT_XPATH_ID = "xmp";
	/** default namespace uri*/
	private static final String DEFAULT_XPATH_URI = "http://ns.adobe.com/xap/1.0/";



	/** The date and time the resource was originally created. */
	private static final String CREATEDATE = "xmp:CreateDate";
	/** The name of the first known tool used to create the resource. If history is present in the metadata, this value should be equivalent to that of xmpMM:History's softwareAgent property. */
	private static final String CREATORTOOL = "xmp:CreatorTool";
	/** An unordered array of text strings that unambiguously identify the resource within a given context. */
	private static final String IDENTIFIER = "xmp:Identifier";
	/** The date and time that any metadata for this resource was last changed. */
	private static final String METADATADATE = "xmp:MetadataDate";
	/** The date and time the resource was last modified. */
	private static final String MODIFYDATE = "xmp:ModifyDate";
	/** A short informal name for the resource. */
	private static final String NICKNAME = "xmp:Nickname";



	public XmpBasicSchema() {
		super("xmlns:" + DEFAULT_XPATH_ID + "=\"" + DEFAULT_XPATH_URI + "\"");
	}

	/**
	 * Adds the creatortool.
	 * @param creator
	 */
	void addCreatorTool(final String creator) {
		setProperty(CREATORTOOL, creator);
	}

	/**
	 * Adds the creation date.
	 * @param date
	 */
	void addCreateDate(final String date) {
		setProperty(CREATEDATE, date);
	}

	/**
	 * Adds the modification date.
	 * @param date
	 */
	void addModDate(final String date) {
		setProperty(MODIFYDATE, date);
	}


}
