/*
 *
 * Copyright 2004 by Leonard Rosenthol.
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

package com.lowagie.text.pdf;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Stack;

import com.lowagie.text.xml.simpleparser.SimpleXMLDocHandler;

/**
 * Reads a XFDF.
 * @author Leonard Rosenthol (leonardr@pdfsages.com)
 */
class XfdfReader implements SimpleXMLDocHandler {
	// stuff used during parsing to handle state
	private boolean foundRoot = false;
    private final Stack fieldNames = new Stack();
    private final Stack fieldValues = new Stack();

    // storage for the field list and their values
	private HashMap	fields;
	/**
	 * Storage for field values if there's more than one value for a field.
	 * @since	2.1.4
	 */
	private HashMap listFields;

	// storage for the path to referenced PDF, if any
	private String	fileSpec;





    /** Gets all the fields. The map is keyed by the fully qualified
     * field name and the value is a merged <CODE>PdfDictionary</CODE>
     * with the field content.
     * @return all the fields
     */
    public HashMap getFields() {
        return this.fields;
    }



    /** Gets the field value or <CODE>null</CODE> if the field does not
     * exist or has no value defined.
     * @param name the fully qualified field name
     * @return the field value or <CODE>null</CODE>
     */
    String getFieldValue(final String name) {
        final String field = (String)this.fields.get(name);
        if (field == null) {
			return null;
		} else {
			return field;
		}
    }

    /**
     * Gets the field values for a list or <CODE>null</CODE> if the field does not
     * exist or has no value defined.
     * @param name the fully qualified field name
     * @return the field values or <CODE>null</CODE>
     * @since	2.1.4
     */
    List getListValues(final String name) {
        return (List)this.listFields.get(name);
    }

    /** Gets the PDF file specification contained in the FDF.
     * @return the PDF file specification contained in the FDF
     */
    public String getFileSpec() {
        return this.fileSpec;
    }

    /**
     * Called when a start tag is found.
     * @param tag the tag name
     * @param h the tag's attributes
     */
    @Override
	public void startElement(final String tag, final HashMap h)
    {
        if ( !this.foundRoot ) {
            if (!tag.equals("xfdf")) {
				throw new RuntimeException("Root element is not Bookmark.");
			} else {
				this.foundRoot = true;
			}
        }

        if ( tag.equals("xfdf") ){

    	} else if ( tag.equals("f") ) {
    		this.fileSpec = (String)h.get( "href" );
    	} else if ( tag.equals("fields") ) {
            this.fields = new HashMap();		// init it!
            this.listFields = new HashMap();
    	} else if ( tag.equals("field") ) {
    		final String	fName = (String) h.get( "name" );
    		this.fieldNames.push( fName );
    	} else if ( tag.equals("value") ) {
    		this.fieldValues.push( "" );
    	}
    }
    /**
     * Called when an end tag is found.
     * @param tag the tag name
     */
    @Override
	public void endElement(final String tag) {
        if ( tag.equals("value") ) {
            String	fName = "";
            for (int k = 0; k < this.fieldNames.size(); ++k) {
                fName += "." + (String)this.fieldNames.elementAt(k);
            }
            if (fName.startsWith(".")) {
				fName = fName.substring(1);
			}
            final String fVal = (String) this.fieldValues.pop();
            final String old = (String) this.fields.put( fName, fVal );
            if (old != null) {
            	List l = (List) this.listFields.get(fName);
            	if (l == null) {
            		l = new ArrayList();
            		l.add(old);
            	}
            	l.add(fVal);
            	this.listFields.put(fName, l);
            }
        }
        else if (tag.equals("field") ) {
            if (!this.fieldNames.isEmpty()) {
				this.fieldNames.pop();
			}
        }
    }

    /**
     * Called when the document starts to be parsed.
     */
    @Override
	public void startDocument()
    {
        this.fileSpec = "";
    }
    /**
     * Called after the document is parsed.
     */
    @Override
	public void endDocument()
	{

	}
    /**
     * Called when a text element is found.
     * @param str the text element, probably a fragment.
     */
    @Override
	public void text(final String str)
    {
        if (this.fieldNames.isEmpty() || this.fieldValues.isEmpty()) {
			return;
		}

        String val = (String)this.fieldValues.pop();
        val += str;
        this.fieldValues.push(val);
    }
}