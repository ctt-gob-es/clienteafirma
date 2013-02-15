/*
 * $Id: XmpWriter.java 3949 2009-06-03 15:19:04Z blowagie $
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

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import com.lowagie.text.pdf.PdfDate;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfString;
import com.lowagie.text.pdf.PdfWriter;

/**
 * With this class you can create an Xmp Stream that can be used for adding
 * Metadata to a PDF Dictionary. Remark that this class doesn't cover the
 * complete XMP specification.
 */
public class XmpWriter {

	/** A possible charset for the XMP. */
	private static final String UTF8 = "UTF-8";

	/** String used to fill the extra space. */
	static final String EXTRASPACE = "                                                                                                   \n";

	/** You can add some extra space in the XMP packet; 1 unit in this variable represents 100 spaces and a newline. */
	private final int extraSpace;

	/** The writer to which you can write bytes for the XMP stream. */
	private final OutputStreamWriter writer;

	/** The about string that goes into the rdf:Description tags. */
	private String about;

	/**
	 * Processing Instruction required at the start of an XMP stream
	 * @since iText 2.1.6
	 */
	static final String XPACKET_PI_BEGIN = "<?xpacket begin=\"\uFEFF\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>\n";

	/**
	 * Processing Instruction required at the end of an XMP stream for XMP streams that can be updated
	 * @since iText 2.1.6
	 */
	static final String XPACKET_PI_END_W = "<?xpacket end=\"w\"?>";

	/**
	 * Processing Instruction required at the end of an XMP stream for XMP streams that are read only
	 * @since iText 2.1.6
	 */
	private static final String XPACKET_PI_END_R = "<?xpacket end=\"r\"?>";

	/** The end attribute. */
	private final char end = 'w';

	/**
	 * Creates an XmpWriter.
	 * @param os
	 * @param utfEncoding
	 * @param extraSpace
	 * @throws IOException
	 */
	private XmpWriter(final OutputStream os, final String utfEncoding, final int extraSpace) throws IOException {
		this.extraSpace = extraSpace;
		this.writer = new OutputStreamWriter(os, utfEncoding);
		this.writer.write(XPACKET_PI_BEGIN);
		this.writer.write("<x:xmpmeta xmlns:x=\"adobe:ns:meta/\">\n");
		this.writer.write("<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\n");
		this.about = "";
	}

	/**
	 * Creates an XmpWriter.
	 * @param os
	 * @throws IOException
	 */
	private XmpWriter(final OutputStream os) throws IOException {
		this(os, UTF8, 20);
	}



	/**
	 * @param about The about to set.
	 */
	public void setAbout(final String about) {
		this.about = about;
	}



	/**
	 * Adds an rdf:Description.
	 * @param s
	 * @throws IOException
	 */
	private void addRdfDescription(final XmpSchema s) throws IOException {
		this.writer.write("<rdf:Description rdf:about=\"");
		this.writer.write(this.about);
		this.writer.write("\" ");
		this.writer.write(s.getXmlns());
		this.writer.write(">");
		this.writer.write(s.toString());
		this.writer.write("</rdf:Description>\n");
	}

	/**
	 * Flushes and closes the XmpWriter.
	 * @throws IOException
	 */
	public void close() throws IOException {
		this.writer.write("</rdf:RDF>");
		this.writer.write("</x:xmpmeta>\n");
		for (int i = 0; i < this.extraSpace; i++) {
			this.writer.write(EXTRASPACE);
		}
		this.writer.write(this.end == 'r' ? XPACKET_PI_END_R : XPACKET_PI_END_W);
		this.writer.flush();
		this.writer.close();
	}

    /**
     * @param os
     * @param info
     * @throws IOException
     */
    public XmpWriter(final OutputStream os, final PdfDictionary info, final int PdfXConformance) throws IOException {
        this(os);
        if (info != null) {
        	final DublinCoreSchema dc = new DublinCoreSchema();
        	final PdfSchema p = new PdfSchema();
        	final XmpBasicSchema basic = new XmpBasicSchema();
        	PdfName key;
        	PdfObject obj;
        	for (final Object element : info.getKeys()) {
        		key = (PdfName)element;
        		obj = info.get(key);
        		if (obj == null) {
					continue;
				}
        		if (PdfName.TITLE.equals(key)) {
        			dc.addTitle(((PdfString)obj).toUnicodeString());
        		}
        		if (PdfName.AUTHOR.equals(key)) {
        			dc.addAuthor(((PdfString)obj).toUnicodeString());
        		}
        		if (PdfName.SUBJECT.equals(key)) {
        			dc.addSubject(((PdfString)obj).toUnicodeString());
        			dc.addDescription(((PdfString)obj).toUnicodeString());
        		}
        		if (PdfName.KEYWORDS.equals(key)) {
        			p.addKeywords(((PdfString)obj).toUnicodeString());
        		}
        		if (PdfName.CREATOR.equals(key)) {
        			basic.addCreatorTool(((PdfString)obj).toUnicodeString());
        		}
        		if (PdfName.PRODUCER.equals(key)) {
        			p.addProducer(((PdfString)obj).toUnicodeString());
        		}
        		if (PdfName.CREATIONDATE.equals(key)) {
        			basic.addCreateDate(((PdfDate)obj).getW3CDate());
        		}
        		if (PdfName.MODDATE.equals(key)) {
        			basic.addModDate(((PdfDate)obj).getW3CDate());
        		}
        	}
        	if (dc.size() > 0) {
				addRdfDescription(dc);
			}
        	if (p.size() > 0) {
				addRdfDescription(p);
			}
        	if (basic.size() > 0) {
				addRdfDescription(basic);
			}
            if (PdfXConformance == PdfWriter.PDFA1A || PdfXConformance == PdfWriter.PDFA1B) {
                final PdfA1Schema a1 = new PdfA1Schema();
                if (PdfXConformance == PdfWriter.PDFA1A) {
					a1.addConformance("A");
				} else {
					a1.addConformance("B");
				}
                addRdfDescription(a1);
            }
        }
    }


}