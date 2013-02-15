/*
 * Copyright 2004 by Paulo Soares.
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

import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.StringTokenizer;

import com.lowagie.text.xml.simpleparser.SimpleXMLDocHandler;
import com.lowagie.text.xml.simpleparser.SimpleXMLParser;

/**
 *
 * @author Paulo Soares (psoares@consiste.pt)
 */
final class SimpleNamedDestination implements SimpleXMLDocHandler {

    private HashMap xmlNames;
    private HashMap xmlLast;

    private SimpleNamedDestination() {
    }





    /**
     * Exports the destinations to XML.
     * @param names the names
     * @param wrt the export destination. The writer is not closed
     * @param encoding the encoding according to IANA conventions
     * @param onlyASCII codes above 127 will always be escaped with &amp;#nn; if <CODE>true</CODE>,
     * whatever the encoding
     * @throws IOException on error
     */
    private static void exportToXML(final HashMap names, final Writer wrt, final String encoding, final boolean onlyASCII) throws IOException {
        wrt.write("<?xml version=\"1.0\" encoding=\"");
        wrt.write(SimpleXMLParser.escapeXML(encoding, onlyASCII));
        wrt.write("\"?>\n<Destination>\n");
        for (final Iterator it = names.entrySet().iterator(); it.hasNext();) {
            final Map.Entry entry = (Map.Entry)it.next();
            final String key = (String)entry.getKey();
            final String value = (String)entry.getValue();
            wrt.write("  <Name Page=\"");
            wrt.write(SimpleXMLParser.escapeXML(value, onlyASCII));
            wrt.write("\">");
            wrt.write(SimpleXMLParser.escapeXML(escapeBinaryString(key), onlyASCII));
            wrt.write("</Name>\n");
        }
        wrt.write("</Destination>\n");
        wrt.flush();
    }





    private static PdfArray createDestinationArray(final String value, final PdfWriter writer) {
        final PdfArray ar = new PdfArray();
        final StringTokenizer tk = new StringTokenizer(value);
        final int n = Integer.parseInt(tk.nextToken());
        ar.add(writer.getPageReference(n));
        if (!tk.hasMoreTokens()) {
            ar.add(PdfName.XYZ);
            ar.add(new float[]{0, 10000, 0});
        }
        else {
            String fn = tk.nextToken();
            if (fn.startsWith("/")) {
				fn = fn.substring(1);
			}
            ar.add(new PdfName(fn));
            for (int k = 0; k < 4 && tk.hasMoreTokens(); ++k) {
                fn = tk.nextToken();
                if (fn.equals("null")) {
					ar.add(PdfNull.PDFNULL);
				} else {
					ar.add(new PdfNumber(fn));
				}
            }
        }
        return ar;
    }

    static String escapeBinaryString(final String s) {
        final StringBuffer buf = new StringBuffer();
        final char cc[] = s.toCharArray();
        final int len = cc.length;
        for (int k = 0; k < len; ++k) {
            final char c = cc[k];
            if (c < ' ') {
                buf.append('\\');
                final String octal = "00" + Integer.toOctalString(c);
                buf.append(octal.substring(octal.length() - 3));
            }
            else if (c == '\\') {
				buf.append("\\\\");
			} else {
				buf.append(c);
			}
        }
        return buf.toString();
    }

    static String unEscapeBinaryString(final String s) {
        final StringBuffer buf = new StringBuffer();
        final char cc[] = s.toCharArray();
        final int len = cc.length;
        for (int k = 0; k < len; ++k) {
            char c = cc[k];
            if (c == '\\') {
                if (++k >= len) {
                    buf.append('\\');
                    break;
                }
                c = cc[k];
                if (c >= '0' && c <= '7') {
                    int n = c - '0';
                    ++k;
                    for (int j = 0; j < 2 && k < len; ++j) {
                        c = cc[k];
                        if (c >= '0' && c <= '7') {
                            ++k;
                            n = n * 8 + c - '0';
                        }
                        else {
                            break;
                        }
                    }
                    --k;
                    buf.append((char)n);
                } else {
					buf.append(c);
				}
            } else {
				buf.append(c);
			}
        }
        return buf.toString();
    }

    @Override
	public void endDocument() {
    }

    @Override
	public void endElement(final String tag) {
        if (tag.equals("Destination")) {
            if (this.xmlLast == null && this.xmlNames != null) {
				return;
			} else {
				throw new RuntimeException("Destination end tag out of place.");
			}
        }
        if (!tag.equals("Name")) {
			throw new RuntimeException("Invalid end tag - " + tag);
		}
        if (this.xmlLast == null || this.xmlNames == null) {
			throw new RuntimeException("Name end tag out of place.");
		}
        if (!this.xmlLast.containsKey("Page")) {
			throw new RuntimeException("Page attribute missing.");
		}
        this.xmlNames.put(unEscapeBinaryString((String)this.xmlLast.get("Name")), this.xmlLast.get("Page"));
        this.xmlLast = null;
    }

    @Override
	public void startDocument() {
    }

    @Override
	public void startElement(final String tag, final HashMap h) {
        if (this.xmlNames == null) {
            if (tag.equals("Destination")) {
                this.xmlNames = new HashMap();
                return;
            } else {
				throw new RuntimeException("Root element is not Destination.");
			}
        }
        if (!tag.equals("Name")) {
			throw new RuntimeException("Tag " + tag + " not allowed.");
		}
        if (this.xmlLast != null) {
			throw new RuntimeException("Nested tags are not allowed.");
		}
        this.xmlLast = new HashMap(h);
        this.xmlLast.put("Name", "");
    }

    @Override
	public void text(final String str) {
        if (this.xmlLast == null) {
			return;
		}
        String name = (String)this.xmlLast.get("Name");
        name += str;
        this.xmlLast.put("Name", name);
    }
}