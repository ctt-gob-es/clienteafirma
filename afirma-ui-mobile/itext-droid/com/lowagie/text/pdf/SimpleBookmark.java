/*
 * Copyright 2003 by Paulo Soares.
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.StringTokenizer;

import com.lowagie.text.xml.simpleparser.SimpleXMLDocHandler;
import com.lowagie.text.xml.simpleparser.SimpleXMLParser;
/**
 * Bookmark processing in a simple way. It has some limitations, mainly the only
 * action types supported are GoTo, GoToR, URI and Launch.
 * <p>
 * The list structure is composed by a number of HashMap, keyed by strings, one HashMap
 * for each bookmark.
 * The element values are all strings with the exception of the key "Kids" that has
 * another list for the child bookmarks.
 * <p>
 * All the bookmarks have a "Title" with the
 * bookmark title and optionally a "Style" that can be "bold", "italic" or a
 * combination of both. They can also have a "Color" key with a value of three
 * floats separated by spaces. The key "Open" can have the values "true" or "false" and
 * signals the open status of the children. It's "true" by default.
 * <p>
 * The actions and the parameters can be:
 * <ul>
 * <li>"Action" = "GoTo" - "Page" | "Named"
 * <ul>
 * <li>"Page" = "3 XYZ 70 400 null" - page number followed by a destination (/XYZ is also accepted)
 * <li>"Named" = "named_destination"
 * </ul>
 * <li>"Action" = "GoToR" - "Page" | "Named" | "NamedN", "File", ["NewWindow"]
 * <ul>
 * <li>"Page" = "3 XYZ 70 400 null" - page number followed by a destination (/XYZ is also accepted)
 * <li>"Named" = "named_destination_as_a_string"
 * <li>"NamedN" = "named_destination_as_a_name"
 * <li>"File" - "the_file_to_open"
 * <li>"NewWindow" - "true" or "false"
 * </ul>
 * <li>"Action" = "URI" - "URI"
 * <ul>
 * <li>"URI" = "http://sf.net" - URI to jump to
 * </ul>
 * <li>"Action" = "Launch" - "File"
 * <ul>
 * <li>"File" - "the_file_to_open_or_execute"
 * </ul>
 * @author Paulo Soares (psoares@consiste.pt)
 */
final class SimpleBookmark implements SimpleXMLDocHandler {

    private ArrayList topList;
    private final Stack attr = new Stack();

    /** Creates a new instance of SimpleBookmark */
    private SimpleBookmark() {
    }

    private static List bookmarkDepth(final PdfReader reader, PdfDictionary outline, final IntHashtable pages) {
        final ArrayList list = new ArrayList();
        while (outline != null) {
            final HashMap map = new HashMap();
            final PdfString title = (PdfString)PdfReader.getPdfObjectRelease(outline.get(PdfName.TITLE));
            map.put("Title", title.toUnicodeString());
            final PdfArray color = (PdfArray)PdfReader.getPdfObjectRelease(outline.get(PdfName.C));
            if (color != null && color.size() == 3) {
                final ByteBuffer out = new ByteBuffer();
                out.append(color.getAsNumber(0).floatValue()).append(' ');
                out.append(color.getAsNumber(1).floatValue()).append(' ');
                out.append(color.getAsNumber(2).floatValue());
                map.put("Color", PdfEncodings.convertToString(out.toByteArray(), null));
            }
            final PdfNumber style = (PdfNumber)PdfReader.getPdfObjectRelease(outline.get(PdfName.F));
            if (style != null) {
                final int f = style.intValue();
                String s = "";
                if ((f & 1) != 0) {
					s += "italic ";
				}
                if ((f & 2) != 0) {
					s += "bold ";
				}
                s = s.trim();
                if (s.length() != 0) {
					map.put("Style", s);
				}
            }
            final PdfNumber count = (PdfNumber)PdfReader.getPdfObjectRelease(outline.get(PdfName.COUNT));
            if (count != null && count.intValue() < 0) {
				map.put("Open", "false");
			}
            try {
                PdfObject dest = PdfReader.getPdfObjectRelease(outline.get(PdfName.DEST));
                if (dest != null) {
                    mapGotoBookmark(map, dest, pages); //changed by ujihara 2004-06-13
                }
                else {
                    final PdfDictionary action = (PdfDictionary)PdfReader.getPdfObjectRelease(outline.get(PdfName.A));
                    if (action != null) {
                        if (PdfName.GOTO.equals(PdfReader.getPdfObjectRelease(action.get(PdfName.S)))) {
                            dest = PdfReader.getPdfObjectRelease(action.get(PdfName.D));
                            if (dest != null) {
                                mapGotoBookmark(map, dest, pages);
                            }
                        }
                        else if (PdfName.URI.equals(PdfReader.getPdfObjectRelease(action.get(PdfName.S)))) {
                            map.put("Action", "URI");
                            map.put("URI", ((PdfString)PdfReader.getPdfObjectRelease(action.get(PdfName.URI))).toUnicodeString());
                        }
                        else if (PdfName.GOTOR.equals(PdfReader.getPdfObjectRelease(action.get(PdfName.S)))) {
                            dest = PdfReader.getPdfObjectRelease(action.get(PdfName.D));
                            if (dest != null) {
                                if (dest.isString()) {
									map.put("Named", dest.toString());
								} else if (dest.isName()) {
									map.put("NamedN", PdfName.decodeName(dest.toString()));
								} else if (dest.isArray()) {
                                    final PdfArray arr = (PdfArray)dest;
                                    final StringBuffer s = new StringBuffer();
                                    s.append(arr.getPdfObject(0).toString());
                                    s.append(' ').append(arr.getPdfObject(1).toString());
                                    for (int k = 2; k < arr.size(); ++k) {
										s.append(' ').append(arr.getPdfObject(k).toString());
									}
                                    map.put("Page", s.toString());
                                }
                            }
                            map.put("Action", "GoToR");
                            PdfObject file = PdfReader.getPdfObjectRelease(action.get(PdfName.F));
                            if (file != null) {
                                if (file.isString()) {
									map.put("File", ((PdfString)file).toUnicodeString());
								} else if (file.isDictionary()) {
                                    file = PdfReader.getPdfObject(((PdfDictionary)file).get(PdfName.F));
                                    if (file.isString()) {
										map.put("File", ((PdfString)file).toUnicodeString());
									}
                                }
                            }
                            final PdfObject newWindow = PdfReader.getPdfObjectRelease(action.get(PdfName.NEWWINDOW));
                            if (newWindow != null) {
								map.put("NewWindow", newWindow.toString());
							}
                        }
                        else if (PdfName.LAUNCH.equals(PdfReader.getPdfObjectRelease(action.get(PdfName.S)))) {
                            map.put("Action", "Launch");
                            PdfObject file = PdfReader.getPdfObjectRelease(action.get(PdfName.F));
                            if (file == null) {
								file = PdfReader.getPdfObjectRelease(action.get(PdfName.WIN));
							}
                            if (file != null) {
                                if (file.isString()) {
									map.put("File", ((PdfString)file).toUnicodeString());
								} else if (file.isDictionary()) {
                                    file = PdfReader.getPdfObjectRelease(((PdfDictionary)file).get(PdfName.F));
                                    if (file.isString()) {
										map.put("File", ((PdfString)file).toUnicodeString());
									}
                                }
                            }
                        }
                    }
                }
            }
            catch (final Exception e) {
                //empty on purpose
            }
            final PdfDictionary first = (PdfDictionary)PdfReader.getPdfObjectRelease(outline.get(PdfName.FIRST));
            if (first != null) {
                map.put("Kids", bookmarkDepth(reader, first, pages));
            }
            list.add(map);
            outline = (PdfDictionary)PdfReader.getPdfObjectRelease(outline.get(PdfName.NEXT));
        }
        return list;
    }

	private static void mapGotoBookmark(final HashMap map, final PdfObject dest, final IntHashtable pages)
	{
		if (dest.isString()) {
			map.put("Named", dest.toString());
		} else if (dest.isName()) {
			map.put("Named", PdfName.decodeName(dest.toString()));
		} else if (dest.isArray())
		 {
			map.put("Page", makeBookmarkParam((PdfArray)dest, pages)); //changed by ujihara 2004-06-13
		}
		map.put("Action", "GoTo");
	}

	private static String makeBookmarkParam(final PdfArray dest, final IntHashtable pages)
	{
		final StringBuffer s = new StringBuffer();
		final PdfObject obj = dest.getPdfObject(0);
        if (obj.isNumber()) {
			s.append(((PdfNumber)obj).intValue() + 1);
		}
		else {
			s.append(pages.get(getNumber((PdfIndirectReference)obj))); //changed by ujihara 2004-06-13
		}
		s.append(' ').append(dest.getPdfObject(1).toString().substring(1));
		for (int k = 2; k < dest.size(); ++k) {
			s.append(' ').append(dest.getPdfObject(k).toString());
		}
		return s.toString();
	}

	/**
	 * Gets number of indirect. If type of directed indirect is PAGES, it refers PAGE object through KIDS.
	 * (Contributed by Kazuya Ujihara)
	 * @param indirect
	 * 2004-06-13
	 */
	private static int getNumber(PdfIndirectReference indirect)
	{
		final PdfDictionary pdfObj = (PdfDictionary)PdfReader.getPdfObjectRelease(indirect);
		if (pdfObj.contains(PdfName.TYPE) && pdfObj.get(PdfName.TYPE).equals(PdfName.PAGES) && pdfObj.contains(PdfName.KIDS))
		{
			final PdfArray kids = (PdfArray)pdfObj.get(PdfName.KIDS);
			indirect = (PdfIndirectReference)kids.getPdfObject(0);
		}
		return indirect.getNumber();
	}







    private static void createOutlineAction(final PdfDictionary outline, final HashMap map, final PdfWriter writer, final boolean namedAsNames) {
        try {
            final String action = (String)map.get("Action");
            if ("GoTo".equals(action)) {
                String p;
                if ((p = (String)map.get("Named")) != null) {
                    if (namedAsNames) {
						outline.put(PdfName.DEST, new PdfName(p));
					} else {
						outline.put(PdfName.DEST, new PdfString(p, null));
					}
                }
                else if ((p = (String)map.get("Page")) != null) {
                    final PdfArray ar = new PdfArray();
                    final StringTokenizer tk = new StringTokenizer(p);
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
                    outline.put(PdfName.DEST, ar);
                }
            }
            else if ("GoToR".equals(action)) {
                String p;
                final PdfDictionary dic = new PdfDictionary();
                if ((p = (String)map.get("Named")) != null) {
					dic.put(PdfName.D, new PdfString(p, null));
				} else if ((p = (String)map.get("NamedN")) != null) {
					dic.put(PdfName.D, new PdfName(p));
				} else if ((p = (String)map.get("Page")) != null){
                    final PdfArray ar = new PdfArray();
                    final StringTokenizer tk = new StringTokenizer(p);
                    ar.add(new PdfNumber(tk.nextToken()));
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
                    dic.put(PdfName.D, ar);
                }
                final String file = (String)map.get("File");
                if (dic.size() > 0 && file != null) {
                    dic.put(PdfName.S,  PdfName.GOTOR);
                    dic.put(PdfName.F, new PdfString(file));
                    final String nw = (String)map.get("NewWindow");
                    if (nw != null) {
                        if (nw.equals("true")) {
							dic.put(PdfName.NEWWINDOW, PdfBoolean.PDFTRUE);
						} else if (nw.equals("false")) {
							dic.put(PdfName.NEWWINDOW, PdfBoolean.PDFFALSE);
						}
                    }
                    outline.put(PdfName.A, dic);
                }
            }
            else if ("URI".equals(action)) {
                final String uri = (String)map.get("URI");
                if (uri != null) {
                    final PdfDictionary dic = new PdfDictionary();
                    dic.put(PdfName.S, PdfName.URI);
                    dic.put(PdfName.URI, new PdfString(uri));
                    outline.put(PdfName.A, dic);
                }
            }
            else if ("Launch".equals(action)) {
                final String file = (String)map.get("File");
                if (file != null) {
                    final PdfDictionary dic = new PdfDictionary();
                    dic.put(PdfName.S, PdfName.LAUNCH);
                    dic.put(PdfName.F, new PdfString(file));
                    outline.put(PdfName.A, dic);
                }
            }
        }
        catch (final Exception e) {
            // empty on purpose
        }
    }

    static Object[] iterateOutlines(final PdfWriter writer, final PdfIndirectReference parent, final List kids, final boolean namedAsNames) throws IOException {
        final PdfIndirectReference refs[] = new PdfIndirectReference[kids.size()];
        for (int k = 0; k < refs.length; ++k) {
			refs[k] = writer.getPdfIndirectReference();
		}
        int ptr = 0;
        int count = 0;
        for (final Iterator it = kids.listIterator(); it.hasNext(); ++ptr) {
            final HashMap map = (HashMap)it.next();
            Object lower[] = null;
            final List subKid = (List)map.get("Kids");
            if (subKid != null && !subKid.isEmpty()) {
				lower = iterateOutlines(writer, refs[ptr], subKid, namedAsNames);
			}
            final PdfDictionary outline = new PdfDictionary();
            ++count;
            if (lower != null) {
                outline.put(PdfName.FIRST, (PdfIndirectReference)lower[0]);
                outline.put(PdfName.LAST, (PdfIndirectReference)lower[1]);
                final int n = ((Integer)lower[2]).intValue();
                if ("false".equals(map.get("Open"))) {
                    outline.put(PdfName.COUNT, new PdfNumber(-n));
                }
                else {
                    outline.put(PdfName.COUNT, new PdfNumber(n));
                    count += n;
                }
            }
            outline.put(PdfName.PARENT, parent);
            if (ptr > 0) {
				outline.put(PdfName.PREV, refs[ptr - 1]);
			}
            if (ptr < refs.length - 1) {
				outline.put(PdfName.NEXT, refs[ptr + 1]);
			}
            outline.put(PdfName.TITLE, new PdfString((String)map.get("Title"), PdfObject.TEXT_UNICODE));
            final String color = (String)map.get("Color");
            if (color != null) {
                try {
                    final PdfArray arr = new PdfArray();
                    final StringTokenizer tk = new StringTokenizer(color);
                    for (int k = 0; k < 3; ++k) {
                        float f = Float.parseFloat(tk.nextToken());
                        if (f < 0) {
							f = 0;
						}
                        if (f > 1) {
							f = 1;
						}
                        arr.add(new PdfNumber(f));
                    }
                    outline.put(PdfName.C, arr);
                } catch(final Exception e){} //in case it's malformed
            }
            String style = (String)map.get("Style");
            if (style != null) {
                style = style.toLowerCase();
                int bits = 0;
                if (style.indexOf("italic") >= 0) {
					bits |= 1;
				}
                if (style.indexOf("bold") >= 0) {
					bits |= 2;
				}
                if (bits != 0) {
					outline.put(PdfName.F, new PdfNumber(bits));
				}
            }
            createOutlineAction(outline, map, writer, namedAsNames);
            writer.addToBody(outline, refs[ptr]);
        }
        return new Object[]{refs[0], refs[refs.length - 1], new Integer(count)};
    }

    /**
     * Exports the bookmarks to XML. Only of use if the generation is to be include in
     * some other XML document.
     * @param list the bookmarks
     * @param out the export destination. The writer is not closed
     * @param indent the indentation level. Pretty printing significant only
     * @param onlyASCII codes above 127 will always be escaped with &amp;#nn; if <CODE>true</CODE>,
     * whatever the encoding
     * @throws IOException on error
     */
    private static void exportToXMLNode(final List list, final Writer out, final int indent, final boolean onlyASCII) throws IOException {
        String dep = "";
        for (int k = 0; k < indent; ++k) {
			dep += "  ";
		}
        for (final Iterator it = list.iterator(); it.hasNext();) {
            final HashMap map = (HashMap)it.next();
            String title = null;
            out.write(dep);
            out.write("<Title ");
            List kids = null;
            for (final Iterator e = map.entrySet().iterator(); e.hasNext();) {
                final Map.Entry entry = (Map.Entry) e.next();
                final String key = (String) entry.getKey();
                if (key.equals("Title")) {
                    title = (String) entry.getValue();
                    continue;
                }
                else if (key.equals("Kids")) {
                    kids = (List) entry.getValue();
                    continue;
                }
                else {
                    out.write(key);
                    out.write("=\"");
                    String value = (String) entry.getValue();
                    if (key.equals("Named") || key.equals("NamedN")) {
						value = SimpleNamedDestination.escapeBinaryString(value);
					}
                    out.write(SimpleXMLParser.escapeXML(value, onlyASCII));
                    out.write("\" ");
                }
            }
            out.write(">");
            if (title == null) {
				title = "";
			}
            out.write(SimpleXMLParser.escapeXML(title, onlyASCII));
            if (kids != null) {
                out.write("\n");
                exportToXMLNode(kids, out, indent + 1, onlyASCII);
                out.write(dep);
            }
            out.write("</Title>\n");
        }
    }



    /**
     * Exports the bookmarks to XML.
     * @param list the bookmarks
     * @param wrt the export destination. The writer is not closed
     * @param encoding the encoding according to IANA conventions
     * @param onlyASCII codes above 127 will always be escaped with &amp;#nn; if <CODE>true</CODE>,
     * whatever the encoding
     * @throws IOException on error
     */
    private static void exportToXML(final List list, final Writer wrt, final String encoding, final boolean onlyASCII) throws IOException {
        wrt.write("<?xml version=\"1.0\" encoding=\"");
        wrt.write(SimpleXMLParser.escapeXML(encoding, onlyASCII));
        wrt.write("\"?>\n<Bookmark>\n");
        exportToXMLNode(list, wrt, 1, onlyASCII);
        wrt.write("</Bookmark>\n");
        wrt.flush();
    }

    @Override
	public void endDocument() {
    }

    @Override
	public void endElement(final String tag) {
        if (tag.equals("Bookmark")) {
            if (this.attr.isEmpty()) {
				return;
			} else {
				throw new RuntimeException("Bookmark end tag out of place.");
			}
        }
        if (!tag.equals("Title")) {
			throw new RuntimeException("Invalid end tag - " + tag);
		}
        final HashMap attributes = (HashMap)this.attr.pop();
        final String title = (String)attributes.get("Title");
        attributes.put("Title",  title.trim());
        String named = (String)attributes.get("Named");
        if (named != null) {
			attributes.put("Named", SimpleNamedDestination.unEscapeBinaryString(named));
		}
        named = (String)attributes.get("NamedN");
        if (named != null) {
			attributes.put("NamedN", SimpleNamedDestination.unEscapeBinaryString(named));
		}
        if (this.attr.isEmpty()) {
			this.topList.add(attributes);
		} else {
            final HashMap parent = (HashMap)this.attr.peek();
            List kids = (List)parent.get("Kids");
            if (kids == null) {
                kids = new ArrayList();
                parent.put("Kids", kids);
            }
            kids.add(attributes);
        }
    }

    @Override
	public void startDocument() {
    }

    @Override
	public void startElement(final String tag, final HashMap h) {
        if (this.topList == null) {
            if (tag.equals("Bookmark")) {
                this.topList = new ArrayList();
                return;
            } else {
				throw new RuntimeException("Root element is not Bookmark: " + tag);
			}
        }
        if (!tag.equals("Title")) {
			throw new RuntimeException("Tag " + tag + " not allowed.");
		}
        final HashMap attributes = new HashMap(h);
        attributes.put("Title", "");
        attributes.remove("Kids");
        this.attr.push(attributes);
    }

    @Override
	public void text(final String str) {
        if (this.attr.isEmpty()) {
			return;
		}
        final HashMap attributes = (HashMap)this.attr.peek();
        String title = (String)attributes.get("Title");
        title += str;
        attributes.put("Title", title);
    }
}
