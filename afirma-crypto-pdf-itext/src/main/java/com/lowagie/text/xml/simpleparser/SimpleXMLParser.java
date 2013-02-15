/*
 * Copyright 2003 Paulo Soares
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
 *
 * The code to recognize the encoding in this class and in the convenience class IanaEncodings was taken from Apache Xerces published under the following license:
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Part of this code is based on the Quick-and-Dirty XML parser by Steven Brandt.
 * The code for the Quick-and-Dirty parser was published in JavaWorld (java tip 128).
 * Steven Brandt and JavaWorld gave permission to use the code for free.
 * (Bruno Lowagie and Paulo Soares chose to use it under the MPL/LGPL in
 * conformance with the rest of the code).
 * The original code can be found on this url: <A HREF="http://www.javaworld.com/javatips/jw-javatip128_p.html">http://www.javaworld.com/javatips/jw-javatip128_p.html</A>.
 * It was substantially refactored by Bruno Lowagie.
 *
 * The method 'private static String getEncodingName(byte[] b4)' was found
 * in org.apache.xerces.impl.XMLEntityManager, originaly published by the
 * Apache Software Foundation under the Apache Software License; now being
 * used in iText under the MPL.
 */
package com.lowagie.text.xml.simpleparser;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.HashMap;
import java.util.Stack;

/**
 * A simple XML and HTML parser.  This parser is, like the SAX parser,
 * an event based parser, but with much less functionality.
 * <p>
 * The parser can:
 * <p>
 * <ul>
 * <li>It recognizes the encoding used
 * <li>It recognizes all the elements' start tags and end tags
 * <li>It lists attributes, where attribute values can be enclosed in single or double quotes
 * <li>It recognizes the <code>&lt;[CDATA[ ... ]]&gt;</code> construct
 * <li>It recognizes the standard entities: &amp;amp;, &amp;lt;, &amp;gt;, &amp;quot;, and &amp;apos;, as well as numeric entities
 * <li>It maps lines ending in <code>\r\n</code> and <code>\r</code> to <code>\n</code> on input, in accordance with the XML Specification, Section 2.11
 * </ul>
 * <p>
 */
public final class SimpleXMLParser {
    /** possible states */
	private final static int UNKNOWN = 0;
	private final static int TEXT = 1;
	private final static int TAG_ENCOUNTERED = 2;
	private final static int EXAMIN_TAG = 3;
	private final static int TAG_EXAMINED = 4;
	private final static int IN_CLOSETAG = 5;
	private final static int SINGLE_TAG = 6;
	private final static int CDATA = 7;
	private final static int COMMENT = 8;
	private final static int PI = 9;
	private final static int ENTITY = 10;
	private final static int QUOTE = 11;
	private final static int ATTRIBUTE_KEY = 12;
	private final static int ATTRIBUTE_EQUAL = 13;
	private final static int ATTRIBUTE_VALUE = 14;

	/** the state stack */
	private final Stack stack;
	/** The current character. */
	private int character = 0;
	/** The previous character. */
	private int previousCharacter = -1;
	/** the line we are currently reading */
	private int lines = 1;
	/** the column where the current character occurs */
	private int columns = 0;
	/** was the last character equivalent to a newline? */
	private boolean eol = false;
	/**
	 * A boolean indicating if the next character should be taken into account
	 * if it's a space character. When nospace is false, the previous character
	 * wasn't whitespace.
	 * @since 2.1.5
	 */
	private boolean nowhite = false;
	/** the current state */
	private int state;
	/** Are we parsing HTML? */
	private final boolean html;
	/** current text (whatever is encountered between tags) */
	private final StringBuffer text = new StringBuffer();
	/** current entity (whatever is encountered between & and ;) */
	private final StringBuffer entity = new StringBuffer();
	/** current tagname */
	private String tag = null;
	/** current attributes */
	private HashMap attributes = null;
	/** The handler to which we are going to forward document content */
	private final SimpleXMLDocHandler doc;
	/** The handler to which we are going to forward comments. */
	private final SimpleXMLDocHandlerComment comment;
	/** Keeps track of the number of tags that are open. */
	private int nested = 0;
	/** the quote character that was used to open the quote. */
	private int quoteCharacter = '"';
	/** the attribute key. */
	private String attributekey = null;
	/** the attribute value. */
	private String attributevalue = null;

	/**
	 * Creates a Simple XML parser object.
	 * Call go(BufferedReader) immediately after creation.
	 */
    private SimpleXMLParser(final SimpleXMLDocHandler doc, final SimpleXMLDocHandlerComment comment, final boolean html) {
    	this.doc = doc;
    	this.comment = comment;
    	this.html = html;
    	this.stack = new Stack();
    	this.state = html ? TEXT : UNKNOWN;
    }

    /**
     * Does the actual parsing. Perform this immediately
     * after creating the parser object.
     */
    private void go(final Reader r) throws IOException {
        BufferedReader reader;
        if (r instanceof BufferedReader) {
			reader = (BufferedReader)r;
		} else {
			reader = new BufferedReader(r);
		}
        this.doc.startDocument();
        while(true) {
			// read a new character
			if (this.previousCharacter == -1) {
				this.character = reader.read();
			}
			// or re-examine the previous character
			else {
				this.character = this.previousCharacter;
				this.previousCharacter = -1;
			}

			// the end of the file was reached
			if (this.character == -1) {
				if (this.html) {
					if (this.html && this.state == TEXT) {
						flush();
					}
					this.doc.endDocument();
				} else {
					throwException("Missing end tag");
				}
				return;
			}

			// dealing with  \n and \r
			if (this.character == '\n' && this.eol) {
				this.eol = false;
				continue;
			} else if (this.eol) {
				this.eol = false;
			} else if (this.character == '\n') {
				this.lines++;
				this.columns = 0;
			} else if (this.character == '\r') {
				this.eol = true;
				this.character = '\n';
				this.lines++;
				this.columns = 0;
			} else {
				this.columns++;
			}

			switch(this.state) {
            // we are in an unknown state before there's actual content
			case UNKNOWN:
                if(this.character == '<') {
                    saveState(TEXT);
                    this.state = TAG_ENCOUNTERED;
                }
                break;
            // we can encounter any content
			case TEXT:
                if(this.character == '<') {
                    flush();
                    saveState(this.state);
                    this.state = TAG_ENCOUNTERED;
                } else if(this.character == '&') {
                    saveState(this.state);
                    this.entity.setLength(0);
                    this.state = ENTITY;
                } else if (Character.isWhitespace((char)this.character)) {
                	if (this.nowhite) {
						this.text.append((char)this.character);
					}
                	this.nowhite = false;
                } else {
                    this.text.append((char)this.character);
                    this.nowhite = true;
                }
                break;
            // we have just seen a < and are wondering what we are looking at
            // <foo>, </foo>, <!-- ... --->, etc.
			case TAG_ENCOUNTERED:
                initTag();
                if(this.character == '/') {
                    this.state = IN_CLOSETAG;
                } else if (this.character == '?') {
                    restoreState();
                    this.state = PI;
                } else {
                    this.text.append((char)this.character);
                    this.state = EXAMIN_TAG;
                }
                break;
            // we are processing something like this <foo ... >.
            // It could still be a <!-- ... --> or something.
			case EXAMIN_TAG:
                if(this.character == '>') {
                    doTag();
                    processTag(true);
                    initTag();
                    this.state = restoreState();
                } else if(this.character == '/') {
                    this.state = SINGLE_TAG;
                } else if(this.character == '-' && this.text.toString().equals("!-")) {
                    flush();
                    this.state = COMMENT;
                } else if(this.character == '[' && this.text.toString().equals("![CDATA")) {
                    flush();
                    this.state = CDATA;
                } else if(this.character == 'E' && this.text.toString().equals("!DOCTYP")) {
                    flush();
                    this.state = PI;
                } else if(Character.isWhitespace((char)this.character)) {
                    doTag();
                    this.state = TAG_EXAMINED;
                } else {
                    this.text.append((char)this.character);
                }
                break;
            // we know the name of the tag now.
			case TAG_EXAMINED:
                if(this.character == '>') {
                    processTag(true);
                    initTag();
                    this.state = restoreState();
                } else if(this.character == '/') {
                    this.state = SINGLE_TAG;
                } else if(Character.isWhitespace((char)this.character)) {
                    // empty
                } else {
                    this.text.append((char)this.character);
                    this.state = ATTRIBUTE_KEY;
                }
                break;

                // we are processing a closing tag: e.g. </foo>
			case IN_CLOSETAG:
                if(this.character == '>') {
                    doTag();
                    processTag(false);
                    if(!this.html && this.nested==0) {
						return;
					}
                    this.state = restoreState();
                } else {
                    if (!Character.isWhitespace((char)this.character)) {
						this.text.append((char)this.character);
					}
                }
                break;

            // we have just seen something like this: <foo a="b"/
            // and are looking for the final >.
			case SINGLE_TAG:
                if(this.character != '>') {
					throwException("Expected > for tag: <"+this.tag+"/>");
				}
				doTag();
                processTag(true);
                processTag(false);
                initTag();
                if(!this.html && this.nested==0) {
                    this.doc.endDocument();
                    return;
                }
                this.state = restoreState();
                break;

            // we are processing CDATA
			case CDATA:
                if(this.character == '>'
                && this.text.toString().endsWith("]]")) {
                    this.text.setLength(this.text.length()-2);
                    flush();
                    this.state = restoreState();
                } else {
					this.text.append((char)this.character);
				}
                break;

            // we are processing a comment.  We are inside
            // the <!-- .... --> looking for the -->.
			case COMMENT:
                if(this.character == '>'
                && this.text.toString().endsWith("--")) {
                    this.text.setLength(this.text.length() - 2);
                    flush();
                    this.state = restoreState();
                } else {
					this.text.append((char)this.character);
				}
                break;

            // We are inside one of these <? ... ?> or one of these <!DOCTYPE ... >
			case PI:
                if(this.character == '>') {
                    this.state = restoreState();
                    if(this.state == TEXT) {
						this.state = UNKNOWN;
					}
                }
                break;

            // we are processing an entity, e.g. &lt;, &#187;, etc.
			case ENTITY:
                if(this.character == ';') {
                    this.state = restoreState();
                    final String cent = this.entity.toString();
                    this.entity.setLength(0);
                    final char ce = EntitiesToUnicode.decodeEntity(cent);
                    if (ce == '\0') {
						this.text.append('&').append(cent).append(';');
					} else {
						this.text.append(ce);
					}
                } else if (this.character != '#' && (this.character < '0' || this.character > '9') && (this.character < 'a' || this.character > 'z')
                    && (this.character < 'A' || this.character > 'Z') || this.entity.length() >= 7) {
                    this.state = restoreState();
                    this.previousCharacter = this.character;
                    this.text.append('&').append(this.entity.toString());
                    this.entity.setLength(0);
                }
                else {
                    this.entity.append((char)this.character);
                }
                break;
            // We are processing the quoted right-hand side of an element's attribute.
			case QUOTE:
                if (this.html && this.quoteCharacter == ' ' && this.character == '>') {
                    flush();
                    processTag(true);
                    initTag();
                    this.state = restoreState();
                }
                else if (this.html && this.quoteCharacter == ' ' && Character.isWhitespace((char)this.character)) {
                	flush();
                    this.state = TAG_EXAMINED;
                }
                else if (this.html && this.quoteCharacter == ' ') {
                    this.text.append((char)this.character);
                }
                else if(this.character == this.quoteCharacter) {
                	flush();
                    this.state = TAG_EXAMINED;
                } else if(" \r\n\u0009".indexOf(this.character)>=0) {
                    this.text.append(' ');
                } else if(this.character == '&') {
                    saveState(this.state);
                    this.state = ENTITY;
                    this.entity.setLength(0);
                } else {
                    this.text.append((char)this.character);
                }
                break;

			case ATTRIBUTE_KEY:
                if(Character.isWhitespace((char)this.character)) {
                    flush();
                    this.state = ATTRIBUTE_EQUAL;
                } else if(this.character == '=') {
                	flush();
                    this.state = ATTRIBUTE_VALUE;
                } else if (this.html && this.character == '>') {
                    this.text.setLength(0);
                    processTag(true);
                    initTag();
                    this.state = restoreState();
                } else {
                    this.text.append((char)this.character);
                }
                break;

			case ATTRIBUTE_EQUAL:
                if(this.character == '=') {
                    this.state = ATTRIBUTE_VALUE;
                } else if(Character.isWhitespace((char)this.character)) {
                    // empty
                } else if (this.html && this.character == '>') {
                    this.text.setLength(0);
                    processTag(true);
                    initTag();
                    this.state = restoreState();
                } else if (this.html && this.character == '/') {
                    flush();
                    this.state = SINGLE_TAG;
                } else if (this.html) {
                    flush();
                    this.text.append((char)this.character);
                    this.state = ATTRIBUTE_KEY;
                } else {
                    throwException("Error in attribute processing.");
                }
                break;

			case ATTRIBUTE_VALUE:
                if(this.character == '"' || this.character == '\'') {
                    this.quoteCharacter = this.character;
                    this.state = QUOTE;
                } else if(Character.isWhitespace((char)this.character)) {
                    // empty
                } else if (this.html && this.character == '>') {
                    flush();
                    processTag(true);
                    initTag();
                    this.state = restoreState();
                } else if (this.html) {
                    this.text.append((char)this.character);
                    this.quoteCharacter = ' ';
                    this.state = QUOTE;
                } else {
                    throwException("Error in attribute processing");
                }
                break;
            }
        }
    }

    /**
     * Gets a state from the stack
     * @return the previous state
     */
    private int restoreState() {
        if(!this.stack.empty()) {
			return ((Integer)this.stack.pop()).intValue();
		} else {
			return UNKNOWN;
		}
    }
    /**
     * Adds a state to the stack.
     * @param	s	a state to add to the stack
     */
    private void saveState(final int s) {
    	this.stack.push(new Integer(s));
    }
    /**
     * Flushes the text that is currently in the buffer.
     * The text can be ignored, added to the document
     * as content or as comment,... depending on the current state.
     */
    private void flush() {
    	switch(this.state){
    	case TEXT:
    	case CDATA:
            if(this.text.length() > 0) {
                this.doc.text(this.text.toString());
            }
            break;
    	case COMMENT:
        	if (this.comment != null) {
                this.comment.comment(this.text.toString());
            }
        	break;
    	case ATTRIBUTE_KEY:
            this.attributekey = this.text.toString();
            if (this.html) {
				this.attributekey = this.attributekey.toLowerCase();
			}
    		break;
    	case QUOTE:
    	case ATTRIBUTE_VALUE:
        	this.attributevalue = this.text.toString();
            this.attributes.put(this.attributekey,this.attributevalue);
            break;
    	default:
    		// do nothing
    	}
        this.text.setLength(0);
    }
    /**
     * Initialized the tag name and attributes.
     */
    private void initTag() {
        this.tag = null;
        this.attributes = new HashMap();
    }
    /** Sets the name of the tag. */
    private void doTag() {
    	if(this.tag == null) {
			this.tag = this.text.toString();
		}
    	if (this.html) {
			this.tag = this.tag.toLowerCase();
		}
    	this.text.setLength(0);
    }
    /**
     * processes the tag.
     * @param start	if true we are dealing with a tag that has just been opened; if false we are closing a tag.
     */
    private void processTag(final boolean start) {
    	if (start) {
    		this.nested++;
    		this.doc.startElement(this.tag,this.attributes);
    	}
    	else {
            this.nested--;
            this.doc.endElement(this.tag);
    	}
    }
    /** Throws an exception */
    private void throwException(final String s) throws IOException {
        throw new IOException(s+" near line " + this.lines + ", column " + this.columns);
    }

    /**
     * Parses the XML document firing the events to the handler.
     * @param doc the document handler
     * @param r the document. The encoding is already resolved. The reader is not closed
     * @throws IOException on error
     */
    private static void parse(final SimpleXMLDocHandler doc, final SimpleXMLDocHandlerComment comment, final Reader r, final boolean html) throws IOException {
    	final SimpleXMLParser parser = new SimpleXMLParser(doc, comment, html);
    	parser.go(r);
    }

    /**
     * Parses the XML document firing the events to the handler.
     * @param doc the document handler
     * @param in the document. The encoding is deduced from the stream. The stream is not closed
     * @throws IOException on error
     */
    public static void parse(final SimpleXMLDocHandler doc, final InputStream in) throws IOException {
        final byte b4[] = new byte[4];
        final int count = in.read(b4);
        if (count != 4) {
			throw new IOException("Insufficient length.");
		}
        String encoding = getEncodingName(b4);
        String decl = null;
        if (encoding.equals("UTF-8")) {
            final StringBuffer sb = new StringBuffer();
            int c;
            while ((c = in.read()) != -1) {
                if (c == '>') {
					break;
				}
                sb.append((char)c);
            }
            decl = sb.toString();
        }
        else if (encoding.equals("CP037")) {
            final ByteArrayOutputStream bi = new ByteArrayOutputStream();
            int c;
            while ((c = in.read()) != -1) {
                if (c == 0x6e) {
					break;
				}
                bi.write(c);
            }
            decl = new String(bi.toByteArray(), "CP037");
        }
        if (decl != null) {
            decl = getDeclaredEncoding(decl);
            if (decl != null) {
				encoding = decl;
			}
        }
        parse(doc, new InputStreamReader(in, IanaEncodings.getJavaEncoding(encoding)));
    }

    private static String getDeclaredEncoding(final String decl) {
        if (decl == null) {
			return null;
		}
        final int idx = decl.indexOf("encoding");
        if (idx < 0) {
			return null;
		}
        final int idx1 = decl.indexOf('"', idx);
        final int idx2 = decl.indexOf('\'', idx);
        if (idx1 == idx2) {
			return null;
		}
        if (idx1 < 0 && idx2 > 0 || idx2 > 0 && idx2 < idx1) {
            final int idx3 = decl.indexOf('\'', idx2 + 1);
            if (idx3 < 0) {
				return null;
			}
            return decl.substring(idx2 + 1, idx3);
        }
        if (idx2 < 0 && idx1 > 0 || idx1 > 0 && idx1 < idx2) {
            final int idx3 = decl.indexOf('"', idx1 + 1);
            if (idx3 < 0) {
				return null;
			}
            return decl.substring(idx1 + 1, idx3);
        }
        return null;
    }

    private static void parse(final SimpleXMLDocHandler doc,final Reader r) throws IOException {
        parse(doc, null, r, false);
    }

    /**
     * Escapes a string with the appropriated XML codes.
     * @param s the string to be escaped
     * @param onlyASCII codes above 127 will always be escaped with &amp;#nn; if <CODE>true</CODE>
     * @return the escaped string
     */
    public static String escapeXML(final String s, final boolean onlyASCII) {
        final char cc[] = s.toCharArray();
        final int len = cc.length;
        final StringBuffer sb = new StringBuffer();
        for (int k = 0; k < len; ++k) {
            final int c = cc[k];
            switch (c) {
                case '<':
                    sb.append("&lt;");
                    break;
                case '>':
                    sb.append("&gt;");
                    break;
                case '&':
                    sb.append("&amp;");
                    break;
                case '"':
                    sb.append("&quot;");
                    break;
                case '\'':
                    sb.append("&apos;");
                    break;
                default:
                	if (c == 0x9 || c == 0xA || c == 0xD
                		|| c >= 0x20 && c <= 0xD7FF
                		|| c >= 0xE000 && c <= 0xFFFD
                		|| c >= 0x10000 && c <= 0x10FFFF) {
                		if (onlyASCII && c > 127) {
							sb.append("&#").append(c).append(';');
						} else {
							sb.append((char)c);
						}
                	}
            }
        }
        return sb.toString();
    }
    /**
     * Returns the IANA encoding name that is auto-detected from
     * the bytes specified, with the endian-ness of that encoding where appropriate.
     * (method found in org.apache.xerces.impl.XMLEntityManager, originally published
     * by the Apache Software Foundation under the Apache Software License; now being
     * used in iText under the MPL)
     * @param b4    The first four bytes of the input.
     * @return an IANA-encoding string
     */
    private static String getEncodingName(final byte[] b4) {

        // UTF-16, with BOM
        final int b0 = b4[0] & 0xFF;
        final int b1 = b4[1] & 0xFF;
        if (b0 == 0xFE && b1 == 0xFF) {
            // UTF-16, big-endian
            return "UTF-16BE";
        }
        if (b0 == 0xFF && b1 == 0xFE) {
            // UTF-16, little-endian
            return "UTF-16LE";
        }

        // UTF-8 with a BOM
        final int b2 = b4[2] & 0xFF;
        if (b0 == 0xEF && b1 == 0xBB && b2 == 0xBF) {
            return "UTF-8";
        }

        // other encodings
        final int b3 = b4[3] & 0xFF;
        if (b0 == 0x00 && b1 == 0x00 && b2 == 0x00 && b3 == 0x3C) {
            // UCS-4, big endian (1234)
            return "ISO-10646-UCS-4";
        }
        if (b0 == 0x3C && b1 == 0x00 && b2 == 0x00 && b3 == 0x00) {
            // UCS-4, little endian (4321)
            return "ISO-10646-UCS-4";
        }
        if (b0 == 0x00 && b1 == 0x00 && b2 == 0x3C && b3 == 0x00) {
            // UCS-4, unusual octet order (2143)
            // REVISIT: What should this be?
            return "ISO-10646-UCS-4";
        }
        if (b0 == 0x00 && b1 == 0x3C && b2 == 0x00 && b3 == 0x00) {
            // UCS-4, unusual octet order (3412)
            // REVISIT: What should this be?
            return "ISO-10646-UCS-4";
        }
        if (b0 == 0x00 && b1 == 0x3C && b2 == 0x00 && b3 == 0x3F) {
            // UTF-16, big-endian, no BOM
            // (or could turn out to be UCS-2...
            // REVISIT: What should this be?
            return "UTF-16BE";
        }
        if (b0 == 0x3C && b1 == 0x00 && b2 == 0x3F && b3 == 0x00) {
            // UTF-16, little-endian, no BOM
            // (or could turn out to be UCS-2...
            return "UTF-16LE";
        }
        if (b0 == 0x4C && b1 == 0x6F && b2 == 0xA7 && b3 == 0x94) {
            // EBCDIC
            // a la xerces1, return CP037 instead of EBCDIC here
            return "CP037";
        }

        // default encoding
        return "UTF-8";
    }
}