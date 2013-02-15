/*
 * $Id: PRTokeniser.java 3947 2009-06-02 17:51:05Z trumpetinc $
 *
 * Copyright 2001, 2002 by Paulo Soares.
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
/**
 *
 * @author  Paulo Soares (psoares@consiste.pt)
 */
public class PRTokeniser {

    static final int TK_NUMBER = 1;
    static final int TK_STRING = 2;
    static final int TK_NAME = 3;
    static final int TK_COMMENT = 4;
    static final int TK_START_ARRAY = 5;
    static final int TK_END_ARRAY = 6;
    static final int TK_START_DIC = 7;
    static final int TK_END_DIC = 8;
    static final int TK_REF = 9;
    static final int TK_OTHER = 10;
    private static final boolean delims[] = {
        true,  true,  false, false, false, false, false, false, false, false,
        true,  true,  false, true,  true,  false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, true,  false, false, false, false, true,  false,
        false, true,  true,  false, false, false, false, false, true,  false,
        false, false, false, false, false, false, false, false, false, false,
        false, true,  false, true,  false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, true,  false, true,  false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false, false, false, false,
        false, false, false, false, false, false, false};

    private static final String EMPTY = "";


    private final RandomAccessFileOrArray file;
    private int type;
    private String stringValue;
    private int reference;
    private int generation;
    private boolean hexString;

    PRTokeniser(final String filename) throws IOException {
        this.file = new RandomAccessFileOrArray(filename);
    }

    public PRTokeniser(final byte pdfIn[]) {
        this.file = new RandomAccessFileOrArray(pdfIn);
    }

    PRTokeniser(final RandomAccessFileOrArray file) {
        this.file = file;
    }

    void seek(final int pos) throws IOException {
        this.file.seek(pos);
    }

    public int getFilePointer() throws IOException {
        return this.file.getFilePointer();
    }

    void close() throws IOException {
        this.file.close();
    }

    int length() throws IOException {
        return this.file.length();
    }

    int read() throws IOException {
        return this.file.read();
    }

    public RandomAccessFileOrArray getSafeFile() {
        return new RandomAccessFileOrArray(this.file);
    }

    public RandomAccessFileOrArray getFile() {
        return this.file;
    }

    String readString(int size) throws IOException {
        final StringBuffer buf = new StringBuffer();
        int ch;
        while (size-- > 0) {
            ch = this.file.read();
            if (ch == -1) {
				break;
			}
            buf.append((char)ch);
        }
        return buf.toString();
    }

    static final boolean isWhitespace(final int ch) {
        return ch == 0 || ch == 9 || ch == 10 || ch == 12 || ch == 13 || ch == 32;
    }





    public int getTokenType() {
        return this.type;
    }

    public String getStringValue() {
        return this.stringValue;
    }

    public int getReference() {
        return this.reference;
    }

    public int getGeneration() {
        return this.generation;
    }

    void backOnePosition(final int ch) {
        if (ch != -1) {
			this.file.pushBack((byte)ch);
		}
    }

    void throwError(final String error) throws IOException {
        throw new IOException(error + " at file pointer " + this.file.getFilePointer());
    }

    char checkPdfHeader() throws IOException {
        this.file.setStartOffset(0);
        final String str = readString(1024);
        final int idx = str.indexOf("%PDF-");
        if (idx < 0) {
			throw new IOException("PDF header signature not found.");
		}
        this.file.setStartOffset(idx);
        return str.charAt(idx + 7);
    }

    void checkFdfHeader() throws IOException {
        this.file.setStartOffset(0);
        final String str = readString(1024);
        final int idx = str.indexOf("%FDF-1.2");
        if (idx < 0) {
			throw new IOException("FDF header signature not found.");
		}
        this.file.setStartOffset(idx);
    }

    public int getStartxref() throws IOException {
        final int size = Math.min(1024, this.file.length());
        final int pos = this.file.length() - size;
        this.file.seek(pos);
        final String str = readString(1024);
        final int idx = str.lastIndexOf("startxref");
        if (idx < 0) {
			throw new IOException("PDF startxref not found.");
		}
        return pos + idx;
    }

    public static int getHex(final int v) {
        if (v >= '0' && v <= '9') {
			return v - '0';
		}
        if (v >= 'A' && v <= 'F') {
			return v - 'A' + 10;
		}
        if (v >= 'a' && v <= 'f') {
			return v - 'a' + 10;
		}
        return -1;
    }

    void nextValidToken() throws IOException {
        int level = 0;
        String n1 = null;
        String n2 = null;
        int ptr = 0;
        while (nextToken()) {
            if (this.type == TK_COMMENT) {
				continue;
			}
            switch (level) {
                case 0:
                {
                    if (this.type != TK_NUMBER) {
						return;
					}
                    ptr = this.file.getFilePointer();
                    n1 = this.stringValue;
                    ++level;
                    break;
                }
                case 1:
                {
                    if (this.type != TK_NUMBER) {
                        this.file.seek(ptr);
                        this.type = TK_NUMBER;
                        this.stringValue = n1;
                        return;
                    }
                    n2 = this.stringValue;
                    ++level;
                    break;
                }
                default:
                {
                    if (this.type != TK_OTHER || !this.stringValue.equals("R")) {
                        this.file.seek(ptr);
                        this.type = TK_NUMBER;
                        this.stringValue = n1;
                        return;
                    }
                    this.type = TK_REF;
                    this.reference = Integer.parseInt(n1);
                    this.generation = Integer.parseInt(n2);
                    return;
                }
            }
        }
        // if we hit here, the file is either corrupt (stream ended unexpectedly),
        // or the last token ended exactly at the end of a stream.  This last
        // case can occur inside an Object Stream.
    }

    boolean nextToken() throws IOException {
        int ch = 0;
        do {
            ch = this.file.read();
        } while (ch != -1 && isWhitespace(ch));
        if (ch == -1) {
			return false;
		}

        // Note:  We have to initialize stringValue here, after we've looked for the end of the stream,
        // to ensure that we don't lose the value of a token that might end exactly at the end
        // of the stream
        StringBuffer outBuf = null;
        this.stringValue = EMPTY;

        switch (ch) {
            case '[':
                this.type = TK_START_ARRAY;
                break;
            case ']':
                this.type = TK_END_ARRAY;
                break;
            case '/':
            {
                outBuf = new StringBuffer();
                this.type = TK_NAME;
                while (true) {
                    ch = this.file.read();
                    if (delims[ch + 1]) {
						break;
					}
                    if (ch == '#') {
                        ch = (getHex(this.file.read()) << 4) + getHex(this.file.read());
                    }
                    outBuf.append((char)ch);
                }
                backOnePosition(ch);
                break;
            }
            case '>':
                ch = this.file.read();
                if (ch != '>') {
					throwError("'>' not expected");
				}
                this.type = TK_END_DIC;
                break;
            case '<':
            {
                int v1 = this.file.read();
                if (v1 == '<') {
                    this.type = TK_START_DIC;
                    break;
                }
                outBuf = new StringBuffer();
                this.type = TK_STRING;
                this.hexString = true;
                int v2 = 0;
                while (true) {
                    while (isWhitespace(v1)) {
						v1 = this.file.read();
					}
                    if (v1 == '>') {
						break;
					}
                    v1 = getHex(v1);
                    if (v1 < 0) {
						break;
					}
                    v2 = this.file.read();
                    while (isWhitespace(v2)) {
						v2 = this.file.read();
					}
                    if (v2 == '>') {
                        ch = v1 << 4;
                        outBuf.append((char)ch);
                        break;
                    }
                    v2 = getHex(v2);
                    if (v2 < 0) {
						break;
					}
                    ch = (v1 << 4) + v2;
                    outBuf.append((char)ch);
                    v1 = this.file.read();
                }
                if (v1 < 0 || v2 < 0) {
					throwError("Error reading string");
				}
                break;
            }
            case '%':
                this.type = TK_COMMENT;
                do {
                    ch = this.file.read();
                } while (ch != -1 && ch != '\r' && ch != '\n');
                break;
            case '(':
            {
                outBuf = new StringBuffer();
                this.type = TK_STRING;
                this.hexString = false;
                int nesting = 0;
                while (true) {
                    ch = this.file.read();
                    if (ch == -1) {
						break;
					}
                    if (ch == '(') {
                        ++nesting;
                    }
                    else if (ch == ')') {
                        --nesting;
                    }
                    else if (ch == '\\') {
                        boolean lineBreak = false;
                        ch = this.file.read();
                        switch (ch) {
                            case 'n':
                                ch = '\n';
                                break;
                            case 'r':
                                ch = '\r';
                                break;
                            case 't':
                                ch = '\t';
                                break;
                            case 'b':
                                ch = '\b';
                                break;
                            case 'f':
                                ch = '\f';
                                break;
                            case '(':
                            case ')':
                            case '\\':
                                break;
                            case '\r':
                                lineBreak = true;
                                ch = this.file.read();
                                if (ch != '\n') {
									backOnePosition(ch);
								}
                                break;
                            case '\n':
                                lineBreak = true;
                                break;
                            default:
                            {
                                if (ch < '0' || ch > '7') {
                                    break;
                                }
                                int octal = ch - '0';
                                ch = this.file.read();
                                if (ch < '0' || ch > '7') {
                                    backOnePosition(ch);
                                    ch = octal;
                                    break;
                                }
                                octal = (octal << 3) + ch - '0';
                                ch = this.file.read();
                                if (ch < '0' || ch > '7') {
                                    backOnePosition(ch);
                                    ch = octal;
                                    break;
                                }
                                octal = (octal << 3) + ch - '0';
                                ch = octal & 0xff;
                                break;
                            }
                        }
                        if (lineBreak) {
							continue;
						}
                        if (ch < 0) {
							break;
						}
                    }
                    else if (ch == '\r') {
                        ch = this.file.read();
                        if (ch < 0) {
							break;
						}
                        if (ch != '\n') {
                            backOnePosition(ch);
                            ch = '\n';
                        }
                    }
                    if (nesting == -1) {
						break;
					}
                    outBuf.append((char)ch);
                }
                if (ch == -1) {
					throwError("Error reading string");
				}
                break;
            }
            default:
            {
                outBuf = new StringBuffer();
                if (ch == '-' || ch == '+' || ch == '.' || ch >= '0' && ch <= '9') {
                    this.type = TK_NUMBER;
                    do {
                        outBuf.append((char)ch);
                        ch = this.file.read();
                    } while (ch != -1 && (ch >= '0' && ch <= '9' || ch == '.'));
                }
                else {
                    this.type = TK_OTHER;
                    do {
                        outBuf.append((char)ch);
                        ch = this.file.read();
                    } while (!delims[ch + 1]);
                }
                backOnePosition(ch);
                break;
            }
        }
        if (outBuf != null) {
			this.stringValue = outBuf.toString();
		}
        return true;
    }

    int intValue() {
        return Integer.parseInt(this.stringValue);
    }

    boolean readLineSegment(final byte input[]) throws IOException {
        int c = -1;
        boolean eol = false;
        int ptr = 0;
        final int len = input.length;
	// ssteward, pdftk-1.10, 040922:
	// skip initial whitespace; added this because PdfReader.rebuildXref()
	// assumes that line provided by readLineSegment does not have init. whitespace;
	if ( ptr < len ) {
	    while ( isWhitespace( c = read() ) ) {
			;
		}
	}
	while ( !eol && ptr < len ) {
	    switch (c) {
                case -1:
                case '\n':
                    eol = true;
                    break;
                case '\r':
                    eol = true;
                    final int cur = getFilePointer();
                    if (read() != '\n') {
                        seek(cur);
                    }
                    break;
                default:
                    input[ptr++] = (byte)c;
                    break;
            }

	    // break loop? do it before we read() again
	    if( eol || len <= ptr ) {
		break;
	    }
	    else {
		c = read();
	    }
        }
        if (ptr >= len) {
            eol = false;
            while (!eol) {
                switch (c = read()) {
                    case -1:
                    case '\n':
                        eol = true;
                        break;
                    case '\r':
                        eol = true;
                        final int cur = getFilePointer();
                        if (read() != '\n') {
                            seek(cur);
                        }
                        break;
                }
            }
        }

        if (c == -1 && ptr == 0) {
            return false;
        }
        if (ptr + 2 <= len) {
            input[ptr++] = (byte)' ';
            input[ptr] = (byte)'X';
        }
        return true;
    }

    static int[] checkObjectStart(final byte line[]) {
        try {
            final PRTokeniser tk = new PRTokeniser(line);
            int num = 0;
            int gen = 0;
            if (!tk.nextToken() || tk.getTokenType() != TK_NUMBER) {
				return null;
			}
            num = tk.intValue();
            if (!tk.nextToken() || tk.getTokenType() != TK_NUMBER) {
				return null;
			}
            gen = tk.intValue();
            if (!tk.nextToken()) {
				return null;
			}
            if (!tk.getStringValue().equals("obj")) {
				return null;
			}
            return new int[]{num, gen};
        }
        catch (final Exception ioe) {
            // empty on purpose
        }
        return null;
    }

    public boolean isHexString() {
        return this.hexString;
    }

}
