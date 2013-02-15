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

import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

/**
 * This class expands a string into a list of numbers. The main use is to select a
 * range of pages.
 * <p>
 * The general syntax is:<br>
 * [!][o][odd][e][even]start-end
 * <p>
 * You can have multiple ranges separated by commas ','. The '!' modifier removes the
 * range from what is already selected. The range changes are incremental, that is,
 * numbers are added or deleted as the range appears. The start or the end, but not both, can be omitted.
 */
class SequenceList {
	private static final int COMMA = 1;
    private static final int MINUS = 2;
    private static final int NOT = 3;
    private static final int TEXT = 4;
    private static final int NUMBER = 5;
    private static final int END = 6;
    private static final char EOT = '\uffff';

    private static final int FIRST = 0;
    private static final int DIGIT = 1;
    private static final int OTHER = 2;
    private static final int DIGIT2 = 3;
    private static final String NOT_OTHER = "-,!0123456789";

    private final char text[];
    private int ptr;
    private int number;
    private String other;

    private int low;
    private int high;
    private boolean odd;
    private boolean even;
    private boolean inverse;

    private SequenceList(final String range) {
        this.ptr = 0;
        this.text = range.toCharArray();
    }

    private char nextChar() {
        while (true) {
            if (this.ptr >= this.text.length) {
				return EOT;
			}
            final char c = this.text[this.ptr++];
            if (c > ' ') {
				return c;
			}
        }
    }

    private void putBack() {
        --this.ptr;
        if (this.ptr < 0) {
			this.ptr = 0;
		}
    }

    private int getType() {
        final StringBuffer buf = new StringBuffer();
        int state = FIRST;
        while (true) {
            final char c = nextChar();
            if (c == EOT) {
                if (state == DIGIT) {
                    this.number = Integer.parseInt(this.other = buf.toString());
                    return NUMBER;
                }
                else if (state == OTHER) {
                    this.other = buf.toString().toLowerCase();
                    return TEXT;
                }
                return END;
            }
            switch (state) {
                case FIRST:
                    switch (c) {
                        case '!':
                            return NOT;
                        case '-':
                            return MINUS;
                        case ',':
                            return COMMA;
                    }
                    buf.append(c);
                    if (c >= '0' && c <= '9') {
						state = DIGIT;
					} else {
						state = OTHER;
					}
                    break;
                case DIGIT:
                    if (c >= '0' && c <= '9') {
						buf.append(c);
					} else {
                        putBack();
                        this.number = Integer.parseInt(this.other = buf.toString());
                        return NUMBER;
                    }
                    break;
                case OTHER:
                    if (NOT_OTHER.indexOf(c) < 0) {
						buf.append(c);
					} else {
                        putBack();
                        this.other = buf.toString().toLowerCase();
                        return TEXT;
                    }
                    break;
            }
        }
    }

    private void otherProc() {
        if (this.other.equals("odd") || this.other.equals("o")) {
            this.odd = true;
            this.even = false;
        }
        else if (this.other.equals("even") || this.other.equals("e")) {
            this.odd = false;
            this.even = true;
        }
    }

    private boolean getAttributes() {
        this.low = -1;
        this.high = -1;
        this.odd = this.even = this.inverse = false;
        int state = OTHER;
        while (true) {
            final int type = getType();
            if (type == END || type == COMMA) {
                if (state == DIGIT) {
					this.high = this.low;
				}
                return type == END;
            }
            switch (state) {
                case OTHER:
                    switch (type) {
                        case NOT:
                            this.inverse = true;
                            break;
                        case MINUS:
                            state = DIGIT2;
                            break;
                        default:
                            if (type == NUMBER) {
                                this.low = this.number;
                                state = DIGIT;
                            } else {
								otherProc();
							}
                            break;
                    }
                    break;
                case DIGIT:
                    switch (type) {
                        case NOT:
                            this.inverse = true;
                            state = OTHER;
                            this.high = this.low;
                            break;
                        case MINUS:
                            state = DIGIT2;
                            break;
                        default:
                            this.high = this.low;
                            state = OTHER;
                            otherProc();
                            break;
                    }
                    break;
                case DIGIT2:
                    switch (type) {
                        case NOT:
                            this.inverse = true;
                            state = OTHER;
                            break;
                        case MINUS:
                            break;
                        case NUMBER:
                            this.high = this.number;
                            state = OTHER;
                            break;
                        default:
                            state = OTHER;
                            otherProc();
                            break;
                    }
                    break;
            }
        }
    }

    /**
     * Generates a list of numbers from a string.
     * @param ranges the comma separated ranges
     * @param maxNumber the maximum number in the range
     * @return a list with the numbers as <CODE>Integer</CODE>
     */
    public static List expand(final String ranges, final int maxNumber) {
        final SequenceList parse = new SequenceList(ranges);
        final LinkedList list = new LinkedList();
        boolean sair = false;
        while (!sair) {
            sair = parse.getAttributes();
            if (parse.low == -1 && parse.high == -1 && !parse.even && !parse.odd) {
				continue;
			}
            if (parse.low < 1) {
				parse.low = 1;
			}
            if (parse.high < 1 || parse.high > maxNumber) {
				parse.high = maxNumber;
			}
            if (parse.low > maxNumber) {
				parse.low = maxNumber;
			}

            //System.out.println("low="+parse.low+",high="+parse.high+",odd="+parse.odd+",even="+parse.even+",inverse="+parse.inverse);
            int inc = 1;
            if (parse.inverse) {
                if (parse.low > parse.high) {
                    final int t = parse.low;
                    parse.low = parse.high;
                    parse.high = t;
                }
                for (final ListIterator it = list.listIterator(); it.hasNext();) {
                    final int n = ((Integer)it.next()).intValue();
                    if (parse.even && (n & 1) == 1) {
						continue;
					}
                    if (parse.odd && (n & 1) == 0) {
						continue;
					}
                    if (n >= parse.low && n <= parse.high) {
						it.remove();
					}
                }
            }
            else {
                if (parse.low > parse.high) {
                    inc = -1;
                    if (parse.odd || parse.even) {
                        --inc;
                        if (parse.even) {
							parse.low &= ~1;
						} else {
							parse.low -= (parse.low & 1) == 1 ? 0 : 1;
						}
                    }
                    for (int k = parse.low; k >= parse.high; k += inc) {
						list.add(new Integer(k));
					}
                }
                else {
                    if (parse.odd || parse.even) {
                        ++inc;
                        if (parse.odd) {
							parse.low |= 1;
						} else {
							parse.low += (parse.low & 1) == 1 ? 1 : 0;
						}
                    }
                    for (int k = parse.low; k <= parse.high; k += inc) {
                        list.add(new Integer(k));
                    }
                }
            }
//            for (int k = 0; k < list.size(); ++k)
//                System.out.print(((Integer)list.get(k)).intValue() + ",");
//            System.out.println();
        }
        return list;
    }
}