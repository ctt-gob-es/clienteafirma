/*
 * Copyright (c) 1995, 1997, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */


package es.gob.afirma.standalone.configurator.jre.misc;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;

/**
 * This class encodes a buffer into the classic: "Hexadecimal Dump" format of
 * the past. It is useful for analyzing the contents of binary buffers.
 * The format produced is as follows:
 * <pre>
 * xxxx: 00 11 22 33 44 55 66 77   88 99 aa bb cc dd ee ff ................
 * </pre>
 * Where xxxx is the offset into the buffer in 16 byte chunks, followed
 * by ascii coded hexadecimal bytes followed by the ASCII representation of
 * the bytes or '.' if they are not valid bytes.
 *
 * @author      Chuck McManis
 */

public class HexDumpEncoder extends CharacterEncoder {

    private int offset;
    private int thisLineLength;
    private int currentByte;
    private final byte thisLine[] = new byte[16];

    static void hexDigit(final PrintStream p, final byte x) {
        char c;

        c = (char) ((x >> 4) & 0xf);
        if (c > 9) {
			c = (char) ((c-10) + 'A');
		} else {
			c = (char)(c + '0');
		}
        p.write(c);
        c = (char) (x & 0xf);
        if (c > 9) {
			c = (char)((c-10) + 'A');
		} else {
			c = (char)(c + '0');
		}
        p.write(c);
    }

    @Override
	protected int bytesPerAtom() {
        return (1);
    }

    @Override
	protected int bytesPerLine() {
        return (16);
    }

    @Override
	protected void encodeBufferPrefix(final OutputStream o) throws IOException {
        this.offset = 0;
        super.encodeBufferPrefix(o);
    }

    @Override
	protected void encodeLinePrefix(final OutputStream o, final int len) throws IOException {
        hexDigit(this.pStream, (byte)((this.offset >>> 8) & 0xff));
        hexDigit(this.pStream, (byte)(this.offset & 0xff));
        this.pStream.print(": ");
        this.currentByte = 0;
        this.thisLineLength = len;
    }

    @Override
	protected void encodeAtom(final OutputStream o, final byte buf[], final int off, final int len) throws IOException {
        this.thisLine[this.currentByte] = buf[off];
        hexDigit(this.pStream, buf[off]);
        this.pStream.print(" ");
        this.currentByte++;
        if (this.currentByte == 8) {
			this.pStream.print("  ");
		}
    }

    @Override
	protected void encodeLineSuffix(final OutputStream o) throws IOException {
        if (this.thisLineLength < 16) {
            for (int i = this.thisLineLength; i < 16; i++) {
                this.pStream.print("   ");
                if (i == 7) {
					this.pStream.print("  ");
				}
            }
        }
        this.pStream.print(" ");
        for (int i = 0; i < this.thisLineLength; i++) {
            if ((this.thisLine[i] < ' ') || (this.thisLine[i] > 'z')) {
                this.pStream.print(".");
            } else {
                this.pStream.write(this.thisLine[i]);
            }
        }
        this.pStream.println();
        this.offset += this.thisLineLength;
    }

}
