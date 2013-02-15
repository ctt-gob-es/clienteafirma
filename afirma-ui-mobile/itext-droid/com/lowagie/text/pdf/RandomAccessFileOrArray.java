/*
 * $Id: RandomAccessFileOrArray.java 3488 2008-06-02 14:19:26Z blowagie $
 *
 * Copyright 2001, 2002 Paulo Soares
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

import java.io.ByteArrayOutputStream;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.net.URL;
import java.nio.channels.FileChannel;

import com.lowagie.text.Document;
/** An implementation of a RandomAccessFile for input only
 * that accepts a file or a byte array as data source.
 *
 * @author Paulo Soares (psoares@consiste.pt)
 */
public class RandomAccessFileOrArray implements DataInput {

    private MappedRandomAccessFile rf;
    private RandomAccessFile trf;
    private boolean plainRandomAccess;
    private String filename;
    private byte arrayIn[];
    private int arrayInPtr;
    private byte back;
    private boolean isBack = false;

    /** Holds value of property startOffset. */
    private int startOffset = 0;

    public RandomAccessFileOrArray(final String filename) throws IOException {
    	this(filename, false, Document.plainRandomAccess);
    }

    RandomAccessFileOrArray(final String filename, final boolean forceRead, final boolean plainRandomAccess) throws IOException {
        this.plainRandomAccess = plainRandomAccess;
        final File file = new File(filename);
        if (!file.canRead()) {
            if (filename.startsWith("file:/") || filename.startsWith("http://") || filename.startsWith("https://") || filename.startsWith("jar:")) {
                final InputStream is = new URL(filename).openStream();
                try {
                    this.arrayIn = InputStreamToArray(is);
                    return;
                }
                finally {
                    try {is.close();}catch(final IOException ioe){}
                }
            }
            else {
                final InputStream is = BaseFont.getResourceStream(filename);
                if (is == null) {
					throw new IOException(filename + " not found as file or resource.");
				}
                try {
                    this.arrayIn = InputStreamToArray(is);
                    return;
                }
                finally {
                    try {is.close();}catch(final IOException ioe){}
                }
            }
        }
        else if (forceRead) {
            InputStream s = null;
            try {
                s = new FileInputStream(file);
                this.arrayIn = InputStreamToArray(s);
            }
            finally {
                try {if (s != null) {s.close();}}catch(final Exception e){}
            }
        	return;
        }
        this.filename = filename;
        if (plainRandomAccess) {
			this.trf = new RandomAccessFile(filename, "r");
		} else {
			this.rf = new MappedRandomAccessFile(filename, "r");
		}
    }

    public RandomAccessFileOrArray(final URL url) throws IOException {
        final InputStream is = url.openStream();
        try {
            this.arrayIn = InputStreamToArray(is);
        }
        finally {
            try {is.close();}catch(final IOException ioe){}
        }
    }

    RandomAccessFileOrArray(final InputStream is) throws IOException {
        this.arrayIn = InputStreamToArray(is);
    }

    private static byte[] InputStreamToArray(final InputStream is) throws IOException {
        final byte b[] = new byte[8192];
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        while (true) {
            final int read = is.read(b);
            if (read < 1) {
				break;
			}
            out.write(b, 0, read);
        }
        out.close();
        return out.toByteArray();
    }

    public RandomAccessFileOrArray(final byte arrayIn[]) {
        this.arrayIn = arrayIn;
    }

    RandomAccessFileOrArray(final RandomAccessFileOrArray file) {
        this.filename = file.filename;
        this.arrayIn = file.arrayIn;
        this.startOffset = file.startOffset;
        this.plainRandomAccess = file.plainRandomAccess;
    }

    void pushBack(final byte b) {
        this.back = b;
        this.isBack = true;
    }

    public int read() throws IOException {
        if(this.isBack) {
            this.isBack = false;
            return this.back & 0xff;
        }
        if (this.arrayIn == null) {
			return this.plainRandomAccess ? this.trf.read() : this.rf.read();
		} else {
            if (this.arrayInPtr >= this.arrayIn.length) {
				return -1;
			}
            return this.arrayIn[this.arrayInPtr++] & 0xff;
        }
    }

    int read(final byte[] b, int off, int len) throws IOException {
        if (len == 0) {
			return 0;
		}
        int n = 0;
        if (this.isBack) {
            this.isBack = false;
            if (len == 1) {
                b[off] = this.back;
                return 1;
            }
            else {
                n = 1;
                b[off++] = this.back;
                --len;
            }
        }
        if (this.arrayIn == null) {
            return (this.plainRandomAccess ? this.trf.read(b, off, len) : this.rf.read(b, off, len)) + n;
        }
        else {
            if (this.arrayInPtr >= this.arrayIn.length) {
				return -1;
			}
            if (this.arrayInPtr + len > this.arrayIn.length) {
				len = this.arrayIn.length - this.arrayInPtr;
			}
            System.arraycopy(this.arrayIn, this.arrayInPtr, b, off, len);
            this.arrayInPtr += len;
            return len + n;
        }
    }

    public int read(final byte b[]) throws IOException {
        return read(b, 0, b.length);
    }

    @Override
	public void readFully(final byte b[]) throws IOException {
        readFully(b, 0, b.length);
    }

    @Override
	public void readFully(final byte b[], final int off, final int len) throws IOException {
        int n = 0;
        do {
            final int count = read(b, off + n, len - n);
            if (count < 0) {
				throw new EOFException();
			}
            n += count;
        } while (n < len);
    }

    public long skip(final long n) throws IOException {
        return skipBytes((int)n);
    }

    @Override
	public int skipBytes(int n) throws IOException {
        if (n <= 0) {
            return 0;
        }
        int adj = 0;
        if (this.isBack) {
            this.isBack = false;
            if (n == 1) {
                return 1;
            }
            else {
                --n;
                adj = 1;
            }
        }
        int pos;
        int len;
        int newpos;

        pos = getFilePointer();
        len = length();
        newpos = pos + n;
        if (newpos > len) {
            newpos = len;
        }
        seek(newpos);

        /* return the actual number of bytes skipped */
        return newpos - pos + adj;
    }

    void reOpen() throws IOException {
        if (this.filename != null && this.rf == null && this.trf == null) {
            if (this.plainRandomAccess) {
				this.trf = new RandomAccessFile(this.filename, "r");
			} else {
				this.rf = new MappedRandomAccessFile(this.filename, "r");
			}
        }
        seek(0);
    }

    private void insureOpen() throws IOException {
        if (this.filename != null && this.rf == null && this.trf == null) {
            reOpen();
        }
    }

    public boolean isOpen() {
        return this.filename == null || this.rf != null || this.trf != null;
    }

    public void close() throws IOException {
        this.isBack = false;
        if (this.rf != null) {
            this.rf.close();
            this.rf = null;
            // it's very expensive to open a memory mapped file and for the usage pattern of this class
            // in iText it's faster the next re-openings to be done as a plain random access
            // file
            this.plainRandomAccess = true;
        }
        else if (this.trf != null) {
            this.trf.close();
            this.trf = null;
        }
    }

    public int length() throws IOException {
        if (this.arrayIn == null) {
            insureOpen();
            return (int)(this.plainRandomAccess ? this.trf.length() : this.rf.length()) - this.startOffset;
        } else {
			return this.arrayIn.length - this.startOffset;
		}
    }

    public void seek(int pos) throws IOException {
        pos += this.startOffset;
        this.isBack = false;
        if (this.arrayIn == null) {
            insureOpen();
            if (this.plainRandomAccess) {
				this.trf.seek(pos);
			} else {
				this.rf.seek(pos);
			}
        } else {
			this.arrayInPtr = pos;
		}
    }

    public void seek(final long pos) throws IOException {
        seek((int)pos);
    }

    public int getFilePointer() throws IOException {
        insureOpen();
        final int n = this.isBack ? 1 : 0;
        if (this.arrayIn == null) {
            return (int)(this.plainRandomAccess ? this.trf.getFilePointer() : this.rf.getFilePointer()) - n - this.startOffset;
        } else {
			return this.arrayInPtr - n - this.startOffset;
		}
    }

    @Override
	public boolean readBoolean() throws IOException {
        final int ch = this.read();
        if (ch < 0) {
			throw new EOFException();
		}
        return ch != 0;
    }

    @Override
	public byte readByte() throws IOException {
        final int ch = this.read();
        if (ch < 0) {
			throw new EOFException();
		}
        return (byte)ch;
    }

    @Override
	public int readUnsignedByte() throws IOException {
        final int ch = this.read();
        if (ch < 0) {
			throw new EOFException();
		}
        return ch;
    }

    @Override
	public short readShort() throws IOException {
        final int ch1 = this.read();
        final int ch2 = this.read();
        if ((ch1 | ch2) < 0) {
			throw new EOFException();
		}
        return (short)((ch1 << 8) + ch2);
    }

    /**
     * Reads a signed 16-bit number from this stream in little-endian order.
     * The method reads two
     * bytes from this stream, starting at the current stream pointer.
     * If the two bytes read, in order, are
     * <code>b1</code> and <code>b2</code>, where each of the two values is
     * between <code>0</code> and <code>255</code>, inclusive, then the
     * result is equal to:
     * <blockquote><pre>
     *     (short)((b2 &lt;&lt; 8) | b1)
     * </pre></blockquote>
     * <p>
     * This method blocks until the two bytes are read, the end of the
     * stream is detected, or an exception is thrown.
     *
     * @return     the next two bytes of this stream, interpreted as a signed
     *             16-bit number.
     * @exception  EOFException  if this stream reaches the end before reading
     *               two bytes.
     * @exception  IOException   if an I/O error occurs.
     */
    public final short readShortLE() throws IOException {
        final int ch1 = this.read();
        final int ch2 = this.read();
        if ((ch1 | ch2) < 0) {
			throw new EOFException();
		}
        return (short)((ch2 << 8) + (ch1 << 0));
    }

    @Override
	public int readUnsignedShort() throws IOException {
        final int ch1 = this.read();
        final int ch2 = this.read();
        if ((ch1 | ch2) < 0) {
			throw new EOFException();
		}
        return (ch1 << 8) + ch2;
    }

    /**
     * Reads an unsigned 16-bit number from this stream in little-endian order.
     * This method reads
     * two bytes from the stream, starting at the current stream pointer.
     * If the bytes read, in order, are
     * <code>b1</code> and <code>b2</code>, where
     * <code>0&nbsp;&lt;=&nbsp;b1, b2&nbsp;&lt;=&nbsp;255</code>,
     * then the result is equal to:
     * <blockquote><pre>
     *     (b2 &lt;&lt; 8) | b1
     * </pre></blockquote>
     * <p>
     * This method blocks until the two bytes are read, the end of the
     * stream is detected, or an exception is thrown.
     *
     * @return     the next two bytes of this stream, interpreted as an
     *             unsigned 16-bit integer.
     * @exception  EOFException  if this stream reaches the end before reading
     *               two bytes.
     * @exception  IOException   if an I/O error occurs.
     */
    public final int readUnsignedShortLE() throws IOException {
        final int ch1 = this.read();
        final int ch2 = this.read();
        if ((ch1 | ch2) < 0) {
			throw new EOFException();
		}
        return (ch2 << 8) + (ch1 << 0);
    }

    @Override
	public char readChar() throws IOException {
        final int ch1 = this.read();
        final int ch2 = this.read();
        if ((ch1 | ch2) < 0) {
			throw new EOFException();
		}
        return (char)((ch1 << 8) + ch2);
    }



    @Override
	public int readInt() throws IOException {
        final int ch1 = this.read();
        final int ch2 = this.read();
        final int ch3 = this.read();
        final int ch4 = this.read();
        if ((ch1 | ch2 | ch3 | ch4) < 0) {
			throw new EOFException();
		}
        return (ch1 << 24) + (ch2 << 16) + (ch3 << 8) + ch4;
    }

    /**
     * Reads a signed 32-bit integer from this stream in little-endian order.
     * This method reads 4
     * bytes from the stream, starting at the current stream pointer.
     * If the bytes read, in order, are <code>b1</code>,
     * <code>b2</code>, <code>b3</code>, and <code>b4</code>, where
     * <code>0&nbsp;&lt;=&nbsp;b1, b2, b3, b4&nbsp;&lt;=&nbsp;255</code>,
     * then the result is equal to:
     * <blockquote><pre>
     *     (b4 &lt;&lt; 24) | (b3 &lt;&lt; 16) + (b2 &lt;&lt; 8) + b1
     * </pre></blockquote>
     * <p>
     * This method blocks until the four bytes are read, the end of the
     * stream is detected, or an exception is thrown.
     *
     * @return     the next four bytes of this stream, interpreted as an
     *             <code>int</code>.
     * @exception  EOFException  if this stream reaches the end before reading
     *               four bytes.
     * @exception  IOException   if an I/O error occurs.
     */
    public final int readIntLE() throws IOException {
        final int ch1 = this.read();
        final int ch2 = this.read();
        final int ch3 = this.read();
        final int ch4 = this.read();
        if ((ch1 | ch2 | ch3 | ch4) < 0) {
			throw new EOFException();
		}
        return (ch4 << 24) + (ch3 << 16) + (ch2 << 8) + (ch1 << 0);
    }

    /**
     * Reads an unsigned 32-bit integer from this stream. This method reads 4
     * bytes from the stream, starting at the current stream pointer.
     * If the bytes read, in order, are <code>b1</code>,
     * <code>b2</code>, <code>b3</code>, and <code>b4</code>, where
     * <code>0&nbsp;&lt;=&nbsp;b1, b2, b3, b4&nbsp;&lt;=&nbsp;255</code>,
     * then the result is equal to:
     * <blockquote><pre>
     *     (b1 &lt;&lt; 24) | (b2 &lt;&lt; 16) + (b3 &lt;&lt; 8) + b4
     * </pre></blockquote>
     * <p>
     * This method blocks until the four bytes are read, the end of the
     * stream is detected, or an exception is thrown.
     *
     * @return     the next four bytes of this stream, interpreted as a
     *             <code>long</code>.
     * @exception  EOFException  if this stream reaches the end before reading
     *               four bytes.
     * @exception  IOException   if an I/O error occurs.
     */
    public final long readUnsignedInt() throws IOException {
        final long ch1 = this.read();
        final long ch2 = this.read();
        final long ch3 = this.read();
        final long ch4 = this.read();
        if ((ch1 | ch2 | ch3 | ch4) < 0) {
			throw new EOFException();
		}
        return (ch1 << 24) + (ch2 << 16) + (ch3 << 8) + (ch4 << 0);
    }

    public final long readUnsignedIntLE() throws IOException {
        final long ch1 = this.read();
        final long ch2 = this.read();
        final long ch3 = this.read();
        final long ch4 = this.read();
        if ((ch1 | ch2 | ch3 | ch4) < 0) {
			throw new EOFException();
		}
        return (ch4 << 24) + (ch3 << 16) + (ch2 << 8) + (ch1 << 0);
    }

    @Override
	public long readLong() throws IOException {
        return ((long)readInt() << 32) + (readInt() & 0xFFFFFFFFL);
    }

    public final long readLongLE() throws IOException {
        final int i1 = readIntLE();
        final int i2 = readIntLE();
        return ((long)i2 << 32) + (i1 & 0xFFFFFFFFL);
    }

    @Override
	public float readFloat() throws IOException {
        return Float.intBitsToFloat(readInt());
    }

    public final float readFloatLE() throws IOException {
        return Float.intBitsToFloat(readIntLE());
    }

    @Override
	public double readDouble() throws IOException {
        return Double.longBitsToDouble(readLong());
    }

    public final double readDoubleLE() throws IOException {
        return Double.longBitsToDouble(readLongLE());
    }

    @Override
	public String readLine() throws IOException {
        final StringBuffer input = new StringBuffer();
        int c = -1;
        boolean eol = false;

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
                default:
                    input.append((char)c);
                    break;
            }
        }

        if (c == -1 && input.length() == 0) {
            return null;
        }
        return input.toString();
    }

    @Override
	public String readUTF() throws IOException {
        return DataInputStream.readUTF(this);
    }

    /** Getter for property startOffset.
     * @return Value of property startOffset.
     *
     */
    public int getStartOffset() {
        return this.startOffset;
    }

    /** Setter for property startOffset.
     * @param startOffset New value of property startOffset.
     *
     */
    public void setStartOffset(final int startOffset) {
        this.startOffset = startOffset;
    }

    /**
     * @since 2.0.8
     */
    public java.nio.ByteBuffer getNioByteBuffer() throws IOException {
    	if (this.filename != null) {
    		FileChannel channel;
            if (this.plainRandomAccess) {
				channel = this.trf.getChannel();
			} else {
				channel = this.rf.getChannel();
			}
            return channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size());
    	}
    	return java.nio.ByteBuffer.wrap(this.arrayIn);
    }
}
