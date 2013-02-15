/*
 * Copyright 2003-2008 by Paulo Soares.
 *
 * This code was originally released in 2001 by SUN (see class
 * com.sun.media.imageio.plugins.tiff.TIFFDirectory.java)
 * using the BSD license in a specific wording. In a mail dating from
 * January 23, 2008, Brian Burkhalter (@sun.com) gave us permission
 * to use the code under the following version of the BSD license:
 *
 * Copyright (c) 2006 Sun Microsystems, Inc. All  Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * - Redistribution of source code must retain the above copyright
 *   notice, this  list of conditions and the following disclaimer.
 *
 * - Redistribution in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in
 *   the documentation and/or other materials provided with the
 *   distribution.
 *
 * Neither the name of Sun Microsystems, Inc. or the names of
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 *
 * This software is provided "AS IS," without a warranty of any
 * kind. ALL EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND
 * WARRANTIES, INCLUDING ANY IMPLIED WARRANTY OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT, ARE HEREBY
 * EXCLUDED. SUN MIDROSYSTEMS, INC. ("SUN") AND ITS LICENSORS SHALL
 * NOT BE LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF
 * USING, MODIFYING OR DISTRIBUTING THIS SOFTWARE OR ITS
 * DERIVATIVES. IN NO EVENT WILL SUN OR ITS LICENSORS BE LIABLE FOR
 * ANY LOST REVENUE, PROFIT OR DATA, OR FOR DIRECT, INDIRECT, SPECIAL,
 * CONSEQUENTIAL, INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER CAUSED AND
 * REGARDLESS OF THE THEORY OF LIABILITY, ARISING OUT OF THE USE OF OR
 * INABILITY TO USE THIS SOFTWARE, EVEN IF SUN HAS BEEN ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGES.
 *
 * You acknowledge that this software is not designed or intended for
 * use in the design, construction, operation or maintenance of any
 * nuclear facility.
 */
package com.lowagie.text.pdf.codec;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import com.lowagie.text.pdf.RandomAccessFileOrArray;

/**
 * A class representing an Image File Directory (IFD) from a TIFF 6.0
 * stream.  The TIFF file format is described in more detail in the
 * comments for the TIFFDescriptor class.
 *
 * <p> A TIFF IFD consists of a set of TIFFField tags.  Methods are
 * provided to query the set of tags and to obtain the raw field
 * array.  In addition, convenience methods are provided for acquiring
 * the values of tags that contain a single value that fits into a
 * byte, int, long, float, or double.
 *
 * <p> Every TIFF file is made up of one or more public IFDs that are
 * joined in a linked list, rooted in the file header.  A file may
 * also contain so-called private IFDs that are referenced from
 * tag data and do not appear in the main list.
 *
 * <p><b> This class is not a committed part of the JAI API.  It may
 * be removed or changed in future releases of JAI.</b>
 *
 * @see TIFFField
 */
class TIFFDirectory extends Object implements Serializable {

    private static final long serialVersionUID = -168636766193675380L;

	/** A boolean storing the endianness of the stream. */
    private boolean isBigEndian;

    /** The number of entries in the IFD. */
    private int numEntries;

    /** An array of TIFFFields. */
    private TIFFField[] fields;

    /** A Hashtable indexing the fields by tag number. */
    private final Hashtable fieldIndex = new Hashtable();

    /** The offset of this IFD. */
    private long IFDOffset = 8;

    /** The offset of the next IFD. */
    private long nextIFDOffset = 0;

    /** The default constructor. */
    TIFFDirectory() {}

    private static boolean isValidEndianTag(final int endian) {
        return endian == 0x4949 || endian == 0x4d4d;
    }

    /**
     * Constructs a TIFFDirectory from a SeekableStream.
     * The directory parameter specifies which directory to read from
     * the linked list present in the stream; directory 0 is normally
     * read but it is possible to store multiple images in a single
     * TIFF file by maintaining multiple directories.
     *
     * @param stream a SeekableStream to read from.
     * @param directory the index of the directory to read.
     */
    TIFFDirectory(final RandomAccessFileOrArray stream, final int directory)
    throws IOException {

        final long global_save_offset = stream.getFilePointer();
        long ifd_offset;

        // Read the TIFF header
        stream.seek(0L);
        final int endian = stream.readUnsignedShort();
        if (!isValidEndianTag(endian)) {
            throw new
            IllegalArgumentException("Bad endianness tag (not 0x4949 or 0x4d4d).");
        }
        this.isBigEndian = endian == 0x4d4d;

        final int magic = readUnsignedShort(stream);
        if (magic != 42) {
            throw new
            IllegalArgumentException("Bad magic number, should be 42.");
        }

        // Get the initial ifd offset as an unsigned int (using a long)
        ifd_offset = readUnsignedInt(stream);

        for (int i = 0; i < directory; i++) {
            if (ifd_offset == 0L) {
                throw new
                IllegalArgumentException("Directory number too large.");
            }

            stream.seek(ifd_offset);
            final int entries = readUnsignedShort(stream);
            stream.skip(12*entries);

            ifd_offset = readUnsignedInt(stream);
        }

        stream.seek(ifd_offset);
        initialize(stream);
        stream.seek(global_save_offset);
    }



    private static final int[] sizeOfType = {
        0, //  0 = n/a
        1, //  1 = byte
        1, //  2 = ascii
        2, //  3 = short
        4, //  4 = long
        8, //  5 = rational
        1, //  6 = sbyte
        1, //  7 = undefined
        2, //  8 = sshort
        4, //  9 = slong
        8, // 10 = srational
        4, // 11 = float
        8  // 12 = double
    };

    private void initialize(final RandomAccessFileOrArray stream) throws IOException {
        long nextTagOffset = 0L;
        final long maxOffset = stream.length();
        int i, j;

        this.IFDOffset = stream.getFilePointer();

        this.numEntries = readUnsignedShort(stream);
        this.fields = new TIFFField[this.numEntries];

        for (i = 0; i < this.numEntries && nextTagOffset < maxOffset; i++) {
            final int tag = readUnsignedShort(stream);
            final int type = readUnsignedShort(stream);
            int count = (int)readUnsignedInt(stream);
            boolean processTag = true;

            // The place to return to to read the next tag
            nextTagOffset = stream.getFilePointer() + 4;

            try {
                // If the tag data can't fit in 4 bytes, the next 4 bytes
                // contain the starting offset of the data
                if (count*sizeOfType[type] > 4) {
                    final long valueOffset = readUnsignedInt(stream);

                    // bounds check offset for EOF
                    if (valueOffset < maxOffset) {
                    	stream.seek(valueOffset);
                    }
                    else {
                    	// bad offset pointer .. skip tag
                    	processTag = false;
                    }
                }
            } catch (final ArrayIndexOutOfBoundsException ae) {
                // if the data type is unknown we should skip this TIFF Field
                processTag = false;
            }

            if (processTag) {
            this.fieldIndex.put(new Integer(tag), new Integer(i));
            Object obj = null;

            switch (type) {
                case TIFFField.TIFF_BYTE:
                case TIFFField.TIFF_SBYTE:
                case TIFFField.TIFF_UNDEFINED:
                case TIFFField.TIFF_ASCII:
                    final byte[] bvalues = new byte[count];
                    stream.readFully(bvalues, 0, count);

                    if (type == TIFFField.TIFF_ASCII) {

                        // Can be multiple strings
                        int index = 0, prevIndex = 0;
                        final ArrayList v = new ArrayList();

                        while (index < count) {

                            while (index < count && bvalues[index++] != 0) {
								;
							}

                            // When we encountered zero, means one string has ended
                            v.add(new String(bvalues, prevIndex,
                            index - prevIndex) );
                            prevIndex = index;
                        }

                        count = v.size();
                        final String strings[] = new String[count];
                        for (int c = 0 ; c < count; c++) {
                            strings[c] = (String)v.get(c);
                        }

                        obj = strings;
                    } else {
                        obj = bvalues;
                    }

                    break;

                case TIFFField.TIFF_SHORT:
                    final char[] cvalues = new char[count];
                    for (j = 0; j < count; j++) {
                        cvalues[j] = (char)readUnsignedShort(stream);
                    }
                    obj = cvalues;
                    break;

                case TIFFField.TIFF_LONG:
                    final long[] lvalues = new long[count];
                    for (j = 0; j < count; j++) {
                        lvalues[j] = readUnsignedInt(stream);
                    }
                    obj = lvalues;
                    break;

                case TIFFField.TIFF_RATIONAL:
                    final long[][] llvalues = new long[count][2];
                    for (j = 0; j < count; j++) {
                        llvalues[j][0] = readUnsignedInt(stream);
                        llvalues[j][1] = readUnsignedInt(stream);
                    }
                    obj = llvalues;
                    break;

                case TIFFField.TIFF_SSHORT:
                    final short[] svalues = new short[count];
                    for (j = 0; j < count; j++) {
                        svalues[j] = readShort(stream);
                    }
                    obj = svalues;
                    break;

                case TIFFField.TIFF_SLONG:
                    final int[] ivalues = new int[count];
                    for (j = 0; j < count; j++) {
                        ivalues[j] = readInt(stream);
                    }
                    obj = ivalues;
                    break;

                case TIFFField.TIFF_SRATIONAL:
                    final int[][] iivalues = new int[count][2];
                    for (j = 0; j < count; j++) {
                        iivalues[j][0] = readInt(stream);
                        iivalues[j][1] = readInt(stream);
                    }
                    obj = iivalues;
                    break;

                case TIFFField.TIFF_FLOAT:
                    final float[] fvalues = new float[count];
                    for (j = 0; j < count; j++) {
                        fvalues[j] = readFloat(stream);
                    }
                    obj = fvalues;
                    break;

                case TIFFField.TIFF_DOUBLE:
                    final double[] dvalues = new double[count];
                    for (j = 0; j < count; j++) {
                        dvalues[j] = readDouble(stream);
                    }
                    obj = dvalues;
                    break;

                default:
                    break;
            }

            this.fields[i] = new TIFFField(tag, type, count, obj);
            }

            stream.seek(nextTagOffset);
        }

        // Read the offset of the next IFD.
        try {
            this.nextIFDOffset = readUnsignedInt(stream);
        }
        catch (final Exception e) {
            // broken tiffs may not have this pointer
            this.nextIFDOffset = 0;
        }
    }

    /** Returns the number of directory entries. */
    public int getNumEntries() {
        return this.numEntries;
    }

    /**
     * Returns the value of a given tag as a TIFFField,
     * or null if the tag is not present.
     */
    TIFFField getField(final int tag) {
        final Integer i = (Integer)this.fieldIndex.get(new Integer(tag));
        if (i == null) {
            return null;
        } else {
            return this.fields[i.intValue()];
        }
    }

    /**
     * Returns true if a tag appears in the directory.
     */
    boolean isTagPresent(final int tag) {
        return this.fieldIndex.containsKey(new Integer(tag));
    }

    /**
     * Returns an ordered array of ints indicating the tag
     * values.
     */
    public int[] getTags() {
        final int[] tags = new int[this.fieldIndex.size()];
        final Enumeration e = this.fieldIndex.keys();
        int i = 0;

        while (e.hasMoreElements()) {
            tags[i++] = ((Integer)e.nextElement()).intValue();
        }

        return tags;
    }

    /**
     * Returns an array of TIFFFields containing all the fields
     * in this directory.
     */
    public TIFFField[] getFields() {
        return this.fields;
    }

    /**
     * Returns the value of a particular index of a given tag as a
     * byte.  The caller is responsible for ensuring that the tag is
     * present and has type TIFFField.TIFF_SBYTE, TIFF_BYTE, or
     * TIFF_UNDEFINED.
     */
    private byte getFieldAsByte(final int tag, final int index) {
        final Integer i = (Integer)this.fieldIndex.get(new Integer(tag));
        final byte [] b = this.fields[i.intValue()].getAsBytes();
        return b[index];
    }



    /**
     * Returns the value of a particular index of a given tag as a
     * long.  The caller is responsible for ensuring that the tag is
     * present and has type TIFF_BYTE, TIFF_SBYTE, TIFF_UNDEFINED,
     * TIFF_SHORT, TIFF_SSHORT, TIFF_SLONG or TIFF_LONG.
     */
    private long getFieldAsLong(final int tag, final int index) {
        final Integer i = (Integer)this.fieldIndex.get(new Integer(tag));
        return this.fields[i.intValue()].getAsLong(index);
    }

    /**
     * Returns the value of index 0 of a given tag as a
     * long.  The caller is responsible for ensuring that the tag is
     * present and has type TIFF_BYTE, TIFF_SBYTE, TIFF_UNDEFINED,
     * TIFF_SHORT, TIFF_SSHORT, TIFF_SLONG or TIFF_LONG.
     */
    long getFieldAsLong(final int tag) {
        return getFieldAsLong(tag, 0);
    }

    /**
     * Returns the value of a particular index of a given tag as a
     * float.  The caller is responsible for ensuring that the tag is
     * present and has numeric type (all but TIFF_UNDEFINED and
     * TIFF_ASCII).
     */
    private float getFieldAsFloat(final int tag, final int index) {
        final Integer i = (Integer)this.fieldIndex.get(new Integer(tag));
        return this.fields[i.intValue()].getAsFloat(index);
    }



    /**
     * Returns the value of a particular index of a given tag as a
     * double.  The caller is responsible for ensuring that the tag is
     * present and has numeric type (all but TIFF_UNDEFINED and
     * TIFF_ASCII).
     */
    private double getFieldAsDouble(final int tag, final int index) {
        final Integer i = (Integer)this.fieldIndex.get(new Integer(tag));
        return this.fields[i.intValue()].getAsDouble(index);
    }



    // Methods to read primitive data types from the stream

    private short readShort(final RandomAccessFileOrArray stream)
    throws IOException {
        if (this.isBigEndian) {
            return stream.readShort();
        } else {
            return stream.readShortLE();
        }
    }

    private int readUnsignedShort(final RandomAccessFileOrArray stream)
    throws IOException {
        if (this.isBigEndian) {
            return stream.readUnsignedShort();
        } else {
            return stream.readUnsignedShortLE();
        }
    }

    private int readInt(final RandomAccessFileOrArray stream)
    throws IOException {
        if (this.isBigEndian) {
            return stream.readInt();
        } else {
            return stream.readIntLE();
        }
    }

    private long readUnsignedInt(final RandomAccessFileOrArray stream)
    throws IOException {
        if (this.isBigEndian) {
            return stream.readUnsignedInt();
        } else {
            return stream.readUnsignedIntLE();
        }
    }

    private long readLong(final RandomAccessFileOrArray stream)
    throws IOException {
        if (this.isBigEndian) {
            return stream.readLong();
        } else {
            return stream.readLongLE();
        }
    }

    private float readFloat(final RandomAccessFileOrArray stream)
    throws IOException {
        if (this.isBigEndian) {
            return stream.readFloat();
        } else {
            return stream.readFloatLE();
        }
    }

    private double readDouble(final RandomAccessFileOrArray stream)
    throws IOException {
        if (this.isBigEndian) {
            return stream.readDouble();
        } else {
            return stream.readDoubleLE();
        }
    }

    private static int readUnsignedShort(final RandomAccessFileOrArray stream,
    final boolean isBigEndian)
    throws IOException {
        if (isBigEndian) {
            return stream.readUnsignedShort();
        } else {
            return stream.readUnsignedShortLE();
        }
    }

    private static long readUnsignedInt(final RandomAccessFileOrArray stream,
    final boolean isBigEndian)
    throws IOException {
        if (isBigEndian) {
            return stream.readUnsignedInt();
        } else {
            return stream.readUnsignedIntLE();
        }
    }

    // Utilities



    /**
     * Returns a boolean indicating whether the byte order used in the
     * the TIFF file is big-endian (i.e. whether the byte order is from
     * the most significant to the least significant)
     */
    public boolean isBigEndian() {
        return this.isBigEndian;
    }

    /**
     * Returns the offset of the IFD corresponding to this
     * <code>TIFFDirectory</code>.
     */
    public long getIFDOffset() {
        return this.IFDOffset;
    }

    /**
     * Returns the offset of the next IFD after the IFD corresponding to this
     * <code>TIFFDirectory</code>.
     */
    public long getNextIFDOffset() {
        return this.nextIFDOffset;
    }
}
