/*
 * $Id: JBIG2SegmentReader.java 3714 2009-02-20 21:04:16Z xlv $
 *
 * Copyright 2009 by Nigel Kerr.
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
 * the Initial Developer are Copyright (C) 1999-2009 by Bruno Lowagie.
 * All Rights Reserved.
 * Co-Developer of the code is Paulo Soares. Portions created by the Co-Developer
 * are Copyright (C) 2000-2009 by Paulo Soares. All Rights Reserved.
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

package com.lowagie.text.pdf.codec;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import com.lowagie.text.pdf.RandomAccessFileOrArray;

/**
 * Class to read a JBIG2 file at a basic level: understand all the segments,
 * understand what segments belong to which pages, how many pages there are,
 * what the width and height of each page is, and global segments if there
 * are any.  Or: the minimum required to be able to take a normal sequential
 * or random-access organized file, and be able to embed JBIG2 pages as images
 * in a PDF.
 *
 * TODO: the indeterminate-segment-size value of dataLength, else?
 *
 * @since 2.1.5
 */

class JBIG2SegmentReader {

















	private static final int PAGE_INFORMATION = 48; //see 7.4.8.
	private static final int END_OF_PAGE = 49; //see 7.4.9.

	private static final int END_OF_FILE = 51; //see 7.4.11.




	private final SortedMap segments = new TreeMap();
	private final SortedMap pages = new TreeMap();
	private final SortedSet globals = new TreeSet();
	private final RandomAccessFileOrArray ra;
	private boolean sequential;
	private boolean number_of_pages_known;
	private int number_of_pages = -1;
	private boolean read = false;

	/**
	 * Inner class that holds information about a JBIG2 segment.
	 * @since	2.1.5
	 */
	private static class JBIG2Segment implements Comparable {

		private final int segmentNumber;
		private long dataLength = -1;
		private int page = -1;
		private int[] referredToSegmentNumbers = null;
		private boolean[] segmentRetentionFlags = null;
		private int type = -1;
		private boolean deferredNonRetain = false;
		private int countOfReferredToSegments = -1;
		private byte[] data = null;
		private byte[] headerData = null;
		private boolean page_association_size = false;
		private int page_association_offset = -1;

		private JBIG2Segment(final int segment_number) {
			this.segmentNumber = segment_number;
		}

		// for the globals treeset
		@Override
		public int compareTo(final Object o) {
			return this.compareTo((JBIG2Segment)o);
		}
		private int compareTo(final JBIG2Segment s) {
			return this.segmentNumber - s.segmentNumber;
		}


	}
	/**
	 * Inner class that holds information about a JBIG2 page.
	 * @since	2.1.5
	 */
	public static class JBIG2Page {

		private final JBIG2SegmentReader sr;
		private final SortedMap segs = new TreeMap();
		int pageBitmapWidth = -1;
		int pageBitmapHeight = -1;
		private JBIG2Page(final int page, final JBIG2SegmentReader sr) {
			this.sr = sr;
		}
		/**
		 * return as a single byte array the header-data for each segment in segment number
		 * order, EMBEDDED organization, but i am putting the needed segments in SEQUENTIAL organization.
		 * if for_embedding, skip the segment types that are known to be not for acrobat.
		 * @param for_embedding
		 * @return	a byte array
		 * @throws IOException
		 */
		public byte[] getData(final boolean for_embedding) throws IOException {
			final ByteArrayOutputStream os = new ByteArrayOutputStream();
			for (final Iterator i = this.segs.keySet().iterator(); i.hasNext();  ) {
				final Integer sn = (Integer) i.next();
				final JBIG2Segment s = (JBIG2Segment) this.segs.get(sn);

				// pdf reference 1.4, section 3.3.6 JBIG2Decode Filter
				// D.3 Embedded organisation
				if ( for_embedding &&
						( s.type == END_OF_FILE || s.type == END_OF_PAGE ) ) {
					continue;
				}

				if ( for_embedding ) {
					// change the page association to page 1
					final byte[] headerData_emb = copyByteArray(s.headerData);
					if ( s.page_association_size ) {
						headerData_emb[s.page_association_offset] = 0x0;
						headerData_emb[s.page_association_offset+1] = 0x0;
						headerData_emb[s.page_association_offset+2] = 0x0;
						headerData_emb[s.page_association_offset+3] = 0x1;
					} else {
						headerData_emb[s.page_association_offset] = 0x1;
					}
					os.write(headerData_emb);
				} else {
					os.write(s.headerData);
				}
				os.write(s.data);
			}
			os.close();
			return os.toByteArray();
		}
		private void addSegment(final JBIG2Segment s) {
			this.segs.put(new Integer(s.segmentNumber), s);
		}

	}

	public JBIG2SegmentReader(final RandomAccessFileOrArray ra ) throws IOException {
		this.ra = ra;
	}

	private static byte[] copyByteArray(final byte[] b) {
		final byte[] bc = new byte[b.length];
		System.arraycopy(b, 0, bc, 0, b.length);
		return bc;
	}

	public void read() throws IOException {
		if ( this.read ) {
			throw new IllegalStateException("already attempted a read() on this Jbig2 File");
		}
		this.read = true;

		readFileHeader();
		// Annex D
		if ( this.sequential ) {
			// D.1
			do {
				final JBIG2Segment tmp = readHeader();
				readSegment(tmp);
				this.segments.put(new Integer(tmp.segmentNumber), tmp);
			} while ( this.ra.getFilePointer() < this.ra.length() );
		} else {
			// D.2
			JBIG2Segment tmp;
			do {
				tmp = readHeader();
				this.segments.put(new Integer(tmp.segmentNumber), tmp);
			} while ( tmp.type != END_OF_FILE );
			final Iterator segs = this.segments.keySet().iterator();
			while ( segs.hasNext() ) {
				readSegment((JBIG2Segment)this.segments.get(segs.next()));
			}
		}
	}

	private void readSegment(final JBIG2Segment s) throws IOException {
		final int ptr = this.ra.getFilePointer();

		if ( s.dataLength == 0xffffffffl ) {
			// TODO figure this bit out, 7.2.7
			return;
		}

		final byte[] data = new byte[(int)s.dataLength];
		this.ra.read(data);
		s.data = data;

		if ( s.type == PAGE_INFORMATION ) {
			final int last = this.ra.getFilePointer();
			this.ra.seek(ptr);
			final int page_bitmap_width = this.ra.readInt();
			final int page_bitmap_height = this.ra.readInt();
			this.ra.seek(last);
			final JBIG2Page p = (JBIG2Page)this.pages.get(new Integer(s.page));
			if ( p == null ) {
				throw new IllegalStateException("referring to widht/height of page we havent seen yet? " + s.page);
			}

			p.pageBitmapWidth = page_bitmap_width;
			p.pageBitmapHeight = page_bitmap_height;
		}
	}

	private JBIG2Segment readHeader() throws IOException {
		final int ptr = this.ra.getFilePointer();
		// 7.2.1
		final int segment_number = this.ra.readInt();
		final JBIG2Segment s = new JBIG2Segment(segment_number);

		// 7.2.3
		final int segment_header_flags = this.ra.read();
		final boolean deferred_non_retain = ( segment_header_flags & 0x80 ) == 0x80;
		s.deferredNonRetain = deferred_non_retain;
		final boolean page_association_size = ( segment_header_flags & 0x40 ) == 0x40;
		final int segment_type = segment_header_flags & 0x3f;
		s.type = segment_type;

		//7.2.4
		int referred_to_byte0 = this.ra.read();
		int count_of_referred_to_segments = (referred_to_byte0 & 0xE0) >> 5;
		int[] referred_to_segment_numbers = null;
		boolean[] segment_retention_flags = null;

		if ( count_of_referred_to_segments == 7 ) {
			// at least five bytes
			this.ra.seek(this.ra.getFilePointer() - 1);
			count_of_referred_to_segments = this.ra.readInt() & 0x1fffffff;
			segment_retention_flags = new boolean[count_of_referred_to_segments+1];
			int i = 0;
			int referred_to_current_byte = 0;
			do {
				final int j = i % 8;
				if ( j == 0) {
					referred_to_current_byte = this.ra.read();
				}
				segment_retention_flags[i] = (0x1 << j & referred_to_current_byte) >> j == 0x1;
				i++;
			} while ( i <= count_of_referred_to_segments );

		} else if ( count_of_referred_to_segments <= 4 ) {
			// only one byte
			segment_retention_flags = new boolean[count_of_referred_to_segments+1];
			referred_to_byte0 &= 0x1f;
			for ( int i = 0; i <= count_of_referred_to_segments; i++ ) {
				segment_retention_flags[i] = (0x1 << i & referred_to_byte0) >> i == 0x1;
			}

		} else if ( count_of_referred_to_segments == 5 || count_of_referred_to_segments == 6 ) {
			throw new IllegalStateException("count of referred-to segments had bad value in header for segment " + segment_number + " starting at " + ptr);
		}
		s.segmentRetentionFlags = segment_retention_flags;
		s.countOfReferredToSegments = count_of_referred_to_segments;

		// 7.2.5
		referred_to_segment_numbers = new int[count_of_referred_to_segments+1];
		for ( int i = 1; i <= count_of_referred_to_segments; i++ ) {
			if ( segment_number <= 256 ) {
				referred_to_segment_numbers[i] = this.ra.read();
			} else if ( segment_number <= 65536 ) {
				referred_to_segment_numbers[i] = this.ra.readUnsignedShort();
			} else {
				referred_to_segment_numbers[i] = (int)this.ra.readUnsignedInt(); // TODO wtf ack
			}
		}
		s.referredToSegmentNumbers = referred_to_segment_numbers;

		// 7.2.6
		int segment_page_association;
		final int page_association_offset = this.ra.getFilePointer() - ptr;
		if ( page_association_size ) {
			segment_page_association = this.ra.readInt();
		} else {
			segment_page_association = this.ra.read();
		}
		if ( segment_page_association < 0 ) {
			throw new IllegalStateException("page " + segment_page_association + " invalid for segment " + segment_number + " starting at " + ptr);
		}
		s.page = segment_page_association;
		// so we can change the page association at embedding time.
		s.page_association_size = page_association_size;
		s.page_association_offset = page_association_offset;

		if ( segment_page_association > 0 && ! this.pages.containsKey(new Integer(segment_page_association)) ) {
			this.pages.put(new Integer(segment_page_association), new JBIG2Page(segment_page_association, this));
		}
		if ( segment_page_association > 0 ) {
			((JBIG2Page)this.pages.get(new Integer(segment_page_association))).addSegment(s);
		} else {
			this.globals.add(s);
		}

		// 7.2.7
		final long segment_data_length = this.ra.readUnsignedInt();
		// TODO the 0xffffffff value that might be here, and how to understand those afflicted segments
		s.dataLength = segment_data_length;

		final int end_ptr = this.ra.getFilePointer();
		this.ra.seek(ptr);
		final byte[] header_data = new byte[end_ptr - ptr];
		this.ra.read(header_data);
		s.headerData  = header_data;

		return s;
	}

	private void readFileHeader() throws IOException {
		this.ra.seek(0);
		final byte[] idstring = new byte[8];
		this.ra.read(idstring);

		final byte[] refidstring = {(byte)0x97, 0x4A, 0x42, 0x32, 0x0D, 0x0A, 0x1A, 0x0A};

		for ( int i = 0; i < idstring.length; i++ ) {
			if ( idstring[i] != refidstring[i] ) {
				throw new IllegalStateException("file header idstring not good at byte " + i);
			}
		}

		final int fileheaderflags = this.ra.read();

		this.sequential = ( fileheaderflags & 0x1 ) == 0x1;
		this.number_of_pages_known = ( fileheaderflags & 0x2) == 0x0;

		if ( (fileheaderflags & 0xfc) != 0x0 ) {
			throw new IllegalStateException("file header flags bits 2-7 not 0");
		}

		if ( this.number_of_pages_known ) {
			this.number_of_pages = this.ra.readInt();
		}
	}

	private int numberOfPages() {
		return this.pages.size();
	}





	JBIG2Page getPage(final int page) {
		return (JBIG2Page)this.pages.get(new Integer(page));
	}

	byte[] getGlobal(final boolean for_embedding) {
		final ByteArrayOutputStream os = new ByteArrayOutputStream();
		try {
			for (final Iterator gitr = this.globals.iterator(); gitr.hasNext();) {
				final JBIG2Segment s = (JBIG2Segment)gitr.next();
				if ( for_embedding &&
						( s.type == END_OF_FILE || s.type == END_OF_PAGE ) ) {
					continue;
				}
				os.write(s.headerData);
				os.write(s.data);
			}
			os.close();
		} catch (final IOException e) {
			e.printStackTrace();
		}
		if ( os.size() <= 0 ) {
			return null;
		}
		return os.toByteArray();
	}

	@Override
	public String toString() {
		if ( this.read ) {
			return "Jbig2SegmentReader: number of pages: " + this.numberOfPages();
		} else {
			return "Jbig2SegmentReader in indeterminate state.";
		}
	}
}