/*
 * Copyright 2003-2005 by Paulo Soares.
 *
 * This list of constants was originally released with libtiff
 * under the following license:
 *
 * Copyright (c) 1988-1997 Sam Leffler
 * Copyright (c) 1991-1997 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Sam Leffler and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Sam Leffler and Silicon Graphics.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 * OF THIS SOFTWARE.
 */
package com.lowagie.text.pdf.codec;

/**
 * A list of constants used in class TIFFImage.
 */
public class TIFFConstants {

/*
 * TIFF Tag Definitions (from tifflib).
 */

    public static final int TIFFTAG_IMAGEWIDTH = 256; /* image width in pixels */
    public static final int TIFFTAG_IMAGELENGTH = 257; /* image height in pixels */
    public static final int TIFFTAG_BITSPERSAMPLE = 258; /* bits per channel (sample) */
    public static final int TIFFTAG_COMPRESSION = 259; /* data compression technique */
    public static final int     COMPRESSION_NONE = 1; /* dump mode */
    public static final int     COMPRESSION_CCITTRLE = 2; /* CCITT modified Huffman RLE */
    public static final int     COMPRESSION_CCITTFAX3 = 3;	/* CCITT Group 3 fax encoding */
    public static final int     COMPRESSION_CCITTFAX4 = 4;	/* CCITT Group 4 fax encoding */
    public static final int     COMPRESSION_LZW = 5;       /* Lempel-Ziv  & Welch */
    public static final int     COMPRESSION_OJPEG = 6; /* !6.0 JPEG */
    public static final int     COMPRESSION_JPEG = 7; /* %JPEG DCT compression */

    public static final int     COMPRESSION_CCITTRLEW = 32771; /* #1 w/ word alignment */
    public static final int     COMPRESSION_PACKBITS = 32773; /* Macintosh RLE */

    public static final int     COMPRESSION_DEFLATE = 32946; /* Deflate compression */
    public static final int     COMPRESSION_ADOBE_DEFLATE = 8;       /* Deflate compression, as recognized by Adobe */
    /* compression code 32947 is reserved for Oceana Matrix <dev@oceana.com> */

    public static final int TIFFTAG_PHOTOMETRIC = 262; /* photometric interpretation */
    public static final int     PHOTOMETRIC_MINISWHITE = 0; /* min value is white */
    public static final int     PHOTOMETRIC_MINISBLACK = 1; /* min value is black */
    public static final int     PHOTOMETRIC_RGB = 2; /* RGB color model */
    public static final int     PHOTOMETRIC_PALETTE = 3; /* color map indexed */

    public static final int     PHOTOMETRIC_SEPARATED = 5; /* !color separations */

    public static final int TIFFTAG_FILLORDER = 266; /* data order within a byte */

    public static final int     FILLORDER_LSB2MSB = 2; /* least significant -> most */

    public static final int TIFFTAG_STRIPOFFSETS = 273; /* offsets to data strips */
    public static final int TIFFTAG_ORIENTATION = 274; /* +image orientation */

    public static final int     ORIENTATION_BOTRIGHT = 3; /* row 0 bottom, col 0 rhs */
    public static final int     ORIENTATION_BOTLEFT = 4; /* row 0 bottom, col 0 lhs */
    public static final int     ORIENTATION_LEFTTOP = 5; /* row 0 lhs, col 0 top */
    public static final int     ORIENTATION_RIGHTTOP = 6; /* row 0 rhs, col 0 top */
    public static final int     ORIENTATION_RIGHTBOT = 7; /* row 0 rhs, col 0 bottom */
    public static final int     ORIENTATION_LEFTBOT = 8; /* row 0 lhs, col 0 bottom */
    public static final int TIFFTAG_SAMPLESPERPIXEL = 277; /* samples per pixel */
    public static final int TIFFTAG_ROWSPERSTRIP = 278; /* rows per strip of data */
    public static final int TIFFTAG_STRIPBYTECOUNTS = 279; /* bytes counts for strips */

    public static final int TIFFTAG_XRESOLUTION = 282; /* pixels/resolution in x */
    public static final int TIFFTAG_YRESOLUTION = 283; /* pixels/resolution in y */
    public static final int TIFFTAG_PLANARCONFIG = 284; /* storage organization */

    public static final int     PLANARCONFIG_SEPARATE = 2; /* separate planes of data */

    public static final int TIFFTAG_GROUP3OPTIONS = 292; /* 32 flag bits */
    public static final int     GROUP3OPT_2DENCODING = 0x1;	/* 2-dimensional coding */

    public static final int     GROUP3OPT_FILLBITS = 0x4;	/* fill to byte boundary */
    public static final int TIFFTAG_GROUP4OPTIONS = 293; /* 32 flag bits */

    public static final int TIFFTAG_RESOLUTIONUNIT = 296; /* units of resolutions */
    public static final int     RESUNIT_NONE = 1; /* no meaningful units */
    public static final int     RESUNIT_INCH = 2; /* english */
    public static final int     RESUNIT_CENTIMETER = 3;	/* metric */

    public static final int TIFFTAG_PREDICTOR = 317;	/* prediction scheme w/ LZW */

    public static final int TIFFTAG_COLORMAP = 320;	/* RGB map for pallette image */

    public static final int TIFFTAG_TILEWIDTH = 322;	/* !rows/data tile */

    public static final int TIFFTAG_EXTRASAMPLES = 338;	/* !info about extra samples */

/*
 * Tags 512-521 are obsoleted by Technical Note #2
 * which specifies a revised JPEG-in-TIFF scheme.
 */

    public static final int TIFFTAG_JPEGIFOFFSET = 513;	/* !pointer to SOI marker */
    public static final int TIFFTAG_JPEGIFBYTECOUNT = 514;	/* !JFIF stream length */


    /* tag 34750 is a private tag registered to Adobe? */
    public static final int TIFFTAG_ICCPROFILE = 34675;	/* ICC profile data */
    /* tag 34377 is private tag registered to Adobe for PhotoShop */


}
