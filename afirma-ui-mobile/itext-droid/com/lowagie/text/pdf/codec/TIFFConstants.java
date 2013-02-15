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
class TIFFConstants {

/*
 * TIFF Tag Definitions (from tifflib).
 */

    static final int TIFFTAG_IMAGEWIDTH = 256; /* image width in pixels */
    static final int TIFFTAG_IMAGELENGTH = 257; /* image height in pixels */
    static final int TIFFTAG_BITSPERSAMPLE = 258; /* bits per channel (sample) */
    static final int TIFFTAG_COMPRESSION = 259; /* data compression technique */
    static final int     COMPRESSION_NONE = 1; /* dump mode */
    static final int     COMPRESSION_CCITTRLE = 2; /* CCITT modified Huffman RLE */
    static final int     COMPRESSION_CCITTFAX3 = 3;	/* CCITT Group 3 fax encoding */
    static final int     COMPRESSION_CCITTFAX4 = 4;	/* CCITT Group 4 fax encoding */
    static final int     COMPRESSION_LZW = 5;       /* Lempel-Ziv  & Welch */
    static final int     COMPRESSION_OJPEG = 6; /* !6.0 JPEG */
    static final int     COMPRESSION_JPEG = 7; /* %JPEG DCT compression */

    static final int     COMPRESSION_CCITTRLEW = 32771; /* #1 w/ word alignment */
    static final int     COMPRESSION_PACKBITS = 32773; /* Macintosh RLE */

    static final int     COMPRESSION_DEFLATE = 32946; /* Deflate compression */
    static final int     COMPRESSION_ADOBE_DEFLATE = 8;       /* Deflate compression, as recognized by Adobe */
    /* compression code 32947 is reserved for Oceana Matrix <dev@oceana.com> */

    static final int TIFFTAG_PHOTOMETRIC = 262; /* photometric interpretation */
    static final int     PHOTOMETRIC_MINISWHITE = 0; /* min value is white */
    static final int     PHOTOMETRIC_MINISBLACK = 1; /* min value is black */
    static final int     PHOTOMETRIC_RGB = 2; /* RGB color model */
    static final int     PHOTOMETRIC_PALETTE = 3; /* color map indexed */

    static final int     PHOTOMETRIC_SEPARATED = 5; /* !color separations */

    static final int TIFFTAG_FILLORDER = 266; /* data order within a byte */

    static final int     FILLORDER_LSB2MSB = 2; /* least significant -> most */

    static final int TIFFTAG_STRIPOFFSETS = 273; /* offsets to data strips */
    static final int TIFFTAG_ORIENTATION = 274; /* +image orientation */

    static final int     ORIENTATION_BOTRIGHT = 3; /* row 0 bottom, col 0 rhs */
    static final int     ORIENTATION_BOTLEFT = 4; /* row 0 bottom, col 0 lhs */
    static final int     ORIENTATION_LEFTTOP = 5; /* row 0 lhs, col 0 top */
    static final int     ORIENTATION_RIGHTTOP = 6; /* row 0 rhs, col 0 top */
    static final int     ORIENTATION_RIGHTBOT = 7; /* row 0 rhs, col 0 bottom */
    static final int     ORIENTATION_LEFTBOT = 8; /* row 0 lhs, col 0 bottom */
    static final int TIFFTAG_SAMPLESPERPIXEL = 277; /* samples per pixel */
    static final int TIFFTAG_ROWSPERSTRIP = 278; /* rows per strip of data */
    static final int TIFFTAG_STRIPBYTECOUNTS = 279; /* bytes counts for strips */

    static final int TIFFTAG_XRESOLUTION = 282; /* pixels/resolution in x */
    static final int TIFFTAG_YRESOLUTION = 283; /* pixels/resolution in y */
    static final int TIFFTAG_PLANARCONFIG = 284; /* storage organization */

    static final int     PLANARCONFIG_SEPARATE = 2; /* separate planes of data */

    static final int TIFFTAG_GROUP3OPTIONS = 292; /* 32 flag bits */
    static final int     GROUP3OPT_2DENCODING = 0x1;	/* 2-dimensional coding */

    static final int     GROUP3OPT_FILLBITS = 0x4;	/* fill to byte boundary */
    static final int TIFFTAG_GROUP4OPTIONS = 293; /* 32 flag bits */

    static final int TIFFTAG_RESOLUTIONUNIT = 296; /* units of resolutions */
    static final int     RESUNIT_NONE = 1; /* no meaningful units */
    static final int     RESUNIT_INCH = 2; /* english */
    static final int     RESUNIT_CENTIMETER = 3;	/* metric */

    static final int TIFFTAG_PREDICTOR = 317;	/* prediction scheme w/ LZW */

    static final int TIFFTAG_COLORMAP = 320;	/* RGB map for pallette image */

    static final int TIFFTAG_TILEWIDTH = 322;	/* !rows/data tile */

    static final int TIFFTAG_EXTRASAMPLES = 338;	/* !info about extra samples */

/*
 * Tags 512-521 are obsoleted by Technical Note #2
 * which specifies a revised JPEG-in-TIFF scheme.
 */

    static final int TIFFTAG_JPEGIFOFFSET = 513;	/* !pointer to SOI marker */
    static final int TIFFTAG_JPEGIFBYTECOUNT = 514;	/* !JFIF stream length */


    /* tag 34750 is a private tag registered to Adobe? */
    static final int TIFFTAG_ICCPROFILE = 34675;	/* ICC profile data */
    /* tag 34377 is private tag registered to Adobe for PhotoShop */


}
