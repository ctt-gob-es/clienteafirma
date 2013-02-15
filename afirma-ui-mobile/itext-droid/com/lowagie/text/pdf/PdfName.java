/*
 * $Id: PdfName.java 3934 2009-05-27 11:23:23Z blowagie $
 *
 * Copyright 1999-2006 Bruno Lowagie
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

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;

/**
 * <CODE>PdfName</CODE> is an object that can be used as a name in a PDF-file.
 * <P>
 * A name, like a string, is a sequence of characters.
 * It must begin with a slash followed by a sequence of ASCII characters in
 * the range 32 through 136 except %, (, ), [, ], &lt;, &gt;, {, }, / and #.
 * Any character except 0x00 may be included in a name by writing its
 * two character hex code, preceded by #. The maximum number of characters
 * in a name is 127.<BR>
 * This object is described in the 'Portable Document Format Reference Manual
 * version 1.7' section 3.2.4 (page 56-58).
 * <P>
 *
 * @see		PdfObject
 * @see		PdfDictionary
 * @see		BadPdfFormatException
 */

public class PdfName extends PdfObject implements Comparable{


    /** A name */
    public static final PdfName A = new PdfName("A");
    /** A name */
    static final PdfName AA = new PdfName("AA");
    /**
     * A name
     * @since 2.1.5 renamed from ABSOLUTECALORIMETRIC
     */
    public static final PdfName ABSOLUTECOLORIMETRIC = new PdfName("AbsoluteColorimetric");
    /** A name */
    static final PdfName AC = new PdfName("AC");
    /** A name */
    static final PdfName ACROFORM = new PdfName("AcroForm");




    /** A name */
    public static final PdfName ADBE_PKCS7_DETACHED = new PdfName("adbe.pkcs7.detached");
    /** A name */
    static final PdfName ADBE_PKCS7_S4 =new PdfName("adbe.pkcs7.s4");
    /** A name */
    static final PdfName ADBE_PKCS7_S5 =new PdfName("adbe.pkcs7.s5");
    /** A name */
    static final PdfName ADBE_PKCS7_SHA1 = new PdfName("adbe.pkcs7.sha1");
    /** A name */
    static final PdfName ADBE_X509_RSA_SHA1 = new PdfName("adbe.x509.rsa_sha1");
    /** A name */
    public static final PdfName ADOBE_PPKLITE = new PdfName("Adobe.PPKLite");
    /** A name */
    static final PdfName ADOBE_PPKMS = new PdfName("Adobe.PPKMS");
    /** A name */
    static final PdfName AESV2 = new PdfName("AESV2");
    /** A name */
    static final PdfName AIS = new PdfName("AIS");


    /** A name */
    static final PdfName ALTERNATE = new PdfName("Alternate");

    /** A name */
    static final PdfName ANNOT = new PdfName("Annot");
    /** A name */
    static final PdfName ANNOTS = new PdfName("Annots");
    /** A name */
    static final PdfName ANTIALIAS = new PdfName("AntiAlias");
    /** A name */
    static final PdfName AP = new PdfName("AP");
    /** A name */
    public static final PdfName APPDEFAULT = new PdfName("AppDefault");

    /** A name */
    public static final PdfName ARTBOX = new PdfName("ArtBox");
    /** A name */
    static final PdfName ASCENT = new PdfName("Ascent");
    /** A name */
    static final PdfName AS = new PdfName("AS");




    /** A name */
    static final PdfName AUTHEVENT = new PdfName("AuthEvent");
    /** A name */
    public static final PdfName AUTHOR = new PdfName("Author");
    /** A name */
    static final PdfName B = new PdfName("B");

    /** A name */
    static final PdfName BASEENCODING = new PdfName("BaseEncoding");
    /** A name */
    static final PdfName BASEFONT = new PdfName("BaseFont");
    /**
     * A name
     * @since	2.1.6
     */
    public static final PdfName BASEVERSION = new PdfName("BaseVersion");
    /** A name */
    static final PdfName BBOX = new PdfName("BBox");
    /** A name */
    static final PdfName BC = new PdfName("BC");
    /** A name */
    static final PdfName BG = new PdfName("BG");

    /** A name */
    static final PdfName BIGFIVE = new PdfName("BigFive");


    /** A name */
    public static final PdfName BITSPERCOMPONENT = new PdfName("BitsPerComponent");


    /** A name */
    static final PdfName BLACKIS1 = new PdfName("BlackIs1");


    /** A name */
    public static final PdfName BLEEDBOX = new PdfName("BleedBox");
    /** A name */
    static final PdfName BLINDS = new PdfName("Blinds");
    /** A name */
    public static final PdfName BM = new PdfName("BM");
    /** A name */
    static final PdfName BORDER = new PdfName("Border");

    /** A name */
    static final PdfName BOX = new PdfName("Box");
    /** A name */
    static final PdfName BS = new PdfName("BS");
    /** A name */
    static final PdfName BTN = new PdfName("Btn");
    /** A name */
    static final PdfName BYTERANGE = new PdfName("ByteRange");
    /** A name */
    static final PdfName C = new PdfName("C");
    /** A name */
    static final PdfName C0 = new PdfName("C0");
    /** A name */
    static final PdfName C1 = new PdfName("C1");
    /** A name */
    public static final PdfName CA = new PdfName("CA");
    /** A name */
    public static final PdfName ca = new PdfName("ca");
    /** A name */
    public static final PdfName CALGRAY = new PdfName("CalGray");
    /** A name */
    public static final PdfName CALRGB = new PdfName("CalRGB");
    /** A name */
    static final PdfName CAPHEIGHT = new PdfName("CapHeight");

    /** A name */
    static final PdfName CATALOG = new PdfName("Catalog");
    /** A name */
    static final PdfName CATEGORY = new PdfName("Category");
    /** A name */
    static final PdfName CCITTFAXDECODE = new PdfName("CCITTFaxDecode");

    /** A name */
    public static final PdfName CENTERWINDOW = new PdfName("CenterWindow");
    /** A name */
    static final PdfName CERT = new PdfName("Cert");
    /** A name */
    static final PdfName CF = new PdfName("CF");
    /** A name */
    static final PdfName CFM = new PdfName("CFM");
    /** A name */
    static final PdfName CH = new PdfName("Ch");


    /** A name */
    static final PdfName CIDFONTTYPE0 = new PdfName("CIDFontType0");
    /** A name */
    static final PdfName CIDFONTTYPE2 = new PdfName("CIDFontType2");
    /**
     * A name
     * @since 2.0.7
     */
    static final PdfName CIDSET = new PdfName("CIDSet");
    /** A name */
    static final PdfName CIDSYSTEMINFO = new PdfName("CIDSystemInfo");
    /** A name */
    static final PdfName CIDTOGIDMAP = new PdfName("CIDToGIDMap");


    /** A name */
    static final PdfName CO = new PdfName("CO");

    /** A name */
    public static final PdfName COLORS = new PdfName("Colors");
    /** A name */
    public static final PdfName COLORSPACE = new PdfName("ColorSpace");
    /** A name */
    static final PdfName COLLECTION = new PdfName("Collection");


    /** A name */
    public static final PdfName COLLECTIONSCHEMA = new PdfName("CollectionSchema");


    /** A name */
    public static final PdfName COLUMNS = new PdfName("Columns");



    /** A name */
    static final PdfName CONTACTINFO = new PdfName("ContactInfo");

    /** A name */
    public static final PdfName CONTENTS = new PdfName("Contents");
    /** A name */
    static final PdfName COORDS = new PdfName("Coords");
    /** A name */
    static final PdfName COUNT = new PdfName("Count");
    /** A name of a base 14 type 1 font */
    static final PdfName COURIER = new PdfName("Courier");
    /** A name of a base 14 type 1 font */
    static final PdfName COURIER_BOLD = new PdfName("Courier-Bold");
    /** A name of a base 14 type 1 font */
    static final PdfName COURIER_OBLIQUE = new PdfName("Courier-Oblique");
    /** A name of a base 14 type 1 font */
    static final PdfName COURIER_BOLDOBLIQUE = new PdfName("Courier-BoldOblique");
    /** A name */
    public static final PdfName CREATIONDATE = new PdfName("CreationDate");
    /** A name */
    public static final PdfName CREATOR = new PdfName("Creator");

    /** A name */
    public static final PdfName CROPBOX = new PdfName("CropBox");
    /** A name */
    static final PdfName CRYPT = new PdfName("Crypt");
    /** A name */
    static final PdfName CS = new PdfName("CS");


    /** A name */
    public static final PdfName D = new PdfName("D");
    /** A name */
    static final PdfName DA = new PdfName("DA");
    /** A name */
    static final PdfName DATA = new PdfName("Data");

    /** A name */
    static final PdfName DCTDECODE = new PdfName("DCTDecode");

    /** A name */
    static final PdfName DECODE = new PdfName("Decode");
    /** A name */
    public static final PdfName DECODEPARMS = new PdfName("DecodeParms");

    /**
     * A name
     * @since	2.1.5 renamed from DEFAULTCRYPTFILER
     */
    static final PdfName DEFAULTCRYPTFILTER = new PdfName("DefaultCryptFilter");


    /** A name */
    static final PdfName DEFAULTRGB = new PdfName("DefaultRGB");
    /** A name */
    static final PdfName DESC = new PdfName("Desc");
    /** A name */
    static final PdfName DESCENDANTFONTS = new PdfName("DescendantFonts");
    /** A name */
    static final PdfName DESCENT = new PdfName("Descent");
    /** A name */
    static final PdfName DEST = new PdfName("Dest");
    /** A name */
    static final PdfName DESTOUTPUTPROFILE = new PdfName("DestOutputProfile");
    /** A name */
    static final PdfName DESTS = new PdfName("Dests");
    /** A name */
    public static final PdfName DEVICEGRAY = new PdfName("DeviceGray");
    /** A name */
    public static final PdfName DEVICERGB = new PdfName("DeviceRGB");
    /** A name */
    static final PdfName DEVICECMYK = new PdfName("DeviceCMYK");
    /** A name */
    static final PdfName DI = new PdfName("Di");
    /** A name */
    static final PdfName DIFFERENCES = new PdfName("Differences");
    /** A name */
    static final PdfName DISSOLVE = new PdfName("Dissolve");
    /** A name */
    public static final PdfName DIRECTION = new PdfName("Direction");
    /** A name */
    public static final PdfName DISPLAYDOCTITLE = new PdfName("DisplayDocTitle");

    /** A name */
    static final PdfName DM = new PdfName("Dm");
    /** A name */
    static final PdfName DOCMDP = new PdfName("DocMDP");
    /** A name */
    static final PdfName DOCOPEN = new PdfName("DocOpen");

    /** A name */
    static final PdfName DOMAIN = new PdfName("Domain");
    /** A name */
    static final PdfName DP = new PdfName("DP");
    /** A name */
    static final PdfName DR = new PdfName("DR");
    /** A name */
    static final PdfName DS = new PdfName("DS");
    /** A name */
    static final PdfName DUR = new PdfName("Dur");
    /** A name */
    public static final PdfName DUPLEX = new PdfName("Duplex");
    /** A name */
    public static final PdfName DUPLEXFLIPSHORTEDGE = new PdfName("DuplexFlipShortEdge");
    /** A name */
    public static final PdfName DUPLEXFLIPLONGEDGE = new PdfName("DuplexFlipLongEdge");
    /** A name */
    static final PdfName DV = new PdfName("DV");
    /** A name */
    static final PdfName DW = new PdfName("DW");
    /** A name */
    public static final PdfName E = new PdfName("E");

    /** A name */
    static final PdfName EF = new PdfName("EF");
    /**
     * A name
     * @since	2.1.3
     */
    static final PdfName EFF = new PdfName("EFF");
    /**
     * A name
     * @since	2.1.3
     */
    static final PdfName EFOPEN = new PdfName("EFOpen");

    /** A name */
    static final PdfName EMBEDDEDFILE = new PdfName("EmbeddedFile");
    /** A name */
    static final PdfName EMBEDDEDFILES = new PdfName("EmbeddedFiles");

    /** A name */
    static final PdfName ENCODEDBYTEALIGN = new PdfName("EncodedByteAlign");
    /** A name */
    static final PdfName ENCODING = new PdfName("Encoding");
    /** A name */
    static final PdfName ENCRYPT = new PdfName("Encrypt");
    /** A name */
    static final PdfName ENCRYPTMETADATA = new PdfName("EncryptMetadata");
    /** A name */
    static final PdfName ENDOFBLOCK = new PdfName("EndOfBlock");
    /** A name */
    static final PdfName ENDOFLINE = new PdfName("EndOfLine");
    /** A name */
    static final PdfName EXTEND = new PdfName("Extend");
    /**
     * A name
     * @since	2.1.6
     */
    public static final PdfName EXTENSIONS = new PdfName("Extensions");
    /**
     * A name
     * @since	2.1.6
     */
    public static final PdfName EXTENSIONLEVEL = new PdfName("ExtensionLevel");
    /** A name */
    public static final PdfName EXTGSTATE = new PdfName("ExtGState");
    /** A name */
    static final PdfName EXPORT = new PdfName("Export");
    /** A name */
    static final PdfName EXPORTSTATE = new PdfName("ExportState");
    /** A name */
    static final PdfName EVENT = new PdfName("Event");
    /** A name */
    static final PdfName F = new PdfName("F");

    /** A name */
    static final PdfName FB = new PdfName("FB");

    /** A name */
    static final PdfName FDF = new PdfName("FDF");
    /** A name */
    static final PdfName FF = new PdfName("Ff");

    /** A name */
    static final PdfName FIELDS = new PdfName("Fields");

    /** A name */
    static final PdfName FILEATTACHMENT = new PdfName("FileAttachment");
    /** A name */
    static final PdfName FILESPEC = new PdfName("Filespec");
    /** A name */
    static final PdfName FILTER = new PdfName("Filter");
    /** A name */
    static final PdfName FIRST = new PdfName("First");
    /** A name */
    static final PdfName FIRSTCHAR = new PdfName("FirstChar");
    /** A name */
    static final PdfName FIRSTPAGE = new PdfName("FirstPage");







    /** A name */
    public static final PdfName FITWINDOW = new PdfName("FitWindow");
    /** A name */
    static final PdfName FLAGS = new PdfName("Flags");


    /** A name */
    static final PdfName FLATEDECODE = new PdfName("FlateDecode");

    /** A name */
    public static final PdfName FONT = new PdfName("Font");
    /** A name */
    static final PdfName FONTBBOX = new PdfName("FontBBox");
    /** A name */
    static final PdfName FONTDESCRIPTOR = new PdfName("FontDescriptor");
    /** A name */
    static final PdfName FONTFILE = new PdfName("FontFile");
    /** A name */
    static final PdfName FONTFILE2 = new PdfName("FontFile2");
    /** A name */
    static final PdfName FONTFILE3 = new PdfName("FontFile3");

    /** A name */
    static final PdfName FONTNAME = new PdfName("FontName");

    /** A name */
    static final PdfName FORM = new PdfName("Form");
    /** A name */
    static final PdfName FORMTYPE = new PdfName("FormType");

    /** A name */
    static final PdfName FREETEXT = new PdfName("FreeText");
    /** A name */
    static final PdfName FRM = new PdfName("FRM");
    /** A name */
    static final PdfName FS = new PdfName("FS");
    /** A name */
    static final PdfName FT = new PdfName("FT");
    /** A name */
    public static final PdfName FULLSCREEN = new PdfName("FullScreen");
    /** A name */
    static final PdfName FUNCTION = new PdfName("Function");

    /** A name */
    static final PdfName FUNCTIONTYPE = new PdfName("FunctionType");
    /** A name of an attribute. */
    public static final PdfName GAMMA = new PdfName("Gamma");
    /** A name of an attribute. */
    static final PdfName GBK = new PdfName("GBK");
    /** A name of an attribute. */
    static final PdfName GLITTER = new PdfName("Glitter");
    /** A name of an attribute. */
    static final PdfName GOTO = new PdfName("GoTo");
    /** A name of an attribute. */
    static final PdfName GOTOE = new PdfName("GoToE");
    /** A name of an attribute. */
    static final PdfName GOTOR = new PdfName("GoToR");
    /** A name of an attribute. */
    static final PdfName GROUP = new PdfName("Group");
    /** A name of an attribute. */
    static final PdfName GTS_PDFA1 = new PdfName("GTS_PDFA1");
    /** A name of an attribute. */
    public static final PdfName GTS_PDFX = new PdfName("GTS_PDFX");
    /** A name of an attribute. */
    public static final PdfName GTS_PDFXVERSION = new PdfName("GTS_PDFXVersion");
    /** A name of an attribute. */
    static final PdfName H = new PdfName("H");

    /** A name of an attribute. */
    public static final PdfName HEIGHT = new PdfName("Height");
    /** A name */
    static final PdfName HELV = new PdfName("Helv");
    /** A name of a base 14 type 1 font */
    static final PdfName HELVETICA = new PdfName("Helvetica");
    /** A name of a base 14 type 1 font */
    static final PdfName HELVETICA_BOLD = new PdfName("Helvetica-Bold");
    /** A name of a base 14 type 1 font */
    static final PdfName HELVETICA_OBLIQUE = new PdfName("Helvetica-Oblique");
    /** A name of a base 14 type 1 font */
    static final PdfName HELVETICA_BOLDOBLIQUE = new PdfName("Helvetica-BoldOblique");

    /** A name */
    static final PdfName HIDE = new PdfName("Hide");
    /** A name */
    public static final PdfName HIDEMENUBAR = new PdfName("HideMenubar");
    /** A name */
    public static final PdfName HIDETOOLBAR = new PdfName("HideToolbar");
    /** A name */
    public static final PdfName HIDEWINDOWUI = new PdfName("HideWindowUI");


    /** A name */
    static final PdfName I = new PdfName("I");
    /** A name */
    static final PdfName ICCBASED = new PdfName("ICCBased");
    /** A name */
    static final PdfName ID = new PdfName("ID");
    /** A name */
    static final PdfName IDENTITY = new PdfName("Identity");
    /** A name */
    static final PdfName IF = new PdfName("IF");
    /** A name */
    static final PdfName IMAGE = new PdfName("Image");



    /** A name */
    static final PdfName IMAGEMASK = new PdfName("ImageMask");
    /** A name */
    static final PdfName INDEX = new PdfName("Index");
    /** A name */
    public static final PdfName INDEXED = new PdfName("Indexed");
    /** A name */
    public static final PdfName INFO = new PdfName("Info");




    /** A name */
    public static final PdfName INTENT = new PdfName("Intent");
    /** A name */
    static final PdfName INTERPOLATE = new PdfName("Interpolate");
    /** A name */
    static final PdfName ISMAP = new PdfName("IsMap");

    /** A name */
    static final PdfName ITALICANGLE = new PdfName("ItalicAngle");
    /**
     * A name
     * @since	2.1.6
     */
    static final PdfName ITXT = new PdfName("ITXT");
    /** A name */
    static final PdfName IX = new PdfName("IX");
    /** A name */
    static final PdfName JAVASCRIPT = new PdfName("JavaScript");
    /**
     * A name
     * @since	2.1.5
     */
    static final PdfName JBIG2DECODE = new PdfName("JBIG2Decode");
    /**
     * A name
     * @since	2.1.5
     */
    static final PdfName JBIG2GLOBALS = new PdfName("JBIG2Globals");
    /** A name */
    static final PdfName JPXDECODE = new PdfName("JPXDecode");
    /** A name */
    static final PdfName JS = new PdfName("JS");
    /** A name */
    static final PdfName K = new PdfName("K");
    /** A name */
    public static final PdfName KEYWORDS = new PdfName("Keywords");
    /** A name */
    static final PdfName KIDS = new PdfName("Kids");

    /** A name */
    public static final PdfName L2R = new PdfName("L2R");


    /** A name */
    static final PdfName LAST = new PdfName("Last");
    /** A name */
    static final PdfName LASTCHAR = new PdfName("LastChar");
    /** A name */
    static final PdfName LASTPAGE = new PdfName("LastPage");
    /** A name */
    static final PdfName LAUNCH = new PdfName("Launch");


    /** A name */
    static final PdfName LENGTH = new PdfName("Length");


    /** A name */
    static final PdfName LIMITS = new PdfName("Limits");


    /** A name */
    static final PdfName LINK = new PdfName("Link");
    /** A name */
    static final PdfName LISTMODE = new PdfName("ListMode");
    /** A name */
    static final PdfName LOCATION = new PdfName("Location");
    /** A name */
    static final PdfName LOCK = new PdfName("Lock");
    /**
     * A name
     * @since	2.1.2
     */
    static final PdfName LOCKED = new PdfName("Locked");

    /** A name */
    static final PdfName M = new PdfName("M");

    /** A name */
    public static final PdfName MATRIX = new PdfName("Matrix");

    /** A name of an encoding */
    static final PdfName MAC_ROMAN_ENCODING = new PdfName("MacRomanEncoding");
    /** A name */
    static final PdfName MARKED = new PdfName("Marked");
    /** A name */
    static final PdfName MARKINFO = new PdfName("MarkInfo");
    /** A name */
    public static final PdfName MASK = new PdfName("Mask");


    /** A name */
    static final PdfName MAXLEN = new PdfName("MaxLen");
    /** A name */
    public static final PdfName MEDIABOX = new PdfName("MediaBox");


    /** A name */
    static final PdfName METADATA = new PdfName("Metadata");


    /** A name */
    static final PdfName MK = new PdfName("MK");
    /** A name */
    static final PdfName MMTYPE1 = new PdfName("MMType1");
    /** A name */
    public static final PdfName MODDATE = new PdfName("ModDate");
    /** A name */
    public static final PdfName N = new PdfName("N");





    /** A name */
    static final PdfName NAME = new PdfName("Name");
    /** A name */
    static final PdfName NAMED = new PdfName("Named");
    /** A name */
    static final PdfName NAMES = new PdfName("Names");



    /** A name */
    static final PdfName NEEDAPPEARANCES = new PdfName("NeedAppearances");
    /** A name */
    static final PdfName NEWWINDOW = new PdfName("NewWindow");
    /** A name */
    static final PdfName NEXT = new PdfName("Next");
    /** A name */
    static final PdfName NEXTPAGE = new PdfName("NextPage");
    /** A name */
    static final PdfName NM = new PdfName("NM");
    /** A name */
    public static final PdfName NONE = new PdfName("None");
    /** A name */
    public static final PdfName NONFULLSCREENPAGEMODE = new PdfName("NonFullScreenPageMode");


    /** A name */
    public static final PdfName NUMCOPIES = new PdfName("NumCopies");
    /** A name */
    static final PdfName NUMS = new PdfName("Nums");
    /** A name */
    public static final PdfName O = new PdfName("O");


    /** A name */
    static final PdfName OBJSTM = new PdfName("ObjStm");
    /** A name */
    static final PdfName OC = new PdfName("OC");

    /** A name */
    static final PdfName OCGS = new PdfName("OCGs");
    /** A name */
    static final PdfName OCMD = new PdfName("OCMD");
    /** A name */
    static final PdfName OCPROPERTIES = new PdfName("OCProperties");
    /** A name */
    static final PdfName Off = new PdfName("Off");
    /** A name */
    static final PdfName OFF = new PdfName("OFF");
    /** A name */
    static final PdfName ON = new PdfName("ON");
    /** A name */
    public static final PdfName ONECOLUMN = new PdfName("OneColumn");

    /** A name */
    static final PdfName OPENACTION = new PdfName("OpenAction");
    /** A name */
    static final PdfName OP = new PdfName("OP");
    /** A name */
    static final PdfName op = new PdfName("op");
    /** A name */
    static final PdfName OPM = new PdfName("OPM");
    /** A name */
    static final PdfName OPT = new PdfName("Opt");
    /** A name */
    static final PdfName ORDER = new PdfName("Order");
    /** A name */
    static final PdfName ORDERING = new PdfName("Ordering");


    /** A name */
    static final PdfName OUTLINES = new PdfName("Outlines");
    /** A name */
    public static final PdfName OUTPUTCONDITION = new PdfName("OutputCondition");
    /** A name */
    public static final PdfName OUTPUTCONDITIONIDENTIFIER = new PdfName("OutputConditionIdentifier");
    /** A name */
    public static final PdfName OUTPUTINTENT = new PdfName("OutputIntent");
    /** A name */
    public static final PdfName OUTPUTINTENTS = new PdfName("OutputIntents");
    /** A name */
    public static final PdfName P = new PdfName("P");
    /** A name */
    static final PdfName PAGE = new PdfName("Page");
    /** A name */
    static final PdfName PAGELABELS = new PdfName("PageLabels");
    /** A name */
    public static final PdfName PAGELAYOUT = new PdfName("PageLayout");
    /** A name */
    public static final PdfName PAGEMODE = new PdfName("PageMode");
    /** A name */
    static final PdfName PAGES = new PdfName("Pages");
    /** A name */
    static final PdfName PAINTTYPE = new PdfName("PaintType");
    /** A name */
    static final PdfName PANOSE = new PdfName("Panose");
    /** A name */
    static final PdfName PARAMS = new PdfName("Params");
    /** A name */
    static final PdfName PARENT = new PdfName("Parent");
    /** A name */
    static final PdfName PARENTTREE = new PdfName("ParentTree");



    /** A name */
    static final PdfName PATTERN = new PdfName("Pattern");
    /** A name */
    static final PdfName PATTERNTYPE = new PdfName("PatternType");



    /** A name */
    public static final PdfName PERCEPTUAL = new PdfName("Perceptual");
    /** A name */
    static final PdfName PERMS = new PdfName("Perms");


    /** A name */
    public static final PdfName PICKTRAYBYPDFSIZE = new PdfName("PickTrayByPDFSize");


    /** A name */
    static final PdfName POPUP = new PdfName("Popup");

    /** A name */
    public static final PdfName PREDICTOR = new PdfName("Predictor");



    /** A name */
    static final PdfName PREV = new PdfName("Prev");
    /** A name */
    static final PdfName PREVPAGE = new PdfName("PrevPage");
    /** A name */
    static final PdfName PRINT = new PdfName("Print");
    /** A name */
    public static final PdfName PRINTAREA = new PdfName("PrintArea");
    /** A name */
    public static final PdfName PRINTCLIP = new PdfName("PrintClip");
    /** A name */
    public static final PdfName PRINTPAGERANGE = new PdfName("PrintPageRange");
    /** A name */
    public static final PdfName PRINTSCALING = new PdfName("PrintScaling");


    /** A name */
    static final PdfName PROCSET = new PdfName("ProcSet");
    /** A name */
    public static final PdfName PRODUCER = new PdfName("Producer");
    /** A name */
    static final PdfName PROPERTIES = new PdfName("Properties");

    /** A name */
    static final PdfName PUBSEC = new PdfName("Adobe.PubSec");

    /** A name */
    static final PdfName Q = new PdfName("Q");


    /** A name */
    static final PdfName R = new PdfName("R");
    /** A name */
    public static final PdfName R2L = new PdfName("R2L");
    /** A name */
    static final PdfName RANGE = new PdfName("Range");
    /** A name */
    static final PdfName RC = new PdfName("RC");
    /** A name */
    static final PdfName RBGROUPS = new PdfName("RBGroups");
    /** A name */
    static final PdfName REASON = new PdfName("Reason");
    /** A name */
    static final PdfName RECIPIENTS = new PdfName("Recipients");
    /** A name */
    public static final PdfName RECT = new PdfName("Rect");
    /** A name */
    static final PdfName REFERENCE = new PdfName("Reference");
    /** A name */
    static final PdfName REGISTRY = new PdfName("Registry");
    /** A name */
    public static final PdfName REGISTRYNAME = new PdfName("RegistryName");
    /**
     * A name
     * @since	2.1.5 renamed from RELATIVECALORIMETRIC
     */
    public static final PdfName RELATIVECOLORIMETRIC = new PdfName("RelativeColorimetric");
    /** A name */
    static final PdfName RENDITION = new PdfName("Rendition");

    /** A name */
    public static final PdfName RESOURCES = new PdfName("Resources");
    /** A name */
    static final PdfName RI = new PdfName("RI");


    /** A name */
    static final PdfName ROOT = new PdfName("Root");
    /** A name */
    static final PdfName ROTATE = new PdfName("Rotate");
    /** A name */
    static final PdfName ROWS = new PdfName("Rows");


    /** A name */
    static final PdfName RV = new PdfName("RV");
    /** A name */
    public static final PdfName S = new PdfName("S");
    /** A name */
    public static final PdfName SATURATION = new PdfName("Saturation");
    /** A name */
    public static final PdfName SCHEMA = new PdfName("Schema");
    /** A name */
    static final PdfName SCREEN = new PdfName("Screen");


    /** A name */
    static final PdfName SEPARATION = new PdfName("Separation");


    /** A name */
    static final PdfName SHADING = new PdfName("Shading");
    /** A name */
    static final PdfName SHADINGTYPE = new PdfName("ShadingType");
    /** A name */
    static final PdfName SHIFT_JIS = new PdfName("Shift-JIS");
    /** A name */
    static final PdfName SIG = new PdfName("Sig");
    /** A name */
    static final PdfName SIGFLAGS = new PdfName("SigFlags");
    /** A name */
    static final PdfName SIGREF = new PdfName("SigRef");
    /** A name */
    public static final PdfName SIMPLEX = new PdfName("Simplex");
    /** A name */
    public static final PdfName SINGLEPAGE = new PdfName("SinglePage");
    /** A name */
    static final PdfName SIZE = new PdfName("Size");
    /** A name */
    public static final PdfName SMASK = new PdfName("SMask");
    /** A name */
    public static final PdfName SORT = new PdfName("Sort");



    /** A name */
    static final PdfName SPLIT = new PdfName("Split");


    /** A name */
    static final PdfName ST = new PdfName("St");

    /** A name */
    static final PdfName STANDARD = new PdfName("Standard");

    /** A name */
    static final PdfName STDCF = new PdfName("StdCF");
    /** A name */
    static final PdfName STEMV = new PdfName("StemV");
    /** A name */
    static final PdfName STMF = new PdfName("StmF");
    /** A name */
    static final PdfName STRF = new PdfName("StrF");

    /** A name */
    static final PdfName STRUCTPARENT = new PdfName("StructParent");
    /** A name */
    static final PdfName STRUCTPARENTS = new PdfName("StructParents");
    /** A name */
    static final PdfName STRUCTTREEROOT = new PdfName("StructTreeRoot");
    /** A name */
    static final PdfName STYLE = new PdfName("Style");
    /** A name */
    static final PdfName SUBFILTER = new PdfName("SubFilter");
    /** A name */
    public static final PdfName SUBJECT = new PdfName("Subject");

    /** A name */
    static final PdfName SUBTYPE = new PdfName("Subtype");
    /** A name */
    static final PdfName SUPPLEMENT = new PdfName("Supplement");
    /** A name */
    static final PdfName SV = new PdfName("SV");
    /** A name */
    static final PdfName SW = new PdfName("SW");
    /** A name of a base 14 type 1 font */
    static final PdfName SYMBOL = new PdfName("Symbol");
    /** A name */
    public static final PdfName T = new PdfName("T");


    /**
     * A name
     * @since	2.1.5
     */
    static final PdfName TABS = new PdfName("Tabs");


    /** A name */
    static final PdfName TEXT = new PdfName("Text");



    /** A name */
    static final PdfName THUMB = new PdfName("Thumb");

    /** A name */
    static final PdfName TI = new PdfName("TI");

    /** A name */
    static final PdfName TILINGTYPE = new PdfName("TilingType");
    /** A name of a base 14 type 1 font */
    static final PdfName TIMES_ROMAN = new PdfName("Times-Roman");
    /** A name of a base 14 type 1 font */
    static final PdfName TIMES_BOLD = new PdfName("Times-Bold");
    /** A name of a base 14 type 1 font */
    static final PdfName TIMES_ITALIC = new PdfName("Times-Italic");
    /** A name of a base 14 type 1 font */
    static final PdfName TIMES_BOLDITALIC = new PdfName("Times-BoldItalic");
    /** A name */
    public static final PdfName TITLE = new PdfName("Title");
    /** A name */
    static final PdfName TK = new PdfName("TK");
    /** A name */
    static final PdfName TM = new PdfName("TM");




    /** A name */
    static final PdfName TOUNICODE = new PdfName("ToUnicode");
    /** A name */
    static final PdfName TP = new PdfName("TP");

    /** A name */
    static final PdfName TRANS = new PdfName("Trans");
    /** A name */
    static final PdfName TRANSFORMPARAMS = new PdfName("TransformParams");
    /** A name */
    static final PdfName TRANSFORMMETHOD = new PdfName("TransformMethod");
    /** A name */
    static final PdfName TRANSPARENCY = new PdfName("Transparency");

    /** A name */
    public static final PdfName TRAPPED = new PdfName("Trapped");
    /** A name */
    public static final PdfName TRIMBOX = new PdfName("TrimBox");
    /** A name */
    static final PdfName TRUETYPE = new PdfName("TrueType");
    /** A name */
    public static final PdfName TU = new PdfName("TU");
    /** A name */
    public static final PdfName TWOCOLUMNLEFT = new PdfName("TwoColumnLeft");
    /** A name */
    public static final PdfName TWOCOLUMNRIGHT = new PdfName("TwoColumnRight");
    /** A name */
    public static final PdfName TWOPAGELEFT = new PdfName("TwoPageLeft");
    /** A name */
    public static final PdfName TWOPAGERIGHT = new PdfName("TwoPageRight");
    /** A name */
    static final PdfName TX = new PdfName("Tx");
    /** A name */
    static final PdfName TYPE = new PdfName("Type");
    /** A name */
    static final PdfName TYPE0 = new PdfName("Type0");
    /** A name */
    static final PdfName TYPE1 = new PdfName("Type1");

    /** A name of an attribute. */
    static final PdfName U = new PdfName("U");
    /** A name of an attribute. */
    static final PdfName UF = new PdfName("UF");
    /** A name of an attribute. */
    static final PdfName UHC = new PdfName("UHC");

    /** A name */
    static final PdfName UR = new PdfName("UR");
    /** A name */
    static final PdfName UR3 = new PdfName("UR3");
    /** A name */
    static final PdfName URI = new PdfName("URI");

    /** A name */
    static final PdfName USAGE = new PdfName("Usage");
    /** A name */
    public static final PdfName USEATTACHMENTS = new PdfName("UseAttachments");
    /** A name */
    public static final PdfName USENONE = new PdfName("UseNone");
    /** A name */
    public static final PdfName USEOC = new PdfName("UseOC");
    /** A name */
    public static final PdfName USEOUTLINES = new PdfName("UseOutlines");

    /** A name */
    static final PdfName USERPROPERTIES = new PdfName("UserProperties");
    /** A name */
    static final PdfName USERUNIT = new PdfName("UserUnit");
    /** A name */
    public static final PdfName USETHUMBS = new PdfName("UseThumbs");
    /** A name */
    public static final PdfName V = new PdfName("V");
    /** A name */
    static final PdfName V2 = new PdfName("V2");

    /** A name */
    static final PdfName VERISIGN_PPKVS = new PdfName("VeriSign.PPKVS");
    /** A name */
	public static final PdfName VERSION = new PdfName("Version");

    /** A name */
    static final PdfName VIEW = new PdfName("View");

    /** A name */
    public static final PdfName VIEWAREA = new PdfName("ViewArea");
    /** A name */
    public static final PdfName VIEWCLIP = new PdfName("ViewClip");
    /** A name */
    public static final PdfName VIEWERPREFERENCES = new PdfName("ViewerPreferences");
    /** A name */
    static final PdfName VIEWSTATE = new PdfName("ViewState");
    /** A name */
    static final PdfName VISIBLEPAGES = new PdfName("VisiblePages");

    /** A name of an attribute. */
    static final PdfName W = new PdfName("W");
    /** A name of an attribute. */
    static final PdfName W2 = new PdfName("W2");

    /** A name of an attribute. */
    static final PdfName WC = new PdfName("WC");
    /** A name of an attribute. */
    static final PdfName WIDGET = new PdfName("Widget");
    /** A name of an attribute. */
    public static final PdfName WIDTH = new PdfName("Width");
    /** A name */
    static final PdfName WIDTHS = new PdfName("Widths");
    /** A name of an encoding */
    static final PdfName WIN = new PdfName("Win");
    /** A name of an encoding */
    static final PdfName WIN_ANSI_ENCODING = new PdfName("WinAnsiEncoding");


    /** A name of an encoding */
    static final PdfName WIPE = new PdfName("Wipe");
    /** A name */
    public static final PdfName WHITEPOINT = new PdfName("WhitePoint");
    /** A name */
    static final PdfName WP = new PdfName("WP");
    /** A name of an encoding */
    static final PdfName WS = new PdfName("WS");



    /** A name */
    static final PdfName XFA = new PdfName("XFA");
    /** A name */
    static final PdfName XML = new PdfName("XML");
    /** A name */
    static final PdfName XOBJECT = new PdfName("XObject");
    /** A name */
    static final PdfName XSTEP = new PdfName("XStep");
    /** A name */
    static final PdfName XREF = new PdfName("XRef");
    /** A name */
    static final PdfName XREFSTM = new PdfName("XRefStm");
    /** A name */
    static final PdfName XYZ = new PdfName("XYZ");
    /** A name */
    static final PdfName YSTEP = new PdfName("YStep");
    /** A name */
    static final PdfName ZADB = new PdfName("ZaDb");
    /** A name of a base 14 type 1 font */
    static final PdfName ZAPFDINGBATS = new PdfName("ZapfDingbats");
    /** A name */
    static final PdfName ZOOM = new PdfName("Zoom");

    /**
     * map strings to all known static names
     * @since 2.1.6
     */
    static Map staticNames;

    /**
     * Use reflection to cache all the static public final names so
     * future <code>PdfName</code> additions don't have to be "added twice".
     * A bit less efficient (around 50ms spent here on a 2.2ghz machine),
     *  but Much Less error prone.
     * @since 2.1.6
     */

    static {
        final Field fields[] = PdfName.class.getDeclaredFields();
        staticNames = new HashMap( fields.length );
        final int flags = Modifier.STATIC | Modifier.PUBLIC | Modifier.FINAL;
        try {
            for (final Field curFld : fields) {
                if ((curFld.getModifiers() & flags) == flags &&
                    curFld.getType().equals( PdfName.class )) {
                    final PdfName name = (PdfName)curFld.get( null );
                    staticNames.put( decodeName( name.toString() ), name );
                }
            }
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }
    // CLASS VARIABLES

    private int hash = 0;

    // CONSTRUCTORS

    /**
     * Constructs a new <CODE>PdfName</CODE>. The name length will be checked.
     *
     * @param name the new name
     */
    public PdfName(final String name) {
        this(name, true);
    }

    /**
     * Constructs a new <CODE>PdfName</CODE>.
     * @param name the new name
     * @param lengthCheck if <CODE>true</CODE> check the length validity,
     * if <CODE>false</CODE> the name can have any length
     */
    PdfName(final String name, final boolean lengthCheck) {
        super(PdfObject.NAME);
        // The minimum number of characters in a name is 0, the maximum is 127 (the '/' not included)
        final int length = name.length();
        if (lengthCheck && length > 127) {
			throw new IllegalArgumentException("The name '" + name + "' is too long (" + length + " characters).");
		}
        this.bytes = encodeName(name);
    }

    /**
     * Constructs a PdfName.
     *
     * @param bytes the byte representation of the name
     */
    PdfName(final byte bytes[]) {
        super(PdfObject.NAME, bytes);
    }

    // CLASS METHODS

    /**
     * Compares this object with the specified object for order.
     * Returns a negative integer, zero, or a positive integer as this object
     * is less than, equal to, or greater than the specified object.<p>
     *
     * @param object the Object to be compared.
     * @return a negative integer, zero, or a positive integer as this object
     * is less than, equal to, or greater than the specified object.
     * @throws ClassCastException if the specified object's type prevents it
     * from being compared to this Object.
     */
    @Override
	public int compareTo(final Object object) {
        final PdfName name = (PdfName) object;
        final byte myBytes[] = this.bytes;
        final byte objBytes[] = name.bytes;
        final int len = Math.min(myBytes.length, objBytes.length);
        for(int i = 0; i < len; i++) {
            if (myBytes[i] > objBytes[i]) {
				return 1;
			}
            if (myBytes[i] < objBytes[i]) {
				return -1;
			}
        }
        if (myBytes.length < objBytes.length) {
			return -1;
		}
        if (myBytes.length > objBytes.length) {
			return 1;
		}
        return 0;
    }

    /**
     * Indicates whether some other object is "equal to" this one.
     *
     * @param   obj   the reference object with which to compare.
     * @return  <code>true</code> if this object is the same as the obj
     * argument; <code>false</code> otherwise.
     */
    @Override
	public boolean equals(final Object obj) {
        if (this == obj) {
			return true;
		}
        if (obj instanceof PdfName) {
			return compareTo(obj) == 0;
		}
        return false;
    }

    /**
     * Returns a hash code value for the object.
     * This method is supported for the benefit of hashtables such as those provided by
     * <code>java.util.Hashtable</code>.
     *
     * @return  a hash code value for this object.
     */
    @Override
	public int hashCode() {
        int h = this.hash;
        if (h == 0) {
            int ptr = 0;
            final int len = this.bytes.length;
            for (int i = 0; i < len; i++) {
				h = 31*h + (this.bytes[ptr++] & 0xff);
			}
            this.hash = h;
        }
        return h;
    }

    /**
     * Encodes a plain name given in the unescaped form "AB CD" into "/AB#20CD".
     *
     * @param name the name to encode
     * @return the encoded name
     * @since	2.1.5
     */
    private static byte[] encodeName(final String name) {
    	final int length = name.length();
    	final ByteBuffer buf = new ByteBuffer(length + 20);
    	buf.append('/');
    	char c;
    	final char chars[] = name.toCharArray();
    	for (int k = 0; k < length; k++) {
    		c = (char)(chars[k] & 0xff);
    		// Escape special characters
    		switch (c) {
    			case ' ':
    			case '%':
    			case '(':
    			case ')':
    			case '<':
    			case '>':
    			case '[':
    			case ']':
    			case '{':
    			case '}':
    			case '/':
    			case '#':
    				buf.append('#');
    				buf.append(Integer.toString(c, 16));
    				break;
    			default:
    				if (c >= 32 && c <= 126) {
						buf.append(c);
					} else {
    					buf.append('#');
    					if (c < 16) {
							buf.append('0');
						}
    					buf.append(Integer.toString(c, 16));
    				}
    				break;
    			}
    		}
    	return buf.toByteArray();
    }

    /**
     * Decodes an escaped name given in the form "/AB#20CD" into "AB CD".
     *
     * @param name the name to decode
     * @return the decoded name
     */
    static String decodeName(final String name) {
        final StringBuffer buf = new StringBuffer();
        try {
            final int len = name.length();
            for (int k = 1; k < len; ++k) {
                char c = name.charAt(k);
                if (c == '#') {
                	final char c1 = name.charAt(k + 1);
                	final char c2 = name.charAt(k + 2);
                    c = (char)((PRTokeniser.getHex(c1) << 4) + PRTokeniser.getHex(c2));
                    k += 2;
                }
                buf.append(c);
            }
        }
        catch (final IndexOutOfBoundsException e) {
            // empty on purpose
        }
        return buf.toString();
    }
}
