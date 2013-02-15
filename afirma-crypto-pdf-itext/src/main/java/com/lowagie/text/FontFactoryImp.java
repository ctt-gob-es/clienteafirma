/*
 * $Id: FontFactoryImp.java 3548 2008-07-12 11:15:35Z blowagie $
 *
 * Copyright 2002 by Bruno Lowagie.
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

package com.lowagie.text;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;

import com.lowagie.text.html.Markup;
import com.lowagie.text.pdf.BaseFont;

/**
 * If you are using True Type fonts, you can declare the paths of the different ttf- and ttc-files
 * to this class first and then create fonts in your code using one of the getFont method
 * without having to enter a path as parameter.
 *
 * @author  Bruno Lowagie
 */

class FontFactoryImp {

/** This is a map of postscriptfontnames of True Type fonts and the path of their ttf- or ttc-file. */
    private final Properties trueTypeFonts = new Properties();

    private static String[] TTFamilyOrder = {
        "3", "1", "1033",
        "3", "0", "1033",
        "1", "0", "0",
        "0", "3", "0"
    };

/** This is a map of fontfamilies. */
    private final Hashtable fontFamilies = new Hashtable();

/** This is the default encoding to use. */
    String defaultEncoding = BaseFont.WINANSI;

/** This is the default value of the <VAR>embedded</VAR> variable. */
    boolean defaultEmbedding = BaseFont.NOT_EMBEDDED;

/** Creates new FontFactory */
    public FontFactoryImp() {
        this.trueTypeFonts.setProperty(FontFactory.COURIER.toLowerCase(), FontFactory.COURIER);
        this.trueTypeFonts.setProperty(FontFactory.COURIER_BOLD.toLowerCase(), FontFactory.COURIER_BOLD);
        this.trueTypeFonts.setProperty(FontFactory.COURIER_OBLIQUE.toLowerCase(), FontFactory.COURIER_OBLIQUE);
        this.trueTypeFonts.setProperty(FontFactory.COURIER_BOLDOBLIQUE.toLowerCase(), FontFactory.COURIER_BOLDOBLIQUE);
        this.trueTypeFonts.setProperty(FontFactory.HELVETICA.toLowerCase(), FontFactory.HELVETICA);
        this.trueTypeFonts.setProperty(FontFactory.HELVETICA_BOLD.toLowerCase(), FontFactory.HELVETICA_BOLD);
        this.trueTypeFonts.setProperty(FontFactory.HELVETICA_OBLIQUE.toLowerCase(), FontFactory.HELVETICA_OBLIQUE);
        this.trueTypeFonts.setProperty(FontFactory.HELVETICA_BOLDOBLIQUE.toLowerCase(), FontFactory.HELVETICA_BOLDOBLIQUE);
        this.trueTypeFonts.setProperty(FontFactory.SYMBOL.toLowerCase(), FontFactory.SYMBOL);
        this.trueTypeFonts.setProperty(FontFactory.TIMES_ROMAN.toLowerCase(), FontFactory.TIMES_ROMAN);
        this.trueTypeFonts.setProperty(FontFactory.TIMES_BOLD.toLowerCase(), FontFactory.TIMES_BOLD);
        this.trueTypeFonts.setProperty(FontFactory.TIMES_ITALIC.toLowerCase(), FontFactory.TIMES_ITALIC);
        this.trueTypeFonts.setProperty(FontFactory.TIMES_BOLDITALIC.toLowerCase(), FontFactory.TIMES_BOLDITALIC);
        this.trueTypeFonts.setProperty(FontFactory.ZAPFDINGBATS.toLowerCase(), FontFactory.ZAPFDINGBATS);

        ArrayList tmp;
        tmp = new ArrayList();
        tmp.add(FontFactory.COURIER);
        tmp.add(FontFactory.COURIER_BOLD);
        tmp.add(FontFactory.COURIER_OBLIQUE);
        tmp.add(FontFactory.COURIER_BOLDOBLIQUE);
        this.fontFamilies.put(FontFactory.COURIER.toLowerCase(), tmp);
        tmp = new ArrayList();
        tmp.add(FontFactory.HELVETICA);
        tmp.add(FontFactory.HELVETICA_BOLD);
        tmp.add(FontFactory.HELVETICA_OBLIQUE);
        tmp.add(FontFactory.HELVETICA_BOLDOBLIQUE);
        this.fontFamilies.put(FontFactory.HELVETICA.toLowerCase(), tmp);
        tmp = new ArrayList();
        tmp.add(FontFactory.SYMBOL);
        this.fontFamilies.put(FontFactory.SYMBOL.toLowerCase(), tmp);
        tmp = new ArrayList();
        tmp.add(FontFactory.TIMES_ROMAN);
        tmp.add(FontFactory.TIMES_BOLD);
        tmp.add(FontFactory.TIMES_ITALIC);
        tmp.add(FontFactory.TIMES_BOLDITALIC);
        this.fontFamilies.put(FontFactory.TIMES.toLowerCase(), tmp);
        this.fontFamilies.put(FontFactory.TIMES_ROMAN.toLowerCase(), tmp);
        tmp = new ArrayList();
        tmp.add(FontFactory.ZAPFDINGBATS);
        this.fontFamilies.put(FontFactory.ZAPFDINGBATS.toLowerCase(), tmp);
    }

    /**
     * Constructs a <CODE>Font</CODE>-object.
     *
     * @param	fontname    the name of the font
     * @param	encoding    the encoding of the font
     * @param       embedded    true if the font is to be embedded in the PDF
     * @param	size	    the size of this font
     * @param	style	    the style of this font
     * @param	color	    the <CODE>Color</CODE> of this font.
     * @return the Font constructed based on the parameters
     */
    Font getFont(final String fontname, final String encoding, final boolean embedded, final float size, final int style, final Color color) {
        return getFont(fontname, encoding, embedded, size, style, color, true);
    }



    /**
     * Constructs a <CODE>Font</CODE>-object.
     *
     * @param	fontname    the name of the font
     * @param	encoding    the encoding of the font
     * @param       embedded    true if the font is to be embedded in the PDF
     * @param	size	    the size of this font
     * @param	style	    the style of this font
     * @param	color	    the <CODE>Color</CODE> of this font.
     * @param	cached 		true if the font comes from the cache or is added to
     * 				the cache if new, false if the font is always created new
     * @return the Font constructed based on the parameters
     */
    private Font getFont(String fontname, final String encoding, final boolean embedded, final float size, int style, final Color color, final boolean cached) {
    	if (fontname == null) {
			return new Font(Font.UNDEFINED, size, style, color);
		}
        final String lowercasefontname = fontname.toLowerCase();
        final ArrayList tmp = (ArrayList) this.fontFamilies.get(lowercasefontname);
        if (tmp != null) {
            // some bugs were fixed here by Daniel Marczisovszky
            final int s = style == Font.UNDEFINED ? Font.NORMAL : style;
            int fs = Font.NORMAL;
            boolean found = false;
            for (final Iterator i = tmp.iterator(); i.hasNext(); ) {
                final String f = (String) i.next();
                final String lcf = f.toLowerCase();
                fs = Font.NORMAL;
                if (lcf.toLowerCase().indexOf("bold") != -1) {
					fs |= Font.BOLD;
				}
                if (lcf.toLowerCase().indexOf("italic") != -1 || lcf.toLowerCase().indexOf("oblique") != -1) {
					fs |= Font.ITALIC;
				}
                if ((s & Font.BOLDITALIC) == fs) {
                    fontname = f;
                    found = true;
                    break;
                }
            }
            if (style != Font.UNDEFINED && found) {
                style &= ~fs;
            }
        }
        BaseFont basefont = null;
        try {
            try {
                // the font is a type 1 font or CJK font
                basefont = BaseFont.createFont(fontname, encoding, embedded, cached, null, null, true);
            }
            catch(final DocumentException de) {
            }
            if (basefont == null) {
                // the font is a true type font or an unknown font
                fontname = this.trueTypeFonts.getProperty(fontname.toLowerCase());
                // the font is not registered as truetype font
                if (fontname == null) {
					return new Font(Font.UNDEFINED, size, style, color);
				}
                // the font is registered as truetype font
                basefont = BaseFont.createFont(fontname, encoding, embedded, cached, null, null);
            }
        }
        catch(final DocumentException de) {
            // this shouldn't happen
            throw new ExceptionConverter(de);
        }
        catch(final IOException ioe) {
            // the font is registered as a true type font, but the path was wrong
            return new Font(Font.UNDEFINED, size, style, color);
        }
        catch(final NullPointerException npe) {
            // null was entered as fontname and/or encoding
            return new Font(Font.UNDEFINED, size, style, color);
        }
        return new Font(basefont, size, style, color);
    }


/**
 * Constructs a <CODE>Font</CODE>-object.
 *
 * @param   attributes  the attributes of a <CODE>Font</CODE> object.
 * @return the Font constructed based on the attributes
 */

    Font getFont(final Properties attributes) {
        String fontname = null;
        String encoding = this.defaultEncoding;
        boolean embedded = this.defaultEmbedding;
        float size = Font.UNDEFINED;
        int style = Font.NORMAL;
        Color color = null;
        String value = attributes.getProperty(Markup.HTML_ATTR_STYLE);
        if (value != null && value.length() > 0) {
            final Properties styleAttributes = Markup.parseAttributes(value);
            if (styleAttributes.isEmpty()) {
                attributes.put(Markup.HTML_ATTR_STYLE, value);
            }
            else {
                fontname = styleAttributes.getProperty(Markup.CSS_KEY_FONTFAMILY);
                if (fontname != null) {
                    String tmp;
                    while (fontname.indexOf(',') != -1) {
                        tmp = fontname.substring(0, fontname.indexOf(','));
                        if (isRegistered(tmp)) {
                            fontname = tmp;
                        }
                        else {
                            fontname = fontname.substring(fontname.indexOf(',') + 1);
                        }
                    }
                }
                if ((value = styleAttributes.getProperty(Markup.CSS_KEY_FONTSIZE)) != null) {
                    size = Markup.parseLength(value);
                }
                if ((value = styleAttributes.getProperty(Markup.CSS_KEY_FONTWEIGHT)) != null) {
                    style |= Font.getStyleValue(value);
                }
                if ((value = styleAttributes.getProperty(Markup.CSS_KEY_FONTSTYLE)) != null) {
                    style |= Font.getStyleValue(value);
                }
                if ((value = styleAttributes.getProperty(Markup.CSS_KEY_COLOR)) != null) {
                    color = Markup.decodeColor(value);
                }
                attributes.putAll(styleAttributes);
                for (final Enumeration e = styleAttributes.keys(); e.hasMoreElements();) {
                    final Object o = e.nextElement();
                    attributes.put(o, styleAttributes.get(o));
                }
            }
        }
        if ((value = attributes.getProperty(ElementTags.ENCODING)) != null) {
            encoding = value;
        }
        if ("true".equals(attributes.getProperty(ElementTags.EMBEDDED))) {
            embedded = true;
        }
        if ((value = attributes.getProperty(ElementTags.FONT)) != null) {
            fontname = value;
        }
        if ((value = attributes.getProperty(ElementTags.SIZE)) != null) {
            size = Markup.parseLength(value);
        }
        if ((value = attributes.getProperty(Markup.HTML_ATTR_STYLE)) != null) {
            style |= Font.getStyleValue(value);
        }
        if ((value = attributes.getProperty(ElementTags.STYLE)) != null) {
            style |= Font.getStyleValue(value);
        }
        final String r = attributes.getProperty(ElementTags.RED);
        final String g = attributes.getProperty(ElementTags.GREEN);
        final String b = attributes.getProperty(ElementTags.BLUE);
        if (r != null || g != null || b != null) {
            int red = 0;
            int green = 0;
            int blue = 0;
            if (r != null) {
				red = Integer.parseInt(r);
			}
            if (g != null) {
				green = Integer.parseInt(g);
			}
            if (b != null) {
				blue = Integer.parseInt(b);
			}
            color = new Color(red, green, blue);
        }
        else if ((value = attributes.getProperty(ElementTags.COLOR)) != null) {
            color = Markup.decodeColor(value);
        }
        if (fontname == null) {
            return getFont(null, encoding, embedded, size, style, color);
        }
        return getFont(fontname, encoding, embedded, size, style, color);
    }

    /**
     * Register a font by giving explicitly the font family and name.
     * @param familyName the font family
     * @param fullName the font name
     * @param path the font path
     */
    private void registerFamily(final String familyName, final String fullName, final String path) {
        if (path != null) {
			this.trueTypeFonts.setProperty(fullName, path);
		}
        ArrayList tmp = (ArrayList) this.fontFamilies.get(familyName);
        if (tmp == null) {
            tmp = new ArrayList();
            tmp.add(fullName);
            this.fontFamilies.put(familyName, tmp);
        }
        else {
            final int fullNameLength = fullName.length();
            boolean inserted = false;
            for (int j = 0; j < tmp.size(); ++j) {
                if (((String)tmp.get(j)).length() >= fullNameLength) {
                    tmp.add(j, fullName);
                    inserted = true;
                    break;
                }
            }
            if (!inserted) {
				tmp.add(fullName);
			}
        }
    }

/**
 * Register a ttf- or a ttc-file.
 *
 * @param   path    the path to a ttf- or ttc-file
 */

    private void register(final String path) {
        register(path, null);
    }

/**
 * Register a font file and use an alias for the font contained in it.
 *
 * @param   path    the path to a font file
 * @param   alias   the alias you want to use for the font
 */

    private void register(final String path, final String alias) {
        try {
            if (path.toLowerCase().endsWith(".ttf") || path.toLowerCase().endsWith(".otf") || path.toLowerCase().indexOf(".ttc,") > 0) {
                final Object allNames[] = BaseFont.getAllFontNames(path, BaseFont.WINANSI, null);
                this.trueTypeFonts.setProperty(((String)allNames[0]).toLowerCase(), path);
                if (alias != null) {
                    this.trueTypeFonts.setProperty(alias.toLowerCase(), path);
                }
                // register all the font names with all the locales
                String[][] names = (String[][])allNames[2]; //full name
                for (final String[] name : names) {
                    this.trueTypeFonts.setProperty(name[3].toLowerCase(), path);
                }
                String fullName = null;
                String familyName = null;
                names = (String[][])allNames[1]; //family name
                for (int k = 0; k < TTFamilyOrder.length; k += 3) {
                    for (final String[] name : names) {
                        if (TTFamilyOrder[k].equals(name[0]) && TTFamilyOrder[k + 1].equals(name[1]) && TTFamilyOrder[k + 2].equals(name[2])) {
                            familyName = name[3].toLowerCase();
                            k = TTFamilyOrder.length;
                            break;
                        }
                    }
                }
                if (familyName != null) {
                    String lastName = "";
                    names = (String[][])allNames[2]; //full name
                    for (final String[] name : names) {
                        for (int k = 0; k < TTFamilyOrder.length; k += 3) {
                            if (TTFamilyOrder[k].equals(name[0]) && TTFamilyOrder[k + 1].equals(name[1]) && TTFamilyOrder[k + 2].equals(name[2])) {
                                fullName = name[3];
                                if (fullName.equals(lastName)) {
									continue;
								}
                                lastName = fullName;
                                registerFamily(familyName, fullName, null);
                                break;
                            }
                        }
                    }
                }
            }
            else if (path.toLowerCase().endsWith(".ttc")) {
                if (alias != null) {
					System.err.println("class FontFactory: You can't define an alias for a true type collection.");
				}
                final String[] names = BaseFont.enumerateTTCNames(path);
                for (int i = 0; i < names.length; i++) {
                    register(path + "," + i);
                }
            }
            else if (path.toLowerCase().endsWith(".afm") || path.toLowerCase().endsWith(".pfm")) {
                final BaseFont bf = BaseFont.createFont(path, BaseFont.CP1252, false);
                final String fullName = bf.getFullFontName()[0][3].toLowerCase();
                final String familyName = bf.getFamilyFontName()[0][3].toLowerCase();
                final String psName = bf.getPostscriptFontName().toLowerCase();
                registerFamily(familyName, fullName, null);
                this.trueTypeFonts.setProperty(psName, path);
                this.trueTypeFonts.setProperty(fullName, path);
            }
        }
        catch(final DocumentException de) {
            // this shouldn't happen
            throw new ExceptionConverter(de);
        }
        catch(final IOException ioe) {
            throw new ExceptionConverter(ioe);
        }
    }

    /**
     * Register all the fonts in a directory and possibly its subdirectories.
     * @param dir the directory
     * @param scanSubdirectories recursively scan subdirectories if <code>true</true>
     * @return the number of fonts registered
     * @since 2.1.2
     */
    private int registerDirectory(final String dir, final boolean scanSubdirectories) {
        int count = 0;
        try {
            File file = new File(dir);
            if (!file.exists() || !file.isDirectory()) {
				return 0;
			}
            final String files[] = file.list();
            if (files == null) {
				return 0;
			}
            for (final String file2 : files) {
                try {
                    file = new File(dir, file2);
                    if (file.isDirectory()) {
                        if (scanSubdirectories) {
                            count += registerDirectory(file.getAbsolutePath(), true);
                        }
                    } else {
                        final String name = file.getPath();
                        final String suffix = name.length() < 4 ? null : name.substring(name.length() - 4).toLowerCase();
                        if (".afm".equals(suffix) || ".pfm".equals(suffix)) {
                            /* Only register Type 1 fonts with matching .pfb files */
                            final File pfb = new File(name.substring(0, name.length() - 4) + ".pfb");
                            if (pfb.exists()) {
                                register(name, null);
                                ++count;
                            }
                        } else if (".ttf".equals(suffix) || ".otf".equals(suffix) || ".ttc".equals(suffix)) {
                            register(name, null);
                            ++count;
                        }
                    }
                }
                catch (final Exception e) {
                    //empty on purpose
                }
            }
        }
        catch (final Exception e) {
            //empty on purpose
        }
        return count;
    }

/**
 * Gets a set of registered fontnames.
 * @return a set of registered fonts
 */

    public Set getRegisteredFonts() {
        return Utilities.getKeySet(this.trueTypeFonts);
    }

/**
 * Gets a set of registered fontnames.
 * @return a set of registered font families
 */

    public Set getRegisteredFamilies() {
        return Utilities.getKeySet(this.fontFamilies);
    }

/**
 * Checks if a certain font is registered.
 *
 * @param   fontname    the name of the font that has to be checked.
 * @return  true if the font is found
 */
    private boolean isRegistered(final String fontname) {
        return this.trueTypeFonts.containsKey(fontname.toLowerCase());
    }
}
