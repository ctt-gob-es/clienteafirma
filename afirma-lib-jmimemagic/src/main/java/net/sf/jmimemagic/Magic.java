/*
jMimeMagic(TM) is a Java library for determining the content type of files or
streams.

Copyright (C) 2004 David Castro

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

For more information, please email arimus@users.sourceforge.net
*/
package net.sf.jmimemagic;

import java.io.File;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;


/**
 * This class is the primary class for jMimeMagic
 * @author $Author: arimus $
 * @version $Revision: 1.8 $
 */
public class Magic {
    private static boolean initialized = false;
    private static MagicParser magicParser = null;
    private static HashMap hintMap = new HashMap();

    /**
     * Add a hint to use the specified matcher for the given extension
     * 
     * @param extension DOCUMENT ME!
     * @param matcher DOCUMENT ME!
     */
    private static void addHint(String extension, MagicMatcher matcher) {
        if (hintMap.keySet().contains(extension)) {
            ArrayList a = (ArrayList) hintMap.get(extension);
            a.add(matcher);
        } else {
            ArrayList a = new ArrayList();
            a.add(matcher);
            hintMap.put(extension, a);
        }
    }

    /** Create a parser and initialize it. */
    public static synchronized void initialize() throws MagicParseException {

        if (!initialized) {
            magicParser = new MagicParser();
            magicParser.initialize();

            // build hint map
            Iterator i = magicParser.getMatchers().iterator();

            while (i.hasNext()) {
                MagicMatcher matcher = (MagicMatcher) i.next();
                String ext = matcher.getMatch().getExtension();

                if ((ext != null) && !ext.trim().equals("")) addHint(ext, matcher);
                else if (matcher.getMatch().getType().equals("detector")) {
                    String[] exts = matcher.getDetectorExtensions();
                    for (int j = 0; j < exts.length; j++) addHint(exts[j], matcher);
                }
            }

            initialized = true;
        }
    }

    /**
     * return the parsed MagicMatch objects that were created from the magic.xml
     * definitions
     * @return the parsed MagicMatch objects
     * @throws MagicParseException DOCUMENT ME!
     */
    public static Collection getMatchers() throws MagicParseException {

        if (!initialized) initialize();

        Iterator i = magicParser.getMatchers().iterator();
        ArrayList m = new ArrayList();

        while (i.hasNext()) {
            MagicMatcher matcher = (MagicMatcher) i.next();

            try {
                m.add(matcher.clone());
            } 
            catch (CloneNotSupportedException e) {
                throw new MagicParseException("failed to clone matchers");
            }
        }

        return m;
    }

    /** Get a match from a stream of data. */
    public static MagicMatch getMagicMatch(byte[] data) throws MagicParseException, MagicMatchNotFoundException, MagicException {
        return getMagicMatch(data, false);
    }

    /** Get a match from a stream of data. */
    public static MagicMatch getMagicMatch(byte[] data, boolean onlyMimeMatch) throws MagicParseException, MagicMatchNotFoundException, MagicException {

        if (!initialized) initialize();

        Collection matchers = magicParser.getMatchers();

        MagicMatcher matcher = null;
        MagicMatch match = null;
        Iterator i = matchers.iterator();

        while (i.hasNext()) {
            matcher = (MagicMatcher) i.next();
            try {
                if ((match = matcher.test(data, onlyMimeMatch)) != null) return match;
            } 
            catch (Throwable e) {
                throw new MagicException(e);
            }
        }
        throw new MagicMatchNotFoundException();
    }

    /**
     * get a match from a file
     *
     * @param file the file to match content in
     * @param extensionHints whether or not to use extension to optimize order of content tests
     * @return the MagicMatch object representing a match in the file
     *
     */
    public static MagicMatch getMagicMatch(File file, boolean extensionHints) throws MagicParseException, MagicMatchNotFoundException, MagicException {
        return getMagicMatch(file, extensionHints, false);
    }

    /**
     * get a match from a file
     *
     * @param file the file to match content in
     * @param extensionHints whether or not to use extension to optimize order of content tests
     * @param onlyMimeMatch only try to get mime type, no submatches are processed when true
     * @return the MagicMatch object representing a match in the file
     *
     * @throws MagicParseException DOCUMENT ME!
     * @throws MagicMatchNotFoundException DOCUMENT ME!
     * @throws MagicException DOCUMENT ME!
     */
    public static MagicMatch getMagicMatch(File file, boolean extensionHints, boolean onlyMimeMatch) throws MagicParseException, MagicMatchNotFoundException, MagicException {

        if (!initialized) initialize();

        MagicMatcher matcher = null;
        MagicMatch match = null;

        // check for extension hints
        ArrayList checked = new ArrayList();

        if (extensionHints) {

            String name = file.getName();
            int pos = name.lastIndexOf('.');

            if (pos > -1) {
                String ext = name.substring(pos + 1, name.length());

                if ((ext != null) && !ext.equals("")) {
                    Collection c = (Collection) hintMap.get(ext);
                    if (c != null) {
                        Iterator i = c.iterator();
                        while (i.hasNext()) {
                            matcher = (MagicMatcher) i.next();
                            try {
                                if ((match = matcher.test(file, onlyMimeMatch)) != null) return match;
                            } 
                            catch (Throwable e) {
                                throw new MagicException(e);
                            }
                            // add to the already checked list
                            checked.add(matcher);
                        }
                    }
                } 
            } 
        }

        Collection matchers = magicParser.getMatchers();
        Iterator i = matchers.iterator();

        while (i.hasNext()) {
            matcher = (MagicMatcher) i.next();
            if (!checked.contains(matcher)) {
                try {
                    if ((match = matcher.test(file, onlyMimeMatch)) != null) return match;
                } 
                catch (Throwable e) {
                    throw new MagicException(e);
                }
            } 
        }

        throw new MagicMatchNotFoundException();
    }

    /** Print the contents of a magic file. */
    public static void printMagicFile(PrintStream stream) throws MagicParseException {
        if (!initialized) initialize();

        Collection matchers = Magic.getMatchers();

        MagicMatcher matcher = null;
        Iterator i = matchers.iterator();

        while (i.hasNext()) {
            matcher = (MagicMatcher) i.next();
            printMagicMatcher(stream, matcher, "");
        }
    }

    /** Print a magic match. */
    private static void printMagicMatcher(PrintStream stream, MagicMatcher matcher, String spacing) {
        stream.println(spacing + "name: " + matcher.getMatch().getDescription());
        stream.println(spacing + "children: ");
        Collection matchers = matcher.getSubMatchers();
        Iterator i = matchers.iterator();
        while (i.hasNext()) printMagicMatcher(stream, (MagicMatcher) i.next(), spacing + "  ");
    }

    /** Print a magic match. */
    public static void printMagicMatch(PrintStream stream, MagicMatch match, String spacing) {
        stream.println(spacing + "=============================");
        stream.println(spacing + "mime type: " + match.getMimeType());
        stream.println(spacing + "description: " + match.getDescription());
        stream.println(spacing + "extension: " + match.getExtension());
        stream.println(spacing + "test: " + new String(match.getTest().array()));
        stream.println(spacing + "bitmask: " + match.getBitmask());
        stream.println(spacing + "offset: " + match.getOffset());
        stream.println(spacing + "length: " + match.getLength());
        stream.println(spacing + "type: " + match.getType());
        stream.println(spacing + "comparator: " + match.getComparator());
        stream.println(spacing + "=============================");

        Collection submatches = match.getSubMatches();
        Iterator i = submatches.iterator();

        while (i.hasNext()) printMagicMatch(stream, (MagicMatch) i.next(), spacing + "    ");
    }

    public static void main(String[] args)
    {
        //		Magic magic = new Magic();
        try {
            
            String filename = "C:/Entrada2.txt";
//            String filename = "C:/jmagick.dll";
            
            //Magic.initialize();
            File f = new File(filename);

            if (f.exists()) {
                MagicMatch match = Magic.getMagicMatch(f, true, false);
                
                System.out.println("filename: " + filename);
                printMagicMatch(System.out, match, "");

                //				Collection submatches = match.getSubMatches();
                //				if (match == null) {
                //					System.out.println(args[0]+": unknown");
                //				} else {
                //					System.out.println("=============================");
                //					System.out.println("filename: "+args[0]);
                //					System.out.println("mime type: "+match.getMimeType());
                //					System.out.println("description: "+match.getDescription());
                //					System.out.println("extension: "+match.getExtension());
                //					System.out.println("test: "+new String(match.getTest().array()));
                //					System.out.println("bitmask: "+match.getBitmask());
                //					System.out.println("offset: "+match.getOffset());
                //					System.out.println("length: "+match.getLength());
                //					System.out.println("type: "+match.getType());
                //					System.out.println("comparator: "+match.getComparator());
                //					System.out.println("=============================");
                //
                //					Iterator i = submatches.iterator();
                //					while (i.hasNext()) {
                //						System.out.println("== SUBMATCH =================");
                //						MagicMatch m = (MagicMatch)i.next();
                //						System.out.println(m.print());
                //						System.out.println("=============================");
                //					}
                //				}

                //				FileInputStream fis = new FileInputStream(f);
                //				ByteBuffer buffer = ByteBuffer.allocate((int)f.length());
                //				byte []buf = new byte[2048];
                //				int size = 0;
                //				while ((size = fis.read(buf, 0, 2048)) > 0) {
                //					buffer.put(buf, 0, size);
                //				}
                //				byte []tmp = buffer.array();
                //				match = parser.getMagicMatch(tmp);
                //				if (match == null) {
                //					System.out.println(args[0]+": unknown");
                //				} else {
                //					System.out.println(args[0]+": "+match.getDescription());
                //					System.out.println(match.getMimeType());
                //				}
            } else {
                System.err.println("file '" + f.getCanonicalPath() + "' not found");
            }
        } catch (MagicMatchNotFoundException e) {
            System.out.println("no match found");
        } catch (Exception e) {
            System.err.println("error: " + e);
            e.printStackTrace(System.err);
        }
    }
}
