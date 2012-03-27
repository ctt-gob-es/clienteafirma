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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;


/**
 * This class is the primary class for jMimeMagic
 * @author $Author: arimus $
 * @version $Revision: 1.8 $
 */
@SuppressWarnings("javadoc")
public class Magic {
    private static boolean initialized = false;
    private static MagicParser magicParser = null;
    private static HashMap<String, ArrayList<MagicMatcher>> hintMap = new HashMap<String, ArrayList<MagicMatcher>>();

    /**
     * Add a hint to use the specified matcher for the given extension
     *
     * @param extension DOCUMENT ME!
     * @param matcher DOCUMENT ME!
     */
    private static void addHint(final String extension, final MagicMatcher matcher) {
        if (hintMap.keySet().contains(extension)) {
            final ArrayList<MagicMatcher> a = hintMap.get(extension);
            a.add(matcher);
        } else {
            final ArrayList<MagicMatcher> a = new ArrayList<MagicMatcher>();
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
            final Iterator<MagicMatcher> i = magicParser.getMatchers().iterator();

            while (i.hasNext()) {
                final MagicMatcher matcher = i.next();
                final String ext = matcher.getMatch().getExtension();

                if ((ext != null) && !ext.trim().equals("")) { //$NON-NLS-1$
					addHint(ext, matcher);
				}
                else if (matcher.getMatch().getType().equals("detector")) { //$NON-NLS-1$
                    final String[] exts = matcher.getDetectorExtensions();
                    for (final String ext2 : exts) {
						addHint(ext2, matcher);
					}
                }
            }

            initialized = true;
        }
    }

    /** Get a match from a stream of data. */
    public static MagicMatch getMagicMatch(final byte[] data) throws MagicParseException, MagicMatchNotFoundException, MagicException {
        return getMagicMatch(data, false);
    }

    /** Get a match from a stream of data. */
	public static MagicMatch getMagicMatch(final byte[] data, final boolean onlyMimeMatch) throws MagicParseException, MagicMatchNotFoundException, MagicException {

        if (!initialized) {
			initialize();
		}

        final Collection<MagicMatcher> matchers = magicParser.getMatchers();

        MagicMatcher matcher = null;
        MagicMatch match = null;
        final Iterator<MagicMatcher> i = matchers.iterator();

        while (i.hasNext()) {
            matcher = i.next();
            try {
                if ((match = matcher.test(data, onlyMimeMatch)) != null) {
					return match;
				}
            }
            catch (final Throwable e) {
                throw new MagicException(e);
            }
        }
        throw new MagicMatchNotFoundException();
    }
}
