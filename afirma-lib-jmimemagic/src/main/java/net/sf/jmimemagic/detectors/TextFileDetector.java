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
package net.sf.jmimemagic.detectors;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.util.Map;

import net.sf.jmimemagic.MagicDetector;

import org.apache.oro.text.perl.Perl5Util;


/**
 * DOCUMENT ME!
 *
 * @author $Author$
 * @version $Revision$
  */
public final class TextFileDetector implements MagicDetector
{

    /**
     * Creates a new TextFileDetector object.
     */
    public TextFileDetector()
    {
        super();
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getDisplayName()
    {
        return "Text File Detector"; //$NON-NLS-1$
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getVersion()
    {
        return "0.1"; //$NON-NLS-1$
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] getHandledExtensions()
    {
        return new String[] { "txt", "text" }; //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] getHandledTypes()
    {
        return new String[] { "text/plain" }; //$NON-NLS-1$
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getName()
    {
        return "textfiledetector"; //$NON-NLS-1$
    }

    /**
     * DOCUMENT ME!
     *
     * @param data DOCUMENT ME!
     * @param offset DOCUMENT ME!
     * @param length DOCUMENT ME!
     * @param bitmask DOCUMENT ME!
     * @param comparator DOCUMENT ME!
     * @param mimeType DOCUMENT ME!
     * @param params DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] process(final byte[] data, final int offset, final int length, final long bitmask, final char comparator,
        final String mimeType, final Map params)
    {

        final Perl5Util util = new Perl5Util();

        try {
            final String s = new String(data, "UTF-8"); //$NON-NLS-1$

            if (!util.match("/[^[:ascii:][:space:]]/", s)) { //$NON-NLS-1$
                return new String[] { "text/plain" }; //$NON-NLS-1$
            }

            return null;
        } catch (final UnsupportedEncodingException e) {

            return null;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param file DOCUMENT ME!
     * @param offset DOCUMENT ME!
     * @param length DOCUMENT ME!
     * @param bitmask DOCUMENT ME!
     * @param comparator DOCUMENT ME!
     * @param mimeType DOCUMENT ME!
     * @param params DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String[] process(final File file, final int offset, final int length, final long bitmask, final char comparator,
        final String mimeType, final Map params)
    {

        return new String[] {  };
    }
}
