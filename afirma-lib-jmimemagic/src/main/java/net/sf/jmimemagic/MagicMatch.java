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

import java.io.Serializable;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;


/**
 * This class represents a single match test
 *
 * @author $Author: arimus $
 * @version $Revision: 1.10 $
 */
public class MagicMatch implements Cloneable, Serializable {

	private static final long serialVersionUID = -1070505530849518849L;

	private String mimeType = null;
    private String extension = null;
    private String description = null;
    private byte[] test = null;
    private int offset = 0;
    private int length = 0;

    // possible types:
    //     byte, short, long, string, date, beshort, belong, bedate, leshort,
    //     lelong, ledate, regex
    private String type = ""; //$NON-NLS-1$
    private long bitmask = 0xFFFFFFFFL;
    private char comparator = '\0';
    private final ArrayList<MagicMatch> subMatches = new ArrayList<MagicMatch>(0);
    private Map properties;

    /** Set the mime type for this magic match. */
    public void setMimeType(final String value) {
        this.mimeType = value;
    }

    /** Get the magic match for this magic match. */
    public String getMimeType() {
        return this.mimeType;
    }

    /** Set the extension for this magic match. */
    public void setExtension(final String value) {
        this.extension = value;
    }

    /**
     * Get the extension for this magic match.
     * @return the extension for this magic match
     */
    public String getExtension() {
        return this.extension;
    }

    /** Set the description for this magic match. */
    public void setDescription(final String value) {
        this.description = value;
    }

    /**
     * Get the description for this magic match.
     * @return the description for thie magic match
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * set the test value for thie magic match
     *
     * @param value DOCUMENT ME!
     */
    public void setTest(final ByteBuffer value)
    {
        this.test = value.array();
    }

    /**
     * get the test value for this magic match
     *
     * @return DOCUMENT ME!
     */
    public ByteBuffer getTest()
    {
        return ByteBuffer.wrap(this.test);
    }

    /**
     * set the offset in the stream we are comparing to the test value for this magic match
     *
     * @param value DOCUMENT ME!
     */
    public void setOffset(final int value)
    {
        this.offset = value;
    }

    /**
     * get the offset in the stream we are comparing to the test value for this magic match
     *
     * @return the offset for this magic match
     */
    public int getOffset()
    {
        return this.offset;
    }

    /**
     * set the length we are restricting the comparison to for this magic match
     *
     * @param value DOCUMENT ME!
     */
    public void setLength(final int value)
    {
        this.length = value;
    }

    /**
     * get the length we are restricting the comparison to for this magic match
     *
     * @return DOCUMENT ME!
     */
    public int getLength()
    {
        return this.length;
    }

    /**
     * set the type of match to perform for this magic match
     *
     * @param value DOCUMENT ME!
     */
    public void setType(final String value)
    {
        this.type = value;
    }

    /**
     * get the type of match for this magic match
     *
     * @return DOCUMENT ME!
     */
    public String getType()
    {
        return this.type;
    }

    /**
     * set the bitmask that will be applied for this magic match
     *
     * @param value DOCUMENT ME!
     */
    public void setBitmask(final String value)
    {
        if (value != null) {
            this.bitmask = Long.decode(value).intValue();
        }
    }

    /**
     * get the bitmask that will be applied for this magic match
     *
     * @return the bitmask for this magic match
     */
    public long getBitmask()
    {
        return this.bitmask;
    }

    /**
     * set the comparator for this magic match
     *
     * @param value DOCUMENT ME!
     */
    public void setComparator(final String value)
    {
        this.comparator = value.charAt(0);
    }

    /**
     * get the comparator for this magic match
     *
     * @return the comparator for this magic match
     */
    public char getComparator()
    {
        return this.comparator;
    }

    /**
     * set the properties for this magic match
     *
     * @param properties DOCUMENT ME!
     */
    public void setProperties(final Map properties)
    {
        this.properties = properties;
    }

    /**
     * get the properties for this magic match
     *
     * @return the properties for this magic match
     */
    public Map getProperties()
    {
        return this.properties;
    }

    /**
     * Add a submatch to this magic match.
     * @param m a magic match
     */
    public void addSubMatch(final MagicMatch m) {
        this.subMatches.add(m);
    }

    /**
     * Set all submatches.
     * @param a a collection of submatches
     */
    public void setSubMatches(final Collection<MagicMatch> a) {
        this.subMatches.clear();
        this.subMatches.addAll(a);
    }

    /**
     * get all submatches for this magic match
     *
     * @return a collection of submatches
     */
    public Collection<MagicMatch> getSubMatches()
    {
        return this.subMatches;
    }

    /**
     * determine if this match or any submatches has the description
     *
     * @param desc DOCUMENT ME!
     *
     * @return whether or not the description matches
     */
    public boolean descriptionMatches(final String desc)
    {
        if ((this.description != null) && this.description.equals(desc)) {
            return true;
        }

        final Collection<MagicMatch> submatches = getSubMatches();
        final Iterator<MagicMatch> i = submatches.iterator();
        MagicMatch m = null;

        while (i.hasNext()) {
            m = i.next();

            if (m.descriptionMatches(desc)) {
                return true;
            }
        }

        return false;
    }

    /**
     * determine if this match or any submatches has the description
     *
     * @param desc DOCUMENT ME!
     *
     * @return whether or not the description matches
     */
    public boolean mimeTypeMatches(final String desc)
    {
        if ((this.mimeType != null) && this.mimeType.equals(desc)) {
            return true;
        }

        final Collection<MagicMatch> submatches = getSubMatches();
        final Iterator<MagicMatch> i = submatches.iterator();
        MagicMatch m = null;

        while (i.hasNext()) {
            m = i.next();

            if (m.mimeTypeMatches(desc)) {
                return true;
            }
        }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws CloneNotSupportedException DOCUMENT ME!
     */
    @Override
	protected Object clone()
        throws CloneNotSupportedException
    {
        final MagicMatch clone = new MagicMatch();
        clone.setBitmask(Long.toString(this.bitmask, 8));
        clone.setComparator("" + this.comparator); //$NON-NLS-1$
        clone.setDescription(this.description);
        clone.setExtension(this.extension);
        clone.setLength(this.length);
        clone.setMimeType(this.mimeType);
        clone.setOffset(this.offset);

        // these properties should only be String types, so we shouldn't have to clone them
        final HashMap m = new HashMap();
        m.putAll(this.properties);
        clone.setProperties(m);

        final Iterator<MagicMatch> i = this.subMatches.iterator();
        final ArrayList<MagicMatch> a = new ArrayList<MagicMatch>();

        while (i.hasNext()) {
            final MagicMatch mm = i.next();
            a.add(mm);
        }

        clone.setSubMatches(a);

        clone.setTest(ByteBuffer.wrap(this.test));
        clone.setType(this.type);

        // TODO Auto-generated method stub
        return clone;
    }
}
