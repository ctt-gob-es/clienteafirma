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
public class MagicMatch implements Cloneable {
    private String mimeType = null;
    private String extension = null;
    private String description = null;
    private ByteBuffer test = null;
    private int offset = 0;
    private int length = 0;

    // possible types:
    //     byte, short, long, string, date, beshort, belong, bedate, leshort,
    //     lelong, ledate, regex
    private String type = "";
    private long bitmask = 0xFFFFFFFFL;
    private char comparator = '\0';
    private ArrayList subMatches = new ArrayList(0);
    private Map properties;

    /** Print information about this match. */
    public String print() {
        StringBuffer string = new StringBuffer();
        string.append("\n");
        string.append("mime type: ").append(mimeType).append("\n");
        string.append("description: ").append(description).append("\n");
        string.append("extension: ").append(extension).append("\n");
        string.append("offset: ").append(offset).append("\n");
        string.append("length: ").append(length).append("\n");
        string.append("test: ").append(new String(test.array())).append("\n");
        string.append("type: ").append(type).append("\n");
        string.append("comparator: ").append(comparator).append("\n");
        string.append("bitmask: ").append(bitmask);
        return string.toString();
    }

    /** Set the mime type for this magic match. */
    public void setMimeType(String value) {
        mimeType = value;
    }

    /** Get the magic match for this magic match. */
    public String getMimeType() {
        return mimeType;
    }

    /** Set the extension for this magic match. */
    public void setExtension(String value) {
        extension = value;
    }

    /**
     * Get the extension for this magic match.
     * @return the extension for this magic match
     */
    public String getExtension() {
        return extension;
    }

    /** Set the description for this magic match. */
    public void setDescription(String value) {
        description = value;
    }

    /**
     * Get the description for this magic match.
     * @return the description for thie magic match
     */
    public String getDescription() {
        return description;
    }

    /**
     * set the test value for thie magic match
     *
     * @param value DOCUMENT ME!
     */
    public void setTest(ByteBuffer value)
    {
        test = value;
    }

    /**
     * get the test value for this magic match
     *
     * @return DOCUMENT ME!
     */
    public ByteBuffer getTest()
    {
        return test;
    }

    /**
     * set the offset in the stream we are comparing to the test value for this magic match
     *
     * @param value DOCUMENT ME!
     */
    public void setOffset(int value)
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
        return offset;
    }

    /**
     * set the length we are restricting the comparison to for this magic match
     *
     * @param value DOCUMENT ME!
     */
    public void setLength(int value)
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
        return length;
    }

    /**
     * set the type of match to perform for this magic match
     *
     * @param value DOCUMENT ME!
     */
    public void setType(String value)
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
        return type;
    }

    /**
     * set the bitmask that will be applied for this magic match
     *
     * @param value DOCUMENT ME!
     */
    public void setBitmask(String value)
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
        return bitmask;
    }

    /**
     * set the comparator for this magic match
     *
     * @param value DOCUMENT ME!
     */
    public void setComparator(String value)
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
        return comparator;
    }

    /**
     * set the properties for this magic match
     *
     * @param properties DOCUMENT ME!
     */
    public void setProperties(Map properties)
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
        return properties;
    }

    /**
     * Add a submatch to this magic match.
     * @param m a magic match
     */
    public void addSubMatch(MagicMatch m) {
        subMatches.add(m);
    }

    /**
     * Set all submatches.
     * @param a a collection of submatches
     */
    public void setSubMatches(Collection a) {
        subMatches.clear();
        subMatches.addAll(a);
    }

    /**
     * get all submatches for this magic match
     *
     * @return a collection of submatches
     */
    public Collection getSubMatches()
    {
        return subMatches;
    }

    /**
     * determine if this match or any submatches has the description
     *
     * @param desc DOCUMENT ME!
     *
     * @return whether or not the description matches
     */
    public boolean descriptionMatches(String desc)
    {
        if ((description != null) && description.equals(desc)) {
            return true;
        }

        Collection submatches = getSubMatches();
        Iterator i = submatches.iterator();
        MagicMatch m = null;

        while (i.hasNext()) {
            m = (MagicMatch) i.next();

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
    public boolean mimeTypeMatches(String desc)
    {
        if ((mimeType != null) && mimeType.equals(desc)) {
            return true;
        }

        Collection submatches = getSubMatches();
        Iterator i = submatches.iterator();
        MagicMatch m = null;

        while (i.hasNext()) {
            m = (MagicMatch) i.next();

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
    protected Object clone()
        throws CloneNotSupportedException
    {
        MagicMatch clone = new MagicMatch();
        clone.setBitmask(Long.toString(bitmask, 8));
        clone.setComparator("" + comparator);
        clone.setDescription(description);
        clone.setExtension(extension);
        clone.setLength(length);
        clone.setMimeType(mimeType);
        clone.setOffset(offset);

        // these properties should only be String types, so we shouldn't have to clone them
        HashMap m = new HashMap();
        m.putAll(properties);
        clone.setProperties(m);

        Iterator i = subMatches.iterator();
        ArrayList a = new ArrayList();

        while (i.hasNext()) {
            MagicMatch mm = (MagicMatch) i.next();
            a.add(mm);
        }

        clone.setSubMatches(a);

        clone.setTest(test);
        clone.setType(type);

        // TODO Auto-generated method stub
        return clone;
    }
}
