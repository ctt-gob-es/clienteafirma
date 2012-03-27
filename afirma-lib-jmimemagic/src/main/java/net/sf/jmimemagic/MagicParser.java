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

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * DOCUMENT ME!
 *
 * @author $Author$
 * @version $Revision$
  */
public final class MagicParser extends DefaultHandler {
    private static String magicFile = "/magic.xml"; //$NON-NLS-1$

    private boolean initialized = false;
    private XMLReader parser = null;
    private final ArrayList<MagicMatcher> stack = new ArrayList<MagicMatcher>();
    private final Collection<MagicMatcher> matchers = new ArrayList<MagicMatcher>();
    private MagicMatcher matcher = null;
    private MagicMatch match = null;
    private HashMap<String, String> properties = null;
    private String finalValue = ""; //$NON-NLS-1$
    private boolean isMimeType = false;
    private boolean isExtension = false;
    private boolean isDescription = false;
    private boolean isTest = false;


    /**
     * parse the xml file and create our MagicMatcher object list
     *
     * @throws MagicParseException DOCUMENT ME!
     */
    public synchronized void initialize()
        throws MagicParseException
    {
        if (!this.initialized) {
            // use default parser
            try {
            	this.parser = (XMLReader) MagicMatcher.classForName(
            			"com.sun.org.apache.xerces.internal.parsers.SAXParser").newInstance(); //$NON-NLS-1$
            }
            catch (final Exception e) {
            	try {
            		this.parser = XMLReaderFactory.createXMLReader();
            	} catch (final Exception e2) {
            		throw new MagicParseException("unable to instantiate parser"); //$NON-NLS-1$
            	}
            }

            // set handlers
            this.parser.setErrorHandler(this);
            this.parser.setContentHandler(this);

            // parse file
            try {
                // get the magic file URL
                final String magicURL = MagicParser.class.getResource(magicFile).toString();

                if (magicURL == null) {
                    throw new MagicParseException("couldn't load '" + magicURL + "'"); //$NON-NLS-1$ //$NON-NLS-2$
                }

                this.parser.parse(magicURL);
            }
            catch (final SAXParseException e) {
                // ignore
            }
            catch (final Exception e) {
                throw new MagicParseException("parse error occurred - " + e.getMessage()); //$NON-NLS-1$
            }

            this.initialized = true;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Collection<MagicMatcher> getMatchers()
    {
        return this.matchers;
    }

    @Override
	public void startDocument() throws SAXException { /* No implementado */ }

    @Override
	public void endDocument() throws SAXException { /* No implementado */ }

    @Override
	public void processingInstruction(final String target, final String data) throws SAXException {
        // do nothing
    }

    /**
     * DOCUMENT ME!
     *
     * @param ch DOCUMENT ME!
     * @param offset DOCUMENT ME!
     * @param length DOCUMENT ME!
     *
     * @throws SAXException DOCUMENT ME!
     */
    @Override
	public void characters(final char[] ch, final int offset, final int length)
        throws SAXException
    {
        final String value = new String(ch, offset, length);
        this.finalValue += value;
    }

    /**
     * DOCUMENT ME!
     *
     * @param ch DOCUMENT ME!
     * @param offset DOCUMENT ME!
     * @param length DOCUMENT ME!
     *
     * @throws SAXException DOCUMENT ME!
     */
    @Override
	public void ignorableWhitespace(final char[] ch, final int offset, final int length)
        throws SAXException
    {
        // do nothing
    }

    /**
     * DOCUMENT ME!
     *
     * @param uri DOCUMENT ME!
     * @param localName DOCUMENT ME!
     * @param qname DOCUMENT ME!
     * @param attributes DOCUMENT ME!
     *
     * @throws SAXException DOCUMENT ME!
     */
    @Override
	public void startElement(final String uri, final String localName, final String qname, final Attributes attributes)
        throws SAXException
    {

        // create a new matcher
        if (localName.equals("match")) { //$NON-NLS-1$
            // match to hold data
            this.match = new MagicMatch();
            // our matcher
            this.matcher = new MagicMatcher();
            this.matcher.setMatch(this.match);
        }

        // these are subelements of matcher, but also occur elsewhere
        if (this.matcher != null) {
            if (localName.equals("mimetype")) { //$NON-NLS-1$
                this.isMimeType = true;
            }
            else if (localName.equals("extension")) { //$NON-NLS-1$
                this.isExtension = true;
            }
            else if (localName.equals("description")) { //$NON-NLS-1$
                this.isDescription = true;
            }
            else if (localName.equals("test")) { //$NON-NLS-1$
                this.isTest = true;

                final int length = attributes.getLength();

                for (int i = 0; i < length; i++) {
                    final String attrLocalName = attributes.getLocalName(i);
                    final String attrValue = attributes.getValue(i);

                    if (attrLocalName.equals("offset")) { //$NON-NLS-1$
                        if (!attrValue.equals("")) { //$NON-NLS-1$
                            this.match.setOffset(new Integer(attrValue).intValue());
                        }
                    }
                    else if (attrLocalName.equals("length")) { //$NON-NLS-1$
                        if (!attrValue.equals("")) { //$NON-NLS-1$
                            this.match.setLength(new Integer(attrValue).intValue());
                        }
                    }
                    else if (attrLocalName.equals("type")) { //$NON-NLS-1$
                        this.match.setType(attrValue);
                    }
                    else if (attrLocalName.equals("bitmask")) { //$NON-NLS-1$
                        if (!attrValue.equals("")) { //$NON-NLS-1$
                            this.match.setBitmask(attrValue);
                        }
                    }
                    else if (attrLocalName.equals("comparator")) { //$NON-NLS-1$
                        this.match.setComparator(attrValue);
                    }
                }
            }
            else if (localName.equals("property")) { //$NON-NLS-1$
                final int length = attributes.getLength();
                String name = null;
                String value = null;

                for (int i = 0; i < length; i++) {
                    final String attrLocalName = attributes.getLocalName(i);
                    final String attrValue = attributes.getValue(i);

                    if (attrLocalName.equals("name")) { //$NON-NLS-1$
                        if (!attrValue.equals("")) { //$NON-NLS-1$
                            name = attrValue;
                        }
                    } else if (attrLocalName.equals("value")) { //$NON-NLS-1$
                        if (!attrValue.equals("")) { //$NON-NLS-1$
                            value = attrValue;
                        }
                    }
                }

                // save the property to our map
                if ((name != null) && (value != null)) {
                    if (this.properties == null) {
                        this.properties = new HashMap<String, String>();
                    }

                    if (!this.properties.containsKey(name)) {
                        this.properties.put(name, value);
                    }
                }
            }
            else if (localName.equals("match-list")) { //$NON-NLS-1$
                // this means we are processing a child match, so we need to push
                // the existing match on the stack
                this.stack.add(this.matcher);
            }
            else {
                // we don't care about this type
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param uri DOCUMENT ME!
     * @param localName DOCUMENT ME!
     * @param qname DOCUMENT ME!
     *
     * @throws SAXException DOCUMENT ME!
     */
    @Override
	public void endElement(final String uri, final String localName, final String qname) throws SAXException {

        // determine which tag these chars are for and save them
        if (this.isMimeType) {
            this.isMimeType = false;
            this.match.setMimeType(this.finalValue);
        }
        else if (this.isExtension) {
            this.isExtension = false;
            this.match.setExtension(this.finalValue);
        }
        else if (this.isDescription) {
            this.isDescription = false;
            this.match.setDescription(this.finalValue);
        }
        else if (this.isTest) {
            this.isTest = false;
            this.match.setTest(convertOctals(this.finalValue));
        }
        else {
            // do nothing
        }

        this.finalValue = ""; //$NON-NLS-1$

        // need to save the current matcher here if it is filled out enough and
        // we have an /matcher
        if (localName.equals("match")) { //$NON-NLS-1$
            // FIXME - make sure the MagicMatcher isValid() test works
            if (this.matcher.isValid()) {
                // set the collected properties on this matcher
                this.match.setProperties(this.properties);

                // add root match
                if (this.stack.size() == 0) {
                    this.matchers.add(this.matcher);
                }
                else {
                    // we need to add the match to it's parent which is on the
                    // stack
                    final MagicMatcher m = this.stack.get(this.stack.size() - 1);
                    m.addSubMatcher(this.matcher);
                }
            }

            this.matcher = null;
            this.properties = null;

            // restore matcher from the stack if we have an /matcher-list
        }
        else if (localName.equals("match-list")) { //$NON-NLS-1$
            if (this.stack.size() > 0) {
                this.matcher = this.stack.get(this.stack.size() - 1);
                // pop from the stack
                this.stack.remove(this.matcher);
            }
        }
        else if (localName.equals("mimetype")) { //$NON-NLS-1$
            this.isMimeType = false;
        }
        else if (localName.equals("extension")) { //$NON-NLS-1$
            this.isExtension = false;
        }
        else if (localName.equals("description")) { //$NON-NLS-1$
            this.isDescription = false;
        }
        else if (localName.equals("test")) { //$NON-NLS-1$
            this.isTest = false;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param ex DOCUMENT ME!
     *
     * @throws SAXException DOCUMENT ME!
     */
    @Override
	public void warning(final SAXParseException ex)
        throws SAXException
    {
        // FIXME
    }

    /**
     * DOCUMENT ME!
     *
     * @param ex DOCUMENT ME!
     *
     * @throws SAXException DOCUMENT ME!
     */
    @Override
	public void error(final SAXParseException ex) throws SAXException {
        // FIXME
        throw ex;
    }

    /**
     * DOCUMENT ME!
     *
     * @param ex DOCUMENT ME!
     *
     * @throws SAXException DOCUMENT ME!
     */
    @Override
	public void fatalError(final SAXParseException ex)
        throws SAXException
    {
        // FIXME
        throw ex;
    }

    /**
     * replaces octal representations of bytes, written as \ddd to actual byte values.
     *
     * @param s a string with encoded octals
     *
     * @return string with all octals decoded
     */
    private static ByteBuffer convertOctals(final String s)
    {
        int beg = 0;
        int end = 0;
        int chr;
        final ByteArrayOutputStream buf = new ByteArrayOutputStream();

        while ((end = s.indexOf('\\', beg)) != -1) {
            if (s.charAt(end + 1) != '\\') {

                for (int z = beg; z < end; z++) {
                    buf.write(s.charAt(z));
                }

                if ((end + 4) <= s.length()) {
                    try {
                        chr = Integer.parseInt(s.substring(end + 1, end + 4), 8);

                        buf.write(chr);
                        beg = end + 4;
                        end = beg;
                    }
                    catch (final NumberFormatException nfe) {
                        buf.write('\\');
                        beg = end + 1;
                        end = beg;
                    }
                }
                else {
                    buf.write('\\');
                    beg = end + 1;
                    end = beg;
                }
            }
            else {
                buf.write('\\');
                beg = end + 1;
                end = beg;
            }
        }

        if (end < s.length()) {
            for (int z = beg; z < s.length(); z++) {
                buf.write(s.charAt(z));
            }
        }

        try {
            final ByteBuffer b = ByteBuffer.allocate(buf.size());
            return b.put(buf.toByteArray());
        }
        catch (final Exception e) {
            return ByteBuffer.allocate(0);
        }
    }
}
