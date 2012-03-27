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
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.Serializable;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import org.apache.oro.text.perl.Perl5Util;


/**
 * This class represents a single match test
 *
 * @author $Author: arimus $
 * @version $Revision: 1.1 $
 */
@SuppressWarnings("javadoc")
public class MagicMatcher implements Cloneable, Serializable {

	private static final long serialVersionUID = -1109707428218614961L;

	private final ArrayList<MagicMatcher> subMatchers = new ArrayList<MagicMatcher>(0);
    private MagicMatch match = null;


    public void setMatch(final MagicMatch match) {
        this.match = match;
    }

    public MagicMatch getMatch() {
        return this.match;
    }

    /**
     * test to see if everything is in order for this match
     *
     * @return whether or not this match has enough data to be valid
     */
    public boolean isValid() {

        if ((this.match == null) || (this.match.getTest() == null)) {
            return false;
        }

        final String type = new String(this.match.getTest().array());
        final char comparator = this.match.getComparator();
        final String description = this.match.getDescription();
        final String test = new String(this.match.getTest().array());

        if (!type.equals("") && (comparator != '\0') && //$NON-NLS-1$
                ((comparator == '=') || (comparator == '!') || (comparator == '>') ||
                (comparator == '<')) && (description != null) && !description.equals("") && //$NON-NLS-1$
                !test.equals("")) { //$NON-NLS-1$
            return true;
        }

        return false;
    }

    /**
     * add a submatch to this magic match
     *
     * @param m a magic match
     */
    public void addSubMatcher(final MagicMatcher m) {
        this.subMatchers.add(m);
    }

    /**
     * set all submatches
     *
     * @param a a collection of submatches
     */
    public void setSubMatchers(final Collection<MagicMatcher> a)
    {
        this.subMatchers.clear();
        this.subMatchers.addAll(a);
    }

    /**
     * get all submatches for this magic match
     *
     * @return a collection of submatches
     */
    public Collection<MagicMatcher> getSubMatchers() {
        return this.subMatchers;
    }

    /**
     * test to see if this match or any submatches match
     *
     * @param f the file that should be used to test the match
     * @param onlyMimeMatch DOCUMENT ME!
     *
     * @return the deepest magic match object that matched
     *
     * @throws IOException DOCUMENT ME!
     * @throws UnsupportedTypeException DOCUMENT ME!
     */
    public MagicMatch test(final File f, final boolean onlyMimeMatch) throws IOException, UnsupportedTypeException {

        final int offset = this.match.getOffset();
        this.match.getDescription();
        final String type = this.match.getType();
        this.match.getMimeType();

        RandomAccessFile file = null;
        file = new RandomAccessFile(f, "r"); //$NON-NLS-1$

        try {
            int length = 0;

            if (type.equals("byte")) { //$NON-NLS-1$
                length = 1;
            } else if (type.equals("short") || type.equals("leshort") || type.equals("beshort")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                length = 4;
            } else if (type.equals("long") || type.equals("lelong") || type.equals("belong")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                length = 8;
            } else if (type.equals("string")) { //$NON-NLS-1$
                length = this.match.getTest().capacity();
            } else if (type.equals("regex")) { //$NON-NLS-1$
                length = (int) file.length() - offset;

                if (length < 0) {
                    length = 0;
                }
            } else if (type.equals("detector")) { //$NON-NLS-1$
                length = (int) file.length() - offset;

                if (length < 0) {
                    length = 0;
                }
            } else {
                throw new UnsupportedTypeException("unsupported test type '" + type + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            }

            // we know this match won't work since there isn't enough data for the test
            if (length > (file.length() - offset)) {
                return null;
            }

            final byte[] buf = new byte[length];
            file.seek(offset);

            int bytesRead = 0;
            int size = 0;
            boolean done = false;

            while (!done) {
                size = file.read(buf, 0, length - bytesRead);

                if (size == -1) {
                    throw new IOException("reached end of file before all bytes were read"); //$NON-NLS-1$
                }

                bytesRead += size;

                if (bytesRead == length) {
                    done = true;
                }
            }

            MagicMatch match1 = null;
            MagicMatch submatch = null;

            if (testInternal(buf)) {
                // set the top level match to this one
                match1 = getMatch();

                // set the data on this match
                if ((onlyMimeMatch == false) && (this.subMatchers != null) && (this.subMatchers.size() > 0)) {

                    for (int i = 0; i < this.subMatchers.size(); i++) {

                        final MagicMatcher m = this.subMatchers.get(i);

                        if ((submatch = m.test(f, false)) != null) {
                            match1.addSubMatch(submatch);
                        }
                    }
                }
            }

            return match1;
        }
        finally {
            try {
                file.close();
            }
            catch (final Exception fce) {
            	// Ignorada
            }
        }
    }

    /**
     * test to see if this match or any submatches match
     *
     * @param data the data that should be used to test the match
     * @param onlyMimeMatch DOCUMENT ME!
     *
     * @return the deepest magic match object that matched
     *
     * @throws IOException DOCUMENT ME!
     * @throws UnsupportedTypeException DOCUMENT ME!
     */
    public MagicMatch test(final byte[] data, final boolean onlyMimeMatch)
        throws IOException, UnsupportedTypeException
    {

        final int offset = this.match.getOffset();
        this.match.getDescription();
        final String type = this.match.getType();
        this.match.getMimeType();

        int length = 0;

        if (type.equals("byte")) { //$NON-NLS-1$
            length = 1;
        }
        else if (type.equals("short") || type.equals("leshort") || type.equals("beshort")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            length = 4;
        }
        else if (type.equals("long") || type.equals("lelong") || type.equals("belong")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            length = 8;
        }
        else if (type.equals("string")) { //$NON-NLS-1$
            length = this.match.getTest().capacity();
        }
        else if (type.equals("regex")) { //$NON-NLS-1$
            // FIXME - something wrong here, shouldn't have to subtract 1???
            length = data.length - offset - 1;

            if (length < 0) {
                length = 0;
            }
        } else if (type.equals("detector")) { //$NON-NLS-1$
            // FIXME - something wrong here, shouldn't have to subtract 1???
            length = data.length - offset - 1;

            if (length < 0) {
                length = 0;
            }
        } else {
            throw new UnsupportedTypeException("unsupported test type " + type); //$NON-NLS-1$
        }

        final byte[] buf = new byte[length];

        if ((offset + length) < data.length) {
            System.arraycopy(data, offset, buf, 0, length);

            MagicMatch match1 = null;
            MagicMatch submatch = null;

            if (testInternal(buf)) {
                // set the top level match to this one
                match1 = getMatch();

                // set the data on this match
                if ((onlyMimeMatch == false) && (this.subMatchers != null) && (this.subMatchers.size() > 0)) {

                    for (int i = 0; i < this.subMatchers.size(); i++) {

                        final MagicMatcher m = this.subMatchers.get(i);

                        if ((submatch = m.test(data, false)) != null) {
                            match1.addSubMatch(submatch);
                        }
                    }
                }
            }

            return match1;
        }
        return null;
    }

    /**
     * internal test switch
     *
     * @param data DOCUMENT ME!
     * @return DOCUMENT ME!
     */
    private boolean testInternal(final byte[] data)
    {

        if (data.length == 0) {
            return false;
        }

        final String type = this.match.getType();
        final String test = new String(this.match.getTest().array());
        this.match.getMimeType();
        this.match.getDescription();

        ByteBuffer buffer = ByteBuffer.allocate(data.length);

        if ((type != null) && (test.length() > 0)) {
            if (type.equals("string")) { //$NON-NLS-1$
                buffer = buffer.put(data);

                return testString(buffer);
            } else if (type.equals("byte")) { //$NON-NLS-1$
                buffer = buffer.put(data);

                return testByte(buffer);
            } else if (type.equals("short")) { //$NON-NLS-1$
                buffer = buffer.put(data);

                return testShort(buffer);
            } else if (type.equals("leshort")) { //$NON-NLS-1$
                buffer = buffer.put(data);
                buffer.order(ByteOrder.LITTLE_ENDIAN);

                return testShort(buffer);
            } else if (type.equals("beshort")) { //$NON-NLS-1$
                buffer = buffer.put(data);
                buffer.order(ByteOrder.BIG_ENDIAN);

                return testShort(buffer);
            } else if (type.equals("long")) { //$NON-NLS-1$
                buffer = buffer.put(data);

                return testLong(buffer);
            } else if (type.equals("lelong")) { //$NON-NLS-1$
                buffer = buffer.put(data);
                buffer.order(ByteOrder.LITTLE_ENDIAN);

                return testLong(buffer);
            } else if (type.equals("belong")) { //$NON-NLS-1$
                buffer = buffer.put(data);
                buffer.order(ByteOrder.BIG_ENDIAN);

                return testLong(buffer);
            } else if (type.equals("regex")) { //$NON-NLS-1$
                return testRegex(new String(data));
            } else if (type.equals("detector")) { //$NON-NLS-1$
                buffer = buffer.put(data);

                return testDetector(buffer);

                //			} else if (type.equals("date")) {
                //				return testDate(data, BIG_ENDIAN);
                //			} else if (type.equals("ledate")) {
                //				return testDate(data, LITTLE_ENDIAN);
                //			} else if (type.equals("bedate")) {
                //				return testDate(data, BIG_ENDIAN);
            }
        }

        return false;
    }

    /**
     * test the data against the test byte
     *
     * @param data the data we are testing
     *
     * @return if we have a match
     */
    private boolean testByte(final ByteBuffer data)
    {
        final String test = new String(this.match.getTest().array());
        final char comparator = this.match.getComparator();
        final long bitmask = this.match.getBitmask();

        byte b = data.get(0);
        b = (byte) (b & bitmask);

        final int tst = Integer.decode(test).byteValue();
        final byte t = (byte) (tst & 0xff);

        switch (comparator) {
        case '=':
            return t == b;

        case '!':
            return t != b;

        case '>':
            return t > b;

        case '<':
            return t < b;
        }

        return false;
    }

    /**
     * test the data against the byte array
     *
     * @param data the data we are testing
     *
     * @return if we have a match
     */
    private boolean testString(final ByteBuffer data)
    {

        final ByteBuffer test = this.match.getTest();
        final char comparator = this.match.getComparator();

        final byte[] b = data.array();
        final byte[] t = test.array();

        boolean diff = false;
        int i = 0;

        for (i = 0; i < t.length; i++) {

            if (t[i] != b[i]) {
                diff = true;

                break;
            }
        }

        switch (comparator) {
        case '=':
            return !diff;

        case '!':
            return diff;

        case '>':
            return t[i] > b[i];

        case '<':
            return t[i] < b[i];
        }

        return false;
    }

    /**
     * test the data against a short
     *
     * @param data the data we are testing
     *
     * @return if we have a match
     */
    private boolean testShort(final ByteBuffer data)
    {

        short val = 0;
        final String test = new String(this.match.getTest().array());
        final char comparator = this.match.getComparator();
        final long bitmask = this.match.getBitmask();

        val = byteArrayToShort(data);

        // apply bitmask before the comparison
        val = (short) (val & (short) bitmask);

        short tst = 0;

        try {
            tst = Integer.decode(test).shortValue();
        } catch (final NumberFormatException e) {

            return false;

            //if (test.length() == 1) {
            //	tst = new Integer(Character.getNumericValue(test.charAt(0))).shortValue();
            //}
        }

        switch (comparator) {
        case '=':
            return val == tst;

        case '!':
            return val != tst;

        case '>':
            return val > tst;

        case '<':
            return val < tst;
        }

        return false;
    }

    /**
     * test the data against a long
     *
     * @param data the data we are testing
     *
     * @return if we have a match
     */
    private boolean testLong(final ByteBuffer data)
    {

        long val = 0;
        final String test = new String(this.match.getTest().array());
        final char comparator = this.match.getComparator();
        final long bitmask = this.match.getBitmask();

        val = byteArrayToLong(data);

        // apply bitmask before the comparison
        val = val & bitmask;

        final long tst = Long.decode(test).longValue();

        switch (comparator) {
        case '=':
            return val == tst;

        case '!':
            return val != tst;

        case '>':
            return val > tst;

        case '<':
            return val < tst;
        }

        return false;
    }

    /**
     * test the data against a regex
     *
     * @param text the data we are testing
     *
     * @return if we have a match
     */
    private boolean testRegex(final String text)
    {

        final String test = new String(this.match.getTest().array());
        final char comparator = this.match.getComparator();

        final Perl5Util utility = new Perl5Util();

        if (comparator == '=') {
            if (utility.match(test, text)) {
                return true;
            }
            return false;
        }
        else if (comparator == '!') {
            if (utility.match(test, text)) {
                return false;
            }
            return true;
        }

        return false;
    }

    /**
     * test the data using a detector
     *
     * @param data the data we are testing
     *
     * @return if we have a match
     */
    private boolean testDetector(final ByteBuffer data) {

        final String detectorClass = new String(this.match.getTest().array());

        try {
            final MagicDetector detector = (MagicDetector) classForName(detectorClass).newInstance();
            final String[] types = detector.process(data.array(), this.match.getOffset(), this.match.getLength(),
                    this.match.getBitmask(), this.match.getComparator(), this.match.getMimeType(),
                    this.match.getProperties());

            if ((types != null) && (types.length > 0)) {
                // the match object has no mime type set, so set from the detector class processing
                this.match.setMimeType(types[0]);

                return true;
            }
        }
        catch (final Throwable e) {
        	java.util.logging.Logger.getAnonymousLogger().warning(e.toString());
        }

        return false;
    }

    /**
     * Get the extensions for the underlying detectory
     *
     * @return DOCUMENT ME!
     */
    public String[] getDetectorExtensions() {

        final String detectorClass = new String(this.match.getTest().array());

        try {
            final MagicDetector detector = (MagicDetector) classForName(detectorClass).newInstance();
            return detector.getHandledTypes();
        }
        catch (final Exception e) {
        	Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
    			"Error cargando e instanciando la clase " + detectorClass + ": " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
        }

        return new String[0];
    }

    /**
     * convert a byte array to a short
     *
     * @param data buffer of byte data
     *
     * @return byte array converted to a short
     */
    private static short byteArrayToShort(final ByteBuffer data) {
        return data.getShort(0);
    }

    /**
     * convert a byte array to a long
     *
     * @param data buffer of byte data
     *
     * @return byte arrays (high and low bytes) converted to a long value
     */
    private static long byteArrayToLong(final ByteBuffer data)
    {
        return data.getInt(0);
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
        final MagicMatcher clone = new MagicMatcher();

        clone.setMatch((MagicMatch) this.match.clone());

        final Iterator<MagicMatcher> i = this.subMatchers.iterator();
        final ArrayList<MagicMatcher> sub = new ArrayList<MagicMatcher>();

        while (i.hasNext()) {
            final MagicMatcher m = i.next();
            sub.add((MagicMatcher) m.clone());
        }

        clone.setSubMatchers(sub);

        return clone;
    }

    /** Carga una clase excluyendo de la ruta de b&uacute;squeda de clases las URL que no correspondan con JAR.
     * @param className Nombre de la clase a cargar
     * @return Clase cargada
     * @throws ClassNotFoundException cuando no se encuentra la clase a cargar
     */
    static Class<?> classForName(final String className) throws ClassNotFoundException {
        getCleanClassLoader().loadClass(className);
        return Class.forName(className);
    }

    /** Obtiene un ClassLoader que no incluye URL que no referencien directamente a ficheros JAR.
     * @return ClassLoader sin URL adicionales a directorios sueltos Web
     */
    private static ClassLoader getCleanClassLoader() {
        ClassLoader classLoader = MagicMatcher.class.getClassLoader();
        if (classLoader instanceof URLClassLoader && !classLoader.getClass().toString().contains("sun.plugin2.applet.JNLP2ClassLoader")) { //$NON-NLS-1$
        	final List<URL> urls = new ArrayList<URL>();
        	for (final URL url : ((URLClassLoader) classLoader).getURLs()) {
        		if (url.toString().endsWith(".jar")) { //$NON-NLS-1$
        			urls.add(url);
        		}
        	}
        	classLoader = new URLClassLoader(urls.toArray(new URL[0]));
        }
        return classLoader;
    }
}
