/*
 * Copyright (c) 1997, 2012, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package es.gob.afirma.standalone.configurator.jre.security.x509;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.security.cert.CertificateException;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Map;
import java.util.TreeMap;

import es.gob.afirma.standalone.configurator.jre.misc.HexDumpEncoder;
import es.gob.afirma.standalone.configurator.jre.security.util.Debug;
import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;
import es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier;

/**
 * This class defines the Extensions attribute for the Certificate.
 *
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 * @see CertAttrSet
 */
public class CertificateExtensions implements CertAttrSet<Extension> {
    /**
     * Identifier for this attribute, to be used with the
     * get, set, delete methods of Certificate, x509 type.
     */
    public static final String IDENT = "x509.info.extensions"; //$NON-NLS-1$
    /**
     * name
     */
    public static final String NAME = "extensions"; //$NON-NLS-1$

    private static final Debug debug = Debug.getInstance("x509"); //$NON-NLS-1$

    private final Map<String,Extension> map = Collections.synchronizedMap(
            new TreeMap<String,Extension>());
    private boolean unsupportedCritExt = false;

    private Map<String,Extension> unparseableExtensions;

    /**
     * Default constructor.
     */
    public CertificateExtensions() { }

    /**
     * Create the object, decoding the values from the passed DER stream.
     *
     * @param in the DerInputStream to read the Extension from.
     * @exception IOException on decoding errors.
     */
    public CertificateExtensions(final DerInputStream in) throws IOException {
        init(in);
    }

    // helper routine
    private void init(final DerInputStream in) throws IOException {

        final DerValue[] exts = in.getSequence(5);

        for (final DerValue ext2 : exts) {
            final Extension ext = new Extension(ext2);
            parseExtension(ext);
        }
    }

    private static Class[] PARAMS = {Boolean.class, Object.class};

    // Parse the encoded extension
    private void parseExtension(final Extension ext) throws IOException {
        try {
            final Class<?> extClass = OIDMap.getClass(ext.getExtensionId());
            if (extClass == null) {   // Unsupported extension
                if (ext.isCritical()) {
                    this.unsupportedCritExt = true;
                }
                if (this.map.put(ext.getExtensionId().toString(), ext) == null) {
                    return;
                } else {
                    throw new IOException("Duplicate extensions not allowed"); //$NON-NLS-1$
                }
            }
            final Constructor<?> cons = extClass.getConstructor(PARAMS);

            final Object[] passed = new Object[] {Boolean.valueOf(ext.isCritical()),
                    ext.getExtensionValue()};
                    final CertAttrSet<?> certExt = (CertAttrSet<?>)
                            cons.newInstance(passed);
                    if (this.map.put(certExt.getName(), (Extension)certExt) != null) {
                        throw new IOException("Duplicate extensions not allowed"); //$NON-NLS-1$
                    }
        } catch (final InvocationTargetException invk) {
            final Throwable e = invk.getTargetException();
            if (ext.isCritical() == false) {
                // ignore errors parsing non-critical extensions
                if (this.unparseableExtensions == null) {
                    this.unparseableExtensions = new TreeMap<String,Extension>();
                }
                this.unparseableExtensions.put(ext.getExtensionId().toString(),
                        new UnparseableExtension(ext, e));
                if (debug != null) {
                    debug.println("Error parsing extension: " + ext); //$NON-NLS-1$
                    e.printStackTrace();
                    final HexDumpEncoder h = new HexDumpEncoder();
                    System.err.println(h.encodeBuffer(ext.getExtensionValue()));
                }
                return;
            }
            if (e instanceof IOException) {
                throw (IOException)e;
            } else {
                throw new IOException(e);
            }
        } catch (final IOException e) {
            throw e;
        } catch (final Exception e) {
            throw new IOException(e);
        }
    }

    /**
     * Encode the extensions in DER form to the stream, setting
     * the context specific tag as needed in the X.509 v3 certificate.
     *
     * @param out the DerOutputStream to marshal the contents to.
     * @exception CertificateException on encoding errors.
     * @exception IOException on errors.
     */
    @Override
	public void encode(final OutputStream out)
    throws CertificateException, IOException {
        encode(out, false);
    }

    /**
     * Encode the extensions in DER form to the stream.
     *
     * @param out the DerOutputStream to marshal the contents to.
     * @param isCertReq if true then no context specific tag is added.
     * @exception CertificateException on encoding errors.
     * @exception IOException on errors.
     */
    public void encode(final OutputStream out, final boolean isCertReq)
    throws CertificateException, IOException {
        final DerOutputStream extOut = new DerOutputStream();
        final Collection<Extension> allExts = this.map.values();
        final Object[] objs = allExts.toArray();

        for (final Object obj : objs) {
            if (obj instanceof CertAttrSet) {
				((CertAttrSet)obj).encode(extOut);
			} else if (obj instanceof Extension) {
				((Extension)obj).encode(extOut);
			} else {
				throw new CertificateException("Illegal extension object"); //$NON-NLS-1$
			}
        }

        final DerOutputStream seq = new DerOutputStream();
        seq.write(DerValue.tag_Sequence, extOut);

        DerOutputStream tmp;
        if (!isCertReq) { // certificate
            tmp = new DerOutputStream();
            tmp.write(DerValue.createTag(DerValue.TAG_CONTEXT, true, (byte)3),
                    seq);
        }
		else {
			tmp = seq; // pkcs#10 certificateRequest
		}

        out.write(tmp.toByteArray());
    }

    /**
     * Set the attribute value.
     * @param name the extension name used in the cache.
     * @param obj the object to set.
     * @exception IOException if the object could not be cached.
     */
    @Override
	public void set(final String name, final Object obj) throws IOException {
        if (obj instanceof Extension) {
            this.map.put(name, (Extension)obj);
        } else {
            throw new IOException("Unknown extension type."); //$NON-NLS-1$
        }
    }

    /**
     * Get the attribute value.
     * @param name the extension name used in the lookup.
     * @exception IOException if named extension is not found.
     */
    @Override
	public Extension get(final String name) throws IOException {
        final Extension obj = this.map.get(name);
        if (obj == null) {
            throw new IOException("No extension found with name " + name); //$NON-NLS-1$
        }
        return (obj);
    }

    // Similar to get(String), but throw no exception, might return null.
    // Used in X509CertImpl::getExtension(OID).
    Extension getExtension(final String name) {
        return this.map.get(name);
    }

    /**
     * Delete the attribute value.
     * @param name the extension name used in the lookup.
     * @exception IOException if named extension is not found.
     */
    @Override
	public void delete(final String name) throws IOException {
        final Object obj = this.map.get(name);
        if (obj == null) {
            throw new IOException("No extension found with name " + name); //$NON-NLS-1$
        }
        this.map.remove(name);
    }

    public String getNameByOid(final ObjectIdentifier oid) throws IOException {
        for (final String name: this.map.keySet()) {
            if (this.map.get(name).getExtensionId().equals(oid)) {
                return name;
            }
        }
        return null;
    }

    /**
     * Return an enumeration of names of attributes existing within this
     * attribute.
     */
    @Override
	public Enumeration<Extension> getElements() {
        return Collections.enumeration(this.map.values());
    }

    /**
     * Return a collection view of the extensions.
     * @return a collection view of the extensions in this Certificate.
     */
    public Collection<Extension> getAllExtensions() {
        return this.map.values();
    }

    public Map<String,Extension> getUnparseableExtensions() {
        if (this.unparseableExtensions == null) {
            return Collections.emptyMap();
        } else {
            return this.unparseableExtensions;
        }
    }

    /**
     * Return the name of this attribute.
     */
    @Override
	public String getName() {
        return NAME;
    }

    /**
     * Return true if a critical extension is found that is
     * not supported, otherwise return false.
     */
    public boolean hasUnsupportedCriticalExtension() {
        return this.unsupportedCritExt;
    }

    /**
     * Compares this CertificateExtensions for equality with the specified
     * object. If the <code>other</code> object is an
     * <code>instanceof</code> <code>CertificateExtensions</code>, then
     * all the entries are compared with the entries from this.
     *
     * @param other the object to test for equality with this
     * CertificateExtensions.
     * @return true iff all the entries match that of the Other,
     * false otherwise.
     */
    @Override
	public boolean equals(final Object other) {
        if (this == other) {
			return true;
		}
        if (!(other instanceof CertificateExtensions)) {
			return false;
		}
        final Collection<Extension> otherC =
                ((CertificateExtensions)other).getAllExtensions();
        final Object[] objs = otherC.toArray();

        final int len = objs.length;
        if (len != this.map.size()) {
			return false;
		}

        Extension otherExt, thisExt;
        String key = null;
        for (int i = 0; i < len; i++) {
            if (objs[i] instanceof CertAttrSet) {
				key = ((CertAttrSet)objs[i]).getName();
			}
            otherExt = (Extension)objs[i];
            if (key == null) {
				key = otherExt.getExtensionId().toString();
			}
            thisExt = this.map.get(key);
            if (thisExt == null) {
				return false;
			}
            if (! thisExt.equals(otherExt)) {
				return false;
			}
        }
        return this.getUnparseableExtensions().equals(
                ((CertificateExtensions)other).getUnparseableExtensions());
    }

    /**
     * Returns a hashcode value for this CertificateExtensions.
     *
     * @return the hashcode value.
     */
    @Override
	public int hashCode() {
        return this.map.hashCode() + getUnparseableExtensions().hashCode();
    }

    /**
     * Returns a string representation of this <tt>CertificateExtensions</tt>
     * object in the form of a set of entries, enclosed in braces and separated
     * by the ASCII characters "<tt>,&nbsp;</tt>" (comma and space).
     * <p>Overrides to <tt>toString</tt> method of <tt>Object</tt>.
     *
     * @return  a string representation of this CertificateExtensions.
     */
    @Override
	public String toString() {
        return this.map.toString();
    }

}

class UnparseableExtension extends Extension {
    private String name;
    private final Throwable why;

    public UnparseableExtension(final Extension ext, final Throwable why) {
        super(ext);

        this.name = ""; //$NON-NLS-1$
        try {
            final Class<?> extClass = OIDMap.getClass(ext.getExtensionId());
            if (extClass != null) {
                final Field field = extClass.getDeclaredField("NAME"); //$NON-NLS-1$
                this.name = (String)(field.get(null)) + " "; //$NON-NLS-1$
            }
        } catch (final Exception e) {
            // If we cannot find the name, just ignore it
        }

        this.why = why;
    }

    @Override public String toString() {
        return super.toString() +
                "Unparseable " + this.name + "extension due to\n" + this.why + "\n\n" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                new HexDumpEncoder().encodeBuffer(getExtensionValue());
    }
}
