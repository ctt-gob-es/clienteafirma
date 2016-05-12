/*
 * Copyright (c) 2000, 2011, Oracle and/or its affiliates. All rights reserved.
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
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;
import es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier;

/**
 * This class defines the Extended Key Usage Extension, which
 * indicates one or more purposes for which the certified public key
 * may be used, in addition to or in place of the basic purposes
 * indicated in the key usage extension field.  This field is defined
 * as follows:<p>
 *
 * id-ce-extKeyUsage OBJECT IDENTIFIER ::= {id-ce 37}<p>
 *
 * ExtKeyUsageSyntax ::= SEQUENCE SIZE (1..MAX) OF KeyPurposeId<p>
 *
 * KeyPurposeId ::= OBJECT IDENTIFIER<p>
 *
 * Key purposes may be defined by any organization with a need. Object
 * identifiers used to identify key purposes shall be assigned in
 * accordance with IANA or ITU-T Rec. X.660 | ISO/IEC/ITU 9834-1.<p>
 *
 * This extension may, at the option of the certificate issuer, be
 * either critical or non-critical.<p>
 *
 * If the extension is flagged critical, then the certificate MUST be
 * used only for one of the purposes indicated.<p>
 *
 * If the extension is flagged non-critical, then it indicates the
 * intended purpose or purposes of the key, and may be used in finding
 * the correct key/certificate of an entity that has multiple
 * keys/certificates. It is an advisory field and does not imply that
 * usage of the key is restricted by the certification authority to
 * the purpose indicated. Certificate using applications may
 * nevertheless require that a particular purpose be indicated in
 * order for the certificate to be acceptable to that application.<p>

 * If a certificate contains both a critical key usage field and a
 * critical extended key usage field, then both fields MUST be
 * processed independently and the certificate MUST only be used for a
 * purpose consistent with both fields.  If there is no purpose
 * consistent with both fields, then the certificate MUST NOT be used
 * for any purpose.<p>
 *
 * @since       1.4
 */
public class ExtendedKeyUsageExtension extends Extension
implements CertAttrSet<String> {

    /**
     * Identifier for this attribute, to be used with the
     * get, set, delete methods of Certificate, x509 type.
     */
    public static final String IDENT = "x509.info.extensions.ExtendedKeyUsage";

    /**
     * Attribute names.
     */
    public static final String NAME = "ExtendedKeyUsage";
    public static final String USAGES = "usages";

    // OID defined in RFC 3280 Sections 4.2.1.13
    // more from http://www.alvestrand.no/objectid/1.3.6.1.5.5.7.3.html
    private static final Map <ObjectIdentifier, String> map =
            new HashMap <ObjectIdentifier, String> ();

    private static final int[] anyExtendedKeyUsageOidData = {2, 5, 29, 37, 0};
    private static final int[] serverAuthOidData = {1, 3, 6, 1, 5, 5, 7, 3, 1};
    private static final int[] clientAuthOidData = {1, 3, 6, 1, 5, 5, 7, 3, 2};
    private static final int[] codeSigningOidData = {1, 3, 6, 1, 5, 5, 7, 3, 3};
    private static final int[] emailProtectionOidData = {1, 3, 6, 1, 5, 5, 7, 3, 4};
    private static final int[] ipsecEndSystemOidData = {1, 3, 6, 1, 5, 5, 7, 3, 5};
    private static final int[] ipsecTunnelOidData = {1, 3, 6, 1, 5, 5, 7, 3, 6};
    private static final int[] ipsecUserOidData = {1, 3, 6, 1, 5, 5, 7, 3, 7};
    private static final int[] timeStampingOidData = {1, 3, 6, 1, 5, 5, 7, 3, 8};
    private static final int[] OCSPSigningOidData = {1, 3, 6, 1, 5, 5, 7, 3, 9};

    static {
        map.put(ObjectIdentifier.newInternal(anyExtendedKeyUsageOidData), "anyExtendedKeyUsage");
        map.put(ObjectIdentifier.newInternal(serverAuthOidData), "serverAuth");
        map.put(ObjectIdentifier.newInternal(clientAuthOidData), "clientAuth");
        map.put(ObjectIdentifier.newInternal(codeSigningOidData), "codeSigning");
        map.put(ObjectIdentifier.newInternal(emailProtectionOidData), "emailProtection");
        map.put(ObjectIdentifier.newInternal(ipsecEndSystemOidData), "ipsecEndSystem");
        map.put(ObjectIdentifier.newInternal(ipsecTunnelOidData), "ipsecTunnel");
        map.put(ObjectIdentifier.newInternal(ipsecUserOidData), "ipsecUser");
        map.put(ObjectIdentifier.newInternal(timeStampingOidData), "timeStamping");
        map.put(ObjectIdentifier.newInternal(OCSPSigningOidData), "OCSPSigning");
    };

    /**
     * Vector of KeyUsages for this object.
     */
    private Vector<ObjectIdentifier> keyUsages;

    // Encode this extension value.
    private void encodeThis() throws IOException {
        if (this.keyUsages == null || this.keyUsages.isEmpty()) {
            this.extensionValue = null;
            return;
        }
        final DerOutputStream os = new DerOutputStream();
        final DerOutputStream tmp = new DerOutputStream();

        for (int i = 0; i < this.keyUsages.size(); i++) {
            tmp.putOID(this.keyUsages.elementAt(i));
        }

        os.write(DerValue.tag_Sequence, tmp);
        this.extensionValue = os.toByteArray();
    }

    /**
     * Create a ExtendedKeyUsageExtension object from
     * a Vector of Key Usages; the criticality is set to false.
     *
     * @param keyUsages the Vector of KeyUsages (ObjectIdentifiers)
     */
    public ExtendedKeyUsageExtension(final Vector<ObjectIdentifier> keyUsages)
    throws IOException {
        this(Boolean.FALSE, keyUsages);
    }

    /**
     * Create a ExtendedKeyUsageExtension object from
     * a Vector of KeyUsages with specified criticality.
     *
     * @param critical true if the extension is to be treated as critical.
     * @param keyUsages the Vector of KeyUsages (ObjectIdentifiers)
     */
    public ExtendedKeyUsageExtension(final Boolean critical, final Vector<ObjectIdentifier> keyUsages)
    throws IOException {
        this.keyUsages = keyUsages;
        this.extensionId = PKIXExtensions.ExtendedKeyUsage_Id;
        this.critical = critical.booleanValue();
        encodeThis();
    }

    /**
     * Create the extension from its DER encoded value and criticality.
     *
     * @param critical true if the extension is to be treated as critical.
     * @param value an array of DER encoded bytes of the actual value.
     * @exception ClassCastException if value is not an array of bytes
     * @exception IOException on error.
     */
    public ExtendedKeyUsageExtension(final Boolean critical, final Object value)
    throws IOException {
        this.extensionId = PKIXExtensions.ExtendedKeyUsage_Id;
        this.critical = critical.booleanValue();
        this.extensionValue = (byte[]) value;
        final DerValue val = new DerValue(this.extensionValue);
        if (val.tag != DerValue.tag_Sequence) {
            throw new IOException("Invalid encoding for " +
                                   "ExtendedKeyUsageExtension.");
        }
        this.keyUsages = new Vector<ObjectIdentifier>();
        while (val.data.available() != 0) {
            final DerValue seq = val.data.getDerValue();
            final ObjectIdentifier usage = seq.getOID();
            this.keyUsages.addElement(usage);
        }
    }

    /**
     * Return the extension as user readable string.
     */
    @Override
	public String toString() {
        if (this.keyUsages == null) {
			return "";
		}
        String usage = "  ";
        boolean first = true;
        for (final ObjectIdentifier oid: this.keyUsages) {
            if(!first) {
                usage += "\n  ";
            }

            final String result = map.get(oid);
            if (result != null) {
                usage += result;
            } else {
                usage += oid.toString();
            }
            first = false;
        }
        return super.toString() + "ExtendedKeyUsages [\n"
               + usage + "\n]\n";
    }

    /**
     * Write the extension to the DerOutputStream.
     *
     * @param out the DerOutputStream to write the extension to.
     * @exception IOException on encoding errors.
     */
    @Override
	public void encode(final OutputStream out) throws IOException {
        final DerOutputStream tmp = new DerOutputStream();
        if (this.extensionValue == null) {
          this.extensionId = PKIXExtensions.ExtendedKeyUsage_Id;
          this.critical = false;
          encodeThis();
        }
        super.encode(tmp);
        out.write(tmp.toByteArray());
    }

    /**
     * Set the attribute value.
     */
    @Override
	@SuppressWarnings("unchecked") // Checked with instanceof
    public void set(final String name, final Object obj) throws IOException {
        if (name.equalsIgnoreCase(USAGES)) {
            if (!(obj instanceof Vector)) {
                throw new IOException("Attribute value should be of type Vector.");
            }
            this.keyUsages = (Vector<ObjectIdentifier>)obj;
        } else {
          throw new IOException("Attribute name [" + name +
                                "] not recognized by " +
                                "CertAttrSet:ExtendedKeyUsageExtension.");
        }
        encodeThis();
    }

    /**
     * Get the attribute value.
     */
    @Override
	public Vector<ObjectIdentifier> get(final String name) throws IOException {
        if (name.equalsIgnoreCase(USAGES)) {
            //XXXX May want to consider cloning this
            return this.keyUsages;
        } else {
          throw new IOException("Attribute name [" + name +
                                "] not recognized by " +
                                "CertAttrSet:ExtendedKeyUsageExtension.");
        }
    }

    /**
     * Delete the attribute value.
     */
    @Override
	public void delete(final String name) throws IOException {
        if (name.equalsIgnoreCase(USAGES)) {
            this.keyUsages = null;
        } else {
          throw new IOException("Attribute name [" + name +
                                "] not recognized by " +
                                "CertAttrSet:ExtendedKeyUsageExtension.");
        }
        encodeThis();
    }

    /**
     * Return an enumeration of names of attributes existing within this
     * attribute.
     */
    @Override
	public Enumeration<String> getElements() {
        final AttributeNameEnumeration elements = new AttributeNameEnumeration();
        elements.addElement(USAGES);

        return (elements.elements());
    }

    /**
     * Return the name of this attribute.
     */
    @Override
	public String getName() {
        return (NAME);
    }

    public List<String> getExtendedKeyUsage() {
        final List<String> al = new ArrayList<String>(this.keyUsages.size());
        for (final ObjectIdentifier oid : this.keyUsages) {
            al.add(oid.toString());
        }
        return al;
    }

}
