/*
 * Copyright (c) 1997, 2013, Oracle and/or its affiliates. All rights reserved.
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

package es.gob.afirma.standalone.configurator.jre.security.pkcs;

import java.io.IOException;
import java.io.OutputStream;
import java.security.cert.CertificateException;
import java.util.Date;
import java.util.Hashtable;
import java.util.Locale;

import es.gob.afirma.standalone.configurator.jre.misc.HexDumpEncoder;
import es.gob.afirma.standalone.configurator.jre.security.util.Debug;
import es.gob.afirma.standalone.configurator.jre.security.util.DerEncoder;
import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;
import es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier;
import es.gob.afirma.standalone.configurator.jre.security.x509.CertificateExtensions;

/**
 * Class supporting any PKCS9 attributes.
 * Supports DER decoding/encoding and access to attribute values.
 *
 * <a name="classTable"><h3>Type/Class Table</h3></a>
 * The following table shows the correspondence between
 * PKCS9 attribute types and value component classes.
 * For types not listed here, its name is the OID
 * in string form, its value is a (single-valued)
 * byte array that is the SET's encoding.
 *
 * <P>
 * <TABLE BORDER CELLPADDING=8 ALIGN=CENTER>
 *
 * <TR>
 * <TH>Object Identifier</TH>
 * <TH>Attribute Name</TH>
 * <TH>Type</TH>
 * <TH>Value Class</TH>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.1</TD>
 * <TD>EmailAddress</TD>
 * <TD>Multi-valued</TD>
 * <TD><code>String[]</code></TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.2</TD>
 * <TD>UnstructuredName</TD>
 * <TD>Multi-valued</TD>
 * <TD><code>String[]</code></TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.3</TD>
 * <TD>ContentType</TD>
 * <TD>Single-valued</TD>
 * <TD><code>ObjectIdentifier</code></TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.4</TD>
 * <TD>MessageDigest</TD>
 * <TD>Single-valued</TD>
 * <TD><code>byte[]</code></TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.5</TD>
 * <TD>SigningTime</TD>
 * <TD>Single-valued</TD>
 * <TD><code>Date</code></TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.6</TD>
 * <TD>Countersignature</TD>
 * <TD>Multi-valued</TD>
 * <TD><code>SignerInfo[]</code></TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.7</TD>
 * <TD>ChallengePassword</TD>
 * <TD>Single-valued</TD>
 * <TD><code>String</code></TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.8</TD>
 * <TD>UnstructuredAddress</TD>
 * <TD>Single-valued</TD>
 * <TD><code>String</code></TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.9</TD>
 * <TD>ExtendedCertificateAttributes</TD>
 * <TD>Multi-valued</TD>
 * <TD>(not supported)</TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.10</TD>
 * <TD>IssuerAndSerialNumber</TD>
 * <TD>Single-valued</TD>
 * <TD>(not supported)</TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.{11,12}</TD>
 * <TD>RSA DSI proprietary</TD>
 * <TD>Single-valued</TD>
 * <TD>(not supported)</TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.13</TD>
 * <TD>S/MIME unused assignment</TD>
 * <TD>Single-valued</TD>
 * <TD>(not supported)</TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.14</TD>
 * <TD>ExtensionRequest</TD>
 * <TD>Single-valued</TD>
 * <TD>CertificateExtensions</TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.15</TD>
 * <TD>SMIMECapability</TD>
 * <TD>Single-valued</TD>
 * <TD>(not supported)</TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.16.2.12</TD>
 * <TD>SigningCertificate</TD>
 * <TD>Single-valued</TD>
 * <TD>SigningCertificateInfo</TD>
 * </TR>
 *
 * <TR>
 * <TD>1.2.840.113549.1.9.16.2.14</TD>
 * <TD>SignatureTimestampToken</TD>
 * <TD>Single-valued</TD>
 * <TD>byte[]</TD>
 * </TR>
 *
 * </TABLE>
 *
 * @author Douglas Hoover
 */
public class PKCS9Attribute implements DerEncoder {

    /* Are we debugging ? */
    private static final Debug debug = Debug.getInstance("jar");

    /**
     * Array of attribute OIDs defined in PKCS9, by number.
     */
    static final ObjectIdentifier[] PKCS9_OIDS = new ObjectIdentifier[18];

    private final static Class<?> BYTE_ARRAY_CLASS;

    static {   // static initializer for PKCS9_OIDS
        for (int i = 1; i < PKCS9_OIDS.length - 2; i++) {
            PKCS9_OIDS[i] =
                ObjectIdentifier.newInternal(new int[]{1,2,840,113549,1,9,i});
        }
        // Initialize SigningCertificate and SignatureTimestampToken
        // separately (because their values are out of sequence)
        PKCS9_OIDS[PKCS9_OIDS.length - 2] =
            ObjectIdentifier.newInternal(new int[]{1,2,840,113549,1,9,16,2,12});
        PKCS9_OIDS[PKCS9_OIDS.length - 1] =
            ObjectIdentifier.newInternal(new int[]{1,2,840,113549,1,9,16,2,14});

        try {
            BYTE_ARRAY_CLASS = Class.forName("[B");
        } catch (final ClassNotFoundException e) {
            throw new ExceptionInInitializerError(e.toString());
        }
    }

    // first element [0] not used
    public static final ObjectIdentifier EMAIL_ADDRESS_OID = PKCS9_OIDS[1];
    public static final ObjectIdentifier UNSTRUCTURED_NAME_OID = PKCS9_OIDS[2];
    public static final ObjectIdentifier CONTENT_TYPE_OID = PKCS9_OIDS[3];
    public static final ObjectIdentifier MESSAGE_DIGEST_OID = PKCS9_OIDS[4];
    public static final ObjectIdentifier SIGNING_TIME_OID = PKCS9_OIDS[5];
    public static final ObjectIdentifier COUNTERSIGNATURE_OID = PKCS9_OIDS[6];
    public static final ObjectIdentifier CHALLENGE_PASSWORD_OID = PKCS9_OIDS[7];
    public static final ObjectIdentifier UNSTRUCTURED_ADDRESS_OID = PKCS9_OIDS[8];
    public static final ObjectIdentifier EXTENDED_CERTIFICATE_ATTRIBUTES_OID
                                         = PKCS9_OIDS[9];
    public static final ObjectIdentifier ISSUER_SERIALNUMBER_OID = PKCS9_OIDS[10];
    // [11], [12] are RSA DSI proprietary
    // [13] ==> signingDescription, S/MIME, not used anymore
    public static final ObjectIdentifier EXTENSION_REQUEST_OID = PKCS9_OIDS[14];
    public static final ObjectIdentifier SMIME_CAPABILITY_OID = PKCS9_OIDS[15];
    public static final ObjectIdentifier SIGNING_CERTIFICATE_OID = PKCS9_OIDS[16];
    public static final ObjectIdentifier SIGNATURE_TIMESTAMP_TOKEN_OID =
                                PKCS9_OIDS[17];
    public static final String EMAIL_ADDRESS_STR = "EmailAddress";
    public static final String UNSTRUCTURED_NAME_STR = "UnstructuredName";
    public static final String CONTENT_TYPE_STR = "ContentType";
    public static final String MESSAGE_DIGEST_STR = "MessageDigest";
    public static final String SIGNING_TIME_STR = "SigningTime";
    public static final String COUNTERSIGNATURE_STR = "Countersignature";
    public static final String CHALLENGE_PASSWORD_STR = "ChallengePassword";
    public static final String UNSTRUCTURED_ADDRESS_STR = "UnstructuredAddress";
    public static final String EXTENDED_CERTIFICATE_ATTRIBUTES_STR =
                               "ExtendedCertificateAttributes";
    public static final String ISSUER_SERIALNUMBER_STR = "IssuerAndSerialNumber";
    // [11], [12] are RSA DSI proprietary
    private static final String RSA_PROPRIETARY_STR = "RSAProprietary";
    // [13] ==> signingDescription, S/MIME, not used anymore
    private static final String SMIME_SIGNING_DESC_STR = "SMIMESigningDesc";
    public static final String EXTENSION_REQUEST_STR = "ExtensionRequest";
    public static final String SMIME_CAPABILITY_STR = "SMIMECapability";
    public static final String SIGNING_CERTIFICATE_STR = "SigningCertificate";
    public static final String SIGNATURE_TIMESTAMP_TOKEN_STR =
                                "SignatureTimestampToken";

    /**
     * Hashtable mapping names and variant names of supported
     * attributes to their OIDs. This table contains all name forms
     * that occur in PKCS9, in lower case.
     */
    private static final Hashtable<String, ObjectIdentifier> NAME_OID_TABLE =
        new Hashtable<String, ObjectIdentifier>(18);

    static { // static initializer for PCKS9_NAMES
        NAME_OID_TABLE.put("emailaddress", PKCS9_OIDS[1]);
        NAME_OID_TABLE.put("unstructuredname", PKCS9_OIDS[2]);
        NAME_OID_TABLE.put("contenttype", PKCS9_OIDS[3]);
        NAME_OID_TABLE.put("messagedigest", PKCS9_OIDS[4]);
        NAME_OID_TABLE.put("signingtime", PKCS9_OIDS[5]);
        NAME_OID_TABLE.put("countersignature", PKCS9_OIDS[6]);
        NAME_OID_TABLE.put("challengepassword", PKCS9_OIDS[7]);
        NAME_OID_TABLE.put("unstructuredaddress", PKCS9_OIDS[8]);
        NAME_OID_TABLE.put("extendedcertificateattributes", PKCS9_OIDS[9]);
        NAME_OID_TABLE.put("issuerandserialnumber", PKCS9_OIDS[10]);
        NAME_OID_TABLE.put("rsaproprietary", PKCS9_OIDS[11]);
        NAME_OID_TABLE.put("rsaproprietary", PKCS9_OIDS[12]);
        NAME_OID_TABLE.put("signingdescription", PKCS9_OIDS[13]);
        NAME_OID_TABLE.put("extensionrequest", PKCS9_OIDS[14]);
        NAME_OID_TABLE.put("smimecapability", PKCS9_OIDS[15]);
        NAME_OID_TABLE.put("signingcertificate", PKCS9_OIDS[16]);
        NAME_OID_TABLE.put("signaturetimestamptoken", PKCS9_OIDS[17]);
    };

    /**
     * Hashtable mapping attribute OIDs defined in PKCS9 to the
     * corresponding attribute value type.
     */
    private static final Hashtable<ObjectIdentifier, String> OID_NAME_TABLE =
        new Hashtable<ObjectIdentifier, String>(16);
    static {
        OID_NAME_TABLE.put(PKCS9_OIDS[1], EMAIL_ADDRESS_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[2], UNSTRUCTURED_NAME_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[3], CONTENT_TYPE_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[4], MESSAGE_DIGEST_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[5], SIGNING_TIME_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[6], COUNTERSIGNATURE_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[7], CHALLENGE_PASSWORD_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[8], UNSTRUCTURED_ADDRESS_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[9], EXTENDED_CERTIFICATE_ATTRIBUTES_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[10], ISSUER_SERIALNUMBER_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[11], RSA_PROPRIETARY_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[12], RSA_PROPRIETARY_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[13], SMIME_SIGNING_DESC_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[14], EXTENSION_REQUEST_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[15], SMIME_CAPABILITY_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[16], SIGNING_CERTIFICATE_STR);
        OID_NAME_TABLE.put(PKCS9_OIDS[17], SIGNATURE_TIMESTAMP_TOKEN_STR);
    }

    /**
     * Acceptable ASN.1 tags for DER encodings of values of PKCS9
     * attributes, by index in <code>PKCS9_OIDS</code>.
     * Sets of acceptable tags are represented as arrays.
     */
    private static final Byte[][] PKCS9_VALUE_TAGS = {
        null,
        {new Byte(DerValue.tag_IA5String)},   // EMailAddress
        {new Byte(DerValue.tag_IA5String),   // UnstructuredName
         new Byte(DerValue.tag_PrintableString)},
        {new Byte(DerValue.tag_ObjectId)},    // ContentType
        {new Byte(DerValue.tag_OctetString)}, // MessageDigest
        {new Byte(DerValue.tag_UtcTime)},     // SigningTime
        {new Byte(DerValue.tag_Sequence)},    // Countersignature
        {new Byte(DerValue.tag_PrintableString),
         new Byte(DerValue.tag_T61String)},   // ChallengePassword
        {new Byte(DerValue.tag_PrintableString),
         new Byte(DerValue.tag_T61String)},   // UnstructuredAddress
        {new Byte(DerValue.tag_SetOf)},       // ExtendedCertificateAttributes
        {new Byte(DerValue.tag_Sequence)},    // issuerAndSerialNumber
        null,
        null,
        null,
        {new Byte(DerValue.tag_Sequence)},    // extensionRequest
        {new Byte(DerValue.tag_Sequence)},    // SMIMECapability
        {new Byte(DerValue.tag_Sequence)},    // SigningCertificate
        {new Byte(DerValue.tag_Sequence)}     // SignatureTimestampToken
    };

    private static final Class<?>[] VALUE_CLASSES = new Class<?>[18];

    static {
        try {
            final Class<?> str = Class.forName("[Ljava.lang.String;");

            VALUE_CLASSES[0] = null;  // not used
            VALUE_CLASSES[1] = str;   // EMailAddress
            VALUE_CLASSES[2] = str;   // UnstructuredName
            VALUE_CLASSES[3] =        // ContentType
                Class.forName("es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier");
            VALUE_CLASSES[4] = BYTE_ARRAY_CLASS; // MessageDigest (byte[])
            VALUE_CLASSES[5] = Class.forName("java.util.Date"); // SigningTime
            VALUE_CLASSES[6] =        // Countersignature
                Class.forName("[Les.gob.afirma.standalone.configurator.jre.security.pkcs.SignerInfo;");
            VALUE_CLASSES[7] =        // ChallengePassword
                Class.forName("java.lang.String");
            VALUE_CLASSES[8] = str;   // UnstructuredAddress
            VALUE_CLASSES[9] = null;  // ExtendedCertificateAttributes
            VALUE_CLASSES[10] = null;  // IssuerAndSerialNumber
            VALUE_CLASSES[11] = null;  // not used
            VALUE_CLASSES[12] = null;  // not used
            VALUE_CLASSES[13] = null;  // not used
            VALUE_CLASSES[14] =        // ExtensionRequest
                Class.forName("es.gob.afirma.standalone.configurator.jre.security.x509.CertificateExtensions");
            VALUE_CLASSES[15] = null;  // not supported yet
            VALUE_CLASSES[16] = null;  // not supported yet
            VALUE_CLASSES[17] = BYTE_ARRAY_CLASS;  // SignatureTimestampToken
        } catch (final ClassNotFoundException e) {
            throw new ExceptionInInitializerError(e.toString());
        }
    }

    /**
     * Array indicating which PKCS9 attributes are single-valued,
     * by index in <code>PKCS9_OIDS</code>.
     */
    private static final boolean[] SINGLE_VALUED = {
      false,
      false,   // EMailAddress
      false,   // UnstructuredName
      true,    // ContentType
      true,    // MessageDigest
      true,    // SigningTime
      false,   // Countersignature
      true,    // ChallengePassword
      false,   // UnstructuredAddress
      false,   // ExtendedCertificateAttributes
      true,    // IssuerAndSerialNumber - not supported yet
      false,   // not used
      false,   // not used
      false,   // not used
      true,    // ExtensionRequest
      true,    // SMIMECapability - not supported yet
      true,    // SigningCertificate
      true     // SignatureTimestampToken
    };

    /**
     * The OID of this attribute.
     */
    private ObjectIdentifier oid;

    /**
     * The index of the OID of this attribute in <code>PKCS9_OIDS</code>,
     * or -1 if it's unknown.
     */
    private int index;

    /**
     * Value set of this attribute.  Its class is given by
     * <code>VALUE_CLASSES[index]</code>. The SET itself
     * as byte[] if unknown.
     */
    private Object value;

    /**
     * Construct an attribute object from the attribute's OID and
     * value.  If the attribute is single-valued, provide only one
     * value.  If the attribute is multi-valued, provide an array
     * containing all the values.
     * Arrays of length zero are accepted, though probably useless.
     *
     * <P> The
     * <a href=#classTable>table</a> gives the class that <code>value</code>
     * must have for a given attribute.
     *
     * @exception IllegalArgumentException
     * if the <code>value</code> has the wrong type.
     */
    public PKCS9Attribute(final ObjectIdentifier oid, final Object value)
    throws IllegalArgumentException {
        init(oid, value);
    }

    /**
     * Construct an attribute object from the attribute's name and
     * value.  If the attribute is single-valued, provide only one
     * value.  If the attribute is multi-valued, provide an array
     * containing all the values.
     * Arrays of length zero are accepted, though probably useless.
     *
     * <P> The
     * <a href=#classTable>table</a> gives the class that <code>value</code>
     * must have for a given attribute. Reasonable variants of these
     * attributes are accepted; in particular, case does not matter.
     *
     * @exception IllegalArgumentException
     * if the <code>name</code> is not recognized or the
     * <code>value</code> has the wrong type.
     */
    public PKCS9Attribute(final String name, final Object value)
    throws IllegalArgumentException {
        final ObjectIdentifier oid = getOID(name);

        if (oid == null) {
			throw new IllegalArgumentException(
                       "Unrecognized attribute name " + name +
                       " constructing PKCS9Attribute.");
		}

        init(oid, value);
    }

    private void init(final ObjectIdentifier oid, final Object value)
        throws IllegalArgumentException {

        this.oid = oid;
        this.index = indexOf(oid, PKCS9_OIDS, 1);
        final Class<?> clazz = this.index == -1 ? BYTE_ARRAY_CLASS: VALUE_CLASSES[this.index];
        if (!clazz.isInstance(value)) {
                throw new IllegalArgumentException(
                           "Wrong value class " +
                           " for attribute " + oid +
                           " constructing PKCS9Attribute; was " +
                           value.getClass().toString() + ", should be " +
                           clazz.toString());
        }
        this.value = value;
    }


    /**
     * Construct a PKCS9Attribute from its encoding on an input
     * stream.
     *
     * @param val the DerValue representing the DER encoding of the attribute.
     * @exception IOException on parsing error.
     */
    public PKCS9Attribute(final DerValue derVal) throws IOException {

        final DerInputStream derIn = new DerInputStream(derVal.toByteArray());
        final DerValue[] val =  derIn.getSequence(2);

        if (derIn.available() != 0) {
			throw new IOException("Excess data parsing PKCS9Attribute");
		}

        if (val.length != 2) {
			throw new IOException("PKCS9Attribute doesn't have two components");
		}

        // get the oid
        this.oid = val[0].getOID();
        final byte[] content = val[1].toByteArray();
        final DerValue[] elems = new DerInputStream(content).getSet(1);

        this.index = indexOf(this.oid, PKCS9_OIDS, 1);
        if (this.index == -1) {
            if (debug != null) {
                debug.println("Unsupported signer attribute: " + this.oid);
            }
            this.value = content;
            return;
        }

        // check single valued have only one value
        if (SINGLE_VALUED[this.index] && elems.length > 1) {
			throwSingleValuedException();
		}

        // check for illegal element tags
        Byte tag;
        for (final DerValue elem : elems) {
            tag = new Byte(elem.tag);

            if (indexOf(tag, PKCS9_VALUE_TAGS[this.index], 0) == -1) {
				throwTagException(tag);
			}
        }

        switch (this.index) {
        case 1:     // email address
        case 2:     // unstructured name
        case 8:     // unstructured address
            { // open scope
                final String[] values = new String[elems.length];

                for (int i=0; i < elems.length; i++) {
					values[i] = elems[i].getAsString();
				}
                this.value = values;
            } // close scope
            break;

        case 3:     // content type
            this.value = elems[0].getOID();
            break;

        case 4:     // message digest
            this.value = elems[0].getOctetString();
            break;

        case 5:     // signing time
            this.value = (new DerInputStream(elems[0].toByteArray())).getUTCTime();
            break;

        case 6:     // countersignature
            { // open scope
                final SignerInfo[] values = new SignerInfo[elems.length];
                for (int i=0; i < elems.length; i++) {
					values[i] =
                        new SignerInfo(elems[i].toDerInputStream());
				}
                this.value = values;
            } // close scope
            break;

        case 7:     // challenge password
            this.value = elems[0].getAsString();
            break;

        case 9:     // extended-certificate attribute -- not supported
            throw new IOException("PKCS9 extended-certificate " +
                                  "attribute not supported.");
            // break unnecessary
        case 10:    // issuerAndserialNumber attribute -- not supported
            throw new IOException("PKCS9 IssuerAndSerialNumber" +
                                  "attribute not supported.");
            // break unnecessary
        case 11:    // RSA DSI proprietary
        case 12:    // RSA DSI proprietary
            throw new IOException("PKCS9 RSA DSI attributes" +
                                  "11 and 12, not supported.");
            // break unnecessary
        case 13:    // S/MIME unused attribute
            throw new IOException("PKCS9 attribute #13 not supported.");
            // break unnecessary

        case 14:     // ExtensionRequest
            this.value = new CertificateExtensions(
                       new DerInputStream(elems[0].toByteArray()));
            break;

        case 15:     // SMIME-capability attribute -- not supported
            throw new IOException("PKCS9 SMIMECapability " +
                                  "attribute not supported.");
            // break unnecessary
        case 16:     // SigningCertificate attribute
            this.value = new SigningCertificateInfo(elems[0].toByteArray());
            break;

        case 17:     // SignatureTimestampToken attribute
            this.value = elems[0].toByteArray();
            break;
        default: // can't happen
        }
    }

    /**
     * Write the DER encoding of this attribute to an output stream.
     *
     * <P> N.B.: This method always encodes values of
     * ChallengePassword and UnstructuredAddress attributes as ASN.1
     * <code>PrintableString</code>s, without checking whether they
     * should be encoded as <code>T61String</code>s.
     */
    @Override
	public void derEncode(final OutputStream out) throws IOException {
        final DerOutputStream temp = new DerOutputStream();
        temp.putOID(this.oid);
        switch (this.index) {
        case -1:    // Unknown
            temp.write((byte[])this.value);
            break;
        case 1:     // email address
        case 2:     // unstructured name
            { // open scope
                final String[] values = (String[]) this.value;
                final DerOutputStream[] temps = new
                    DerOutputStream[values.length];

                for (int i=0; i < values.length; i++) {
                    temps[i] = new DerOutputStream();
                    temps[i].putIA5String( values[i]);
                }
                temp.putOrderedSetOf(DerValue.tag_Set, temps);
            } // close scope
            break;

        case 3:     // content type
            {
                final DerOutputStream temp2 = new DerOutputStream();
                temp2.putOID((ObjectIdentifier) this.value);
                temp.write(DerValue.tag_Set, temp2.toByteArray());
            }
            break;

        case 4:     // message digest
            {
                final DerOutputStream temp2 = new DerOutputStream();
                temp2.putOctetString((byte[]) this.value);
                temp.write(DerValue.tag_Set, temp2.toByteArray());
            }
            break;

        case 5:     // signing time
            {
                final DerOutputStream temp2 = new DerOutputStream();
                temp2.putUTCTime((Date) this.value);
                temp.write(DerValue.tag_Set, temp2.toByteArray());
            }
            break;

        case 6:     // countersignature
            temp.putOrderedSetOf(DerValue.tag_Set, (DerEncoder[]) this.value);
            break;

        case 7:     // challenge password
            {
                final DerOutputStream temp2 = new DerOutputStream();
                temp2.putPrintableString((String) this.value);
                temp.write(DerValue.tag_Set, temp2.toByteArray());
            }
            break;

        case 8:     // unstructured address
            { // open scope
                final String[] values = (String[]) this.value;
                final DerOutputStream[] temps = new
                    DerOutputStream[values.length];

                for (int i=0; i < values.length; i++) {
                    temps[i] = new DerOutputStream();
                    temps[i].putPrintableString(values[i]);
                }
                temp.putOrderedSetOf(DerValue.tag_Set, temps);
            } // close scope
            break;

        case 9:     // extended-certificate attribute -- not supported
            throw new IOException("PKCS9 extended-certificate " +
                                  "attribute not supported.");
            // break unnecessary
        case 10:    // issuerAndserialNumber attribute -- not supported
            throw new IOException("PKCS9 IssuerAndSerialNumber" +
                                  "attribute not supported.");
            // break unnecessary
        case 11:    // RSA DSI proprietary
        case 12:    // RSA DSI proprietary
            throw new IOException("PKCS9 RSA DSI attributes" +
                                  "11 and 12, not supported.");
            // break unnecessary
        case 13:    // S/MIME unused attribute
            throw new IOException("PKCS9 attribute #13 not supported.");
            // break unnecessary

        case 14:     // ExtensionRequest
            {
                final DerOutputStream temp2 = new DerOutputStream();
                final CertificateExtensions exts = (CertificateExtensions)this.value;
                try {
                    exts.encode(temp2, true);
                } catch (final CertificateException ex) {
                    throw new IOException(ex.toString());
                }
                temp.write(DerValue.tag_Set, temp2.toByteArray());
            }
            break;
        case 15:    // SMIMECapability
            throw new IOException("PKCS9 attribute #15 not supported.");
            // break unnecessary

        case 16:    // SigningCertificate
            throw new IOException(
                "PKCS9 SigningCertificate attribute not supported.");
            // break unnecessary

        case 17:    // SignatureTimestampToken
            temp.write(DerValue.tag_Set, (byte[])this.value);
            break;

        default: // can't happen
        }

        final DerOutputStream derOut = new DerOutputStream();
        derOut.write(DerValue.tag_Sequence, temp.toByteArray());

        out.write(derOut.toByteArray());

    }

    /**
     * Returns if the attribute is known. Unknown attributes can be created
     * from DER encoding with unknown OIDs.
     */
    public boolean isKnown() {
        return this.index != -1;
    }

    /**
     * Get the value of this attribute.  If the attribute is
     * single-valued, return just the one value.  If the attribute is
     * multi-valued, return an array containing all the values.
     * It is possible for this array to be of length 0.
     *
     * <P> The
     * <a href=#classTable>table</a> gives the class of the value returned,
     * depending on the type of this attribute.
     */
    public Object getValue() {
        return this.value;
    }

    /**
     * Show whether this attribute is single-valued.
     */
    public boolean isSingleValued() {
        return this.index == -1 || SINGLE_VALUED[this.index];
    }

    /**
     *  Return the OID of this attribute.
     */
    public ObjectIdentifier getOID() {
        return this.oid;
    }

    /**
     *  Return the name of this attribute.
     */
    public String getName() {
        return this.index == -1 ?
                this.oid.toString() :
                OID_NAME_TABLE.get(PKCS9_OIDS[this.index]);
    }

    /**
     * Return the OID for a given attribute name or null if we don't recognize
     * the name.
     */
    public static ObjectIdentifier getOID(final String name) {
        return NAME_OID_TABLE.get(name.toLowerCase(Locale.ENGLISH));
    }

    /**
     * Return the attribute name for a given OID or null if we don't recognize
     * the oid.
     */
    public static String getName(final ObjectIdentifier oid) {
        return OID_NAME_TABLE.get(oid);
    }

    /**
     * Returns a string representation of this attribute.
     */
    @Override
	public String toString() {
        final StringBuffer buf = new StringBuffer(100);

        buf.append("[");

        if (this.index == -1) {
            buf.append(this.oid.toString());
        } else {
            buf.append(OID_NAME_TABLE.get(PKCS9_OIDS[this.index]));
        }
        buf.append(": ");

        if (this.index == -1 || SINGLE_VALUED[this.index]) {
            if (this.value instanceof byte[]) { // special case for octet string
                final HexDumpEncoder hexDump = new HexDumpEncoder();
                buf.append(hexDump.encodeBuffer((byte[]) this.value));
            } else {
                buf.append(this.value.toString());
            }
            buf.append("]");
            return buf.toString();
        } else { // multi-valued
            boolean first = true;
            final Object[] values = (Object[]) this.value;

            for (final Object value2 : values) {
                if (first) {
					first = false;
				} else {
					buf.append(", ");
				}

                buf.append(value2.toString());
            }
            return buf.toString();
        }
    }

    /**
     * Beginning the search at <code>start</code>, find the first
     * index <code>i</code> such that <code>a[i] = obj</code>.
     *
     * @return the index, if found, and -1 otherwise.
     */
    static int indexOf(final Object obj, final Object[] a, final int start) {
        for (int i=start; i < a.length; i++) {
            if (obj.equals(a[i])) {
				return i;
			}
        }
        return -1;
    }

    /**
     * Throw an exception when there are multiple values for
     * a single-valued attribute.
     */
    private void throwSingleValuedException() throws IOException {
        throw new IOException("Single-value attribute " +
                              this.oid + " (" + getName() + ")" +
                              " has multiple values.");
    }

    /**
     * Throw an exception when the tag on a value encoding is
     * wrong for the attribute whose value it is. This method
     * will only be called for known tags.
     */
    private void throwTagException(final Byte tag)
    throws IOException {
        final Byte[] expectedTags = PKCS9_VALUE_TAGS[this.index];
        final StringBuffer msg = new StringBuffer(100);
        msg.append("Value of attribute ");
        msg.append(this.oid.toString());
        msg.append(" (");
        msg.append(getName());
        msg.append(") has wrong tag: ");
        msg.append(tag.toString());
        msg.append(".  Expected tags: ");

        msg.append(expectedTags[0].toString());

        for (int i = 1; i < expectedTags.length; i++) {
            msg.append(", ");
            msg.append(expectedTags[i].toString());
        }
        msg.append(".");
        throw new IOException(msg.toString());
    }
}
