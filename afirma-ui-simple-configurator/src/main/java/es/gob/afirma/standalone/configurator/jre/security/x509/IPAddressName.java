/*
 * Copyright (c) 1997, 2002, Oracle and/or its affiliates. All rights reserved.
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
import java.net.InetAddress;
import java.util.Arrays;

import es.gob.afirma.standalone.configurator.jre.misc.HexDumpEncoder;
import es.gob.afirma.standalone.configurator.jre.security.util.BitArray;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;

/**
 * This class implements the IPAddressName as required by the GeneralNames
 * ASN.1 object.  Both IPv4 and IPv6 addresses are supported using the
 * formats specified in IETF PKIX RFC2459.
 * <p>
 * [RFC2459 4.2.1.7 Subject Alternative Name]
 * When the subjectAltName extension contains a iPAddress, the address
 * MUST be stored in the octet string in "network byte order," as
 * specified in RFC 791. The least significant bit (LSB) of
 * each octet is the LSB of the corresponding byte in the network
 * address. For IP Version 4, as specified in RFC 791, the octet string
 * MUST contain exactly four octets.  For IP Version 6, as specified in
 * RFC 1883, the octet string MUST contain exactly sixteen octets.
 * <p>
 * [RFC2459 4.2.1.11 Name Constraints]
 * The syntax of iPAddress MUST be as described in section 4.2.1.7 with
 * the following additions specifically for Name Constraints.  For IPv4
 * addresses, the ipAddress field of generalName MUST contain eight (8)
 * octets, encoded in the style of RFC 1519 (CIDR) to represent an
 * address range.[RFC 1519]  For IPv6 addresses, the ipAddress field
 * MUST contain 32 octets similarly encoded.  For example, a name
 * constraint for "class C" subnet 10.9.8.0 shall be represented as the
 * octets 0A 09 08 00 FF FF FF 00, representing the CIDR notation
 * 10.9.8.0/255.255.255.0.
 * <p>
 * @see GeneralName
 * @see GeneralNameInterface
 * @see GeneralNames
 *
 *
 * @author Amit Kapoor
 * @author Hemma Prafullchandra
 */
public class IPAddressName implements GeneralNameInterface {
    private byte[] address;
    private boolean isIPv4;
    private String name;

    /**
     * Create the IPAddressName object from the passed encoded Der value.
     *
     * @params derValue the encoded DER IPAddressName.
     * @exception IOException on error.
     */
    public IPAddressName(final DerValue derValue) throws IOException {
        this(derValue.getOctetString());
    }

    /**
     * Create the IPAddressName object with the specified octets.
     *
     * @params address the IP address
     * @throws IOException if address is not a valid IPv4 or IPv6 address
     */
    public IPAddressName(final byte[] address) throws IOException {
        /*
         * A valid address must consist of 4 bytes of address and
         * optional 4 bytes of 4 bytes of mask, or 16 bytes of address
         * and optional 16 bytes of mask.
         */
        if (address.length == 4 || address.length == 8) {
            this.isIPv4 = true;
        } else if (address.length == 16 || address.length == 32) {
            this.isIPv4 = false;
        } else {
            throw new IOException("Invalid IPAddressName"); //$NON-NLS-1$
        }
        this.address = address;
    }

    /**
     * Create an IPAddressName from a String.
     * [IETF RFC1338 Supernetting & IETF RFC1519 Classless Inter-Domain
     * Routing (CIDR)] For IPv4 addresses, the forms are
     * "b1.b2.b3.b4" or "b1.b2.b3.b4/m1.m2.m3.m4", where b1 - b4 are decimal
     * byte values 0-255 and m1 - m4 are decimal mask values
     * 0 - 255.
     * <p>
     * [IETF RFC2373 IP Version 6 Addressing Architecture]
     * For IPv6 addresses, the forms are "a1:a2:...:a8" or "a1:a2:...:a8/n",
     * where a1-a8 are hexadecimal values representing the eight 16-bit pieces
     * of the address. If /n is used, n is a decimal number indicating how many
     * of the leftmost contiguous bits of the address comprise the prefix for
     * this subnet. Internally, a mask value is created using the prefix length.
     * <p>
     * @param name String form of IPAddressName
     * @throws IOException if name can not be converted to a valid IPv4 or IPv6
     *     address
     */
    public IPAddressName(final String name) throws IOException {

        if (name == null || name.length() == 0) {
            throw new IOException("IPAddress cannot be null or empty"); //$NON-NLS-1$
        }
        if (name.charAt(name.length() - 1) == '/') {
            throw new IOException("Invalid IPAddress: " + name); //$NON-NLS-1$
        }

        if (name.indexOf(':') >= 0) {
            // name is IPv6: uses colons as value separators
            // Parse name into byte-value address components and optional
            // prefix
            parseIPv6(name);
            this.isIPv4 = false;
        } else if (name.indexOf('.') >= 0) {
            //name is IPv4: uses dots as value separators
            parseIPv4(name);
            this.isIPv4 = true;
        } else {
            throw new IOException("Invalid IPAddress: " + name); //$NON-NLS-1$
        }
    }

    /**
     * Parse an IPv4 address.
     *
     * @param name IPv4 address with optional mask values
     * @throws IOException on error
     */
    private void parseIPv4(final String name) throws IOException {

        // Parse name into byte-value address components
        final int slashNdx = name.indexOf('/');
        if (slashNdx == -1) {
            this.address = InetAddress.getByName(name).getAddress();
        } else {
            this.address = new byte[8];

            // parse mask
            final byte[] mask = InetAddress.getByName
                (name.substring(slashNdx+1)).getAddress();

            // parse base address
            final byte[] host = InetAddress.getByName
                (name.substring(0, slashNdx)).getAddress();

            System.arraycopy(host, 0, this.address, 0, 4);
            System.arraycopy(mask, 0, this.address, 4, 4);
        }
    }

    /**
     * Parse an IPv6 address.
     *
     * @param name String IPv6 address with optional /<prefix length>
     *             If /<prefix length> is present, address[] array will
     *             be 32 bytes long, otherwise 16.
     * @throws IOException on error
     */
    private final static int MASKSIZE = 16;
    private void parseIPv6(final String name) throws IOException {

        final int slashNdx = name.indexOf('/');
        if (slashNdx == -1) {
            this.address = InetAddress.getByName(name).getAddress();
        } else {
            this.address = new byte[32];
            final byte[] base = InetAddress.getByName
                (name.substring(0, slashNdx)).getAddress();
            System.arraycopy(base, 0, this.address, 0, 16);

            // append a mask corresponding to the num of prefix bits specified
            final int prefixLen = Integer.parseInt(name.substring(slashNdx+1));
            if (prefixLen > 128) {
				throw new IOException("IPv6Address prefix is longer than 128"); //$NON-NLS-1$
			}

            // create new bit array initialized to zeros
            final BitArray bitArray = new BitArray(MASKSIZE * 8);

            // set all most significant bits up to prefix length
            for (int i = 0; i < prefixLen; i++) {
				bitArray.set(i, true);
			}
            final byte[] maskArray = bitArray.toByteArray();

            // copy mask bytes into mask portion of address
            for (int i = 0; i < MASKSIZE; i++) {
				this.address[MASKSIZE+i] = maskArray[i];
			}
        }
    }

    /**
     * Return the type of the GeneralName.
     */
    @Override
	public int getType() {
        return NAME_IP;
    }

    /**
     * Encode the IPAddress name into the DerOutputStream.
     *
     * @params out the DER stream to encode the IPAddressName to.
     * @exception IOException on encoding errors.
     */
    @Override
	public void encode(final DerOutputStream out) throws IOException {
        out.putOctetString(this.address);
    }

    /**
     * Return a printable string of IPaddress
     */
    @Override
	public String toString() {
        try {
            return "IPAddress: " + getName(); //$NON-NLS-1$
        } catch (final IOException ioe) {
            // dump out hex rep for debugging purposes
            final HexDumpEncoder enc = new HexDumpEncoder();
            return "IPAddress: " + enc.encodeBuffer(this.address); //$NON-NLS-1$
        }
    }

    /**
     * Return a standard String representation of IPAddress.
     * See IPAddressName(String) for the formats used for IPv4
     * and IPv6 addresses.
     *
     * @throws IOException if the IPAddress cannot be converted to a String
     */
    public String getName() throws IOException {
        if (this.name != null) {
			return this.name;
		}

        if (this.isIPv4) {
            //IPv4 address or subdomain
            final byte[] host = new byte[4];
            System.arraycopy(this.address, 0, host, 0, 4);
            this.name = InetAddress.getByAddress(host).getHostAddress();
            if (this.address.length == 8) {
                final byte[] mask = new byte[4];
                System.arraycopy(this.address, 4, mask, 0, 4);
                this.name = this.name + "/" + //$NON-NLS-1$
                       InetAddress.getByAddress(mask).getHostAddress();
            }
        } else {
            //IPv6 address or subdomain
            final byte[] host = new byte[16];
            System.arraycopy(this.address, 0, host, 0, 16);
            this.name = InetAddress.getByAddress(host).getHostAddress();
            if (this.address.length == 32) {
                // IPv6 subdomain: display prefix length

                // copy subdomain into new array and convert to BitArray
                final byte[] maskBytes = new byte[16];
                for (int i=16; i < 32; i++) {
					maskBytes[i-16] = this.address[i];
				}
                final BitArray ba = new BitArray(16*8, maskBytes);
                // Find first zero bit
                int i=0;
                for (; i < 16*8; i++) {
                    if (!ba.get(i)) {
						break;
					}
                }
                this.name = this.name + "/" + i; //$NON-NLS-1$
                // Verify remaining bits 0
                for (; i < 16*8; i++) {
                    if (ba.get(i)) {
                        throw new IOException("Invalid IPv6 subdomain - set " + //$NON-NLS-1$
                            "bit " + i + " not contiguous"); //$NON-NLS-1$ //$NON-NLS-2$
                    }
                }
            }
        }
        return this.name;
    }

    /**
     * Returns this IPAddress name as a byte array.
     */
    public byte[] getBytes() {
        return this.address.clone();
    }

    /**
     * Compares this name with another, for equality.
     *
     * @return true iff the names are identical.
     */
    @Override
	public boolean equals(final Object obj) {
        if (this == obj) {
			return true;
		}

        if (!(obj instanceof IPAddressName)) {
			return false;
		}

        final byte[] other = ((IPAddressName)obj).getBytes();

        if (other.length != this.address.length) {
			return false;
		}

        if (this.address.length == 8 || this.address.length == 32) {
            // Two subnet addresses
            // Mask each and compare masked values
            final int maskLen = this.address.length/2;
            final byte[] maskedThis = new byte[maskLen];
            final byte[] maskedOther = new byte[maskLen];
            for (int i=0; i < maskLen; i++) {
                maskedThis[i] = (byte)(this.address[i] & this.address[i+maskLen]);
                maskedOther[i] = (byte)(other[i] & other[i+maskLen]);
                if (maskedThis[i] != maskedOther[i]) {
                    return false;
                }
            }
            // Now compare masks
            for (int i=maskLen; i < this.address.length; i++) {
				if (this.address[i] != other[i]) {
					return false;
				}
			}
            return true;
        } else {
            // Two IPv4 host addresses or two IPv6 host addresses
            // Compare bytes
            return Arrays.equals(other, this.address);
        }
    }

    /**
     * Returns the hash code value for this object.
     *
     * @return a hash code value for this object.
     */
    @Override
	public int hashCode() {
        int retval = 0;

        for (int i=0; i<this.address.length; i++) {
			retval += this.address[i] * i;
		}

        return retval;
    }

    /**
     * Return type of constraint inputName places on this name:<ul>
     *   <li>NAME_DIFF_TYPE = -1: input name is different type from name
     *       (i.e. does not constrain).
     *   <li>NAME_MATCH = 0: input name matches name.
     *   <li>NAME_NARROWS = 1: input name narrows name (is lower in the naming
     *       subtree)
     *   <li>NAME_WIDENS = 2: input name widens name (is higher in the naming
     *       subtree)
     *   <li>NAME_SAME_TYPE = 3: input name does not match or narrow name, but
     *       is same type.
     * </ul>.  These results are used in checking NameConstraints during
     * certification path verification.
     * <p>
     * [RFC2459] The syntax of iPAddress MUST be as described in section
     * 4.2.1.7 with the following additions specifically for Name Constraints.
     * For IPv4 addresses, the ipAddress field of generalName MUST contain
     * eight (8) octets, encoded in the style of RFC 1519 (CIDR) to represent an
     * address range.[RFC 1519]  For IPv6 addresses, the ipAddress field
     * MUST contain 32 octets similarly encoded.  For example, a name
     * constraint for "class C" subnet 10.9.8.0 shall be represented as the
     * octets 0A 09 08 00 FF FF FF 00, representing the CIDR notation
     * 10.9.8.0/255.255.255.0.
     * <p>
     * @param inputName to be checked for being constrained
     * @returns constraint type above
     * @throws UnsupportedOperationException if name is not exact match, but
     * narrowing and widening are not supported for this name type.
     */
    @Override
	public int constrains(final GeneralNameInterface inputName)
    throws UnsupportedOperationException {
        int constraintType;
        if (inputName == null) {
			constraintType = NAME_DIFF_TYPE;
		} else if (inputName.getType() != NAME_IP) {
			constraintType = NAME_DIFF_TYPE;
		} else if (((IPAddressName)inputName).equals(this)) {
			constraintType = NAME_MATCH;
		} else {
            final byte[] otherAddress = ((IPAddressName)inputName).getBytes();
            if (otherAddress.length == 4 && this.address.length == 4) {
				// Two host addresses
                constraintType = NAME_SAME_TYPE;
			} else if ((otherAddress.length == 8 && this.address.length == 8) ||
                     (otherAddress.length == 32 && this.address.length == 32)) {
                // Two subnet addresses
                // See if one address fully encloses the other address
                boolean otherSubsetOfThis = true;
                boolean thisSubsetOfOther = true;
                boolean thisEmpty = false;
                boolean otherEmpty = false;
                final int maskOffset = this.address.length/2;
                for (int i=0; i < maskOffset; i++) {
                    if ((byte)(this.address[i] & this.address[i+maskOffset]) != this.address[i]) {
						thisEmpty=true;
					}
                    if ((byte)(otherAddress[i] & otherAddress[i+maskOffset]) != otherAddress[i]) {
						otherEmpty=true;
					}
                    if (!(((byte)(this.address[i+maskOffset] & otherAddress[i+maskOffset]) == this.address[i+maskOffset]) &&
                          ((byte)(this.address[i]   & this.address[i+maskOffset])      == (byte)(otherAddress[i] & this.address[i+maskOffset])))) {
                        otherSubsetOfThis = false;
                    }
                    if (!(((byte)(otherAddress[i+maskOffset] & this.address[i+maskOffset])      == otherAddress[i+maskOffset]) &&
                          ((byte)(otherAddress[i]   & otherAddress[i+maskOffset]) == (byte)(this.address[i] & otherAddress[i+maskOffset])))) {
                        thisSubsetOfOther = false;
                    }
                }
                if (thisEmpty || otherEmpty) {
                    if (thisEmpty && otherEmpty) {
						constraintType = NAME_MATCH;
					} else if (thisEmpty) {
						constraintType = NAME_WIDENS;
					} else {
						constraintType = NAME_NARROWS;
					}
                } else if (otherSubsetOfThis) {
					constraintType = NAME_NARROWS;
				} else if (thisSubsetOfOther) {
					constraintType = NAME_WIDENS;
				} else {
					constraintType = NAME_SAME_TYPE;
				}
            } else if (otherAddress.length == 8 || otherAddress.length == 32) {
                //Other is a subnet, this is a host address
                int i = 0;
                final int maskOffset = otherAddress.length/2;
                for (; i < maskOffset; i++) {
                    // Mask this address by other address mask and compare to other address
                    // If all match, then this address is in other address subnet
                    if ((this.address[i] & otherAddress[i+maskOffset]) != otherAddress[i]) {
						break;
					}
                }
                if (i == maskOffset) {
					constraintType = NAME_WIDENS;
				} else {
					constraintType = NAME_SAME_TYPE;
				}
            } else if (this.address.length == 8 || this.address.length == 32) {
                //This is a subnet, other is a host address
                int i = 0;
                final int maskOffset = this.address.length/2;
                for (; i < maskOffset; i++) {
                    // Mask other address by this address mask and compare to this address
                    if ((otherAddress[i] & this.address[i+maskOffset]) != this.address[i]) {
						break;
					}
                }
                if (i == maskOffset) {
					constraintType = NAME_NARROWS;
				} else {
					constraintType = NAME_SAME_TYPE;
				}
            } else {
                constraintType = NAME_SAME_TYPE;
            }
        }
        return constraintType;
    }

    /**
     * Return subtree depth of this name for purposes of determining
     * NameConstraints minimum and maximum bounds and for calculating
     * path lengths in name subtrees.
     *
     * @returns distance of name from root
     * @throws UnsupportedOperationException if not supported for this name type
     */
    @Override
	public int subtreeDepth() throws UnsupportedOperationException {
        throw new UnsupportedOperationException
            ("subtreeDepth() not defined for IPAddressName"); //$NON-NLS-1$
    }
}
