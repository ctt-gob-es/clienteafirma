/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc.http;

/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 *
 * Contributor(s):
 *
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */

/*
 * @(#)URLName.java	1.19 07/05/04
 */

import java.io.UnsupportedEncodingException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.BitSet;
import java.util.Locale;
import java.util.logging.Logger;

/** The name of a URL. This class represents a URL name and also provides the
 * basic parsing functionality to parse most internet standard URL schemes.
 * <p>
 * Note that this class differs from <code>java.net.URL</code> in that this
 * class just represents the name of a URL, it does not model the connection to
 * a URL.
 * @version 1.19, 07/05/04
 * @author Christopher Cotton
 * @author Bill Shannon */

final class URLName {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** The full version of the URL. */
	protected String fullURL;

	/** The protocol to use (ftp, http, nntp, imap, pop3 ... etc.). */
	private String protocol;

	/** The username to use when connecting. */
	private String username;

	/** The password to use when connecting. */
	private String password;

	/** The host name to which to connect. */
	private String host;

	/** The host's IP address, used in equals and hashCode. Computed on demand. */
	private InetAddress hostAddress;
	private boolean hostAddressKnown = false;

	/** The protocol port to connect to. */
	private int port = -1;

	/** The specified file name on that host. */
	private String file;

	/** # reference. */
	private String ref;

	/** Our hash code. */
	private int hashCode = 0;

	/** A way to turn off encoding, just in case... */
	private static boolean doEncode = true;

	static {
		try {
			doEncode = !Boolean.getBoolean("mail.URLName.dontencode"); //$NON-NLS-1$
		}
		catch (final Exception ex) {
			LOGGER.warning("No se ha podido leer la variable 'mail.URLName.dontencode': " + ex); //$NON-NLS-1$
			// ignore any errors
		}
	}

	/** Construct a URLName from the string. Parses out all the possible
	 * information (protocol, host, port, file, username, password).
	 * @param url URL string. */
	URLName(final String url) {
		parseString(url);
	}

	/** Constructs a string representation of this URLName. */
	@Override
	public String toString() {
		if (this.fullURL == null) {
			// add the "protocol:"
			final StringBuffer tempURL = new StringBuffer();
			if (this.protocol != null) {
				tempURL.append(this.protocol);
				tempURL.append(":"); //$NON-NLS-1$
			}

			if (this.username != null || this.host != null) {
				// add the "//"
				tempURL.append("//"); //$NON-NLS-1$

				// add the user:password@
				// XXX - can you just have a password? without a username?
				if (this.username != null) {
					tempURL.append(this.username);

					if (this.password != null) {
						tempURL.append(":"); //$NON-NLS-1$
						tempURL.append(this.password);
					}

					tempURL.append("@"); //$NON-NLS-1$
				}

				// add host
				if (this.host != null) {
					tempURL.append(this.host);
				}

				// add port (if needed)
				if (this.port != -1) {
					tempURL.append(":"); //$NON-NLS-1$
					tempURL.append(Integer.toString(this.port));
				}
				if (this.file != null) {
					tempURL.append("/"); //$NON-NLS-1$
				}
			}

			// add the file
			if (this.file != null) {
				tempURL.append(this.file);
			}

			// add the ref
			if (this.ref != null) {
				tempURL.append("#"); //$NON-NLS-1$
				tempURL.append(this.ref);
			}

			// create the fullURL now
			this.fullURL = tempURL.toString();
		}

		return this.fullURL;
	}

	/** Method which does all of the work of parsing the string.
	 * @param url URL string. */
	private void parseString(final String url) {
		// initialize everything in case called from subclass
		// (URLName really should be a final class)
		this.protocol = this.file = this.ref = this.host = this.username = this.password = null;
		this.port = -1;

		final int len = url.length();

		// find the protocol
		// XXX - should check for only legal characters before the colon
		// (legal: a-z, A-Z, 0-9, "+", ".", "-")
		final int protocolEnd = url.indexOf(':');
		if (protocolEnd != -1) {
			this.protocol = url.substring(0, protocolEnd);
		}

		// is this an Internet standard URL that contains a host name?
		if (url.regionMatches(protocolEnd + 1, "//", 0, 2)) { //$NON-NLS-1$
			// find where the file starts
			String fullhost = null;
			final int fileStart = url.indexOf('/', protocolEnd + 3);
			if (fileStart != -1) {
				fullhost = url.substring(protocolEnd + 3, fileStart);
				if (fileStart + 1 < len) {
					this.file = url.substring(fileStart + 1);
				}
				else {
					this.file = ""; //$NON-NLS-1$
				}
			}
			else {
				fullhost = url.substring(protocolEnd + 3);
			}

			// examine the fullhost, for username password etc.
			final int i = fullhost.indexOf('@');
			if (i != -1) {
				final String fulluserpass = fullhost.substring(0, i);
				fullhost = fullhost.substring(i + 1);

				// get user and password
				final int passindex = fulluserpass.indexOf(':');
				if (passindex != -1) {
					this.username = fulluserpass.substring(0, passindex);
					this.password = fulluserpass.substring(passindex + 1);
				}
				else {
					this.username = fulluserpass;
				}
			}

			// get the port (if there)
			int portindex;
			if (fullhost.length() > 0 && fullhost.charAt(0) == '[') {
				// an IPv6 address?
				portindex = fullhost.indexOf(':', fullhost.indexOf(']'));
			}
			else {
				portindex = fullhost.indexOf(':');
			}
			if (portindex != -1) {
				final String portstring = fullhost.substring(portindex + 1);
				if (portstring.length() > 0) {
					try {
						this.port = Integer.parseInt(portstring);
					}
					catch (final NumberFormatException nfex) {
						LOGGER.warning("El numero de puerto establecido '" + portstring + "' no es valido: " + nfex); //$NON-NLS-1$ //$NON-NLS-2$
						this.port = -1;
					}
				}

				this.host = fullhost.substring(0, portindex);
			}
			else {
				this.host = fullhost;
			}
		}
		else {
			if (protocolEnd + 1 < len) {
				this.file = url.substring(protocolEnd + 1);
			}
		}

		// extract the reference from the file name, if any
		int refStart;
		if (this.file != null && (refStart = this.file.indexOf('#')) != -1) {
			this.ref = this.file.substring(refStart + 1);
			this.file = this.file.substring(0, refStart);
		}
	}

	/** Returns the port number of this URLName. Returns -1 if the port is not
	 * set.
	 * @return Port number of this URLName or -1 if the port is not
	 *         set. */
	int getPort() {
		return this.port;
	}

	/** Returns the protocol of this URLName. Returns null if this URLName has no
	 * protocol.
	 * @return The protocol of this URLName or null if this URLName has no
	 *         protocol. */
	String getProtocol() {
		return this.protocol;
	}

	/** Returns the file name of this URLName. Returns null if this URLName has
	 * no file name.
	 * @return The file name of this URLName or null if this URLName has
	 *         no file name. */
	String getFile() {
		return this.file;
	}

	/** Returns the host of this URLName. Returns null if this URLName has no
	 * host.
	 * @return The host of this URLName or null if this URLName has no
	 *         host. */
	String getHost() {
		return this.host;
	}

	/** Returns the user name of this URLName. Returns null if this URLName has
	 * no user name.
	 * @return The user name of this URLName or null if this URLName has
	 *         no user name.*/
	String getUsername() {
		return doEncode ? decode(this.username) : this.username;
	}

	/** Returns the password of this URLName. Returns null if this URLName has no
	 * password.
	 * @return The password of this URLName or null if this URLName has no
	 * password.*/
	String getPassword() {
		return doEncode ? decode(this.password) : this.password;
	}

	/**
	 * Compares two URLNames. The result is true if and only if the argument is
	 * not null and is a URLName object that represents the same URLName as this
	 * object. Two URLName objects are equal if they have the same protocol and
	 * the same host, the same port number on the host, the same username, and
	 * the same file on the host. The fields (host, username, file) are also
	 * considered the same if they are both null.
	 * <p>
	 *
	 * Hosts are considered equal if the names are equal (case independent) or
	 * if host name lookups for them both succeed and they both reference the
	 * same IP address.
	 * <p>
	 *
	 * Note that URLName has no knowledge of default port numbers for particular
	 * protocols, so "imap://host" and "imap://host:143" would not compare as
	 * equal.
	 * <p>
	 *
	 * Note also that the password field is not included in the comparison, nor
	 * is any reference field appended to the filename.
	 */
	@Override
	public boolean equals(final Object obj) {
		if (!(obj instanceof URLName)) {
			return false;
		}
		final URLName u2 = (URLName) obj;

		// compare protocols
		if (u2.protocol == null || !u2.protocol.equals(this.protocol)) {
			return false;
		}

		// compare hosts
		final InetAddress a1 = getHostAddress(), a2 = u2.getHostAddress();
		// if we have internet address for both, and they're not the same, fail
		if (a1 != null && a2 != null) {
			if (!a1.equals(a2)) {
				return false;
				// else, if we have host names for both, and they're not the
				// same, fail
			}
		}
		else if (this.host != null && u2.host != null) {
			if (!this.host.equalsIgnoreCase(u2.host)) {
				return false;
				// else, if not both null
			}
		}
		else if (this.host != u2.host) {
			return false;
		}
		// at this point, hosts match

		// compare usernames
		if (!(this.username == u2.username || this.username != null
				&& this.username.equals(u2.username))) {
			return false;
		}

		// Forget about password since it doesn't
		// really denote a different store.

		// compare files
		final String f1 = this.file == null ? "" : this.file; //$NON-NLS-1$
		final String f2 = u2.file == null ? "" : u2.file; //$NON-NLS-1$

		if (!f1.equals(f2)) {
			return false;
		}

		// compare ports
		if (this.port != u2.port) {
			return false;
		}

		// all comparisons succeeded, they're equal
		return true;
	}

	/** Compute the hash code for this URLName. */
	@Override
	public int hashCode() {
		if (this.hashCode != 0) {
			return this.hashCode;
		}
		if (this.protocol != null) {
			this.hashCode += this.protocol.hashCode();
		}
		final InetAddress addr = getHostAddress();
		if (addr != null) {
			this.hashCode += addr.hashCode();
		}
		else if (this.host != null) {
			this.hashCode += this.host.toLowerCase(Locale.ENGLISH).hashCode();
		}
		if (this.username != null) {
			this.hashCode += this.username.hashCode();
		}
		if (this.file != null) {
			this.hashCode += this.file.hashCode();
		}
		this.hashCode += this.port;
		return this.hashCode;
	}

	/** Get the IP address of our host. Look up the name the first time and
	 * remember that we've done so, whether the lookup fails or not.
	 * @return IP address of our host. */
	private synchronized InetAddress getHostAddress() {
		if (this.hostAddressKnown) {
			return this.hostAddress;
		}
		if (this.host == null) {
			return null;
		}
		try {
			this.hostAddress = InetAddress.getByName(this.host);
		}
		catch (final UnknownHostException ex) {
			LOGGER.warning("El host establecido no se puede resolver: " + ex); //$NON-NLS-1$
			this.hostAddress = null;
		}
		this.hostAddressKnown = true;
		return this.hostAddress;
	}

	/** The class contains a utility method for converting a <code>String</code>
	 * into a MIME format called "<code>x-www-form-urlencoded</code>" format.
	 * <p>
	 * To convert a <code>String</code>, each character is examined in turn:
	 * <ul>
	 * <li>The ASCII characters '<code>a</code>' through '<code>z</code>', '
	 * <code>A</code>' through '<code>Z</code>', '<code>0</code>' through '
	 * <code>9</code>', and &quot;.&quot;, &quot;-&quot;, &quot;*&quot;,
	 * &quot;_&quot; remain the same.
	 * <li>The space character '<code>&nbsp;</code>' is converted into a plus
	 * sign '<code>+</code>'.
	 * <li>All other characters are converted into the 3-character string "
	 * <code>%<i>xy</i></code>", where <i>xy</i> is the two-digit hexadecimal
	 * representation of the lower 8-bits of the character.
	 * </ul>
	 * @author Herb Jellinek
	 * @version 1.16, 10/23/99
	 * @since JDK1.0 */
	static BitSet dontNeedEncoding;
	static final int CASE_DIFF = 'a' - 'A';

	/* The list of characters that are not encoded have been determined by
	 * referencing O'Reilly's "HTML: The Definitive Guide" (page 164). */

	static {
		dontNeedEncoding = new BitSet(256);
		int i;
		for (i = 'a'; i <= 'z'; i++) {
			dontNeedEncoding.set(i);
		}
		for (i = 'A'; i <= 'Z'; i++) {
			dontNeedEncoding.set(i);
		}
		for (i = '0'; i <= '9'; i++) {
			dontNeedEncoding.set(i);
		}
		/* encoding a space to a + is done in the encode() method */
		dontNeedEncoding.set(' ');
		dontNeedEncoding.set('-');
		dontNeedEncoding.set('_');
		dontNeedEncoding.set('.');
		dontNeedEncoding.set('*');
	}

	/** The class contains a utility method for converting from a MIME format
	 * called "<code>x-www-form-urlencoded</code>" to a <code>String</code>
	 * <p>
	 * To convert to a <code>String</code>, each character is examined in turn:
	 * <ul>
	 * <li>The ASCII characters '<code>a</code>' through '<code>z</code>', '
	 * <code>A</code>' through '<code>Z</code>', and '<code>0</code>' through '
	 * <code>9</code>' remain the same.
	 * <li>The plus sign '<code>+</code>'is converted into a space character '
	 * <code>&nbsp;</code>'.
	 * <li>The remaining characters are represented by 3-character strings which
	 * begin with the percent sign, "<code>%<i>xy</i></code>", where <i>xy</i>
	 * is the two-digit hexadecimal representation of the lower 8-bits of the
	 * character.
	 * </ul>
	 *
	 * @author Mark Chamness
	 * @author Michael McCloskey
	 * @version 1.7, 10/22/99
	 * @since 1.2
	 */

	/** Decodes a &quot;x-www-form-urlencoded&quot; to a <code>String</code>.
	 * @param s The <code>String</code> to decode.
	 * @return The newly decoded <code>String</code>. */
	static String decode(final String s) {
		if (s == null) {
			return null;
		}
		if (indexOfAny(s, "+%") == -1) { //$NON-NLS-1$
			return s; // the common case
		}

		final StringBuffer sb = new StringBuffer();
		for (int i = 0; i < s.length(); i++) {
			final char c = s.charAt(i);
			switch (c) {
				case '+':
					sb.append(' ');
					break;
				case '%':
					try {
						sb.append((char) Integer.parseInt(
							s.substring(i + 1, i + 3), 16)
						);
					}
					catch (final NumberFormatException e) {
						throw new IllegalArgumentException(e);
					}
					i += 2;
					break;
				default:
					sb.append(c);
					break;
			}
		}
		// Undo conversion to external encoding
		String result = sb.toString();
		try {
			final byte[] inputBytes = result.getBytes("8859_1"); //$NON-NLS-1$
			result = new String(inputBytes);
		}
		catch (final UnsupportedEncodingException e) {
			LOGGER.warning("El sistema no soporta la codificacion ISO 8859-1: " + e); //$NON-NLS-1$
			// The system should always have 8859_1
		}
		return result;
	}

	/** Return the first index of any of the characters in "any" in "s", or -1 if
	 * none are found.
	 * This should be a method on String.
	 * @param s Input string.
	 * @param any String of characters to search for.
	 * @return  The first index of any of the characters in "any" in "s", or -1 if
	 *          none are found. */
	private static int indexOfAny(final String s, final String any) {
		return indexOfAny(s, any, 0);
	}

	private static int indexOfAny(final String s, final String any, final int start) {
		try {
			final int len = s.length();
			for (int i = start; i < len; i++) {
				if (any.indexOf(s.charAt(i)) >= 0) {
					return i;
				}
			}
			return -1;
		}
		catch (final StringIndexOutOfBoundsException e) {
			return -1;
		}
	}

}
