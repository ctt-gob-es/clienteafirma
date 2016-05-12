/*
 * Copyright (c) 1998, 2014, Oracle and/or its affiliates. All rights reserved.
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

package es.gob.afirma.standalone.configurator.jre.security.util;

import java.math.BigInteger;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import es.gob.afirma.standalone.configurator.jre.security.action.GetPropertyAction;

/**
 * A utility class for debuging.
 *
 * @author Roland Schemers
 */
public class Debug {

    private String prefix;

    private static String args;

    static {
        args = java.security.AccessController.doPrivileged
                (new GetPropertyAction
                ("java.security.debug")); //$NON-NLS-1$

        final String args2 = java.security.AccessController.doPrivileged
                (new GetPropertyAction
                ("java.security.auth.debug")); //$NON-NLS-1$

        if (args == null) {
            args = args2;
        } else {
            if (args2 != null) {
				args = args + "," + args2; //$NON-NLS-1$
			}
        }

        if (args != null) {
            args = marshal(args);
            if (args.equals("help")) { //$NON-NLS-1$
                Help();
            }
        }
    }

    public static void Help()
    {
        System.err.println();
        System.err.println("all           turn on all debugging"); //$NON-NLS-1$
        System.err.println("access        print all checkPermission results"); //$NON-NLS-1$
        System.err.println("certpath      PKIX CertPathBuilder and"); //$NON-NLS-1$
        System.err.println("              CertPathValidator debugging"); //$NON-NLS-1$
        System.err.println("combiner      SubjectDomainCombiner debugging"); //$NON-NLS-1$
        System.err.println("gssloginconfig"); //$NON-NLS-1$
        System.err.println("              GSS LoginConfigImpl debugging"); //$NON-NLS-1$
        System.err.println("configfile    JAAS ConfigFile loading"); //$NON-NLS-1$
        System.err.println("configparser  JAAS ConfigFile parsing"); //$NON-NLS-1$
        System.err.println("jar           jar verification"); //$NON-NLS-1$
        System.err.println("logincontext  login context results"); //$NON-NLS-1$
        System.err.println("jca           JCA engine class debugging"); //$NON-NLS-1$
        System.err.println("policy        loading and granting"); //$NON-NLS-1$
        System.err.println("provider      security provider debugging"); //$NON-NLS-1$
        System.err.println("pkcs11        PKCS11 session manager debugging"); //$NON-NLS-1$
        System.err.println("pkcs11keystore"); //$NON-NLS-1$
        System.err.println("              PKCS11 KeyStore debugging"); //$NON-NLS-1$
        System.err.println("sunpkcs11     SunPKCS11 provider debugging"); //$NON-NLS-1$
        System.err.println("scl           permissions SecureClassLoader assigns"); //$NON-NLS-1$
        System.err.println("ts            timestamping"); //$NON-NLS-1$
        System.err.println();
        System.err.println("The following can be used with access:"); //$NON-NLS-1$
        System.err.println();
        System.err.println("stack         include stack trace"); //$NON-NLS-1$
        System.err.println("domain        dump all domains in context"); //$NON-NLS-1$
        System.err.println("failure       before throwing exception, dump stack"); //$NON-NLS-1$
        System.err.println("              and domain that didn't have permission"); //$NON-NLS-1$
        System.err.println();
        System.err.println("The following can be used with stack and domain:"); //$NON-NLS-1$
        System.err.println();
        System.err.println("permission=<classname>"); //$NON-NLS-1$
        System.err.println("              only dump output if specified permission"); //$NON-NLS-1$
        System.err.println("              is being checked"); //$NON-NLS-1$
        System.err.println("codebase=<URL>"); //$NON-NLS-1$
        System.err.println("              only dump output if specified codebase"); //$NON-NLS-1$
        System.err.println("              is being checked"); //$NON-NLS-1$
        System.err.println();
        System.err.println("The following can be used with provider:"); //$NON-NLS-1$
        System.err.println();
        System.err.println("engine=<engines>"); //$NON-NLS-1$
        System.err.println("              only dump output for the specified list"); //$NON-NLS-1$
        System.err.println("              of JCA engines. Supported values:"); //$NON-NLS-1$
        System.err.println("              Cipher, KeyAgreement, KeyGenerator,"); //$NON-NLS-1$
        System.err.println("              KeyPairGenerator, KeyStore, Mac,"); //$NON-NLS-1$
        System.err.println("              MessageDigest, SecureRandom, Signature."); //$NON-NLS-1$
        System.err.println();
        System.err.println("Note: Separate multiple options with a comma"); //$NON-NLS-1$
        System.exit(0);
    }


    /**
     * Get a Debug object corresponding to whether or not the given
     * option is set. Set the prefix to be the same as option.
     */

    public static Debug getInstance(final String option)
    {
        return getInstance(option, option);
    }

    /**
     * Get a Debug object corresponding to whether or not the given
     * option is set. Set the prefix to be prefix.
     */
    public static Debug getInstance(final String option, final String prefix)
    {
        if (isOn(option)) {
            final Debug d = new Debug();
            d.prefix = prefix;
            return d;
        } else {
            return null;
        }
    }

    /**
     * True if the system property "security.debug" contains the
     * string "option".
     */
    public static boolean isOn(final String option)
    {
        if (args == null) {
			return false;
		} else {
            if (args.indexOf("all") != -1) { //$NON-NLS-1$
				return true;
			} else {
				return (args.indexOf(option) != -1);
			}
        }
    }

    /**
     * print a message to stderr that is prefixed with the prefix
     * created from the call to getInstance.
     */

    public void println(final String message)
    {
        System.err.println(this.prefix + ": "+message); //$NON-NLS-1$
    }

    /**
     * print a blank line to stderr that is prefixed with the prefix.
     */

    public void println()
    {
        System.err.println(this.prefix + ":"); //$NON-NLS-1$
    }

    /**
     * print a message to stderr that is prefixed with the prefix.
     */

    public static void println(final String prefix, final String message)
    {
        System.err.println(prefix + ": "+message); //$NON-NLS-1$
    }

    /**
     * return a hexadecimal printed representation of the specified
     * BigInteger object. the value is formatted to fit on lines of
     * at least 75 characters, with embedded newlines. Words are
     * separated for readability, with eight words (32 bytes) per line.
     */
    public static String toHexString(final BigInteger b) {
        String hexValue = b.toString(16);
        final StringBuffer buf = new StringBuffer(hexValue.length()*2);

        if (hexValue.startsWith("-")) { //$NON-NLS-1$
            buf.append("   -"); //$NON-NLS-1$
            hexValue = hexValue.substring(1);
        } else {
            buf.append("    ");     // four spaces //$NON-NLS-1$
        }
        if ((hexValue.length()%2) != 0) {
            // add back the leading 0
            hexValue = "0" + hexValue; //$NON-NLS-1$
        }
        int i=0;
        while (i < hexValue.length()) {
            // one byte at a time
            buf.append(hexValue.substring(i, i+2));
            i+=2;
            if (i!= hexValue.length()) {
                if ((i%64) == 0) {
                    buf.append("\n    ");     // line after eight words //$NON-NLS-1$
                } else if (i%8 == 0) {
                    buf.append(" ");     // space between words //$NON-NLS-1$
                }
            }
        }
        return buf.toString();
    }

    /**
     * change a string into lower case except permission classes and URLs.
     */
    private static String marshal(final String args) {
        if (args != null) {
            final StringBuffer target = new StringBuffer();
            StringBuffer source = new StringBuffer(args);

            // obtain the "permission=<classname>" options
            // the syntax of classname: IDENTIFIER.IDENTIFIER
            // the regular express to match a class name:
            // "[a-zA-Z_$][a-zA-Z0-9_$]*([.][a-zA-Z_$][a-zA-Z0-9_$]*)*"
            String keyReg = "[Pp][Ee][Rr][Mm][Ii][Ss][Ss][Ii][Oo][Nn]="; //$NON-NLS-1$
            String keyStr = "permission="; //$NON-NLS-1$
            String reg = keyReg +
                "[a-zA-Z_$][a-zA-Z0-9_$]*([.][a-zA-Z_$][a-zA-Z0-9_$]*)*"; //$NON-NLS-1$
            Pattern pattern = Pattern.compile(reg);
            Matcher matcher = pattern.matcher(source);
            StringBuffer left = new StringBuffer();
            while (matcher.find()) {
                final String matched = matcher.group();
                target.append(matched.replaceFirst(keyReg, keyStr));
                target.append("  "); //$NON-NLS-1$

                // delete the matched sequence
                matcher.appendReplacement(left, ""); //$NON-NLS-1$
            }
            matcher.appendTail(left);
            source = left;

            // obtain the "codebase=<URL>" options
            // the syntax of URL is too flexible, and here assumes that the
            // URL contains no space, comma(','), and semicolon(';'). That
            // also means those characters also could be used as separator
            // after codebase option.
            // However, the assumption is incorrect in some special situation
            // when the URL contains comma or semicolon
            keyReg = "[Cc][Oo][Dd][Ee][Bb][Aa][Ss][Ee]="; //$NON-NLS-1$
            keyStr = "codebase="; //$NON-NLS-1$
            reg = keyReg + "[^, ;]*"; //$NON-NLS-1$
            pattern = Pattern.compile(reg);
            matcher = pattern.matcher(source);
            left = new StringBuffer();
            while (matcher.find()) {
                final String matched = matcher.group();
                target.append(matched.replaceFirst(keyReg, keyStr));
                target.append("  "); //$NON-NLS-1$

                // delete the matched sequence
                matcher.appendReplacement(left, ""); //$NON-NLS-1$
            }
            matcher.appendTail(left);
            source = left;

            // convert the rest to lower-case characters
            target.append(source.toString().toLowerCase(Locale.ENGLISH));

            return target.toString();
        }

        return null;
    }

    private final static char[] hexDigits = "0123456789abcdef".toCharArray(); //$NON-NLS-1$

    public static String toString(final byte[] b) {
        if (b == null) {
            return "(null)"; //$NON-NLS-1$
        }
        final StringBuilder sb = new StringBuilder(b.length * 3);
        for (int i = 0; i < b.length; i++) {
            final int k = b[i] & 0xff;
            if (i != 0) {
                sb.append(':');
            }
            sb.append(hexDigits[k >>> 4]);
            sb.append(hexDigits[k & 0xf]);
        }
        return sb.toString();
    }

}
