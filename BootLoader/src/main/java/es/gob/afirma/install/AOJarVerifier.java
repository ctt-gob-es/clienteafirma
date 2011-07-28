/*
 * Copyright 1997-2006 Sun Microsystems, Inc.  All Rights Reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the LICENSE file that accompanied this code.
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
 * Please contact Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * CA 95054 USA or visit www.sun.com if you need additional information or
 * have any questions.
 */

package es.gob.afirma.install;

import java.io.InputStream;
import java.security.CodeSigner;
import java.security.PublicKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Vector;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

final class AOJarVerifier {

    private boolean hasExpiredCert = false;
    private boolean hasExpiringCert = false;
    private boolean notYetValidCert = false;

    private static final long SIX_MONTHS = 180 * 24 * 60 * 60 * 1000L; // milliseconds

    private static final String META_INF = "META-INF/";

    // prefix for new signature-related files in META-INF directory
    private static final String SIG_PREFIX = META_INF + "SIG-";

    /** Verifica la firma JAR de un fichero JAR o ZIP-
     * @param jarName Nombre (incluida ruta) del fichero cuya firma se desea verificar
     * @param caCert Certificado de la autoridad de certificaci&oacute;n aceptada para las
     *        firmas del fichero
     * @throws SecurityException Si la firma no es v&aacute;lida u ocurre cualquier
     *         problema durante la verificaci&oacute;n */
    void verifyJar(final String jarName, final X509Certificate caCert, final X509Certificate signerCert) {

        if (jarName == null || "".equals(jarName)) {
            throw new SecurityException("El fichero proporcionado es nulo o vacio, y por lo tanto no esta firmado");
        }

        if (caCert == null) {
            throw new SecurityException("Es obligatorio proporcionar un certificado CA para comprobar las firmas");
        }

        final PublicKey pkCA = caCert.getPublicKey();

        boolean anySigned = false;
        boolean hasUnsignedEntry = false;
        JarFile jf = null;

        try {
            jf = new JarFile(jarName, true);
            final Vector<JarEntry> entriesVec = new Vector<JarEntry>();
            final byte[] buffer = new byte[8192];

            final Enumeration<JarEntry> entries = jf.entries();
            while (entries.hasMoreElements()) {
                final JarEntry je = entries.nextElement();
                entriesVec.addElement(je);
                InputStream is = null;
                try {
                    is = jf.getInputStream(je);
                    while (is.read(buffer, 0, buffer.length) != -1) {
                        // we just read. this will throw a SecurityException
                        // if a signature/digest check fails.
                    }
                }
                finally {
                    if (is != null) {
                        try {
                            is.close();
                        }
                        catch (final Exception e) {}
                    }
                }
            }

            final Manifest man = jf.getManifest();

            if (man != null) {
                final Enumeration<JarEntry> e = entriesVec.elements();

                final long now = System.currentTimeMillis();

                while (e.hasMoreElements()) {
                    final JarEntry je = e.nextElement();
                    final String name = je.getName();
                    final CodeSigner[] signers = je.getCodeSigners();
                    final boolean isSigned = (signers != null);
                    anySigned |= isSigned;
                    hasUnsignedEntry |= !je.isDirectory() && !isSigned && !signatureRelated(name);

                    if (isSigned && signers != null) {
                        for (final CodeSigner cs : signers) {
                            final Certificate cert = cs.getSignerCertPath().getCertificates().get(0);
                            if (cert instanceof X509Certificate) {
                                final long notAfter = ((X509Certificate) cert).getNotAfter().getTime();
                                if (notAfter < now) {
                                    this.hasExpiredCert = true;
                                }
                                else if (notAfter < now + SIX_MONTHS) {
                                    this.hasExpiringCert = true;
                                }
                                else if (((X509Certificate) cert).getNotBefore().getTime() > now) {
                                    this.notYetValidCert = true;
                                }
                                ((X509Certificate) cert).verify(pkCA);
                                // No me fio del .equals directo del PublicKey
                                if (signerCert != null && (!Arrays.equals(signerCert.getPublicKey().getEncoded(), cert.getPublicKey().getEncoded()))) {
                                    throw new SecurityException("El certificado firmante era de una CA valida, pero no se corresponde con el indicado");
                                }
                            }
                            else {
                                throw new SecurityException("El codigo se ha firmado con un certificado que no cumple la norma X.509");
                            }
                        }
                    } // if isSigned

                } // while
            } // if man != null
            else {
                throw new SecurityException("No se encontro manifest en el fichero ZIP/JAR");
            }

            if (!anySigned) {
                throw new SecurityException("El fichero ZIP/JAR no esta firmado (faltan firmas o no son procesables)");
            }

            if (hasUnsignedEntry) {
                throw new SecurityException("Hay entradas sin firmar en el fichero ZIP/JAR");
            }

            if (this.hasExpiringCert) {
                AfirmaBootLoader.LOGGER.warning("El fichero ZIP/JAR contiene entradas firmadas con un certificado que caduca en los proximos meses");
            }

            if (this.hasExpiredCert) {
                AfirmaBootLoader.LOGGER.warning("El fichero ZIP/JAR contiene entradas firmadas con un certificado caducado");
            }

            if (this.notYetValidCert) {
                AfirmaBootLoader.LOGGER.warning("El fichero ZIP/JAR contiene entradas firmadas con un certificado aun no valido");
            }

        }
        catch (final Exception e) {
            throw new SecurityException("La firma del fichero JAR/ZIP '" + jarName + "' no es valida o no se ha podido comprobar", e);
        }
        finally { // close the resource
            if (jf != null) {
                try {
                    jf.close();
                }
                catch (final Exception e) {}
            }
        }

    }

    /** signature-related files include:
     * . META-INF/MANIFEST.MF
     * . META-INF/SIG-*
     * . META-INF/*.SF
     * . META-INF/*.DSA
     * . META-INF/*.RSA */
    private boolean signatureRelated(final String name) {
        final String ucName = name.toUpperCase();
        if (ucName.equals(JarFile.MANIFEST_NAME) || ucName.equals(META_INF)
                || (ucName.startsWith(SIG_PREFIX) && ucName.indexOf('/') == ucName.lastIndexOf('/'))) {
            return true;
        }

        if (ucName.startsWith(META_INF) && isBlockOrSF(ucName)) {
            // .SF/.DSA/.RSA files in META-INF subdirs
            // are not considered signature-related
            return (ucName.indexOf('/') == ucName.lastIndexOf('/'));
        }

        return false;
    }

    /** Utility method to determine the signature file names and PKCS7 block
     * files names that are supported
     * @param s file name
     * @return true if the input file name is a supported
     *         Signature File or PKCS7 block file name */
    private boolean isBlockOrSF(final String s) {
        // we currently only support DSA and RSA PKCS7 blocks
        if (s.endsWith(".SF") || s.endsWith(".DSA") || s.endsWith(".RSA")) {
            return true;
        }
        return false;
    }
}
