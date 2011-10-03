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
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.logging.Logger;

final class AOJarVerifier {
    
    /** Gestor de registro. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$;

    private boolean hasExpiredCert = false;
    private boolean hasExpiringCert = false;
    private boolean notYetValidCert = false;
    
    private static final int BUFFER_SIZE = 8192;

    private static final long SIX_MONTHS = 180 * 24 * 60 * 60 * 1000L; // milliseconds

    private static final String META_INF = "META-INF/"; //$NON-NLS-1$

    // prefix for new signature-related files in META-INF directory
    private static final String SIG_PREFIX = META_INF + "SIG-"; //$NON-NLS-1$

    /** Verifica la firma JAR de un fichero JAR o ZIP-
     * @param jarName Nombre (incluida ruta) del fichero cuya firma se desea verificar
     * @param signerCert Certificado que debe ser el firmante del fichero
     * @throws SecurityException Si la firma no es v&aacute;lida u ocurre cualquier
     *         problema durante la verificaci&oacute;n */
    void verifyJar(final String jarName, final X509Certificate signerCert) {

        if (jarName == null || "".equals(jarName)) { //$NON-NLS-1$
            throw new SecurityException("El fichero proporcionado es nulo o vacio, y por lo tanto no esta firmado"); //$NON-NLS-1$
        }

        if (signerCert == null) {
            throw new SecurityException("Es obligatorio proporcionar un certificado para comprobar las firmas"); //$NON-NLS-1$
        }

        boolean anySigned = false;
        boolean hasUnsignedEntry = false;
        JarFile jf = null;

        try {
            jf = new JarFile(jarName, true);
            final ArrayList<JarEntry> entriesVec = new ArrayList<JarEntry>();
            final byte[] buffer = new byte[BUFFER_SIZE];

            final Enumeration<JarEntry> entries = jf.entries();
            while (entries.hasMoreElements()) {
                final JarEntry je = entries.nextElement();
                entriesVec.add(je);
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
                        catch (final Exception e) {
                         // Ignoramos los errores en el cierre
                        }
                    }
                }
            }

            final Manifest man = jf.getManifest();

            if (man != null) {
                final long now = System.currentTimeMillis();
                for (final JarEntry je : entriesVec) {
                    final CodeSigner[] signers = je.getCodeSigners();
                    final boolean isSigned = (signers != null);
                    anySigned |= isSigned;
                    hasUnsignedEntry |= !je.isDirectory() && !isSigned && !signatureRelated(je.getName());

                    if (isSigned && (signers != null)) {
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

                                if (!signerCert.equals(cert)) {
                                    throw new SecurityException("El certificado firmante no se corresponde con el indicado"); //$NON-NLS-1$
                                }
                            }
                            else {
                                throw new SecurityException("El codigo se ha firmado con un certificado que no cumple la norma X.509"); //$NON-NLS-1$
                            }
                        }
                    } // if isSigned

                } // while
            } // if man != null
            else {
                throw new SecurityException("No se encontro manifest en el fichero ZIP/JAR"); //$NON-NLS-1$
            }

            if (!anySigned) {
                throw new SecurityException("El fichero ZIP/JAR no esta firmado (faltan firmas o no son procesables)"); //$NON-NLS-1$
            }

            if (hasUnsignedEntry) {
                throw new SecurityException("Hay entradas sin firmar en el fichero ZIP/JAR"); //$NON-NLS-1$
            }

            if (this.hasExpiringCert) {
                LOGGER.warning("El fichero ZIP/JAR contiene entradas firmadas con un certificado que caduca en los proximos meses"); //$NON-NLS-1$
            }

            if (this.hasExpiredCert) {
                LOGGER.warning("El fichero ZIP/JAR contiene entradas firmadas con un certificado caducado"); //$NON-NLS-1$
            }

            if (this.notYetValidCert) {
                LOGGER.warning("El fichero ZIP/JAR contiene entradas firmadas con un certificado aun no valido"); //$NON-NLS-1$
            }

        }
        catch (final Exception e) {
            throw new SecurityException("La firma del fichero JAR/ZIP '" + jarName + "' no es valida o no se ha podido comprobar", e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        finally { // close the resource
            if (jf != null) {
                try {
                    jf.close();
                }
                catch (final Exception e) {
                 // Ignoramos los errores en el cierre
                }
            }
        }

    }

    /** signature-related files include:
     * . META-INF/MANIFEST.MF
     * . META-INF/SIG-*
     * . META-INF/*.SF
     * . META-INF/*.DSA
     * . META-INF/*.RSA */
    static boolean signatureRelated(final String name) {
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
    private static boolean isBlockOrSF(final String s) {
        // we currently only support DSA and RSA PKCS7 blocks
        if (s.endsWith(".SF") || s.endsWith(".DSA") || s.endsWith(".RSA")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            return true;
        }
        return false;
    }
}
