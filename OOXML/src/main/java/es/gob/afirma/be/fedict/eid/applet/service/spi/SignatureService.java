/*
 * eID Applet Project.
 * Copyright (C) 2008-2009 FedICT.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License version
 * 3.0 as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, see
 * http://www.gnu.org/licenses/.
 */

package es.gob.afirma.be.fedict.eid.applet.service.spi;

import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.List;

/** Interface for signature service component.
 * @author Frank Cornelis */
public interface SignatureService {

    /** Gives back the digest algorithm to be used for construction of the digest
     * infos of the preSign method. Return a digest algorithm here if you want
     * to let the client sign some locally stored files. Return <code>null</code> if no pre-sign digest infos are required. */
    String getFilesDigestAlgorithm();

    /** Pre-sign callback method. Depending on the configuration some parameters
     * are passed. The returned value will be signed by the eID Applet.
     * <p>
     * TODO: service must be able to throw some exception on failure.
     * </p>
     * @param digestInfos
     *        the optional list of digest infos.
     * @param signingCertificateChain
     *        the optional list of certificates.
     * @return the digest to be signed.
     * @throws NoSuchAlgorithmException */
    byte[] preSign(List<DigestInfo> digestInfos, List<X509Certificate> signingCertificateChain, PrivateKey signingKey) throws NoSuchAlgorithmException;

    /** Post-sign callback method. Received the signature value. Depending on the
     * configuration the signing certificate chain is also obtained.
     * <p>
     * TODO: service must be able to throw some exception on failure.
     * </p>
     * @param signatureValue
     * @param signingCertificateChain
     *        the optional chain of signing certificates. */
    byte[] postSign(byte[] signedXML, List<X509Certificate> signingCertificateChain, String signatureId, byte[] signatureValue);
}
