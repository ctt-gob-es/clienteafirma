/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.multi.cades;

import java.io.ByteArrayInputStream;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.signers.AOCoSigner;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cades.CAdESValidator;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/** Operaciones de cofirma CAdES. */
public class AOCAdESCoSigner implements AOCoSigner {

	/** {@inheritDoc} */

	public byte[] cosign(final byte[] data,
                         final byte[] sign,
                         final String algorithm,
                         final PrivateKeyEntry keyEntry,
                         final Properties xParams) throws AOException {

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        byte[] messageDigest = null;
        final String precalculatedDigest = extraParams.getProperty("precalculatedHashAlgorithm"); //$NON-NLS-1$
        if (precalculatedDigest != null) {
            messageDigest = data;
        }

        boolean signingCertificateV2;
        if (AOSignConstants.isSHA2SignatureAlgorithm(algorithm)) {
        	signingCertificateV2 = true;
        } else if (extraParams.containsKey("signingCertificateV2")) { //$NON-NLS-1$
        	signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2")); //$NON-NLS-1$
        } else {
        	signingCertificateV2 = !"SHA1".equals(AOSignConstants.getDigestAlgorithmName(algorithm));	 //$NON-NLS-1$
        }

        final P7ContentSignerParameters csp = new P7ContentSignerParameters(
    		data,
    		algorithm,
    		(Boolean.parseBoolean(extraParams.getProperty("includeOnlySignningCertificate"))) ? //$NON-NLS-1$
    			new X509Certificate[] { (X509Certificate) keyEntry.getCertificateChain()[0] } :
				(X509Certificate[]) keyEntry.getCertificateChain()
		);

        String contentTypeOid = MimeHelper.DEFAULT_CONTENT_OID_DATA;
        String contentDescription = MimeHelper.DEFAULT_CONTENT_DESCRIPTION;
		if (data != null) {
			try {
				final MimeHelper mimeHelper = new MimeHelper(data);
				contentDescription = mimeHelper.getDescription();
				contentTypeOid = MimeHelper.transformMimeTypeToOid(mimeHelper.getMimeType());
			} catch (final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
						"No se han podido cargar las librerias para identificar el tipo de dato firmado: " + e); //$NON-NLS-1$
			}
		}

        try {
            // Si la firma que nos introducen es SignedData
            //final boolean signedData = new ValidateCMS().isCMSSignedData(sign);
            final boolean signedData = CAdESValidator.isCAdESSignedData(sign);
            if (signedData) {

                final String mode = extraParams.getProperty("mode", AOSignConstants.DEFAULT_SIGN_MODE); //$NON-NLS-1$
                final boolean omitContent = mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT) || precalculatedDigest != null;

                return new CAdESCoSigner().coSigner(
                    csp,
                    sign,
                    omitContent,
                    new AdESPolicy(extraParams),
                    signingCertificateV2,
                    keyEntry,
                    messageDigest,
                    contentTypeOid,
                    contentDescription
                );
            }

            return new CAdESCoSignerEnveloped().coSigner(
                 csp,
                 sign,
                 new AdESPolicy(extraParams),
                 signingCertificateV2,
                 keyEntry,
                 messageDigest,
                 contentTypeOid,
                 contentDescription
            );

        }
        catch (final Exception e) {
            throw new AOException("Error generando la Cofirma CAdES", e); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
	public byte[] cosign(final byte[] sign,
                         final String algorithm,
                         final PrivateKeyEntry keyEntry,
                         final Properties xParams) throws AOException {

        final Properties extraParams = (xParams != null) ? xParams : new Properties();

        boolean signingCertificateV2;
        if (AOSignConstants.isSHA2SignatureAlgorithm(algorithm)) {
        	signingCertificateV2 = true;
        } else if (extraParams.containsKey("signingCertificateV2")) { //$NON-NLS-1$
        	signingCertificateV2 = Boolean.parseBoolean(extraParams.getProperty("signingCertificateV2")); //$NON-NLS-1$
        } else {
        	signingCertificateV2 = !"SHA1".equals(AOSignConstants.getDigestAlgorithmName(algorithm));	 //$NON-NLS-1$
        }

        // algoritmo de firma.
        final String typeAlgorithm = algorithm;

        String contentTypeOid = MimeHelper.DEFAULT_CONTENT_OID_DATA;
        String contentDescription = MimeHelper.DEFAULT_CONTENT_DESCRIPTION;
        final byte[] data = new AOCAdESSigner().getData(sign);
        if (data != null) {
        	final MimeHelper mimeHelper = new MimeHelper(data);
			contentDescription = mimeHelper.getDescription();
			contentTypeOid = MimeHelper.transformMimeTypeToOid(mimeHelper.getMimeType());
        }


        // Si la firma que nos introducen es SignedData
        if (CAdESValidator.isCAdESSignedData(sign)) {
            try {
                return new CAdESCoSigner().coSigner(
                    typeAlgorithm,
                    (Boolean.parseBoolean(extraParams.getProperty("includeOnlySignningCertificate"))) ? //$NON-NLS-1$
            			new X509Certificate[] { (X509Certificate) keyEntry.getCertificateChain()[0] } :
        				(X509Certificate[]) keyEntry.getCertificateChain(),
                    new ByteArrayInputStream(sign),
                    new AdESPolicy(extraParams),
                    signingCertificateV2,
                    keyEntry,
                    null, // null porque no nos pueden dar un hash en este metodo, tendria que ser en el que incluye datos
                    contentTypeOid,
                    contentDescription
                );
            }
            catch (final Exception e) {
                throw new AOException("Error generando la Cofirma CADES", e); //$NON-NLS-1$
            }
        }

        // Signed And Enveloped.
        try {
            return new CAdESCoSignerEnveloped().coSigner(
                 typeAlgorithm,
                 (Boolean.parseBoolean(extraParams.getProperty("includeOnlySignningCertificate"))) ? //$NON-NLS-1$
         			new X509Certificate[] { (X509Certificate) keyEntry.getCertificateChain()[0] } :
     				(X509Certificate[]) keyEntry.getCertificateChain(),
                 new ByteArrayInputStream(sign),
                 new AdESPolicy(extraParams),
                 signingCertificateV2,
                 keyEntry,
                 null, // null porque no nos pueden dar un hash en este metodo, tendria que ser en el que incluye datos
                 contentTypeOid,
                 contentDescription
            );
        }
        catch (final Exception e) {
            throw new AOException("Error generando la Cofirma CADES", e); //$NON-NLS-1$
        }

    }

}
